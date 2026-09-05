{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Operations
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Operations, a rewrite of upstream's @src\/XMonad\/Operations.hs@.
--
-- This is where the two backends diverge most, and the divergence is not a
-- matter of a few differing lines: upstream's file is 899 lines of Xlib calls,
-- this one is a fraction of that, because river does the work.  They are not
-- two implementations of the same code -- they are different programs that
-- agree on an interface.
--
-- The interface they agree on is smaller here, deliberately.  Everything X11
-- reached for through the display and cannot be reproduced over Wayland is
-- absent rather than present and inert.  Each omission is justified in
-- tests/api/unportable.txt, and tests/api/check-api.sh fails if one appears
-- that is not.
--
-- The single structural difference to understand: __'windows' does not run the
-- layout.__  river only permits window management state to change during a
-- manage sequence, so the layout runs once at the end of the current sequence,
-- after every queued action has had its say.  Called from outside a sequence
-- -- a timer, a @dbus@ callback, a key binding -- 'windows' asks river to
-- start one.  Upstream's @windows@ tiles immediately because X11 let it.
--
-----------------------------------------------------------------------------

module XMonad.Operations (
    -- * Manage One Window
    unmanage, killWindow, kill, isClient,
    hide, reveal, setWindowBorderWithFallback,
    focus, isFixedSizeOrTransient,

    -- * Manage Windows
    windows, refresh, rescreen, modifyWindowSet, windowBracket, windowBracket_,
    withFocused, withUnfocused,

    -- * Keyboard and Mouse
    cleanMask, extraModifiers, cacheNumlockMask,

    -- * Messages
    sendMessage, broadcastMessage, sendMessageWithNoRefresh,

    -- * Floating Layer
    float, floatLocation,

    -- * Window Size Hints
    D, mkAdjust, applySizeHints, applySizeHints', applySizeHintsContents,
    applyAspectHint, applyResizeIncHint, applyMaxSizeHint,

    -- * Rectangles
    containedIn, nubScreens, pointWithin, scaleRationalRect,
    getCleanedScreenInfo,

    -- * Pointer
    warpPointer,
    mouseDrag, mouseMoveWindow, mouseResizeWindow,

    -- * Lifecycle
    StateFile (..), writeStateToFile, readStateFile, restart,

    -- * Other Utilities
    pointScreen, screenWorkspace,
    setLayout, updateLayout,
    ) where

import XMonad.Core
import XMonad.River.Plan (Op(..))
import XMonad.River.Runtime (emitNow, emitOp, setBorderColor)
import XMonad.River.State (RiverState(..), updatePlacement)
import XMonad.River.Types
import XMonad.River.Protocol.WindowManagement
import qualified XMonad.StackSet as W

import Control.Arrow        (second)
import Data.IORef (readIORef, writeIORef)
import Data.List            (find, nub, sortOn)
import Data.Maybe
import Data.Monoid          (Any(..))
import Data.Ratio           ((%))
import System.Directory     (removeFile)
import System.IO            (Handle, IOMode (ReadMode), hGetContents, withFile)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (forM_, guard, join, unless, void, when)

-- ---------------------------------------------------------------------
-- Window manager operations

isFixedSizeOrTransient :: Display -> Window -> X Bool
isFixedSizeOrTransient _ w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    pure $ case M.lookup w known of
        Nothing -> False
        Just rw ->
            let sh = rwSizeHints rw
                isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
                isTransient = isJust (rwParent rw)
            in isFixedSize || isTransient

-- | Given a window, build an adjuster function that will reduce the given
-- dimensions according to the window's border width and size hints.

unmanage :: Window -> X ()
unmanage = windows . W.delete

-- | Close the given window.  Politely -- this is @xdg_toplevel.close@, which
-- the client may ignore or prompt about, exactly as an X11 @WM_DELETE_WINDOW@
-- could be.

killWindow :: Window -> X ()
killWindow w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    -- Queued rather than sent: @close@ is legal only during a manage sequence,
    -- and this runs from a keybinding, which is outside one.
    when (M.member w known) $ emitOp (OpClose w)

-- | Kill the currently focused client.

kill :: X ()
kill = withFocused killWindow

-- | Hide a window, by removing it from the visible set.
--
-- The compositor call happens in the render sequence -- river only applies
-- rendering state at @render_finish@ -- so this records the intent and lets
-- 'XMonad.River.WM.renderSequence' carry it out.

-- ---------------------------------------------------------------------
-- Managing windows

-- | Modify the current window list with a pure function, and arrange for the
-- result to be applied.
--
-- Unlike the X11 version this does not itself run the layout.  See the module
-- header: layout belongs to the manage sequence, and this function's job is to
-- make sure one happens.
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    modify $ \st -> st { windowset = f (windowset st) }
    inSeq <- io . readIORef =<< asks (inManageSeq . riverState)
    unless inSeq requestManageSequence
    asks (logHook . config) >>= userCodeDef ()

-- | Ask river to start a manage sequence, because state it cannot observe has
-- changed.

-- Recorded rather than sent.  The event loop owns the connection, and this
-- runs wherever an X action does -- which after the thread split is not the
-- loop.  The loop sends @manage_dirty@ for whatever is flagged before it waits
-- again, so the request costs one pass through the loop and no round trip.
requestManageSequence :: X ()
requestManageSequence = do
    ref <- asks (riverDirty . riverState)
    io (writeIORef ref True)

-- | Re-run the layout.  Under river this is a request for another manage
-- sequence; the layout runs there.

modifyWindowSet :: (WindowSet -> WindowSet) -> X ()
modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

-- | Perform an @X@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.

windowBracket :: (a -> Bool) -> X a -> X a
windowBracket p action = withWindowSet $ \old -> do
  a <- action
  when (p a) . withWindowSet $ \new -> do
    modifyWindowSet $ const old
    windows         $ const new
  return a

-- | Perform an @X@ action. If it returns @Any True@, unwind the
-- changes to the @WindowSet@ and replay them using @windows@. This is
-- a version of @windowBracket@ that discards the return value and
-- handles an @X@ action that reports its need for refresh via @Any@.

windowBracket_ :: X Any -> X ()
windowBracket_ = void . windowBracket getAny

-- | A window no longer exists; remove it from the window
-- list, on whatever workspace it is.
--

scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh)
 = Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
 where scale s r = floor (toRational s * r)

-- | Return workspace visible on screen @sc@, or 'Nothing'.

setWindowBorderWithFallback :: Display -> Window -> String -> Pixel -> X ()
setWindowBorderWithFallback _ w color fallback =
    io (setBorderColor w (pixelColor (fromMaybe fallback (parseColorMaybe color))))

-- | Is the window is under management by xmonad?

hide :: Window -> X ()
hide w = whenX (gets (S.member w . mapped)) $
    modify (\s -> s { mapped = S.delete w (mapped s) })

-- | Show a window.
-- This is harmless if the window was already visible.

reveal :: Window -> X ()
reveal w = whenX (isClient w) $
    modify (\s -> s { mapped = S.insert w (mapped s) })

-- | Set the border colour of a window from a colour name, falling back to a
-- 'Pixel' if the name does not parse.
--
-- X11's fallback existed because @initColor@ asked the server to allocate the
-- colour and that could fail for reasons a config could not predict -- a full
-- colormap, an unknown name from @rgb.txt@.  Here the only way to fail is a
-- string that is not @\"#rrggbb\"@, since nothing is allocated and there is no
-- server to ask.  The fallback is still used, just for a narrower reason.
--
-- The signature is X11's exactly, 'Pixel' and all.

refresh :: X ()
refresh = windows id

-- | Modify the @WindowSet@ in state with no special handling.

-- ---------------------------------------------------------------------

containedIn :: Rectangle -> Rectangle -> Bool
containedIn r1@(Rectangle x1 y1 w1 h1) r2@(Rectangle x2 y2 w2 h2)
 = and [ r1 /= r2
       , x1 >= x2
       , y1 >= y2
       , fromIntegral x1 + w1 <= fromIntegral x2 + w2
       , fromIntegral y1 + h1 <= fromIntegral y2 + h2 ]

-- | Given a list of screens, remove all duplicated screens and screens that
-- are entirely contained within another.

nubScreens :: [Rectangle] -> [Rectangle]
nubScreens xs = nub . filter (\x -> not $ any (x `containedIn`) xs) $ xs

-- | The screen rectangles, cleaned according to the rules for 'nubScreens'.
--
-- Under X11 this queried xinerama through the display.  Here the outputs are
-- accumulated in 'XConf', so the 'Display' is accepted and unused, as for
-- 'isFixedSizeOrTransient'.  Removed outputs and
-- zero-sized ones are skipped, and an output's layer-shell area is preferred
-- to its raw rectangle where one has been reported, so that a bar or dock
-- claiming an exclusive zone shrinks the tiling area rather than being tiled
-- over.
--
-- Ordering is by position, which is what keeps screen ids stable across a
-- monitor being unplugged and replugged.

getCleanedScreenInfo :: Display -> X [Rectangle]
getCleanedScreenInfo _ = do
    outs <- io . readIORef =<< asks (riverOutputs . riverState)
    pure $ nubScreens
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]

-- | Given a point, determine the screen (if any) that contains it.

rescreen :: X ()
rescreen = refresh

-- | Returns 'True' if the first rectangle is contained within, but not equal
-- to the second.

-- ---------------------------------------------------------------------
-- Setting keyboard focus

focus :: Window -> X ()
focus w = local (\c -> c { mouseFocused = True }) $ withWindowSet $ \s ->
    when (W.member w s && W.peek s /= Just w) $ windows (W.focusWindow w)

-- | Apply an 'X' operation to the currently focused window, if there is one.

cacheNumlockMask :: X ()
cacheNumlockMask = modify $ \s -> s { numberlockMask = 0 }


-- | The screen configuration may have changed; update the state and refresh.
--
-- Under X11 this queried xinerama.  Here the reconciliation against river's
-- outputs happens at the start of every manage sequence anyway, so this only
-- has to ask for one.

sendMessage :: Message a => a -> X ()
sendMessage a = windowBracket_ $ do
    w <- gets $ W.workspace . W.current . windowset
    ml' <- handleMessage (W.layout w) (SomeMessage a) `catchX` return Nothing
    whenJust ml' $ \l' ->
        modifyWindowSet $ \ws -> ws { W.current = (W.current ws)
                                { W.workspace = (W.workspace $ W.current ws)
                                  { W.layout = l' }}}
    return (Any $ isJust ml')

-- | Send a message to all layouts, without refreshing.

broadcastMessage :: Message a => a -> X ()
broadcastMessage a = withWindowSet $ \ws -> do
    let c = W.workspace . W.current $ ws
        v = map W.workspace . W.visible $ ws
        h = W.hidden ws
    mapM_ (sendMessageWithNoRefresh a) (c : v ++ h)

-- | Send a message to a layout, without refreshing.

sendMessageWithNoRefresh :: Message a => a -> WindowSpace -> X ()
sendMessageWithNoRefresh a w =
    handleMessage (W.layout w) (SomeMessage a) `catchX` return Nothing >>=
    updateLayout  (W.tag w)

-- | Update the layout field of a workspace.

updateLayout :: WorkspaceId -> Maybe (Layout Window) -> X ()
updateLayout i ml = whenJust ml $ \l ->
    runOnWorkspaces $ \ww -> return $ if W.tag ww == i then ww { W.layout = l} else ww

-- | Set the layout of the currently viewed workspace.

setLayout :: Layout Window -> X ()
setLayout l = do
    ss@W.StackSet{ W.current = c@W.Screen{ W.workspace = ws }} <- gets windowset
    handleMessage (W.layout ws) (SomeMessage ReleaseResources)
    windows $ const $ ss{ W.current = c{ W.workspace = ws{ W.layout = l } } }

------------------------------------------------------------------------
-- Floating layer support

-- | Given a window, find the screen it is located on, and compute
-- the geometry of that window WRT that screen.
--
-- X11 read this from the window's attributes and size hints.  River reports a
-- window's current dimensions directly, and nothing else: there is no position
-- to read, because under Wayland the compositor owns placement and a client
-- never had one to report.  So the fraction is computed from the dimensions
-- against the current screen, and the window is centred.

screenWorkspace :: ScreenId -> X (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ return . W.lookupWorkspace sc

------------------------------------------------------------------------
-- Message handling

-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows, in which case changes are handled through a refresh.

withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f

-- | Apply an 'X' operation to all unfocused windows on the current workspace, if there are any.

withUnfocused :: (Window -> X ()) -> X ()
withUnfocused f = withWindowSet $ \ws ->
    whenJust (W.peek ws) $ \w ->
        let unfocusedWindows = filter (/= w) $ W.index ws
        in mapM_ f unfocusedWindows


-- | Strip numlock\/capslock from a mask.
--
-- The identity, because river delivers modifiers already resolved: the bits
-- this was written to remove are never set.  Stripping nothing is the correct
-- strip, not a skipped one.

isClient :: Window -> X Bool
isClient w = withWindowSet $ return . W.member w

-- | Set focus explicitly to window @w@ if it is managed by us.

-- These three -- 'cleanMask', 'extraModifiers' and 'cacheNumlockMask' -- have a
-- correct total implementation under river rather than an unavailable one,
-- because the situation each guards against cannot arise.  They are not stubs:
-- each establishes exactly the postcondition its caller is entitled to.
--
-- Their neighbours in upstream's export list -- unGrab, setButtonGrab and
-- clearEvents -- are deliberately *not* here, even though `pure ()` would be
-- equally vacuous.  Those two are advice-shaped: unGrab's caller writes
-- @unGrab >> spawn "slock"@ believing it has handed the keyboard over, and
-- under river it has not.  The locker gets input by a different route
-- entirely, so a silent success would be true about grabs and misleading
-- about the thing the caller cares about.  See tests/api/unportable.txt.

extraModifiers :: X [KeyMask]
extraModifiers = return [0]

-- | Find the numlock modifier and remember it.
--
-- There is no keymap to inspect and no mask to find.  The field is set to
-- zero, which is what 'cleanMask' and 'extraModifiers' above already assume,
-- so this establishes its postcondition rather than skipping it.

cleanMask :: KeyMask -> X KeyMask
cleanMask = return

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
--
-- Exactly one, the empty one.  This existed so that a binding could be
-- grabbed once per numlock\/capslock combination; river's bindings are
-- protocol objects matched against resolved modifiers, so one is all there is.

writeStateToFile :: X ()
writeStateToFile = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    let identOf w = B8.unpack <$> (rwIdentifier =<< M.lookup w known)

        maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
        maybeShow (t, Left str) = Just (t, str)
        maybeShow _ = Nothing

        wsData   = retagWindows identOf . W.mapLayout show . windowset
        extState = mapMaybe maybeShow . M.toList . extensibleState

    path <- asks $ stateFileName . directories
    stateData <- gets (\s -> StateFile (wsData s) (extState s))
    catchIO (writeFile path $ show stateData)

-- | Read the state of a previous xmonad instance from a file and
-- return that state.  The state file is removed after reading it.
--
-- Unlike the X11 version this must not be called before the compositor has
-- advertised its windows, because that is what the identifiers in the file are
-- resolved against.  river sends a @window@ event, and the @identifier@ with
-- it, for every window it already has before the first @manage_start@, so the
-- first manage sequence is both the earliest and the right place.  See
-- @restoreState@ in "XMonad.River.WM".
--
-- The layout witness comes from the config's own 'Layout' rather than from a
-- @Read (l Window)@ constraint on the caller: 'Layout' carries that dictionary
-- itself, and by the time this runs the concrete layout type has already been
-- wrapped.  X11's signature takes the unwrapped config because it is called
-- from @launch@, where the type is still concrete.

retagWindows :: Ord b => (a -> Maybe b) -> W.StackSet i l a s sd -> W.StackSet i l b s sd
retagWindows f (W.StackSet cur vis hid flt) = W.StackSet
    (onScreen cur)
    (map onScreen vis)
    (map onWorkspace hid)
    (M.fromList [ (b, r) | (a, r) <- M.toList flt, Just b <- [f a] ])
  where
    onScreen (W.Screen ws sid sd) = W.Screen (onWorkspace ws) sid sd
    onWorkspace (W.Workspace t l st) = W.Workspace t l (onStack =<< st)
    -- 'up' runs outwards from the focus, so its head is the nearest window
    -- above and no reversal is needed to pick a replacement.
    onStack (W.Stack fo u d) = case (f fo, mapMaybe f u, mapMaybe f d) of
      (Just b, u', d')      -> Just (W.Stack b u' d')
      (Nothing, u', x : d') -> Just (W.Stack x u' d')
      (Nothing, x : u', []) -> Just (W.Stack x u' [])
      (Nothing, [], [])     -> Nothing

-- | Write the current window state (and extensible state) to a file
-- so that xmonad can resume with that state intact.
--
-- A window river has not given an identifier for is left out, and so comes
-- back as a new window through the manage hook.  On river 4 and later there
-- are none: the event is sent to every window as it is created.

readStateFile :: XConfig Layout -> X (Maybe XState)
readStateFile xmc = do
    path <- asks $ stateFileName . directories

    -- I'm trying really hard here to make sure we read the entire
    -- contents of the file before it is removed from the file system.
    sf' <- userCode . io $ do
        raw <- withFile path ReadMode readStrict
        return $! maybeRead reads raw

    io (removeFile path)

    known <- io . readIORef =<< asks (riverWindows . riverState)
    let byIdent = M.fromList
          [ (B8.unpack i, rwObject w)
          | w <- M.elems known, Just i <- [rwIdentifier w] ]

    return $ do
      sf <- join sf'

      let winset = retagWindows (`M.lookup` byIdent)
                 . W.ensureTags layout (workspaces xmc)
                 $ W.mapLayout (fromMaybe layout . maybeRead lreads) (sfWins sf)
          extState = M.fromList . map (second Left) $ sfExt sf

      return XState { windowset       = winset
                    , numberlockMask  = 0
                    , mapped          = S.empty
                    , waitingUnmap    = M.empty
                    , dragging        = Nothing
                    , extensibleState = extState
                    }
  where
    layout = layoutHook xmc
    lreads = readsLayout layout
    maybeRead reads' s = case reads' s of
                           [(x, "")] -> Just x
                           _         -> Nothing

    readStrict :: Handle -> IO String
    readStrict h = hGetContents h >>= \s -> length s `seq` return s

-- | @restart name resume@ restarts the window manager by executing @name@.
--
-- This is the river analogue of xmonad's @restart@, and the reason @M-q@
-- survives the move to Wayland.  The compositor owns the windows, not the
-- window manager, so tearing the window manager down disturbs nothing: river
-- supports hot-swapping window managers without restarting itself or any
-- client.
--
-- The sequence is @stop@, wait for @finished@, then exec.  Overlapping the two
-- is not allowed -- river answers the second connection with @unavailable@ --
-- so the handover has to be ordered this way.
--
-- @resume@ means what it does under X11: write the window state out so that
-- the successor picks it up.  What is serialised is keyed on
-- @river_window_v1.identifier@ rather than on object ids, which are
-- per-connection; see 'StateFile'.

restart :: String -> Bool -> X ()
restart cmd resume = do
    ref <- asks (riverRestart . riverState)
    broadcastMessage ReleaseResources
    when resume writeStateToFile
    io (writeIORef ref (Just (cmd, [])))
    -- Queued for the event loop, which owns the connection.  Ordering holds:
    -- this action finishes before the loop's next pass, so the state file is
    -- written before river is asked to let go.
    emitNow OpStop



-- | An alias for a (width, height) pair
type D = (Dimension, Dimension)

-- | Detect whether a window has fixed size or is transient. This check
-- can be used to determine whether the window should be floating or not
--
-- The 'Display' is accepted and unused: there is only one, 'XConf' already has
-- it, and what river reports about a window is accumulated there rather than
-- queried over the wire.  Keeping the parameter costs nothing and lets a call
-- site written for X11 compile unchanged.
--
-- Fixed size is @dimensions_hint@ reporting an equal minimum and maximum;
-- transient is @river_window_v1.parent@, which is @xdg_toplevel.set_parent@ --
-- the faithful translation of X11's @WM_TRANSIENT_FOR@.

-- | Where a window should go when it becomes floating.
--
-- Upstream asks the server: @getWindowAttributes@ says where the window is,
-- and the float leaves it there.  There is no server here, so the answer comes
-- from 'riverPlacements' -- what the last layout run decided, and what both
-- 'XMonad.River.windowRect' and the published @getWindowAttributes@ report.
-- That is this port's stand-in for server geometry, and reading it back is
-- what makes floating a window keep it where it is instead of moving it.
--
-- Answering without consulting it is not a small inaccuracy.  The rectangle
-- returned here is the /only/ durable record of a float's geometry:
-- 'XMonad.River.WM.applyLayout' recomputes every placement from
-- 'XMonad.StackSet.floating' on each manage sequence, and the render sequence
-- restates node positions from that.  So a 'floatLocation' that disregards
-- where a window is does not merely misplace it once -- it makes every mouse
-- drag undo itself, because each motion step calls 'float' and has its work
-- thrown away.
--
-- A window with no placement has no position to keep, which is the case a
-- @doFloat@ manage hook hits: a window being managed for the first time has
-- been through no layout run.  It is centred on the current screen at the size
-- river reports, which is what upstream does when it likewise has nothing
-- usable to go on.
floatLocation :: Window -> X (ScreenId, W.RationalRect)
floatLocation w = do
    ws <- gets windowset
    placements <- io . readIORef =<< asks (riverPlacements . riverState)
    case lookup w placements of
        Just r -> do
            -- The screen the window is on, not the focused one.  Upstream
            -- takes the same care, and for the same reason: floating a window
            -- that lives on another screen must not haul it onto this one.
            msc <- pointScreen (rect_x r) (rect_y r)
            let sc = fromMaybe (W.current ws) msc
            pure (W.screen sc, relativeRect (screenRect (W.screenDetail sc)) r)
        Nothing -> do
            known <- io . readIORef =<< asks (riverWindows . riverState)
            let sc = W.current ws
                sr = screenRect (W.screenDetail sc)
                sw = max 1 (fromIntegral (rect_width sr))
                sh = max 1 (fromIntegral (rect_height sr))
            pure $ case M.lookup w known of
                Nothing -> (W.screen sc, W.RationalRect 0 0 1 1)
                Just rw ->
                    let (width, height) = sizeOf rw
                        rwidth  = fromIntegral width  % sw
                        rheight = fromIntegral height % sh
                    in ( W.screen sc
                       , W.RationalRect (0.5 - rwidth / 2) (0.5 - rheight / 2)
                                        rwidth rheight )

-- | The best size available for a window that has never been laid out.
--
-- A window river has not yet sent a @dimensions@ event for reports @0 x 0@,
-- and that is the ordinary case here: the protocol does not display a new
-- window until the window manager has proposed dimensions for it, so a
-- @doFloat@ manage hook always runs before any real size exists.  Clamping
-- that to @1 x 1@, as this used to, proposed a one-pixel window -- which a
-- GTK dialog with a 353x225 minimum can never take, leaving its recorded
-- rectangle permanently disagreeing with the window on screen.
--
-- The minimum from @dimensions_hint@ is the honest answer when there is one:
-- it arrives before the first manage sequence, and a window asked for its
-- minimum takes it exactly.  Failing that, zero, which river documents as
-- "the window will be allowed to decide its own dimensions" and is a more
-- truthful statement of not knowing than any invented number.
sizeOf :: RiverWindow -> (Dimension, Dimension)
sizeOf rw = case rwDimensions rw of
    (width, height) | width > 0 && height > 0 ->
        (fromIntegral width, fromIntegral height)
    _ -> fromMaybe (0, 0) (sh_min_size (rwSizeHints rw))

-- | A rectangle as a fraction of a screen, which is how the 'WindowSet'
-- records a float.
--
-- 'scaleRationalRect' is the inverse, exactly: it floors an exact rational, so
-- a rectangle that came out of a layout run survives the round trip unchanged.
-- That matters more than it looks -- it is why floating an already-placed
-- window is a no-op on screen rather than a one-pixel twitch per drag step.
relativeRect :: Rectangle -> Rectangle -> W.RationalRect
relativeRect sr r = W.RationalRect
    (fromIntegral (rect_x r - rect_x sr) % sw)
    (fromIntegral (rect_y r - rect_y sr) % sh)
    (fromIntegral (rect_width r)  % sw)
    (fromIntegral (rect_height r) % sh)
  where
    sw = max 1 (fromIntegral (rect_width sr))
    sh = max 1 (fromIntegral (rect_height sr))

-- | Make a tiled window floating, using its suggested rectangle

pointScreen :: Position -> Position
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
pointScreen x y = withWindowSet $ return . find p . W.screens
  where p = pointWithin x y . screenRect . W.screenDetail

-- | @pointWithin x y r@ returns 'True' if the @(x, y)@ co-ordinate is within
-- @r@.

pointWithin :: Position -> Position -> Rectangle -> Bool
pointWithin x y r = x >= rect_x r &&
                    x <  rect_x r + fromIntegral (rect_width r) &&
                    y >= rect_y r &&
                    y <  rect_y r + fromIntegral (rect_height r)

-- | Produce the actual rectangle from a screen and a ratio on that screen.

float :: Window -> X ()
float w = do
    (sc, rr) <- floatLocation w
    windows $ \ws -> W.float w rr . fromMaybe ws $ do
        i  <- W.findTag w ws
        guard $ i `elem` map (W.tag . W.workspace) (W.screens ws)
        f  <- W.peek ws
        sw <- W.lookupWorkspace sc ws
        return (W.focusWindow f . W.shiftWin sw w $ ws)


-- | Move the pointer, in river's global coordinate space.
--
-- Wayland forbids ordinary clients from warping the pointer, but the window
-- manager is not an ordinary client: @river_seat_v1.pointer_warp@ exists
-- precisely for this.

-- ---------------------------------------------------------------------
-- Mouse handling

mouseDrag :: (Position -> Position -> X ()) -> X () -> X ()
mouseDrag f done = do
    drag <- gets dragging
    case drag of
        Just _ -> return () -- already dragging
        Nothing -> do
            seats <- io . readIORef =<< asks (riverSeats . riverState)
            case M.elems seats of
                [] -> return ()
                (s:_) -> do
                    origin <- asks (riverDragOrigin . riverState)
                    io (writeIORef origin (rsPointer s))
                    emitOp (OpPointerOpStart (rsObject s))
                    modify $ \st -> st { dragging = Just (f, cleanup) }
  where
    cleanup = do
        modify $ \st -> st { dragging = Nothing }
        done

-- | Drag the window under the cursor with the mouse while it is dragged.

warpPointer :: Position -> Position -> X ()
warpPointer x y = do
    seats <- io . readIORef =<< asks (riverSeats . riverState)
    forM_ (M.keys seats) $ \seat -> emitOp (OpWarpPointer seat x y)

-- | Accumulate mouse motion events.
--
-- river drives interactive operations through the seat rather than by letting
-- the window manager grab the pointer: @op_start_pointer@ begins one,
-- @op_delta@ reports the total offset since it began, and @op_release@ fires
-- when the button goes up.  "XMonad.River.WM" routes those two events back
-- here, so the @(motion, cleanup)@ contract is the one xmonad has always had.
--
-- The offset is turned back into an absolute position by adding the pointer
-- position recorded when the operation started, so the callback still receives
-- root coordinates.

-- | Where the pointer was when the drag began.
--
-- Read from inside the motion callback and nowhere else.  'mouseDrag' is what
-- writes this, so a caller reading it beforehand -- as both drags below once
-- did -- gets the previous drag's origin, or @(0, 0)@ on the first drag of the
-- session.  The callback runs only on @op_delta@, which cannot arrive before
-- the operation it belongs to has started, so by then the value is the right
-- one.
dragOrigin :: X (Position, Position)
dragOrigin = io . readIORef =<< asks (riverDragOrigin . riverState)

-- | Put a window where a drag has decided it goes, and keep the recorded
-- geometry in step so that 'float' can read the result back.
--
-- The twin of 'XMonad.River.moveResizeWindow', which cannot be called from
-- here because "XMonad.River" imports this module.  It differs in one respect:
-- size hints are the caller's business, because both drags below have already
-- had to apply them to work out the rectangle they want.
dragWindowTo :: Window -> Rectangle -> X ()
dragWindowTo w r = do
    emitOp (OpSetPosition w (rect_x r) (rect_y r))
    emitOp (OpProposeDimensions w (rect_width r) (rect_height r))
    ref <- asks (riverPlacements . riverState)
    updatePlacement ref w r

mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = whenX (isClient w) $ do
    placements <- io . readIORef =<< asks (riverPlacements . riverState)
    -- The window's own origin is the thing the pointer's movement is added to.
    -- This used to add it to the /screen's/ origin, which meant a window
    -- jumped to wherever in the screen the pointer had travelled from the
    -- corner rather than following the pointer.
    forM_ (lookup w placements) $ \r ->
        mouseDrag
            (\ex ey -> do
                (ox, oy) <- dragOrigin
                dragWindowTo w r { rect_x = rect_x r + (ex - ox)
                                 , rect_y = rect_y r + (ey - oy) }
                float w)
            (float w)

-- | Resize the window under the cursor with the mouse while it is dragged.
--
-- The size follows the pointer's displacement from where the drag began, which
-- is a deliberate difference from upstream: X11 warped the pointer to the
-- window's bottom-right corner first and then treated the pointer as that
-- corner.  River can warp -- 'warpPointer' does -- but a drag that begins by
-- yanking the pointer somewhere else is worth avoiding when the alternative is
-- this, and the two agree once the drag is under way.
--
-- Sizes are clamped by 'applySizeHintsContents', which takes @max 1@ before
-- widening, so dragging back past the window's own origin bottoms out at one
-- pixel rather than wrapping around.
mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = whenX (isClient w) $ do
    placements <- io . readIORef =<< asks (riverPlacements . riverState)
    known <- io . readIORef =<< asks (riverWindows . riverState)
    forM_ ((,) <$> lookup w placements <*> M.lookup w known) $ \(r, rw) ->
        mouseDrag
            (\ex ey -> do
                (ox, oy) <- dragOrigin
                let (width, height) =
                        applySizeHintsContents (rwSizeHints rw)
                            ( fromIntegral (rect_width r)  + (ex - ox)
                            , fromIntegral (rect_height r) + (ey - oy) )
                dragWindowTo w r { rect_width = width, rect_height = height }
                float w)
            (float w)


-- | A type to help serialize xmonad's state to a file.
--
-- The window type is a river @identifier@ rather than a 'Window'.  That is the
-- whole reason this can exist at all: a river object id is per-connection and
-- recycled after @wl_display.delete_id@, so an id written by one window
-- manager names nothing to its successor.  @river_window_v1.identifier@ is a
-- string river promises is unique and never reused, and -- crucially -- it is
-- derived from the window's @ext_foreign_toplevel_handle_v1@, which belongs to
-- the window rather than to the window manager's connection.  river creates
-- one only if the window has none already, precisely so that it survives a
-- window manager restart.
data StateFile = StateFile
  { sfWins :: W.StackSet WorkspaceId String String ScreenId ScreenDetail
  , sfExt  :: [(String, String)]
  } deriving (Show, Read)

-- | Re-key a 'W.StackSet', dropping anything the function has no answer for.
--
-- Used in both directions: 'Window' to identifier when writing, identifier
-- back to 'Window' when reading.  Dropping is the normal case on the way back
-- in -- a window that was closed while the successor was starting, or one
-- opened by a client river has not re-advertised -- so a dropped focus has to
-- be replaced rather than lost.  The nearest survivor below it is preferred,
-- then the nearest above, which is what closing the focused window does.

-- ---------------------------------------------------------------------
-- Support for window size hints
--
-- The arithmetic below is exactly upstream's, unchanged, and that is the
-- point: it is pure, it already treats every hint as optional, and river's
-- 'SizeHints' simply has fewer of them populated.  A hint river cannot report
-- leaves the corresponding function as the identity, which is what it always
-- did for a window that declared no such hint under X11 either.

mkAdjust :: Window -> X (D -> D)
mkAdjust w = do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    bw <- asks (borderWidth . config)
    pure $ case M.lookup w known of
        Nothing -> id
        Just rw -> applySizeHints bw (rwSizeHints rw)

-- | Reduce the dimensions if needed to comply to the given SizeHints, taking
-- window borders into account.

applySizeHints :: Integral a => Dimension -> SizeHints -> (a, a) -> D
applySizeHints bw sh =
    tmap (+ 2 * bw) . applySizeHintsContents sh . tmap (subtract $ 2 * fromIntegral bw)
    where
    tmap f (x, y) = (f x, f y)

-- | Reduce the dimensions if needed to comply to the given SizeHints.

applySizeHintsContents :: Integral a => SizeHints -> (a, a) -> D
applySizeHintsContents sh (w, h) =
    applySizeHints' sh (fromIntegral $ max 1 w, fromIntegral $ max 1 h)

-- | Use size hints to scale a pair of dimensions.

applySizeHints' :: SizeHints -> D -> D
applySizeHints' sh =
      maybe id applyMaxSizeHint                   (sh_max_size   sh)
    . maybe id (\(bw, bh) (w, h) -> (w+bw, h+bh)) (sh_base_size  sh)
    . maybe id applyResizeIncHint                 (sh_resize_inc sh)
    . maybe id applyAspectHint                    (sh_aspect     sh)
    . maybe id (\(bw,bh) (w,h)   -> (w-bw, h-bh)) (sh_base_size  sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
--
-- Correct as written, and under river it never fires: Wayland has no aspect
-- ratio hint, so 'sh_aspect' is always 'Nothing'.

applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
    | w * maxy > h * maxx                         = (h * maxx `div` maxy, h)
    | w * miny < h * minx                         = (w, w * miny `div` minx)
    | otherwise                                   = x

-- | Reduce the dimensions so they are a multiple of the size increments.
--
-- As 'applyAspectHint': correct, and never fires under river.

applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw,ih) x@(w,h) =
    if iw > 0 && ih > 0 then (w - w `mod` iw, h - h `mod` ih) else x

-- | Reduce the dimensions if they exceed the given maximum dimensions.
--
-- This one does fire: @dimensions_hint@ carries a maximum.

applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw,mh) x@(w,h) =
    if mw > 0 && mh > 0 then (min w mw,min h mh) else x
