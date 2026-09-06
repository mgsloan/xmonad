{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | The river window management state machine, driving an xmonad 'WindowSet'.
--
-- river splits state into two disjoint categories, and the split shapes this
-- module:
--
-- * __Window management state__ — dimensions, keyboard focus, keyboard
--   bindings — may only be modified during a /manage sequence/, between the
--   @manage_start@ event and the @manage_finish@ request.
-- * __Rendering state__ — position, stacking order, borders, hide\/show — may
--   be modified during either sequence, but is only applied at
--   @render_finish@.
--
-- So layout runs during the manage sequence (it must, to propose dimensions),
-- its results are stashed, and positions are applied during the following
-- render sequence.
--
-- Deciding and transmitting are separate. Everything in the 'X' monad computes
-- a "XMonad.River.Plan" and touches the connection not at all; 'transmitManage'
-- and 'transmitRender' take that value and send it, filtered against the
-- windows river still has. That split is what the thread split in @DESIGN.md@
-- is built on -- the thing that decides and the thing that owns the connection
-- are on their way to being different threads -- and it is worth keeping even
-- before then: a plan can be re-sent, so a sequence the window manager has
-- nothing new for is answered by restating the last one.
--
-- Bindings fire outside any sequence. Their actions are queued and run at the
-- start of the next manage sequence, which river is asked to schedule with
-- @manage_dirty@. This is the same deferral river's own reference window
-- manager uses.
module XMonad.River.WM
  ( riverMain
  , applyLayout
  , queueAction
  ) where

import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid (All(..), appEndo)
import Data.Int (Int32)
import Data.Word (Word32)
import Control.Concurrent (Chan, MVar, forkIO, killThread, newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, tryPutMVar, writeChan)
import Control.Exception (SomeException, catch, handle)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Process (executeFile)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import XMonad.Core
import XMonad.Operations (StateFile (..), broadcastMessage, floatLocation, focus, isFixedSizeOrTransient, readStateFile, scaleRationalRect, writeStateToFile)
import XMonad.River.Runtime (emitOp, setModifierWatcher, takeNowOps, takeOps, RestartRequested(..), forgetBorderOverride, takeModifierWatcher, lookupBorderOverride, publishGeometry, publishSizeHints, sendRestart, setMainThread, warnUnimplemented)
import qualified XMonad.River.Control as Ctl
import XMonad.River.Client (closeAllClients)
import XMonad.River.Connection
import XMonad.River.Keyboard (riverModifiers)
import qualified XMonad.River.Mailbox as MB
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.Core
import XMonad.River.Protocol.LayerShell
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Wire (ObjectId, isNullObject)
import XMonad.River.Types
import XMonad.River.Plan
import XMonad.River.State (InputCapture(..), RiverState(..), updatePlacement)
import XMonad.River.Trace (initTrace, showPlacements, traceLine)
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Mutable plumbing shared with the event callbacks

-- | State that the Wayland event callbacks write and the manage sequence
-- reads. Kept outside 'XState' because callbacks run in 'IO', below the 'X'
-- monad.
data Runtime = Runtime
  { rtPending     :: !(IORef [X ()])
    -- ^ Binding actions awaiting the next manage sequence, newest first.
  , rtBindings    :: !(IORef (M.Map ObjectId (X ())))
    -- ^ The same 'IORef' as 'riverKeyBindings' in the 'XConf'.  A submap
    -- disables every one of these while it is open, so it needs to reach them
    -- from the 'X' monad; the callbacks here reach them from 'IO'.
  , rtSubmap      :: !(IORef (Maybe (InputCapture X)))
    -- ^ The same 'IORef' as 'riverCapture'.  Written by the config; taken by
    -- whichever of a key, an unbound key, a modifier release or the deadline
    -- gets there first.
  , rtGrabbed     :: !(IORef [ObjectId])
    -- ^ The standing bindings 'XMonad.River.grabKeys' asked for.  Loop state
    -- for the same reason 'rtArmed' is.
  , rtExtraKeys   :: !(IORef [(X (), X ())])
    -- ^ The same 'IORef' as 'riverExtraKeys': what each of those runs, indexed
    -- the way the loop bound them.
  , rtArmed       :: !(IORef [ObjectId])
    -- ^ The temporary bindings an open submap installed, so that tearing it
    -- down can destroy them.  Loop state: arming and disarming are protocol
    -- work, and only the loop may do protocol work.
  , rtDisarm      :: !(IORef Bool)
    -- ^ Set when a submap has ended and its bindings are still installed.
    -- Acted on at the start of the next manage sequence, because @enable@ and
    -- @disable@ are legal nowhere else -- and a submap always ends outside
    -- one, on a key press.
  , rtXkbVersion  :: !Word32
    -- ^ Negotiated @river_xkb_bindings_v1@ version.  @get_seat@ and
    -- @ensure_next_key_eaten@ arrived in 2.
  , rtPointerBind :: !(IORef (M.Map ObjectId (Window -> X ())))
  , rtPlan        :: !(IORef Plan)
    -- ^ What the last manage sequence decided.  Computed in the 'X' monad and
    -- transmitted from 'IO', which is the separation @DESIGN.md@ is built on:
    -- the thing that decides and the thing that holds the connection are on
    -- their way to being different threads.
  , rtWindows     :: !(IORef (M.Map Window RiverWindow))
    -- ^ The same 'IORef' as 'riverWindows'.  Transmitting a plan has to filter
    -- it against the windows river still has, and does so from 'IO'.
  , rtSeats       :: !(IORef (M.Map ObjectId RiverSeat))
    -- ^ Likewise 'riverSeats', for the focus half of a plan.
  , rtOutputs     :: !(IORef (M.Map ObjectId RiverOutput))
    -- ^ Likewise 'riverOutputs'.
  , rtBindingsGlobal :: !ObjectId
    -- ^ The @river_xkb_bindings_v1@ global.  Held here so that installing a
    -- seat's listeners needs no 'XConf', which is what lets that happen on the
    -- event loop rather than in the 'X' monad.
  , rtBoundSeats  :: !(IORef (S.Set ObjectId))
  , rtHovered     :: !(IORef (Maybe Window))
  , rtManager     :: !ObjectId
    -- ^ Needed by binding callbacks, which run outside the 'X' monad and must
    -- request a manage sequence with @manage_dirty@.
  , rtConn        :: !Connection
    -- ^ Likewise: 'queueAction' has to ask for the sequence it queued into.
  , rtFollowsMouse :: !Bool
    -- ^ The config's 'focusFollowsMouse', copied here because @pointer_enter@
    -- is handled in 'IO' with no 'XConf' to consult -- and consulting it from
    -- a queued action would be too late: queueing at all asks river for a
    -- manage sequence, so a config with this off would pay for one on every
    -- pointer crossing.  Safe to snapshot: it cannot change without a restart.
  , rtStartupDone :: !(IORef Bool)
  , rtRestored    :: !(IORef Bool)
    -- ^ Whether the state file left by a previous window manager has been read
    -- back yet.  Only the first manage sequence may do it; see 'restoreState'.
  , rtLayerShell  :: !(Maybe ObjectId)
    -- ^ The @river_layer_shell_v1@ global, when the compositor offers it.
    -- Binding it is what tells river to let clients map layer surfaces at all;
    -- without it river closes every one on sight.
  , rtLayerDefault :: !(IORef (Maybe ObjectId))
    -- ^ The output most recently nominated as the default for layer surfaces
    -- that do not name one. Held so the request is only reissued when the
    -- choice actually changes.
  }

--------------------------------------------------------------------------------
-- Entry point

-- | Connect to river and run the window manager. Does not return.
riverMain :: XConfig Layout -> Directories -> IO ()
riverMain userConfig dirs = do
  initTrace (cfgDir dirs)
  conn <- connect
  (registry, globals) <- getRegistry conn
  mManager <- bindGlobal conn registry globals
                riverWindowManagerV1Interface 4 riverWindowManagerV1Version
  mBindings <- bindGlobal conn registry globals
                 riverXkbBindingsV1Interface 1 riverXkbBindingsV1Version
  -- Optional, and river only advertises it alongside the window manager
  -- global. Binding it is not merely how we learn about exclusive zones: it is
  -- the signal that this window manager supports layer shell, without which
  -- river refuses to map layer surfaces. Everything that draws outside the
  -- window layout depends on it — fuzzel prompts, notification daemons,
  -- wallpaper setters, bars and lock screens.
  mLayerShell <- bindGlobal conn registry globals
                   riverLayerShellV1Interface 1 riverLayerShellV1Version
  -- Surfaces the window manager draws itself -- prompts, decorations -- need
  -- these two.  Both are core Wayland rather than river extensions, so a
  -- compositor without them would be broken in a way worth reporting rather
  -- than working around.
  mCompositor <- bindGlobal conn registry globals
                   wlCompositorInterface 4 wlCompositorVersion
  mShm <- bindGlobal conn registry globals wlShmInterface 1 wlShmVersion

  case (mManager, mBindings) of
    (Just (manager, _), Just (bindings, bindingsVer)) -> do
      when (mLayerShell == Nothing) $ hPutStrLn stderr
        "xmonad-river: river_layer_shell_v1 is unavailable; layer surfaces \
        \(fuzzel prompts, notifications, wallpaper, bars) will not be shown"
      when (mCompositor == Nothing || mShm == Nothing) $ hPutStrLn stderr
        "xmonad-river: wl_compositor or wl_shm is unavailable; anything the \
        \window manager draws itself (prompts, decorations) will not appear"
      when (bindingsVer < 2) $ hPutStrLn stderr
        "xmonad-river: river_xkb_bindings_v1 is version 1; submaps cannot \
        \detect an unbound key and will wait for one of their own"
      run conn manager bindings bindingsVer (fmap fst mLayerShell)
          (fmap fst mCompositor) (fmap fst mShm) userConfig dirs
    _ -> do
      hPutStrLn stderr
        "xmonad-river: river_window_manager_v1 (>= 4) or \
        \river_xkb_bindings_v1 not supported by the compositor"
      exitFailure

run :: Connection -> ObjectId -> ObjectId -> Word32 -> Maybe ObjectId
    -> Maybe ObjectId -> Maybe ObjectId -> XConfig Layout
    -> Directories -> IO ()
run conn manager bindings bindingsVer layerShell compositor shm userConfig dirs = do
  windowsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  seatsRef   <- newIORef M.empty
  dirtyRef   <- newIORef False
  manageRef  <- newIORef False
  restartRef <- newIORef Nothing
  dragOrigin <- newIORef (0, 0)
  mailbox    <- MB.newMailbox
  -- One IORef each, shared between the XConf and the Runtime: the event loop's
  -- callbacks reach them from IO, and a submap reaches them from X.
  bindingsRef <- newIORef M.empty
  submapRef   <- newIORef Nothing
  placeRef    <- newIORef []
  restackRef  <- newIORef []
  extraKeysRef <- newIORef []
  afterLayoutRef <- newIORef []
  rt <- Runtime
          <$> newIORef []
          <*> pure bindingsRef
          <*> pure submapRef
          <*> newIORef []
          <*> pure extraKeysRef
          <*> newIORef []
          <*> newIORef False
          <*> pure bindingsVer
          <*> newIORef M.empty
          <*> newIORef emptyPlan
          <*> pure windowsRef
          <*> pure seatsRef
          <*> pure outputsRef
          <*> pure bindings
          <*> newIORef S.empty
          <*> newIORef Nothing
          <*> pure manager
          <*> pure conn
          <*> pure (focusFollowsMouse userConfig)
          <*> newIORef False
          <*> newIORef False
          <*> pure layerShell
          <*> newIORef Nothing

  when (null (workspaces userConfig)) $ hPutStrLn stderr
    "xmonad-river: the config has no workspaces; using a single one named \"1\""

  -- Already existentially wrapped: XMonad.xmonad does that before calling
  -- here, so that every workspace can hold a different layout.
  let layout = layoutHook userConfig
      -- One workspace per name, and a single screen to begin with. Screens are
      -- reconciled against river's outputs at the start of every manage
      -- sequence, so the placeholder here is replaced before anything is laid
      -- out.
      -- A StackSet must have a current screen, so there must be at least one
      -- workspace.  A config with `workspaces = []` is a config error rather
      -- than something to crash on later with a head of an empty list, so it
      -- gets one unnamed workspace and a diagnostic.
      initialWorkspaces = case workspaces userConfig of
        []    -> [ W.Workspace "1" layout Nothing ]
        names -> [ W.Workspace i layout Nothing | i <- names ]
      (firstWorkspace, otherWorkspaces) = case initialWorkspaces of
        (w:ws) -> (w, ws)
        []     -> error "unreachable: initialWorkspaces is never empty"
      placeholder = W.Screen firstWorkspace 0 (SD (Rectangle 0 0 0 0))
      -- The fourth field is the floating window map, empty at startup.
      initialSet = W.StackSet placeholder [] otherWorkspaces M.empty

      xconf = XConf
        { config = userConfig
        , display = conn
        , riverState = RiverState
            { riverManager = manager
            , riverBindings = bindings
            , riverCompositor = compositor
            , riverShm = shm
            , riverWindows = windowsRef
            , riverOutputs = outputsRef
            , riverSeats = seatsRef
            , riverDirty = dirtyRef
            , inManageSeq = manageRef
            , riverRestart = restartRef
            , riverMailbox = mailbox
            , riverKeyBindings = bindingsRef
            , riverPlacements = placeRef
            , riverExtraKeys = extraKeysRef
            , riverRestack = restackRef
            , riverCapture = submapRef
            , riverDragOrigin = dragOrigin
            , riverAfterLayout = afterLayoutRef
            }
        , normalBorder = parseColor (normalBorderColor userConfig)
        , focusedBorder = parseColor (focusedBorderColor userConfig)
        , keyActions = keys userConfig userConfig
        , buttonActions = mouseBindings userConfig userConfig
        , mouseFocused = False
        , mousePosition = Nothing
        , currentEvent = Nothing
        , directories = dirs
        }
      xstate = XState
        { windowset = initialSet
        , mapped = S.empty
        , waitingUnmap = M.empty
        , dragging = Nothing
        , extensibleState = M.empty
        , numberlockMask = 0
        }

  stateRef <- newIORef xstate
  let runX' :: X a -> IO a
      runX' act = do
        st <- readIORef stateRef
        (a, st') <- runX xconf st act
        writeIORef stateRef st'
        pure a

  -- The worker thread.  It owns 'XState' and runs every scrap of user code --
  -- binding actions, manage hooks, layouts, window adoption -- serialized, in
  -- the order things were asked for, which is the order upstream runs them in.
  -- The event loop below owns the connection and runs none of it, so a config
  -- action that blocks stops window management and leaves the compositor
  -- answered.  See @DESIGN.md@.
  work <- newChan
  tick <- newEmptyMVar
  let submit :: X () -> IO ()
      submit = writeChan work
  workerTid <- forkIO $ forever $ do
    act <- readChan work
    -- Per action, so that one that throws costs its own effect and not the
    -- window manager.  A worker that died under a live loop would leave
    -- something that still answers river and responds to nothing, which looks
    -- alive -- worse than a crash.
    runX' act `catch` \e -> hPutStrLn stderr
      ("xmonad-river: worker: " ++ show (e :: SomeException))
    -- Non-blocking: the loop may not be waiting, and a worker that stalled
    -- because nobody collected a tick would be the bug this exists to avoid.
    void (tryPutMVar tick ())

  riverWindowManagerV1Listen conn manager $
    onManagerEvent conn manager restartRef rt submit tick

  riverWindowManagerV1ManageDirty conn manager

  setMainThread
  -- The way in for a restart request that does not come from a keybinding.
  -- This is what @xmonad --restart@ sends, and is the only handle a script --
  -- or a test -- has on a running window manager, since river offers nothing
  -- between one window manager process and another.  Set up after
  -- 'setMainThread', because that is what reaching the event loop needs.
  ctlPath <- Ctl.controlSocketPath
  case ctlPath of
    Left err -> hPutStrLn stderr
      ("xmonad-river: no control socket (" ++ err ++ "); \
       \`xmonad --restart` will not work")
    Right path -> do
      served <- Ctl.serveControl path $ \case
        Ctl.Restart -> Ctl.awaitRestart sendRestart
      case served of
        Left err -> hPutStrLn stderr
          ("xmonad-river: could not listen on " ++ path ++ ": " ++ err)
        Right _  -> pure ()
  -- The startup hook is deliberately *not* run here. river holds a watchdog
  -- over the manage sequence, and this config's startup hook spawns upwards of
  -- a dozen processes; doing that before the event loop starts means river
  -- waits on a manage_finish that cannot come, and logs
  -- "timeout occurred, some imperfect frames may be shown". It runs at the end
  -- of the first manage sequence instead, once manage_finish is already on its
  -- way. See 'runStartupHook'.
  -- The loop waits on the compositor socket *and* the mailbox, so an action
  -- posted from a timer thread is noticed now rather than whenever the next
  -- unrelated Wayland event happens along -- which on an idle desktop could be
  -- minutes.  Anything drained is queued for the next manage sequence and one
  -- is requested, because river permits window management state to change
  -- nowhere else.
  let loop = do
        -- Anything that asked for a manage sequence since the last pass.
        -- 'XMonad.Operations.requestManageSequence' and
        -- 'XMonad.River.manageDirty' only set a flag, because they run
        -- wherever an X action does and the connection is the loop's.
        wantsSeq <- atomicModifyIORef' dirtyRef (\d -> (False, d))
        when wantsSeq $ riverWindowManagerV1ManageDirty conn manager
        -- Requests that need no sequence and must not wait for one: ending the
        -- session, or asking river to release this window manager.  Waiting
        -- for a sequence would mean waiting for something nothing will ask for.
        nowOps <- takeNowOps
        seatsNow <- readIORef (rtSeats rt)
        forM_ nowOps $ \case
          OpExitSession -> riverWindowManagerV1ExitSession conn manager
          OpStop -> riverWindowManagerV1Stop conn manager
          OpSetXcursorTheme s name size -> when (M.member s seatsNow) $
            riverSeatV1SetXcursorTheme conn s name size
          _ -> pure ()
        -- Flush before waiting, or a request queued but not yet written --
        -- the manage_dirty just above, on the very first pass -- never
        -- reaches the compositor, and the wait blocks for a reply to
        -- something that was never sent.  The loop this replaced could not
        -- get this wrong: dispatch flushes before it reads.
        flush conn
        sockFd <- connectionFd conn
        ready <- MB.waitEither sockFd (MB.mailboxFd mailbox)
        case ready of
          Left () -> dispatch conn
          Right () -> do
            MB.clearWakeups mailbox
            acts <- MB.drain mailbox
            mapM_ (queueAction rt) acts
            flush conn
        loop
  -- A restart request arrives as an async exception thrown into this thread,
  -- which interrupts the blocking read. Ask river to release us, then keep
  -- dispatching: the 'finished' event does the exec.
  let restartGraceMicros = 2 * 1000 * 1000
  loop `catch` \RestartRequested -> do
    mExe <- restartTarget
    case mExe of
      -- Nothing to come back as.  Say so and carry on rather than stopping:
      -- once river has released this window manager there is no way back, and
      -- a session with an out-of-date window manager beats a session with
      -- none.
      Nothing -> do
        let msg = "the executable is gone; still running the old one"
        -- Whoever asked hears the refusal.  Before the control socket this
        -- reached only the session log, so `xmonad --restart` looked like it
        -- had worked and the old window manager simply stayed.
        Ctl.answerRestart (Ctl.Refused msg)
        hPutStrLn stderr ("xmonad-river: refusing to restart, " ++ msg)
        loop
      Just exe -> do
        args <- getArgs
        -- The same two things 'XMonad.Operations.restart' does before asking
        -- river to stop.  This path exists because 'sendRestart' is callable
        -- from any thread and so cannot run 'X' code itself; it must not
        -- therefore be a restart that quietly loses state.
        -- The same two things 'XMonad.Operations.restart' does, but on the
        -- worker, because that is what owns 'XState' now.  Bounded: if the
        -- action being escaped is the one that is stuck, waiting for it would
        -- make the escape hatch as stuck as the thing it is escaping.
        done <- newEmptyMVar
        submit (broadcastMessage ReleaseResources >> writeStateToFile
                  >> io (putMVar done ()))
        yielded <- timeout restartGraceMicros (takeMVar done)
        when (isNothing yielded) $ do
          hPutStrLn stderr
            "xmonad-river: the worker did not yield; restarting from the last \
            \committed state"
          -- Killed rather than merely interrupted, so that nothing races the
          -- loop for 'XState' while it writes the file below.
          killThread workerTid
          runX' writeStateToFile
        writeIORef restartRef (Just (exe, args))
        -- Answered before the stop rather than after, because after it there
        -- is an exec and no thread left to answer with.
        Ctl.answerRestart Ctl.Ok
        riverWindowManagerV1Stop conn manager
        loop

-- | What to exec on restart, or 'Nothing' if there is nothing to exec.
--
-- 'getExecutablePath' reads @\/proc\/self\/exe@, and Linux appends
-- @\" (deleted)\"@ to that link once the binary has been replaced -- which is
-- exactly what installing a new build does, since it links a fresh inode over
-- the old name.  Passing the literal string through means @sh@ looks for a
-- file whose name ends in @\" (deleted)\"@, does not find it, and the restart
-- dies /after/ river has already released the window manager: the session is
-- left with no window manager at all, no keybindings, and no way to ask for
-- one.
--
-- Stripping the suffix gives the path the new build was installed to, which is
-- what should be exec'd.  Checked for existence first, because the whole point
-- is not to tear down a working session for a successor that is not there.
restartTarget :: IO (Maybe FilePath)
restartTarget = do
  raw <- getExecutablePath
  let suffix = " (deleted)"
      path | suffix `isSuffixOf` raw = take (length raw - length suffix) raw
           | otherwise = raw
  ok <- doesFileExist path
  pure (if ok then Just path else Nothing)

--------------------------------------------------------------------------------
-- Manager events

onManagerEvent
  :: Connection -> ObjectId -> IORef (Maybe (FilePath, [String])) -> Runtime
  -> (X () -> IO ()) -> MVar ()
  -> RiverWindowManagerV1Event -> IO ()
onManagerEvent conn manager restartRef rt submit tick = \case
  RiverWindowManagerV1Unavailable -> do
    hPutStrLn stderr "xmonad-river: another window manager is already running"
    exitFailure
  -- river has confirmed this window manager is no longer active, so a
  -- successor may now connect. Exec it in place, which is what makes M-q a
  -- restart rather than a logout.
  RiverWindowManagerV1Finished -> readIORef restartRef >>= \case
    Nothing  -> exitSuccess
    -- Straight to the successor, with no shell in between.  @sh -c@ execs
    -- only in the narrowest cases and otherwise forks and waits, so it does
    -- not disappear the way an exec'd process does -- and since each restart
    -- would inherit the last one's shell, they nested: four M-q presses, four
    -- idle dash processes, one inside the next.
    Just (prog, as) -> executeFile prog True as Nothing
  RiverWindowManagerV1ManageStart -> do
    before <- planSerial <$> readIORef (rtPlan rt)
    submit (manageSequence rt)
    -- Bounded.  An action that finishes in time has its result in this
    -- sequence, exactly as when the loop ran it itself; one that overruns
    -- leaves the sequence to be answered with the plan already in hand, which
    -- is valid and cheap.  What is never allowed is waiting for user code.
    landed <- awaitPlan rt tick before
    -- Only when the sequence actually ran: 'reapClosed' has to have seen the
    -- entries this deletes, or a closed window is dropped from the map while
    -- the WindowSet still holds it.
    when landed (reapObjects rt conn)
    transmitManage rt conn
    riverWindowManagerV1ManageFinish conn manager
    -- Deliver manage_finish before running anything slow. Requests are only
    -- buffered until the event loop flushes, and the startup hook spawns
    -- enough processes to trip river's watchdog in the meantime.
    flush conn
    submit (runStartupHook rt)
  RiverWindowManagerV1RenderStart -> do
    transmitRender rt conn
    riverWindowManagerV1RenderFinish conn manager
  -- The bookkeeping happens on the loop; only the hook a config might have
  -- installed is user code, and that is all that goes to the worker.
  RiverWindowManagerV1Window win -> addWindow rt conn win
  RiverWindowManagerV1Output out -> do
    addOutput rt conn out
    submit (void (broadcastEvent (OutputAdded out)))
  RiverWindowManagerV1Seat seat  -> do
    addSeat rt conn seat
    submit (void (broadcastEvent (SeatAdded seat)))
  RiverWindowManagerV1SessionLocked   -> submit (void (broadcastEvent SessionLocked))
  RiverWindowManagerV1SessionUnlocked -> submit (void (broadcastEvent SessionUnlocked))
  _ -> pure ()

-- | Wait, briefly, for the worker to publish a plan newer than the one in
-- hand.
--
-- 'True' if it did.  The bound is what makes the split safe: the loop must
-- answer river whatever the worker is doing, and river holds every input event
-- for the seat until it does.  A plan that says it must land in its own
-- sequence -- arming a submap, which has to be atomic with the key press that
-- opened it -- is worth waiting longer for, but not indefinitely.
awaitPlan :: Runtime -> MVar () -> Int -> IO Bool
awaitPlan rt tick before = go planGraceMicros
  where
    go budget
      | budget <= 0 = pure False
      | otherwise = do
          now <- readIORef (rtPlan rt)
          if planSerial now > before
            then pure True
            else do
              let step = min budget planPollMicros
              _ <- timeout step (takeMVar tick)
              go (budget - step)

-- | How long the loop will wait for a plan before answering with the one it
-- has.  Long enough that anything not doing I/O lands in its own sequence.
planGraceMicros :: Int
planGraceMicros = 8 * 1000

-- | How long a single wait for a tick lasts, so that a tick arriving for some
-- other action does not spin.
planPollMicros :: Int
planPollMicros = 2 * 1000

broadcastEvent :: Event -> X All
broadcastEvent ev = do
  hook <- asks (handleEventHook . config)
  userCodeDef (All True) (hook ev)

--------------------------------------------------------------------------------
-- Object tracking

-- | Take note of a window river has just told us about.
--
-- Runs on the event loop, in 'IO', and touches no 'XState': everything it does
-- is a protocol request or a write to a map the loop owns.  Keeping it out of
-- the 'X' monad is what allows the loop to keep answering river while user code
-- runs elsewhere -- see @DESIGN.md@.
addWindow :: Runtime -> Connection -> ObjectId -> IO ()
addWindow rt conn win = do
  node <- riverWindowV1GetNode conn win
  let ref = rtWindows rt
  modifyIORef' ref $ M.insert win RiverWindow
    { rwObject = win, rwNode = node
    , rwAppId = Nothing, rwTitle = Nothing, rwPid = Nothing
    , rwIdentifier = Nothing, rwParent = Nothing
    , rwDimensions = (0, 0)
    , rwProposed = Nothing
    , rwSizeHints = noSizeHints
    , rwNew = True, rwClosed = False, rwFullscreen = False, rwHidden = False
    }
  riverWindowV1Listen conn win $ \case
    RiverWindowV1Closed        -> adjust ref win $ \w -> w { rwClosed = True }
    RiverWindowV1AppId a       -> adjust ref win $ \w -> w { rwAppId = a }
    RiverWindowV1Title t       -> adjust ref win $ \w -> w { rwTitle = t }
    RiverWindowV1UnreliablePid p -> adjust ref win $ \w -> w { rwPid = Just p }
    RiverWindowV1Identifier i  -> adjust ref win $ \w -> w { rwIdentifier = Just i }
    RiverWindowV1Parent p      -> adjust ref win $ \w ->
      w { rwParent = if isNullObject p then Nothing else Just p }
    RiverWindowV1Dimensions width height ->
      adjust ref win $ \w -> w { rwDimensions = (width, height) }
    -- A bound of zero or less means the window did not state one.
    RiverWindowV1DimensionsHint minW minH maxW maxH ->
      adjust ref win $ \w -> w { rwSizeHints = SizeHints
        { sh_min_size   = sizeBound minW minH
        , sh_max_size   = sizeBound maxW maxH
        , sh_resize_inc = Nothing
        , sh_aspect     = Nothing
        , sh_base_size  = Nothing
        } }
    -- Both are followed by a manage_start, so a manage hook asking
    -- 'XMonad.Hooks.ManageHelpers.isFullscreen' sees the up-to-date answer.
    --
    -- The flag is set before the event is queued, and that order is the point:
    -- a handler reading 'isFullscreen' for the window it was just told about
    -- must not see the old answer.  A manage hook covers a window that is
    -- fullscreen when it first appears; this covers one that changes later,
    -- which is every video player and every browser.
    RiverWindowV1FullscreenRequested _ -> do
      adjust ref win $ \w -> w { rwFullscreen = True }
      queueAction rt $ void $ broadcastEvent (WindowFullscreenChanged win True)
    RiverWindowV1ExitFullscreenRequested -> do
      adjust ref win $ \w -> w { rwFullscreen = False }
      queueAction rt $ void $ broadcastEvent (WindowFullscreenChanged win False)
    RiverWindowV1PointerMoveRequested _ -> pure ()
    _ -> pure ()

adjust :: IORef (M.Map ObjectId a) -> ObjectId -> (a -> a) -> IO ()
adjust ref k f = modifyIORef' ref (M.adjust f k)

-- | Likewise for an output.  The event a config might hook is /not/ broadcast
-- here: that is user code, and its caller runs it separately.
addOutput :: Runtime -> Connection -> ObjectId -> IO ()
addOutput rt conn out = do
  let ref = rtOutputs rt

  mLayer <- forM (rtLayerShell rt) $ \shell -> do
    lo <- riverLayerShellV1GetOutput conn shell out
    riverLayerShellOutputV1Listen conn lo $ \case
      RiverLayerShellOutputV1NonExclusiveArea x y width height ->
        adjust ref out $ \o -> o { roLayerArea = Just (Rectangle x y
          (fromIntegral width) (fromIntegral height)) }
      _ -> pure ()
    pure lo

  modifyIORef' ref $ M.insert out RiverOutput
    { roObject = out, roPosition = (0, 0), roSize = (0, 0), roRemoved = False
    , roLayerObject = mLayer, roLayerArea = Nothing }

  riverOutputV1Listen conn out $ \case
    RiverOutputV1Removed -> adjust ref out $ \o -> o { roRemoved = True }
    RiverOutputV1Position x y -> adjust ref out $ \o -> o { roPosition = (x, y) }
    RiverOutputV1Dimensions width height ->
      adjust ref out $ \o -> o { roSize = (width, height) }
    _ -> pure ()

-- | Likewise for a seat.  As with 'addOutput', the broadcast is the caller's.
addSeat :: Runtime -> Connection -> ObjectId -> IO ()
addSeat rt conn seat = do
  let ref = rtSeats rt

  mLayer <- forM (rtLayerShell rt) $ \shell -> do
    ls <- riverLayerShellV1GetSeat conn shell seat
    riverLayerShellSeatV1Listen conn ls $ \ev -> do
      let set f = adjust ref seat $ \s -> s { rsLayerFocus = f }
      case ev of
        RiverLayerShellSeatV1FocusExclusive    -> set LayerFocusExclusive
        RiverLayerShellSeatV1FocusNonExclusive -> set LayerFocusNonExclusive
        RiverLayerShellSeatV1FocusNone         -> set LayerFocusNone
        _ -> pure ()
    pure ls

  -- The object a submap requests @ensure_next_key_eaten@ on.  Created once per
  -- seat, because doing it twice is a protocol error, and only when river
  -- offers version 2 or better -- the request does not exist before that.
  let bindingsGlobal = rtBindingsGlobal rt
  mXkbSeat <- if rtXkbVersion rt < 2 then pure Nothing else do
    xs <- riverXkbBindingsV1GetSeat conn bindingsGlobal seat
    riverXkbBindingsSeatV1Listen conn xs $ \case
      -- A key the open submap did not ask for.  Under X11 this arrived on the
      -- keyboard grab and the submap simply did not match it; here the only
      -- way to hear about it at all is to have asked for the key to be eaten.
      -- Acting on it matters more than it sounds: a submap disables every one
      -- of the window manager's bindings while it is open, so a submap that
      -- could not notice an unknown key would stay armed and leave the session
      -- with no working shortcuts.
      RiverXkbBindingsSeatV1AteUnboundKey -> do
        taken <- atomicModifyIORef' (rtSubmap rt) (\s -> (Nothing, s))
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
      -- What ends an Alt-Tab.  Only sent for modifiers something asked to
      -- watch, so this is silent unless 'XMonad.River.whileModifiersHeld' has
      -- an interaction open.
      RiverXkbBindingsSeatV1ModifiersUpdate old new ->
        takeModifierWatcher >>= mapM_ (\f -> f old new)
      _ -> pure ()
    pure (Just xs)

  modifyIORef' ref $ M.insert seat RiverSeat
    { rsObject = seat, rsRemoved = False
    , rsLayerObject = mLayer, rsLayerFocus = LayerFocusNone
    , rsPointer = (0, 0), rsXkbSeat = mXkbSeat }

  riverSeatV1Listen conn seat $ \case
    RiverSeatV1Removed -> adjust ref seat $ \s -> s { rsRemoved = True }
    -- @pointer_enter@ is river's equivalent of X11's @EnterNotify@, and is
    -- what makes 'focusFollowsMouse' work.  X11 checked @ev_mode ==
    -- notifyNormal@ to ignore the crossings a grab synthesises; river sends
    -- this only for genuine pointer movement, so there is nothing to filter.
    RiverSeatV1PointerEnter win -> do
      traceLine ("enter win=" ++ show win)
      writeIORef (rtHovered rt) (Just win)
      when (rtFollowsMouse rt) $ queueAction rt $ do
        -- Both conditions are about the delay.  The action runs at the start
        -- of the next manage sequence rather than now, so the pointer may
        -- have moved on -- refocusing to where it used to be would fight the
        -- crossing that has already been queued behind this one.  And a drag
        -- in progress owns the focus outright: X11 held a pointer grab for
        -- the duration, which suppressed crossing events entirely, and there
        -- is no grab here to do it.
        stillThere <- io ((== Just win) <$> readIORef (rtHovered rt))
        drag <- gets dragging
        when (stillThere && isNothing drag) (focus win)
    RiverSeatV1PointerLeave -> do
      traceLine "leave"
      writeIORef (rtHovered rt) Nothing
    RiverSeatV1PointerPosition x y ->
      adjust ref seat $ \s -> s { rsPointer = (x, y) }
    -- A surface this window manager drew was pressed.  X11 delivered that as a
    -- ButtonPress on the decoration window and river will not: a decoration is
    -- a river_shell_surface_v1, not a river_window_v1, so it gets an event of
    -- its own.  The id it carries is the one XMonad.Util.XUtils.createNewWindow
    -- handed out, so a hook can match on it directly.
    --
    -- The position is read here rather than carried by the event, because
    -- pointer_position for this sequence has already arrived: every event in
    -- this listener precedes the manage_start that ends the sequence, so
    -- rsPointer is where the pointer was at the moment of the press.
    RiverSeatV1ShellSurfaceInteraction surf -> do
      seats <- readIORef ref
      let (px, py) = maybe (0, 0) rsPointer (M.lookup seat seats)
      queueAction rt $ void $ broadcastEvent (SurfaceClicked surf px py)
    -- An interactive operation reports the total offset since it began.
    -- XMonad.Operations.mouseDrag stashed where the pointer was at the time,
    -- so the absolute position its caller expects is origin + delta.
    RiverSeatV1OpDelta dx dy -> queueAction rt $ do
      drag <- gets dragging
      whenJust drag $ \(motion, _) -> do
        (ox, oy) <- io . readIORef =<< asks (riverDragOrigin . riverState)
        motion (ox + dx) (oy + dy)
    RiverSeatV1OpRelease -> do
      riverSeatV1OpEnd conn seat
      queueAction rt $ do
        drag <- gets dragging
        whenJust drag $ \(_, cleanup) -> cleanup
    _ -> pure ()

--------------------------------------------------------------------------------
-- The manage sequence

manageSequence :: Runtime -> X ()
manageSequence rt = do
  asks (inManageSeq . riverState) >>= \r -> io (writeIORef r True)
  restoreState rt
  reapClosed
  syncScreens
  nominateLayerOutput rt
  createBindings rt
  adoptNewWindows
  runPending rt
  applyLayout rt
  asks (inManageSeq . riverState) >>= \r -> io (writeIORef r False)

-- | Pick up where the previous window manager left off, if it left a state
-- file.
--
-- This is what makes @M-q@ keep windows on the workspaces they were on.  It
-- runs first in the first manage sequence and never again, and both halves of
-- that matter:
--
-- * /First in the sequence/, because everything after it -- reconciling
--   screens, adopting windows, laying out -- has to see the restored
--   'WindowSet' rather than the empty one the process started with.
--
-- * /The first sequence/, because that is the earliest moment the identifiers
--   in the file can be resolved, and also the last moment they can be resolved
--   without a visible flicker.  river sends a @window@ event, and the
--   @identifier@ with it, for every window it already has /before/ it sends
--   @manage_start@ -- @WindowManager.manageStart@ iterates the windows and
--   then sends the event -- so by the time this runs the map is complete.
--
-- A restored window has already been through a manage hook, in the process
-- that wrote the file, so it is marked as no longer new and 'adoptNewWindows'
-- leaves it alone.  Anything the file did not account for -- opened while the
-- successor was starting, or belonging to a client that connected since --
-- stays new and is managed normally.
restoreState :: Runtime -> X ()
restoreState rt = do
  done <- io (readIORef (rtRestored rt))
  unless done $ do
    io (writeIORef (rtRestored rt) True)
    path <- asks (stateFileName . directories)
    exists <- io (doesFileExist path)
    when exists $ do
      xmc <- asks config
      wanted <- io (windowsInStateFile path)
      mst <- readStateFile xmc
      whenJust mst $ \st -> do
        modify $ \s -> s { windowset = windowset st
                         , extensibleState = extensibleState st }
        let restored = W.allWindows (windowset st)
        -- Worth saying out loud, and not only for the test that reads it: the
        -- two numbers differing is the one interesting failure here.  It means
        -- identifiers written by the previous window manager did not match the
        -- ones river is now reporting, and the symptom a user sees is windows
        -- silently back on the wrong workspace -- with nothing else to
        -- distinguish it from a state file that was never written.
        io $ hPutStrLn stderr $ "xmonad-river: note: restored "
          <> show (length restored) <> " of " <> show wanted
          <> " windows from " <> path

-- | How many windows the state file claims, read before it is consumed.
--
-- Counted from the raw text rather than from a parse, because the point is to
-- compare against what the parse produced.  Returns 0 for a file that cannot
-- be read, which is also what 'readStateFile' will make of it.
windowsInStateFile :: FilePath -> IO Int
windowsInStateFile path = handle (\(_ :: SomeException) -> pure 0) $ do
  raw <- readFile path
  case reads raw of
    [(sf, _)] -> pure (length (W.allWindows (sfWins sf)))
    _         -> pure 0

-- | Drop windows river has told us are gone, and destroy the protocol objects.
-- | Drop what river has closed from the 'WindowSet', and tell the config.
--
-- Only the parts that need 'XState' or run user code.  Destroying the protocol
-- objects is 'reapObjects', which the event loop does immediately afterwards --
-- the two halves have to stay in that order, because this one still needs to
-- see the entries that one deletes.
reapClosed :: X ()
reapClosed = do
  ref <- asks (riverWindows . riverState)
  ws <- io (readIORef ref)
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w ->
    modify $ \st -> st { windowset = W.delete (rwObject w) (windowset st) }

  outs <- io . readIORef =<< asks (riverOutputs . riverState)
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o ->
    void (broadcastEvent (OutputRemoved (roObject o)))

  seats <- io . readIORef =<< asks (riverSeats . riverState)
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s ->
    void (broadcastEvent (SeatRemoved (rsObject s)))

-- | Destroy the protocol objects for everything river has closed.
--
-- Runs on the event loop: object lifetime is the connection's business, and
-- the maps are the loop's.  Deliberately after 'reapClosed', and deliberately
-- before a plan is transmitted -- a plan naming a window destroyed here would
-- be a protocol error, and filtering it out is exactly what transmitting
-- against these maps does.
reapObjects :: Runtime -> Connection -> IO ()
reapObjects rt conn = do
  let ref = rtWindows rt
  ws <- readIORef ref
  forM_ [ w | w <- M.elems ws, rwClosed w ] $ \w -> do
    forgetBorderOverride (rwObject w)
    riverNodeV1Destroy conn (rwNode w)
    riverWindowV1Destroy conn (rwObject w)
    modifyIORef' ref (M.delete (rwObject w))

  let outRef = rtOutputs rt
  outs <- readIORef outRef
  -- The layer shell objects are inert once removed is sent, but destroying
  -- them is still what completes destruction of the output.
  forM_ [ o | o <- M.elems outs, roRemoved o ] $ \o -> do
    forM_ (roLayerObject o) (riverLayerShellOutputV1Destroy conn)
    riverOutputV1Destroy conn (roObject o)
    modifyIORef' outRef (M.delete (roObject o))

  let seatRef = rtSeats rt
  seats <- readIORef seatRef
  forM_ [ s | s <- M.elems seats, rsRemoved s ] $ \s -> do
    forM_ (rsLayerObject s) (riverLayerShellSeatV1Destroy conn)
    forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1Destroy conn)
    riverSeatV1Destroy conn (rsObject s)
    modifyIORef' seatRef (M.delete (rsObject s))

-- | Nominate an output for layer surfaces that do not pick one themselves.
--
-- Until this is done the default output is undefined, and a client like fuzzel
-- that names no output has nowhere to be placed. @set_default@ modifies window
-- management state, so it belongs in the manage sequence.
--
-- The choice follows the current screen, so that a prompt opens on the output
-- being worked on. It is reissued whenever that changes, which also covers the
-- case of the previous default being unplugged.
nominateLayerOutput :: Runtime -> X ()
nominateLayerOutput rt = forM_ (rtLayerShell rt) $ \_ -> do
  outs <- io . readIORef =<< asks (riverOutputs . riverState)
  ws <- gets windowset
  let SD current = W.screenDetail (W.current ws)
      live = filter (not . roRemoved) (M.elems outs)
      -- Match by position: that is the only thing tying a StackSet screen back
      -- to the output it was built from in 'syncScreens'.
      onScreen o = let (x, y) = roPosition o
                   in x == rect_x current && y == rect_y current
      chosen = case filter onScreen live of
        (o:_) -> Just o
        []    -> case sortOn roPosition live of
          (o:_) -> Just o
          []    -> Nothing
  -- Recorded rather than sent.  Which output should host layer surfaces is a
  -- decision, and 'transmitManage' is what turns decisions into requests; it
  -- also holds the "only when the choice actually changes" comparison, since
  -- that is about what has been sent rather than about what was chosen.
  io $ modifyIORef' (rtPlan rt) $ \p ->
    p { planLayerDefault = (\o -> (roObject o, roLayerObject o)) <$> chosen }

-- | Reconcile the 'WindowSet'\'s screens with river's outputs.
--
-- This is xmonad's @rescreen@, driven by the output list rather than xinerama.
-- Outputs are ordered by position so that screen ids are stable across
-- reconnects, which is what @XMonad.Actions.PhysicalScreens@ relies on.
syncScreens :: X ()
syncScreens = do
  outs <- io . readIORef =<< asks (riverOutputs . riverState)
  let rects =
        [ rect
        | o <- sortOn roPosition (filter (not . roRemoved) (M.elems outs))
        , let (x, y) = roPosition o
        , let (width, height) = roSize o
        , width > 0 && height > 0
          -- Prefer the area layer shell reports, so a bar or dock that claims
          -- an exclusive zone shrinks the tiling area instead of being tiled
          -- over. It is only a hint, but honouring it is what users expect and
          -- it costs nothing to do so.
        , let rect = case roLayerArea o of
                Just a | rect_width a > 0 && rect_height a > 0 -> a
                _ -> Rectangle x y (fromIntegral width) (fromIntegral height)
        ]
  unless (null rects) $ do
    -- In SCREEN-ID order, to line up with @rects@, which is sorted by output
    -- position.  'screensOf' yields current-first, which is a different order
    -- as soon as the focused screen is not screen 0 -- comparing that list
    -- directly reported a change on nearly every sequence.
    before <- gets (map (screenRect . W.screenDetail) . sortOn W.screen . screensOf . windowset)
    -- Only when it actually changed.  A manage sequence runs for all sorts of
    -- reasons and most of them leave the outputs alone.
    --
    -- Guarding the rescreen itself, not merely the broadcast, is the point.
    -- 'rescreen' reassigns workspaces to screens and forces the focused one
    -- onto screen 0 -- which is correct when the outputs have genuinely
    -- changed and ruinous when they have not.  Running it on every sequence
    -- dragged the focused workspace onto the leftmost monitor after every
    -- keystroke, so windows appeared to wander between monitors at random and
    -- screen-directed commands never seemed to work.
    when (before /= rects) $ do
      modify $ \st -> st { windowset = rescreen rects (windowset st) }
      void (broadcastEvent ScreenLayoutChanged)

-- | The screens a 'WindowSet' currently has, current first.
screensOf :: WindowSet -> [W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail]
screensOf ws = W.current ws : W.visible ws

-- | Lay the given screen rectangles over the current workspaces, preserving
-- which workspace is on which screen where possible.
rescreen :: [Rectangle] -> WindowSet -> WindowSet
rescreen rects ws = ws
    { W.current = (W.current ws) { W.screen = 0, W.screenDetail = SD firstRect }
    , W.visible = newVisible
    , W.hidden  = newHidden
    }
  where
    (firstRect, restRects) = case rects of
      (r:rs) -> (r, rs)
      []     -> (Rectangle 0 0 0 0, [])

    -- Each additional screen takes one workspace, drawn from the previously
    -- visible screens first and then from hidden.  Every workspace is consumed
    -- exactly once.
    --
    -- The previous implementation indexed into both lists independently, so
    -- when the screen count GREW -- one output at startup, three once kanshi
    -- applies a profile -- every new screen was handed @head newHidden@, and
    -- the same workspace ended up on several screens at once while still
    -- appearing in hidden.  That violates StackSet's invariant that a
    -- workspace occurs exactly once, and the visible symptoms were a status
    -- bar listing the same workspace repeatedly, windows only ever appearing
    -- on the first screen, and screen-directed commands doing nothing.
    (newVisible, newHidden) = go (1 :: Int) restRects (W.visible ws) (W.hidden ws)

    go _ []     surplus hid = ([], map W.workspace surplus ++ hid)
    go i (r:rs) (s:vs)  hid =
      let (rest, hid') = go (i + 1) rs vs hid
      in  (s { W.screen = fromIntegral i, W.screenDetail = SD r } : rest, hid')
    go i (r:rs) []      (h:hs) =
      let (rest, hid') = go (i + 1) rs [] hs
      in  (W.Screen h (fromIntegral i) (SD r) : rest, hid')
    -- Degenerate: more screens than workspaces.  Nothing distinct is left to
    -- show, so the current workspace is repeated, as before.
    go i (r:rs) []      [] =
      let (rest, hid') = go (i + 1) rs [] []
      in  (W.Screen (W.workspace (W.current ws)) (fromIntegral i) (SD r) : rest, hid')

-- | Run the manage hook for windows river has just told us about, and insert
-- them into the 'WindowSet'.
--
-- This happens during a manage sequence, before the window has been rendered,
-- which is the same ordering guarantee xmonad's manage hook has — and the one
-- sway's IPC cannot provide.
adoptNewWindows :: X ()
adoptNewWindows = do
  ref <- asks (riverWindows . riverState)
  ws <- io (readIORef ref)
  let fresh = [ w | w <- M.elems ws, rwNew w, not (rwClosed w) ]
  forM_ fresh $ \w -> do
    io $ adjust ref (rwObject w) $ \x -> x { rwNew = False }
    -- Ask for server-side decoration before the manage hook runs, so a config
    -- that wants otherwise can override it there.
    --
    -- This has to be said out loud: river's default, if a window manager makes
    -- neither request, is use_csd -- the client draws its own title bar and
    -- close button.  That is right for a floating desktop and wrong for a
    -- tiling one, where the window manager owns the frame and a client-drawn
    -- title bar is decoration nobody asked for on every single window.
    --
    -- "Server side" here does not mean river draws a title bar either: it
    -- draws exactly what this window manager asks for, which is the border
    -- from 'borderWidth' and nothing else.  A client that only supports CSD
    -- ignores the request, which river documents and which is why this is not
    -- conditional on the decoration_hint.
    emitOp (OpUseDecorations (rwObject w) True)
    -- Everything above is setup this connection owes river for any window it
    -- has not spoken to before, including one carried over from the previous
    -- window manager: those requests were made on a connection that no longer
    -- exists, and river's defaults are back.  Everything below is /managing/ a
    -- window, which a restored one has already had done to it -- by the
    -- process that wrote the state file.  Running the manage hook again would
    -- re-shift it to whatever the hook says and undo the restore, and
    -- 'W.insertUp' would put a second copy of it in the 'WindowSet'.
    --
    -- Membership in the 'WindowSet' is the test rather than a flag, because it
    -- is the actual question being asked: is this already a managed window.
    managed <- gets (W.allWindows . windowset)
    unless (rwObject w `elem` managed) $ do
      let win = rwObject w
      -- Upstream's 'XMonad.Operations.manage' floats a window that is
      -- transient or fixed-size /before/ running the manage hook, and that --
      -- not any config rule -- is where every dialog on an xmonad desktop
      -- gets its floating from.  A config names only the handful of windows
      -- whose own app_id has to be recognised; the file chooser, the print
      -- dialog, the alert box and the preferences window are all floated by
      -- this rule and appear in no config anywhere.  Porting the hook but not
      -- this is what left them tiled.
      --
      -- 'isFixedSizeOrTransient' answers from what river has already reported
      -- -- @dimensions_hint@ for the size and @river_window_v1.parent@, which
      -- is @xdg_toplevel.set_parent@, for the transience -- so the 'Display'
      -- it takes for signature compatibility goes unread.
      autoFloat <- withDisplay $ \d -> isFixedSizeOrTransient d win
      -- Asked before the hook runs, as upstream asks it: a window being
      -- managed for the first time has been through no layout run, so
      -- 'floatLocation' centres it on the current screen at the size river
      -- reports.
      rr <- snd <$> floatLocation win
      io $ traceLine $ "adopt win=" ++ show win
        ++ " appid=" ++ show (maybe "" decodeUtf8 (rwAppId w))
        ++ " title=" ++ show (maybe "" decodeUtf8 (rwTitle w))
        ++ " parent=" ++ show (rwParent w)
        ++ " minmax=" ++ show (sh_min_size (rwSizeHints w), sh_max_size (rwSizeHints w))
        ++ " autofloat=" ++ show autoFloat
      mh <- asks (manageHook . config)
      g <- userCodeDef (mempty) (runQuery mh win)
      ws' <- gets windowset
      -- Keep a float that would hang off the edge on the screen, as upstream
      -- does.  'floatLocation' centres a first-time window, so this only ever
      -- catches one whose remembered rectangle no longer fits.
      let keepOnScreen r@(W.RationalRect x y wid h)
            | x + wid > 1 || y + h > 1 || x < 0 || y < 0 =
                W.RationalRect (0.5 - wid / 2) (0.5 - h / 2) wid h
            | otherwise = r
          -- The manage hook is applied last, exactly as upstream applies it
          -- last, so a config rule still outranks this: 'doShift' moves the
          -- window and leaves it floating, and a rule that wants a dialog
          -- tiled can sink it.
          placed | autoFloat = W.float win (keepOnScreen rr) (W.insertUp win ws')
                 | otherwise = W.insertUp win ws'
      modify $ \st -> st { windowset = appEndo g placed }
      void (broadcastEvent (WindowAdded win))

-- | Run the user's startup hook exactly once, after the first manage sequence
-- has been finished.
--
-- Anything it does that changes window management state goes through
-- 'XMonad.Operations.windows', which requests another manage sequence, so
-- deferring costs nothing but keeps a slow hook from tripping river's
-- watchdog.
-- Setting @XMONAD_RIVER_NO_STARTUP_HOOK@ skips it, which is what makes it safe
-- to run a real config under a test compositor.  A startup hook is the one
-- part of a config that reaches outside the session it belongs to: this one
-- spawns a dozen processes and kills tmux sessions by name, and doing that
-- from a throwaway headless river would interfere with the desktop the user is
-- actually sitting in front of.  Everything worth testing -- manage sequences,
-- layout, bindings -- happens without it.
runStartupHook :: Runtime -> X ()
runStartupHook rt = do
  done <- io (readIORef (rtStartupDone rt))
  unless done $ do
    io (writeIORef (rtStartupDone rt) True)
    skip <- io (lookupEnv "XMONAD_RIVER_NO_STARTUP_HOOK")
    case skip of
      Just _ -> io $ hPutStrLn stderr
        "xmonad-river: note: startup hook skipped \
        \(XMONAD_RIVER_NO_STARTUP_HOOK is set)"
      Nothing -> do
        hook <- asks (startupHook . config)
        _ <- userCode hook
        pure ()

--------------------------------------------------------------------------------
-- Bindings

-- | Create river bindings for any seat that does not have them yet.
createBindings :: Runtime -> X ()
createBindings rt = do
  seats <- io . readIORef =<< asks (riverSeats . riverState)
  bound <- io (readIORef (rtBoundSeats rt))
  let new = [ s | s <- M.keys seats, not (S.member s bound) ]
  forM_ new $ \seat -> do
    bindSeat rt seat
    io $ modifyIORef' (rtBoundSeats rt) (S.insert seat)

bindSeat :: Runtime -> ObjectId -> X ()
bindSeat rt seat = do
  conn <- asks display
  bindingsGlobal <- asks (riverBindings . riverState)
  ks <- asks keyActions
  bs <- asks buttonActions

  bindPanic rt seat

  forM_ (M.toList ks) $ \((mask, keysym), action) -> do
    b <- io (riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat keysym
               (riverModifiers mask))
    io $ modifyIORef' (rtBindings rt) (M.insert b action)
    io $ riverXkbBindingV1Listen conn b $ \case
      RiverXkbBindingV1Pressed -> do
        acts <- readIORef (rtBindings rt)
        forM_ (M.lookup b acts) $ \a -> do
          modifyIORef' (rtPending rt) (a :)
          riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverXkbBindingV1Enable conn b)

  forM_ (M.toList bs) $ \((mask, button), action) -> do
    b <- io (riverSeatV1GetPointerBinding conn seat (linuxButton button)
               (riverModifiers mask))
    io $ modifyIORef' (rtPointerBind rt) (M.insert b action)
    io $ riverPointerBindingV1Listen conn b $ \case
      RiverPointerBindingV1Pressed -> do
        acts <- readIORef (rtPointerBind rt)
        forM_ (M.lookup b acts) $ \a -> do
          mHover <- readIORef (rtHovered rt)
          forM_ mHover $ \win -> do
            modifyIORef' (rtPending rt) (a win :)
            riverWindowManagerV1ManageDirty conn (rtManager rt)
      _ -> pure ()
    io (riverPointerBindingV1Enable conn b)

-- | The chord that always works.
--
-- Two things in this window manager can make a session unusable by taking
-- something and not giving it back, and both have done so:
--
-- * a prompt is a layer surface holding an exclusive keyboard grab, so if its
--   thread stops answering, every keystroke goes somewhere nothing is reading;
-- * a submap disables every one of the config's bindings until it closes, so
--   one that never closes leaves a session with no shortcuts at all.
--
-- Each now has its own guard -- guaranteed teardown, and a deadline -- and
-- this is what remains when a guard is the thing that failed.  It is deliberately
-- not part of @keyActions@: it is registered separately, never recorded in
-- 'rtBindings', and therefore not among the bindings a submap disables.  A
-- config cannot rebind or remove it, which is the point; a break-glass key
-- that the broken thing can switch off is not one.
--
-- The chord is @Ctrl-Alt-Shift-Escape@, chosen to be one nothing else wants.
--
-- This works at all because of how river dispatches: @KeyboardGroup.processKey@
-- matches xkb bindings /before/ it consults keyboard focus, so a binding fires
-- even while a layer surface holds an exclusive grab.  Without that ordering
-- there would be no key that could rescue a wedged prompt, and the only way
-- out would be a TTY.
bindPanic :: Runtime -> ObjectId -> X ()
bindPanic rt seat = do
  conn <- asks display
  bindingsGlobal <- asks (riverBindings . riverState)
  b <- io (riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat
             xK_Escape (riverModifiers (controlMask .|. mod1Mask .|. shiftMask)))
  io $ riverXkbBindingV1Listen conn b $ \case
    RiverXkbBindingV1Pressed -> do
      n <- closeAllClients
      -- Re-enable everything the config asked for, and forget any submap.
      -- Whatever state a half-finished submap left behind, the bindings the
      -- user knows about are the ones that should be live afterwards.
      globals <- readIORef (rtBindings rt)
      forM_ (M.keys globals) (riverXkbBindingV1Enable conn)
      writeIORef (rtSubmap rt) Nothing
      writeIORef (rtArmed rt) []
      writeIORef (rtDisarm rt) False
      hPutStrLn stderr $ "xmonad-river: panic: closed " <> show n
        <> " prompt(s) and re-enabled " <> show (M.size globals) <> " binding(s)"
      riverWindowManagerV1ManageDirty conn (rtManager rt)
    _ -> pure ()
  io (riverXkbBindingV1Enable conn b)

-- | X11 button numbers to Linux input event codes, which is what river's
-- pointer bindings take.
linuxButton :: Button -> Word32
linuxButton = \case
  1 -> 0x110  -- BTN_LEFT
  2 -> 0x112  -- BTN_MIDDLE
  3 -> 0x111  -- BTN_RIGHT
  4 -> 0x113  -- BTN_SIDE
  5 -> 0x114  -- BTN_EXTRA
  n -> 0x110 + fromIntegral n

-- | Queue an action to run at the start of the next manage sequence.
-- | Queue an action to run at the start of the next manage sequence, and ask
-- river for one.
--
-- Everything that fires outside a sequence -- bindings, and the deltas of an
-- interactive drag -- has to go through here, because river only permits
-- window management state to change during a sequence.
queueAction :: Runtime -> X () -> IO ()
queueAction rt act = do
  modifyIORef' (rtPending rt) (act :)
  riverWindowManagerV1ManageDirty (rtConn rt) (rtManager rt)

runPending :: Runtime -> X ()
runPending rt = do
  acts <- io (atomicModifyIORef' (rtPending rt) (\as -> ([], reverse as)))
  mapM_ (userCode) acts

--------------------------------------------------------------------------------
-- Layout

-- | Run the layout for every visible screen, propose the resulting dimensions,
-- set keyboard focus, and stash the rectangles for the render sequence.
-- | Make a float's recorded rectangle agree with the size its client took.
--
-- @propose_dimensions@ is a request, and the @dimensions@ event answering it
-- carries what the window actually settled on.  For a /tiled/ window that
-- answer is deliberately discarded: the layout owns the geometry, and
-- re-proposing on the difference between a proposal and a reported size is the
-- comparison that cannot converge -- see 'rwProposed'.
--
-- A float is the opposite case.  Nothing else owns its rectangle, so if the
-- record does not follow what the client took, the two drift apart and never
-- come back.  The way in is a window that states no size hints at all: river
-- has sent no @dimensions@ when the manage hook runs, so 'floatLocation' has
-- nothing to go on and records @0x0@ -- river's "the window will be allowed to
-- decide its own dimensions".  The client then draws itself at its natural
-- size while the record stays at zero, and a resize drag works from zero,
-- proposing sizes far below anything the client will accept.  The window never
-- moves and the drag looks dead.
--
-- Adopting the answer is not the non-convergent comparison, because it does
-- not re-propose on a difference: the proposal is still gated on the /proposal/
-- moving.  One round settles it -- propose P, the client takes A, record A,
-- and the next sequence proposes A, which the client already chose and takes
-- again.  Both drags capture their starting rectangle once, so a record
-- updated mid-drag cannot disturb the arithmetic of a drag in progress either.
--
-- The recorded rectangle includes the border and what river reports does not,
-- which is the same asymmetry 'insetBorder' handles when transmitting, so the
-- border goes back on here.
reconcileFloats :: X ()
reconcileFloats = do
  -- Never while a drag is in progress.  For its duration the drag owns the
  -- rectangle and rewrites it on every motion step, so reconciling against the
  -- size the client last reported chases the drag and is undone by the next
  -- step -- measured at ~2,600 reconciliations across one six-second resize,
  -- none of which survived.  Waiting costs nothing: the drag's final rectangle
  -- is reconciled once, on the first sequence after it ends.
  drag <- gets dragging
  floats <- gets (W.floating . windowset)
  unless (isJust drag || M.null floats) $ do
    known <- io . readIORef =<< asks (riverWindows . riverState)
    ref <- asks (riverPlacements . riverState)
    placements <- io (readIORef ref)
    bw0 <- asks (borderWidth . config)
    forM_ (M.keys floats) $ \w ->
      forM_ ((,) <$> M.lookup w known <*> lookup w placements) $ \(rw, r) -> do
        (mWidth, _) <- io (lookupBorderOverride w)
        let bw = fromMaybe bw0 mWidth
            (aw, ah) = rwDimensions rw
            wantW = fromIntegral aw + 2 * bw
            wantH = fromIntegral ah + 2 * bw
        when (aw > 0 && ah > 0
                && (wantW /= rect_width r || wantH /= rect_height r)) $ do
          io $ traceLine $ "reconcile float w=" ++ show w
            ++ " recorded=" ++ show (rect_width r, rect_height r)
            ++ " actual=" ++ show (wantW, wantH)
          io (updatePlacement ref w r { rect_width = wantW, rect_height = wantH })
          (_, rr) <- floatLocation w
          modify $ \st -> st { windowset = W.float w rr (windowset st) }

applyLayout :: Runtime -> X ()
applyLayout rt = do
  -- Before reading the windowset, because this can change it.
  reconcileFloats
  ws <- gets windowset
  -- By screen id, and NOT @W.current : W.visible@.
  --
  -- The render sequence @place_top@s the concatenation of these in order, so
  -- this is the stacking order.  Written the other way it is a function of
  -- which screen holds focus -- 'W.view' moves the newly focused screen to the
  -- head and pushes the old one behind it -- so alternating focus between two
  -- screens alternates their windows' relative stacking, forever.
  --
  -- That is a feedback loop with 'focusFollowsMouse' on, and it was the cause
  -- of all three storms on 2026-09-04.  river draws a window's borders outside
  -- its box, so two windows on either side of the seam between two outputs
  -- overlap there; restacking them changes which one answers river's hit test;
  -- river reports the new one as @pointer_enter@; hovering turns that into a
  -- focus change; and the focus change restacks them back.  Measured at 1-4 ms
  -- a lap, with nothing moving and no geometry changing at all -- which is why
  -- the @propose_dimensions@ guard, a correct fix for a real defect, did
  -- nothing for it.
  --
  -- Screens are disjoint, so the order between them carries no meaning.  All
  -- it has to be is stable.
  let screens = sortOn W.screen (W.current ws : W.visible ws)
  placements <- fmap concat $ forM screens $ \scr -> do
    let wsp = W.workspace scr
        SD rect = W.screenDetail scr
        -- The floating layer, as upstream's 'windows' handles it.  Floats are
        -- withheld from the layout -- it would tile them -- and placed from the
        -- rectangles 'XMonad.Operations.float' recorded, scaled to the screen.
        --
        -- Leaving this out is not a small omission: it makes 'float' inert, so
        -- every drag, keyboard move and doFloat hook is undone by the next
        -- manage sequence, which looks like the mouse bindings not working at
        -- all.
        floats = W.floating ws
        onWs = W.integrate' (W.stack wsp)
        tiled = W.stack wsp >>= W.filter (`M.notMember` floats)
        flt = [ (fw, scaleRationalRect rect rr)
              | fw <- onWs, Just rr <- [M.lookup fw floats] ]
    (rs, mLayout) <- userCodeDef ([], Nothing) (runLayout wsp { W.stack = tiled } rect)
    forM_ mLayout $ \l' -> modify $ \st ->
      st { windowset = updateLayout (W.tag wsp) l' (windowset st) }
    -- Floats last, because the render sequence place_tops this list in order,
    -- so later is higher.  Upstream expresses the same thing the other way
    -- round and then restacks.
    pure (rs ++ flt)

  -- Borders are resolved here rather than while transmitting, because the
  -- override lookup and the focused/normal choice are decisions and belong
  -- with the rest of them.  river forgets rendering state between frames, so
  -- the answer is restated on every render sequence from this map.
  bw0 <- asks (borderWidth . config)
  focusedCol <- asks focusedBorder
  normalCol <- asks normalBorder
  let mFocus = W.peek ws
  borders <- fmap M.fromList $ forM placements $ \(win, _) -> do
    (mWidth, mColor) <- io (lookupBorderOverride win)
    let width = fromMaybe bw0 mWidth
        rgba = case mColor of
          Just c  -> c
          Nothing -> pixelColor (if Just win == mFocus then focusedCol else normalCol)
    pure (win, (width, rgba))

  -- What is actually transmitted: the same placements with the border taken
  -- out of them, per window, since a border override may make one window's
  -- wider than the rest.  'placements' itself stays as the layout produced it
  -- -- see 'insetBorder' for which of the two each consumer wants.
  let contents = [ (win, insetBorder (maybe bw0 fst (M.lookup win borders)) r)
                 | (win, r) <- placements ]

  -- The order the screens were visited in, what the layout placed, and the
  -- geometry actually transmitted for it.  The screen order is the one the
  -- render sequence @place_top@s in, so a trace showing it permute while
  -- nothing moves is what tells a restacking loop from a geometry one -- and,
  -- now that it is sorted, what shows it staying put.
  io $ traceLine $ "layout screens="
    ++ show [ (W.screen scr, W.tag (W.workspace scr)) | scr <- screens ]
    ++ " placed=" ++ showPlacements placements
    ++ " content=" ++ showPlacements contents

  raised <- io . readIORef =<< asks (riverRestack . riverState)
  let placed = S.fromList (map fst placements)
      stillUp = filter (`S.member` placed) raised
  io . flip writeIORef stillUp =<< asks (riverRestack . riverState)

  -- The same placements, where the @IO@-shaped queries in "XMonad.River" can
  -- reach them: 'windowRect' and 'windowUnderPointer' answer from here.  Kept
  -- alongside the plan rather than derived from it because 'RiverState' is
  -- what an 'X' action has in hand, and the plan is not.
  io . flip writeIORef placements =<< asks (riverPlacements . riverState)

  io $ modifyIORef' (rtPlan rt) $ \p -> p
    { planSerial     = planSerial p + 1
    , planPlacements = contents
    , planBorders    = borders
    , planVisible    = placed
    , planRaised     = stillUp
    , planFocus      = maybe ClearFocus FocusWindow mFocus
    }

  -- Publish what the layout decided, so that the IO-shaped queries in
  -- "XMonad.Core" -- getWindowAttributes, getGeometry -- have something to
  -- answer with.  Every window river has told us about is included, not just
  -- the placed ones: X11 answered for any window that existed, and a window on
  -- a workspace that is not on screen still exists.  It is simply unmapped and
  -- at the origin, which is what X11 would have said of an unmapped window
  -- too.
  bw <- asks (borderWidth . config)
  allKnown <- io . readIORef =<< asks (riverWindows . riverState)
  let placedMap = M.fromList placements
      -- X11 reported the OUTER corner and the CONTENT size, with the border
      -- width alongside, and that is what upstream's 'floatLocation' arithmetic
      -- adds @bw*2@ back onto.  Reporting the layout rectangle as the width --
      -- as this did -- overstated every window by twice its border.
      attrs w rw = case M.lookup w placedMap of
        Just r ->
          let wbw = maybe bw fst (M.lookup w borders)
              c = insetBorder wbw r
          in WindowAttributes
          { wa_x = rect_x r, wa_y = rect_y r
          , wa_width = rect_width c, wa_height = rect_height c
          , wa_border_width = wbw, wa_map_state = waIsViewable
          , wa_override_redirect = False }
        Nothing -> let (dw, dh) = rwDimensions rw in WindowAttributes
          { wa_x = 0, wa_y = 0
          , wa_width = fromIntegral dw, wa_height = fromIntegral dh
          , wa_border_width = bw, wa_map_state = waIsUnmapped
          , wa_override_redirect = False }
  io $ publishGeometry (M.mapWithKey attrs allKnown)
  io $ publishSizeHints (M.map rwSizeHints allKnown)

  -- Last, because the whole point of waiting is to see everything above:
  -- 'riverPlacements' for 'XMonad.River.windowRect', and the published
  -- geometry for 'getWindowAttributes'.  Drained once rather than to a fixed
  -- point -- an action queued by one of these waits for the next layout, which
  -- is the honest answer, since this layout is over.
  ref <- asks (riverAfterLayout . riverState)
  queued <- io (atomicModifyIORef' ref (\as -> ([], reverse as)))
  mapM_ userCode queued

-- | Send the window management half of a plan.
--
-- Everything here is resolved against the windows river currently has, not the
-- ones the plan was computed against: the two can differ, and naming a window
-- river has destroyed is a protocol error that disconnects the window manager
-- rather than a request that is ignored.
transmitManage :: Runtime -> Connection -> IO ()
transmitManage rt conn = do
  plan <- readIORef (rtPlan rt)
  known <- readIORef (rtWindows rt)
  seats <- readIORef (rtSeats rt)

  traceLine $ "manage serial=" ++ show (planSerial plan)
    ++ " focus=" ++ show (planFocus plan)

  -- What user code asked for since the last sequence.  Drained rather than
  -- kept: every one of these is an effect river performs once, so re-sending
  -- would not be a no-op.  Filtered against the live objects for the same
  -- reason the plan is -- a window closed between the request and now would
  -- otherwise be a protocol error rather than a request that does nothing.
  -- Restoring the config's bindings after a submap ended.  First, so that a
  -- submap opened by the action the last one ran arms on top of a clean set
  -- rather than fighting this.
  disarm <- readIORef (rtDisarm rt)
  when disarm $ do
    temps <- atomicModifyIORef' (rtArmed rt) (\ts -> ([], ts))
    forM_ temps $ \b -> do
      riverXkbBindingV1Disable conn b
      riverXkbBindingV1Destroy conn b
    globals <- readIORef (rtBindings rt)
    forM_ (M.keys globals) (riverXkbBindingV1Enable conn)
    writeIORef (rtDisarm rt) False

  -- The layer-surface default, when the choice has moved since it was last
  -- sent.  river remembers this one, so restating it every sequence would be
  -- traffic for nothing.
  forM_ (planLayerDefault plan) $ \(out, mLayerObj) -> do
    prev <- readIORef (rtLayerDefault rt)
    forM_ mLayerObj $ \lo -> unless (prev == Just out) $ do
      riverLayerShellOutputV1SetDefault conn lo
      writeIORef (rtLayerDefault rt) (Just out)

  ops <- takeOps
  forM_ ops $ \case
    OpClose w -> when (M.member w known) $ riverWindowV1Close conn w
    OpWarpPointer s x y -> when (M.member s seats) $ riverSeatV1PointerWarp conn s x y
    OpPointerOpStart s -> when (M.member s seats) $ riverSeatV1OpStartPointer conn s
    OpUseDecorations w ssd -> when (M.member w known) $
      (if ssd then riverWindowV1UseSsd else riverWindowV1UseCsd) conn w
    OpSetPosition w x y -> forM_ (M.lookup w known) $ \rw ->
      riverNodeV1SetPosition conn (rwNode rw) x y
    OpProposeDimensions w dw dh -> when (M.member w known) $ do
      riverWindowV1ProposeDimensions conn w (fromIntegral dw) (fromIntegral dh)
      adjust (rtWindows rt) w $ \x ->
        x { rwProposed = Just (fromIntegral dw, fromIntegral dh) }
    OpCaptureInput ks mods oneShot gen -> armCapture rt conn seats ks mods oneShot gen
    OpUngrabKeys -> do
      old <- atomicModifyIORef' (rtGrabbed rt) (\bs -> ([], bs))
      forM_ old $ \b -> do
        riverXkbBindingV1Disable conn b
        riverXkbBindingV1Destroy conn b
    OpGrabKeys ks -> do
      bs <- fmap concat $ forM (M.elems seats) $ \seat ->
        forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
          b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
                 (rsObject seat) keysym (riverModifiers mask)
          -- Indexed rather than keyed on the binding object, so that what runs
          -- stays with the config and the loop holds only the objects.
          let fire pick = do
                acts <- readIORef (rtExtraKeys rt)
                forM_ (take 1 (drop i acts)) (queueAction rt . pick)
          riverXkbBindingV1Listen conn b $ \case
            RiverXkbBindingV1Pressed  -> fire fst
            RiverXkbBindingV1Released -> fire snd
            _ -> pure ()
          riverXkbBindingV1Enable conn b
          pure b
      writeIORef (rtGrabbed rt) bs
    -- Drained by the loop rather than here; see 'takeNowOps'.
    OpExitSession -> pure ()
    OpStop -> pure ()
    OpSetXcursorTheme{} -> pure ()

  -- Dimensions are window management state, so they go here rather than in
  -- the render sequence -- and, being state river keeps, only when the answer
  -- has moved.  Rendering state is restated in full every frame because river
  -- forgets it; a proposal is remembered, so restating one is not a no-op but
  -- a fresh request the server must answer with another @dimensions@ event.
  --
  -- Sending it unconditionally is a feedback loop.  Every manage sequence
  -- re-drove the geometry of every window; each window that could not take the
  -- proposal exactly settled somewhere else and had its edges move across a
  -- stationary pointer; river reported the crossing as a genuine
  -- @pointer_enter@; and with 'focusFollowsMouse' on that crossing queued an
  -- action, which asked for a manage sequence, which proposed again.  Three
  -- terminals were enough to sustain it at some seven hundred layout runs a
  -- second, and a floating GTK dialog -- proposed 1x1 and unable to go below
  -- its own minimum -- made it permanent.
  --
  -- The comparison is against the last proposal, never against the
  -- @dimensions@ event: see 'rwProposed'.
  forM_ (planPlacements plan) $ \(win, r) -> forM_ (M.lookup win known) $ \w -> do
    let want = (fromIntegral (rect_width r), fromIntegral (rect_height r))
    when (rwProposed w /= Just want) $ do
      uncurry (riverWindowV1ProposeDimensions conn win) want
      adjust (rtWindows rt) win $ \x -> x { rwProposed = Just want }

  -- Keyboard focus, likewise. A seat whose keyboard has gone to a layer
  -- surface is left alone: river discards focus requests outright while focus
  -- is exclusive, and in the non-exclusive case setting focus in this same
  -- manage sequence would silently steal the keyboard back — which is the
  -- difference between a fuzzel prompt you can type into and one you cannot.
  forM_ (M.elems seats) $ \s ->
    unless (layerHasFocus (rsLayerFocus s)) $
      case planFocus plan of
        FocusWindow win | M.member win known ->
          riverSeatV1FocusWindow conn (rsObject s) win
        _ -> riverSeatV1ClearFocus conn (rsObject s)

updateLayout :: WorkspaceId -> Layout Window -> WindowSet -> WindowSet
updateLayout i l = W.mapWorkspace $ \wsp ->
  if W.tag wsp == i then wsp { W.layout = l } else wsp

--------------------------------------------------------------------------------
-- The render sequence

-- | Take the keyboard for a submap or a hold-to-cycle, and disable the
-- config's own bindings while it is held.
--
-- Inside the manage sequence that carries the key press which asked for it,
-- which is the whole reason this is the loop's work rather than the config's:
-- river holds every input event for the seat until the sequence finishes, so
-- arming here is atomic with the press.  Arming one sequence later would leave
-- an interval in which the globals are live and this is not, and the second
-- key of a chord would run the wrong thing.
--
-- Every one of the window manager's own bindings is disabled meanwhile.  river
-- leaves it to compositor policy which of several bindings matching one
-- physical key receives the press, so leaving the globals live alongside these
-- would make a prefix key that is also a captured key -- @M-m m@, say -- do
-- something undefined.
armCapture
  :: Runtime -> Connection -> M.Map ObjectId RiverSeat
  -> [(KeyMask, KeySym)] -> KeyMask -> Bool -> Int -> IO ()
armCapture rt conn seats ks mods oneShot gen = do
  globals <- readIORef (rtBindings rt)
  forM_ (M.keys globals) (riverXkbBindingV1Disable conn)

  -- Taking the slot is what makes a key, an unbound key, a modifier release
  -- and the deadline exclusive: whoever gets a 'Just' owns the teardown.
  let claim = atomicModifyIORef' (rtSubmap rt) $ \case
        Just cap | icGeneration cap == gen -> (Nothing, Just cap)
        other -> (other, Nothing)

  temps <- fmap concat $ forM (M.elems seats) $ \seat ->
    forM (zip [0 :: Int ..] ks) $ \(i, (mask, keysym)) -> do
      b <- riverXkbBindingsV1GetXkbBinding conn (rtBindingsGlobal rt)
             (rsObject seat) keysym (riverModifiers mask)
      riverXkbBindingV1Listen conn b $ \case
        RiverXkbBindingV1Pressed
          -- A submap ends on its first key, so the slot is taken here.
          | oneShot -> claim >>= \taken -> forM_ taken $ \cap -> do
              writeIORef (rtDisarm rt) True
              queueAction rt (icOnKey cap True i)
          -- A hold-to-cycle keeps going, so the slot is only read.
          | otherwise -> do
              held <- readIORef (rtSubmap rt)
              forM_ held $ \cap -> queueAction rt (icOnKey cap True i)
        RiverXkbBindingV1Released | not oneShot -> do
          held <- readIORef (rtSubmap rt)
          forM_ held $ \cap -> queueAction rt (icOnKey cap False i)
        _ -> pure ()
      riverXkbBindingV1Enable conn b
      pure b
  writeIORef (rtArmed rt) temps

  -- What ends a hold-to-cycle: the modifier going up.  The event carries both
  -- the old and the new mask precisely so that a release can be told from a
  -- press.
  when (mods /= 0) $ do
    setModifierWatcher $ Just $ \old new ->
      when (old .&. mods /= 0 && new .&. mods /= old .&. mods) $ do
        taken <- claim
        forM_ taken $ \cap -> do
          writeIORef (rtDisarm rt) True
          queueAction rt (icOnEnd cap)
    forM_ (M.elems seats) $ \s ->
      forM_ (rsXkbSeat s) $ \x -> riverXkbBindingsSeatV1ModifiersWatch conn x mods

  -- Ask to be told about a key this did not want, so it can be abandoned.
  -- Without it -- on version 1, where the request does not exist -- an unknown
  -- key does nothing and the capture stays open with the config's bindings
  -- disabled, which is a session with no shortcuts.
  when oneShot $ forM_ (M.elems seats) $ \s ->
    forM_ (rsXkbSeat s) (riverXkbBindingsSeatV1EnsureNextKeyEaten conn)

  -- A deadline, because the alternative to one is a session with no
  -- keybindings at all.  If nothing else ever ends this -- the compositor is
  -- too old for the events that would, or it was opened by accident and
  -- abandoned -- the disabling would be permanent.  The generation check in
  -- 'claim' is what stops this closing a later capture.
  void $ forkIO $ do
    threadDelay captureDeadlineMicros
    taken <- claim
    forM_ taken $ \cap -> do
      hPutStrLn stderr
        "xmonad-river: keyboard capture abandoned after 60s; restoring bindings"
      writeIORef (rtDisarm rt) True
      queueAction rt (icOnEnd cap)

-- | How long an interaction may hold the keyboard before it gives up.
--
-- Long enough that no deliberate use reaches it and short enough that the
-- mistake is a nuisance rather than the end of the session.
captureDeadlineMicros :: Int
captureDeadlineMicros = 60 * 1000 * 1000

-- | Send the rendering half of a plan.
--
-- Rendering state is restated in full every frame: river keeps no memory of
-- it, so a request skipped here is not "unchanged" but "reverted".  Filtered
-- against the live window map for the reason 'transmitManage' is.
transmitRender :: Runtime -> Connection -> IO ()
transmitRender rt conn = do
  plan <- readIORef (rtPlan rt)
  let winRef = rtWindows rt
  known <- readIORef winRef

  -- Bottom-to-top, as transmitted: river's own @place_top@ appends, so the
  -- last id here is the one that ends up on top -- and so the one that wins a
  -- hit test where two windows' borders overlap.
  traceLine $ "render serial=" ++ show (planSerial plan)
    ++ " order=" ++ show (map fst (planPlacements plan))
    ++ " raised=" ++ show (planRaised plan)

  forM_ (planPlacements plan) $ \(win, r) -> forM_ (M.lookup win known) $ \w -> do
    riverNodeV1SetPosition conn (rwNode w) (rect_x r) (rect_y r)
    -- Unconditional, where this used to skip a zero width entirely: an
    -- override *to* zero is how NoBorders removes a border, and skipping the
    -- request would leave the previous one standing.  river reads width 0 as
    -- "no borders", so the two cases need no distinguishing here.
    let (width, (red, green, blue, alpha)) =
          M.findWithDefault (0, (0, 0, 0, 0)) win (planBorders plan)
    riverWindowV1SetBorders conn win allEdges (fromIntegral width)
      red green blue alpha
    when (rwHidden w) $ do
      riverWindowV1Show conn win
      adjust winRef win $ \x -> x { rwHidden = False }

  -- Anything not placed by the layout belongs to a workspace that is not on
  -- screen. river has no concept of workspaces, so this is what implements
  -- them.
  forM_ (M.elems known) $ \w ->
    unless (S.member (rwObject w) (planVisible plan) || rwHidden w) $ do
      riverWindowV1Hide conn (rwObject w)
      adjust winRef (rwObject w) $ \x -> x { rwHidden = True }

  -- Stacking order: the layout list is in the desired bottom-to-top order,
  -- then anything asked to sit above it, re-applied every frame because this
  -- loop would otherwise have just undone it.
  forM_ (planPlacements plan) $ \(win, _) ->
    forM_ (M.lookup win known) $ \w -> riverNodeV1PlaceTop conn (rwNode w)
  forM_ (planRaised plan) $ \win ->
    forM_ (M.lookup win known) $ \w -> riverNodeV1PlaceTop conn (rwNode w)

-- | A dimension bound river reports as zero or less was not stated.
sizeBound :: Int32 -> Int32 -> Maybe (Dimension, Dimension)
sizeBound w h
  | w > 0 && h > 0 = Just (fromIntegral w, fromIntegral h)
  | otherwise      = Nothing

allEdges :: Word32
allEdges = riverWindowV1EdgesTop + riverWindowV1EdgesBottom
         + riverWindowV1EdgesLeft + riverWindowV1EdgesRight

