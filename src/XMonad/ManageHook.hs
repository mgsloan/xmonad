-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.ManageHook
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
--
-- An EDSL for ManageHooks
--
-----------------------------------------------------------------------------
--
-- River backend notes.
--
-- The algebra -- @-->@, @=?@, @\<&&\>@, @composeAll@, @doF@ and friends -- is
-- backend-independent and is carried over unchanged.  What differs is where
-- the queries get their answers, and which queries exist at all.
--
-- Two mappings are worth knowing:
--
-- * 'className' and 'appName' both come from @river_window_v1.app_id@.  X11
--   distinguished the two halves of @WM_CLASS@; Wayland has only @app_id@, so
--   here they are the same string.  Config code matching on either still
--   works; code relying on them differing will not.
-- * 'title' is @river_window_v1.title@, i.e. @xdg_toplevel.set_title@ -- the
--   same string X11 clients put in @_NET_WM_NAME@.
--
-- @stringProperty@ and @getStringProperty@ are absent rather than stubbed.
-- Wayland has no window properties at all: no @_NET_WM_*@, no
-- @WM_WINDOW_ROLE@, no generic key-value store on a surface.  A hook matching
-- on one could only ever be dead code, and a compile error naming the line is
-- a better way to learn that than a rule that silently never fires.
--
-- This module needs an explicit export list where the X11 one does not,
-- precisely so those two names can be withheld.
-----------------------------------------------------------------------------

module XMonad.ManageHook (
    (-->), (<&&>), (<+>), (<||>), (=?),
    appName, className, resource, title,
    composeAll, idHook, liftX,
    doF, doFloat, doIgnore, doShift,
    willFloat,
    ) where

import Control.Monad.Reader
import Data.IORef (readIORef)
import Data.Monoid (Endo(..))
import qualified Data.Map as M

import XMonad.Core
import XMonad.River.State (RiverState(..))
import XMonad.River.Types
import XMonad.Operations (floatLocation, reveal)
import qualified XMonad.StackSet as W

-- | Lift an 'X' action to a 'Query'.
liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged.
idHook :: Monoid m => m
idHook = mempty

-- | Infix 'mappend'. Compose two 'ManageHook' from right to left.
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- | Compose the list of 'ManageHook's.
composeAll :: Monoid m => [m] -> m
composeAll = mconcat

infix 0 -->

-- | @p --> x@.  If @p@ returns 'True', execute the 'ManageHook'.
--
-- > (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else return mempty

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to a 'Monad'.
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
x <&&> y = ifM x y (pure False)

-- | '||' lifted to a 'Monad'.
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
x <||> y = ifM x (pure True) y

-- | Look up the current window's accumulated river state.
askWindow :: Query (Maybe RiverWindow)
askWindow = do
    w <- ask
    liftX $ do
        ref <- asks (riverWindows . riverState)
        M.lookup w <$> io (readIORef ref)

-- | Return the window title; i.e., the string a client sets with
-- @xdg_toplevel.set_title@ -- the same string X11 clients put in
-- @_NET_WM_NAME@.
title :: Query String
title = maybe "" (maybe "" decodeUtf8 . rwTitle) <$> askWindow

-- | Return the application name; i.e., @app_id@.  River has no separate
-- instance name, so this is the same string as 'className'.
appName :: Query String
appName = className

-- | Backwards compatible alias for 'appName'.
resource :: Query String
resource = appName

-- | Return the resource class; i.e., the string a client sets with
-- @xdg_toplevel.set_app_id@.
className :: Query String
className = maybe "" (maybe "" decodeUtf8 . rwAppId) <$> askWindow

-- | Return whether the window will be a floating window or not
willFloat :: Query Bool
willFloat = do
    w <- ask
    liftX $ withWindowSet $ \ws -> pure (M.member w (W.floating ws))

-- | Modify the 'WindowSet' with a pure function.
doF :: (s -> s) -> Query (Endo s)
doF = return . Endo

-- | Move the window to the floating layer.
doFloat :: ManageHook
doFloat = ask >>= \w -> doF . W.float w . snd =<< liftX (floatLocation w)

-- | Reveal the window and remove it from the 'WindowSet'.
doIgnore :: ManageHook
doIgnore = ask >>= \w -> liftX (reveal w) >> doF (W.delete w)

-- | Move the window to a given workspace
doShift :: WorkspaceId -> ManageHook
doShift i = doF . W.shiftWin i =<< ask
