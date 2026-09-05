-- | A gated, timestamped trace of the window management loop.
--
-- __Why this is not just 'XMonad.Core.trace'.__  It writes to the same place
-- @trace@ does -- stderr, which the session script already captures -- and
-- adds the two things instrumenting a hot loop needs and @trace@ has no way to
-- provide.
--
-- The first is a __gate__.  Idle is about one manage sequence a second and
-- ordinary use peaks around six, so an ungated trace of this loop would write
-- to every user's session log forever, for a facility almost nobody has asked
-- for.  Tracing is off unless the switch file exists, at a cost of one
-- 'IORef' read per call.
--
-- The second is a __timestamp__.  Wall clock with microseconds, so a line here
-- lines up against river's own log, which stamps the same way, and so rates
-- come out of the trace by differencing it -- which is how a storm is told
-- from ordinary use at all.
--
-- This exists because three focus storms on 2026-09-04 were undiagnosable
-- after the fact.  Each locked the session hard enough to need a power cycle,
-- and neither river at @-log-level info@ nor xmonad recorded anything per
-- sequence, so all that survived was the knowledge that something had run very
-- fast.  Two causes were proposed with confidence and both were wrong.
--
-- What is recorded is deliberately the /loop/ rather than the state: the order
-- the layout visited the screens in, what it placed where, the stacking order
-- actually transmitted, and every @pointer_enter@ river reported.  A feedback
-- loop shows up in the sequence of those lines even when no single line is
-- surprising -- which is exactly the case for a loop driven by restacking,
-- where nothing moves and no geometry changes at all.
--
-- __Switched on by a file, not by an environment variable.__  That looks like
-- the odd choice and is the deliberate one: a window manager's environment
-- belongs to whatever launched the Wayland session, so an environment variable
-- can only be changed by logging the session out and back in -- taking every
-- terminal, and every debugging session running in one, with it.  The file is
-- read afresh on each start, and @xmonad --restart@ re-execs, so tracing can be
-- turned on and off from inside the very session being diagnosed.
--
-- Nothing here imports "XMonad.Core", so that Core is free to trace itself.
module XMonad.River.Trace
  ( initTrace
  , traceLine
  , showGeom
  , showPlacements
  ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (showFFloat)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import XMonad.River.Types (Position, Rectangle (..), Window)

-- | Whether tracing is on.
--
-- A top-level 'IORef' rather than a field of
-- 'XMonad.River.Runtime.Runtime', because the point of a trace is to be
-- reachable from wherever the question happens to be -- including from code
-- with no runtime in hand -- and because instrumentation should be removable
-- without touching a core type.  It holds 'False' until 'initTrace' runs, so
-- the unsafety here is confined to allocating a reference to a 'Bool'.
{-# NOINLINE enabled #-}
enabled :: IORef Bool
enabled = unsafePerformIO (newIORef False)

-- | Turn tracing on if @\<config dir\>\/trace@ exists.
--
-- Takes the directory rather than a 'XMonad.Core.Directories' so that nothing
-- here depends on "XMonad.Core".  The file's contents are not read: it is a
-- switch, and its presence is the whole of it.
initTrace :: FilePath -> IO ()
initTrace cfgDir = do
  let switch = cfgDir </> "trace"
  present <- doesFileExist switch
  when present $ do
    writeIORef enabled True
    traceLine ("trace enabled by " ++ switch)

-- | Write one timestamped line to stderr, if tracing is on.
traceLine :: String -> IO ()
traceLine msg = do
  on <- readIORef enabled
  when on $ do
    t <- getPOSIXTime
    hPutStrLn stderr (showFFloat (Just 6) (realToFrac t :: Double) (' ' : msg))

-- | A rectangle as @WxH+X+Y@, the way X11 tools have always written a geometry.
--
-- 'show' on a 'Rectangle' costs about ninety characters, and the layout line
-- carries two lists of them -- which made that one line 84% of everything the
-- trace wrote.  A trace nobody can afford to leave on is not much of a trace.
showGeom :: Rectangle -> String
showGeom r = show (rect_width r) ++ "x" ++ show (rect_height r)
          ++ off (rect_x r) ++ off (rect_y r)
  where
    off :: Position -> String
    off n | n < 0     = show n
          | otherwise = '+' : show n

-- | Windows and where they were put, as @[#id WxH+X+Y, ...]@.
showPlacements :: [(Window, Rectangle)] -> String
showPlacements ps =
  "[" ++ intercalate ", " [ show w ++ " " ++ showGeom r | (w, r) <- ps ] ++ "]"
