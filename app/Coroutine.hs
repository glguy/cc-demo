{-# Language MagicHash, UnboxedTuples, BlockArguments, TupleSections #-}
module Coroutine (
    Coroutine,
    new,
    yield,
    resume,

    isDead,
    isSuspended,
    isRunning,
) where

import GHC.Prim ( PromptTag#, newPromptTag#, prompt#, control0#, State#, RealWorld )
import GHC.IO ( IO(IO), unIO )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

-- | Coroutines that are resumed with 'i' values and yield 'o' values
data Coroutine i o = Coroutine (PromptTag# o) (IORef (Status i o))

instance Eq (Coroutine i o) where
    Coroutine _ r1 == Coroutine _ r2 = r1 == r2

data Status i o
    = Running -- ^ coroutine has been resumed
    | Dead -- ^ coroutine body has completed
    | Suspended ((State# RealWorld -> (# State# RealWorld, i #)) -> State# RealWorld -> (# State# RealWorld, o #))
    -- ^ coroutine has yielded

-- | Predicate for coroutines that have completed and can not be yielded or resumed.
isDead :: Coroutine i o -> IO Bool
isDead (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Dead -> True
            _ -> False

-- | Predicate for coroutines that have yielded and can be resumed.
isSuspended :: Coroutine i o -> IO Bool
isSuspended (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Suspended{} -> True
            _ -> False

-- | Predicate for coroutines that have have been resumed and can yield.
isRunning :: Coroutine i o -> IO Bool
isRunning (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Running -> True
            _ -> False

-- | Start a new coroutine given a body function.
new :: (Coroutine i o -> i -> IO o) -> IO (Coroutine i o)
new k =
 do r <- newIORef undefined -- placeholder
    IO \s0# ->
        case newPromptTag# s0# of
            (# s1# , p# #) ->
                case Coroutine p# r of
                    c ->
                        unIO
                        (c <$ writeIORef r (Suspended \getI# sa# ->
                             case getI# sa# of
                                (# sb# , i #) ->
                                    unIO (k c i) sb#)) s1#

-- | Suspend a running coroutine from inside its body
yield :: Coroutine i o -> o -> IO i
yield (Coroutine p# r) o =
 do status <- readIORef r
    case status of
        Dead -> fail "yield on dead coroutine"
        Suspended{} -> fail "yield on suspended coroutine"
        Running -> IO (control0# p# \k -> unIO (o <$ writeIORef r (Suspended k)))

-- | Resume execution of a yielded coroutine.
resume :: Coroutine i o -> i -> IO o
resume (Coroutine p# r) i =
 do status <- readIORef r
    case status of
        Dead -> fail "resume on dead coroutine"
        Running -> fail "resume on running coroutine"
        Suspended k# ->
         do writeIORef r Running
            IO (prompt# p# (k# (# , i #)))
