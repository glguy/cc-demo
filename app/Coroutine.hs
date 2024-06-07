{-# Language MagicHash, UnboxedTuples, BlockArguments, TupleSections #-}
module Coroutine (
    Coroutine,
    new,
    yield,
    resume,

    isDead,
) where

import GHC.Prim ( PromptTag#, newPromptTag#, prompt#, control0# )
import GHC.IO ( IO(IO), unIO )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

data Coroutine i o = Coroutine (PromptTag# o) (IORef (Status i o))

data Status i o
    = Running
    | Suspended (i -> IO o)
    | Dead

isDead :: Coroutine i o -> IO Bool
isDead (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Dead -> True
            _ -> False

new :: (Coroutine i o -> i -> IO o) -> IO (Coroutine i o)
new k =
 do r <- newIORef Dead -- placeholder
    IO \s0# ->
        case newPromptTag# s0# of
            (# s1# , p# #) ->
                case Coroutine p# r of
                    c ->
                        unIO
                        (c <$ writeIORef r (Suspended (\i -> IO (prompt# p# (unIO (k c i <* writeIORef r Dead)))))) s1#

yield :: Coroutine i o -> o -> IO i
yield (Coroutine p# r) o = IO $
    control0# p# \k -> unIO
     do status <- readIORef r
        case status of
            Dead -> fail "yield on dead coroutine"
            Suspended{} -> fail "yield on suspended coroutine"
            Running ->
                o <$ writeIORef r (Suspended (\i -> IO (prompt# p# (k (# , i #)))))
            

resume :: Coroutine i o -> i -> IO o
resume (Coroutine _ r) i =
 do mb <- readIORef r
    case mb of
        Dead -> fail "resume on dead coroutine"
        Running -> fail "resume on running coroutine"
        Suspended k ->
         do writeIORef r Running
            k i
        