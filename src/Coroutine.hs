{-# Language MagicHash, UnboxedTuples, BlockArguments, TupleSections #-}
{- |
Module      :  Coroutine
Copyright   :  (C) Eric Mertens, 2025
License     :  ISC
Maintainer  :  Eric Mertens <emertens@gmail.com>

This module provides an implementation of coroutines inspired by the
Lua API.

# Example

This expression defines an action that constructs a new coroutine
whose behavior is to yield the numbers @[1, -1, 2, -2, 3, -3, ...]@
in sequence each time it is resumed. This coroutine ignores the the
unit parameter that it is resumed with.

>>> :set -XBlockArguments
>>> import Data.Foldable
>>> :{
 do h <- create \h _ ->
     do for_ [1::Integer ..] \i ->
         do yield h i
            yield h (-i)
        fail "unreachable"
    print =<< resume' h ()
    print =<< resume' h ()
    print =<< resume' h ()
:}
1
-1
2

-}
module Coroutine (
    Coroutine,
    create,
    yield,
    resume,
    close,
    
    -- * Convenience functions
    resume',
    wrap,

    -- ** Status predicates
    isDead,
    isSuspended,
    isRunning,
) where

import Control.Exception ( throwIO, SomeException, try )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import GHC.IO ( IO(IO), unIO )
import GHC.Prim ( PromptTag#, newPromptTag#, prompt#, control0#, State#, RealWorld )

-- | Coroutines that are resumed with 'i' values and yield 'o' values
data Coroutine i o = Coroutine (PromptTag# o) (IORef (Status i o))

-- | Coroutine handles are equal by reference.
instance Eq (Coroutine i o) where
    Coroutine _ r1 == Coroutine _ r2 = r1 == r2

-- | Internal status of a coroutine. Users will inspect status with
-- 'isDead',  'isSuspended' and 'isRunning'.
data Status i o
    = Running -- ^ coroutine has been resumed
    | Dead -- ^ coroutine body has completed
    | Suspended ((State# RealWorld -> (# State# RealWorld, i #)) -> State# RealWorld -> (# State# RealWorld, o #))
    -- ^ coroutine has suspended

-- | Predicate for coroutines that have completed and can not be yielded or resumed.
isDead :: Coroutine i o -> IO Bool
isDead (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Dead -> True
            _ -> False

-- | Predicate for coroutines that have suspended and can be resumed.
isSuspended :: Coroutine i o -> IO Bool
isSuspended (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Suspended{} -> True
            _ -> False

-- | Predicate for coroutines that have have been resumed and can suspend.
isRunning :: Coroutine i o -> IO Bool
isRunning (Coroutine _ r) =
 do status <- readIORef r
    pure $!
        case status of
            Running -> True
            _ -> False

-- | Start a new, suspended coroutine given a body function.
-- The body of the coroutine can use 'yield' to suspend with
-- a return value and wait to be resumed by 'resume'. The final
-- return value of the coroutine will generate the last return
-- value to 'resume' and will mark the continuation dead.
-- If the body of the coroutine fails with an exception the
-- coroutine will also be marked dead.
create :: (Coroutine i o -> i -> IO o) -> IO (Coroutine i o)
create k =
 do r <- newIORef undefined -- placeholder immediately replaced below
    IO \s0# ->
        case newPromptTag# s0# of
            (# s1# , p# #) ->
                case Coroutine p# r of
                    c ->
                        let status = Suspended \getI# sa# ->
                                        case getI# sa# of
                                            (# sb# , i #) ->
                                                unIO (k c i <* writeIORef r Dead) sb#
                        in unIO (c <$ writeIORef r status) s1#

-- | Suspend a running coroutine from inside its body. Handle must be
-- 'isRunning'.
yield :: Coroutine i o -> o -> IO i
yield (Coroutine p# r) o =
 do status <- readIORef r
    case status of
        Dead -> fail "yield on dead coroutine"
        Suspended{} -> fail "yield on suspended coroutine"
        Running -> IO (control0# p# \k -> unIO (o <$ writeIORef r (Suspended k)))

-- | Resume execution of a suspended coroutine returning either the
-- exception that terminated the coroutine or the value it yielded.
-- A coroutine that terminates with exception always becomes dead.
-- Handle must be 'isSuspended'.
resume :: Coroutine i o -> i -> IO (Either SomeException o)
resume (Coroutine p# r) i =
 do status <- readIORef r
    case status of
        Dead -> fail "resume on dead coroutine"
        Running -> fail "resume on running coroutine"
        Suspended k ->
         do writeIORef r Running
            result <- try (IO (prompt# p# (k (# , i #))))
            case result of
                Left{} -> writeIORef r Dead
                Right{} -> pure ()
            pure result

-- | Set a coroutine to be dead. This discards the coroutine's body
-- and does not signal to that body that it has been terminated in
-- in any way. Closing a coroutine is optional; the GC will also clean
-- up coroutines.
close :: Coroutine i o -> IO ()
close (Coroutine _ r) = writeIORef r Dead

-- | Wrap up a coroutine with the resume function so that each
-- application of the function resume the coroutine body.
wrap :: (Coroutine i o -> i -> IO o) -> IO (i -> IO o)
wrap k = resume' <$> create k

-- | Resume a coroutine like 'resume' but rethrow exceptions raised by the
-- coroutine if they occur.
resume' :: Coroutine i b -> i -> IO b
resume' c i =
 do result <- resume c i
    case result of
        Left e -> throwIO e
        Right x -> pure x        
