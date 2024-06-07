{-# Language ImportQualifiedPost, BlockArguments #-}
module Main (main) where

import Coroutine (Coroutine)
import Coroutine qualified
import Data.Foldable (for_)

main :: IO ()
main =
 do generator <-
        Coroutine.create \c _ ->
         do for_ [1..4::Int] \i ->
             do Coroutine.yield c (Just i)
                Coroutine.yield c (Just (-i))
            pure Nothing

    printAll generator

    s <- summer
    s 10
    s 11
    s 12

type Generator a = Coroutine () (Maybe a)

printAll :: Show a => Generator a -> IO ()
printAll generator =
 do mb <- Coroutine.resume' generator ()
    case mb of
        Nothing -> pure ()
        Just i ->
         do print i
            printAll generator

summer :: IO (Integer -> IO ())
summer = Coroutine.wrap (loop 0)
    where
        loop acc c i =
         do let i' = acc + i
            putStrLn ("Running total: " ++ show i')
            loop i' c =<< Coroutine.yield c ()
