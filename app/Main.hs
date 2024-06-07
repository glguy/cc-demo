{-# Language ImportQualifiedPost, BlockArguments #-}
module Main (main) where

import Coroutine (Coroutine)
import Coroutine qualified
import Data.Foldable ( for_ )

main :: IO ()
main =
 do generator <-
        Coroutine.create \c _ ->
         do for_ [0..9::Int] \i ->
                Coroutine.yield c (Just i)
            pure Nothing

    printAll generator

    s <- summer
    Coroutine.resume s 10
    Coroutine.resume s 11
    Coroutine.resume s 12

printAll :: Show a => Coroutine.Coroutine () (Maybe a) -> IO ()
printAll generator =
 do mb <- Coroutine.resume generator ()
    case mb of
        Nothing -> pure ()
        Just i ->
         do print i
            printAll generator

summer :: IO (Coroutine Integer ())
summer = Coroutine.create (loop 0)
    where
        loop acc c i =
         do let i' = acc + i
            putStrLn ("Running total: " ++ show i')
            loop i' c =<< Coroutine.yield c ()
