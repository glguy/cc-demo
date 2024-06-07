{-# Language ImportQualifiedPost, BlockArguments #-}
module Main (main) where

import Coroutine qualified
import Data.Foldable ( for_ )

main :: IO ()
main =
 do generator <-
        Coroutine.new \c _ ->
         do for_ [0..9::Int] \i ->
                Coroutine.yield c (Just i)
            pure Nothing

    printAll generator

printAll :: Show a => Coroutine.Coroutine () (Maybe a) -> IO ()
printAll generator =
 do mb <- Coroutine.resume generator ()
    case mb of
        Nothing -> pure ()
        Just i ->
         do print i
            printAll generator

