{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data.Sexp.Main where 
import Data.Sexp.Example 
import Data.Foldable  (Foldable (..))

import           System.Environment             (getArgs)


main = mainWith =<< getArgs

mainWith = \case
 _ -> do 
  print$ exampleSexp 
  print$ toList exampleSexp
