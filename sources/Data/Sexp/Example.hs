{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-| (see source) 

-}
module Data.Sexp.Example where 
import Data.Sexp 


exampleSexp :: Sexp () String
exampleSexp = ["f", "x", ["g", "y"], "z"]

