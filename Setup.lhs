#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where
import Distribution.Extra.Doctest ( defaultMainWithDoctests )
main :: IO ()
main = defaultMainWithDoctests "doctests"
\end{code}
