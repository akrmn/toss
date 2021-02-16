{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Toss

main :: IO ()
main = do
  pure ()

mightThrowQuxErr ::
  Throws QuxErr m =>
  m String
mightThrowQuxErr =
  (mightThrowSomeErrors >> pure "there were no errors")
    & handle (\FooErr -> pure "there was a foo error")
    & handle (\BarErr -> pure "there was a bar error")

mightThrowSomeErrors ::
  Throws FooErr m =>
  Throws BarErr m =>
  Throws QuxErr m =>
  m ()
mightThrowSomeErrors = mightThrowSomeErrors

data FooErr = FooErr

data BarErr = BarErr

data QuxErr = QuxErr
