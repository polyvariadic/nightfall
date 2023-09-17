{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Fun ( collatzFixedProg
                    , collatzPrivProg
                    ) where

import Nightfall.Lang.Types
import Nightfall.Lang.Syntax.DotRecord

-- * The Collatz Sequence, start from 10

-- Haskell version
{-
collatzFixed :: Felt
collatzFixed = collatz' 10
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzFixedStmts :: Body ()
collatzFixedStmts = do
    comment "Compute the Collatz sequence, starting from a fixed position: 10 and returns the length of the sequence."
    comment "It should return 7"
    emptyLine
    Var <- declare.start 10
    Var <- declare.length 1
    Var <- declare.n #start
    while (#n `gt` 1) $ do
        set.length $ #length + 1
        ifElse (isOdd #n)
            (set.n $ #n * 3 + 1)
            (set.n $ #n `div'` 2)
    ret #length

collatzFixedProg :: ZKProgram
collatzFixedProg = mkSimpleProgram "Fixed Collatz (10)" collatzFixedStmts

-- * The Collatz Sequence, but the starting number comes from the private inputs

-- Haskell version
{-
collatzPriv :: Felt -> Felt
collatzPriv n = collatz' n
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzPrivStmts :: Body ()
collatzPrivStmts = do
    comment "Compute the Collatz sequence, starting position taken from secret input"
    comment "It returns the length of the sequence"
    emptyLine
    Var @Felt <- declare.start nextSecret
    Var <- declare.n #start
    Var <- declare.length 1
    while (#n `gt` 1) $ do
        add.mut.length 1
        ifElse (isOdd #n)
            (mut.n $ \n -> n * 3 + 1)
            (mut.n $ \n -> n `div'` 2)
    ret #length

collatzPrivProg :: ZKProgram
collatzPrivProg = mkZKProgram "collatz private" collatzPrivStmts [] "src/Examples/collatz_secrets.inputs"
