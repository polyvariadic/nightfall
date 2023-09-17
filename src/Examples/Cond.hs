{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Examples.Cond ( simpleIfProg
                     , ifVarProg
                     , simpleInfProg
                     ) where

import Nightfall.Lang.Types
import Nightfall.Lang.Syntax.DotRecord

-- * Simple program that uses one if / else statement

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = if 4 == 8
           then 10
           else 20
-}

-- | EDSL version
simpleIfBody :: Body ()
simpleIfBody = do
    comment "Simple, stupid and trivial program that makes uses of a condition"
    comment "if (4 == 8) then return 10 else return 20"
    comment "Should return 20"
    ifElse (eq 4 8)
        (ret 10)
        (ret 20)

simpleIfProg :: ZKProgram
simpleIfProg = mkSimpleProgram "simple if" simpleIfBody

-- * Simple program that uses one if / else statement on a moderately complex computation involving variables

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = let a = 145
               b = 79
               target = 203
               sum = a + b
               okVal = 10
               nokVal = 20
            in if sum == target
               then okVal
               else nokVal
-}

-- | EDSL version
ifVarBody :: Body ()
ifVarBody = do
    comment "Makes a if/else comparison on a moderately complex computation, involving variables"
    comment "It sums a=145 + b=79 and compares equality with target=203."
    comment "If equal, it returns okVal=10, otherwise nokVal=20"
    comment "It should return 20"
    Var <- declare.a 145
    Var <- declare.b 79
    Var <- declare.target 203
    Var <- declare.sum $ #a + #b
    Var <- declare.okVal 10
    Var <- declare.nokVal 20
    ifElse (#sum `eq` #target)
        (ret #okVal)
        (ret #nokVal)

ifVarProg :: ZKProgram
ifVarProg = mkSimpleProgram "If with vars" ifVarBody

-- * Simple program that compares two fixed numbers stored in variables and return the lowest

{-
simpleInf :: Felt
simpleInf = let n1 = 4238
                n2 = 21987
            in if n1 <= n2
               then n1
               else n2
-}

-- | EDSL version
simpleInfBody :: Body ()
simpleInfBody = do
    comment "if n1=4238 <= n2=21987 then n1 else n2."
    comment "It should return 4238"
    emptyLine
    Var <- declare.n1 4238
    Var <- declare.n2 21987
    ifElse (#n1 `lte` #n2)
        (ret #n1)
        (ret #n2)

simpleInfProg :: ZKProgram
simpleInfProg = mkSimpleProgram "simple inf" simpleInfBody
