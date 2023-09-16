{-# LANGUAGE GADTs #-}

module Nightfall.Lang.Syntax.Default where

import Nightfall.Lang.Types
import Nightfall.Lang.Internal.Types

-- simpleVar3Body :: Body ()
-- simpleVar3Body = do
--     comment "Rewrite on the same variable several times"
--     comment "a = 10, b = 20, a = 50, a + b. Should return 70"
--     Felt <- declare.a 10
--     Felt <- declare.b 20
--     set.a 50
--     ret $ get.a + get.b

-- varF :: VarName -> Expr Felt

-- assignVarF :: VarName -> Expr Felt -> Body ()
-- assignVarF varname (Expr e) = statement $ AssignVar varname e

data DeclType a where
    Felt :: DeclType Felt
    Bool :: DeclType Bool

data Var a = Var
    { _varDeclType :: DeclType a
    , _varName     :: VarName
    }

declare :: DeclType a -> VarName -> Expr a -> Body (Var a)
declare declType varName expr = do
    statement . DeclVariable varName $ unExpr expr
    pure $ Var declType varName

get :: Var a -> Expr a
get (Var declType name) = case declType of
    Felt -> Expr $ VarF name
    Bool -> Expr $ VarB name

set :: Var a -> Expr a -> Body ()
set var expr = statement $ AssignVar (_varName var) (unExpr expr)

mut :: Var a -> (Expr a -> Expr a) -> Body ()
mut var f = set var . f $ get var
