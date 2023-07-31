{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Nightfall.Targets.Miden ( Context(..)
                               , defaultContext
                               , Config(..)
                               , defaultConfig
                               , transpile
                               ) where

import Nightfall.Lang.Internal.Types as NFTypes
import Nightfall.Lang.Types
import Nightfall.MASM.Types as MASM
import Control.Monad ( when )
import Control.Monad.State
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import Data.List ( singleton )
import Data.Coerce ( coerce )

-- | Transpilation configuration options
data Config = Config {
      cgfTraceVariablesDecl :: Bool     -- ^ Whether or not adding comments when declaring variables
    , cfgTraceVariablesUsage :: Bool    -- ^ Whether or not adding comments when using ("calling") variables
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cgfTraceVariablesDecl = False
    , cfgTraceVariablesUsage = False
}

-- | Context for the transpilation, this is the state of the State Monad in which transpilation happens
data Context = Context
    { progName :: Text                   -- ^ Name of the program, for outputs logs, etc.
    , memPos :: MemoryIndex                -- ^ Next free indice in Miden's global memory
    , variables :: Map Text MemoryIndex  -- ^ Variables in the EDSL are stored in Miden's random
                                             -- access memory, we keep a map to match them
    , config :: Config                     -- ^ Transpilation configuration options
    }

defaultContext :: Context
defaultContext = Context
    { progName = "<unnamed-program>"
    , memPos = case toMemoryIndex 0 of
            Nothing  -> error "Internal error: wrong index for initial memory position"
            Just pos -> pos
    , variables = Map.empty
    , config = defaultConfig
    }

-- | Entry point: transpile a EDSL-described ZK program into a Miden Module
transpile :: ZKProgram -> State Context Module
transpile zkProg = do
    warning <- transpileStatement . coerce $ comment "This program was generated by Nightfall (https://github.com/qredo/nightfall), avoid editing by hand.\n"
    modify $ \ctx -> ctx { progName = pName zkProg }
    midenInstr <- concat <$> mapM (transpileStatement . coerce) (pStatements zkProg)
    return $ Module { moduleImports = [] -- No import for now (TODO)
                    , moduleProcs = Map.empty -- No procs either (TODO)
                    , moduleProg = Program (head warning : midenInstr)
                    }

transpileStatement :: Statement_ -> State Context [Instruction]
transpileStatement (NFTypes.Comment str) = return . singleton . MASM.Comment . Text.pack $ str
transpileStatement (IfElse cond ifBlock elseBlock) = do
    ifBlock' <- concat <$> mapM transpileStatement ifBlock
    elseBlock' <- concat <$> mapM transpileStatement elseBlock
    -- Not entirely sure, Miden talks about a "a small, but non-negligible overhead", but they don't say.
    -- I'm supposing it takes a pop/drop operation + a comparison
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.If ifBlock' elseBlock' ]
transpileStatement (NFTypes.While cond body) = do
    body' <- concat <$> mapM transpileStatement body
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.While $ body' <> cond' ]
-- | Declaring a variable loads the expression into Miden's global memory, and we track the index in memory
transpileStatement (DeclVariable varname e) = do
    vars <- gets variables
    pos <- gets memPos
    cfg <- gets config

    -- Check if this variable hasn't already been declared
    when (Map.member varname vars) $ do
        error $ "Variable \"" ++ varname ++ "\" has already been declared!"
    
    -- Insert the record
    let vars' = Map.insert varname pos vars

    -- Update the context
    modify $ \s -> s
        { memPos = case toMemoryIndex $ unMemoryIndex pos + 1 of
                Nothing   -> error "Out of Miden's memory"
                Just pos' -> pos'
        , variables = vars'
        }

    -- Trace the variable declaration if configured
    let traceVar = [MASM.Comment $ "var " <> Text.pack varname | cgfTraceVariablesDecl cfg]

    -- Transpile the variable value
    e' <- transpileExpr e

    -- Return instruction for the variable value and instruction to store the value in global memory
    return $ e' <> traceVar <> [ MASM.MemStore . Just $ pos ]

-- | Assigning a new value to a variable if just erasing the memory location
transpileStatement (AssignVar varname e) = do
    vars <- gets variables
    shouldTrace <- gets (cfgTraceVariablesUsage . config)
    -- Fetch the memory location for that variable.
    case Map.lookup varname vars of
        Nothing -> error $ concat @[]
            [ "Variable \""
            , varname
            , "\" has not been declared before: can't assign value"
            ]
        Just pos -> do
            -- Trace the variable usage if configured
            let traceVar = [MASM.Comment $ "var " <> Text.pack varname | shouldTrace]
            e' <- transpileExpr e
            return $ e' <> traceVar <> [ MASM.MemStore . Just $ pos ]

-- | A (naked) function call is done by pushing the argument on the stack and caling the procedure name
transpileStatement (NakedCall fname args) = do
    args' <- concat <$> mapM transpileExpr args
    return $ args' <> [ MASM.Exec . Text.pack $ fname ]

transpileStatement (Return mE) = case mE of
    -- an empty return statement doesn't correspond to an action in Miden
    Nothing -> return []
    -- When we do have a value, we simply push it to the stack
    Just e -> transpileExpr e

transpileStatement EmptyLine = return . singleton $ MASM.EmptyL

-- | Transpile an unary operation.
transpileUnOp :: UnOp -> [Instruction]
transpileUnOp NFTypes.Not   = [ MASM.Not   ]
transpileUnOp NFTypes.IsOdd = [ MASM.IsOdd ]

-- | Transpile a binary operation.
transpileBinOp :: BinOp -> [Instruction]
-- Arithmetics operations are matched to their corresponding Miden operations
transpileBinOp NFTypes.Add    = [ MASM.Add Nothing ]
transpileBinOp NFTypes.Sub    = [ MASM.Sub Nothing ]
transpileBinOp NFTypes.Mul    = [ MASM.Mul Nothing ]
transpileBinOp NFTypes.Div    = [ MASM.Div Nothing ]
transpileBinOp NFTypes.IDiv32 = [ MASM.IDiv ]
transpileBinOp Equal          = [ MASM.Eq Nothing ]
transpileBinOp Lower          = [ MASM.Lt ]
transpileBinOp LowerEq        = [ MASM.Lte ]
transpileBinOp Greater        = [ MASM.Gt ]
transpileBinOp GreaterEq      = [ MASM.Gte ]

-- TODO: range check, etc.
transpileExpr :: Expr_ -> State Context [Instruction]
-- transpileExpr (Lit _) = error "Can't transpile standalone literal" -- should be simply push it to the stack??
-- transpileExpr (Bo _)  = error "Can't transpile standalone boolean"
-- | Literals are simply pushed onto the stack
transpileExpr (Lit felt) = do
    return . singleton . Push $ felt
transpileExpr (Bo bo) = do
    let felt = if bo then 1 else 0
    return . singleton . Push $ felt
-- Using a variable means we fetch the value from (global) memory and push it to the stack
transpileExpr (VarF varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- gets variables
    case Map.lookup varname vars of
        Nothing -> error $ "Felt variable \"" ++ varname ++ "\" unknown (undeclared)"
        Just idx -> do
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar = [MASM.Comment $ "var " <> Text.pack varname <> " (felt)" | shouldTrace]
            return $ traceVar <> [MemLoad . Just $ idx]
transpileExpr (VarB varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- gets variables
    case Map.lookup varname vars of
        Nothing -> error $ "Boolean variable \"" ++ varname ++ "\" unknown (undeclared)"
        Just idx -> do
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar = [MASM.Comment $ "var " <> Text.pack varname <> " (bool)" | shouldTrace]
            return $ traceVar <> [MemLoad . Just $ idx]
transpileExpr (UnOp op e) = do
    es <- transpileExpr e
    return $ es <> transpileUnOp op
transpileExpr (BinOp op e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    return $ e1s <> e2s <> transpileBinOp op
transpileExpr NextSecret =
    case toStackIndex 1 of
        Nothing  -> error "Internal error: wrong index for 'AdvPush'"
        Just idx -> return . singleton . MASM.AdvPush $ idx

transpileExpr FCall{} = error "transpileExpr::TODO"
