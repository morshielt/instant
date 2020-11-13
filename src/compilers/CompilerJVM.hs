module CompilerJVM where

import           Control.Exception              ( assert )
import           Control.Monad.State            ( StateT
                                                , runStateT
                                                , get
                                                , gets
                                                , modify
                                                , liftIO
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , lift
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Data.Map                      as M
                                                ( Map
                                                , empty
                                                , lookup
                                                , insert
                                                , assocs
                                                , size
                                                )

import           AbsInstant

transAss :: Ident -> Exp -> JM String
transAss (Ident ident) exp = do
    (code, expStack, _) <- transExp exp
    state               <- get
    case M.lookup ident (varToIndex state) of
        Nothing -> do
            let index = fromIntegral $ M.size $ varToIndex state
            modify
                (\st -> st { varToIndex = M.insert ident index (varToIndex st)
                           , maxStack   = max expStack (maxStack st)
                           }
                )
            return (code ++ istore index)
        Just index -> return (code ++ istore index)

-- tutaj printować expy
transStmt :: Stmt -> JM String
transStmt x = case x of
    SAss ident exp -> transAss ident exp
    SExp exp       -> do
        (code, stack, _) <- transExp exp
        -- (liftIO . putStrLn) ("stack SExp: " ++ show stack)
        modify (\st -> st { maxStack = max 2 $ max stack (maxStack st) })
        return (code ++ printf)


transExp :: Exp -> JM (String, StackHeight, ExpDifficulty)
transExp (ExpLit integer      ) = return (ipush integer, 1, 1)
transExp (ExpVar (Ident ident)) = do
    varToIndex <- gets varToIndex
    case M.lookup ident varToIndex of
        Nothing    -> lift . throwE $ "Undefined variable"
        Just index -> return (iload index, 1, 1)

-- TODO: zabić swap dla + i * bo przemienne gówniaki są
transExp x =
    let
        go exp1 exp2 op optionalSwap = do
            (code1, stack1, diff1) <- transExp exp1
            (code2, stack2, diff2) <- transExp exp2

            let stack =
                    if stack1 == stack2 then stack1 + 1 else max stack1 stack2
            -- (liftIO . putStrLn) (show stack ++ "\n" ++ show exp1 ++ "\n" ++ show exp2)
            let code = if diff1 >= diff2
                    then code1 ++ code2 ++ op
                    else code2 ++ code1 ++ optionalSwap ++ op
            let diff = diff1 + diff2 + 1
            -- TODO: remove asserts
            if diff1 >= diff2
                then assert (stack1 >= stack2) return (code, stack, diff)
                else assert (stack1 <= stack2) return (code, stack, diff)
    in  case x of
            ExpAdd exp1 exp2 -> go exp1 exp2 "iadd\n" ""
            ExpSub exp1 exp2 -> go exp1 exp2 "isub\n" swap
            ExpMul exp1 exp2 -> go exp1 exp2 "imul\n" ""
            ExpDiv exp1 exp2 -> go exp1 exp2 "idiv\n" swap

genExpr :: [Stmt] -> JM [String]
genExpr (s : stmts) = do
    genCode <- transStmt s
    rest    <- genExpr stmts
    -- s       <- gets varToIndex
    -- (liftIO . putStrLn) (show $ assocs s)
    return (genCode : rest)
genExpr [] = return [jvmOutro]

gen :: String -> [Stmt] -> JM [String]
gen file stmts = do
    code  <- genExpr stmts
    state <- get
    return
        $ jvmIntro file
        : jvmLimits (maxStack state) (max 1 (M.size $ varToIndex state))
        : code

compileJVM file (Prog prog) = runStateT (gen file prog) initialState
    where initialState = JState M.empty 0
    -- TODO: od 0 czy od 1 rejestry!
    -- where initialState = JState M.empty 1 0

type Var = String
type Index = Integer
type StackHeight = Integer
type ExpDifficulty = Integer

type Store = M.Map Var Integer
data JState = JState
  { varToIndex :: Store
--   , freeIndex :: Index
  , maxStack :: StackHeight
  }
type JM a = StateT JState (ExceptT String IO) a

jvmIntro :: String -> String
jvmIntro className = unlines
    [ ".class  public " ++ className
    , ".super  java/lang/Object"
    , ""
    , "; standard initializer"
    , ".method public <init>()V"
    , "   aload_0"
    , "   invokespecial java/lang/Object/<init>()V"
    , "   return"
    , ".end method"
    , ".method public static main([Ljava/lang/String;)V"
    ]

-- jvmLimits :: StackHeight -> Integer -> String
jvmLimits stack locals =
    " .limit stack " ++ show stack ++ "\n .limit locals " ++ show locals ++ "\n"

jvmOutro = "return\n.end method"

iload :: Index -> String
iload index =
    let iload = if index `elem` [0 .. 3] then "iload_" else "iload "
    in  iload ++ show index ++ "\n"

istore :: Index -> String
istore index =
    let istore = if index `elem` [0 .. 3] then "istore_" else "istore "
    in  istore ++ show index ++ "\n"

ipush :: Integer -> String
ipush val =
    let push val | val == -1                    = "iconst_m1"
                 | val `elem` [0 .. 5]          = "iconst_"
                 | val >= -128 && val < 128     = "bipush "
                 | val >= -32768 && val < 32768 = "sipush "
                 | otherwise                    = "ldc "
    in  push val ++ show val ++ "\n"

printf :: String
printf = unlines
    [ "getstatic  java/lang/System/out Ljava/io/PrintStream;"
    , "swap"
    , "invokevirtual  java/io/PrintStream/println(I)V"
    ]

swap = "swap\n"

nop = ""
