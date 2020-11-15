module CompilerJVM
    ( compileJVM
    )
where

import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                , gets
                                                , modify
                                                , foldM
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
                                                , size
                                                )

import           AbsInstant
import           Utils


type Var = String
type Location = Integer
type Store = M.Map Var Location

type Stack = Integer
data JState = JState
  { varToLoc :: Store
  , maxStack :: Stack
  }

type JM a = StateT JState (ExceptT String IO) a

compileJVM :: String -> Program -> ExceptT String IO String
compileJVM file (Prog prog) = evalStateT (gen file prog) initialState
  where
    gen :: String -> [Stmt] -> JM String
    gen file stmts = do
        code  <- genExpr stmts
        state <- get
        let stack  = maxStack state
        let locals = max 1 (fromIntegral $ M.size $ varToLoc state)
        return $ jvmIntro file . jvmLimits stack locals . code $ ""

    initialState :: JState
    initialState = JState M.empty 0

genExpr :: [Stmt] -> JM ShowS
genExpr ss = flip (.) jvmOutro <$> foldM go nop ss
  where
    go accCode stmt = do
        code <- transStmt stmt
        return (accCode . code)

transStmt :: Stmt -> JM ShowS
transStmt (SAss ident exp) = transAss ident exp
transStmt (SExp exp      ) = do
    (code, stack) <- transExp exp
    modify (\st -> st { maxStack = max printfStack $ max stack $ maxStack st })
    return (code . printf)
    where printfStack = 2

transAss :: Ident -> Exp -> JM ShowS
transAss (Ident ident) exp = do
    (code, stack) <- transExp exp
    modify (\st -> st { maxStack = max stack $ maxStack st })
    state <- get
    index <- case M.lookup ident (varToLoc state) of
        Nothing -> do
            let index = fromIntegral $ M.size $ varToLoc state
            modify (\st -> st { varToLoc = M.insert ident index $ varToLoc st })
            return index
        Just index -> return index
    return (code . istore index)

transExp :: Exp -> JM (ShowS, Stack)
transExp (ExpLit integer      ) = return (ipush integer, 1)
transExp (ExpVar (Ident ident)) = do
    varToLoc <- gets varToLoc
    case M.lookup ident varToLoc of
        Nothing    -> lift . throwE $ "Undefined variable `" ++ ident ++ "`"
        Just index -> return (iload index, 1)
transExp (ExpAdd exp1 exp2) = transBinOp exp1 exp2 (showString "  iadd\n") nop
transExp (ExpSub exp1 exp2) = transBinOp exp1 exp2 (showString "  isub\n") swap
transExp (ExpMul exp1 exp2) = transBinOp exp1 exp2 (showString "  imul\n") nop
transExp (ExpDiv exp1 exp2) = transBinOp exp1 exp2 (showString "  idiv\n") swap

transBinOp :: Exp -> Exp -> ShowS -> ShowS -> JM (ShowS, Stack)
transBinOp exp1 exp2 op optionalSwap = do
    (code1, stack1) <- transExp exp1
    (code2, stack2) <- transExp exp2
    if stack2 <= stack1
        then return (code1 . code2 . op, max stack1 (stack2 + 1))
        else return (code2 . code1 . optionalSwap . op, max (stack1 + 1) stack2)


jvmIntro :: String -> ShowS
jvmIntro className = showSify
    [ ".class  public " ++ className
    , ".super  java/lang/Object"
    , ""
    , ".method public <init>()V"
    , "  aload_0"
    , "  invokespecial java/lang/Object/<init>()V"
    , "  return"
    , ".end method"
    , ""
    , ".method public static main([Ljava/lang/String;)V"
    ]

jvmLimits :: Stack -> Integer -> ShowS
jvmLimits stack locals =
    showString ".limit stack "
        . shows stack
        . endl
        . showString ".limit locals "
        . shows locals
        . endl

jvmOutro :: ShowS
jvmOutro = showSify ["  return", ".end method"]

iload :: Location -> ShowS
iload index =
    let iload = if index `elem` [0 .. 3] then "  iload_" else "  iload "
    in  showString iload . shows index . endl

istore :: Location -> ShowS
istore index =
    let istore = if index `elem` [0 .. 3] then "  istore_" else "  istore "
    in  showString istore . shows index . endl

ipush :: Integer -> ShowS
ipush val =
    let push val | val == -1                              = "  iconst_m1"
                 | val `elem` [0 .. 5]                    = "  iconst_"
                 | val >= -byteRange && val < byteRange   = "  bipush "
                 | val >= -shortRange && val < shortRange = "  sipush "
                 | otherwise                              = "  ldc "
    in  showString (push val) . shows val . endl
  where
    byteRange  = 128
    shortRange = 32768

printf, swap :: ShowS
printf = showSify
    [ "  getstatic  java/lang/System/out Ljava/io/PrintStream;"
    , "  swap"
    , "  invokevirtual  java/io/PrintStream/println(I)V"
    ]
swap = showString "  swap\n"
