module CompilerLLVM where

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
                                                )

import           AbsInstant
import           Utils

-- TODO: sdiv czy udiv sprawdzić

type Var = String
type Register = Integer
type Store = M.Map Var Register

data LLState = LLState
  { varToRegister :: Store
  , freeRegister :: Register
  }

type LLM a = StateT LLState (ExceptT String IO) a

compileLLVM :: String -> Program -> ExceptT String IO String
compileLLVM file (Prog prog) = evalStateT (gen prog) initialState
  where
    gen :: [Stmt] -> LLM String
    gen stmts = flip ($) "" <$> genExpr stmts
    initialState :: LLState
    initialState = LLState M.empty 0

genExpr :: [Stmt] -> LLM ShowS
genExpr ss = flip (.) llvmOutro <$> foldM go llvmIntro ss
  where
    go accCode stmt = do
        code <- transStmt stmt
        return (accCode . code)

transStmt :: Stmt -> LLM ShowS
transStmt x = case x of
    SAss ident exp -> transAss ident exp
    SExp exp       -> do
        (code, reg) <- transExp exp
        return $ code . printf reg

transAss :: Ident -> Exp -> LLM ShowS
transAss (Ident ident) exp = do
    (code, expReg) <- transExp exp
    state          <- get
    case M.lookup ident (varToRegister state) of
        Nothing -> do
            let register = freeRegister state
            modify
                (\st -> st
                    { varToRegister = M.insert ident register (varToRegister st)
                    , freeRegister  = register + 1
                    }
                )
            return $ code . alloca register . store expReg register
        Just register -> return $ code . store expReg register

transExp :: Exp -> LLM (ShowS, ShowS)
transExp (ExpLit integer      ) = return (nop, shows integer)
transExp (ExpVar (Ident ident)) = do
    state <- get
    case M.lookup ident (varToRegister state) of
        Nothing -> lift . throwE $ "Undefined variable" ++ ident
        Just id -> do
            let register = freeRegister state
            modify (\st -> st { freeRegister = register + 1 })
            return (load register id, regName register)
transExp (ExpAdd exp1 exp2) = transBinOp exp1 exp2 (showString "add")
transExp (ExpSub exp1 exp2) = transBinOp exp1 exp2 (showString "sub")
transExp (ExpMul exp1 exp2) = transBinOp exp1 exp2 (showString "mul")
transExp (ExpDiv exp1 exp2) = transBinOp exp1 exp2 (showString "sdiv")

transBinOp :: Exp -> Exp -> ShowS -> LLM (ShowS, ShowS)
transBinOp exp1 exp2 op = do
    (code1, expReg1) <- transExp exp1
    (code2, expReg2) <- transExp exp2
    register         <- gets freeRegister
    modify (\st -> st { freeRegister = register + 1 })
    return (code1 . code2 . binOp op expReg1 expReg2 register, regName register)


llvmIntro, llvmOutro :: ShowS
llvmIntro = showSify
    [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
    , "declare i32 @printf(i8*, ...)"
    , ""
    , "define void @printInt(i32 %x) {"
    , "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
    , "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
    , "  ret void"
    , "}"
    , ""
    , "define i32 @main() {"
    ]
llvmOutro = showString "  ret i32 0 \n}"

regName :: Integer -> ShowS
regName reg = showString "%r" . shows reg

alloca :: Register -> ShowS
alloca reg =
    showString "  " . regName reg . showString " = alloca i32" . align4endl

load :: Register -> Register -> ShowS
load to from =
    showString "  "
        . regName to
        . showString " = load i32, i32* "
        . regName from
        . align4endl

store :: ShowS -> Register -> ShowS
store val to =
    showString "  store i32 "
        . val
        . showString ", i32* "
        . regName to
        . align4endl

printf :: ShowS -> ShowS
printf val =
    showString "  call void @printInt(i32 " . val . showString ")" . endl

binOp :: ShowS -> ShowS -> ShowS -> Register -> ShowS
binOp op val1 val2 to =
    showString "  "
        . regName to
        . showString " = "
        . op
        . showString " i32 "
        . val1
        . showString ", "
        . val2
        . endl

align4endl :: ShowS
align4endl = showString ", align 4" . endl
