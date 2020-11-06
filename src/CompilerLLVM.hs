module CompilerLLVM where

import           Control.Monad.State           as MS
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Map                      as M
                                                -- ( Map
                                                -- , empty
                                                -- , lookup
                                                -- )

import           AbsInstant

transAss :: Ident -> Exp -> LLM String
transAss (Ident ident) exp = do
    (code, expReg) <- transExp exp
    state          <- MS.get
    case M.lookup ident (varToRegister state) of
        Nothing -> do
            let register = freeRegister state
            modify
                (\st -> st
                    { varToRegister = M.insert ident register (varToRegister st)
                    , freeRegister  = register + 1
                    }
                )
            return $ code ++ alloca register ++ store expReg register
        Just register -> return $ code ++ store expReg register

-- tutaj printować expy
transStmt :: Stmt -> LLM String
transStmt x = case x of
    SAss ident exp      -> transAss ident exp
    SExp a@(ExpLit int) -> do
        register <- gets freeRegister
        let register2 = register + 1
        modify (\st -> st { freeRegister = register2 + 1 })
        return
            $  alloca register
            ++ store (show int) register
            ++ load register2 register
            ++ printf (regName register2)
    SExp exp -> do
        (code, reg) <- transExp exp
        return $ code ++ printf reg


transExp :: Exp -> LLM (String, String)
transExp (ExpLit integer      ) = return ("\n", show integer)
transExp (ExpVar (Ident ident)) = do
    state <- MS.get
    case M.lookup ident (varToRegister state) of
        Nothing -> lift . throwE $ "Undefined variable"
        Just id -> do
            let register = freeRegister state
            modify (\st -> st { freeRegister = register + 1 })
            return (load register id, regName register)

transExp x =
    let
        go exp1 exp2 op = do
            (code1, expReg1) <- transExp exp1
            (code2, expReg2) <- transExp exp2
            register         <- gets freeRegister
            modify (\st -> st { freeRegister = register + 1 })
            return
                ( code1 ++ code2 ++ binOp op expReg1 expReg2 register
                , regName register
                )
    in  case x of
            ExpAdd exp1 exp2 -> go exp1 exp2 "add"
            ExpSub exp1 exp2 -> go exp1 exp2 "sub"
            ExpMul exp1 exp2 -> go exp1 exp2 "mul"
            ExpDiv exp1 exp2 -> go exp1 exp2 "sdiv"

genExpr :: [Stmt] -> LLM [String]
genExpr (s : stmts) = do
    genCode <- transStmt s
    rest    <- genExpr stmts
    return $ genCode : rest
genExpr [] = return [llvmOutro]

gen :: [Stmt] -> LLM [String]
gen stmts = do
    code <- genExpr stmts
    return $ llvmIntro : code

compileLLVM (Prog prog) = MS.runStateT (gen prog) initialState
    where initialState = LLState M.empty 0

type Var = String
type Register = Integer

type LLM a = MS.StateT LLState (ExceptT String IO) a

type Store = M.Map Var Register

data LLState = LLState
  { varToRegister :: Store
  , freeRegister :: Register
  }

regName :: Integer -> String
regName reg = "%r" ++ show reg

llvmIntro = unlines
    [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
    , ""
    , "declare i32 @printf(i8*, ...)"
    , "define void @printInt(i32 %x) {"
    , "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
    , "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
    , "ret void"
    , "}"
    , "define i32 @main() {"
    ]

llvmOutro = "ret i32 0 \n}"

alloca :: Register -> String
alloca reg = regName reg ++ " = alloca i32, align 4\n"

load :: Register -> Register -> String
load to from = regName to ++ " = load i32, i32* " ++ regName from ++ "\n"

store :: String -> Register -> String
store val to = "store i32 " ++ val ++ ", i32* " ++ regName to ++ ", align 4\n"

printf :: String -> String
printf val = "call void @printInt(i32 " ++ val ++ ")\n"

binOp :: String -> String -> String -> Register -> String
binOp op val1 val2 to =
    regName to ++ " = " ++ op ++ " i32 " ++ val1 ++ ", " ++ val2 ++ "\n"
