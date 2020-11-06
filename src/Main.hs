import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath.Posix          ( replaceExtension )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           Control.Monad.Except

import           CompilerLLVM

import           ParInstant
import           AbsInstant

import           ErrM

import           Control.Monad                  ( when )
import           PrintInstant

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree


check :: String -> IO String
check s = case pProgram (myLexer s) of
    Bad err -> do
        hPutStrLn stderr $ "[Syntax error] " ++ err
        exitFailure
    Ok tree -> do
        showTree tree
        genCode <- runExceptT $ compileLLVM tree
        case genCode of
            Left e -> do
                hPutStrLn stderr $ "[Compiler exception] " ++ e
                exitFailure
            Right (strs, _) -> return $ unlines strs
        -- x tree
        -- compileLLVM
        -- tcRes <- runExceptT $ runTypeChecker tree
        -- case tcRes of
        --     Left e -> do
        --         hPutStrLn stderr $ "[Typecheck exception] " ++ e
        --         exitFailure
        --     Right _ -> do
        --         res <- runExceptT $ runInterpreter tree
        --         case res of
        --             Left e -> do
        --                 hPutStrLn stderr $ "[Runtime exception] " ++ e
        --                 exitFailure
        --             Right _ -> return ()


saveFile :: String -> String -> IO ()
saveFile filename content = do
    let name = replaceExtension filename ".ll"
    writeFile name content
    return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> readFile file >>= check >>= saveFile file
            -- do
            -- cont <- readFile file
            -- res  <- check cont
            -- saveFile file res
        _      -> do
            putStrLn "Usage: ??? <SourceFile>"
            exitFailure
