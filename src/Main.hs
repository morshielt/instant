import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath.Posix          ( replaceExtension
                                                , takeBaseName
                                                )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           Control.Monad.Except
import           Control.Monad                  ( when )

import           CompilerLLVM
import           CompilerJVM

import           ParInstant
import           ErrM
import           PrintInstant

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree


check :: String -> String -> IO String
check file s = case pProgram (myLexer s) of
    Bad err -> do
        hPutStrLn stderr $ "[Syntax error] " ++ err
        exitFailure
    Ok tree -> do
        showTree tree
        genCode <- runExceptT $ compileJVM (takeBaseName file) tree
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
    -- let name = replaceExtension filename ".ll"
    let name = replaceExtension filename ".j"
    writeFile name content
    return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> readFile file >>= check file >>= saveFile file
        _      -> do
            putStrLn "Usage: ??? <SourceFile>"
            exitFailure
