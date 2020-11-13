import           Data.Char                      ( toLower )
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

import           CompilerLLVM                   ( compileLLVM )
import           CompilerJVM                    ( compileJVM )

import           ParInstant
import           ErrM
import           PrintInstant

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree


check :: String -> String -> String -> IO String
check file mode s = case pProgram (myLexer s) of
    Bad err -> do
        hPutStrLn stderr $ "[Syntax error] " ++ err
        exitFailure
    Ok tree -> do
        -- showTree tree
        let compiler = if mode == "llvm" then compileLLVM else compileJVM
        genCode <- runExceptT $ compiler (takeBaseName file) tree
        case genCode of
            Left e -> do
                hPutStrLn stderr $ "[Compiler exception] " ++ e
                exitFailure
            Right strs -> return strs

saveFile :: String -> String -> String -> IO ()
saveFile filename mode content = do
    let extension = if mode == "llvm" then ".ll" else ".j"
    let name      = replaceExtension filename extension
    writeFile name content
    return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [mode, file] -> do
            let mode_ = map toLower mode
            readFile file >>= check file mode_ >>= saveFile file mode_
        _ -> do
            putStrLn "Usage: instc_<jvm\\llvm> <SourceFile> <jvm\\llvm>"
            exitFailure
