module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

import AbsGramma ( Program )
import LexGramma ( Token )
import ParGramma ( pProgram, myLexer )
import Evaluator (runInterpreter)
import TypeChecker (runTypeChecker)
import System.IO (hPutStr, stderr)

type ParseFun a = [Token] -> Either String a

typeCheck :: Program -> IO ()
typeCheck p = return ()

run :: ParseFun Program -> String -> IO ()
run p s =
  case p (myLexer s) of
    Left err -> do
      putStrLn $ "\nParse failed: " ++ err
      exitFailure
    Right tree -> do
      let res = runTypeChecker tree
      case res of
        Left err -> do
          hPutStr stderr err
          exitFailure
        Right x0 -> do
          ret <- runInterpreter tree
          case ret of
            Left err -> do
              hPutStr stderr err
              exitFailure
            Right x0 -> return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> getContents >>= run pProgram
    [progName] -> readFile progName >>= run pProgram
    _ -> putStrLn "Wrong nr of arguments; usage: ./interpreter <progName> or <progName> | ./interpreter"
