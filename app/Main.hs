module Main where

import qualified System.Environment as Sys

import Ebpf.Asm
import Ebpf.AsmParser

data Trans =
    NonCF Instruction -- no jumps, calls, or exit
  | Test Jcmp Reg RegImm

r0 :: Reg
r0 = Reg 0

unconditional :: Trans
unconditional = Test Jeq r0 (Left r0)

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occured while parsing:"
          print err
        Right prog ->
          putStrLn ("We got a program with "++(show $ length prog)++" instructions")
    _ ->
      putStrLn "Usage <EBPF_FILE>"
