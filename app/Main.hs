{-# LANGUAGE LambdaCase #-}

module Main where

import qualified System.Environment as Sys
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Text.Printf


import Ebpf.Asm
import Ebpf.AsmParser

data Trans =
    NonCF Instruction -- no jumps, calls, or exit
  | Test Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type CFG = Set (Label, Trans, Label)

r0 :: Reg
r0 = Reg 0

unconditional :: Trans
unconditional = Test Jeq r0 (Left r0)

neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne ; Jne -> Jeq
    Jgt -> Jle; Jge -> Jlt; Jlt -> Jge; Jle -> Jgt
    Jsgt -> Jsle; Jsge -> Jslt; Jslt -> Jsge; Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

cfg :: Program -> CFG
cfg prog = Set.unions $ map transfer $ zip [0..] prog
  where
    transfer (i, instr) =
      case instr of
        JCond cmp r ir off ->
          Set.singleton (i, Test cmp r ir, i+1+fromIntegral off)
          `Set.union`
          Set.singleton (i, Test (neg cmp) r ir, i+1)
        Jmp off ->
          Set.singleton (i, unconditional, i+1+fromIntegral off)
        Exit ->
          Set.empty
        Call _ ->
          Set.empty
        _ ->
          Set.singleton (i, NonCF instr, i+1)

cfgToDot :: CFG -> [String]
cfgToDot graph = map showTrans $ Set.toList graph
  where
    showTrans (x, NonCF i, y) = concat [show x, " -> ", show y,
                                        " [label=\"", show i, "\"];"]
    showTrans (x, Test c r ir, y) = concat [show x, " -> ", show y,
                                        " [label=\"", show c, " ", show r, " ", show ir, "\"];"]

dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"++
  "node [fontname=\"monospace\"];\n"++
  "node [shape=box];\n"++
  "edge [fontname=\"monospace\"];\n"

exitNodes :: Program -> String
exitNodes prog =
  exits >>= (\(lab,_) -> printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab)
  where
    exits :: [(Int, Instruction)]
    exits = filter (\case (_, Exit) -> True; _ -> False) $ zip [0..] prog


main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    [ebpfFile, dotFile] -> do
      res <- parseFromFile ebpfFile
      case res of
        Left err -> do
          putStrLn "Some sort of error occurred while parsing:"
          print err
        Right prog -> do
          putStrLn ("We got a program with "++(show $ length prog)++" instructions")
          let edges = cfgToDot $ cfg prog
          writeFile dotFile (dotPrelude ++
                             intercalate "\n" edges ++
                             exitNodes prog ++ "}")

    _ ->
      putStrLn "Usage <EBPF_FILE>"
