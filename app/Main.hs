{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Environment as Sys
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display

data Trans =
    NonCF Instruction -- no jumps, calls, or exit
  | Test Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

label :: Program -> LabeledProgram
label = zip [0..]

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
cfg prog = Set.unions $ map transfer $ label prog
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


------------------- The following is just for visualisation ------------------------

cfgToDot :: CFG -> String
cfgToDot graph = Set.toList graph >>= showTrans
  where
    showTrans (x, NonCF i, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (display i)
    showTrans (x, Test c r ir, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (showJump c r ir)
    showJump c r ir = display c <> " " <> display r <> ", " <> displayRegImm ir

dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"++
  "node [fontname=\"monospace\"];\n"++
  "node [shape=box];\n"++
  "edge [fontname=\"monospace\"];\n"

markNodes :: Program -> String
markNodes prog = concat $ mapMaybe mark $ label prog
  where
    mark (lab, Exit) = return $ printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab
    mark (lab, JCond _ _ _ _) = return $ printf "%d [shape=diamond];\n" lab
    mark _ = Nothing

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
          printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
          let edges = cfgToDot $ cfg prog
          writeFile dotFile (dotPrelude ++
                             edges ++
                             markNodes prog ++ "}")
          printf "Visualised the CFG in %s\n" dotFile
    _ ->
      putStrLn "Usage <EBPF_FILE>"
