{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Display
import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()
import qualified System.Environment as Sys
import Text.Printf

data Trans
  = NonCF Instruction -- no jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int

type LabeledProgram = [(Int, Instruction)]

type CFG = Set (Label, Trans, Label)

label :: Program -> LabeledProgram
label = zip [0 ..]

r0 :: Reg
r0 = Reg 0

neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne
    Jne -> Jeq
    Jgt -> Jle
    Jge -> Jlt
    Jlt -> Jge
    Jle -> Jgt
    Jsgt -> Jsle
    Jsge -> Jslt
    Jslt -> Jsge
    Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

cfg :: Program -> CFG
cfg prog = Set.unions $ map transfer $ label prog
  where
    transfer (i, instr) =
      case instr of
        JCond cmp r ir off ->
          Set.singleton (i, Assert cmp r ir, i + 1 + fromIntegral off)
            `Set.union` Set.singleton (i, Assert (neg cmp) r ir, i + 1)
        Jmp off ->
          Set.singleton (i, Unconditional, i + 1 + fromIntegral off)
        Exit ->
          Set.empty
        _ ->
          Set.singleton (i, NonCF instr, i + 1)

------------------- The following is just for visualisation ------------------------

cfgToDot :: CFG -> String
cfgToDot graph = Set.toList graph >>= showTrans
  where
    showTrans (x, NonCF i, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (display i)
    showTrans (x, Unconditional, y) = printf "  %d -> %d [label=\"jmp\"];\n" x y
    showTrans (x, Assert c r ir, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (showJump c r ir)
    showJump c r ir = display c <> " " <> display r <> ", " <> display ir

dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"
    ++ "node [fontname=\"monospace\"];\n"
    ++ "node [shape=box];\n"
    ++ "edge [fontname=\"monospace\"];\n"

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
          writeFile
            dotFile
            ( dotPrelude
                ++ edges
                ++ markNodes prog
                ++ "}"
            )
          printf "Visualised the CFG in %s\n" dotFile
    _ ->
      putStrLn "Usage <EBPF_FILE>"