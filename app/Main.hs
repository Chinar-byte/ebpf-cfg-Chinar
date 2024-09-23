{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Environment as Sys
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
-- import Interval (Interval (..))

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()

data Trans =
    NonCF Instruction -- no jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

label :: Program -> LabeledProgram
label = zip [0..]

-- r0 :: Reg
-- r0 = Reg 0

-- r1 :: Reg
-- r1 = Reg 1

-- r2 :: Reg
-- r2 = Reg 2

-- r3 :: Reg
-- r3 = Reg 3

-- r4 :: Reg
-- r4 = Reg 4

-- r5 :: Reg
-- r5 = Reg 5

-- r6 :: Reg
-- r6 = Reg 6

-- r7 :: Reg
-- r7 = Reg 7

-- r8 :: Reg
-- r8 = Reg 8

-- r9 :: Reg
-- r9 = Reg 9

-- r10 :: Reg
-- r10 = Reg 10

-- type Memory = Array Int Instruction 

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
          Set.singleton (i, Assert cmp r ir, i+1+fromIntegral off)
          `Set.union`
          Set.singleton (i, Assert (neg cmp) r ir, i+1)
        Jmp off ->
          Set.singleton (i, Unconditional, i+1+fromIntegral off)
        Exit ->
          Set.empty
        _ ->
          Set.singleton (i, NonCF instr, i+1)


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

-- main :: IO ()
-- main = do
--   args <- Sys.getArgs
--   case args of
--     [ebpfFile, dotFile] -> do
--       res <- parseFromFile ebpfFile
--       case res of
--         Left err -> do
--           putStrLn "Some sort of error occurred while parsing:"
--           print err
--         Right prog -> do
--           printf "The eBPF file %s has %d instructions\n" ebpfFile (length prog)
--           let edges = cfgToDot $ cfg prog
--           writeFile dotFile (dotPrelude ++
--                              edges ++
--                              markNodes prog ++ "}")
--           printf "Visualised the CFG in %s\n" dotFile
--     _ ->
--       putStrLn "Usage <EBPF_FILE>"



-- Definizione di Reg, RegOrImm e Jcmp
type Reg = String
data RegOrImm = Imm Double | R Reg deriving (Eq, Show)
data Jcmp = Eq | Neq | Lt | Leq | Gt | Geq deriving (Eq, Show)

-- Definizione dell'ambiente e della memoria
type Memory = Map.Map Int Interval
type Environment = (Memory, Map.Map Reg Interval)

data Interval = Bottom                       -- undefined
    -- | Interval Integer Integer               -- [l, u]
    | Top                                    -- T (any possible value)
    | Interval Double Double           -- negative infinity to the positive infinity 
    | Intervals Interval Interval
    deriving (Eq, Show)

-- Funzione di inizializzazione della memoria
initMemory :: Memory
initMemory = Map.fromList [(i, Bottom) | i <- [0..511]]

-- Funzione di lookup per la memoria
lookupMemory :: Memory -> Int -> Interval
lookupMemory mem i = Map.findWithDefault Bottom i mem

-- Funzione di aggiornamento della memoria
updateMemory :: Memory -> Int -> Interval -> Memory
updateMemory mem i interval = Map.insert i interval mem

-- Funzione di inizializzazione dei registri
initRegisters :: Map.Map Reg Interval
initRegisters = Map.fromList [(r, Bottom) | r <- ["r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10"]]

-- Funzione di trasferimento per il MOV
transferMov :: Reg -> Interval -> Environment -> Environment
transferMov reg interval (mem, regs) =
    let newRegs = Map.insert reg interval regs
    in (mem, newRegs)

-- Funzione di trasferimento per ADD
transferAdd :: Reg -> Reg -> Reg -> Environment -> Environment
transferAdd reg ri rj (mem, regs) =
    let intervalRi = Map.findWithDefault Bottom ri regs
        intervalRj = Map.findWithDefault Bottom rj regs
        newInterval = intervalAdd intervalRi intervalRj
        newRegs = Map.insert reg newInterval regs
    in (mem, newRegs)

-- Funzione di aggiornamento memoria con assegnazione
transferMemAssign :: Reg -> Reg -> Environment -> Environment
transferMemAssign addrReg srcReg (mem, regs) =
    let intervalAddr = Map.findWithDefault Bottom addrReg regs
        intervalSrc = Map.findWithDefault Bottom srcReg regs
        newMem = case intervalAddr of
                   Interval l u | l == u -> updateMemory mem (round l) intervalSrc
                   _ -> mem -- if it's not a single value address, leave memory unchanged
    in (newMem, regs)

-- Funzione di trasferimento per Assert (salto condizionale)
transferAssert :: Jcmp -> Reg -> RegOrImm -> Environment -> (Environment, Environment)
transferAssert cmp reg (R rj) (mem, regs) =
    let intervalRi = Map.findWithDefault Bottom reg regs
        intervalRj = Map.findWithDefault Bottom rj regs
        -- Branches for true and false conditions (semplificazione, dipende dal confronto)
        trueBranch = (mem, Map.insert reg (intervalIntersection intervalRi intervalRj) regs)
        falseBranch = (mem, Map.insert reg (intervalMinus intervalRi intervalRj) regs)
    in (trueBranch, falseBranch)
transferAssert _ _ _ env = (env, env) -- Altri casi per semplificare

-- Esempio di operazioni sugli intervalli (aggiunta, intersezione, ecc.)
intervalAdd :: Interval -> Interval -> Interval
intervalAdd (Interval l1 u1) (Interval l2 u2) = Interval (l1 + l2) (u1 + u2)
intervalAdd _ _ = Top

intervalIntersection :: Interval -> Interval -> Interval
intervalIntersection (Interval l1 u1) (Interval l2 u2) = Interval (max l1 l2) (min u1 u2)
intervalIntersection _ _ = Bottom

intervalMinus :: Interval -> Interval -> Interval
intervalMinus (Interval l1 u1) (Interval l2 u2) = Interval (l1 - u2) (u1 - l2)
intervalMinus _ _ = Bottom

-- Esecuzione del CFG
evalCFG :: CFG -> Environment -> Environment
evalCFG cfg env = 
    foldl (\env (fromNode, trans, toNode) -> evalTrans trans env) env (Set.toList cfg)

-- Valutazione delle transizioni
evalTrans :: Trans -> Environment -> Environment
evalTrans (NonCF instr) env = evalInstr instr env
evalTrans Unconditional env = env -- Salto incondizionato, semplice transizione
evalTrans (Assert cmp reg regImm) env =
    let (trueEnv, falseEnv) = transferAssert cmp reg regImm env
    in trueEnv -- Per ora seguiamo solo il ramo true

-- Esecuzione di una singola istruzione
evalInstr :: Instruction -> Environment -> Environment
evalInstr (Mov ri (Imm n)) env = transferMov ri (Interval n n) env
evalInstr (Mov ri (R rj)) env = 
    let (_, regs) = env
        intervalRj = Map.findWithDefault Bottom rj regs
    in transferMov ri intervalRj env
evalInstr (Add ri rj rk) env = transferAdd ri rj rk env
evalInstr (MemAssign ri rj) env = transferMemAssign ri rj env
evalInstr _ env = env -- altri casi da aggiungere per istruzioni restanti

-- Esempio di test del CFG con istruzioni
exampleCFG :: CFG
exampleCFG = Set.fromList [
    (0, NonCF (Mov "r1" (Imm 5)), 1),
    (1, NonCF (Add "r2" "r1" "r1"), 2),
    (2, Assert Eq "r2" (R "r1"), 3)
    ]

-- Esempio di esecuzione
main :: IO ()
main = do
    let env = (initMemory, initRegisters)
    let finalEnv = evalCFG exampleCFG env
    print finalEnv