{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Environment as Sys
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import Data.Text.Display

import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()

-- Transitions representing different types of instructions
data Trans =
    NonCF Instruction  -- No jumps, or exit
  | Unconditional      -- Unconditional jumps
  | Assert Jcmp Reg RegImm -- Conditional jumps
  deriving (Show, Eq, Ord)

type Label = Int
type LabeledProgram = [(Int, Instruction)]
type CFG = Set (Label, Trans, Label)

-- Assigns a label to each instruction in the program
label :: Program -> LabeledProgram
label = zip [0..]

-- Negates the condition for conditional jumps
neg :: Jcmp -> Jcmp
neg cmp =
  case cmp of
    Jeq -> Jne ; Jne -> Jeq
    Jgt -> Jle; Jge -> Jlt; Jlt -> Jge; Jle -> Jgt
    Jsgt -> Jsle; Jsge -> Jslt; Jslt -> Jsge; Jsle -> Jsgt
    Jset -> error "Don't know how to negate JSET"

-- Constructs the control flow graph (CFG) for a given program
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

-- Converts the CFG into DOT format for visualization
cfgToDot :: CFG -> String
cfgToDot graph = Set.toList graph >>= showTrans
  where
    showTrans (x, NonCF i, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (display i)
    showTrans (x, Unconditional, y) = printf "  %d -> %d [label=\"jmp\"];\n" x y
    showTrans (x, Assert c r ir, y) = printf "  %d -> %d [label=\"%s\"];\n" x y (showJump c r ir)
    showJump c r ir = display c <> " " <> display r <> ", " <> display ir

-- DOT prelude for CFG visualization
dotPrelude :: String
dotPrelude =
  "digraph cfg { \n"++
  "node [fontname=\"monospace\"];\n"++
  "node [shape=box];\n"++
  "edge [fontname=\"monospace\"];\n"

-- Mark specific nodes in the CFG for visualization
markNodes :: Program -> String
markNodes prog = concat $ mapMaybe mark $ label prog
  where
    mark (lab, Exit) = return $ printf "%d [style=\"rounded,filled\",fillcolor=grey];\n" lab
    mark (lab, JCond _ _ _ _) = return $ printf "%d [shape=diamond];\n" lab
    mark _ = Nothing

-- Definition of the Interval type for abstract interpretation
data Interval = Bottom                       -- undefined
              | Top                          -- any possible value
              | Interval Double Double       -- [l, u]
              | Intervalreg RegImm RegImm
              | Intervals Interval Interval  -- composition of intervals
              deriving (Eq, Show)

-- Memory and Environment definition
type Memory = Map.Map Int Interval
type Environment = (Memory, Map.Map Reg Interval)

-- Initializes memory
initMemory :: Memory
initMemory = Map.fromList [(i, Bottom) | i <- [0..511]]

-- Memory lookup function
lookupMemory :: Memory -> Int -> Interval
lookupMemory mem i = Map.findWithDefault Bottom i mem

-- Memory update function
updateMemory :: Memory -> Int -> Interval -> Memory
updateMemory mem i interval = Map.insert i interval mem

-- Initializes registers
initRegisters :: Map.Map Reg Interval
initRegisters = Map.fromList [(Reg r, Interval 0 0) | r <- [0..10]]

-- Transfer function for MOV (LoadImm)
transferMov :: Reg -> Interval -> Environment -> Environment
transferMov reg interval (mem, regs) =
    let newRegs = Map.insert reg interval regs
    in (mem, newRegs)

-- Transfer function for binary operations like ADD
transferBinOp :: BinAlu -> Reg -> Reg -> Reg -> Environment -> Environment
transferBinOp binOp reg ri rj (mem, regs) =
    let intervalRi = Map.findWithDefault Bottom ri regs
        intervalRj = Map.findWithDefault Bottom rj regs
        newInterval = case binOp of
                        Add -> intervalAdd intervalRi intervalRj
                        Sub -> intervalSub intervalRi intervalRj
                        _   -> Top -- Other binary operators can be defined
        newRegs = Map.insert reg newInterval regs
    in (mem, newRegs)

-- Transfer function for Assert (conditional jump)
transferAssert :: Jcmp -> Reg -> RegImm -> Environment -> Boolean
transferAssert cmp reg (R rj) (mem, regs) =
    let intervalRi = Map.findWithDefault Bottom reg regs
        intervalRj = Map.findWithDefault Bottom rj regs
        
        -- Check the condition and determine the branches
        conditionTrue = evaluateCondition cmp intervalRi intervalRj
        newRegsTrue = if conditionTrue
            then Map.insert reg (intervalIntersection intervalRi intervalRj) regs
            else Map.insert reg (Bottom) regs  -- Handle false case appropriately

        newRegsFalse = if not conditionTrue
            then Map.insert reg (intervalMinus intervalRi intervalRj) regs
            else Map.insert reg (Bottom) regs  -- Handle true case appropriately
            
        trueBranch = (mem, newRegsTrue)
        falseBranch = (mem, newRegsFalse)
    in (trueBranch, falseBranch)

transferAssert _ _ _ env = (env, env) -- Other cases simplified


-- Interval operations (addition, subtraction, etc.)
intervalAdd :: Interval -> Interval -> Interval
intervalAdd (Interval l1 u1) (Interval l2 u2) = Interval (l1 + l2) (u1 + u2)
intervalAdd _ _ = Top

intervalSub :: Interval -> Interval -> Interval
intervalSub (Interval l1 u1) (Interval l2 u2) = Interval (l1 - u2) (u1 - l2)
intervalSub _ _ = Bottom

intervalIntersection :: Interval -> Interval -> Interval
intervalIntersection (Interval l1 u1) (Interval l2 u2) = Interval (max l1 l2) (min u1 u2)
intervalIntersection _ _ = Bottom

intervalMinus :: Interval -> Interval -> Interval
intervalMinus (Interval l1 u1) (Interval l2 u2) = Interval (l1 - u2) (u1 - l2)
intervalMinus _ _ = Bottom

-- Evaluates the CFG based on transitions
evalCFG :: Set.Set (Int, Trans, Int) -> Environment -> Environment
evalCFG cfg env = 
    foldl (\env (fromNode, trans, toNode) -> evalTrans trans env) env (Set.toList cfg)

-- Evaluates a transition (NonCF, Unconditional, or Assert)
evalTrans :: Trans -> Environment -> Environment
evalTrans (NonCF instr) env = evalInstr instr env
evalTrans Unconditional env = env -- Unconditional jump (handled separately in control flow)
evalTrans (Assert cmp reg regimm) env = 
    let (trueEnv, falseEnv) = transferAssert cmp reg regimm env
    in trueEnv -- You can extend this to handle branching explicitly

-- Helper function to evaluate conditions
evaluateCondition :: Jcmp -> Interval -> Interval -> Bool
evaluateCondition Jeq (Interval l1 u1) (Interval l2 u2) = l1 == l2 && u1 == u2
evaluateCondition Jne (Interval l1 u1) (Interval l2 u2) = not (l1 == l2 && u1 == u2)
evaluateCondition Jgt (Interval l1 u1) (Interval l2 u2) = u1 > l2
evaluateCondition Jge (Interval l1 u1) (Interval l2 u2) = l1 >= l2
evaluateCondition Jlt (Interval l1 u1) (Interval l2 u2) = l1 < u2
evaluateCondition Jle (Interval l1 u1) (Interval l2 u2) = u1 <= l2
evaluateCondition _ _ _ = False  -- Handle other cases or invalid inputs


-- Evaluates a single instruction
evalInstr :: Instruction -> Environment -> Environment
evalInstr (Binary _ Add ri (R rj)) env = transferBinOp Add ri ri rj env
evalInstr (Binary _ Sub ri (R rj)) env = transferBinOp Sub ri ri rj env
evalInstr (Binary _ Mov ri num) env = transferMov ri (Intervalreg num num) env
evalInstr (LoadImm ri imm) env = transferMov ri (Interval (fromIntegral imm) (fromIntegral imm)) env
evalInstr Exit env = env -- Exit instruction terminates execution
evalInstr _ env = env -- Other cases

-- Example CFG with transitions
exampleCFG :: Set.Set (Int, Trans, Int)
exampleCFG = Set.fromList [
    (0, NonCF (Binary B32 Mov (Reg 0) (Imm 21)), 1),
    (1, NonCF (Binary B64 Mov (Reg 1) (Imm 21)), 2),
    (2, Assert Jeq (Reg 0) (Imm 0), 4),
    (2, Assert Jne (Reg 0) (Imm 0), 3),
    (3, NonCF (Binary B64 Add (Reg 0) (R (Reg 1))), 4)
    ]

-- Example of running the CFG evaluation
main :: IO ()
main = do
    let env = (initMemory, initRegisters)
    let finalEnv = evalCFG exampleCFG env
    print finalEnv