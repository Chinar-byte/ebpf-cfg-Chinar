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

-- Function to lookup the value in a given register
lookupRegister :: Reg -> Environment -> Interval
lookupRegister reg (_, regs) = Map.findWithDefault Bottom reg regs

-- Transfer function for Unary operations
transferUnaryOp :: UnAlu -> Reg -> Environment -> Environment
transferUnaryOp Neg reg (mem, regs) =
    let interval = Map.findWithDefault Bottom reg regs
        newInterval = case interval of
            Interval l u -> Interval (-u) (-l)  -- Negate the interval bounds
            _ -> Top
        newRegs = Map.insert reg newInterval regs
    in (mem, newRegs)
transferUnaryOp _ reg env = env  -- Simplified for other unary operations

transferStore :: BSize -> Reg -> Maybe MemoryOffset -> RegImm -> Environment -> Environment
transferStore bsize dst maybeOffset srcRegImm (mem, regs) =
    let value = case srcRegImm of
                    R r -> Map.findWithDefault Bottom r regs
                    Imm imm -> Interval (fromIntegral imm) (fromIntegral imm)  -- Carica l'imm immediato

        updatedMem = case maybeOffset of
                        Just offset -> updateMemory mem (fromIntegral offset) value  -- Aggiorna la memoria
                        Nothing -> mem  -- Se non c'è offset, non facciamo nulla
    in (updatedMem, regs)

transferLoadImm :: Reg -> Imm -> Environment -> Environment
transferLoadImm reg imm (mem, regs) =
    let newRegs = Map.insert reg (Interval (fromIntegral imm) (fromIntegral imm)) regs
    in (mem, newRegs)

transferLoadMapFd :: Reg -> Imm -> Environment -> Environment
transferLoadMapFd reg fd (mem, regs) =
    let newRegs = Map.insert reg (Interval (fromIntegral fd) (fromIntegral fd)) regs
    in (mem, newRegs)

transferBinOp :: BinAlu -> Reg -> Reg -> Reg -> Environment -> Environment
transferBinOp op dst src1 src2 (mem, regs) =
  let interval1 = Map.findWithDefault Bottom src1 regs
      interval2 = Map.findWithDefault Bottom src2 regs
      newInterval = case op of
          Add -> intervalAdd interval1 interval2
          Sub -> intervalSub interval1 interval2
          -- Per altre operazioni binarie (Or, And, ecc.)
          _   -> Top
      newRegs = Map.insert dst newInterval regs
  in (mem, newRegs)

-- -- Transfer function for Assert (conditional jump)
-- transferAssert :: Jcmp -> Reg -> RegImm -> Environment -> Bool
-- transferAssert cmp reg (R rj) (mem, regs) =
--     let intervalRi = Map.findWithDefault Bottom reg regs
--         intervalRj = Map.findWithDefault Bottom rj regs
        
--         -- Check the condition and determine the branches
--         conditionTrue = evaluateCondition cmp intervalRi intervalRj
--         newRegsTrue = if conditionTrue
--             then Map.insert reg (intervalIntersection intervalRi intervalRj) regs
--             else Map.insert reg (Bottom) regs  -- Handle false case appropriately

--         newRegsFalse = if not conditionTrue
--             then Map.insert reg (intervalMinus intervalRi intervalRj) regs
--             else Map.insert reg (Bottom) regs  -- Handle true case appropriately
            
--         trueBranch = (mem, newRegsTrue)
--         falseBranch = (mem, newRegsFalse)
--     in (trueBranch, falseBranch)

-- transferAssert _ _ _ env = (env, env) -- Other cases simplified

transferMov :: Reg -> Interval -> Environment -> Environment
transferMov reg interval (mem, regs) =
    let newRegs = Map.insert reg interval regs
    in (mem, newRegs)

-- Interval operations (addition, subtraction, etc.)
intervalAdd :: Interval -> Interval -> Interval
intervalAdd (Interval l1 u1) (Interval l2 u2) = Interval (l1 + l2) (u1 + u2)
intervalAdd _ _ = Top

intervalSub :: Interval -> Interval -> Interval
intervalSub (Interval l1 u1) (Interval l2 u2) = Interval (l1 - l2) (u1 - u2) -- finite intervals
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
evalTrans (Assert cmp reg regimm) env = undefined
    -- let (trueEnv, falseEnv) = transferAssert cmp reg regimm env
    -- in trueEnv -- You can extend this to handle branching explicitly

-- Helper function to evaluate conditions
evaluateCondition :: Jcmp -> Interval -> Interval -> Bool
evaluateCondition Jeq (Interval l1 u1) (Interval l2 u2) = l1 == l2 && u1 == u2
evaluateCondition Jne (Interval l1 u1) (Interval l2 u2) = not (l1 == l2 && u1 == u2)
evaluateCondition Jgt (Interval l1 u1) (Interval l2 u2) = u1 > l2
evaluateCondition Jge (Interval l1 u1) (Interval l2 u2) = l1 >= l2
evaluateCondition Jlt (Interval l1 u1) (Interval l2 u2) = l1 < u2
evaluateCondition Jle (Interval l1 u1) (Interval l2 u2) = u1 <= l2
evaluateCondition _ _ _ = False  -- Handle other cases or invalid inputs

-- Function to find the next element based on the last value of the first element
-- findNextByLastValue :: CFG -> Maybe (Label, Trans, Label)
-- findNextByLastValue cfg =
--     case Set.toList cfg of
--         [] -> Nothing
--         (firstElem:_) -> let lastValue = getLastValue firstElem
--                              in findNextElement lastValue (Set.toList cfg)

-- -- Helper function to get the last value from a tuple
-- getLastValue :: (Label, Trans, Label) -> Label
-- getLastValue (_, _, last) = last

-- -- Function to find the next element with the matching first value
-- findNextElement :: Label -> [(Label, Trans, Label)] -> Maybe (Label, Trans, Label)
-- findNextElement _ [] = Nothing
-- findNextElement lastValue (x:xs) =
--     if getFirstValue x == lastValue
--         then Just x
--         else findNextElement lastValue xs

-- -- Helper function to get the first value from a tuple
-- getFirstValue :: (Label, Trans, Label) -> Label
-- getFirstValue (first, _, _) = first


-- Evaluates a single instruction
evalInstr :: Instruction -> Environment -> Environment
evalInstr (Binary _ Add ri (R rj)) env = transferBinOp Add ri ri rj env
evalInstr (Binary _ Sub ri (R rj)) env = transferBinOp Sub ri ri rj env
evalInstr (Binary _ Mov ri (Imm r)) env = transferMov ri (Interval (fromIntegral r) (fromIntegral r)) env
evalInstr (Unary _ Neg reg) env = transferUnaryOp Neg reg env
evalInstr (Store bsize dst maybeOffset src) env = transferStore bsize dst maybeOffset src env
evalInstr (LoadImm reg imm) env = transferLoadImm reg imm env 
evalInstr (LoadMapFd reg fd) env = transferLoadMapFd reg fd env  
evalInstr Exit env = env -- Exit instruction terminates execution

-- Example CFG with transitions
exampleCFG :: Set.Set (Int, Trans, Int)
exampleCFG = Set.fromList [
    (0, NonCF (Binary B32 Mov (Reg 0) (Imm 21)), 1),
    (1, NonCF (Binary B64 Mov (Reg 1) (Imm 21)), 2),
    -- (2, Assert Jeq (Reg 0) (Imm 0), 4),
    -- (2, Assert Jne (Reg 0) (Imm 0), 3),
    (3, NonCF (Binary B64 Add (Reg 0) (R (Reg 1))), 4)
    ]

-- Project Two 
bitwiseShiftConstant :: Int 
bitwiseShiftConstant = 0x0000FFFF  -- mask used for address masking

dataLast :: Int 
dataLast = 0x1234FFFF -- should be r1 + r2

dataBegin :: Int
dataBegin =  0x12340000 -- should be r1

-- Process the program by applying transformations to each instruction
processProgram :: Program -> Program
processProgram program =
  let numInstructions = length program
  in concatMap (uncurry (processInstruction numInstructions)) (zip [0..] program)

-- Function that decides which processing function to call, passing counter and numInstructions only for JCond and Jmp
processInstruction :: Int -> Int -> Instruction -> Program
processInstruction numInstructions counter (JCond cmp reg regimm offset) = processJCond counter numInstructions cmp reg regimm offset
processInstruction numInstructions counter (Jmp offset) = processJmp counter numInstructions offset
processInstruction _ _ (Binary size alu reg regimm) = processBinary size alu reg regimm
processInstruction _ _ (Unary size unalu reg) = processUnary size unalu reg
processInstruction _ _ (Store size reg memOffset regimm) = processStore size reg memOffset regimm
processInstruction _ _ (Load size reg1 reg2 memOffset) = processLoad size reg1 reg2 memOffset
processInstruction _ _ (LoadImm reg imm) = processLoadImm reg imm
processInstruction _ _ (LoadMapFd reg imm) = processLoadMapFd reg imm
processInstruction _ _ (LoadAbs size imm) = processLoadAbs size imm
processInstruction _ _ (LoadInd size reg imm) = processLoadInd size reg imm
processInstruction _ _ (Call extern) = processCall extern
processInstruction _ _ Exit = processExit

-- Functions that return the instruction passed as an argument
processBinary :: BSize -> BinAlu -> Reg -> RegImm -> Program
processBinary size alu reg regimm = [Binary size alu reg regimm]

processUnary :: BSize -> UnAlu -> Reg -> Program
processUnary size unalu reg = [Unary size unalu reg]

processStore :: BSize -> Reg -> Maybe MemoryOffset -> RegImm -> Program
processStore bin regDest memOffset regImm = 
  let offsetValue = case memOffset of
                      Just offset -> fromIntegral offset  -- Use offset if present
                      Nothing     -> 0                    -- Use 0 if offset is Nothing
  in
  [ Binary B64 Add regDest (Imm offsetValue)  -- Add offset (or 0) to regDest
  , Binary bin And regDest (Imm (fromIntegral bitwiseShiftConstant))   -- Mask the lower 16 bits
  , Binary bin Or regDest (Imm (fromIntegral dataBegin))    -- Set the upper 16 bits
  , Store bin regDest (if memOffset == Nothing then Nothing else Just (fromIntegral offsetValue)) regImm  -- Execute store with modified register
  ]

processLoad :: BSize -> Reg -> Reg -> Maybe MemoryOffset -> Program
processLoad bin regDest regSrc memOffset =
  let offsetValue = case memOffset of
                      Just offset -> fromIntegral offset  -- Use offset if present
                      Nothing     -> 0                    -- Use 0 if offset is Nothing
  in
  [ Binary B64 Add regSrc (Imm offsetValue)  -- Add offset (or 0) to regSrc
  , Binary bin And regSrc (Imm (fromIntegral bitwiseShiftConstant))   -- Mask the lower 16 bits
  , Binary bin Or regSrc (Imm (fromIntegral dataBegin))    -- Set the upper 16 bits
  , Load bin regDest regSrc Nothing  -- Execute the original load
  ]

processLoadImm :: Reg -> Imm -> Program
processLoadImm reg imm = [LoadImm reg imm]

processLoadMapFd :: Reg -> Imm -> Program
processLoadMapFd reg imm = [LoadMapFd reg imm]

processLoadAbs :: BSize -> Imm -> Program
processLoadAbs size imm = [LoadAbs size imm]

processLoadInd :: BSize -> Reg -> Imm -> Program
processLoadInd size reg imm = [LoadInd size reg imm]

-- Processing for JCond
processJCond :: Int -> Int -> Jcmp -> Reg -> RegImm -> CodeOffset -> Program
processJCond counter numInstructions cmp reg regimm offset = 
  let targetIndex = counter + fromIntegral offset  -- Calculate the target instruction index
  in if targetIndex < 0 || targetIndex >= numInstructions
     then error $ "Jump exceeds code bounds: " ++ show targetIndex ++ " is not valid."  -- Handle out-of-bound jumps
     else [JCond cmp reg regimm offset]  -- Valid jump, return the JCond instruction

-- Processing for Jmp
processJmp :: Int -> Int -> CodeOffset -> Program
processJmp counter numInstructions offset = 
  let targetIndex = counter + fromIntegral offset  -- Calculate the target instruction index
  in if targetIndex < 0 || targetIndex >= numInstructions
     then error $ "Jump exceeds code bounds: " ++ show targetIndex ++ " is not valid."  -- Handle out-of-bound jumps
     else [Jmp offset]  -- Valid jump, return the Jmp instruction

processCall :: HelperId -> Program
processCall extern = [Call extern]

processExit :: Program
processExit = [Exit]

-- Process and adjust the program
processAndAdjustProgram :: Program -> Program
processAndAdjustProgram program =
  let processedProgram = processProgram program
  in adjustJumps processedProgram

-- Adjust jumps after processing
adjustJumps :: Program -> Program
adjustJumps program = snd $ foldl adjustInstruction (0, []) program
  where
    adjustInstruction :: (Int, Program) -> Instruction -> (Int, Program)
    adjustInstruction (counter, accProgram) (JCond cmp reg regimm offset) =
      let adjustedOffset = adjustOffset program counter (fromIntegral offset) -- Convert offset to Int
          adjustedInstr = JCond cmp reg regimm (fromIntegral adjustedOffset) -- Convert adjustedOffset back to CodeOffset
      in (counter + 1, accProgram ++ [adjustedInstr])

    adjustInstruction (counter, accProgram) (Jmp offset) =
      let adjustedOffset = adjustOffset program counter (fromIntegral offset) -- Convert offset to Int
          adjustedInstr = Jmp (fromIntegral adjustedOffset) -- Convert adjustedOffset back to CodeOffset
      in (counter + 1, accProgram ++ [adjustedInstr])

    adjustInstruction (counter, accProgram) instr =
      (counter + 1, accProgram ++ [instr])

-- Adjust the offset for jumps, taking into account the load/store instructions
adjustOffset :: Program -> Int -> Int -> Int
adjustOffset program counter originalOffset =
  let targetIndex = counter + originalOffset
      instructionsBetween = if originalOffset > 0
                            then take (targetIndex - counter) (drop (counter + 3) program)
                            else take (counter - targetIndex) (drop targetIndex program)
      loadAndStoreCount = length $ filter isLoadOrStore instructionsBetween
  in if originalOffset > 0
     then originalOffset + 3 * loadAndStoreCount
     else originalOffset - 3 * loadAndStoreCount

-- Check if the instruction is a Load or Store
isLoadOrStore :: Instruction -> Bool
isLoadOrStore (Load _ _ _ _) = True
isLoadOrStore (Store _ _ _ _) = True
isLoadOrStore _ = False

-- Example of running the CFG evaluation
main :: IO ()
main = do
    let env = (initMemory, initRegisters)
    let finalEnv = evalCFG exampleCFG env
    print finalEnv