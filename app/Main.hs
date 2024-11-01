{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Display
import Ebpf.Asm
import Ebpf.AsmParser
import Ebpf.Display ()
import System.Environment qualified as Sys
import Text.Printf

data Trans
  = NonCF Instruction -- no jumps, or exit
  | Unconditional
  | Assert Jcmp Reg RegImm
  deriving (Show, Eq, Ord)

type Label = Int

data ProgramValues
  = InstructionValue Instruction
  | AssertValue (Jcmp, Reg, RegImm)
  deriving (Show, Eq)

type Program3 = [ProgramValues]

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

-- Mini Project 3 code below

data Security = H | L deriving (Show, Eq)

type Memory = Map.Map String Security

-- New type to hold node security values
type NodeSecurity = [(Label, Security)]

-- Main function to get registers and their associated security levels
checkRegisters :: CFG -> Memory
checkRegisters cfg = Map.fromList $ map assignSecurity (Set.toList regsUsed)
  where
    regsUsed = Set.fromList $ mapMaybe extractMovRegister (Set.toList cfg)
    assignSecurity reg@(Reg n)
      | n == 0 || n == 1 = (show reg, H)
      | otherwise = (show reg, L)

-- Support function to extract registers from Mov instructions
extractMovRegister :: (Label, Trans, Label) -> Maybe Reg
extractMovRegister (_, NonCF (Binary B64 Mov reg _), _) = Just reg
extractMovRegister _ = Nothing

-- Function to update the security level of a specific register
updateSecurity :: Memory -> String -> Security -> Memory
updateSecurity memory regName newLevel = Map.insert regName newLevel memory

invertCFG :: CFG -> CFG
invertCFG cfg = Set.map invertEdge cfg
  where
    invertEdge (from, trans, to) = (to, trans, from)

computePredecessors :: CFG -> Map.Map Label [Label]
computePredecessors cfg = Map.unionWith (++) baseMap predecessorsMap
  where
    -- Base map with all nodes initialized to empty lists
    nodes = Set.toList $ Set.map (\(from, _, to) -> from) cfg `Set.union` Set.map (\(_, _, to) -> to) cfg
    baseMap = Map.fromList [(node, []) | node <- nodes]

    -- Map calculated from the actual predecessors
    predecessorsMap = Set.foldr addEdge Map.empty cfg
    addEdge (from, _, to) = Map.insertWith (++) to [from]

-- Calculates the dominators for a graph given the number of nodes and the map of predecessors
computeDominators :: CFG -> Map Label [Label] -> Map Label (Set.Set Label)
computeDominators cfg predecessors = fixpoint initialDom
  where
    numNodes = countNodes cfg

    -- Find the node with no predecessors
    entryPoint = head [n | n <- [0 .. numNodes - 1], null (Map.findWithDefault [] n predecessors)]

    -- Initialize the dominator set for each node
    initialDom = Map.fromList [(n, if n == entryPoint then Set.singleton entryPoint else Set.fromList [0 .. numNodes - 1]) | n <- [0 .. numNodes - 1]]

    -- Iterative function until convergence
    fixpoint doms
      | doms == newDoms = doms
      | otherwise = fixpoint newDoms
      where
        newDoms = Map.mapWithKey updateDom doms

        -- Calculate the new set of dominators for each node n
        updateDom n _
          | n == entryPoint = Set.singleton entryPoint -- The entry point only dominates itself
          | otherwise =
              case Map.findWithDefault [] n predecessors of
                [] -> Set.singleton n -- No predecessors: only dominates itself
                preds -> Set.insert n $ foldr1 Set.intersection [doms Map.! p | p <- preds]

countNodes :: CFG -> Int
countNodes cfg = Set.size nodes
  where
    nodes =
      Set.fromList [from | (from, _, _) <- Set.toList cfg]
        `Set.union` Set.fromList [to | (_, _, to) <- Set.toList cfg]

computePostDominanceFrontier :: CFG -> Map Label (Set Label) -> Map Label [Label]
computePostDominanceFrontier cfg doms = Map.fromList [(n, filterUnrelatedPredecessors n (associatedPredecessors (findPostDominatedBy n))) | n <- Map.keys doms]
  where
    -- Calculate predecessors using the `computePredecessors` function
    predecessors = computePredecessors cfg

    -- Find all nodes `m` that are post-dominated by node `n`
    findPostDominatedBy :: Label -> [Label]
    findPostDominatedBy n = [m | (m, domSet) <- Map.toList doms, n `Set.member` domSet]

    -- For each node in the list, obtain unique predecessors
    associatedPredecessors :: [Label] -> [Label]
    associatedPredecessors nodes = Set.toList . Set.fromList $ concatMap (\node -> Map.findWithDefault [] node predecessors) nodes

    -- Filter predecessors to remove nodes present in `findPostDominatedBy`
    filterUnrelatedPredecessors :: Label -> [Label] -> [Label]
    filterUnrelatedPredecessors n preds =
      let dominatedNodes = findPostDominatedBy n
       in filter (`notElem` dominatedNodes) preds

-- Function to process the program based on the control flow graph
processCFG :: CFG -> IO ()
processCFG cfg = do
  let initialMemory = Map.fromList [(show n, L) | n <- [0 .. countNodes cfg - 1]] -- Initialize Memory with low security
  let initialNodeSecurity = [(n, L) | n <- [0 .. countNodes cfg - 1]] -- Initialize NodeSecurity with low security
  let finalMemory = processNode cfg initialMemory initialNodeSecurity -- Process the CFG
  printMemory finalMemory -- Print the final memory states

-- Function to count the number of nodes in the CFG
countNodes1 :: CFG -> Int
countNodes1 cfg = Set.size $ Set.map (\(from, _, to) -> from) cfg `Set.union` Set.map (\(_, _, to) -> to) cfg

isolateAndRemoveFirst :: CFG -> Maybe ((Label, Trans, Label), CFG)
isolateAndRemoveFirst s =
  case Set.minView s of
    Nothing -> Nothing -- If the set is empty
    Just (firstElem, remainingSet) -> Just (firstElem, remainingSet)

filterForNextNodeWithCurrentNode :: Int -> CFG -> Int
filterForNextNodeWithCurrentNode currentNode cfg =
  let tuple = filter (\(start, trans, end) -> start == currentNode) (Set.toList cfg)
   in case tuple of
        [(start, trans, end)] -> end

processNode :: CFG -> Memory -> NodeSecurity -> Memory
processNode cfg memory nodeSecurity =
  case isolateAndRemoveFirst cfg of
    Nothing -> memory -- Finished processing all nodes, return final memory
    Just ((start, trans, end), newCFG) ->
      case trans of
        NonCF instr ->
          let updatedMemory = processInstruction instr memory start cfg
           in processNode newCFG updatedMemory nodeSecurity
        Assert j r r1 ->
          let updatedMemory = processAssert j r r1 memory nodeSecurity start
           in processNode newCFG updatedMemory nodeSecurity
        _ -> memory -- Handle other cases as necessary

processInstruction :: Instruction -> Memory -> Label -> CFG -> Memory
processInstruction instr memory currentNode cfg =
  case instr of
    (Binary _ _ reg regimm) -> updateSecurityForRegisters memory currentNode [reg]
    (Unary _ _ reg) -> updateSecurityForRegisters memory currentNode [reg]
    (Store _ reg _ regimm) -> updateSecurityForRegisters memory currentNode [reg]
    (Load _ reg1 reg2 _) -> updateSecurityForRegisters memory currentNode [reg1, reg2]
    (LoadImm reg imm) -> updateSecurityForRegisters memory currentNode [reg]
    (LoadMapFd reg imm) -> updateSecurityForRegisters memory currentNode [reg]
    (LoadAbs _ imm) -> memory
    (LoadInd _ reg imm) -> updateSecurityForRegisters memory currentNode [reg]
    (Call _) -> memory
    _ -> memory -- Handle other cases as necessary

updateSecurityForRegisters :: Memory -> Label -> [Reg] -> Memory
updateSecurityForRegisters memory currentNode regs =
  let currentSecurity = fromMaybe L (Map.lookup (show currentNode) memory)
      highSecurity = currentSecurity == H
   in foldl updateSecurity memory regs
  where
    updateSecurity mem reg =
      if reg == (Reg 0) || reg == (Reg 1) || (fromMaybe L (Map.lookup (show currentNode) memory) == H)
        then Map.insert (show currentNode) H mem -- Update security for the current node to high
        else mem

printMemory :: Memory -> IO ()
printMemory memory = do
  putStrLn "Final Memory Security Levels:"
  mapM_ print (Map.toList memory)

-- The Asserts
processAssert :: Jcmp -> Reg -> RegImm -> Memory -> NodeSecurity -> Label -> Memory
processAssert jcmp reg1 regImm memory nodeSecurity currentNode =
  let secReg1 = fromMaybe L (lookupRegisterSecurity reg1 nodeSecurity)
      secRegImm = L
      assertionPassed = evaluateAssert jcmp secReg1 secRegImm
   in if assertionPassed
        then updateSecurity2 memory currentNode H -- Update the security level of the current node if assertion passes
        else memory -- Return unchanged memory if assertion fails

lookupRegisterSecurity :: Reg -> NodeSecurity -> Maybe Security
lookupRegisterSecurity (Reg n) ns = lookup n ns -- Assuming Reg has an Int identifier

evaluateAssert :: Jcmp -> Security -> Security -> Bool
evaluateAssert jcmp sec1 sec2 =
  case jcmp of
    Jeq -> sec1 == sec2
    Jne -> sec1 /= sec2
    Jgt -> sec1 == H && sec2 == L -- High can imply greater than Low
    Jge -> sec1 == H -- High implies greater than or equal
    Jlt -> sec1 == L && sec2 == H -- Low can imply less than High
    Jle -> sec1 == L -- Low implies less than or equal
    _ -> False -- Handle other cases as needed

updateSecurity2 :: Memory -> Label -> Security -> Memory
updateSecurity2 memory currentNode newLevel = Map.insert (show currentNode) newLevel memory

-- compareSecurityLevelInstruction :: Security -> Instruction -> Bool
-- compareSecurityLevelInstruction sec inst = False

-- compareSecurityLevelAssert :: Security -> Jcmp -> Reg -> RegImm -> Bool
-- compareSecurityLevelAssert = False