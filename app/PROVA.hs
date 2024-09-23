-- import qualified Data.Map as Map

-- type Memory = Map.Map Int Interval
-- -- Here, Memory is a map from Int (the cell index) to Interval. This allows us to store the abstract interval values for each memory cell.

-- initMemory :: Memory
-- initMemory = Map.fromList [(i, Bottom) | i <- [0..511]]
-- -- This creates a memory with 512 cells, all initially set to Bottom.

-- lookupMemory :: Memory -> Int -> Interval
-- lookupMemory mem i = Map.findWithDefault Bottom i mem
-- -- This will return the interval at memory cell i, or Bottom if the cell has not been initialized.

-- updateMemory :: Memory -> Int -> Interval -> Memory
-- updateMemory mem i interval = Map.insert i interval mem
-- --This will update the interval stored at cell i in the memory.


-- type Environment = (Memory, Map.Map Reg Interval)

-- -- Example register environment
-- initRegisters :: Map.Map Reg Interval
-- initRegisters = Map.fromList [(r0, Bottom), (r1, Bottom), (r2, Bottom), (r3, Bottom), 
--                               (r4, Bottom), (r5, Bottom), (r6, Bottom), (r7, Bottom), 
--                               (r8, Bottom), (r9, Bottom), (r10, Bottom)]
-- -- You already have memory defined as a Map from indices to intervals. Registers can be similarly represented as a Map from register numbers to intervals.


-- -- Transfer function for a move operation
-- transferMov :: Reg -> Interval -> Environment -> Environment
-- transferMov reg interval (mem, regs) =
--     let newRegs = Map.insert reg interval regs
--     in (mem, newRegs)


-- -- Transfer function for an add operation
-- transferAdd :: Reg -> Reg -> Reg -> Environment -> Environment
-- transferAdd reg ri rj (mem, regs) =
--     let intervalRi = Map.findWithDefault Bottom ri regs
--         intervalRj = Map.findWithDefault Bottom rj regs
--         newInterval = intervalAdd intervalRi intervalRj
--         newRegs = Map.insert reg newInterval regs
--     in (mem, newRegs)


-- -- Transfer function for memory lookup
-- transferMemLookup :: Reg -> Reg -> Environment -> Environment
-- transferMemLookup reg addrReg (mem, regs) =
--     let intervalAddr = Map.findWithDefault Bottom addrReg regs
--         -- For simplicity, assume intervalAddr holds a concrete value
--         interval = case intervalAddr of
--                      Interval l u | l == u -> lookupMemory mem (round l)
--                      _ -> Top
--         newRegs = Map.insert reg interval regs
--     in (mem, newRegs)



-- -- Transfer function for memory assignment
-- transferMemAssign :: Reg -> Reg -> Environment -> Environment
-- transferMemAssign addrReg srcReg (mem, regs) =
--     let intervalAddr = Map.findWithDefault Bottom addrReg regs
--         intervalSrc = Map.findWithDefault Bottom srcReg regs
--         newMem = case intervalAddr of
--                    Interval l u | l == u -> updateMemory mem (round l) intervalSrc
--                    _ -> mem -- if it's not a single value address, leave memory unchanged
--     in (newMem, regs)


-- -- Transfer function for conditional jump
-- transferCondJump :: Reg -> Reg -> Environment -> (Environment, Environment)
-- transferCondJump ri rj (mem, regs) =
--     let intervalRi = Map.findWithDefault Bottom ri regs
--         intervalRj = Map.findWithDefault Bottom rj regs
--         trueBranch = (mem, Map.insert ri (intervalIntersection intervalRi intervalRj) regs)
--         falseBranch = (mem, Map.insert ri (intervalMinus intervalRi intervalRj) regs)
--     in (trueBranch, falseBranch)




-- -- CFG node: (fromNode, instruction, toNode)
-- data Instr = Mov Reg RegOrImm 
--            | Add Reg Reg Reg 
--            | Sub Reg Reg Reg 
--            | MemAssign Reg Reg
--            | MemLookup Reg Reg
--            | CondJump Reg Reg Int Int  -- ri, rj, trueNode, falseNode

-- type CFG = Map.Map Int (Instr, Int)

-- -- Evaluate a single CFG node
-- evalNode :: Instr -> Environment -> Environment
-- evalNode (Mov ri (Imm n)) env = transferMov ri (intervalConst n) env
-- evalNode (Mov ri (R rj)) env =
--     let (_, regs) = env
--         intervalRj = Map.findWithDefault Bottom rj regs
--     in transferMov ri intervalRj env
-- evalNode (Add ri rj rk) env = transferAdd ri rj rk env
-- evalNode (MemAssign ri rj) env = transferMemAssign ri rj env
-- evalNode (MemLookup ri rj) env = transferMemLookup ri rj env
-- evalNode (CondJump ri rj trueNode falseNode) env =
--     let (trueEnv, falseEnv) = transferCondJump ri rj env
--     in trueEnv -- For simplicity, just return true branch (you'd handle branches in full implementation)

-- -- Evaluate the entire CFG by iterating over the nodes
-- evalCFG :: CFG -> Environment -> Environment
-- evalCFG cfg env =
--     foldl (\env (instr, _) -> evalNode instr env) env (Map.elems cfg)
