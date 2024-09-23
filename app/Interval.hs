{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Interval (
    Interval
) 
where

import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe (mapMaybe)
import GHC.Base (minInt)

positiveInfinity :: Double
positiveInfinity = 1 / 0

negativeInfinity :: Double
negativeInfinity = -1 / 0

data Interval = Bottom                       -- undefined
    -- | Interval Integer Integer               -- [l, u]
    | Top                                    -- T (any possible value)
    | Interval Double Double           -- negative infinity to the positive infinity 
    | Intervals Interval Interval
    deriving (Eq, Show)

-- Basic Operations of intervals
intervalAdd :: Interval -> Interval -> Interval
intervalAdd (Interval l1 u1) (Interval l2 u2) = Interval (l1 + l2) (u1 + u2) -- finite intervals
intervalAdd Top _ = Top
intervalAdd _ Top = Top
intervalAdd Bottom _ = Bottom
intervalAdd _ Bottom = Bottom -- if we have time do the infinity implementation

intervalMinus :: Interval -> Interval -> Interval
intervalMinus (Interval l1 u1) (Interval l2 u2) = Interval (l1 - l2) (u1 - u2) -- finite intervals
intervalMinus Top _ = Top
intervalMinus _ Top = Bottom
intervalMinus Bottom _ = Bottom
intervalMinus _ Bottom = Bottom

intervalMul :: Interval -> Interval -> Interval
intervalMul (Interval l1 u1) (Interval l2 u2) = Interval (minimum [l1 * l2, l1 * u2, u1 * l2, u1 * u2]) (maximum [l1 * l2, l1 * u2, u1 * l2, u1 * u2])
intervalMul Top _ = Top
intervalMul _ Top = Top
intervalMul Bottom _ = Bottom
intervalMul _ Bottom = Bottom

intervalConst :: Double -> Interval
intervalConst x = Interval x x

-- Comparison of intervals Operations
intervalUnion :: Interval -> Interval -> Interval
intervalUnion (Interval l1 u1) (Interval l2 u2) = Interval (min l1 l2) (max u1 u2)
intervalUnion Top _ = Top
intervalUnion _ Top = Top
intervalUnion Bottom (Interval l1 u1) = Interval l1 u1
intervalUnion (Interval l1 u1) Bottom = Interval l1 u1
intervalUnion (Interval l1 u1) (Interval negativeInfinity positiveInfinity) = Interval negativeInfinity positiveInfinity
intervalUnion (Interval negativeInfinity positiveInfinity) (Interval l2 u2) = Interval negativeInfinity positiveInfinity
intervalUnion (Interval negativeInfinity positiveInfinity) _ = Interval negativeInfinity positiveInfinity
intervalUnion _ (Interval negativeInfinity positiveInfinity) = Interval negativeInfinity positiveInfinity
intervalUnion (Interval l1 u1) (Interval negativeInfinity u2) = Interval negativeInfinity (max u1 u2)
intervalUnion (Interval l1 u1) (Interval l2 positiveInfinity) = Interval (min l1 l2) positiveInfinity
intervalUnion (Interval negativeInfinity u1) (Interval l2 u2) = Interval negativeInfinity (max u1 u2)
intervalUnion (Interval l1 positiveInfinity) (Interval l2 u2) = Interval (min l1 l2) positiveInfinity

normaliser :: Interval -> Interval
normaliser (Interval a b) =
    if a <= b
    then Interval a b
    else Bottom

intervalIntersection :: Interval -> Interval -> Interval
intervalIntersection (Interval l1 u1) (Interval l2 u2) = normaliser (Interval (max l1 l2) (min u2 u1))
intervalIntersection (Interval l1 u1) Top = Interval l1 u1
intervalIntersection Top (Interval l1 u1) = Interval l1 u1
intervalIntersection Bottom _ = Bottom
intervalIntersection _ Bottom = Bottom
intervalIntersection (Interval l1 u1) (Interval negativeInfinity positiveInfinity) = Interval l1 u1
intervalIntersection (Interval negativeInfinity positiveInfinity) (Interval l2 u2) = Interval l2 u2
intervalIntersection (Interval negativeInfinity positiveInfinity) Top = Interval negativeInfinity positiveInfinity
intervalIntersection Top (Interval negativeInfinity positiveInfinity) = Interval negativeInfinity positiveInfinity
intervalIntersection (Interval l1 u1) (Interval negativeInfinity u2) = normaliser (Interval l1 (min u1 u2))
intervalIntersection (Interval l1 u1) (Interval l2 positiveInfinity) = normaliser (Interval (max l1 l2) u1)
intervalIntersection (Interval negativeInfinity u1) (Interval l2 u2) = normaliser (Interval l2 (min u1 u2))
intervalIntersection (Interval l1 positiveInfinity) (Interval l2 u2) = normaliser (Interval (max l1 l2) u2)

intervalEqual :: Interval -> Interval -> Interval
intervalEqual a b = Intervals (intervalIntersection a b) (intervalIntersection a b)

intervalLessThan :: Interval -> Interval -> Interval
intervalLessThan (Interval l1 u1) (Interval l2 u2) = Intervals (intervalIntersection (Interval l1 u1) (Interval negativeInfinity (u2 - 1))) (intervalIntersection (Interval (l1 + 1) positiveInfinity) (Interval l2 u2))

intervalLessThanAndEqualTo :: Interval -> Interval -> Interval
intervalLessThanAndEqualTo (Interval l1 u1) (Interval l2 u2) = Intervals (intervalIntersection (Interval l1 u1) (Interval negativeInfinity u2)) (intervalIntersection (Interval l1 positiveInfinity) (Interval l2 u2))


-- This is the DataType for CFG
-- fromList [(0,NonCF (Binary B32 Mov (Reg 0) (Imm 21)),1),(1,NonCF (Binary B64 Mov (Reg 1) (Imm 21)),2),(2,Assert Jeq (Reg 0) (Imm 0),4),(2,Assert Jne (Reg 0) (Imm 0),3),(3,NonCF (Binary B64 Add (Reg 0) (R (Reg 1))),4)]


-- Defining the Registers
-- newtype Register = Register Interval
--     deriving (Eq, Show)

-- r0 :: Register
-- r0 = Register (Interval 0 0)
