module Assembler
  ( Reg(..)
  , Operand(..)
  , Instr(..)
  , regCode
  , rex
  , modRM
  , encodeInstr
  ) where

import Data.Bits ((.|.), shiftL)
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import qualified Data.ByteString.Builder as B

data Reg
  = RAX | RCX | RDX | RBX
  | RSP | RBP | RSI | RDI
  | R8 | R9 | R10 | R11
  | R12 | R13 | R14 | R15
  deriving (Eq, Show)

data Operand
  = RegOp Reg
  | ImmOp Int64
  deriving (Eq, Show)

data Instr
  = Mov Operand Operand
  | Add Operand Operand
  | Sub Operand Operand
  | Push Reg
  | Pop Reg
  | Call Int32
  | Ret
  deriving (Eq, Show)

-- Register hardware codes (0..7) and whether they need a REX extension bit
regCode :: Reg -> (Word8, Bool)
regCode r = case r of
  RAX -> (0, False); RCX -> (1, False); RDX -> (2, False); RBX -> (3, False)
  RSP -> (4, False); RBP -> (5, False); RSI -> (6, False); RDI -> (7, False)
  R8  -> (0, True);  R9  -> (1, True);  R10 -> (2, True);  R11 -> (3, True)
  R12 -> (4, True);  R13 -> (5, True);  R14 -> (6, True);  R15 -> (7, True)

-- Build 64-bit REX prefix (0x48 = 64-bit operand size)
rex :: Bool -> Bool -> Bool -> Word8
rex w r b = 0x40
        .|. (if w then 0x08 else 0)
        .|. (if r then 0x04 else 0) -- extends ModR/M reg field
        .|. (if b then 0x01 else 0) -- extends ModR/M r/m or opcode reg field

-- Build ModR/M byte: mod (2 bits) | reg (3 bits) | r/m (3 bits)
modRM :: Word8 -> Word8 -> Word8 -> Word8
modRM modBits reg rm =
  (modBits `shiftL` 6) .|. (reg `shiftL` 3) .|. rm

encodeInstr :: Instr -> B.Builder
encodeInstr instr = case instr of
  -- movq src_reg, dst_reg  (Opcode 0x89: MOV r/m64, r64)
  Mov (RegOp src) (RegOp dst) ->
    let (sCode, sExt) = regCode src
        (dCode, dExt) = regCode dst
    in B.word8 (rex True sExt dExt)
    <> B.word8 0x89
    <> B.word8 (modRM 0b11 sCode dCode)

  -- movq $imm64, dst_reg  (Opcode 0xB8 + rd: MOV r64, imm64)
  Mov (ImmOp imm) (RegOp dst) ->
    let (dCode, dExt) = regCode dst
    in B.word8 (rex True False dExt)
    <> B.word8 (0xB8 + dCode)
    <> B.int64LE imm

  -- addq src_reg, dst_reg  (Opcode 0x01: ADD r/m64, r64)
  Add (RegOp src) (RegOp dst) ->
    let (sCode, sExt) = regCode src
        (dCode, dExt) = regCode dst
    in B.word8 (rex True sExt dExt)
    <> B.word8 0x01
    <> B.word8 (modRM 0b11 sCode dCode)

  -- subq src_reg, dst_reg  (Opcode 0x29: SUB r/m64, r64)
  Sub (RegOp src) (RegOp dst) ->
    let (sCode, sExt) = regCode src
        (dCode, dExt) = regCode dst
    in B.word8 (rex True sExt dExt)
    <> B.word8 0x29
    <> B.word8 (modRM 0b11 sCode dCode)

  -- pushq reg  (Opcode 0x50 + rd)
  Push r ->
    let (code, ext) = regCode r
    in (if ext then B.word8 (rex False False True) else mempty)
    <> B.word8 (0x50 + code)

  -- popq reg   (Opcode 0x58 + rd)
  Pop r ->
    let (code, ext) = regCode r
    in (if ext then B.word8 (rex False False True) else mempty)
    <> B.word8 (0x58 + code)

  -- call rel32
  Call displacement ->
    B.word8 0xE8 <> B.int32LE displacement

  Ret ->
    B.word8 0xC3

  Mov _ _ -> error "Mov supports register sources/destinations or an immediate source and register destination"
  Add _ _ -> error "Add requires two register operands"
  Sub _ _ -> error "Sub requires two register operands"
