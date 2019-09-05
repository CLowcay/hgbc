module GBC.ISA where

import           Data.Word
import           Common
import           Data.Int

-- | An 8-bit register.
data Register8 = RegA | RegB | RegC | RegD | RegE | RegH | RegL deriving (Eq, Ord, Show, Bounded, Enum)

-- | A 16-bit register.
data Register16 = RegSP | RegBC | RegDE | RegHL deriving (Eq, Ord, Show, Bounded, Enum)

-- | An operand (register, immediate, or HL indirect).
data Operand8 = R8 Register8 | I8 !Word8 | HLI deriving (Eq, Ord, Show)

-- | A small operand (cannot be an immediate).
data SmallOperand8 = SmallR8 !Register8 | SmallHLI deriving (Eq, Ord, Show)

data ConditionCode = CondNZ | CondZ | CondNC | CondC deriving (Eq, Ord, Show, Bounded, Enum)

-- | Format an 8-bit register.
instance Format Register8 where
  format RegA = "A"
  format RegB = "B"
  format RegC = "C"
  format RegD = "D"
  format RegE = "E"
  format RegH = "H"
  format RegL = "L"

-- | Format a 16-bit register.
instance Format Register16 where
  format RegSP = "SP"
  format RegBC = "BC"
  format RegDE = "DE"
  format RegHL = "HL"

-- | Format an operand.
instance Format Operand8 where
  format (R8 r) = format r
  format (I8 r) = formatHex r
  format HLI    = "(HL)"

-- | Format a small operand.
instance Format SmallOperand8 where
  format (SmallR8 r) = format r
  format SmallHLI    = "(HL)"

-- | Format a condition code.
instance Format ConditionCode where
  format CondC  = "C"
  format CondNC = "NC"
  format CondZ  = "Z"
  format CondNZ = "NZ"

-- | A CPU instruction.
data Instruction = LD_R8 !Register8 !Operand8          -- ^ LD r8 \<r8|im8|(HL)\>
                 | LDHLI_R8 !Register8                 -- ^ LD (HL) r8
                 | LDHLI_I8 !Word8                     -- ^ LD (HL) im8
                 | LDA_BCI                             -- ^ LD A (BC)
                 | LDA_DEI                             -- ^ LD A (DE)
                 | LDA_CI                              -- ^ LD A (C)
                 | LDCI_A                              -- ^ LD (C) A
                 | LDA_I8I !Word8                      -- ^ LD A (im8)
                 | LDI8I_A !Word8                      -- ^ LD (im8) A
                 | LDA_I16I !Word16                    -- ^ LD A (im16)
                 | LDI16I_A !Word16                    -- ^ LD (im16) A
                 | LDA_INC                             -- ^ LD A (HL++)
                 | LDA_DEC                             -- ^ LD A (HL--)
                 | LDBCI_A                             -- ^ LD (BC) A
                 | LDDEI_A                             -- ^ LD (DE) A
                 | LDHLI_INC                           -- ^ LD (HL++) A
                 | LDHLI_DEC                           -- ^ LD (HL--) A
                 | LD16_I16 !Register16 !Word16        -- ^ LD r16 im16
                 | LDSP                                -- ^ LD HL SP
                 | PUSH !Register16                    -- ^ PUSH r16
                 | POP !Register16                     -- ^ POP r16
                 | LDHL !Int8                          -- ^ LDHL SP im8
                 | LDI16I_SP !Word16                   -- ^ LD (im16) SP

                 | ADD !Operand8                       -- ^ ADD \<r8|im8|(HL)\>
                 | ADC !Operand8                       -- ^ ADC \<r8|im8|(HL)\>
                 | SUB !Operand8                       -- ^ SUB \<r8|im8|(HL)\>
                 | SBC !Operand8                       -- ^ SBC \<r8|im8|(HL)\>
                 | AND !Operand8                       -- ^ AND \<r8|im8|(HL)\>
                 | OR !Operand8                        -- ^ OR \<r8|im8|(HL)\>
                 | XOR !Operand8                       -- ^ XOR \<r8|im8|(HL)\>
                 | CP !Operand8                        -- ^ CP \<r8|im8|(HL)\>
                 | INC !SmallOperand8                  -- ^ INC \<r8|(HL)\>
                 | DEC !SmallOperand8                  -- ^ DEC \<r8|(HL)\>

                 | ADDHL !Register16                   -- ^ ADD HL r16
                 | ADDSP !Int8                         -- ^ ADD SP im8
                 | INC16 !Register16                   -- ^ INC16 r16
                 | DEC16 !Register16                   -- ^ DEC16 r16

                 | RLCA                                -- ^ RLCA
                 | RLA                                 -- ^ RLA
                 | RRCA                                -- ^ RRCA
                 | RRA                                 -- ^ RRA
                 | RLC !SmallOperand8                  -- ^ RLC \<r8|(HL)\>
                 | RL !SmallOperand8                   -- ^ RL \<r8|(HL)\>
                 | RRC !SmallOperand8                  -- ^ RRC \<r8|(HL)\>
                 | RR !SmallOperand8                   -- ^ RR \<r8|(HL)\>
                 | SLA !SmallOperand8                  -- ^ SLA \<r8|(HL)\>
                 | SRA !SmallOperand8                  -- ^ SRA \<r8|(HL)\>
                 | SRL !SmallOperand8                  -- ^ SRL \<r8|(HL)\>
                 | SWAP !SmallOperand8                 -- ^ SWAP \<r8|(HL)\>
                 | BIT !Word8 !SmallOperand8           -- ^ BIT b \<r8|(HL)\>
                 | SET !Word8 !SmallOperand8           -- ^ SET b \<r8|(HL)\>
                 | RES !Word8 !SmallOperand8           -- ^ RES b \<r8|(HL)\>

                 | JP !(Maybe ConditionCode) !Word16   -- ^ JP cc im16
                 | JR !(Maybe ConditionCode) !Int8     -- ^ JR cc im8
                 | JPI                                 -- ^ JP (HL)
                 | CALL !(Maybe ConditionCode) !Word16 -- ^ CALL cc im16
                 | RETI                                -- ^ RETI
                 | RET !(Maybe ConditionCode)          -- ^ RET cc
                 | RST !Word8                          -- ^ RST t

                 | DAA                                 -- ^ DAA
                 | CPL                                 -- ^ CPL
                 | NOP                                 -- ^ NOP
                 | HALT                                -- ^ HALT
                 | STOP                                -- ^ STOP

                 | CCF                                 -- ^ CCF
                 | SCF                                 -- ^ SCF
                 | DI                                  -- ^ DI
                 | EI                                  -- ^ EI
                 deriving (Eq, Ord, Show)

instance Format Instruction where
  format (LD_R8 r8 o8)        = "LD " ++ format r8 ++ ", " ++ format o8
  format (LDHLI_R8 r8)        = "LD (HL) " ++ format r8
  format (LDHLI_I8 w8)        = "LD (HL) " ++ formatHex w8
  format LDA_BCI              = "LD A (BC)"
  format LDA_DEI              = "LD A (DE)"
  format LDA_CI               = "LD A (C)"
  format LDCI_A               = "LD (C) A"
  format (LDA_I8I  w8 )       = "LD A (FF" ++ formatHex w8 ++ ")"
  format (LDI8I_A  w8 )       = "LD (FF" ++ formatHex w8 ++ ") A"
  format (LDA_I16I w16)       = "LD A (" ++ formatHex w16 ++ ")"
  format (LDI16I_A w16)       = "LD (" ++ formatHex w16 ++ ") A"
  format LDA_INC              = "LD A (HL++)"
  format LDA_DEC              = "LD A (HL--)"
  format LDBCI_A              = "LD (BC) A"
  format LDDEI_A              = "LD (DE) A"
  format LDHLI_INC            = "LD (HL++) A"
  format LDHLI_DEC            = "LD (HL--) A"
  format (LD16_I16 r16 w16)   = "LD " ++ format r16 ++ " " ++ formatHex w16
  format LDSP                 = "LD HL SP"
  format (PUSH      RegSP)    = "PUSH AF"
  format (PUSH      r16  )    = "PUSH " ++ format r16
  format (POP       RegSP)    = "POP AF"
  format (POP       r16  )    = "POP " ++ format r16
  format (LDHL      i8   )    = "LDHL " ++ show i8
  format (LDI16I_SP w16  )    = "LD (" ++ formatHex w16 ++ ") SP"
  format (ADD       o8   )    = "ADD " ++ format o8
  format (ADC       o8   )    = "ADC " ++ format o8
  format (SUB       o8   )    = "SUB " ++ format o8
  format (SBC       o8   )    = "SBC " ++ format o8
  format (AND       o8   )    = "AND " ++ format o8
  format (OR        o8   )    = "OR " ++ format o8
  format (XOR       o8   )    = "XOR " ++ format o8
  format (CP        o8   )    = "CP " ++ format o8
  format (INC       so8  )    = "INC " ++ format so8
  format (DEC       so8  )    = "DEC " ++ format so8
  format (ADDHL     r16  )    = "ADD HL " ++ format r16
  format (ADDSP     i8   )    = "ADD SP " ++ formatHex i8
  format (INC16     r16  )    = "INC " ++ format r16
  format (DEC16     r16  )    = "INC " ++ format r16
  format RLCA                 = "RLCA"
  format RLA                  = "RLA"
  format RRCA                 = "RRCA"
  format RRA                  = "RRA"
  format (RLC  so8         )  = "RLC " ++ format so8
  format (RL   so8         )  = "RL " ++ format so8
  format (RRC  so8         )  = "RRC " ++ format so8
  format (RR   so8         )  = "RR " ++ format so8
  format (SLA  so8         )  = "SLA " ++ format so8
  format (SRA  so8         )  = "SRA " ++ format so8
  format (SRL  so8         )  = "SRL " ++ format so8
  format (SWAP so8         )  = "SWAP " ++ format so8
  format (BIT w8        so8)  = "BIT " ++ formatHex w8 ++ " " ++ format so8
  format (SET w8        so8)  = "SET " ++ formatHex w8 ++ " " ++ format so8
  format (RES w8        so8)  = "RES " ++ formatHex w8 ++ " " ++ format so8
  format (JP  Nothing   w16)  = "JP " ++ formatHex w16
  format (JP  (Just cc) w16)  = "JP " ++ format cc ++ " " ++ formatHex w16
  format (JR  Nothing   i8 )  = "JR " ++ show i8
  format (JR  (Just cc) i8 )  = "JR " ++ format cc ++ " " ++ show i8
  format JPI                  = "JP (HL)"
  format (CALL Nothing   w16) = "CALL " ++ formatHex w16
  format (CALL (Just cc) w16) = "CALL " ++ format cc ++ " " ++ formatHex w16
  format RETI                 = "RETI"
  format (RET Nothing  )      = "RET"
  format (RET (Just cc))      = "RET " ++ format cc
  format (RST t        )      = "RST" ++ formatHex t
  format DAA                  = "DAA"
  format CPL                  = "CPL"
  format NOP                  = "NOP"
  format HALT                 = "HALT"
  format STOP                 = "STOP"
  format DI                   = "DI"
  format EI                   = "EI"
  format CCF                  = "CCF"
  format SCF                  = "SCF"
