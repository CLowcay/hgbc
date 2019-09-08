module GBC.ISA
  ( Register8(..)
  , Register16(..)
  , Operand8(..)
  , SmallOperand8(..)
  , ConditionCode(..)
  , Instruction(..)
  , clocks
  , formatOrLookup8
  , formatOrLookup16
  )
where

import           Data.Word
import           Common
import           Data.Int
import qualified Data.HashMap.Strict           as HM
import           Data.Bits

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
                 | LDSP                                -- ^ LD SP HL
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

-- | Calculate the number of clock cycles taken by an instruction.
clocks
  :: Instruction -- ^ The instruction to check.
  -> Bool        -- ^ True if the conditional branch is taken, otherwise False.
  -> Int
clocks (LD_R8 _ (R8 _))        _          = 4
clocks (LD_R8 _ (I8 _))        _          = 8
clocks (LD_R8 _ HLI   )        _          = 8
clocks (LDHLI_R8 _    )        _          = 8
clocks (LDHLI_I8 _    )        _          = 12
clocks LDA_BCI                 _          = 8
clocks LDA_DEI                 _          = 8
clocks LDA_CI                  _          = 8
clocks LDCI_A                  _          = 8
clocks (LDA_I8I  _)            _          = 12
clocks (LDI8I_A  _)            _          = 12
clocks (LDA_I16I _)            _          = 16
clocks (LDI16I_A _)            _          = 16
clocks LDA_INC                 _          = 8
clocks LDA_DEC                 _          = 8
clocks LDBCI_A                 _          = 8
clocks LDDEI_A                 _          = 8
clocks LDHLI_INC               _          = 8
clocks LDHLI_DEC               _          = 8
clocks (LD16_I16 _ _)          _          = 12
clocks LDSP                    _          = 8
clocks (PUSH      _          ) _          = 16
clocks (POP       _          ) _          = 12
clocks (LDHL      _          ) _          = 12
clocks (LDI16I_SP _          ) _          = 20
clocks (ADD       (R8 _)     ) _          = 4
clocks (ADD       _          ) _          = 8
clocks (ADC       (R8 _)     ) _          = 4
clocks (ADC       _          ) _          = 8
clocks (SUB       (R8 _)     ) _          = 4
clocks (SUB       _          ) _          = 8
clocks (SBC       (R8 _)     ) _          = 4
clocks (SBC       _          ) _          = 8
clocks (AND       (R8 _)     ) _          = 4
clocks (AND       _          ) _          = 8
clocks (OR        (R8 _)     ) _          = 4
clocks (OR        _          ) _          = 8
clocks (XOR       (R8 _)     ) _          = 4
clocks (XOR       _          ) _          = 8
clocks (CP        (R8 _)     ) _          = 4
clocks (CP        _          ) _          = 8
clocks (INC       (SmallR8 _)) _          = 4
clocks (INC       SmallHLI   ) _          = 12
clocks (DEC       (SmallR8 _)) _          = 4
clocks (DEC       SmallHLI   ) _          = 12
clocks (ADDHL     _          ) _          = 8
clocks (ADDSP     _          ) _          = 16
clocks (INC16     _          ) _          = 8
clocks (DEC16     _          ) _          = 8
clocks RLCA                    _          = 4
clocks RLA                     _          = 4
clocks RRCA                    _          = 4
clocks RRA                     _          = 4
clocks (RLC  (SmallR8 _) )     _          = 8
clocks (RLC  SmallHLI    )     _          = 16
clocks (RL   (SmallR8 _) )     _          = 8
clocks (RL   SmallHLI    )     _          = 16
clocks (RRC  (SmallR8 _) )     _          = 8
clocks (RRC  SmallHLI    )     _          = 16
clocks (RR   (SmallR8 _) )     _          = 8
clocks (RR   SmallHLI    )     _          = 16
clocks (SLA  (SmallR8 _) )     _          = 8
clocks (SLA  SmallHLI    )     _          = 16
clocks (SRA  (SmallR8 _) )     _          = 8
clocks (SRA  SmallHLI    )     _          = 16
clocks (SRL  (SmallR8 _) )     _          = 8
clocks (SRL  SmallHLI    )     _          = 16
clocks (SWAP (SmallR8 _) )     _          = 8
clocks (SWAP SmallHLI    )     _          = 16
clocks (BIT _ (SmallR8 _))     _          = 8
clocks (BIT _ SmallHLI   )     _          = 12
clocks (SET _ (SmallR8 _))     _          = 8
clocks (SET _ SmallHLI   )     _          = 16
clocks (RES _ (SmallR8 _))     _          = 8
clocks (RES _ SmallHLI   )     _          = 16
clocks (JP  _ _          )     takeBranch = if takeBranch then 16 else 12
clocks (JR  _ _          )     takeBranch = if takeBranch then 12 else 8
clocks JPI                     _          = 4
clocks (CALL _ _)              takeBranch = if takeBranch then 24 else 12
clocks RETI                    _          = 16
clocks (RET Nothing)           _          = 16
clocks (RET _      )           takeBranch = if takeBranch then 20 else 8
clocks (RST _      )           _          = 16
clocks DAA                     _          = 4
clocks CPL                     _          = 4
clocks NOP                     _          = 4
clocks HALT                    _          = 4
clocks STOP                    _          = 4
clocks CCF                     _          = 4
clocks SCF                     _          = 4
clocks DI                      _          = 4
clocks EI                      _          = 4

formatOrLookup16 :: SymbolTable -> Word16 -> String
formatOrLookup16 table value =
  let SymbolTable reverseMap _ = table
  in  maybe (formatHex value) (\l -> formatHex value ++ "@" ++ l) $ HM.lookup value reverseMap

formatOrLookup8 :: SymbolTable -> Word8 -> String
formatOrLookup8 table value = formatOrLookup16 table $ 0xFF00 .|. fromIntegral value

instance Format Instruction where
  formatWithSymbolTable _     (LD_R8 r8 o8)  = "LD " ++ format r8 ++ ", " ++ format o8
  formatWithSymbolTable _     (LDHLI_R8 r8)  = "LD (HL), " ++ format r8
  formatWithSymbolTable _     (LDHLI_I8 w8)  = "LD (HL), " ++ formatHex w8
  formatWithSymbolTable _     LDA_BCI        = "LD A, (BC)"
  formatWithSymbolTable _     LDA_DEI        = "LD A, (DE)"
  formatWithSymbolTable _     LDA_CI         = "LD A, (C)"
  formatWithSymbolTable _     LDCI_A         = "LD (C), A"
  formatWithSymbolTable table (LDA_I8I  w8 ) = "LD A, (" ++ formatOrLookup8 table w8 ++ ")"
  formatWithSymbolTable table (LDI8I_A  w8 ) = "LD (" ++ formatOrLookup8 table w8 ++ "), A"
  formatWithSymbolTable table (LDA_I16I w16) = "LD A, (" ++ formatOrLookup16 table w16 ++ ")"
  formatWithSymbolTable table (LDI16I_A w16) = "LD (" ++ formatOrLookup16 table w16 ++ "), A"
  formatWithSymbolTable _     LDA_INC        = "LD A, (HLI)"
  formatWithSymbolTable _     LDA_DEC        = "LD A, (HLD)"
  formatWithSymbolTable _     LDBCI_A        = "LD (BC), A"
  formatWithSymbolTable _     LDDEI_A        = "LD (DE), A"
  formatWithSymbolTable _     LDHLI_INC      = "LD (HLI), A"
  formatWithSymbolTable _     LDHLI_DEC      = "LD (HLD), A"
  formatWithSymbolTable table (LD16_I16 r16 w16) =
    "LD " ++ format r16 ++ " " ++ formatOrLookup16 table w16
  formatWithSymbolTable _     LDSP              = "LD SP, HL"
  formatWithSymbolTable _     (PUSH      RegSP) = "PUSH AF"
  formatWithSymbolTable _     (PUSH      r16  ) = "PUSH " ++ format r16
  formatWithSymbolTable _     (POP       RegSP) = "POP AF"
  formatWithSymbolTable _     (POP       r16  ) = "POP " ++ format r16
  formatWithSymbolTable _     (LDHL      i8   ) = "LDHL " ++ show i8
  formatWithSymbolTable table (LDI16I_SP w16  ) = "LD (" ++ formatOrLookup16 table w16 ++ "), SP"
  formatWithSymbolTable _     (ADD       o8   ) = "ADD " ++ format o8
  formatWithSymbolTable _     (ADC       o8   ) = "ADC " ++ format o8
  formatWithSymbolTable _     (SUB       o8   ) = "SUB " ++ format o8
  formatWithSymbolTable _     (SBC       o8   ) = "SBC " ++ format o8
  formatWithSymbolTable _     (AND       o8   ) = "AND " ++ format o8
  formatWithSymbolTable _     (OR        o8   ) = "OR " ++ format o8
  formatWithSymbolTable _     (XOR       o8   ) = "XOR " ++ format o8
  formatWithSymbolTable _     (CP        o8   ) = "CP " ++ format o8
  formatWithSymbolTable _     (INC       so8  ) = "INC " ++ format so8
  formatWithSymbolTable _     (DEC       so8  ) = "DEC " ++ format so8
  formatWithSymbolTable _     (ADDHL     r16  ) = "ADD HL " ++ format r16
  formatWithSymbolTable _     (ADDSP     i8   ) = "ADD SP " ++ formatHex i8
  formatWithSymbolTable _     (INC16     r16  ) = "INC " ++ format r16
  formatWithSymbolTable _     (DEC16     r16  ) = "INC " ++ format r16
  formatWithSymbolTable _     RLCA              = "RLCA"
  formatWithSymbolTable _     RLA               = "RLA"
  formatWithSymbolTable _     RRCA              = "RRCA"
  formatWithSymbolTable _     RRA               = "RRA"
  formatWithSymbolTable _     (RLC  so8       ) = "RLC " ++ format so8
  formatWithSymbolTable _     (RL   so8       ) = "RL " ++ format so8
  formatWithSymbolTable _     (RRC  so8       ) = "RRC " ++ format so8
  formatWithSymbolTable _     (RR   so8       ) = "RR " ++ format so8
  formatWithSymbolTable _     (SLA  so8       ) = "SLA " ++ format so8
  formatWithSymbolTable _     (SRA  so8       ) = "SRA " ++ format so8
  formatWithSymbolTable _     (SRL  so8       ) = "SRL " ++ format so8
  formatWithSymbolTable _     (SWAP so8       ) = "SWAP " ++ format so8
  formatWithSymbolTable _     (BIT w8      so8) = "BIT " ++ formatHex w8 ++ ", " ++ format so8
  formatWithSymbolTable _     (SET w8      so8) = "SET " ++ formatHex w8 ++ ", " ++ format so8
  formatWithSymbolTable _     (RES w8      so8) = "RES " ++ formatHex w8 ++ ", " ++ format so8
  formatWithSymbolTable table (JP  Nothing w16) = "JP " ++ formatOrLookup16 table w16
  formatWithSymbolTable table (JP (Just cc) w16) =
    "JP " ++ format cc ++ ", " ++ formatOrLookup16 table w16
  formatWithSymbolTable _     (JR Nothing   i8)  = "JR " ++ show i8
  formatWithSymbolTable _     (JR (Just cc) i8)  = "JR " ++ format cc ++ ", " ++ show i8
  formatWithSymbolTable _     JPI                = "JP (HL)"
  formatWithSymbolTable table (CALL Nothing w16) = "CALL " ++ formatOrLookup16 table w16
  formatWithSymbolTable table (CALL (Just cc) w16) =
    "CALL " ++ format cc ++ ", " ++ formatOrLookup16 table w16
  formatWithSymbolTable _ RETI            = "RETI"
  formatWithSymbolTable _ (RET Nothing  ) = "RET"
  formatWithSymbolTable _ (RET (Just cc)) = "RET " ++ format cc
  formatWithSymbolTable _ (RST t        ) = "RST" ++ formatHex t
  formatWithSymbolTable _ DAA             = "DAA"
  formatWithSymbolTable _ CPL             = "CPL"
  formatWithSymbolTable _ NOP             = "NOP"
  formatWithSymbolTable _ HALT            = "HALT"
  formatWithSymbolTable _ STOP            = "STOP"
  formatWithSymbolTable _ DI              = "DI"
  formatWithSymbolTable _ EI              = "EI"
  formatWithSymbolTable _ CCF             = "CCF"
  formatWithSymbolTable _ SCF             = "SCF"
