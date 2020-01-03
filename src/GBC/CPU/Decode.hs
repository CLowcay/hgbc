{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GBC.CPU.Decode
  ( MonadFetch(..)
  , FetchAndExecute(..)
  , fetchAndExecute
  , table0
  , table1
  )
where

import           GBC.CPU.ISA
import           Common
import           Data.Bits
import           Data.Word
import qualified Data.Vector                   as V

class Monad m => MonadFetch m where
  nextByte :: m Word8
  nextWord :: m Word16

class FetchAndExecute p m where
  type InnerExecuteResult p m
  execute :: ExecuteResult p -> m (InnerExecuteResult p m)

{-# INLINE fetchAndExecute #-}
fetchAndExecute
  :: (ExecuteUnit p, MonadFetch m, FetchAndExecute p m) => p -> m (InnerExecuteResult p m)
fetchAndExecute p = do
  b0 <- nextByte
  V.unsafeIndex table0 (fromIntegral b0) p

table0
  :: (ExecuteUnit p, MonadFetch m, FetchAndExecute p m)
  => V.Vector (p -> m (InnerExecuteResult p m))
table0 = V.generate 256 (decodeByte0 . splitByte . fromIntegral)

table1 :: (ExecuteUnit p) => V.Vector (p -> ExecuteResult p)
table1 = V.generate 256 (decodeByte1 . splitByte . fromIntegral)

decodeByte0
  :: (ExecuteUnit p, MonadFetch m, FetchAndExecute p m)
  => (Word8, Word8, Word8)
  -> (p -> m (InnerExecuteResult p m))
decodeByte0 (0, 0, 0) = \p -> execute $ nop p
decodeByte0 (0, 1, 0) = \p -> execute . ldnnSP p =<< nextWord
decodeByte0 (0, 2, 0) = \p -> do
  b1 <- nextByte
  execute $ if b1 == 0 then stop p else invalid p 0o020
decodeByte0 (0, 3, 0) = \p -> execute . jr p . fromIntegral =<< nextByte
decodeByte0 (0, xcc, 0) =
  let cc = conditionCode (xcc .&. 3) in \p -> execute . jrcc p cc . fromIntegral =<< nextByte

decodeByte0 (0, xss, 1) =
  let ss = registerSS (xss .&. 6)
  in  if xss .&. 1 == 0
        then \p -> execute . ldddnn p ss =<< nextWord
        else \p -> execute $ addhlss p ss

decodeByte0 (0, 0, 2) = \p -> execute $ ldBCa p
decodeByte0 (0, 1, 2) = \p -> execute $ ldaBC p
decodeByte0 (0, 2, 2) = \p -> execute $ ldDEa p
decodeByte0 (0, 3, 2) = \p -> execute $ ldaDE p
decodeByte0 (0, 4, 2) = \p -> execute $ ldHLIa p
decodeByte0 (0, 5, 2) = \p -> execute $ ldaHLI p
decodeByte0 (0, 6, 2) = \p -> execute $ ldHLDa p
decodeByte0 (0, 7, 2) = \p -> execute $ ldaHLD p

decodeByte0 (0, xss, 3) =
  let ss = registerSS (xss .&. 6)
  in  if xss .&. 1 == 0 then \p -> execute $ incss p ss else \p -> execute $ decss p ss

decodeByte0 (0, 6, 4) = \p -> execute $ inchl p
decodeByte0 (0, r, 4) = let reg = register r in \p -> execute $ incr p reg
decodeByte0 (0, 6, 5) = \p -> execute $ dechl p
decodeByte0 (0, r, 5) = let reg = register r in \p -> execute $ decr p reg
decodeByte0 (0, 6, 6) = \p -> execute . ldHLn p =<< nextByte
decodeByte0 (0, r, 6) = let reg = register r in \p -> execute . ldrn p reg =<< nextByte
decodeByte0 (0, 0, 7) = \p -> execute $ rlca p
decodeByte0 (0, 1, 7) = \p -> execute $ rrca p
decodeByte0 (0, 2, 7) = \p -> execute $ rla p
decodeByte0 (0, 3, 7) = \p -> execute $ rra p
decodeByte0 (0, 4, 7) = \p -> execute $ daa p
decodeByte0 (0, 5, 7) = \p -> execute $ cpl p
decodeByte0 (0, 6, 7) = \p -> execute $ scf p
decodeByte0 (0, 7, 7) = \p -> execute $ ccf p

decodeByte0 (1, 6, 6) = \p -> execute $ halt p
decodeByte0 (1, r, 6) = let reg = register r in \p -> execute $ ldrHL p reg
decodeByte0 (1, 6, r) = let reg = register r in \p -> execute $ ldHLr p reg
decodeByte0 (1, r1, r2) =
  let reg1 = register r1
      reg2 = register r2
  in  \p -> execute $ ldrr p reg1 reg2

decodeByte0 (2, 0  , 6) = \p -> execute $ addhl p
decodeByte0 (2, 0  , r) = let reg = register r in \p -> execute $ addr p reg
decodeByte0 (2, 1  , 6) = \p -> execute $ adchl p
decodeByte0 (2, 1  , r) = let reg = register r in \p -> execute $ adcr p reg
decodeByte0 (2, 2  , 6) = \p -> execute $ subhl p
decodeByte0 (2, 2  , r) = let reg = register r in \p -> execute $ subr p reg
decodeByte0 (2, 3  , 6) = \p -> execute $ sbchl p
decodeByte0 (2, 3  , r) = let reg = register r in \p -> execute $ sbcr p reg
decodeByte0 (2, 4  , 6) = \p -> execute $ andhl p
decodeByte0 (2, 4  , r) = let reg = register r in \p -> execute $ andr p reg
decodeByte0 (2, 5  , 6) = \p -> execute $ xorhl p
decodeByte0 (2, 5  , r) = let reg = register r in \p -> execute $ xorr p reg
decodeByte0 (2, 6  , 6) = \p -> execute $ orhl p
decodeByte0 (2, 6  , r) = let reg = register r in \p -> execute $ orr p reg
decodeByte0 (2, 7  , 6) = \p -> execute $ cphl p
decodeByte0 (2, 7  , r) = let reg = register r in \p -> execute $ cpr p reg

decodeByte0 (3, 4  , 0) = \p -> execute . ldna p =<< nextByte
decodeByte0 (3, 5  , 0) = \p -> execute . addSP p . fromIntegral =<< nextByte
decodeByte0 (3, 6  , 0) = \p -> execute . ldan p =<< nextByte
decodeByte0 (3, 7  , 0) = \p -> execute . ldhl p . fromIntegral =<< nextByte
decodeByte0 (3, xcc, 0) = let cc = conditionCode xcc in \p -> execute $ retcc p cc
decodeByte0 (3, 1  , 1) = \p -> execute $ ret p
decodeByte0 (3, 3  , 1) = \p -> execute $ reti p
decodeByte0 (3, 5  , 1) = \p -> execute $ jphl p
decodeByte0 (3, 7  , 1) = \p -> execute $ ldSPHL p
decodeByte0 (3, qqx, 1) = let qq = registerQQ qqx in \p -> execute $ pop p qq
decodeByte0 (3, 4  , 2) = \p -> execute $ ldCa p
decodeByte0 (3, 5  , 2) = \p -> execute . ldnna p =<< nextWord
decodeByte0 (3, 6  , 2) = \p -> execute $ ldaC p
decodeByte0 (3, 7  , 2) = \p -> execute . ldann p =<< nextWord
decodeByte0 (3, xcc, 2) = let cc = conditionCode xcc in \p -> execute . jpccnn p cc =<< nextWord
decodeByte0 (3, 0  , 3) = \p -> execute . jpnn p =<< nextWord
decodeByte0 (3, 6  , 3) = \p -> execute $ di p
decodeByte0 (3, 7  , 3) = \p -> execute $ ei p

decodeByte0 (3, xcc, 4) = if xcc .&. 0x4 /= 0
  then \p -> execute $ invalid p (0o304 .|. xcc .<<. 3)
  else let cc = conditionCode xcc in \p -> execute . callcc p cc =<< nextWord

decodeByte0 (3, 1  , 5) = \p -> execute . call p =<< nextWord
decodeByte0 (3, 3  , 5) = \p -> execute $ invalid p 0o335
decodeByte0 (3, 5  , 5) = \p -> execute $ invalid p 0o355
decodeByte0 (3, 7  , 5) = \p -> execute $ invalid p 0o375
decodeByte0 (3, qqx, 5) = let qq = registerQQ qqx in \p -> execute $ push p qq
decodeByte0 (3, 0  , 6) = \p -> execute . addn p =<< nextByte
decodeByte0 (3, 1  , 6) = \p -> execute . adcn p =<< nextByte
decodeByte0 (3, 2  , 6) = \p -> execute . subn p =<< nextByte
decodeByte0 (3, 3  , 6) = \p -> execute . sbcn p =<< nextByte
decodeByte0 (3, 4  , 6) = \p -> execute . andn p =<< nextByte
decodeByte0 (3, 5  , 6) = \p -> execute . xorn p =<< nextByte
decodeByte0 (3, 6  , 6) = \p -> execute . orn p =<< nextByte
decodeByte0 (3, 7  , 6) = \p -> execute . cpn p =<< nextByte
decodeByte0 (3, t  , 7) = \p -> execute $ rst p t
decodeByte0 (3, 1  , 3) = \p -> do
  b1 <- nextByte
  execute $ V.unsafeIndex table1 (fromIntegral b1) p
decodeByte0 (a, b, c) = \p -> execute $ invalid p ((a .<<. 6) .|. (b .<<. 3) .|. c)

decodeByte1 :: ExecuteUnit p => (Word8, Word8, Word8) -> (p -> ExecuteResult p)
decodeByte1 (0, 0, 6) = rlchl
decodeByte1 (0, 0, r) = let reg = register r in \p -> rlcr p reg
decodeByte1 (0, 1, 6) = rrchl
decodeByte1 (0, 1, r) = let reg = register r in \p -> rrcr p reg
decodeByte1 (0, 2, 6) = rlhl
decodeByte1 (0, 2, r) = let reg = register r in \p -> rlr p reg
decodeByte1 (0, 3, 6) = rrhl
decodeByte1 (0, 3, r) = let reg = register r in \p -> rrr p reg
decodeByte1 (0, 4, 6) = slahl
decodeByte1 (0, 4, r) = let reg = register r in \p -> slar p reg
decodeByte1 (0, 5, 6) = srahl
decodeByte1 (0, 5, r) = let reg = register r in \p -> srar p reg
decodeByte1 (0, 6, 6) = swaphl
decodeByte1 (0, 6, r) = let reg = register r in \p -> swapr p reg
decodeByte1 (0, 7, 6) = srlhl
decodeByte1 (0, 7, r) = let reg = register r in \p -> srlr p reg
decodeByte1 (1, b, 6) = \p -> bithl p b
decodeByte1 (1, b, r) = let reg = register r in \p -> bitr p reg b
decodeByte1 (2, b, 6) = \p -> reshl p b
decodeByte1 (2, b, r) = let reg = register r in \p -> resr p reg b
decodeByte1 (3, b, 6) = \p -> sethl p b
decodeByte1 (3, b, r) = let reg = register r in \p -> setr p reg b
decodeByte1 (a, b, c) = \p -> invalid p ((a .<<. 6) .|. (b .<<. 3) .|. c)

splitByte :: Word8 -> (Word8, Word8, Word8)
splitByte x = ((x .>>. 6) .&. 0x03, (x .>>. 3) .&. 0x07, x .&. 0x07)

register :: Word8 -> RegisterR
register 0o0 = RegB
register 0o1 = RegC
register 0o2 = RegD
register 0o3 = RegE
register 0o4 = RegH
register 0o5 = RegL
register 0o7 = RegA
register r   = error ("invalid register code " <> show r)

registerQQ :: Word8 -> RegisterQQ
registerQQ 0 = PushPopBC
registerQQ 2 = PushPopDE
registerQQ 4 = PushPopHL
registerQQ 6 = PushPopAF
registerQQ x = error ("invalid register pair code " <> show x)

registerSS :: Word8 -> RegisterSS
registerSS 0 = RegBC
registerSS 2 = RegDE
registerSS 4 = RegHL
registerSS 6 = RegSP
registerSS x = error ("invalid register pair code " <> show x)

conditionCode :: Word8 -> ConditionCode
conditionCode 0 = CondNZ
conditionCode 1 = CondZ
conditionCode 2 = CondNC
conditionCode 3 = CondC
conditionCode x = error ("invalid condition code " <> show x)
