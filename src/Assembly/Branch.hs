{-# LANGUAGE LambdaCase #-}

module Assembly.Branch (
    BranchMnemonic(..),
    -- Nie eksportujemy emitBranch bezpośrednio stąd,
    -- lepiej żeby główny moduł Assembly go re-eksportował
    -- jeśli jest potrzebny użytkownikowi końcowemu.
    -- Funkcje pomocnicze jak emitBranch będą używane wewnętrznie.
) where

import Data.Word (Word8)
import qualified Data.Map.Strict as Map
-- WAŻNE: Importujemy potrzebne rzeczy z Core
-- import Assembly.Core.Types (Mnemonic(..), AddressingMode(..), instructionTable) -- Załóżmy, że typy są w Core.Types

-- Definicja przeniesiona z Core.hs
data BranchMnemonic = B_BNE | B_BEQ | B_BCS | B_BCC | B_BMI | B_BPL | B_BVS | B_BVC
    deriving (Show, Eq, Ord, Enum, Bounded)
