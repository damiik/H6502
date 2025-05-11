{-# LANGUAGE PatternSynonyms, BinaryLiterals #-}
module Assembly.Macros (
    -- Control Flow (from Assembly.ControlFlow)
    ifnzThen, ifzThen, ifneThen, ifeqThen, ifcThen, ifncThen,
    ifmThen, ifpThen, ifoThen, ifnoThen,
    whileZ, doWhileZ,
    whileNz, doWhileNz,
    whileC, doWhileC,
    whileNc, doWhileNc,
    whileM, doWhileM,
    whileP, doWhileP,
    whileO, doWhileO,
    whileNo, doWhileNo,
    whileEq, doWhileEq,
    whileNe, doWhileNe,
    doWhileX,
    doWhileY,
    doUntilX,
    doUntilY,   
    whileX, whileY,
    forX, forY,

    caseOf, caseOfA, caseOfX, caseOfY, caseOfAddr, caseOfZP, caseOfMultiBytes,

    -- Arithmetic/Logic (from Assembly.ArithmeticLogic) - Using apostrophe names
    cmp'r, cmp'y, cmp'x, adc'rb, add'rb, sub'rb, sub'br, decsum, binsum,

    -- Memory (from Assembly.Memory) - Using apostrophe names
    addAto16bit, sta'rb, sta'rw, sta'rrw, adc'rrw, add'rrw, copyBlock, fillMemory, swap'b, fillMemory,

    -- C64/Helpers (from C64.HelpersC64) - Using camelCase names based on definitions
    waitRaster, vicWaitLine, vicWaitBottom, vicWaitTop, configureVectors, printChar, printColorChar, printByte, hundreds2Petscii, tens2Petscii, macrosLib, skipNext2B,

    -- Other exported items from original (makeUniqueLabel is from Assembly.Core)
    makeUniqueLabel -- Keep in export list as it was originally
) where

-- Import necessary identifiers from the new modules
import Assembly.ControlFlow
import Assembly.ArithmeticLogic
import Assembly.Memory
import C64.HelpersC64
import Assembly.Core (makeUniqueLabel) -- makeUniqueLabel is defined in Core and re-exported here
