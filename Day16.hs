{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day16 where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Functor
import Numeric
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L


main :: IO ()
main = getInputAndSolve parser sumVersionNumbers evaluate
    where
        parser :: String -> Packet
        parser =
            parseInputRaw (parsePacket True) . show . parseInputRaw parseBitstring


-- SOLVE

sumVersionNumbers :: Packet -> Int
sumVersionNumbers =
    sum . getVersions
    where
        getVersions :: Packet -> [Int]
        getVersions = \case
            Literal v _ _ -> [v]
            Operator v _ subPackets -> v : concatMap getVersions subPackets

evaluate :: Packet -> Integer
evaluate = \case
    Literal _ _ v ->
        toInteger v
    Operator _ typeId (map evaluate -> subPackets) ->
        case typeId of
            0 ->
                sum subPackets
            1 ->
                product subPackets
            2 ->
                minimum subPackets
            3 ->
                maximum subPackets
            5 ->
                let [l, r] = subPackets
                in  if l > r then 1 else 0
            6 ->
                let [l, r] = subPackets
                in  if l < r then 1 else 0
            7 ->
                let [l, r] = subPackets
                in  if l == r then 1 else 0
            _ ->
                error $ "Unexpected typeId: " <> show typeId


-- HELPERS



-- PARSE

newtype Bitstring =
    Bitstring [Int]
    deriving (Read, Eq, Ord)

instance Show Bitstring where
    show (Bitstring is) = concatMap show is

parseBitstring :: ReadP Bitstring
parseBitstring = do
    hexChars <- many1 $ satisfy (\c -> isDigit c || c `elem` ['A' .. 'F'])
    void newline
    let hexInts = map (fst . head . readHex . (: [])) hexChars
    return $ Bitstring $ concatMap splitHexInt hexInts
    where
        splitHexInt :: Int -> [Int]
        splitHexInt i =
            map (getBit i) [3, 2 .. 0]
        getBit :: (Bits a, Num a) => a -> Int -> a
        getBit byte ix =
            if testBit byte ix then
                1
            else
                0

data Packet
    = Literal !Int !Int !Int
    | Operator !Int !Int ![Packet]
    deriving (Show)

parsePacket :: Bool -> ReadP Packet
parsePacket isOutermostPacket = do
    version <- toInt <$> count 3 readBit
    pType <- toInt <$> count 3 readBit
    packet <- case pType of
        4 ->
            Literal version pType <$> parseLiteral
        _ -> do
            lengthType <- readBit
            Operator version pType <$> parseOperator lengthType
    when isOutermostPacket . void . many $ char '0'
    return packet
    where
        parseLiteral :: ReadP Int
        parseLiteral = do
            leading <- concat <$> many (char '1' *> count 4 readBit)
            final <- char '0' *> count 4 readBit
            return $ toInt $ leading <> final
        parseOperator :: Int -> ReadP [Packet]
        parseOperator = \case
            0 -> do
                subpacketBitCount <- toInt <$> count 15 readBit
                subpacketsBitstring <- count subpacketBitCount (choice [char '0', char '1'])
                let subpackets = parseInputRaw (many1 $ parsePacket False) subpacketsBitstring
                return subpackets
            1 -> do
                subpacketCount <- toInt <$> count 11 readBit
                count subpacketCount $ parsePacket False
            e -> error $ "unexpected length type: " <> show e
        readBit :: ReadP Int
        readBit =
            choice
                [ char '1' $> 1
                , char '0' $> 0
                ]
        toInt :: [Int] -> Int
        toInt =
            L.foldl'
                (\i b ->
                    shiftL i 1 .|. b
                )
                0
