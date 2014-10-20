module ByteStream where

import qualified Data.ByteString as B
import Data.Word

import qualified Data.Map.Strict as M

import System.IO
import System.Environment

import Control.Monad

data Info = Info {
	offset :: Int
	, size :: Int
} deriving (Show)

type BitChunks = (Info, [[Bool]])

type ByteChunks = (Info, [Word8])

asString :: [Bool] -> String
asString = map (\bool -> if bool then '1' else '0')

bitsToBytes :: [Bool] -> ByteChunks
bitsToBytes [] = (Info {size = 0, offset = 0}, [])
bitsToBytes bits = (fst bitChunks, map bitsToByte $ snd $ bitChunks)
	where 
		bitChunks = getChunks bits

bitsToByte :: [Bool] -> Word8
bitsToByte = foldl (\byte bit -> byte * 2 + if bit then 1 else 0) 0

getChunks :: [Bool] -> BitChunks
getChunks [] = (Info {size = 0, offset = 0}, [])
getChunks xs
	| length xs < 8 = (Info {size = 1, offset = length xs}, [take 8 (xs ++ repeat False)])
	| otherwise = (let (these, rest) = splitAt 8 xs;
					(Info {size = len, offset = offs}, bitChunks) = getChunks rest
				    in (Info {size = len + 1, offset = offs}, these:bitChunks))

bytesToBits :: B.ByteString -> [Bool]
bytesToBits = B.foldl (\bits byte -> bits ++ (snd $ byteToBits 8 byte)) []

{-given number of significant digits converts to that form in bits-}
byteToBits :: Int -> Word8 -> (Int, [Bool])
byteToBits nDigits byte
	| byte == 0 = (nDigits, take nDigits (repeat False))
	| otherwise = let (count, lowerBits) = (byteToBits (nDigits - 1) (byte `div` 2))
					in (nDigits, lowerBits ++ [(if byte `mod` 2 == 1 then True else False)])

{-here's how it converts: 1026 -> [2, 4]-}
toWord8 :: Int -> [Word8]
toWord8 x
	| integer < base = [fromInteger integer :: Word8]
	| otherwise = rem : lowerParts
	where
		base = toInteger 256
		integer = toInteger x
		rem = fromInteger $ integer `mod` base
		lowerParts = toWord8 $ (fromInteger integer) `div` (fromInteger base)

fromWord8 :: [Word8] -> Int
fromWord8 = foldr (\byte integer -> integer * 256 + (fromInteger $ toInteger byte)) 0