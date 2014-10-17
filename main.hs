import Control.Concurrent (forkIO)

import System.IO
import System.Environment

import Control.Monad

import qualified Data.ByteString as B
import Data.Word

import qualified Data.Map.Strict as M

import Huffman
import ByteStream

main = do
	[srcName, destName] <- getArgs

	withFile srcName ReadMode (\handle -> do
		contents <- hGetContents handle

		let hTree = stringTree contents
		let encodedBits = encode' hTree contents :: [Bool]

		{-write to file encoded-}
		bitsToFile destName encodedBits

		{-get from file encoded-}
		bits <- bitsFromFile destName

		putStrLn $ decode hTree bits

		return ()
		)