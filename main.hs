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
	[commandArg, srcName, destName] <- getArgs

	let (Just command) = lookup commandArg commands

	command srcName destName

commands :: [(String, (String -> String -> IO ()))]  
commands = [("decode", decodeFile),
			("encode", encodeFile)
		]