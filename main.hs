import Control.Concurrent (forkIO)

import System.IO
import System.Environment
import System.Directory

import Control.Monad

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.List.Split
import qualified Data.Maybe as Maybe

import Huffman
import ByteStream

main = do
	(commandArg:name:args) <- getArgs

	let maybeCommand = lookup commandArg commands

	if Maybe.isJust maybeCommand then
		let (Just command) = maybeCommand in command name (head args)
	else
		if commandArg == "encodeFiles" then
			encodeFiles args name
		else
			decodeFiles name

encodeFiles :: [String] -> String -> IO ()
encodeFiles srcNames destName = do
	{-get all files into one string-}
	contents <- mapM (\name -> readFile name) srcNames
	let content = L.intercalate contentDelimiter contents
	let nameData = L.intercalate nameDelimiter srcNames
	let metaAndData = L.intercalate waterLine [nameData, content]

	{-write that string to temp file-}
	(tempName, tempHandle) <- openTempFile "." "temp"
	hClose tempHandle
	writeFile tempName metaAndData

	{-encode that file-}
	encodeFile tempName destName

	removeFile tempName

	return ()

decodeFiles :: String -> IO ()
decodeFiles destName = do
	{-decode all into temp file-}
	(tempName, tempHandle) <- openTempFile "." "temp"
	hClose tempHandle
	decodeFile destName tempName

	{-extract names and contents from temp file-}
	content <- readFile tempName
	let [nameData, contentData] = splitOn waterLine content
	let names = splitOn nameDelimiter nameData
	let contents = splitOn contentDelimiter contentData

	mapM (\(name, content) -> writeFile name content) $ L.zip names contents

	removeFile tempName

	return ()

contentDelimiter = "#@(*Y#"
nameDelimiter = "#@(*$Y"
waterLine = "*&#@$^)"

commands :: [(String, (String -> String -> IO ()))]
commands = [("decode", decodeFile),
			("encode", encodeFile)
		]