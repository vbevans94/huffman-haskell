module Huffman where

import System.IO
import System.Environment

import Control.Monad

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function

import ByteStream

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Word

class Eq a => Bits a where
    zer :: a
    one :: a

instance Bits Int where
    zer = 0
    one = 1

instance Bits Bool where
    zer = False
    one = True

instance Bits Char where
    zer = '0'
    one = '1'

type Codemap a = M.Map Char [a]

data HTree  = Leaf Char Int
            | Fork HTree HTree Int
            deriving (Show)

weight :: HTree -> Int
weight (Leaf _ w)    = w
weight (Fork _ _ w)  = w

merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

freqList :: String -> [(Char, Int)]
freqList = M.toList . M.fromListWith (+) . map (flip (,) 1)

buildTree :: [(Char, Int)] -> HTree
buildTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
    where  bld (t:[])    = t
           bld (a:b:cs)  = bld $ insertBy (compare `on` weight) (merge a b) cs

buildCodemap :: Bits a => HTree -> Codemap a
buildCodemap = M.fromList . buildCodelist
    where  buildCodelist (Leaf c w)    = [(c, [])]
           buildCodelist (Fork l r w)  = map (addBit zer) (buildCodelist l) ++ map (addBit one) (buildCodelist r)
             where addBit b = second (b :)

stringTree :: String -> HTree
stringTree = buildTree . freqList

stringCodemap :: Bits a => String -> Codemap a
stringCodemap = buildCodemap . stringTree

encode :: Bits a => Codemap a -> String -> [a]
encode m = concat . map (m M.!)

encode' :: Bits a => HTree -> String -> [a]
encode' t = encode $ buildCodemap t

decode :: Bits a => HTree -> [a] -> String
decode tree = dcd tree
    where  dcd (Leaf c _) []        = [c]
           dcd (Leaf c _) bs        = c : dcd tree bs
           dcd (Fork l r _) (b:bs)  = dcd (if b == zer then l else r) bs

printCodemap :: Codemap Bool -> IO [()]
printCodemap = mapM putStrLn . map (\(char, bits) -> char : ' ' : (show $ boolsToBits bits)) . M.toList

boolsToBits :: [Bool] -> String
boolsToBits = map (\bool -> if bool then '1' else '0')

treeToBits :: HTree -> [Bool]
treeToBits (Leaf c _) = True : (bytesToBits $ C.singleton c)
treeToBits (Fork l r _) = False : ((treeToBits l) ++ (treeToBits r))

bitsToTree :: [Bool] -> (HTree, [Bool])
bitsToTree [] = ((Leaf 'v' 0), []) {-just fake tree-}
bitsToTree (x:xs) = if x then ((Leaf (getChar xs) 1), restBits) else ((Fork leftTree rightTree 1), restAfterRight)
  where getChar xs = C.head $ B.singleton $ bitsToByte $ byteBits
        (byteBits, restBits) = splitAt 8 xs
        (leftTree, restAfterLeft) = bitsToTree xs
        (rightTree, restAfterRight) = bitsToTree restAfterLeft

{-<_size: size of __size(1 byte)><__size: size of _content in bytes(_size bytes)><_offset: count of non-empty bits in last byte(1 byte)><_content: bits as bytes(__size bytes)>-}
toFile :: String -> [Bool] -> HTree -> IO ()
toFile fileName bits hTree = withFile fileName WriteMode (\handle -> do
  let (Info {size = len, offset = offs}, words) = bitsToBytes bits
  
  let lengthAsBytes = toWord8 len

  let (Info {size = lenTree, offset = offsTree}, treeWords) = bitsToBytes $ treeToBits hTree

  B.hPut handle
    $ B.cons (fromInteger (toInteger $ length lengthAsBytes))
      $ B.append (B.pack lengthAsBytes)
        $ B.cons (fromInteger (toInteger offs))
          $ B.append (B.pack words) (B.cons (fromInteger (toInteger offsTree)) (B.pack treeWords))
  )

{-fetch from file formatted by the function above-}
fromFile :: String -> IO (([Bool], HTree))
fromFile fileName = withFile fileName ReadMode (\handle -> do
  contents <- B.hGetContents handle

  let (sizeSizeString, rest) = B.splitAt 1 contents
  let sizeSize = toInteger ((B.unpack $ sizeSizeString) !! 0)
  let sizeSizeInt = fromInteger sizeSize
  
  let (sizeString, restRest) = B.splitAt sizeSizeInt rest
  let size = fromWord8 $ B.unpack sizeString
  let sizeInt = fromInteger $ toInteger size
  
  let (offsetString, restRestRest) = B.splitAt 1 restRest
  let offset = fromInteger $ toInteger $ B.head offsetString

  let (bytes, restForTree) = B.splitAt sizeInt restRestRest
  let allBits = bytesToBits bytes

  let (treeOffsetString, treeBytes) = B.splitAt 1 restForTree
  let treeOffset = fromInteger $ toInteger $ B.head treeOffsetString

  let allTreeBits = bytesToBits treeBytes
  let tree = fst $ bitsToTree $ (take (length allTreeBits - 8 + treeOffset) allTreeBits)
  let bits = take (length allBits - 8 + offset) allBits

  return (bits, tree)
  )

encodeFile :: String -> String -> IO ()
encodeFile srcName destName = do
	contents <- readFile srcName
	let hTree = stringTree contents
	let encodedBits = encode' hTree contents :: [Bool]

	{-write to file encoded-}
	toFile destName encodedBits hTree


decodeFile :: String -> String -> IO ()
decodeFile srcName destName = do
	{-get from file encoded-}
	(bits, tree) <- fromFile srcName

	writeFile destName (decode tree bits)
	