-- TODO: remove len from Packet ADT
-- Instead, just have PacketParser return (Packer, Int)
import Text.Parsec
import ParsecUtil
import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- fmap (hexToBinary . head . lines) $ readFile "day16.in"
  let (Right packet) = applyParser packetParser input
  print $ versionSum packet
  print $ calcValue packet
  return ()

versionSum :: Packet -> Int
versionSum (Lit _ version _ _) = version
versionSum (Operator _ version _ ps_) = version + (sum $ fmap versionSum ps_)

calcValue :: Packet -> Int
calcValue (Lit _ _ _ v) = v
calcValue (Operator _ _ 0 ps) = sum $ fmap calcValue ps
calcValue (Operator _ _ 1 ps) = product $ fmap calcValue ps
calcValue (Operator _ _ 2 ps) = minimum $ fmap calcValue ps
calcValue (Operator _ _ 3 ps) = maximum $ fmap calcValue ps
calcValue (Operator _ _ 5 [a, b]) = if (calcValue a) > (calcValue b) then 1 else 0
calcValue (Operator _ _ 6 [a, b]) = if (calcValue a) < (calcValue b) then 1 else 0
calcValue (Operator _ _ 7 [a, b]) = if (calcValue a) == (calcValue b) then 1 else 0

data Packet = Lit { len :: Int, version :: Int, typeID :: Int, value :: Int }
            | Operator { len :: Int, version :: Int, typeID :: Int, ps :: [Packet] }
            deriving Show

binDigit :: Parser Char
binDigit = char '0' <|> char '1'

packetParser :: Parser Packet
packetParser = do
  version <- fmap toDec $ count 3 binDigit
  typeID <- fmap toDec $ count 3 binDigit
  if typeID == 4 then do
    (value, l) <- parseValue
    return $ Lit (6 + l) version typeID (toDec value)
  else do
    lengthTypeID <- binDigit
    if lengthTypeID == '0' then do
      subPacketLength <- fmap toDec $ count 15 binDigit
      subPackets <- lenSubPacketParser subPacketLength
      let newLen = 22 + subPacketLength
      return $ Operator newLen version typeID subPackets
    else do
      numSubPackets <- fmap toDec $ count 11 binDigit
      subPackets <- count numSubPackets packetParser
      let newLen = 18 + (sum $ fmap len subPackets)
      return $ Operator newLen version typeID subPackets

lenSubPacketParser :: Int -> Parser [Packet]
lenSubPacketParser 0 = return []
lenSubPacketParser n = do
  p <- packetParser
  rest <- lenSubPacketParser (n - (len p))
  return (p:rest)

parseValue :: Parser (String, Int)
parseValue = do
  prefix <- binDigit
  d <- count 4 binDigit
  if (prefix == '1') then do
    (d', n) <- parseValue
    return (d ++ d', 5 + n)
  else return (d, 5)

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

hexToBinary :: String -> String
hexToBinary = concatMap single
  where
  single :: Char -> String
  single '0' = "0000"
  single '1' = "0001"
  single '2' = "0010"
  single '3' = "0011"
  single '4' = "0100"
  single '5' = "0101"
  single '6' = "0110"
  single '7' = "0111"
  single '8' = "1000"
  single '9' = "1001"
  single 'A' = "1010"
  single 'B' = "1011"
  single 'C' = "1100"
  single 'D' = "1101"
  single 'E' = "1110"
  single 'F' = "1111"