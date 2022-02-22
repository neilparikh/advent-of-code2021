import Text.Parsec
import ParsecUtil
import Data.Char (digitToInt)
import Control.Monad (guard)

main :: IO ()
main = do
  input <- fmap (hexToBinary . head . lines) $ readFile "day16.in"
  let (Right p) = applyParser packet input
  print $ versionSum p
  print $ calcValue p
  return ()

versionSum :: Packet -> Int
versionSum (Lit version _) = version
versionSum (Operator version _ ps) = version + (sum $ fmap versionSum ps)

calcValue :: Packet -> Int
calcValue (Lit _ v) = v
calcValue (Operator _ 0 ps) = sum (calcValue <$> ps)
calcValue (Operator _ 1 ps) = product (calcValue <$> ps)
calcValue (Operator _ 2 ps) = minimum (calcValue <$> ps)
calcValue (Operator _ 3 ps) = maximum (calcValue <$> ps)
calcValue (Operator _ 5 [a, b]) = if (calcValue a) > (calcValue b) then 1 else 0
calcValue (Operator _ 6 [a, b]) = if (calcValue a) < (calcValue b) then 1 else 0
calcValue (Operator _ 7 [a, b]) = if (calcValue a) == (calcValue b) then 1 else 0

data Packet = Lit { version :: Int, v :: Int }
            | Operator { version :: Int, typeID :: Int, ps :: [Packet] }
            deriving Show

packet :: Parser Packet
packet = do
  version <- fmap toDec $ count 3 binDigit
  typeID <- fmap toDec $ count 3 binDigit
  if typeID == 4 then do
    v <- toDec <$> value
    return $ Lit version v
  else do
    lengthTypeID <- binDigit
    if lengthTypeID == '0' then do
      subPacketLength <- fmap toDec $ count 15 binDigit
      startPos <- sourceColumn <$> getPosition
      subPackets <- manyTill packet $ do
        endPos <- sourceColumn <$> getPosition
        guard (endPos - startPos == subPacketLength)
      return $ Operator version typeID subPackets
    else do
      numSubPackets <- fmap toDec $ count 11 binDigit
      subPackets <- count numSubPackets packet
      return $ Operator version typeID subPackets

value :: Parser String
value = do
  prefix <- binDigit
  d <- count 4 binDigit
  if (prefix == '1') then do
    d' <- value
    return $ d ++ d'
  else return d

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

binDigit :: Parser Char
binDigit = char '0' <|> char '1'

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