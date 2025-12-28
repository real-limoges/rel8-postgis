module Rel8.PostGIS.Codec (
    decodePoint,
    decodeLineString,
    decodePolygon,
    encodePoint,
    encodeLineString,
    encodePolygon,
) where

import Control.Monad (replicateM, void, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Serialize (Get, Put, getFloat64be, getFloat64le, getWord32be, getWord32le, getWord8, putFloat64le, putWord32le, putWord8, runGet, runPut, skip)
import Rel8.PostGIS.Types (Geo (..), LineString (..), Point (..), Polygon (..))

-- Public API

decodePoint :: ByteString -> Point
decodePoint bs = case runGet (parseWKB 1 parsePointBody) bs of
    Left err -> error $ "WKB Point Parse Error: " ++ err
    Right p -> p

encodePoint :: Point -> ByteString
encodePoint (Point x y) = runPut $ do
    putEndian
    putWord32le 1 -- Type: Point
    putFloat64le x
    putFloat64le y

decodeLineString :: ByteString -> LineString
decodeLineString bs = case runGet (parseWKB 2 parseLineStringBody) bs of
    Left err -> error $ "WKB LineString Parse Error: " ++ err
    Right l -> l

encodeLineString :: LineString -> ByteString
encodeLineString (LineString points) = runPut $ do
    putEndian
    putWord32le 2 -- Type: LineString
    putWord32le (fromIntegral $ length points)
    traverse_ putPointRaw points

decodePolygon :: ByteString -> Polygon
decodePolygon bs = case runGet (parseWKB 3 parsePolygonBody) bs of
    Left err -> error $ "WKB Polygon Parse Error: " ++ err
    Right p -> p

encodePolygon :: Polygon -> ByteString
encodePolygon (Polygon rings) = runPut $ do
    putEndian
    putWord32le 3 -- Type: Polygon
    putWord32le (fromIntegral $ length rings)
    traverse_ putRing rings
  where
    putRing (LineString points) = do
        putWord32le (fromIntegral $ length points)
        traverse_ putPointRaw points

-- Helpers

putEndian :: Put
putEndian = putWord8 1 -- Little Endian

putPointRaw :: Point -> Put
putPointRaw (Point x y) = do
    putFloat64le x
    putFloat64le y

-- Parsers
parseWKB :: Int -> (Get Double -> Get Int -> Get a) -> Get a
parseWKB expectedType parseBody = do
    endian <- getWord8
    let isLittle = endian /= 0
    let getW32 = if isLittle then getWord32le else getWord32be
    let getDbl = if isLittle then getFloat64le else getFloat64be

    -- Helper to read count (Word32) as Int
    let getCount = fromIntegral <$> getW32

    -- Type header
    typeCode <- getW32
    let hasSRID = typeCode .&. 0x20000000 /= 0
    let rawType = fromIntegral (typeCode .&. 0x1FFFFFFF)

    Control.Monad.when (rawType /= expectedType) $ fail $ "Mismatch on Geometry Type. Expected: " ++ show expectedType ++ ", Found: " ++ show rawType

    Control.Monad.when hasSRID $ skip 4

    parseBody getDbl getCount

-- | Body Parsers
parsePointBody :: Get Double -> Get Int -> Get Point
parsePointBody getDbl _ = do
    x <- getDbl
    Point x <$> getDbl

parseLineStringBody :: Get Double -> Get Int -> Get LineString
parseLineStringBody getDbl getCount = do
    numPoints <- getCount
    points <- replicateM numPoints (parsePointBody getDbl getCount)
    return (LineString points)

parsePolygonBody :: Get Double -> Get Int -> Get Polygon
parsePolygonBody getDbl getCount = do
    numRings <- getCount
    rings <- replicateM numRings (parseLineStringBody getDbl getCount)
    return (Polygon rings)
