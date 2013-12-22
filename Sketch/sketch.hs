-- defines our composable "sketch" object, which will be at the core of this graphics library.
-- Currently it's just set up to generate a simple red bitmap, as a simple proof of concept.

module Sketch where
import Data.Char
import Data.List
import Data.Word
import Codec.BMP
import Data.ByteString( ByteString, pack )

main :: IO()
main = ((writeBMP "output.bmp" ) . (packRGBA32ToBMP 800 800)) testBitmapData


--print out ByteString for an 800 by 800 red bitmap
testBitmapData :: ByteString
testBitmapData = (pack . concat . (Data.List.take (800*800)) . repeat) [255,0,0,0]

