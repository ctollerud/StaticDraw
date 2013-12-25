-- defines our composable "sketch" object, which will be at the core of this graphics library.
-- Currently it's just set up to generate a simple red bitmap, as a simple proof of concept.

module Sketch where
import Data.Char
import Data.List
import Data.Word
import Codec.BMP
import Data.Monoid
import Data.ByteString( ByteString, pack )

main :: IO()
main = ((writeBMP "output.bmp" ) . (packRGBA32ToBMP 800 800)) testBitmapData

-----------------------------------------------------------------------------------------
--print out a stack of of alternating red-blue rows.
-- used to confirm that the the list works right to left
testBitmapData :: ByteString
testBitmapData = (pack . concat . concat . (Data.List.take (400)) . repeat) redblueStack
			where redblueStack = ((take 800) . repeat) [127,0,0,0] ++
					     ((take 800) . repeat) [0,0,127,0]
					     
-----------------------------------------------------------------------------------------

type Color = (Word8, Word8, Word8)
r (r,g,b) = r
g (r,g,b) = g
b (r,g,b) = b

--A double-precision coordinate, used when working in vector coordinates
type Coord = (Double,Double)

--used to hold the state of a pixel as it is represented in world coordinates.
--will most likely be kept internal to this module.
type PState = (Color, Coord)
-----------------------------------------------------------------------------------------
data Sketch = SketchCons (PState -> PState)

instance Monoid Sketch where
    --mempty :: Sketch
    mempty = SketchCons (\s->s)
    --mappend::Sketch->Sketch->Sketch
    mappend (SketchCons f1) (SketchCons f2) = SketchCons (f2 . f1)

-- a little syntactic sugar
(<+>)::Sketch->Sketch->Sketch
(<+>) = mappend
sketchFunc::Sketch->(PState->PState)
sketchFunc (SketchCons f) = f

world::Int->Int->(Int,Int)->Coord
world px py (x,y) = (zmap (0,fint (px-1)) (-1, 1) (fint x), zmap (0,fint (py-1)) (1,-1) (fint y))
	where fint = fromIntegral

renderSketch :: String->Int->Int->Sketch->IO ()
renderSketch path px py (SketchCons f) = ((writeBMP path) . (packRGBA32ToBMP px py) . pack . concatColors . (map evalPixel)) [(x,y) | y <- [0..(py-1)], x <- [0..(px-1)]]
        where evalPixel :: (Int,Int)->Color
              evalPixel p = ( evaluateColor . (world px py)) p
                        where evaluateColor :: Coord->Color
                              evaluateColor c = let (color, coord) = f ((0,0,0), c ) in
                                                color
              concatColors ::[Color]->[Word8]
              concatColors = concat . (map colorToList )
                  where colorToList ::Color->[Word8]
                        colorToList (r,g,b) = (r:g:b:[0])

--returns a sketch that only has an effect if the coordinate satisfies a specific condition
sketchIf::(PState->Bool)->Sketch->Sketch
sketchIf f (SketchCons g ) = (SketchCons (\ps->if f ps then g ps else sketchFunc mempty ps))
----------------------------------------------------------------------------------------
sketchIfLoc::(Coord->Bool)->Sketch->Sketch
sketchIfLoc p = sketchIf (\(a, b)->p b)

solidColor::Color->Sketch
solidColor c = modColor (\color->c)

modColor::(Color->Color)->Sketch
modColor f = (SketchCons modColor')
             where modColor'::(Color,Coord)->(Color,Coord)
                   modColor' (col, coord) = (f col, coord)

--render a little test circle to get a sense of the progress.
testCircle::IO ()
testCircle = renderSketch "output.bmp" 800 800 (solidColor (255, 0, 0) <+> (sketchFig (circle (0,0) 1 ) (solidColor (127,127,127))))

-----------------------------------------------------------------------------------------
--Implementation of "figure", an anti-aliasing-based data type used in rendering/modulating sketches
-- it is based on a method used in determining whether a shape is inside or outside a data structure.
-- a negative return value means it's inside the shape
data Figure = FigCon (Coord->Double)

--creates a new sketch from a figure and a sketch, such that the sketch only gets drawn 
sketchFig :: Figure->Sketch->Sketch
sketchFig (FigCon f) s = sketchIfLoc (\coord-> (f coord) <= 0) s

circle::Coord->Double->Figure
circle coord rad = FigCon (\c->(distance c coord) - rad )
-----------------------------------------------------------------------------------------
--some math stuff that may move elsewhere eventually
zmap ::(Double,Double)->(Double,Double)->Double->Double
zmap (inmin,inmax) (outmin,outmax) =  (outmin +) . ((outmax-outmin) *) . ( / (inmax - inmin)) . (subtract inmin)

distance::Coord->Coord->Double
distance (x1, y1) (x2, y2) = sqrt (((x1-x2)**2) + ((y1-y2)**2) )

white::Color
white = (255,255,255)
