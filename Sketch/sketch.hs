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

getX::Coord->Double
getX (x,_) = x

getY (_,y) = y
--used to hold the state of a pixel as it is represented in world coordinates.
--will most likely be kept internal to this module.
type PState = (Color, Coord, Double)
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
                              evaluateColor c = let (color, coord, pw) = f ((0,0,0), c, pwidth ) in color
                                                where pwidth = 2*sqrt(2)/(fromIntegral (px) )
              concatColors ::[Color]->[Word8]
              concatColors = concat . (map colorToList )
                  where colorToList ::Color->[Word8]
                        colorToList (r,g,b) = (r:g:b:[0])

--returns a sketch that only has an effect if the coordinate satisfies a specific condition
sketchIf::(PState->Bool)->Sketch->Sketch
sketchIf f (SketchCons g ) = (SketchCons (\ps->if f ps then g ps else sketchFunc mempty ps))
----------------------------------------------------------------------------------------
sketchIfLoc::(Coord->Bool)->Sketch->Sketch
sketchIfLoc p = sketchIf (\(a, b, c)->p b)

solidColor::Color->Sketch
solidColor c = modColor (\color->c)

modColor::(Color->Color)->Sketch
modColor f = (SketchCons modColor')
             where modColor'::(Color,Coord,Double)->(Color,Coord,Double)
                   modColor' (col, coord, d) = (f col, coord, d)

--render a little test circle to get a sense of the progress.
testCircle::IO ()
testCircle = renderSketch "output2.bmp" 800 800 (solidColor (255, 0, 0) <+> (sketchFig2 (circle (-1,-1) 1 ) (solidColor (0,0,127))))

-----------------------------------------------------------------------------------------
--Implementation of "figure", an anti-aliasing-based data type used in rendering/modulating sketches
-- it is based on a method used in determining whether a shape is inside or outside.
-- a negative return value means it's inside the shape
data Figure = FigCon (Coord->Double)

--creates a new sketch from a figure and a sketch, such that the sketch only gets drawn 
sketchFig :: Figure->Sketch->Sketch
sketchFig (FigCon f) s = sketchIfLoc (\coord-> (f coord) <= 0) s

--anti-aliased version
--sketchFig2 :: Figure->Sketch->Sketch
--        where drawAntiAliased::PState->PState
--	      drawAntiAliased ((r,g,b), c) = (aliasedColor,c)
--             where aliasedColor::Color->Color
--             aliasedColor c = let testVal = f (x,y) in
--                               if testVal < -1 then sketchFun


--anti-aliased version
sketchFig2 :: Figure->Sketch->Sketch
sketchFig2 (FigCon f) s = SketchCons drawAntiAliased
        where drawAntiAliased::PState->PState
              drawAntiAliased ((r,g,b), c, w) = let dist = f c in
                  if dist < (-w) then sketchFunc s ((r,g,b), c, w) else
                      if dist > w then sketchFunc mempty ((r,g,b), c, w) else drawAntiAliased'
                          where drawAntiAliased'::PState
                                drawAntiAliased' = (colormix, c, w)
                                    where colormix::Color
                                          colormix = blend (r,g,b) color2  (((/ 16) . fromIntegral . foldr tallyInside 0) [(x,y)|x<-xRange, y<-yRange])
                                              where xRange = ((take 4) . (iterate (\x->x+(w/4)))) ((getX c) - (3*w/8)) 
                                                    yRange = ((take 4) . (iterate (\y->y+(w/4)))) ((getY c) - (3*w/8))
                                                    tallyInside::Coord->Int->Int
                                                    tallyInside c i = if f c < 0 then i + 1 else i
                                                    color2 = let (cbunk,_,_) = sketchFunc s ((r,g,b), c, w) in cbunk                                                    

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

blend::Color->Color->Double->Color
blend (r1,g1,b1) (r2,g2,b2) r = ( mix r1 r2 r, mix g1 g2 r, mix b1 b2 r )
	where mix ::Word8->Word8->Double->Word8
              mix c1 c2 = round . (zmap (0,1) (fromIntegral c1, fromIntegral c2))
