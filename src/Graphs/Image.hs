 {-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphs.Image where

{- Imagetools, like drawing dots with text -}

import Utils.Utils
import Text.Blaze.Svg11 ((!), stringValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Text.Blaze.Svg.Renderer.String (renderSvg)-- DO NOT USE THE PRETTY RENDERER; text won't be centered then
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Monad




data ColorScheme	= CS {	_fg		:: String,
				_bg		:: String,
				_lineColor	:: String,
				_fontSize	:: Int,
				_lineThickness	:: Int,
				_dotSize	:: Int
				}
terminalCS	= CS "#00ff00" "#000000" "#00ff00" 20 1 4
whiteCS		= CS "#000000" "#ffffff" "#000000" 20 1 4

makeLenses ''ColorScheme

type X	= Int
type Y 	= Int

type W	= Int
type H	= Int



intValue	:: Int -> S.AttributeValue
intValue i	= stringValue $ show i



packageSVG	:: (Int, Int) -> (Int, Int) -> S.Svg -> String
packageSVG (pxW, pxH) (viewBoxW, viewBoxH) svg
	= let	svg'	= S.docTypeSvg	! A.version "1.1"
					! A.width (intValue pxW)
					! A.height (intValue pxH)
					! A.viewbox (stringValue $ "0 0 "++show viewBoxW++" "++show viewBoxH)
					$ svg
		in
		renderSvg svg'



drawLineBetween	:: ColorScheme -> Bool -> Map Text (X, Y) -> Text -> Text -> S.Svg
drawLineBetween cs dashed coors start end
	= do	let (start', end')	= lookupPoints coors (start, end)
		drawLine cs dashed start' end'

lookupPoints	:: Map Text (X, Y) -> (Text, Text) -> ((X, Y), (X, Y))
lookupPoints coors (start, end)
	= let	find x	= findWithDefault (error $ "Images: I do not know what "++Text.unpack x++" is") x coors 
		start'	= find start
		end'	= find end
		in (start', end')



linesIntersect	:: ((X, Y), (X, Y)) -> ((X, Y), (X, Y)) -> Bool
linesIntersect (e@(ex, ey), f@(fx, fy)) (p@(px, py), q@(qx, qy))
 | e == p || e == q || f == p || f == q
		= False
 | otherwise
		= let	side0	= (fx - ex)*(py - fy) - (fy - ey)*(px - fx)
			side1	= (fx - ex)*(qy - fy) - (fy - ey)*(qx - fx)
			in
			signum side0 /= signum side1
			
distanceBetween	:: (X, Y) -> (X, Y) -> Float
distanceBetween	(x0, y0) (x1, y1)
	= let	sqr a	= a * a
		in
		sqrt $ fromIntegral $ sqr (x1 - x0) + sqr (y1 - y0)

distanceBetween':: Map Text (X, Y) -> Text -> Text -> Float
distanceBetween' dict p0 p1
		= let	coor0	= dict M.! p0
			coor1	= dict M.! p1
			in
			distanceBetween coor0 coor1 


drawLine	:: ColorScheme -> Bool -> (X, Y) -> (X, Y) -> S.Svg
drawLine cs dashed (x0,y0) (x1, y1)
	= let 	lt	= get lineThickness cs
		pth = S.path ! A.d (stringValue $ ["M", show x0, show y0, ","
						, show x1, show y1] & unwords) 
			! A.stroke (stringValue $ get lineColor cs)
			! A.strokeWidth (intValue $ get lineThickness cs)
			! A.strokeLinecap "round"
	   in if dashed then pth ! A.strokeDasharray (stringValue $ [show lt, ",", show $ 5 * lt] & unwords) else pth

{-
Height: 
fs * 3
-}
annotatedDot	:: ColorScheme -> Bool -> (Text, (X, Y)) -> S.Svg
annotatedDot cs under (nm, (x, y))
	= do	let fs	= get fontSize cs
		let nml	= Text.length nm
		let rW	= round $ fromIntegral (fs * nml) * 0.65
		let offsetY	= if under then fs `div` 2 else negate 2 * fs
		unless (Text.null nm) $ do 
			[(-8, "1.0"), (-6, "0.9"), (-4, "0.75"), (-3, "0.5"), (-2,"0.25"), (0, "0.1")]
				|+> uncurry (drawRect cs (x - rW `div` 2) (y + offsetY) rW (fs + fs `div` 2) )
			S.text_ ! A.x (intValue x)
				! A.y (intValue $ y + fs + offsetY) 
				! A.fontSize (intValue fs)
				! A.textAnchor (stringValue "middle")
				! A.fontFamily "mono"
				! A.fill (stringValue $ get fg cs)
				$ S.text  nm
		S.circle ! A.r (intValue $ get dotSize cs) ! A.cx (intValue x) ! A.cy (intValue y) ! A.fill (stringValue $ get fg cs)


drawRect	:: ColorScheme -> X -> Y -> W -> H -> Int ->  String -> S.Svg
drawRect cs x y w h border opacity
	= S.rect	! A.x 		(intValue $ x-border)
			! A.y 		(intValue $ y-border)
			! A.width 	(intValue $ w+2*border)
			! A.height 	(intValue $ h+2*border)
			! A.fill 	(stringValue $ get bg cs)
			! A.fillOpacity	(stringValue opacity)


