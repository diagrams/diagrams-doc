---
title: DeclarationRights
author: Alexis Praga
authorurl: https://www.projects.haskell.org/diagrams/
date: 2017-06-08
description: Kenneth Knowlton's "Universal Declaration of Human Rights"
tags: gradient, text, knowlton
width: 800
---


We recreate the "Universal Declaration of Human Rights" by Kenneth Knowlton (see
http://recodeproject.com/artwork/v1n2universal-declaration-of-human-rights).

> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> import Codec.Picture
> import Codec.Picture.Types
> import Data.Colour.CIE
> import Data.List.Split
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import Diagrams.Prelude
> import Diagrams.Backend.Rasterific.CmdLine

The idea is to split the Declaration into a set of lines and apply to each line
a gradient corresponding to the picture.

First, we load an image using JuicyPixel. More precisely, we extract a list of
colors. For convenience, we use a custom datatype containing the height, width
and the list of colours. To be consisten with Diagrams, colors are stored as
Colour.

> data ColourImage b = ColourImage { cColors :: [Colour b]
>                    , cHeight :: Int
>                    , cWidth :: Int
>                    }
  

Then we load the image using JuicyPixels, knowing the Image is in YcbCr8 format
(not quite portable...). Each pixel is converted to a Colour.

> getColors :: (Ord b, Floating b) => FilePath -> IO (Maybe (ColourImage b))
> getColors fp = do
>     image <- readImage fp
>     case image of
>         Left _ -> return Nothing
>         Right image' -> return (getColors' image')
> 
> getColors' :: (Ord b, Floating b) => DynamicImage -> Maybe (ColourImage b)
> getColors' (ImageYCbCr8 image@(Image w h _)) =
>     Just (ColourImage colors h w)
>       where colors =  pixelFold (\acc i j pix -> (ycbcrToRGB pix) : acc) [] image
> getColors' _ = Nothing
> 
> ycbcrToRGB :: (Ord b, Floating b) => PixelYCbCr8 -> Colour b
> ycbcrToRGB pix = sRGB24 r g b
>   where (PixelRGB8 r g b ) = convertPixel pix :: PixelRGB8

Now we have the colors, we need the text. Knowing the width and height of the
picture, we generate a set of lines with constant width from the Declaration :

>   -- We want as many lines as there are pixels lines
> content :: Int -> Int -> IO [T.Text]
> content w h = do 
>   text1 <- TIO.readFile "Universal Declaration of Human Rights.txt"
>   -- At most h repeat as we cannot have "cycle" for Text
>   return $ take h $ T.chunksOf w $ T.replicate h text1

Next, we create the gradient. All colors will be split into constant-width
"lines" of color. For each line, we create a gradient.

> stops :: Fractional d => [Colour Double] -> d -> [GradientStop d]
> stops colors w = mkStops stops'
>   where 
>     n = length colors-1
>     stops' = map createStop [0..n]
>     dx = 1 / fromIntegral n
>     createStop i = (colors !! i, fromIntegral i * dx, 1)
> 
> gradient colors w = 
>   mkLinearGradient (stops colors w) ((-frac*w) ^& 0) ((frac*w) ^& 0) GradPad
>     where frac = 0.7
>     -- The fraction show only parts of the image here

We can now put create the line from the text and apply the gradient 

> createText :: T.Text -> Diagram B
> createText s = text (T.unpack s) # fontSize (local 8)
> 
> createLine :: Int -> Int -> ([Colour Double], T.Text) ->  Diagram B
> createLine w h (colors, s) = 
>   (createText s # fillTexture (gradient colors w')) `atop` rect w' h' # fc black
>     where (w', h') = (fromIntegral w, fromIntegral h)
> 

The final function will load the list of colors and create lines according to
the procedure above. As some parts of the picture are pitch black, we still want
sto see the text so we define a minimum luminance :

> minLuminance :: (Ord b, Floating b) => Colour b -> Colour b
> minLuminance c 
>   | (luminance c) < 0.032 = sRGB24 50 50 50
>   | otherwise = c
> 
> 
> text1 :: IO (Diagram B )
> text1 = do
>   (Just img) <- getColors "girl_gaze_contrast1_small2.jpg"
>   let colors' = map minLuminance $ cColors img
>   let (h, w) = (cHeight img, cWidth img)
>   let heightRect = 15
>   let width = heightRect*h
>   let colors = chunksOf w colors'
>   lines <- content 280 h
>   -- We need a rectangle for each line
>   let all = map (createLine width heightRect) $ zip colors lines  :: [Diagram B]
>   return $ cat (r2 (0, 1)) all
> 
> 
> main = mainWith text1
