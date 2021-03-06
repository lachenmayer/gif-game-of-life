module GifGameOfLife (gifGameOfLife) where

import Control.Monad (when)
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Gif
import qualified Data.ByteString as B
import qualified Data.Vector as V
import System.Exit (exitFailure)

type BWImage = Image Pixel8
makeBW :: Image PixelRGB8 -> BWImage
makeBW =
  pixelMap (\(PixelRGB8 r g b) -> minimum [r, g, b])

type Grid = V.Vector (V.Vector Bool)
width = V.length . V.head
height = V.length
at grid x y =
  (V.!) ((V.!) grid y') x'
  where
    y' = (y + h) `mod` h
    x' = (x + w) `mod` w
    w = width grid
    h = height grid

gifGameOfLife :: FilePath -> FilePath -> IO ()
gifGameOfLife inPath outPath = do
  inFile <- B.readFile inPath
  case (decodeGif inFile) of
    Right (ImageRGB8 image) -> step image
    Right (ImageRGBA8 image) -> step . dropAlphaLayer $ image
    Right _ -> error "gif has weird format."
    Left message -> error $ "file provided could not be read as a gif file: " ++ message
  where
    step image = do
      writeGifImage outPath . gifGameOfLifeStep . makeBW $ image
    error message = do
      putStrLn message
      exitFailure

gifGameOfLifeStep :: BWImage -> BWImage
gifGameOfLifeStep image =
  toImage . gameOfLifeStep . toGrid $ image

toGrid :: BWImage -> Grid
toGrid image =
  V.generate (imageHeight image) row
  where
    row y = V.generate (imageWidth image) (\x -> alive $ pixelAt image x y)
    alive pixel = pixel == 0

toImage :: Grid -> BWImage
toImage grid =
  generateImage pixelValue w h
  where
    w = width grid
    h = height grid
    pixelValue x y = if at grid x y then 0 else 255

gameOfLifeStep :: Grid -> Grid
gameOfLifeStep grid =
  forEachCell cellRule grid
  where
    forEachCell f =
      V.imap (\y row -> (V.imap (\x alive -> f alive x y) row))
    cellRule alive x y =
      let neighborCount' = neighborCount x y in
        if alive
          then neighborCount' == 2 || neighborCount' == 3
          else neighborCount' == 3
    neighborCount x y =
      length (aliveNeighbors x y)
    aliveNeighbors x y =
      filter id (neighbors x y)
    neighbors x y =
      [ x - 1 & y - 1 , x & y - 1 , x + 1 & y - 1
      , x - 1 &   y               , x + 1 &   y
      , x - 1 & y + 1 , x & y + 1 , x + 1 & y + 1
      ]
    infix 0 &
    (&) = at grid
