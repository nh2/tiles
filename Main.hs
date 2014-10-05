{-# LANGUAGE NamedFieldPuns, ViewPatterns, LambdaCase, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Exception
import qualified Data.ByteString as BS
import           Data.List
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import           Data.SafeCopy
import           Data.Serialize.Get (runGet)
import           Data.Serialize.Put (runPut)
import           Graphics.Gloss.Interface.IO.Game hiding (Vector)
import           System.IO


_N :: Int
_N = 20

tileSize :: Int
tileSize = 20

tileDiff :: Int
tileDiff = tileSize + 1

_UNDO_STEPS :: Int
_UNDO_STEPS = 20


initWinSize :: (Int, Int)
initWinSize = (_N * tileDiff, _N * tileDiff)


data World = World
  { tiles :: Vector (Vector Tile)
  , winSize :: (Int, Int)
  , hover :: (Int, Int)
  , tileColor :: Color
  , tileType :: TileType
  , undoHist :: [World]
  }


emptyTileColor :: Color
emptyTileColor = makeColor 0.1 0.1 0.1 1


initWorld :: World
initWorld = World
  { tiles = V.replicate _N (V.replicate _N (Small, emptyTileColor))
  , winSize = initWinSize
  , hover = (0, 0)
  , tileColor = makeColor 1 0 0 1
  , tileType = Small
  , undoHist = []
  }


data TileType = Small | Big (Int, Int)
  deriving (Eq, Ord, Show)

type Tile = (TileType, Color)


toList2 :: Vector (Vector a) -> [[a]]
toList2 = V.toList . V.map V.toList


asciiWorld :: World -> String
asciiWorld world =
  unlines [ unwords [ case t of Small -> "."
                                Big (a, b) -> show (a*3+b + 4)
                    | (t, _) <- row ]
          | row <- reverse . transpose . toList2 $ tiles world ]


render :: World -> Picture
render World{ tiles, hover, tileType, tileColor, winSize = (wx, wy) } =
  -- Translate from Gloss's "0,0 is center" to "0,0 is bottom left corner"
  Translate (fi (tileDiff - wx) / 2) (fi (tileDiff - wy) / 2)
    . Pictures
    . (++ [renderTile hover (tileType, tileColor)])
    . concat
    . toList2
    $ V.imap (\i -> V.imap (\j -> renderTile (i, j))) tiles
  where
    fi = fromIntegral
    tileSize3 = tileDiff * 2 + tileSize

    renderTile :: (Int, Int) -> Tile -> Picture
    renderTile (i, j) tt = case tt of
      (Small, col) -> Translate (fi $ i * tileDiff) (fi $ j * tileDiff)
                        . Color col
                        $ Polygon (rectanglePath (fi tileSize) (fi tileSize))
      (Big (0, 0), col) -> Translate (fi $ i * tileDiff) (fi $ j * tileDiff)
                              . Color col
                              $ Polygon (rectanglePath (fi tileSize3) (fi tileSize3))
      (Big _, _) -> Blank



data Save = Save
  { saveWorld :: World
  }

deriveSafeCopy 1 'base ''Color
deriveSafeCopy 1 'base ''TileType
deriveSafeCopy 1 'base ''World
deriveSafeCopy 1 'base ''Save


save :: World -> IO ()
save world = saveTo world "save.safecopy"


saveTo :: World -> FilePath -> IO ()
saveTo world path = do
  putStrLn $ "Saving to " ++ path
  BS.writeFile path . runPut . safePut $ Save
    { saveWorld = world
    }


load :: IO (Either String Save)
load = loadFrom "save.safecopy"


loadFrom :: FilePath -> IO (Either String Save)
loadFrom path = do
  putStrLn $ "Loading " ++ path
  try (BS.readFile path) >>= \case
    Left (e :: IOError) -> return . Left $ show e
    Right bs -> case runGet safeGet bs of
      Left err -> return . Left $ "Failed loading " ++ path ++ ": " ++ err
      Right s -> return $ Right s


topleft :: (RealFrac a) => World -> (a, a) -> (Int, Int)
topleft World{ winSize = (wx, wy) } (x, y) = (floor x + (wx `div` 2), floor y + (wy `div` 2))


event :: Event -> World -> IO World
event ev world = case ev of
  EventKey (Char 's') Down _ _ -> save world >> return world
  EventKey (Char 'l') Down _ _ -> load >>= \case
                                    Left err -> hPutStrLn stderr err >> return world
                                    Right Save{ saveWorld = w } -> return w
  _ -> return $ case ev of -- pure event handlers
    EventMotion (topleft world -> (x, y)) -> world{ hover = (tileOf x, tileOf y) }
    EventResize (x, y) -> world{ winSize = (x, y) }
    EventKey (Char 'r') Down _ _ -> world{ tileColor = makeColor 0.8627 0.1843 0.17647 1 }
    EventKey (Char 'g') Down _ _ -> world{ tileColor = makeColor 0.427 0.427 0.427 1 }
    EventKey (Char 'w') Down _ _ -> world{ tileColor = makeColor 0.9450 0.9333 0.796 1 }
    EventKey (Char '\b') Down _ _ -> world{ tileColor = emptyTileColor }
    EventKey (Char '\SUB') Down (Modifiers -- Not sure why gloss gives me \SUB and not 'z'
                                  { ctrl  = Down
                                  , shift = Up
                                  , alt   = Up
                                  }) _
                                 -> (head $ undoHist world ++ [world])
                                      { -- Reattach history of last world to the current.
                                        undoHist = drop 1 (undoHist world)
                                      }
    EventKey (Char '1') Down _ _ -> world{ tileType = Small }
    EventKey (Char '3') Down _ _ -> world{ tileType = Big (0, 0) }
    EventKey (MouseButton LeftButton) Down _ (topleft world -> (x, y)) ->
      let i = tileOf x
          j = tileOf y
          ts = tiles world
          new = case tileType world of
            Small -> [(i, j, (Small, tileColor world))]
            Big _
              | 1 <= i          && 1 <= j &&
                     i < _N - 1 &&      j < _N - 1 ->
                -- ^ bounds check: must not be an outermost 1-tile
                  [ (i+di, j+dj, (Big (di, dj), tileColor world))
                  | di <- [-1,0,1]
                  , dj <- [-1,0,1]
                  ]
              | otherwise -> []

          split = [ (i', j', (Small, col))
                  | (bi, bj, _) <- new
                  , (Big (bdi, bdj), col) <- [ts ! bi ! bj]
                  , di <- [-1,0,1]
                  , dj <- [-1,0,1]
                  , let i' = bi - bdi + di
                  , let j' = bj - bdj + dj
                  ]
      in world{ tiles = ts /// split /// new
              , undoHist = take _UNDO_STEPS $ world{ undoHist = [] } : undoHist world }
                -- ^ have to set undoHist = [] here to avoid exponential explosion (without sharing)
    _ -> world
  where
    clamp n = max 0 . min (_N - 1) $ n
    tileOf pix = clamp $ pix `quot` tileDiff


(///) :: Vector (Vector a) -> [(Int, Int, a)] -> Vector (Vector a)
(///) = foldl' (\v (i, j, a) -> v // [(i, v ! i // [(j, a)])])


step :: Float -> World -> World
step _time _world = error "shouldn't be called since we set the step time to 0 in `play`"


main :: IO ()
main =
  playIO
    (InWindow "Tiles" initWinSize (0, 0))
    (makeColor 0 0 0 1)
    0
    initWorld
    (return . render)
    event
    (\time world -> return $ step time world)
