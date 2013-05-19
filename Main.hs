module Main where

import Prelude hiding (flip)
import System.Exit
import System.Random
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.SDL
import Data.IORef
import Data.Maybe
import Data.Map hiding (foldr, map)

import Environment
import GameMonad
import Config
import Agent
import Bullet
import Ship
import Assets

main :: IO ()
main = withInit [InitEverything] $ do
  screen <- setVideoMode screenWidth screenHeight 32 [HWSurface]
  setCaption "Something something" []
  black <- mapRGB (surfaceGetPixelFormat screen) 0 0 0
  startTime <- getTicks
  let mainLoop = do
        let whileEvents = do
            event <- liftIO $ pollEvent
            case event of
              Quit -> return True
              NoEvent -> return False
              KeyDown keySym -> do
                updateInputs True keySym
                whileEvents
              KeyUp keySym -> do
                updateInputs False keySym
                whileEvents
              _ -> whileEvents
        quit <- whileEvents
        when quit $ liftIO $ exitSuccess
        shipMovement
        bulletPattern
        runRecurring
        runDelayed
        collisionHandling
        bullets' <- use $ environment . bullets
        mShip <- use $ environment . ship
        liftIO $ fillRect screen Nothing black
        case mShip of
          Just ship' -> do
            let 
              shipFrame = head $ ship' ^. animation
              shipX = (round $ snd $ ship' ^. position)
              shipY = (round $ fst $ ship' ^. position)
              screenRect = Rect {
                rectX = shipX - (surfaceGetWidth shipFrame `div` 2),
                rectY = shipY - (surfaceGetHeight shipFrame `div` 2), 
                rectW = surfaceGetWidth shipFrame,
                rectH = surfaceGetHeight shipFrame
                }
            liftIO $ do
              blitSurface shipFrame Nothing screen (Just screenRect)
              return ()
          Nothing -> return ()
        liftIO $ do
          forM_ bullets' $ \bullet -> do
            let bulletFrame = head $ bullet ^. animation
                bulletX = (round $ snd $ bullet ^. position)
                bulletY = (round $ fst $ bullet ^. position)
                bulletRect = Rect {
                  rectX = bulletX - (surfaceGetWidth bulletFrame `div` 2),
                  rectY = bulletY - (surfaceGetHeight bulletFrame `div` 2),
                  rectW = surfaceGetWidth bulletFrame,
                  rectH = surfaceGetHeight bulletFrame
                  }
            blitSurface bulletFrame Nothing screen (Just bulletRect)
        renderLifeCounter screen
        remaining <- use $ lifeRemaining
        if remaining >= 0 
         then do
          liftIO $ do
            flip screen
            delay frameLength    
          mainLoop
         else return ()
  runGame $ do
    mainLoop
    gameOverAnims <- use $ assets . gameOverAnimations
    let gameOverSurf = head (gameOverAnims ! "neutral")
    liftIO $ do
      blitSurface gameOverSurf Nothing screen Nothing
      flip screen
      delay 5000

renderLifeCounter :: Surface -> GameMonad ()
renderLifeCounter screen = do
  remaining <- use $ lifeRemaining
  liftIO $ putStrLn $ "life remaining: " ++ show remaining
  counterAnims <- use $ assets . counterAnimations
  let counterSurf = head $ counterAnims ! "neutral"
  forM_ [1..remaining] $ \i -> do
    let counterRect = Rect {
          rectX = 16 + (i-1) * surfaceGetWidth counterSurf,
          rectY = 16,
          rectW = surfaceGetWidth counterSurf,
          rectH = surfaceGetHeight counterSurf
          }
    liftIO $ blitSurface counterSurf Nothing screen (Just counterRect)

updateInputs :: Bool -> Keysym -> GameMonad ()
updateInputs newValue changedKey =
  case symKey changedKey of
    SDLK_UP -> inputs . up .= newValue
    SDLK_DOWN -> inputs . down .= newValue
    SDLK_LEFT -> inputs . left .= newValue
    SDLK_RIGHT -> inputs . right .= newValue
    _ -> return ()

maxSpeed :: Float
maxSpeed = 7

shipMovement :: GameMonad ()
shipMovement = do
  mShip <- use $ environment . ship
  case mShip of
    Just ship' -> do
      up <- use $ inputs . up
      down <- use $ inputs . down
      left <- use $ inputs . left
      right <- use $ inputs . right
      if up || down || left || right
        then environment . ship .= Just ((speed .~ maxSpeed) ship')
        else environment . ship .= Just ((speed .~ 0) ship')
      let newAngle = 
            case (up,down,left,right) of
              (True,False,False,False) -> pi/2
              (False,True,False,False) -> -pi/2
              (False,False,True,False) -> pi
              (False,False,False,True) -> 0
              (True,False,True,False) -> 3 * pi / 4
              (True,False,False,True) -> pi / 4
              (False,True,True,False) -> -3 * pi / 4
              (False,True,False,True) -> -pi / 4
              _ -> pi/2
      ship'' <- fmap fromJust $ use $ environment . ship
      let angledShip = (angle .~ newAngle) ship''
          updatedShip = updateAgent angledShip
          (x,y) = updatedShip ^. position
          wrappedShip = position .~ (max 0 (min (realToFrac screenHeight) x), max 0 (min (realToFrac screenWidth) y)) $ updatedShip
      environment . ship .= Just wrappedShip
    Nothing -> return ()

bulletPattern :: GameMonad ()
bulletPattern = do
  every 7000 $ do
    randAngle <- fmap (* (2 * pi)) $ liftIO randomIO
    assets' <- use assets
    let
      center = (realToFrac screenHeight / 2, realToFrac screenWidth / 2)
      bulletSpeed = 10
    environment . bullets %= (newBullet assets' "neutral" center randAngle bulletSpeed:)
  bullets' <- use $ environment . bullets
  newBullets <- liftIO $ newIORef []
  forM bullets' $ \bullet -> do
    let 
      newBullet = updateAgent bullet
      (x,y) = newBullet ^. position
      bulletFrame = head $ newBullet ^. animation
      w = surfaceGetWidth bulletFrame
      h = surfaceGetHeight bulletFrame
    when (round x + w >= 0 && round x - w <= screenHeight && round y + h >= 0 && round y - h <= screenWidth) $ do
      liftIO $ modifyIORef newBullets (newBullet:)
  newBullets' <- liftIO $ readIORef newBullets
  environment . bullets .= newBullets'

hitboxHalfSize :: (Float,Float)
hitboxHalfSize = (4,4)

intersect1D :: Float -> Float -> Float -> Float -> Bool
intersect1D x1 hs1 x2 hs2 =
  not $ x1 + hs1 < x2 - hs2 || x2 + hs2 < x1 - hs2

intersectAABB :: (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
intersectAABB (x1,y1) (hw1,hh1) (x2,y2) (hw2,hh2) =
  intersect1D x1 hw1 x2 hw2 && intersect1D y1 hh1 y2 hh2 

collisionHandling :: GameMonad ()
collisionHandling = do
  mShip <- use $ environment . ship
  case mShip of
    Just ship' -> do
        bullets' <- use $ environment . bullets
        let collided = foldr (||) False $ map (\bullet -> intersectAABB (bullet ^. position) hitboxHalfSize (ship' ^. position) hitboxHalfSize) bullets'
        when collided $ do
          lifeRemaining %= (\l -> l - 1)
          environment . ship .= Nothing
          delayedAction 1000 $ do
            assets' <- use assets
            environment . ship .= (Just $ initShip assets' defaultShipPosition)          
            environment . bullets .= []
    Nothing -> return ()
