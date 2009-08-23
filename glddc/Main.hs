{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Graphics.Rendering.OpenGL hiding (Color)
import qualified Graphics.Rendering.OpenGL as GL (Color3(Color3))
import Graphics.UI.GLUT hiding (Color)
import Data.IORef
import Control.Monad.Reader
import qualified Interpreter as I
import Color (mix, black, Color(Color))
import Debug.Trace

data AppState = AppState {
      appLastMousePosition :: Maybe Position,
      appRotating :: Bool,
      appRotation :: Vector3 GLfloat,
      appInterpreter :: I.Interpreter
    }

class AppFun a r where
    appFun :: IORef AppState -> (a -> r) -> IO ()

instance AppFun AppState (IO ()) where
    appFun appstateRef f = do appstate <- readIORef appstateRef
                              f appstate

instance AppFun AppState (IO AppState) where
    appFun appstateRef f = do appstate <- readIORef appstateRef
                              appstate' <- f appstate
                              writeIORef appstateRef appstate'


cube :: GLfloat -> GLfloat -> GLfloat
     -> GLfloat -> GLfloat -> GLfloat
     -> ReaderT (GLfloat -> GLfloat -> GLfloat -> Color3 GLfloat) IO ()
cube x y z w h d = do
  colorAt <- ask
  let v x y z = do color $ colorAt x y z
                   putStrLn $ "v " ++ (show (x,y,z))
                   vertex (Vertex3 x y z)
  lift $ renderPrimitive Quads $ do
                     -- Front
                     v x y z
                     v (x + w) y z
                     v (x + w) (y + h) z
                     v x (y + h) z
                     -- Back
                     v x y (z + d)
                     v x (y + h) (z + d)
                     v (x + w) (y + h) (z + d)
                     v (x + w) y (z + d)
                     -- Top
                     v x (y + h) z
                     v (x + w) (y + h) z
                     v (x + w) (y + h) (z + d)
                     v x (y + h) (z + d)
                     -- Bottom
                     v x y z
                     v x y (z + d)
                     v (x + w) y (z + d)
                     v (x + w) y z
                     -- Left
                     v x y z
                     v x (y + h) z
                     v x (y + h) (z + d)
                     v x y (z + d)
                     -- Right
                     v (x + w) y z
                     v (x + w) y (z + d)
                     v (x + w) (y + h) (z + d)
                     v (x + w) (y + h) z

colorToGL :: Color -> GL.Color3 GLfloat
colorToGL (Color r g b)
    = let r' = realToFrac r
          g' = realToFrac g
          b' = realToFrac b
      in GL.Color3 r' g' b'

display :: AppState -> IO ()
display appstate = do
  -- Setup
  clearDepth $= 1.0
  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back
  depthFunc $= Just Less
  shadeModel $= Smooth
  {-hint PerspectiveCorrection $= Nicest-}
  hint PolygonSmooth $= Nicest
  polygonSmooth $= Enabled

  matrixMode $= Projection
  loadIdentity
  Size w h <- get windowSize
  let aspect = fromIntegral w / fromIntegral h
  perspective 45 aspect 1 100

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 0 0 20) (Vertex3 0 0 0) (Vector3 0 1 0)


  -- Rotation
  let Vector3 rx ry rz = appRotation appstate
  rotate (180 * ry) $ Vector3 1.0 0 0
  rotate (180 * rx) $ Vector3 0 1.0 0
  rotate (180 * rz) $ Vector3 0 0 1.0

  -- Drawing
  let cCoords :: Int -> Vector3 GLfloat
      cCoords 1 = Vector3 (-4) 0 1
      cCoords 2 = Vector3 0 0 0
      cCoords 3 = Vector3 4 0 (-1)
      cLeds :: Int -> [(String, (GLfloat, GLfloat, GLfloat))]
      cLeds n = let ids = case n of
                            1 -> ['A'..'E']
                            2 -> ['F'..'J']
                            3 -> ['K'..'O']
                in [([fb, id], (x, y, z))
                    | (fb, z) <- [('F', 1), ('B', 0)],
                      (id, (x, y)) <- zip ids [(2.5, (-0.5)), (0.5, (-0.5)),
                                               (0.5, 1.0),
                                               (0.5, 2.5), (2.5, 2.5)]]
      cLightColors :: Int -> [((GLfloat, GLfloat, GLfloat)  -- ^light coordinates
                              ,Color                        -- ^light color
                              )]
      cLightColors n = map (\(ledid, coords) ->
                                let color = I.colorFor ledid $ appInterpreter appstate
                                in (coords, color)
                           ) $ cLeds n
      colorAt :: Int -> GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
      colorAt n x y z = let nearLights :: [(GLfloat, Color)]
                            nearLights = map (\((lx, ly, lz), color) ->
                                                  let distance = sqrt ((x - lx) ** 2 +
                                                                       (y - ly) ** 2 +
                                                                       (z - lz) ** 2)
                                                      nearness | distance > 0 = 1 / distance
                                                  in (nearness, color)
                                             ) $ cLightColors n
                            totalNearness = sum $ map fst nearLights
                        in ("totalNearness = " ++ (show totalNearness)) `trace` colorToGL $
                           foldl (\color' (nearness, color) ->
                                      ("mix " ++
                                       (show $ realToFrac $ nearness / totalNearness) ++
                                       " " ++
                                       (show color) ++
                                       " " ++
                                       (show color') ++
                                       " = " ++
                                       (show $ mix (realToFrac $ nearness / totalNearness) color color')) `trace`
                                         mix (realToFrac $ nearness / totalNearness) color color'
                                 ) black nearLights
      drawC :: Int -> IO ()
      drawC n = preservingMatrix $ do
                      translate $ cCoords n
                      runReaderT (do cube 2 2 0 1 1 1
                                     cube 1 2 0 1 1 1
                                     cube 0 2 0 1 1 1
                                     cube 0 1 0 1 1 1
                                     cube 0 0 0 1 1 1
                                     cube 0 (-1) 0 1 1 1
                                     cube 1 (-1) 0 1 1 1
                                     cube 2 (-1) 0 1 1 1) (colorAt n)
  forM [1..3] drawC

  -- Done
  flush
  swapBuffers

update :: AppState -> IO AppState
update appstate = do
  i <- I.update $ appInterpreter appstate
  postRedisplay Nothing
  return appstate { appInterpreter = i }

reshape :: Size -> IO ()
reshape s = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

mouseMotion :: Position -> AppState -> IO AppState
mouseMotion pos@(Position x y) appstate
    | appRotating appstate == True = do Size w h <- get windowSize
                                        let Just (Position x' y') = appLastMousePosition appstate
                                            Vector3 rx ry rz = appRotation appstate
                                            rx' = rx + fromIntegral (x' - x) / fromIntegral w
                                            ry' = ry + fromIntegral (y' - y) / fromIntegral h
                                            rotation' = Vector3 rx' ry' rz
                                        postRedisplay Nothing
                                        return appstate { appLastMousePosition = Just pos,
                                                          appRotation = rotation' }
    | otherwise = return appstate

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> AppState -> IO AppState
keyboardMouse (MouseButton LeftButton) Down modifiers pos appstate
    = return appstate { appRotating = True,
                        appLastMousePosition = Just pos
                      }
keyboardMouse (MouseButton LeftButton) Up modifiers pos appstate
    = return appstate { appRotating = False,
                        appLastMousePosition = Nothing
                      }
keyboardMouse _ _ modifiers pos appstate
    = return appstate

main = do 
  (progname, args) <- getArgsAndInitialize
  case args of
    [scriptfile] -> run scriptfile
    _ -> putStrLn $ "Usage: " ++ progname ++ " <animation.ddc>"

run scriptfile = do
  i <- I.new scriptfile
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

  createWindow "Die Drei C"
  let appstate = AppState { appLastMousePosition = Nothing,
                            appRotating = False,
                            appRotation = Vector3 0 0 0,
                            appInterpreter = i
                          }
  appstateRef <- newIORef appstate

  displayCallback $= (appFun appstateRef display)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (\k ks m p -> appFun appstateRef $ keyboardMouse k ks m p)
  motionCallback $= Just (appFun appstateRef . mouseMotion)
  let timer = addTimerCallback 10 $
              do timer
                 appFun appstateRef update
  timer
  mainLoop
