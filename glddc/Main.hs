{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

data AppState = AppState {
      appLastMousePosition :: Maybe Position,
      appRotating :: Bool,
      appRotation :: Vector3 GLfloat
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
     -> IO ()
cube x y z w h d = do
  renderPrimitive Quads $ do
                     -- Front
                     color (Color3 (1::GLfloat) 1 1)
                     vertex (Vertex3 x y z)
                     vertex (Vertex3 (x + w) y z)
                     vertex (Vertex3 (x + w) (y + h) z)
                     vertex (Vertex3 x (y + h) z)
                     -- Back
                     color (Color3 (1::GLfloat) 0 0)
                     vertex (Vertex3 x y (z + d))
                     vertex (Vertex3 x (y + h) (z + d))
                     vertex (Vertex3 (x + w) (y + h) (z + d))
                     vertex (Vertex3 (x + w) y (z + d))
                     -- Top
                     color (Color3 (0::GLfloat) 1 0)
                     vertex (Vertex3 x (y + h) z)
                     vertex (Vertex3 (x + w) (y + h) z)
                     vertex (Vertex3 (x + w) (y + h) (z + d))
                     vertex (Vertex3 x (y + h) (z + d))
                     -- Bottom
                     color (Color3 (0::GLfloat) 0 1)
                     vertex (Vertex3 x y z)
                     vertex (Vertex3 x y (z + d))
                     vertex (Vertex3 (x + w) y (z + d))
                     vertex (Vertex3 (x + w) y z)
                     -- Left
                     color (Color3 (1::GLfloat) 1 0)
                     vertex (Vertex3 x y z)
                     vertex (Vertex3 x (y + h) z)
                     vertex (Vertex3 x (y + h) (z + d))
                     vertex (Vertex3 x y (z + d))
                     -- Right
                     color (Color3 (1::GLfloat) 0 1)
                     vertex (Vertex3 (x + w) y z)
                     vertex (Vertex3 (x + w) y (z + d))
                     vertex (Vertex3 (x + w) (y + h) (z + d))
                     vertex (Vertex3 (x + w) (y + h) z)
                     

display :: AppState -> IO ()
display appstate = do
  clearDepth $= 1.0
  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back
  depthFunc $= Just Less
  shadeModel $= Smooth
  hint PerspectiveCorrection $= Nicest
  hint PolygonSmooth $= Nicest

  matrixMode $= Projection
  loadIdentity
  Size w h <- get windowSize
  let aspect = fromIntegral w / fromIntegral h
  perspective 45 aspect 1 100

  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 0 0 (-25)) (Vertex3 0 0 0) (Vector3 0 1 0)
  scale (-1::GLfloat) 1 1  -- ^make right-hand left-handed

  let drawC :: GLfloat -> GLfloat -> GLfloat -> IO ()
      drawC x y z = preservingMatrix $ do
                      translate $ Vector3 (-x) (-y) (-z)
                      cube 2 2 0 1 1 1
                      cube 1 2 0 1 1 1
                      cube 0 2 0 1 1 1
                      cube 0 1 0 1 1 1
                      cube 0 0 0 1 1 1
                      cube 0 (-1) 0 1 1 1
                      cube 1 (-1) 0 1 1 1
                      cube 2 (-1) 0 1 1 1
      Vector3 rx ry rz = appRotation appstate
  putStrLn $ "rotation=" ++ (show $ appRotation appstate)
  rotate (180 * ry) $ Vector3 1.0 0 0
  rotate (180 * rx) $ Vector3 0 1.0 0
  rotate (180 * rz) $ Vector3 0 0 1.0
  -- 1st C
  drawC (-4) 0 (-1)
  -- 2nd C
  drawC 0 0 0
  -- 3rd C
  drawC 4 0 1
  flush
  swapBuffers

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
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

  createWindow "Die Drei C"
  let appstate = AppState { appLastMousePosition = Nothing,
                            appRotating = False,
                            appRotation = Vector3 0 0 0
                          }
  appstateRef <- newIORef appstate

  displayCallback $= (appFun appstateRef display)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (\k ks m p -> appFun appstateRef $ keyboardMouse k ks m p)
  motionCallback $= Just (appFun appstateRef . mouseMotion)
  mainLoop
