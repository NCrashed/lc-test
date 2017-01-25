module Main where

import Codec.Picture as Juicy
import Control.Concurrent
import Data.IORef
import Matrix
import SDL as SDL

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import LambdaCube.GL as LC
import LambdaCube.GL.Mesh as LC
import LambdaCube.Compiler as LC

-- | Load and compile LambdaCube pipeline
initPipe :: IO (GLRenderer, PipelineSchema)
initPipe = do
  mpd <- LC.compileMain ["."] OpenGL33 "main.lc"
  case mpd of
    Left err -> fail $ "compile error:\n" ++ show err
    Right pd -> do
      let sch = makeSchema $ do
            defObjectArray "objects" Triangles $ do
              "position"  @: Attribute_V3F
              "uv"        @: Attribute_V2F
            defUniforms $ do
              "projmat"        @: M44F
              "diffuseTexture" @: FTexture2D
      r <- LC.allocRenderer pd
      return (r, sch)

-- | Initialise storage and load GPU resources
initStorage :: GLRenderer -> PipelineSchema -> IO (GLStorage, TextureData)
initStorage renderer scheme = do
  storage <- LC.allocStorage scheme
  Nothing <- LC.setStorage renderer storage
  _ <- LC.uploadMeshToGPU cubeMesh >>= LC.addMeshToObjectArray storage "objects" []
  Right img <- Juicy.readImage "logo.png"
  texData <- LC.uploadTexture2DToGPU img
  return (storage, texData)

-- | Draw single frame
drawFrame :: GLRenderer -> GLStorage -> TextureData -> Float -> Window -> IO ()
drawFrame renderer storage textureData t win = do
  SDL.V2 w h <- glGetDrawableSize win
  let aspect = fromIntegral w / fromIntegral h
  LC.updateUniforms storage $ do
    "diffuseTexture" @= return textureData
    "projmat" @= return (mvp aspect t)
  LC.setScreenSize storage (fromIntegral w) (fromIntegral h)
  LC.renderFrame renderer
  glSwapWindow win

-- | Initialise application window with OpenGL context
initWindow :: IO Window
initWindow = do
  win <- createWindow "Test" defaultWindow {
      windowOpenGL = Just defaultOpenGL {
        glProfile = Core Normal 3 3
      }
    }
  ctx <- glCreateContext win
  glMakeCurrent win ctx
  return win

main :: IO ()
main = do
  SDL.initializeAll
  win <- initWindow
  (renderer, scheme) <- initPipe
  (storage, textureData) <- initStorage renderer scheme
  tref <- newIORef 0
  let
    go = do
      t <- readIORef tref
      drawFrame renderer storage textureData t win
      let fps = 60
          dt = 1 / fps :: Float
          dms = ceiling $ dt * 1000000
      threadDelay dms
      writeIORef tref $ t + dt
      go
  go

-- geometry data: triangles
cubeMesh :: LC.Mesh
cubeMesh = Mesh
  { mAttributes   = M.fromList
      [ ("position",  A_V3F $ V.fromList vertecies)
      , ("uv",        A_V2F $ V.fromList uvs)
      ]
  , mPrimitive    = P_Triangles
  }
  where
  vertecies = [
      v3, v2, v1, v3, v1, v0
    , v4, v7, v6, v4, v6, v5
    , v0, v1, v7, v0, v7, v4
    , v5, v6, v2, v5, v2, v3
    , v2, v6, v7, v2, v7, v1
    , v5, v3, v0, v5, v0, v4
    ]
  uvs = concat $ replicate 6 [u1, u2, u3, u1, u3, u0]

  v0 = LC.V3 (-1) (-1) (-1)
  v1 = LC.V3 (-1)   1  (-1)
  v2 = LC.V3   1    1  (-1)
  v3 = LC.V3   1  (-1) (-1)
  v4 = LC.V3 (-1) (-1)   1
  v5 = LC.V3   1  (-1)   1
  v6 = LC.V3   1    1    1
  v7 = LC.V3 (-1)   1    1

  u0 = LC.V2 0 0
  u1 = LC.V2 1 0
  u2 = LC.V2 1 1
  u3 = LC.V2 0 1
