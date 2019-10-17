{-# LANGUAGE ScopedTypeVariables #-}

-- | OpenGL utility and convenience functions.
module GLUtils
  ( Uniform(..)
  , Attribute(..)
  , linkUniform
  , linkAttribute
  , setAttribConst
  , newVertexArrayObject
  , loadVertexData
  , loadElementData
  , loadShader
  )
where

import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Rendering.OpenGL      ( ($=) )
import qualified Data.ByteString               as B
import qualified Graphics.Rendering.OpenGL     as GL

-- | A type to represent GLSL uniform parameters.
newtype Uniform a = Uniform String deriving Show

-- | A buffer attribute that can be accessed from shaders.
data Attribute a = Attribute !String !GL.NumComponents !GL.DataType !GL.IntegerHandling deriving Show

-- | Get a 'GL.StateVar' that links to a 'Uniform'.
{-# INLINE linkUniform #-}
linkUniform :: GL.Uniform a => GL.Program -> Uniform a -> IO (GL.StateVar a)
linkUniform program (Uniform uniform) = GL.uniform <$> GL.uniformLocation program uniform

-- | Create a new vertex array object.
newVertexArrayObject :: IO GL.VertexArrayObject
newVertexArrayObject = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  pure vao

-- | Set the offset and stride of an 'Attribute' in the current buffer.
linkAttribute :: GL.Program -> Attribute a -> Int -> GL.Stride -> IO ()
linkAttribute program (Attribute name numComponents dataType integerHandling) offset stride
  = do
    attribute <- GL.get $ GL.attribLocation program name
    GL.vertexAttribArray attribute $= GL.Enabled
    GL.vertexAttribPointer attribute
      $= ( integerHandling
         , GL.VertexArrayDescriptor numComponents dataType stride (nullPtr `plusPtr` offset)
         )

-- | Set an attribute to a constant value.
setAttribConst :: GL.VertexAttrib a => GL.Program -> Attribute a -> a -> IO ()
setAttribConst program (Attribute name _ _ integerHandling) value = do
  inputAttrib <- GL.get $ GL.attribLocation program name
  GL.vertexAttribArray inputAttrib $= GL.Disabled
  GL.vertexAttrib integerHandling inputAttrib value

{-# INLINE loadVertexData #-}
loadVertexData :: forall a . Storable a => [a] -> IO GL.BufferObject
loadVertexData = loadBufferData GL.ArrayBuffer

{-# INLINE loadElementData #-}
loadElementData :: [GL.GLuint] -> IO GL.BufferObject
loadElementData = loadBufferData GL.ElementArrayBuffer

{-# INLINE loadBufferData #-}
loadBufferData :: forall a . Storable a => GL.BufferTarget -> [a] -> IO GL.BufferObject
loadBufferData bufferTarget vdata = do
  bo <- GL.genObjectName
  GL.bindBuffer bufferTarget $= Just bo
  withArray vdata $ \pdata ->
    GL.bufferData bufferTarget
      $= (fromIntegral $ length vdata * sizeOf (undefined :: a), pdata, GL.StaticRead)
  pure bo

-- | Load a shader into a shader program.
loadShader :: GL.Program -> GL.ShaderType -> B.ByteString -> IO ()
loadShader program shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $= source
  GL.compileShader shader
  GL.attachShader program shader
