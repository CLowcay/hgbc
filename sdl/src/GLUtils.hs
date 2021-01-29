{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | OpenGL utility and convenience functions.
module GLUtils
  ( OpenGLError (..),

    -- * Matrices
    GLmatrix (..),
    GLmatrix4 (..),

    -- * Uniforms
    linkUniform,

    -- * Vertex array objects
    VertexArrayObject,
    genVertexArrayObject,
    bindVertexArrayObject,

    -- * Attributes
    Attribute (..),
    AttributeDivisor (..),
    IntegerHandling (..),
    NumComponents,
    ElementDataType (..),
    Offset,
    Stride,
    linkAttribute,

    -- * Buffers
    BufferObject (..),
    BufferTarget (..),
    BufferUpdateStrategy (..),
    genBuffer,
    bindBuffer,
    makeVertexBuffer,
    makeElementBuffer,
    makeWritablePersistentBuffer,
    linkTextureBuffer,
    linkTextureBufferRange,
    linkUniformBuffer,

    -- * Textures
    Texture (..),
    TextureUnit (..),
    TextureTarget (..),
    activeTextureUnit,
    genTexture,
    bindTexture,

    -- * Programs
    Program (..),
    ShaderType (..),
    useProgram,
    compileShaders,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadUnliftIO, allocate, runResourceT, unprotect)
import Data.Bits (Bits (..))
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.StateVar (StateVar, makeStateVar)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (pokeArray, withArray)
import Foreign.Ptr (IntPtr, Ptr, intPtrToPtr, nullPtr)
import Foreign.Storable (Storable (peek, poke, sizeOf))
import qualified Graphics.GL.Core44 as Raw

data OpenGLError = OpenGLError String !B.ByteString deriving (Eq, Ord, Show, Exception)

-- | Types that wrap OpenGL enums.
class OpenGLEnum a where
  toOpenGLEnum :: a -> Raw.GLenum

-- | Types that can be used as GLSL uniforms.
class UniformAccess a where
  getUniform :: MonadIO m => Raw.GLuint -> Raw.GLint -> m a
  setUniform :: MonadIO m => Raw.GLint -> a -> m ()

instance UniformAccess Raw.GLint where
  {-# INLINE getUniform #-}
  getUniform program uniform = capture (Raw.glGetUniformiv program uniform)
  {-# INLINE setUniform #-}
  setUniform = Raw.glUniform1i

-- | Matrix types.
class GLmatrix a where
  makeMatrix :: MonadIO m => [Float] -> m a

-- | An OpenGL matrix.
newtype GLmatrix4 = GLmatrix4 (ForeignPtr Float)

-- | Load matrix data (data must be in column major order).
instance GLmatrix GLmatrix4 where
  {-# INLINE makeMatrix #-}
  makeMatrix elements =
    if length elements /= 16
      then error "Invalid args to makeMatrix"
      else do
        fptr <- liftIO (mallocForeignPtrArray 16)
        liftIO . withForeignPtr fptr $ \ptr -> pokeArray ptr elements
        pure (GLmatrix4 fptr)

instance UniformAccess GLmatrix4 where
  {-# INLINE getUniform #-}
  getUniform program uniform = do
    fptr <- liftIO (mallocForeignPtrArray 16)
    liftIO (withForeignPtr fptr (Raw.glGetUniformfv program uniform))
    pure (GLmatrix4 fptr)
  {-# INLINE setUniform #-}
  setUniform uniform (GLmatrix4 fptr) =
    liftIO (withForeignPtr fptr (Raw.glUniformMatrix4fv uniform 1 Raw.GL_FALSE))

-- | Get a 'StateVar' that links to a uniform.
{-# INLINE linkUniform #-}
linkUniform :: (UniformAccess a, MonadIO m) => Program -> B.ByteString -> m (StateVar a)
linkUniform (Program program) uniform = liftIO $ do
  uniformLocation <- B.useAsCString uniform (Raw.glGetUniformLocation program)
  pure (makeStateVar (getUniform program uniformLocation) (setUniform uniformLocation))

-- | Link the currently bound texture buffer to the currently buffer texture.
{-# INLINEABLE linkTextureBuffer #-}
linkTextureBuffer :: MonadIO m => BufferObject -> m ()
linkTextureBuffer (BufferObject buffer) = Raw.glTexBuffer Raw.GL_TEXTURE_BUFFER Raw.GL_R8UI buffer

-- | Link the currently bound texture buffer to the currently buffer texture.
{-# INLINEABLE linkTextureBufferRange #-}
linkTextureBufferRange :: MonadIO m => BufferObject -> Raw.GLintptr -> Raw.GLsizeiptr -> m ()
linkTextureBufferRange (BufferObject buffer) = Raw.glTexBufferRange Raw.GL_TEXTURE_BUFFER Raw.GL_R8UI buffer

-- | A vertex array object.
newtype VertexArrayObject = VertexArrayObject Raw.GLuint deriving (Eq, Show)

-- | Create a new vertex array object.
genVertexArrayObject :: IO VertexArrayObject
genVertexArrayObject = VertexArrayObject <$> capture (Raw.glGenVertexArrays 1)

-- | Bind a vertex array object
{-# INLINE bindVertexArrayObject #-}
bindVertexArrayObject :: MonadIO m => VertexArrayObject -> m ()
bindVertexArrayObject (VertexArrayObject vao) = Raw.glBindVertexArray vao

-- | How to convert and normalize vertex attribute elements.
data IntegerHandling
  = NormalizeToFloat
  | ConvertToFloat
  | KeepInteger
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Number of components in a vertex attribute.
type NumComponents = Raw.GLint

-- | Element data type of a vertex attribute.
data ElementDataType
  = Bytes
  | Shorts
  | Ints
  | UnsignedBytes
  | UnsignedShorts
  | UnsignedInts
  | Floats
  | HalfFloats
  | Doubles
  | Fixeds
  deriving (Eq, Ord, Show, Bounded, Enum)

instance OpenGLEnum ElementDataType where
  toOpenGLEnum Bytes = Raw.GL_BYTE
  toOpenGLEnum Shorts = Raw.GL_SHORT
  toOpenGLEnum Ints = Raw.GL_INT
  toOpenGLEnum UnsignedBytes = Raw.GL_UNSIGNED_BYTE
  toOpenGLEnum UnsignedShorts = Raw.GL_UNSIGNED_SHORT
  toOpenGLEnum UnsignedInts = Raw.GL_UNSIGNED_INT
  toOpenGLEnum Floats = Raw.GL_FLOAT
  toOpenGLEnum HalfFloats = Raw.GL_HALF_FLOAT
  toOpenGLEnum Doubles = Raw.GL_DOUBLE
  toOpenGLEnum Fixeds = Raw.GL_FIXED

data AttributeDivisor = PerVertex | PerInstance deriving (Eq, Ord, Show, Bounded, Enum)

-- | A buffer attribute that can be accessed from shaders.
data Attribute = Attribute !B.ByteString !NumComponents !ElementDataType !AttributeDivisor !IntegerHandling deriving (Eq, Show)

type Offset = IntPtr

type Stride = Raw.GLsizei

-- | Set the offset and stride of an 'Attribute' in the current vertex buffer.
{-# INLINEABLE linkAttribute #-}
linkAttribute :: Program -> Attribute -> Offset -> Stride -> IO ()
linkAttribute (Program program) (Attribute name numComponents elementType divisor integerHandling) offset stride =
  do
    attribute <- fromIntegral <$> B.useAsCString name (Raw.glGetAttribLocation program)
    Raw.glEnableVertexAttribArray attribute
    case integerHandling of
      NormalizeToFloat -> vertexAttribPointerFloat attribute Raw.GL_TRUE
      ConvertToFloat -> vertexAttribPointerFloat attribute Raw.GL_FALSE
      KeepInteger ->
        Raw.glVertexAttribIPointer
          attribute
          numComponents
          (toOpenGLEnum elementType)
          stride
          (intPtrToPtr offset)
    handleDivisor attribute
  where
    handleDivisor attribute = case divisor of
      PerVertex -> pure ()
      PerInstance -> Raw.glVertexAttribDivisor attribute 1
    vertexAttribPointerFloat attribute normalize =
      Raw.glVertexAttribPointer
        attribute
        numComponents
        (toOpenGLEnum elementType)
        normalize
        stride
        (intPtrToPtr offset)

-- | A buffer of some sort.
newtype BufferObject = BufferObject Raw.GLuint deriving (Eq, Show)

-- | The buffer types that we can bind.
data BufferTarget
  = ArrayBuffer
  | ElementArrayBuffer
  | TextureBufferBuffer
  | UniformBuffer
  | PixelUpload
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Get the OpenGL enum for a 'BufferTarget'.
instance OpenGLEnum BufferTarget where
  toOpenGLEnum ArrayBuffer = Raw.GL_ARRAY_BUFFER
  toOpenGLEnum ElementArrayBuffer = Raw.GL_ELEMENT_ARRAY_BUFFER
  toOpenGLEnum TextureBufferBuffer = Raw.GL_TEXTURE_BUFFER
  toOpenGLEnum UniformBuffer = Raw.GL_UNIFORM_BUFFER
  toOpenGLEnum PixelUpload = Raw.GL_PIXEL_UNPACK_BUFFER

-- | Generate an empty buffer object.
{-# INLINE genBuffer #-}
genBuffer :: MonadIO m => m BufferObject
genBuffer = BufferObject <$> capture (Raw.glGenBuffers 1)

-- | Bind a buffer.
{-# INLINE bindBuffer #-}
bindBuffer :: MonadIO m => BufferTarget -> BufferObject -> m ()
bindBuffer target (BufferObject bo) = Raw.glBindBuffer (toOpenGLEnum target) bo

-- | Create and initialize a vertex buffer.
{-# INLINEABLE makeVertexBuffer #-}
makeVertexBuffer :: MonadIO m => forall a. Storable a => [a] -> m BufferObject
makeVertexBuffer = makeBuffer ArrayBuffer

-- | Create and initialize an element buffer.
{-# INLINEABLE makeElementBuffer #-}
makeElementBuffer :: MonadIO m => [Raw.GLuint] -> m BufferObject
makeElementBuffer = makeBuffer ElementArrayBuffer

-- | Make a static buffer.
{-# INLINE makeBuffer #-}
makeBuffer :: forall m a. (MonadIO m, Storable a) => BufferTarget -> [a] -> m BufferObject
makeBuffer bufferTarget vdata = do
  bo <- capture (Raw.glGenBuffers 1)
  Raw.glBindBuffer target bo
  liftIO . withArray vdata $ \pData -> do
    let bufferSize = fromIntegral (length vdata * sizeOf (undefined :: a))
    Raw.glBufferData target bufferSize pData Raw.GL_STATIC_DRAW
    pure (BufferObject bo)
  where
    target = toOpenGLEnum bufferTarget

-- | The strategy for propagating changes to a persistent buffer back to OpenGL.
data BufferUpdateStrategy
  = -- | Writes are propagated immediately.
    Coherent
  | -- | Writes are never propagated until glFlush is executed.
    ExplicitFlush
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Allocate and bind a coherent writeable persistent buffer.
{-# INLINEABLE makeWritablePersistentBuffer #-}
makeWritablePersistentBuffer ::
  MonadIO m => BufferUpdateStrategy -> BufferTarget -> Raw.GLsizeiptr -> m (BufferObject, Ptr a)
makeWritablePersistentBuffer updateStrategy bufferTarget size = do
  bo <- capture (Raw.glGenBuffers 1)
  Raw.glBindBuffer target bo
  Raw.glBufferStorage target size nullPtr storageFlags
  ptr <- Raw.glMapBufferRange target 0 size mapFlags
  pure (BufferObject bo, ptr)
  where
    target = toOpenGLEnum bufferTarget
    baseFlags = Raw.GL_MAP_WRITE_BIT .|. Raw.GL_MAP_PERSISTENT_BIT
    (storageFlags, mapFlags) = case updateStrategy of
      Coherent -> (baseFlags .|. Raw.GL_MAP_COHERENT_BIT, baseFlags .|. Raw.GL_MAP_COHERENT_BIT)
      ExplicitFlush -> (baseFlags, baseFlags .|. Raw.GL_MAP_FLUSH_EXPLICIT_BIT)

-- | Bind a buffer to a buffer-backed uniform.
{-# INLINEABLE linkUniformBuffer #-}
linkUniformBuffer :: MonadIO m => Program -> B.ByteString -> BufferObject -> Raw.GLuint -> m ()
linkUniformBuffer (Program program) uniform (BufferObject buffer) bindingLocation = do
  Raw.glBindBufferBase Raw.GL_UNIFORM_BUFFER bindingLocation buffer
  uniformBlockIndex <- liftIO (B.useAsCString uniform (Raw.glGetUniformBlockIndex program))
  Raw.glUniformBlockBinding program uniformBlockIndex bindingLocation

-- | An OpenGL texture unit.
newtype TextureUnit = TextureUnit Raw.GLint deriving (Eq, Show)

instance UniformAccess TextureUnit where
  getUniform program uniform = TextureUnit <$> capture (Raw.glGetUniformiv program uniform)
  setUniform uniform (TextureUnit unit) = Raw.glUniform1i uniform unit

-- | Set the active texture unit.
{-# INLINE activeTextureUnit #-}
activeTextureUnit :: MonadIO m => TextureUnit -> m ()
activeTextureUnit (TextureUnit tu) = Raw.glActiveTexture (Raw.GL_TEXTURE0 + fromIntegral tu)

-- | An OpenGL texture.
newtype Texture = Texture Raw.GLuint deriving (Eq, Show)

-- | An OpenGL texture target.
data TextureTarget
  = Texture1D
  | Texture2D
  | Texture3D
  | TextureBuffer
  deriving (Eq, Ord, Show, Bounded, Enum)

instance OpenGLEnum TextureTarget where
  toOpenGLEnum Texture1D = Raw.GL_TEXTURE_1D
  toOpenGLEnum Texture2D = Raw.GL_TEXTURE_2D
  toOpenGLEnum Texture3D = Raw.GL_TEXTURE_3D
  toOpenGLEnum TextureBuffer = Raw.GL_TEXTURE_BUFFER

-- | Make a new texture.
{-# INLINE genTexture #-}
genTexture :: MonadIO m => m Texture
genTexture = Texture <$> capture (Raw.glGenTextures 1)

{-# INLINE bindTexture #-}
bindTexture :: MonadIO m => TextureTarget -> Texture -> m ()
bindTexture target (Texture texture) = Raw.glBindTexture (toOpenGLEnum target) texture

-- | A shader type.
data ShaderType
  = ComputeShader
  | VertexShader
  | TessControlShader
  | TessEvaluationShader
  | GeometryShader
  | FragmentShader
  deriving (Eq, Ord, Show, Bounded, Enum)

instance OpenGLEnum ShaderType where
  toOpenGLEnum ComputeShader = Raw.GL_COMPUTE_SHADER
  toOpenGLEnum VertexShader = Raw.GL_VERTEX_SHADER
  toOpenGLEnum TessControlShader = Raw.GL_TESS_CONTROL_SHADER
  toOpenGLEnum TessEvaluationShader = Raw.GL_TESS_EVALUATION_SHADER
  toOpenGLEnum GeometryShader = Raw.GL_GEOMETRY_SHADER
  toOpenGLEnum FragmentShader = Raw.GL_FRAGMENT_SHADER

-- A shader program.
newtype Program = Program Raw.GLuint deriving (Eq, Show)

-- | Use a program.
{-# INLINE useProgram #-}
useProgram :: MonadIO m => Program -> m ()
useProgram (Program program) = Raw.glUseProgram program

-- | Compile and link a set of shaders.
{-# INLINEABLE compileShaders #-}
compileShaders :: MonadUnliftIO m => [(ShaderType, B.ByteString)] -> m Program
compileShaders shaders = runResourceT $ do
  (programKey, program) <- allocate Raw.glCreateProgram Raw.glDeleteProgram
  traverse_ (loadShader program) shaders
  Raw.glLinkProgram program

  isLinked <- capture (Raw.glGetProgramiv program Raw.GL_LINK_STATUS)
  when (isLinked == 0) . liftIO $ do
    logLength <- capture (Raw.glGetProgramiv program Raw.GL_INFO_LOG_LENGTH)
    message <- allocaBytes (fromIntegral logLength) $ \ptr -> do
      Raw.glGetProgramInfoLog program (fromIntegral logLength) nullPtr ptr
      B.packCString ptr
    throwIO (OpenGLError "Program linkage failed" message)

  void (unprotect programKey)
  pure (Program program)
  where
    loadShader program (shaderType, source) = do
      (_, shader) <- allocate (Raw.glCreateShader (toOpenGLEnum shaderType)) Raw.glDeleteShader
      liftIO . B.useAsCString source $ \pSource -> alloca $ \ppSource -> do
        poke ppSource pSource
        Raw.glShaderSource shader 1 ppSource nullPtr
      Raw.glCompileShader shader

      compilePassed <- capture (Raw.glGetShaderiv shader Raw.GL_COMPILE_STATUS)
      when (compilePassed == 0) . liftIO $ do
        logLength <- capture (Raw.glGetShaderiv shader Raw.GL_INFO_LOG_LENGTH)
        message <- allocaBytes (fromIntegral logLength) $ \ptr -> do
          Raw.glGetShaderInfoLog shader (fromIntegral logLength) nullPtr ptr
          B.packCString ptr
        throwIO (OpenGLError "Shader compilation failed" message)

      void (allocate (shader <$ Raw.glAttachShader program shader) (Raw.glDetachShader program))
      pure shader

-- | Utility to deal with foreign functions that use pointers to return values.
{-# INLINE capture #-}
capture :: (Storable a, MonadIO m) => (Ptr a -> IO ()) -> m a
capture action = liftIO . alloca $ \p -> do
  action p
  peek p
