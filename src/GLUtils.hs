{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | OpenGL utility and convenience functions.
module GLUtils
  ( OpenGLError(..)

  -- * Matrices
  , GLmatrix(..)
  , GLmatrix4(..)

  -- * Uniforms
  , linkUniform

  -- * Vertex array objects
  , VertexArrayObject
  , genVertexArrayObject
  , bindVertexArrayObject

  -- * Attributes
  , Attribute(..)
  , IntegerHandling(..)
  , NumComponents
  , ElementDataType(..)
  , Offset
  , Stride
  , linkAttribute

  -- * Buffers
  , BufferObject(..)
  , BufferTarget(..)
  , BufferUpdateStrategy(..)
  , genBuffer
  , bindBuffer
  , makeVertexBuffer
  , makeElementBuffer
  , makeWritablePersistentBuffer
  , linkTextureBuffer
  , linkTextureBufferRange
  , linkUniformBuffer

  -- * Textures
  , Texture(..)
  , TextureUnit(..)
  , TextureTarget(..)
  , activeTextureUnit
  , genTexture
  , bindTexture

  -- * Synchronization
  , Fence(..)
  , SyncStatus(..)
  , Timeout
  , insertFence
  , waitForFence

  -- * Programs
  , Program(..)
  , ShaderType(..)
  , useProgram
  , compileShaders
  )
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable
import           Data.StateVar
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL.Core44
import qualified Data.ByteString               as B

data OpenGLError = OpenGLError String !B.ByteString deriving (Eq, Ord, Show, Exception)

-- | Types that wrap OpenGL enums.
class OpenGLEnum a where
  toOpenGLEnum :: a -> GLenum

-- | Types that can be used as GLSL uniforms.
class UniformAccess a where
  getUniform :: MonadIO m => GLuint -> GLint -> m a
  setUniform :: MonadIO m => GLint -> a -> m ()

instance UniformAccess GLint where
  getUniform program uniform = capture (glGetUniformiv program uniform)
  setUniform = glUniform1i

-- | Matrix types.
class GLmatrix a where
  makeMatrix :: MonadIO m => [Float] -> m a

-- | An OpenGL matrix.
newtype GLmatrix4 = GLmatrix4 (ForeignPtr Float)

-- | Load matrix data (data must be in column major order).
instance GLmatrix GLmatrix4 where
  {-# INLINE makeMatrix #-}
  makeMatrix elements = if length elements /= 16
    then error "Invalid args to makeMatrix"
    else do
      fptr <- liftIO (mallocForeignPtrArray 16)
      liftIO . withForeignPtr fptr $ \ptr -> pokeArray ptr elements
      pure (GLmatrix4 fptr)

instance UniformAccess GLmatrix4 where
  getUniform program uniform = do
    fptr <- liftIO (mallocForeignPtrArray 16)
    liftIO (withForeignPtr fptr (glGetUniformfv program uniform))
    pure (GLmatrix4 fptr)
  setUniform uniform (GLmatrix4 fptr) =
    liftIO (withForeignPtr fptr (glUniformMatrix4fv uniform 1 GL_FALSE))

-- | Get a 'StateVar' that links to a uniform.
{-# INLINE linkUniform #-}
linkUniform :: (UniformAccess a, MonadIO m) => Program -> B.ByteString -> m (StateVar a)
linkUniform (Program program) uniform = liftIO $ do
  uniformLocation <- B.useAsCString uniform (glGetUniformLocation program)
  pure (makeStateVar (getUniform program uniformLocation) (setUniform uniformLocation))

-- | Link the currently bound texture buffer to the currently buffer texture.
linkTextureBuffer :: MonadIO m => BufferObject -> m ()
linkTextureBuffer (BufferObject buffer) = glTexBuffer GL_TEXTURE_BUFFER GL_R8UI buffer

-- | Link the currently bound texture buffer to the currently buffer texture.
linkTextureBufferRange :: MonadIO m => BufferObject -> GLintptr -> GLsizeiptr -> m ()
linkTextureBufferRange (BufferObject buffer) = glTexBufferRange GL_TEXTURE_BUFFER GL_R8UI buffer

-- | A vertex array object.
newtype VertexArrayObject = VertexArrayObject GLuint deriving (Eq, Show)

-- | Create a new vertex array object.
genVertexArrayObject :: IO VertexArrayObject
genVertexArrayObject = VertexArrayObject <$> capture (glGenVertexArrays 1)

-- | Bind a vertex array object
{-# INLINE bindVertexArrayObject #-}
bindVertexArrayObject :: MonadIO m => VertexArrayObject -> m ()
bindVertexArrayObject (VertexArrayObject vao) = glBindVertexArray vao

-- | How to convert and normalize vertex attribute elements.
data IntegerHandling = NormalizeToFloat
                     | ConvertToFloat
                     | KeepInteger
                     deriving (Eq, Ord, Show, Bounded, Enum)

-- | Number of components in a vertex attribute.
type NumComponents = GLint

-- | Element data type of a vertex attribute.
data ElementDataType = Bytes
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
  toOpenGLEnum Bytes          = GL_BYTE
  toOpenGLEnum Shorts         = GL_SHORT
  toOpenGLEnum Ints           = GL_INT
  toOpenGLEnum UnsignedBytes  = GL_UNSIGNED_BYTE
  toOpenGLEnum UnsignedShorts = GL_UNSIGNED_SHORT
  toOpenGLEnum UnsignedInts   = GL_UNSIGNED_INT
  toOpenGLEnum Floats         = GL_FLOAT
  toOpenGLEnum HalfFloats     = GL_HALF_FLOAT
  toOpenGLEnum Doubles        = GL_DOUBLE
  toOpenGLEnum Fixeds         = GL_FIXED

-- | A buffer attribute that can be accessed from shaders.
data Attribute = Attribute !B.ByteString !NumComponents !ElementDataType !IntegerHandling deriving (Eq, Show)

type Offset = IntPtr
type Stride = GLsizei

-- | Set the offset and stride of an 'Attribute' in the current vertex buffer.
{-# INLINABLE linkAttribute #-}
linkAttribute :: Program -> Attribute -> Offset -> Stride -> IO ()
linkAttribute (Program program) (Attribute name numComponents elementType integerHandling) offset stride
  = do
    attribute <- fromIntegral <$> B.useAsCString name (glGetAttribLocation program)
    glEnableVertexAttribArray attribute
    case integerHandling of
      NormalizeToFloat -> vertexAttribPointerFloat attribute GL_TRUE
      ConvertToFloat   -> vertexAttribPointerFloat attribute GL_FALSE
      KeepInteger      -> glVertexAttribIPointer attribute
                                                 numComponents
                                                 (toOpenGLEnum elementType)
                                                 stride
                                                 (intPtrToPtr offset)
 where
  vertexAttribPointerFloat attribute normalize = glVertexAttribPointer attribute
                                                                       numComponents
                                                                       (toOpenGLEnum elementType)
                                                                       normalize
                                                                       stride
                                                                       (intPtrToPtr offset)

-- | A buffer of some sort.
newtype BufferObject = BufferObject GLuint deriving (Eq, Show)

-- | The buffer types that we can bind.
data BufferTarget = ArrayBuffer
                  | ElementArrayBuffer
                  | TextureBufferBuffer
                  | UniformBuffer
                  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Get the OpenGL enum for a 'BufferTarget'.
instance OpenGLEnum BufferTarget where
  toOpenGLEnum ArrayBuffer         = GL_ARRAY_BUFFER
  toOpenGLEnum ElementArrayBuffer  = GL_ELEMENT_ARRAY_BUFFER
  toOpenGLEnum TextureBufferBuffer = GL_TEXTURE_BUFFER
  toOpenGLEnum UniformBuffer       = GL_UNIFORM_BUFFER

-- | Generate an empty buffer object.
{-# INLINE genBuffer #-}
genBuffer :: MonadIO m => m BufferObject
genBuffer = BufferObject <$> capture (glGenBuffers 1)

-- | Bind a buffer.
{-# INLINE bindBuffer #-}
bindBuffer :: MonadIO m => BufferTarget -> BufferObject -> m ()
bindBuffer target (BufferObject bo) = glBindBuffer (toOpenGLEnum target) bo

-- | Create and initialize a vertex buffer.
{-# INLINABLE makeVertexBuffer #-}
makeVertexBuffer :: MonadIO m => forall a . Storable a => [a] -> m BufferObject
makeVertexBuffer = makeBuffer ArrayBuffer

-- | Create and initialize an element buffer.
{-# INLINABLE makeElementBuffer #-}
makeElementBuffer :: MonadIO m => [GLuint] -> m BufferObject
makeElementBuffer = makeBuffer ElementArrayBuffer

-- | Make a static buffer.
{-# INLINE makeBuffer #-}
makeBuffer :: forall m a . (MonadIO m, Storable a) => BufferTarget -> [a] -> m BufferObject
makeBuffer bufferTarget vdata = do
  bo <- capture (glGenBuffers 1)
  glBindBuffer target bo
  liftIO . withArray vdata $ \pData -> do
    let bufferSize = fromIntegral (length vdata * sizeOf (undefined :: a))
    glBufferData target bufferSize pData GL_STATIC_DRAW
    pure (BufferObject bo)
  where target = toOpenGLEnum bufferTarget

-- | The strategy for propagating changes to a persistent buffer back to OpenGL.
data BufferUpdateStrategy = Coherent       -- ^ Writes are propagated immediately.
                          | ExplicitFlush  -- ^ Writes are never propagated until glFlush is executed.
                          deriving (Eq, Ord, Show, Bounded, Enum)

-- | Allocate and bind a coherent writeable persistent buffer.
{-# INLINABLE makeWritablePersistentBuffer #-}
makeWritablePersistentBuffer
  :: MonadIO m => BufferUpdateStrategy -> BufferTarget -> GLsizeiptr -> m (BufferObject, Ptr a)
makeWritablePersistentBuffer updateStrategy bufferTarget size = do
  bo <- capture (glGenBuffers 1)
  glBindBuffer target bo
  glBufferStorage target size nullPtr storageFlags
  ptr <- glMapBufferRange target 0 size mapFlags
  pure (BufferObject bo, ptr)
 where
  target                   = toOpenGLEnum bufferTarget
  baseFlags                = GL_MAP_WRITE_BIT .|. GL_MAP_PERSISTENT_BIT
  (storageFlags, mapFlags) = case updateStrategy of
    Coherent      -> (baseFlags .|. GL_MAP_COHERENT_BIT, baseFlags .|. GL_MAP_COHERENT_BIT)
    ExplicitFlush -> (baseFlags, baseFlags .|. GL_MAP_FLUSH_EXPLICIT_BIT)

-- | Bind a buffer to a buffer-backed uniform.
{-# INLINABLE linkUniformBuffer #-}
linkUniformBuffer :: MonadIO m => Program -> B.ByteString -> BufferObject -> GLuint -> m ()
linkUniformBuffer (Program program) uniform (BufferObject buffer) bindingLocation = do
  glBindBufferBase GL_UNIFORM_BUFFER bindingLocation buffer
  uniformBlockIndex <- liftIO (B.useAsCString uniform (glGetUniformBlockIndex program))
  glUniformBlockBinding program uniformBlockIndex bindingLocation

-- | An OpenGL texture unit.
newtype TextureUnit = TextureUnit GLint deriving (Eq, Show)

instance UniformAccess TextureUnit where
  getUniform program uniform = TextureUnit <$> capture (glGetUniformiv program uniform)
  setUniform uniform (TextureUnit unit) = glUniform1i uniform unit

-- | Set the active texture unit.
{-# INLINE activeTextureUnit #-}
activeTextureUnit :: MonadIO m => TextureUnit -> m ()
activeTextureUnit (TextureUnit tu) = glActiveTexture (GL_TEXTURE0 + fromIntegral tu)

-- | An OpenGL texture.
newtype Texture = Texture GLuint deriving (Eq, Show)

-- | An OpenGL texture target.
data TextureTarget = Texture1D
                   | Texture2D
                   | Texture3D
                   | TextureBuffer
                   deriving (Eq, Ord, Show, Bounded, Enum)

instance OpenGLEnum TextureTarget where
  toOpenGLEnum Texture1D     = GL_TEXTURE_1D
  toOpenGLEnum Texture2D     = GL_TEXTURE_2D
  toOpenGLEnum Texture3D     = GL_TEXTURE_3D
  toOpenGLEnum TextureBuffer = GL_TEXTURE_BUFFER

-- | Make a new texture.
{-# INLINE genTexture #-}
genTexture :: MonadIO m => m Texture
genTexture = Texture <$> capture (glGenTextures 1)

{-# INLINE bindTexture #-}
bindTexture :: MonadIO m => TextureTarget -> Texture -> m ()
bindTexture target (Texture texture) = glBindTexture (toOpenGLEnum target) texture

-- | An OpenGL fence object.
newtype Fence = Fence GLsync deriving (Eq, Show)

-- | Insert a fence into the GPU command pipeline.
insertFence :: MonadIO m => m Fence
insertFence = Fence <$> glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0

-- | Status of a fence following a wait operation.
data SyncStatus = AlreadySignaled
                | TimeoutExpired
                | ConditionSatisfied
                | WaitFailed
                deriving (Eq, Ord, Show, Bounded, Enum)

type Timeout = GLuint64
waitForFence :: MonadIO m => Fence -> Timeout -> m SyncStatus
waitForFence (Fence fence) timeout = do
  status <- glClientWaitSync fence GL_SYNC_FLUSH_COMMANDS_BIT timeout
  unless (status == GL_WAIT_FAILED) $ glDeleteSync fence
  pure $ case status of
    GL_ALREADY_SIGNALED    -> AlreadySignaled
    GL_TIMEOUT_EXPIRED     -> TimeoutExpired
    GL_CONDITION_SATISFIED -> ConditionSatisfied
    GL_WAIT_FAILED         -> WaitFailed
    code                   -> error ("Unknown sync wait result code " ++ show code)

-- | A shader type.
data ShaderType = ComputeShader
                | VertexShader
                | TessControlShader
                | TessEvaluationShader
                | GeometryShader
                | FragmentShader
                deriving (Eq, Ord, Show, Bounded, Enum)

instance OpenGLEnum ShaderType where
  toOpenGLEnum ComputeShader        = GL_COMPUTE_SHADER
  toOpenGLEnum VertexShader         = GL_VERTEX_SHADER
  toOpenGLEnum TessControlShader    = GL_TESS_CONTROL_SHADER
  toOpenGLEnum TessEvaluationShader = GL_TESS_EVALUATION_SHADER
  toOpenGLEnum GeometryShader       = GL_GEOMETRY_SHADER
  toOpenGLEnum FragmentShader       = GL_FRAGMENT_SHADER

-- A shader program.
newtype Program = Program GLuint deriving (Eq, Show)

-- | Use a program.
{-# INLINE useProgram #-}
useProgram :: MonadIO m => Program -> m ()
useProgram (Program program) = glUseProgram program

-- | Compile and link a set of shaders.
{-# INLINABLE compileShaders #-}
compileShaders :: MonadUnliftIO m => [(ShaderType, B.ByteString)] -> m Program
compileShaders shaders = runResourceT $ do
  (programKey, program) <- allocate glCreateProgram glDeleteProgram
  traverse_ (loadShader program) shaders
  glLinkProgram program

  isLinked <- capture (glGetProgramiv program GL_LINK_STATUS)
  when (isLinked == 0) . liftIO $ do
    logLength <- capture (glGetProgramiv program GL_INFO_LOG_LENGTH)
    message   <- allocaBytes (fromIntegral logLength) $ \ptr -> do
      glGetProgramInfoLog program (fromIntegral logLength) nullPtr ptr
      B.packCString ptr
    throwIO (OpenGLError "Program linkage failed" message)

  void (unprotect programKey)
  pure (Program program)

 where
  loadShader program (shaderType, source) = do
    (_, shader) <- allocate (glCreateShader (toOpenGLEnum shaderType)) glDeleteShader
    liftIO . B.useAsCString source $ \pSource -> alloca $ \ppSource -> do
      poke ppSource pSource
      glShaderSource shader 1 ppSource nullPtr
    glCompileShader shader

    compilePassed <- capture (glGetShaderiv shader GL_COMPILE_STATUS)
    when (compilePassed == 0) . liftIO $ do
      logLength <- capture (glGetShaderiv shader GL_INFO_LOG_LENGTH)
      message   <- allocaBytes (fromIntegral logLength) $ \ptr -> do
        glGetShaderInfoLog shader (fromIntegral logLength) nullPtr ptr
        B.packCString ptr
      throwIO (OpenGLError "Shader compilation failed" message)

    void (allocate (shader <$ glAttachShader program shader) (glDetachShader program))
    pure shader

-- | Utility to deal with foreign functions that use pointers to return values.
capture :: (Storable a, MonadIO m) => (Ptr a -> IO ()) -> m a
capture action = liftIO . alloca $ \p -> do
  action p
  peek p
