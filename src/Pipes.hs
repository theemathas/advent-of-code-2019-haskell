-- The pipes library, for some reason, didn't export the constructors of the
-- Pipe type, which I needed. So I figured, I might as well reimplement part of
-- it as a learning experience.

module Pipes
  ( Pipe (..),
    yield,
    await,
    (>->),
    evalStateP,
    runStateP,
  )
where

import Relude

-- A monad transformer that consumes a stream of `i`s and produces a stream of
-- `o`s.
data Pipe i o m a
  = PipeIn (i -> Pipe i o m a)
  | PipeOut o (Pipe i o m a)
  | PipeM (m (Pipe i o m a))
  | PipePure a

instance Functor m => Functor (Pipe i o m) where
  fmap f (PipeIn inFunc) = PipeIn $ \inValue -> fmap f (inFunc inValue)
  fmap f (PipeOut outValue pipe) = PipeOut outValue (fmap f pipe)
  fmap f (PipeM m) = PipeM $ fmap f <$> m
  fmap f (PipePure x) = PipePure (f x)

instance Functor m => Applicative (Pipe i o m) where
  pure = PipePure
  (PipeIn inFunc) <*> pipe' = PipeIn $ \inValue ->
    let pipe = inFunc inValue in pipe <*> pipe'
  (PipeOut outValue pipe) <*> pipe' = PipeOut outValue (pipe <*> pipe')
  (PipeM m) <*> pipe' = PipeM (fmap (<*> pipe') m)
  (PipePure f) <*> pipe' = fmap f pipe'

instance Functor m => Monad (Pipe i o m) where
  (PipeIn inFunc) >>= pipeFunc = PipeIn $ \inValue ->
    let pipe = inFunc inValue in pipe >>= pipeFunc
  (PipeOut outValue pipe) >>= pipeFunc = PipeOut outValue (pipe >>= pipeFunc)
  (PipeM m) >>= pipeFunc = PipeM (fmap (>>= pipeFunc) m)
  (PipePure x) >>= pipeFunc = pipeFunc x

instance MonadTrans (Pipe i o) where
  lift m = PipeM (PipePure <$> m)

instance MonadState s m => MonadState s (Pipe i o m) where
  get = lift get
  put x = lift $ put x

instance Alternative m => Alternative (Pipe i o m) where
  empty = PipeM empty
  l <|> r = PipeM (pure l <|> pure r)

instance MonadPlus m => MonadPlus (Pipe i o m)

yield :: o -> Pipe i o m ()
yield outValue = PipeOut outValue (PipePure ())

await :: Pipe i o m i
await = PipeIn $ \inValue -> PipePure inValue

(>->) :: Functor m => Pipe a b m () -> Pipe b c m () -> Pipe a c m ()
pipe >-> pipe'@(PipeIn inFunc') = case pipe of
  PipeIn inFunc -> PipeIn $ \inValue -> inFunc inValue >-> pipe'
  PipeOut outValue innerPipe -> innerPipe >-> inFunc' outValue
  PipeM m -> PipeM $ fmap (>-> pipe') m
  PipePure () -> PipePure () -- right pipe attempt to consume while left pipe already finished producing.
pipe >-> PipeOut outValue pipe' = PipeOut outValue (pipe >-> pipe')
pipe >-> PipeM m = PipeM $ fmap (pipe >->) m
_ >-> PipePure () = PipePure ()

runStateP :: Monad m => s -> Pipe i o (StateT s m) a -> Pipe i o m (s, a)
runStateP initialState (PipeIn inFunc) = PipeIn $ \inValue -> runStateP initialState (inFunc inValue)
runStateP initialState (PipeOut outValue pipe) = PipeOut outValue (runStateP initialState pipe)
runStateP initialState (PipeM m) = do
  (pipe', secondState) <- lift $ runStateT m initialState
  runStateP secondState pipe'
runStateP initialState (PipePure x) = PipePure (initialState, x)

evalStateP :: Monad m => s -> Pipe i o (StateT s m) a -> Pipe i o m a
evalStateP initialState pipe = snd <$> runStateP initialState pipe