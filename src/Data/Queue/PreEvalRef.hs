{-# LANGUAGE TupleSections #-}

module Data.Queue.PreEvalRef where

import qualified Data.List                     as L
import           Data.Text               hiding ( empty )
import           MonadVar
import           Protolude               hiding ( empty
                                                , length
                                                , rotate
                                                )

data Queue v a = Queue
  { front    :: v [a]
  , back     :: v [a]
  , unevaled :: v [a]
  }

readF :: MonadRead m v => Queue v a -> m [a]
readF = read . front

readB :: MonadRead m v => Queue v a -> m [a]
readB = read . back

readU :: MonadRead m v => Queue v a -> m [a]
readU = read . unevaled


showQueue :: (MonadRead m v, Show a) => Queue v a -> m Text
showQueue queue = do
  f <- readF queue
  b <- readB queue
  u <- readU queue
  pure $ show f <> show b <> show u


empty :: MonadNew m v => m (Queue v a)
empty = Queue <$> new [] <*> new [] <*> new []

fromFold
  :: (MonadNew m v, MonadRead m v, MonadMutate_ m v, Foldable t)
  => t a
  -> m (Queue v a)
fromFold fold = do
  queue <- empty
  forM_ fold $ insert queue
  pure queue

length :: MonadRead m v => Queue v a -> m Int
length queue = do
  (+) <$> (L.length <$> readF queue) <*> (L.length <$> readB queue)


insert :: (MonadRead m v, MonadMutate_ m v) => Queue v a -> a -> m ()
insert queue element = do
  cons' element $ back queue
  balance queue

remove :: (MonadRead m v, MonadMutate_ m v) => Queue v a -> m (Maybe a)
remove queue = do
  f <- readF queue
  case f of
    []         -> pure Nothing

    result : _ -> do
      tail' $ front queue
      balance queue
      pure $ Just result

balance :: (MonadRead m v, MonadMutate_ m v) => Queue v a -> m ()
balance queue = do
  u <- readU queue
  case u of
    (_ : _) -> tail' $ unevaled queue

    []      -> do
      f <- readF queue
      b <- readB queue
      let rotated = rotate f b []
      write (front queue)    rotated
      write (back queue)     []
      write (unevaled queue) rotated



rotate :: [a] -> [a] -> [a] -> [a]
rotate []       (b : bs) acc = b : acc
rotate (f : fs) (b : bs) acc = f : (rotate fs bs $ b : acc)



cons' :: MonadMutate_ m v => a -> v [a] -> m ()
cons' elem var = mutate_ var (elem :)

tail' :: MonadMutate_ m v => v [a] -> m ()
tail' var = mutate_ var tailSafe
