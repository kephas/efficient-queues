module Data.Queue.PreEval where

import qualified Data.List                     as L
import           Data.Text
import           MonadVar
import           Protolude               hiding ( length
                                                , rotate
                                                )

data Queue a = Queue
  { _front    :: [a]
  , _back     :: [a]
  , _unevaled :: [a]
  }
  deriving Show


showQueue :: Show a => Queue a -> Text
showQueue (Queue front back unevaled) = do
  show front <> show back <> show unevaled


empty :: Queue a
empty = Queue [] [] []

length :: Queue a -> Int
length (Queue front back _) = (L.length front) + (L.length back)

insert :: Queue a -> a -> Queue a
insert (Queue front back unevaled) element =
  balance $ Queue front (element : back) unevaled

remove :: Queue a -> (a, Queue a)
remove (Queue (f : fs) back unevaled) = (f, balance $ Queue fs back unevaled)

balance :: Queue a -> Queue a
balance (Queue front back (u : us)) = Queue front back us
balance (Queue front back []) =
  let rotated = rotate front back [] in Queue rotated [] rotated


rotate :: [a] -> [a] -> [a] -> [a]
rotate []       (b : bs) acc = b : acc
rotate (f : fs) (b : bs) acc = f : (rotate fs bs $ b : acc)
