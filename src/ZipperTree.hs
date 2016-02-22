module ZipperTree (
    Axis (..), Sign (..), Direction (..),
    Node (..), ZipperTree,
    empty, cursor, leaf,
    insert, delete,
    prune, push, tree, find, lastDir,
    zip, unzip,
    left, right, up, down, rotRight
) where

import           Data.Foldable (foldl')
import           Data.Maybe
import           Prelude       hiding (negate, unzip, zip)

data Axis = X | Y deriving (Show, Eq)

data Sign = Neg | Pos deriving (Show, Eq)

negate :: Sign -> Sign
negate Pos = Neg
negate Neg = Pos

data Direction = Dir Axis Sign deriving (Show, Eq)

opposite :: Direction -> Direction
opposite (Dir a s) = Dir a (negate s)

rotRight :: Direction -> Direction
rotRight (Dir X s) = Dir Y s
rotRight (Dir Y s) = Dir X (negate s)

left :: Direction
left = Dir X Neg

right :: Direction
right = Dir X Pos

up :: Direction
up = Dir Y Neg

down :: Direction
down = Dir Y Pos

data Ctx a = Ctx Direction Rational (ZipperTree a) deriving (Show)

type Context a = Zipper a (Ctx a)

data Zipper c a = Zipper [Context c] a deriving (Show)

instance Functor (Zipper c) where
    fmap f (Zipper ctx a) = Zipper ctx (f a)

instance Applicative (Zipper c) where
    pure a = Zipper [] a
    (<*>) (Zipper ctx f) = appendCtx ctx . fmap f

instance Monad (Zipper c) where
    (Zipper ctx a) >>= f = appendCtx ctx $ f a

appendCtx :: [Context c] -> Zipper c a -> Zipper c a
appendCtx cs (Zipper ctx t) = Zipper (ctx ++ cs) t

type ZipperTree a = Zipper a (Node a)

type Location a = Zipper a (ZipperTree a)

data Node a = Empty | Leaf a | Fork Axis Rational (ZipperTree a) (ZipperTree a)
    deriving (Show)

applyContext :: ZipperTree a -> Context a -> ZipperTree a
applyContext t ctx = ctx >>= \(Ctx d p t') -> insert d p t' t

ctxToTree :: [Context a] -> Maybe (ZipperTree a)
ctxToTree []     = Nothing
ctxToTree (c:cs) = Just $ foldl' applyContext (c >>= \(Ctx _ _ t) -> t) cs

asSibling :: Direction -> Rational -> [Context a] -> [Context a]
asSibling d p ctx = maybeToList $ pure . Ctx d p <$> ctxToTree ctx

fork :: Axis -> Rational -> ZipperTree a -> ZipperTree a -> ZipperTree a
fork a p (Zipper ctx Empty) t = appendCtx (asSibling (Dir a Neg) p ctx) t
fork a p t (Zipper ctx Empty) = appendCtx (asSibling (Dir a Pos) (1-p) ctx) t
fork a p l r                  = pure $ Fork a p l r

insert :: Direction -> Rational -> ZipperTree a -> ZipperTree a -> ZipperTree a
insert (Dir a Neg) p l r = fork a p l r
insert (Dir a Pos) p r l = fork a (1-p) l r

empty :: ZipperTree a
empty = Zipper [] Empty

cursor :: Zipper c a -> a
cursor (Zipper _ a) = a

leaf :: a -> ZipperTree a
leaf = pure . Leaf

child :: Sign -> ZipperTree a -> Maybe (Location a)
child s (Zipper ctx (Fork a p l r)) = Just $ case s of
    Neg -> Zipper [mkContext p r] l
    Pos -> Zipper [mkContext (1-p) l] r
    where mkContext part t = Zipper ctx $ Ctx (Dir a (negate s)) part t
child _ _ = Nothing

descend :: Sign -> Location a -> Maybe (Location a)
descend s = fmap (>>= id) . maybeZ . fmap (child s)
    where maybeZ (Zipper ctx m) = Zipper ctx <$> m

ascend :: Location a -> Maybe (Location a)
ascend (Zipper [] _)     = Nothing
ascend (Zipper (c:cs) t) = Just $ Zipper cs $ applyContext t c

tree :: Location a -> ZipperTree a
tree t = maybe (cursor t) (tree) $ ascend t

find :: Eq a => a -> ZipperTree a -> Maybe (Location a)
find a = listToMaybe . findAll a . pure

findAll :: Eq a => a -> Location a -> [Location a]
findAll a t
    | Leaf l <- cursor (cursor t), a == l = return t
    | otherwise = catMaybes [descend Pos t, descend Neg t] >>= findAll a

try :: (a -> Maybe a) -> a -> a
try f a = fromMaybe a (f a)

prune :: Location a -> Location a
prune l = try ascend $ fmap (const Empty) <$> l

delete :: Eq a => a -> ZipperTree a -> ZipperTree a
delete a = try (fmap (tree . prune) . find a)

sibling :: Location a -> Maybe (Location a)
sibling (Zipper ((Zipper ctx (Ctx d p t')):cs) t) =
    Just $ Zipper ((Zipper ctx (Ctx (opposite d) (1-p) t)):cs) t'
sibling _ = Nothing

lastDir :: Location a -> Maybe (Direction)
lastDir (Zipper ((Zipper _ (Ctx d _ _)):_) _) = Just $ opposite d
lastDir _                                     = Nothing

push :: Direction -> Location a -> Location a
push d = try (push' d)

push' :: Direction -> Location a -> Maybe (Location a)
push' d loc = lastDir loc >>= \d' -> case () of
    _ | d == d'          -> insertUp d t loc'
      | opposite d == d' -> Just $ insertDeep d t loc'
      | otherwise        -> Just $ insert d 0.5 t <$> loc'
    where t = pure $ cursor $ cursor $ loc
          loc' = prune loc

insertUp :: Direction -> ZipperTree a -> Location a -> Maybe (Location a)
insertUp d t loc = lastDir loc >>= \d' -> case () of
    _ | opposite d == d' -> fmap (insert (opposite d) 0.5 t) <$> sibling loc
      | otherwise        -> fmap (insert d 0.5 t) <$> ascend loc

insertDeep :: Direction -> ZipperTree a -> Location a -> Location a
insertDeep d t loc = case descend (negate s) loc of
    Just loc' -> insert (opposite d) 0.5 t <$> loc'
    Nothing   -> insert d 0.5 t <$> loc
    where (Dir _ s) = d

zip :: (a -> Bool) -> ZipperTree a -> ZipperTree a
zip cond t = t >>= zipNode cond

zipNode :: (a -> Bool) -> Node a -> ZipperTree a
zipNode _    Empty          = pure Empty
zipNode cond (Fork a p l r) = fork a p (zip cond l) (zip cond r)
zipNode cond (Leaf a)
    | cond a    = pure (Leaf a)
    | otherwise = Zipper [pure (Ctx left 1.0 (pure (Leaf a)))] Empty

unzip :: ZipperTree a -> ZipperTree a
unzip (Zipper cs n) = foldl' applyContext (pure $ unzipNode n) cs

unzipNode :: Node a -> Node a
unzipNode (Fork a p l r) = Fork a p (unzip l) (unzip r)
unzipNode n              = n
