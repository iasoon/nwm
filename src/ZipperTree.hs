module ZipperTree (
    Axis (..), Sign (..), Direction (..),
    Node (..), ZipperTree,
    empty, node, leaf,
    insert, delete
) where

import           Data.Foldable
import           Data.Maybe

data Axis = X | Y deriving (Show)

data Sign = Neg | Pos deriving (Show)

data Direction = Dir Axis Sign deriving (Show)

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

delete :: Eq a => a -> ZipperTree a -> ZipperTree a
delete _ (Zipper ctx Empty) = Zipper ctx $ Empty
delete e (Zipper ctx (Leaf l))
    | l == e    = Zipper ctx Empty
    | otherwise = Zipper ctx $ Leaf l
delete e (Zipper ctx (Fork a p l r)) =
    appendCtx ctx $ fork a p (delete e l) (delete e r)

empty :: ZipperTree a
empty = Zipper [] Empty

node :: ZipperTree a -> Node a
node (Zipper _ n) = n

leaf :: a -> ZipperTree a
leaf = pure . Leaf
