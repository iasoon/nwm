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

data Context a = Context [Context a] Direction Rational (ZipperTree a)
    deriving (Show)

data ZipperTree a = Zipper [Context a] (Node a)
    deriving (Show)

data Node a = Empty | Leaf a | Fork Axis Rational (ZipperTree a) (ZipperTree a)
            deriving (Show)

appendCtx :: [Context a] -> ZipperTree a -> ZipperTree a
appendCtx cs (Zipper ctx t) = Zipper (ctx ++ cs) t

ctxToTree :: [Context a] -> Maybe (ZipperTree a)
ctxToTree []                       = Nothing
ctxToTree ((Context ctx _ _ t):cs) = Just $ foldl' (flip applyCtx) bottom cs
    where bottom = appendCtx ctx t

asSibling :: Direction -> Rational -> [Context a] -> [Context a]
asSibling d p ctx = maybeToList $ Context [] d p <$> ctxToTree ctx

fork :: Axis -> Rational -> ZipperTree a -> ZipperTree a -> ZipperTree a
fork a p (Zipper ctx Empty) t = appendCtx (asSibling (Dir a Neg) p ctx) t
fork a p t (Zipper ctx Empty) = appendCtx (asSibling (Dir a Pos) (1-p) ctx) t
fork a p l r                  = Zipper [] $ Fork a p l r

insert :: Direction -> Rational -> ZipperTree a -> ZipperTree a -> ZipperTree a
insert (Dir a Neg) p l r = fork a p l r
insert (Dir a Pos) p r l = fork a (1-p) l r

applyCtx :: Context a -> ZipperTree a -> ZipperTree a
applyCtx (Context ctx d p t') t = appendCtx ctx $ insert d p t' t

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
leaf a = Zipper [] (Leaf a)
