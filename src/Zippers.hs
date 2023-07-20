{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Zippers (
  freeTree,
  newFocus,
  newFocus2
) where
import           Data.Maybe (fromJust)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node 'P'
    (Node 'O'
      (Node 'L'
        (Node 'N' Empty Empty)
        (Node 'T' Empty Empty)
      )
      (Node 'Y'
        (Node 'S' Empty Empty)
        (Node 'A' Empty Empty)
      )
    )
    (Node 'L'
      (Node 'W'
        (Node 'C' Empty Empty)
        (Node 'R' Empty Empty)
      )
      (Node 'A'
        (Node 'A' Empty Empty)
        (Node 'C' Empty Empty)
      )
    )

-- Bad version, pattern matching the entire way.
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)
changeToP _ = undefined

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Better, way more general. However, we still have to navigate the entire path
-- each time we want to modify an element.
changeToP' :: Directions-> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r)     = Node 'P' l r
changeToP' _ _                 = undefined

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _)     = x
elemAt _ _                 = undefined

-- Directions can be seen as a focus, since it focuses on an exact subtree.

type Breadcrumbs' = [Direction]

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L : bs)
goLeft' _                = undefined

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R : bs)
goRight' _                = undefined

subTreeEx1 :: (Tree Char, Breadcrumbs')
subTreeEx1 = goLeft' $ goRight' (freeTree, [])

(-:) :: a -> (a -> b) -> b
x -: f = f x

subTreeEx2 :: (Tree Char, Breadcrumbs')
subTreeEx2 = (freeTree, []) -: goRight' -: goLeft'

-----------------------------------
-- Tree Focus

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- This data structure contains the content of the parent node, as well as the subtree that we
-- did not choose.

type Breadcrumbs a = [Crumb a]

goLeft :: Focus a -> Maybe (Focus a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _)       = Nothing

goRight :: Focus a -> Maybe (Focus a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _)       = Nothing

goUp :: Focus a -> Maybe (Focus a)
goUp (subTree, b : bs) = Just (tree, bs)
  where
    tree = case b of
      LeftCrumb x r  -> Node x subTree r
      RightCrumb x l -> Node x l subTree
goUp (_, []) = Nothing

type Focus a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Focus a -> Focus a
modify _ (Empty, bs)      = (Empty, bs)
modify f (Node x l r, bs) = (Node (f x) l r, bs)

newFocus :: Maybe (Focus Char)
newFocus = Just (freeTree, []) >>= goLeft >>= goRight

newFocus2 :: Maybe (Focus Char)
newFocus2 = newFocus >>= goUp

attach :: Tree a -> Focus a -> Focus a
attach t (_, bs) = (t, bs)

topMost :: Focus a -> Focus a
topMost (t, []) = (t, [])
topMost focus   = topMost $ fromJust $ goUp focus

-------------------------------------------------
-- List Focus

type ListFocus a = ([a], [a])

goForward :: ListFocus a -> Maybe (ListFocus a)
goForward (x : xs, bs) = Just (xs, x : bs)
goForward ([], _)      = Nothing

goBack :: ListFocus a -> Maybe (ListFocus a)
goBack (xs, b : bs) = Just (b : xs, bs)
goBack (_, [])      = Nothing

--------------------------------------------------
-- A very simple file system

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
      [ File "ape_throwing_up.jpg" "bleargh"
      , File "watermelon_smash.gif" "smash!!"
      , File "skull_man(scary).bmp" "Yikes!"
      ]
    , File "dijon_poupon.doc" "best mustard"
    , Folder "programs"
      [ File "fartwizard.exe" "10gotofart"
      , File "owl_bandit.dmg" "mov eax, h00t"
      , File "not_a_virus.exe" "really not a virus"
      , Folder "source code"
        [ File "best_hs_prog.hs" "main = print (fix error)"
        , File "random.hs" "main = print 4"
        ]
      ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSFocus = (FSItem, [FSCrumb])

fsUp :: FSFocus -> Maybe FSFocus
fsUp (fsi, FSCrumb name ls rs : bs) = Just (Folder name (ls ++ fsi : rs), bs)
fsUp (_, [])                        = Nothing

fsTo :: Name -> FSFocus -> Maybe FSFocus
fsTo name (Folder fName items, bs) = case break (nameIs name) items of
  (_, [])         -> Nothing
  (ls, item : rs) -> Just (item, FSCrumb fName ls rs : bs)
fsTo _ (File _ _, _) = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs name (File fName _)   = name == fName
nameIs name (Folder fName _) = name == fName

fsRename :: Name -> FSFocus -> FSFocus
fsRename name (File _ content, bs) = (File name content, bs)
fsRename name (Folder _ items, bs) = (Folder name items, bs)

fsNewFile :: FSItem -> FSFocus -> Maybe FSFocus
fsNewFile item (Folder name items, bs) = Just (Folder name (item : items), bs)
fsNewFile _ (File _ _, _)              = Nothing
