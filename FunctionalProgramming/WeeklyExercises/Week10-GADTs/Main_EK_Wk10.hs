-- Ernests Kuznecovs
-- 17332791

{-# LANGUAGE GADTs #-}
data Zero
data Succ n

data Red
data Black

data RedBlack a n c where
   RootRightChildRed ::
    a                         ->
    (RedBlack a n Black)      ->
    (RedBlack a (Succ n) Red) ->
    (RedBlack a (Succ n) Black)

   RootLeftChildRed ::
    a                         ->
    (RedBlack a (Succ n) Red) ->
    (RedBlack a n Black)      ->
    (RedBlack a (Succ n) Black)

   RootChildrenRed ::
    a                  ->
    (RedBlack a n Red) ->
    (RedBlack a n Red) ->
    (RedBlack a n Black)

   RootChildrenBlack    ::
    a                  ->
    (RedBlack a n Black) ->
    (RedBlack a n Black) ->
    (RedBlack a (Succ n) Black)

   Leaf  :: RedBlack a Zero Black
   Red   :: a -> (RedBlack a n Black) -> (RedBlack a n Black) -> (RedBlack a (Succ n) Red)

   -- Same rules for the root, found that couldnt assign types to each other
   -- e.g RootChildrenBlack :: BlackChildrenBlack

   BlackRightChildRed ::
    a                         ->
    (RedBlack a n Black)      ->
    (RedBlack a (Succ n) Red) ->
    (RedBlack a (Succ n) Black)

   BlackLeftChildRed ::
    a                         ->
    (RedBlack a (Succ n) Red) ->
    (RedBlack a n Black)      ->
    (RedBlack a (Succ n) Black)

   BlackChildrenRed ::
    a                  ->
    (RedBlack a n Red) ->
    (RedBlack a n Red) ->
    (RedBlack a n Black)

   BlackChildrenBlack ::
    a                  ->
    (RedBlack a n Black) ->
    (RedBlack a n Black) ->
    (RedBlack a (Succ n) Black)



-- Compilation Error
-- mr_RedBlack_tree = (RootLeftChildRed 2 (BlackChildrenBlack 4 Leaf Leaf) Leaf)

-- OK
mr_RedBlack_tree = (RootLeftChildRed 2 (Red 4 Leaf Leaf) Leaf)

-- Compilation Error
--mr_RedBlack_tree2 = (RootLeftChildRed 2 (Red 4 (Red 5 Leaf Leaf) Leaf) Leaf)

-- OK
mr_RedBlack_tree2 = (RootChildrenRed 2 (Red 4 Leaf Leaf) (Red 5 Leaf Leaf))
