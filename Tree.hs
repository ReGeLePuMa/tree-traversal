module Tree
    (
        BST(..), 
        getRoot, addElem, findElem, deleteTree, printInorder, printPreorder, printPostorder
    ) where
        
import Data.List
import Data.Maybe
import Debug.Trace
import Node

data BST t = BSTNod (TreeNode t) | BSTNull deriving Show


getRoot :: BST t -> Maybe t
getRoot BSTNull = Nothing
getRoot (BSTNod NodeNull) = Nothing
getRoot root@(BSTNod (Node elem _ _) ) = Just elem

addElem :: Ord t => t -> BST t -> BST t
addElem elem BSTNull = BSTNod (Node elem NodeNull NodeNull)
addElem elem (BSTNod NodeNull) = BSTNod (Node elem NodeNull NodeNull)
addElem elem (BSTNod root) = BSTNod (addNod elem root) where addNod elem NodeNull = Node elem NodeNull NodeNull
                                                             addNod elem radacina@(Node val left right)        
                                                                | elem == val = radacina
                                                                | elem < val = Node val (addNod elem left) right
                                                                | elem > val = Node val left (addNod elem right) 

findElem :: Ord t => t -> BST t -> Maybe t
findElem elem BSTNull = Nothing
findElem elem root@(BSTNod NodeNull) = Nothing
findElem elem root@(BSTNod (Node val left right))
    | elem == val = Just val
    | elem < val = findElem elem (BSTNod left)
    | elem > val = findElem elem (BSTNod right)

deleteTree :: p -> BST t
deleteTree tree = BSTNull

printInorder :: Show a => BST a -> [Char]
printInorder BSTNull = "[]"
printInorder (BSTNod NodeNull) = "[]"
printInorder (BSTNod root@(Node val left right)) = "[" ++ init (init (helper root)) ++ "]" where
                                                                                helper NodeNull = ""
                                                                                helper (Node val left right) = helper left ++ show val ++ ", " ++ helper right


printPreorder :: Show a => BST a -> [Char]
printPreorder BSTNull = "[]"
printPreorder (BSTNod NodeNull) = "[]"
printPreorder (BSTNod root@(Node val left right)) = "[" ++ init (init (helper root)) ++ "]" where
                                                                                helper NodeNull = ""
                                                                                helper (Node val left right) = show val ++ ", " ++ helper left ++ helper right

                                                                                
printPostorder :: Show a => BST a -> [Char]
printPostorder BSTNull = "[]"
printPostorder (BSTNod NodeNull) = "[]"
printPostorder (BSTNod root@(Node val left right)) = "[" ++ init (init (helper root)) ++ "]" where
                                                                                helper NodeNull = ""
                                                                                helper (Node val left right) = helper left ++ helper right ++ show val ++ ", "
