{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Data.Maybe
import Debug.Trace
import Tree

main = do
    let tree = foldl (flip addElem) BSTNull [3,4,0,8,2]
    putStrLn $ "Inorder: " ++ printInorder tree
    putStrLn $ "Preorder: " ++ printPreorder tree
    putStrLn $ "Postorder: " ++ printPostorder tree
