module Node
    (
        TreeNode(..)
    ) where
    
data TreeNode t = Node { root:: t, left:: TreeNode t, right:: TreeNode t} | NodeNull deriving Show