module Shape.Collections where

import Render.CNode
import Shape

nearest :: [CNode] -> Maybe CNode
nearest [] = Nothing
nearest (n : ns) = Just $ foldr (\n1@(CNode _ _ _ t1) n2@(CNode _ _ _ t2) -> if t1 < t2 then n1 else n2) n ns
