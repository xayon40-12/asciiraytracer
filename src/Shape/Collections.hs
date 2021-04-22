module Shape.Collections where

import Shape

nearest :: Maybe CNode -> Maybe CNode -> Maybe CNode
nearest Nothing Nothing = Nothing
nearest n@(Just _) Nothing = n
nearest Nothing n@(Just _) = n
nearest (Just n1@(CNode _ _ t1)) (Just n2@(CNode _ _ t2)) = Just $ if t1 < t2 then n1 else n2
