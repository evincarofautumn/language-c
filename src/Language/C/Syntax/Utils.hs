module Language.C.Syntax.Utils (
  -- * Generic operations
  getSubStmts,
  mapSubStmts,
  mapBlockItemStmts,
  -- * Concrete operations
  getLabels
) where

import Data.List
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- XXX: This is should be generalized !!!
--      Data.Generics sounds attractive, but we really need to control the evaluation order
-- XXX: Expression statements (which are somewhat problematic anyway), aren't handled yet
getSubStmts :: CStat -> Vector CStat
getSubStmts (CLabel _ s _ _)      = Vector.singleton s
getSubStmts (CCase _ s _)         = Vector.singleton s
getSubStmts (CCases _ _ s _)      = Vector.singleton s
getSubStmts (CDefault s _)        = Vector.singleton s
getSubStmts (CExpr _ _)           = Vector.empty
getSubStmts (CCompound _ body _)  = Vector.concatMap compoundSubStmts body
getSubStmts (CIf _ sthen selse _) = maybe (Vector.singleton sthen) (\s -> Vector.fromList [sthen,s]) selse
getSubStmts (CSwitch _ s _)       = Vector.singleton s
getSubStmts (CWhile _ s _ _)      = Vector.singleton s
getSubStmts (CFor _ _ _ s _)      = Vector.singleton s
getSubStmts (CGoto _ _)           = Vector.empty
getSubStmts (CGotoPtr _ _)        = Vector.empty
getSubStmts (CCont _)             = Vector.empty
getSubStmts (CBreak _)            = Vector.empty
getSubStmts (CReturn _ _)         = Vector.empty
getSubStmts (CAsm _ _)            = Vector.empty

mapSubStmts :: (CStat -> Bool) -> (CStat -> CStat) -> CStat -> CStat
mapSubStmts stop _ s | stop s = s
mapSubStmts stop f (CLabel i s attrs ni) =
  f (CLabel i (mapSubStmts stop f s) attrs ni)
mapSubStmts stop f (CCase e s ni) =
  f (CCase e (mapSubStmts stop f s) ni)
mapSubStmts stop f (CCases e1 e2 s ni) =
  f (CCases e1 e2 (mapSubStmts stop f s) ni)
mapSubStmts stop f (CDefault s ni) =
  f (CDefault (mapSubStmts stop f s) ni)
mapSubStmts stop f (CCompound ls body ni) =
  f (CCompound ls (fmap (mapBlockItemStmts stop f) body) ni)
mapSubStmts stop f (CIf e sthen selse ni) =
  f (CIf e
     (mapSubStmts stop f sthen)
     (fmap (mapSubStmts stop f) selse)
     ni)
mapSubStmts stop f (CSwitch e s ni) =
  f (CSwitch e (mapSubStmts stop f s) ni)
mapSubStmts stop f (CWhile e s isdo ni) =
  f (CWhile e (mapSubStmts stop f s) isdo ni)
mapSubStmts stop f (CFor i t a s ni) =
  f (CFor i t a (mapSubStmts stop f s) ni)
mapSubStmts _ f s  = f s

mapBlockItemStmts :: (CStat -> Bool)
                  -> (CStat -> CStat)
                  -> CBlockItem
                  -> CBlockItem
mapBlockItemStmts stop f (CBlockStmt s) = CBlockStmt (mapSubStmts stop f s)
mapBlockItemStmts _ _ bi                = bi

compoundSubStmts :: CBlockItem -> Vector CStat
compoundSubStmts (CBlockStmt s)    = Vector.singleton s
compoundSubStmts (CBlockDecl _)    = Vector.empty
compoundSubStmts (CNestedFunDef _) = Vector.empty

getLabels :: CStat -> Vector Ident
getLabels (CLabel l s _ _)      = Vector.snoc (getLabels s) l
getLabels (CCompound ls body _) =
  Vector.filter (`Vector.notElem` ls)
    $ Vector.concatMap (Vector.concatMap getLabels . compoundSubStmts) body
getLabels stmt                  = Vector.concatMap getLabels (getSubStmts stmt)
