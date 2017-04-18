-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Export
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  prototype
-- Portability :  ghc
--
-- /WARNING/ : This is just an implementation sketch and not very well tested.
--
-- Export 'SemRep' entities to 'AST' nodes.
-----------------------------------------------------------------------------
module Language.C.Analysis.Export (
exportDeclr,
exportType, exportTypeDecl, exportTypeSpec,
exportTypeDef,
exportCompType, exportCompTypeDecl, exportCompTypeRef,
exportEnumType, exportEnumTypeDecl, exportEnumTypeRef,
)
where
import Language.C.Data.Ident
import Language.C.Data.Name (nameId)
import Language.C.Data.Node
import Language.C.Syntax.AST
import Language.C.Analysis.SemRep
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Monoid ((<>))

-- |Export Declarator
--
--  Synopsis: @exportDeclr other_specs type attributes variable-name@
exportDeclr :: Vector CDeclSpec -> Type -> Attributes -> VarName -> (Vector CDeclSpec,CDeclr)
exportDeclr other_specs ty attrs name =
    (other_specs <> specs, CDeclr ident derived asmname (exportAttrs attrs) ni)
    where
    (specs,derived) = exportType ty
    (ident,asmname) = case name of (VarName vident asmname_opt) -> (Just vident, asmname_opt)
                                   _ -> (Nothing,Nothing)

exportTypeDecl :: Type -> CDecl
exportTypeDecl ty =
  CDecl declspecs declrs ni
  where
  (declspecs,derived) = exportType ty
  declrs | Vector.null derived = mempty
         | otherwise = Vector.singleton (Just $ CDeclr Nothing derived Nothing mempty ni,Nothing,Nothing)

exportTypeDef :: TypeDef -> CDecl
exportTypeDef (TypeDef ident ty attrs node_info) =
  CDecl (CStorageSpec (CTypedef ni) `Vector.cons` declspecs) (Vector.singleton declr) node_info
  where
  (declspecs,derived) = exportType ty
  declr = (Just $ CDeclr (Just ident) derived Nothing (exportAttrs attrs) ni, Nothing, Nothing)

-- |Export a type to syntax
exportType :: Type -> (Vector CDeclSpec, Vector CDerivedDeclr)
exportType ty = exportTy mempty ty
  where
    exportTy dd (PtrType ity tyquals attrs) =
        let ptr_declr = CPtrDeclr (exportTypeQualsAttrs tyquals attrs) ni
        in  exportTy (ptr_declr `Vector.cons` dd) ity
    exportTy dd (ArrayType ity array_sz tyquals attrs) =
        let arr_declr = CArrDeclr (exportTypeQualsAttrs tyquals attrs) (exportArraySize array_sz) ni
        in  exportTy (arr_declr `Vector.cons` dd) ity
    exportTy dd (FunctionType (FunType ity params variadic) attrs) =
        let fun_declr = CFunDeclr (Right (fmap exportParamDecl params,variadic)) (exportAttrs attrs) ni
        in  exportTy (fun_declr `Vector.cons` dd) ity
    exportTy dd (FunctionType (FunTypeIncomplete ity) attrs) =
        let fun_declr = CFunDeclr (Right (mempty,False)) (exportAttrs attrs) ni
        in  exportTy (fun_declr `Vector.cons` dd) ity
    exportTy dd (TypeDefType (TypeDefRef ty_ident _ node) quals attrs) =
        let declspecs = CTypeSpec (CTypeDef ty_ident node)
                        `Vector.cons` fmap CTypeQual (exportTypeQualsAttrs quals attrs)
        in (declspecs, Vector.reverse dd)
    exportTy dd (DirectType ity quals attrs) =
        let declspecs =    fmap CTypeQual (exportTypeQualsAttrs quals attrs)
                        <> fmap CTypeSpec (exportTypeSpec ity)
        in (declspecs, Vector.reverse dd)

exportTypeQuals :: TypeQuals -> Vector CTypeQual
exportTypeQuals quals = Vector.fromList
    $ mapMaybe select [(constant,CConstQual ni),(volatile,CVolatQual ni),(restrict,CRestrQual ni)]
    where
    select (predicate,tyqual) | predicate quals = Just tyqual
                              | otherwise       = Nothing

exportTypeQualsAttrs :: TypeQuals -> Attributes -> Vector CTypeQual
exportTypeQualsAttrs tyqs attrs = (exportTypeQuals tyqs <> fmap CAttrQual (exportAttrs attrs))

exportArraySize :: ArraySize -> CArrSize
exportArraySize (ArraySize static e) = CArrSize static e
exportArraySize (UnknownArraySize complete) = CNoArrSize complete

exportTypeSpec :: TypeName -> Vector CTypeSpec
exportTypeSpec tyname =
    case tyname of
        TyVoid -> Vector.singleton $ CVoidType ni
        TyIntegral ity -> exportIntType ity
        TyFloating fty -> exportFloatType fty
        TyComplex fty -> exportComplexType fty
        TyComp comp -> exportCompTypeDecl comp
        TyEnum enum -> exportEnumTypeDecl enum
        TyBuiltin TyVaList -> Vector.singleton $ CTypeDef (internalIdent "va_list") ni
        TyBuiltin TyAny -> Vector.singleton $ CTypeDef (internalIdent "__ty_any") ni

exportIntType :: IntType -> Vector CTypeSpec
exportIntType ty = Vector.fromList
    $ case ty of
      TyBool    -> [CBoolType ni]
      TyChar    -> [CCharType ni]
      TySChar   -> [CSignedType ni,CCharType ni]
      TyUChar   -> [CUnsigType ni,CCharType ni]
      TyShort   -> [CShortType ni]
      TyUShort  -> [CUnsigType ni, CShortType ni]
      TyInt     -> [CIntType ni]
      TyUInt    -> [CUnsigType ni, CIntType ni]
      TyInt128  -> [CInt128Type ni]
      TyUInt128 -> [CUnsigType ni, CInt128Type ni]
      TyLong    -> [CLongType ni]
      TyULong   -> [CUnsigType ni,CLongType ni]
      TyLLong   -> [CLongType ni, CLongType ni]
      TyULLong  -> [CUnsigType ni, CLongType ni, CLongType ni]

exportFloatType :: FloatType -> Vector CTypeSpec
exportFloatType ty = Vector.fromList
    $ case ty of
      TyFloat   -> [CFloatType ni]
      TyDouble  -> [CDoubleType ni]
      TyLDouble -> [CLongType ni, CDoubleType ni]

exportComplexType :: FloatType -> Vector CTypeSpec
exportComplexType ty = (CComplexType ni) `Vector.cons` exportFloatType ty

exportCompTypeDecl :: CompTypeRef -> Vector CTypeSpec
exportCompTypeDecl ty = Vector.fromList [CSUType (exportComp ty) ni]
    where
    exportComp (CompTypeRef sue_ref comp_tag _n) =
        CStruct (if comp_tag == StructTag then CStructTag else CUnionTag)
                (exportSUERef sue_ref) Nothing mempty ni

exportEnumTypeDecl :: EnumTypeRef -> Vector CTypeSpec
exportEnumTypeDecl ty = Vector.fromList [CEnumType (exportEnum ty) ni]
    where
    exportEnum (EnumTypeRef sue_ref _n) =
        CEnum (exportSUERef sue_ref) Nothing mempty ni

exportCompType :: CompType -> Vector CTypeSpec
exportCompType (CompType sue_ref comp_tag members attrs node_info) = Vector.fromList [CSUType comp ni]
    where
    comp = CStruct (if comp_tag == StructTag then CStructTag else CUnionTag)
                   (exportSUERef sue_ref)
                   (Just (fmap exportMemberDecl members))
                   (exportAttrs attrs)
                   node_info
exportCompTypeRef :: CompType -> Vector CTypeSpec
exportCompTypeRef (CompType sue_ref com_tag  _ _ node_info) = exportCompTypeDecl (CompTypeRef sue_ref com_tag node_info)

exportEnumType :: EnumType -> Vector CTypeSpec
exportEnumType (EnumType sue_ref enumerators attrs node_info) = Vector.fromList [CEnumType enum ni]
    where
    enum = CEnum (exportSUERef sue_ref)
                 (Just (fmap exportEnumerator enumerators))
                 (exportAttrs attrs)
                 node_info
    exportEnumerator (Enumerator ident val _ty _) = (ident,Just val)

exportEnumTypeRef :: EnumType -> Vector CTypeSpec
exportEnumTypeRef (EnumType sue_ref _ _ node_info) = exportEnumTypeDecl (EnumTypeRef sue_ref node_info)

-- XXX: relies on a the source program not having any $'s in it
exportSUERef :: SUERef -> Maybe Ident
exportSUERef (AnonymousRef name) = Just (internalIdent $ "$" ++ show (nameId name))
exportSUERef (NamedRef ident) = Just ident

exportMemberDecl :: MemberDecl -> CDecl
exportMemberDecl (AnonBitField ty expr node_info) =
    CDecl (fmap CTypeSpec $ exportTypeSpec $ fromDirectType ty) (Vector.fromList [(Nothing,Nothing,Just expr)]) node_info
exportMemberDecl (MemberDecl vardecl bitfieldsz node_info) =
    let (specs,declarator) = exportVarDecl vardecl
    in  CDecl specs (Vector.fromList [(Just declarator, Nothing, bitfieldsz)]) node_info
exportVarDecl :: VarDecl -> (Vector CDeclSpec,CDeclr)

-- NOTE: that there is an ambiguity between two possible places for __attributes__ s here
exportVarDecl (VarDecl name attrs ty) = exportDeclr (exportDeclAttrs attrs) ty mempty name
exportParamDecl :: ParamDecl -> CDecl
exportParamDecl paramdecl =
    let (specs,declr) = exportVarDecl (getVarDecl paramdecl)
    in CDecl specs (Vector.fromList [(Just declr, Nothing , Nothing) ]) (nodeInfo paramdecl)

exportDeclAttrs :: DeclAttrs -> Vector CDeclSpec
exportDeclAttrs (DeclAttrs fun_attrs storage attrs) =
       fmap CFunSpec (exportFunAttrs fun_attrs)
    <> fmap CStorageSpec (exportStorage storage)
    <> fmap (CTypeQual . CAttrQual) (exportAttrs attrs)

-- | export function attributes to C function specifiers
exportFunAttrs :: FunctionAttrs -> Vector CFunSpec
exportFunAttrs fattrs = Vector.fromList $ catMaybes [inlQual, noretQual]
  where
    inlQual = if isInline fattrs then Just (CInlineQual ni) else Nothing
    noretQual = if isNoreturn fattrs then Just (CNoreturnQual ni) else Nothing

-- | express storage in terms of storage specifiers.
--
-- This isn't always possible and depends on the context the identifier is declared.
-- Most importantly, if there is a /conflicting/ declaration in scope, export is impossible.
-- Furthermore, automatic storage is impossible in file scope.
-- If the storage can actually be specified, the export is correct.
exportStorage :: Storage -> Vector CStorageSpec
exportStorage NoStorage = mempty
exportStorage (Auto reg) = if reg then Vector.fromList [CRegister ni] else mempty
exportStorage (Static InternalLinkage thread_local) = threadLocal thread_local $ Vector.fromList [CStatic ni]
exportStorage (Static ExternalLinkage thread_local) = threadLocal thread_local $ Vector.fromList [CExtern ni]
exportStorage (Static NoLinkage _) = error "impossible storage: static without linkage"
exportStorage (FunLinkage InternalLinkage) = Vector.fromList [CStatic ni]
exportStorage (FunLinkage ExternalLinkage) = mempty
exportStorage (FunLinkage NoLinkage) = error "impossible storage: function without linkage"

threadLocal :: Bool -> Vector CStorageSpec -> Vector CStorageSpec
threadLocal False = id
threadLocal True = ((CThread ni) `Vector.cons`)

exportAttrs :: Functor f => f Attr -> f CAttr
exportAttrs = fmap exportAttr where
    exportAttr (Attr ident es n) = CAttr ident es n

fromDirectType :: Type -> TypeName
fromDirectType (DirectType ty _ _) = ty
fromDirectType (TypeDefType (TypeDefRef _ ty _) _ _) = fromDirectType ty
fromDirectType _ = error "fromDirectType"

ni :: NodeInfo
ni = undefNode
