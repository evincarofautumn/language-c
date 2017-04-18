module Language.C.Analysis.Builtins (builtins) where

import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep
import Language.C.Analysis.TypeUtils
import Language.C.Data.Ident
import Language.C.Data.Node
import Data.Vector (Vector)
import qualified Data.Vector as Vector

builtins :: DefTable
builtins = foldr doIdent (foldr doTypeDef emptyDefTable typedefs) idents
  where doTypeDef d = snd . defineTypeDef (identOfTypeDef d) d
        doIdent   d = snd . defineGlobalIdent (declIdent d) d
        dName     s = VarName (builtinIdent s) Nothing
        param ty    = ParamDecl (VarDecl
                                 NoName
                                 (DeclAttrs noFunctionAttrs (Auto False) mempty)
                                 ty) undefNode
        fnAttrs     = DeclAttrs noFunctionAttrs (FunLinkage ExternalLinkage) mempty
        varAttrs    = DeclAttrs noFunctionAttrs (Static InternalLinkage False) mempty
        fnType r as = FunctionType (FunType r (fmap param as) False) noAttributes
        fnType' r as = FunctionType (FunType r (fmap param as) True) noAttributes
        func n r as = Declaration
                      (Decl
                       (VarDecl (dName n) fnAttrs (fnType r as))
                       undefNode)
        func' n r as = Declaration
                       (Decl
                        (VarDecl (dName n) fnAttrs (fnType' r as))
                        undefNode)
        var n t     = Declaration
                      (Decl (VarDecl (dName n) varAttrs t) undefNode)
        typedef n t = TypeDef (builtinIdent n) t mempty undefNode
        typedefs    = [ typedef "__builtin_va_list"
                                valistType
                      ]
        idents      = [ func "__builtin_expect"
                             (integral TyLong)
                             $ Vector.fromList [integral TyLong, integral TyLong]
                      , func "__builtin_bswap16"
                             uint16_tType
                             $ Vector.fromList [uint16_tType]
                      , func "__builtin_bswap32"
                             uint32_tType
                             $ Vector.fromList [uint32_tType]
                      , func "__builtin_bswap64"
                             uint64_tType
                             $ Vector.fromList [uint64_tType]
                      , func "__builtin_fabs"
                             (floating TyDouble)
                             $ Vector.fromList [floating TyDouble]
                      , func "__builtin_fabsf"
                             (floating TyFloat)
                             $ Vector.fromList [floating TyFloat]
                      , func "__builtin_fabsl"
                             (floating TyLDouble)
                             $ Vector.fromList [floating TyLDouble]
                      , func "__builtin_inf" (floating TyDouble) mempty
                      , func "__builtin_inff" (floating TyFloat) mempty
                      , func "__builtin_infl" (floating TyLDouble) mempty
                      , func "__builtin_huge_val" (floating TyDouble) mempty
                      , func "__builtin_huge_valf" (floating TyFloat) mempty
                      , func "__builtin_huge_vall" (floating TyLDouble) mempty
                      , func "__builtin_copysign"
                             (floating TyDouble)
                             $ Vector.fromList [ floating TyDouble, floating TyDouble ]
                      , func "__builtin_va_start"
                             voidType
                             $ Vector.fromList [ valistType , voidPtr ]
                      , func "__builtin_va_end"
                             voidType
                             $ Vector.fromList [valistType]
                      , func "__builtin_va_copy"
                             voidType
                             $ Vector.fromList [ valistType, valistType ]
                      , func "__builtin_va_arg_pack" (integral TyInt) mempty
                      , func "__builtin_va_arg_pack_len" (integral TyInt) mempty
                      , func "__builtin_alloca"
                             voidPtr
                             $ Vector.fromList [ size_tType ]
                      , func "__builtin_memcpy"
                             voidPtr
                             $ Vector.fromList [ voidPtr
                             , constVoidPtr
                             , size_tType
                             ]
                      , func "__builtin_strspn"
                             size_tType
                             $ Vector.fromList [ constCharPtr, constCharPtr ]
                      , func "__builtin_strcspn"
                             size_tType
                             $ Vector.fromList [ constCharPtr, constCharPtr ]
                      , func "__builtin_strchr"
                             charPtr
                             $ Vector.fromList [ constCharPtr, integral TyInt]
                      , func "__builtin_strncpy"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin_strncat"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin_strcmp"
                             (integral TyInt)
                             $ Vector.fromList
                             [ constCharPtr, constCharPtr ]
                      , func "__builtin_strpbrk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr, constCharPtr ]
                      , func "__builtin_bzero"
                             voidType
                             $ Vector.fromList
                             [ voidPtr, size_tType ]
                      , func "__builtin_clz"
                             (integral TyInt)
                             $ Vector.fromList
                             [ integral TyUInt ]
                      , func "__builtin_constant_p"
                             (integral TyInt)
                             $ Vector.fromList
                             [DirectType (TyBuiltin TyAny) noTypeQuals noAttributes]
                      -- XXX: I don't know if the following has the
                      -- correct type. It doesn't seem to be
                      -- documented.
                      , func "__builtin_extract_return_addr"
                             voidPtr
                             $ Vector.fromList
                             [ voidPtr ]
                      , func "__builtin_return_address"
                             voidPtr
                             $ Vector.fromList
                             [ integral TyUInt ]
                      , func "__builtin_frame_address"
                             voidPtr
                             $ Vector.fromList
                             [ integral TyUInt ]
                      , func "__builtin_expect"
                             (integral TyLong)
                             $ Vector.fromList
                             [ integral TyLong, integral TyLong ]
                      , func "__builtin_prefetch"
                             voidType
                             $ Vector.fromList
                             [ constVoidPtr ]
                      , var "__func__"
                            stringType
                      , var "__PRETTY_FUNCTION__"
                            stringType
                      , var "__FUNCTION__"
                            stringType
                      -- Builtin GCC error checking functions
                      , func "__builtin_object_size"
                             size_tType
                             $ Vector.fromList
                             [ voidPtr, integral TyInt ]
                      , func "__builtin___memcpy_chk"
                             voidPtr
                             $ Vector.fromList
                             [ voidPtr, constVoidPtr, size_tType, size_tType ]
                      , func "__builtin___mempcpy_chk"
                             voidPtr
                             $ Vector.fromList
                             [ voidPtr, constVoidPtr, size_tType, size_tType ]
                      , func "__builtin___memmove_chk"
                             voidPtr
                             $ Vector.fromList
                             [ voidPtr, constVoidPtr, size_tType, size_tType ]
                      , func "__builtin___memset_chk"
                             voidPtr
                             $ Vector.fromList
                             [ voidPtr, integral TyInt, size_tType, size_tType ]
                      , func "__builtin___strcpy_chk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin___stpcpy_chk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin___strncpy_chk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             , size_tType
                             ]
                      , func "__builtin___strcat_chk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             ]
                      , func "__builtin___strncat_chk"
                             charPtr
                             $ Vector.fromList
                             [ constCharPtr -- XXX: restrict
                             , constCharPtr -- XXX: restrict
                             , size_tType
                             , size_tType
                             ]
                      , func' "__builtin___sprintf_chk"
                             (integral TyInt)
                             $ Vector.fromList
                             [ charPtr
                             , integral TyInt
                             , size_tType
                             , constCharPtr
                             ]
                      , func' "__builtin___snprintf_chk"
                             (integral TyInt)
                             $ Vector.fromList
                             [ charPtr
                             , size_tType
                             , integral TyInt
                             , size_tType
                             , constCharPtr
                             ]
                      , func "__builtin___vsprintf_chk"
                             (integral TyInt)
                             $ Vector.fromList
                             [ charPtr
                             , integral TyInt
                             , size_tType
                             , constCharPtr
                             , valistType
                             ]
                      , func "__builtin___vsnprintf_chk"
                             (integral TyInt)
                             $ Vector.fromList
                             [ charPtr
                             , size_tType
                             , integral TyInt
                             , size_tType
                             , constCharPtr
                             , valistType
                             ]
                      ]
