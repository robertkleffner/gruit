module Core where

import qualified Data.Map as Map



----------------------------------------------------------------------------
-- Names
----------------------------------------------------------------------------

newtype UpperName = UN String deriving (Eq,Ord)
newtype LowerName = LN String deriving (Eq,Ord)

data FullTypeName = FTN Qualifier UpperName deriving (Eq,Ord)

type Qualifier = [UpperName]

type PackageName = LowerName

----------------------------------------------------------------------------
-- Packages
----------------------------------------------------------------------------

-- Based pretty heavily on Haskell's Backpack (POPL 2014)

type Program = [Package]

data Package
    = Package PackageName Thinning [Binding]
      deriving Eq

type Thinning = [Qualifier]

data Binding
    = Bind Qualifier Bindable
    | Include PackageName Thinning Renaming
      deriving Eq

type Renaming = [(Qualifier,Qualifier)]

data Bindable
    = BMod Module
    | BSig Signature
    | BEq Qualifier
      deriving Eq

----------------------------------------------------------------------------
-- Modules & Signatures
----------------------------------------------------------------------------

data Module
    = Module [Import] [Export] [Definition]
      deriving Eq

data Signature
    = Signature [Import] [Declaration]
      deriving Eq

data Import
    = Import Qualifier [ImportSpec]
    | ImportAs Qualifier Qualifier [ImportSpec]
      deriving Eq

data ImportSpec
    = ImpType UpperName
    | ImpConstructor UpperName [UpperName]
    | ImpClass UpperName [LowerName]
    | ImpTypeAll UpperName
    | ImpFunction LowerName
      deriving Eq

data Export
    = ExpType UpperName
    | ExpConstructor UpperName [UpperName]
    | ExpClass UpperName [LowerName]
    | ExpTypeAll UpperName
    | ExpFunction LowerName
      deriving Eq

----------------------------------------------------------------------------
-- Definitions & Declarations
----------------------------------------------------------------------------

data Declaration
    = ConstructorDecl UpperName [TypeVar] [(UpperName,[Type])]
    | AliasDecl UpperName [TypeVar] Type
    | ClassDecl UpperName [TypeVar] ConstraintSet [(LowerName,Type)]
    | FunctionDecl LowerName TypeExpr
      deriving Eq

data Definition
    = ConstructorDef UpperName [LowerName] [(UpperName,[Type])]
    | AliasDef UpperName [LowerName] Type
    | ClassDef UpperName [TypeVar] ConstraintSet [(LowerName,Type)]
    | InstanceDef UpperName [TypeExpr] ConstraintSet [(LowerName,Expr)]
    | FunctionDecl LowerName (Maybe Type) Expr
      deriving Eq

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data Kind
    = KStar
    | KSeq
    | KEff
    | KEffRow
    | KRec
    | KLabel
    | KRecRow
    | KFun Kind Kind
      deriving Eq

data TypeVar
    = TV LowerName Kind
      deriving Eq

data TypeExpr
    = TVar TypeVar
    | TLabel LowerName
    | TApp TypeExpr TypeExpr
    | TConstructor FullTypeName
      deriving Eq

-- Invariant: must be of kind *
newtype Type
    = Type TypeExpr
      deriving Eq

data Constraint
    = Constraint FullTypeName [TypeExpr]

data Constrained
    = Constrained [Constraint] Type

data Scheme
    = Scheme [TypeVar] Constrained

-- primitive type constructors
primConstructors = Map.fromList [
    -- effects
    (prim "Par", KEff), -- partial function
    (prim "Div", KEff), -- possibly diverges
    (prim "Ndet", KEff), -- nondeterministic
    (prim "IO", KEff), -- IO
    (prim "EmptyEff", KEffRow),
    (prim "AddEff", KFun KEff KEffRow)

    -- fields
    (prim "EmptyField", KRecRow),
    (prim "AddField", KFun KLabel (KFun KStar KRecRow)),

    -- sequences
    (prim "EmptySeq", KSeq),
    (prim "AddSeq", KFun KStar KSeq),

    -- basics
    (prim "I32", KStar),
    (prim "I64", KStar),
    (prim "U32", KStar),
    (prim "U64", KStar),
    (prim "F32", KStar),
    (prim "F64", KStar),
    (prim "Bool", KStar),

    -- builders
    (prim "->", KFun KEffRow (KFun KSeq (KFun KSeq KStar))),
    (prim "List", KFun KStar KStar),
    (prim "Tuple", KFun KSeq KStar),
    (prim "Record", KFun KRecRow KStar),
    (prim "Variant", KFun KRecRow KStar)
    ]
    where prim n = ([],n)

----------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------

type Expr = [Word]

data Word
    -- basic data
    = WBool Bool | WNat Integer | WReal Double
    | WRune Char | WString String
    -- lists
    | WListNil | WListCons | WListHead | WListTail
    -- tuples
    | WTupleNil | WTupleCons | WTupleHead | WTupleTail
    -- records
    | WRecordNil | WExtension LowerName
    | WRestriction LowerName | WSelection LowerName
    -- variants
    | WVariant LowerName | WEmbed LowerName | WDecompose LowerName
    -- functions
    | WCall | WBlock Expr
    -- variables
    | WBind [LowerName] Expr | WLet LowerName Expr Expr
    | WVar Qualifier LowerName
    -- others
    | WConstructor Qualifier UpperName
    | WMatch [Option]
    | WPrimitive PrimitiveWord
      deriving Eq

data Option
    = TOption [Pattern] Expr
      deriving Eq

data Pattern
    = PBool Bool | PNat Integer | PReal Double
    | PRune Char | PString String
    | PList [Pattern] | PTuple [Pattern]
    | PRecord [(LowerName, Pattern)] (Maybe LowerName)
    | PVariant LowerName Pattern
    | PIndWildcard | PSeqWildcard
    | PNamed LowerName Pattern
    | PConstructor Qualifier UpperName [Pattern]
      deriving Eq