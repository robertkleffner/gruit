module Core where

import qualified Data.Map as Map



----------------------------------------------------------------------------
-- Names
----------------------------------------------------------------------------

newtype UpperName = UN String deriving (Eq,Ord)
newtype LowerName = LN String deriving (Eq,Ord)
newtype AuthorName = AN String deriving (Eq,Ord)

type Qualifier = [LowerName]

type PackageName = LowerName
type FullPackageName = (AuthorName, LowerName)
type QualifiedTypeName = (Qualifier, UpperName)
type QualifiedDefName = (Qualifier, LowerName)

----------------------------------------------------------------------------
-- Programs
----------------------------------------------------------------------------

type Program = Map.Map (AuthorName, PackageName) Package

----------------------------------------------------------------------------
-- Structure
----------------------------------------------------------------------------

-- Packages aren't nested
data Package
    = Package [Using] [Provides] [PackageDefinition]
      deriving Eq

data Using
    = Using FullPackageName SemanticVersion
    | UsingQual FullPackageName SemanticVersion PackageName
      deriving Eq

-- format: MAJOR.MINOR.PATCH
-- MAJOR: removes or changes existing API (breaks backward compatibility)
-- MINOR: adds to existing API (no breaking of backward compatibility, but could introduce name conflicts)
-- PATCH: no changes to API (no breaking backward compatibility, no possible introduction of name conflicts)
type SemanticVersion = (VersionNum, VersionNum, VersionNum)

data VersionNum
    = SpecVersion Int
    | AnyVersion
      deriving Eq

data Provides
    = ProvideType UpperName [UpperName]
    | ProvideTypeAll UpperName
    | ProvideVal LowerName
      deriving Eq

data PackageDefinition
    = OverloadDef LowerName Polytype
    | InstanceDef LowerName [Polytype] Expr
    | SignatureDef UpperName Signature
    | RecipeDef UpperName [Import] [Export] [WordDefinition]
    | CompoundDef UpperName [Import] [Export] [Linkage]
    | WordDef WordDefinition
      deriving Eq

data Linkage
    = Link UpperName [LinkImport] [LinkExport]
      deriving Eq

type LinkImport = LinkExternal
type LinkExport = LinkExternal

data LinkExternal
    = LinkVal LowerName LowerName
    | LinkType UpperName UpperName
      deriving Eq

type Import = Signature
type Export = Signature

data Signature
    = NamedSig UpperName
    | SigSet [External]
    | SigSum Signature Signature
    | SigDiff Signature Signature
      deriving Eq

data WordDefinition
    = TypeDef UpperName [TypeVar] [Constructor]
    | TypeAlias UpperName [TypeVar] Monotype
    | FunDef LowerName (Maybe Polytype) Expr
    | KegDef LowerName QualifiedTypeName [Supplied]
    | LawDef LowerName [LowerName] Expr
      deriving Eq

type Constructor = (UpperName,[Monotype])

data External
    = ExternalType UpperName [TypeVar]
    | ExternalVal LowerName
      deriving Eq

data Supplied
    = SupplyType UpperName QualifiedTypeName
    | SupplyVal LowerName QualifiedDefName
      deriving Eq

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data Kind
    = KStar
    | KSeq
    | KEffRow
    | KFieldRow
    | KFun Kind Kind
      deriving Eq

data TypeVar
    = IVar LowerName
    | EVar LowerName Kind
      deriving Eq

data Monotype
    = TVar TypeVar
    | TApp Monotype Monotype
    | TConstructor Qualifier UpperName
    | TSeq [Monotype]
    | TEffectRow [Effect] (Maybe TypeVar)
    | TFieldRow [Field] (Maybe TypeVar)
    | TDotted Monotype
    | TPrim TypePrimitive
      deriving Eq

data Polytype
    = Poly [TypeVar] Monotype
      deriving Eq

data Effect
    = EPartial
    | EDiverge
    | ENondet
    | EIO
      deriving Eq

type Field = (LowerName,Monotype)

data TypePrimitive
    = TI32 | TI64 | TU32 | TU64 | TF32 | TF64 | TBool
    | TFun | TList | TTuple | TRecord | TVariant
      deriving Eq

----------------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------------

type Expr = [Word]

data Word
    -- basic data
    = WBool Bool | WNat Integer | WReal Double
    | WRune Char | WString String
    -- lists
    | WListNil | WListCons
    -- tuples
    | WTupleNil | WTupleCons
    | WTupleLift | WTupleFoldLeft | WTupleFoldRight
    -- records
    | WRecordNil | WExtension LowerName | WSelection LowerName
    -- variants
    | WVariant LowerName | WDecompose LowerName
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

type FieldPattern = (LowerName, Pattern)

data Pattern
    = PBool Bool | PNat Integer | PReal Double
    | PRune Char | PString String
    | PList [Pattern] | PTuple [Pattern] (Maybe LowerName)
    | PRecord [FieldPattern] (Maybe LowerName)
    | PVariant LowerName Pattern
    | PIndWildcard | PSeqWildcard
    | PNamed LowerName Pattern
    | PConstructor Qualifier UpperName [Pattern]
      deriving Eq

-- list of eventual primitives
{-

error : a... String -> b...
add : a... Num Num -> a... Num
sub
mul
divmod
div

-}