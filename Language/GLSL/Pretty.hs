{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Use camelCase" #-}
module Language.GLSL.Pretty where

import Language.GLSL.Syntax
import Text.PrettyPrint.HughesPJClass
import Text.Printf
import Prelude hiding ((<>))

----------------------------------------------------------------------
-- helpers (TODO clean)
----------------------------------------------------------------------

type Assoc = (Rational -> Rational, Rational -> Rational)

assocLeft, assocRight, assocNone :: Assoc
assocLeft = (id, bump)
assocRight = (bump, id)
assocNone = (bump, bump)

bump :: Rational -> Rational
bump = (+ 0.5)

prettyBinary ::
  Pretty a =>
  PrettyLevel ->
  Rational ->
  Rational ->
  Assoc ->
  String ->
  a ->
  a ->
  Doc
prettyBinary l p op (lf, rf) o e1 e2 =
  prettyParen (p > op) $
    pPrintPrec l (lf op) e1 <+> text o <+> pPrintPrec l (rf op) e2

option :: Pretty a => Maybe a -> Doc
option Nothing = empty
option (Just x) = pPrint x

indexing :: Pretty a => Maybe (Maybe a) -> Doc
indexing Nothing = empty
indexing (Just Nothing) = brackets empty
indexing (Just (Just e)) = brackets $ pPrint e

indexing' :: Pretty a => Maybe (String, Maybe a) -> Doc
indexing' Nothing = empty
indexing' (Just (i, Nothing)) = text i
indexing' (Just (i, Just e)) = brackets (pPrint e) <> text i

initialize :: Pretty a => Maybe a -> Maybe FullType -> Doc --changed
-- e: FunctionCall FuncIdTypeSpect (TypeSpec .. Vec2) (Params ..)
initialize Nothing (Just t) = char ':' <+> char ' ' <> pPrint t
--
-- initialize (Just e) t = char ':' <> char ' ' <> pPrint t <> pPrint e
initialize (Just e) Nothing = char ' ' <> pPrint e
initialize (Just e) (Just t) = char ':' <> char ' ' <> pPrint t <> text " = " <> pPrint e
initialize Nothing Nothing = empty

-- initialize (Just e) = char ' ' <> equals <+> pPrint e

ident :: Pretty a => Maybe (String, Maybe (Maybe a)) -> Doc
ident Nothing = empty
ident (Just (i, Nothing)) = text i
ident (Just (i, Just Nothing)) = text i <> brackets empty
ident (Just (i, Just (Just e))) = text i <> brackets (pPrint e)

initialize' :: Pretty a => Maybe (String, Maybe a) -> Doc
initialize' Nothing = empty
initialize' (Just (i, Nothing)) = text i
initialize' (Just (i, Just e)) = text i <+> char '=' <+> pPrint e

isFloat :: TypeSpecifierNonArray -> Bool
isFloat Vec2 = True
isFloat Vec3 = True
isFloat Vec4 = True
isFloat BVec2 = True
isFloat BVec3 = True
isFloat BVec4 = True
isFloat IVec2 = True
isFloat IVec3 = True
isFloat IVec4 = True
isFloat UVec2 = True
isFloat UVec3 = True
isFloat UVec4 = True
isFloat Mat2 = True
isFloat Mat3 = True
isFloat Mat4 = True
isFloat Mat2x2 = True
isFloat Mat2x3 = True
isFloat Mat2x4 = True
isFloat Mat3x2 = True
isFloat Mat3x3 = True
isFloat Mat3x4 = True
isFloat Mat4x2 = True
isFloat Mat4x3 = True
isFloat Mat4x4 = True
isFloat _ = False

----------------------------------------------------------------------
-- Pretty instances
----------------------------------------------------------------------

instance Pretty TranslationUnit where --changed
  pPrint (TranslationUnit ds) = vcat $ map pPrint ds

--  pPrint (Alternative p e) = text "(" <> nest 2 (vcat [pPrint p, pPrint e]) <> text ")"

instance Pretty ExternalDeclaration where
  pPrint (FunctionDeclaration p) = pPrint p <> semi
  pPrint (FunctionDefinition p s) = vcat [pPrint p, pPrint s]
  pPrint (Declaration d) = pPrint d

----------------------------- Declaration -------------------------------------------------
getVal :: [InitDeclarator] -> InitDeclarator -> Doc
getVal _ dec@(InitDecl i Nothing Nothing (Just v)) = text "let" <+> pPrint dec <> semi
getVal decs dec = text "let" <+> pPrint (consDec (getLastVal decs) dec) <> semi

-- example: InitDecl "i" Nothing (Just (IntConstant Decimal 34)) (Just (FullType Nothing (..)))
-- recursively find the value, prioritizing the leftmost value
getLastVal :: [InitDeclarator] -> InitDeclarator
-- getLastVal [dec@(InitDecl _ _ (Just _) _)] = dec
getLastVal (dec@(InitDecl _ _ (Just _) _) : _) = dec
getLastVal ((InitDecl _ _ Nothing _) : ds') = getLastVal ds'
getLastVal _ = InitDecl "" Nothing Nothing Nothing

-- if variable has a value, keep it
consDec :: InitDeclarator -> InitDeclarator -> InitDeclarator
consDec _ ownDec@(InitDecl _ _ (Just _) _) = ownDec
-- if not keep the rightmost value (this is probably not how glsl works though. I couldn't find info on this)
consDec (InitDecl _ w ty p) (InitDecl s _ _ _) = InitDecl s w ty p

instance Pretty Declaration where --changed
-- struct
  pPrint (InitDeclaration typeDec []) = pPrint typeDec <> semi
  --
  -- typical let or var declaration without a value
  pPrint (InitDeclaration _ [d]) = text "let" <+> pPrint d <> semi
  --
  -- pPrint (InitDeclaration t ds) = hsep $ map (getVal (reverse ds)) ds

  --case of comma separated declarations: take the value assigned to the first var
  pPrint (InitDeclaration _ ds) = vcat $ map (getVal (reverse ds)) ds
  ----------------------------------- Declaration -------------------------------------------

  --

  -- pPrint (InitDeclaration it ds) = pPrint it <+> hsep (punctuate comma (map pPrint ds)) <> semi
  pPrint (Precision pq t) = text "precision" <+> pPrint pq <+> pPrint t <> semi
  pPrint (Block tq i ds n) =
    vcat
      [ text "struct"
          <+> text i,
        lbrace,
        char ' ',
        nest 2 (vcat $ map pPrint ds),
        rbrace,
        pPrint tq
          <+> ident n <> colon
          <+> text i <> semi
      ]
  pPrint (TQ tq) = pPrint tq <> semi

-- checks whether the glsl code has integers instead of floats: vec2(1,2) -> vec2(1.0,2.0)
instance Pretty InitDeclarator where --changed
-- i: name of variable
-- a: Nothing
-- b: value
-- t: type

  -- array

  -- no value, but a type declaration
  pPrint (InitDecl i a Nothing t) = text i <> colon <> initialize t (Nothing :: Maybe FullType) <> indexing a
  -- pPrint (InitDecl i Nothing Nothing
  --   tt@(Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing))))) = text i <>
  --     initialize tt (Nothing :: Maybe FullType)

  -- (convertToFloat t b) is just b normally, but we check if the type is a float and
  -- change the value(s) to floats if it is
  pPrint (InitDecl i a b t) = text i <> initialize (convertToFloat t b) t <> indexing a
    where
      --if the type is a float, but the number is an integer, convert to it to a float.
      --pattern match on simple Float type, and get the value
      convertToFloat :: Maybe FullType -> Maybe Expr -> Maybe Expr
      convertToFloat
        (Just (FullType _ (TypeSpec _ (TypeSpecNoPrecision Float _))))
        (Just (IntConstant Decimal value)) = Just (FloatConstant $ fromIntegral value)
      --
      -- match on types that are used by isFloat
      convertToFloat
        _
        ( Just
            ( FunctionCall
                (FuncIdTypeSpec (TypeSpec n (TypeSpecNoPrecision t' n')))
                (Params params)
              )
          )
          -- check if the type is a float
          | isFloat t' =
            Just
              ( FunctionCall
                  (FuncIdTypeSpec (TypeSpec n (TypeSpecNoPrecision t' n')))
                  (Params (map paramToFloat params))
              )
      -- do no convert into float if the type isn't a float
      convertToFloat _ x = x

      paramToFloat (IntConstant Decimal val) = FloatConstant $ fromIntegral val
      paramToFloat x = x

instance Pretty InvariantOrType where
  pPrint InvariantDeclarator = text "invariant"
  pPrint (TypeDeclarator ft) = pPrint ft

instance Pretty FullType where
  pPrint (FullType tq ts) = option tq <+> pPrint ts

instance Pretty TypeQualifier where
  pPrint (TypeQualSto sq) = pPrint sq
  pPrint (TypeQualLay lq sq) = pPrint lq <+> option sq
  pPrint (TypeQualInt iq sq) = pPrint iq <+> option sq
  pPrint (TypeQualInv iq sq) = pPrint iq <+> option sq
  pPrint (TypeQualInv3 iq iq' sq) = pPrint iq <+> pPrint iq' <+> pPrint sq

instance Pretty StorageQualifier where
  pPrint q = case q of
    Const -> text "const"
    Attribute -> text "attribute"
    Varying -> text "varying"
    CentroidVarying -> text "centroid varying"
    In -> text "in"
    Out -> text "out"
    CentroidIn -> text "centroid in"
    CentroidOut -> text "centroid out"
    Uniform -> text "var<uniform>"

instance Pretty LayoutQualifier where --changed
  pPrint (Layout is) = char '@' <> (hsep $ map pPrint is)

-- to_wgsl x = if x == "set" then ""
layoutToWgsl :: [Char] -> [Char]
layoutToWgsl "set" = "group"
layoutToWgsl i = i

instance Pretty LayoutQualifierId where --changed
  pPrint (LayoutQualId i Nothing) = text $ layoutToWgsl i
  pPrint (LayoutQualId i (Just e)) = text (layoutToWgsl i) <> char '(' <> pPrint e <> char ')'

instance Pretty InterpolationQualifier where
  pPrint q = case q of
    Smooth -> text "smooth"
    Flat -> text "flat"
    NoPerspective -> text "noperspective"

instance Pretty InvariantQualifier where
  pPrint Invariant = text "invariant"

instance Pretty TypeSpecifier where
  pPrint (TypeSpec (Just pq) t) = pPrint pq <+> pPrint t
  pPrint (TypeSpec Nothing t) = pPrint t

instance Pretty PrecisionQualifier where
  pPrint HighP = text "highp"
  pPrint MediumP = text "mediump"
  pPrint LowP = text "lowp"

instance Pretty TypeSpecifierNoPrecision where
  pPrint (TypeSpecNoPrecision t a) = pPrint t <> indexing a

instance Pretty TypeSpecifierNonArray where
  pPrint t = case t of
    Void -> text "void"
    Float -> text "f32"
    Int -> text "i32"
    UInt -> text "u32"
    Bool -> text "bool"
    Vec2 -> text "vec2<f32>"
    Vec3 -> text "vec3<f32>"
    Vec4 -> text "vec4<f32>"
    BVec2 -> text "vec2<bool>"
    BVec3 -> text "vec3<bool>"
    BVec4 -> text "vec4<bool>"
    IVec2 -> text "vec2<i32>"
    IVec3 -> text "vec3<i32>"
    IVec4 -> text "vec4<i32>"
    UVec2 -> text "vec2<u32>"
    UVec3 -> text "vec3<u32>"
    UVec4 -> text "vec4<u32>"
    Mat3 -> text "mat3x3<f32>"
    Mat2 -> text "mat2x2<f32>"
    Mat4 -> text "mat4x4<f32>"
    Mat2x2 -> text "mat2x2<f32>"
    Mat2x3 -> text "mat2x3<f32>"
    Mat2x4 -> text "mat2x4<f32>"
    Mat3x2 -> text "mat3x2<f32>"
    Mat3x3 -> text "mat3x3<f32>"
    Mat3x4 -> text "mat3x4<f32>"
    Mat4x2 -> text "mat4x2<f32>"
    Mat4x3 -> text "mat4x3<f32>"
    Mat4x4 -> text "mat4x4<f32>"
    Sampler1D -> text "sampler1D"
    Sampler2D -> text "sampler2D"
    Sampler3D -> text "sampler3D"
    SamplerCube -> text "samplerCube"
    Sampler1DShadow -> text "sampler1DShadow"
    Sampler2DShadow -> text "sampler2DShadow"
    SamplerCubeShadow -> text "samplerCubeShadow"
    Sampler1DArray -> text "sampler1DArray"
    Sampler2DArray -> text "sampler2DArray"
    Sampler1DArrayShadow -> text "sampler1DArrayShadow"
    Sampler2DArrayShadow -> text "sampler2DArrayShadow"
    ISampler1D -> text "isampler1D"
    ISampler2D -> text "isampler2D"
    ISampler3D -> text "isampler3D"
    ISamplerCube -> text "isamplerCube"
    ISampler1DArray -> text "isampler1DArray"
    ISampler2DArray -> text "isampler2DArray"
    USampler1D -> text "usampler1D"
    USampler2D -> text "usampler2D"
    USampler3D -> text "usampler3D"
    USamplerCube -> text "usamplerCube"
    USampler1DArray -> text "usampler1DArray"
    USampler2DArray -> text "usampler2DArray"
    Sampler2DRect -> text "sampler2DRect"
    Sampler2DRectShadow -> text "sampler2DRectShadow"
    ISampler2DRect -> text "isampler2DRect"
    USampler2DRect -> text "usampler2DRect"
    SamplerBuffer -> text "samplerBuffer"
    ISamplerBuffer -> text "isamplerBuffer"
    USamplerBuffer -> text "usamplerBuffer"
    Sampler2DMS -> text "sampler2DMS"
    ISampler2DMS -> text "isampler2DMS"
    USampler2DMS -> text "usampler2DMS"
    Sampler2DMSArray -> text "sampler2DMSArray"
    ISampler2DMSArray -> text "isampler2DMSArray"
    USampler2DMSArray -> text "usampler2DMSArray"
    StructSpecifier i ds ->
      vcat [text "struct" <+> i', lbrace, char ' ', nest 2 (vcat $ map pPrint ds), rbrace]
      where
        i' = case i of Nothing -> empty; Just n -> text n
    TypeName i -> text i

instance Pretty Field where --changed
{-
struct PatternBlock
{
  pattern: f32[100];
  arr: f32[];
}
-}
-- field with array with a fixed number of elements
  pPrint (Field tq s [StructDeclarator name (Just (Just (IntConstant Decimal num)))]) =
    option tq <+> text name <> colon <+> pPrint s <> brackets (pPrint num) <> semi
  -- field with array with no number in array
  pPrint (Field tq s [StructDeclarator name (Just Nothing)]) =
    option tq <+> text name <> colon <+> pPrint s <> brackets empty <> semi
  -- field with no array
  pPrint (Field tq s ds) =
    option tq <+> hsep (punctuate comma $ map pPrint ds) <> colon <+> pPrint s <> semi

instance Pretty StructDeclarator where
  pPrint (StructDeclarator i e) = ident (Just (i, e))

instance Pretty Expr where
  pPrintPrec l p e = case e of
    -- primaryExpression
    Variable v -> text v
    IntConstant Decimal i -> text (show i)
    IntConstant Hexadecimal i -> text (printf "0x%x" i)
    IntConstant Octal i -> text (printf "0%o" i)
    FloatConstant f -> text (show f)
    BoolConstant True -> text "true"
    BoolConstant False -> text "false"
    -- postfixExpression
    Bracket e1 e2 ->
      prettyParen (p > 16) $
        pPrintPrec l 16 e1 <> brackets (pPrint e2)
    FieldSelection e1 f ->
      prettyParen (p > 16) $
        pPrintPrec l 16 e1 <> char '.' <> text f
    MethodCall e1 i ps ->
      prettyParen (p > 16) $
        pPrintPrec l 16 e1 <> char '.' <> pPrint i <+> parens (pPrint ps)
    -- i: FuncIdTypeSpect (TypeSpec .. Vec2) ---> declaration type
    -- ps: (Params ..) ----> (0,1)
    -- changed
    FunctionCall i ps ->
      prettyParen (p > 16) $ -- changed
      -- pPrint i <> char ' ' <> equals <+> parens (pPrint ps)
      -- char ' ' <> equals <+> pPrint i <+> parens (pPrint ps)
        pPrint i <> parens (pPrint ps)
    -- parens (pPrint ps) <+>  pPrint i

    PostInc e1 ->
      prettyParen (p > 15) $
        pPrintPrec l 15 e1 <+> text "++"
    PostDec e1 ->
      prettyParen (p > 15) $
        pPrintPrec l 15 e1 <+> text "--"
    PreInc e1 ->
      prettyParen (p > 15) $
        text "++" <+> pPrintPrec l 15 e1
    PreDec e1 ->
      prettyParen (p > 15) $
        text "--" <+> pPrintPrec l 15 e1
    -- unary expression
    UnaryPlus e1 ->
      prettyParen (p > 15) $
        text "+" <> pPrintPrec l 15 e1
    UnaryNegate e1 ->
      prettyParen (p > 15) $
        text "-" <> pPrintPrec l 15 e1
    UnaryNot e1 ->
      prettyParen (p > 15) $
        text "!" <> pPrintPrec l 15 e1
    UnaryOneComplement e1 ->
      prettyParen (p > 15) $
        text "~" <> pPrintPrec l 15 e1
    -- binary expression
    Mul e1 e2 -> prettyBinary l p 14 assocLeft "*" e1 e2
    Div e1 e2 -> prettyBinary l p 14 assocLeft "/" e1 e2
    Mod e1 e2 -> prettyBinary l p 14 assocLeft "%" e1 e2
    Add e1 e2 -> prettyBinary l p 13 assocLeft "+" e1 e2
    Sub e1 e2 -> prettyBinary l p 13 assocLeft "-" e1 e2
    LeftShift e1 e2 -> prettyBinary l p 12 assocLeft "<<" e1 e2
    RightShift e1 e2 -> prettyBinary l p 12 assocLeft ">>" e1 e2
    Lt e1 e2 -> prettyBinary l p 11 assocLeft "<" e1 e2
    Gt e1 e2 -> prettyBinary l p 11 assocLeft ">" e1 e2
    Lte e1 e2 -> prettyBinary l p 11 assocLeft "<=" e1 e2
    Gte e1 e2 -> prettyBinary l p 11 assocLeft ">=" e1 e2
    Equ e1 e2 -> prettyBinary l p 10 assocLeft "==" e1 e2
    Neq e1 e2 -> prettyBinary l p 10 assocLeft "!=" e1 e2
    BitAnd e1 e2 -> prettyBinary l p 9 assocLeft "&" e1 e2
    BitXor e1 e2 -> prettyBinary l p 8 assocLeft "^" e1 e2
    BitOr e1 e2 -> prettyBinary l p 7 assocLeft "|" e1 e2
    And e1 e2 -> prettyBinary l p 6 assocLeft "&&" e1 e2
    -- TODO Xor 5 "^^"
    Or e1 e2 -> prettyBinary l p 4 assocLeft "||" e1 e2
    -- Selection e1 e2 e3 -> prettyParen (p > 3) $
    --   pPrintPrec l 3 e1 <+> char '?' <+> pPrintPrec l 3 e2
    --   <+> char ':' <+> pPrintPrec l 3 e3
    Selection e1 e2 e3 ->
      prettyParen (p > 3) $ -- changed
        text "if" <+> lparen <> pPrintPrec l 3 e1 <> rparen <+> lbrace <+> pPrintPrec l 3 e2
          <+> rbrace
          <+> text "else"
          <+> lbrace
          <+> pPrintPrec l 3 e3
          <+> rbrace
    -- assignment, the left Expr should be unary expression
    Equal e1 e2 -> prettyBinary l p 2 assocRight "=" e1 e2
    -- MulAssign e1 e2 -> prettyBinary l p 2 assocRight "*=" e1 e2
    MulAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Mul e1 e2]
    DivAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Div e1 e2]
    ModAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Mod e1 e2]
    AddAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Add e1 e2]
    SubAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Sub e1 e2]
    LeftAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ LeftShift e1 e2]
    RightAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ RightShift e1 e2]
    AndAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ And e1 e2]
    XorAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ BitXor e1 e2]
    OrAssign e1 e2 -> hsep $ [prettyBinary l p 2 assocRight "=" e1 (Variable ""), pPrint $ Or e1 e2]
    -- sequence
    Sequence e1 e2 ->
      prettyParen (p > 1) $
        pPrintPrec l 1 e1 <> char ',' <+> pPrintPrec l 1 e2

instance Pretty FunctionIdentifier where
  pPrint (FuncIdTypeSpec t) = pPrint t
  pPrint (FuncId i) = text i

instance Pretty Parameters where
  pPrint ParamVoid = empty
  pPrint (Params es) = hsep $ punctuate comma $ map pPrint es

instance Pretty FunctionPrototype where --changed
-- ps:  ParameterDeclaration N N (TypeSpec N (TypeSpecNoPrecision Float N)) (Just ("x", N))
  pPrint (FuncProt t i ps) =
    text "fn" <+> text i <+> char '('
      <> hsep (punctuate comma $ map pPrint ps)
      <> text ")" <+> voidf t
    where
      voidf (FullType _ (TypeSpec _ (TypeSpecNoPrecision Void _))) = text "" -- void
      voidf ft = text " -> " <+> pPrint ft -- if not void return type, print arrow and type

instance Pretty ParameterDeclaration where
  -- parameter with an array type
  pPrint (ParameterDeclaration tq q s (Just (i, Just e))) =
    option tq <+> option q <+> text i <> text ": " <+> pPrint s <> indexing (Just (Just e))
  -- non array parameter
  pPrint (ParameterDeclaration tq q s (Just (i, Nothing))) =
    option tq <+> option q <> text i <> text ":" <+> pPrint s

instance Pretty ParameterTypeQualifier where
  pPrint ConstParameter = text "const"

instance Pretty ParameterQualifier where
  pPrint InParameter = text "in"
  pPrint OutParameter = text "out"
  pPrint InOutParameter = text "inout"

instance Pretty Statement where
  pPrint s = case s of
    -- declaration statement
    DeclarationStatement d -> pPrint d
    -- jump statement
    Continue -> text "continue" <> semi
    Break -> text "break" <> semi
    Return e -> text "return" <+> option e <> semi
    Discard -> text "discard" <> semi
    -- compound statement
    CompoundStatement c -> pPrint c
    -- expression statement
    ExpressionStatement e -> option e <> semi
    -- selection statement
    -- SelectionStatement e s1 s2 -> vcat [text "if" <+> parens (pPrint e), nest 2 $  pPrint s1, text "else", option s2]
    -- with an else clause
    SelectionStatement e s1 (Just s2) ->
      vcat
        [ text "if" <+> parens (pPrint e) <+> lbrace,
          nest 2 $ pPrint s1,
          rbrace,
          text "else" <+> lbrace,
          nest 2 $ pPrint s2,
          rbrace
        ]
    -- only if clause without the else
    SelectionStatement e s1 s2 -> vcat [text "if" <+> parens (pPrint e) <+> lbrace, nest 2 $ pPrint s1, rbrace]
    -- switch statement
    SwitchStatement e s1 -> vcat [text "switch" <+> parens (pPrint e), lbrace, nest 2 $ vcat $ map pPrint s1, rbrace]
    CaseLabel l -> pPrint l
    -- iteration statement
    While c s1 -> vcat [text "while" <+> parens (pPrint c), pPrint s1]
    DoWhile s1 e -> vcat [text "do", pPrint s1, text "while" <+> parens (pPrint e)]
    For (Left e1) c e2 s1 -> vcat [text "for" <+> parens (option e1 <+> semi <+> option c <+> semi <+> option e2), pPrint s1]
    For (Right d) c e2 s1 -> vcat [text "for" <+> parens (pPrint d <+> semi <+> option c <+> semi <+> option e2), pPrint s1]

instance Pretty Compound where
  pPrint (Compound s) = vcat [lbrace, char ' ', nest 2 $ vcat $ map pPrint s, rbrace]

instance Pretty Condition where
  pPrint (Condition e) = pPrint e
  pPrint (InitializedCondition t i e) = pPrint t <+> pPrint i <+> pPrint e

instance Pretty CaseLabel where
  pPrint (Case e) = text "case" <+> pPrint e <> colon
  pPrint Default = text "default:"
