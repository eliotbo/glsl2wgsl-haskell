

[FunctionDefinition (
    FuncProt (
        FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))
    )
        "main" [
            ParameterDeclaration Nothing Nothing (
                TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) 
                (Just ("x",Nothing)),
            ParameterDeclaration Nothing Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing)) 
                (Just ("v",Nothing))]) (Compound [])
        ]


TranslationUnit [FunctionDefinition 
    (FuncProt (
        FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))) 
        "main" [
            ParameterDeclaration Nothing Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) 
                (Just ("x",Nothing)),
            ParameterDeclaration Nothing Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing)) 
                (Just ("v",Nothing))
        ]
    ) 
(Compound [
    DeclarationStatement (
        InitDeclaration (
            TypeDeclarator (
                FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))
            )
        ) 
        [
            InitDecl "uv" Nothing (Just (
                FunctionCall (
                    FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))
                ) 
                    
                (Params [IntConstant Decimal 0,IntConstant Decimal 0])
            )) 
                (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))))
        ])
])]

[FunctionDefinition (FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))) 
"main" [ParameterDeclaration Nothing Nothing (
    TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) (Just ("x",Nothing)),
    ParameterDeclaration Nothing Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing)) 
    (Just ("v",Nothing))]) 
(Compound [DeclarationStatement 
    (InitDeclaration (TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))))
        [InitDecl "dir" Nothing (Just (IntConstant Decimal 5)) 
            (Just (
                FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)))
            )
        ])])]



(Compound [DeclarationStatement (InitDeclaration 

(TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing)))) 

[InitDecl "uv" Nothing (
    Just (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))) 
        (Params [IntConstant Decimal 0,IntConstant Decimal 0]))
) 
    (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))))
])])]


 [InitDecl "uv" Nothing 
    (Just (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))) 
            (Params [IntConstant Decimal 0,IntConstant Decimal 0]))) 
    (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec2 Nothing))))
])])]


(Compound [DeclarationStatement (
    InitDeclaration (TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing)))) 
        [
            InitDecl "i" Nothing (Just (IntConstant Decimal 34)) (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing)))),
            InitDecl "j" Nothing (Just (IntConstant Decimal 42)) (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing))))
        ])])]


struct 

TranslationUnit 
[Declaration (
    InitDeclaration (
        TypeDeclarator (
            FullType Nothing (
                TypeSpec Nothing (
                    TypeSpecNoPrecision (StructSpecifier (Just "Light") [
                        Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)) [
                            StructDeclarator "eyePosOrDir" Nothing
                        ], 
                        Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Bool Nothing)) [
                            StructDeclarator "isDirectional" Nothing
                        ], 
                        Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)) [
                            StructDeclarator "intensity" Nothing
                        ], 
                        Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) [
                            StructDeclarator "attenuation" Nothing
                        ]
                    ]
                ) Nothing)))) 
        []
    )
]

TranslationUnit 
[Declaration (
    InitDeclaration (
        TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)))
    ) [
        InitDecl "frequencies" (Just (Just (IntConstant Decimal 3))) Nothing 
        (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))))
    ])]

TranslationUnit 
[Declaration (
    Block (
        TypeQualLay (Layout [LayoutQualId "std140" Nothing]) (Just Uniform)) 
            "PatternBlock" [
                    Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) 
                    [
                        StructDeclarator "pattern" (Just (Just (IntConstant Decimal 100)))
                    ],
                    Field Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) 
                    [
                        StructDeclarator "arr" (Just Nothing)
                    ]
            ] Nothing
    )
]

TranslationUnit [
    FunctionDefinition (
        FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing))) 
        "norm" 
        [
            ParameterDeclaration Nothing Nothing (TypeSpec Nothing 
                (TypeSpecNoPrecision Vec3 Nothing)) (Just ("po", Nothing))
        ]
    ) 
    (Compound [])]


TranslationUnit [
    FunctionDefinition (
        FuncProt 
            (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing))) 
            "norm" 
            [
                ParameterDeclaration Nothing Nothing 
                    (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)) 
                    (Just ("po",Just (IntConstant Decimal 10)))
            ]
    ) 
(Compound [])]


TranslationUnit [
    FunctionDefinition (
        FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing))) 
            "map" 
            [
                ParameterDeclaration Nothing Nothing 
                (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)) 
                (Just ("p",Nothing))
            ]
    ) 
(Compound [
    ExpressionStatement (Just (FunctionCall (FuncId "sin") (Params [Variable "p"])))])
]

TranslationUnit [FunctionDefinition (FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))) "f" []) 
(Compound [ExpressionStatement (Just (MulAssign (Variable "a") (IntConstant Decimal 1)))])]


[FunctionDefinition (FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))) "map" []) (Compound [For (Right (InitDeclaration (TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing)))) [InitDecl "i" Nothing (Just (IntConstant Decimal 0)) (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Int Nothing))))])) (Just (Condition (Lt (Variable "i") (IntConstant Decimal 120)))) (Just (PostInc (Variable "i"))) (CompoundStatement (Compound [ExpressionStatement (Just (Equal (Variable "a") (IntConstant Decimal 3)))]))])]


TranslationUnit 
[FunctionDefinition (
    FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Mat2 Nothing))) 
        "rot" 
        [
            ParameterDeclaration Nothing Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)) 
                (Just ("a", Nothing))
        ]
) 
(Compound [
    DeclarationStatement (
        InitDeclaration (TypeDeclarator (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)))) 
            [
                InitDecl "s" Nothing (Just (FunctionCall (FuncId "sin") (Params [Variable "a"]))) 
                    (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing)))),
                InitDecl "c" Nothing (Just (FunctionCall (FuncId "cos") (Params [Variable "a"]))) 
                    (Just (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Float Nothing))))
            ]), 
    Return (Just (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Mat2 Nothing))) 
        (Params [Variable "c", Variable "s", UnaryNegate (Variable "s"), Variable "c"])))
    ]
)]