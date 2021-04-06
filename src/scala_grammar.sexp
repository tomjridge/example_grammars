((RULE
  (Literal
   (RHS
    (((OPTION (((LITERAL -)))) (TM integerLiteral))
     ((OPTION (((LITERAL -)))) (TM floatingPointLiteral))
     ((TM booleanLiteral)) ((TM characterLiteral)) ((TM stringLiteral))
     ((TM interpolatedString)) ((TM symbolLiteral)) ((LITERAL null))))))
 (RULE (QualId (RHS (((TM id) (REPETITION (((LITERAL .) (TM id)))))))))
 (RULE (Ids (RHS (((TM id) (REPETITION (((LITERAL ,) (TM id)))))))))
 (RULE
  (Path
   (RHS (((NT StableId)) ((OPTION (((TM id) (LITERAL .)))) (LITERAL this))))))
 (RULE
  (StableId
   (RHS
    (((TM id)) ((NT Path) (LITERAL .) (TM id))
     ((OPTION (((TM id) (LITERAL .)))) (LITERAL super)
      (OPTION (((NT ClassQualifier)))) (LITERAL .) (TM id))))))
 (RULE (ClassQualifier (RHS (((LITERAL [) (TM id) (LITERAL ]))))))
 (RULE
  (Type
   (RHS
    (((NT FunctionArgTypes) (LITERAL =>) (NT Type))
     ((NT InfixType) (OPTION (((NT ExistentialClause)))))))))
 (RULE
  (FunctionArgTypes
   (RHS
    (((NT InfixType))
     ((LITERAL "(")
      (OPTION (((NT ParamType) (REPETITION (((LITERAL ,) (NT ParamType)))))))
      (LITERAL ")"))))))
 (RULE
  (ExistentialClause
   (RHS
    (((LITERAL forSome) (LITERAL {) (NT ExistentialDcl)
      (REPETITION (((TM semi) (NT ExistentialDcl)))) (LITERAL }))))))
 (RULE
  (ExistentialDcl
   (RHS (((LITERAL type) (NT TypeDcl)) ((LITERAL val) (NT ValDcl))))))
 (RULE
  (InfixType
   (RHS
    (((NT CompoundType)
      (REPETITION (((TM id) (OPTION (((TM nl)))) (NT CompoundType)))))))))
 (RULE
  (CompoundType
   (RHS
    (((NT AnnotType) (REPETITION (((LITERAL with) (NT AnnotType))))
      (OPTION (((NT Refinement)))))
     ((NT Refinement))))))
 (RULE
  (AnnotType (RHS (((NT SimpleType) (REPETITION (((NT Annotation)))))))))
 (RULE
  (SimpleType
   (RHS
    (((NT SimpleType) (NT TypeArgs)) ((NT SimpleType) (LITERAL #) (TM id))
     ((NT StableId)) ((NT Path) (LITERAL .) (LITERAL type))
     ((LITERAL "(") (NT Types) (LITERAL ")"))))))
 (RULE (TypeArgs (RHS (((LITERAL [) (NT Types) (LITERAL ]))))))
 (RULE (Types (RHS (((NT Type) (REPETITION (((LITERAL ,) (NT Type)))))))))
 (RULE
  (Refinement
   (RHS
    (((OPTION (((TM nl)))) (LITERAL {) (NT RefineStat)
      (REPETITION (((TM semi) (NT RefineStat)))) (LITERAL }))))))
 (RULE (RefineStat (RHS (((NT Dcl)) ((LITERAL type) (NT TypeDef)) ()))))
 (RULE (TypePat (RHS (((NT Type))))))
 (RULE
  (Ascription
   (RHS
    (((LITERAL :) (NT InfixType))
     ((LITERAL :) (NT Annotation) (REPETITION (((NT Annotation)))))
     ((LITERAL :) (LITERAL _) (LITERAL *))))))
 (RULE
  (Expr
   (RHS
    (((BRACKET
       (((NT Bindings)) ((OPTION (((LITERAL implicit)))) (TM id))
        ((LITERAL _))))
      (LITERAL =>) (NT Expr))
     ((NT Expr1))))))
 (RULE
  (Expr1
   (RHS
    (((LITERAL if) (LITERAL "(") (NT Expr) (LITERAL ")")
      (REPETITION (((TM nl)))) (NT Expr)
      (OPTION (((OPTION (((TM semi)))) (LITERAL else) (NT Expr)))))
     ((LITERAL while) (LITERAL "(") (NT Expr) (LITERAL ")")
      (REPETITION (((TM nl)))) (NT Expr))
     ((LITERAL try) (NT Expr) (OPTION (((LITERAL catch) (NT Expr))))
      (OPTION (((LITERAL finally) (NT Expr)))))
     ((LITERAL do) (NT Expr) (OPTION (((TM semi)))) (LITERAL while)
      (LITERAL "(") (NT Expr) (LITERAL ")"))
     ((LITERAL for)
      (BRACKET
       (((LITERAL "(") (NT Enumerators) (LITERAL ")"))
        ((LITERAL {) (NT Enumerators) (LITERAL }))))
      (REPETITION (((TM nl)))) (OPTION (((LITERAL yield)))) (NT Expr))
     ((LITERAL throw) (NT Expr)) ((LITERAL return) (OPTION (((NT Expr)))))
     ((OPTION (((NT SimpleExpr) (LITERAL .)))) (TM id) (LITERAL =) (NT Expr))
     ((NT SimpleExpr1) (NT ArgumentExprs) (LITERAL =) (NT Expr))
     ((NT PostfixExpr)) ((NT PostfixExpr) (NT Ascription))
     ((NT PostfixExpr) (LITERAL match) (LITERAL {) (NT CaseClauses)
      (LITERAL }))))))
 (RULE
  (PostfixExpr
   (RHS (((NT InfixExpr) (OPTION (((TM id) (OPTION (((TM nl))))))))))))
 (RULE
  (InfixExpr
   (RHS
    (((NT PrefixExpr))
     ((NT InfixExpr) (TM id) (OPTION (((TM nl)))) (NT InfixExpr))))))
 (RULE
  (PrefixExpr
   (RHS
    (((OPTION (((LITERAL -)) ((LITERAL +)) ((LITERAL ~)) ((LITERAL !))))
      (NT SimpleExpr))))))
 (RULE
  (SimpleExpr
   (RHS
    (((LITERAL new) (BRACKET (((NT ClassTemplate)) ((NT TemplateBody)))))
     ((NT BlockExpr)) ((NT SimpleExpr1) (OPTION (((LITERAL _)))))))))
 (RULE
  (SimpleExpr1
   (RHS
    (((NT Literal)) ((NT Path)) ((LITERAL _))
     ((LITERAL "(") (OPTION (((NT Exprs)))) (LITERAL ")"))
     ((NT SimpleExpr) (LITERAL .) (TM id)) ((NT SimpleExpr) (NT TypeArgs))
     ((NT SimpleExpr1) (NT ArgumentExprs)) ((NT XmlExpr))))))
 (RULE (Exprs (RHS (((NT Expr) (REPETITION (((LITERAL ,) (NT Expr)))))))))
 (RULE
  (ArgumentExprs
   (RHS
    (((LITERAL "(") (OPTION (((NT Exprs)))) (LITERAL ")"))
     ((LITERAL "(") (OPTION (((NT Exprs) (LITERAL ,)))) (NT PostfixExpr)
      (LITERAL :) (LITERAL _) (LITERAL *) (LITERAL ")"))
     ((OPTION (((TM nl)))) (NT BlockExpr))))))
 (RULE
  (BlockExpr
   (RHS
    (((LITERAL {) (NT CaseClauses) (LITERAL }))
     ((LITERAL {) (NT Block) (LITERAL }))))))
 (RULE
  (Block
   (RHS
    (((NT BlockStat) (REPETITION (((TM semi) (NT BlockStat))))
      (OPTION (((NT ResultExpr)))))))))
 (RULE
  (BlockStat
   (RHS
    (((NT Import))
     ((REPETITION (((NT Annotation)))) (OPTION (((LITERAL implicit))))
      (OPTION (((LITERAL lazy)))) (NT Def))
     ((REPETITION (((NT Annotation)))) (REPETITION (((NT LocalModifier))))
      (NT TmplDef))
     ((NT Expr1)) ()))))
 (RULE
  (ResultExpr
   (RHS
    (((NT Expr1))
     ((BRACKET
       (((NT Bindings))
        ((BRACKET (((OPTION (((LITERAL implicit)))) (TM id)) ((LITERAL _))))
         (LITERAL :) (NT CompoundType))))
      (LITERAL =>) (NT Block))))))
 (RULE
  (Enumerators
   (RHS (((NT Generator) (REPETITION (((TM semi) (NT Generator)))))))))
 (RULE
  (Generator
   (RHS
    (((NT Pattern1) (LITERAL <-) (NT Expr)
      (REPETITION
       (((OPTION (((TM semi)))) (NT Guard))
        ((TM semi) (NT Pattern1) (LITERAL =) (NT Expr)))))))))
 (RULE
  (CaseClauses (RHS (((NT CaseClause) (REPETITION (((NT CaseClause)))))))))
 (RULE
  (CaseClause
   (RHS
    (((LITERAL case) (NT Pattern) (OPTION (((NT Guard)))) (LITERAL =>)
      (NT Block))))))
 (RULE (Guard (RHS (((LITERAL if) (NT PostfixExpr))))))
 (RULE
  (Pattern
   (RHS (((NT Pattern1) (REPETITION (((LITERAL |) (NT Pattern1)))))))))
 (RULE
  (Pattern1
   (RHS
    (((TM boundvarid) (LITERAL :) (NT TypePat))
     ((LITERAL _) (LITERAL :) (NT TypePat)) ((NT Pattern2))))))
 (RULE
  (Pattern2
   (RHS (((TM id) (OPTION (((LITERAL @) (NT Pattern3))))) ((NT Pattern3))))))
 (RULE
  (Pattern3
   (RHS
    (((NT SimplePattern))
     ((NT SimplePattern)
      (REPETITION (((TM id) (OPTION (((TM nl)))) (NT SimplePattern)))))))))
 (RULE
  (SimplePattern
   (RHS
    (((LITERAL _)) ((TM varid)) ((NT Literal)) ((NT StableId))
     ((NT StableId) (LITERAL "(") (OPTION (((NT Patterns)))) (LITERAL ")"))
     ((NT StableId) (LITERAL "(") (OPTION (((NT Patterns) (LITERAL ,))))
      (OPTION (((TM id) (LITERAL @)))) (LITERAL _) (LITERAL *) (LITERAL ")"))
     ((LITERAL "(") (OPTION (((NT Patterns)))) (LITERAL ")"))
     ((NT XmlPattern))))))
 (RULE
  (Patterns
   (RHS
    (((NT Pattern) (OPTION (((LITERAL ,) (NT Patterns)))))
     ((LITERAL _) (LITERAL *))))))
 (RULE
  (TypeParamClause
   (RHS
    (((LITERAL [) (NT VariantTypeParam)
      (REPETITION (((LITERAL ,) (NT VariantTypeParam)))) (LITERAL ]))))))
 (RULE
  (FunTypeParamClause
   (RHS
    (((LITERAL [) (NT TypeParam) (REPETITION (((LITERAL ,) (NT TypeParam))))
      (LITERAL ]))))))
 (RULE
  (VariantTypeParam
   (RHS
    (((REPETITION (((NT Annotation)))) (OPTION (((LITERAL +)) ((LITERAL -))))
      (NT TypeParam))))))
 (RULE
  (TypeParam
   (RHS
    (((BRACKET (((TM id)) ((LITERAL _)))) (OPTION (((NT TypeParamClause))))
      (OPTION (((LITERAL >:) (NT Type)))) (OPTION (((LITERAL <:) (NT Type))))
      (REPETITION (((LITERAL <%) (NT Type))))
      (REPETITION (((LITERAL :) (NT Type)))))))))
 (RULE
  (ParamClauses
   (RHS
    (((REPETITION (((NT ParamClause))))
      (OPTION
       (((OPTION (((TM nl)))) (LITERAL "(") (LITERAL implicit) (NT Params)
         (LITERAL ")")))))))))
 (RULE
  (ParamClause
   (RHS
    (((OPTION (((TM nl)))) (LITERAL "(") (OPTION (((NT Params))))
      (LITERAL ")"))))))
 (RULE (Params (RHS (((NT Param) (REPETITION (((LITERAL ,) (NT Param)))))))))
 (RULE
  (Param
   (RHS
    (((REPETITION (((NT Annotation)))) (TM id)
      (OPTION (((LITERAL :) (NT ParamType))))
      (OPTION (((LITERAL =) (NT Expr)))))))))
 (RULE
  (ParamType
   (RHS (((NT Type)) ((LITERAL =>) (NT Type)) ((NT Type) (LITERAL *))))))
 (RULE
  (ClassParamClauses
   (RHS
    (((REPETITION (((NT ClassParamClause))))
      (OPTION
       (((OPTION (((TM nl)))) (LITERAL "(") (LITERAL implicit)
         (NT ClassParams) (LITERAL ")")))))))))
 (RULE
  (ClassParamClause
   (RHS
    (((OPTION (((TM nl)))) (LITERAL "(") (OPTION (((NT ClassParams))))
      (LITERAL ")"))))))
 (RULE
  (ClassParams
   (RHS (((NT ClassParam) (REPETITION (((LITERAL ,) (NT ClassParam)))))))))
 (RULE
  (ClassParam
   (RHS
    (((REPETITION (((NT Annotation)))) (REPETITION (((NT Modifier))))
      (OPTION (((BRACKET (((LITERAL val)) ((LITERAL var))))))) (TM id)
      (LITERAL :) (NT ParamType) (OPTION (((LITERAL =) (NT Expr)))))))))
 (RULE
  (Bindings
   (RHS
    (((LITERAL "(") (NT Binding) (REPETITION (((LITERAL ,) (NT Binding))))
      (LITERAL ")"))))))
 (RULE
  (Binding
   (RHS
    (((BRACKET (((TM id)) ((LITERAL _)))) (OPTION (((LITERAL :) (NT Type)))))))))
 (RULE
  (Modifier
   (RHS (((NT LocalModifier)) ((NT AccessModifier)) ((LITERAL override))))))
 (RULE
  (LocalModifier
   (RHS
    (((LITERAL abstract)) ((LITERAL final)) ((LITERAL sealed))
     ((LITERAL implicit)) ((LITERAL lazy))))))
 (RULE
  (AccessModifier
   (RHS
    (((BRACKET (((LITERAL private)) ((LITERAL protected))))
      (OPTION (((NT AccessQualifier)))))))))
 (RULE
  (AccessQualifier
   (RHS (((LITERAL [) (BRACKET (((TM id)) ((LITERAL this)))) (LITERAL ]))))))
 (RULE
  (Annotation
   (RHS (((LITERAL @) (NT SimpleType) (REPETITION (((NT ArgumentExprs)))))))))
 (RULE
  (ConstrAnnotation (RHS (((LITERAL @) (NT SimpleType) (NT ArgumentExprs))))))
 (RULE
  (TemplateBody
   (RHS
    (((OPTION (((TM nl)))) (LITERAL {) (OPTION (((NT SelfType))))
      (NT TemplateStat) (REPETITION (((TM semi) (NT TemplateStat))))
      (LITERAL }))))))
 (RULE
  (TemplateStat
   (RHS
    (((NT Import))
     ((REPETITION (((NT Annotation) (OPTION (((TM nl)))))))
      (REPETITION (((NT Modifier)))) (NT Def))
     ((REPETITION (((NT Annotation) (OPTION (((TM nl)))))))
      (REPETITION (((NT Modifier)))) (NT Dcl))
     ((NT Expr)) ()))))
 (RULE
  (SelfType
   (RHS
    (((TM id) (OPTION (((LITERAL :) (NT Type)))) (LITERAL =>))
     ((LITERAL this) (LITERAL :) (NT Type) (LITERAL =>))))))
 (RULE
  (Import
   (RHS
    (((LITERAL import) (NT ImportExpr)
      (REPETITION (((LITERAL ,) (NT ImportExpr)))))))))
 (RULE
  (ImportExpr
   (RHS
    (((NT StableId) (LITERAL .)
      (BRACKET (((TM id)) ((LITERAL _)) ((NT ImportSelectors)))))))))
 (RULE
  (ImportSelectors
   (RHS
    (((LITERAL {) (REPETITION (((NT ImportSelector) (LITERAL ,))))
      (BRACKET (((NT ImportSelector)) ((LITERAL _)))) (LITERAL }))))))
 (RULE
  (ImportSelector
   (RHS
    (((TM id) (OPTION (((LITERAL =>) (TM id)) ((LITERAL =>) (LITERAL _)))))))))
 (RULE
  (Dcl
   (RHS
    (((LITERAL val) (NT ValDcl)) ((LITERAL var) (NT VarDcl))
     ((LITERAL def) (NT FunDcl))
     ((LITERAL type) (REPETITION (((TM nl)))) (NT TypeDcl))))))
 (RULE (ValDcl (RHS (((NT Ids) (LITERAL :) (NT Type))))))
 (RULE (VarDcl (RHS (((NT Ids) (LITERAL :) (NT Type))))))
 (RULE (FunDcl (RHS (((NT FunSig) (OPTION (((LITERAL :) (NT Type)))))))))
 (RULE
  (FunSig
   (RHS (((TM id) (OPTION (((NT FunTypeParamClause)))) (NT ParamClauses))))))
 (RULE
  (TypeDcl
   (RHS
    (((TM id) (OPTION (((NT TypeParamClause))))
      (OPTION (((LITERAL >:) (NT Type))))
      (OPTION (((LITERAL <:) (NT Type)))))))))
 (RULE
  (PatVarDef (RHS (((LITERAL val) (NT PatDef)) ((LITERAL var) (NT VarDef))))))
 (RULE
  (Def
   (RHS
    (((NT PatVarDef)) ((LITERAL def) (NT FunDef))
     ((LITERAL type) (REPETITION (((TM nl)))) (NT TypeDef)) ((NT TmplDef))))))
 (RULE
  (PatDef
   (RHS
    (((NT Pattern2) (REPETITION (((LITERAL ,) (NT Pattern2))))
      (OPTION (((LITERAL :) (NT Type)))) (LITERAL =) (NT Expr))))))
 (RULE
  (VarDef
   (RHS
    (((NT PatDef)) ((NT Ids) (LITERAL :) (NT Type) (LITERAL =) (LITERAL _))))))
 (RULE
  (FunDef
   (RHS
    (((NT FunSig) (OPTION (((LITERAL :) (NT Type)))) (LITERAL =) (NT Expr))
     ((NT FunSig) (OPTION (((TM nl)))) (LITERAL {) (NT Block) (LITERAL }))
     ((LITERAL this) (NT ParamClause) (NT ParamClauses)
      (BRACKET
       (((LITERAL =) (NT ConstrExpr))
        ((OPTION (((TM nl)))) (NT ConstrBlock)))))))))
 (RULE
  (TypeDef
   (RHS (((TM id) (OPTION (((NT TypeParamClause)))) (LITERAL =) (NT Type))))))
 (RULE
  (TmplDef
   (RHS
    (((OPTION (((LITERAL case)))) (LITERAL class) (NT ClassDef))
     ((OPTION (((LITERAL case)))) (LITERAL object) (NT ObjectDef))
     ((LITERAL trait) (NT TraitDef))))))
 (RULE
  (ClassDef
   (RHS
    (((TM id) (OPTION (((NT TypeParamClause))))
      (REPETITION (((NT ConstrAnnotation)))) (OPTION (((NT AccessModifier))))
      (NT ClassParamClauses) (NT ClassTemplateOpt))))))
 (RULE
  (TraitDef
   (RHS (((TM id) (OPTION (((NT TypeParamClause)))) (NT TraitTemplateOpt))))))
 (RULE (ObjectDef (RHS (((TM id) (NT ClassTemplateOpt))))))
 (RULE
  (ClassTemplateOpt
   (RHS
    (((LITERAL extends) (NT ClassTemplate))
     ((OPTION (((OPTION (((LITERAL extends)))) (NT TemplateBody)))))))))
 (RULE
  (TraitTemplateOpt
   (RHS
    (((LITERAL extends) (NT TraitTemplate))
     ((OPTION (((OPTION (((LITERAL extends)))) (NT TemplateBody)))))))))
 (RULE
  (ClassTemplate
   (RHS
    (((OPTION (((NT EarlyDefs)))) (NT ClassParents)
      (OPTION (((NT TemplateBody)))))))))
 (RULE
  (TraitTemplate
   (RHS
    (((OPTION (((NT EarlyDefs)))) (NT TraitParents)
      (OPTION (((NT TemplateBody)))))))))
 (RULE
  (ClassParents
   (RHS (((NT Constr) (REPETITION (((LITERAL with) (NT AnnotType)))))))))
 (RULE
  (TraitParents
   (RHS (((NT AnnotType) (REPETITION (((LITERAL with) (NT AnnotType)))))))))
 (RULE (Constr (RHS (((NT AnnotType) (REPETITION (((NT ArgumentExprs)))))))))
 (RULE
  (EarlyDefs
   (RHS
    (((LITERAL {)
      (OPTION (((NT EarlyDef) (REPETITION (((TM semi) (NT EarlyDef)))))))
      (LITERAL }) (LITERAL with))))))
 (RULE
  (EarlyDef
   (RHS
    (((REPETITION (((NT Annotation) (OPTION (((TM nl)))))))
      (REPETITION (((NT Modifier)))) (NT PatVarDef))))))
 (RULE (ConstrExpr (RHS (((NT SelfInvocation)) ((NT ConstrBlock))))))
 (RULE
  (ConstrBlock
   (RHS
    (((LITERAL {) (NT SelfInvocation)
      (REPETITION (((TM semi) (NT BlockStat)))) (LITERAL }))))))
 (RULE
  (SelfInvocation
   (RHS
    (((LITERAL this) (NT ArgumentExprs) (REPETITION (((NT ArgumentExprs)))))))))
 (RULE
  (TopStatSeq (RHS (((NT TopStat) (REPETITION (((TM semi) (NT TopStat)))))))))
 (RULE
  (TopStat
   (RHS
    (((REPETITION (((NT Annotation) (OPTION (((TM nl)))))))
      (REPETITION (((NT Modifier)))) (NT TmplDef))
     ((NT Import)) ((NT Packaging)) ((NT PackageObject)) ()))))
 (RULE
  (Packaging
   (RHS
    (((LITERAL package) (NT QualId) (OPTION (((TM nl)))) (LITERAL {)
      (NT TopStatSeq) (LITERAL }))))))
 (RULE
  (PackageObject (RHS (((LITERAL package) (LITERAL object) (NT ObjectDef))))))
 (RULE
  (CompilationUnit
   (RHS
    (((REPETITION (((LITERAL package) (NT QualId) (TM semi))))
      (NT TopStatSeq)))))))
