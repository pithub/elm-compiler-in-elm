{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Canonicalize exposing
  ( Error(..)
  , BadArityContext(..)
  , InvalidPayload(..)
  , PortProblem(..)
  , DuplicatePatternContext(..)
  , PossibleNames(..)
  , VarKind(..)
  , toReport
  )


import Compiler.AST.Source as Src
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (Doc, d, da)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set



-- CANONICALIZATION ERRORS


type Error
  = AnnotationTooShort A.Region Name.Name Index.ZeroBased Int
  | AmbiguousVar A.Region (Maybe Name.Name) Name.Name ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)
  | AmbiguousType A.Region (Maybe Name.Name) Name.Name ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)
  | AmbiguousVariant A.Region (Maybe Name.Name) Name.Name ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)
  | AmbiguousBinop A.Region Name.Name ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)
  | BadArity A.Region BadArityContext Name.Name Int Int
  | Binop A.Region Name.Name Name.Name
  | DuplicateDecl Name.Name A.Region A.Region
  | DuplicateType Name.Name A.Region A.Region
  | DuplicateCtor Name.Name A.Region A.Region
  | DuplicateBinop Name.Name A.Region A.Region
  | DuplicateField Name.Name A.Region A.Region
  | DuplicateAliasArg Name.Name Name.Name A.Region A.Region
  | DuplicateUnionArg Name.Name Name.Name A.Region A.Region
  | DuplicatePattern DuplicatePatternContext Name.Name A.Region A.Region
  | EffectNotFound A.Region Name.Name
  | EffectFunctionNotFound A.Region Name.Name
  | ExportDuplicate Name.Name A.Region A.Region
  | ExportNotFound A.Region VarKind Name.Name (TList Name.Name)
  | ExportOpenAlias A.Region Name.Name
  | ImportCtorByName A.Region Name.Name Name.Name
  | ImportOpenAlias A.Region Name.Name
  | ImportExposingNotFound A.Region ModuleName.Canonical Name.Name (TList Name.Name)
  | NotFoundVar A.Region (Maybe Name.Name) Name.Name PossibleNames
  | NotFoundType A.Region (Maybe Name.Name) Name.Name PossibleNames
  | NotFoundVariant A.Region (Maybe Name.Name) Name.Name PossibleNames
  | NotFoundBinop A.Region Name.Name (Set.Set Name.Name)
  | PatternHasRecordCtor A.Region Name.Name
  | PortPayloadInvalid A.Region Name.Name {-Can.Type-} InvalidPayload
  | PortTypeInvalid A.Region Name.Name PortProblem
  | RecursiveAlias A.Region Name.Name (TList Name.Name) Src.Type (TList Name.Name)
  | RecursiveDecl A.Region Name.Name (TList Name.Name)
  | RecursiveLet (A.Located Name.Name) (TList Name.Name)
  | Shadowing Name.Name A.Region A.Region
  | TupleLargerThanThree A.Region
  | TypeVarsUnboundInUnion A.Region Name.Name (TList Name.Name) (Name.Name, A.Region) (TList (Name.Name, A.Region))
  | TypeVarsMessedUpInAlias A.Region Name.Name (TList Name.Name) (TList (Name.Name, A.Region)) (TList (Name.Name, A.Region))


type BadArityContext
  = TypeArity
  | PatternArity


type DuplicatePatternContext
  = DPLambdaArgs
  | DPFuncArgs Name.Name
  | DPCaseBranch
  | DPLetBinding
  | DPDestruct


type InvalidPayload
  = ExtendedRecord
  | Function
  | TypeVariable Name.Name
  | UnsupportedType Name.Name


type PortProblem
  = CmdNoArg
  | CmdExtraArgs Int
  | CmdBadMsg
  | SubBad
  | NotCmdOrSub


type PossibleNames =
  PossibleNames
    {- locals -} (Set.Set Name.Name)
    {- quals -} (Map.Map Name.Name (Set.Set Name.Name))



-- KIND


type VarKind
  = BadOp
  | BadVar
  | BadPattern
  | BadType


toKindInfo : VarKind -> Name.Name -> ( Doc, Doc, Doc )
toKindInfo kind name =
  case kind of
    BadOp ->
      ( d"an", d"operator", da[d"(" , D.fromName name , d")"] )

    BadVar ->
      ( d"a", d"value", da[d"`" , D.fromName name , d"`"] )

    BadPattern ->
      ( d"a", d"pattern", da[d"`" , D.fromName name , d"`"] )

    BadType ->
      ( d"a", d"type", da[d"`" , D.fromName name , d"`"] )



-- TO REPORT


toReport : Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    AnnotationTooShort region name index leftovers ->
      let
        numTypeArgs = Index.toMachine index
        numDefArgs = numTypeArgs + leftovers
      in
      Report.Report "BAD TYPE ANNOTATION" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "The type annotation for `" ++ name ++ "` says it can accept "
              ++ D.args numTypeArgs ++ ", but the definition says it has "
              ++ D.args numDefArgs ++ ":"
          ,
            D.reflow <|
              "Is the type annotation missing something? Should some argument"
              ++ (if leftovers == 1 then "" else "s")
              ++ " be deleted? Maybe some parentheses are missing?"
          )

    AmbiguousVar region maybePrefix name h hs ->
      ambiguousName source region maybePrefix name h hs "variable"

    AmbiguousType region maybePrefix name h hs ->
      ambiguousName source region maybePrefix name h hs "type"

    AmbiguousVariant region maybePrefix name h hs ->
      ambiguousName source region maybePrefix name h hs "variant"

    AmbiguousBinop region name h hs ->
      ambiguousName source region Nothing name h hs "operator"

    BadArity region badArityContext name expected actual ->
      let
        thing =
          case badArityContext of
            TypeArity    -> "type"
            PatternArity -> "variant"
      in
      if actual < expected then
        Report.Report "TOO FEW ARGS" region {-[]-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "The `" ++ name ++ "` " ++ thing ++ " needs "
                ++ D.args expected ++ ", but I see " ++ String.fromInt actual ++ " instead:"
            ,
              D.reflow <|
                "What is missing? Are some parentheses misplaced?"
            )

      else
        Report.Report "TOO MANY ARGS" region {-[]-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "The `" ++ name ++ "` " ++ thing ++ " needs "
                ++ D.args expected ++ ", but I see " ++ String.fromInt actual ++ " instead:"
            ,
              if actual - expected == 1 then
                d"Which is the extra one? Maybe some parentheses are missing?"
              else
                d"Which are the extra ones? Maybe some parentheses are missing?"
            )

    Binop region op1 op2 ->
      Report.Report "INFIX PROBLEM" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You cannot mix (" ++ op1 ++ ") and (" ++ op2 ++ ") without parentheses."
          ,
            D.reflow
              "I do not know how to group these expressions. Add parentheses for me!"
          )

    DuplicateDecl name r1 r2 ->
      nameClash source r1 r2 <|
        "This file has multiple `" ++ name ++ "` declarations."

    DuplicateType name r1 r2 ->
      nameClash source r1 r2 <|
        "This file defines multiple `" ++ name ++ "` types."

    DuplicateCtor name r1 r2 ->
      nameClash source r1 r2 <|
        "This file defines multiple `" ++ name ++ "` type constructors."

    DuplicateBinop name r1 r2 ->
      nameClash source r1 r2 <|
        "This file defines multiple (" ++ name ++ ") operators."

    DuplicateField name r1 r2 ->
      nameClash source r1 r2 <|
        "This record has multiple `" ++ name ++ "` fields."

    DuplicateAliasArg typeName name r1 r2 ->
      nameClash source r1 r2 <|
        "The `" ++ typeName ++ "` type alias has multiple `" ++ name ++ "` type variables."

    DuplicateUnionArg typeName name r1 r2 ->
      nameClash source r1 r2 <|
        "The `" ++ typeName ++ "` type has multiple `" ++ name ++ "` type variables."

    DuplicatePattern context name r1 r2 ->
      nameClash source r1 r2 <|
        case context of
          DPLambdaArgs ->
            "This anonymous function has multiple `" ++ name ++ "` arguments."

          DPFuncArgs funcName ->
            "The `" ++ funcName ++ "` function has multiple `" ++ name ++ "` arguments."

          DPCaseBranch ->
            "This `case` pattern has multiple `" ++ name ++ "` variables."

          DPLetBinding ->
            "This `let` expression defines `" ++ name ++ "` more than once!"

          DPDestruct ->
            "This pattern contains multiple `" ++ name ++ "` variables."

    EffectNotFound region name ->
      Report.Report "EFFECT PROBLEM" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You have declared that `" ++ name ++ "` is an effect type:"
          ,
            D.reflow <|
              "But I cannot find a custom type named `" ++ name ++ "` in this file!"
          )

    EffectFunctionNotFound region name ->
      Report.Report "EFFECT PROBLEM" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "This kind of effect module must define a `" ++ name ++ "` function."
          ,
            D.reflow <|
              "But I cannot find `" ++ name ++ "` in this file!"
          )


    ExportDuplicate name r1 r2 ->
      let
        messageThatEndsWithPunctuation =
          "You are trying to expose `" ++ name ++ "` multiple times!"
      in
      Report.Report "REDUNDANT EXPORT" r2 {-[]-} <|
        Code.toPair source r1 r2
          (
            D.reflow messageThatEndsWithPunctuation
          ,
            d"Remove one of them and you should be all set!"
          )
          (
            D.reflow (messageThatEndsWithPunctuation ++ " Once here:")
          ,
            d"And again right here:"
          ,
            d"Remove one of them and you should be all set!"
          )

    ExportNotFound region kind rawName possibleNames ->
      let
        suggestions =
          MList.map identity <| MList.take 4 <|
            Suggest.sort rawName identity possibleNames
      in
      Report.Report "UNKNOWN EXPORT" region {-suggestions-} <|
        let (a, thing, name) = toKindInfo kind rawName in
        D.stack
          [ D.fillSep
              [d"You",d"are",d"trying",d"to",d"expose",a,thing,d"named"
              ,name,d"but",d"I",d"cannot",d"find",d"its",d"definition."
              ]
          , case MList.map D.fromChars suggestions of
              [] ->
                D.reflow <|
                  "I do not see any super similar names in this file. Is the definition missing?"

              [alt] ->
                D.fillSep [d"Maybe",d"you",d"want",D.dullyellow alt,d"instead?"]

              alts ->
                D.stack
                  [ d"These names seem close though:"
                  , D.indent 4 <| D.vcat <| MList.map D.dullyellow alts
                  ]
          ]

    ExportOpenAlias region name ->
      Report.Report "BAD EXPORT" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "The (..) syntax is for exposing variants of a custom type. It cannot be used with a type alias like `"
              ++ name ++ "` though."
          ,
            D.reflow <|
              "Remove the (..) and you should be fine!"
          )

    ImportCtorByName region ctor tipe ->
      Report.Report "BAD IMPORT" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You are trying to import the `" ++ ctor
              ++ "` variant by name:"
          ,
            D.fillSep
              [d"Try",d"importing",D.green (da[D.fromName tipe , d"(..)"]),d"instead."
              ,d"The",d"dots",d"mean",d"“expose",d"the",D.fromName tipe,d"type",d"and"
              ,d"all",d"its",d"variants",d"so",d"it",d"gives",d"you",d"access",d"to"
              , da[D.fromName ctor, d"."]
              ]
          )

    ImportOpenAlias region name ->
      Report.Report "BAD IMPORT" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "The `" ++ name ++ "` type alias cannot be followed by (..) like this:"
          ,
            D.reflow <|
              "Remove the (..) and it should work."
          )

    ImportExposingNotFound region (ModuleName.Canonical _ home) value possibleNames ->
      let
        suggestions =
          MList.map identity <| MList.take 4 <|
            Suggest.sort home identity possibleNames
      in
      Report.Report "BAD IMPORT" region {-suggestions-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "The `" ++ home
              ++ "` module does not expose `"
              ++ value ++ "`:"
          ,
            case MList.map D.fromChars suggestions of
              [] ->
                d"I cannot find any super similar exposed names. Maybe it is private?"

              [alt] ->
                D.fillSep [d"Maybe",d"you",d"want",D.dullyellow alt,d"instead?"]

              alts ->
                D.stack
                  [ d"These names seem close though:"
                  , D.indent 4 <| D.vcat <| MList.map D.dullyellow alts
                  ]
          )

    NotFoundVar region prefix name possibleNames ->
      notFound source region prefix name "variable" possibleNames

    NotFoundType region prefix name possibleNames ->
      notFound source region prefix name "type" possibleNames

    NotFoundVariant region prefix name possibleNames ->
      notFound source region prefix name "variant" possibleNames

    NotFoundBinop region op locals ->
      if op == "===" then
        Report.Report "UNKNOWN OPERATOR" region {-["=="]-} <|
          Code.toSnippet source region Nothing
            (
              d"Elm does not have a (===) operator like JavaScript."
            ,
              d"Switch to (==) instead."
            )

      else if op == "!=" || op == "!==" then
        Report.Report "UNKNOWN OPERATOR" region {-["/="]-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "Elm uses a different name for the “not equal” operator:"
            ,
              D.stack
                [ D.reflow "Switch to (/=) instead."
                , D.toSimpleNote <|
                    "Our (/=) operator is supposed to look like a real “not equal” sign (≠). I hope that history will remember ("
                    ++ op ++ ") as a weird and temporary choice."
                ]
            )

      else if op == "**" then
        Report.Report "UNKNOWN OPERATOR" region {-["^","*"]-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "I do not recognize the (**) operator:"
            ,
              D.reflow <|
                "Switch to (^) for exponentiation. Or switch to (*) for multiplication."
            )

      else if op == "%" then
        Report.Report "UNKNOWN OPERATOR" region {-[]-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "Elm does not use (%) as the remainder operator:"
            ,
              D.stack
                [ D.reflow <|
                    "If you want the behavior of (%) like in JavaScript, switch to:"
                    ++" <https://package.elm-lang.org/packages/elm/core/latest/Basics#remainderBy>"
                , D.reflow <|
                    "If you want modular arithmetic like in math, switch to:"
                    ++" <https://package.elm-lang.org/packages/elm/core/latest/Basics#modBy>"
                , D.reflow <|
                    "The difference is how things work when negative numbers are involved."
                ]
            )

      else
        let
          suggestions =
            MList.map identity <| MList.take 2 <|
              Suggest.sort op identity (Set.toList locals)

          format altOp =
            D.green <| da[d"(", altOp, d")"]
        in
        Report.Report "UNKNOWN OPERATOR" region {-suggestions-} <|
          Code.toSnippet source region Nothing
            (
              D.reflow <|
                "I do not recognize the (" ++ op ++ ") operator."
            ,
              D.fillSep <|
                [d"Is",d"there",d"an",d"`import`",d"and",d"`exposing`",d"entry",d"for",d"it?"]
                ++
                  case MList.map D.fromChars suggestions of
                    [] ->
                      []

                    alts ->
                      [d"Maybe",d"you",d"want"] ++ D.commaSep (d"or") format alts ++ [d"instead?"]
            )

    PatternHasRecordCtor region name ->
      Report.Report "BAD PATTERN" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You can construct records by using `" ++ name
              ++ "` as a function, but it is not available in pattern matching like this:"
          ,
            D.reflow <|
              "I recommend matching the record as a variable and unpacking it later."
          )

    PortPayloadInvalid region portName {-badType-} invalidPayload ->
      let
        formatDetails (aBadKindOfThing, elaboration) =
          Report.Report "PORT ERROR" region {-[]-} <|
            Code.toSnippet source region Nothing
              (
                D.reflow <|
                  "The `" ++ portName ++ "` port is trying to transmit " ++ aBadKindOfThing ++ ":"
              ,
                D.stack
                  [ elaboration
                  , D.link "Hint"
                      "Ports are not a traditional FFI, so if you have tons of annoying ports, definitely read"
                      "ports"
                      "to learn how they are meant to work. They require a different mindset!"
                  ]
              )
      in
      formatDetails <|
        case invalidPayload of
          ExtendedRecord ->
            (
              "an extended record"
            ,
              D.reflow <|
                "But the exact shape of the record must be known at compile time. No type variables!"
            )

          Function ->
            (
              "a function"
            ,
              D.reflow <|
                "But functions cannot be sent in and out ports. If we allowed functions in from JS"
                ++" they may perform some side-effects. If we let functions out, they could produce"
                ++" incorrect results because Elm optimizations assume there are no side-effects."
            )


          TypeVariable name ->
            (
              "an unspecified type"
            ,
              D.reflow <|
                "But type variables like `" ++ name ++ "` cannot flow through ports."
                ++" I need to know exactly what type of data I am getting, so I can guarantee that"
                ++" unexpected data cannot sneak in and crash the Elm program."
            )

          UnsupportedType name ->
            (
              "a `" ++ name ++ "` value"
            ,
              D.stack
                [ D.reflow <| "I cannot handle that. The types that CAN flow in and out of Elm include:"
                , D.indent 4 <|
                    D.reflow <|
                      "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,"
                      ++" tuples, records, and JSON values."
                , D.reflow <|
                    "Since JSON values can flow through, you can use JSON encoders and decoders"
                    ++" to allow other types through as well. More advanced users often just do"
                    ++" everything with encoders and decoders for more control and better errors."
                ]
            )

    PortTypeInvalid region name portProblem ->
      let
        formatDetails (before, after) =
          Report.Report "BAD PORT" region {-[]-} <|
            Code.toSnippet source region Nothing <|
              (
                D.reflow before
              ,
                D.stack
                  [ after
                  , D.link "Hint" "Read" "ports"
                      "for more advice. For example, do not end up with one port per JS function!"
                  ]
              )
      in
      formatDetails <|
        case portProblem of
          CmdNoArg ->
            (
              "The `" ++ name ++ "` port cannot be just a command."
            ,
              D.reflow <|
                "It can be (() -> Cmd msg) if you just need to trigger a JavaScript"
                ++" function, but there is often a better way to set things up."
            )

          CmdExtraArgs n ->
            (
              "The `" ++ name ++ "` port can only send ONE value out to JavaScript."
            ,
              let
                theseItemsInSomething =
                       if n == 2 then "both of these items into a tuple or record"
                  else if n == 3 then "these " ++ String.fromInt n ++ " items into a tuple or record"
                  else                "these " ++ String.fromInt n ++ " items into a record"
              in
              D.reflow <|
                "You can put " ++ theseItemsInSomething ++ " to send them out though."
            )

          CmdBadMsg ->
            (
              "The `" ++ name ++ "` port cannot send any messages to the `update` function."
            ,
              D.reflow <|
                "It must produce a (Cmd msg) type. Notice the lower case `msg` type"
                ++" variable. The command will trigger some JS code, but it will not send"
                ++" anything particular back to Elm."
            )

          SubBad ->
            ( "There is something off about this `" ++ name ++ "` port declaration."
            ,
              D.stack
                [ D.reflow <|
                    "To receive messages from JavaScript, you need to define a port like this:"
                , D.indent 4 <| D.dullyellow <| D.fromChars <|
                    "port " ++ name ++ " : (Int -> msg) -> Sub msg"
                , D.reflow <|
                    "Now every time JS sends an `Int` to this port, it is converted to a `msg`."
                    ++" And if you subscribe, those `msg` values will be piped into your `update`"
                    ++" function. The only thing you can customize here is the `Int` type."
                ]
            )

          NotCmdOrSub ->
            (
              "I am confused about the `" ++ name ++ "` port declaration."
            ,
              D.reflow <|
                "Ports need to produce a command (Cmd) or a subscription (Sub) but"
                ++" this is neither. I do not know how to handle this."
            )

    RecursiveAlias region name args tipe others ->
      aliasRecursionReport source region name args tipe others

    RecursiveDecl region name names ->
      let
        makeTheory question details =
          D.fillSep <| MList.map (D.dullyellow << D.fromChars) (String.words question) ++ MList.map D.fromChars (String.words details)
      in
      Report.Report "CYCLIC DEFINITION" region {-[]-} <|
        Code.toSnippet source region Nothing <|
          case names of
            [] ->
              (
                D.reflow <|
                  "The `" ++ name ++ "` value is defined directly in terms of itself, causing an infinite loop."
              ,
                D.stack
                  [ makeTheory "Are you trying to mutate a variable?" <|
                      "Elm does not have mutation, so when I see " ++ name
                      ++ " defined in terms of " ++ name
                      ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                  , makeTheory "Maybe you DO want a recursive value?" <|
                      "To define " ++ name ++ " we need to know what " ++ name
                      ++ " is, so let’s expand it. Wait, but now we need to know what " ++ name
                      ++ " is, so let’s expand it... This will keep going infinitely!"
                  , D.link "Hint"
                      "The root problem is often a typo in some variable name, but I recommend reading"
                      "bad-recursion"
                      "for more detailed advice, especially if you actually do need a recursive value."
                  ]
              )

            _::_ ->
              (
                D.reflow <|
                  "The `" ++ name ++ "` definition is causing a very tricky infinite loop."
              ,
                D.stack
                  [ D.reflow <|
                      "The `" ++ name
                      ++ "` value depends on itself through the following chain of definitions:"
                  , D.cycle 4 name names
                  , D.link "Hint"
                      "The root problem is often a typo in some variable name, but I recommend reading"
                      "bad-recursion"
                      "for more detailed advice, especially if you actually do want mutually recursive values."
                  ]
              )

    RecursiveLet (A.At region name) names ->
      Report.Report "CYCLIC VALUE" region {-[]-} <|
        Code.toSnippet source region Nothing <|
          case names of
            [] ->
              let
                makeTheory question details =
                  D.fillSep <| MList.map (D.dullyellow << D.fromChars) (String.words question) ++ MList.map D.fromChars (String.words details)
              in
                (
                  D.reflow <|
                    "The `" ++ name ++ "` value is defined directly in terms of itself, causing an infinite loop."
                ,
                  D.stack
                    [ makeTheory "Are you trying to mutate a variable?" <|
                        "Elm does not have mutation, so when I see " ++ name
                        ++ " defined in terms of " ++ name
                        ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
                    , makeTheory "Maybe you DO want a recursive value?" <|
                        "To define " ++ name ++ " we need to know what " ++ name
                        ++ " is, so let’s expand it. Wait, but now we need to know what " ++ name
                        ++ " is, so let’s expand it... This will keep going infinitely!"
                    , D.link "Hint"
                        "The root problem is often a typo in some variable name, but I recommend reading"
                        "bad-recursion"
                        "for more detailed advice, especially if you actually do need a recursive value."
                    ]
                )

            _ ->
              (
                D.reflow <|
                  "I do not allow cyclic values in `let` expressions."
              ,
                D.stack
                  [ D.reflow <|
                      "The `" ++ name
                      ++ "` value depends on itself through the following chain of definitions:"
                  , D.cycle 4 name names
                  , D.link "Hint"
                      "The root problem is often a typo in some variable name, but I recommend reading"
                      "bad-recursion"
                      "for more detailed advice, especially if you actually do want mutually recursive values."
                  ]
              )

    Shadowing name r1 r2 ->
      Report.Report "SHADOWING" r2 {-[]-} <|
        let
          advice =
            D.stack
              [ D.reflow <|
                  "Think of a more helpful name for one of them and you should be all set!"
              , D.link "Note"
                  "Linters advise against shadowing, so Elm makes “best practices” the default. Read"
                  "shadowing"
                  "for more details on this choice."
              ]
        in
        Code.toPair source r1 r2
          ( d"These variables cannot have the same name:"
          , advice
          )
          ( D.reflow <| "The name `" ++ name ++ "` is first defined here:"
          , d"But then it is defined AGAIN over here:"
          , advice
          )

    TupleLargerThanThree region ->
      Report.Report "BAD TUPLE" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            d"I only accept tuples with two or three items. This has too many:"
          ,
            D.stack
              [ D.reflow <|
                  "I recommend switching to records. Each item will be named, and you can use"
                  ++" the `point.x` syntax to access them."

              , D.link "Note" "Read" "tuples"

                  "for more comprehensive advice on working with large chunks of data in Elm."
              ]
          )

    TypeVarsUnboundInUnion unionRegion typeName allVars unbound unbounds ->
      unboundTypeVars source unionRegion [d"type"] typeName allVars unbound unbounds

    TypeVarsMessedUpInAlias aliasRegion typeName allVars unusedVars unboundVars ->
      case (unusedVars, unboundVars) of
        (unused::unuseds, []) ->
          let
            backQuote name =
              da[d"`" , D.fromName name , d"`"]

            allUnusedNames =
              MList.map Tuple.first unusedVars

            ((title, subRegion), overview, stuff) =
              case unuseds of
                [] ->
                  (("UNUSED TYPE VARIABLE"
                  , Just (Tuple.second unused))
                  , [d"Type",d"alias",backQuote typeName,d"does",d"not",d"use",d"the"
                    ,backQuote (Tuple.first unused),d"type",d"variable."
                    ]
                  , [D.dullyellow (backQuote (Tuple.first unused))]
                  )

                _::_ ->
                  (("UNUSED TYPE VARIABLES"
                  , Nothing)
                  , [d"Type",d"variables"]
                    ++ D.commaSep (d"and") identity (MList.map D.fromName allUnusedNames)
                    ++ [d"are",d"unused",d"in",d"the",backQuote typeName,d"definition."]
                  , D.commaSep (d"and") D.dullyellow (MList.map D.fromName allUnusedNames)
                  )
          in
          Report.Report title aliasRegion {-[]-} <|
            Code.toSnippet source aliasRegion subRegion
              (
                D.fillSep overview
              ,
                D.stack
                  [ D.fillSep <|
                      [d"I",d"recommend",d"removing"] ++ stuff ++ [d"from",d"the",d"declaration,",d"like",d"this:"]
                  , D.indent 4 <| D.hsep <|
                      [d"type",d"alias",D.green (D.fromName typeName)]
                      ++ MList.map D.fromName (MList.filter (\x_ -> MList.notElem x_ allUnusedNames) allVars)
                      ++ [d"=", d"..."]
                  , D.reflow <|
                      "Why? Well, if I allowed `type alias Height a = Float` I would need to answer"
                      ++" some weird questions. Is `Height Bool` the same as `Float`? Is `Height Bool`"
                      ++" the same as `Height Int`? My solution is to not need to ask them!"
                  ]
              )

        ([], unbound::unbounds) ->
          unboundTypeVars source aliasRegion [d"type",d"alias"] typeName allVars unbound unbounds

        _ {- (_, _) -} ->
          let
            unused = MList.map Tuple.first unusedVars
            unbound = MList.map Tuple.first unboundVars

            theseAreUsed =
              case unbound of
                [x] ->
                  [d"Type",d"variable",D.dullyellow (da[d"`" , D.fromName x , d"`"]),d"appears"
                  ,d"in",d"the",d"definition,",d"but",d"I",d"do",d"not",d"see",d"it",d"declared."
                  ]

                _ ->
                  [d"Type",d"variables"]
                  ++ D.commaSep (d"and") D.dullyellow (MList.map D.fromName unbound)
                  ++ [d"are",d"used",d"in",d"the",d"definition,",d"but",d"I",d"do",d"not",d"see",d"them",d"declared."]

            butTheseAreUnused =
              case unused of
                [x] ->
                  [d"Likewise,",d"type",d"variable"
                  ,D.dullyellow (da[d"`" , D.fromName x , d"`"])
                  ,d"is",d"delared,",d"but",d"not",d"used."
                  ]

                _ ->
                  [d"Likewise,",d"type",d"variables"]
                  ++ D.commaSep (d"and") D.dullyellow (MList.map D.fromName unused)
                  ++ [d"are",d"delared,",d"but",d"not",d"used."]

          in
          Report.Report "TYPE VARIABLE PROBLEMS" aliasRegion {-[]-} <|
            Code.toSnippet source aliasRegion Nothing
              (
                D.reflow <|
                  "Type alias `" ++ typeName ++ "` has some type variable problems."
              ,
                D.stack
                  [ D.fillSep <| theseAreUsed ++ butTheseAreUnused
                  , D.reflow <|
                      "My guess is that a definition like this will work better:"
                  , D.indent 4 <| D.hsep <|
                      [d"type",d"alias", D.fromName typeName]
                      ++ MList.map D.fromName (MList.filter (\x_ -> MList.notElem x_ unused) allVars)
                      ++ MList.map (D.green << D.fromName) unbound
                      ++ [d"=", d"..."]
                  ]
              )



-- BAD TYPE VARIABLES


unboundTypeVars : Code.Source -> A.Region -> TList D.Doc -> Name.Name -> TList Name.Name -> (Name.Name, A.Region) -> TList (Name.Name, A.Region) -> Report.Report
unboundTypeVars source declRegion tipe typeName allVars (unboundVar, varRegion) unboundVars =
  let
    backQuote name =
      da[d"`" , D.fromName name , d"`"]

    (title, subRegion, overview) =
      case MList.map Tuple.first unboundVars of
        [] ->
          ( "UNBOUND TYPE VARIABLE"
          , Just varRegion
          , [d"The",backQuote typeName]
            ++ tipe
            ++ [d"uses",d"an",d"unbound",d"type",d"variable",D.dullyellow (backQuote unboundVar),d"in",d"its",d"definition:"]
          )

        vars ->
          ( "UNBOUND TYPE VARIABLES"
          , Nothing
          , [d"Type",d"variables"]
            ++ D.commaSep (d"and") D.dullyellow (D.fromName unboundVar :: MList.map D.fromName vars)
            ++ [d"are",d"unbound",d"in",d"the",backQuote typeName] ++ tipe ++ [d"definition:"]
          )
  in
  Report.Report title declRegion {-[]-} <|
    Code.toSnippet source declRegion subRegion
      (
        D.fillSep overview
      ,
        D.stack
          [ D.reflow <|
              "You probably need to change the declaration to something like this:"
          , D.indent 4 <| D.hsep <|
              tipe
              ++ [D.fromName typeName]
              ++ MList.map D.fromName allVars
              ++ MList.map (D.green << D.fromName) (unboundVar :: MList.map Tuple.first unboundVars)
              ++ [d"=", d"..."]
          , D.reflow <|
              "Why? Well, imagine one `" ++ typeName ++ "` where `" ++ unboundVar ++
              "` is an Int and another where it is a Bool. When we explicitly list the type"
              ++" variables, the type checker can see that they are actually different types."
          ]
      )



-- NAME CLASH


nameClash : Code.Source -> A.Region -> A.Region -> String -> Report.Report
nameClash source r1 r2 messageThatEndsWithPunctuation =
  Report.Report "NAME CLASH" r2 {-[]-} <|
    Code.toPair source r1 r2
      (
        D.reflow messageThatEndsWithPunctuation
      ,
        d"How can I know which one you want? Rename one of them!"
      )
      (
        D.reflow (messageThatEndsWithPunctuation ++ " One here:")
      ,
        d"And another one here:"
      ,
        d"How can I know which one you want? Rename one of them!"
      )



-- AMBIGUOUS NAME


ambiguousName : Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> ModuleName.Canonical -> OneOrMore.OneOrMore ModuleName.Canonical -> String -> Report.Report
ambiguousName source region maybePrefix name h hs thing =
  let
    possibleHomes = MList.sortBy ModuleName.comparison (h :: OneOrMore.destruct (::) hs)
  in
  Report.Report "AMBIGUOUS NAME" region {-[]-} <|
    Code.toSnippet source region Nothing <|
      case maybePrefix of
        Nothing ->
          let
            homeToYellowDoc (ModuleName.Canonical _ home) =
              D.dullyellow (da[D.fromName home , d"." , D.fromName name])
          in
          (
            D.reflow <| "This usage of `" ++ name ++ "` is ambiguous:"
          ,
            D.stack
              [ D.reflow <|
                  "This name is exposed by " ++ String.fromInt (MList.length possibleHomes) ++ " of your imports, so I am not"
                  ++" sure which one to use:"
              , D.indent 4 <| D.vcat <| MList.map homeToYellowDoc possibleHomes
              , D.reflow <|
                  "I recommend using qualified names for imported values. I also recommend having"
                  ++" at most one `exposing (..)` per file to make name clashes like this less common"
                  ++" in the long run."
              , D.link "Note" "Check out" "imports" "for more info on the import syntax."
              ]
          )

        Just prefix ->
          let
            homeToYellowDoc (ModuleName.Canonical _ home) =
              if prefix == home then
                D.hsep[D.cyanS "import", D.fromName home]
              else
                D.hsep[D.cyanS "import", D.fromName home, D.cyanS "as", D.fromName prefix]

            eitherOrAny =
              if MList.length possibleHomes == 2 then "either" else "any"
          in
          (
            D.reflow <| "This usage of `" ++ toQualString prefix name ++ "` is ambiguous."
          ,
            D.stack
              [ D.reflow <|
                  "It could refer to a " ++ thing ++ " from "
                  ++ eitherOrAny ++ " of these imports:"
              , D.indent 4 <| D.vcat <| MList.map homeToYellowDoc possibleHomes
              , D.reflowLink "Read" "imports" "to learn how to clarify which one you want."
              ]
          )



-- NOT FOUND


notFound : Code.Source -> A.Region -> Maybe Name.Name -> Name.Name -> String -> PossibleNames -> Report.Report
notFound source region maybePrefix name thing (PossibleNames locals quals) =
  let
    givenName =
      MMaybe.maybe identity toQualString maybePrefix name

    possibleNames =
      let
        addQuals prefix localSet allNames =
          Set.foldr (\x xs -> toQualString prefix x :: xs) allNames localSet
      in
      Map.foldrWithKey addQuals (MList.map identity (Set.toList locals)) quals

    nearbyNames =
      MList.take 4 (Suggest.sort givenName identity possibleNames)

    toDetails noSuggestionDetails yesSuggestionDetails =
      case nearbyNames of
        [] ->
          D.stack
            [ D.reflow noSuggestionDetails
            , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

        suggestions ->
          D.stack
            [ D.reflow yesSuggestionDetails
            , D.indent 4 <| D.vcat <| MList.map D.dullyellow <| MList.map D.fromChars suggestions
            , D.link "Hint" "Read" "imports" "to see how `import` declarations work in Elm."
            ]

  in
  Report.Report "NAMING ERROR" region {-nearbyNames-} <|
    Code.toSnippet source region Nothing
      (
        D.reflow <|
          "I cannot find a `" ++ givenName ++ "` " ++ thing ++ ":"
      ,
        case maybePrefix of
          Nothing ->
            toDetails
              "Is there an `import` or `exposing` missing up top?"
              "These names seem close though:"

          Just prefix ->
            case Map.lookup prefix quals of
              Nothing ->
                toDetails
                  ("I cannot find a `" ++ prefix ++ "` module. Is there an `import` for it?")
                  ("I cannot find a `" ++ prefix ++ "` import. These names seem close though:")

              Just _ ->
                toDetails
                  ("The `" ++ prefix ++ "` module does not expose a `" ++ name ++ "` " ++ thing ++ ".")
                  ("The `" ++ prefix ++ "` module does not expose a `" ++ name ++ "` " ++ thing ++ ". These names seem close though:")
      )


toQualString : Name.Name -> Name.Name -> String
toQualString prefix name =
  prefix ++ "." ++ name



-- BAD ALIAS RECURSION


aliasRecursionReport : Code.Source -> A.Region -> Name.Name -> TList Name.Name -> Src.Type -> TList Name.Name -> Report.Report
aliasRecursionReport source region name args tipe others =
  case others of
    [] ->
      Report.Report "ALIAS PROBLEM" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            d"This type alias is recursive, forming an infinite type!"
          ,
            D.stack
              [ D.reflow <|
                  "When I expand a recursive type alias, it just keeps getting bigger and bigger."
                  ++" So dealiasing results in an infinitely large type! Try this instead:"
              , D.indent 4 <|
                  aliasToUnionDoc name args tipe
              , D.link "Hint"
                  "This is kind of a subtle distinction. I suggested the naive fix, but I recommend reading"
                  "recursive-alias"
                  "for ideas on how to do better."
              ]
          )

    _ ->
      Report.Report "ALIAS PROBLEM" region {-[]-} <|
        Code.toSnippet source region Nothing
          (
            d"This type alias is part of a mutually recursive set of type aliases."
          ,
            D.stack
              [ d"It is part of this cycle of type aliases:"
              , D.cycle 4 name others
              , D.reflow <|
                  "You need to convert at least one of these type aliases into a `type`."
              , D.link "Note" "Read" "recursive-alias"
                  "to learn why this `type` vs `type alias` distinction matters. It is subtle but important!"
              ]
          )


aliasToUnionDoc : Name.Name -> TList Name.Name -> Src.Type -> Doc
aliasToUnionDoc name args tipe =
  D.vcat
    [ D.dullyellow <|
        D.hsep[d"type", D.fromName name, D.hsep ((MList.map D.fromName args) ++ [d"="])]
    , D.green <|
        D.indent 4 (D.fromName name)
    , D.dullyellow <|
        D.indent 8 (RT.srcToDoc RT.App tipe)
    ]
