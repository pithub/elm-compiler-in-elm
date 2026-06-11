{- MANUALLY FORMATTED -}
module Terminal.Main exposing
  ( runMain
  )


import Builder.Generate as Generate
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D exposing (d, da)
import Extra.System.IO as IO
import Extra.System.Path as Path
import Extra.Type.List as MList
import Terminal.Bump as Bump
import Terminal.Develop as Develop
import Terminal.Diff as Diff
import Terminal.Impl.Terminal as Terminal exposing
  ( parser
  , common, uncommon
  , noArgs, zeroOrMore, oneOf, require0, require1, require2, require3
  , noFlags, flags, andFlag, onOff, flag
  )
import Terminal.Impl.Terminal.Helpers as Helpers
import Terminal.Init as Init
import Terminal.Install as Install
import Terminal.Make as Make
import Terminal.Publish as Publish
import Terminal.Repl as Repl



-- STATE AND IO


type alias GlobalState h =
  Generate.GlobalState h


type alias IO h v =
  Generate.IO h v


type alias Command h =
  Terminal.Command (GlobalState h)


type alias Parser h v =
  Terminal.Parser (GlobalState h) v



-- RUN


runMain : IO h ()
runMain =
  Terminal.app intro outro
    [ repl
    , init
    , reactor
    , make
    , install
    , bump
    , diff
    , publish
    ]


intro : D.Doc
intro =
  D.vcat
    [ D.fillSep
        [d"Hi,",d"thank",d"you",d"for",d"trying",d"out"
        ,D.greenS "Elm"
        ,da[D.greenS (V.toChars V.compiler), d"."]
        ,d"I hope you like it!"
        ]
    , d""
    , D.blackS "-------------------------------------------------------------------------------"
    , D.blackS "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , D.blackS "It teaches many important concepts, including how to use `elm` in the terminal."
    , D.blackS "-------------------------------------------------------------------------------"
    ]


outro : D.Doc
outro =
  D.fillSep <| MList.map D.fromChars <| String.words <|
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and"
    ++ " happy to help out. They hang out there because it is fun, so be kind to get the"
    ++ " best results!"



-- INIT


init : Command h
init =
  let
    summary =
      "Start an Elm project. It creates a starter elm.json file and"
      ++ " provides a link explaining what to do from there."

    details =
      "The `init` command helps start Elm projects:"

    example =
      D.reflow <|
        "It will ask permission to create an elm.json file, the one thing common"
        ++ " to all Elm projects. It also provides a link explaining what to do from there."
  in
  Terminal.command "init" (common summary) details example noArgs noFlags Init.run



-- REPL


repl : Command h
repl =
  let
    summary =
      "Open up an interactive programming session. Type in Elm expressions"
      ++ " like (2 + 2) or (String.length \"test\") and see if they equal four!"

    details =
      "The `repl` command opens up an interactive programming session:"

    example =
      D.reflow <|
        "Start working through <https://guide.elm-lang.org> to learn how to use this!"
        ++ " It has a whole chapter that uses the REPL for everything, so that is probably"
        ++ " the quickest way to get started."

    replFlags =
      flags Repl.Flags
        |> andFlag (flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs.")
        |> andFlag (onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme.")
  in
  Terminal.command "repl" (common summary) details example noArgs replFlags Repl.run


interpreter : Parser h Path.FilePath
interpreter =
  parser
    {- singular -} "interpreter"
    {- plural -} "interpreters"
    {- parser -} (Just << Path.fromString)
    {- suggest -} (\_ -> IO.return [])
    {- examples -} (\_ -> IO.return ["node","nodejs"])



-- REACTOR


reactor : Command h
reactor =
  let
    summary =
      "Compile code with a click. It opens a file viewer in your browser, and"
      ++ " when you click on an Elm file, it compiles and you see the result."

    details =
      "The `reactor` command starts a local server on your computer:"

    example =
      D.reflow <|
        "After running that command, you would have a server at <http://localhost:8000>"
        ++ " that helps with development. It shows your files like a file viewer. If you"
        ++ " click on an Elm file, it will compile it for you! And you can just press"
        ++ " the refresh button in the browser to recompile things."

    reactorFlags =
      flags Develop.Flags
        |> andFlag (flag "port" port_ "The port of the server (default: 8000)")
  in
  Terminal.command "reactor" (common summary) details example noArgs reactorFlags Develop.run


port_ : Parser h Int
port_ =
  parser
    {- singular -} "port"
    {- plural -} "ports"
    {- parser -} String.toInt
    {- suggest -} (\_ -> IO.return [])
    {- examples -} (\_ -> IO.return ["3000","8000"])



-- MAKE


make : Command h
make =
  let
    details =
      "The `make` command compiles Elm code into JS or HTML:"

    example =
      D.stack
        [ D.reflow
            "For example:"
        , D.indent 4 <| D.greenS "elm make src/Main.elm"
        , D.reflow <|
            "This tries to compile an Elm file named src/Main.elm, generating an index.html"
            ++ " file if possible."
        ]

    makeFlags =
      flags Make.Flags
        |> andFlag (onOff "debug" "Turn on the time-travelling debugger. It allows you to rewind and replay events. The events can be imported/exported into a file, which makes for very precise bug reports!")
        |> andFlag (onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation.")
        |> andFlag (flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!")
        |> andFlag (flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!")
        |> andFlag (flag "docs" Make.docsFile "Generate a JSON file of documentation for a package. Eventually it will be possible to preview docs with `reactor` because it is quite hard to deal with these JSON files directly.")
  in
  Terminal.command "make" uncommon details example (zeroOrMore Helpers.elmFile) makeFlags Make.run



-- INSTALL


install : Command h
install =
  let
    details =
      "The `install` command fetches packages from <https://package.elm-lang.org> for"
      ++ " use in your project:"

    example =
      D.stack
        [ D.reflow
            "For example, if you want to get packages for HTTP and JSON, you would say:"
        , D.indent 4 <| D.green <| D.vcat <|
            [ D.d "elm install elm/http"
            , D.d "elm install elm/json"
            ]
        , D.reflow <|
            "Notice that you must say the AUTHOR name and PROJECT name! After running those"
            ++ " commands, you could say `import Http` or `import Json.Decode` in your code."
        , D.reflow <|
            "What if two projects use different versions of the same package? No problem!"
            ++ " Each project is independent, so there cannot be conflicts like that!"
        ]

    installArgs =
      oneOf
        [ require0 Install.NoArgs
        , require1 Install.Install Helpers.package
        ]
  in
  Terminal.command "install" uncommon details example installArgs noFlags Install.run



-- PUBLISH


publish : Command h
publish =
  let
    details =
      "The `publish` command publishes your package on <https://package.elm-lang.org>"
      ++ " so that anyone in the Elm community can use it."

    example =
      D.stack
        [ D.reflow
            "Think hard if you are ready to publish NEW packages though!"
        , D.reflow <|
            "Part of what makes Elm great is the packages ecosystem. The fact that"
            ++ " there is usually one option (usually very well done) makes it way"
            ++ " easier to pick packages and become productive. So having a million"
            ++ " packages would be a failure in Elm. We do not need twenty of"
            ++ " everything, all coded in a single weekend."
        , D.reflow <|
            "So as community members gain wisdom through experience, we want"
            ++ " them to share that through thoughtful API design and excellent"
            ++ " documentation. It is more about sharing ideas and insights than"
            ++ " just sharing code! The first step may be asking for advice from"
            ++ " people you respect, or in community forums. The second step may"
            ++ " be using it at work to see if it is as nice as you think. Maybe"
            ++ " it ends up as an experiment on GitHub only. Point is, try to be"
            ++ " respectful of the community and package ecosystem!"
        , D.reflow
            "Check out <https://package.elm-lang.org/help/design-guidelines> for guidance on how to create great packages!"
        ]
  in
  Terminal.command "publish" uncommon details example noArgs noFlags Publish.run



-- BUMP


bump : Command h
bump =
  let
    details =
      "The `bump` command figures out the next version number based on API changes:"

    example =
      D.reflow <|
        "Say you just published version 1.0.0, but then decided to remove a function."
        ++ " I will compare the published API to what you have locally, figure out that"
        ++ " it is a MAJOR change, and bump your version number to 2.0.0. I do this with"
        ++ " all packages, so there cannot be MAJOR changes hiding in PATCH releases in Elm!"
  in
  Terminal.command "bump" uncommon details example noArgs noFlags Bump.run



-- DIFF


diff : Command h
diff =
  let
    details =
      "The `diff` command detects API changes:"

    example =
      D.stack
        [ D.reflow <|
            "For example, to see what changed in the HTML package between"
            ++ " versions 1.0.0 and 2.0.0, you can say:"
        , D.indent 4 <| D.greenS "elm diff elm/html 1.0.0 2.0.0"
        , D.reflow <|
            "Sometimes a MAJOR change is not actually very big, so"
            ++ " this can help you plan your upgrade timelines."
        ]

    diffArgs =
      oneOf
        [ require0 Diff.CodeVsLatest
        , require1 Diff.CodeVsExactly Helpers.version
        , require2 Diff.LocalInquiry Helpers.version Helpers.version
        , require3 Diff.GlobalInquiry Helpers.package Helpers.version Helpers.version
        ]
  in
  Terminal.command "diff" uncommon details example diffArgs noFlags Diff.run
