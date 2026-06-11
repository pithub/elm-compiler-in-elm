module Extra.System.Path exposing
    ( FileName
    , FilePath
    , absolute
    , addExtension
    , addName
    , addNames
    , combine
    , dropLastName
    , fromString
    , getNames
    , isRelative
    , makeAbsolute
    , makeRelative
    , splitExtension
    , splitLastName
    , toString
    )

import Extra.Type.List as MList exposing (TList)



-- FILE PATHS


type alias FileName =
    String


type FilePath
    = Absolute (TList FileName)
    | Relative (TList FileName)


absolute : TList FileName -> FilePath
absolute =
    Absolute


getNames : FilePath -> TList FileName
getNames path =
    case path of
        Absolute names ->
            names

        Relative names ->
            names


makeAbsolute : FilePath -> FilePath -> FilePath
makeAbsolute cwd path =
    case path of
        Absolute _ ->
            path

        Relative _ ->
            combine cwd path


modifyNames : FilePath -> (TList FileName -> TList FileName) -> FilePath
modifyNames path f =
    case path of
        Absolute names ->
            Absolute (f names)

        Relative names ->
            Relative (f names)



-- FROM AND TO STRING


fromString : String -> FilePath
fromString string =
    if String.startsWith "/" string then
        fromStringHelper Absolute (String.dropLeft 1 string)

    else
        fromStringHelper Relative string


fromStringHelper : (TList FileName -> FilePath) -> String -> FilePath
fromStringHelper constructor string =
    string
        |> String.split "/"
        |> MList.filter (\name -> name /= "" && name /= ".")
        |> MList.reverse
        |> constructor


toString : FilePath -> String
toString path =
    case path of
        Absolute names ->
            "/" ++ String.join "/" (MList.reverse names)

        Relative [] ->
            "."

        Relative names ->
            String.join "/" (MList.reverse names)



-- STANDARD FUNCTIONS


addExtension : FilePath -> String -> FilePath
addExtension path extension =
    case splitLastName path of
        ( parent, name ) ->
            addName parent (name ++ "." ++ extension)


addName : FilePath -> FileName -> FilePath
addName path name =
    modifyNames path (\names -> name :: names)


addNames : FilePath -> TList FileName -> FilePath
addNames path names =
    MList.foldl addName path names


combine : FilePath -> FilePath -> FilePath
combine bPath aPath =
    case aPath of
        Absolute _ ->
            aPath

        Relative aNames ->
            modifyNames bPath (\bNames -> aNames ++ bNames)


dropLastName : FilePath -> FilePath
dropLastName path =
    Tuple.first (splitLastName path)


isRelative : FilePath -> Bool
isRelative path =
    case path of
        Absolute _ ->
            False

        Relative _ ->
            True


makeRelative : FilePath -> FilePath -> FilePath
makeRelative base path =
    case ( base, path ) of
        ( Absolute baseNames, Absolute pathNames ) ->
            Relative <| MList.reverse <| makeRelativeHelper (MList.reverse baseNames) (MList.reverse pathNames)

        _ ->
            path


makeRelativeHelper : TList FileName -> TList FileName -> TList FileName
makeRelativeHelper baseNames pathNames =
    case ( baseNames, pathNames ) of
        ( baseName :: baseRest, pathName :: pathRest ) ->
            if baseName == pathName then
                makeRelativeHelper baseRest pathRest

            else
                pathNames

        _ ->
            pathNames


splitExtension : FileName -> ( FileName, String )
splitExtension name =
    case MList.reverse (String.split "." name) of
        extension :: rest ->
            ( String.join "." (MList.reverse rest), extension )

        _ ->
            ( name, "" )


splitLastName : FilePath -> ( FilePath, FileName )
splitLastName path =
    case path of
        Absolute (name :: rest) ->
            ( Absolute rest, name )

        Relative (name :: rest) ->
            ( Relative rest, name )

        _ ->
            ( path, "" )
