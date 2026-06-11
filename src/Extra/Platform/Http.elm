module Extra.Platform.Http exposing
    ( Exception
    , Header
    , Manager
    , Method
    , Request
    , defaultManager
    , methodGet
    , methodPost
    , parseUrlThrow
    , urlEncodeVars
    , userAgent
    , withBytesResponse
    , withStringResponse
    )

import Bytes exposing (Bytes)
import Extra.Platform as Platform
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Http



-- IO


type alias IO b c d e v =
    Platform.IO b c d e v



-- MANAGER


type Manager
    = Manager (Maybe String)


defaultManager : IO b c d e Manager
defaultManager =
    IO.bind Platform.httpPrefix newManager


newManager : Maybe String -> IO b c d e Manager
newManager maybePrefix =
    IO.return (Manager maybePrefix)


managedUrl : Manager -> String -> Maybe String
managedUrl (Manager maybePrefix) url =
    Maybe.map (\prefix -> prefix ++ url) maybePrefix



-- HEADERS


type alias Header =
    Http.Header


userAgent : String -> Header
userAgent agent =
    Http.header "User-Agent" agent



-- REQUEST


type alias Method =
    String


methodGet : Method
methodGet =
    "GET"


methodPost : Method
methodPost =
    "POST"


type alias Request =
    { method : Method
    , headers : TList Header
    , url : String
    , body : Http.Body
    }


parseUrlThrow : String -> IO b c d e Request
parseUrlThrow url =
    IO.return
        { method = methodGet
        , headers = []
        , url = url
        , body = Http.emptyBody
        }



-- RESPONSE


type alias Exception =
    Http.Error



-- IO


withStringResponse : Request -> Manager -> (Either Exception String -> IO b c d e v) -> IO b c d e v
withStringResponse request manager handler =
    withExpect stringExpect request manager handler


withBytesResponse : Request -> Manager -> (Either Exception Bytes -> IO b c d e v) -> IO b c d e v
withBytesResponse request manager handler =
    withExpect bytesExpect request manager handler


withExpect : ((Either Exception v -> IO b c d e w) -> Http.Expect (IO b c d e w)) -> Request -> Manager -> (Either Exception v -> IO b c d e w) -> IO b c d e w
withExpect expectFun request manager handler =
    case managedUrl manager request.url of
        Just url ->
            Http.request
                { method = request.method
                , headers = request.headers
                , url = url
                , body = request.body
                , expect = expectFun handler
                , timeout = Nothing
                , tracker = Nothing
                }
                |> IO.liftCmdIO

        Nothing ->
            handler (Left Http.NetworkError)


stringExpect : (Either Exception String -> IO b c d e v) -> Http.Expect (IO b c d e v)
stringExpect handler =
    Http.expectString (mapHandler handler)


bytesExpect : (Either Exception Bytes -> IO b c d e v) -> Http.Expect (IO b c d e v)
bytesExpect handler =
    Http.expectBytesResponse (mapHandler handler) toResult


mapHandler : (Either Exception v -> IO b c d e w) -> Result Http.Error v -> IO b c d e w
mapHandler handler result =
    handler <|
        case result of
            Ok a ->
                Right a

            Err error ->
                Left error


toResult : Http.Response Bytes -> Result Http.Error Bytes
toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            Ok body



-- URL ENCODING


urlEncode : String -> String
urlEncode string =
    string |> String.toList |> MList.map urlEncodeChar |> String.join ""


urlEncodeVars : TList ( String, String ) -> String
urlEncodeVars vars =
    MList.map (\( key, value ) -> urlEncode key ++ "=" ++ urlEncode value) vars
        |> String.join "&"


urlEncodeChar : Char -> String
urlEncodeChar char =
    case Map.lookup char specialChars of
        Just replacement ->
            replacement

        Nothing ->
            String.fromChar char


specialChars : Map.Map Char String
specialChars =
    Map.fromList
        [ ( ' ', "%20" )
        , ( '!', "%21" )
        , ( '#', "%23" )
        , ( '$', "%24" )
        , ( '%', "%25" )
        , ( '&', "%26" )
        , ( '\'', "%27" )
        , ( '(', "%28" )
        , ( ')', "%29" )
        , ( '*', "%2A" )
        , ( '+', "%2B" )
        , ( ',', "%2C" )
        , ( '/', "%2F" )
        , ( ':', "%3A" )
        , ( ';', "%3B" )
        , ( '=', "%3D" )
        , ( '?', "%3F" )
        , ( '@', "%40" )
        , ( '[', "%5B" )
        , ( ']', "%5D" )
        ]
