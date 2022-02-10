module Main exposing (keyParser, main)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as A
import Keyboard exposing (RawKey)
import Set exposing (Set)
import Task
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { acceptedChars : List Char
    , remainingChars : List Char
    , mistypedChars : List Char
    , mistakePositions : Set Int

    -- TODO should this be zipped with accepted chars?
    , charDurationMillis : List Int
    , lastTime : Posix
    }


type Msg
    = KeyDown RawKey
    | GotInitialTime Posix
    | GotTimeWhenCharWasTyped Posix


init : () -> ( Model, Cmd Msg )
init () =
    ( { remainingChars = String.toList "Hello, this is a test."
      , acceptedChars = []
      , mistypedChars = []
      , charDurationMillis = []
      , mistakePositions = Set.empty
      , lastTime = Time.millisToPosix 0
      }
    , Task.perform GotInitialTime Time.now
    )


view : Model -> Document Msg
view model =
    { title = "eyboard"
    , body =
        [ Html.div
            [ A.style "font-size" "30px"
            , A.style "font-family" "pt-mono"
            , A.style "font-weight" "500"
            ]
            (Html.span [ A.style "color" "#95c590" ] [ Html.text (String.fromList model.acceptedChars) ]
                :: (case model.mistypedChars of
                        [] ->
                            remainingCharsView model.remainingChars

                        _ ->
                            let
                                mistypedLength =
                                    List.length model.mistypedChars
                            in
                            Html.span
                                [ A.style "color" "#803333"
                                , A.style "background-color" "#f0a3a3"
                                ]
                                [ Html.text (String.fromList <| List.take mistypedLength model.remainingChars) ]
                                :: (remainingCharsView <| List.drop mistypedLength model.remainingChars)
                   )
            )
        , Html.div []
            [ Html.text <| String.fromList model.mistypedChars ]
        , if List.isEmpty model.remainingChars then
            Html.div []
                [ Html.text <| "WPM: " ++ String.fromInt (calculateWPM model.charDurationMillis)
                , Html.br [] []

                -- TODO proper formatting of decimal
                , Html.text <|
                    "Success rate: "
                        ++ String.left 5
                            (String.fromFloat (calculateSuccessRate model.mistakePositions model.acceptedChars))
                ]

          else
            Html.text ""
        ]
    }


calculateSuccessRate : Set Int -> List Char -> Float
calculateSuccessRate mistakeIndices acceptedChars =
    100 * (1 - toFloat (Set.size mistakeIndices) / toFloat (List.length acceptedChars))


calculateWPM : List Int -> Int
calculateWPM charDurations =
    let
        chars =
            toFloat (List.length charDurations)

        millis =
            toFloat (List.sum charDurations)
    in
    -- word has 5 chars, second has 1000 millis, minute has 60 seconds;  so 60 * 1000 / 5 = 12000
    round <| 12000 * chars / millis


remainingCharsView : List Char -> List (Html Msg)
remainingCharsView chars =
    case chars of
        [] ->
            []

        c :: rest ->
            [ Html.span
                [ A.style "color" "#3295db"
                , A.style "text-decoration" "underline"
                ]
                [ Html.text (String.fromChar c) ]
            , Html.span [] [ Html.text (String.fromList rest) ]
            ]


keyParser : RawKey -> Maybe Keyboard.Key
keyParser =
    Keyboard.oneOf
        [ Keyboard.characterKeyOriginal
        , Keyboard.editingKey
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialTime posix ->
            ( { model | lastTime = posix }, Cmd.none )

        GotTimeWhenCharWasTyped posix ->
            ( { model
                | lastTime = posix
                , charDurationMillis =
                    (Time.posixToMillis posix - Time.posixToMillis model.lastTime)
                        :: model.charDurationMillis
              }
            , Cmd.none
            )

        KeyDown rawKey ->
            case keyParser rawKey of
                Just Keyboard.Backspace ->
                    -- TODO should we allow backspacing already typed chars?
                    ( { model | mistypedChars = List.take (List.length model.mistypedChars - 1) model.mistypedChars }
                    , Cmd.none
                    )

                Just (Keyboard.Character c) ->
                    case model.remainingChars of
                        b :: rest ->
                            if List.isEmpty model.mistypedChars && c == String.fromChar b then
                                ( { model
                                    | acceptedChars = model.acceptedChars ++ String.toList c
                                    , remainingChars = rest
                                  }
                                  -- We only measure time after successfully typed character
                                , Task.perform GotTimeWhenCharWasTyped Time.now
                                )

                            else
                                ( { model
                                    | mistypedChars = model.mistypedChars ++ String.toList c
                                    , mistakePositions = Set.insert (List.length model.acceptedChars) model.mistakePositions
                                  }
                                , Cmd.none
                                )

                        [] ->
                            ( model, Cmd.none )

                _ ->
                    -- TODO do we need to handle other chars?
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



-- TODO count mistakes an accuracy
-- TODO how to count if mistake is made multiple times for one letter?
-- TODO when typing finished, show summary stats + error rate
