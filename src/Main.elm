module Main exposing (keyParser, main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
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
    , charDurationMillis : List Int
    , lastTime : Maybe Posix
    , inputText : Maybe String
    }


type Msg
    = KeyDown RawKey
    | GotTimeWhenCharWasTyped Posix
    | InputTextEdit
    | InputTextChanged String
    | InputTextSaved
    | InputTextChangeCancelled
    | RestartClicked
    | NoOp


init : () -> ( Model, Cmd Msg )
init () =
    ( { remainingChars = String.toList "This is a test."
      , acceptedChars = []
      , mistypedChars = []
      , charDurationMillis = []
      , mistakePositions = Set.empty
      , lastTime = Nothing
      , inputText = Nothing
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "eyboard"
    , body =
        [ Html.div
            [ A.style "margin" "auto"
            , A.style "width" "950px"
            ]
            [ Html.div
                [ A.style "font-size" "30px"
                , A.style "font-family" "Courier new"
                , A.style "font-weight" "500"
                , A.style "line-height" "200%"
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
            , case model.inputText of
                Nothing ->
                    Html.div
                        [ A.style "display" "flex"
                        , A.style "justify-content" "flex-end"
                        ]
                        [ Html.button [ E.onClick InputTextEdit ] [ Html.text "Edit" ]
                        , Html.button [ E.onClick RestartClicked, A.id restartBtnId ] [ Html.text "Restart" ]
                        ]

                Just txt ->
                    Html.div []
                        [ Html.textarea
                            [ A.rows 20
                            , A.style "width" "950px"
                            , A.value txt
                            , E.onInput InputTextChanged
                            ]
                            []
                        , Html.div
                            [ A.style "display" "flex"
                            , A.style "justify-content" "flex-end"
                            ]
                            [ Html.button [ E.onClick InputTextSaved ] [ Html.text "Save" ]
                            , Html.button [ E.onClick InputTextChangeCancelled ] [ Html.text "Cancel" ]
                            ]
                        ]
            ]
        ]
    }


restartBtnId : String
restartBtnId =
    "restart"


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
        GotTimeWhenCharWasTyped posix ->
            ( { model
                | lastTime = Just posix
                , charDurationMillis =
                    case model.lastTime of
                        Nothing ->
                            model.charDurationMillis

                        Just prevTime ->
                            (Time.posixToMillis posix - Time.posixToMillis prevTime)
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

        InputTextEdit ->
            ( { model | inputText = Just (String.fromList (model.acceptedChars ++ model.remainingChars)) }
            , Cmd.none
            )

        InputTextChanged str ->
            ( { model | inputText = Just str }, Cmd.none )

        InputTextSaved ->
            ( case model.inputText of
                Just newText ->
                    resetTypingState newText model

                Nothing ->
                    model
            , Cmd.none
            )

        InputTextChangeCancelled ->
            ( { model | inputText = Nothing }, Cmd.none )

        RestartClicked ->
            let
                origText =
                    String.fromList (model.acceptedChars ++ model.remainingChars)
            in
            ( resetTypingState origText model
              -- Blur restart button after it's clicked.
              -- It was getting focused after user started typing,
              -- leading to Space presses causing restart button to be clicked
            , Task.attempt (\_ -> NoOp) (Dom.blur restartBtnId)
            )

        NoOp ->
            ( model, Cmd.none )


resetTypingState : String -> Model -> Model
resetTypingState textToType model =
    { model
        | remainingChars = List.filter isAscii (String.toList (String.trim textToType))
        , acceptedChars = []
        , mistypedChars = []
        , mistakePositions = Set.empty
        , charDurationMillis = []
        , inputText = Nothing
        , lastTime = Nothing
    }


{-| Only keep ASCII chars, as non-ASCII chars are hard to type on the keyboard
-}
isAscii : Char -> Bool
isAscii c =
    Char.toCode c <= 256


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.inputText of
        Nothing ->
            Keyboard.downs KeyDown

        Just _ ->
            Sub.none



-- TODO center the text being typed on the page
-- TODO save stats and progress in local storage
-- TODO move the already written text up
