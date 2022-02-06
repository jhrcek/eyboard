module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as A
import Keyboard exposing (RawKey)


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
    }


type Msg
    = KeyDown RawKey


init : () -> ( Model, Cmd Msg )
init () =
    ( { remainingChars = String.toList "But in a solitary life, there are rare moments when another soul dips near yours, as stars once a year brush the earth. Such a constellation was he to me."
      , acceptedChars = []
      , mistypedChars = []
      }
    , Cmd.none
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
        ]
    }


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


keyParser =
    Keyboard.oneOf
        [ Keyboard.characterKeyOriginal
        , Keyboard.editingKey
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown rawKey ->
            case keyParser rawKey of
                Just Keyboard.Backspace ->
                    ( { model | mistypedChars = List.take (List.length model.mistypedChars - 1) model.mistypedChars }
                    , Cmd.none
                    )

                Just (Keyboard.Character c) ->
                    case model.remainingChars of
                        b :: rest ->
                            if c == String.fromChar b then
                                ( { model
                                    | acceptedChars = model.acceptedChars ++ String.toList c
                                    , remainingChars = rest
                                  }
                                , Cmd.none
                                )

                            else
                                ( { model | mistypedChars = model.mistypedChars ++ String.toList c }
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
