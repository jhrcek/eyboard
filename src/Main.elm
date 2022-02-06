module Main exposing (main)

import Browser exposing (Document)
import Html
import Html.Attributes as A
import Html.Events as E


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
    , wordInput : String
    }


type Msg
    = InputUpdated String


init : () -> ( Model, Cmd Msg )
init () =
    ( { acceptedChars = []
      , remainingChars = String.toList "But in a solitary life, there are rare moments when another soul dips near yours, as stars once a year brush the earth. Such a constellation was he to me."
      , wordInput = ""
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
                :: (case model.remainingChars of
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
                   )
            )
        , Html.div []
            [ Html.input
                [ A.type_ "text"
                , E.onInput InputUpdated
                , A.value model.wordInput
                ]
                []
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUpdated str ->
            case ( List.head <| List.reverse <| String.toList str, model.remainingChars ) of
                ( Just a, b :: rest ) ->
                    if a == b then
                        ( { model
                            | acceptedChars = model.acceptedChars ++ [ a ]
                            , remainingChars = rest
                            , wordInput =
                                if a == ' ' then
                                    ""

                                else
                                    model.wordInput ++ String.fromChar a
                          }
                        , Cmd.none
                        )

                    else
                        -- TODO display bad chars and allow backspacing them
                        ( model, Cmd.none )

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

                ( _, [] ) ->
                    ( { model | wordInput = "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
