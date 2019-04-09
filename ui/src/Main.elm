module Main exposing (Model, Msg(..), init, main, update, view)

import Api
import Browser
import Debug
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http



---- MODEL ----


type alias Model =
    { brukere : Maybe (List Api.UserInfo) }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = HentBrukere
    | Hentet (Result Http.Error (List Api.UserInfo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HentBrukere ->
            ( model, Api.getApiUsersListjson Hentet )

        Hentet r ->
            ( { model | brukere = Result.toMaybe r }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
         , h1 [] [ text "Your Elm App is working!" ]
         , button [ onClick HentBrukere ] [ text "Hent brukere" ]
         ]
            ++ (case model.brukere of
                    Just users ->
                        List.map (\u -> p [] [ text u.name ]) users

                    _ ->
                        []
               )
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
