module Main exposing (Model, Msg(..), init, main, update, view)

import Api
import Browser
import Debug
import Html exposing (Html, br, button, div, h1, img, input, label, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import Http



---- MODEL ----


type alias Model =
    { brukere : Maybe (List Api.UserInfo)
    , createUserForm : Api.CreateUserForm
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing (Api.CreateUserForm "" ""), Cmd.none )



---- UPDATE ----


type Msg
    = HentBrukere
    | LagNyBruker
    | Hentet (Result Http.Error (List Api.UserInfo))
    | Done (Result Http.Error Api.UserInfo)
    | UpdateName String
    | UpdateSurname String


setName : String -> Api.CreateUserForm -> Api.CreateUserForm
setName newName form =
    { form | name = newName }


setSurname : String -> Api.CreateUserForm -> Api.CreateUserForm
setSurname newSurname form =
    { form | surname = newSurname }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HentBrukere ->
            ( model, Api.getApiUsersListjson Hentet )

        Hentet r ->
            ( { model | brukere = Result.toMaybe r }, Cmd.none )

        UpdateName n ->
            ( { model | createUserForm = setName n model.createUserForm }, Cmd.none )

        UpdateSurname n ->
            ( { model | createUserForm = setSurname n model.createUserForm }, Cmd.none )

        LagNyBruker ->
            ( { model | createUserForm = Api.CreateUserForm "" "" }, Api.postApiUsersCreatejson model.createUserForm Done )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
         , h1 [] [ text "Your Elm App is working!" ]
         , div [] [ label [] [ text "Name: " ], input [ onInput UpdateName ] [] ]
         , div [] [ label [] [ text "Surname: " ], input [ onInput UpdateSurname ] [] ]
         , button [ onClick LagNyBruker ] [ text "Lag ny bruker" ]
         , br [] []
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
