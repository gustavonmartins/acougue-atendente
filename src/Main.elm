module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    {disponiveis: List Atendente,
    emAtendimento: List Atendente}

type alias Atendente = String


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel = 
    {disponiveis = ["Pedro", "Joao"]
    ,emAtendimento = []}

---- UPDATE ----


type Msg
    = NoOp
    | AtendenteLivre Atendente



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
       AtendenteLivre atendente->
        ( {model | 
            emAtendimento = List.reverse(atendente :: List.reverse(model.emAtendimento))
            ,disponiveis = List.filter (\nome -> nome /= atendente) model.disponiveis}, Cmd.none )
       NoOp ->
        ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
    [
        div [] [
            text "DISPONIVEIS"
            , ul []                 (List.map (\name -> li [onClick (AtendenteLivre name)] [text name]) model.disponiveis)
        ]
        ,div [] [
            text "EM ATENDIMENTO"
            , ul [] (List.map (\name -> li [] [text name]) model.emAtendimento)
        ]
        , div [] [text ("FINALIZAR")]
    ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
