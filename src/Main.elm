module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    {disponiveis: List Atendente
    ,emAtendimento: List Atendente
    ,emFinalizacao: Bool
    ,atendenteFinalizando: Maybe Atendente}

type alias Atendente = String


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel = 
    {disponiveis = ["Pedro", "Joao", "Carlos", "Ivan"]
    ,emAtendimento = []
    ,emFinalizacao = False
    ,atendenteFinalizando = Nothing}

---- UPDATE ----


type Msg
    = NoOp
    | AtendenteLivre Atendente
    --Finalizacoes
    | ComecaFinalizacao
    | SelecionaFinalizar Atendente
    | ConfirmaFinalizar
    | CancelaFinalizacao



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
       AtendenteLivre atendente->
        ( {model | 
            emAtendimento = List.reverse(atendente :: List.reverse(model.emAtendimento))
            ,disponiveis = List.filter (\nome -> nome /= atendente) model.disponiveis}, Cmd.none )

       ComecaFinalizacao ->
        ( {model | emFinalizacao = True}, Cmd.none )
       
       SelecionaFinalizar atendente->
        ( {model | atendenteFinalizando = Just atendente}, Cmd.none )
       
       ConfirmaFinalizar->
        case model.atendenteFinalizando of
           Just atendente ->
            ( {model | disponiveis = atendente :: model.disponiveis
            , atendenteFinalizando = Nothing
            ,emAtendimento = List.filter (\nome -> nome /= atendente) model.emAtendimento
            , emFinalizacao = False}, Cmd.none )
           Nothing -> 
            (model, Cmd.none)
       
       CancelaFinalizacao->
        case model.atendenteFinalizando of
            Just atendente ->
                ( {model | atendenteFinalizando = Nothing
                , emFinalizacao = False}, Cmd.none )
            Nothing ->
                ({model | emFinalizacao = False}, Cmd.none)
       
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
        , div [onClick ComecaFinalizacao] [text ("FINALIZAR")]
        , viewFinalizar model
        , viewConfirmaFinalizar model
    ]

viewConfirmaFinalizar : Model -> Html Msg
viewConfirmaFinalizar model =
    case model.atendenteFinalizando of
        Nothing ->
            text ""
        Just atendente ->
            div [] 
                [
                    text ("Confirma finalizacao de " ++ atendente)
                    , p [onClick ConfirmaFinalizar] [text "CONFIRMA"]
                    , p [onClick CancelaFinalizacao] [text "CANCELA"]
                ]


viewFinalizar : Model -> Html Msg
viewFinalizar model =
    case  model.emFinalizacao of
       True ->
            div []  [
                div [] [
                         ul [] (List.map (\name -> li [onClick (SelecionaFinalizar name)] [text name]) model.emAtendimento)
                        ]
            ]
       False -> 
            text ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
