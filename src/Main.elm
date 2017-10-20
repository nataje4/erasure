module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes as Hattr exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as Lex exposing (..)
import Random.Extra as Rex exposing (..)




---- MODEL ----

type alias ClickableWord =
    { text: String
    , erased: Bool
    , position: Int
    }


type alias Model =
    { clickableText: List ClickableWord
    , textEntered: Bool
    , inputText: String
    , percentRandom: Int
    }


initModel: Model
initModel = 
    { clickableText = []
    , textEntered = False
    , inputText = ""
    , percentRandom = 50
    }

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



textToClickableWords: String -> List ClickableWord
textToClickableWords inputText = 
    let 
        rawWordsArray = String.split " " inputText
    in 
        List.map2 createWord rawWordsArray (List.range 1 <| List.length <| rawWordsArray)



createWord: String -> Int -> ClickableWord
createWord string int = 
    ClickableWord string False int 

---- UPDATE ----

eraseOrBringBack: ClickableWord -> ClickableWord 
eraseOrBringBack word = 
    case word.erased of 
        True -> 
            ClickableWord word.text False word.position
        False -> 
            ClickableWord word.text True word.position

hasPosition: Int -> ClickableWord -> Bool 
hasPosition int word = 
    if word.position == int then 
        True 
    else 
        False 

type Msg
    = ToggleWord ClickableWord
    | MakeTextClickable String 
    | UpdateInputText String 
    | GoBackToTextEntry
    | Randomize Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ToggleWord word -> 
            let 
                newText = Lex.updateAt (word.position - 1) eraseOrBringBack model.clickableText
            in 
                case newText of 
                    Just text -> 
                        ({model | clickableText = text}, Cmd.none) 
                    Nothing -> 
                        Debug.crash "You're trying to toggle a word that doesn't exist."

        MakeTextClickable text -> 

            let 
                clickableText = textToClickableWords model.inputText 
            in 
                ({ model 
                    | clickableText = clickableText
                    , textEntered = True
                    }
                , Cmd.none)

        UpdateInputText text -> 
            ({model | inputText = text}, Cmd.none)

        GoBackToTextEntry -> 
            ({model | textEntered = False}, Cmd.none)

        Randomize int -> 
            model ! [ ]



---- VIEW ----

myStyles: List (Html.Attribute Msg) 
myStyles = 
    List.singleton 
        (style 
            [ ("font-family", "Georgia")
            , ("font-size", "20px")
            , ("display", "inline-block")
            , ("margin", "auto")
            ] )


view : Model -> Html Msg
view model =
    case model.textEntered of 
        False -> 
            enterYourTextScreen model
        True -> 
            div myStyles
                [ 
                    div 
                        [ 
                            style 
                                [ ("width", "75%")
                                , ("display", "inline-block")
                                , ("margin", "auto") 
                                , ("margin-top", "2em")
                                , ("margin-bottom", "1em")
                                ] 
                        ] 
                        (List.map displayClickableWord model.clickableText) 
                    , Html.br [] []
                    , Html.button (onClick GoBackToTextEntry :: appButtonStyle) [Html.text "Enter different text"]

                ] 

appButtonStyle: List (Html.Attribute Msg)
appButtonStyle = 
    style
        [ ("padding", "0 5px") 
        , ("border-radius", "0") 
        , ("border-width", "0") 
        , ("color", "black") 
        , ("background", "transparent") 
        , ("font-family", "'Arial', sans-serif")
        , ("padding-left", "6em")
        , ("padding-right", "6em")  
        ]
    |> List.singleton

enterYourTextScreen: Model -> Html Msg 
enterYourTextScreen model = 
    div (myStyles)
        [ Html.br [] [] , Html.br [] []
        , Html.textarea 
            [ placeholder "Enter your text here"
            , onInput UpdateInputText
            , style [("width", "800px"), ("height", "200px") ]
            ] []
        , Html.br [] [] , Html.br [] [] 
        , Html.button ( onClick (MakeTextClickable model.inputText) :: appButtonStyle) [Html.text "Let's erase stuff!"]
        , Html.button ( onClick (Randomize model.percentRandom) :: appButtonStyle) [Html.text "Randomize!"]
        ]



displayClickableWord: ClickableWord -> Html Msg 
displayClickableWord word = 
    Html.span 
        ([onClick (ToggleWord word),  (style [("color", (wordColor word))]) ])
        [Html.text (word.text ++ " ") ]


wordColor: ClickableWord -> String
wordColor word = 
    case word.erased of 
        True -> 
            "whitesmoke"
        False -> 
            "black"


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
