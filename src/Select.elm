module Select exposing (..)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Maybe exposing (withDefault)
import String exposing (toInt)
import Style


type alias Model a =
    { id : String
    , allOptionLabel : Maybe String
    , htmlFunc : a -> Html
    , data : List ( Int, a )
    , selectedIndex : Maybe Int
    }


type Msg a
    = Select (Maybe Int)
    | SelectionChanged String
    | SetData (List a)


init : String -> Maybe String -> (a -> Html) -> Model a
init id allLabel htmlFunc =
    Model id allLabel htmlFunc [] Nothing


update : Msg a -> Model a -> Model a
update act model =
    case act of
        SetData list ->
            { model | data = List.indexedMap (,) list }

        Select mbItem ->
            { model | selectedIndex = mbItem }

        SelectionChanged str ->
            (update (Select (Result.toMaybe (String.toInt str))) model)


data : Model a -> List a
data m =
    List.map snd m.data


map : (a -> a) -> Model a -> Model a
map f m =
    update (SetData <| List.map (\( id, obj ) -> f obj) m.data) m



--selected: Model a -> Maybe a
--selected m =
--  case m.selectedIndex of
--    Nothing -> Nothing
--    Just idx -> List.head <| List.map snd <| List.filter (\(i,_) -> i == idx ) m.data


selected : Model a -> List a
selected m =
    case m.selectedIndex of
        Nothing ->
            List.map snd m.data

        Just idx ->
            List.map snd <| List.filter (\( i, _ ) -> i == idx) m.data


onInput : Signal.Address b -> (String -> b) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))


toOption : Model a -> ( Int, a ) -> Html
toOption m ( id, e ) =
    option [ value <| toString id ] [ m.htmlFunc e ]


options : Model a -> List Html
options m =
    let
        allOption =
            case m.allOptionLabel of
                Nothing ->
                    []

                Just label ->
                    [ option [ value "*" ] [ text label ] ]
    in
        allOption ++ (List.map (toOption m) m.data)


view : Signal.Address (Msg a) -> Model a -> Html
view addr m =
    let
        idx =
            Maybe.map toString m.selectedIndex
    in
        Html.select
            [ onInput addr SelectionChanged
            , st [ adjustSelectStyle ]
            , value (withDefault "*" idx)
            ]
            (options m)
