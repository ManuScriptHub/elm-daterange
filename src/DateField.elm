module DateField exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes as HA exposing (for, id, max, min, name, style, type_, value)
import Html.Events as HE exposing (on, targetValue)
import Util exposing ((=>))


type alias Model =
    { id : String
    , minValue : Maybe Date
    , maxValue : Maybe Date
    , dateValue : Maybe Date
    }


type Msg
    = Changed String
    | SetVal (Maybe Date)
    | SetMin (Maybe Date)
    | SetMax (Maybe Date)


init : String -> Model
init id =
    { id = id
    , dateValue = Nothing
    , minValue = Nothing
    , maxValue = Nothing
    }


view : Model -> Html Msg
view m =
    input
        [ HA.type_ "date"
        , HA.id m.id
        , HA.name m.id
        , HA.min (Util.maybeDateToString m.minValue)
        , HA.max (Util.maybeDateToString m.maxValue)
        , HA.value (Util.maybeDateToString m.dateValue)
        , HE.onInput Changed
        ]
        []


update : Msg -> Model -> Model
update a m =
    case a of
        SetVal maybeDate ->
            { m | dateValue = maybeDate }

        Changed str ->
            { m | dateValue = Util.parseDate str }

        SetMin maybeDate ->
            let
                newValue =
                    Maybe.map (Util.notBefore maybeDate) m.dateValue
            in
            update (SetVal newValue) { m | minValue = maybeDate }

        SetMax maybeDate ->
            let
                newValue =
                    Maybe.map (Util.notAfter maybeDate) m.dateValue
            in
            update (SetVal newValue) { m | maxValue = maybeDate }
