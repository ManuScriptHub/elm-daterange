module DateRangeSelect exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as HA exposing (for)
import DateRange
import DateField
import Style exposing (st, flexVert, flexHoriz, marginLeft10)


type alias Model =
    { id : String
    , range : DateRange.Model
    , fromField : DateField.Model
    , toField : DateField.Model
    }


type Msg
    = FromFieldMsg DateField.Msg
    | ToFieldMsg DateField.Msg
    | RangeMsg DateRange.Msg


init : String -> DateRange.Model -> Model
init id rng =
    { id = id
    , range = rng
    , fromField = DateField.init (id ++ "_rFrom")
    , toField = DateField.init (id ++ "_rTo")
    }


update : Msg -> Model -> Model
update a m =
    case a of
        RangeMsg msg ->
            let
                newRange =
                    DateRange.update msg m.range
            in
                { m
                    | range = newRange
                    , fromField = List.foldl DateField.update m.fromField [ DateField.SetMax newRange.to, DateField.SetVal newRange.from ]
                    , toField = List.foldl DateField.update m.toField [ DateField.SetVal newRange.to, DateField.SetMin newRange.from ]
                }

        FromFieldMsg msg ->
            let
                next =
                    { m | fromField = DateField.update msg m.fromField }
            in
                update (RangeMsg <| DateRange.SetFrom next.fromField.dateValue) next

        ToFieldMsg msg ->
            let
                next =
                    { m | toField = DateField.update msg m.toField }
            in
                update (RangeMsg <| DateRange.SetTo next.toField.dateValue) next


viewField : DateField.Model -> (DateField.Msg -> Msg) -> String -> Html Msg
viewField df act str =
    Html.div [ st [ flexVert, marginLeft10 ] ]
        [ Html.label [ for df.id ] [ Html.text str ]
        , App.map act (DateField.view df)
        ]


view : Model -> Html Msg
view m =
    Html.div [ st [ flexHoriz ] ]
        <| List.map3 viewField
            [ m.fromField, m.toField ]
            [ FromFieldMsg, ToFieldMsg ]
            [ "Von", "Bis" ]
