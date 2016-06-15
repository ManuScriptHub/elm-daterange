module Basic exposing (..)

import Html
import Html.App as App
import Maybe exposing (withDefault)
import Time exposing (second, Time)
import Date exposing (Date)
import DateRange
import Date.Extra.Duration as Duration
import DateRangeSelect
import Style


type alias Model =
    { initialized : Bool
    , curTime : Float
    , dateRange : DateRangeSelect.Model
    }


type Msg
    = UpdateClock Time
    | RangeSelectMsg DateRangeSelect.Msg



main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Time.every Time.second UpdateClock)
        }

init : ( Model, Cmd Msg )
init =
    { initialized = False
    , curTime = 0
    , dateRange = DateRangeSelect.init "rng" (DateRange.init Nothing Nothing)
    }  ! []


defaultRange : Time -> ( Maybe Date, Maybe Date )
defaultRange time =
    let
        cur =
            Date.fromTime time
    in
        ( Just cur, Just <| Duration.add Duration.Week 12 cur )


initOrUpdateTime : Time -> Model -> ( Model, Cmd Msg )
initOrUpdateTime time m =
    let
        next =
            { m | curTime = time, initialized = True }
    in
        if m.initialized == False then
            update (RangeSelectMsg <| DateRangeSelect.RangeMsg <| DateRange.SetRange <| defaultRange time) next
        else
            next ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update a m =
    case a of
        UpdateClock time ->
            initOrUpdateTime time m

        RangeSelectMsg msg ->
            let
                newRange =
                    DateRangeSelect.update msg m.dateRange

                rng =
                    newRange.range
            in
                --update (RangeChanged rng) { m | dateRange = newRange }
                { m | dateRange = newRange } ! []


view : Model -> Html.Html Msg
view  m =
    Html.body [ Style.st [ Style.bodyStyle ] ]
        [ Html.header []
            [ Html.h1 [] [ Html.text "strTitle" ]
            , Html.div [] [ App.map RangeSelectMsg (DateRangeSelect.view m.dateRange) ]
            ]
        , Html.main' []
            [ Html.h2 [] [ Html.text "be-bop" ]
            ]
        , Html.footer [ ] [ Html.text "ba-dumm-tssss" ]
        ]
