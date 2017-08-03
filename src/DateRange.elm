module DateRange exposing (..)

import Date exposing (Date)
import List
import Util


type alias Model =
    { from : Maybe Date
    , to : Maybe Date
    }


type Msg
    = SetFrom (Maybe Date)
    | SetTo (Maybe Date)
    | SetRange ( Maybe Date, Maybe Date )


init : Maybe Date -> Maybe Date -> Model
init f t =
    { from = f, to = t }


update : Msg -> Model -> Model
update a m =
    case a of
        SetFrom maybeDate ->
            { m | from = Maybe.map (Util.notAfter m.to) maybeDate }

        SetTo maybeDate ->
            { m | to = Maybe.map (Util.notBefore m.from) maybeDate }

        SetRange ( maybeFrom, maybeTo ) ->
            List.foldl update m [ SetTo maybeTo, SetFrom maybeFrom ]


isValid : Model -> Bool
isValid { from, to } =
    not (from == Nothing || to == Nothing)


uut : Model
uut =
    let
        i0 =
            init Nothing Nothing

        i1 =
            update (SetFrom (Util.parseDate "2016-04-26")) i0

        i2 =
            update (SetFrom (Util.parseDate "2017-01-12")) i1
    in
    i2
