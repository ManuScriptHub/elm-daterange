module Util exposing (..)

import String exposing (fromChar)
import Char exposing (fromCode)
import Color exposing (Color)
import Date exposing (Date,Month,fromString)
import Date.Extra.Format as Format
import Date.Extra.Config.Configs as Configs
import Date.Extra.Config as Config
import Date.Extra.Compare as Compare
 
(=>) : a -> b -> ( a, b )
(=>)  = (,)

charMap : List ( String, number )
charMap = 
  [ "ä"=>228
  , "ö"=>246
  , "ü"=>252 
  , "Ä"=>196 
  , "Ö"=>214
  , "Ü"=>220 
  ]

replace : ( String, Char.KeyCode ) -> String -> String
replace (umlaut, code) src = 
   String.join (fromChar <| fromCode code)  <| String.split umlaut src    

decode : String -> String
decode str = List.foldl replace str charMap  

config : Config.Config
config = Configs.getConfig "de_CH"

parseDate: String->Maybe Date
parseDate str =
   let result = Date.fromString str
   in case result of 
      Err msg -> Nothing
      Ok dt -> Just dt

timeToString: Float->String
timeToString time = Format.format config "%Y-%m-%d %H:%M:%S" <|  Date.fromTime time  

dateToString: Date->String
dateToString dt = Format.format config "%Y-%m-%d" dt

maybeDateToString: Maybe Date->String
maybeDateToString mbDt = 
  Maybe.withDefault "" <| Maybe.map dateToString mbDt

notBefore: Maybe Date->Date->Date
notBefore a b = 
  case a of
    Nothing -> b
    Just target -> 
      if (Compare.is Compare.Before b target) then target else b

notAfter: Maybe Date->Date->Date
notAfter a b = 
  case a of
    Nothing -> b
    Just target -> 
      if (Compare.is Compare.After b target) then target else b

sameDate: Maybe Date->Maybe Date->Bool
sameDate a b = 
  case a of 
    Nothing -> b == Nothing
    Just dateA -> 
      case b of 
        Nothing -> False
        Just dateB -> Compare.is Compare.Same dateA dateB

monthToInt : Month -> number
monthToInt m = case m of
  Date.Jan -> 1
  Date.Feb -> 2
  Date.Mar -> 3
  Date.Apr -> 4
  Date.May -> 5
  Date.Jun -> 6
  Date.Jul -> 7
  Date.Aug -> 8
  Date.Sep -> 9
  Date.Oct -> 10
  Date.Nov -> 11
  Date.Dec -> 12

quarterOf: Month -> Int
quarterOf mon = ceiling ((toFloat <| monthToInt mon) / 3)

isBefore: Maybe Date->Date-> Bool
isBefore when what = 
  case when of 
    Nothing -> False
    Just than -> Compare.is Compare.Before than what

isAfter: Maybe Date->Date-> Bool
isAfter when what = 
  case when of 
    Nothing -> False
    Just than -> Compare.is Compare.After than what

rgbString: { red : Int, green : Int, blue : Int, alpha : Float } -> String
rgbString { red, green, blue, alpha } = 
  "rgba(" ++ (toString red) ++ "," ++ (toString green) ++ "," ++ (toString blue) ++ "," ++ (toString alpha) ++ ")"

colorString: Color->String
colorString color = 
  rgbString (Color.toRgb color)