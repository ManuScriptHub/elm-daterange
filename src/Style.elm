module Style exposing (..)

import Color exposing (Color)
import Html
import Html.Attributes as HA
import Util exposing ((=>))


type alias StyleDef =
    List ( String, String )


boxSize =
    25


boxPx i =
    toString (i * boxSize) ++ "px"


st : List StyleDef -> Html.Attribute a
st l =
    HA.style (List.concatMap identity l)


border : String -> StyleDef
border clr =
    [ "border" => ("1px solid " ++ clr) ]


box =
    [ "height" => boxPx 1 ]


bodyStyle : StyleDef
bodyStyle =
    [ "width" => "960px"
    , "margin" => "auto"
    ]


flexHoriz : StyleDef
flexHoriz =
    [ "display" => "flex"
    , "flexDirection" => "row"
    , "flexWrap" => "nowrap"
    ]


flexVert : StyleDef
flexVert =
    [ "display" => "flex"
    , "flexDirection" => "column"
    , "flexWrap" => "nowrap"
    ]


flexEnd : StyleDef
flexEnd =
    [ "alignSelf" => "flex-end"
    ]


fixFlex : StyleDef
fixFlex =
    [ "flex" => "0 0 auto"
    ]


autoFlex : StyleDef
autoFlex =
    [ "flex" => "auto"
    ]


adjustSelectStyle : StyleDef
adjustSelectStyle =
    [ "padding" => "0.2em"
    ]


marginLeft10 : StyleDef
marginLeft10 =
    [ "marginLeft" => "10px"
    ]


debugStyle : StyleDef
debugStyle =
    [ "marginTop" => "120px"
    , "backgroundColor" => "#EEEEEE"
    , "border" => "1px solid #050505"
    , "padding" => "40px"
    ]


colStyle : StyleDef
colStyle =
    fixFlex
        ++ flexVert
        ++ [ "width" => "240px"
           , "borderRight" => "1px dashed gray"
           ]


empColStyle : StyleDef
empColStyle =
    colStyle
        ++ [ "width" => "100px"
           ]


sklColStyle : StyleDef
sklColStyle =
    colStyle
        ++ [ "width" => "70px"
           ]


actColStyle : StyleDef
actColStyle =
    colStyle
        ++ [ "width" => "160px"
           ]


slotStyle : StyleDef
slotStyle =
    headerGridCellStyle
        ++ [ "width" => boxPx 1
           , "flex" => "0 0 auto"
           ]


assStyle : Color -> StyleDef
assStyle clr =
    headerGridCellStyle
        ++ [ "backgroundColor" => Util.colorString clr
           , "fontSize" => "10px"
           , "fontWeight" => "bold"
           ]


rowStyle : StyleDef
rowStyle =
    [--"borderTop" => "1px dashed gray"
    ]


empRowStyle : StyleDef
empRowStyle =
    [ "borderBottom" => "1px dashed gray"
    ]


itemStyle : StyleDef
itemStyle =
    fixFlex
        ++ [ "borderTop" => "1px dashed green"
           ]


empItemStyle : StyleDef
empItemStyle =
    itemStyle
        ++ [ "fontWeight" => "bold"
           ]


sklItemStyle : StyleDef
sklItemStyle =
    itemStyle
        ++ []


actItemStyle : StyleDef
actItemStyle =
    itemStyle
        ++ [ "fontStyle" => "italics"
           , "height" => boxPx 1
           ]


headerGridCellStyle : StyleDef
headerGridCellStyle =
    [ "height" => boxPx 1
    , "textAlign" => "center"
    ]


headerBoxCellStyle : StyleDef
headerBoxCellStyle =
    headerGridCellStyle
        ++ [ "width" => boxPx 1
           , "flex" => "0 0 auto"
           ]


headerMonthCellStyle : Int -> Int -> StyleDef
headerMonthCellStyle month count =
    let
        clr =
            if month % 2 == 0 then
                "#f1ae87"
            else
                "#ffe3d3"
    in
    headerGridCellStyle
        ++ [ "backgroundColor" => clr
           , "flex" => "auto"
           , "overflow" => "hidden"
           , "textOverflow" => "ellipsis"

           --, "width" => (toString (count * boxWidth) ++ "px")
           ]


headerWeekCellStyle : Int -> StyleDef
headerWeekCellStyle wk =
    let
        clr =
            if wk % 2 == 0 then
                "#a9d190"
            else
                "#dfeddb"
    in
    headerBoxCellStyle
        ++ [ "backgroundColor" => clr ]


storageMsgStyle : StyleDef
storageMsgStyle =
    [ "marginTop" => "10px"
    , "color" => "#EE4444"
    , "border" => "1px solid #050505"
    , "padding" => "5px"
    ]


copyrightStyle : StyleDef
copyrightStyle =
    [ "marginTop" => "10px" ]
