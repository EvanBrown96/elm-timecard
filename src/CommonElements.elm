module CommonElements exposing (timerSegment)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border


rowHeight = 80

defaultSpacing = spacing 10

defaultPadding = padding 10

primaryColor = BG.color <| rgb255 220 220 220

secondaryColor = BG.color <| rgb255 119 136 153

timerSegmentBasicAttrib = [ height <| px rowHeight, defaultPadding ]

borderColor = Border.color <| rgb255 50 50 50

borderWidth = 4

borderRadius = rowHeight//10

borderBasicAttrib = [ Border.solid, borderColor]

leftBorder = borderBasicAttrib ++ [ Border.widthEach { bottom = borderWidth
                                                     , left   = borderWidth
                                                     , right  = 0
                                                     , top    = borderWidth
                                                     }
                                  , Border.roundEach { topLeft     = borderRadius
                                                     , topRight    = 0
                                                     , bottomRight = 0
                                                     , bottomLeft  = borderRadius
                                                     }
                                  ]

middleBorder = borderBasicAttrib ++ [ Border.widthXY 0 borderWidth ]

rightBorder = borderBasicAttrib ++ [ Border.widthEach { bottom = borderWidth
                                                      , left   = 0
                                                      , right  = borderWidth
                                                      , top    = borderWidth
                                                      }
                                   , Border.roundEach { topLeft     = 0
                                                      , topRight    = borderRadius
                                                      , bottomRight = borderRadius
                                                      , bottomLeft  = 0
                                                      }
                                   ]

timerSegment elem1 elem2 elem3 elem4 =
  row [ defaultPadding, width fill ]
    [ el (timerSegmentBasicAttrib ++ leftBorder ++ [ secondaryColor, width <| px <| rowHeight ]) elem1
    , el (timerSegmentBasicAttrib ++ middleBorder ++ [ primaryColor, width <| fillPortion 1]) elem2
    , el (timerSegmentBasicAttrib ++ middleBorder ++ [ primaryColor, width <| fillPortion 1]) elem3
    , el (timerSegmentBasicAttrib ++ rightBorder ++ [ secondaryColor, width <| px <| rowHeight ]) elem4
    ]
