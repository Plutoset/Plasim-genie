******Include file for F77PC
******February 1989


      INTEGER   Black, Blue, Green, Cyan, Red, Magenta,
     +          Brown, LightGrey, LightGray, DarkGrey,
     +          LightBlue, LightGreen, LightCyan, LightRed,
     +          LightMagenta, Yellow, White, DarkGray


      PARAMETER ( Black = 0,       DarkGrey = 8,
     +            Blue = 1,        LightBlue = 9,
     +            Green = 2,       LightGreen = 10,
     +            Cyan = 3,        LightCyan = 11,
     +            Red = 4,         LightRed = 12,
     +            Magenta = 5,     LightMagenta = 13,
     +            Brown = 6,       Yellow = 14,
     +            LightGrey = 7,   White = 15,
     +            LightGray = 7,   DarkGray = 8 )


      INTEGER   BW40Text, C40Text, BW80Text, C80Text,
     +          CGAGraph, BWGraph, CGAHiRes, MonoText,
     +          ECGraph, ECHiRes, EBWGraph, EGAGraph,
     +          VBWGraph, VCGraph, VHiCol, HercGraph


      PARAMETER ( BW40Text = 0,    C40Text = 1,
     +            BW80Text = 2,    C80Text = 3,
     +            CGAGraph = 4,    BWGraph = 5,
     +            CGAHiRes = 6,    MonoText = 7,
     +            ECGraph = 13,    ECHiRes = 14,
     +            EBWGraph = 15,   EGAGraph = 16,
     +            VBWGraph = 17,   VCGraph = 18,
     +            VHiCol = 19,     HercGraph = 20 )


      INTEGER   AdapUnknown, MonoAdap, CGAdap, EGAdap, VGAdap, Hercules


      PARAMETER ( AdapUnknown = 0, MonoAdap = 1,
     +            CGAdap = 2,      EGAdap = 3,
     +            VGAdap = 4,      Hercules = 5 )


      INTEGER   North, South, East, West


      PARAMETER ( North = 0,       East = 90,
     +            South = 180,     West = 270 )


      INTEGER   Com1, Com2, Com3


      PARAMETER ( Com1 = 0,        Com2 = 1,
     +            Com3 = 2 )


******Function type declarations

c      INTEGER*2 Adaptor, Pixel, ScreenMode, WhereX, WhereY, EditReturn
c      INTEGER*2 AttrXY, Heading, KeyFlags, XCor, YCor
c      CHARACTER CharXY*1
c      LOGICAL*1 EscapeKey, KeyPressed

******End of F77PC include file.
