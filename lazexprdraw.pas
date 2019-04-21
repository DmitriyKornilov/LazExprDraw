//Copyright © 2002 Anton Grigoriev. Contacts: <grigorievab@mail.ru>
//Copyright © 2019 Dmitriy Kornilov. Contacts: <dakor2017@yandex.ru>
//License: https://opensource.org/licenses/MIT

unit LazExprDraw;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, LazUTF8;

const
  DefaultFontName = 'XITS';

  esApproxLess     = 8818;
  esApproxGreater  = 8819;
  esLessOrEqual    = 8804;
  esGreaterOrEqual = 8805;
  esMuchLess       = 8810;
  esMuchGreater    = 8811;
  esLess           = 60;
  esGreater        = 62;
  esPlus           = 43;
  esPlusMinus      = 177;
  esMinus          = 8722;
  esMinusPlus      = 8723;
  esEqual          = 61;
  esNotEqual       = 8800;
  esIdentical      = 8801;
  esNotIdentical   = 8802;
  esApproxEqual    = 8773;
  esAlmostEqual    = 8776;
  esMultiply       = 183;
  esCrossMultiply  = 215;
  esSlash          = 47;
  esBackSlash      = 92;
  esDotsDivide     = 247;
  esSemicolon      = 59;
  esColon          = 58;
  esComma          = 44;
  esDot            = 46;
  esTilde          = 126;
  esParallel       = 8741;
  esNotParallel    = 8742;
  esPerpendicular  = 8869;
  esEmptySet       = 8709;
  esExists         = 8707;
  esNotExists      = 8708;
  esBelongs        = 8712;
  esNotBelongs     = 8713;
  esIntersection   = 8745;
  esUnion          = 8746;
  esAnd            = 8896;
  esOr             = 8897;
  esNot            = 172;
  esXor            = 8891;
  esSubSet         = 8834;
  esNotSubSet      = 8836;
  esSuperSet       = 8835;
  esNotSuperSet    = 8837;
  esProportional   = 8733;
  esInfinity       = 8734;
  esNatural        = 8469;
  esReal           = 8477;
  esRational       = 8474;
  esEntire         = 8484;
  esComplex        = 8450;
  esQuaternion     = 8461;
  esProjective     = 8473;
  esSum            = 8721;
  esProd           = 8719;
  esInt            = 8747;
  esCirc           = 8750;
  esSurf           = 8751;
  esVolume         = 8752;
  esStroke         = 39;
  esPartDiff       = 8706;
  esAngle          = 8736;
  esNabla          = 8711;
  esForAll         = 8704;
  esPlanck         = 295;
  esLambdaSpec     = 411;
  esArc            = 9181;
  esDivide         = 8739;
  esNotDivide      = 8740;
  esAsterisk       = 8727;
  esTriangle       = 9651;
  esQuadrate       = 9633;
  esRectangle      = 9645;
  esCircle         = 9675;
  esParallelogram  = 9649;
  esRhomb          = 9671;
  esBegin          = 9664;
  esEnd            = 9654;
  esSimilar        = 8766;
  esDegree         = 176;

  esPlusCircle     = 8853;
  esMinusCircle    = 8854;
  esCrossCircle    = 8855;
  esSlashCircle    = 8856;
  esDotCircle      = 8857;
  esCircleCircle   = 8858;
  esAsteriskCircle = 8859;
  esEqualCircle    = 8860;

  esEllipsis       = 8230;
  esEllipsVert     = 8942;
  esEllipsHoriz    = 8943;
  esEllipsDiagUp   = 8944;
  esEllipsDiagDown = 8945;

  esLeftAngleBracket  = 10216;
  esRightAngleBracket = 10217;
  esLeftFloorBracket  = 8970;
  esRightFloorBracket = 8971;
  esLeftCeilBracket   = 8968;
  esRightCeilBracket  = 8969;

  esArrowLeft         = 8592;
  esArrowRight        = 8594;
  esArrowUp           = 8593;
  esArrowDown         = 8595;
  esArrowLeftRight    = 8596;
  esArrowUpDown       = 8597;
  esArrowLeftUp       = 8598;
  esArrowRightUp      = 8599;
  esArrowRightDown    = 8600;
  esArrowLeftDown     = 8601;
  esArrowLeftNot      = 8602;
  esArrowRightNot     = 8603;
  esArrowLeftRightNot = 8622;

  esDoubleArrowLeft         = 8656;
  esDoubleArrowRight        = 8658;
  esDoubleArrowUp           = 8657;
  esDoubleArrowDown         = 8659;
  esDoubleArrowLeftRight    = 8660;
  esDoubleArrowUpDown       = 8661;
  esDoubleArrowLeftUp       = 8662;
  esDoubleArrowRightUp      = 8663;
  esDoubleArrowRightDown    = 8664;
  esDoubleArrowLeftDown     = 8665;
  esDoubleArrowLeftNot      = 8653;
  esDoubleArrowRightNot     = 8655;
  esDoubleArrowLeftRightNot = 8654;

  esHarpArrowsLeftRight = 8651;
  esHarpArrowsRightLeft = 8652;

  esTwoArrowsRightLeft = 8644;
  esTwoArrowsLeftRight = 8646;
  esTwoArrowsUpDown    = 8645;
  esTwoArrowsDownUp    = 8693;
  esTwoArrowsLeft      = 8647;
  esTwoArrowsRight     = 8649;
  esTwoArrowsUp        = 8648;
  esTwoArrowsDown      = 8650;
  esThreeArrows        = 8694;

  esBarArrowLeft  = 8612;
  esBarArrowRight = 8614;
  esBarArrowUp    = 8613;
  esBarArrowDown  = 8615;

  esColonEqual         = 8788;
  esEqualColon         = 8789;
  esDotEqual           = 8784;
  esDotsEqualCenter    = 8785;
  esDotsEqualLeftRight = 8786;
  esDotsEqualRightLeft = 8787;

  esAlphaBig   = 913;  esAlphaSmall   = esAlphaBig+32;
  esBetaBig    = 914;  esBetaSmall    = esBetaBig+32;
  esGammaBig   = 915;  esGammaSmall   = esGammaBig+32;
  esDeltaBig   = 916;  esDeltaSmall   = esDeltaBig+32;
  esEpsilonBig = 917;  esEpsilonSmall = esEpsilonBig+32;
  esZetaBig    = 918;  esZetaSmall    = esZetaBig+32;
  esEtaBig     = 919;  esEtaSmall     = esEtaBig+32;
  esThetaBig   = 920;  esThetaSmall   = esThetaBig+32;
  esIotaBig    = 921;  esIotaSmall    = esIotaBig+32;
  esKappaBig   = 922;  esKappaSmall   = esKappaBig+32;
  esLambdaBig  = 923;  esLambdaSmall  = esLambdaBig+32;
  esMuBig      = 924;  esMuSmall      = esMuBig+32;
  esNuBig      = 925;  esNuSmall      = esNuBig+32;
  esXiBig      = 926;  esXiSmall      = esXiBig+32;
  esOmicronBig = 927;  esOmicronSmall = esOmicronBig+32;
  esPiBig      = 928;  esPiSmall      = esPiBig+32;
  esRhoBig     = 929;  esRhoSmall     = esRhoBig+32;
  esSigmaBig   = 931;  esSigmaSmall   = esSigmaBig+32;
  esTauBig     = 932;  esTauSmall     = esTauBig+32;
  esUpsilonBig = 933;  esUpsilonSmall = esUpsilonBig+32;
  esPhiBig     = 934;  esPhiSmall     = esPhiBig+32;
  esChiBig     = 935;  esChiSmall     = esChiBig+32;
  esPsiBig     = 936;  esPsiSmall     = esPsiBig+32;
  esOmegaBig   = 937;  esOmegaSmall   = esOmegaBig+32;
  esThetaOther = 977;
  esSigmaOther = 962;

  tcWidth        = 1 shl 0;
  tcHeight       = 1 shl 1;
  tcPowerXPos    = 1 shl 2;
  tcPowerYPos    = 1 shl 3;
  tcIndexXPos    = 1 shl 4;
  tcIndexYPos    = 1 shl 5;
  tcCapDX        = 1 shl 6;
  tcCapDY        = 1 shl 7;
  tcMidLineUp    = 1 shl 8;
  tcMidlineDn    = 1 shl 9;
  tcCellSize     = 1 shl 10;
  tcSymbolWidth  = 1 shl 10;
  tcSymbolHeight = 1 shl 11;

  efLeft          = 1;
  efRight         = 2;
  efNegative      = 4;
  efRoundBrackets = 24;
  efBrackets      = 16;
  efNumber        = 32;

type
  TExprOrigin       = (eoTop,eoBottom);
  TExprHorAlign     = (ehLeft,ehCenter,ehRight);
  TExprVertAlign    = (evTop,evCenter,evBottom);
  TExprBracketStyle = (ebNone,ebRound,ebSquare,ebFigure,ebModule,ebNorm,ebAngle,ebFloor,ebCeil);
  TExprCapStyle     = (ecPoints,ecVector,ecCap,ecTilde,ecLine);

  { TExprClass }

  TExprClass = class (TObject)
  private
    FParent: TExprClass;
    FNext: TExprClass;
    FColor: TColor;
    FFont: TFont;
    FWidth,FHeight,FMidLineUp,FMidLineDn,
    FPowerXPos,FPowerYPos,FIndexXPos,
    FIndexYPos,FCapDY,FCapDXLeft,FCapDXRight: Integer;
    FCanvas: TCanvas;
    FToChange: Cardinal;
    procedure SetNext(const AValue: TExprClass);
    procedure SetLineWidth;
    procedure SetFont(const ANewFont: TFont);
    procedure SetCanvas(const AValue: TCanvas);
    procedure SetColor(const AValue: TColor);
    procedure SetParent(const AValue: TExprClass);
    procedure FontNotify(Sender: TObject);
    function GetColor: TColor;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetTextHeight: Integer;
    function GetMidLineUp: Integer;
    function GetMidLineDn: Integer;
    function GetPowerXPos: Integer;
    function GetPowerYPos: Integer;
    function GetIndexXPos: Integer;
    function GetIndexYPos: Integer;
    function GetCapDXLeft: Integer;
    function GetCapDXRight: Integer;
    function GetCapDY: Integer;
  protected
    FWX,FWY: Integer;
    FRWX,FRWY: Extended;
    property Parent: TExprClass read FParent write SetParent;
    procedure DynaSetFont; virtual;
    procedure DynaSetCanvas; virtual;
    procedure SetCanvasFont; virtual;
    procedure SetPenAndBrush;
    procedure ConvertCoords(var X,Y: Integer; const AHorAlign: TExprHorAlign; const AVertAlign: TExprVertAlign);
    procedure AssignCanvas(const AValue: TCanvas; const AWX,AWY: Integer; const ARWX,ARWY: Extended);
    procedure AssignFont(const ANewFont: TFont; const EWX,EWY: Integer; const ERWX,ERWY: Extended);
    procedure Paint({%H-}X,{%H-}Y: Integer); virtual;
    function NeedBrackets: Boolean; virtual;
    function ArgNeedBrackets: Boolean; virtual;
    function CalcWidth: Integer; virtual;
    function CalcHeight: Integer; virtual;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; virtual;
    function CalcPowerXPos: Integer; virtual;
    function CalcIndexXPos: Integer; virtual;
    function CalcPowerYPos: Integer; virtual;
    function CalcIndexYPos: Integer; virtual;
    function CalcCapDY: Integer; virtual;
    procedure CalcCapDX(out DLeft,DRight: Integer); virtual;
  public
    property Next: TExprClass read FNext write SetNext;
    property Color: TColor read GetColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property MidLineUp: Integer read GetMidLineUp;
    property MidLineDn: Integer read GetMidLineDn;
    property PowerXPos: Integer read GetPowerXPos;
    property PowerYPos: Integer read GetPowerYPos;
    property IndexXPos: Integer read GetIndexXPos;
    property IndexYPos: Integer read GetIndexYPos;
    property CapDXLeft: Integer read GetCapDXLeft;
    property CapDXRight: Integer read GetCapDXRight;
    property CapDY: Integer read GetCapDY;
    property TextHeight: Integer read GetTextHeight;
    constructor Create;
    destructor Destroy; override;
    function FTType: Integer; virtual;
    procedure AddNext(const AValue: TExprClass);
    function CutOff: TExprClass;
    procedure Draw(X,Y: Integer; const AHorAlign: TExprHorAlign; const AVertAlign: TExprVertAlign);
  end;

  { TExprParent }

  TExprParent = class (TExprClass)
  private
    FSon: TExprClass;
    procedure SetSon(const AValue: TExprClass);
    procedure SetSonFont; virtual;
    procedure SetSonCanvas; virtual;
  protected
    procedure DynaSetFont; override;
    procedure DynaSetCanvas; override;
  public
    property Son: TExprClass read FSon write SetSon;
    constructor Create(const ASon: TExprClass);
    destructor Destroy; override;
    function CutOffSon: TExprClass;
  end;

  { TExprBigParent }

  TExprBigParent = class (TExprParent)
  private
    FDaughter: TExprClass;
    procedure SetDaughter(const AValue: TExprClass);
    procedure SetDaughterFont; virtual;
    procedure SetDaughterCanvas; virtual;
  protected
    procedure DynaSetCanvas; override;
    procedure DynaSetFont; override;
  public
    property Daughter: TExprClass read FDaughter write SetDaughter;
    constructor Create(const ASon, ADaughter: TExprClass);
    destructor Destroy; override;
    function CutOffDaughter: TExprClass;
  end;

  { TExprChain }

  TExprChain = class (TExprParent)
  private
    procedure CalcOverAbove(out AOver, AAbove: Integer);
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcCapDY: Integer; override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  public
    procedure BuildUpChain(const AValue: TExprClass);
    function FTType: Integer; override;
  end;

  { TExprSimple }

  TExprSimple = class (TExprClass)
  protected
    FExpr: String;
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcCapDY: Integer; override;
  public
    constructor Create(const AExpr: String);
  end;

  { TExprVar }

  TExprVar = class (TExprSimple)
  protected
    procedure SetCanvasFont; override;
    function CalcWidth: Integer; override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
    function CalcPowerXPos: Integer; override;
    function CalcIndexXPos: Integer; override;
  end;

  { TExprCustomText }

  TExprCustomText = class (TExprSimple)
  protected
    FFontStyle: TFontStyles;
    FFontName: String;
    procedure SetCanvasFont; override;
  public
    constructor Create(const AExpr: String; const AFontStyle: TFontStyles=[fsBold];
                       const AFontName: String=DefaultFontName);
  end;

 { TExprNumber }

  TExprNumber = class(TExprClass)
  private
    FNumber: Extended;
    FSM,FSE: String;
    FExpForm: Boolean;
    procedure SetNumber(const AValue: Extended);
  protected
    function NumToStr: String; virtual;
    function CalcCapDY: Integer; override;
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  public
    property Number: Extended read FNumber write SetNumber;
    constructor Create(const ANumber: Extended; const AExpForm: Boolean);
    function FTType:Integer;override;
  end;

  { TExprExpNumber }

  TExprExpNumber = class(TExprNumber)
  private
    FPrecision,FDigits,FMaxDeg: Integer;
  protected
    function NumToStr: String; override;
  public
    constructor Create(const ANumber: Extended; const APrecision: Integer=4;
                       const ADigits: Integer=4; const AMaxDeg: Integer=2);
  end;

  { TExprRatio }

  TExprRatio = class(TExprBigParent)
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin:TExprOrigin): Integer; override;
  end;

  { TExprRoot }

  TExprRoot = class(TExprBigParent)
  private
    procedure SetDaughterFont; override;
    procedure SetDaughterCanvas; override;
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  end;

  { TExprBracketed }

  TExprBracketed = class(TExprChain)
  private
    procedure SetBracketCanvas(const AFontSize: Integer);
    procedure SetExprCanvas(const AFontSize: Integer; const AFontStyle: TFontStyles);
    function GetBracketFontSize(out ADesc: Integer): Integer;
  protected
    FLeftBracket,FRightBracket: TExprBracketStyle;
    FLeftSymbol,FRightSymbol: String;
    procedure Paint(X,Y: Integer); override;
    function IsBracketed: Boolean; virtual;
    function CalcCapDY: Integer; override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  public
    constructor Create(const ASon: TExprClass; const ALeftBracket, ARightBracket: TExprBracketStyle);
    function FTType: Integer; override;
    procedure RemoveBrackets;
  end;

  { TExprRound }

  TExprRound = class(TExprBracketed)
  public
    constructor Create(const ASon: TExprClass);
    function FTType: Integer; override;
  end;

  { TExprExtSymbol }

  TExprExtSymbol = class(TExprClass)
    FSymbol: String;
    FCode: Integer;
  protected
    procedure Paint(X,Y:Integer); override;
    function CalcCapDY: Integer; override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
    function CalcPowerXPos: Integer; override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  public
    constructor Create(const ASymbolCode: Integer = esComma);
  end;

  { TExprExtSymbolItalic }

  TExprExtSymbolItalic = class(TExprExtSymbol)
  protected
    procedure SetCanvasFont; override;
    function CalcCapDY: Integer; override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
  end;

  { TExprSign }

  TExprSign = class(TExprExtSymbol)
  protected
    function NeedBrackets: Boolean; override;
    procedure Paint(X,Y: Integer); override;
    function CalcCapDY: Integer; override;
    function CalcWidth: Integer; override;
  public
    function FTType: Integer; override;
  end;

  { TExprTwinParent }

  TExprTwinParent = class(TExprParent)
  private
    Twins: array[1..2] of TExprClass;
    procedure SetTwins(const AIndex: Integer; const AValue: TExprClass);
  protected
    procedure DynaSetFont; override;
    procedure DynaSetCanvas; override;
  public
    property Twin1: TExprClass index 1 read Twins[1] write SetTwins;
    property Twin2: TExprClass index 2 read Twins[2] write SetTwins;
    constructor Create(const ASon,ATwin1,ATwin2: TExprClass);
    destructor Destroy; override;
  end;

  { TExprIndex }

  TExprIndex = class(TExprTwinParent)
  protected
    function CalcCapDY: Integer; override;
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  public
    function ArgNeedBrackets: Boolean; override;
    function FTType: Integer; override;
  end;

  { TExprArgument }

  TExprArgument = class(TExprBracketed)
  protected
    ForcedBrackets: Boolean;
    function IsBracketed: Boolean; override;
  public
    constructor Create(const ASon: TExprClass);
    procedure SetBrackets;
  end;

  { TExprCommonFunc }

  TExprCommonFunc = class(TExprBigParent)
  protected
    procedure Paint(X,Y:Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  public
    function ArgumentNeedBrackets: Boolean;
    function FTType: Integer; override;
  end;

  { TExprFuncName }

  TExprFuncName = class(TExprSimple)
  protected
    function ArgNeedBrackets: Boolean; override;
  end;

  { TExprFunc }

  TExprFunc = class(TExprCommonFunc)
  public
    constructor Create(const AFuncName: String; const ADaughter: TExprClass);
  end;

  { TExprBase }

  TExprBase = class(TExprBracketed)
  protected
    function IsBracketed: Boolean; override;
  public
    constructor Create(const ASon: TExprClass);
  end;

  { TExprSeparator }

  TExprSeparator = class(TExprExtSymbol)
  protected
    function NeedBrackets: Boolean; override;
    function CalcCapDY: Integer; override;
  end;

  { TExprSub }

  TExprSub = class(TExprParent)
  protected
    FFuncName: String;
    procedure SetSonFont; override;
    procedure SetSonCanvas; override;
    procedure Paint(X,Y:Integer); override;
    function ArgNeedBrackets: Boolean; override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  public
    constructor Create(const ASon: TExprClass; const AFuncName: String);
  end;

  { TExprSpace }

  TExprSpace = class(TExprClass)
  private
    FCount: Integer;
  protected
    function CalcWidth: Integer; override;
  public
    constructor Create(const ACount: Integer);
  end;

  { TExprStrokes }

  TExprStrokes = class(TExprClass)
  private
    FCount: Integer;
    FSymbol: String;
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  public
    constructor Create(const ACount: Integer);
  end;

  { TExprAt }

  TExprAt = class(TExprTwinParent)
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
    function TwinsHeight: Integer;
    function TwinsWidth: Integer;
  public
    function FTType: Integer; override;
  end;

  { TExprCap }

  TExprCap = class(TExprParent)
  private
    FStyle: TExprCapStyle;
    FCount: Integer;
  protected
    function CapWidth: Integer;
    function CapHeight: Integer;
    function SelfHeight: Integer;
    function CalcPowerXPos: Integer; override;
    function CalcPowerYPos: Integer; override;
    function CalcIndexXPos: Integer; override;
    function CalcCapDY: Integer; override;
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
  public
    constructor Create(const ASon: TExprClass; const ACapStyle: TExprCapStyle; const ACount: Integer);
    function FTType: Integer; override;
  end;

  { TExprStand }

  TExprStand = class(TExprParent)
  private
    FHorAlign: TExprHorAlign;
  protected
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  public
    constructor Create(const ASon: TExprClass; const AHorAlign: TExprHorAlign);
  end;

  { TExprMatrix }

  TExprMatrix = class(TExprParent)
  private
    FHorSize,FVertSize: Integer;
    FCX,FCY: Integer;
    FHorAlign: TExprHorAlign;
  protected
    procedure GetCellSize(out CX,CY: Integer);
    procedure Paint(X,Y:Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function GetCellWidth: Integer;
    function GetCellHeight: Integer;
  public
    constructor Create(const ASon: TExprClass; const AHorSize, AVertSize: Integer;
                       const AHorAlign: TExprHorAlign = ehCenter);
  end;

  { TExprCorr }

  TExprCorr = class(TExprMatrix)
  protected
    procedure Paint(X,Y:Integer); override;
  public
    constructor Create(const ASon: TExprClass; const AVertSize: Integer);
  end;

  { TExprGroup }

  TExprGroup = class(TExprTwinParent)
  private
    FSymbolHeight, FSymbolWidth, FSymbolFontSize, FDes: Integer;
    FSymbol: String;
    function CalcSymbolHeight: Integer; virtual;
    function CalcSymbolWidth: Integer; virtual;
  protected
    procedure DrawSymbol(X,Y: Integer); virtual;
    function GetTwinWidth: Integer;
    function GetSymbolWidth: Integer;
    function GetSymbolHeight: Integer;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcMidLine(const AOrigin: TExprOrigin): Integer; override;
    procedure Paint(X,Y: Integer); override;
  public
    constructor Create(const AMainSymbolCode: Integer; const ASon,AFirstTwin,ASecondTwin: TExprClass);
    property SymbolFontSize: Integer read FSymbolFontSize;
  end;

  { TExprIntegral }

  TExprIntegral = class(TExprGroup)
  private
    function CalcTwin1XPos: Integer;
    function CalcTwin2XPos: Integer;
  protected
    FMult: Integer;
    function CalcSingleSymbolWidth: Integer; virtual;
    function CalcSymbolHeight: Integer; override;
    function CalcSymbolWidth: Integer; override;
    function CalcWidth: Integer; override;
    procedure Paint(X,Y: Integer); override;
    procedure DrawSymbol(X,Y: Integer); override;
    procedure CalcCapDX(out DLeft,DRight: Integer); override;
  public
    constructor Create(const AMainSymbolCode: Integer;
                       const ASon,AFirstTwin,ASecondTwin: TExprClass;
                       const AMult: Integer = 1);
  end;

  { TExprInt }

  TExprInt = class(TExprIntegral)
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass; const AMult: Integer);
  end;

  { TExprCirc }

  TExprCirc = class(TExprIntegral)
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
  end;

  { TExprSurf }

  TExprSurf = class(TExprIntegral)
  protected
    function CalcSingleSymbolWidth: Integer; override;
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
  end;

  { TExprVolume }

  TExprVolume = class(TExprIntegral)
  protected
    function CalcSingleSymbolWidth: Integer; override;
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
  end;

  { TExprSumProd }

  TExprSumProd = class(TExprGroup)
  protected
    function CalcSymbolHeight: Integer; override;
    procedure DrawSymbol(X,Y: Integer); override;
  end;

  { TExprSum }

  TExprSum = class(TExprSumProd)
  protected
    function CalcSymbolWidth: Integer; override;
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
  end;

  { TExprProd }

  TExprProd = class(TExprSumProd)
  protected
    function CalcSymbolWidth: Integer; override;
  public
    constructor Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
  end;

  { TExprAsterisk }

  TExprAsterisk = class(TExprSimple)
  protected
    procedure Paint(X,Y: Integer); override;
  public
    constructor Create;
  end;

  { TExprCase }

  TExprCase = class(TExprParent)
  private
    FSymbolHeight, FSymbolWidth, FSymbolFontSize, FDes,
    FCol1Width, FCol2Width: Integer;
    FSymbol: String;
    FRX, FRY: Extended;
    function GetSymbolWidth: Integer;
    function GetSymbolHeight: Integer;
  protected
    procedure DrawSymbol(X,Y: Integer);
    procedure DrawContent(X,Y: Integer);
    procedure Paint(X,Y: Integer); override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    function CalcSymbolHeight: Integer;
    function CalcSymbolWidth: Integer;
  public
    constructor Create(const ASon: TExprClass; const ASymbol: String; const ARX, ARY: Extended);
  end;

  { TExprCaseAnd }

  TExprCaseAnd = class(TExprCase)
  public
    constructor Create(const ASon: TExprClass);
  end;

  { TExprCaseOr }

  TExprCaseOr = class(TExprCase)
  public
    constructor Create(const ASon: TExprClass);
  end;

  { TExprEmpty }

  TExprEmpty = class(TExprClass)
  protected
    function CalcHeight: Integer; override;
  end;

  procedure SetOutputDPI(const AHorzDPI: Integer = 0; const AVertDPI: Integer = 0);

  function SLength(const AStr: String): PtrInt;
  function SCopy(const AStr: String; const AStart, ACount: PtrInt): String;
  function SSymbol(const AStr: String; const ASymbolPos: PtrInt): String;
  function SUpper(const AStr: String): String;
  function SCodeToStr(const ACode: Integer): String;
  function SPos(const AValue, AStr: String; const AStartPos: SizeInt = 1): PtrInt;

implementation

var
  ExprXDPI, ExprYDPI: Integer;

procedure SetOutputDPI(const AHorzDPI: Integer = 0; const AVertDPI: Integer = 0);
begin
  if AHorzDPI<=0 then
    ExprXDPI:= ScreenInfo.PixelsPerInchX
  else
    ExprXDPI:= AHorzDPI;
  if AVertDPI<=0 then
    ExprYDPI:= ScreenInfo.PixelsPerInchY
  else
    ExprYDPI:= AVertDPI;
end;

function Max(const AValue1, AValue2: Integer): Integer;
begin
  if AValue1>=AValue2 then
    Max:= AValue1
  else
    Max:= AValue2;
end;

function Min(const AValue1, AValue2: Integer): Integer;
begin
  if AValue1<=AValue2 then
    Min:= AValue1
  else
    Min:= AValue2;
end;

function SPos(const AValue, AStr: String; const AStartPos: SizeInt = 1): PtrInt;
begin
  SPos:= UTF8Pos(AValue, AStr, AStartPos);
end;

function SLength(const AStr: String): PtrInt;
begin
  SLength:= UTF8Length(AStr);
end;

function SCopy(const AStr: String; const AStart, ACount: PtrInt): String;
begin
  SCopy:= UTF8Copy(AStr, AStart, ACount);
end;

function SSymbol(const AStr: String; const ASymbolPos: PtrInt): String;
begin
  SSymbol:= SCopy(AStr, ASymbolPos, 1);
end;

function SSymbolFirst(const AStr: String): String;
begin
  SSymbolFirst:= SSymbol(AStr, 1);
end;

function SSymbolLast(const AStr: String): String;
begin
  SSymbolLast:= SSymbol(AStr, SLength(AStr));
end;

function SUpper(const AStr: String): String;
begin
  SUpper:= UTF8UpperCase(AStr);
end;

function SCodeToStr(const ACode: Integer): String;
begin
  SCodeToStr:= UTF16ToUTF8(WideChar(ACode));
end;

procedure SDel(var AStr: String; const AStart, AEnd: PtrInt);
begin
  UTF8Delete(AStr, AStart, AEnd-AStart+1);
end;

function ChooseFont(const AFont: TFont; const ANeedHeight: Integer;
                     out AFontDescender: Integer;
                     const ADescCoef: Extended = 1.0): Integer;
var
  BM: TBitmap;
  TM: TLCLTextMetric;
  TextHeight, NeedHeight: Integer;
begin
  NeedHeight:= Round(ANeedHeight*ScreenInfo.PixelsPerInchY/ExprYDPI);
  BM:= TBitmap.Create;
  TextHeight:= 0;
  try
    BM.Canvas.Font.Assign(AFont);
    repeat
      BM.Canvas.Font.Size:= BM.Canvas.Font.Size + 1;
      BM.Canvas.GetTextMetrics(TM);
      {$IFDEF WINDOWS}
      TextHeight:= BM.Canvas.TextHeight('X') - Round(ADescCoef*TM.Descender);
      {$ENDIF}
      {$IFDEF LINUX}
      TextHeight:= Round(1.5*(BM.Canvas.TextHeight('X') - ADescCoef*TM.Descender));
      {$ENDIF}
    until TextHeight>NeedHeight;
    Result:= BM.Canvas.Font.Size - 1;
    BM.Canvas.GetTextMetrics(TM);
    {$IFDEF WINDOWS}
    AFontDescender:= TM.Descender;
    {$ENDIF}
    {$IFDEF LINUX}
    AFontDescender:= - Round(0.7*TM.Descender);
    {$ENDIF}
    AFontDescender:= Round(AFontDescender*ExprYDPI/ScreenInfo.PixelsPerInchY);
  finally
    FreeAndNil(BM);
  end;
end;

function ChooseFont(const AFont: TFont; const ANeedHeight: Integer): Integer;
var
  X: Integer;
begin
  Result:= ChooseFont(AFont, ANeedHeight, X);
end;

{ TExprClass }

constructor TExprClass.Create;
begin
  inherited Create;
  FNext:= nil;
  Parent:= nil;
  FColor:= clNone;
  FFont:= TFont.Create;
  FFont.Name:= DefaultFontName;
  FFont.OnChange:= @FontNotify;
  FWidth:= 0;
  FHeight:= 0;
  FMidLineUp:= 0;
  FMidLineDn:= 0;
  FPowerXPos:= 0;
  FPowerYPos:= 0;
  FIndexXPos:= 0;
  FIndexYPos:= 0;
  FCapDXLeft:= 0;
  FCapDXRight:= 0;
  FCapDY:= 0;
  FWX:= 0;
  FWY:= 0;
  FRWX:= 0;
  FRWY:= 0;
  FToChange:= 0;
  FCanvas:= nil;
end;

destructor TExprClass.Destroy;
begin
  FreeAndNil(FNext);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TExprClass.SetNext(const AValue: TExprClass);
begin
  if Assigned(FNext) then
    FreeAndNil(FNext);
  FNext:= AValue;
  if Assigned(FNext) then
    FNext.Parent:= Parent;
end;

function TExprClass.GetTextHeight: Integer;
begin
  Result:= Canvas.TextHeight('X');
end;

procedure TExprClass.SetParent(const AValue: TExprClass);
begin
  if FParent<>AValue then
  begin
    FParent:= AValue;
    if Assigned(FNext) then  FNext.Parent:= Parent;
  end;
end;

function TExprClass.GetColor: TColor;
begin
  if FColor<>clNone then
    Result:= FColor
  else if Assigned(Parent) then
   Result:= Parent.Color
  else
   Result:= clBlack;
end;

procedure TExprClass.SetColor(const AValue: TColor);
begin
  if AValue=FColor then Exit;
  FColor:= AValue;
  if Assigned(FNext) then FNext.Color:= AValue;
end;

procedure TExprClass.SetCanvas(const AValue: TCanvas);
begin
  if FCanvas=AValue then Exit;
  FCanvas:= AValue;
  if Assigned(Canvas) then
  begin
    FToChange:= $FFFFFFFF;
    SetLineWidth;
  end
  else
    FToChange:= 0;
  DynaSetCanvas;
end;

procedure TExprClass.AssignCanvas(const AValue: TCanvas; const AWX,
  AWY: Integer; const ARWX, ARWY: Extended);
begin
  if FCanvas=AValue then Exit;
  FCanvas:= AValue;
  if Assigned(Canvas) then
    FToChange:= $FFFFFFFF
  else
    FToChange:= 0;
  FWX:= AWX;
  FWY:= AWY;
  FRWX:= ARWX;
  FRWY:= ARWY;
  DynaSetCanvas;
end;

procedure TExprClass.SetLineWidth;
begin
  SetCanvasFont;
  FRWX:= TextHeight/27.6;
  FRWY:= FRWX*ExprYDPI/ExprXDPI;
  FWX:= Round(0.3*Canvas.TextWidth('|'));
  if FWX=0 then FWX:= 1;
  FWY:= Round(FWX*ExprYDPI/ExprXDPI);
  if FWY=0 then FWY:= 1;
end;

procedure TExprClass.SetFont(const ANewFont: TFont);
begin
  FFont.Assign(ANewFont);
  if Assigned(Canvas) then SetLineWidth;
  DynaSetFont;
end;

procedure TExprClass.AssignFont(const ANewFont: TFont;
     const EWX,EWY: Integer; const ERWX, ERWY: Extended);
begin
  FFont.Assign(ANewFont);
  FWX:= EWX;
  FWY:= EWY;
  FRWX:= ERWX;
  FRWY:= ERWY;
  DynaSetFont;
end;

procedure TExprClass.FontNotify(Sender:TObject);
begin
  if Assigned(Canvas) then
    SetLineWidth;
  DynaSetFont;
end;

procedure TExprClass.DynaSetCanvas;
begin
  if Assigned(Next) then
    Next.AssignCanvas(Canvas,FWX,FWY,FRWX,FRWY);
end;

procedure TExprClass.DynaSetFont;
begin
  FToChange:= $FFFFFFFF;
  if Assigned(Next) then
    Next.AssignFont(Font,FWX,FWY,FRWX,FRWY);
end;

procedure TExprClass.AddNext(const AValue: TExprClass);
var
  P: TExprClass;
begin
  P:= Self;
  while Assigned(P.Next) do P:=P.Next;
  P.FNext:= AValue;
  P.FNext.Font:= Font;
  P.FNext.Canvas:= Canvas;
end;

function TExprClass.CutOff: TExprClass;
begin
 Result:= FNext;
 FNext:= nil;
end;

function TExprClass.NeedBrackets: Boolean;
begin
  Result:= False;
end;

function TExprClass.ArgNeedBrackets: Boolean;
begin
  Result:= True;
end;

function TExprClass.FTType: Integer;
begin
  Result:= efLeft or efRight;
end;

function TExprClass.CalcPowerXPos: Integer;
begin
  Result:= Width;
end;

function TExprClass.CalcIndexXPos: Integer;
begin
  Result:= Width;
end;

function TExprClass.CalcPowerYPos: Integer;
begin
  SetCanvasFont;
  Result:= TextHeight div 2;
end;

function TExprClass.CalcIndexYPos:Integer;
begin
  SetCanvasFont;
  Result:= Height-TextHeight div 2-2;
end;

function TExprClass.CalcCapDY:Integer;
begin
  Result:= 0;
end;

procedure TExprClass.CalcCapDX(out DLeft, DRight: Integer);
begin
  DLeft:= 0;
  DRight:= 0;
end;

function TExprClass.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoTop then
    Result:= Height div 2
  else
    Result:= -((Height-1) div 2);
end;

procedure TExprClass.SetCanvasFont;
begin
  Canvas.Brush.Style:= bsClear;
  Canvas.Font:= Font;
  Canvas.Font.Color:= Color;
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
end;

procedure TExprClass.SetPenAndBrush;
begin
  Canvas.Pen.Style:= psSolid;
  Canvas.Pen.Width:= 1;
  Canvas.Pen.Color:= Color;
  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= Color;
end;

procedure TExprClass.ConvertCoords(var X, Y: Integer;
     const AHorAlign: TExprHorAlign; const AVertAlign: TExprVertAlign);
begin
  case AHorAlign of
    ehCenter: Dec(X,Width div 2);
    ehRight:  Dec(X,Width-1);
  end;
  case AVertAlign of
    evCenter: Dec(Y,MidLineUp);
    evBottom: Dec(Y,Height-1);
  end
end;

function TExprClass.CalcWidth: Integer;
begin
  Result:= 0;
end;

function TExprClass.CalcHeight: Integer;
begin
  Result:= 0;
end;

procedure TExprClass.Paint(X,Y: Integer);
begin
  //Empty
end;

procedure TExprClass.Draw(X, Y: Integer; const AHorAlign: TExprHorAlign;
   const AVertAlign: TExprVertAlign);
begin
  ConvertCoords(X,Y,AHorAlign,AVertAlign);
  SetCanvasFont;
  Paint(X,Y);
end;

function TExprClass.GetWidth: Integer;
begin
  if FToChange and tcWidth<>0 then
  begin
    FWidth:= CalcWidth;
    FToChange:= FToChange and not tcWidth;
  end;
  Result:= FWidth;
end;

function TExprClass.GetHeight: Integer;
begin
  if FToChange and tcHeight<>0 then
  begin
    FHeight:= CalcHeight;
    FToChange:= FToChange and not tcHeight;
  end;
  Result:= FHeight;
end;

function TExprClass.GetMidLineUp: Integer;
begin
  if FToChange and tcMidLineUp<>0 then
  begin
    FMidLineUp:= CalcMidLine(eoTop);
    FToChange:= FToChange and not tcMidLineUp;
  end;
  Result:= FMidLineUp;
end;

function TExprClass.GetMidLineDn: Integer;
begin
  if FToChange and tcMidLineDn<>0 then
  begin
    FMidLineDn:= CalcMidLine(eoBottom);
    FToChange:= FToChange and not tcMidLineDn;
  end;
  Result:= FMidLineDn;
end;

function TExprClass.GetPowerXPos: Integer;
begin
  if FToChange and tcPowerXPos<>0 then
  begin
    FPowerXPos:= CalcPowerXPos;
    FToChange:= FToChange and not tcPowerXPos;
  end;
  Result:= FPowerXPos;
end;

function TExprClass.GetPowerYPos: Integer;
begin
  if FToChange and tcPowerYPos<>0 then
  begin
    FPowerYPos:= CalcPowerYPos;
    FToChange:= FToChange and not tcPowerYPos;
  end;
 Result:= FPowerYPos;
end;

function TExprClass.GetIndexXPos: Integer;
begin
  if FToChange and tcIndexXPos<>0 then
  begin
    FIndexXPos:= CalcIndexXPos;
    FToChange:= FToChange and not tcIndexXPos;
  end;
  Result:= FIndexXPos;
end;

function TExprClass.GetIndexYPos: Integer;
begin
  if FToChange and tcIndexYPos<>0 then
  begin
    FIndexYPos:= CalcIndexYPos;
    FToChange:= FToChange and not tcIndexYPos;
  end;
  Result:= FIndexYPos;
end;

function TExprClass.GetCapDXLeft: Integer;
begin
  if FToChange and tcCapDX<>0 then
  begin
    CalcCapDX(FCapDXLeft,FCapDXRight);
    FToChange:= FToChange and not tcCapDX;
  end;
  Result:= FCapDXLeft;
end;

function TExprClass.GetCapDXRight: Integer;
begin
  if FToChange and tcCapDX<>0 then
  begin
    CalcCapDX(FCapDXLeft,FCapDXRight);
    FToChange:= FToChange and not tcCapDX;
  end;
  Result:= FCapDXRight;
end;

function TExprClass.GetCapDY: Integer;
begin
  if FToChange and tcCapDY<>0 then
  begin
    FCapDY:= CalcCapDY;
    FToChange:= FToChange and not tcCapDY;
  end;
  Result:= FCapDY;
end;

{ TExprParent }

constructor TExprParent.Create(const ASon: TExprClass);
begin
  inherited Create;
  Son:= ASon;
end;

destructor TExprParent.Destroy;
begin
  FreeAndNil(FSon);
  inherited Destroy;
end;

procedure TExprParent.SetSon(const AValue: TExprClass);
begin
  FreeAndNil(FSon);
  FSon:= AValue;
  if Assigned(FSon) then
  begin
    FSon.Parent:= Self;
    SetSonFont;
    SetSonCanvas;
  end;
  FToChange:= $FFFFFFFF;
end;

procedure TExprParent.DynaSetFont;
begin
  inherited DynaSetFont;
  SetSonFont;
end;

procedure TExprParent.DynaSetCanvas;
begin
  inherited DynaSetCanvas;
  SetSonCanvas;
end;

procedure TExprParent.SetSonFont;
begin
  if Assigned(FSon) then
    FSon.AssignFont(Font,FWX,FWY,FRWX,FRWY);
end;

procedure TExprParent.SetSonCanvas;
begin
  if Assigned(FSon) then
    Son.AssignCanvas(Canvas,FWX,FWY,FRWX,FRWY);
end;

function TExprParent.CutOffSon: TExprClass;
begin
  Result:= FSon;
  FSon:= nil;
  FToChange:= $FFFFFFFF;
end;

{ TExprBigParent }

constructor TExprBigParent.Create(const ASon, ADaughter: TExprClass);
begin
  inherited Create(ASon);
  Daughter:= ADaughter;
end;

destructor TExprBigParent.Destroy;
begin
  FreeAndNil(FDaughter);
  inherited Destroy;
end;

procedure TExprBigParent.SetDaughter(const AValue: TExprClass);
begin
  FreeAndNil(FDaughter);
  FDaughter:= AValue;
  if Assigned(FDaughter) then
  begin
    FDaughter.Parent:= Self;
    SetDaughterFont;
    SetDaughterCanvas;
  end;
  FToChange:= $FFFFFFFF;
end;

procedure TExprBigParent.DynaSetFont;
begin
  inherited DynaSetFont;
  SetDaughterFont;
end;

procedure TExprBigParent.DynaSetCanvas;
begin
  inherited DynaSetCanvas;
  SetDaughterCanvas;
end;

procedure TExprBigParent.SetDaughterFont;
begin
  if Assigned(FDaughter) then
    FDaughter.AssignFont(Font,FWX,FWY,FRWX,FRWY);
end;

procedure TExprBigParent.SetDaughterCanvas;
begin
  if Assigned(FDaughter) then
    FDaughter.AssignCanvas(Canvas,FWX,FWY,FRWX,FRWY);
end;

function TExprBigParent.CutOffDaughter: TExprClass;
begin
  Result:= FDaughter;
  FDaughter:= nil;
  FToChange:= $FFFFFFFF;
end;

{ TExprChain }

procedure TExprChain.CalcOverAbove(out AOver,AAbove: Integer);
var
  P: TExprClass;
begin
  AOver:= 0;
  AAbove:= 0;
  P:=Son;
  while Assigned(P) do
  begin
    with P do
    begin
      AOver:= Max(AOver,MidLineUp+1);
      AAbove:= Max(AAbove,Height-MidLineUp-1);
      P:= Next;
    end;
  end;
end;

procedure TExprChain.BuildUpChain(const AValue: TExprClass);
var
  P: TExprClass;
begin
  if Assigned(Son) then
  begin
    P:= Son;
    while Assigned(P.Next) do P:= P.Next;
    P.Next:= AValue;
    AValue.Parent:= Self;
  end
  else
    Son:= AValue;
  FToChange:= $FFFFFFFF;
end;

function TExprChain.CalcWidth: Integer;
var
  P: TExprClass;
begin
  Result:= 0;
  P:= Son;
  while Assigned(P) do
  begin
    Inc(Result,P.Width);
    P:= P.Next;
  end
end;

function TExprChain.CalcHeight: Integer;
var
  Over,Above: Integer;
begin
  CalcOverAbove(Over,Above);
  Result:= Over+Above;
end;

function TExprChain.CalcMidLine(const AOrigin: TExprOrigin): Integer;
var
  Over,Above: Integer;
begin
  CalcOverAbove(Over,Above);
  if AOrigin=eoTop then
    Result:= Over-1
  else
    Result:= -Above;
end;

function TExprChain.FTType: Integer;
var
  P: TExprClass;
begin
  P:= Son;
  while Assigned(P.Next) do P:= P.Next;
  Result:= Son.FTType and efLeft or P.FTType and efRight or Son.FTType and efNegative;
end;

procedure TExprChain.Paint(X,Y: Integer);
var
  P: TExprClass;
begin
  Inc(Y,MidLineUp);
  P:= Son;
  while Assigned(P) do
  begin
    with P do
    begin
      Draw(X,Y,ehLeft,evCenter);
      Inc(X,Width);
      P:= Next;
    end;
  end;
end;

function TExprChain.CalcCapDY: Integer;
var
  P: TExprClass;
  DY: Integer;
begin
  Result:= MaxInt;
  P:= Son;
  while Assigned(P) do
  begin
    DY:= P.CapDY;
    if DY<Result then
      Result:= DY;
    P:= P.Next;
  end;
end;

procedure TExprChain.CalcCapDX(out DLeft, DRight: Integer);
var
  P: TExprClass;
begin
  DLeft:= Son.CapDXLeft;
  P:= Son;
  while Assigned(P.Next) do P:= P.Next;
  DRight:= P.CapDXRight;
end;

{ TExprSimple }

constructor TExprSimple.Create(const AExpr: String);
begin
  inherited Create;
  FExpr:= AExpr;
end;

function TExprSimple.CalcWidth: Integer;
begin
  SetCanvasFont;
  Result:= Canvas.TextWidth(FExpr);
end;

function TExprSimple.CalcHeight: Integer;
begin
  SetCanvasFont;
  Result:= TextHeight;
end;

procedure TExprSimple.Paint(X,Y: Integer);
begin
  SetCanvasFont;
  Canvas.TextOut(X,Y,FExpr);
end;

function TExprSimple.CalcCapDY: Integer;
var
  DY: Extended;
begin
  case SSymbolFirst(FExpr) of
    'A'..'Z','А'..'И','К'..'Я': DY:= 4;
    'a','c','e','g','m'..'s','u'..'z','а'..'и','к'..'я': DY:= 9.5;
    'b','d','f','h','k','l': DY:= 4;
    'i','j','t','й','ё': DY:= 7;
    'Ё','Й':DY:=2.5
  else
    DY:= 0;
  end;
  Result:= Round(DY*FRWY);
end;

{ TExprVar }

procedure TExprVar.SetCanvasFont;
begin
  Canvas.Brush.Style:= bsClear;
  Canvas.Font:= Font;
  Canvas.Font.Style:= Canvas.Font.Style + [fsItalic];
  Canvas.Font.Color:= Color;
end;

function TExprVar.CalcWidth: Integer;
begin
  Result:= inherited CalcWidth + 2*FWX;
end;

procedure TExprVar.CalcCapDX(out DLeft,DRight: Integer);
var
  DX: Extended;
begin
  case SSymbolFirst(FExpr) of
    'A','f': DX:= 5;
    'B','D'..'F','L','P','R'..'T','Y','Z','a'..'e','g'..'o','q'..'t','y'..'z': DX:= 1;
    'C','G'..'K','M'..'O','Q','X','p': DX:= 2;
    'U'..'W': DX:= 1.5;
    'u','v','w': DX:= -0.5;
  else
    DX:= 0;
  end;
  DLeft:= Round(DX*FRWX);
  case SSymbolLast(FExpr) of
    'A','f': DX:= 5.5;
    'B','D'..'F','I','P','R','X': DX:= 4;
    'C','G','H','J'..'L','O','Q','S','U'..'W','Y','Z','p': DX:= 2;
    'M','N': DX:= 1.5;
    'T': DX:= 2.4;
    'a'..'e','g','h','k','m'..'o','q'..'s','u'..'z': DX:= 1;
    'i','j','l','t': DX:= 3;
  else
    DX:= 0;
  end;
  DRight:= Round(DX*FRWX);
end;

function TExprVar.CalcIndexXPos: Integer;
var
  DX: Extended;
begin
  case SSymbolLast(FExpr) of
    'R': DX:= 1.5;
    'W': DX:= 6;
    'x': DX:= 1;
  else
    DX:= 3;
  end;
  Result:= inherited CalcIndexXPos-Round(DX*FRWX);
end;

function TExprVar.CalcPowerXPos: Integer;
var
  DX: Integer;
begin
  DX:= 0;
  case SSymbolLast(FExpr) of
    'f','d': DX:= 2;
    'r': DX:= 1;
  end;
  Result:= inherited CalcPowerXPos+Round(DX*FRWX);
end;

{ TExprCustomText }

constructor TExprCustomText.Create(const AExpr: String;
     const AFontStyle: TFontStyles; const AFontName: String);
begin
  inherited Create(AExpr);
  FFontStyle:= AFontStyle;
  FFontName:= AFontName;
end;

procedure TExprCustomText.SetCanvasFont;
begin
  Canvas.Brush.Style:= bsClear;
  Canvas.Font:= Font;
  Canvas.Font.Name:= FFontName;
  Canvas.Font.Style:= FFontStyle;
  Canvas.Font.Color:= Color;
end;

{ TExprNumber }

constructor TExprNumber.Create(const ANumber: Extended; const AExpForm: Boolean);
begin
  inherited Create;
  FExpForm:= AExpForm;
  if ANumber=0 then
    FNumber:= 1
  else
    FNumber:= 0;
  Number:= ANumber;
end;

function TExprNumber.NumToStr: String;
begin
  if FExpForm then
    Result:= FloatToStrF(Number,ffExponent,14,0)
  else
    Result:= FloatToStr(Number);
end;

procedure TExprNumber.SetNumber(const AValue: Extended);
var
  S: String;
  P: Integer;
begin
  if AValue=FNumber then Exit;
  FNumber:= AValue;
  S:= NumToStr;
  P:= SPos('E',S);
  if P=0 then
  begin
    FSM:= FloatToStr(StrToFloat(S));
    FSE:='';
  end
  else begin
    FSM:= SCopy(S,1,P-1);
    FSE:= SCopy(S,P+1,MaxInt);
    FSM:= FloatToStr(StrToFloat(FSM));
    if SSymbolFirst(FSE)='+' then
      SDel(FSE,1,1);
    while (SSymbolFirst(FSE)='0') and (SLength(FSE)>1) do
      SDel(FSE,1,1);
    P:= SPos('.',FSM);
    if P>0 then
    begin
      while SSymbolLast(FSM)='0' do
        SDel(FSM,SLength(FSM),1);
      if SSymbolLast(FSM)='.' then
        SDel(FSM,SLength(FSM),1)
    end
  end;
  FToChange:= FToChange or tcWidth or tcHeight or tcCapDY;
end;

function TExprNumber.CalcWidth: Integer;
begin
  SetCanvasFont;
  if FSE<>'' then
  begin
    if FSM='1' then
      Result:= Canvas.TextWidth('10')
    else
      Result:= Canvas.TextWidth(FSM+'·10');
    Canvas.Font.Height:= Round(0.7*Canvas.Font.Height);
    Inc(Result,Canvas.TextWidth(FSE));
  end
  else
    Result:= Canvas.TextWidth(FSM);
end;

function TExprNumber.CalcHeight: Integer;
begin
  SetCanvasFont;
  Result:= TextHeight;
  if FSE<>'' then
    Result:= Round(1.2*Result);
end;

function TExprNumber.CalcCapDY: Integer;
begin
  if FSE='' then
    Result:= Round(8*FRWY)
  else
    Result:= Round(3*FRWY);
end;

function TExprNumber.CalcMidLine(const AOrigin: TExprOrigin): Integer;
var
  H: Integer;
begin
  if (FSE='') then
    Result:= inherited CalcMidLine(AOrigin)
  else begin
    SetCanvasFont;
    H:= TextHeight;
    if AOrigin=eoTop then
      Result:= H div 2+Round(H*0.2)
    else
      Result:= -((H-1) div 2);
   end;
end;

function TExprNumber.FTType:Integer;
begin
  Result:= efRight or efNegative*Integer(Number<0);
end;

procedure TExprNumber.Paint(X,Y: Integer);
var
  H,W: Integer;
begin
  SetCanvasFont;
  if FSE='' then
    Canvas.TextOut(X,Y,FSM)
  else begin
    with Canvas do
    begin
      H:= Round(0.2*TextHeight(FSM));
      if FSM='1' then
      begin
        TextOut(X,Y+H,'10');
        W:= TextWidth('10');
      end
      else begin
        TextOut(X,Y+H,FSM+'·10');
        W:= TextWidth(FSM+'·10');
      end;
      Font.Height:= Round(0.7*Font.Height);
      TextOut(X+W,Y,FSE);
    end;
  end;
end;

{ TExprExpNumber }

constructor TExprExpNumber.Create(const ANumber: Extended; const APrecision: Integer;
     const ADigits: Integer; const AMaxDeg: Integer);
begin
  FPrecision:= APrecision;
  FDigits:= ADigits;
  FMaxDeg:= AMaxDeg;
  inherited Create(ANumber,False);
end;

function TExprExpNumber.NumToStr:String;
begin
  if (FNumber<>0) and (Ln(Abs(FNumber))/Ln(10)<=-FMaxDeg) then
    Result:= FloatToStrF(FNumber,ffExponent,FPrecision,1)
  else
    Result:= FloatToStrF(FNumber,ffFixed,FPrecision,FDigits);
end;

{ TExprRatio }

function TExprRatio.CalcWidth: Integer;
begin
  Result:= 8*FWX+Max(Son.Width,Daughter.Width);
end;

function TExprRatio.CalcHeight: Integer;
begin
  Result:=3*FWY+Son.Height+Daughter.Height;
end;

function TExprRatio.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoTop then
    Result:= Son.Height+FWY+FWY div 2
  else
    Result:= -Daughter.Height-FWY-FWY div 2;
end;

procedure TExprRatio.Paint(X,Y: Integer);
var
  XC,YC,dY1,dY2: Integer;
begin
  YC:= MidLineUp;
  XC:= Width div 2;
  Son.Draw(X+XC,Y+YC-FWY-FWY div 2,ehCenter,evBottom);
  Daughter.Draw(X+XC,Y+YC+3*FWY,ehCenter,evTop);
  SetPenAndBrush;
  {$IFDEF WINDOWS}
  dY1:= -FWY div 2;
  dY2:= 3*FWY div 2;
  {$ENDIF}
  {$IFDEF LINUX}
  dY1:= FWY;
  dY2:= 0;
  {$ENDIF}
  Canvas.Rectangle(X+3*FWX,Y+YC-dY1,X+Width-3*FWX,Y+YC+dY2);
end;

{ TExprRoot }

function TExprRoot.CalcWidth: Integer;
begin
  Result:= Son.Width+8*FWX+Round((Son.Height+FWY)/2);
  if Assigned(Daughter) then
    Inc(Result, Max(0,Daughter.Width-5*FWX));
end;

function TExprRoot.CalcHeight: Integer;
begin
  Result:= 3*FWY+Son.Height;
  if Assigned(Daughter) then
    Inc(Result, Max(0,Daughter.Height-4*FWY));
end;

function TExprRoot.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoBottom then
    Result:= Son.MidLineDn
  else if Assigned(Daughter) and (Daughter.Height>4*FWY) then
    Result:= Son.MidLineUp-FWY+Daughter.Height
  else
    Result:= Son.MidLineUp+3*FWY;
end;

procedure TExprRoot.SetDaughterFont;
var
  TmpFont: TFont;
begin
  if not Assigned(Daughter) then Exit;
  TmpFont:=TFont.Create;
  try
    TmpFont.Assign(Font);
    TmpFont.Height:= Round(0.7*Font.Height);
    Daughter.Font:= TmpFont;
  finally
    FreeAndNil(TmpFont);
  end;
end;

procedure TExprRoot.SetDaughterCanvas;
begin
  if Assigned(Daughter) then
    Daughter.Canvas:=Canvas;
end;

procedure TExprRoot.Paint(X,Y: Integer);
var
  DX,DY,I,W,H, YY: Integer;
  Pt: array[1..12] of TPoint;
begin
  H:= 3*FWY+Son.Height;
  W:= Son.Width+8*FWX+Round((Son.Height+FWY)/2);
  Pt[1]:= Point(X+FWX,Y+6*FWY);
  Pt[2]:= Point(X+6*FWX-1,Y+6*FWY);
  Pt[3]:= Point(X+6*FWX-1,Y+Round(H-3*Sqrt(3)*FWY));
  Pt[4]:= Point(X+W-3*FWX-Son.Width-FWX div 2-1,Y+FWY);
  Pt[5]:= Point(X+W-FWX-1,Y+FWY);
  Pt[6]:= Point(X+W-FWX-1,Y+4*FWY-1);
  Pt[7]:= Point(X+W-2*FWX,Y+4*FWY-1);
  Pt[8]:= Point(X+W-2*FWX,Y+2*FWY-1);
  Pt[9]:= Point(X+W-3*FWX-Son.Width-1,Y+2*FWY-1);
  Pt[10]:= Point(X+4*FWX,Y+H);
  Pt[11]:= Point(X+4*FWX,Y+7*FWY-1);
  Pt[12]:= Point(X+FWX,Y+7*FWY-1);
  YY:= 0;
  {$IFDEF LINUX}
  YY:= 2*FWY;
  for I:=1 to 12 do Pt[I].Y:= Pt[I].Y - YY;
  {$ENDIF}
  if Assigned(Daughter) then
  begin
    DX:= Max(0,Daughter.Width-5*FWX);
    DY:= Max(0,Daughter.Height-4*FWY);
    for I:=1 to 12 do
    begin
      Inc(Pt[I].X,DX);
      Inc(Pt[I].Y,DY);
    end;
    Daughter.Draw(Pt[2].X,Pt[2].Y-FWY,ehRight,evBottom);
  end;
  Son.Draw(Pt[9].X,Pt[9].Y+FWY+YY,ehLeft,evTop);
  SetPenAndBrush;
  Canvas.Polygon(Pt);
end;

{ TExprBracketed }

constructor TExprBracketed.Create(const ASon: TExprClass; const ALeftBracket,
  ARightBracket: TExprBracketStyle);
begin
  inherited Create(ASon);
  FLeftBracket:= ALeftBracket;
  FRightBracket:= ARightBracket;
  FLeftSymbol:= '';
  FRightSymbol:= '';
  case FLeftBracket of
    ebRound:  FLeftSymbol:= '(';
    ebSquare: FLeftSymbol:= '[';
    ebFigure: FLeftSymbol:= '{';
    ebAngle:  FLeftSymbol:= SCodeToStr(esLeftAngleBracket);
    ebFloor:  FLeftSymbol:= SCodeToStr(esLeftFloorBracket);
    ebCeil:   FLeftSymbol:= SCodeToStr(esLeftCeilBracket);
  end;
  case FRightBracket of
    ebRound:  FRightSymbol:= ')';
    ebSquare: FRightSymbol:= ']';
    ebFigure: FRightSymbol:= '}';
    ebAngle:  FRightSymbol:= SCodeToStr(esRightAngleBracket);
    ebFloor:  FRightSymbol:= SCodeToStr(esRightFloorBracket);
    ebCeil:   FRightSymbol:= SCodeToStr(esRightCeilBracket);
  end;
end;

function TExprBracketed.IsBracketed: Boolean;
begin
  Result:= True;
end;

function TExprBracketed.FTType: Integer;
begin
 if IsBracketed and (FLeftBracket<>ebNone) and (FRightBracket<>ebNone) then
 begin
   Result:= efLeft or efRight or efBrackets;
   if (FLeftBracket=ebRound) and (FRightBracket=ebRound) then
     Result:= Result or efRoundBrackets;
 end
 else
  Result:= inherited FTType;
end;

function TExprBracketed.CalcWidth: Integer;
var
  OldFontStyle: TFontStyles;
  OldFontSize, BracketFontSize: Integer;
  BracketW, BracketSpace, X: Integer;
begin
  Result:=inherited CalcWidth;
  if IsBracketed and ((FLeftBracket<>ebNone) or (FRightBracket<>ebNone)) then
  begin
    OldFontSize:= Canvas.Font.Size;
    OldFontStyle:= Canvas.Font.Style;
    BracketFontSize:= GetBracketFontSize(X);
    SetBracketCanvas(BracketFontSize);
    if (FLeftBracket=ebNorm) or (FLeftBracket=ebModule) then
    begin
      BracketW:= FWX;
      BracketSpace:= 3*FWX;
      Inc(Result, FWX);
    end
    else begin
      BracketW:= Canvas.TextWidth(FLeftSymbol);
      BracketSpace:= 0;
    end;
    Inc(Result, BracketW + BracketSpace);
    if (FLeftBracket=ebNorm) then
      Inc(Result, 2*BracketW);
    if (FRightBracket=ebNorm) or (FRightBracket=ebModule) then
    begin
      BracketW:= FWX;
      BracketSpace:= 3*FWX;
      Inc(Result, FWX);
    end
    else begin
      BracketW:= Canvas.TextWidth(FRightSymbol);
      BracketSpace:= 0;
    end;
    Inc(Result, BracketW + BracketSpace);
    if (FRightBracket=ebNorm) then
      Inc(Result, 2*BracketW);
    SetExprCanvas(OldFontSize, OldFontStyle);
  end;
end;

function TExprBracketed.CalcHeight: Integer;
begin
  Result:= inherited CalcHeight + 2*FWY;
end;

procedure TExprBracketed.SetBracketCanvas(const AFontSize: Integer);
begin
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
  Canvas.Font.Size:= AFontSize;
  Canvas.Brush.Style:= bsClear;
end;

procedure TExprBracketed.SetExprCanvas(const AFontSize: Integer; const AFontStyle: TFontStyles);
begin
  Canvas.Font.Size:=  AFontSize;
  Canvas.Font.Style:= AFontStyle;
  Canvas.Brush.Style:= bsClear;
end;

function TExprBracketed.GetBracketFontSize(out ADesc: Integer): Integer;
begin
  {$IFDEF WINDOWS}
  Result:= ChooseFont(Canvas.Font, Height, ADesc, 1.30);
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= ChooseFont(Canvas.Font, Height, ADesc, 1.55);
  {$ENDIF}
end;

procedure TExprBracketed.Paint(X,Y: Integer);
var
  OldFontSize, FontDesc, W,H, BracketFontSize: Integer;
  OldFontStyle: TFontStyles;
  BracketW, BracketSpace, dY: Integer;
begin
  if not IsBracketed or ((FLeftBracket=ebNone) and (FRightBracket=ebNone)) then
    inherited Paint(X,Y)
  else begin
    BracketFontSize:= GetBracketFontSize(FontDesc);
    {$IFDEF WINDOWS}
    dY:= FontDesc div 4;
    {$ENDIF}
    {$IFDEF LINUX}
    dY:= -Round(0.8*FontDesc);
    {$ENDIF}
    W:= inherited CalcWidth;
    H:= FontDesc div 2;
    OldFontSize:= Canvas.Font.Size;
    OldFontStyle:= Canvas.Font.Style;
    if (FLeftBracket=ebNorm) or (FLeftBracket=ebModule) then
    begin
      Inc(X, FWX);
      SetPenAndBrush;
      BracketW:= FWX;
      BracketSpace:= 3*FWX;
      Canvas.Rectangle(X,Y+2*FWY,X+BracketW,Y+Height-2*FWY);
      if (FLeftBracket=ebNorm) then
      begin
        Inc(X, 2*BracketW);
        Canvas.Rectangle(X,Y+2*FWY,X+BracketW,Y+Height-2*FWY);
      end;
    end
    else begin
      SetBracketCanvas(BracketFontSize);
      BracketW:= Canvas.TextWidth(FLeftSymbol);
      BracketSpace:= 0;
      Canvas.TextOut(X,Y-H-FWY-dY,FLeftSymbol);
    end;
    Inc(X, BracketW+BracketSpace);
    SetExprCanvas(OldFontSize, OldFontStyle);
    inherited Paint(X,Y);
    if (FRightBracket=ebNorm) or (FRightBracket=ebModule) then
    begin
      BracketW:= FWX;
      BracketSpace:= 3*FWX;
    end
    else begin
      BracketSpace:= 0;
    end;
    Inc(X, W+BracketSpace);
    if (FRightBracket=ebNorm) or (FRightBracket=ebModule) then
    begin
      SetPenAndBrush;
      Canvas.Rectangle(X,Y+2*FWY,X+BracketW,Y+Height-2*FWY);
      if (FRightBracket=ebNorm) then
      begin
        Inc(X, 2*BracketW);
        Canvas.Rectangle(X,Y+2*FWY,X+BracketW,Y+Height-2*FWY);
      end;
    end
    else begin
      SetBracketCanvas(BracketFontSize);
      Canvas.TextOut(X,Y-H-FWY-dY,FRightSymbol);
    end;
  end;
end;

function TExprBracketed.CalcCapDY: Integer;
begin
  if IsBRacketed and ((FLeftBracket<>ebNone) or (FRightBracket<>ebNone)) then
    Result:= 0
  else
    Result:= inherited CalcCapDY;
end;

procedure TExprBracketed.CalcCapDX(out DLeft, DRight: Integer);
begin
  if IsBRacketed and ((FLeftBracket<>ebNone) or (FRightBracket<>ebNone)) then
  begin
    DLeft:= 0;
    DRight:= 0;
  end
  else
    inherited CalcCapDX(DLeft,DRight);
end;

procedure TExprBracketed.RemoveBrackets;
begin
  FLeftBracket:= ebNone;
  FRightBracket:= ebNone;
  FToChange:= FToChange or tcWidth or tcHeight or tcCapDX or tcCapDY;
end;

{ TExprRound }

constructor TExprRound.Create(const ASon: TExprClass);
begin
  inherited Create(ASon,ebRound,ebRound);
end;

function TExprRound.FTType: Integer;
begin
  Result:= efLeft or efRight or efBrackets;
end;

{ TExprExtSymbol }

constructor TExprExtSymbol.Create(const ASymbolCode: Integer);
begin
  inherited Create;
  FCode:= ASymbolCode;
  FSymbol:= SCodeToStr(ASymbolCode);
end;

function TExprExtSymbol.CalcWidth: Integer;
begin
  SetCanvasFont;
  Result:= Canvas.TextWidth(FSymbol);
end;

function TExprExtSymbol.CalcHeight: Integer;
begin
  SetCanvasFont;
  Result:= TextHeight;
end;

procedure TExprExtSymbol.Paint(X,Y: Integer);
begin
  SetCanvasFont;
  Canvas.TextOut(X,Y, FSymbol);
end;

function TExprExtSymbol.CalcCapDY: Integer;
var
  DY: Extended;
begin
  case FCode of
    913..929,931..937: DY:= 4;
    945,947,949,951,953,954,956,957,959..961,963..969: DY:= 8.8;
    946,948,950,952,955,958: DY:= 4;
    esEllipsis: DY:= MaxInt/FRWY-1;
  else
    DY:= 0;
  end;
  Result:= Round(DY*FRWY);
end;

procedure TExprExtSymbol.CalcCapDX(out DLeft, DRight: Integer);
var
  DX: Extended;
begin
  case FCode of
    913,915..929,931..937,952: DX:= 1;
  else
    DX:= 0;
  end;
  DLeft:= Round(DX*FRWX);
  case FCode of
    913..929,931..937: DX:= -1;
    949: DX:= 1;
    952: DX:= -0.5;
  else
    DX:= 0;
  end;
  DRight:= Round(DX*FRWX);
end;

function TExprExtSymbol.CalcPowerXPos: Integer;
var
  DX: Integer;
begin
  DX:= 0;
  if FCode = esPartDiff then DX:= 2;
  Result:= inherited CalcPowerXPos+Round(DX*FRWX);
end;

{ TExprExtSymbolItalic }

procedure TExprExtSymbolItalic.SetCanvasFont;
begin
  inherited SetCanvasFont;
  Canvas.Font.Style:= Canvas.Font.Style + [fsItalic];
end;

function TExprExtSymbolItalic.CalcCapDY:Integer;
begin
  Result:= Round(4*FRWY);
end;

procedure TExprExtSymbolItalic.CalcCapDX(out DLeft, DRight: Integer);
begin
  DLeft:= Round(FRWX);
  DRight:= DLeft;
end;

{ TExprSign }

function TExprSign.CalcWidth: Integer;
begin
  Result:= inherited CalcWidth;
  Inc(Result,6*FWX);
end;

procedure TExprSign.Paint(X,Y: Integer);
begin
  inherited Paint(X+2*FWX,Y);
end;

function TExprSign.FTType: Integer;
begin
  Result:= efNegative*Integer((FCode=esMinus) or (FCode=esPlusMinus) or (FCode=esMinusPlus));
end;

function TExprSign.NeedBrackets:Boolean;
begin
  Result:= (FCode=esMinus) or (FCode=esPlus) or (FCode=esPlusMinus) or (FCode=esMinusPlus);
end;

function TExprSign.CalcCapDY:Integer;
begin
 Result:= MaxInt;
end;

{ TExprTwinParent }

constructor TExprTwinParent.Create(const ASon, ATwin1, ATwin2: TExprClass);
begin
  inherited Create(ASon);
  Twin1:= ATwin1;
  Twin2:= ATwin2;
end;

destructor TExprTwinParent.Destroy;
begin
  FreeAndNil(Twins[1]);
  FreeAndNil(Twins[2]);
  inherited Destroy;
end;

procedure TExprTwinParent.SetTwins(const AIndex: Integer; const AValue: TExprClass);
begin
  FreeAndNil(Twins[AIndex]);
  Twins[AIndex]:= AValue;
  if Assigned(Twins[AIndex]) then
  begin
    with Twins[AIndex] do
    begin
      Parent:= Self;
      Font:= Self.Font;
      Font.Height:= Round(0.7*Font.Height);
      Canvas:= FCanvas;
    end;
  end;
  FToChange:= $FFFFFFFF;
end;

procedure TExprTwinParent.DynaSetFont;
var
  TmpFont: TFont;
begin
  inherited DynaSetFont;
  TmpFont:= TFont.Create;
  try
    TmpFont.Assign(Font);
    TmpFont.Height:= Round(0.7*Font.Height);
    if Assigned(Twin1) then
      Twin1.Font:= TmpFont;
    if Assigned(Twin2) then
      Twin2.Font:= TmpFont;
  finally
    FreeAndNil(TmpFont);
  end;
end;

procedure TExprTwinParent.DynaSetCanvas;
begin
  inherited DynaSetCanvas;
  if Assigned(Twin1) then
    Twin1.Canvas:= Canvas;
  if Assigned(Twin2) then
    Twin2.Canvas:= Canvas;
end;

{ TExprIndex }

function TExprIndex.CalcWidth: Integer;
begin
  Result:= Son.Width;
  if Assigned(Twin1) then
    Result:= Max(Result, Son.IndexXPos+Twin1.Width);
  if Assigned(Twin2) then
    Result:= Max(Result, Son.PowerXPos+Twin2.Width);
end;

function TExprIndex.CalcHeight: Integer;
begin
  Result:= Son.Height;
  if Assigned(Twin1) then
    Inc(Result,Max(0,Twin1.Height-Result+Son.IndexYPos));
  if Assigned(Twin2) then
    Inc(Result,Max(0,Twin2.Height-Son.PowerYPos));
end;

function TExprIndex.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoTop then
  begin
    if not Assigned(Twin2) then
      Result:= Son.MidLineUp
    else
      Result:= Son.MidLineUp+Max(0,Twin2.Height-Son.PowerYPos);
  end
  else begin
    if not Assigned(Twin1) then
      Result:= Son.MidLineDn
    else
      Result:= Son.MidLineDn-Max(0,Twin1.Height-Son.Height+Son.IndexYPos);
  end;
end;

function TExprIndex.CalcCapDY: Integer;
begin
 if Assigned(Twin2) then
   Result:= Twin2.CapDY
 else
   Result:= Son.CapDY;
end;

procedure TExprIndex.Paint(X,Y: Integer);
var
  DY: Integer;
begin
  if Assigned(Twin2) then
  begin
    DY:= Max(0,Twin2.Height-Son.PowerYPos);
    Twin2.Draw(X+Son.PowerXPos,Y+DY+Son.PowerYPos,ehLeft,evBottom);
  end
  else
    DY:=0;
  Son.Draw(X,Y+DY,ehLeft,evTop);
  if Assigned(Twin1) then
    Twin1.Draw(X+Son.IndexXPos,Y+DY+Son.IndexYPos,ehLeft,evTop);
end;

function TExprIndex.ArgNeedBrackets: Boolean;
begin
  Result:= not (Son is TExprFuncName);
end;

function TExprIndex.FTType: Integer;
begin
  FTType:= Son.FTType or efRight;
end;

{ TExprArgument }

constructor TExprArgument.Create(const ASon: TExprClass);
begin
  inherited Create(ASon,ebRound,ebRound);
  ForcedBrackets:= False;
end;

function TExprArgument.IsBracketed: Boolean;
var
  P: TExprClass;
begin
  if (ForcedBrackets) or ((Parent is TExprCommonFunc) and (TExprCommonFunc(Parent).ArgumentNeedBrackets)) then
    Result:= True
  else begin
    P:= Son;
    while Assigned(P) do
    begin
      if P.NeedBrackets then
      begin
        Result:= True;
        Exit;
      end;
      P:= P.Next;
    end;
    Result:=False;
  end;
end;

procedure TExprArgument.SetBrackets;
begin
  ForcedBrackets:= True;
  FToChange:= $FFFFFFFF;
end;

{ TExprCommonFunc }

function TExprCommonFunc.CalcWidth: Integer;
begin
  Result:= Son.Width;
  Inc(Result,3*FWX+Daughter.Width);
end;

function TExprCommonFunc.CalcHeight: Integer;
begin
  Result:= MidLineUp-MidLineDn+1;
end;

function TExprCommonFunc.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoTop then
    Result:= Max(Son.MidLineUp,Daughter.MidLineUp)
  else
    Result:= Min(Son.MidLineDn,Daughter.MidLineDn);
end;

procedure TExprCommonFunc.Paint(X,Y: Integer);
var
  DX,DY: Integer;
begin
  DY:= Y+MidLineUp-Son.MidLineUp;
  Son.Draw(X,DY,ehLeft,evTop);
  DX:= X+3*FWX+Son.Width;
  DY:= Y+MidLineUp-Daughter.MidLineUp;
  Daughter.Draw(DX,DY,ehLeft,evTop);
end;

function TExprCommonFunc.FTType: Integer;
begin
  Result:= efLeft+efRight*Integer(Daughter.FTType and efBrackets>0);
end;

function TExprCommonFunc.ArgumentNeedBrackets: Boolean;
begin
  Result:= Son.ArgNeedBrackets;
end;

{ TExprFuncName }

function TExprFuncName.ArgNeedBrackets: Boolean;
begin
  Result:= False;
end;

{ TExprFunc }

constructor TExprFunc.Create(const AFuncName: String; const ADaughter: TExprClass);
begin
  inherited Create(nil,ADaughter);
  if SLength(AFuncName)=1 then
    Son:= TExprVar.Create(AFuncName)
  else
    Son:= TExprFuncName.Create(AFuncName);
end;

{ TExprBase }

constructor TExprBase.Create(const ASon: TExprClass);
begin
  inherited Create(ASon,ebRound,ebRound);
end;

function TExprBase.IsBracketed: Boolean;
begin
  Result:= Assigned(Son.Next);
end;

{ TExprSeparator }

function TExprSeparator.NeedBrackets: Boolean;
begin
  Result:= True;
end;

function TExprSeparator.CalcCapDY: Integer;
begin
  Result:= MaxInt;
end;

{ TExprSub }

procedure TExprSub.SetSonFont;
var
  TmpFont:TFont;
begin
  if not Assigned(Son) then Exit;
  TmpFont:= TFont.Create;
  try
    TmpFont.Assign(Font);
    TmpFont.Height:= Round(0.7*Font.Height);
    Son.Font:= TmpFont;
  finally
    FreeAndNil(TmpFont);
  end;
end;

procedure TExprSub.SetSonCanvas;
begin
  if Assigned(Son) then
    Son.Canvas:= Canvas;
end;

function TExprSub.CalcWidth: Integer;
begin
  SetCanvasFont;
  Result:= Canvas.TextWidth(FFuncName);
  Result:= Max(Result,Son.Width);
end;

function TExprSub.CalcHeight: Integer;
begin
  SetCanvasFont;
  Result:= TextHeight;
  Inc(Result,Son.Height);
end;

function TExprSub.CalcMidLine(const AOrigin: TExprOrigin): Integer;
var
  H: Integer;
begin
  SetCanvasFont;
  H:= TextHeight;
  if AOrigin=eoTop then
    Result:= H div 2
  else
    Result:= -((H-1) div 2)-Son.Height;
end;

constructor TExprSub.Create(const ASon: TExprClass;
  const AFuncName: String);
begin
  inherited Create(ASon);
  FFuncName:= AFuncName;
end;

procedure TExprSub.Paint(X,Y: Integer);
var
  W, XX, YY: Integer;
  K: Extended;
begin
  SetCanvasFont;
  W:= Son.Width;
  SetCanvasFont;
  XX:= X+Max(0,(W-Canvas.TextWidth(FFuncName)) div 2);
  YY:= Y;
  Canvas.TextOut(XX,YY,FFuncName);
  XX:= X+Max(0,(Canvas.TextWidth(FFuncName)-W) div 2);
  {$IFDEF WINDOWS}
  K:= 0.75;
  {$ENDIF}
  {$IFDEF LINUX}
  K:= 0.95;
  {$ENDIF}
  YY:= Y+Round(K*TextHeight);
  Son.Draw(XX,YY,ehLeft,evTop);
end;

function TExprSub.ArgNeedBrackets: Boolean;
begin
  Result:= False;
end;

{ TExprSpace }

constructor TExprSpace.Create(const ACount: Integer);
begin
  inherited Create;
  FCount:= ACount;
end;

function TExprSpace.CalcWidth:Integer;
begin
  Result:= FCount*FWX;
end;

{ TExprStrokes }

constructor TExprStrokes.Create(const ACount: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCount:= ACount;
  FSymbol:= EmptyStr;
  For I:= 1 to FCount do
    FSymbol:= FSymbol + SCodeToStr(esStroke);
end;

function TExprStrokes.CalcWidth:Integer;
var
  OldFontSize: Integer;
  OldFontStyle: TFontStyles;
begin
  OldFontStyle:= Canvas.Font.Style;
  OldFontSize:= Canvas.Font.Size;
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
  Canvas.Font.Height:= Round(1.4*Canvas.Font.Height);
  Result:= Canvas.TextWidth(FSymbol);
  Canvas.Font.Style:= OldFontStyle;
  Canvas.Font.Size:= OldFontSize;
end;

function TExprStrokes.CalcHeight: Integer;
begin
  SetCanvasFont;
  Result:= Round(0.6*TextHeight);
end;

procedure TExprStrokes.Paint(X,Y:Integer);
var
  dH, OldFontSize: Integer;
  OldFontStyle: TFontStyles;
begin
  OldFontStyle:= Canvas.Font.Style;
  OldFontSize:= Canvas.Font.Size;
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
  Canvas.Font.Height:= Round(2*Canvas.Font.Height);
  Canvas.Brush.Style:= bsClear;
  dH:= TextHeight div 5;
  Canvas.TextOut(X-FWX, Y-dH, FSymbol);
  Canvas.Font.Style:= OldFontStyle;
  Canvas.Font.Size:= OldFontSize;
end;

{ TExprAt }

procedure TExprAt.Paint(X, Y: Integer);
var
  H1,H2,DH,W:Integer;
begin
  H1:= Son.Height;
  H2:= TwinsHeight;
  DH:= Max(0,H2-H1);
  H1:= Max(H1,H2);
  Son.Draw(X,Y+DH,ehLeft,evTop);
  W:= X+Son.Width+2*FWX;
  SetPenAndBrush;
  Canvas.Rectangle(W,Y,W+FWX,Y+H1);
  if Assigned(Twin1) then
    Twin1.Draw(W+4*FWX,Y+H1,ehLeft,evBottom);
  if Assigned(Twin2) then
    Twin2.Draw(W+4*FWX,Y,ehLeft,evTop);
end;

function TExprAt.CalcWidth: Integer;
var
  W: Integer;
begin
  W:= TwinsWidth;
  Result:= Son.Width+4*FWX;
  if W>0 then
    Result:= Result+W+5*FWX;
end;

function TExprAt.CalcHeight: Integer;
begin
  Result:= Max(Son.Height,TwinsHeight);
end;

function TExprAt.CalcMidLine(const AOrigin: TExprOrigin): Integer;
var
  DH: Integer;
begin
  DH:= Max(0,TwinsHeight-Son.Height);
  if AOrigin=eoTop then
    Result:= Son.MidLineUp+DH
  else
    Result:= Son.MidLineDn;
end;

function TExprAt.TwinsHeight: Integer;
begin
  Result:= 0;
  if Assigned(Twin1) then
    Result:= Result+Twin1.Height;
  if Assigned(Twin2) then
    Result:= Result+Twin2.Height;
end;

function TExprAt.TwinsWidth: Integer;
var
  W1, W2: Integer;
begin
  W1:= 0;
  if Assigned(Twin1) then
    W1:= Twin1.Width;
  W2:= 0;
  if Assigned(Twin2) then
    W2:= Twin2.Width;
  Result:= Max(W1,W2);
end;

function TExprAt.FTType: Integer;
begin
  if Son.FTType and efLeft>0 then
    Result:= efLeft
  else
    Result:= 0;
end;

{ TExprCap }

constructor TExprCap.Create(const ASon: TExprClass;
  const ACapStyle: TExprCapStyle; const ACount: Integer);
begin
  inherited Create(ASon);
  FStyle:= ACapStyle;
  FCount:= ACount;
end;

function TExprCap.CalcWidth: Integer;
var
  DLeft,DRight,W,CX: Integer;
begin
  Result:= Son.Width;
  DLeft:= Son.CapDXLeft;
  DRight:= Son.CapDXRight;
  if FStyle in [ecVector,ecLine] then
  begin
    if DLeft<0 then
      Dec(Result,DLeft);
    if DRight>0 then
      Inc(Result,DRight);
  end
  else begin
    W:= CapWidth div 2;
    CX:= (DLeft+DRight+Result) div 2;
    Result:= Max(CX,W)+Max(Result-CX,W);
  end;
end;

function TExprCap.CalcHeight: Integer;
begin
  Result:= Son.Height+SelfHeight;
end;

function TExprCap.CalcMidLine(const AOrigin: TExprOrigin): Integer;
begin
  if AOrigin=eoTop then
    Result:= Son.MidLineUp+SelfHeight
  else
    Result:= Son.MidLineDn;
end;

function TExprCap.CalcPowerXPos: Integer;
begin
  if Width=Son.Width then
    Result:= Son.PowerXPos
  else
    Result:= inherited CalcPowerXPos;
end;

function TExprCap.CalcPowerYPos: Integer;
begin
  Result:= Son.PowerYPos+SelfHeight;
end;

function TExprCap.CalcIndexXPos: Integer;
var
  DL,DX: Integer;
begin
  DL:= Son.GetCapDXLeft;
  if FStyle in [ecPoints,ecCap,ecTilde] then
    DX:= Max(CapWidth div 2-(DL+Son.Width+Son.GetCapDXRight) div 2,0)
  else
    DX:= Max(0,-DL);
  Result:= Son.CalcIndexXPos+DX;
end;

function TExprCap.CapWidth: Integer;
begin
  SetCanvasFont;
  case FStyle of
    ecPoints: Result:= FWX*(4*FCount-2);
    ecCap:    Result:= Canvas.TextWidth('^');
    ecTilde:  Result:= Canvas.TextWidth('~');
  end;
end;

function TExprCap.CapHeight: Integer;
begin
  case FStyle of
    ecPoints: Result:= 5*FWY;
    ecVector: Result:= 6*FWY;
    ecCap:    Result:= 11*FWY;
    ecTilde:  Result:= 6*FWY;
    ecLine:   Result:= 4*FWY;
  else
    Result:= 0;
  end
end;

function TExprCap.SelfHeight: Integer;
begin
  Result:= Max(0,CapHeight-Son.CapDY);
end;

function TExprCap.CalcCapDY: Integer;
begin
  Result:= Max(0,Son.CapDY-CapHeight);
end;

procedure TExprCap.Paint(X,Y:Integer);
var
  DY,DX,W,DLeft,DRight,LX,RX,TW,CX,I:Integer;
  Pt:array[1..8] of TPoint;
begin
  DY:= Y+SelfHeight;
  DLeft:= Son.CapDXLeft;
  DRight:= Son.CapDXRight;
  W:= Width;
  if FStyle in [ecPoints,ecCap,ecTilde] then
  begin
    TW:= CapWidth div 2;
    CX:= (DLeft+Son.Width+DRight) div 2;
    DX:= Max(TW-CX,0);
  end
  else
    DX:= Max(0,-DLeft);
  Son.Draw(X+DX,DY,ehLeft,evTop);
  Inc(DY,Son.CapDY-FWY);
  SetPenAndBrush;
  LX:= X+Max(0,DLeft);
  RX:= X+W+Min(0,DRight);
  {$IFDEF LINUX}
  Dec(DY, 2*FWY);
  {$ENDIF}
  case FStyle of
    ecPoints: for I:= 0 to FCount-1 do
              begin
                LX:= X+DX+CX-TW;
                RX:= LX+2*FWX;
                Canvas.Ellipse(LX+4*FWX*I,DY-3*FWY,RX+4*FWX*I,DY-FWY);
              end;
    ecVector: begin
                if Odd(FWY) then
                begin
                  Pt[1]:= Point(LX, DY-3*FWY);
                  Pt[2]:= Point(RX-2*FWY, Pt[1].Y);
                  Pt[3]:= Point(Pt[2].X, Pt[2].Y-FWY);
                  Pt[4]:= Point(RX-1, Pt[2].Y+FWY div 2);
                  Pt[5]:= Point(Pt[3].X, Pt[3].Y+3*FWY-1);
                  Pt[6]:= Point(Pt[5].X, Pt[5].Y-FWY);
                  Pt[7]:= Point(Pt[1].X, Pt[6].Y);
                  Canvas.Polygon(Slice(Pt,7));
                end
                else begin
                  Pt[1]:= Point(LX, DY-3*FWY);
                  Pt[2]:= Point(RX-2*FWY, Pt[1].Y);
                  Pt[3]:= Point(Pt[2].X, Pt[2].Y-FWY);
                  Pt[4]:= Point(RX-1, Pt[2].Y+FWY div 2-1);
                  Pt[5]:= Point(Pt[4].X, Pt[4].Y+1);
                  Pt[6]:= Point(Pt[3].X, Pt[3].Y+3*FWY-1);
                  Pt[7]:= Point(Pt[6].X, Pt[6].Y-FWY);
                  Pt[8]:= Point(Pt[1].X, Pt[7].Y);
                  Canvas.Polygon(Pt);
                end;
              end;
    ecCap:    begin
                SetCanvasFont;
                Canvas.TextOut(X+DX+CX-TW,DY-Round(15*FRWY),'^')
              end;
    ecTilde:  begin
                SetCanvasFont;
                Canvas.TextOut(X+DX+CX-TW,DY-Round(18.5*FRWY),'~')
              end;
    ecLine: Canvas.Rectangle(LX,DY-3*FWY,RX,DY-2*FWY);
  end;
end;

function TExprCap.FTType: Integer;
begin
  Result:= Son.FTType;
end;

{ TExprStand }

constructor TExprStand.Create(const ASon: TExprClass; const AHorAlign: TExprHorAlign);
begin
  inherited Create(ASon);
  FHorAlign:= AHorAlign;
end;

function TExprStand.CalcWidth: Integer;
var
  P: TExprClass;
begin
  Result:= Son.Width;
  P:= Son.Next;
  while Assigned(P) do
  begin
    Result:= Max(Result,P.Width);
    P:= P.Next;
  end;
end;

function TExprStand.CalcHeight: Integer;
var
  P: TExprClass;
begin
  Result:= Son.Height;
  P:= Son.Next;
  while Assigned(P) do
  begin
    Inc(Result,P.Height);
    P:= P.Next;
  end
end;

procedure TExprStand.Paint(X,Y: Integer);
var
  P: TExprClass;
  W: Integer;
begin
  W:= Width;
  P:= Son;
  while Assigned(P) do
  begin
    case FHorAlign of
      ehLeft:   P.Draw(X,Y,ehLeft,evTop);
      ehCenter: P.Draw(X+W div 2,Y,ehCenter,evTop);
      ehRight:  P.Draw(X+W,Y,ehRight,evTop);
    end;
    Inc(Y,P.Height);
    P:= P.Next;
  end;
end;

{ TExprMatrix }

constructor TExprMatrix.Create(const ASon: TExprClass; const AHorSize,
  AVertSize: Integer; const AHorAlign: TExprHorAlign);
begin
  inherited Create(ASon);
  FHorSize:= AHorSize;
  FVertSize:= AVertSize;
  FHorAlign:= AHorAlign;
end;

procedure TExprMatrix.GetCellSize(out CX,CY: Integer);
var
  P: TExprClass;
  Over,Above: Integer;
begin
  CX:= 0;
  Over:= 0;
  Above:= 0;
  P:= Son;
  while Assigned(P) do
  begin
    with P do
    begin
      Over:= Max(Over,MidLineUp+1);
      Above:= Max(Above,Height-MidLineUp-1);
      CX:= Max(CX,Width);
      P:= Next;
    end;
  end;
  CY:= Over+Above;
end;

function TExprMatrix.GetCellWidth: Integer;
begin
  if FToChange and tcCellSize>0 then
  begin
    GetCellSize(FCX,FCY);
    FToChange:=FToChange and not tcCellSize;
  end;
  Result:= FCX;
end;

function TExprMatrix.GetCellHeight: Integer;
begin
  if FToChange and tcCellSize>0 then
  begin
    GetCellSize(FCX,FCY);
    FToChange:= FToChange and not tcCellSize;
  end;
  Result:= FCY;
end;

function TExprMatrix.CalcWidth: Integer;
begin
  Result:= GetCellWidth*FHorSize+FWX*(4+6*(FHorSize-1));
end;

function TExprMatrix.CalcHeight: Integer;
begin
  Result:= GetCellHeight*FVertSize;
end;

procedure TExprMatrix.Paint(X,Y: Integer);
var
  I,J,CX,CY,DX: Integer;
  P: TExprClass;
begin
  CX:= GetCellWidth;
  CY:= GetCellHeight;
  P:= Son;
  Inc(Y,CY div 2);
  for J:=0 to FVertSize-1 do
  begin
    case FHorAlign of
    ehCenter: DX:= X+2*FWX+CX div 2;
    ehLeft  : DX:= X+2*FWX;
    ehRight : DX:= X+2*FWX+CX;
    end;
    for I:=0 to FHorSize-1 do
    begin
      if Assigned(P) then
      begin
        P.Draw(DX,Y,FHorAlign,evCenter);
        P:= P.Next;
        Inc(DX,CX+6*FWX);
      end;
    end;
    Inc(Y,CY);
  end;
end;

{ TExprCorr }

procedure TExprCorr.Paint(X, Y: Integer);
begin
  inherited Paint(X, Y);
  SetPenAndBrush;
  X:= X + Width div 2;
  Canvas.Rectangle(X-FWX,Y+2*FWY,X,Y+Height-2*FWY);
  SetPenAndBrush;
end;

constructor TExprCorr.Create(const ASon: TExprClass; const AVertSize: Integer);
begin
  inherited Create(ASon,2,AVertSize,ehLeft);
end;

{ TExprGroup }

function TExprGroup.CalcSymbolHeight: Integer;
var
  P: TExprClass;
  K1,K2: Extended;
begin
  if (Son is TExprChain) and (Son.FTType and efRoundBrackets=0) then
  begin
    P:= TExprChain(Son).Son;
    while Assigned(P) do
    begin
      if P is TExprGroup then
      begin
        Result:= TExprGroup(P).GetSymbolHeight;
        Exit;
      end;
      P:= P.Next;
    end;
  end;
  if Son is TExprGroup then
    Result:= TExprGroup(Son).GetSymbolHeight
  else begin
    {$IFDEF WINDOWS}
    K1:= 1.5;
    K2:= 1.1;
    {$ENDIF}
    {$IFDEF LINUX}
    K1:= 2.0;
    K2:= 1.7;
    {$ENDIF}
    Result:= Son.Height;
    Result:= Min(Result,Round(K1*TextHeight));
    Result:= Max(Result,Round(K2*TextHeight));
  end;
end;

function TExprGroup.CalcSymbolWidth: Integer;
begin
  Result:= 0;
end;

procedure TExprGroup.DrawSymbol(X,Y: Integer);
var
  FontSize, dH, dW: Integer;
  FontStyle: TFontStyles;
begin
  Canvas.Brush.Style:= bsClear;
  dH:= (GetSymbolHeight+FDes) div 2;
  dW:= GetSymbolWidth div 2;
  FontSize:= Canvas.Font.Size;
  FontStyle:= Canvas.Font.Style;
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
  Canvas.Font.Size:= FSymbolFontSize;
  Canvas.TextOut(X-dW,Y-dH, FSymbol);
  Canvas.Font.Size:= FontSize;
  Canvas.Font.Style:= FontStyle;
end;

function TExprGroup.GetTwinWidth: Integer;
begin
  Result:= 0;
  if Assigned(Twin1) then
    Result:= Max(Result, Twin1.Width);
  if Assigned(Twin2) then
    Result:= Max(Result, Twin2.Width);
end;

function TExprGroup.GetSymbolWidth: Integer;
begin
  if FToChange and tcSymbolWidth>0 then
  begin
    FSymbolWidth:= CalcSymbolWidth;
    FToChange:= FToChange and not tcSymbolWidth;
  end;
  Result:= FSymbolWidth;
end;

function TExprGroup.GetSymbolHeight:Integer;
begin
  if FToChange and tcSymbolHeight>0 then
  begin
    FSymbolHeight:= CalcSymbolHeight;
    FToChange:= FToChange and not tcSymbolHeight;
  end;
  Result:= FSymbolHeight;
end;

function TExprGroup.CalcWidth: Integer;
begin
  Result:= Max(GetTwinWidth,GetSymbolWidth)+Son.Width+5*FWX;
end;

function TExprGroup.CalcHeight: Integer;
var
  H1,H2,SH1,SH2: Integer;
begin
  SH1:= 0;
  SH2:= 0;
  if Son is TExprGroup then
  begin
    with TExprGroup(Son) do
    begin
      if Assigned(Twin1) then
        SH1:= Twin1.Height;
      if Assigned(Twin2) then
        SH2:= Twin2.Height;
    end;
  end;
  H1:= 2*FWY;
  H2:= 2*FWY;
  if Assigned(Twin1) then
    H1:= Twin1.Height;
  if Assigned(Twin2) then
    H2:= Twin2.Height;
  Result:= GetSymbolHeight+Max(H1,SH1)+Max(H2,SH2);
end;

function TExprGroup.CalcMidLine(const AOrigin: TExprOrigin): Integer;
var
  H,SH: Integer;
begin
  SH:= 0;
  H:= 2*FWY;
  if AOrigin=eoTop then
  begin
    if (Son is TExprGroup) and Assigned(TExprGroup(Son).Twin2) then
      SH:= TExprGroup(Son).Twin2.Height;
    if Assigned(Twin2) then
      H:= Twin2.Height;
    Result:= GetSymbolHeight div 2+Max(H,SH);
  end
  else begin
   if (Son is TExprGroup) and Assigned(TExprGroup(Son).Twin1) then
     SH:= TExprGroup(Son).Twin1.Height;
   if Assigned(Twin1) then
     H:= Twin1.Height;
   Result:= -((GetSymbolHeight-1) div 2+Max(H,SH));
 end;
end;

procedure TExprGroup.Paint(X, Y: Integer);
var
  W1,W2,H,HS: Integer;
begin
  W1:= Max(GetTwinWidth, GetSymbolWidth);
  W2:= X+W1 div 2+2*FWX;
  H:= MidLineUp;
  HS:= GetSymbolHeight div 2;
  if Assigned(Twin2) then
    Twin2.Draw(W2,Y+H-HS,ehCenter,evBottom);
  if Assigned(Twin1) then
    Twin1.Draw(W2,Y+H+HS,ehCenter,evTop);
  DrawSymbol(W2, Y + H);
  Son.Draw(X+W1+2*FWX,Y+H,ehLeft,evCenter);
end;

constructor TExprGroup.Create(const AMainSymbolCode: Integer; const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(ASon,AFirstTwin,ASecondTwin);
  FSymbol:= SCodeToStr(AMainSymbolCode);
  FSymbolHeight:= 0;
  FSymbolWidth:= 0
end;

{ TExprSumProd }

function TExprSumProd.CalcSymbolHeight: Integer;
begin
  Result:=inherited CalcSymbolHeight;
  FSymbolFontSize:= ChooseFont(Canvas.Font, Result, FDes, 0.9);
end;

procedure TExprSumProd.DrawSymbol(X, Y: Integer);
begin
  {$IFDEF WINDOWS}
  inherited DrawSymbol(X - 2*FWX, Y);
  {$ENDIF}
  {$IFDEF LINUX}
  inherited DrawSymbol(X - FWX, Y);
  {$ENDIF}
end;

{ TExprProd }

function TExprProd.CalcSymbolWidth: Integer;
begin
  {$IFDEF WINDOWS}
  Result:= Round(0.87*GetSymbolHeight);
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= Round(0.78*GetSymbolHeight);
  {$ENDIF}
end;

constructor TExprProd.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(esProd, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprSum }

function TExprSum.CalcSymbolWidth: Integer;
begin
  {$IFDEF WINDOWS}
  Result:= Round(0.78*GetSymbolHeight);
  {$ENDIF}
  {$IFDEF LINUX}
  Result:= Round(0.69*GetSymbolHeight);
  {$ENDIF}
end;

constructor TExprSum.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(esSum, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprIntegral }

function TExprIntegral.CalcSymbolHeight: Integer;
begin
  Result:= inherited CalcSymbolHeight;
  FSymbolFontSize:= ChooseFont(Canvas.Font, Result, FDes, 0.8);
end;

function TExprIntegral.CalcSymbolWidth: Integer;
begin
  Result:= CalcSingleSymbolWidth;
  if FMult<1 then
    Result:= Result + 2*(Result div 2)+Canvas.TextWidth(SCodeToStr(esEllipsis))
  else if FMult>1 then
    Result:= Result + (Result div 2)*(FMult-1)+FWX*(FMult-1);
end;

procedure TExprIntegral.CalcCapDX(out DLeft,DRight: Integer);
var
  Twin1X, Twin1W, Twin2X, Twin2W, W,  WLeft1, WLeft2, WRight1, WRight2: Integer;
begin
  Twin1W:= 0;
  Twin1X:= CalcTwin1XPos;
  Twin2W:= 0;
  Twin2X:= CalcTwin2XPos;
  W:= GetSymbolWidth;
  WLeft1:= 0;
  WLeft2:= 0;
  WRight1:= 0;
  WRight2:= 0;
  if Assigned(Twin1) then
  begin
    Twin1W:= Twin1.Width div 2;
    if Twin1W>Twin1X then
      WLeft1:= Twin1W - Twin1X;
    if Twin1W>W-Twin1X then
      WRight1:= Twin1W - (W-Twin1X);
  end;
  if Assigned(Twin2) then
  begin
    Twin2W:= Twin2.Width div 2;
    if Twin2W>W-Twin2X then
      WRight2:= Twin2W - (W-Twin2X);
    if Twin2W>Twin2X then
      WLeft2:= Twin2W - Twin2X;
  end;
  DLeft:= Max(WLeft1,WLeft2);
  DRight:= Max(WRight1,WRight2);
end;

function TExprIntegral.CalcWidth: Integer;
begin
  Result:= GetSymbolWidth+CapDXLeft+CapDXRight+Son.Width+4*FWX;
end;

function TExprIntegral.CalcSingleSymbolWidth: Integer;
begin
  Result:= Round(0.57*GetSymbolHeight);
end;

function TExprIntegral.CalcTwin1XPos: Integer;
var
  W: Integer;
begin
  W:= CalcSingleSymbolWidth;
  if FMult>0 then
    Result:= (FMult-1)*FWX + FMult*(W div 4)
  else
    Result:= 2*FWX + 4*(W div 4);
end;

function TExprIntegral.CalcTwin2XPos: Integer;
var
  W: Integer;
begin
  W:= CalcSingleSymbolWidth;
  Result:=  CalcTwin1XPos + W - W div 4 - 2*FWX;
end;

procedure TExprIntegral.Paint(X, Y: Integer);
var
  H,SH,SW,Y1,Y2: Integer;
begin
  Inc(X, CapDXLeft+2*FWX);
  SW:= GetSymbolWidth;
  H:= MidLineUp;
  SH:= GetSymbolHeight div 2;
  {$IFDEF WINDOWS}
  Y1:= Y+H+SH-FWY;
  Y2:= Y+H-SH-FWY;
  {$ENDIF}
  {$IFDEF LINUX}
  Y1:= Y+H+SH+FWY;
  Y2:= Y+H-SH-FWY;
  {$ENDIF}
  if Assigned(Twin1) then
    Twin1.Draw(X+CalcTwin1XPos,Y1,ehCenter,evTop);
  if Assigned(Twin2) then
    Twin2.Draw(X+CalcTwin2XPos,Y2,ehCenter,evBottom);
  DrawSymbol(X+SW div 2, Y+H);
  Son.Draw(X+SW+CapDXRight+2*FWX,Y+H,ehLeft,evCenter);
end;

procedure TExprIntegral.DrawSymbol(X, Y: Integer);
var
  I: Integer;
  W,dW: Integer;
begin
  Canvas.Brush.Style:= bsClear;
  dW:= CalcSingleSymbolWidth div 2;
  if FMult<1 then
  begin
    W:= GetSymbolWidth;
    inherited DrawSymbol(X,Y);
    inherited DrawSymbol(X+dW+FWX,Y);
    Canvas.TextOut(X - W div 2 + 2*dW + 2*FWX,Y - 4*FWY, SCodeToStr(esEllipsis));
    inherited DrawSymbol(X+W-2*dW,Y);
  end
  else
    for I:=0 to FMult-1 do
      inherited DrawSymbol(X+I*(dW+FWX),Y);
end;

constructor TExprIntegral.Create(const AMainSymbolCode: Integer; const ASon,
  AFirstTwin, ASecondTwin: TExprClass; const AMult: Integer);
begin
  FMult:= AMult;
  inherited Create(AMainSymbolCode, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprInt }

constructor TExprInt.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass;
 const AMult: Integer);
begin
  inherited Create(esInt, ASon,AFirstTwin,ASecondTwin,AMult);
end;

{ TExprCirc }

constructor TExprCirc.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(esCirc, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprSurf }

function TExprSurf.CalcSingleSymbolWidth: Integer;
begin
  Result:= Round(0.75*GetSymbolHeight);
end;

constructor TExprSurf.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(esSurf, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprVolume }

function TExprVolume.CalcSingleSymbolWidth: Integer;
begin
  Result:= Round(0.89*GetSymbolHeight);
end;

constructor TExprVolume.Create(const ASon, AFirstTwin, ASecondTwin: TExprClass);
begin
  inherited Create(esVolume, ASon,AFirstTwin,ASecondTwin);
end;

{ TExprAsterisk }

constructor TExprAsterisk.Create;
begin
  inherited Create('*');
end;

procedure TExprAsterisk.Paint(X,Y: Integer);
var
  dY: Integer;
begin
  SetCanvasFont;
  {$IFDEF WINDOWS}
  dY:= Round(8*FRWY);
  {$ENDIF}
  {$IFDEF LINUX}
  dY:= Round(5*FRWY);
  {$ENDIF}
  Canvas.TextOut(X,Y+dY,FExpr);
end;

{ TExprCase }

function TExprCase.CalcSymbolHeight: Integer;
var
  OldFontSize: Integer;
  K: Extended;
begin
  OldFontSize:= Canvas.Font.Size;
  {$IFDEF WINDOWS}
  K:= 1.2;
  {$ENDIF}
  {$IFDEF LINUX}
  K:= 1.6;
  {$ENDIF}
  FSymbolFontSize:= ChooseFont(Canvas.Font, Height, FDes, K);
  Canvas.Font.Size:= FSymbolFontSize;
  Result:= TextHeight;
  Canvas.Font.Size:= OldFontSize;
end;

procedure TExprCase.DrawSymbol(X, Y: Integer);
var
  OldFontSize, W: Integer;
  OldFontStyle: TFontStyles;
begin
  W:= GetSymbolWidth div 4;
  OldFontSize:= Canvas.Font.Size;
  OldFontStyle:= Canvas.Font.Style;
  Canvas.Brush.Style:= bsClear;
  Canvas.Font.Style:= Canvas.Font.Style - [fsItalic];
  Canvas.Font.Size:= FSymbolFontSize;
  {$IFDEF WINDOWS}
  Canvas.TextOut(X-W, Y-Round(FRY*FDes),FSymbol);
  {$ENDIF}
  {$IFDEF LINUX}
  Canvas.TextOut(X-W, Y+Round(FRY*FDes),FSymbol);
  {$ENDIF}
  Canvas.Font.Style:= OldFontStyle;
  Canvas.Font.Size:= OldFontSize;
end;

function TExprCase.CalcSymbolWidth: Integer;
var
  OldFontSize: Integer;
begin
  GetSymbolHeight;
  OldFontSize:= Canvas.Font.Size;
  Canvas.Font.Size:= FSymbolFontSize;
  Result:= Canvas.TextWidth(FSymbol);
  Canvas.Font.Size:= OldFontSize;
end;

function TExprCase.CalcWidth: Integer;
var
  I: Integer;
  P: TExprClass;
begin
  FCol1Width:= 0;
  FCol2Width:= 0;
  P:= Son;
  I:= 1;
  while Assigned(P) do
  begin
    if Odd(I) then
      FCol1Width:= Max(FCol1Width,P.Width)
    else
      FCol2Width:= Max(FCol2Width,P.Width);
    Inc(I);
    P:= P.Next;
  end;
  Result:= GetSymbolWidth+FCol1Width+FCol2Width+12*FWX;
end;

function TExprCase.GetSymbolWidth: Integer;
begin
  if FToChange and tcSymbolWidth>0 then
  begin
    FSymbolWidth:= CalcSymbolWidth;
    FToChange:= FToChange and not tcSymbolWidth;
  end;
  Result:= FSymbolWidth;
end;

function TExprCase.GetSymbolHeight:Integer;
begin
  if FToChange and tcSymbolHeight>0 then
  begin
    FSymbolHeight:= CalcSymbolHeight;
    FToChange:= FToChange and not tcSymbolHeight;
  end;
  Result:= FSymbolHeight;
end;

procedure TExprCase.DrawContent(X, Y: Integer);
var
  I,H,W,H1,H2,X1,X2: Integer;
  P,PP: TExprClass;
begin
  W:= Width;
  H:= GetSymbolHeight;
  W:= GetSymbolWidth;
  X1:= X + Round(FRX*W);
  X2:= X + W + FCol1Width + 10*FWX;
  H:= Y;
  P:= Son;
  I:= 1;
  while Assigned(P) do
  begin
    if Odd(I) then
    begin
      H1:= P.MidLineUp;
      H2:= -P.MidLineDn;
      PP:= P;
    end
    else begin
      H1:= Max(H1,P.MidLineUp);
      H2:= Max(H2,-P.MidLineDn);
      Inc(H,H1);
      PP.Draw(X1,H,ehLeft,evCenter);
      P.Draw(X2,H,ehLeft,evCenter);
      Inc(H,H2);
      Inc(H,2*FWY);
    end;
    Inc(I);
    P:= P.Next;
  end;
end;

procedure TExprCase.Paint(X, Y: Integer);
begin
  DrawSymbol(X,Y);
  DrawContent(X,Y);
end;

function TExprCase.CalcHeight: Integer;
var
  H1,H2,I: Integer;
  P: TExprClass;
begin
  Result:= 0;
  P:= Son;
  I:= 1;
  while Assigned(P) do
  begin
    if Odd(I) then
    begin
      H1:= P.MidLineUp;
      H2:= -P.MidLineDn;
    end
    else begin
      Inc(Result,Max(H1,P.MidLineUp));
      Inc(Result,Max(H2,-P.MidLineDn));
      if Assigned(P.Next) then
        Inc(Result,2*FWY);
    end;
    Inc(I);
    P:= P.Next;
  end;
  if not Odd(Result) then
    Inc(Result);
end;

constructor TExprCase.Create(const ASon: TExprClass; const ASymbol: String; const ARX, ARY: Extended);
begin
  inherited Create(ASon);
  FSymbol:= ASymbol;
  FRX:= ARX;
  FRY:= ARY;
end;

{ TExprCaseOr }

constructor TExprCaseOr.Create(const ASon: TExprClass);
begin
  {$IFDEF WINDOWS}
  inherited Create(ASon, '[', 0.65, 0.62);
  {$ENDIF}
  {$IFDEF LINUX}
  inherited Create(ASon, '[', 0.8, 0.12);
  {$ENDIF}
end;

{ TExprCaseAnd }

constructor TExprCaseAnd.Create(const ASon: TExprClass);
begin
  {$IFDEF WINDOWS}
  inherited Create(ASon, '{', 0.6, 0.68);
  {$ENDIF}
  {$IFDEF LINUX}
  inherited Create(ASon, '{', 0.7, 0.28);
  {$ENDIF}
end;

{ TExprEmpty }

function TExprEmpty.CalcHeight:Integer;
begin
  SetCanvasFont;
  Result:= TextHeight;
end;

initialization

 SetOutputDPI;

end.
