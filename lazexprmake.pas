// Copyright © 2002 Anton Grigoriev. Contacts: <grigorievab@mail.ru>
// Copyright © 2019 Dmitriy Kornilov. Contacts: <dakor2017@yandex.ru>
// License: http://opensource.org/licenses/MIT

unit LazExprMake;

{$mode objfpc}{$H+}

interface

uses LazExprDraw, SysUtils;

type

  { TExprBuilder }

  TExprBuilder = class
  protected
    FBuild: Boolean;
    FPos: Integer;
    FExpr: String;
    function Preprocess(const AExpr: String): String;
    procedure AddMult(var AExisting: TExprClass; const AMultiplier: TExprClass);
    function MakePower(const ABase, AExponent: TExprClass): TExprClass;
    function MakeIndex(const ABase, AIndex: TExprClass): TExprClass;
    function MakeCap(const ABase: TExprClass; const AStyle: TExprCapStyle; const ACount: Integer): TExprClass;
    procedure Decorate(var ABase: TExprClass);
    function ExprString(const ANeed: Integer; const ACommaAllow: Boolean=False): TExprClass;
    function BoolExpr(out AFlags: Integer): TExprClass;
    function Expr(out AFlags: Integer): TExprClass;
    function Factor(out AFlags: Integer): TExprClass;
    function Trans(out AFlags: Integer): TExprClass;
    function Func(var AFlags: Integer): TExprClass;
    function FuncName(AName: String; var Flags: Integer; const Brackets: Boolean): TExprClass;
    function Token(const AName: String): TExprClass;
    function GreekLetter(const AName: String): Integer;
    function Comma: Boolean;
    procedure LookForComma;
    function NumberExt: Extended;
    function NumberInt: Integer;
  public
    FuncAutoIndex, VarAutoIndex, PostSymbols: Boolean;
    constructor Create;
    function BuildExpr(const AExpr: String): TExprClass;
    function SafeBuildExpr(const AExpr: String): TExprClass;
  end;

  EIncorrectExpr = class(Exception);

  function BuildExpr(const AExpr: String): TExprClass;
  function SafeBuildExpr(const AExpr: String): TExprClass;

implementation

const
  EChain=0;
  EExpression=1;
  EBracketed=2;
  EArgument=3;
  EPower=4;
  EAbs=5;
  ESquared=6;
  EFigured=7;

  FlagPower=1;
  FlagTrans=3;

  SymbolNotEqual          = #1;  // <> - не равно
  SymbolGreaterOrEqual    = #2;  // >= - больше или равно
  SymbolLessOrEqual       = #3;  // <= - меньше или равно
  SymbolMuchGreater       = #4;  // >> - много больше
  SymbolMuchLess          = #5;  // << - много меньше
  SymbolArrow             = #6;  // -> - стрелка (стремится к пределу)
  SymbolAlmostEqual       = #7;  // ~~ - примерно равно
  SymbolPlusMinus         = #8;  // +- - плюс-минус
  SymbolMinusPlus         = #9;  // -+ - минус-плюс
  SymbolIdentical         = #10; // == - тождественно
  SymbolDivide            = #11; // /+ - знак деления - "минус с точкой снизу и точкой сверху"
  SymbolCrossMultiply     = #12; // *+ - знак умножения - косой крест
  SymbolEllipsis          = #13; //... - многоточие
  SymbolApproxGreater     = #14; // >~ - больше или порядка
  SymbolApproxLess        = #15; // <~ - меньше или порядка
  SymbolSlash             = #16; // // - делить символом "/", без отрисовки дроби
  SymbolRequiredBracket   = #17; // !( - обязательная скобка (закрывается обычной)
  SymbolMultiply          = #18; // *. - знак умножения - точка
  SymbolMultiplyStay      = #19; // ** - умножение без перестановки множителей
  SymbolApproxEqual       = #20; // =~ - знак равенства с тильдой сверху (конгруэнтно)

  DiffSpaceCount = 3;

  LETTERS_LATIN = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LETTERS_CYR   = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  DIGITS        = '0123456789';

function IsStrIn(const AValue, AStr: String): Boolean;
begin
  Result:= SPos(AValue, AStr)>0;
end;

 {TExprBuilder}

constructor TExprBuilder.Create;
begin
  inherited Create;
  VarAutoIndex:= True;
  FuncAutoIndex:= True;
  PostSymbols:= True;
end;

function TExprBuilder.Preprocess(const AExpr: String): String;
var
  I: Integer;

  procedure SetResult(const AStr: String; const AInc: Integer);
  begin
    Result:= Result+AStr;
    Inc(I, AInc);
  end;

begin
   I:= 1;
   Result:= EmptyStr;
   while I<SLength(AExpr) do
   begin
     case SSymbol(AExpr, I) of
      '<': case SSymbol(AExpr, I+1) of
             '<': SetResult(SymbolMuchLess, 1);     // << - много меньше
             '>': SetResult(SymbolNotEqual, 1);     // <> - не равно
             '=': SetResult(SymbolLessOrEqual, 1);  // <= - меньше или равно
             '~': SetResult(SymbolApproxLess, 1)    // <~ - меньше или порядка
           else Result:= Result+'<'
           end;
      '>': case SSymbol(AExpr, I+1) of
             '=': SetResult(SymbolGreaterOrEqual, 1);  // >= - больше или равно
             '>': SetResult(SymbolMuchGreater, 1);     // >> - много больше
             '~': SetResult(SymbolApproxGreater, 1)    // >~ - больше или порядка
           else  Result:= Result+'>'
           end;
      '=': case SSymbol(AExpr, I+1) of
             '=': SetResult(SymbolIdentical, 1);  // == - тождественно
             '~': SetResult(SymbolApproxEqual, 1)  // =~ - знак равенства с тильдой сверху
           else  Result:= Result+'='
           end;
      '~': if SSymbol(AExpr, I+1)='~' then SetResult(SymbolAlmostEqual, 1) // ~~ - примерно равно
           else Result:= Result+'~';
      '+': if SSymbol(AExpr, I+1)='-' then SetResult(SymbolPlusMinus, 1) // +- - плюс-минус
           else Result:= Result+'+';
      '-': case SSymbol(AExpr, I+1) of
             '+': SetResult(SymbolMinusPlus, 1); // -+ - минус-плюс
             '>': SetResult(SymbolArrow, 1)      // -> - стрелка (стремится к пределу)
           else  Result:= Result+'-'
           end;
      '/': case SSymbol(AExpr, I+1) of
             '/': SetResult(SymbolSlash, 1);  // // - делить символом "/", без дроби
             '+': SetResult(SymbolDivide, 1)  // /+ - знак "минус с точкой снизу и точкой сверху"
           else Result:= Result+'/'
           end;
      '*': case SSymbol(AExpr, I+1) of
             '+': SetResult(SymbolCrossMultiply, 1); // *+ - косой крест
             '*': SetResult(SymbolMultiplyStay, 1);  // ** - умножение без перестановки множителей
             '.': SetResult(SymbolMultiply, 1)       // *. - умножение точкой
           else  Result:= Result+'*'
           end;
      '!': if SSymbol(AExpr, I+1)='(' then SetResult(SymbolRequiredBracket, 1)  // !( - обязательная скобка (закрывается обычной)
           else Result:= Result+'!';
      ' ': if (I+1<SLength(AExpr)) and
              (SSymbol(AExpr, I+1)='&') and
              (SSymbol(AExpr, I+2)=' ') then SetResult('&', 2) // " & " - то же самое, что и "&"
           else Result:= Result+' ';
      '.': if (I+1<SLength(AExpr)) and
              (SSymbol(AExpr, I+1)='.') and
              (SSymbol(AExpr, I+2)='.') then SetResult(SymbolEllipsis, 2) // ... - эллипсис
           else Result:= Result+'.'
     else
      Result:= Result + SSymbol(AExpr, I);
     end;
     Inc(I);
   end;
   Result:= Result + SSymbol(AExpr, I);
end;

procedure TExprBuilder.AddMult(var AExisting: TExprClass; const AMultiplier: TExprClass);
var
  ELast, MLast, Temp: TExprClass;
begin
  if not FBuild then
  begin
    AExisting:= nil;
    Exit;
  end;

  if not Assigned(AExisting) then
  begin
    AExisting:= AMultiplier;
    Exit;
  end;

  ELast:= AExisting;
  while Assigned(ELast.Next) do ELast:= ELast.Next;
  MLast:= AMultiplier;
  while Assigned(MLast.Next) do MLast:= MLast.Next;
  if (ELast.FTType and efRight>0) and (AMultiplier.FTType and efLeft>0) then
    AExisting.AddNext(AMultiplier)
  else begin
    if (MLast.FTType and efRight>0) and (AExisting.FTType and efLeft>0) then
    begin
      AMultiplier.AddNext(AExisting);
      AExisting:= AMultiplier
    end
    else begin
      if (AExisting is TExprNumber) and (MLast is TExprNumber) then
      begin
        TExprNumber(MLast).Number:= TExprNumber(MLast).Number*TExprNumber(AExisting).Number;
        MLast.Next:= AExisting.CutOff;
        FreeAndNil(AExisting);
        AExisting:= AMultiplier;
      end
      else begin
        if (AMultiplier.FTType and efLeft>0) and (MLast.FTType and efRight>0) then
        begin
          Temp:= AExisting;
          while Assigned(Temp.Next) do
          begin
            if (Temp.FTType and efRight>0) and (Temp.Next.FTType and efLeft>0) then
              Break
            else
              Temp:= Temp.Next;
          end;
          if Assigned(Temp.Next) then
          begin
            MLast.Next:= Temp.CutOff;
            Temp.Next:= AMultiplier;
          end
          else begin
            AExisting.AddNext(TExprSign.Create(esMultiply));
            AExisting.AddNext(AMultiplier);
          end;
        end
        else begin
          AExisting.AddNext(TExprSign.Create(esMultiply));
          AExisting.AddNext(AMultiplier);
        end;
      end;
    end;
  end;
end;

function TExprBuilder.MakePower(const ABase, AExponent: TExprClass): TExprClass;
var
  A: TExprClass;
begin
  Result:= nil;
  if not FBuild then Exit;

  if ABase is TExprCommonFunc then
  begin
    with TExprCommonFunc(ABase) do
    begin
      if (Son is TExprIndex) and (not Assigned(TExprIndex(Son).Twin2)) then
      begin
        TExprIndex(Son).Twin2:= AExponent;
        Result:= ABase;
        Exit;
      end
      else begin
        if not (Son is TExprIndex) then
        begin
          A:= TExprIndex.Create(CutOffSon, nil, AExponent);
          Son:= A;
          Result:= ABase;
          Exit;
        end;
      end;
    end;
  end;
  if (ABase is TExprIndex) and (not Assigned(TExprIndex(ABase).Twin2)) then
  begin
    TExprIndex(ABase).Twin2:=AExponent;
    Result:= ABase;
  end
  else
    Result:=TExprIndex.Create(ABase,nil,AExponent);
end;

function TExprBuilder.MakeIndex(const ABase, AIndex: TExprClass): TExprClass;
var
  A: TExprClass;
begin
  Result:= nil;
  if not FBuild then Exit;

  if ABase is TExprCommonFunc then
  begin
    with TExprCommonFunc(ABase) do
    begin
      if (Son is TExprIndex) and (not Assigned(TExprIndex(Son).Twin1)) then
      begin
        TExprIndex(Son).Twin1:= AIndex;
        Result:= ABase;
        Exit;
      end
      else begin
       if not (Son is TExprIndex) then
        begin
         A:= TExprIndex.Create(CutOffSon, AIndex, nil);
         Son:= A;
         Result:= ABase;
         Exit;
        end;
      end;
    end;
  end;
  if (ABase is TExprIndex) and (not Assigned(TExprIndex(ABase).Twin1)) then
  begin
    TExprIndex(ABase).Twin1:= AIndex;
    Result:= ABase;
  end
  else
   Result:= TExprIndex.Create(ABase, AIndex, nil);
end;

function TExprBuilder.MakeCap(const ABase: TExprClass; const AStyle: TExprCapStyle; const ACount:Integer): TExprClass;
var
  A: TExprClass;
begin
  Result:= nil;
  if not Assigned(ABase) then Exit;

  if ABase is TExprCommonFunc then
  begin
    with TExprCommonFunc(ABase) do
    begin
     A:= MakeCap(CutOffSon, AStyle, ACount);
     Son:= A;
     Result:= ABase;
    end;
  end
  else begin
    if (ABase is TExprIndex) and (not Assigned(TExprIndex(ABase).Twin2)) then
    begin
      with TExprIndex(ABase) do
      begin
        A:= TExprCap.Create(CutOffSon, AStyle, ACount);
        Son:= A;
        Result:= ABase;
      end
    end
    else
      Result:= TExprCap.Create(ABase, AStyle, ACount);
  end;
end;

procedure TExprBuilder.Decorate(var ABase: TExprClass);
var
  A: TExprClass;
begin
  if (ABase is TExprChain) and Assigned(TExprChain(ABase).Son.Next) then
  begin
    A:= TExprChain(ABase).CutOffSon;
    FreeAndNil(ABase);
    ABase:= TExprBracketed.Create(A, ebRound, ebRound);
  end
end;

function TExprBuilder.ExprString(const ANeed: Integer; const ACommaAllow: Boolean=False): TExprClass;
var
  Flags: Integer;
  A: TExprClass;
  Sep: String;

  function IsMark(const ASymbol: String): Boolean;
  begin
    Result:= (ASymbol=',') or (ASymbol=';');
  end;

begin
  Result:= BoolExpr(Flags);
  while (SSymbol(FExpr,FPos)='&') or (IsMark(SSymbol(FExpr,FPos)) and ACommaAllow) do
  begin
    Sep:= SSymbol(FExpr,FPos);
    Inc(FPos);
    if IsMark(Sep) then
     while (SSymbol(FExpr,FPos)=' ') and (FPos<SLength(FExpr)-1) do
      Inc(FPos);
    A:= BoolExpr(Flags);
    if FBuild then
    begin
      if Sep='&' then
        Result.AddNext(A)
      else begin
        case Sep of
        ',': Result.AddNext(TExprSeparator.Create(esComma));
        ';': Result.AddNext(TExprSeparator.Create(esSemicolon));
        end;
        Result.AddNext(TExprSpace.Create(7));
        Result.AddNext(A);
      end;
    end;
  end;
  if FBuild then
  begin
    case ANeed of
    EExpression: if Assigned(Result.Next) then Result:= TExprChain.Create(Result);
    EBracketed:  Result:= TExprRound.Create(Result);
    EArgument:   if Assigned(Result.Next) then Result:= TExprArgument.Create(Result);
    EPower:      if Assigned(Result.Next) then
                   Result:= TExprBase.Create(Result)
                 else if (Flags and FlagPower)=FlagPower then
                   Result:= TExprBracketed.Create(Result, ebRound, ebRound);
    EAbs:        Result:= TExprBracketed.Create(Result, ebModule, ebModule);
    ESquared:    Result:= TExprBracketed.Create(Result, ebSquare, ebSquare);
    EFigured:    Result:= TExprBracketed.Create(Result, ebFigure, ebFigure);
    end;
  end;
end;

function TExprBuilder.BoolExpr(out AFlags: Integer): TExprClass;
var
  LFlags, Sign: Integer;
  A: TExprClass;
begin
  Result:= Expr(LFlags);
  while IsStrIn(SSymbol(FExpr,FPos),
                '<>=~'+SymbolNotEqual+SymbolGreaterOrEqual+SymbolLessOrEqual+
                SymbolMuchGreater+SymbolMuchLess+SymbolArrow+SymbolAlmostEqual+
                SymbolIdentical+SymbolApproxGreater+SymbolApproxLess+SymbolApproxEqual) do
  begin
    case SSymbol(FExpr,FPos) of
     SymbolNotEqual:       Sign:= esNotEqual;
     SymbolGreaterOrEqual: Sign:= esGreaterOrEqual;
     SymbolLessOrEqual:    Sign:= esLessOrEqual;
     SymbolMuchGreater:    Sign:= esMuchGreater;
     SymbolMuchLess:       Sign:= esMuchLess;
     SymbolArrow:          Sign:= esArrowRight;
     SymbolAlmostEqual:    Sign:= esAlmostEqual;
     SymbolIdentical:      Sign:= esIdentical;
     SymbolApproxGreater:  Sign:= esApproxGreater;
     SymbolApproxLess:     Sign:= esApproxLess;
     SymbolApproxEqual:    Sign:= esApproxEqual;
     '<': Sign:= esLess;
     '=': Sign:= esEqual;
     '>': Sign:= esGreater;
     '~': Sign:= esTilde;
    end;
    Inc(FPos);
    A:= Expr(LFlags);
    if FBuild then
    begin
      Result.AddNext(TExprSign.Create(Sign));
      Result.AddNext(A)
    end
  end;
  if FBuild then
  begin
    if Assigned(Result.Next) then
      AFlags:=FlagPower
    else
      AFlags:=LFlags;
  end;
end;

function TExprBuilder.Expr(out AFlags: Integer): TExprClass;
var
  LFlags, Sign: Integer;
  A: TExprClass;
begin
  Result:= Trans(LFlags);
  while IsStrIn(SSymbol(FExpr,FPos), '-+'+SymbolPlusMinus+SymbolMinusPlus) do
  begin
    case SSymbol(FExpr,FPos) of
     SymbolPlusMinus:  Sign:= esPlusMinus;
     SymbolMinusPlus:  Sign:= esMinusPlus;
     '-': Sign:= esMinus;
     '+': Sign:= esPlus;
    end;
    Inc(FPos);
    A:= Trans(LFlags);
    if FBuild then
    begin
      if (LFlags and FlagTrans)=FlagTrans then
        A:= TExprBracketed.Create(A, ebRound, ebRound);
      Result.AddNext(TExprSign.Create(Sign));
      Result.AddNext(A);
    end
  end;
  if FBuild then
  begin
    if Assigned(Result.Next) then
      AFlags:=FlagPower
    else
      AFlags:=LFlags;
  end;
end;

function TExprBuilder.Trans(out AFlags: Integer): TExprClass;
var
  LFlags: Integer;
  D1, D2, A: TExprClass;

  procedure SetTrans(const SymbolCode: Integer);
  begin
    Inc(FPos);
    A:= Factor(LFlags);
    if FBuild then
    begin
      D1.AddNext(TExprSign.Create(SymbolCode));
      D1.AddNext(A);
    end
  end;

begin
  D2:= nil;
  D1:= Factor(LFlags);
  while IsStrIn(SSymbol(FExpr,FPos), SymbolDivide+SymbolCrossMultiply+SymbolSlash+
                                     SymbolMultiply+SymbolMultiplyStay+'*/\:') do
  begin
    case SSymbol(FExpr,FPos) of
    ':':                 SetTrans(esColon);
    '\':                 SetTrans(esBackSlash);
    SymbolDivide:        SetTrans(esDotsDivide);
    SymbolCrossMultiply: SetTrans(esCrossMultiply);
    SymbolSlash:         SetTrans(esSlash);
    SymbolMultiply:      SetTrans(esMultiply);
    SymbolMultiplyStay:  begin
                           Inc(FPos);
                           A:= Factor(LFlags);
                           if FBuild then D1.AddNext(A);
                         end;
    '*': begin
           Inc(FPos);
           AddMult(D1,Factor(LFlags));
         end;
    '/': begin
           Inc(FPos);
           AddMult(D2,Factor(LFlags));
         end;
    end;
  end;

  if FBuild then
  begin
    AFlags:= 0;
    if (not Assigned(D2)) and (not Assigned(D1.Next)) then AFlags:= LFlags;
    if Assigned(D2) then
    begin
      if Assigned(D1.Next) then D1:= TExprChain.Create(D1);
      if Assigned(D2.Next) then D2:= TExprChain.Create(D2);
      AFlags:= FlagPower;
      if (D1.FTType and efRoundBrackets)=efRoundBrackets then
        TExprBracketed(D1).RemoveBrackets;
      if (D2.FTType and efRoundBrackets)=efRoundBrackets then
        TExprBracketed(D2).RemoveBrackets;
      Result:= TExprRatio.Create(D1,D2);
    end
    else Result:= D1;
  end
  else Result:= nil;
end;

function TExprBuilder.NumberExt: Extended;
var
  A,B: Integer;
begin
   Val(SCopy(FExpr,FPos,255),Result,A);
   if A=1 then
     raise EIncorrectExpr.Create('Ожидается число в позиции '+IntToStr(FPos));
   B:= A;
   Val(SCopy(FExpr,FPos,B-1),Result,A);
   Inc(FPos,B-1);
end;

function TExprBuilder.NumberInt: Integer;
var
  A,B: Integer;
begin
   Val(SCopy(FExpr,FPos,255),Result,A);
   if A=1 then
     raise EIncorrectExpr.Create('Ожидается число в позиции '+IntToStr(FPos));
   B:= A;
   Val(SCopy(FExpr,FPos,B-1),Result,A);
   Inc(FPos,B-1);
end;

function TExprBuilder.Factor(out AFlags: Integer): TExprClass;
var
  B: TExprClass;
  R: Extended;
  D: Integer;

  procedure SetNumber(const ExpForm: Boolean);
  begin
    R:= NumberExt;
    if FBuild then
      Result:= TExprNumber.Create(R, ExpForm)
    else
      Result:= nil;
  end;

  procedure SetSign(const SymbolCode: Integer);
  begin
    Inc(FPos);
    AFlags:= FlagTrans;
    B:= Factor(D);
    if FBuild then
    begin
      Result:= TExprSign.Create(SymbolCode);
      Result.AddNext(B);
    end
    else Result:= nil;
  end;

  procedure SetBracket(const Need: Integer; const AllowComma: Boolean; const ABracket: String);
  begin
    Inc(FPos);
    Result:= ExprString(Need, AllowComma);
    if SSymbol(FExpr,FPos)=ABracket then
      Inc(FPos)
    else
      raise EIncorrectExpr.Create('Ожидается "' + ABracket + '" в позиции '+IntToStr(FPos));
  end;

begin
  AFlags:= 0;
  case SSymbol(FExpr,FPos) of
   '0'..'9': SetNumber(False);
   '#': begin
          Inc(FPos);
          if not IsStrIn(SSymbol(FExpr,FPos), DIGITS) then
            raise EIncorrectExpr('Ожидается цифра в позиции '+IntToStr(FPos));
          SetNumber(True);
        end;
   '+': SetSign(esPlus);
   '-': SetSign(esMinus);
   SymbolPlusMinus:  SetSign(esPlusMinus);
   SymbolMinusPlus:  SetSign(esMinusPlus);
   '[': SetBracket(ESquared, True, ']');
   '{': SetBracket(EFigured, True, '}');
   '(': SetBracket(EArgument, False, ')');
   '|': SetBracket(EAbs, True, '|');
   SymbolRequiredBracket: SetBracket(EBracketed, True, ')');
   SymbolEllipsis: begin
          Inc(FPos);
          if FBuild then
            Result:= TExprExtSymbol.Create(esEllipsis)
          else
            Result:= nil;
        end;
   '_': begin
          Inc(FPos);
          B:= Factor(AFlags);
          if FBuild then
            Result:= MakeCap(B, ecVector, 0)
          else
            Result:= nil
        end;
   'A'..'Z','a'..'z','А'..'я','Ё','ё': Result:= Func(AFlags)
  else
    raise EIncorrectExpr.Create('Недопустимый символ в позиции '+IntToStr(FPos))
  end;

  if PostSymbols then
  begin
    while IsStrIn(SSymbol(FExpr,FPos), '^_!`') do
    begin
      case SSymbol(FExpr,FPos) of
      '^': begin
              Inc(FPos);
              B:= Factor(D);
              if FBuild then
              begin
                if B is TExprArgument then
                  TExprArgument(B).RemoveBrackets;
                Decorate(Result);
                Result:= MakePower(Result,B);
              end;
              AFlags:= FlagPower;
           end;
      '_': begin
              Inc(FPos);
              PostSymbols:= False;
              B:= Factor(D);
              PostSymbols:= True;
              if FBuild then
              begin
                if B is TExprArgument then
                  TExprArgument(B).RemoveBrackets;
                Decorate(Result);
                Result:= MakeIndex(Result,B);
              end;
           end;
      '!': begin
              Inc(FPos);
              Decorate(Result);
              if FBuild then
                Result.AddNext(TExprSimple.Create('!'));
           end;
      '`': begin
              Inc(FPos);
              D:= 1;
              while SSymbol(FExpr,FPos)='`' do
              begin
                Inc(FPos);
                Inc(D);
              end;
              Decorate(Result);
              Result:= MakePower(Result,TExprStrokes.Create(D));
           end;
      end;
    end;
  end;
end;

function TExprBuilder.Func(var AFlags: Integer): TExprClass;
var
  N: String;
  I,J: Integer;
  WasIndex: Boolean;
begin
  N:= SSymbol(FExpr,FPos);
  Inc(FPos);
  while IsStrIn(SUpper(SSymbol(FExpr,FPos)), LETTERS_LATIN+LETTERS_CYR+DIGITS) do
  begin
    N:= N + SSymbol(FExpr,FPos);
    Inc(FPos);
  end;
  if (SSymbol(FExpr,FPos)='(') or (SSymbol(FExpr,FPos)=SymbolRequiredBracket) then
  begin
    Inc(FPos);
    Result:= FuncName(N,AFlags,SSymbol(FExpr,FPos-1)=SymbolRequiredBracket);
    if SSymbol(FExpr,FPos)=')' then
      Inc(FPos)
    else
      raise EIncorrectExpr.Create('Ожидается ")" в позиции '+IntToStr(FPos))
  end
  else begin
    if VarAutoIndex then
    begin
      I:=SLength(N);
      while IsStrIn(SSymbol(N,I), DIGITS) do
        Dec(I);
      if I<SLength(N) then
      begin
        WasIndex:= True;
        J:= StrToInt(SCopy(N,I+1,MaxInt));
        N:= SCopy(N,1,I);
      end
      else
        WasIndex:= False;
    end;
    Result:=Token(N);
    if FBuild and VarAutoIndex and WasIndex then
      Result:= TExprIndex.Create(Result,TExprNumber.Create(J,False),nil);
  end;
end;

function TExprBuilder.FuncName(AName: String; var Flags: Integer;
  const Brackets: Boolean): TExprClass;
var
  Str: String;

  procedure SetSYSTEM(const ABracket: TExprBracketStyle);
  var
    A,B: TExprClass;
  begin
    B:= ExprString(EExpression);
    while Comma do
    begin
      A:= ExprString(EExpression);
      if FBuild then B.AddNext(A);
    end;
    if FBuild then
      Result:= TExprBracketed.Create(TExprStand.Create(B,ehLeft),ABracket,ebNone);
  end;

  procedure SetSTAND(const AAlign: TExprHorAlign);
  var
    A,B: TExprClass;
  begin
    B:= ExprString(EExpression);
    while Comma do
    begin
      A:= ExprString(EExpression);
      if FBuild then B.AddNext(A);
    end;
    if FBuild then
      Result:= TExprStand.Create(B, AAlign);
  end;

  procedure SetTWIN(const AStr: String; const AMult: Integer = 0);
  var
    A,B1,B2: TExprClass;
  begin
    A:= ExprString(EArgument);
    if FBuild and Brackets then
    begin
      if A is TExprArgument then
        TExprArgument(A).SetBrackets
      else
        A:= TExprRound.Create(A);
    end;
    if Comma then
      B1:= ExprString(EExpression)
    else
      B1:= nil;
    if Comma then
      B2:= ExprString(EExpression)
    else
      B2:= nil;
    if FBuild then
    begin
      case AStr of
      'SUM': Result:= TExprSum.Create(A,B1,B2);
      'PROD':  Result:= TExprProd.Create(A,B1,B2);
      'CIRC':  Result:= TExprCirc.Create(A,B1,B2);
      'SURF':  Result:= TExprSurf.Create(A,B1,B2);
      'VOLUME':  Result:= TExprVolume.Create(A,B1,B2);
      'INT', 'INTM': Result:= TExprInt.Create(A,B1,B2,AMult);
      'AT': Result:= TExprAt.Create(A,B1,B2);
      end;
    end;
  end;

  procedure SetINTM;
  var
    N: Integer;
  begin
    N:= NumberInt;
    LookForComma;
    SetTWIN(Str, N);
  end;

  procedure SetPunctMark(const ACode: Integer);
  var
    N: Integer;
  begin
    N:= NumberInt;
    if FBuild then
    begin
      Result:= TExprSeparator.Create(ACode);
      Result.AddNext(TExprSpace.Create(N));
    end;
  end;

  function GetSTRING: String;
  begin
    if SSymbol(FExpr,FPos)='"' then
    begin
      Inc(FPos);
      Result:= '';
      repeat
        if SSymbol(FExpr,FPos)<>'"' then
          Result:= Result + SSymbol(FExpr,FPos)
        else if SSymbol(FExpr,FPos+1)='"' then
        begin
          Result:= Result + '"';
          Inc(FPos);
        end;
        Inc(FPos);
        if FPos>=SLength(FExpr) then
          raise EIncorrectExpr.Create('Незавершённая строка');
      until SSymbol(FExpr,FPos)='"';
      Inc(FPos);
    end
    else begin
      Result:= '';
      while SSymbol(FExpr,FPos)<>')' do
      begin
        Result:= Result + SSymbol(FExpr,FPos);
        Inc(FPos);
        if FPos>=SLength(FExpr) then
          raise EIncorrectExpr.Create('Незавершённая строка')
      end
    end;
  end;

  procedure SetSUBFUNC(const AFuncName: String = '');
  var
    S: String;
    A,B: TExprClass;
  begin
    Flags:= FlagPower;
    S:= AFuncName;
    if S='' then
    begin
      S:= GetSTRING;
      LookForComma;
    end;
    A:= ExprString(EExpression);
    LookForComma;
    B:= ExprString(EArgument);
    if FBuild and Brackets then
      if A is TExprArgument then
        TExprArgument(A).SetBrackets
      else
       A:= TExprRound.Create(A);
    if FBuild then
      Result:= TExprCommonFunc.Create(TExprSub.Create(A,S),B);
  end;

  procedure SetDIFF(const ASymbolExpr: TExprClass);
  var
    A,B: TExprClass;
  begin
    B:= ExprString(EPower);
    if Comma then
    begin
      A:= ExprString(EExpression);
      if FBuild then
      begin
        Result:= TExprSpace.Create(DiffSpaceCount);
        Result.AddNext(ASymbolExpr);
        Result.AddNext(TExprIndex.Create(B,nil,A));
      end;
    end
    else begin
      if FBuild then
      begin
        Result:= TExprSpace.Create(DiffSpaceCount);
        Result.AddNext(ASymbolExpr);
        Result.AddNext(B);
      end;
    end;
  end;

  procedure SetDIFFN(const ASymbolExpr: TExprClass);
  var
    A,B: TExprClass;
  begin
    B:= ExprString(EPower);
    if Comma then
    begin
      A:= ExprString(EPower);
      if FBuild then
      begin
        Result:= TExprSpace.Create(DiffSpaceCount);
        Result.AddNext(TExprIndex.Create(ASymbolExpr,nil,A));
        Result.AddNext(B);
      end;
    end
    else begin
      if FBuild then
      begin
        Result:= TExprSpace.Create(DiffSpaceCount);
        Result.AddNext(ASymbolExpr);
        Result.AddNext(B);
      end;
    end;
  end;

  procedure SetDIFFR(const ASymbolExpr1, ASymbolExpr2: TExprClass);
  var
    P0: Integer;
    A,B: TExprClass;
  begin
    B:= ExprString(EPower);
    if Comma then
    begin
      P0:=FPos;
      A:= ExprString(EExpression);
      if FBuild then
      begin
        Result:= ASymbolExpr1;
        Result.AddNext(TExprIndex.Create(B,nil,A));
        FPos:= P0;
        A:= ExprString(EExpression);
        Result:= TExprChain.Create(Result);
        Result:= TExprRatio.Create(TExprIndex.Create(ASymbolExpr2,nil,A),Result);
      end;
    end
    else begin
      if FBuild then
      begin
        Result:= ASymbolExpr1;
        Result.AddNext(B);
        Result:= TExprChain.Create(Result);
        Result:= TExprRatio.Create(ASymbolExpr2,Result);
      end;
    end;
  end;

  procedure SetDIFFRF(const ASymbolExpr1, ASymbolExpr2: TExprClass);
  var
    P0: Integer;
    A,B,C,D: TExprClass;
  begin
    D:= ExprString(EPower);
    LookForComma;
    B:= ExprString(EPower);
    if Comma then
    begin
      P0:= FPos;
      A:= ExprString(EExpression);
      if FBuild then
      begin
        Result:= ASymbolExpr1;
        Result.AddNext(TExprIndex.Create(B,nil,A));
        FPos:= P0;
        A:= ExprString(EExpression);
        Result:= TExprChain.Create(Result);
        C:= TExprIndex.Create(ASymbolExpr2,nil,A);
        C.AddNext(D);
        C:= TExprChain.Create(C);
        Result:= TExprRatio.Create(C,Result);
      end;
    end
    else begin
      if FBuild then
      begin
        Result:= ASymbolExpr1;
        Result.AddNext(B);
        Result:= TExprChain.Create(Result);
        C:= ASymbolExpr2;
        C.AddNext(D);
        C:= TExprChain.Create(C);
        Result:= TExprRatio.Create(C,Result);
      end;
    end;
  end;

  procedure SetCASE(const ALeftSymbol: String);
  var
    A,B: TExprClass;
  begin
    A:= ExprString(EExpression);
    while Comma do
    begin
      B:= ExprString(EExpression);
      if FBuild then A.AddNext(B);
    end;
    if FBuild then
    begin
      case ALeftSymbol of
      '(': Result:= TExprCaseAnd.Create(A);
      '{': Result:= TExprCaseOr.Create(A);
      end;
    end;
  end;

  procedure SetMATRIX;
  var
    N1,N2: Integer;
    A,B: TExprClass;
  begin
    N1:= NumberInt;
    LookForComma;
    N2:= NumberInt;
    LookForComma;
    B:= ExprString(EExpression);
    while Comma do
    begin
      A:= ExprString(EExpression);
      if FBuild then B.AddNext(A);
    end;
    if FBuild then
      Result:= TExprMatrix.Create(B,N1,N2);
  end;

  procedure SetCORR;
  var
    N: Integer;
    A,B: TExprClass;
  begin
    A:= ExprString(EExpression);
    N:= 1;
    while Comma do
    begin
      B:= ExprString(EExpression);
      if FBuild then A.AddNext(B);
      Inc(N);
    end;
    if Odd(N) then Inc(N);
    N:= N div 2;
    if FBuild then
      Result:= TExprCorr.Create(A,N);
  end;

  procedure SetCustomSymbol(const AName: String);
  var
    N: Integer;
  begin
    N:= NumberInt;
    if not FBuild then Exit;
    case AName of
    'SYMBOL': Result:= TExprExtSymbol.Create(N);
    'SYMBOLI': Result:= TExprExtSymbolItalic.Create(N);
    end;
  end;

  procedure SetNUM;
  var
    Pr,Dig,MD: Integer;
    X: Extended;
  begin
    X:= NumberExt;
    Dig:= 4;
    MD:= 2;
    if Comma then
    begin
      Pr:= NumberInt;
      if Comma then
      begin
        Dig:= NumberInt;
        if Comma then
          MD:= NumberInt;
      end;
    end
    else
      Pr:= 4;
    if FBuild then
      Result:= TExprExpNumber.Create(X,Pr,Dig,MD);
  end;

  procedure SetBRACKETS;
  var
    LeftBr,RightBr: TExprBracketStyle;
    A: TExprClass;
  begin
    case SSymbol(FExpr,FPos) of
     '(': LeftBr:= ebRound;
     '[': LeftBr:= ebSquare;
     '{': LeftBr:= ebFigure;
     '|': LeftBr:= ebModule;
     '0': LeftBr:= ebNone;
     '1': LeftBr:= ebAngle;
     '2': LeftBr:= ebFloor;
     '3': LeftBr:= ebCeil;
     '4': LeftBr:= ebNorm;
    else
      raise EIncorrectExpr.Create('Ожидается знак открывающей скобки в позиции '+IntToStr(FPos));
    end;
    Inc(FPos);
    if FPos>=SLength(FExpr) then
      raise EIncorrectExpr.Create('Незавершённая строка');
    case SSymbol(FExpr,FPos) of
     ')': RightBr:= ebRound;
     ']': RightBr:= ebSquare;
     '}': RightBr:= ebFigure;
     '|': RightBr:= ebModule;
     '0': RightBr:= ebNone;
     '1': RightBr:= ebAngle;
     '2': RightBr:= ebFloor;
     '3': RightBr:= ebCeil;
     '4': RightBr:= ebNorm;
    else
      raise EIncorrectExpr.Create('Ожидается знак закрывающей скобки в позиции '+IntToStr(FPos));
    end;
    Inc(FPos);
    LookForComma;
    A:= ExprString(EExpression);
    if FBuild then
      Result:= TExprBracketed.Create(A,LeftBr,RightBr);
  end;

  procedure SetLOG;
  var
    A,B: TExprClass;
  begin
    A:= ExprString(EExpression);
    LookForComma;
    B:= ExprString(EArgument);
    if FBuild and Brackets then
      if B is TExprArgument then
        TExprArgument(B).SetBrackets
      else
        B:= TExprRound.Create(B);
    if FBuild then
      Result:= TExprCommonFunc.Create(TExprIndex.Create(TExprFuncName.Create(AName),A,nil),B);
  end;

  procedure SetPOW;
  var
    A: TExprClass;
  begin
    Flags:= FlagPower;
    A:= ExprString(EPower);
    LookForComma;
    Result:= MakePower(A,ExprString(EExpression));
  end;

  procedure SetROOT;
  var
    A,B: TExprClass;
  begin
    B:= ExprString(EExpression);
    LookForComma;
    A:= ExprString(EExpression);
    if FBuild then
      Result:= TExprRoot.Create(A,B);
  end;

  procedure SetIND;
  var
    A: TExprClass;
  begin
    A:= ExprString(EPower);
    LookForComma;
    Result:= MakeIndex(A,ExprString(EExpression));
  end;

  procedure SetFUNC;
  var
    A,B: TExprClass;
  begin
    A:= ExprString(EExpression);
    LookForComma;
    B:= ExprString(EChain);
    while Comma do
    begin
      A:= ExprString(EChain);
      if FBuild then
      begin
        B.AddNext(TExprSeparator.Create);
        B.AddNext(A);
      end;
    end;
    if FBuild then
    begin
      B:= TExprArgument.Create(B);
      if Brackets then
        TExprArgument(B).SetBrackets;
      Result:= TExprCommonFunc.Create(A,B);
    end;
  end;

  procedure SetSPACE;
  var
    N: Integer;
  begin
    N:= NumberInt;
    if FBuild then
      Result:= TExprSpace.Create(N);
  end;

  procedure SetSTRING;
  var
    S: String;
  begin
    S:= GetSTRING;
    if FBuild then
      Result:= TExprSimple.Create(S);
  end;

  procedure SetSTROKES;
  var
    N: Integer;
    A: TExprClass;
  begin
    Flags:= FlagPower;
    A:= ExprString(EPower);
    if Comma then
      N:= NumberInt
    else
      N:=1;
    if FBuild then
      Result:=MakePower(A,TExprStrokes.Create(N));
  end;

  procedure SetPOINTS;
  var
    N: Integer;
    A: TExprClass;
  begin
    A:= ExprString(EExpression);
    if Comma then
      N:= NumberInt
    else
      N:=1;
    if FBuild then
      Result:= MakeCap(A,ecPoints,N);
  end;

  procedure SetCap(const AStyle: TExprCapStyle);
  var
    A: TExprClass;
  begin
    A:= ExprString(EExpression);
    if FBuild then
      Result:= MakeCap(A, AStyle, 0);
  end;

  procedure SetFACT;
  var
    A: TExprClass;
  begin
    Flags:= FlagPower;
    A:= ExprString(EPower);
    if FBuild then
    begin
      Result:= A;
      Result.AddNext(TExprSimple.Create('!'));
    end;
  end;

  procedure SetSQRT;
  var
    A: TExprClass;
  begin
    A:= ExprString(EExpression);
    if FBuild then
      Result:= TExprRoot.Create(A,nil);
  end;

  procedure SetSQR;
  begin
    Flags:= FlagPower;
    Result:= MakePower(ExprString(EPower),TExprNumber.Create(2,False))
  end;

  procedure SetOTHER;
  var
    X: Extended;
    I, GI: Integer;
    A: TExprClass;

    procedure SetCustomFunc;
    begin
      if not FBuild then Exit;
      GI:= GreekLetter(AName);
      if GI=0 then
        Result:= TExprFunc.Create(AName,A)
      else
        Result:= TExprCommonFunc.Create(TExprExtSymbol.Create(GI),A);
    end;

  begin
    A:= ExprString(EChain,True);
    if FBuild then
    begin
      A:= TExprArgument.Create(A);
      if Brackets then TExprArgument(A).SetBrackets;
    end;
    if FuncAutoIndex then
    begin
      I:= SLength(AName);
      while IsStrIn(SSymbol(AName,I), DIGITS) do
        Dec(I);
      if I<SLength(AName) then
      begin
        if FBuild then
        begin
          X:= StrToFloat(SCopy(AName,I+1,MaxInt));
          AName:= SCopy(AName,1,I);
          GI:= GreekLetter(AName);
          if GI=0 then
            Result:= MakeIndex(TExprFunc.Create(AName,A),TExprNumber.Create(X,False))
          else
            Result:= MakeIndex(TExprCommonFunc.Create(TExprExtSymbol.Create(GI),A),TExprNumber.Create(X,False));
        end;
      end
      else SetCustomFunc;
    end
    else SetCustomFunc;
  end;

begin
  Result:= nil;
  Flags:=0;
  Str:= SUpper(AName);
  if      Str='SQRT'      then SetSQRT
  else if Str='SQR'       then SetSQR
  else if Str='LOG'       then SetLOG
  else if Str='ABS'       then Result:= ExprString(EAbs)
  else if Str='POW'       then SetPOW
  else if Str='ROOT'      then SetROOT
  else if Str='IND'       then SetIND
  else if Str='LIM'       then SetSUBFUNC('lim')
  else if Str='FUNCSUB'   then SetSUBFUNC
  else if Str='FUNC'      then SetFUNC
  else if Str='SPACE'     then SetSPACE
  else if Str='DIFF'      then SetDIFF(TExprVar.Create('d'))              // Diff(x[,AName]) - дифференциал от dx^n
  else if Str='PDIFF'     then SetDIFF(TExprExtSymbol.Create(esPartDiff)) // PDiff(x[,AName]) - "частный дифференциал" от dx^n
  else if Str='DIFFN'     then SetDIFFN(TExprVar.Create('d'))             // DiffN(x[,AName]) - d(^n)x
  else if Str='PDIFFN'    then SetDIFFN(TExprExtSymbol.Create(esPartDiff))// PDiffN(x[,AName]) - d(^n)x - "частный дифференциал"
  else if Str='DIFFR'     then SetDIFFR(TExprVar.Create('d'), TExprVar.Create('d'))// DiffR(x[,AName]) - d(^n)/dx^n
  else if Str='PDIFFR'    then SetDIFFR(TExprExtSymbol.Create(esPartDiff), TExprExtSymbol.Create(esPartDiff))// DiffR(x[,AName]) - d(^n)/dx^n - частный дифференциал
  else if Str='DIFFRF'    then SetDIFFRF(TExprVar.Create('d'), TExprVar.Create('d'))// DiffRF(y,x[,AName]) - d(^n)y/dx^n
  else if Str='PDIFFRF'   then SetDIFFRF(TExprExtSymbol.Create(esPartDiff), TExprExtSymbol.Create(esPartDiff))
  else if Str='STRING'    then SetSTRING
  else if Str='STROKES'   then SetSTROKES
  else if Str='FACT'      then SetFACT
  else if Str='LINE'      then SetCap(ecLine)
  else if Str='VECT'      then SetCap(ecVector)
  else if Str='CAP'       then SetCap(ecCap)
  else if Str='TILDE'     then SetCap(ecTilde)
  else if Str='POINTS'    then SetPoints
  else if Str='STANDL'    then SetSTAND(ehLeft)
  else if Str='STANDC'    then SetSTAND(ehCenter)
  else if Str='STANDR'    then SetSTAND(ehRight)
  else if Str='MATRIX'    then SetMATRIX
  else if Str='CORR'      then SetCORR
  else if Str='AT'        then SetTWIN(Str)
  else if Str='SUM'       then SetTWIN(Str)
  else if Str='PROD'      then SetTWIN(Str)
  else if Str='CIRC'      then SetTWIN(Str)
  else if Str='SURF'      then SetTWIN(Str)
  else if Str='VOLUME'    then SetTWIN(Str)
  else if Str='INT'       then SetTWIN(Str, 1)
  else if Str='INTM'      then SetINTM
  else if Str='CASEAND'   then SetCASE('(')
  else if Str='CASEOR'    then SetCASE('{')
  else if Str='COLON'     then SetPunctMark(esColon)
  else if Str='SEMICOLON' then SetPunctMark(esSemicolon)
  else if Str='COMMA'     then SetPunctMark(esComma)
  else if Str='DOT'       then SetPunctMark(esDot)
  else if Str='BRACKETS'  then SetBrackets
  else if Str='SYSTEMAND' then SetSYSTEM(ebFigure)
  else if Str='SYSTEMOR'  then SetSYSTEM(ebSquare)
  else if Str='NUM'       then SetNUM
  else if Str='SYMBOL'    then SetCustomSymbol('SYMBOL')
  else if Str='SYMBOLI'   then SetCustomSymbol('SYMBOLI')
  else SetOTHER;
end;

function TExprBuilder.Token(const AName: String): TExprClass;
var
  M: String;
  GI: Integer;
begin
  Result:= nil;
  if not FBuild then Exit;
  M:= SUpper(AName);
  GI:= GreekLetter(AName);
  if GI<>0 then Result:= TExprExtSymbol.Create(GI)
  else if M='SEMICOLON'     then Result:= TExprExtSymbol.Create(esSemicolon)
  else if M='COLON'         then Result:= TExprExtSymbol.Create(esColon)
  else if M='DOT'           then Result:= TExprExtSymbol.Create(esDot)
  else if M='DOTSV'         then Result:= TExprExtSymbol.Create(esEllipsVert)
  else if M='DOTSH'         then Result:= TExprExtSymbol.Create(esEllipsHoriz)
  else if M='DOTSU'         then Result:= TExprExtSymbol.Create(esEllipsDiagUp)
  else if M='DOTSD'         then Result:= TExprExtSymbol.Create(esEllipsDiagDown)
  else if M='DEGREE'        then Result:= TExprExtSymbol.Create(esDegree)
  else if M='FORALL'        then Result:= TExprSign.Create(esForAll)
  else if M='INF'           then Result:= TExprExtSymbol.Create(esInfinity)
  else if M='PLANCK'        then Result:= TExprExtSymbolItalic.Create(esPlanck)
  else if M='EXISTS'        then Result:= TExprSign.Create(esExists)
  else if M='EXISTSN'       then Result:= TExprSign.Create(esNotExists)
  else if M='EMPTYSET'      then Result:= TExprExtSymbol.Create(esEmptySet)
  else if M='BELONGS'       then Result:= TExprSign.Create(esBelongs)
  else if M='BELONGSN'      then Result:= TExprSign.Create(esNotBelongs)
  else if M='INTERSECTION'  then Result:= TExprSign.Create(esIntersection)
  else if M='UNION'         then Result:= TExprSign.Create(esUnion)
  else if M='PLUSC'         then Result:= TExprSign.Create(esPlusCircle)
  else if M='MINUSC'        then Result:= TExprSign.Create(esMinusCircle)
  else if M='CROSSC'        then Result:= TExprSign.Create(esCrossCircle)
  else if M='SLASHC'        then Result:= TExprSign.Create(esSlashCircle)
  else if M='DOTC'          then Result:= TExprSign.Create(esDotCircle)
  else if M='CIRCLEC'       then Result:= TExprSign.Create(esCircleCircle)
  else if M='ASTERISKC'     then Result:= TExprSign.Create(esAsteriskCircle)
  else if M='EQUALC'        then Result:= TExprSign.Create(esEqualCircle)
  else if M='NIL'           then Result:= TExprClass.Create
  else if M='COMMA'         then Result:= TExprSeparator.Create
  else if M='ASTERISK'      then Result:= TExprAsterisk.Create
  else if M='MINUS'         then Result:= TExprExtSymbol.Create(esMinus)
  else if M='PARALLEL'      then Result:= TExprSign.Create(esParallel)
  else if M='PARALLELN'     then Result:= TExprSign.Create(esNotParallel)
  else if M='PERPENDICULAR' then Result:= TExprSign.Create(esPerpendicular)
  else if M='ANGLE'         then Result:= TExprSign.Create(esAngle)
  else if M='ARC'           then Result:= TExprSign.Create(esArc)
  else if M='DIVIDE'        then Result:= TExprSign.Create(esDivide)
  else if M='DIVIDEN'       then Result:= TExprSign.Create(esNotDivide)
  else if M='EMPTY'         then Result:= TExprEmpty.Create
  else if M='NATURAL'       then Result:= TExprExtSymbol.Create(esNatural)
  else if M='REAL'          then Result:= TExprExtSymbol.Create(esReal)
  else if M='RATIONAL'      then Result:= TExprExtSymbol.Create(esRational)
  else if M='ENTIRE'        then Result:= TExprExtSymbol.Create(esEntire)
  else if M='COMPLEX'       then Result:= TExprExtSymbol.Create(esComplex)
  else if M='QUATERNION'    then Result:= TExprExtSymbol.Create(esQuaternion)
  else if M='PROJECTIVE'    then Result:= TExprExtSymbol.Create(esProjective)
  else if M='AND'           then Result:= TExprSign.Create(esAnd)
  else if M='OR'            then Result:= TExprSign.Create(esOr)
  else if M='NOT'           then Result:= TExprSign.Create(esNot)
  else if M='XOR'           then Result:= TExprSign.Create(esXor)
  else if M='SUBSET'        then Result:= TExprSign.Create(esSubSet)
  else if M='SUBSETN'       then Result:= TExprSign.Create(esNotSubSet)
  else if M='SUPERSET'      then Result:= TExprSign.Create(esSuperSet)
  else if M='SUPERSETN'     then Result:= TExprSign.Create(esNotSuperSet)
  else if M='PROP'          then Result:= TExprSign.Create(esProportional)
  else if M='IDENT'         then Result:= TExprSign.Create(esIdentical)
  else if M='IDENTN'        then Result:= TExprSign.Create(esNotIdentical)
  else if M='DOTEQUAL'      then Result:= TExprSign.Create(esDotEqual)
  else if M='DOTEQUALC'     then Result:= TExprSign.Create(esDotsEqualCenter)
  else if M='DOTEQUALLR'    then Result:= TExprSign.Create(esDotsEqualLeftRight)
  else if M='DOTEQUALRL'    then Result:= TExprSign.Create(esDotsEqualRightLeft)
  else if M='ARROWL'        then Result:= TExprSign.Create(esArrowLeft)
  else if M='ARROWR'        then Result:= TExprSign.Create(esArrowRight)
  else if M='ARROWU'        then Result:= TExprSign.Create(esArrowUp)
  else if M='ARROWD'        then Result:= TExprSign.Create(esArrowDown)
  else if M='ARROWLR'       then Result:= TExprSign.Create(esArrowLeftRight)
  else if M='ARROWUD'       then Result:= TExprSign.Create(esArrowUpDown)
  else if M='ARROWLU'       then Result:= TExprSign.Create(esArrowLeftUp)
  else if M='ARROWRU'       then Result:= TExprSign.Create(esArrowRightUp)
  else if M='ARROWRD'       then Result:= TExprSign.Create(esArrowRightDown)
  else if M='ARROWLD'       then Result:= TExprSign.Create(esArrowLeftDown)
  else if M='ARROWLN'       then Result:= TExprSign.Create(esArrowLeftNot)
  else if M='ARROWRN'       then Result:= TExprSign.Create(esArrowRightNot)
  else if M='ARROWLRN'      then Result:= TExprSign.Create(esArrowLeftRightNot)
  else if M='DARROWL'       then Result:= TExprSign.Create(esDoubleArrowLeft)
  else if M='DARROWR'       then Result:= TExprSign.Create(esDoubleArrowRight)
  else if M='DARROWU'       then Result:= TExprSign.Create(esDoubleArrowUp)
  else if M='DARROWD'       then Result:= TExprSign.Create(esDoubleArrowDown)
  else if M='DARROWLR'      then Result:= TExprSign.Create(esDoubleArrowLeftRight)
  else if M='DARROWUD'      then Result:= TExprSign.Create(esDoubleArrowUpDown)
  else if M='DARROWLU'      then Result:= TExprSign.Create(esDoubleArrowLeftUp)
  else if M='DARROWRU'      then Result:= TExprSign.Create(esDoubleArrowRightUp)
  else if M='DARROWRD'      then Result:= TExprSign.Create(esDoubleArrowRightDown)
  else if M='DARROWLD'      then Result:= TExprSign.Create(esDoubleArrowLeftDown)
  else if M='DARROWLN'      then Result:= TExprSign.Create(esDoubleArrowLeftNot)
  else if M='DARROWRN'      then Result:= TExprSign.Create(esDoubleArrowRightNot)
  else if M='DARROWLRN'     then Result:= TExprSign.Create(esDoubleArrowLeftRightNot)
  else if M='BARROWL'       then Result:= TExprSign.Create(esBarArrowLeft)
  else if M='BARROWR'       then Result:= TExprSign.Create(esBarArrowRight)
  else if M='BARROWU'       then Result:= TExprSign.Create(esBarArrowUp)
  else if M='BARROWD'       then Result:= TExprSign.Create(esBarArrowDown)
  else if M='BARROWD'       then Result:= TExprSign.Create(esBarArrowDown)
  else if M='HARROWLR'      then Result:= TExprSign.Create(esHarpArrowsLeftRight)
  else if M='HARROWRL'      then Result:= TExprSign.Create(esHarpArrowsRightLeft)
  else if M='TARROWRL'      then Result:= TExprSign.Create(esTwoArrowsRightLeft)
  else if M='TARROWLR'      then Result:= TExprSign.Create(esTwoArrowsLeftRight)
  else if M='TARROWUD'      then Result:= TExprSign.Create(esTwoArrowsUpDown)
  else if M='TARROWDU'      then Result:= TExprSign.Create(esTwoArrowsDownUp)
  else if M='TARROWL'       then Result:= TExprSign.Create(esTwoArrowsLeft)
  else if M='TARROWR'       then Result:= TExprSign.Create(esTwoArrowsRight)
  else if M='TARROWU'       then Result:= TExprSign.Create(esTwoArrowsUp)
  else if M='TARROWD'       then Result:= TExprSign.Create(esTwoArrowsDown)
  else if M='TRARROW'       then Result:= TExprSign.Create(esThreeArrows)
  else if M='SIMILAR'       then Result:= TExprSign.Create(esSimilar)
  else if M='TRIANGLE'      then Result:= TExprSign.Create(esTriangle)
  else if M='QUADRATE'      then Result:= TExprSign.Create(esQuadrate)
  else if M='RECTANGLE'     then Result:= TExprSign.Create(esRectangle)
  else if M='CIRCLE'        then Result:= TExprSign.Create(esCircle)
  else if M='PARALLELOGRAM' then Result:= TExprSign.Create(esParallelogram)
  else if M='RHOMB'         then Result:= TExprSign.Create(esRhomb)
  else if M='END'           then Result:= TExprSign.Create(esEnd)
  else if M='BEGIN'         then Result:= TExprSign.Create(esBegin)
  else if M='CONST' then
  begin
    Result:= TExprSimple.Create(AName);
    Result.AddNext(TExprSpace.Create(3));
  end
  else begin
    if IsStrIn(SUpper(SSymbol(AName,1)), LETTERS_LATIN) then
      Result:= TExprVar.Create(AName)
    else
      Result:= TExprSimple.Create(AName)
  end;
end;

function TExprBuilder.GreekLetter(const AName: String): Integer;
var
  M: String;
  DS:Integer;
begin
  M:= SUpper(AName);
  if IsStrIn(SSymbol(AName,1), LETTERS_LATIN) then
    DS:= 0
  else
    DS:= 32;
  Result:=0;
  case M of
  'ALPHA'  : Result:= esAlphaBig+DS;
  'BETA'   : Result:= esBetaBig+DS;
  'GAMMA'  : Result:= esGammaBig+DS;
  'DELTA'  : Result:= esDeltaBig+DS;
  'EPSILON': Result:= esEpsilonBig+DS;
  'ZETA'   : Result:= esZetaBig+DS;
  'ETA'    : Result:= esEtaBig+DS;
  'THETA'  : Result:= esThetaBig+DS;
  'IOTA'   : Result:= esIotaBig+DS;
  'KAPPA'  : Result:= esKappaBig+DS;
  'LAMBDA' : Result:= esLambdaBig+DS;
  'MU'     : Result:= esMuBig+DS;
  'NU'     : Result:= esNuBig+DS;
  'XI'     : Result:= esXiBig+DS;
  'OMICRON': Result:= esOmicronBig+DS;
  'PI'     : Result:= esPiBig+DS;
  'RHO'    : Result:= esRhoBig+DS;
  'SIGMA'  : Result:= esSigmaBig+DS;
  'TAU'    : Result:= esTauBig+DS;
  'UPSILON': Result:= esUpsilonBig+DS;
  'PHI'    : Result:= esPhiBig+DS;
  'CHI'    : Result:= esChiBig+DS;
  'PSI'    : Result:= esPsiBig+DS;
  'OMEGA'  : Result:= esOmegaBig+DS;
  'SIGMAO' : Result:= esSigmaOther;
  'THETAO' : Result:= esThetaOther;
  'NABLA'  : Result:= esNabla;
  'PLAMBDA': Result:= esLambdaSpec;
  end;
end;

function TExprBuilder.Comma: Boolean;
begin
  Result:= SSymbol(FExpr,FPos)=',';
  if Result then
  begin
    Inc(FPos);
    while (FPos<SLength(FExpr)) and (SSymbol(FExpr,FPos)=' ') do
      Inc(FPos);
  end;
end;

procedure TExprBuilder.LookForComma;
begin
  if not Comma then
    raise EIncorrectExpr.Create('Ожидается "," в позиции '+IntToStr(FPos));
end;

function TExprBuilder.SafeBuildExpr(const AExpr: String): TExprClass;
begin
  FExpr:= PreProcess(AExpr) + ' ';
  FBuild:= False;
  FPos:= 1;
  ExprString(EExpression,True);
  FBuild:= True;
  FPos:= 1;
  Result:= ExprString(EExpression,True);
end;

function TExprBuilder.BuildExpr(const AExpr: String): TExprClass;
begin
  FExpr:= PreProcess(AExpr) + ' ';
  FBuild:= True;
  FPos:= 1;
  Result:= ExprString(EExpression,True);
end;

function SafeBuildExpr(const AExpr: String): TExprClass;
var
  Builder: TExprBuilder;
begin
  Builder:= TExprBuilder.Create;
  try
    Result:= Builder.SafeBuildExpr(AExpr);
  finally
    FreeAndNil(Builder);
  end;
end;

function BuildExpr(const AExpr: String): TExprClass;
var
  Builder: TExprBuilder;
begin
  Builder:= TExprBuilder.Create;
  try
    Result:= Builder.BuildExpr(AExpr);
  finally
    FreeAndNil(Builder);
  end;
end;

end.
