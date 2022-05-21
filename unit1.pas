unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Printers;

type

  TArticle = record
    Title, Desc: String;
    Samples: array of String;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RusRadioButton: TRadioButton;
    EngRadioButton: TRadioButton;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabControl1: TTabControl;
    procedure Button1Click(Sender: TObject);
    procedure EngRadioButtonClick(Sender: TObject);
    procedure RusRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    Arts: array of TArticle;
    BM: TBitmap;
    procedure AddArt(const ATitle, ADesc: String; const ASamples: array of String);
    procedure GetArtStrings(out S1, S2, S3: String);
    procedure PrintArt(const AIndex: Integer);
    procedure ShowArt(const AIndex: Integer);
    procedure SetArts;
    procedure SetArtList(const AKindIndex: Integer);
    procedure SetInfo;
    procedure SetInfoEng;
    procedure SetInfoRus;
    procedure SetSigns;
    procedure SetSignsEng;
    procedure SetSignsRus;
    procedure SetBeforeSymbols;
    procedure SetBeforeSymbolsEng;
    procedure SetBeforeSymbolsRus;
    procedure SetAfterSymbols;
    procedure SetAfterSymbolsEng;
    procedure SetAfterSymbolsRus;
    procedure SetConcatenates;
    procedure SetConcatenatesEng;
    procedure SetConcatenatesRus;
    procedure SetBrackets;
    procedure SetBracketsEng;
    procedure SetBracketsRus;
    procedure SetGreekLetters;
    procedure SetGreekLettersEng;
    procedure SetGreekLettersRus;
    procedure SetArrows;
    procedure SetArrowsEng;
    procedure SetArrowsRus;
    procedure SetTokens;
    procedure SetTokensEng;
    procedure SetTokensRus;
    procedure SetFunctions;
    procedure SetFunctionsEng;
    procedure SetFunctionsRus;
    procedure SetMainStrings;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses LazExprDraw, LazExprMake;


{ TForm1 }

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  SetArts;
end;

procedure TForm1.AddArt(const ATitle, ADesc: String; const ASamples: array of String);
var
  I: Integer;
begin
  SetLength(Arts, Length(Arts)+1);
  with Arts[High(Arts)] do
  begin
     Title:= ATitle;
     Desc:= ADesc;
     SetLength(Samples, Length(ASamples));
     for I:= Low(ASamples) to High(ASamples) do
       Samples[I]:= ASamples[I];
  end;
  ListBox1.Items.Add(ATitle);
end;

procedure TForm1.GetArtStrings(out S1,S2,S3: String);
begin
  if EngRadioButton.Checked then
  begin
    S1:= 'String(Example ';
    S2:= 'Entry:  ';
    S3:= 'Result:  ';
  end
  else begin
    S1:= 'String(Пример ';
    S2:= 'Cтрока:  ';
    S3:= 'Результат:  ';
  end;
end;

procedure TForm1.PrintArt(const AIndex: Integer);
var
  Builder: TExprBuilder;
  Expr: TExprClass;
  I, H: Integer;
  S1, S2, S3: String;
const
  dH1 = 5;
  dH2 = 10;
begin
  GetArtStrings(S1, S2, S3);

  Builder:= TExprBuilder.Create;
  try
    H:= 5;
    SetOutputDPI(Printer.XDPI, Printer.YDPI);
    Printer.BeginDoc;
    for I:=0 to High(Arts[AIndex].Samples) do
    begin
      Expr:= Builder.BuildExpr(S1+IntToStr(I+1)+':)');
      Expr.Canvas:= Printer.Canvas;
      Expr.Color:= clBlack;
      Expr.Font.Size:= 13;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1;
      FreeAndNil(Expr);

      Expr:= TExprVar.Create(S2);
      Expr.AddNext(TExprSimple.Create(Arts[AIndex].Samples[I]));
      Expr:= TExprChain.Create(Expr);
      Expr.Canvas:= Printer.Canvas;
      Expr.Font.Size:= 12;
      Expr.Color:= clBlack;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1;
      FreeAndNil(Expr);

      Expr:= TExprVar.Create(S3);
      Expr.AddNext(Builder.BuildExpr(Arts[AIndex].Samples[I]));
      Expr:= TExprChain.Create(Expr);
      Expr.Canvas:= Printer.Canvas;
      Expr.Font.Size:= 12;
      Expr.Color:= clBlack;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1 + dH2;
      FreeAndNil(Expr);
    end;
    Printer.EndDoc;
  finally
    FreeAndNil(Builder);
  end;
end;

procedure TForm1.ShowArt(const AIndex: Integer);
var
  Builder: TExprBuilder;
  Expr: TExprClass;
  I, H, W: Integer;
  S1, S2, S3: String;
const
  {$IFDEF WINDOWS}
  dH1 = 0;
  {$ENDIF}
  {$IFDEF LINUX}
  dH1 = 5;
  {$ENDIF}
  dH2 = 10;
begin
  GetArtStrings(S1, S2, S3);

  Memo1.Text:= Arts[AIndex].Desc;

  BM.Width:= 3000;
  BM.Height:= 3000;
  BM.Canvas.Brush.Color:= clWindow;
  BM.Canvas.FillRect(BM.Canvas.ClipRect);

  Builder:= TExprBuilder.Create;
  try
    H:= 5;
    W:= 0;
    SetOutputDPI;
    for I:=0 to High(Arts[AIndex].Samples) do
    begin
      Expr:= Builder.BuildExpr(S1+IntToStr(I+1)+':)');
      Expr.Color:= clRed;
      Expr.Canvas:= BM.Canvas;
      Expr.Font.Size:= 13;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1;
      if W<Expr.Width then W:= Expr.Width;
      FreeAndNil(Expr);

      Expr:= TExprVar.Create(S2);
      Expr.Color:= clGreen;
      Expr.AddNext(TExprSimple.Create(Arts[AIndex].Samples[I]));
      Expr.Next.Color:= clWindowText;
      Expr:= TExprChain.Create(Expr);
      Expr.Canvas:= BM.Canvas;
      Expr.Font.Size:= 13;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1;
      if W<Expr.Width then W:= Expr.Width;
      FreeAndNil(Expr);

      Expr:= TExprVar.Create(S3);
      Expr.Color:= clGreen;
      Expr.AddNext(Builder.BuildExpr(Arts[AIndex].Samples[I]));
      Expr.Next.Color:= clWindowText;
      Expr:= TExprChain.Create(Expr);
      Expr.Canvas:= BM.Canvas;
      Expr.Font.Size:= 13;
      Expr.Draw(5,H,ehLeft,evTop);
      H:= H + Expr.Height + dH1 + dH2;
      if W<Expr.Width then W:= Expr.Width;
      FreeAndNil(Expr);
    end;
    BM.Height:= H;
    BM.Width:= W + 10;
    Image1.Picture.Bitmap.Assign(BM);
  finally
    FreeAndNil(Builder);
  end;
end;

procedure TForm1.SetArts;
begin
  SetArtList(TabControl1.TabIndex);
  ShowArt(ListBox1.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BM:= TBitmap.Create;
  TabControl1.TabIndex:= 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PrintArt(ListBox1.ItemIndex);
end;

procedure TForm1.RusRadioButtonClick(Sender: TObject);
begin
  SetMainStrings;
  SetArts;
end;

procedure TForm1.EngRadioButtonClick(Sender: TObject);
begin
  SetMainStrings;
  SetArts;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BM);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetArts;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  ShowArt(ListBox1.ItemIndex);
end;



procedure TForm1.SetArtList(const AKindIndex: Integer);
begin
  Arts:= nil;
  ListBox1.Clear;
  case AKindIndex of
  0: SetInfo;
  1: SetSigns;
  2: SetBeforeSymbols;
  3: SetAfterSymbols;
  4: SetConcatenates;
  5: SetBrackets;
  6: SetGreekLetters;
  7: SetArrows;
  8: SetTokens;
  9: SetFunctions;
  end;
  ListBox1.ItemIndex:= 0;
end;

procedure TForm1.SetInfoRus;
begin
  AddArt('1. Выражения',
     '     Строка, описывающая формулу состоит из выражений. К простым '+
     'выражениям относятся идентификаторы, токены и числовые константы.'#13#10+
     '     Токены – это зарезервированные слова, использующиеся для '+
     'обозначения специальных символов (пример 1). Регистр символов при '+
     'написании токенов не важен, за исключением токенов, обозначающих '+
     'греческие буквы. Если первый символ такого токена имеет верхний регистр, '+
     'то получается заглавная буква, если нижний – строчная.'#13#10+
     '     Идентификаторы воспроизводятся так, как они написаны в тексте. ' +
     'Идентификаторы могут состоять из английских и русских букв и цифр, '+
     'причём начинаться идентификатор должен с буквы. Если идентификатор '+
     'начинается с английской буквы, он выводится наклонным шрифтом  '+
     '(пример 2), если с русской – прямым (пример 3). Вывести английский '+
     'текст прямым шрифтом можно с помощью функции String. Если в конце '+
     'идентификатора стоят цифры, а TexprBuilder.VarAutoIndex:=True '+
     '(по умолчанию), то они рассматриваются как нижний индекс (пример 4). '+
     'Если цифры стоят после токена, то они также рассматриваются как '+
     'нижний индекс (пример 5).'#13#10+
     '     Числовые константы записываются в обычном для Паскаля синтаксисе '+
     '(пример 6). Допускается использование английской «e» или «E» для '+
     'обозначения экспоненциальной части числа. Оптимальный формат вывода '+
     'числа подбирается автоматически. Если он по каким-то причинам не '+
     'устраивает, можно воспользоваться символом «#» или функцией Num.'#13#10+
     '     Сложные выражения состоят из операндов, разделённых знаками ' +
     'операций (пример 7). Операнды могут быть одного из следующих видов:'#13#10+
     '     1. Простое выражение, т.е. числовая константа (в том числе '+
     'начинающаяся с символа «#»), идентификатор или токен.'#13#10+
     '     2. Выражение, заключенное в круглые, квадратные, прямые или '+
     'фигурные скобки.'#13#10+
     '     3. Функция.',
     ['Alpha, alpha','EnglEnc','РУС','x12','beta0','1.234,0.7e9,1234678',
      'B_(2*k)=((-1)^(n-1)**2*(2*k)!/(2*pi)^(2*k))*sum(1/n^(2*k),n=1,inf)']);
  AddArt('2. Единицы ширины',
      'Единицы ширины, использующиеся в некоторых зарезервированных функциях для ' +
      'указания размера пробела, подобраны так, что одна единица ширины примерно ' +
      'равна толщине вертикальной линии в символе "+".', ['a&space(20)&b']);
  AddArt('3. Функции',
      '1. Функцией считается текст, после которого в скобках стоит один или '+
      'несколько аргументов. Существуют зарезервированные имена функций.'#13#10+
      '2. Если имя функции не является зарезервированным словом, результат '+
      'зависит от длины имени. Имена функций, состоящие из одного символа, '+
      'выводятся курсивом, а их аргументы всегда заключаются в круглые скобки '+
      '(пример 1). Более длинные имена функций выводятся прямым шрифтом, а их '+
      'аргументы заключаются в скобки только при необходимости (пример 2). '+
      'Можно принудительно заключить аргумент в скобки, используя вместо '+
      'открывающей скобки комбинацию "!(" (пример 3).'#13#10+
      '3. При возведении функции в степень или присоединении к ней индекса '+
      'знаки соответствующих операций должны стоять после аргумента (пример 4).'#13#10+
      '4. Цифры, стоящие в конце имени функции, интерпретируются как нижний '+
      'индекс, если TExprBuilder.FuncAutoIndex:=True (по умолчанию; пример 5).'#13#10+
      '5. В качестве имён функций можно использовать токены, обозначающие '+
      'греческие буквы, а также токены Nabla и PLambda. В этом случае цифры '+
      'в конце также интерпретируются как нижний индекс (пример 6).'#13#10+
      '6. Имена зарезервированных функций нечувствительны к регистру ' +
      'символов. При описании зарезервированных функций используются следующие '+
      'условные обозначения:'#13#10+
      '     E,E1,E2 и т. д. – произвольные обязательные выражения (если '+
      'требуется, чтобы обязательное выражение было пустым, нужно вместо него '+
      'вставлять токен Nil (пример7));'#13#10+
      '     [,E1] и т.д. – произвольные необязательные выражения (могут не '+
      'прописываться в зависимости от ситуации);'#13#10+
      '     m,n,n1,n2 и т. д. – целочисленные константы;'#13#10+
      '     R - вещественная константа.',
      ['f(x,y,z)','cos(x), sin(pi/2+x), tg(1/x)','cos!(x)','f(x)_n,cos(x)^2',
       'f0(x)=g1(x)','gamma0(x)', 'lim(x->0,f(x)), lim(Nil,f(x))']);
  AddArt('4. Примеры', 'Примеры различных формул',
      ['Si!(x)=Int((sin(x)/x)*Diff(x),0,x)=pi/2-Int((sin(x)/x)*Diff(x),x,+Inf)=x-(1/3!)*(x^3/3)+(1/5!)*(x^5/5)-+...',
       'FuncSub("Res",z=a,f(z))=lim(z->a,[(1/(m-1)!)*.DiffR(z,m-1)*[(z-a)^m*.f(z)]])',
       'P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)=P(H_i)*P(E&Divide&H_i)/Sum(P(H_i)*P(E&Divide&H_i),i)',
       '{StandC(1,1&space(15)&2)}_S&Ident&{StandC(1,2&space(15)&1)}_S&Ident&(G*E_thetao-F*G_u)/(2*(E*G-F^2))',
       'SystemAnd(a11*x1+a12*x2+...+a_(1&n)*x_n=b1 & Comma,'+
       'a21*x1+a22*x2+...+a_(2&n)*x_n=b2 & Comma,DotsV,'+
       'a_(n&1)*x1+a_(n&2)*x2+...+a_nn*x_n=b_n & Semicolon)&'+
       'String(     или     ) & Sum(a_(i*k)*b_i,k=1,n),!(i=1,2,...,n)',
       'A*.X=B & String(,     где  )&'+
       'A=!(Matrix(4,4,a11,a12,DotsH,a_(1&n),a21,a22,DotsH,a_(2&n),DotsV,DotsV,DotsH,DotsV,a_(n&1),a_(n&2),DotsH,a_nn))&comma(20)&'+
       'X=!(StandC(x1,x2,DotsV,x_n))&comma(20)&B=!(StandC(b1,b2,DotsV,b_n)) & Dot',
       'Int(Int(x*y*Diff(x)*Diff(y),x^2,sqrt(x)),0,1)=Int(At([(1/2)*x*y^2],x^2,sqrt(x))**Diff(x),0,1)='+
       '(1/2)*Int(x*(x-x^4)*Diff(x),0,1)=(1/2)*At(x^3/3-x^6/6,0,1)=1/12']);
end;

procedure TForm1.SetInfoEng;
begin
  AddArt('1. Expressions',
     '     The string describing the formula consists of expressions. Simple '+
     'expressions include identifiers, tokens, and numeric constants.'#13#10+
     '     Tokens are reserved words used to denote special characters '+
     '(see example 1). Tokens are case insensitive, '+
     'with the exception of tokens denoting Greek letters. If the first '+
     'character of such a token has an uppercase, then a capital letter '+
     'is obtained, if the lower one is lowercase.'#13#10+
     '     Identifiers are displayed as they are written. Identifiers can ' +
     'consist of English and Russian letters and numbers. The identifier must '+
     'begin with a letter. If the identifier begins with an English letter, '+
     'it is displayed in italics (example 2), if with Russian - in the regular  '+
     'font (example 3). You can display English text in regular font using '+
     'the function String. If at the end of the identifier are numbers, '+
     'and TExprBuilder.VarAutoIndex:=True (by default), then they are '+
     'displayed as a subscript (example 4). If the numbers are after the '+
     'token, then they are also displayed as a subscript (example 5).'#13#10+
     '     Numeric constants are written in the Pascal syntax (example 6). '+
     'You can use the English "e" or "E" to indicate the exponential part of '+
     'the number. The optimal format for displaying the number is selected '+
     'automatically. If this format doesn’t suit you for some reason, '+
     'you can use the “#” symbol or the Num function.'#13#10+
     '      Complex expressions consist of operands separated by operation ' +
     'symbols. Operands can be one of the following types:'#13#10+
     '     1. Simple expression i.e. numeric constant (including starting with '+
     'the symbol "#"), identifier or token.'#13#10+
     '     2. Expression enclosed in round, square or figure brackets.'#13#10+
     '     3. Function.',
     ['Alpha, alpha','EnglEnc','РУС','x12','beta0','1.234,0.7e9,1234678',
      'B_(2*k)=((-1)^(n-1)**2*(2*k)!/(2*pi)^(2*k))*sum(1/n^(2*k),n=1,inf)']);
  AddArt('2. Units of width ',
      'Units of width are used in some reserved functions to set space. ' +
      'One unit of width is approximately equal to the width of the vertical ' +
      'line in the "+" symbol.', ['a&space(20)&b']);
  AddArt('3. Functions',
      '1. A function is a text followed by one or more arguments in parentheses. '+
      'There are reserved function names.'#13#10+
      '2. If the function name is not a reserved word, the result depends '+
      'from the length of the name. Single-character function names are output '+
      'in italics, and their arguments are always enclosed in parentheses '+
      '(example 1). Longer function names are displayed in roman type and their '+
      'arguments are enclosed in brackets only when necessary (example 2). '+
      'You can force parentheses around an argument by using instead '+
      'opening bracket combination "!(" (example 3).'#13#10+
      '3. When raising a function to a power or adding an index to it, '+
      'the signs of the corresponding operations must appear after the '+
      'argument (example 4).'#13#10+
      '4. The digits at the end of the function name are interpreted as '+
      'a subscript if TExprBuilder.FuncAutoIndex:=True (default) (example 5).'#13#10+
      '5. Greek character tokens, Nabla and PLambda tokens can be used  '+
      'as function names. In this case the numbers '+
      'at the end are also interpreted as a subscript (example 6).'#13#10+
      '6. Reserved function names are case insensitive. ' +
      'The following conventions are used:'#13#10+
      '     E,E1,E2 etc. – arbitrary mandatory expressions (if you want '+
      'the mandatory expression was empty, you need to insert '+
      'the Nil token instead (example 7));'#13#10+
      '     [,E1] etc. – arbitrary optional expressions (may not be written '+
      'depending on the situation);'#13#10+
      '     m,n,n1,n2 etc. – integer constants;'#13#10+
      '     R - real constant.',
      ['f(x,y,z)','cos(x), sin(pi/2+x), tg(1/x)','cos!(x)','f(x)_n,cos(x)^2',
       'f0(x)=g1(x)','gamma0(x)', 'lim(x->0,f(x)), lim(Nil,f(x))']);
  AddArt('4. Examples', 'Examples of various formulas',
      ['Si!(x)=Int((sin(x)/x)*Diff(x),0,x)=pi/2-Int((sin(x)/x)*Diff(x),x,+Inf)=x-(1/3!)*(x^3/3)+(1/5!)*(x^5/5)-+...',
       'FuncSub("Res",z=a,f(z))=lim(z->a,[(1/(m-1)!)*.DiffR(z,m-1)*[(z-a)^m*.f(z)]])',
       'P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)=P(H_i)*P(E&Divide&H_i)/Sum(P(H_i)*P(E&Divide&H_i),i)',
       '{StandC(1,1&space(15)&2)}_S&Ident&{StandC(1,2&space(15)&1)}_S&Ident&(G*E_thetao-F*G_u)/(2*(E*G-F^2))',
       'SystemAnd(a11*x1+a12*x2+...+a_(1&n)*x_n=b1 & Comma,'+
       'a21*x1+a22*x2+...+a_(2&n)*x_n=b2 & Comma,DotsV,'+
       'a_(n&1)*x1+a_(n&2)*x2+...+a_nn*x_n=b_n & Semicolon)&'+
       'String(     or     ) & Sum(a_(i*k)*b_i,k=1,n),!(i=1,2,...,n)',
       'A*.X=B & String(,     где  )&'+
       'A=!(Matrix(4,4,a11,a12,DotsH,a_(1&n),a21,a22,DotsH,a_(2&n),DotsV,DotsV,DotsH,DotsV,a_(n&1),a_(n&2),DotsH,a_nn))&comma(20)&'+
       'X=!(StandC(x1,x2,DotsV,x_n))&comma(20)&B=!(StandC(b1,b2,DotsV,b_n)) & Dot',
       'Int(Int(x*y*Diff(x)*Diff(y),x^2,sqrt(x)),0,1)=Int(At([(1/2)*x*y^2],x^2,sqrt(x))**Diff(x),0,1)='+
       '(1/2)*Int(x*(x-x^4)*Diff(x),0,1)=(1/2)*At(x^3/3-x^6/6,0,1)=1/12']);
end;

procedure TForm1.SetInfo;
begin
  if EngRadioButton.Checked then
    SetInfoEng
  else
    SetInfoRus;
end;

procedure TForm1.SetSigns;
begin
  if EngRadioButton.Checked then
    SetSignsEng
  else
    SetSignsRus;
end;

procedure TForm1.SetSignsEng;
begin
  AddArt('  +', 'Addition',['a+b']);
  AddArt('  -', 'Subtraction',['a-b']);
  AddArt('  *',
      'Multiplication. Checks if the factors can be multiplied unsigned. ' +
      'If they can, then the multiplication sign is not used (example 1). ' +
      'The factors can be interchanged to provide multiplication without ' +
      'a character (example 2). If there are several numbers among the factors ' +
      'then the numbers are combined into one (example 3). If multiplication ' +
      'without a symbol is impossible under any order of factors the dot symbol ' +
      'is used (example 4).',
      ['5*x','y*2','2*a*3','sin(x)*cos(x)']);
  AddArt('  **',
      'Multiplication. The factors are unsigned multiplied regardless of whether ' +
      'such multiplication is permissible. ' +
      'The permutation of the factors is not performed.',
      ['5**x','y**2','2**a**3','sin(x)**cos(x)']);
  AddArt('  *.',
      'Multiplication. The factors are multiplied using the dot sign. ' +
      'The permutation of the factors is not performed.',
      ['5*.x','y*.2','2*.a*.3','sin(x)*.cos(x)']);
  AddArt('  *+',
      'Multiplication. The factors are multiplied using the cross sign. ' +
      'The permutation of the factors is not performed.',
      ['5*+x','y*+2','2*+a*+3','sin(x)*+cos(x)']);
  AddArt('  /',
      'Division. The division in the form of a simple fraction is always used. ' +
      'In complex expressions using various multiplication and division symbols ' +
      'in random order, distributes the factors between the numerator and ' +
      'denominator (example 2). To take the factor outside the fraction you ' +
      'need to enclose the fraction in brackets (examples 3, 4 and 5).',
      ['a/b','(x+1)/(x-1)*(x+2)/(x-2)/(x//y)*4','(1/2)*x','(3/4)*((x+1)/(x-1))','(3/4)*(x+1)/(x-1)']);
  AddArt('  //',
      'Division. A slash is used. In some cases, it removes the brackets ' +
      'incorrectly (example 2). In this case, it is recommended ' +
      'to use    “! ()” brackets (example 3).',
      ['a//b','x//(2*y)','x//!(2*y)']);
  AddArt('  /+', 'Division.', ['a/+b','x/+(2*y)','x/+!(2*y)']);
  AddArt('  :', 'Division.', ['(1/2):(1/3):(1/4)=12/2=6','x:(2*y)','x:!(2*y)']);
  AddArt('  \', 'Set difference.', ['A\B', 's&Belongs&Real\{0}', 'A\A=EmptySet']);
  AddArt('  +-', 'Plus-minus.', ['a+-b', 'sin(A+-B)=sin(A)*cos(B)+-sin(B)*cos(A)']);
  AddArt('  -+',  'Minus-plus.', ['a-+b', 'sin(A)+-sin(B)=2*sin((A+-B)/2)*cos((A-+B)/2)']);
  AddArt('  =',  'Equally.', ['a=b']);
  AddArt('  ==', 'Identically."', ['a==b']);
  AddArt('  =~', 'Equal or same order.', ['a=~b']);
  AddArt('  ~',  'Same order.', ['a~b']);
  AddArt('  ~~', 'Approximately equal.', ['a~~b']);
  AddArt('  <>', 'Not equal.', ['a<>b']);
  AddArt('  >',  'More.', ['a>b']);
  AddArt('  >>', 'Mach more.', ['a>>b']);
  AddArt('  >~', 'More or same order.', ['a>~b']);
  AddArt('  >=', 'More or equally.', ['a>=b']);
  AddArt('  <',  'Less.', ['a<b']);
  AddArt('  <<', 'Mach less.', ['a<<b']);
  AddArt('  <~', 'Less or same order.', ['a<~b']);
  AddArt('  <=', 'Less or equally.', ['a<=b']);
  AddArt('  ->', 'Tends to.', ['a->b']);
end;

procedure TForm1.SetSignsRus;
begin
  AddArt('  +', 'Символ "плюс", использующийя обычно для обозначения сложения',['a+b']);
  AddArt('  -', 'Символ "минус", использующийся обычно для обозначения вычитания',['a-b']);
  AddArt('  *',
      'Операция умножения. Проверяет, могут ли сомножители быть перемноженными без ' +
      'знака. Если могут, то знак умножения не используется (пример 1). Сомножители ' +
      'могут быть переставлены местами, чтобы обеспечить умножение без символа ' +
      '(пример 2). Если среди сомножителей несколько чисел, то числа объединяются ' +
      'в одно (пример 3). Если перемножение без символа невозможно ни при каком ' +
      'порядке множителей, используется символ точка (пример 4)',
      ['5*x','y*2','2*a*3','sin(x)*cos(x)']);
  AddArt('  **',
      'Операция умножения. Сомножители перемножаются без знака независимо от того, ' +
      'допустимо ли такое перемножение. Перестановка множителей не производится.',
      ['5**x','y**2','2**a**3','sin(x)**cos(x)']);
  AddArt('  *.',
      'Операция умножения. Сомножители перемножаются с помощью знака точки. ' +
      'Перестановка множителей не производится.',
      ['5*.x','y*.2','2*.a*.3','sin(x)*.cos(x)']);
  AddArt('  *+',
      'Операция умножения. Сомножители перемножаются с помощью знака косого креста. ' +
      'Перестановка множителей не производится.',
      ['5*+x','y*+2','2*+a*+3','sin(x)*+cos(x)']);
  AddArt('  /',
      'Знак операции деления. Всегда используется деление в виде простой дроби. ' +
      'В сложных выражениях, использующих различные символы умножения и деления ' +
      'в произвольном порядке, распределяет множители между ' +
      'числителем и знаменателем (пример 2). Чтобы вынести множитель за пределы ' +
      'дроби, нужно дробь заключить в скобки (примеры 3, 4 и 5)',
      ['a/b','(x+1)/(x-1)*(x+2)/(x-2)/(x//y)*4','(1/2)*x','(3/4)*((x+1)/(x-1))','(3/4)*(x+1)/(x-1)']);
  AddArt('  //',
      'Знак операции деления. Используется косая черта. В некотогрых случаях ' +
      'неправильно убирает скобки (пример 2). В этом случае рекомендуется ' +
      'пользоваться скобками !() (пример 3)',
      ['a//b','x//(2*y)','x//!(2*y)']);
  AddArt('  /+', 'Знак операции деления.', ['a/+b','x/+(2*y)','x/+!(2*y)']);
  AddArt('  :', 'Знак операции деления.', ['(1/2):(1/3):(1/4)=12/2=6','x:(2*y)','x:!(2*y)']);
  AddArt('  \', 'Обратная косая черта. Может обозначать, например, разность множеств.',
      ['A\B', 's&Belongs&Real\{0}', 'A\A=EmptySet']);
  AddArt('  +-', 'Знак операции "плюс-минус"', ['a+-b', 'sin(A+-B)=sin(A)*cos(B)+-sin(B)*cos(A)']);
  AddArt('  -+',  'Знак операции "минус-плюс"', ['a-+b', 'sin(A)+-sin(B)=2*sin((A+-B)/2)*cos((A-+B)/2)']);
  AddArt('  =',  'Знак операции "равно"', ['a=b']);
  AddArt('  ==', 'Знак операции "тождественно"', ['a==b']);
  AddArt('  =~', 'Знак операции "равно или порядка"', ['a=~b']);
  AddArt('  ~',  'Знак операции "порядка"', ['a~b']);
  AddArt('  ~~', 'Знак операции "примерно равно"', ['a~~b']);
  AddArt('  <>', 'Знак операции "не равно"', ['a<>b']);
  AddArt('  >',  'Знак операции "больше"', ['a>b']);
  AddArt('  >>', 'Знак операции "много больше"', ['a>>b']);
  AddArt('  >~', 'Знак операции "больше или порядка"', ['a>~b']);
  AddArt('  >=', 'Знак операции "больше или равно"', ['a>=b']);
  AddArt('  <',  'Знак операции "меньше"', ['a<b']);
  AddArt('  <<', 'Знак операции "много меньше"', ['a<<b']);
  AddArt('  <~', 'Знак операции "меньше или порядка"', ['a<~b']);
  AddArt('  <=', 'Знак операции "меньше или равно"', ['a<=b']);
  AddArt('  ->', 'Знак операции "стремится к"', ['a->b']);
end;

procedure TForm1.SetBeforeSymbols;
begin
  if EngRadioButton.Checked then
    SetBeforeSymbolsEng
  else
    SetBeforeSymbolsRus;
end;

procedure TForm1.SetBeforeSymbolsEng;
begin
  AddArt('  "_"',
      'Vector. In some cases, incorrect placement of brackets is possible then ' +
      'it is recommended to use the function Vect.',
      ['_a',
       'DiffRF(_a,t)=_i*.DiffRF(a_x,t)+_j*.DiffRF(a_y,t)+_k*.DiffRF(a_z,t)']);
  AddArt('  "+"',  'Unary plus.',['+a', 'lim(x->+0,f(x)=1)']);
  AddArt('  "-"',  'Unary minus.',['-a', 'lim(x->-0,f(x)=0)']);
  AddArt('  "+-"', 'Unary plus-minus.', ['+-a']);
  AddArt('  "-+"', 'Unary minus-plus.', ['-+a']);
  AddArt('  "#"','Preceded by numeric constants to indicate that they should be ' +
      'output in scientific format. Compare example 1 and example 2',
      ['0.03*x','#0.03*x']);
end;

procedure TForm1.SetBeforeSymbolsRus;
begin
  AddArt('  "_"',
      'Символ используется для обозначения вектора, ставится перед выражением. ' +
      'В некоторых случаях возможна неправильная расстановка скобок. В таких ' +
      'случаях вместо символа рекомендуется использовать функцию Vect.',
      ['_a',
       'DiffRF(_a,t)=_i*.DiffRF(a_x,t)+_j*.DiffRF(a_y,t)+_k*.DiffRF(a_z,t)']);
  AddArt('  "+"', 'Унарный "плюс"',['+a', 'lim(x->+0,f(x)=1)']);
  AddArt('  "-"', 'Унарный "минус"',['-a', 'lim(x->-0,f(x)=0)']);
  AddArt('  "+-"', 'Унарный "плюс-минус"', ['+-a']);
  AddArt('  "-+"',  'Унарный "минус-плюс"', ['-+a']);
  AddArt('  "#"','Ставится перед цифровыми константами для указания, что они ' +
      'должны быть выведены в научном формате. Сравните пример 1 и пример 2',
      ['0.03*x','#0.03*x']);
end;

procedure TForm1.SetAfterSymbols;
begin
  if EngRadioButton.Checked then
    SetAfterSymbolsEng
  else
    SetAfterSymbolsRus;
end;

procedure TForm1.SetAfterSymbolsEng;
begin
  AddArt('  "_"',
      'Subscript. Symbol can be used if TExprBuilder.PostSymbols=True (default).'#13#10+
      'In some cases incorrect placement of brackets or ' +
      'multi-stage indexes is possible (example 2). In such cases it is ' +
      'recommended to use the Ind function (example 3).',
      ['a_b','a_x_0','Ind(a,Ind(x,0))']);
  AddArt('  "^"',
      'Superscript or exponentiation. Symbol can be used if TExprBuilder.PostSymbols=True (default).'#13#10+
      'In some cases, incorrect placement of brackets or multi-stage indexes ' +
      'is possible. In such cases, it is recommended to use the Pow function. ' +
      'If used together with a subscript (character "_" or the Ind function) ' +
      'then the subscript is indicated first, and then the superscript  ' +
      '(example 2), otherwise the indices will not be displayed ' +
      'correctly (example 3).',
      ['a^b','Ind(x,a)^2, x_a^2','Ind(x^2,a), x^2_a']);
   AddArt(' "!"',
      'Factorial. Symbol can be used if TExprBuilder.PostSymbols=True (default).',
      ['C_n^k=CaseAnd(n!/k!/(n-k)!,String(для )&0<=k<=n&Comma,0,String(для )&0<=n<k&Dot)']);
   AddArt('  "`"',
      'Derivative. Symbol can be used if TExprBuilder.PostSymbols=True (default).'#13#10+
      'It is permissible to use several characters in a row to denote derivatives ' +
      'of higher degrees. In some cases, incorrect placement of brackets is ' +
      'possible. In such cases, it is recommended to use the Strokes function.',
      ['f(x)`','f(x)```','X=x`*cos(Angle**!(x`,x))+y`*cos(Angle**!(y`,x))+z`*cos(Angle**!(z`,x))']);
end;

procedure TForm1.SetAfterSymbolsRus;
begin
  AddArt('  "_"',
      'Символ используется для обозначения нижнего индекса. В некоторых случаях ' +
      'возможна неправильная расстановка скобок или многоступенчатых индексов ' +
      '(пример 2). В таких случаях вместо символа рекомендуется использовать ' +
      'функцию Ind (пример 3). Использование символа "_" для обозначения нижнего ' +
      'индекса возможно только если TExprBuilder.PostSymbols=True (по умолчанию)',
      ['a_b','a_x_0','Ind(a,Ind(x,0))']);
  AddArt('  "^"',
      'Символ используется для обозначения верхнего индекса или возведения в степень. ' +
      'В некоторых случаях возможна неправильная расстановка скобок или ' +
      'многоступенчатых индексов. В таких случаях вместо символа рекомендуется ' +
      'использовать функцию Pow. Если используется вместе с нижним индексом ' +
      '(символ "_" или функция Ind), то сначала указывается нижний индекс, а ' +
      'затем - верхний (пример 2), в противном случае индексы будут отображаться ' +
      'некорректно (пример 3). Использование символа "^" возможно только если ' +
      'TExprBuilder.PostSymbols=True (по умолчанию)',
      ['a^b','Ind(x,a)^2, x_a^2','Ind(x^2,a), x^2_a']);
   AddArt(' "!"',
      'Символ факториала. Использование символа "!" для обозначения факториала ' +
      'возможно только если TExprBuilder.PostSymbols=True (по умолчанию)',
      ['C_n^k=CaseAnd(n!/k!/(n-k)!,String(для )&0<=k<=n&Comma,0,String(для )&0<=n<k&Dot)']);
   AddArt('  "`"',
      'Используется для обозначения производной. Допустимо использование ' +
      'нескольких знаков подряд для обозначения производных высших степеней. ' +
      'В некоторых случаях возможна неправильная расстановка скобок. '+
      'В таких случаях вместо символа рекомендуется использовать функцию Strokes. ' +
      'Использование символа "`" возможно только если TExprBuilder.PostSymbols=True ' +
      '(по умолчанию)',
      ['f(x)`','f(x)```','X=x`*cos(Angle**!(x`,x))+y`*cos(Angle**!(y`,x))+z`*cos(Angle**!(z`,x))']);
end;

procedure TForm1.SetConcatenates;
begin
  if EngRadioButton.Checked then
    SetConcatenatesEng
  else
    SetConcatenatesRus;
end;

procedure TForm1.SetConcatenatesEng;
begin
  AddArt('  "&"',
      'Concatenation of two expressions. The character must either not be '+
      'separated from both expressions by spaces (example 1) or separated ' +
      'from each of them by one space (example 2).', ['x&y','x & y']);
  AddArt('  ","',
      'A character that separates multiple consecutive expressions. There can ' +
      'be an arbitrary number of spaces after the character, but this does not ' +
      'affect the spacing between expressions, which is seven units of width. ' +
      'There must be no spaces before the character.',
      ['a0,a1, a2,    a3']);
  AddArt('  ";"',
      'A character that separates multiple consecutive expressions. There can ' +
      'be an arbitrary number of spaces after the character, but this does not ' +
      'affect the spacing between expressions, which is seven units of width. ' +
      'There must be no spaces before the character.',
      ['a0;a1; a2;    a3']);
end;

procedure TForm1.SetConcatenatesRus;
begin
  AddArt('  "&"',
      'Конкатенация двух выражений (не рисуется в изображении формулы). '+
      'Символ должен либо не отделяться от обоих ' +
      'выражений пробелами (пример 1), либо отделяться от каждого из них одним ' +
      'пробелом (пример 2)', ['x&y','x & y']);
  AddArt('  ","',
      'Запятая, отделяющая несколько подряд идущих выражений. После запятой ' +
      'может быть произвольное число пробелов, однако это не влияет на расстояние ' +
      'между выражениями, которое составляет семь единиц ширины. Перед запятой ' +
      'пробелов быть не должно.',
      ['a0,a1, a2,    a3']);
  AddArt('  ";"',
      'Точка с запятой, отделяющая несколько подряд идущих выражений. После точки с запятой ' +
      'может быть произвольное число пробелов, однако это не влияет на расстояние ' +
      'между выражениями, которое составляет семь единиц ширины. Перед точкой с запятой ' +
      'пробелов быть не должно.',
      ['a0;a1; a2;    a3']);
end;

procedure TForm1.SetBrackets;
begin
  if EngRadioButton.Checked then
    SetBracketsEng
  else
    SetBracketsRus;
end;

procedure TForm1.SetBracketsEng;
begin
   AddArt('  ()',
      'Parentheses are used to change the order in which actions are performed. ' +
      'They can be removed by the expression builder if this does not lead to a ' +
      'distortion of the meaning of the expression. Use the "!()" brackets to ' +
      'force parentheses.'+
      #13#10'More options for displaying brackets are provided by the Brackets function.',
      ['(x+1)*(y-2)','(x+1)/(x-1)','a+(b+c)=d*.(e*.f)','a*(b+c)<>d+(e*f)','y=(1+1/(1+1/x))']);
  AddArt('  !()',
      'They are used in the same place as regular parentheses, but are never ' +
      'removed by the expression builder.'+
      #13#10'More options for displaying brackets are provided by the Brackets function.',
      ['!(x+1)*!(y-2)','!(x+1)/!(x-1)','a+!(b+c)=d*.!(e*.f)','a*!(b+c)<>d+!(e*f)','y=!(1+1/!(1+1/x))']);
  AddArt('  []', 'Square brackets. Never removed by the expression builder.'+
      #13#10'More options for displaying brackets are provided by the Brackets function.',
      ['[x+1]*[y-2]','[x+1]/[x-1]','a+[b+c]=d*.[e*.f]','a*[b+c]<>d+[e*f]','y=[1+1/[1+1/x]]']);
  AddArt('  {}', 'Braces. Never removed by the expression builder.'+
      #13#10'More options for displaying brackets are provided by the Brackets function.',
      ['{x+1}*{y-2}','{x+1}/{x-1}','a+{b+c}=d*.{e*.f}','a*{b+c}<>d+{e*f}','y={1+1/{1+1/x}}']);
  AddArt('  ||', 'Straight brackets. Never removed by the expression builder.'+
      #13#10'More options for displaying brackets are provided by the Brackets function.',
      ['|x+1|*|y-2|','|x+1|/|x-1|','a+|b+c|=d*.|e*.f|','a*|b+c|<>d+|e*f|','y=|1+1/|1+1/x||']);
end;

procedure TForm1.SetBracketsRus;
begin
   AddArt('  ()',
      'Круглые скобки, служат для изменения порядка выполнения действий. Могут ' +
      'быть убраны, если в это не приведёт к искажению смысла выражения. Для ' +
      'принудительной установки скобок используйте скобки !().'+
      #13#10'Больше возможностей вывода скобок предоставляет функция Brackets',
      ['(x+1)*(y-2)','(x+1)/(x-1)','a+(b+c)=d*.(e*.f)','a*(b+c)<>d+(e*f)','y=(1+1/(1+1/x))']);
  AddArt('  !()',
      'Используются там же, где и обычные круглые скобки, но никогда не ' +
      'убираются построителем формулы.'+
      #13#10'Больше возможностей вывода скобок предоставляет функция Brackets',
      ['!(x+1)*!(y-2)','!(x+1)/!(x-1)','a+!(b+c)=d*.!(e*.f)','a*!(b+c)<>d+!(e*f)','y=!(1+1/!(1+1/x))']);
  AddArt('  []', 'Квадратные скобки. Никогда не убираются построителем.'+
      #13#10'Больше возможностей вывода скобок предоставляет функция Brackets',
      ['[x+1]*[y-2]','[x+1]/[x-1]','a+[b+c]=d*.[e*.f]','a*[b+c]<>d+[e*f]','y=[1+1/[1+1/x]]']);
  AddArt('  {}', 'Фигурные скобки. Никогда не убираются построителем.'+
      #13#10'Больше возможностей вывода скобок предоставляет функция Brackets',
      ['{x+1}*{y-2}','{x+1}/{x-1}','a+{b+c}=d*.{e*.f}','a*{b+c}<>d+{e*f}','y={1+1/{1+1/x}}']);
  AddArt('  ||', 'Прямые скобки. Никогда не убираются построителем.'+
      #13#10'Больше возможностей вывода скобок предоставляет функция Brackets',
      ['|x+1|*|y-2|','|x+1|/|x-1|','a+|b+c|=d*.|e*.f|','a*|b+c|<>d+|e*f|','y=|1+1/|1+1/x||']);
end;

procedure TForm1.SetGreekLetters;
begin
  if EngRadioButton.Checked then
    SetGreekLettersEng
  else
    SetGreekLettersRus;
end;

procedure TForm1.SetGreekLettersEng;
begin
  AddArt(' Alpha', 'Greek letter alpha', ['Alpha','alpha']);
  AddArt(' Beta', 'Greek letter beta', ['Beta','beta']);
  AddArt(' Gamma', 'Greek letter gamma', ['Gamma','gamma']);
  AddArt(' Delta', 'Greek letter delta', ['Delta','delta', 'Delta=PDiffR(x,2)+PDiffR(y,2)+PDiffR(z,2)']);
  AddArt(' Epsilon', 'Greek letter epsilon', ['Epsilon','epsilon']);
  AddArt(' Zeta', 'Greek letter zeta', ['Zeta','zeta']);
  AddArt(' Eta', 'Greek letter eta', ['Eta','eta']);
  AddArt(' Theta', 'Greek letter theta', ['Theta','theta']);
  AddArt(' thetao', 'Another spelling of Greek letter theta', ['thetao']);
  AddArt(' Iota', 'Greek letter iota', ['Iota','iota']);
  AddArt(' Kappa', 'Greek letter kappa', ['Kappa','kappa']);
  AddArt(' Lambda', 'Greek letter lambda', ['Lambda','lambda']);
  AddArt(' Mu', 'Greek letter mu', ['Mu','mu']);
  AddArt(' Nu', 'Greek letter nu', ['Nu','nu']);
  AddArt(' Xi', 'Greek letter xi', ['Xi','xi']);
  AddArt(' Omicron', 'Greek letter omicron', ['Omicron','omicron']);
  AddArt(' Pi', 'Greek letter pi', ['Pi','pi']);
  AddArt(' Rho', 'Greek letter rho', ['Rho','rho']);
  AddArt(' Sigma', 'Greek letter sigma', ['Sigma','sigma']);
  AddArt(' sigmao', 'Another spelling of Greek letter sigma', ['sigmao']);
  AddArt(' Tau', 'Greek letter tau', ['Tau','tau']);
  AddArt(' Upsilon', 'Greek letter upsilon', ['Upsilon','upsilon']);
  AddArt(' Phi', 'Greek letter phi', ['Phi','phi']);
  AddArt(' Chi', 'Greek letter chi', ['Chi','chi']);
  AddArt(' Psi', 'Greek letter psi', ['Psi','psi']);
  AddArt(' Omega', 'Greek letter omega', ['Omega','omega']);
end;

procedure TForm1.SetGreekLettersRus;
begin
  AddArt(' Alpha', 'Греческая буква альфа', ['Alpha','alpha']);
  AddArt(' Beta', 'Греческая буква бета', ['Beta','beta']);
  AddArt(' Gamma', 'Греческая буква гамма', ['Gamma','gamma']);
  AddArt(' Delta', 'Греческая буква дельта', ['Delta','delta', 'Delta=PDiffR(x,2)+PDiffR(y,2)+PDiffR(z,2)']);
  AddArt(' Epsilon', 'Греческая буква эпсилон', ['Epsilon','epsilon']);
  AddArt(' Zeta', 'Греческая буква зета', ['Zeta','zeta']);
  AddArt(' Eta', 'Греческая буква ета', ['Eta','eta']);
  AddArt(' Theta', 'Греческая буква тета', ['Theta','theta']);
  AddArt(' thetao', 'Второй вариант греческой строчной буквы тета', ['thetao']);
  AddArt(' Iota', 'Греческая буква йота', ['Iota','iota']);
  AddArt(' Kappa', 'Греческая буква каппа', ['Kappa','kappa']);
  AddArt(' Lambda', 'Греческая буква лямбда', ['Lambda','lambda']);
  AddArt(' Mu', 'Греческая буква мю', ['Mu','mu']);
  AddArt(' Nu', 'Греческая буква ню', ['Nu','nu']);
  AddArt(' Xi', 'Греческая буква кси', ['Xi','xi']);
  AddArt(' Omicron', 'Греческая буква омикрон', ['Omicron','omicron']);
  AddArt(' Pi', 'Греческая буква пи', ['Pi','pi']);
  AddArt(' Rho', 'Греческая буква ро', ['Rho','rho']);
  AddArt(' Sigma', 'Греческая буква сигма', ['Sigma','sigma']);
  AddArt(' sigmao', 'Второй вариант греческой строчной буквы сигма', ['sigmao']);
  AddArt(' Tau', 'Греческая буква тау', ['Tau','tau']);
  AddArt(' Upsilon', 'Греческая буква упсилон', ['Upsilon','upsilon']);
  AddArt(' Phi', 'Греческая буква фи', ['Phi','phi']);
  AddArt(' Chi', 'Греческая буква хи', ['Chi','chi']);
  AddArt(' Psi', 'Греческая буква пси', ['Psi','psi']);
  AddArt(' Omega', 'Греческая буква омега', ['Omega','omega']);
end;

procedure TForm1.SetArrows;
begin
  if EngRadioButton.Checked then
    SetArrowsEng
  else
    SetArrowsRus;
end;

procedure TForm1.SetArrowsEng;
begin
  AddArt(' ArrowL', '', ['ArrowL']);
  AddArt(' ArrowR', '', ['ArrowR']);
  AddArt(' ArrowU', '', ['ArrowU']);
  AddArt(' ArrowD', '', ['ArrowD']);
  AddArt(' ArrowLR', '', ['ArrowLR']);
  AddArt(' ArrowUD', '', ['ArrowUD']);
  AddArt(' ArrowLU', '', ['ArrowLU']);
  AddArt(' ArrowLD', '', ['ArrowLD']);
  AddArt(' ArrowRU', '', ['ArrowRU']);
  AddArt(' ArrowRD', '', ['ArrowRD']);
  AddArt(' ArrowLN', '', ['ArrowLN']);
  AddArt(' ArrowRN', '', ['ArrowRN']);
  AddArt(' ArrowLRN', '', ['ArrowLRN']);
  AddArt(' BArrowL', '', ['BArrowL']);
  AddArt(' BArrowR', '', ['BArrowR']);
  AddArt(' BArrowU', '', ['BArrowU']);
  AddArt(' BArrowD', '', ['BArrowD']);
  AddArt(' DArrowL', '', ['DArrowL']);
  AddArt(' DArrowR', '', ['DArrowR']);
  AddArt(' DArrowU', '', ['DArrowU']);
  AddArt(' DArrowD', '', ['DArrowD']);
  AddArt(' DArrowLR', '', ['DArrowLR']);
  AddArt(' DArrowUD', '', ['DArrowUD']);
  AddArt(' DArrowLU', '', ['DArrowLU']);
  AddArt(' DArrowLD', '', ['DArrowLD']);
  AddArt(' DArrowRU', '', ['DArrowRU']);
  AddArt(' DArrowRD', '', ['DArrowRD']);
  AddArt(' DArrowLN', '', ['DArrowLN']);
  AddArt(' DArrowRN', '', ['DArrowRN']);
  AddArt(' DArrowLRN', '', ['DArrowLRN']);
  AddArt(' HArrowLR', '', ['HArrowLR']);
  AddArt(' HArrowRL', '', ['HArrowRL']);
  AddArt(' TArrowLR', '', ['TArrowLR']);
  AddArt(' TArrowRL', '', ['TArrowRL']);
  AddArt(' TArrowUD', '', ['TArrowUD']);
  AddArt(' TArrowDU', '', ['TArrowDU']);
  AddArt(' TArrowL', '', ['TArrowL']);
  AddArt(' TArrowR', '', ['TArrowR']);
  AddArt(' TArrowU', '', ['TArrowU']);
  AddArt(' TArrowD', '', ['TArrowD']);
  AddArt(' TRArrow', '', ['TRArrow']);
end;

procedure TForm1.SetArrowsRus;
begin
  AddArt(' ArrowL', 'Одиночная стрелка влево', ['ArrowL']);
  AddArt(' ArrowR', 'Одиночная стрелка вправо', ['ArrowR']);
  AddArt(' ArrowU', 'Одиночная стрелка вверх', ['ArrowU']);
  AddArt(' ArrowD', 'Одиночная стрелка вниз', ['ArrowD']);
  AddArt(' ArrowLR', 'Одиночная стрелка влево-вправо', ['ArrowLR']);
  AddArt(' ArrowUD', 'Одиночная стрелка вверх-вниз', ['ArrowUD']);
  AddArt(' ArrowLU', 'Одиночная стрелка влево-вверх', ['ArrowLU']);
  AddArt(' ArrowLD', 'Одиночная стрелка влево-вниз', ['ArrowLD']);
  AddArt(' ArrowRU', 'Одиночная стрелка вправо-вверх', ['ArrowRU']);
  AddArt(' ArrowRD', 'Одиночная стрелка вправо-вниз', ['ArrowRD']);
  AddArt(' ArrowLN', 'Одиночная стрелка влево (зачеркнутая)', ['ArrowLN']);
  AddArt(' ArrowRN', 'Одиночная стрелка вправо (зачеркнутая)', ['ArrowRN']);
  AddArt(' ArrowLRN', 'Одиночная стрелка влево-вправо (зачеркнутая)', ['ArrowLRN']);
  AddArt(' BArrowL', 'Стрелка от планки влево', ['BArrowL']);
  AddArt(' BArrowR', 'Стрелка от планки вправо', ['BArrowR']);
  AddArt(' BArrowU', 'Стрелка от планки вверх', ['BArrowU']);
  AddArt(' BArrowD', 'Стрелка от планки вниз', ['BArrowD']);
  AddArt(' DArrowL', 'Двойная стрелка влево', ['DArrowL']);
  AddArt(' DArrowR', 'Двойная стрелка вправо', ['DArrowR']);
  AddArt(' DArrowU', 'Двойная стрелка вверх', ['DArrowU']);
  AddArt(' DArrowD', 'Двойная стрелка вниз', ['DArrowD']);
  AddArt(' DArrowLR', 'Двойная стрелка влево-вправо', ['DArrowLR']);
  AddArt(' DArrowUD', 'Двойная стрелка вверх-вниз', ['DArrowUD']);
  AddArt(' DArrowLU', 'Двойная стрелка влево-вверх', ['DArrowLU']);
  AddArt(' DArrowLD', 'Двойная стрелка влево-вниз', ['DArrowLD']);
  AddArt(' DArrowRU', 'Двойная стрелка вправо-вверх', ['DArrowRU']);
  AddArt(' DArrowRD', 'Двойная стрелка вправо-вниз', ['DArrowRD']);
  AddArt(' DArrowLN', 'Двойная стрелка влево (зачеркнутая)', ['DArrowLN']);
  AddArt(' DArrowRN', 'Двойная стрелка вправо (зачеркнутая)', ['DArrowRN']);
  AddArt(' DArrowLRN', 'Двойная стрелка влево-вправо (зачеркнутая)', ['DArrowLRN']);
  AddArt(' HArrowLR', '', ['HArrowLR']);
  AddArt(' HArrowRL', '', ['HArrowRL']);
  AddArt(' TArrowLR', 'Две стрелки влево-вправо', ['TArrowLR']);
  AddArt(' TArrowRL', 'Две стрелки вправо-влево', ['TArrowRL']);
  AddArt(' TArrowUD', 'Две стрелки вверх-вниз', ['TArrowUD']);
  AddArt(' TArrowDU', 'Две стрелки вниз-вверх', ['TArrowDU']);
  AddArt(' TArrowL', 'Две стрелки влево', ['TArrowL']);
  AddArt(' TArrowR', 'Две стрелки вправо', ['TArrowR']);
  AddArt(' TArrowU', 'Две стрелки вверх', ['TArrowU']);
  AddArt(' TArrowD', 'Две стрелки вниз', ['TArrowD']);
  AddArt(' TRArrow', 'Три стрелки', ['TRArrow']);
end;

procedure TForm1.SetTokens;
begin
  if EngRadioButton.Checked then
    SetTokensEng
  else
    SetTokensRus;
end;

procedure TForm1.SetTokensEng;
begin
  AddArt(' ...', 'Ellipsis located at the level of punctuation marks.', ['f(x1,x2,...,x_n)=0']);
  AddArt(' And', 'Logic "AND".', ['And']);
  AddArt(' Angle', 'Angle sign.',
        ['Angle**A',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)',
         'X=x`*cos(Angle**!(x`,x))+y`*cos(Angle**!(y`,x))+z`*cos(Angle**!(z`,x))']);
  AddArt(' Arc', 'Arc sign', ['Arc**AB']);
  AddArt(' Asterisk',
         'Asterisk - the symbol "*", somewhat lowered down compared to the usual ' +
         'position. Intended for use in superscripts.',
         ['Sum(a_n*a_n^Asterisk,n)=Int(Psi*Psi^Asterisk*Diff(q))']);
  AddArt(' Begin', 'Beginning of proof (calculations).', ['Begin']);
  AddArt(' Belongs', 'Belongs.',
         ['Belongs', '[a,b]={x&Divide&x&Belongs&Real,a<=x<=b}']);
  AddArt(' BelongsN', 'Not belongs.', ['BelongsN']);
  AddArt(' Circle', 'Circle.', ['Circle']);
  AddArt(' CircleC', 'Circle in circle.', ['CircleC']);
  AddArt(' Colon',
      'Colon. In most cases, it can be replaced with the Colon function.', ['x&colon&y']);
  AddArt(' Comma',
      'Comma. Unlike the "," character, it does not insert spaces after the comma. '+
      'In most cases, it can be replaced by the "," character or by the Comma ' +
      'function.', ['x&comma&y']);
  AddArt(' Complex', 'Set of complex numbers.',
      ['Complex', 'z&BArrowR&z^n, z&Belongs&Complex']);
  AddArt(' Const',
      'The word "const", denoting an arbitrary constant. Unlike regular ' +
      'identifiers, it is written in roman type, not in italics.',
      ['Const','Int(x*Diff(x))=x^2/2+const']);
  AddArt(' CrossC', 'Cross in circle.', ['CrossC']);
  AddArt(' Degree', 'Degree .', ['sin(30&Degree)=1/2']);
  AddArt(' Divide', 'Devides.',
        ['A&Divide&B', 'P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)']);
  AddArt(' DivideN', 'Not Devides', ['A&DivideN&B']);
  AddArt(' Dot', 'Dot', ['Dot']);
  AddArt(' DotC', 'Dot in circle', ['DotC']);
  AddArt(' DotEqual', 'Approaching the limit', ['DotEqual']);
  AddArt(' DotEqualC', 'Geometrically equal', ['DotEqualC']);
  AddArt(' DotEqualLR', 'Represent',
         ['DotEqualLR', 'f(t)&DotEqualLR&F(omega)']);
  AddArt(' DotEqualRL', 'Represent',
         ['DotEqualRL', 'F(omega)&DotEqualRL&f(t)']);
  AddArt(' DotsD', 'Diagonal dots. Can be used, for example, to indicate missing elements of a matrix',
         ['DotsD', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' DotsH', 'Horizontal dots. Can be used, for example, to indicate missing elements of a matrix',
        ['DotsH', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' DotsU', 'Diagonal dots. Can be used, for example, to indicate missing elements of a matrix',
        ['DotsU']);
  AddArt(' DotsV', 'Vertical dots. Can be used, for example, to indicate missing elements of a matrix',
        ['DotsV', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' Empty',
        'Empty expression. Unlike Nil, it only has a width of zero, and the height ' +
        'is equal to the height of the characters. ',
        ['Empty^2']);
  AddArt(' EmptySet', 'Empty set.',
      ['EmptySet', 'G1&Intersection&G2=EmptySet']);
  AddArt(' End', 'Ending of proof (calculations). Q.E.D', ['End']);
  AddArt(' Entire', 'Set of integers.', ['Entire']);
  AddArt(' EqualC', 'Equal in circle.', ['EqualC']);
  AddArt(' Exists', 'Exists.',
      ['Exists', 'Exists&x(x>1->ForAll&x(x>y&And&Exists&x(x&Divide&10)))']);
  AddArt(' ExistsN', 'Not exists.', ['ExistsN']);
  AddArt(' ForAll', 'For all.',
      ['ForAll', 'a_(n+1)<=a_n&ForAll&n&Belongs&Natural']);
  AddArt(' Ident', 'Identical. Sign analogue "==".', ['Ident']);
  AddArt(' IdentN', 'Not identical.', ['IdentN']);
  AddArt(' Inf', 'Infinity.', ['Inf', 'lim(x->0,1/x)=Inf']);
  AddArt(' Intersection', 'Intersection.',
        ['Intersection','P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)']);
  AddArt(' Minus',
      'Minus. Designed for use primarily in indexes. Other characters ' +
      '("+", "=", etc.) can be displayed using the String function. But the '+
      'expression "String(-)" will not give a minus, but a hyphen, which is ' +
      'significantly shorter.',
      ['a_Minus<>a_String(-)']);
  AddArt(' MinusC', 'Minus in circle.', ['MinusC']);
  AddArt(' Nabla', 'Hamilton operator.',
      ['Nabla*f=PDiffRF(f,x)*_e_x+PDiffRF(f,y)*_e_y+PDiffRF(f,z)*_e_z',
       'Nabla*_a=PDiffRF(a_x,x)+PDiffRF(a_y,y)+PDiffRF(a_z,z)']);
  AddArt(' Natural', 'Set of natural numbers.',
         ['Natural', 'M={x&Divide&x=1+(n+1)/n, n&Belongs&Natural}']);
  AddArt(' Nil',
      'An empty expression with zero dimensions. Intended for use in functions ' +
      'where, according to the syntax, there should be an expression, but in a ' +
      'specific case it is required that it should not be.', ['lim(nil,f(x))=0']);
  AddArt(' Not', 'Logical negation.', ['Not']);
  AddArt(' Or', 'Logical "OR"', ['Or']);
  AddArt(' Parallel.',
      'Parallel.', ['_a & Parallel & _b','H_Parallel']);
  AddArt(' ParallelN',
      'Not parallel.', ['_a & ParallelN & _b','H_ParallelN']);
  AddArt(' Parallelogram', 'Parallelogram.',
      ['Parallelogram', 'Parallelogram**ABCD', 'Parallelogram**A1*B1*C1*D1']);
  AddArt(' Perpendicular',
      'Perpendicular.', ['_a & Perpendicular & _b','v_Perpendicular']);
  AddArt(' PLambda', 'Lambda with dash. Used, for example, in quantum mechanics.',['PLambda=lambda/2/pi']);
  AddArt(' Planck', 'Planck''s constant.', ['Planck=h/2/pi']);
  AddArt(' PlusC', 'Plus in circle.', ['PlusC']);
  AddArt(' Projective', 'Projective space.', ['Projective']);
  AddArt(' Prop', 'Proportionally.', ['Prop']);
  AddArt(' Quadrate', 'Quadrate.',
      ['Quadrate', 'Quadrate**ABCD', 'Quadrate**A1*B1*C1*D1']);
  AddArt(' Quaternion', 'Hamilton''s set of quaternions.', ['Quaternion']);
  AddArt(' Rational', 'Set of rational numbers.',
         ['Rational', 'M={x&Divide&x&Belongs&Rational,0<=x<1}']);
  AddArt(' Real', 'Set of real numbers.',
         ['Real', '[a,b]={x&Divide&x&Belongs&Real,a<=x<=b}']);
  AddArt(' Rectangle', 'Rectangle.',
      ['Rectangle', 'Rectangle**ABCD', 'Rectangle**A1*B1*C1*D1']);
  AddArt(' Rhomb', 'Rhomb.', ['Rhomb', 'Rhomb**ABCD', 'Rhomb**A1*B1*C1*D1']);
  AddArt(' Semicolon',
      'Semicolon. Unlike the character ";" does not insert spaces after the '+
      'semicolon. In most cases, it can be replaced by the character ";" or the ' +
      'Semicolon function.', ['x&semicolon&y']);
  AddArt(' Similar', 'Similar.',
        ['Similar',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)']);
  AddArt(' SlashC', 'Slash in circle.', ['SlashC']);
  AddArt(' SubSet', 'Subset.',
        ['SubSet', 'M={!(x1,x2)&Divide&0<a<Sqr(x1)+Sqr(x2)<b}&SubSet&Real^2']);
  AddArt(' SubSetN', 'Not subset.', ['SubSetN']);
  AddArt(' SuperSet', 'Superset.', ['SuperSet']);
  AddArt(' SuperSet', 'Not superset.', ['SuperSetN']);
  AddArt(' Triangle', 'Triangle.',
        ['Triangle',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)']);
  AddArt(' Union', 'Union.', ['Union', 'Q&Union&[0,1]']);
  AddArt(' Xor', 'Logical "XOR"', ['Xor']);
end;

procedure TForm1.SetTokensRus;
begin
  AddArt(' ...', 'Многоточие, располагающееся на уровне знаков пунктуации.', ['f(x1,x2,...,x_n)=0']);
  AddArt(' And', 'Логическое "И"', ['And']);
  AddArt(' Angle', 'Знак угла',
        ['Angle**A',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)',
         'X=x`*cos(Angle**!(x`,x))+y`*cos(Angle**!(y`,x))+z`*cos(Angle**!(z`,x))']);
  AddArt(' Arc', 'Знак дуги', ['Arc**AB']);
  AddArt(' Asterisk',
         'Астериск - символ "*", несколько опущенный вниз по сравнению с обычным положением. ' +
         'Предназначен для использования в верхних индексах. ' +
         'Может обозначать, например, сопряженную величину',
         ['Sum(a_n*a_n^Asterisk,n)=Int(Psi*Psi^Asterisk*Diff(q))']);
  AddArt(' Begin', 'Символ начала доказательства и расчетов', ['Begin']);
  AddArt(' Belongs', 'Символ "принадлежит"',
         ['Belongs', '[a,b]={x&Divide&x&Belongs&Real,a<=x<=b}']);
  AddArt(' BelongsN', 'Символ "не принадлежит"', ['BelongsN']);
  AddArt(' Circle', 'Круг (окружность)', ['Circle']);
  AddArt(' CircleC', 'Круг в круге', ['CircleC']);
  AddArt(' Colon',
      'Двоеточие. В большинстве случаев может быть заменен функцией Colon.', ['x&colon&y']);
  AddArt(' Comma',
      'Вставляет в текст запятую (в отличие от символа "," не вставляет пробелы после запятой). '+
      'В большинстве случаев может быть заменен ' +
      'символом "," или функцией Comma.', ['x&comma&y']);
  AddArt(' Complex', 'Множество комплексных чисел',
      ['Complex', 'z&BArrowR&z^n, z&Belongs&Complex']);
  AddArt(' Const',
      'Слово const, обозначающее произвольную константу. В отличие от обычных ' +
      'идентификаторов, пишется прямым шрифтом, а не курсивом.',
      ['Const','Int(x*Diff(x))=x^2/2+const']);
  AddArt(' CrossC', 'Крест в круге (прямое произведение)', ['CrossC']);
  AddArt(' Degree', 'Символ градуса', ['sin(30&Degree)=1/2']);
  AddArt(' Divide', 'Знак "делит"',
        ['A&Divide&B', 'P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)']);
  AddArt(' DivideN', 'Знак "не делит"', ['A&DivideN&B']);
  AddArt(' Dot', 'Точка', ['Dot']);
  AddArt(' DotC', 'Точка в круге', ['DotC']);
  AddArt(' DotEqual', 'Приближается к пределу', ['DotEqual']);
  AddArt(' DotEqualC', 'Геометрически равный', ['DotEqualC']);
  AddArt(' DotEqualLR', 'Образ',
         ['DotEqualLR', 'f(t)&DotEqualLR&F(omega)']);
  AddArt(' DotEqualRL', 'Образ',
         ['DotEqualRL', 'F(omega)&DotEqualRL&f(t)']);
  AddArt(' DotsD', 'Диагональное многоточие с наклоном вниз',
         ['DotsD', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' DotsH', 'Горизонтальное многоточие',
        ['DotsH', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' DotsU', 'Диагональное многоточие с наклоном вверх', ['DotsU']);
  AddArt(' DotsV', 'Вертикальное многоточие',
        ['DotsV', 'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|']);
  AddArt(' Empty',
      'Пустое выражение. В отличие от Nil, имеет только нулевую ширину, а высота ' +
      'равна высоте символов.',
      ['Empty^2']);
  AddArt(' EmptySet', 'Символ "пустое множество"',
      ['EmptySet', 'G1&Intersection&G2=EmptySet']);
  AddArt(' End', 'Символ окончания доказательства или расчетов. "Что и требовалось доказать"', ['End']);
  AddArt(' Entire', 'Множество целых чисел', ['Entire']);
  AddArt(' EqualC', 'Равно в круге', ['EqualC']);
  AddArt(' Exists', 'Символ "существует"',
      ['Exists', 'Exists&x(x>1->ForAll&x(x>y&And&Exists&x(x&Divide&10)))']);
  AddArt(' ExistsN', 'Символ "не существует"', ['ExistsN']);
  AddArt(' ForAll', 'Символ "для всех"',
      ['ForAll', 'a_(n+1)<=a_n&ForAll&n&Belongs&Natural']);
  AddArt(' Ident', 'Знак "тождественно" - аналог "=="', ['Ident']);
  AddArt(' IdentN', 'Знак "не тождественно"', ['IdentN']);
  AddArt(' Inf', 'Символ бесконечности', ['Inf', 'lim(x->0,1/x)=Inf']);
  AddArt(' Intersection', 'Пересечение',
        ['Intersection','P(H_i&Divide&E)=P(H_i&Intersection&E)/P(E)']);
  AddArt(' Minus',
      'Знак "минус". Предназначен для использования преимущественно в индексах. ' +
      'Прочие знаки ("+", "=" и т. п.) можно отобразить с помощью функции String. '+
      'Но выражение String(-) даст не минус, а дефис, который существенно короче.',
      ['a_Minus<>a_String(-)']);
  AddArt(' MinusC', 'Минус в круге', ['MinusC']);
  AddArt(' Nabla', 'Символ "набла".',
      ['Nabla*f=PDiffRF(f,x)*_e_x+PDiffRF(f,y)*_e_y+PDiffRF(f,z)*_e_z',
       'Nabla*_a=PDiffRF(a_x,x)+PDiffRF(a_y,y)+PDiffRF(a_z,z)']);
  AddArt(' Natural', 'Множество натуральных чисел',
         ['Natural', 'M={x&Divide&x=1+(n+1)/n, n&Belongs&Natural}']);
  AddArt(' Nil',
      'Пустое выражение, имеющее нулевые размеры. Используется там, где по ' +
      'синтаксису должно быть выражение, но в конкретном случае требуется, ' +
      'чтобы его не было.', ['lim(nil,f(x))=0']);
  AddArt(' Not', 'Логическое отрицание', ['Not']);
  AddArt(' Or', 'Логическое "ИЛИ"', ['Or']);
  AddArt(' Parallel',
      'Знак "параллельно"', ['_a & Parallel & _b','H_Parallel']);
  AddArt(' ParallelN',
      'Знак "не параллельно"', ['_a & ParallelN & _b','H_ParallelN']);
  AddArt(' Parallelogram', 'Параллелограмм',
      ['Parallelogram', 'Parallelogram**ABCD', 'Parallelogram**A1*B1*C1*D1']);
  AddArt(' Perpendicular',
      'Знак "перпендикулярно"', ['_a & Perpendicular & _b','v_Perpendicular']);
  AddArt(' PLambda', 'Лямбда с чертой (используется в квантвой механике).',['PLambda=lambda/2/pi']);
  AddArt(' Planck', 'Постоянная Планка с чертой', ['Planck=h/2/pi']);
  AddArt(' PlusC', 'Плюс в круге (прямая сумма)', ['PlusC']);
  AddArt(' Projective', 'Проективное пространство', ['Projective']);
  AddArt(' Prop', 'Знак "пропорционально"', ['Prop']);
  AddArt(' Quadrate', 'Квадрат',
      ['Quadrate', 'Quadrate**ABCD', 'Quadrate**A1*B1*C1*D1']);
  AddArt(' Quaternion', 'Множество кватернионов Гамильтона', ['Quaternion']);
  AddArt(' Rational', 'Множество рациональных чисел',
         ['Rational', 'M={x&Divide&x&Belongs&Rational,0<=x<1}']);
  AddArt(' Real', 'Множество действительных чисел',
         ['Real', '[a,b]={x&Divide&x&Belongs&Real,a<=x<=b}']);
  AddArt(' Rectangle', 'Прямоугольник',
      ['Rectangle', 'Rectangle**ABCD', 'Rectangle**A1*B1*C1*D1']);
  AddArt(' Rhomb', 'Ромб', ['Rhomb', 'Rhomb**ABCD', 'Rhomb**A1*B1*C1*D1']);
  AddArt(' Semicolon',
      'Вставляет в текст точку с запятой (в отличие от символа ";" не вставляет пробелы после точки с запятой). '+
      'В большинстве случаев может быть заменен ' +
      'символом ";" или функцией Semicolon.', ['x&semicolon&y']);
  AddArt(' Similar', 'Подобно',
        ['Similar',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)']);
  AddArt(' SlashC', 'Косая черта в круге', ['SlashC']);
  AddArt(' SubSet', 'Знак "является подмножеством"',
        ['SubSet', 'M={!(x1,x2)&Divide&0<a<Sqr(x1)+Sqr(x2)<b}&SubSet&Real^2']);
  AddArt(' SubSetN', 'Знак "не является подмножеством"', ['SubSetN']);
  AddArt(' SuperSet', 'Знак "является надмножеством"', ['SuperSet']);
  AddArt(' SuperSet', 'Знак "не является надмножеством"', ['SuperSetN']);
  AddArt(' Triangle', 'Треугольник',
        ['Triangle',
         'Triangle**(A1*B1*C1) & Similar & Triangle**(A2*B2*C2) & String(,  если )&' +
         'SystemAnd(A1*B1=A2*B2&Comma, A1*C1=A2*C2&Comma, Angle**B1*A1*C1=Angle**B2*A2*C2&Dot)']);
  AddArt(' Union', 'Объединение', ['Union', 'Q&Union&[0,1]']);
  AddArt(' Xor', 'Логическое исключающее "ИЛИ"', ['Xor']);
end;

procedure TForm1.SetFunctions;
begin
  if EngRadioButton.Checked then
    SetFunctionsEng
  else
    SetFunctionsRus;
end;

procedure TForm1.SetFunctionsEng;
begin
  AddArt(' Abs',
      'Syntax: Abs(E). The absolute value of E. An analogue of brackets "||", ' +
      'added for compatibility with Pascal syntax.',
      ['Abs(x^2)=Abs(x)^2']);
  AddArt(' At',
      'Syntax: At(E1[,E2[,E3]]). E1 value under E2 condition (example 1) or '+
      'E1 value from E2 to E3 (example 2).',
      ['At(DiffRF(f,x),x=0)=1', 'Int(x*Diff(x),a,b)=At(x^2/2,a,b)=(b^2-a^2)/2']);
  AddArt(' Brackets',
      'Syntax: Brackets(S1S2,E). Encloses E in various brackets. ' +
      'S1 can be the symbol "(", "[", "{", "|" or "0", "1", "2", "3", "4",'+
      'S2 can be the symbol ")", "]", "}", "|" or "0", "1", "2", "3", "4" '+
      #13#10' - "0" - means no parenthesis on that side,'+
      #13#10' - "1" - angle brackets to denote, for example, the scalar product of vectors,'+
      #13#10' - "2" - brackets, denoting rounding down to the nearest integer,'+
      #13#10' - "3" - brackets, denoting rounding up to the nearest integer,'+
      #13#10' - "4" - double straight brackets.',
      ['Brackets((],0&comma&1)', 'Brackets(11,_a&comma(7)&_b)=_a*._b=a_x*b_x+a_y*b_y+a_z*b_z',
       'Brackets(22,3.4)=3<>Brackets(33,3.4)=4', 'Brackets(23,3.4)=3',
       'Brackets(23,3.6)=4', 'Brackets(44,_x)']);
  AddArt(' Cap',
      'Syntax: Cap(E). Sign "^" over E.',
      ['Cap(x)']);
  AddArt(' CaseAnd',
      'Syntax: CaseAnd(E[,...]). Selecting one of the possible options with the ' +
      '"AND" condition. Expressions in parentheses are pairs of conditional options.',
      ['|x|=CaseAnd(-x,x<0,0,x=0,x,x>1)=1',
       '|x|=CaseAnd(-x&comma,x<0&comma,0&comma,x=0&comma,x&comma,x>0&dot)=1']);
  AddArt(' CaseOr',
      'Syntax: CaseOr(E[,...]). Select one of the possible options with the ' +
      '"OR" condition. Expressions in parentheses are pairs of conditional options.',
      ['|x|=CaseOr(-x,x<0,0,x=0,x,x>1)=1',
       '|x|=CaseOr(-x&comma,x<0&comma,0&comma,x=0&comma,x&comma,x>0&dot)=1']);
  AddArt(' Circ',
      'Syntax: Circ(E1[,E2[,E3]]). Closed-loop integral of the expression E1. '+
      'E2 is placed under the integration sign, E3 is placed above it.',
      ['Circ(F*Diff(L),L)',
       'Circ([P(x,y)*Diff(x)+Q(x,y)*Diff(y)], C)=IntM(2, [PDiffRF(Q(x,y),x)-PDiffRF(P(x,y),y)]*Diff(x)*Diff(y), S)']);
  AddArt(' Colon',
      'Syntax: Colon(n). Inserts a colon into the expression followed by a ' +
      'space n units of width ',
      ['x & colon(15) & y']);
  AddArt(' Comma',
      'Syntax: Comma(n). Inserts a comma into the expression followed by a ' +
      'space n units of width.',
      ['x & comma(15) & y']);
  AddArt(' Corr',
      'Syntax: Corr(E[,...]). Arranges expressions in a lookup table of two '+
      'columns separated by a vertical bar. Expressions in parentheses are ' +
      'pairs of corresponding values.',
      ['Int(x*sin(x)**Diff(x))=|Corr(u=x, u`=1, v`=sin(x), v=-cos(x))|=' +
       'x*.(-cos(x))-Int(1*.(-cos(x))**Diff(x))=-x*cos(x)+sin(x)+const']);
  AddArt(' Diff',
      'Синтксис: Diff(E1[,E2]). Differential of the expression E1 raised to the power of E2.',
      ['Diff(x)','Diff(x,n)']);
  AddArt(' DiffN',
      'Syntax: DiffN(E1[,E2]). Differential with E2 degree of E1 expression.',
      ['DiffN(x)','DiffN(x,n)']);
  AddArt(' DiffR',
      'Syntax: DiffR(E1[,E2]). Derivative with E2 degree with respect to E1.',
      ['DiffR(x)','DiffR(x,n)','DiffR(x,2)*f(x)=DiffR(x)*DiffR(x)*f(x)']);
  AddArt(' DiffRF',
      'Syntax: DiffRF(E1,E2[,E3]). Derivative with E3 degree of E1 with respect to E2.',
      ['DiffRF(f,x)','DiffRF(f(x),x,n)']);
  AddArt(' Dot',
      'Syntax: Dot(n). Inserts a dot into the expression followed by a ' +
      'space n units of width.',
      ['x & dot(15) & y']);
  AddArt(' Fact',
      'Syntax: Fact(E). Factorial E.',
      ['Fact(n)','Fact(k+1)']);
  AddArt(' Func',
      'Syntax: Func(E1,E2). Function with name E1 and argument E2.',
      ['Func(PDiffRF(f,x,3),x)']);
  AddArt(' FuncSub',
      'Syntax: FuncSub("Name",E1,E2). Function named "Name" of expression E2 on condition E1.',
      ['FuncSub("Res",z=z0,f(z))=(1/(2*pi*i))*Circ(f(z)*Diff(z), Abs(z-z0)=epsilon)',
       'FuncSub("max",x&Belongs&[a,b],f(x))=max({f(a),...,f(b)})']);
  AddArt(' Ind',
      'Syntax: Ind(E1,E2). Adding a subscript to E1 as E2. In most cases ' +
      'it can be replaced by the character "_". When used with the Pow function ' +
      'must be applied before Pow (example 2).',
      ['Ind(a,n)','Pow(Ind(x,n),2)=(Pow(Ind(x,n+1),2)+Pow(Ind(x,n-1),2))/2']);
  AddArt(' Int',
      'Syntax: Int(E1[,E2[,E3]]). Integral of expression E1. E2 is placed ' +
      'under the integral sign, E3 is placed above it.',
      ['F(x)=Int(f(x)*Diff(x))','Phi=Int(_H*Diff(_S),S)','Int(Diff(x),0,1)=1',
      'M=Int(Diff(x)*Int(x*y*Diff(y),0,1-x),0,1)']);
  AddArt(' IntM',
      'Syntax: IntM(n,E1[,E2[,E3]]). Multiple integrals with fold n of the ' +
      'expression E1. E2 is placed under the integral sign, E3 is placed above it. ' +
      'If n=0, an integral with unknown multiplicity is drawn (ellipsis is used).',
      ['IntM(3,f(x,y,z)*Diff(x)*Diff(y)*Diff(z),V)','IntM(0,f(x1,...,x_n)*DiffN(x,n))']);
  AddArt(' Lim',
      'Синтаскис: Lim(E1,E2). Limit of the expression E2 with the condition E1.',
      ['Lim(Nil,f(x))=1','Lim(StandC(x->0,x>0),f(x))=1', 'Gamma(x+1)/Gamma(x)=Lim(n->Inf, n*x/(x+1+n))=x']);
  AddArt(' Line',
      'Syntax: Line(E). Horizontal line over E.',
      ['Line(x)','Line(x^2)']);
  AddArt(' Log',
      'Syntax: Log(E1,E2). Logarithm of E2 to the base of E1.',
      ['log(a,x+1)=ln(x+1)/ln(a)']);
  AddArt(' Matrix',
      'Syntax: Matrix(n,m,E[,...]). An m by n matrix. The expressions E and ' +
      'those following it are placed in the cells of the matrix. There can be ' +
      'fewer expressions than m*n - in this case, the last cells remain empty. '+
      'he matrix is not framed by brackets, brackets must be added explicitly.',
      ['Matrix(2,3,x,y,x-y,x+y,z,z+y)',
       '!(Matrix(2,2,1,2,-3,4))',
       'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|',
       '[_a,_b]=|Matrix(3,3,_e_x,_e_y,_e_z,x_a,y_a,z_a,x_b,y_b,z_b)|']);
  AddArt(' Num',
      'Syntax: Num(R[,n1[,n2[,n3]]]). Allows you to control the notation format ' +
      'of the number R. If the order of the number is less than or equal to -n3, ' +
      'the scientific notation is used with an accuracy of n1, if it is greater - '+
      'the usual notation with the number of digits before the point n1 and ' +
      'the total n2. Default n1=4, n2=4, n3=2.',
      ['Num(0.00123456)','Num(0.00123456, 6)','Num(0.00123456, 4, 4, 3)','Num(0.00123456, 4, 6, 3)']);
  AddArt(' PDiff',
      'Syntax: PDiff(E1[,E2]). The "partial differential" of the expression E1 ' +
      'raised to the power of E2. From a mathematical point of view, such a ' +
      '"differential" does not make sense, but the function is very convenient ' +
      'for creating expressions like example 3.',
      ['PDiff(x)','PDiff(x,n)','PDiffN(f(x,y),3)/PDiff(x)/PDiff(y,2)']);
  AddArt(' PDiffN',
      'Syntax: PDiffN(E1[,E2]). The "partial differential" of the power E2 of ' +
      'the expression E1. From a mathematical point of view, such a ' +
      '"differential" does not make sense, but the function is very convenient ' +
      'for creating expressions like example 3.',
      ['PDiffN(x)','PDiffN(x,n)','PDiffN(f(x,y),3)/PDiff(x)/PDiff(y,2)']);
  AddArt(' PDiffR',
      'Syntax: PDiffR(E1[,E2]). Partial derivative of the degree E2 with respect to E1.',
      ['PDiffR(x)','PDiffR(x,n)','Nabla=PDiffR(x)*_e_x+PDiffR(y)*_e_y+PDiffR(z)*_e_z']);
  AddArt(' PDiffRF',
      'Syntax: PDiffRF(E1,E2[,E3]). Partial derivative of the degree E3 of the '+
      'expression E1 with respect to E2.',
      ['PDiffRF(f,x)','PDiffRF(f(x,y),x,n)']);
  AddArt(' Points',
      'Syntax: Points(E[,n]). Dots over E, usually meaning derivative with respect to time.',
      ['Points(y,2)=y*Points(x)',
       'x_k=x1-(Points(y)*(Points(x)^2+Points(y)^2))/(Points(x)*Points(y,2)-Points(y)*Points(x,2))&Comma(20)&'+
       'y_k=y1+(Points(x)*(Points(x)^2+Points(y)^2))/(Points(x)*Points(y,2)-Points(y)*Points(x,2))&Dot']);
  AddArt(' Pow',
      'Syntax: Pow(E1,E2). Raising E1 to the power of E2. When used with a ' +
      'Ind function, must be applied after Ind (Example 2). In most cases it ' +
      'can be replaced by the "^" symbol.',
      ['Pow(x+2,2//3)','Pow(Ind(x,a),3)']);
  AddArt(' Prod',
      'Syntax: Prod(E1[,E2[,E3]]). Product of expressions E1. E2 is placed ' +
      'under the product sign, E3 is placed above it.',
      ['Prod(a_i)','Prod(a_i,i<>j)','Prod(a_i,i=0,n)', 'Gamma(x)=(1/x)*Prod((1+1//n)^x/(1+x//n),n=1,Inf)']);
  AddArt(' Root',
      'Syntax: Root(E1,E2). Extracting the root of the degree E1 from the expression E2.',
      ['Root(3,x-1)', 'Root(nil, x-1)']);
  AddArt(' Semicolon',
      'Syntax: Semicolon(n). Inserts a semicolon into the expression followed by ' +
      'a space n units of width.',
      ['x & semicolon(15) & y']);
  AddArt(' Space',
      'Syntax: Space(n). A space of n units of width. ' +
      'Used to separate expressions.',
      ['y=x & space(7) & z=q']);
  AddArt(' Sqr',
      'Syntax: Sqr(E). Squaring expression E. Has no advantage over using the ' +
      '"^" character or the Pow function. Added for compatibility with Pascal ' +
      'syntax.',
      ['Sqr(a+b)=Sqr(a)+2*a*b+Sqr(b)']);
  AddArt(' Sqrt',
      'Syntax: Sqrt(E). Extracting the square root of E.',
      ['Sqrt(x^2+y^2)']);
  AddArt(' StandC',
      'Syntax: StandC(E[,...]). Places multiple expressions one below the other, ' +
      'centered.',
      ['StandC(0<=i<n,i<>j)',
       '{StandC(1,1&space(15)&2)}_S&Ident&{StandC(1,2&space(15)&1)}_S&Ident&(G*E_thetao-F*G_u)/(2*(E*G-F^2))',
       '!(StandC(a,k))+!(StandC(a,k+1))=!(StandC(a+1,k+1))']);
  AddArt(' StandL',
      'Syntax: StandL(E[,...]). Places multiple expressions one below the other, ' +
      'left aligned.',
      ['StandL(0<=i<n,i<>j)']);
  AddArt(' StandR',
      'Syntax: StandR(E[,...]). Places multiple expressions one below the other, ' +
      'right aligned.',
      ['StandR(0<=i<n,i<>j)']);
  AddArt(' String',
      'Syntax: String(Text) или String("Text"). Text displayed in roman type ' +
      'without modification. If the text contains parentheses, it must be ' +
      'enclosed in double quotes.',
      ['String(Arbitrary text)','String("Text (with parentheses)")']);
  AddArt(' Strokes',
      'Syntax: Strokes(E[,n]). Adds strokes to E, usually denoting a derivative.',
      ['Strokes(f(x))','Strokes(y,3)']);
  AddArt(' Sum',
       'Syntax: Sum(E1[,E2[,E3]]). The sum of expressions E1. ' +
       'E2 is placed under the sum sign, E3 is placed above it.',
       ['Sum(a_i)','Sum(a_i,i<>j)','Sum(a_i,i=0,n)', '(a+b)^n=Sum((Fact(n)/Fact(k)/Fact(n-k))*Pow(a,n-k)*Pow(b,k),k=0,n)']);
  AddArt(' Surf',
      'Syntax: Surf(E1[,E2[,E3]]). Integral over a closed surface of the expression E1. ' +
      'E2 is placed under the integration sign, E3 is placed above it.',
      ['Surf(F*Diff(S),S)',
       'Surf([P(x,y,z)*Diff(x)*Diff(z)+Q(x,y,z)*Diff(z)*Diff(x)+R(x,y,z)*Diff(x)*Diff(y)], S)='+
       'IntM(3, [PDiffRF(P(x,y,z),x)+PDiffRF(Q(x,y,z),y)+PDiffRF(R(x,y,z),z)]*Diff(x)*Diff(y)*Diff(z), V)']);
  AddArt(' Symbol',
      'Syntax: Symbol(n). Inserts a UTF-16BE encoded character with decimal '+
      'code n into an expression in roman.',
      ['Symbol(198)=1', 'Symbol(8476)']);
  AddArt(' SymbolI',
      'Syntax: SymbolI(n). Inserts a UTF-16BE encoded character with decimal '+
      'code n into an expression in italic.',
      ['SymbolI(198)=1']);
  AddArt(' SystemAnd',
      'Syntax: SystemAnd(E[,...]). Combines expressions into square bracket system (with "AND" condition).',
      ['SystemAnd(x+y=5,x*y=6)', 'SystemAnd(x+y=5 & Semicolon,x*y=6 & Dot)']);
  AddArt(' SystemOr',
      'Syntax: SystemOr(E[,...]). Combines expressions into a figure bracket system (with "OR" condition).',
      ['x^2=4 & space(7) & DArrowR & space(7) & SystemOr(x=2 & comma, x=-2 & Dot)']);
  AddArt(' Tilde',
      'Syntax: Tilde(E). Sign "~" over E.',
      ['Tilde(x)']);
  AddArt(' Vect',
      'Syntax: Vect(E). Arrow (vector sign) above E. In most cases, can be ' +
      'replaced by the character "_" before E.',
      ['Vect(a)',
       'Vect(a(t))`=DiffRF(Vect(a),t)=Vect(i)*.DiffRF(a_x,t)+Vect(j)*.DiffRF(a_y,t)+Vect(k)*.DiffRF(a_z,t)']);
  AddArt(' Volume',
      'Syntax: Volume(E1[,E2[,E3]]). Integral over a closed volume of the ' +
      'expression E1. E2 is placed under the integration sign, E3 is placed above it.',
      ['Volume(F*Diff(V),V)']);
end;

procedure TForm1.SetFunctionsRus;
begin
  AddArt(' Abs',
      'Синтаксис: Abs(E). Модуль E. Не имеет никаких преимуществ перед ' +
      'использованием скобок ||, добавлена для совместимости с синтаксисом Паскаля.',
      ['Abs(x^2)=Abs(x)^2']);
  AddArt(' At',
      'Синтаксис: At(E1[,E2[,E3]]). Значение E1 при условии E2 (пример 1) или '+
      'от E2 до Е3 (пример 2).',
      ['At(DiffRF(f,x),x=0)=1', 'Int(x*Diff(x),a,b)=At(x^2/2,a,b)=(b^2-a^2)/2']);
  AddArt(' Brackets',
      'Синтаксис: Brackets(S1S2,E). Заключает E в различные скобки. В качестве ' +
      'S1 может стоять символ "(", "[", "{", "|" или "0", "1", "2", "3", "4",'+
      'в качестве S2 - ")", "]", "}", "|" или "0", "1", "2", "3", "4" '+
      #13#10' - "0" - означает отсутствие скобки с данной стороны,'+
      #13#10' - "1" - угловые скобки (для обозначения, например, скалярного произведения векторов),'+
      #13#10' - "2" - скобки, обозначающие округление до ближайшего целого в меньшую сторону ("пол" числа),'+
      #13#10' - "3" - скобки, обозначающие округление до ближайшего целого в большую сторону ("потолок" числа),'+
      #13#10' - "4" - двойные прямые скобки.',
      ['Brackets((],0&comma&1)', 'Brackets(11,_a&comma(7)&_b)=_a*._b=a_x*b_x+a_y*b_y+a_z*b_z',
       'Brackets(22,3.4)=3<>Brackets(33,3.4)=4', 'Brackets(23,3.4)=3',
       'Brackets(23,3.6)=4', 'Brackets(44,_x)']);
  AddArt(' Cap',
      'Синтаксис: Cap(E). Знак "крышка" над E.',
      ['Cap(x)']);
  AddArt(' CaseAnd',
      'Синтаксис: CaseAnd(E[,...]). Выбор одного из возможных вариантов с условием "И". ' +
      'Выражения в скобках идут в виде пар вариант-условие.',
      ['|x|=CaseAnd(-x,x<0,0,x=0,x,x>1)=1',
       '|x|=CaseAnd(-x&comma,x<0&comma,0&comma,x=0&comma,x&comma,x>0&dot)=1']);
  AddArt(' CaseOr',
      'Синтаксис: CaseOr(E[,...]). Выбор одного из возможных вариантов с условием "ИЛИ". ' +
      'Выражения в скобках идут в виде пар вариант-условие.',
      ['|x|=CaseOr(-x,x<0,0,x=0,x,x>1)=1',
       '|x|=CaseOr(-x&comma,x<0&comma,0&comma,x=0&comma,x&comma,x>0&dot)=1']);
  AddArt(' Circ',
      'Синтаксис: Circ(E1[,E2[,E3]]). Интеграл по замкнутому контуру выражения E1. '+
      'Под знаком интегрирования ставится E2, над ним - E3.',
      ['Circ(F*Diff(L),L)',
       'Circ([P(x,y)*Diff(x)+Q(x,y)*Diff(y)], C)=IntM(2, [PDiffRF(Q(x,y),x)-PDiffRF(P(x,y),y)]*Diff(x)*Diff(y), S)']);
  AddArt(' Colon',
      'Синтаксис: Colon(n). Вставляет в выражение двоеточие, а после него - пробел ' +
      'шириной в n единиц ширины.',
      ['x & colon(15) & y']);
  AddArt(' Comma',
      'Синтаксис: Comma(n). Вставляет в выражение запятую, а после неё - пробел ' +
      'шириной в n единиц ширины.',
      ['x & comma(15) & y']);
  AddArt(' Corr',
      'Синтаксис: Corr(E[,...]). Выстраивает выражения в таблицу соответствия из '+
      'двух столбцов, разделенных вертикальной чертой. ' +
      'Выражения в скобках идут в виде пар соответствующих значений',
      ['Int(x*sin(x)**Diff(x))=|Corr(u=x, u`=1, v`=sin(x), v=-cos(x))|=' +
       'x*.(-cos(x))-Int(1*.(-cos(x))**Diff(x))=-x*cos(x)+sin(x)+const']);
  AddArt(' Diff',
      'Синтксис: Diff(E1[,E2]). Дифференциал E1 степени E2.',
      ['Diff(x)','Diff(x,n)']);
  AddArt(' DiffN',
      'Синтаксис: DiffN(E1[,E2]). Дифференциал степени E2 выражения E1',
      ['DiffN(x)','DiffN(x,n)']);
  AddArt(' DiffR',
      'Синтаксис: DiffR(E1[,E2]). Полная производная по E1 степени E2.',
      ['DiffR(x)','DiffR(x,n)','DiffR(x,2)*f(x)=DiffR(x)*DiffR(x)*f(x)']);
  AddArt(' DiffRF',
      'Синтаксис: DiffRF(E1,E2[,E3]). Полная производная E1 по E2 степени E3.',
      ['DiffRF(f,x)','DiffRF(f(x),x,n)']);
  AddArt(' Dot',
      'Синтаксис: Dot(n). Вставляет в выражение точку, а после неё - пробел ' +
      'шириной в n единиц ширины.',
      ['x & dot(15) & y']);
  AddArt(' Fact',
      'Синтаксис: Fact(E). Факториал E. Может быть заменена символом "!" после E.',
      ['Fact(n)','Fact(k+1)']);
  AddArt(' Func',
      'Синтаксис: Func(E1,E2). Функция, "именем" которой является E1, а аргументом - E2',
      ['Func(PDiffRF(f,x,3),x)']);
  AddArt(' FuncSub',
      'Синтаксис: FuncSub("Name",E1,E2). Функция с именем "Name" выражения E2 при условии E1',
      ['FuncSub("Res",z=z0,f(z))=(1/(2*pi*i))*Circ(f(z)*Diff(z), Abs(z-z0)=epsilon)',
       'FuncSub("max",x&Belongs&[a,b],f(x))=max({f(a),...,f(b)})']);
  AddArt(' Ind',
      'Синтаксис: Ind(E1,E2). Добавление к E1 нижнего индекса в виде E2. ' +
      'В большинстве случаев может быть заменена символом "_". При использовании ' +
      'с функцией Pow должна применяться раньше Pow (пример 2).',
      ['Ind(a,n)','Pow(Ind(x,n),2)=(Pow(Ind(x,n+1),2)+Pow(Ind(x,n-1),2))/2']);
  AddArt(' Int',
      'Синтаксис: Int(E1[,E2[,E3]]). Интеграл выражения E1. Под знаком интеграла ' +
      'ставится E2, над ним - E3.',
      ['F(x)=Int(f(x)*Diff(x))','Phi=Int(_H*Diff(_S),S)','Int(Diff(x),0,1)=1',
      'M=Int(Diff(x)*Int(x*y*Diff(y),0,1-x),0,1)']);
  AddArt(' IntM',
      'Синтаксис: IntM(n,E1[,E2[,E3]]). n-кратный интеграл выражения E1. Под ' +
      'знаком интеграла ставится E2, над ним - E3. Если n<=0, рисуется интеграл ' +
      'с неизвестной кратностью (используется многоточие).',
      ['IntM(3,f(x,y,z)*Diff(x)*Diff(y)*Diff(z),V)','IntM(0,f(x1,...,x_n)*DiffN(x,n))']);
  AddArt(' Lim',
      'Синтаскис: Lim(E1,E2). Предел выражения E2 при условии E1.',
      ['Lim(Nil,f(x))=1','Lim(StandC(x->0,x>0),f(x))=1', 'Gamma(x+1)/Gamma(x)=Lim(n->Inf, n*x/(x+1+n))=x']);
  AddArt(' Line',
      'Синтаксис: Line(E). Горизонтальная черта над E.',
      ['Line(x)','Line(x^2)']);
  AddArt(' Log',
      'Синтаксис: Log(E1,E2). Логарифм E2 по основанию E1.',
      ['log(a,x+1)=ln(x+1)/ln(a)']);
  AddArt(' Matrix',
      'Синтаксис: Matrix(n,m,E[,...]). Матрица размером m на n. Выражения E и ' +
      'следующие за ним расставляются по ячейкам матрицы. Выражений может быть ' +
      'меньше, чем m*n - в этом случае последние ячейки остаются пустыми. '+
      'Матрица не обрамляется скобками, скобки надо добавлять явно',
      ['Matrix(2,3,x,y,x-y,x+y,z,z+y)',
       '!(Matrix(2,2,1,2,-3,4))',
       'det(A)=|Matrix(3,3,a_11,DotsH,a_(1&n),DotsV,DotsD,DotsV,a_m1,DotsH,a_mn)|',
       '[_a,_b]=|Matrix(3,3,_e_x,_e_y,_e_z,x_a,y_a,z_a,x_b,y_b,z_b)|']);
  AddArt(' Num',
      'Синтаксис: Num(R[,n1[,n2[,n3]]]). Позволяет управлять форматом записи ' +
      'числа R. Если порядок числа меньше или равен -n3, используется научная ' +
      'запись с точностью n1, если больше - обычная запись с числом разрядов '+
      'перед точкой n1 и общим n2. По умолчанию n1=4, n2=4, n3=2',
      ['Num(0.00123456)','Num(0.00123456, 6)','Num(0.00123456, 4, 4, 3)','Num(0.00123456, 4, 6, 3)']);
  AddArt(' PDiff',
      'Синтаксис: PDiff(E1[,E2]). "Частный дифференциал" E1 спепени E2. С ' +
      'математической точки зрения подобный "дифференциал" не имеет смысла, но ' +
      'функция очень удобна для создания выражений типа примера 3',
      ['PDiff(x)','PDiff(x,n)','PDiffN(f(x,y),3)/PDiff(x)/PDiff(y,2)']);
  AddArt(' PDiffN',
      'Синтаксис: PDiffN(E1[,E2]). "Частный дифференциал" спепени E2 выражения E1. ' +
      'С математической точки зрения подобный "дифференциал" не имеет смысла, но ' +
      'функция очень удобна для создания выражений типа примера 3',
      ['PDiffN(x)','PDiffN(x,n)','PDiffN(f(x,y),3)/PDiff(x)/PDiff(y,2)']);
  AddArt(' PDiffR',
      'Синтаксис: PDiffR(E1[,E2]). Частная производная по E1 степени E2.',
      ['PDiffR(x)','PDiffR(x,n)','Nabla=PDiffR(x)*_e_x+PDiffR(y)*_e_y+PDiffR(z)*_e_z']);
  AddArt(' PDiffRF',
      'Синтаксис: PDiffRF(E1,E2[,E3]). Частная производная E1 по E2 степени E3.',
      ['PDiffRF(f,x)','PDiffRF(f(x,y),x,n)']);
  AddArt(' Points',
      'Синтаксис: Points(E[,n]). Точки над E, обычно означающие производную по времени.',
      ['Points(y,2)=y*Points(x)',
       'x_k=x1-(Points(y)*(Points(x)^2+Points(y)^2))/(Points(x)*Points(y,2)-Points(y)*Points(x,2))&Comma(20)&'+
       'y_k=y1+(Points(x)*(Points(x)^2+Points(y)^2))/(Points(x)*Points(y,2)-Points(y)*Points(x,2))&Dot']);
  AddArt(' Pow',
      'Синтаксис: Pow(E1,E2). Возведение E1 в степень E2. При использовании с ' +
      'функцией Ind должна применяться после Ind (пример 2). В большинстве ' +
      'случаев может быть заменена символом "^"',
      ['Pow(x+2,2//3)','Pow(Ind(x,a),3)']);
  AddArt(' Prod',
      'Синтаксис: Prod(E1[,E2[,E3]]). Произведение выражений E1. Под знаком ' +
      'произведения ставится E2, над ним - E3.',
      ['Prod(a_i)','Prod(a_i,i<>j)','Prod(a_i,i=0,n)', 'Gamma(x)=(1/x)*Prod((1+1//n)^x/(1+x//n),n=1,Inf)']);
  AddArt(' Root',
      'Синтаксис: Root(E1,E2). Извлечение корня степени E1 из выражения E2.',
      ['Root(3,x-1)', 'Root(nil, x-1)']);
  AddArt(' Semicolon',
      'Синтаксис: Semicolon(n). Вставляет в выражение точку с запятой, а после неё - пробел ' +
      'шириной в n единиц ширины.',
      ['x & semicolon(15) & y']);
  AddArt(' Space',
      'Синтаксис: Space(n). Пробел размером в n единиц толщины. ' +
      'Используется для разделения выражений.',
      ['y=x & space(7) & z=q']);
  AddArt(' Sqr',
      'Синтаксис: Sqr(E). Возведение выражения E в квадрат. Не имеет никаких ' +
      'преимуществ по сравнению с использованием символа "^" или функции Pow. ' +
      'Добавлена для совместимости с синтаксисом Паскаля',
      ['Sqr(a+b)=Sqr(a)+2*a*b+Sqr(b)']);
  AddArt(' Sqrt',
      'Синтаксис: Sqrt(E). Извлечение квадратного корня из E.',
      ['Sqrt(x^2+y^2)']);
  AddArt(' StandC',
      'Синтаксис: StandC(E[,...]). Размещает несколько выражений одно под другим, ' +
      'выравнивая по центру.',
      ['StandC(0<=i<n,i<>j)',
       '{StandC(1,1&space(15)&2)}_S&Ident&{StandC(1,2&space(15)&1)}_S&Ident&(G*E_thetao-F*G_u)/(2*(E*G-F^2))',
       '!(StandC(a,k))+!(StandC(a,k+1))=!(StandC(a+1,k+1))']);
  AddArt(' StandL',
      'Синтаксис: StandL(E[,...]). Размещает несколько выражений одно под другим, ' +
      'выравнивая по левому краю.',
      ['StandL(0<=i<n,i<>j)']);
  AddArt(' StandR',
      'Синтаксис: StandR(E[,...]). Размещает несколько выражений одно под другим, ' +
      'выравнивая по правому краю.',
      ['StandR(0<=i<n,i<>j)']);
  AddArt(' String',
      'Синтаксис: String(Текст) или String("Текст"). Текст, выводящийся прямым ' +
      'шрифтом без изменений. Если в тексте встречаются круглые скобки, он ' +
      'должен быть заключён в двойные кавычки',
      ['String(Произвольный текст)','String("Текст (со скобками)")']);
  AddArt(' Strokes',
      'Синтаксис: Strokes(E[,n]). Добавляет к E штрихи, обычно обозначающие производную.',
      ['Strokes(f(x))','Strokes(y,3)']);
  AddArt(' Sum',
       'Синтаксис: Sum(E1[,E2[,E3]]). Сумма выражений E1. Под знаком суммы ' +
       'ставится E2, над ним - E3.',
       ['Sum(a_i)','Sum(a_i,i<>j)','Sum(a_i,i=0,n)', '(a+b)^n=Sum((Fact(n)/Fact(k)/Fact(n-k))*Pow(a,n-k)*Pow(b,k),k=0,n)']);

  AddArt(' Surf',
      'Синтаксис: Surf(E1[,E2[,E3]]). Интеграл по замкнутой поверхности выражения E1. Под знаком ' +
      'интегрирования ставится E2, над ним - E3.',
      ['Surf(F*Diff(S),S)',
       'Surf([P(x,y,z)*Diff(x)*Diff(z)+Q(x,y,z)*Diff(z)*Diff(x)+R(x,y,z)*Diff(x)*Diff(y)], S)='+
       'IntM(3, [PDiffRF(P(x,y,z),x)+PDiffRF(Q(x,y,z),y)+PDiffRF(R(x,y,z),z)]*Diff(x)*Diff(y)*Diff(z), V)']);
  AddArt(' Symbol',
      'Синтаксис: Symbol(n). Вставляет в выражение символ с десятичным кодом n '+
      'в кодировке UTF-16BE прямым шрифтом',
      ['Symbol(198)=1', 'Symbol(8476)']);
  AddArt(' SymbolI',
      'Синтаксис: SymbolI(n). Вставляет в выражение символ с десятичным кодом n '+
      'в кодировке UTF-16BE наклонным шрифтом',
      ['SymbolI(198)=1']);
  AddArt(' SystemAnd',
      'Синтаксис: SystemAnd(E[,...]). Объединяет выражения в систему с фигурной скобкой (с условием "И").',
      ['SystemAnd(x+y=5,x*y=6)', 'SystemAnd(x+y=5 & Semicolon,x*y=6 & Dot)']);
  AddArt(' SystemOr',
      'Синтаксис: SystemOr(E[,...]). Объединяет выражения в систему с квадратной скобкой (с условием "ИЛИ").',
      ['x^2=4 & space(7) & DArrowR & space(7) & SystemOr(x=2 & comma, x=-2 & Dot)']);
  AddArt(' Tilde',
      'Синтаксис: Tilde(E). Знак тильды над E.',
      ['Tilde(x)']);
  AddArt(' Vect',
      'Синтаксис: Vect(E). Стрелка (вектор) над E. В большинстве случаев может ' +
      'быть заменена на символ "_" перед E.',
      ['Vect(a)',
       'Vect(a(t))`=DiffRF(Vect(a),t)=Vect(i)*.DiffRF(a_x,t)+Vect(j)*.DiffRF(a_y,t)+Vect(k)*.DiffRF(a_z,t)']);
  AddArt(' Volume',
      'Синтаксис: Volume(E1[,E2[,E3]]). Интеграл по замкнутому объему выражения E1. Под знаком ' +
      'интегрирования ставится E2, над ним - E3.',
      ['Volume(F*Diff(V),V)']);
end;

procedure TForm1.SetMainStrings;
begin
  if EngRadioButton.Checked then
  begin
    Caption:= 'LazExprMake manual';
    Button1.Caption:= 'Print';
    TabControl1.Tabs[0]:= '1. Info';
    TabControl1.Tabs[1]:= '2. Operation signs';
    TabControl1.Tabs[2]:= '3. Characters before the operand';
    TabControl1.Tabs[3]:= '4. Characters after the operand';
    TabControl1.Tabs[4]:= '5. Expression concatenation';
    TabControl1.Tabs[5]:= '6. Brackets';
    TabControl1.Tabs[6]:= '7. Greek character tokens';
    TabControl1.Tabs[7]:= '8. Arrow character tokens';
    TabControl1.Tabs[8]:= '9. Other tokens';
    TabControl1.Tabs[9]:= '10. Functions';
  end
  else begin
    Caption:= 'Справочник LazExprMake';
    Button1.Caption:= 'Печать';
    TabControl1.Tabs[0]:= '1. Инфо';
    TabControl1.Tabs[1]:= '2. Знаки операций';
    TabControl1.Tabs[2]:= '3. Символы до операндов';
    TabControl1.Tabs[3]:= '4. Символы после операндов';
    TabControl1.Tabs[4]:= '5. Конкатенация выражений';
    TabControl1.Tabs[5]:= '6. Скобки';
    TabControl1.Tabs[6]:= '7. Токены греческого алфавита';
    TabControl1.Tabs[7]:= '8. Токены-стрелки';
    TabControl1.Tabs[8]:= '9. Токены прочие';
    TabControl1.Tabs[9]:= '10. Зарезервированные функции';
  end;
end;

end.

