unit TerminalModifier;

{$mode Delphi}
{$ModeSwitch advancedrecords}

interface

uses
  sysutils, TerminalColor;

type
  TModifierKind = (tmkReset, tmkWeight, tmkStyle, tmkUnderline, tmkBlink,
                   tmkInvert, tmkStrike, tmkFont, tmkForeground,
                   tmkBackground, tmkFrame, tmkOverline);

  TTextWeight = (twBold = 1, twDim = 2, twReset = 22);
  TTextStyle = (tsItalic = 3, tsFracture = 20, tsReset = 23);
  TTextUnderline = (tuSingle = 4, tuDouble = 21, tuReset = 24);
  TTextBlink = (tbSlow = 5, tbFast = 6, tbReset = 25);
  TTextStrike = (tskStriked = 9, tskReset = 29);
  TTextFont = (tfDefault = 10, tfAlternative1, tfAlternative2, tfAlternative3,
               tfAlternative4, tfAlternative5, tfAlternative6, tfAlternative7,
               tfAlternative8, tfAlternative9);
  TTextFrame = (tfrFrame = 51, tfrCircle = 52, tfrReset = 54);
  TTextOverline = (toOverlined = 53, toReset = 55);

  { TTextColor }

  TTextColor = record
    Reset: Boolean;
    Color: TTerminalColor;
    function AsString: String; inline;
    constructor Create(Red: Byte; Green: Byte; Blue: Byte); overload;
    constructor Create(AColor: TTerminalColor); overload;
    class function ResetColor: TTextColor; inline; static;

    class operator Implicit(constref AColor: TTerminalColor): TTextColor; inline;
    class operator Explicit(constref AColor: TTerminalColor): TTextColor; inline;
    class operator Implicit(AColor: Integer): TTextColor; inline;
    class operator Explicit(AColor: Integer): TTextColor; inline;

  end;

  { TTextInvert }

  TTextInvert = record
    class function Create: TTextInvert; inline; static;
  end;

  { TTextReset }

  TTextReset = record
    class function Create: TTextReset; inline; static;
  end;

  { TTerminalModifier }

  TTerminalModifier = record
  function GetSequence: String; inline;

  class function WeigthModifier(AWeight: TTextWeight): TTerminalModifier; inline; static;
  class operator Implicit(AWeight: TTextWeight): TTerminalModifier; inline;
  class operator Explicit(AWeight: TTextWeight): TTerminalModifier; inline;

  class function StyleModifier(AStyle: TTextStyle): TTerminalModifier; inline; static;   
  class operator Implicit(AStyle: TTextStyle): TTerminalModifier; inline;
  class operator Explicit(AStyle: TTextStyle): TTerminalModifier; inline;

  class function UnderlineModifier(AUnderline: TTextUnderline): TTerminalModifier; inline; static;   
  class operator Implicit(AUnderline: TTextUnderline): TTerminalModifier; inline;
  class operator Explicit(AUnderline: TTextUnderline): TTerminalModifier; inline;

  class function BlinkModifier(ABlink: TTextBlink): TTerminalModifier; inline; static;   
  class operator Implicit(ABlink: TTextBlink): TTerminalModifier; inline;
  class operator Explicit(ABlink: TTextBlink): TTerminalModifier; inline;

  class function StrikeModifier(Reset: Boolean = False): TTerminalModifier; inline; static;     
  class operator Implicit(AStrike: TTextStrike): TTerminalModifier; inline;
  class operator Explicit(AStrike: TTextStrike): TTerminalModifier; inline;

  class function FontModifier(AFont: TTextFont): TTerminalModifier; inline; static;    
  class operator Implicit(AFont: TTextFont): TTerminalModifier; inline;
  class operator Explicit(AFont: TTextFont): TTerminalModifier; inline;

  class function ForegroundModifier(constref AColor: TTextColor): TTerminalModifier; inline; static;     
  class function ForegroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier; inline; static;
  class function ResetForegroundColor: TTerminalModifier; inline; static;

  class function BackgroundModifier(constref AColor: TTextColor): TTerminalModifier; inline; static;
  class function BackgroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier; inline; static;
  class function ResetBackgroundColor: TTerminalModifier; inline; static;

  class function FrameModifier(AFrame: TTextFrame): TTerminalModifier; inline; static;     
  class operator Implicit(AFrame: TTextFrame): TTerminalModifier; inline;
  class operator Explicit(AFrame: TTextFrame): TTerminalModifier; inline;

  class function OverlineModifier(Reset: Boolean = False): TTerminalModifier; inline; static;
  class operator Implicit(AOverline: TTextOverline): TTerminalModifier; inline;
  class operator Explicit(AOverline: TTextOverline): TTerminalModifier; inline;

  class function ResetModifier: TTerminalModifier; inline; static;
  class operator Implicit(AReset: TTextReset): TTerminalModifier; inline;
  class operator Explicit(AReset: TTextReset): TTerminalModifier; inline;

  class function InvertModifier: TTerminalModifier; inline; static;
  class operator Implicit(AInvert: TTextInvert): TTerminalModifier; inline;
  class operator Explicit(AInvert: TTextInvert): TTerminalModifier; inline;

  case ModifierKind: TModifierKind of
    tmkWeight: (Weight: TTextWeight);
    tmkStyle: (Style: TTextStyle);
    tmkUnderline: (Underline: TTextUnderline);
    tmkBlink: (Blink: TTextBlink);
    tmkStrike: (Strike: TTextStrike);
    tmkFont: (Font: TTextFont);
    tmkForeground, tmkBackground: (Color: TTextColor);
    tmkFrame: (Frame: TTextFrame);
    tmkOverline: (Overline: TTextOverline);
  end;    

  TModifiers = array of TTerminalModifier;

// Shortcut functions
function ForegroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier; inline; overload;
function ForegroundColor(constref AColor: TTerminalColor): TTerminalModifier; inline; overload;
function ResetForeground: TTerminalModifier; inline;
function BackgroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier; inline; overload;
function BackgroundColor(constref AColor: TTerminalColor): TTerminalModifier; inline; overload;
function ResetBackground: TTerminalModifier; inline;

// Escape sequence construction
function ConstructEscapeSequence(Modifiers: TModifiers): String; inline;
function ModifyString(Modifiers: TModifiers; AString: String): String; inline;

const
  RESET_SEQUENCE = #27'[0m';

implementation

{ TTextInvert }

class function TTextInvert.Create: TTextInvert;
begin

end;

{ TTextReset }

class function TTextReset.Create: TTextReset;
begin

end;

{ TTextColor }

function TTextColor.AsString: String;
begin
  if Reset then
    Exit('9');
  Exit('8;' + Color.GetSequenceString);
end;

constructor TTextColor.Create(Red: Byte; Green: Byte; Blue: Byte);
begin    
  Reset := False;
  Color := TTerminalColor.Create(Red, Green, Blue);
end;

constructor TTextColor.Create(AColor: TTerminalColor);
begin
  Self.Create(AColor.Red, AColor.Green, AColor.Blue);
end;

class function TTextColor.ResetColor: TTextColor;
begin
  Result.Reset := True;
end;

class operator TTextColor.Implicit(constref AColor: TTerminalColor): TTextColor;
begin
  Result := TTextColor.Create(AColor);
end;

class operator TTextColor.Explicit(constref AColor: TTerminalColor): TTextColor;
begin
  Result := TTextColor.Create(AColor);
end;

class operator TTextColor.Implicit(AColor: Integer): TTextColor;
begin
  Result := TTextColor.Create(AColor);
end;

class operator TTextColor.Explicit(AColor: Integer): TTextColor;
begin
  Result := TTextColor.Create(AColor);
end;

{ TTerminalModifier }

function TTerminalModifier.GetSequence: String;
begin
  Result := '0';
  case ModifierKind of
  tmkReset: Exit('0');
  tmkWeight: Exit(ord(Weight).ToString);
  tmkStyle: Exit(ord(Style).ToString);
  tmkUnderline: Exit(ord(Underline).ToString);
  tmkBlink: Exit(ord(Blink).ToString);
  tmkInvert: Exit('7');
  tmkStrike: Exit(ord(Strike).ToString);
  tmkFont: Exit(ord(Font).ToString);
  tmkForeground: Exit('3' + Color.AsString);
  tmkBackground: Exit('4' + Color.AsString);
  tmkFrame: Exit(ord(Frame).ToString);
  tmkOverline: Exit(ord(Overline).ToString);
  end;
end;

class function TTerminalModifier.WeigthModifier(AWeight: TTextWeight
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkWeight;
  Result.Weight := AWeight;
end;

class operator TTerminalModifier.Implicit(AWeight: TTextWeight
  ): TTerminalModifier;
begin
  Result := WeigthModifier(AWeight);
end;

class operator TTerminalModifier.Explicit(AWeight: TTextWeight
  ): TTerminalModifier;
begin
  Result := WeigthModifier(AWeight);
end;

class function TTerminalModifier.StyleModifier(AStyle: TTextStyle
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkStyle;
  Result.Style := AStyle;
end;

class operator TTerminalModifier.Implicit(AStyle: TTextStyle
  ): TTerminalModifier;
begin
  Result := StyleModifier(AStyle);
end;

class operator TTerminalModifier.Explicit(AStyle: TTextStyle
  ): TTerminalModifier;
begin
  Result := StyleModifier(AStyle);
end;

class function TTerminalModifier.UnderlineModifier(AUnderline: TTextUnderline
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkUnderline;
  Result.Underline := AUnderline;
end;

class operator TTerminalModifier.Implicit(AUnderline: TTextUnderline
  ): TTerminalModifier;
begin
  Result := UnderlineModifier(AUnderline);
end;

class operator TTerminalModifier.Explicit(AUnderline: TTextUnderline
  ): TTerminalModifier;
begin
  Result := UnderlineModifier(AUnderline);
end;

class function TTerminalModifier.BlinkModifier(ABlink: TTextBlink
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkBlink;
  Result.Blink := ABlink;
end;

class operator TTerminalModifier.Implicit(ABlink: TTextBlink
  ): TTerminalModifier;
begin
  Result := BlinkModifier(ABlink);
end;

class operator TTerminalModifier.Explicit(ABlink: TTextBlink
  ): TTerminalModifier;
begin
  Result := BlinkModifier(ABlink);
end;

class function TTerminalModifier.StrikeModifier(Reset: Boolean
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkStrike;
  if Reset then
    Result.Strike := tskReset
  else
    Result.Strike := tskStriked;
end;

class operator TTerminalModifier.Implicit(AStrike: TTextStrike
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkStrike;
  Result.Strike := AStrike;
end;

class operator TTerminalModifier.Explicit(AStrike: TTextStrike
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkStrike;
  Result.Strike := AStrike;
end;

class function TTerminalModifier.FontModifier(AFont: TTextFont
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkFont;
  Result.Font := AFont;
end;

class operator TTerminalModifier.Implicit(AFont: TTextFont): TTerminalModifier;
begin
  Result := FontModifier(AFont);
end;

class operator TTerminalModifier.Explicit(AFont: TTextFont): TTerminalModifier;
begin
  Result := FontModifier(AFont);
end;

class function TTerminalModifier.ForegroundModifier(constref AColor: TTextColor
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkForeground;
  Result.Color := AColor;
end;

class function TTerminalModifier.BackgroundModifier(constref AColor: TTextColor
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkBackground;
  Result.Color := AColor;
end;

class function TTerminalModifier.ForegroundColor(Red: Byte; Green: Byte;
  Blue: Byte): TTerminalModifier;
begin
  Result := ForegroundModifier(TTextColor.Create(Red, Green, Blue));
end;

class function TTerminalModifier.ResetForegroundColor: TTerminalModifier;
begin
  Result.ModifierKind := tmkForeground;
  Result.Color.Reset := True;
end;

class function TTerminalModifier.BackgroundColor(Red: Byte; Green: Byte;
  Blue: Byte): TTerminalModifier;
begin
  Result := BackgroundModifier(TTextColor.Create(Red, Green, Blue));
end;

class function TTerminalModifier.ResetBackgroundColor: TTerminalModifier;
begin
  Result.ModifierKind := tmkBackground;
  Result.Color.Reset := True;
end;

class function TTerminalModifier.FrameModifier(AFrame: TTextFrame
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkFrame;
  Result.Frame := AFrame;
end;

class operator TTerminalModifier.Implicit(AFrame: TTextFrame
  ): TTerminalModifier;
begin
  Result := FrameModifier(AFrame);
end;

class operator TTerminalModifier.Explicit(AFrame: TTextFrame
  ): TTerminalModifier;
begin
  Result := FrameModifier(AFrame);
end;

class function TTerminalModifier.OverlineModifier(Reset: Boolean
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkOverline;
  if Reset then
    Result.Overline := toReset
  else
    Result.Overline := toOverlined;
end;

class operator TTerminalModifier.Implicit(AOverline: TTextOverline
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkOverline;
  Result.Overline := AOverline;
end;

class operator TTerminalModifier.Explicit(AOverline: TTextOverline
  ): TTerminalModifier;
begin
  Result.ModifierKind := tmkOverline;
  Result.Overline := AOverline;
end;

class function TTerminalModifier.ResetModifier: TTerminalModifier;
begin
  Result.ModifierKind := tmkReset;
end;

class operator TTerminalModifier.Implicit(AReset: TTextReset
  ): TTerminalModifier;
begin
  Result := ResetModifier;
end;

class operator TTerminalModifier.Explicit(AReset: TTextReset
  ): TTerminalModifier;
begin
  Result := ResetModifier;
end;

class function TTerminalModifier.InvertModifier: TTerminalModifier;
begin
  Result.ModifierKind := tmkInvert;
end;

class operator TTerminalModifier.Implicit(AInvert: TTextInvert
  ): TTerminalModifier;
begin
  Result := InvertModifier;
end;

class operator TTerminalModifier.Explicit(AInvert: TTextInvert
  ): TTerminalModifier;
begin
  Result := InvertModifier;
end;            

function ForegroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier;
begin
  Result := TTerminalModifier.ForegroundColor(Red, Green, Blue);
end;

function ForegroundColor(constref AColor: TTerminalColor): TTerminalModifier;
begin
  Result := TTerminalModifier.ForegroundModifier(TTextColor.Create(AColor));
end;

function ResetForeground: TTerminalModifier;
begin
  Result := TTerminalModifier.ResetForegroundColor;
end;

function BackgroundColor(Red: Byte; Green: Byte; Blue: Byte): TTerminalModifier;
begin
  Result := TTerminalModifier.BackgroundColor(Red, Green, Blue);
end;

function BackgroundColor(constref AColor: TTerminalColor): TTerminalModifier;
begin
  Result := TTerminalModifier.BackgroundModifier(TTextColor.Create(AColor));
end;

function ResetBackground: TTerminalModifier;
begin
  Result := TTerminalModifier.ResetBackgroundColor;
end;  

function ConstructEscapeSequence(Modifiers: TModifiers): String;
var
  modifier: TTerminalModifier;
begin
  Result := '';
  if Length(Modifiers) > 0 then
  begin
    Result := #27'[';
    for modifier in Modifiers do
      Result += modifier.GetSequence + ';';
    Result[Result.Length] := 'm';
  end;
end;

function ModifyString(Modifiers: TModifiers; AString: String): String;
begin
  Result := ConstructEscapeSequence(Modifiers) + AString + RESET_SEQUENCE;
end;

end.

