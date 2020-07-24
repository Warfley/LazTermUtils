unit TerminalColor;

{$mode delphi}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils;

type

  { TTerminalColor }

  TTerminalColor = record
  private
    function Get8BitColor: Byte; inline;
  public
    Red: Byte;
    Green: Byte;
    Blue: Byte;

    constructor Create(R: Byte; G: Byte; B: Byte); overload;
    constructor Create(LCLColor: Integer); overload;

    function Grayscale: Boolean; inline;
    function GetSequenceString: String; inline;

    class operator Implicit(AColor: Integer): TTerminalColor; inline;
    class operator Explicit(AColor: Integer): TTerminalColor; inline;
  end;


implementation

{ TTerminalColor }      

function TTerminalColor.Grayscale: Boolean;
begin
  Result := (Red = Green) and (Green = Blue);
end;

function TTerminalColor.Get8BitColor: Byte;
var
  R, G, B: Byte;
begin
  if Grayscale then
    Exit(Round(Red / 255 * 23) + 232);
  // FIXME: 0-15 values
  R := Round(Red / 255 * 5);
  G := Round(Green / 255 * 5);
  B := Round(Blue / 255 * 5);
  Result := 16 + 36 * R + 6 * G + B;
end;

constructor TTerminalColor.Create(R: Byte; G: Byte; B: Byte);
begin
  Red := R;
  Green := G;
  Blue := B;
end;

constructor TTerminalColor.Create(LCLColor: Integer);
var
  converter: record
  case Boolean of
  True: (Color: Integer);
  {$IfDef ENDIAN_LITTLE}
  False: (B, G, R, Opc: Byte);
  {$Else}
  False: (Opc, R, G, B: Byte);
  {$EndIf}
  end;

begin
  Converter.Color := LCLColor;
  Red := converter.R;
  Green:= converter.G;
  Blue := converter.B;
end;

function TTerminalColor.GetSequenceString: String;
begin
  {$IFDEF Col8}             
    Result := '5;' + Get8BitColor.ToString;
  {$Else}
    Result := '2;%d;%d;%d'.Format([Red, Green, Blue])
  {$ENDIF}
end;

class operator TTerminalColor.Implicit(AColor: Integer): TTerminalColor;
begin
  Result := TTerminalColor.Create(AColor);
end;

class operator TTerminalColor.Explicit(AColor: Integer): TTerminalColor;
begin
  Result := TTerminalColor.Create(AColor);
end;

end.

