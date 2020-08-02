program Colortest;

{$mode objfpc}{$H+}

uses
  Terminal, TerminalModifier, TerminalStreams, math, sysutils;

// https://stackoverflow.com/questions/17743821/how-to-convert-hsb-to-rgb
function RGBFP(R, G, B: Double): TTerminalModifier;
const
  RGBmax = 255;
begin
  Result := BackgroundColor(Round(RGBmax * R), Round(RGBmax * G), Round(RGBmax * B));
end;

function HSVtoRGB(H, S, V: Double): TTerminalModifier;
var
  i: Integer;
  f, p, q, t: Double;
begin
  if S = 0.0 then
  begin
    // achromatic (grey)
    Result := RGBFP(V, V, V);
    exit;
  end;

  H := H * 6.0; // sector 0 to 5
  i := floor(H);
  f := H - i; // fractional part of H
  p := V * (1.0 - S);
  q := V * (1.0 - S * f);
  t := V * (1.0 - S * (1.0 - f));
  case i of
  0:
    Result := RGBFP(V, t, p);
  1:
    Result := RGBFP(q, V, p);
  2:
    Result := RGBFP(p, V, t);
  3:
    Result := RGBFP(p, q, V);
  4:
    Result := RGBFP(t, p, V);
  else
    Result := RGBFP(V, p, q);
  end;
end;

var
  Term: TTerminal;
  i, v, frames: Integer;
  start: QWord;
  sz: TTerminalSize;
begin
  Term := TTerminal.Create;
  try
    start := GetTickCount64;
    v := 0;
    Term.Output.Clear;
    Frames := 0;
    // printing will only occur on flushbuffer because writing SizeInt.Maxvalue
    // bytes to memory is simply impossible
    Term.Output.BufferSize:=SizeInt.MaxValue;
    while True do
    begin
      if GetTickCount64 > start + 1000 then
      begin
        Term.Output.CursorGoto(0, 0);
        Term.Output.WriteLn(frames.ToString + ' fps');
        Start := GetTickCount64;
        Frames := 0;
      end;
      Term.Output.CursorGoto(0, 1);
      v := (v + 1) mod 1001;
      sz := Term.Size;
      for i := 0 to (sz.Rows-1)*sz.Columns - 1 do
      begin
        Term.Output.WriteModified(' ', [HSVtoRGB(i/((sz.Rows-1)*sz.Columns), 1, Abs(v-500)/500)]);
      end;  
      Term.Output.FlushBuffer;
      Inc(Frames);
    end;
  finally
    Term.Free;
  end;
end.

