program TextmodsTest;

{$mode objfpc}{$H+}

uses
  Terminal, TerminalModifier, sysutils;

var
  Term: TTerminal;
  i: Integer;
begin
  Term := TTerminal.Create;
  try
    With Term.Output do
    begin
      WriteModified('Bold', [BoldWeight]);
      Write(' ');
      WriteModified('Dim', [DimWeigth]);   
      WriteLn;
      WriteModified('Italic', [ItalicStyle]); 
      Write(' ');
      WriteModified('Fractured', [FracturedStyle]);   
      WriteLn;
      WriteModified('Single Underline', [SingleUnderline]); 
      Write(' ');
      WriteModified('Double Underline', [DoubleUnderline]);   
      WriteLn;
      WriteModified('Slow Blink', [SlowBlink]);   
      Write(' ');
      WriteModified('Fast Blink', [FastBlink]);
      WriteLn;
      WriteModified('Striked', [StrikeText]);
      WriteLn;
      for i:= 0 to 9 do
        WriteModified('Font ' + i.ToString() + ' ', [TerminalFont(i)]);
      WriteLn;
      WriteModified('Framed', [FrameText]);
      Write(' ');
      WriteModified('Circled', [CircleText]);
      WriteLn;
      WriteModified('Overline', [OverlineText]);
      WriteLn;
      WriteModified('Colored underlied bold and framed', [BoldWeight, SingleUnderline, FrameText, ForegroundColor(255,0,0)]);
      WriteLn;
    end;
  finally
    Term.Free;
  end;
end.

