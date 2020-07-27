program Project1;

{$mode objfpc}{$H+}

uses
  TerminalModifier;

begin
  Writeln(ColorText('Red', $FF0000), 'Neutral');
  WriteLn(ModifyString([SingleUnderline, BackgroundColor(0, 255, 0)], 'Green Background underlined'));
  WriteLn(ColorText('Black on Wite', 0, -1));
  Write(ConstructEscapeSequence([StrikeText]));
  Writeln('from now on');
  WriteLn('everything');
  WriteLn('is modified');
  Write(RESET_SEQUENCE);
  WriteLn('And now its resetted');
end.

