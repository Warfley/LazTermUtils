program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, TerminalColor, TerminalModifier;

begin
  Writeln(ModifyString([tuSingle, ForegroundColor($FF0000)], 'Foo'), 'Bar');
end.

