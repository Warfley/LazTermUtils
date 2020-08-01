program Project1;

{$mode objfpc}{$H+}

uses
  heaptrc, TerminalModifier, Terminal, TerminalKeys;

var
  Term: TTerminal;
  k: TTerminalKey;
begin
  Term := TTerminal.Create;
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.WriteLn;
  Term.Input.DirectRead:=True;
  k := Term.Input.ReadKey;
  Writeln(k.SpecialKeyCode);
  Term.Free;
  ReadLn;
end.

