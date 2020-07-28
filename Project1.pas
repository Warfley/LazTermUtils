program Project1;

{$mode objfpc}{$H+}

uses
  TerminalModifier, Terminal, TerminalStreams, sysutils;

var
  Term: TTerminal;
  s: String;
begin
  Term := TTerminal.Create('/dev/pts/1');
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.ModifyOutput(SingleUnderline);
  Term.Output.WriteLn('Bar');
  s := Term.Input.ReadLn;
  Term.Output.WriteLn(s);
  Term.Free;
end.

