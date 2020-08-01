program Project1;

{$mode objfpc}{$H+}

uses
  heaptrc, TerminalModifier, Terminal, TerminalKeys;

var
  Term: TTerminal;
  s: char;
  k: TTerminalKey;
begin
  Term := TTerminal.Create;
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.WriteLn;
  Term.Output.WriteLn('foo');
  Term.Output.WriteLn('bar');
  Term.Free;
  ReadLn;
end.

