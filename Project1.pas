program Project1;

{$mode objfpc}{$H+}

uses
  heaptrc, TerminalModifier, Terminal;

var
  Term: TTerminal;
  s: String;
begin
  Term := TTerminal.Create;
  WriteLn(Term.Input.IsATTY);
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.ModifyOutput(SingleUnderline);
  Term.Output.Write('Bar');
  Term.Free;
  ReadLn;
end.

