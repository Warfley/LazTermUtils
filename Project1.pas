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
  Term.Input.DirectRead:=True;
  s := Term.Input.ReadLn;
  Term.Input.DirectRead:=False;
  Term.Clear;
  Term.Output.Writeln(s);
  Term.Free;
  ReadLn;
end.

