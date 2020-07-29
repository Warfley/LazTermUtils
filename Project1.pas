program Project1;

{$mode objfpc}{$H+}

uses
  TerminalModifier, Terminal;

var
  Term: TTerminal;
  s: String;
begin
  Term := TTerminal.Create;
  WriteLn(Term.Input.IsATTY);
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.ModifyOutput(SingleUnderline);
  Term.Output.Write('Bar');
  Term.CursorMove(0, -1);
  Term.Output.FlushControls;
  Term.ClearLine;
  s := Term.Input.ReadLn;     
  Term.Output.Writeln(s);
  Term.Free;
  ReadLn;
end.

