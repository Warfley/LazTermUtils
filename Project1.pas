program Project1;

{$mode objfpc}{$H+}

uses
  heaptrc, TerminalModifier, Terminal, TerminalKeys;

var
  Term: TTerminal;
  k: TTerminalKey;
  m: TKeyModifier;
begin
  Term := TTerminal.Create;
  Term.Output.WriteColored('Foo', $ff0000);
  Term.Output.WriteLn;
  Term.Input.DirectRead:=True;
  while True do
  begin
    k := Term.Input.ReadKey;
    if k.SpecialKey then
      WriteLn(k.SpecialKeyCode)
    else
      WriteLn(k.CharValue);
    for m in k.Modifiers do
      Write(m, ' ');
    WriteLn;
  end;
  Term.Free;
  ReadLn;
end.

