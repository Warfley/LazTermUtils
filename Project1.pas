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
  repeat
    k := Term.Input.ReadKey;
    if k.SpecialKey then
      WriteLn(k.SpecialKeyCode, #13)
    else
      WriteLn(k.CharValue, #13);
    for m in k.Modifiers do
      Write(m, ' ');
    WriteLn(#13);
  until (k.CharValue = 'c') and (kmCtrl in k.Modifiers);
  Term.Free;
  ReadLn;
end.

