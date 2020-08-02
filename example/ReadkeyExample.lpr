program ReadkeyExample;

{$mode objfpc}{$H+}

uses
  heaptrc, Terminal, TerminalKeys;

var
  Term: TTerminal;
  k: TTerminalKey;
  m: TKeyModifier;
begin
  Term := TTerminal.Create;
  try
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
  finally
    Term.Free;
  end;
end.

