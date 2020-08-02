program NonBlockingReadTest;

{$mode objfpc}{$H+}

uses
  Terminal, TerminalStreams, TerminalKeys, SysUtils;

var
  Term: TTerminal;
  key: TTerminalKey;
  s: string;
begin
  Term := TTerminal.Create;
  try
    Term.Input.DirectRead:=True;
    Term.Output.HideCursor;
    Term.Clear;
    while true do
    begin
      Term.CursorGoto(0, 0);
      DateTimeToString(s, 'dd.mm.yyyy hh:nn:ss', Now);
      Term.Output.WriteLn(s);
      Term.Output.WriteLn('To exit press escape');
      if Term.Input.ReadKeyNonBlocking(key) then
        if Key.SpecialKey and (Key.SpecialKeyCode = skEscape) then
          Break;
      Sleep(100);
    end;
  finally
    Term.Free;
  end;
end.

