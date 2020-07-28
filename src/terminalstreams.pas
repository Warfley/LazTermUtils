unit TerminalStreams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TerminalModifier,
  {$IfDef UNIX}
  baseunix, termio
  {$Else}
  windows
  {$EndIf};

type           
  TTerminalSize = record
    Rows: Integer;
    Columns: Integer;
  end;


  { TTerminalOutputStream }

  TTerminalOutputStream = class(THandleStream)
  private
    FModifiers: TModifiers;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Write(const AString: String); inline;
    procedure WriteLn(const AString: String); inline;  overload;
    procedure WriteLn; inline; overload;
    procedure WriteModified(const AString: String; const Modifiers: TModifiers); inline;
    procedure WriteColored(const AString: String; Foreground: TTextColor); inline; overload;
    procedure WriteColored(const AString: String; Foreground: TTextColor; Background: TTextColor); inline; overload;

    procedure Close; inline;

    procedure ModifyOutput(const AModifier: TTerminalModifier); inline;
    procedure ResetModifiers; inline;

    function WindowSize: TTerminalSize;
    function IsATTY: Boolean; inline;

    constructor Create(AHandle: THandle);
  end;

  { TTerminalInputStream }

  TTerminalInputStream = class(THandleStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function ReadToEnd: String;
    function ReadLn: String; inline;
    function ReadTo(const APattern: String; MaxLength: SizeInt; out
      PatternFound: Boolean): String;
    function ReadTo(const APattern: String; MaxLength: SizeInt): String; inline;
    function ReadTo(const APattern: String): String; inline;

    procedure Close; inline;

    constructor Create(AHandle: THandle);   
    function IsATTY: Boolean; inline;
  end;

implementation


{ TTerminalInputStream }

function TTerminalInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EWriteError.Create('Can''t write to input');
  Result := -1;
end;

function TTerminalInputStream.ReadToEnd: String;
const
  buffSize = 1024;
var
  len: SizeInt;
  resLen: SizeInt;
  buff: Array[0..buffSize - 1] of Char;
begin
  Result := '';
  resLen := 0;
  while True do
  begin
    len := Read(buff[0], buffSize);
    if len = 0 then Exit;
    if len < 0 then
      raise EReadError.Create('Error while reading from stream');
    if len > 0 then
    begin
      SetLength(Result, resLen + len);
      Move(buff[0], Result[resLen + 1], len);
      Inc(resLen, len);
    end;
    if len < buffSize then
    begin
      Sleep(0);
    end;
  end;
end;

function TTerminalInputStream.ReadTo(const APattern: String; MaxLength: SizeInt; out PatternFound: Boolean): String;
var
  c: Char;
  len: integer;
  pLen: integer;
  backtrack: integer;
  Success: SizeInt;
begin
  Result := '';
  if MaxLength <= 0 then Exit;
  SetLength(Result, 128);
  len := 0;
  plen := 0;
  backtrack := 0;
  PatternFound := False;
  try
    while len < MaxLength do
    begin
      Success := Read(c, 1);
      if Success < 0 then
        raise EWriteError.Create('Error reading from stream');
      if Success = 0 then
        Exit;
      Result[len + 1] := c;
      Inc(len);
      if len = Result.Length then
        SetLength(Result, Result.Length * 2);
      if APattern[pLen + 1] = c then
      begin
        if plen = 0 then
        begin
          backtrack := len;
        end;
        Inc(pLen);
        if pLen = APattern.Length then
        begin
          PatternFound := True;
          Exit;
        end;
      end
      else if plen > 0 then
      begin
        pLen := 0;
        while backtrack + pLen < len do
        begin
          if APattern[pLen + 1] = Result[backtrack + pLen + 1] then
            Inc(pLen)
          else
          begin
            pLen := 0;
            Inc(backtrack);
          end;
        end;
      end;
    end;
  finally
    SetLength(Result, len);
  end;
end;

function TTerminalInputStream.ReadTo(const APattern: String; MaxLength: SizeInt
  ): String;
var dummy: Boolean;
begin
  Result := ReadTo(APattern, MaxLength, dummy);
end;

function TTerminalInputStream.ReadTo(const APattern: String): String;
begin
  Result := ReadTo(APattern, SizeInt.MaxValue);
end;

procedure TTerminalInputStream.Close;
begin
  FileClose(Handle);
end;

function TTerminalInputStream.ReadLn: String;
begin
  Result := ReadTo(LineEnding);
  Result := Result.Substring(0, Result.Length - Length(LineEnding));
end;

constructor TTerminalInputStream.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

function TTerminalInputStream.IsATTY: Boolean;
begin
  Result := termio.IsATTY(Handle) <> 0;
end;

{ TTerminalOutputStream }

function TTerminalOutputStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EReadError.Create('Can''t read from output');   
  Result := -1;
end;

function TTerminalOutputStream.Write(const Buffer; Count: Longint): Longint;
var modStr: String;
begin
  if Length(FModifiers) > 0 then
  begin
    modStr:=ConstructEscapeSequence(FModifiers);
    FModifiers := [];
    WriteBuffer(modStr[1], modStr.Length);
  end;
  Result:=inherited Write(Buffer, Count);
end;

procedure TTerminalOutputStream.Write(const AString: String);
begin
  WriteBuffer(AString[1], AString.Length);
end;

procedure TTerminalOutputStream.WriteLn(const AString: String);
begin
  Write(AString);
  Write(LineEnding);
end;

procedure TTerminalOutputStream.WriteLn;
begin
  Write(LineEnding);
end;

procedure TTerminalOutputStream.WriteModified(const AString: String;
  const Modifiers: TModifiers);
begin
  Write(ModifyString(Modifiers, AString));
end;

procedure TTerminalOutputStream.WriteColored(const AString: String;
  Foreground: TTextColor);
begin
  Write(ColorText(AString, Foreground));
end;

procedure TTerminalOutputStream.WriteColored(const AString: String;
  Foreground: TTextColor; Background: TTextColor);
begin
  Write(ColorText(AString, Foreground, Background));
end;

procedure TTerminalOutputStream.Close;
begin
  FileClose(Handle);
end;

procedure TTerminalOutputStream.ModifyOutput(const AModifier: TTerminalModifier
  );
begin
  Insert(AModifier, FModifiers, Length(FModifiers));
end;

procedure TTerminalOutputStream.ResetModifiers;
begin
  FModifiers := [TerminalModifier.ResetModifiers];
end;

function TTerminalOutputStream.WindowSize: TTerminalSize;
var
  sz: TWinSize;
begin
  if not IsATTY then
  begin
    Result.Rows := -1;
    Result.Columns := -1;
  end
  else
  begin
    FpIOCtl(Handle, TIOCGWINSZ, @sz);
    Result.Rows := sz.ws_row;
    Result.Columns := sz.ws_col;
  end;
end;

function TTerminalOutputStream.IsATTY: Boolean;
begin
  Result := termio.IsATTY(Handle) <> 0;
end;

constructor TTerminalOutputStream.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

end.

