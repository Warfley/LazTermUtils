unit TerminalStreams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TerminalModifier, Compatibility;

type           
  TTerminalSize = record
    Rows: Integer;
    Columns: Integer;
  end;

  TClearMode = (cmFromCursor=0, cmToCursor=1, cmTotalScreen=2, cmResetScreen=3);
  TLineClearMode = (lcmFromCursor=0, lcmToCursor=1, lcmTotalLine=2);

  { TTerminalOutputStream }

  TTerminalOutputStream = class(THandleStream)
  private
    FControlBuffer: String;
    FModifiers: TModifiers;
    FOrigState: Cardinal;
    FClosed: Boolean;

    procedure ResetTerminal; inline;
    procedure WriteNonModified(const AString: String); inline;
  public
    function WindowSize: TTerminalSize; inline;
    function IsATTY: Boolean; inline;

    function isOpen: Boolean; inline;
    procedure Close; inline;

    procedure ModifyOutput(const AModifier: TTerminalModifier); inline;
    procedure ResetModifiers; inline;

    procedure Clear(ClearMode: TClearMode = cmTotalScreen; Flush: Boolean = False); inline;
    procedure ClearLine(ClearMode: TLineClearMode = lcmTotalLine; Flush: Boolean = False); inline;
    procedure CursorStartOfLine(Flush: Boolean = False); inline;
    procedure CursorMove(X: Integer; Y: Integer; Flush: Boolean = False); inline;
    procedure CursorGoto(X: Integer; Y: Integer; Flush: Boolean = False); inline;
    procedure CursorGotoX(X: Integer; Flush: Boolean = False); inline;
    procedure CursorGotoY(Y: Integer; Flush: Boolean = False); inline;

    procedure FlushControls; inline;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Write(const AString: String); inline;
    procedure WriteLn(const AString: String); inline;  overload;
    procedure WriteLn; inline; overload;
    procedure WriteModified(const AString: String; const Modifiers: TModifiers); inline;
    procedure WriteColored(const AString: String; Foreground: TTextColor); inline; overload;
    procedure WriteColored(const AString: String; Foreground: TTextColor; Background: TTextColor); inline; overload;

    constructor Create(AHandle: THandle);
    destructor Destroy; override;
  end;

  { TTerminalInputStream }

  TTerminalInputStream = class(THandleStream)   
  private
    FOrigState: Cardinal;
    FClosed: Boolean;
    FDirectRead: Boolean;
    FDirectReadRestore: TDirectReadState;

    procedure SetDirectRead(AValue: Boolean);
    procedure ResetTerminal; inline;
  public
    function IsATTY: Boolean; inline;

    procedure Close; inline;
    function isOpen: Boolean; inline;

    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadToEnd: String;
    function ReadLn(const LineEnding: String = system.LineEnding): String; inline;
    function ReadTo(const APattern: String; MaxLength: SizeInt; out
      PatternFound: Boolean): String;
    function ReadTo(const APattern: String; MaxLength: SizeInt): String; inline;
    function ReadTo(const APattern: String): String; inline;

    constructor Create(AHandle: THandle);
    destructor Destroy; override;

    property DirectRead: Boolean read FDirectRead write SetDirectRead;
  end;

implementation

{ TTerminalInputStream }

function TTerminalInputStream.IsATTY: Boolean;
begin
  Result := Compatibility.isATTY(Handle);
end;

function TTerminalInputStream.isOpen: Boolean;
begin
  Result := not FClosed;
end;

procedure TTerminalInputStream.SetDirectRead(AValue: Boolean);
begin
  if FDirectRead=AValue then Exit;
  FDirectRead:=AValue;
  if not IsATTY then Exit;
  if FDirectRead then
    FDirectReadRestore := EnableDirectRead(Handle)
  else
    RestoreDirectRead(Handle, FDirectReadRestore);
end;

procedure TTerminalInputStream.ResetTerminal;
begin
  if IsATTY then
  begin
    SetDirectRead(False);
    ResetConsole(Handle, FOrigState);
  end;
end;

procedure TTerminalInputStream.Close;
begin
  if FClosed then Exit;
  ResetTerminal;
  FileClose(Handle);
  FClosed := True;
end;

function TTerminalInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EWriteError.Create('Can''t write to input');
  Result := -1;
end;

function TTerminalInputStream.Read(var Buffer; Count: Longint): Longint;
var
  i: Integer;
begin
  if not isOpen then
    raise EReadError.Create('File already closed');
  Result:=inherited Read(Buffer, Count);
  {$IfDef Unix}
  // a hack because raw read treats enter as #13 not newline
  // this breaks readline, so we simply change it here
  if DirectRead and IsATTY then
    for i := 0 to Result -1 do
      if PChar(@Buffer)[i] = #13 then
        PChar(@Buffer)[i] := LineEnding;
  {$EndIf}
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

function TTerminalInputStream.ReadLn(const LineEnding: String): String;
begin
  Result := ReadTo(LineEnding);
  Result := Result.Substring(0, Result.Length - Length(LineEnding));
end;

constructor TTerminalInputStream.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FClosed := False;
  FDirectRead := False;
  if IsATTY then
    FOrigState := InitInputConsole(Handle);
end;

destructor TTerminalInputStream.Destroy;
begin
  if isOpen then ResetTerminal;
  inherited Destroy;
end;

{ TTerminalOutputStream }   

function TTerminalOutputStream.WindowSize: TTerminalSize;
begin
  if not IsATTY then
  begin
    Result.Rows := -1;
    Result.Columns := -1;
  end
  else
  begin
    Compatibility.GetWindowSize(Handle, Result.Rows, Result.Columns);
  end;
end;

function TTerminalOutputStream.isOpen: Boolean;
begin
  Result := not FClosed;
end;

procedure TTerminalOutputStream.ResetTerminal;
const
  ConsoleClosed: Boolean = False;
begin
  // on windows there is only one possible console, so resetting twice
  // will result in the reset sequence being printed
  if IsATTY  {$IfDef WINDOWS}and not ConsoleClosed{$EndIf} then
  begin
    {$IfDef WINDOWS}
    ConsoleClosed := True;
    {$EndIf}
    Write(RESET_SEQUENCE);
    ResetConsole(Handle, FOrigState);
  end;
end;

procedure TTerminalOutputStream.WriteNonModified(const AString: String);
var
  tmp: TModifiers;
begin
  tmp := FModifiers;
  try
    Write(AString);
  finally
    FModifiers := tmp;
  end;
end;

procedure TTerminalOutputStream.Close;
begin
  if not isOpen then Exit;
  ResetTerminal;
  FileClose(Handle);
  FClosed := False;
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

procedure TTerminalOutputStream.Clear(ClearMode: TClearMode; Flush: Boolean);
begin
  FControlBuffer += RESET_SEQUENCE + #27'[' + ord(ClearMode).ToString + 'J';
  if Flush then FlushControls;
end;

procedure TTerminalOutputStream.ClearLine(ClearMode: TLineClearMode;
  Flush: Boolean);
begin
  FControlBuffer += #27'[' + ord(ClearMode).ToString + 'K';
  if Flush then FlushControls;
end;

procedure TTerminalOutputStream.CursorStartOfLine(Flush: Boolean);
begin
  FControlBuffer += #13;
  if Flush then FlushControls;
end;

procedure TTerminalOutputStream.CursorMove(X: Integer; Y: Integer;
  Flush: Boolean);
begin
  if X > 0 then
    FControlBuffer += #27'[' + X.ToString + 'C'
  else if X < 0 then
    FControlBuffer += #27'[' + (-X).ToString + 'D';
  if Y > 0 then
    FControlBuffer += #27'[' + Y.ToString + 'B'
  else if Y < 0 then
    FControlBuffer += #27'[' + (-Y).ToString + 'A';
  if Flush then FlushControls;
end;

procedure TTerminalOutputStream.CursorGoto(X: Integer; Y: Integer;
  Flush: Boolean);
begin
  FControlBuffer += #27'[' + X.ToString + ';' + Y.ToString + 'H';
  if Flush then FlushControls;
end;

procedure TTerminalOutputStream.CursorGotoX(X: Integer; Flush: Boolean);
begin
  // It will stop at the border
  // So we first move left by an insane amount
  CursorMove(-9999, 0);
  // to then move right the right amount
  CursorMove(X, 0, Flush);
end;

procedure TTerminalOutputStream.CursorGotoY(Y: Integer; Flush: Boolean);
begin
  // It will stop at the border
  // So we first move up by an insane amount
  CursorMove(0, -9999);
  // to then move down the right amount
  CursorMove(0, Y, Flush);
end;

procedure TTerminalOutputStream.FlushControls;
begin
  WriteNonModified('');
end;

function TTerminalOutputStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EReadError.Create('Can''t read from output');   
  Result := -1;
end;

function TTerminalOutputStream.Write(const Buffer; Count: Longint): Longint;
var modStr: String;
begin
  if not isOpen then
    raise EWriteError.Create('File already closed');
  if FControlBuffer.Length > 0 then
  begin
    modStr := FControlBuffer;
    FControlBuffer := '';
    WriteNonModified(modStr);
  end;
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

function TTerminalOutputStream.IsATTY: Boolean;
begin
  Result := Compatibility.isATTY(Handle);
end;

constructor TTerminalOutputStream.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FClosed := False;
  if IsATTY then
    FOrigState := InitOutputConsole(Handle);
end;

destructor TTerminalOutputStream.Destroy;
begin
  if isOpen then ResetTerminal;
  inherited Destroy;
end;

end.

