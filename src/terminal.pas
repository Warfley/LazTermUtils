unit Terminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TerminalModifier, TerminalStreams, Compatibility;

type
  { TTerminal }

  TTerminal = class
  private
    FInputStream: TTerminalInputStream;
    FOutputStream: TTerminalOutputStream;
    FErrorStream: TTerminalOutputStream;
    FOwnsHandles: Boolean;

  public
    function GetWindowSize: TTerminalSize; inline;
    procedure Clear(ClearMode: TClearMode = cmTotalScreen; Flush: Boolean = True); inline;
    procedure ClearLine(ClearMode: TLineClearMode = lcmTotalLine; Flush: Boolean = False); inline;
    procedure CursorMove(X: Integer; Y: Integer; Flush: Boolean = False); inline;
    procedure CursorGoto(X: Integer; Y: Integer; Flush: Boolean = False); inline;
    procedure CursorGotoX(X: Integer; Flush: Boolean = False); inline;
    procedure CursorGotoY(Y: Integer; Flush: Boolean = False); inline;
    function IsATTY: Boolean; inline;

    constructor Create;
    constructor Create(const TTYFile: String);
    constructor Create(const InpFile: String; const OutFile: String);
    constructor Create(const InpFile: String; const OutFile: String; const ErrorFile: String);
    constructor Create(AInput: THandle; AOutput: THandle; AError: THandle);

    destructor Destroy; override;

    property Size: TTerminalSize read GetWindowSize;
    property Input: TTerminalInputStream read FInputStream;
    property Output: TTerminalOutputStream read FOutputStream;
    property Error: TTerminalOutputStream read FErrorStream;
  end;

implementation

{ TTerminal }

function TTerminal.GetWindowSize: TTerminalSize;
begin
  if Output.IsATTY then
    Result := Output.WindowSize
  else
    Result := Error.WindowSize;
end;

procedure TTerminal.Clear(ClearMode: TClearMode; Flush: Boolean);
begin
  if Output.IsATTY then
  begin
    Output.Clear(ClearMode, Flush);
  end
  else
  begin
    Error.Clear(ClearMode, Flush);
  end;
end;

procedure TTerminal.ClearLine(ClearMode: TLineClearMode; Flush: Boolean);
begin
  if Output.IsATTY then
    Output.ClearLine(ClearMode, Flush)
  else
    Error.ClearLine(ClearMode, Flush);
end;

procedure TTerminal.CursorMove(X: Integer; Y: Integer; Flush: Boolean);
begin
  if Output.IsATTY then
    Output.CursorMove(X, Y, Flush)
  else
    Error.CursorMove(X, Y, Flush);
end;

procedure TTerminal.CursorGoto(X: Integer; Y: Integer; Flush: Boolean);
begin
  if Output.IsATTY then
    Output.CursorGoto(X, Y, Flush)
  else
    Error.CursorGoto(X, Y, Flush);
end;

procedure TTerminal.CursorGotoX(X: Integer; Flush: Boolean);
begin
  if Output.IsATTY then
    Output.CursorGotoX(X, Flush)
  else
    Error.CursorGotoX(X, Flush);
end;

procedure TTerminal.CursorGotoY(Y: Integer; Flush: Boolean);
begin
  if Output.IsATTY then
    Output.CursorGotoY(Y, Flush)
  else
    Error.CursorGotoY(Y, Flush);
end;

function TTerminal.IsATTY: Boolean;
begin
  Result := Output.IsATTY Or Error.IsATTY;
end;

constructor TTerminal.Create;
begin
  Self.Create(StandardIn, StandardOut, StandardErr);
end;

constructor TTerminal.Create(const TTYFile: String);
begin
  Create(TTYFile, TTYFile, TTYFile);
end;

constructor TTerminal.Create(const InpFile: String; const OutFile: String);
begin
  Create(InpFile, OutFile, OutFile);
end;

constructor TTerminal.Create(const InpFile: String; const OutFile: String;
  const ErrorFile: String);
var
  inpHandle: THandle;
  outHandle: THandle;
  errHandle: THandle;
begin
  inpHandle := OpenTerminalFile(InpFile, False);
  outHandle := OpenTerminalFile(OutFile, True);
  errHandle := OpenTerminalFile(ErrorFile, True);
  Self.Create(inpHandle, outHandle, errHandle);     
  FOwnsHandles:=True;
end;

constructor TTerminal.Create(AInput: THandle; AOutput: THandle; AError: THandle
  );
begin
  FInputStream := TTerminalInputStream.Create(AInput);
  FOutputStream := TTerminalOutputStream.Create(AOutput);
  FErrorStream := TTerminalOutputStream.Create(AError);
  FOwnsHandles:=False;
end;

destructor TTerminal.Destroy;
begin
  if FOwnsHandles then
  begin
    FInputStream.Close;
    FOutputStream.Close;
    FErrorStream.Close;
  end;
  FInputStream.Free;
  FOutputStream.Free;
  FErrorStream.Free;
  inherited Destroy;
end;

end.

