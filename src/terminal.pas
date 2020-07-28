unit Terminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TerminalModifier, TerminalStreams
  {$IfDef UNIX}
  , BaseUnix
  {$EndIf};

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
    procedure Clear; inline;
    procedure MoveCursor; inline;
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
  Result := Output.WindowSize;
end;

procedure TTerminal.Clear;
begin

end;

procedure TTerminal.MoveCursor;
begin

end;

function TTerminal.IsATTY: Boolean;
begin
  Result := Output.IsATTY;
end;

constructor TTerminal.Create;
begin
  Self.Create(StdInputHandle, StdOutputHandle, StdErrorHandle);
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
  inpHandle := FpOpen(InpFile, O_RDONLY);
  outHandle := FpOpen(OutFile, O_WRONLY);
  errHandle := FpOpen(ErrorFile, O_WRONLY);
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
  // Reset all changes to the terminal
  FErrorStream.Write(RESET_SEQUENCE);  
  FOutputStream.Write(RESET_SEQUENCE);
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

