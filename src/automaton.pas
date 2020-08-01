unit Automaton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TAutomaton }

  TAutomaton = class
  private
    FCount: SizeInt;
    FFinal: SizeInt;
  protected
    procedure DoReset; virtual; abstract;
    procedure DoStep(c: char); virtual; abstract;
    function isSink: boolean; virtual; abstract;

    procedure MakeFinal; inline;
  public
    procedure Step(c: char); inline;
    procedure Reset; inline;

    constructor Create;

    property Sink: boolean read IsSink;
    property Count: SizeInt read FCount;
    property Final: SizeInt read FFinal;
  end;

  TAutomatons = specialize TFPGObjectList<TAutomaton>;

  TCharSet = set of char;

  { TSingleLetterSequenceAutomaton }

  TSingleLetterSequenceAutomaton = class(TAutomaton)
  private
    FSink: boolean;
    FInitialSequence: string;
    FMatchingLetters: TCharSet;
  protected
    procedure DoReset; override;
    procedure DoStep(c: char); override;
    function isSink: boolean; override;
  public
    constructor Create(const ASequence: string; const Letters: TCharSet);
  end;

  { TEnclosedSequenceAutomaton }

  TEnclosedSequenceAutomaton = class(TAutomaton)
  private
    FSink: boolean;
    FStartSequence: string;
    FEndChars: TCharSet;
  protected
    procedure DoReset; override;
    procedure DoStep(c: char); override;
    function isSink: boolean; override;
  public
    constructor Create(const AStart: string; AEnd: TCharSet);
  end;

  { TAutomatonManager }

  TAutomatonManager = class
  private
    FAutomatons: TAutomatons;
  public
    procedure AddAutomaton(AAutomaton: TAutomaton); inline;
    procedure Reset; inline;
    procedure Step(c: Char); inline;
    function LongestMatch: TAutomaton; inline;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TAutomatonManager }

procedure TAutomatonManager.AddAutomaton(AAutomaton: TAutomaton);
begin
  FAutomatons.Add(AAutomaton);
  AAutomaton.Reset;
end;

procedure TAutomatonManager.Reset;
var
  aut: TAutomaton;
begin
  for aut in FAutomatons do
    aut.Reset;
end;

procedure TAutomatonManager.Step(c: Char);  
var
  aut: TAutomaton;
begin
  for aut in FAutomatons do
    aut.Step(c);
end;

function TAutomatonManager.LongestMatch: TAutomaton;
var
  m: SizeInt = -1; 
  aut: TAutomaton;
begin
  Result := nil;
  for aut in FAutomatons do
    if aut.Final > m then
    begin
      m := aut.Final;
      Result := aut;
    end;
end;

constructor TAutomatonManager.Create;
begin
  FAutomatons := TAutomatons.Create;
end;

destructor TAutomatonManager.Destroy;
begin
  FAutomatons.Free;
  inherited Destroy;
end;

{ TAutomaton }

procedure TAutomaton.MakeFinal;
begin
  FFinal := FCount;
end;

procedure TAutomaton.Step(c: char);
begin
  Inc(FCount);
  DoStep(c);
end;

procedure TAutomaton.Reset;
begin
  FCount := 0;
  FFinal := -1;
  DoReset;
end;

constructor TAutomaton.Create;
begin
  Reset;
end;

{ TSingleLetterSequenceAutomaton }

procedure TSingleLetterSequenceAutomaton.DoReset;
begin
  FSink := False;
end;

procedure TSingleLetterSequenceAutomaton.DoStep(c: char);
begin
  if FSink then
    Exit;
  if not (
    // We are currently following the initial sequence
    ((Count <= FInitialSequence.Length) and (c = FInitialSequence[Count])) or
    // We are one step after the initial sequence and have a matching char
    ((Count = FInitialSequence.Length + 1) and (c in FMatchingLetters))) then
    FSink := True
  else if Count = FInitialSequence.Length + 1 then
    MakeFinal;
end;

function TSingleLetterSequenceAutomaton.isSink: boolean;
begin
  Result := FSink;
end;

constructor TSingleLetterSequenceAutomaton.Create(const ASequence: string;
  const Letters: TCharSet);
begin
  FInitialSequence := ASequence;
  FMatchingLetters := Letters;
  inherited Create;
end;

{ TEnclosedSequenceAutomaton }

procedure TEnclosedSequenceAutomaton.DoReset;
begin
  FSink := False;
end;

procedure TEnclosedSequenceAutomaton.DoStep(c: char);
begin
  if FSink then
    Exit;
  if Count <= FStartSequence.Length then
  begin
    if c <> FStartSequence[Count] then
      FSink := True;
  end
  else if c in FEndChars then
    MakeFinal;
end;

function TEnclosedSequenceAutomaton.isSink: boolean;
begin
  Result := FSink;
end;

constructor TEnclosedSequenceAutomaton.Create(const AStart: string; AEnd: TCharSet);
begin
  FStartSequence := AStart;
  FEndChars := AEnd;
end;

end.

