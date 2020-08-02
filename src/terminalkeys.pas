unit TerminalKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Automaton;

type
  TSequenceType = (stUnknown = 0, stChar, stModifiedChar, stCSI, stSS3);

  TSequenceInfo = record
    SequenceType: TSequenceType;
    Sequence: string;
  end;

  TKeyModifier = (kmShift = 1, kmAlt = 2, kmCtrl = 4, kmMeta = 8);
  TKeyModifiers = set of TKeyModifier;
  TSpecialKey = (skNone, skEnter, skEscape, skBackspace, skF1, skF2, skF3,
    skF4, skF5, skF6, skF7,
    skF8, skF9, skF10, skF11, skF12, skInsert, skDelete, skHome, skEnd,
    skPageUp, skPageDown, skArrowUp, skArrowDown, skArrowLeft,
    skArrowRight);

  TTerminalKey = record
    Modifiers: TKeyModifiers;
    SpecialKey: boolean;
    SpecialKeyCode: TSpecialKey;
    CharValue: string; // UTF-8 char
  end;

  EUnknownControlSequenceException = class(Exception);

procedure CreateKeyAutomatons(manager: TAutomatonManager); inline;
function KeyFromSequence(const Sequence: TSequenceInfo): TTerminalKey; inline;

implementation

const
  csi = #27'[';
  ss3 = #27'O';

procedure CreateKeyAutomatons(manager: TAutomatonManager);
begin
  // Special Keys (Arrows, F-Keys, Insert, Delete, whatever)
  manager.AddAutomaton(TEnclosedSequenceAutomaton.Create(csi,
    ['A'..'D', 'H', 'F', 'P'..'S', '~'], Ord(stCSI)));
  // F1-F4
  manager.AddAutomaton(TSingleLetterSequenceAutomaton.Create(ss3,
    ['P'..'S'], Ord(stSS3)));
  // Alt + key
  manager.AddAutomaton(TPrefixAutomaton.Create(#27, TUTF8CharAutomaton.Create(0),
    Ord(stModifiedChar)));
  // Char keys and Ctrl Keys
  manager.AddAutomaton(TUTF8CharAutomaton.Create(Ord(stChar)));
end;

function ControlKey(c: char): TTerminalKey; inline;
begin
  Result.SpecialKey := False;
  Result.Modifiers := [kmCtrl];
  Result.SpecialKeyCode := skNone;
  Result.CharValue := c;
end;

function CharKey(utf8Char: string): TTerminalKey; inline;
begin
  Result.SpecialKey := False;
  Result.Modifiers := [];
  Result.SpecialKeyCode := skNone;
  Result.CharValue := utf8Char;
end;

function SpecialKey(k: TSpecialKey): TTerminalKey; inline;
begin
  Result.SpecialKey := True;
  Result.Modifiers := [];
  Result.SpecialKeyCode := k;
end;

function SingleCharToKey(c: char): TTerminalKey;
begin
  case c of // Handling of control + key
    #0:
      Result := ControlKey(#32); // space or ctrl + 2
    #1..#8:
      Result := ControlKey(chr(Ord(c) - 1 + Ord('a')));
    #9:
      Result := CharKey(#9); // same as ctrl + I
    #10..#12:
      Result := ControlKey(chr(Ord(c) - 1 + Ord('a')));
    #13:
      Result := SpecialKey(skEnter); // enter, same as Ctrl + M
    #14..#26:
      Result := ControlKey(chr(Ord(c) - 1 + Ord('a')));
    #27:
      Result := SpecialKey(skEscape);
    #28..#31:
      Result := ControlKey(chr(Ord(c) - 27 + Ord('4')));
    #127:
      Result := SpecialKey(skBackspace);
    else
      Result := CharKey(c);
  end;
end;

function CharToKey(utf8Char: string): TTerminalKey; inline;
begin
  if utf8Char.Length = 1 then // control + keys
    Result := SingleCharToKey(utf8Char[1])
  else // all other chars
    Result := CharKey(utf8Char);
end;

function AltModified(utf8Char: string): TTerminalKey; inline;
begin
  Result := CharToKey(utf8Char);
  Result.Modifiers += [kmAlt];
end;

function ModifierForCSI(const numSeq: string;
  out success: boolean): TKeyModifiers; inline;
var
  m: integer;
begin
  Result := [];
  success := TryStrToInt(numSeq, m);
  if not success then
    Exit;
  m := m - 1;
  if m and Ord(kmShift) = Ord(kmShift) then
    Result += [kmShift];
  if m and Ord(kmAlt) = Ord(kmAlt) then
    Result += [kmAlt];
  if m and Ord(kmCtrl) = Ord(kmCtrl) then
    Result += [kmCtrl];
  if m and Ord(kmMeta) = Ord(kmMeta) then
    Result += [kmMeta];
end;

function NumericalCSISequence(const ASequence: string): TTerminalKey; inline;
var
  SeqNum: integer;
begin
  if not TryStrToInt(ASequence, SeqNum) then
    Exit(SpecialKey(skNone));
  case SeqNum of
    2:
      Result := SpecialKey(skInsert);
    3:
      Result := SpecialKey(skDelete);
    5:
      Result := SpecialKey(skPageUp);
    6:
      Result := SpecialKey(skPageDown);
    15:
      Result := SpecialKey(skF5);
    17:
      Result := SpecialKey(skF6);
    18:
      Result := SpecialKey(skF7);
    19:
      Result := SpecialKey(skF8);
    20:
      Result := SpecialKey(skF9);
    21:
      Result := SpecialKey(skF10);
    23:
      Result := SpecialKey(skF11);
    25:
      Result := SpecialKey(skF12);
    else
      Result := SpecialKey(skNone);
  end;
end;

function SingleCharCSISequence(c: char): TTerminalKey; inline;
begin
  case c of
    'A':
      Result := SpecialKey(skArrowUp);
    'B':
      Result := SpecialKey(skArrowDown);
    'C':
      Result := SpecialKey(skArrowRight);
    'D':
      Result := SpecialKey(skArrowLeft);
    'H':
      Result := SpecialKey(skHome);
    'F':
      Result := SpecialKey(skEnd);
    'P':
      Result := SpecialKey(skF1);
    'Q':
      Result := SpecialKey(skF2);
    'R':
      Result := SpecialKey(skF3);
    'S':
      Result := SpecialKey(skF4);
    else
      Result := SpecialKey(skNone);
  end;
end;

function CSISequence(ASequence: string): TTerminalKey;
var
  sp: TStringArray;
  lastChar: char;
  success: boolean;
begin
  lastChar := ASequence[ASequence.Length];
  ASequence := ASequence.Substring(0, ASequence.Length - 1);
  sp := ASequence.Split(';');
  if lastChar = '~' then
    Result := NumericalCSISequence(sp[0])
  else
    Result := SingleCharCSISequence(lastChar);
  if Result.SpecialKeyCode = skNone then
    raise EUnknownControlSequenceException.Create('Unknown Sequence: [CSI]' +
      ASequence + lastChar + '. Please report this as a bug');
  if Length(sp) > 1 then
  begin
    Result.Modifiers := ModifierForCSI(sp[1], success);
    if not Success then
      raise EUnknownControlSequenceException.Create('Unknown Sequence: [CSI]' +
        ASequence + lastChar + '. Please report this as a bug');
  end;
end;

function SS3Sequence(Seq: string): TTerminalKey; inline;
begin
  if Seq.Length <> 1 then
    raise EUnknownControlSequenceException.Create(
      'This should have never been parsed. Please report this as a bug: [ss3]' + seq);
  case Seq[1] of
    'P':
      Result := SpecialKey(skF1);
    'Q':
      Result := SpecialKey(skF2);
    'R':
      Result := SpecialKey(skF3);
    'S':
      Result := SpecialKey(skF4);
  end;
end;

function KeyFromSequence(const Sequence: TSequenceInfo): TTerminalKey;
begin
  case Sequence.SequenceType of
    stChar:
      Result := CharToKey(Sequence.Sequence);
    stModifiedChar:
      Result := AltModified(Sequence.Sequence.Substring(1));
    stCSI:
      Result := CSISequence(Sequence.Sequence.Substring(csi.Length));
    stSS3:
      Result := SS3Sequence(Sequence.Sequence.Substring(ss3.Length));
    else
      raise EUnknownControlSequenceException.Create(
        'This should have never been parsed. Please report this as a bug: ' +
        Sequence.Sequence);
  end;
end;

end.
