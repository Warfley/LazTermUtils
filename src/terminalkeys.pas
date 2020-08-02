unit TerminalKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Automaton;

type
  TKeyModifier = (kmShift = 1, kmAlt = 2, kmCtrl = 4, kmMeta = 8);
  TKeyModifiers = set of TKeyModifier;
  TSpecialKey = (skEnter, skEscape, skBackspace, skF1, skF2, skF3,
    skF4, skF5, skF6, skF7,
    skF8, skF9, skF10, skF11, skF12, skInsert, skDelete, skHome, skEnd,
    skPageUp, skPageDown, skArrowUp, skArrowDown, skArrowLeft,
    skArrowRight);

  TTerminalKey = record
    Modifiers: TKeyModifiers;
    case SpecialKey: boolean of
      True: (SpecialKeyCode: TSpecialKey);
      False: (CharValue: char);
  end;

  EUnknownControlSequenceException = class(Exception);

procedure CreateKeyAutomatons(manager: TAutomatonManager); inline;
function KeyFromSequence(const Sequence: string): TTerminalKey; inline;

implementation

const
  csi = #27'[';
  ss3 = #27'O';

procedure CreateKeyAutomatons(manager: TAutomatonManager);
begin
  // Arrows, Home, End
  manager.AddAutomaton(TSingleLetterSequenceAutomaton.Create(csi, ['A'..'D', 'H', 'F']));
  // F1-F4
  manager.AddAutomaton(TSingleLetterSequenceAutomaton.Create(ss3, ['P'..'S']));
  // Alt + key
  manager.AddAutomaton(TSingleLetterSequenceAutomaton.Create(#27,
    [#0..#31, '0'..'9', 'a'..'z']));
  // F-Keys, modified F-Keys, delete, insert and co
  manager.AddAutomaton(TEnclosedSequenceAutomaton.Create(csi, ['~']));
  // Modified f keys because fuck you
  manager.AddAutomaton(TEnclosedSequenceAutomaton.Create(csi, ['P'..'S']));
  // TODO: Alt+GR automatons
end;

function ControlKey(c: char): TTerminalKey; inline;
begin
  Result.SpecialKey := False;
  Result.Modifiers := [kmCtrl];
  Result.CharValue := c;
end;

function CharKey(c: char): TTerminalKey; inline;
begin
  Result.SpecialKey := False;
  Result.Modifiers := [];
  Result.CharValue := c;
end;

function SpecialKey(k: TSpecialKey): TTerminalKey; inline;
begin
  Result.SpecialKey := True;
  Result.Modifiers := [];
  Result.SpecialKeyCode := k;
end;

function CharToKey(c: char): TTerminalKey;
begin
  case c of
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

function AltModified(c: char): TTerminalKey; inline;
begin
  Result := CharToKey(c);
  Result.Modifiers += [kmAlt];
end;

function CSICharToKey(c: char): TTerminalKey;
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
    else
      raise EUnknownControlSequenceException.Create(
        'This should have never been parsed. Please report this as a bug: [CSI]' + c);
  end;
end;

function ModifierForCSI(m: Byte): TKeyModifiers; inline;
begin
  Result := [];
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

function CSISequenceToKey(const seq: string): TTerminalKey;
var
  split: TStringArray;
begin
  split := seq.Split([';']);
  if split[0].Length = 1 then
  begin
    case split[0][1] of
      '2':
        Result := SpecialKey(skInsert);
      '3':
        Result := SpecialKey(skDelete);
      '5':
        Result := SpecialKey(skPageUp);
      '6':
        Result := SpecialKey(skPageDown);
      else
        raise EUnknownControlSequenceException.Create('Unsupported sequence. ' +
          'please report this as a bug [CSI]' + seq + '~');
    end;
  end
  else if split[0].Length = 2 then
  begin
    case split[0] of
      '15':
        Result := SpecialKey(skF5);
      '17':
        Result := SpecialKey(skF6);
      '18':
        Result := SpecialKey(skF7);
      '19':
        Result := SpecialKey(skF8);
      '20':
        Result := SpecialKey(skF9);
      '21':
        Result := SpecialKey(skF10);
      '23':
        Result := SpecialKey(skF11);
      '25':
        Result := SpecialKey(skF12);
      else
        raise EUnknownControlSequenceException.Create('Unsupported sequence. ' +
          'please report this as a bug [CSI]' + seq + '~')
    end;
  end
  else
    raise EUnknownControlSequenceException.Create('Unsupported sequence. ' +
      'please report this as a bug [CSI]' + seq + '~');

  if Length(split) > 1 then
    Result.Modifiers := ModifierForCSI(split[1].ToInteger);
end;

function CSIFKeyToKey(const seq: string): TTerminalKey;
var
  split: TStringArray;
begin
  split := seq.Split([';']);
  case seq[seq.Length] of
  'P':
    Result := SpecialKey(skF1);
  'Q':
    Result := SpecialKey(skF2);
  'R':
    Result := SpecialKey(skF3);
  'S':
    Result := SpecialKey(skF4);
  end;
  Result.Modifiers := ModifierForCSI(split[1].Substring(0, split[1].Length - 1).ToInteger);
  if Length(split) < 1 then
    raise EUnknownControlSequenceException.Create(
      'This should have never been parsed. Please report this as a bug: [CSI]' + seq);
end;

function CSIToKey(Seq: string): TTerminalKey; inline;
begin
  if Seq.Length = 1 then
    Result := CSICharToKey(Seq[1])
  else if Seq.EndsWith('~') then
    Result := CSISequenceToKey(seq.Substring(0, Seq.Length - 1))
  else if (Seq.Length > 1) and (Seq[Seq.Length] in ['P'..'S']) then
    Result := CSIFKeyToKey(Seq)
  else
    raise EUnknownControlSequenceException.Create(
      'This should have never been parsed. Please report this as a bug: [CSI]' + seq);
end;

function SS3ToKey(Seq: string): TTerminalKey; inline;
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

function KeyFromSequence(const Sequence: string): TTerminalKey;
begin
  if Sequence.Length = 1 then
    Exit(CharToKey(Sequence[1]));
  if (Sequence.Length = 2) and (Sequence[1] = #27) then
    Exit(AltModified(Sequence[2]));
  if Sequence.StartsWith(csi) then
    Exit(CSIToKey(Sequence.Substring(csi.Length)));
  if Sequence.StartsWith(ss3) then
    Exit(SS3ToKey(Sequence.Substring(ss3.Length)));
  raise EUnknownControlSequenceException.Create(
    'This should have never been parsed. Please report this as a bug: ' + Sequence);
end;

end.
