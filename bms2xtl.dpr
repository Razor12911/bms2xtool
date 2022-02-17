{ MIT License

  Copyright (c) 2022 Razor12911

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. }

// {$DEFINE DLL_MODE}
// The same source code is used to compile both executable and library
{$IFDEF DLL_MODE}
library bms2xtl;
{$ELSE}
program bms2xtl;
{$APPTYPE CONSOLE}
{$ENDIF}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$R *.res}

uses
  WinAPI.Windows,
  WinAPI.Ole2,
  WinAPI.ShlObj,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  System.StrUtils,
  System.Win.Taskbar,
  System.Win.TaskbarCore,
  Threading in 'common\Threading.pas', Utils in 'common\Utils.pas',
  SynCommons in 'contrib\mORMot\SynCommons.pas',
  SynCrypto in 'contrib\mORMot\SynCrypto.pas',
  SynLZ in 'contrib\mORMot\SynLZ.pas',
  SynTable in 'contrib\mORMot\SynTable.pas', Unit1 in 'Unit1.pas';

function GetCmdStr(CommandLine: String; Index: Integer;
  KeepQuotes: Boolean = False): string;
var
  I, J, Idx: Integer;
  Quoted: Boolean;
begin
  Result := '';
  Quoted := False;
  Idx := 0;
  I := 1;
  while Idx <= Index do
  begin
    Quoted := False;
    while (I <= CommandLine.Length) and (CommandLine[I] = ' ') do
      Inc(I);
    if I > CommandLine.Length then
      break;
    Quoted := CommandLine[I] = '"';
    J := Succ(I);
    if Quoted then
      Inc(I);
    if Quoted then
    begin
      while (J <= CommandLine.Length) and (CommandLine[J] <> '"') do
        Inc(J);
    end
    else
    begin
      while (J <= CommandLine.Length) and
        (not(CharInSet(CommandLine[J], [' ', '"']))) do
        Inc(J);
    end;
    if Idx = Index then
      if (CommandLine[I] = '"') and (CommandLine[I] = CommandLine[Succ(I)]) then
        Result := ''
      else
        Result := CommandLine.Substring(Pred(I), J - I);
    if (Quoted = False) and (CommandLine[J] = '"') then
      I := J
    else
      I := Succ(J);
    Inc(Idx);
  end;
  if KeepQuotes and Quoted then
    Result := '"' + Result + '"';
end;

function GetCmdCount(CommandLine: String): Integer;
begin
  Result := 0;
  while GetCmdStr(CommandLine, Result, True) <> '' do
    Inc(Result);
end;

function IsHexString(S: String): Boolean;
var
  I: Integer;
begin
  Result := S.Length > 0;
  for I := 1 to S.Length do
    if not(CharInSet(S[I], ['0' .. '9'])) and not(CharInSet(S[I], ['a' .. 'f']))
      and not(CharInSet(S[I], ['A' .. 'F'])) then
    begin
      Result := False;
      exit;
    end;
end;

function GetLength(Str: String): Integer;
// https://www.austincc.edu/rickster/COSC1320/handouts/escchar.htm
var
  I, J: Integer;
begin
  Result := 0;
  if (Str.Length > 2) and Str.StartsWith('0x') and IsHexString(Str.Substring(2))
  then
  begin
    Result := Round(Str.Substring(2).Length / 2 + 0.1);
    exit;
  end;
  I := 0;
  while I < Str.Length do
  begin
    if (Str.Substring(I, 1) = '\') then
    begin
      J := IndexStr(Str.Substring(I + 1, 1), ['x', '0', 'n']);
      case J of
        0:
          if IsHexString(Str.Substring(I + 2, 2)) then
            Inc(I, 4)
          else
            Inc(I);
        1 .. 2:
          Inc(I, 2);
      else
        Inc(I);
      end;
    end
    else
      Inc(I);
    Inc(Result);
  end;
end;

procedure ParseScript(ScriptInput, ScriptOutput, DBFile: String;
  IsX64: Boolean = False);
// http://aluigi.altervista.org/papers/quickbms.txt
var
  I, J, K: Integer;
  S1, S2: String;
  SL1, SL2: TStringList;
  Comment: Boolean;
  X64Str1, X64Str2: String;
begin
  if IsX64 then
  begin
    X64Str1 := '64';
    X64Str2 := 'longlong';
  end
  else
  begin
    X64Str1 := '32';
    X64Str2 := 'long';
  end;
  SL1 := TStringList.Create;
  SL2 := TStringList.Create;
  with SL1 do
    try
      LoadFromFile(ScriptInput);
      SL2.Add('set XTL_STR_001 string ""');
      SL2.Add('set XTL_STR_002 string "' + DBFile + '"');
      SL2.Add('set XTL_I' + X64Str1 + '_001 ' + X64Str2 + ' 0');
      SL2.Add('set XTL_INT_001 long 0');
      SL2.Add('set XTL_INT_002 long 0');
      SL2.Add('set XTL_INT_003 long 0');
      SL2.Add('set XTL_BIN_001 binary ""');
      SL2.Add('get XTL_STR_001 fullname 0');
      SL2.Add('calldll "' + ChangeFileExt(GetModuleName, '.dll') +
        '" "Initialise" cdecl "" XTL_STR_001 XTL_STR_002');
      SL2.Add('');
      for I := 0 to Count - 1 do
      begin
        SL1[I] := ReplaceStr(SL1[I], #09, DupeString(' ', 8));
        if SL1[I].Length = 0 then
        begin
          SL2.Add('');
          continue;
        end
        else
        begin
          J := SL1[I].Length - SL1[I].TrimLeft.Length;
          K := 0;
          S1 := '';
          S2 := GetCmdStr(SL1[I], K, True);
          Comment := False;
          while S2 <> '' do
          begin
            if (S2.Length > 1) and S2.StartsWith('#') and S2.EndsWith('#') then
            begin
              Comment := False;
              Inc(K);
              S2 := GetCmdStr(SL1[I], K, True);
              continue;
            end;
            if S2.StartsWith('#') or S2.EndsWith('#') then
              Comment := not Comment;
            if not Comment then
              S1 := S1 + ' ' + S2;
            Inc(K);
            S2 := GetCmdStr(SL1[I], K, True);
          end;
          S1 := DupeString(' ', J) + S1.Substring(1);
          if (S1.TrimLeft.Length > 0) then
            SL2.Add(S1);
        end;
        case IndexText(GetCmdStr(S1, 0), ['comtype', 'encryption', 'clog',
          'log', 'open']) of
          0:
            begin
              SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string "' +
                GetCmdStr(S1, 1) + '"');
              SL2.Add(DupeString(' ', J) + 'calldll "' +
                ChangeFileExt(GetModuleName, '.dll') +
                '" "SetCompressionType" cdecl "" XTL_STR_001');
            end;
          1:
            begin
              SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string "' +
                GetCmdStr(S1, 1) + '"');
              SL2.Add(DupeString(' ', J) + 'set XTL_BIN_001 binary ' +
                GetCmdStr(S1, 2, True));
              S2 := GetCmdStr(S1, 5);
              if S2 = '' then
                S2 := GetLength(GetCmdStr(S1, 2)).ToString;
              SL2.Add(DupeString(' ', J) + 'set XTL_INT_001 long ' + S2);
              SL2.Add(DupeString(' ', J) + 'calldll "' +
                ChangeFileExt(GetModuleName, '.dll') +
                '" "SetEncryptionType" cdecl "" XTL_STR_001 XTL_BIN_001 &XTL_INT_001');
            end;
          2:
            begin
              S2 := GetCmdStr(S1, 5);
              if (S2.StartsWith('MEMORY_FILE') = False) and
                (S2.StartsWith('TEMPORARY_FILE') = False) then
              begin
                S2 := GetCmdStr(S1, 1, True);
                if S2.StartsWith('MEMORY_FILE') or
                  S2.StartsWith('TEMPORARY_FILE') then
                  S2 := '""';
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string ' + S2);
                SL2.Add(DupeString(' ', J) + 'set XTL_I' + X64Str1 + '_001 ' +
                  X64Str2 + ' ' + GetCmdStr(S1, 2, True));
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_001 long ' +
                  GetCmdStr(S1, 3, True));
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_002 long ' +
                  GetCmdStr(S1, 4, True));
                S2 := GetCmdStr(S1, 5);
                if S2 = '' then
                  S2 := I.MaxValue.ToString;
                // means current file
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_003 long ' + S2);
                SL2.Add(DupeString(' ', J) + 'calldll "' +
                  ChangeFileExt(GetModuleName, '.dll') + '" "AddStream1_' +
                  X64Str1 + '" cdecl "" XTL_STR_001 &XTL_INT_003 &XTL_I' +
                  X64Str1 + '_001 &XTL_INT_001 &XTL_INT_002');
              end;
            end;
          3:
            begin
              S2 := GetCmdStr(S1, 4);
              if (S2.StartsWith('MEMORY_FILE') = False) and
                (S2.StartsWith('TEMPORARY_FILE') = False) then
              begin
                S2 := GetCmdStr(S1, 1, True);
                if S2.StartsWith('MEMORY_FILE') or
                  S2.StartsWith('TEMPORARY_FILE') then
                  S2 := '""';
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string ' + S2);
                SL2.Add(DupeString(' ', J) + 'set XTL_I' + X64Str1 + '_001 ' +
                  X64Str2 + ' ' + GetCmdStr(S1, 2, True));
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_001 long ' +
                  GetCmdStr(S1, 3, True));
                S2 := GetCmdStr(S1, 4);
                if S2 = '' then
                  S2 := I.MaxValue.ToString;
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_002 long ' + S2);
                SL2.Add(DupeString(' ', J) + 'calldll "' +
                  ChangeFileExt(GetModuleName, '.dll') + '" "AddStream2_' +
                  X64Str1 + '" cdecl "" XTL_STR_001 &XTL_INT_002 &XTL_I' +
                  X64Str1 + '_001 &XTL_INT_001');
              end;
            end;
          4:
            begin
              if GetCmdCount(S1) = 2 then
              begin
                S2 := GetCmdStr(S1, 1);
                if (S2.StartsWith('MEMORY_FILE') = False) and
                  (S2.StartsWith('TEMPORARY_FILE') = False) then
                  S2 := GetCmdStr(S1, 1, True)
                else
                  S2 := (I.MaxValue - 2).ToString;
                // means ignore all file operations
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string ""');
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_002 string ""');
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_001 long ' + S2);
                SL2.Add(DupeString(' ', J) + 'calldll "' +
                  ChangeFileExt(GetModuleName, '.dll') +
                  '" "OpenFile" cdecl "" XTL_STR_001 XTL_STR_002 &XTL_INT_001');
              end
              else
              begin
                S2 := GetCmdStr(S1, 2, True);
                if (IndexText(GetCmdStr(S1, 1), ['FDDE', 'FDSE']) < 0) and
                  (S2.StartsWith('MEMORY_FILE') = False) and
                  (S2.StartsWith('TEMPORARY_FILE') = False) then
                  raise Exception.Create('File operation not supported');
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_001 string ' +
                  GetCmdStr(S1, 1, True));
                SL2.Add(DupeString(' ', J) + 'set XTL_STR_002 string ' + S2);
                if (S2.StartsWith('MEMORY_FILE') = False) and
                  (S2.StartsWith('TEMPORARY_FILE') = False) then
                begin
                  S2 := GetCmdStr(S1, 3);
                  if S2 = '' then
                    S2 := I.MaxValue.ToString;
                end
                else
                  S2 := (I.MaxValue - 1).ToString;
                // means ignore current file operations
                SL2.Add(DupeString(' ', J) + 'set XTL_INT_001 long ' + S2);
                SL2.Add(DupeString(' ', J) + 'calldll "' +
                  ChangeFileExt(GetModuleName, '.dll') +
                  '" "OpenFile" cdecl "" XTL_STR_001 XTL_STR_002 &XTL_INT_001');
              end;
            end;
        end;
      end;
      SL2.SaveToFile(ScriptOutput);
    finally
      Free;
      SL2.Free;
    end;
end;

{$IFDEF DLL_MODE}

exports Initialise, Deinitialise, SetCompressionType,
  SetEncryptionType, AddStream1_32, AddStream1_64,
  AddStream2_32, AddStream2_64, OpenFile;
{$ENDIF}

resourcestring
  QuickBMS = 'quickbms.exe';
  QuickBMS_4GB = 'quickbms_4gb_files.exe';

var
  I: Integer;
  S: String;
  WorkDir, ScriptFile: String;
  LList: TArray<String>;
  Taskbar: TWinTaskbar;

begin
{$IFNDEF DLL_MODE}
  WriteLn(ErrOutput, 'bms2xtool is created by Razor12911');
  WriteLn(ErrOutput, '');
  if (ParamCount < 3) or ((FileExists(QuickBMS) = False) and
    (FileExists(QuickBMS_4GB) = False)) then
  begin
    WriteLn(ErrOutput, 'Usage:');
    WriteLn(ErrOutput, '  bms2xtl script file/folder database');
    WriteLn(ErrOutput, '');
    WriteLn(ErrOutput, 'Required files:');
    WriteLn(ErrOutput, '  ' + QuickBMS + ' or ' + QuickBMS_4GB);
    exit;
  end;
  try
    CoInitialize(nil);
    Taskbar := TWinTaskbar.Create;
    Taskbar.MainWindow := GetConsoleWindow;
    Taskbar.SetProgressState(TBPF_INDETERMINATE);
    WorkDir := ChangeFileExt(GetModuleName,
      '_' + (Random(Integer.MaxValue).ToHexString).ToLower + '\');
    TDirectory.CreateDirectory(WorkDir);
    ScriptFile := WorkDir + ExtractFileName(ParamStr(1));
    ParseScript(ParamStr(1), ScriptFile, TPath.GetFullPath(ParamStr(3)),
      FileExists(ExtractFilePath(GetModuleName) + QuickBMS_4GB));
    LList := GetFileList([ParamStr(2)], True);
    Taskbar.SetProgressState(TBPF_NORMAL);
    for I := Low(LList) to High(LList) do
    begin
      Taskbar.SetProgressValue(Succ(I), Length(LList));
      S := ExtractFilePath(GetModuleName) +
        IfThen(FileExists(ExtractFilePath(GetModuleName) + QuickBMS_4GB),
        QuickBMS_4GB, QuickBMS);
      Exec(S, '-0 -C -Y -q "' + ScriptFile + '" "' + TPath.GetFullPath(LList[I])
        + '"', WorkDir)
    end;
    TDirectory.Delete(WorkDir, True);
    Taskbar.SetProgressState(TBPF_NOPROGRESS);
    Taskbar.Free;
    CoUninitialize;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}

end.
