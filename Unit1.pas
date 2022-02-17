unit Unit1;

interface

uses
  Utils, SynCommons, SynCrypto,
  WinAPI.Windows,
  System.SysUtils, System.Classes, System.StrUtils, System.Math, System.Masks,
  System.IniFiles, System.Generics.Defaults, System.Generics.Collections;

const
  XTOOL_DB = $31445458;

type
  PEntryStruct = ^TEntryStruct;

  TEntryStruct = record
    Position: Int64;
    OldSize, NewSize: Integer;
  end;

  TEntryStructComparer = class(TComparer<TEntryStruct>)
  public
    function Compare(const Left, Right: TEntryStruct): Integer; override;
  end;

var
  EntryStructCmp: TEntryStructComparer;

procedure Initialise(AFileInput, AFileOutput: PAnsiChar)cdecl;
procedure Deinitialise cdecl;
procedure SetCompressionType(AName: PAnsiChar)cdecl;
procedure SetEncryptionType(AName: PAnsiChar; AKey: Pointer;
  ASize: PInteger)cdecl;
procedure AddStream1_32(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInteger; AOldSize, ANewSize: PInteger)cdecl;
procedure AddStream1_64(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInt64; AOldSize, ANewSize: PInteger)cdecl;
procedure AddStream2_32(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInteger; AOrigSize: PInteger)cdecl;
procedure AddStream2_64(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInt64; AOrigSize: PInteger)cdecl;
procedure OpenFile(AType, AFileName: PAnsiChar; AFileIndex: PInteger)cdecl;

implementation

function FSMode(OpenAndUse: Boolean): Word;
begin
  if OpenAndUse then
    Result := fmOpenReadWrite or fmShareDenyNone
  else
    Result := fmCreate;
end;

function TEntryStructComparer.Compare(const Left, Right: TEntryStruct): Integer;
begin
  Result := Integer(CompareValue(Left.Position, Right.Position));
end;

type
  PResourceInfo = ^TResourceInfo;

  TResourceInfo = record
    Ident: Cardinal;
    Used: Boolean;
    Data: TMemoryStream;
  end;

  PFileInfo = ^TFileInfo;

  TFileInfo = record
    FileName: String;
    FileIndex: Integer;
    CodecName: TArray<String>;
    CodecResource: TArray<Integer>;
    CodecEntries: TArray<TListEx<TEntryStruct>>;
  end;

  PHashStruct = ^THashStruct;

  THashStruct = record
    Size: Integer;
    Hash: Cardinal;
  end;

const
  MinSize = 65536;
  HashSize = 4 * 1024 * 1024;

var
  Initialised: Boolean;
  Ini: TMemIniFile;
  ExcludeList: TArray<String>;
  Accept1, Accept2: Boolean;
  LAlgo1, LAlgo2: String;
  LResMem: TMemoryStream;
  LFileInput, LFileOutput: String;
  LFileIndex: Integer;
  LFiles: TArray<TFileInfo>;
  LResources: TArray<TResourceInfo>;

function GetIndex1(AFileIndex: Integer): Integer;
var
  S: PString;
  I: PInteger;
begin
  Result := IndexText(LAlgo1, LFiles[AFileIndex].CodecName);
  if Result < 0 then
  begin
    New(S);
    S^ := LAlgo1;
    Result := Length(LFiles[AFileIndex].CodecName);
    Insert(S^, LFiles[AFileIndex].CodecName, Result);
    New(I);
    I^ := -1;
    Insert(I^, LFiles[AFileIndex].CodecResource, Result);
    Insert(TListEx<TEntryStruct>.Create(EntryStructCmp),
      LFiles[AFileIndex].CodecEntries, Result);
  end;
end;

function GetIndex2(AFileIndex: Integer): Integer;
var
  I, J: Integer;
  CRC: Cardinal;
  LResInfo: PResourceInfo;
  S1: String;
  S2: PString;
  K: PInteger;
begin
  J := -1;
  CRC := Utils.Hash32(0, LResMem.Memory, LResMem.Position);
  for I := Low(LResources) to High(LResources) do
    if CRC = LResources[I].Ident then
    begin
      J := I;
      break;
    end;
  if J < 0 then
  begin
    New(LResInfo);
    LResInfo^.Ident := CRC;
    LResInfo^.Used := False;
    LResInfo^.Data := TMemoryStream.Create;
    LResInfo^.Data.WriteBuffer(LResMem.Memory^, LResMem.Position);
    J := Length(LResources);
    Insert(LResInfo^, LResources, J);
  end;
  S1 := IfThen((LAlgo1 <> '') and Accept1, LAlgo2 + '/' + LAlgo1, LAlgo2);
  Result := IndexText(S1, LFiles[AFileIndex].CodecName);
  if Result < 0 then
  begin
    New(S2);
    S2^ := S1;
    Result := Length(LFiles[AFileIndex].CodecName);
    Insert(S2^, LFiles[AFileIndex].CodecName, Result);
    New(K);
    K^ := Result;
    Insert(K^, LFiles[AFileIndex].CodecResource, Result);
    Insert(TListEx<TEntryStruct>.Create(EntryStructCmp),
      LFiles[AFileIndex].CodecEntries, Result);
  end;
end;

function GenerateHashList(Stream: TStream;
  var HashList: TArray<THashStruct>): Integer;
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  I: Integer;
  X, Y: Integer;
begin
  Result := 0;
  SetLength(HashList, Max(Length(HashList), IfThen(Stream.Size mod HashSize = 0,
    Stream.Size div HashSize, Succ(Stream.Size div HashSize))));
  Stream.Position := 0;
  for I := Low(HashList) to High(HashList) do
  begin
    HashList[I].Size := 0;
    HashList[I].Hash := 0;
    X := HashSize;
    Y := Stream.Read(Buffer[0], Min(X, BufferSize));
    while Y > 0 do
    begin
      Inc(HashList[I].Size, Y);
      HashList[I].Hash := Utils.Hash32(HashList[I].Hash, @Buffer[0], Y);
      Dec(X, Y);
      Y := Stream.Read(Buffer[0], Min(X, BufferSize));
    end;
    Inc(Result);
    if HashList[I].Size = 0 then
      break;
  end;
end;

procedure Initialise(AFileInput, AFileOutput: PAnsiChar);
var
  LFileInfo: PFileInfo;
begin
  Ini := TMemIniFile.Create(ChangeFileExt(Utils.GetModuleName, '.ini'));
  ExcludeList := DecodeStr(Ini.ReadString('Config', 'Exclude', ''), ',');
  LFileInput := String(AFileInput);
  LFileOutput := String(AFileOutput);
  Initialised := true;
  Accept1 := False;
  Accept2 := False;
  LAlgo1 := '';
  LAlgo2 := '';
  LResMem := TMemoryStream.Create;
  New(LFileInfo);
  LFileInfo^.FileName := LFileInput;
  LFileInfo^.FileIndex := 0;
  SetLength(LFileInfo^.CodecName, 0);
  SetLength(LFileInfo^.CodecResource, 0);
  SetLength(LFileInfo^.CodecEntries, 0);
  LFileIndex := Length(LFiles);
  Insert(LFileInfo^, LFiles, LFileIndex);
end;

procedure Deinitialise;
var
  Buffer: TBytes;
  LBytes: TBytes;
  I, J, K: Integer;
  S: String;
  B: Boolean;
  LEntry: TEntryStruct;
  FS: TFileStream;
  MS: TMemoryStream;
  LSize: Int64;
  Hash: Cardinal;
  HashList: TArray<THashStruct>;
  HashCount: Integer;
begin
  LResMem.Free;
  SetLength(Buffer, HashSize);
  MS := TMemoryStream.Create;
  for I := Low(LFiles) to High(LFiles) do
  begin
    LSize := FileSize(LFiles[I].FileName);
    if LSize >= MinSize then
    begin
      B := False;
      for J := Low(LFiles[I].CodecEntries) to High(LFiles[I].CodecEntries) do
      begin
        LFiles[I].CodecEntries[J].Sort;
        if LFiles[I].CodecEntries[J].Count > 0 then
          B := true;
      end;
      if B then
      begin
        LSize := Min(LSize, HashSize);
        FS := TFileStream.Create(LFiles[I].FileName, fmShareDenyNone);
        FS.ReadBuffer(Buffer[0], LSize);
        HashCount := GenerateHashList(FS, HashList);
        FS.Free;
        Hash := Utils.Hash32(0, @Buffer[0], MinSize);
        for J := Low(LFiles[I].CodecEntries) to High(LFiles[I].CodecEntries) do
        begin
          if LFiles[I].CodecEntries[J].Count = 0 then
            continue;
          MS.WriteBuffer(Buffer[0], Integer.Size);
          MS.WriteBuffer(Buffer[MinSize - Integer.Size], Integer.Size);
          MS.WriteBuffer(Hash, Hash.Size);
          MS.WriteBuffer(HashCount, HashCount.Size);
          MS.WriteBuffer(HashList[0], HashCount * SizeOf(THashStruct));
          K := LFiles[I].CodecResource[J];
          S := LFiles[I].CodecName[J];
          if K >= 0 then
            S.Insert(IfThen(S.IndexOf('/') < 0, S.Length, S.IndexOf('/')),
              ':' + ChangeFileExt(ExtractFileName(LFileOutput),
              '_' + LResources[K].Ident.ToHexString.ToLower));
          LBytes := BytesOf(S);
          K := Length(LBytes);
          MS.WriteBuffer(K, K.Size);
          MS.WriteBuffer(LBytes[0], K);
          K := LFiles[I].CodecEntries[J].Count;
          MS.WriteBuffer(K, K.Size);
          LFiles[I].CodecEntries[J].Index := 0;
          K := LFiles[I].CodecEntries[J].Get(LEntry);
          while K >= 0 do
          begin
            MS.WriteBuffer(LEntry, SizeOf(TEntryStruct));
            K := LFiles[I].CodecEntries[J].Get(LEntry);
          end;
        end;
      end;
    end;
  end;
  if MS.Size > 0 then
  begin
    FS := TFileStream.Create(LFileOutput, FSMode(FileExists(LFileOutput)));
    FS.Position := FS.Size;
    if FS.Position = 0 then
    begin
      K := XTOOL_DB;
      FS.WriteBuffer(K, K.Size);
    end;
    FS.WriteBuffer(MS.Memory^, MS.Size);
    FS.Free;
    for I := Low(LFiles) to High(LFiles) do
      for J := Low(LFiles[I].CodecEntries) to High(LFiles[I].CodecEntries) do
        LFiles[I].CodecEntries[J].Free;
    for I := Low(LResources) to High(LResources) do
      if LResources[I].Used then
      begin
        S := ChangeFileExt(LFileOutput,
          '_' + LResources[I].Ident.ToHexString.ToLower);
        if not FileExists(S) then
          LResources[I].Data.SaveToFile(S);
        LResources[I].Data.Free;
      end;
  end;
  SetLength(Buffer, 0);
  MS.Free;
  Ini.Free;
  Initialised := False;
end;

procedure SetCompressionType(AName: PAnsiChar);
var
  B: Boolean;
  X: Integer;
begin
  LAlgo1 := String(AName);
  B := true;
  for X := Low(ExcludeList) to High(ExcludeList) do
    if MatchesMask(LAlgo1, ExcludeList[X]) then
    begin
      B := False;
      break;
    end;
  LAlgo1 := Ini.ReadString('Config', LAlgo1, LAlgo1);
  Accept1 := B and (LAlgo1 <> '');
  if not Accept1 then
    exit;
end;

procedure SetEncryptionType(AName: PAnsiChar; AKey: Pointer; ASize: PInteger);
var
  B: Boolean;
  X: Integer;
begin
  LAlgo2 := String(AName);
  LResMem.Position := 0;
  B := true;
  for X := Low(ExcludeList) to High(ExcludeList) do
    if MatchesMask(LAlgo2, ExcludeList[X]) then
    begin
      B := False;
      break;
    end;
  LAlgo2 := Ini.ReadString('Config', LAlgo2, LAlgo2);
  Accept2 := B and (LAlgo2 <> '') and (ASize^ > 0);
  if not Accept2 then
    exit;
  LResMem.WriteBuffer(AKey^, ASize^);
end;

procedure AddStream1_32(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInteger; AOldSize, ANewSize: PInteger);
var
  LPosition: Int64;
begin
  LPosition := APosition^;
  AddStream1_64(AFileName, AFileIndex, @LPosition, AOldSize, ANewSize);
end;

procedure AddStream1_64(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInt64; AOldSize, ANewSize: PInteger);
var
  I, J: Integer;
  LEntry: TEntryStruct;
  S: String;
  X: Integer;
begin
  if ((Accept1 = False) and (Accept2 = False)) or
    ((Accept1 = true) and ((Accept2 = False) and (LAlgo2 <> ''))) then
    exit;
  if AOldSize^ <= 0 then
    exit;
  S := ExtractFileName(String(AFileName));
  for X := Low(ExcludeList) to High(ExcludeList) do
    if MatchesMask(S, ExcludeList[X]) then
      exit;
  J := -1;
  if AFileIndex^ = I.MaxValue then
    J := LFileIndex
  else
    for I := High(LFiles) downto Low(LFiles) do
      if AFileIndex^ = LFiles[I].FileIndex then
      begin
        J := I;
        break;
      end;
  if J < 0 then
    exit;
  LEntry.Position := APosition^;
  LEntry.OldSize := AOldSize^;
  if Accept2 then
  begin
    I := GetIndex2(J);
    LEntry.NewSize := AOldSize^;
    LFiles[J].CodecEntries[I].Add(LEntry);
    LResources[LFiles[J].CodecResource[I]].Used := true;
  end
  else
  begin
    I := GetIndex1(J);
    LEntry.NewSize := ANewSize^;
    LFiles[J].CodecEntries[I].Add(LEntry);
  end;
end;

procedure AddStream2_32(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInteger; AOrigSize: PInteger);
var
  LPosition: Int64;
begin
  LPosition := APosition^;
  AddStream2_64(AFileName, AFileIndex, @LPosition, AOrigSize);
end;

procedure AddStream2_64(AFileName: PAnsiChar; AFileIndex: PInteger;
  APosition: PInt64; AOrigSize: PInteger);
var
  I, J: Integer;
  LEntry: TEntryStruct;
  S: String;
  X: Integer;
begin
  if not Accept2 then
    exit;
  if AOrigSize^ <= 0 then
    exit;
  S := ExtractFileName(String(AFileName));
  for X := Low(ExcludeList) to High(ExcludeList) do
    if MatchesMask(S, ExcludeList[X]) then
      exit;
  J := -1;
  if AFileIndex^ = I.MaxValue then
    J := LFileIndex
  else
    for I := High(LFiles) downto Low(LFiles) do
      if AFileIndex^ = LFiles[I].FileIndex then
      begin
        J := I;
        break;
      end;
  if J < 0 then
    exit;
  I := GetIndex2(J);
  LEntry.Position := APosition^;
  LEntry.OldSize := AOrigSize^;
  LEntry.NewSize := AOrigSize^;
  LFiles[J].CodecEntries[I].Add(LEntry);
  LResources[LFiles[J].CodecResource[I]].Used := true;
end;

procedure OpenFile(AType, AFileName: PAnsiChar; AFileIndex: PInteger);
var
  I: Integer;
  S: String;
  LFileInfo: PFileInfo;
begin
  if AFileIndex^ = (I.MaxValue - 2) then
  begin
    LFileIndex := -1;
    exit;
  end
  else if AFileIndex^ = (I.MaxValue - 1) then
    exit;
  // No idea what both FDDE2 and FDSE2 do exactly...
  case IndexText(String(AType), ['FDDE', 'FDSE']) of
    0:
      S := ChangeFileExt(LFileInput, '.' + String(AFileName));
    1:
      S := ExtractFilePath(LFileInput) + String(AFileName);
  else
    S := '';
  end;
  if S = '' then
  begin
    if AFileIndex^ = I.MaxValue then
      exit;
    for I := High(LFiles) downto Low(LFiles) do
      if AFileIndex^ = LFiles[I].FileIndex then
      begin
        LFileIndex := I;
        exit;
      end;
  end
  else
    for I := High(LFiles) downto Low(LFiles) do
      if S = LFiles[I].FileName then
      begin
        LFiles[I].FileIndex := AFileIndex^;
        LFileIndex := I;
        exit;
      end;
  New(LFileInfo);
  LFileInfo^.FileName := S;
  LFileInfo^.FileIndex := AFileIndex^;
  SetLength(LFileInfo^.CodecName, 0);
  SetLength(LFileInfo^.CodecResource, 0);
  SetLength(LFileInfo^.CodecEntries, 0);
  LFileIndex := Length(LFiles);
  Insert(LFileInfo^, LFiles, LFileIndex);
end;

initialization

Initialised := False;
EntryStructCmp := TEntryStructComparer.Create;

finalization

if Initialised then
  Deinitialise;

end.
