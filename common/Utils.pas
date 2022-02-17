unit Utils;

interface

uses
  Threading, SynCommons,
  WinAPI.Windows, WinAPI.PsAPI,
  System.SysUtils, System.Classes, System.SyncObjs, System.Math, System.Types,
  System.AnsiStrings, System.StrUtils, System.IniFiles, System.IOUtils,
  System.RTLConsts, System.TypInfo, System.Net.HttpClientComponent,
  System.Generics.Defaults, System.Generics.Collections;

procedure ShowMessage(Msg: string; Caption: string = '');
procedure WriteLine(S: String);
function GetModuleName: string;

type
  TInt8_BitCount = 1 .. 8;
  TInt8_BitIndex = 0 .. 7;
  TInt16_BitCount = 1 .. 16;
  TInt16_BitIndex = 0 .. 15;
  TInt32_BitCount = 1 .. 32;
  TInt32_BitIndex = 0 .. 31;
  TInt64_BitCount = 1 .. 64;
  TInt64_BitIndex = 0 .. 63;

function GetBits(Data: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount): Int64;
procedure SetBits(var Data: Int8; Value: Int8; Index: TInt8_BitIndex;
  Count: TInt8_BitCount); overload;
procedure SetBits(var Data: UInt8; Value: Int8; Index: TInt8_BitIndex;
  Count: TInt8_BitCount); overload;
procedure SetBits(var Data: Int16; Value: Int16; Index: TInt16_BitIndex;
  Count: TInt16_BitCount); overload;
procedure SetBits(var Data: UInt16; Value: Int16; Index: TInt16_BitIndex;
  Count: TInt16_BitCount); overload;
procedure SetBits(var Data: Int32; Value: Int32; Index: TInt32_BitIndex;
  Count: TInt32_BitCount); overload;
procedure SetBits(var Data: UInt32; Value: Int32; Index: TInt32_BitIndex;
  Count: TInt32_BitCount); overload;
procedure SetBits(var Data: Int64; Value: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount); overload;
procedure SetBits(var Data: UInt64; Value: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount); overload;

type
  TListEx<T> = class(TList<T>)
  private
    FIndex: Integer;
  public
    constructor Create(const AComparer: IComparer<T>); overload;
    procedure Delete(Index: Integer);
    function Get(var Value: T): Integer; overload;
    function Get(var Value: T; Index: Integer): Boolean; overload;
    property Index: Integer read FIndex write FIndex;
  end;

  TSOMethod = (MTF, Transpose, Count);

  TSOList = class(TObject)
  private type
    TSOInfo = record
      Value, Count: Integer;
    end;

    TSOInfoComparer = class(TComparer<TSOInfo>)
    public
      function Compare(const Left, Right: TSOInfo): Integer; override;
    end;
  private
    FComparer: TSOInfoComparer;
    FList: TList<TSOInfo>;
    FSOMethod: TSOMethod;
    FIndex: Integer;
    function GetCount: Integer;
  public
    constructor Create(AValues: TArray<Integer>;
      ASOMethod: TSOMethod = TSOMethod.MTF);
    destructor Destroy; override;
    procedure Update(AValues: TArray<Integer>; Add: Boolean = False);
    procedure Add(Value: Integer);
    function Get(var Value: Integer): Integer;
    property Index: Integer read FIndex write FIndex;
    property Count: Integer read GetCount;
    property Method: TSOMethod read FSOMethod write FSOMethod;
  end;

  TArrayStream = class(TStream)
  private type
    _Stream = ^IStream;

    IStream = record
      Instance: TStream;
      Position, Size, MaxSize: Int64;
    end;
  private const
    FMaxStreamSize = $FFFFFFFFFF;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  private
    FStreams: TArray<IStream>;
    FPosition, FSize: Int64;
    FIndex, FCount: Integer;
    procedure FSetPos(APosition: Int64);
    procedure FSetSize(ASize: Int64);
    procedure FUpdate1;
    procedure FUpdate2;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Clear;
    function Add(AStreamType: Pointer; MaxSize: Int64 = FMaxStreamSize)
      : Integer;
    procedure Update(Index: Integer; MaxSize: Int64);
  end;

  TPointersStream = class(TStream)
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  private
    FPointers: TArray<Pointer>;
    FSizes: TArray<NativeInt>;
    FPosition, FSize: NativeInt;
    FMaxSize: NativeInt;
    FIndex, FCount: Integer;
    FIndexPos: NativeInt;
    procedure Recalculate;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Add(Ptr: Pointer; Size: NativeInt): Integer;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Ptr: Pointer; Size: NativeInt);
    procedure Clear;
    property Count: Integer read FCount;
    property Capacity: NativeInt read FMaxSize;
  end;

  TMemoryStreamEx = class(TMemoryStream)
  private
    FOwnMemory: Boolean;
    FMemory: Pointer;
    FMaxSize: NativeInt;
    FSize, FPosition: NativeInt;
  public
    constructor Create(AOwnMemory: Boolean = True; const AMemory: Pointer = nil;
      AMaxSize: NativeInt = 0); overload;
    destructor Destroy; override;
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Update(const AMemory: Pointer = nil; AMaxSize: NativeInt = 0);
  end;

  TSharedMemoryStream = class(TMemoryStreamEx)
  private const
    FIncSize = 64 * 1024 * 1024;
  private
    FStream: TFileStream;
    FMapHandle: THandle;
    FMapName: String;
    FMapBuffer: Pointer;
    function CalcSize(ASize: NativeInt): NativeInt;
    procedure IncMemory(ANewSize: NativeInt = 0);
  public
    constructor Create(const AMapName: String; AFileName: string); overload;
    constructor Create(const AMapName: String; ASize: NativeInt); overload;
    destructor Destroy; override;
    procedure SetSize(const NewSize: Int64); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TDownloadStream = class(TStream)
  private const
    FChunkSize = 65536;
  private
    FUrl: string;
    FNetHTTPClient: TNetHTTPClient;
    FMemoryStream: TMemoryStream;
    FSize, FPosition: Int64;
    procedure NetHTTPClientReceiveData(const Sender: TObject;
      AContentLength, AReadCount: Int64; var Abort: Boolean);
  public
    constructor Create(Url: string);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TBufferedStream = class(TStream)
  private
    FStream: TStream;
    FReadMode: Boolean;
    FMemory: PByte;
    FBufferSize: Integer;
    FBufPos, FBufSize: Integer;
  public
    constructor Create(Stream: TStream; ReadMode: Boolean;
      BufferSize: Integer = 65536);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  { TGPUMemoryStream = class(TStream)
    private
    FMemory: Pointer;
    FSize, FPosition: NativeInt;
    protected
    procedure SetPointer(Ptr: Pointer; const Size: NativeInt);
    public
    function Read(var Buffer; Count: longint): longint; override;
    function Read(Buffer: TBytes; Offset, Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
    end; }

  TDataStore = class(TObject)
  public
    function Slot(Index: Integer): TMemoryStream; virtual; abstract;
    function Position(Index: Integer): Int64; virtual; abstract;
    function Size(Index: Integer): NativeInt; virtual; abstract;
    function ActualSize(Index: Integer): NativeInt; virtual; abstract;
    function Slots: NativeInt; virtual; abstract;
    function Done: Boolean; virtual; abstract;
  end;

  TDataStore1 = class(TDataStore)
  private const
    FBufferSize = 65536;
  private
    FInput: TStream;
    FBuffer: array [0 .. FBufferSize - 1] of Byte;
    FDynamic: Boolean;
    FIndex: Integer;
    FSlots, FSize: NativeInt;
    FMemPtr: Pointer;
    FMemStm: TMemoryStreamEx;
    FMemData: TArray<TMemoryStreamEx>;
    FPositions: TArray<Int64>;
    FDone, FFirstRead, FLastRead: Boolean;
  public
    constructor Create(AInput: TStream; ADynamic: Boolean;
      ASlots, ASize: NativeInt);
    destructor Destroy; override;
    procedure ChangeInput(AInput: TStream);
    function Slot(Index: Integer): TMemoryStream; override;
    function Position(Index: Integer): Int64; override;
    function Size(Index: Integer): NativeInt; override;
    function ActualSize(Index: Integer): NativeInt; override;
    function Slots: NativeInt; override;
    function Done: Boolean; override;
    procedure Load;
    procedure LoadEx;
  end;

  TDataStore2 = class(TDataStore)
  private
    FSlots: NativeInt;
    FMemData: TArray<TMemoryStream>;
    FPositions, FSizes: TArray<Int64>;
  public
    constructor Create(ASlots: NativeInt);
    destructor Destroy; override;
    function Slot(Index: Integer): TMemoryStream; override;
    function Position(Index: Integer): Int64; override;
    function Size(Index: Integer): NativeInt; override;
    function ActualSize(Index: Integer): NativeInt; override;
    function Slots: NativeInt; override;
    function Done: Boolean; override;
    procedure Load(Index: Integer; Memory: Pointer; Size: Integer);
    procedure Reset(Index: Integer);
  end;

  TDataManager = class(TObject)
  private type
    PBlockInfo = ^TBlockInfo;

    TBlockInfo = record
      Position, CurrSize, FullSize: Int64;
      Count: Integer;
    end;

    TBlockInfoDynArray = TArray<TBlockInfo>;
  private
    FSync: TSynLocker;
    FDictionary: TSynDictionary;
    FStream: TStream;
    FStreamPos: Int64;
  public
    constructor Create(AStream: TStream; ACapacity: Integer = 0);
    destructor Destroy; override;
    procedure Add(ID: Integer; Size: Int64; Count: Integer = Integer.MaxValue);
    procedure Write(ID: Integer; const Buffer; Size: Integer);
    procedure CopyData(ID: Integer; Stream: TStream); overload;
    function CopyData(ID: Integer): Pointer; overload;
  end;

  TArgParser = class(TObject)
  private
    FArgs: TStringDynArray;
  public
    constructor Create(Arguments: TStringDynArray);
    destructor Destroy; override;
    procedure Add(Arguments: String);
    function AsString(Parameter: String; Index: Integer = 0;
      Default: String = ''): String;
    function AsInteger(Parameter: String; Index: Integer = 0;
      Default: Integer = 0): Integer;
    function AsFloat(Parameter: String; Index: Integer = 0;
      Default: Single = 0.00): Single;
    function AsBoolean(Parameter: String; Index: Integer = 0;
      Default: Boolean = False): Boolean;
  end;

  TDynamicEntropy = class(TObject)
  private
    FFirstBytes: TBytes;
    FFirstBytesPos: Integer;
    FEntropy: Single;
    FIndex, FRange: Integer;
    F1: array [0 .. 255] of Integer;
    F2: array of Byte;
    F3: array of Single;
  public
    constructor Create(ARange: Integer);
    destructor Destroy; override;
    procedure Reset;
    function Value: Single;
    procedure AddByte(AByte: Byte);
    procedure AddData(AData: Pointer; Size: Integer);
    property Range: Integer read FRange;
  end;

  PExecOutput = ^TExecOutput;
  TExecOutput = reference to procedure(const Buffer: Pointer; Size: Integer);

function Hash32(CRC: longword; buf: PByte; len: cardinal): longword;

procedure XORBuffer(InBuff: PByte; InSize: Integer; KeyBuff: PByte;
  KeySize: Integer);

function GenerateGUID: string;

function CalculateEntropy(Buffer: Pointer; BufferSize: Integer): Single;

function CopyStream(AStream1, AStream2: TStream; ASize: Int64 = Int64.MaxValue;
  ACallback: TProc<Int64> = nil): Int64;
procedure CopyStreamEx(AStream1, AStream2: TStream; ASize: Int64;
  ACallback: TProc<Int64> = nil);

function EndianSwap(A: Single): Single; overload;
function EndianSwap(A: double): double; overload;
function EndianSwap(A: Int64): Int64; overload;
function EndianSwap(A: UInt64): UInt64; overload;
function EndianSwap(A: Int32): Int32; overload;
function EndianSwap(A: UInt32): UInt32; overload;
function EndianSwap(A: Int16): Int16; overload;
function EndianSwap(A: UInt16): UInt16; overload;

function BinarySearch(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; var ResultPos: NativeInt): Boolean;
function BinarySearch2(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; var ResultPos: NativeInt): Boolean;
procedure ReverseBytes(Source, Dest: Pointer; Size: NativeInt);

function CloseValues(Value, Min, Max: Integer): TArray<Integer>;

function CompareSize(Original, New, Current: Int64): Boolean;

function GetIniString(Section, Key, Default, FileName: string): string;
  overload;
function GetIniString(Section, Key, Default: string; Ini: TMemIniFile)
  : string; overload;
procedure SetIniString(Section, Key, Value, FileName: string); overload;
procedure SetIniString(Section, Key, Value: string; Ini: TMemIniFile); overload;
function DecodeStr(str, Dec: string; Count: Integer = Integer.MaxValue - 1)
  : TStringDynArray;
function AnsiDecodeStr(str, Dec: Ansistring): TArray<Ansistring>;
function GetStr(Input: Pointer; MaxLength: Integer; var outStr: string)
  : Integer;
function IndexTextA(AText: PAnsiChar;
  const AValues: array of PAnsiChar): Integer;
function IndexTextW(AText: PWideChar;
  const AValues: array of PWideChar): Integer;

procedure Relocate(AMemory: PByte; ASize: NativeInt; AFrom, ATo: NativeInt);

function ConvertToBytes(S: string): Int64;
function ConvertToThreads(S: string): Integer;
function ConvertKB2TB(Value: Int64): string;

function BoolArray(const Bool: TArray<Boolean>; Value: Boolean): Boolean;

function GetUsedProcessMemory(hProcess: THandle): Int64;
function GetFreeSystemMemory: Int64;
function GetUsedSystemMemory: Int64;
function GetTotalSystemMemory: Int64;

function FileSize(const AFileName: string): Int64;
function GetFileList(const APath: TArray<string>; SubDir: Boolean = True)
  : TArray<string>;
procedure FileReadBuffer(Handle: THandle; var Buffer; Count: NativeInt);
procedure FileWriteBuffer(Handle: THandle; const Buffer; Count: NativeInt);

function Exec(Executable, CommandLine, WorkDir: string): Boolean;
function ExecStdin(Executable, CommandLine, WorkDir: string; InBuff: Pointer;
  InSize: Integer): Boolean;
function ExecStdout(Executable, CommandLine, WorkDir: string;
  Output: TExecOutput): Boolean;
function ExecStdio(Executable, CommandLine, WorkDir: string; InBuff: Pointer;
  InSize: Integer; Output: TExecOutput): Boolean;
function ExecStdioSync(Executable, CommandLine, WorkDir: string;
  InBuff: Pointer; InSize: Integer; Output: TExecOutput): Boolean;
function GetCmdCount(CommandLine: String): Integer;
function GetCmdStr(CommandLine: String; Index: Integer): string;

implementation

function GetBits(Data: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount): Int64;
begin
  Result := (Data shr Index) and ((1 shl Count) - 1);
end;

procedure SetBits(var Data: Int8; Value: Int8; Index: TInt8_BitIndex;
  Count: TInt8_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: UInt8; Value: Int8; Index: TInt8_BitIndex;
  Count: TInt8_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: Int16; Value: Int16; Index: TInt16_BitIndex;
  Count: TInt16_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: UInt16; Value: Int16; Index: TInt16_BitIndex;
  Count: TInt16_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: Int32; Value: Int32; Index: TInt32_BitIndex;
  Count: TInt32_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: UInt32; Value: Int32; Index: TInt32_BitIndex;
  Count: TInt32_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: Int64; Value: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure SetBits(var Data: UInt64; Value: Int64; Index: TInt64_BitIndex;
  Count: TInt64_BitCount);
begin
  Data := (Data and (not(((1 shl Count) - 1) shl Index))) or (Value shl Index);
end;

procedure ShowMessage(Msg: string; Caption: string = '');
begin
  MessageBox(0, PChar(Msg), PChar(Caption), MB_OK or MB_TASKMODAL);
end;

procedure WriteLine(S: String);
var
  ulLength: cardinal;
begin
  WriteConsole(GetStdHandle(STD_ERROR_HANDLE), PChar(S + #13#10),
    Length(S + #13#10), ulLength, nil);
end;

function GetModuleName: string;
var
  szFileName: array [0 .. MAX_PATH] of char;
begin
  FillChar(szFileName, sizeof(szFileName), #0);
  GetModuleFileName(hInstance, szFileName, MAX_PATH);
  Result := szFileName;
end;

constructor TListEx<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create(AComparer);
end;

procedure TListEx<T>.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if (Index < FIndex) then
    Dec(FIndex);
end;

function TListEx<T>.Get(var Value: T): Integer;
begin
  Result := -1;
  if (InRange(FIndex, 0, Pred(Count)) = False) or (Count <= 0) then
    exit;
  Value := Self[FIndex];
  Result := FIndex;
  Inc(FIndex);
end;

function TListEx<T>.Get(var Value: T; Index: Integer): Boolean;
begin
  Result := False;
  if (InRange(Index, 0, Pred(Count)) = False) or (Count <= 0) then
    exit;
  Value := Self[Index];
  Result := True;
end;

constructor TSOList.Create(AValues: TArray<Integer>; ASOMethod: TSOMethod);
var
  I: Integer;
  FInfo: TSOInfo;
begin
  inherited Create;
  FComparer := TSOInfoComparer.Create;
  FList := TList<TSOInfo>.Create(FComparer);
  FList.Count := Length(AValues);
  for I := 0 to FList.Count - 1 do
  begin
    FInfo.Value := AValues[Low(AValues) + I];
    FInfo.Count := 0;
    FList[I] := FInfo;
  end;
  FSOMethod := ASOMethod;
  FIndex := 0;
end;

destructor TSOList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TSOList.TSOInfoComparer.Compare(const Left, Right: TSOInfo): Integer;
begin
  Result := Right.Count - Left.Count;
end;

procedure TSOList.Update(AValues: TArray<Integer>; Add: Boolean);
var
  I: Integer;
  FInfo: TSOInfo;
begin
  if not Add then
    FList.Count := Length(AValues);
  for I := Low(AValues) to High(AValues) do
  begin
    FInfo.Value := AValues[I];
    FInfo.Count := 0;
    if Add then
      FList.Add(FInfo)
    else
      FList[I] := FInfo;
  end;
  FIndex := 0;
end;

function TSOList.Get(var Value: Integer): Integer;
begin
  Result := -1;
  if (InRange(FIndex, 0, Pred(Count)) = False) or (Count <= 0) then
    exit;
  try
    Value := FList[FIndex].Value;
    Result := FIndex;
    Inc(FIndex);
  except
  end;
end;

procedure TSOList.Add(Value: Integer);
var
  I: Integer;
  FInfo: TSOInfo;
begin
  case FSOMethod of
    TSOMethod.MTF:
      for I := 0 to FList.Count - 1 do
        if FList[I].Value = Value then
        begin
          FList.Move(I, 0);
          break;
        end;
    TSOMethod.Transpose:
      for I := 1 to FList.Count - 1 do
        if FList[I].Value = Value then
        begin
          FList.Move(I, I - 1);
          break;
        end;
    TSOMethod.Count:
      for I := 0 to FList.Count - 1 do
        if FList[I].Value = Value then
        begin
          FInfo := FList[I];
          Inc(FInfo.Count);
          FList[I] := FInfo;
          FList.Sort;
          break;
        end;
  end;
end;

function TSOList.GetCount: Integer;
begin
  Result := FList.Count;
end;

constructor TArrayStream.Create;
begin
  inherited Create;
  Clear;
end;

destructor TArrayStream.Destroy;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FStreams[I].Instance.Free;
  Clear;
  inherited Destroy;
end;

procedure TArrayStream.FSetPos(APosition: Int64);
var
  I: Integer;
  LPosition, LSize: Int64;
  IdxSet: Boolean;
begin
  FIndex := 0;
  LSize := 0;
  IdxSet := False;
  for I := 0 to FCount - 1 do
  begin
    if APosition > LSize + FStreams[I].Size then
      LPosition := FStreams[I].Size
    else
    begin
      LPosition := Max(0, APosition - LSize);
      if not IdxSet then
      begin
        FIndex := I;
        IdxSet := True;
      end;
    end;
    FStreams[I].Instance.Position := LPosition;
    FStreams[I].Position := LPosition;
    if IdxSet then
      break;
    Inc(LSize, FStreams[I].Size);
  end;
  FPosition := APosition;
end;

procedure TArrayStream.FSetSize(ASize: Int64);
var
  I: Integer;
  LSize1, LSize2: Int64;
begin
  LSize2 := 0;
  for I := 0 to FCount - 1 do
  begin
    if ASize > FStreams[I].MaxSize - LSize2 then
      LSize1 := FStreams[I].MaxSize
    else
      LSize1 := Max(0, ASize - LSize2);
    FStreams[I].Instance.Size := LSize1;
    FStreams[I].Size := LSize1;
    FStreams[I].Position := Min(FStreams[I].Position, FStreams[I].Size);
    Inc(LSize2, FStreams[I].Size);
  end;
  FSize := ASize;
end;

procedure TArrayStream.FUpdate1;
begin
  if FStreams[FIndex].Position = FStreams[FIndex].Size then
  begin
    while Succ(FIndex) < FCount do
    begin
      Inc(FIndex);
      FStreams[FIndex].Instance.Position := 0;
      FStreams[FIndex].Position := 0;
      if FStreams[FIndex].Position < FStreams[FIndex].Size then
        break;
    end;
  end;
end;

procedure TArrayStream.FUpdate2;
begin
  if FStreams[FIndex].Position = FStreams[FIndex].MaxSize then
  begin
    while Succ(FIndex) < FCount do
    begin
      Inc(FIndex);
      FStreams[FIndex].Instance.Position := 0;
      FStreams[FIndex].Position := 0;
      if FStreams[FIndex].Position < FStreams[FIndex].MaxSize then
        break;
    end;
  end;
end;

function TArrayStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TArrayStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TArrayStream.SetSize(const NewSize: Int64);
begin
  FSetSize(NewSize);
  FSize := NewSize;
  if FPosition > NewSize then
    Seek(0, soEnd);
end;

function TArrayStream.Read(var Buffer; Count: Longint): Longint;
var
  LCount: Int64;
begin
  Result := 0;
  if FCount = 0 then
    exit;
  FUpdate1;
  LCount := Min(FStreams[FIndex].Size - FStreams[FIndex].Position, Count);
  Result := FStreams[FIndex].Instance.Read(Buffer, LCount);
  Inc(FStreams[FIndex].Position, Result);
  Inc(FPosition, Result);
end;

function TArrayStream.Write(const Buffer; Count: Longint): Longint;
var
  LCount: Int64;
begin
  Result := 0;
  if FCount = 0 then
    exit;
  FUpdate2;
  LCount := Min(FStreams[FIndex].MaxSize - FStreams[FIndex].Position, Count);
  Result := FStreams[FIndex].Instance.Write(Buffer, LCount);
  Inc(FStreams[FIndex].Position, Result);
  Inc(FPosition, Result);
  FStreams[FIndex].Size := Max(FStreams[FIndex].Position,
    FStreams[FIndex].Size);
  FSize := Max(FPosition, FSize);
end;

function TArrayStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      Inc(FPosition, Offset);
    soEnd:
      FPosition := FSize + Offset;
  end;
  FSetPos(FPosition);
  Result := FPosition;
end;

procedure TArrayStream.Clear;
begin
  SetLength(FStreams, 0);
  FPosition := 0;
  FSize := 0;
  FIndex := 0;
  FCount := 0;
end;

function TArrayStream.Add(AStreamType: Pointer; MaxSize: Int64): Integer;
var
  LTypeData: PTypeData;
begin
  Result := FCount;
  Inc(FCount);
  SetLength(FStreams, FCount);
  LTypeData := GetTypeData(AStreamType);
  FStreams[Pred(FCount)].Instance := TStream(LTypeData^.ClassType.Create);
  FStreams[Pred(FCount)].Instance.Position := 0;
  FStreams[Pred(FCount)].Instance.Size := 0;
  FStreams[Pred(FCount)].Position := 0;
  FStreams[Pred(FCount)].Size := 0;
  FStreams[Pred(FCount)].MaxSize := EnsureRange(MaxSize, 0, FMaxStreamSize);
end;

procedure TArrayStream.Update(Index: Integer; MaxSize: Int64);
begin
  if FStreams[Index].Size < FStreams[Index].MaxSize then
    FStreams[Index].MaxSize := MaxSize;
end;

constructor TPointersStream.Create;
begin
  inherited Create;
  Clear;
  Recalculate;
end;

destructor TPointersStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TPointersStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TPointersStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TPointersStream.SetSize(const NewSize: Int64);
var
  OldPosition: NativeInt;
begin
  OldPosition := FPosition;
  if NewSize <= FMaxSize then
    FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soEnd);
end;

procedure TPointersStream.Recalculate;
var
  I: Integer;
begin
  FMaxSize := 0;
  for I := 0 to FCount - 1 do
    Inc(FMaxSize, FSizes[I]);
  if FPosition > FMaxSize then
    FPosition := FMaxSize;
  if FSize > FMaxSize then
    FSize := FMaxSize;
end;

function TPointersStream.Read(var Buffer; Count: Longint): Longint;
begin
  // 2121212
end;

function TPointersStream.Write(const Buffer; Count: Longint): Longint;
begin

end;

function TPointersStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin

end;

function TPointersStream.Add(Ptr: Pointer; Size: NativeInt): Integer;
begin
  System.Insert(Ptr, FPointers, FCount);
  System.Insert(Size, FSizes, FCount);
  Result := FCount;
  Inc(FCount);
  Recalculate;
end;

procedure TPointersStream.Delete(Index: Integer);
begin
  System.Delete(FPointers, Index, 1);
  Dec(FCount);
  Recalculate;
end;

procedure TPointersStream.Insert(Index: Integer; Ptr: Pointer; Size: NativeInt);
begin
  System.Insert(Ptr, FPointers, Index);
  System.Insert(Size, FSizes, Index);
  Inc(FCount);
  Recalculate;
end;

procedure TPointersStream.Clear;
begin
  SetLength(FPointers, 0);
  SetLength(FSizes, 0);
  FCount := 0;
  FPosition := 0;
  FSize := 0;
  FMaxSize := 0;
end;

constructor TMemoryStreamEx.Create(AOwnMemory: Boolean; const AMemory: Pointer;
  AMaxSize: NativeInt);
begin
  inherited Create;
  FOwnMemory := AOwnMemory;
  FMemory := AMemory;
  SetPointer(FMemory, 0);
  FMaxSize := AMaxSize;
  FPosition := 0;
  FSize := 0;
end;

destructor TMemoryStreamEx.Destroy;
begin
  SetPointer(nil, 0);
  if FOwnMemory then
    FreeMemory(FMemory);
  inherited Destroy;
end;

procedure TMemoryStreamEx.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TMemoryStreamEx.SetSize(const NewSize: Int64);
var
  OldPosition: NativeInt;
begin
  OldPosition := FPosition;
  if NewSize <= FMaxSize then
    FSize := NewSize;
  if OldPosition > NewSize then
    Seek(0, soEnd);
end;

function TMemoryStreamEx.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize - FPosition > 0 then
    begin
      if FSize > Count + FPosition then
        Result := Count
      else
        Result := FSize - FPosition;
      Move((PByte(Memory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

function TMemoryStreamEx.Write(const Buffer; Count: Longint): Longint;
var
  FCount: Longint;
begin
  Result := 0;
  FCount := Count;
  if FOwnMemory and (FPosition + FCount > FMaxSize) then
  begin
    if FMaxSize = 0 then
    begin
      FMemory := GetMemory(Count);
      FMaxSize := Count;
    end
    else
    begin
      FMemory := ReallocMemory(FMemory, FPosition + FCount);
      FMaxSize := FPosition + FCount;
    end;
    SetPointer(FMemory, FMaxSize);
  end;
  if FPosition + FCount > FMaxSize then
    FCount := FMaxSize - FPosition;
  if (FPosition >= 0) and (FCount >= 0) then
  begin
    System.Move(Buffer, (PByte(Memory) + FPosition)^, FCount);
    Inc(FPosition, FCount);
    if FPosition > FSize then
      FSize := FPosition;
    Result := FCount;
  end;
end;

function TMemoryStreamEx.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      Inc(FPosition, Offset);
    soEnd:
      FPosition := FSize + Offset;
  end;
  Result := Min(FPosition, FMaxSize);
end;

procedure TMemoryStreamEx.Update(const AMemory: Pointer; AMaxSize: NativeInt);
var
  LSize: NativeInt;
begin
  if FOwnMemory then
    FreeMemory(FMemory);
  LSize := Min(FSize, AMaxSize);
  FMemory := AMemory;
  SetPointer(AMemory, LSize);
  FMaxSize := AMaxSize;
  SetSize(LSize);
end;

constructor TSharedMemoryStream.Create(const AMapName: String;
  AFileName: string);

  function FSMode(OpenAndUse: Boolean): Word;
  begin
    if OpenAndUse then
      Result := fmOpenReadWrite or fmShareDenyNone
    else
      Result := fmCreate;
  end;

var
  LSize: Int64;
begin
  inherited Create(False);
  FStream := nil;
  FMapHandle := 0;
  FMapBuffer := nil;
  FStream := TFileStream.Create(AFileName, FSMode(FileExists(AFileName)));
  FMapName := AMapName;
  LSize := FStream.Size;
  if LSize = 0 then
  begin
    LSize := FIncSize;
    FStream.Size := FIncSize;
  end;
  FMapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(FMapName));
  if FMapHandle = 0 then
    FMapHandle := CreateFileMapping(FStream.Handle, nil, PAGE_READWRITE,
      Int64Rec(LSize).Hi, Int64Rec(LSize).Lo, PChar(FMapName));
  if FMapHandle = 0 then
    raise EFOpenError.CreateResFmt(@SFCreateErrorEx,
      [FMapName, SysErrorMessage(GetLastError)]);
  FMapBuffer := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  Update(FMapBuffer, LSize);
end;

constructor TSharedMemoryStream.Create(const AMapName: String;
  ASize: NativeInt);
var
  LSize: Int64;
begin
  inherited Create(False);
  FStream := nil;
  FMapHandle := 0;
  FMapBuffer := nil;
  FMapName := AMapName;
  LSize := ASize;
  FMapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(FMapName));
  if FMapHandle = 0 then
    FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      Int64Rec(LSize).Hi, Int64Rec(LSize).Lo, PChar(FMapName));
  if FMapHandle = 0 then
    raise EFOpenError.CreateResFmt(@SFCreateErrorEx,
      [FMapName, SysErrorMessage(GetLastError)]);
  FMapBuffer := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  Update(FMapBuffer, LSize);
end;

destructor TSharedMemoryStream.Destroy;
begin
  UnMapViewOfFile(FMapBuffer);
  CloseHandle(FMapHandle);
  if Assigned(FStream) then
  begin
    FStream.Size := FSize;
    FStream.Free;
  end;
  inherited Destroy;
end;

function TSharedMemoryStream.CalcSize(ASize: NativeInt): NativeInt;
begin
  Result := IfThen(ASize mod FIncSize = 0, FIncSize * (ASize div FIncSize),
    FIncSize + FIncSize * (ASize div FIncSize));
end;

procedure TSharedMemoryStream.IncMemory(ANewSize: NativeInt);
var
  LSize: Int64;
begin
  if not Assigned(FStream) then
    exit;
  if Assigned(FMapBuffer) then
  begin
    UnMapViewOfFile(FMapBuffer);
    CloseHandle(FMapHandle);
  end;
  FMaxSize := CalcSize(ANewSize);
  LSize := FMaxSize;
  FStream.Size := FMaxSize;
  FMapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(FMapName));
  if FMapHandle = 0 then
    FMapHandle := CreateFileMapping(FStream.Handle, nil, PAGE_READWRITE,
      Int64Rec(LSize).Hi, Int64Rec(LSize).Lo, PChar(FMapName));
  if FMapHandle = 0 then
    raise EFOpenError.CreateResFmt(@SFCreateErrorEx,
      [FMapName, SysErrorMessage(GetLastError)]);
  FMapBuffer := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  Update(FMapBuffer, LSize);
end;

procedure TSharedMemoryStream.SetSize(const NewSize: Int64);
begin
  if NewSize > FMaxSize then
    IncMemory(NewSize);
  inherited SetSize(NewSize);
end;

function TSharedMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FPosition + Count > FMaxSize then
    IncMemory(FPosition + Count);
  Result := inherited Write(Buffer, Count);
end;

constructor TDownloadStream.Create(Url: string);
begin
  inherited Create;
  FUrl := Url;
  FPosition := 0;
  FSize := 0;
  FNetHTTPClient := TNetHTTPClient.Create(nil);
  FNetHTTPClient.Asynchronous := False;
  FNetHTTPClient.OnReceiveData := NetHTTPClientReceiveData;
  FNetHTTPClient.Get(FUrl);
  FNetHTTPClient.OnReceiveData := nil;
  FMemoryStream := TMemoryStream.Create;
  FMemoryStream.Size := FChunkSize;
end;

destructor TDownloadStream.Destroy;
begin
  FMemoryStream.Free;
  FNetHTTPClient.Free;
  inherited Destroy;
end;

procedure TDownloadStream.NetHTTPClientReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  FSize := AContentLength;
  Abort := True;
end;

function TDownloadStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize - FPosition > 0 then
    begin
      if FSize > Count + FPosition then
        Result := Count
      else
        Result := FSize - FPosition;
      Result := Min(Result, FChunkSize);
      FMemoryStream.Position := 0;
      FNetHTTPClient.GetRange(FUrl, FPosition, FPosition + Result,
        FMemoryStream);
      Result := FMemoryStream.Position;
      Move(FMemoryStream.Memory^, Buffer, Result);
      Inc(FPosition, Result);
      exit;
    end;
  end;
  Result := 0;
end;

function TDownloadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      Inc(FPosition, Offset);
    soEnd:
      FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

constructor TBufferedStream.Create(Stream: TStream; ReadMode: Boolean;
  BufferSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FReadMode := ReadMode;
  GetMem(FMemory, BufferSize);
  FBufferSize := BufferSize;
  FBufPos := 0;
  if FReadMode then
    FBufSize := FStream.Read(FMemory^, FBufferSize)
  else
    FBufSize := 0;
end;

destructor TBufferedStream.Destroy;
begin
  if FReadMode = False then
  begin
    FStream.WriteBuffer(FMemory^, FBufSize);
    FBufSize := 0;
  end;
  FreeMem(FMemory);
  FStream.Free;
  inherited Destroy;
end;

function TBufferedStream.Read(var Buffer; Count: Integer): Integer;
var
  I, FCount: Integer;
  FSrc, FDest: PByte;
begin
  if FReadMode = False then
    raise EReadError.CreateRes(@SReadError);
  Result := 0;
  FCount := Count;
  if (Count <= 0) or (FBufSize = 0) then
    exit;
  while (FCount > 0) and (FBufSize - FBufPos > 0) do
  begin
    FSrc := FMemory + FBufPos;
    FDest := PByte(@Buffer) + (Count - FCount);
    if FCount > (FBufSize - FBufPos) then
      I := (FBufSize - FBufPos)
    else
      I := FCount;
    case I of
      sizeof(Byte):
        PByte(FDest)^ := PByte(FSrc)^;
      sizeof(Word):
        PWord(FDest)^ := PWord(FSrc)^;
      sizeof(cardinal):
        PCardinal(FDest)^ := PCardinal(FSrc)^;
      sizeof(UInt64):
        PUInt64(FDest)^ := PUInt64(FSrc)^;
    else
      Move(FSrc^, FDest^, I);
    end;
    Dec(FCount, I);
    Inc(FBufPos, I);
    if FBufPos = FBufSize then
    begin
      FBufPos := 0;
      FBufSize := FStream.Read(FMemory^, FBufferSize);
    end;
  end;
  Result := Count - FCount;
end;

function TBufferedStream.Write(const Buffer; Count: Integer): Integer;
var
  I, FCount: Integer;
  FSrc, FDest: PByte;
begin
  if FReadMode = True then
    raise EWriteError.CreateRes(@SWriteError);
  Result := 0;
  if Count <= 0 then
    exit;
  FCount := Count;
  while (FCount > 0) do
  begin
    FSrc := PByte(@Buffer) + (Count - FCount);
    FDest := FMemory + FBufSize;
    if (FBufSize = 0) and (FCount >= FBufferSize) then
    begin
      FStream.WriteBuffer(FSrc^, FBufferSize);
      Dec(FCount, FBufferSize);
    end
    else if (FBufSize = FBufferSize) then
    begin
      FStream.WriteBuffer(FMemory^, FBufSize);
      FBufSize := 0;
    end
    else
    begin
      if FCount > (FBufferSize - FBufSize) then
        I := (FBufferSize - FBufSize)
      else
        I := FCount;
      case I of
        sizeof(Byte):
          PByte(FDest)^ := PByte(FSrc)^;
        sizeof(Word):
          PWord(FDest)^ := PWord(FSrc)^;
        sizeof(cardinal):
          PCardinal(FDest)^ := PCardinal(FSrc)^;
        sizeof(UInt64):
          PUInt64(FDest)^ := PUInt64(FSrc)^;
      else
        Move(FSrc^, FDest^, I);
      end;
      Dec(FCount, I);
      Inc(FBufSize, I);
    end;
  end;
  Result := Count - FCount;
end;

constructor TDataStore1.Create(AInput: TStream; ADynamic: Boolean;
  ASlots, ASize: NativeInt);
var
  I: Integer;
begin
  inherited Create;
  FInput := AInput;
  FDynamic := ADynamic;
  FIndex := 0;
  FSlots := ASlots;
  FSize := ASize;
  if FDynamic then
  begin
    FMemPtr := GetMemory((FSlots + 1) * FSize);
    FMemStm := TMemoryStreamEx.Create(False, FMemPtr, (FSlots + 1) * FSize);
    FMemStm.Size := (FSlots + 1) * FSize;
  end
  else
  begin
    FMemPtr := GetMemory(FSlots * FSize);
    FMemStm := TMemoryStreamEx.Create(False, FMemPtr, FSlots * FSize);
    FMemStm.Size := FSlots * FSize;
  end;
  SetLength(FMemData, FSlots);
  SetLength(FPositions, FSlots);
  for I := Low(FMemData) to High(FMemData) do
  begin
    if FDynamic then
      FMemData[I] := TMemoryStreamEx.Create(False,
        (PByte(FMemStm.Memory) + (I * FSize)), FSize * 2)
    else
      FMemData[I] := TMemoryStreamEx.Create(False,
        (PByte(FMemStm.Memory) + (I * FSize)), FSize);
    FPositions[I] := (I * FSize) - (Length(FMemData) * FSize);
  end;
  FDone := False;
  FFirstRead := True;
  FLastRead := False;
end;

destructor TDataStore1.Destroy;
var
  I: Integer;
begin
  for I := Low(FMemData) to High(FMemData) do
    FMemData[I].Free;
  FMemStm.Free;
  FreeMemory(FMemPtr);
  inherited Destroy;
end;

procedure TDataStore1.ChangeInput(AInput: TStream);
var
  I: Integer;
begin
  FInput := AInput;
  FIndex := 0;
  if FDynamic then
    FMemStm.Size := (FSlots + 1) * FSize
  else
    FMemStm.Size := FSlots * FSize;
  for I := Low(FMemData) to High(FMemData) do
  begin
    FMemData[I].Position := 0;
    FMemData[I].Size := 0;
    FPositions[I] := (I * FSize) - (Length(FMemData) * FSize);
  end;
  FDone := False;
  FFirstRead := True;
  FLastRead := False;
end;

function TDataStore1.Slot(Index: Integer): TMemoryStream;
begin
  Result := FMemData[Index];
end;

function TDataStore1.Position(Index: Integer): Int64;
begin
  Result := FPositions[Index];
end;

function TDataStore1.Size(Index: Integer): NativeInt;
begin
  Result := Min(FSize, FMemData[Index].Size);
end;

function TDataStore1.ActualSize(Index: Integer): NativeInt;
begin
  Result := FMemData[Index].Size;
end;

function TDataStore1.Slots: NativeInt;
begin
  Result := FSlots;
end;

function TDataStore1.Done: Boolean;
begin
  Result := FDone;
end;

procedure TDataStore1.Load;
var
  I: Integer;
  W, X: Int64;
begin
  for I := Low(FMemData) to High(FMemData) do
    Inc(FPositions[I], Length(FMemData) * FSize);
  if FDynamic then
  begin
    if FFirstRead then
    begin
      FMemStm.Position := 0;
      FFirstRead := False;
    end
    else
    begin
      W := Min(FSize, Max(0, FMemStm.Position - (FSlots * FSize)));
      Move((PByte(FMemStm.Memory) + (FSlots * FSize))^, FMemStm.Memory^, W);
      FMemStm.Position := W;
    end;
    while FMemStm.Position < FMemStm.Size do
    begin
      X := FInput.Read(FBuffer[0], Min(FMemStm.Size - FMemStm.Position,
        FBufferSize));
      if X > 0 then
        FMemStm.WriteBuffer(FBuffer[0], X)
      else
      begin
        FLastRead := True;
        break;
      end;
    end;
    for I := Low(FMemData) to High(FMemData) do
      FMemData[I].Size := Min(FSize * 2,
        Max(0, FMemStm.Position - (I * FSize)));
  end
  else
  begin
    FMemStm.Position := 0;
    while FMemStm.Position < FMemStm.Size do
    begin
      X := FInput.Read(FBuffer[0], Min(FMemStm.Size - FMemStm.Position,
        FBufferSize));
      if X > 0 then
        FMemStm.WriteBuffer(FBuffer[0], X)
      else
      begin
        FDone := True;
        break;
      end;
    end;
    for I := Low(FMemData) to High(FMemData) do
      FMemData[I].Size := Min(FSize, Max(0, FMemStm.Position - (I * FSize)));
  end;
  FDone := FMemData[0].Size = 0;
end;

procedure TDataStore1.LoadEx;
var
  W, X: Int64;
begin
  Inc(FPositions[FIndex], Length(FMemData) * FSize);
  if FDynamic then
  begin
    if FIndex = 0 then
    begin
      W := Min(FSize, Max(0, FMemStm.Position - (FSlots * FSize)));
      Move((PByte(FMemStm.Memory) + (FSlots * FSize))^, FMemStm.Memory^, W);
      FMemStm.Position := W;
    end;
    W := FMemStm.Position + FSize;
    while FMemStm.Position < W do
    begin
      X := FInput.Read(FBuffer[0], Min(W - FMemStm.Position, FBufferSize));
      if X > 0 then
        FMemStm.WriteBuffer(FBuffer[0], X)
      else
      begin
        FLastRead := True;
        break;
      end;
    end;
    FMemData[FIndex].Size :=
      Min(FSize * 2, Max(0, FMemStm.Position - (FIndex * FSize)));
  end
  else
  begin
    FMemStm.Position := FIndex * FSize;
    W := FMemStm.Position + FSize;
    while FMemStm.Position < W do
    begin
      X := FInput.Read(FBuffer[0], Min(W - FMemStm.Position, FBufferSize));
      if X > 0 then
        FMemStm.WriteBuffer(FBuffer[0], X)
      else
      begin
        FDone := True;
        break;
      end;
    end;
    FMemData[FIndex].Size :=
      Min(FSize, Max(0, FMemStm.Position - (FIndex * FSize)));
  end;
  Inc(FIndex);
  if FIndex = FSlots then
    FIndex := 0;
  FDone := FMemData[0].Size = 0;
end;

constructor TDataStore2.Create(ASlots: NativeInt);
var
  I: Integer;
begin
  inherited Create;
  FSlots := ASlots;
  SetLength(FMemData, FSlots);
  SetLength(FPositions, FSlots);
  SetLength(FSizes, FSlots);
  for I := Low(FMemData) to High(FMemData) do
  begin
    FMemData[I] := TMemoryStream.Create;
    FPositions[I] := 0;
    FSizes[I] := 0;
  end;
end;

destructor TDataStore2.Destroy;
var
  I: Integer;
begin
  for I := Low(FMemData) to High(FMemData) do
    FMemData[I].Free;
  inherited Destroy;
end;

function TDataStore2.Slot(Index: Integer): TMemoryStream;
begin
  Result := FMemData[Index];
end;

function TDataStore2.Position(Index: Integer): Int64;
begin
  Result := FPositions[Index];
end;

function TDataStore2.Size(Index: Integer): NativeInt;
begin
  Result := FSizes[Index];
end;

function TDataStore2.ActualSize(Index: Integer): NativeInt;
begin
  Result := FSizes[Index];
end;

function TDataStore2.Slots: NativeInt;
begin
  Result := FSlots;
end;

function TDataStore2.Done: Boolean;
begin
  Result := False;
end;

procedure TDataStore2.Load(Index: Integer; Memory: Pointer; Size: Integer);
begin
  FMemData[Index].WriteBuffer(Memory^, Size);
  Inc(FSizes[Index], Size);
end;

procedure TDataStore2.Reset(Index: Integer);
begin
  FMemData[Index].Position := 0;
  FSizes[Index] := 0;
end;

constructor TDataManager.Create(AStream: TStream; ACapacity: Integer);
begin
  inherited Create;
  FSync.Init;
  FDictionary := TSynDictionary.Create(TypeInfo(TIntegerDynArray),
    TypeInfo(TBlockInfoDynArray));
  FDictionary.Capacity := ACapacity;
  FStream := AStream;
  FStreamPos := FStream.Position;
end;

destructor TDataManager.Destroy;
begin
  FDictionary.Free;
  FSync.Done;
  inherited Destroy;
end;

procedure TDataManager.Add(ID: Integer; Size: Int64; Count: Integer);
var
  LBlockInfo: TBlockInfo;
  _BlockInfo: PBlockInfo;
  I: Integer;
begin
  if Count <= 0 then
    exit;
  FSync.Lock;
  try
    LBlockInfo.Position := 0;
    for I := 0 to FDictionary.Count - 1 do
    begin
      _BlockInfo := PBlockInfo(FDictionary.Values.ElemPtr(I));
      LBlockInfo.Position := Max(LBlockInfo.Position, _BlockInfo^.Position +
        _BlockInfo^.FullSize);
    end;
    LBlockInfo.CurrSize := 0;
    LBlockInfo.FullSize := Size;
    LBlockInfo.Count := Count;
    FStream.Size := Max(FStream.Size, FStreamPos + LBlockInfo.Position +
      LBlockInfo.FullSize);
    FDictionary.Add(ID, LBlockInfo);
  finally
    FSync.UnLock;
  end;
  // once reads reaches 0, add list of all available spaces
end;

procedure TDataManager.Write(ID: Integer; const Buffer; Size: Integer);
var
  _BlockInfo: PBlockInfo;
begin
  if Size <= 0 then
    exit;
  FSync.Lock;
  try
    _BlockInfo := FDictionary.FindValue(ID);
    if (_BlockInfo^.Position + _BlockInfo^.CurrSize + Size) >
      (_BlockInfo^.Position + _BlockInfo^.FullSize) then
      raise EWriteError.CreateRes(@SWriteError);
    FStream.Position := FStreamPos + _BlockInfo^.Position +
      _BlockInfo^.CurrSize;
    FStream.WriteBuffer(Buffer, Size);
    Inc(_BlockInfo^.CurrSize, Size);
  finally
    FSync.UnLock;
  end;
end;

procedure TDataManager.CopyData(ID: Integer; Stream: TStream);
var
  _BlockInfo: PBlockInfo;
begin
  FSync.Lock;
  try
    _BlockInfo := FDictionary.FindValue(ID);
    if _BlockInfo^.CurrSize <> _BlockInfo^.FullSize then
      raise EReadError.CreateRes(@SReadError);
    FStream.Position := FStreamPos + _BlockInfo^.Position;
    CopyStreamEx(FStream, Stream, _BlockInfo^.FullSize);
    Dec(_BlockInfo^.Count);
  finally
    FSync.UnLock;
  end;
end;

function TDataManager.CopyData(ID: Integer): Pointer;
var
  _BlockInfo: PBlockInfo;
begin
  FSync.Lock;
  try
    _BlockInfo := FDictionary.FindValue(ID);
    if _BlockInfo^.CurrSize <> _BlockInfo^.FullSize then
      raise EReadError.CreateRes(@SReadError);
    FStream.Position := FStreamPos + _BlockInfo^.Position +
      _BlockInfo^.CurrSize;
    FStream.ReadBuffer(Result^, _BlockInfo^.FullSize);
    Dec(_BlockInfo^.Count);
  finally
    FSync.UnLock;
  end;
end;

constructor TArgParser.Create(Arguments: TStringDynArray);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FArgs, Length(Arguments));
  for I := Low(FArgs) to High(FArgs) do
    FArgs[I] := Arguments[I];
end;

destructor TArgParser.Destroy;
begin
  SetLength(FArgs, 0);
  inherited Destroy;
end;

procedure TArgParser.Add(Arguments: String);
var
  I: Integer;
  List: TStringDynArray;
begin
  if Arguments = '' then
    exit;
  List := DecodeStr(Arguments, ' ');
  for I := Low(List) to High(List) do
    Insert(List[I], FArgs, Length(FArgs));
end;

function TArgParser.AsString(Parameter: String; Index: Integer;
  Default: String): String;
var
  I, J: Integer;
begin
  Result := Default;
  J := 0;
  for I := Low(FArgs) to High(FArgs) do
    if FArgs[I].StartsWith(Parameter, True) then
    begin
      if J >= Index then
      begin
        Result := FArgs[I].Substring(Parameter.Length);
        break;
      end
      else
        Inc(J);
    end;
end;

function TArgParser.AsInteger(Parameter: String; Index: Integer;
  Default: Integer): Integer;
var
  I, J: Integer;
begin
  Result := Default;
  J := 0;
  for I := Low(FArgs) to High(FArgs) do
    if FArgs[I].StartsWith(Parameter, True) then
    begin
      if J >= Index then
      begin
        try
          Result := FArgs[I].Substring(Parameter.Length).ToInteger;
          break;
        except
        end;
      end
      else
        Inc(J);
    end;
end;

function TArgParser.AsFloat(Parameter: String; Index: Integer;
  Default: Single): Single;
var
  I, J: Integer;
begin
  Result := Default;
  J := 0;
  for I := Low(FArgs) to High(FArgs) do
    if FArgs[I].StartsWith(Parameter, True) then
    begin
      if J >= Index then
      begin
        try
          Result := FArgs[I].Substring(Parameter.Length).ToSingle;
          break;
        except
        end;
      end
      else
        Inc(J);
    end;
end;

function TArgParser.AsBoolean(Parameter: String; Index: Integer;
  Default: Boolean): Boolean;
var
  I, J: Integer;
begin
  Result := Default;
  J := 0;
  for I := Low(FArgs) to High(FArgs) do
    if FArgs[I].StartsWith(Parameter, True) then
    begin
      if J >= Index then
      begin
        if SameText(Parameter, FArgs[I]) then
        begin
          Result := True;
          break;
        end
        else
          try
            Result := FArgs[I].Substring(Parameter.Length).ToBoolean;
            break;
          except
          end;
      end
      else
        Inc(J);
    end;
end;

constructor TDynamicEntropy.Create(ARange: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FFirstBytes, ARange);
  FFirstBytesPos := 0;
  FEntropy := 0.00;
  FIndex := 0;
  FRange := ARange;
  FillChar(F1[0], sizeof(F1), 0);
  F1[0] := FRange;
  SetLength(F2, FRange);
  FillChar(F2[0], Length(F2), 0);
  SetLength(F3, FRange + 1);
  for I := Low(F3) to High(F3) do
  begin
    F3[I] := I / FRange;
    if I > 0 then
      F3[I] := (F3[I] * log2(F3[I]));
  end;
end;

destructor TDynamicEntropy.Destroy;
begin
  SetLength(FFirstBytes, 0);
  SetLength(F2, 0);
  SetLength(F3, 0);
  inherited Destroy;
end;

procedure TDynamicEntropy.Reset;
begin
  FFirstBytesPos := 0;
  FEntropy := 0.00;
  FIndex := 0;
  FillChar(F1[0], sizeof(F1), 0);
  F1[0] := FRange;
  FillChar(F2[0], Length(F2), 0);
end;

function TDynamicEntropy.Value: Single;
begin
  if FFirstBytesPos < FRange then
    Result := CalculateEntropy(@FFirstBytes[0], Succ(FFirstBytesPos))
  else
    Result := Abs(FEntropy);
end;

procedure TDynamicEntropy.AddByte(AByte: Byte);
begin
  if FFirstBytesPos < FRange then
  begin
    FFirstBytes[FFirstBytesPos] := AByte;
    Inc(FFirstBytesPos);
  end;
  if F2[FIndex] <> AByte then
  begin
    FEntropy := FEntropy - (F3[F1[F2[FIndex]]] - F3[Pred(F1[F2[FIndex]])]);
    Dec(F1[F2[FIndex]]);
    FEntropy := FEntropy + (F3[Succ(F1[AByte])] - F3[F1[AByte]]);
    Inc(F1[AByte]);
    F2[FIndex] := AByte;
  end;
  if Succ(FIndex) = FRange then
    FIndex := 0
  else
    Inc(FIndex);
end;

procedure TDynamicEntropy.AddData(AData: Pointer; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
    AddByte((PByte(AData) + I)^);
end;

function Hash32(CRC: longword; buf: PByte; len: cardinal): longword;
begin
  Result := crc32c(CRC, PAnsiChar(buf), len);
end;

procedure XORBuffer(InBuff: PByte; InSize: Integer; KeyBuff: PByte;
  KeySize: Integer);
var
  I: Integer;
begin
  Assert(Assigned(InBuff));
  Assert(Assigned(KeyBuff));
  for I := 0 to InSize - 1 do
  begin
    InBuff^ := InBuff^ xor KeyBuff^;
    Inc(InBuff);
    Inc(KeyBuff);
    if I mod KeySize = Pred(KeySize) then
      KeyBuff := KeyBuff - KeySize;
  end;
end;

function GenerateGUID: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;

function CalculateEntropy(Buffer: Pointer; BufferSize: Integer): Single;
var
  Entropy: Single;
  Entries: array [0 .. 255] of Integer;
  I: Integer;
  Temp: Single;
begin
  Entropy := 0.00;
  if BufferSize > 0 then
  begin
    FillChar(Entries[0], sizeof(Entries), 0);
    for I := 0 to (BufferSize - 1) do
      Inc(Entries[(PByte(Buffer) + I)^]);
    for I := Low(Entries) to High(Entries) do
    begin
      Temp := Entries[I] / BufferSize;
      if (Temp > 0) then
        Entropy := Entropy + Temp * log2(Temp);
    end;
  end;
  Result := Abs(Entropy);
end;

function CopyStream(AStream1, AStream2: TStream; ASize: Int64;
  ACallback: TProc<Int64>): Int64;
const
  FBufferSize = 65536;
var
  I: Integer;
  FSize: Int64;
  FBuff: array [0 .. FBufferSize - 1] of Byte;
begin
  Result := 0;
  if ASize <= 0 then
    exit;
  FSize := ASize;
  I := AStream1.Read(FBuff[0], Min(FBufferSize, FSize));
  while I > 0 do
  begin
    AStream2.WriteBuffer(FBuff[0], I);
    Dec(FSize, I);
    if Assigned(ACallback) then
      ACallback(ASize - FSize);
    Result := ASize - FSize;
    I := AStream1.Read(FBuff[0], Min(FBufferSize, FSize));
  end;
end;

procedure CopyStreamEx(AStream1, AStream2: TStream; ASize: Int64;
  ACallback: TProc<Int64>);
const
  FBufferSize = 65536;
var
  I: Integer;
  FSize: Int64;
  FBuff: array [0 .. FBufferSize - 1] of Byte;
begin
  if ASize <= 0 then
    exit;
  FSize := ASize;
  I := Min(FBufferSize, FSize);
  AStream1.ReadBuffer(FBuff[0], I);
  while I > 0 do
  begin
    AStream2.WriteBuffer(FBuff[0], I);
    Dec(FSize, I);
    if Assigned(ACallback) then
      ACallback(ASize - FSize);
    I := Min(FBufferSize, FSize);
    AStream1.ReadBuffer(FBuff[0], I);
  end;
end;

function EndianSwap(A: Single): Single;
var
  C: array [0 .. 3] of Byte absolute Result;
  d: array [0 .. 3] of Byte absolute A;
begin
  C[0] := d[3];
  C[1] := d[2];
  C[2] := d[1];
  C[3] := d[0];
end;

function EndianSwap(A: double): double;
var
  C: array [0 .. 7] of Byte absolute Result;
  d: array [0 .. 7] of Byte absolute A;
begin
  C[0] := d[7];
  C[1] := d[6];
  C[2] := d[5];
  C[3] := d[4];
  C[4] := d[3];
  C[5] := d[2];
  C[6] := d[1];
  C[7] := d[0];
end;

{$IFDEF PUREPASCAL}

function EndianSwap(A: Int64): Int64;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IFDEF win64}
  mov rax, rcx
  {$ELSE}
  mov rax, rdi
  {$ENDIF win64}
  bswap rax
  {$ELSE}
  mov edx, A.Int64Rec.Lo
  mov eax, A.Int64Rec.Hi
  bswap edx
  bswap eax
  {$ENDIF}
end;

function EndianSwap(A: UInt64): UInt64;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IFDEF win64}
  mov rax, rcx
  {$ELSE}
  mov rax, rdi
  {$ENDIF win64}
  bswap rax
  {$ELSE}
  mov edx, A.Int64Rec.Lo
  mov eax, A.Int64Rec.Hi
  bswap edx
  bswap eax
  {$ENDIF}
end;

function EndianSwap(A: Int32): Int32;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IF DEFINED(WIN64)}
  mov eax, ecx
  {$ELSE}
  mov eax, edi
  {$ENDIF}
  bswap eax
  {$ELSEIF DEFINED(CPUX86)}
  bswap eax
  {$ENDIF}
end;

function EndianSwap(A: UInt32): UInt32;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IF DEFINED(WIN64)}
  mov eax, ecx
  {$ELSE}
  mov eax, edi
  {$ENDIF}
  bswap eax
  {$ELSEIF DEFINED(CPUX86)}
  bswap eax
  {$ENDIF}
end;

function EndianSwap(A: Int16): Int16;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IF DEFINED(WIN64)}
  mov ax, cx
  {$ELSE}
  mov ax, di
  {$ENDIF}
  rol ax,8
  {$ELSEIF DEFINED(CPUX86)}
  rol ax,8
  {$ENDIF}
end;

function EndianSwap(A: UInt16): UInt16;
asm
  {$IF DEFINED(CPUX64)}
  .NOFRAME
  {$IF DEFINED(WIN64)}
  mov ax, cx
  {$ELSE}
  mov ax, di
  {$ENDIF}
  rol ax,8
  {$ELSEIF DEFINED(CPUX86)}
  rol ax,8
  {$ENDIF}
end;

{$ELSE}

function EndianSwap(A: Int64): Int64;
var
  C: array [0 .. 7] of Byte absolute Result;
  d: array [0 .. 7] of Byte absolute A;
begin
  C[0] := d[7];
  C[1] := d[6];
  C[2] := d[5];
  C[3] := d[4];
  C[4] := d[3];
  C[5] := d[2];
  C[6] := d[1];
  C[7] := d[0];
end;

function EndianSwap(A: UInt64): UInt64;
var
  C: array [0 .. 7] of Byte absolute Result;
  d: array [0 .. 7] of Byte absolute A;
begin
  C[0] := d[7];
  C[1] := d[6];
  C[2] := d[5];
  C[3] := d[4];
  C[4] := d[3];
  C[5] := d[2];
  C[6] := d[1];
  C[7] := d[0];
end;

function EndianSwap(A: Int32): Int32;
var
  C: array [0 .. 3] of Byte absolute Result;
  d: array [0 .. 3] of Byte absolute A;
begin
  C[0] := d[3];
  C[1] := d[2];
  C[2] := d[1];
  C[3] := d[0];
end;

function EndianSwap(A: UInt32): UInt32;
var
  C: array [0 .. 3] of Byte absolute Result;
  d: array [0 .. 3] of Byte absolute A;
begin
  C[0] := d[3];
  C[1] := d[2];
  C[2] := d[1];
  C[3] := d[0];
end;

function EndianSwap(A: Int16): Int16;
var
  C: array [0 .. 1] of Byte absolute Result;
  d: array [0 .. 1] of Byte absolute A;
begin
  C[0] := d[1];
  C[1] := d[0];
end;

function EndianSwap(A: UInt16): UInt16;
var
  C: array [0 .. 1] of Byte absolute Result;
  d: array [0 .. 1] of Byte absolute A;
begin
  C[0] := d[1];
  C[1] := d[0];
end;
{$ENDIF}

function BinarySearch(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; var ResultPos: NativeInt): Boolean;
var
  Pos: NativeInt;
begin
  Result := False;
  if (SearchSize <= 0) then
    exit;
  case SearchSize of
    sizeof(Byte):
      begin
        Pos := SrcPos;
        while Pos <= (SrcSize - SearchSize) do
        begin
          if PByte(PByte(SrcMem) + Pos)^ = PByte(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Inc(Pos);
        end;
      end;
    sizeof(Word):
      begin
        Pos := SrcPos;
        while Pos <= (SrcSize - SearchSize) do
        begin
          if PWord(PByte(SrcMem) + Pos)^ = PWord(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Inc(Pos);
        end;
      end;
    sizeof(cardinal):
      begin
        Pos := SrcPos;
        while Pos <= (SrcSize - SearchSize) do
        begin
          if PCardinal(PByte(SrcMem) + Pos)^ = PCardinal(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Inc(Pos);
        end;
      end;
    sizeof(UInt64):
      begin
        Pos := SrcPos;
        while Pos <= (SrcSize - SearchSize) do
        begin
          if PUInt64(PByte(SrcMem) + Pos)^ = PUInt64(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Inc(Pos);
        end;
      end;
  else
    Pos := SrcPos;
    while Pos <= (SrcSize - SearchSize) do
    begin
      if PWord(PByte(SrcMem) + Pos)^ = PWord(SearchMem)^ then
        if CompareMem(PByte(SrcMem) + Pos, SearchMem, SearchSize) then
        begin
          ResultPos := Pos;
          Result := True;
          break;
        end;
      Inc(Pos);
    end;
  end;
end;

function BinarySearch2(SrcMem: Pointer; SrcPos, SrcSize: NativeInt;
  SearchMem: Pointer; SearchSize: NativeInt; var ResultPos: NativeInt): Boolean;
var
  Pos: NativeInt;
begin
  Result := False;
  if (SearchSize <= 0) then
    exit;
  case SearchSize of
    sizeof(Byte):
      begin
        Pos := SrcPos - SearchSize;
        while Pos >= SrcPos do
        begin
          if PByte(PByte(SrcMem) + Pos)^ = PByte(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Dec(Pos);
        end;
      end;
    sizeof(Word):
      begin
        Pos := SrcPos - SearchSize;
        while Pos >= SrcPos do
        begin
          if PWord(PByte(SrcMem) + Pos)^ = PWord(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Dec(Pos);
        end;
      end;
    sizeof(cardinal):
      begin
        Pos := SrcPos - SearchSize;
        while Pos >= SrcPos do
        begin
          if PCardinal(PByte(SrcMem) + Pos)^ = PCardinal(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Dec(Pos);
        end;
      end;
    sizeof(UInt64):
      begin
        Pos := SrcPos - SearchSize;
        while Pos >= SrcPos do
        begin
          if PUInt64(PByte(SrcMem) + Pos)^ = PUInt64(SearchMem)^ then
          begin
            ResultPos := Pos;
            Result := True;
            break;
          end;
          Dec(Pos);
        end;
      end;
  else
    Pos := SrcPos - SearchSize;
    while Pos >= SrcPos do
    begin
      if PWord(PByte(SrcMem) + Pos)^ = PWord(SearchMem)^ then
        if CompareMem(PByte(SrcMem) + Pos, SearchMem, SearchSize) then
        begin
          ResultPos := Pos;
          Result := True;
          break;
        end;
      Dec(Pos);
    end;
  end;
end;

procedure ReverseBytes(Source, Dest: Pointer; Size: NativeInt);
begin
  Dest := PByte(NativeInt(Dest) + Size - 1);
  while (Size > 0) do
  begin
    PByte(Dest)^ := PByte(Source)^;
    Inc(PByte(Source));
    Dec(PByte(Dest));
    Dec(Size);
  end;
end;

function CloseValues(Value, Min, Max: Integer): TArray<Integer>;
var
  I, Init, Index: Integer;
  Up: Boolean;
begin
  SetLength(Result, Succ(Max - Min));
  if InRange(Value, Min, Max) then
    Init := Value
  else
    Init := Min + (Max - Min) div 2;
  Index := 0;
  for I := Low(Result) to High(Result) do
  begin
    Up := Odd(I);
    if Up then
      Up := Init + Index <= Max
    else
      Up := Init - Index < Min;
    if Up then
      Result[I] := Init + Index
    else
      Result[I] := Init - Index;
    if (Odd(I) = False) or (Init - Index < Min) or (Init + Index > Max) then
      Inc(Index);
  end;
end;

function CompareSize(Original, New, Current: Int64): Boolean;
begin
  Result := (Max(Original, New) - Min(Original, New)) <=
    (Max(Original, Current) - Min(Original, Current));
end;

function GetIniString(Section, Key, Default, FileName: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  with Ini do
    try
      Result := Ini.ReadString(Section, Key, Default);
    finally
      Free;
    end;
end;

function GetIniString(Section, Key, Default: string; Ini: TMemIniFile): string;
begin
  Result := Ini.ReadString(Section, Key, Default);
end;

procedure SetIniString(Section, Key, Value, FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  with Ini do
    try
      Ini.WriteString(Section, Key, Value);
    finally
      Free;
    end;
end;

procedure SetIniString(Section, Key, Value: string; Ini: TMemIniFile);
begin
  Ini.WriteString(Section, Key, Value);
end;

function DecodeStr(str, Dec: string; Count: Integer): TStringDynArray;
var
  tmp, S: string;
  I: Integer;
begin
  tmp := str;
  SetLength(Result, Succ(Min(Length(tmp) - Length(ReplaceText(tmp, Dec, '')
    ), Count)));
  for I := Low(Result) to High(Result) do
  begin
    if I = High(Result) then
      Result[I] := tmp
    else
    begin
      S := Copy(tmp, 1, Pos(Dec, tmp) - 1);
      Delete(tmp, 1, Pos(Dec, tmp));
      Result[I] := S;
    end;
  end;
end;

function AnsiDecodeStr(str, Dec: Ansistring): TArray<Ansistring>;
var
  tmp, S: Ansistring;
  I: Integer;
begin
  tmp := str + Dec;
  SetLength(Result, Length(tmp) - Length(AnsiReplaceText(tmp, Dec, '')));
  for I := Low(Result) to High(Result) do
  begin
    S := Copy(tmp, 1, AnsiPos(Dec, tmp) - 1);
    Delete(tmp, 1, AnsiPos(Dec, tmp));
    Result[I] := S;
  end;
end;

function GetStr(Input: Pointer; MaxLength: Integer; var outStr: string)
  : Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to MaxLength do
  begin
    if (PByte(Input) + I - 1)^ = 0 then
      break;
    Inc(Result);
  end;
  outStr := Copy(String(PAnsiChar(Input)), 0, Result);
end;

function IndexTextA(AText: PAnsiChar;
  const AValues: array of PAnsiChar): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if AnsiSameText(AText, AValues[I]) then
    begin
      Result := I;
      break;
    end;
end;

function IndexTextW(AText: PWideChar;
  const AValues: array of PWideChar): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AValues) to High(AValues) do
    if SameText(AText, AValues[I]) then
    begin
      Result := I;
      break;
    end;
end;

procedure Relocate(AMemory: PByte; ASize: NativeInt; AFrom, ATo: NativeInt);
const
  BuffSize = 65536;
var
  Buff: array [0 .. BuffSize - 1] of Byte;
  Pos: NativeInt;
begin
  if Max(AFrom, ATo) - Min(AFrom, ATo) >= ASize then
    Move((AMemory + AFrom)^, (AMemory + ATo)^, ASize)
  else
  begin
    Pos := 0;
    while Pos < ASize do
    begin
      Move((AMemory + AFrom + Pos)^, Buff[0], Min(BuffSize, ASize));
      Move(Buff[0], (AMemory + ATo + Pos)^, Min(BuffSize, ASize));
      Inc(Pos, BuffSize);
    end;
  end;
end;

function ConvertToBytes(S: string): Int64;
begin
  if AnsiContainsStr(S, 'kb') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 2)) * Power(1024, 1));
    exit;
  end;
  if AnsiContainsStr(S, 'mb') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 2)) * Power(1024, 2));
    exit;
  end;
  if AnsiContainsStr(S, 'gb') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 2)) * Power(1024, 3));
    exit;
  end;
  if AnsiContainsStr(S, 'k') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 1)) * Power(1024, 1));
    exit;
  end;
  if AnsiContainsStr(S, 'm') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 1)) * Power(1024, 2));
    exit;
  end;
  if AnsiContainsStr(S, 'g') then
  begin
    Result := Round(StrToFloat(Copy(S, 1, Length(S) - 1)) * Power(1024, 3));
    exit;
  end;
  Result := StrToInt64(S);
end;

function ConvertToThreads(S: string): Integer;
begin
  if AnsiContainsStr(S, 'p') or AnsiContainsStr(S, '%') then
  begin
    Result := Round((CPUCount * StrToInt(Copy(S, 1, Length(S) - 1))) / 100);
    exit;
  end;
  Result := StrToInt64(S);
end;

function ConvertKB2TB(Value: Int64): string;
  function NumToStr(Float: Single; DeciCount: Integer): string;
  begin
    Result := Format('%.' + IntToStr(DeciCount) + 'n', [Float]);
    Result := ReplaceStr(Result, ',', '');
  end;

const
  MV = 1024;
var
  S, MB, GB, TB: string;
begin
  MB := 'MB';
  GB := 'GB';
  TB := 'TB';
  if Value < Power(1000, 2) then
  begin
    S := NumToStr(Value / Power(MV, 1), 2);
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 1 then
      Result := NumToStr(Value / Power(MV, 1), 2) + ' ' + MB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 2 then
      Result := NumToStr(Value / Power(MV, 1), 1) + ' ' + MB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 3 then
      Result := NumToStr(Value / Power(MV, 1), 0) + ' ' + MB;
  end
  else if Value < Power(1000, 3) then
  begin
    S := NumToStr(Value / Power(MV, 2), 2);
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 1 then
      Result := NumToStr(Value / Power(MV, 2), 2) + ' ' + GB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 2 then
      Result := NumToStr(Value / Power(MV, 2), 1) + ' ' + GB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 3 then
      Result := NumToStr(Value / Power(MV, 2), 0) + ' ' + GB;
  end
  else if Value < Power(1000, 4) then
  begin
    S := NumToStr(Value / Power(MV, 3), 2);
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 1 then
      Result := NumToStr(Value / Power(MV, 3), 2) + ' ' + TB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 2 then
      Result := NumToStr(Value / Power(MV, 3), 1) + ' ' + TB;
    if Length(AnsiLeftStr(S, AnsiPos('.', S) - 1)) = 3 then
      Result := NumToStr(Value / Power(MV, 3), 0) + ' ' + TB;
  end;
end;

function BoolArray(const Bool: TArray<Boolean>; Value: Boolean): Boolean;
var
  I: Integer;
begin
  for I := Low(Bool) to High(Bool) do
  begin
    if Bool[I] <> Value then
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function GetUsedProcessMemory(hProcess: THandle): Int64;
var
  memCounters: TProcessMemoryCounters;
  cb: DWORD;
begin
  Result := 0;
  FillChar(memCounters, sizeof(TProcessMemoryCounters), 0);
  cb := sizeof(TProcessMemoryCounters);
  memCounters.cb := cb;
  if GetProcessMemoryInfo(hProcess, @memCounters, cb) then
    Result := memCounters.WorkingSetSize;
end;

function GetFreeSystemMemory: Int64;
var
  MemoryStatus: TMemoryStatusEx;
begin
  Result := 0;
  FillChar(MemoryStatus, sizeof(TMemoryStatusEx), 0);
  MemoryStatus.dwLength := sizeof(TMemoryStatusEx);
  if GlobalMemoryStatusEx(MemoryStatus) then
    Result := MemoryStatus.ullAvailPhys;
end;

function GetUsedSystemMemory: Int64;
var
  MemoryStatus: TMemoryStatusEx;
begin
  Result := 0;
  FillChar(MemoryStatus, sizeof(TMemoryStatusEx), 0);
  MemoryStatus.dwLength := sizeof(TMemoryStatusEx);
  if GlobalMemoryStatusEx(MemoryStatus) then
    Result := MemoryStatus.ullTotalPhys - MemoryStatus.ullAvailPhys;
end;

function GetTotalSystemMemory: Int64;
var
  MemoryStatus: TMemoryStatusEx;
begin
  Result := 0;
  FillChar(MemoryStatus, sizeof(TMemoryStatusEx), 0);
  MemoryStatus.dwLength := sizeof(TMemoryStatusEx);
  if GlobalMemoryStatusEx(MemoryStatus) then
    Result := MemoryStatus.ullTotalPhys;
end;

function FileSize(const AFileName: string): Int64;
var
  AttributeData: TWin32FileAttributeData;
begin
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @AttributeData)
  then
  begin
    Int64Rec(Result).Lo := AttributeData.nFileSizeLow;
    Int64Rec(Result).Hi := AttributeData.nFileSizeHigh;
  end
  else
    Result := 0;
end;

function GetFileList(const APath: TArray<string>; SubDir: Boolean)
  : TArray<string>;
var
  I: Integer;
  LList: TStringDynArray;
  LSO: TSearchOption;
begin
  SetLength(Result, 0);
  LSO := TSearchOption(SubDir);
  for I := Low(APath) to High(APath) do
  begin
    if FileExists(APath[I]) then
      Insert(APath[I], Result, Length(Result))
    else if DirectoryExists(APath[I]) then
    begin
      LList := TDirectory.GetFiles(APath[I], '*', LSO);
      Insert(LList, Result, Length(Result));
    end
    else if Pos('*', APath[I]) > 0 then
    begin
      LList := TDirectory.GetFiles(IfThen(ExtractFileDir(APath[I]) = '',
        GetCurrentDir, ExtractFilePath(APath[I])),
        ExtractFileName(APath[I]), LSO);
      Insert(LList, Result, Length(Result));
    end;
  end;
  SetLength(LList, 0);
end;

procedure FileReadBuffer(Handle: THandle; var Buffer; Count: NativeInt);
var
  LTotalCount, LReadCount: NativeInt;
begin
  LTotalCount := FileRead(Handle, Buffer, Count);
  if LTotalCount < 0 then
    raise EReadError.CreateRes(@SReadError);
  while (LTotalCount < Count) do
  begin
    LReadCount := FileRead(Handle, (PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));
    if LReadCount <= 0 then
      raise EReadError.CreateRes(@SReadError)
    else
      Inc(LTotalCount, LReadCount);
  end
end;

procedure FileWriteBuffer(Handle: THandle; const Buffer; Count: NativeInt);
var
  LTotalCount, LWrittenCount: NativeInt;
begin
  LTotalCount := FileWrite(Handle, Buffer, Count);
  if LTotalCount < 0 then
    raise EWriteError.CreateRes(@SWriteError);
  while (LTotalCount < Count) do
  begin
    LWrittenCount := FileWrite(Handle, (PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));
    if LWrittenCount <= 0 then
      raise EWriteError.CreateRes(@SWriteError)
    else
      Inc(LTotalCount, LWrittenCount);
  end
end;

function Exec(Executable, CommandLine, WorkDir: string): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  dwExitCode: DWORD;
  LWorkDir: PChar;
begin
  Result := False;
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb := sizeof(StartupInfo);
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    False, 0, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, dwExitCode);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    Result := True;
  end;
end;

function ExecStdin(Executable, CommandLine, WorkDir: string; InBuff: Pointer;
  InSize: Integer): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: sizeof(PipeSecurityAttributes); bInheritHandle: True);
var
  hstdinr, hstdinw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  LWorkDir: PChar;
begin
  Result := False;
  CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.hStdInput := hstdinr;
  StartupInfo.hStdOutput := 0;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, sizeof(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    Result := True;
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    FileWriteBuffer(hstdinw, InBuff^, InSize);
    CloseHandle(hstdinr);
    CloseHandle(hstdinw);
  end
  else
  begin
    CloseHandle(hstdinr);
    CloseHandle(hstdinw);
    RaiseLastOSError;
  end;
end;

function ExecStdout(Executable, CommandLine, WorkDir: string;
  Output: TExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: sizeof(PipeSecurityAttributes); bInheritHandle: True);
  BufferSize = 65536;
var
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
  LWorkDir: PChar;
begin
  Result := False;
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, sizeof(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(hstdoutw);
    while ReadFile(hstdoutr, Buffer, Length(Buffer), BytesRead, nil) and
      (BytesRead > 0) do
      Output(@Buffer[0], BytesRead);
    CloseHandle(hstdoutr);
    Result := True;
  end
  else
  begin
    CloseHandle(hstdoutr);
    CloseHandle(hstdoutw);
    RaiseLastOSError;
  end;
end;

function ExecStdio(Executable, CommandLine, WorkDir: string; InBuff: Pointer;
  InSize: Integer; Output: TExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: sizeof(PipeSecurityAttributes); bInheritHandle: True);
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
  hstdinr, hstdinw: THandle;
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  LWorkDir: PChar;
begin
  Result := True;
  CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.hStdInput := hstdinr;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, sizeof(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(hstdinr);
    CloseHandle(hstdoutw);
    FileWriteBuffer(hstdinw, InBuff^, InSize);
    CloseHandle(hstdinw);
    while ReadFile(hstdoutr, Buffer[0], Length(Buffer), BytesRead, nil) and
      (BytesRead > 0) do
      Output(@Buffer[0], BytesRead);
    CloseHandle(hstdoutr);
    Result := True;
  end
  else
  begin
    CloseHandle(hstdinr);
    CloseHandle(hstdinw);
    CloseHandle(hstdoutr);
    CloseHandle(hstdoutw);
    RaiseLastOSError;
  end;
end;

procedure ExecReadTask(Handle, Stream, Done: IntPtr);
const
  BufferSize = 65536;
var
  Buffer: array [0 .. BufferSize - 1] of Byte;
  BytesRead: DWORD;
begin
  PBoolean(Pointer(Done))^ := False;
  while ReadFile(Handle, Buffer[0], Length(Buffer), BytesRead, nil) and
    (BytesRead > 0) do
    PExecOutput(Pointer(Stream))^(@Buffer[0], BytesRead);
  PBoolean(Pointer(Done))^ := BytesRead = 0;
end;

function ExecStdioSync(Executable, CommandLine, WorkDir: string;
  InBuff: Pointer; InSize: Integer; Output: TExecOutput): Boolean;
const
  PipeSecurityAttributes: TSecurityAttributes =
    (nLength: sizeof(PipeSecurityAttributes); bInheritHandle: True);
var
  hstdinr, hstdinw: THandle;
  hstdoutr, hstdoutw: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  LWorkDir: PChar;
  LTask: TTask;
  LDone: Boolean;
begin
  Result := True;
  CreatePipe(hstdinr, hstdinw, @PipeSecurityAttributes, 0);
  CreatePipe(hstdoutr, hstdoutw, @PipeSecurityAttributes, 0);
  SetHandleInformation(hstdinw, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(hstdoutr, HANDLE_FLAG_INHERIT, 0);
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.hStdInput := hstdinr;
  StartupInfo.hStdOutput := hstdoutw;
  StartupInfo.hStdError := 0;
  ZeroMemory(@ProcessInfo, sizeof(ProcessInfo));
  if WorkDir <> '' then
    LWorkDir := Pointer(WorkDir)
  else
    LWorkDir := Pointer(GetCurrentDir);
  if CreateProcess(nil, PChar('"' + Executable + '" ' + CommandLine), nil, nil,
    True, NORMAL_PRIORITY_CLASS, nil, LWorkDir, StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(hstdinr);
    CloseHandle(hstdoutw);
    LTask := TTask.Create(hstdoutr, NativeInt(@Output), NativeInt(@LDone));
    LTask.Perform(ExecReadTask);
    LTask.Start;
    FileWriteBuffer(hstdinw, InBuff^, InSize);
    CloseHandle(hstdinw);
    LTask.Wait;
    LTask.Free;
    CloseHandle(hstdoutr);
    Result := True;
  end
  else
  begin
    CloseHandle(hstdinr);
    CloseHandle(hstdinw);
    CloseHandle(hstdoutr);
    CloseHandle(hstdoutw);
    RaiseLastOSError;
  end;
end;

type
  PAnsiCharArray = array [0 .. 0] of PAnsiChar;

function GetParamStr(P: PChar; var Param: string): PChar;
var
  I, len: Integer;
  Start, S: PChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then
      Inc(P, 2)
    else
      break;
  end;
  len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Inc(len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Inc(len);
      Inc(P);
    end;
  end;
  SetLength(Param, len);
  P := Start;
  S := Pointer(Param);
  I := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        S[I] := P^;
        Inc(P);
        Inc(I);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      S[I] := P^;
      Inc(P);
      Inc(I);
    end;
  end;
  Result := P;
end;

function GetCmdCount(CommandLine: String): Integer;
var
  P: PChar;
  S: string;
begin
  Result := 0;
  P := GetParamStr(PChar(CommandLine), S);
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then
      break;
    Inc(Result);
  end;
end;

function GetCmdStr(CommandLine: String; Index: Integer): string;
var
  P: PChar;
  Buffer: array [0 .. 260] of char;
begin
  Result := '';
  P := PChar(CommandLine);
  while Index >= 0 do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then
      break;
    Dec(Index);
  end;
end;

end.
