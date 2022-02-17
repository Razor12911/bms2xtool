/// regression tests for mORMot's cross-platform units
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformTests;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2020 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2020
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Should compile with Delphi for any platform, or with FPC or Kylix

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

interface

uses
  SysUtils,
  Classes,
  Variants,
  TypInfo,
{$ifdef ISDELPHI2010}
  System.Generics.Collections,
{$endif}
{$ifndef NEXTGEN}
  Contnrs,
{$endif}
  mORMotClient, // as generated by mORMotWrappers.pas !
  SynCrossPlatformJSON,
  SynCrossPlatformCrypto,
  SynCrossPlatformSpecific,
  SynCrossPlatformRest;


type
  /// the prototype of an individual test
  // - to be used with TSynTest descendants
  TSynTestEvent = procedure of object;

{$M+} { we need the RTTI for the published methods of this object class }
  /// generic class for performing simple tests 
  // - purpose of this ancestor is to have RTTI for its published methods,
  // which will contain the tests
  TSynTest = class
  protected
    fFailureMsg: string;
    fCurrentTest: Integer;
  public
    /// the test case name
    Ident: string;
    /// the registered tests, i.e. all published methods of this class
    Tests: TPublishedMethodDynArray;
    /// how many Check() call did pass
    Passed: cardinal;
    /// how many Check() call did failed
    Failed: cardinal;
    /// create the test instance
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const aIdent: string='');
    /// run all tests
    procedure Run(LogToConsole: boolean);
    /// validate a test
    procedure Check(test: Boolean; const Msg: string=''); overload;
  published
  end;

  /// regression tests of our CrossPlatform units
  TSynCrossPlatformTests = class(TSynTest)
  published
    procedure Iso8601DateTime;
    procedure Base64Encoding;
    procedure JSON;
    procedure Model;
    procedure Cryptography;
  end;

  /// regression tests of our CrossPlatform units
  TSynCrossPlatformClient = class(TSynTest)
  protected
    fAuthentication: TSQLRestServerAuthenticationClass;
    fClient: TSQLRestClientHTTP;
  public
    constructor Create(aAuthentication: TSQLRestServerAuthenticationClass); reintroduce;
    destructor Destroy; override;
  published
    procedure Connection;
    procedure ORM;
    procedure ORMBatch;
    procedure Services;
    procedure CleanUp;
  end;
{$M-}


implementation

type
  TSQLRecordPeopleSimple = class(TSQLRecord)
  private
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;
  
  TMainNested = class(TCollectionItem)
  private
    fNumber: double;
    fIdent: RawUTF8;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
    property Number: double read fNumber write fNumber;
  end;

  TMain = class(TPersistent)
  private
    fName: RawUTF8;
    fNested: TCollection;
    fList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Name: RawUTF8 read fName write fName;
    property Nested: TCollection read fNested;
    property List: TStringList read fList;
  end;


{ TSynTest }

procedure TSynTest.Check(test: Boolean; const Msg: string='');
begin
  if test then
    inc(Passed) else begin
    inc(Failed);
    if Msg<>'' then
      fFailureMsg := fFailureMsg+'['+Msg+'] ';
  end;
end;

constructor TSynTest.Create(const aIdent: string);
begin
  Ident := aIdent;
  GetPublishedMethods(self,Tests);
end;

procedure TSynTest.Run(LogToConsole: boolean);
var i: integer;
    BeforePassed,BeforeFailed: cardinal;
    startclass, startmethod: TDateTime;
    datetime: string;
    LogFile: text;
  procedure Log(const Fmt: string; const Args: array of const);
  var msg: string;
  begin
    msg := format(Fmt,Args);
    if LogToConsole then
      writeln(msg) else
      writeln(LogFile,msg);
    if not LogToConsole then
      Flush(LogFile);
  end;
begin
  startclass := Now;
  datetime := DateTimeToIso8601(startclass);
  if not LogToConsole then begin
    assign(LogFile,ExtractFilePath(ParamStr(0))+
      FormatDateTime('yyyy mm dd hh nn ss',startclass)+'.txt');
    rewrite(LogFile);
  end;
  Log(#13#10' %s'#13#10'%s',[Ident,StringOfChar('-',length(Ident)+2)]);
  for i := 0 to high(Tests) do begin
    Log(#13#10' %d. Running "%s"',[i+1,Tests[i].Name]);
    startmethod := Now;
    BeforePassed := Passed;
    BeforeFailed := Failed;
    try
      fCurrentTest := i;
      TSynTestEvent(Tests[i].Method)();
    except
      on E: Exception do
        Check(False,format('Exception %s raised with message "%s"',[E.ClassName,E.Message]));
    end;
    if Failed<>BeforeFailed then
      Log(' !!! %d test(s) failed / %d %s',[Failed-BeforeFailed,
        Failed-BeforeFailed+Passed-BeforePassed,fFailureMsg]) else
      Log('    %d tests passed in %s',[Passed-BeforePassed,
        FormatDateTime('nn:ss:zzz',Now-startmethod)]);
    fFailureMsg := '';
  end;
  Log(#13#10' Tests failed: %d / %d'#13#10' Time elapsed: %s'#13#10#13#10' %s',
    [Failed,Failed+Passed,FormatDateTime('nn:ss:zzz',Now-startclass),datetime]);
  if not LogToConsole then
    close(LogFile);
end;


{ TSynCrossPlatformTests }

procedure TSynCrossPlatformTests.Base64Encoding;
var b,c: TByteDynArray;
    i: integer;
begin
  check(b=nil);
  for i := 0 to 100 do begin
    SetLength(b,i);
    if i>0 then
      b[i-1] := i;
    check(Base64JSONStringToBytes(BytesToBase64JSONString(b),c));
    check(length(c)=i);
    check(CompareMem(Pointer(b),pointer(c),i));
  end;
end;

procedure TSynCrossPlatformTests.Cryptography;
var c: array of byte;
    s: string;
begin
  SetLength(c,5);
  c[4] := $96;
  Check(crc32(0,c)=$DF4EC16C,'crc32');
  Check(crc32ascii(0,'abcdefghijklmnop')=$943AC093);
  SetLength(c,3);
  c[0] := ord('a');
  c[1] := ord('b');
  c[2] := ord('c');
  s := SHA256(c);
  check(s='ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  check(SHA256('abc')=s);
end;

procedure TSynCrossPlatformTests.Iso8601DateTime;
procedure Test(D: TDateTime);
var s: string;
procedure One(D: TDateTime);
var E: TDateTime;
    V: TTimeLog;
begin
  s := DateTimeToIso8601(D);
  E := Iso8601ToDateTime(s);
  Check(Abs(D-E)<(1/SecsPerDay)); // we allow 1 sec error
  Check(DateTimeToJSON(D)='"'+s+'"');
  V := DateTimeToTTimeLog(D);
  E := TTimeLogToDateTime(V);
  Check(Abs(D-E)<(1/SecsPerDay));
  Check(UrlDecode(UrlEncode(s))=s);
end;
begin
  One(D);
  Check(length(s)=19);
  One(Trunc(D));
  Check(length(s)=10);
  One(Frac(D));
  Check(length(s)=9);
end;
var D: TDateTime;
    i: integer;
    s: string;
    T: TTimeLog;
begin
  s := '2014-06-28T11:50:22';
  D := Iso8601ToDateTime(s);
  Check(Abs(D-41818.49331)<(1/SecsPerDay));
  Check(DateTimeToIso8601(D)=s);
  T := DateTimeToTTimeLog(D);
  Check(T=135181810838);
  D := Now/20+Random*20; // some starting random date/time
  for i := 1 to 2000 do begin
    Test(D);
    D := D+Random*57; // go further a little bit: change date/time
  end;
end;

procedure TSynCrossPlatformTests.JSON;
var doc: variant;
    js,json2,inlined: string;
    i: integer;
    obj1,obj2: TMain;
    item: TMainNested;
begin
  doc := JSONVariant('{"test":1234,"name":"Joh\"n\r","zero":0.0}');
  check(doc.test=1234);
  check(doc.name='Joh"n'#13);
  check(doc.name2=null);
  check(doc.zero=0);
  js := doc;
  check(js='{"test":1234,"name":"Joh\"n\r","zero":0}');
  {$ifdef FPC}
  TJSONVariantData(doc)['name2'] := 3.1415926;
  TJSONVariantData(doc)['name'] := 'John';
  {$else}
  doc.name2 := 3.1415926;
  doc.name := 'John';
  {$endif}
  js := doc;
  check(js='{"test":1234,"name":"John","zero":0,"name2":3.1415926}');
  doc := JSONVariant('[{ID:1,"Username":"xx","FirstName":"System",Active:-1}]');
  check(TJSONVariantData(doc).Kind=jvArray);
  check(TJSONVariantData(doc).Count=1);
  check(TJSONVariantData(doc).Values[0].ID=1);
  check(TJSONVariantData(doc).Values[0].Username='xx');
  check(TJSONVariantData(doc).Values[0].Active=-1);
  check(IsRowID('id'));
  check(IsRowID('iD'));
  check(IsRowID('rowid'));
  check(IsRowID('RowID'));
  check(not IsRowID('iz'));
  check(not IsRowID('i2'));
  check(not IsRowID('rawid'));
  check(not IsRowID(''));
  check(FormatBind('',[])='');
  for i := 1 to 1000 do begin
    js := IntToStr(i);
    inlined := ':('+js+'):';
    check(FormatBind(js,[])=js);
    check(FormatBind(js,[i])=js);
    check(FormatBind('?',[i])=inlined);
    check(FormatBind('a?a',[i])='a'+inlined+'a');
    check(FormatBind('a?',[i])='a'+inlined);
    check(FormatBind('?a',[i])=inlined+'a');
    check(FormatBind('ab?',[i])='ab'+inlined);
    check(FormatBind('?ab',[i])=inlined+'ab');
    check(FormatBind('ab?ab',[i])='ab'+inlined+'ab');
    check(FormatBind('abc?abc',[i])='abc'+inlined+'abc');
    check(FormatBind('abc?abc',[i,1])='abc'+inlined+'abc');
    check(FormatBind(js+'?',[i])=js+inlined);
    check(FormatBind('?'+js,[i])=inlined+js);
    check(FormatBind('ab?ab',[js])='ab:("'+js+'"):ab');
    check(FormatBind('ab?ab',[variant(js)])='ab:("'+js+'"):ab');
    check(FormatBind('ab?ab',[variant(i)])='ab'+inlined+'ab');
    check(FormatBind('ab?ab?',[variant(i)])='ab'+inlined+'ab:(null):');
    check(FormatBind('ab?ab??cd',[i,i,js])='ab'+inlined+'ab'+inlined+
      ':("'+js+'"):cd');
  end;
  RegisterClassForJSON([TMainNested]); // for JSONToNewObject()
  obj1 := TMain.Create;
  obj2 := TMain.Create;
  try
    for i := 1 to 100 do begin
      obj1.Name := IntToStr(i);
      item := obj1.Nested.Add as TMainNested;
      item.Ident := obj1.Name;
      item.Number := i/2;
      check(obj1.Nested.Count=i);
      obj1.list.Add(obj1.Name);
      js := ObjectToJSON(obj1);
      check(js<>'');
      if i=1 then
        check(js='{"Name":"1","Nested":[{"Ident":"1","Number":0.5}],"List":["1"]}');
      JSONToObject(obj2,js);
      check(obj2.Nested.Count=i);
      json2 := ObjectToJSON(obj2);
      check(json2=js);
      js := ObjectToJSON(item,true);
      item := TMainNested(JSONToNewObject(js));
      check(item<>nil);
      json2 := ObjectToJSON(item,true);
      check(json2=js);
      item.Free;
    end;
  finally
    obj2.Free;
    obj1.Free;
  end;
  js := 'one,two,3';
  i := 1;
  check(GetNextCSV(js,i,json2));
  check(json2='one');
  check(GetNextCSV(js,i,json2));
  check(json2='two');
  check(GetNextCSV(js,i,json2));
  check(json2='3');
  check(not GetNextCSV(js,i,json2));
  check(not GetNextCSV(js,i,json2));
  js := 'one';
  i := 1;
  check(GetNextCSV(js,i,json2));
  check(json2='one');
  check(not GetNextCSV(js,i,json2));
  js := '';
  i := 1;
  check(not GetNextCSV(js,i,json2));
  doc := JsonVariant('{}');
  js := doc;
  check(js='{}');
end;

procedure TSynCrossPlatformTests.Model;
var mdel: TSQLModel;
    people: TSQLRecordPeopleSimple;
    i: integer;
    js: string;
    fields: TSQLFieldBits;
begin
  mdel := TSQLModel.Create([TSQLRecordPeopleSimple],'test/');
  Check(mdel.Root='test');
  Check(length(mdel.Info)=1);
  Check(mdel.Info[0].Table=TSQLRecordPeopleSimple);
  Check(mdel.Info[0].Name='PeopleSimple');
  Check(length(mdel.Info[0].Prop)=6);
  people := TSQLRecordPeopleSimple.Create;
  try
    for i := 1 to 1000 do begin
      people.ID := i;
      people.FirstName := IntToStr(i);
      people.LastName := people.FirstName+people.FirstName;
      people.YearOfBirth := i+500;
      people.YearOfDeath := people.YearOfBirth+40;
      js := ObjectToJSON(people);
      check(js=Format('{"ID":%d,"FirstName":"%d","LastName":"%d%d",'+
        '"Data":"","YearOfBirth":%d,"YearOfDeath":%d}',[i,i,i,i,i+500,i+540]));
    end;
  finally
    people.Free;
  end;
  Check(PInteger(@mdel.Info[0].SimpleFields)^=$37);
  Check(PInteger(@mdel.Info[0].BlobFields)^=8);
  fields := mdel.Info[0].FieldNamesToFieldBits('',false);
  Check(PInteger(@fields)^=$37);
  fields := mdel.Info[0].FieldNamesToFieldBits('*',false);
  Check(PInteger(@fields)^=PInteger(@mdel.Info[0].AllFields)^);
  fields := mdel.Info[0].FieldNamesToFieldBits('id,firstname',false);
  Check(PInteger(@fields)^=3);
  fields := mdel.Info[0].FieldNamesToFieldBits('RowID ,  firstname ',false);
  Check(PInteger(@fields)^=3);
  Check(mdel.Info[0].FieldBitsToFieldNames(fields)='RowID,FirstName');
  fields := mdel.Info[0].FieldNamesToFieldBits('firstname,id,toto',false);
  Check(PInteger(@fields)^=3);
  Check(mdel.Info[0].FieldBitsToFieldNames(fields)='RowID,FirstName');
  mdel.Free;
end;


{ TMain }

constructor TMain.Create;
begin
  inherited;
  fNested := TCollection.Create(TMainNested);
  fList := TStringList.Create;
end;

destructor TMain.Destroy;
begin
  fList.Free;
  fNested.Free;
  inherited;
end;

{ TSynCrossPlatformClient }

constructor TSynCrossPlatformClient.Create(
  aAuthentication: TSQLRestServerAuthenticationClass);
begin
  inherited Create;
  fAuthentication := aAuthentication;
end;

destructor TSynCrossPlatformClient.Destroy;
begin
  CleanUp;
  inherited;
end;

procedure TSynCrossPlatformClient.CleanUp;
begin
  FreeAndNil(fClient);
  check(fClient=nil);
end;

procedure TSynCrossPlatformClient.Connection;
var doremotelog: boolean;
    dofilelog: boolean;
begin
  doremotelog := false;
  dofilelog := false;
  if fAuthentication=TSQLRestServerAuthenticationDefault then begin
    fClient := GetClient('localhost','User','synopse');
    if dofilelog then
      fClient.LogToFile(LOG_VERBOSE);
    if doremotelog then
      fClient.LogToRemoteServer(LOG_VERBOSE,'localhost');
  end else begin
    fClient := TSQLRestClientHTTP.Create('localhost',SERVER_PORT,GetModel,true);
    if dofilelog then
      fClient.LogToFile(LOG_VERBOSE);
    if doremotelog then
      fClient.LogToRemoteServer(LOG_VERBOSE,'localhost');
    check(fClient.Connect);
    check(fClient.ServerTimeStamp<>0);
    if fAuthentication<>nil then
      fClient.SetUser(fAuthentication,'User','synopse');
  end;
end;

procedure TSynCrossPlatformClient.ORM;
  procedure TestPeople(people: TSQLRecordPeople; var id: integer);
  begin
    Check(people.InternalState=fClient.InternalState);
    inc(id);
    Check(people.ID=id);
    Check(people.FirstName='');
    Check(people.LastName='');
    Check(people.YearOfBirth=id+1800);
    Check(people.YearOfDeath=id+1825);
    Check(people.Sexe=sFemale);
  end;
var people: TSQLRecordPeople;
    Call: TSQLRestURIParams;
    i,id: integer;
    list: TObjectList;
    {$ifdef ISDELPHI2010}
    peoples: TObjectList<TSQLRecordPeople>;
    {$endif ISDELPHI2010}
begin
  fClient.CallBackGet('DropTable',[],Call,TSQLRecordPeople);
  Check(fClient.InternalState>0);
  Check(Call.OutStatus=HTTP_SUCCESS);
  people := TSQLRecordPeople.Create;
  try
    Check(people.InternalState=0);
    for i := 1 to 200 do begin
      people.FirstName := 'First'+IntToStr(i);
      people.LastName := 'Last'+IntToStr(i);
      people.YearOfBirth := i+1800;
      people.YearOfDeath := i+1825;
      people.Sexe := TPeopleSexe(i and 1);
      Check(fClient.Add(people,true)=i);
      Check(people.InternalState=fClient.InternalState);
    end;
  finally
    people.Free;
  end;
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,'','',[]);
  try
    Check(people.InternalState=0);
    id := 0;
    while people.FillOne do begin
      Check(people.InternalState=fClient.InternalState);
      inc(id);
      Check(people.ID=id);
      Check(people.FirstName='First'+IntToStr(id));
      Check(people.LastName='Last'+IntToStr(id));
      Check(people.YearOfBirth=id+1800);
      Check(people.YearOfDeath=id+1825);
      Check(ord(people.Sexe)=id and 1);
    end;
    Check(id=200);
  finally
    people.Free;
  end;
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,
    'YearOFBIRTH,Yearofdeath,id','',[]);
  try
    Check(people.InternalState=0);
    id := 0;
    while people.FillOne do
       TestPeople(people,id);
    Check(id=200);
  finally
    people.Free;
  end;
  list := fClient.RetrieveList(TSQLRecordPeople,'YearOFBIRTH,Yearofdeath,id','',[]);
  try
    id := 0;
    for i := 0 to list.Count-1 do
     TestPeople(TSQLRecordPeople(list[i]),id);
    Check(id=200);
  finally
    list.Free;
  end;
  {$ifdef ISDELPHI2010}
  peoples := fClient.RetrieveList<TSQLRecordPeople>('YearOFBIRTH,yearofdeath,id','',[]);
  try
    id := 0;
    for i := 0 to peoples.Count-1 do
     TestPeople(peoples[i],id);
    Check(id=200);
  finally
    peoples.Free;
  end;
  {$endif ISDELPHI2010}
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,'',
    'yearofbirth=?',[1900]);
  try
    Check(people.InternalState=0);
    id := 0;
    while people.FillOne do begin
      Check(people.InternalState=fClient.InternalState);
      inc(id);
      Check(people.ID=100);
      Check(people.FirstName='First100');
      Check(people.LastName='Last100');
      Check(people.YearOfBirth=1900);
      Check(people.YearOfDeath=1925);
    end;
    Check(id=1);
  finally
    people.Free;
  end;
  for i := 1 to 200 do
    if i and 15=0 then
      fClient.Delete(TSQLRecordPeople,i) else
    if i mod 82=0 then begin
      people := TSQLRecordPeople.Create;
      try
        id := i+1;
        people.ID := i;
        people.FirstName := 'First'+IntToStr(id);
        people.LastName := 'Last'+IntToStr(id);
        people.YearOfBirth := id+1800;
        people.YearOfDeath := id+1825;
        Check(people.InternalState=0);
        Check(fClient.Update(people,'YEarOFBIRTH,YEarOfDeath'));
        Check(people.InternalState=fClient.InternalState);
      finally
        people.Free;
      end;
    end;
  for i := 1 to 200 do begin
    people := TSQLRecordPeople.Create(fClient,i);
    try
      if i and 15=0 then
        Check(people.ID=0) else begin
        Check(people.InternalState=fClient.InternalState);
        if i mod 82=0 then
          id := i+1 else
          id := i;
        Check(people.ID=i);
        Check(people.FirstName='First'+IntToStr(i));
        Check(people.LastName='Last'+IntToStr(i));
        Check(people.YearOfBirth=id+1800);
        Check(people.YearOfDeath=id+1825);
        Check(ord(people.Sexe)=i and 1);
      end;
    finally
      people.Free;
    end;
  end;
end;

procedure TSynCrossPlatformClient.ORMBatch;
var people: TSQLRecordPeople;
    Call: TSQLRestURIParams;
    res: TIDDynArray;
    {$ifndef ISDWS}
    blob: TSQLRawBlob;
    {$endif}
    i,id: integer;
begin
  fClient.CallBackGet('DropTable',[],Call,TSQLRecordPeople);
  Check(fClient.InternalState>0);
  Check(Call.OutStatus=HTTP_SUCCESS);
  fClient.BatchStart(TSQLRecordPeople);
  people := TSQLRecordPeople.Create;
  try
    for i := 1 to 200 do begin
      Check(people.InternalState=0);
      people.FirstName := 'First'+IntToStr(i);
      people.LastName := 'Last'+IntToStr(i);
      people.YearOfBirth := i+1800;
      people.YearOfDeath := i+1825;
      people.Sexe := TPeopleSexe(i and 1);
      fClient.BatchAdd(people,true);
    end;
  finally
    people.Free;
  end;
  Check(fClient.BatchSend(res)=HTTP_SUCCESS);
  Check(length(res)=200);
  for i := 1 to length(res) do
    Check(res[i-1]=i);
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,'','',[]);
  try
    Check(people.InternalState=0);
    id := 0;
    while people.FillOne do begin
      Check(people.InternalState=fClient.InternalState);
      inc(id);
      Check(people.ID=id);
      Check(people.FirstName='First'+IntToStr(id));
      Check(people.LastName='Last'+IntToStr(id));
      Check(people.YearOfBirth=id+1800);
      Check(people.YearOfDeath=id+1825);
      Check(ord(people.Sexe)=id and 1);
    end;
    Check(id=200);
  finally
    people.Free;
  end;
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,
    'YearOFBIRTH,Yearofdeath,id','',[]);
  try
    id := 0;
    Check(people.InternalState=0);
    while people.FillOne do begin
      Check(people.InternalState=fClient.InternalState);
      inc(id);
      Check(people.ID=id);
      Check(people.FirstName='');
      Check(people.LastName='');
      Check(people.YearOfBirth=id+1800);
      Check(people.YearOfDeath=id+1825);
      Check(people.Sexe=sFemale);
    end;
    Check(id=200);
  finally
    people.Free;
  end;
  people := TSQLRecordPeople.CreateAndFillPrepare(fClient,'',
    'yearofbirth=?',[1900]);
  try
    Check(people.InternalState=0);
    id := 0;
    while people.FillOne do begin
      Check(people.InternalState=fClient.InternalState);
      inc(id);
      Check(people.ID=100);
      Check(people.FirstName='First100');
      Check(people.LastName='Last100');
      Check(people.YearOfBirth=1900);
      Check(people.YearOfDeath=1925);
    end;
    Check(id=1);
  finally
    people.Free;
  end;
  fClient.BatchStart(nil);
  for i := 1 to 200 do
    if i and 15=0 then
      fClient.BatchDelete(TSQLRecordPeople,i) else
    if i mod 82=0 then begin
      people := TSQLRecordPeople.Create;
      try
        id := i+1;
        people.ID := i;
        people.FirstName := 'First'+IntToStr(id);
        people.LastName := 'Last'+IntToStr(id);
        people.YearOfBirth := id+1800;
        people.YearOfDeath := id+1825;
        Check(fClient.BatchUpdate(people,'YEarOFBIRTH,YEarOfDeath')>=0);
        Check(people.InternalState=0);
      finally
        people.Free;
      end;
    end;
  Check(fClient.BatchSend(res)=HTTP_SUCCESS);
  Check(length(res)=14);
  for i := 1 to 14 do
    Check(res[i-1]=HTTP_SUCCESS);
  for i := 1 to 200 do begin
    people := TSQLRecordPeople.Create(fClient,i);
    try
      if i and 15=0 then
        Check(people.ID=0) else begin
        Check(people.InternalState=fClient.InternalState);
        if i mod 82=0 then
          id := i+1 else
          id := i;
        Check(people.ID=i);
        Check(people.FirstName='First'+IntToStr(i));
        Check(people.LastName='Last'+IntToStr(i));
        Check(people.YearOfBirth=id+1800);
        Check(people.YearOfDeath=id+1825);
        Check(ord(people.Sexe)=i and 1);
      end;
    finally
      people.Free;
    end;
  end;
  {$ifndef ISDWS}
  exit; // Add(..,'Data') below is buggy, but RetrieveBlob() seems fine
  people := TSQLRecordPeople.Create;
  try
    people.FirstName := 'With';
    people.LastName := 'Blob';
    SetLength(blob,2);
    blob[0] := 1;
    blob[1] := 2;
    people.Data := blob;
    id := fClient.Add(people,true,false,'FirstName,LastName,Data');
    Check(id=201);
    Check(people.InternalState=fClient.InternalState);
    blob := nil;
  finally
    people.Free;
  end;
  people := TSQLRecordPeople.Create(fClient,id);
  try
    Check(people.FirstName='With');
    Check(people.LastName='Blob');
    Check(people.Data=nil);
    Check(not fClient.RetrieveBlob(TSQLRecordPeople,id,'wrongfieldname',blob));
    Check(blob=nil);
    Check(fClient.RetrieveBlob(TSQLRecordPeople,id,'data',blob));
    Check(blob<>nil);
  finally
    people.Free;
  end;
  {$endif}
end;

procedure TSynCrossPlatformClient.Services;
var calc: ICalculator;
    i,j: integer;
    sex: TPeopleSexe;
    name: string;
    rec: TTestCustomJSONArraySimpleArray;
const SEX_TEXT: array[0..1] of RawUTF8 = ('Miss','Mister');
begin
  calc := TServiceCalculator.Create(fClient);
  check(calc.InstanceImplementation=sicShared);
  check(calc.ServiceName='Calculator');
  for i := 1 to 200 do
    check(calc.Add(i,i+1)=i*2+1);
  for i := 1 to 200 do begin
    sex := TPeopleSexe(i and 1);
    name := 'Smith';
    calc.ToText(i,'$',sex,name);
    check(sex=sFemale);
    check(name=format('$ %d for %s Smith',[i,SEX_TEXT[i and 1]]));
  end;
  Fillchar(rec,SizeOf(rec),0);
  for i := 1 to 100 do begin
    name := calc.RecordToText(rec);
    if i=1 then
      check(name='{"F":"","G":[],"H":{"H1":0,"H2":"","H3":{"H3a":false,"H3b":null}},"I":"","J":[]}');
    check(length(Rec.F)=i);
    for j := 1 to length(Rec.F) do
      check(Rec.F[j]='!');
    check(length(Rec.G)=i);
    for j := 0 to high(Rec.G) do
      check(Rec.G[j]=IntToStr(j+1));
    check(Rec.H.H1=i);
    check(length(Rec.J)=i-1);
    for j := 0 to high(Rec.J) do begin
      Check(Rec.J[j].J1=j);
      Check(Rec.J[j].J2.D2=j);
      Check(Rec.J[j].J3=TRecordEnum(j mod (ord(high(TRecordEnum))+1)));
    end;
  end;
end;

end.
