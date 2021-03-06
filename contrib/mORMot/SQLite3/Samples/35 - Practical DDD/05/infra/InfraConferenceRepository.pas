/// Conference Repository implementation
unit InfraConferenceRepository;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  DomConferenceTypes,
  DomConferenceDepend;

type
  /// defines the table in the DB
  // - ID primary key is the TAttendeeRegistrationNumber
  TSQLBooking = class(TSQLRecord)
  private
    fName: RawUTF8;
    fFirstName: RawUTF8;
    fSessions: variant;
  published
    property Name: RawUTF8 read fName write fName;
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property Sessions: variant read fSessions write fSessions;
  end;

  /// the repository implementation using mORMot REST ORM
  TORMBookingRepository = class(TInterfacedObject, IBookingRepository)
  protected
    fRest: TSQLRest;
    function GetBooking(const Name: TAttendeeName;
      const FirstName: TAttendeeFirstName): TSQLBooking;
  public
    constructor Create(ORM: TSQLRest); reintroduce;
    // IBookingRepository methods
    function SaveNewRegistration(const Attendee: TAttendee; const Days: TSessionDays;
      out RegNum: TAttendeeRegistrationNumber): TBookingRepositoryError;
    function RetrieveRegistration(const Name: TAttendeeName;
      const FirstName: TAttendeeFirstName; out Days: TSessionDays;
      out Attendee: TAttendee): boolean;
  end;

  
implementation

uses Variants;

{ TORMBookingRepository }

constructor TORMBookingRepository.Create(ORM: TSQLRest);
begin
  inherited Create;
  ORM.Model.GetTableIndexExisting(TSQLBooking); // ensure part of the ORM model
  fRest := ORM;
end;

function TORMBookingRepository.GetBooking(const Name: TAttendeeName;
  const FirstName: TAttendeeFirstName): TSQLBooking;
begin
  result := TSQLBooking.Create(fRest, 'Name like ? and FirstName like ?',
     [Name, FirstName]);
end;

function TORMBookingRepository.RetrieveRegistration(
  const Name: TAttendeeName; const FirstName: TAttendeeFirstName;
  out Days: TSessionDays; out Attendee: TAttendee): boolean;
var
  rec: TSQLBooking;
begin
  result := false;
  rec := GetBooking(Name, FirstName);
  try
    if rec.IDValue = 0 then
      exit;
    Attendee.Name := rec.Name;
    Attendee.FirstName := rec.FirstName;
    Attendee.RegistrationNumber := rec.IDValue;
    DynArray(TypeInfo(TSessionDays), Days).LoadFromVariant(rec.Sessions);
    result := true;
  finally
    rec.Free;
  end;
end;

function TORMBookingRepository.SaveNewRegistration(const Attendee: TAttendee;
  const Days: TSessionDays; out RegNum: TAttendeeRegistrationNumber): TBookingRepositoryError;
var
  rec: TSQLBooking;
begin
  rec := GetBooking(Attendee.Name, Attendee.FirstName);
  try
    result := brDuplicatedInfo;
    if rec.IDValue <> 0 then
      exit;
    rec.Name := Attendee.Name;
    rec.FirstName := Attendee.FirstName;
    if Days <> nil then
      rec.Sessions := _Json(ObjArrayToJSON(Days), JSON_OPTIONS_FAST_EXTENDED);
    RegNum := fRest.Add(rec, true);
    result := brWriteFailure;
    if RegNum = 0 then
      exit;
    result := brSuccess;
  finally
    rec.Free;
  end;
end;


end.