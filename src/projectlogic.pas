unit ProjectLogic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, datamodel, globalfunctions;

const
  PROG_NAME = 'Cookie Banner Generator';
  PROG_PATH = 'CookieBannerGenerator';
  PROG_VERSION = '2.0.0';

type

  { TProjectLogic }

  TProjectLogic = class
  private
    class var FInstance: TProjectLogic;

  public
    ProjectFileName: String;
    Dirty: Boolean;
    Model: TProject;
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: TProjectLogic;
    procedure New();
    procedure Save();
    procedure SaveAs(FileName: String);
    procedure Open(FileName: String);
  end;

implementation

constructor TProjectLogic.Create;
begin
  inherited Create;
  Model:= TProject.Create;
  Dirty:= true;
  New();
end;

destructor TProjectLogic.Destroy;
begin
  FreeAndNil(Model);
  inherited Destroy;
end;

class function TProjectLogic.GetInstance: TProjectLogic;
begin
  if not Assigned(FInstance) then
    FInstance := TProjectLogic.Create;
  Result := FInstance;
end;

procedure TProjectLogic.New;
var
  FilePath: String;
begin
  FreeAndNil(Model);
  Model:= TProject.Create;

  //load default file from data dir
  FilePath:= GetApplicationFile('default.cookiebanner');
  FreeAndNil(Model);
  Model:= TProject.Create;
  Model.Load(FilePath);
end;

procedure TProjectLogic.Save;
begin
  if ProjectFileName <> '' then
  begin
    SaveAs(Self.ProjectFileName);
  end;
end;

procedure TProjectLogic.SaveAs(FileName: String);
begin
  Self.ProjectFileName:= FileName;
  Model.Save(Self.ProjectFileName);
end;

procedure TProjectLogic.Open(FileName: String);
begin
  if FileExists(FileName) then
  begin
    Self.ProjectFileName:= FileName;
    FreeAndNil(Model);
    Model:= TProject.Create;
    Model.Load(FileName);

  end;
end;

initialization

finalization
  FreeAndNil(TProjectLogic.FInstance);


end.

