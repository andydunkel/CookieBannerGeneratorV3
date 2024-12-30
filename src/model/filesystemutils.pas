unit FileSystemUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFileSystemUtils }

  TFileSystemUtils = class
  public
    class function GetApplicationPath: string;
    class function GetAppDataPath(const AppName: string): string; overload;
    class function LoadTextFromAppPath(const FileName: string): string;
    class function GetTempFolder: string;
  end;

implementation

{ TFileSystemUtils }

class function TFileSystemUtils.GetApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

class function TFileSystemUtils.GetAppDataPath(const AppName: string): string;
begin
  Result := GetAppConfigDir(False);
  Result := IncludeTrailingPathDelimiter(Result) + AppName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;


class function TFileSystemUtils.LoadTextFromAppPath(const FileName: string): string;
var
  FilePath: string;
  StringList: TStringList;
begin
  FilePath := IncludeTrailingPathDelimiter(GetApplicationPath) + FileName;
  if not FileExists(FilePath) then
    raise Exception.CreateFmt('File %s not found in %s', [FileName, GetApplicationPath]);

  // Load text file and return result
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FilePath);
    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

class function TFileSystemUtils.GetTempFolder: string;
begin
  Result := GetTempDir;
end;

end.

