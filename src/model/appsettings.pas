unit appsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLRead, XMLWrite, DOM;

type

  { TAppSettings }

  TAppSettings = class
  private
    const
      APP_NAME = 'CookieBannerGenerator';
      SETTINGS_FILENAME = 'settings.xml';
      XML_ROOT = 'xml';
    class var
      FLang: string;
      FFirstStart: Boolean;
      FRecentFiles: TStringList;
    class function GetSettingsPath: string;
    class procedure CreateTextElement(Root: TDOMNode; const NodeName, Text: string);
    class function GetSetting(Doc: TXMLDocument; const SettingName, DefaultValue: string): string;
  public
    class property Lang: string read FLang write FLang;
    class property FirstStart: Boolean read FFirstStart write FFirstStart;
    class constructor Create;
    class destructor Destroy;
    class procedure Save;
    class procedure Load;
    class procedure Init;
  end;

implementation

{ TAppSettings }

class function TAppSettings.GetSettingsPath: string;
begin
  Result := GetAppConfigDir(False);
  if not DirectoryExists(Result) then
    CreateDir(Result);
  Result := IncludeTrailingPathDelimiter(Result) + SETTINGS_FILENAME;
end;

class procedure TAppSettings.CreateTextElement(Root: TDOMNode; const NodeName, Text: string);
var
  Node: TDOMNode;
begin
  Node := Root.OwnerDocument.CreateElement(NodeName);
  Node.TextContent := Text;
  Root.AppendChild(Node);
end;

class function TAppSettings.GetSetting(Doc: TXMLDocument; const SettingName, DefaultValue: string): string;
var
  Node: TDOMNode;
begin
  Node := Doc.DocumentElement.FindNode(SettingName);
  if Assigned(Node) then
    Result := Node.TextContent
  else
    Result := DefaultValue;
end;

class constructor TAppSettings.Create;
begin
  FRecentFiles := TStringList.Create;
end;

class destructor TAppSettings.Destroy;
begin
  FRecentFiles.Free;
end;

class procedure TAppSettings.Save;
var
  Doc: TXMLDocument;
  Root: TDOMNode;
begin
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement(XML_ROOT);
    Doc.AppendChild(Root);
    Root := Doc.DocumentElement;

    CreateTextElement(Root, 'Lang', FLang);
    CreateTextElement(Root, 'FirstStart', BoolToStr(FFirstStart, True));

    WriteXMLFile(Doc, GetSettingsPath);
  finally
    Doc.Free;
  end;
end;

class procedure TAppSettings.Load;
var
  Doc: TXMLDocument;
begin
  if not FileExists(GetSettingsPath) then
  begin
    Init;
    Exit;
  end;

  ReadXMLFile(Doc, GetSettingsPath);
  try
    FLang := GetSetting(Doc, 'Lang', 'DefaultLanguageCode');
    FFirstStart := StrToBoolDef(GetSetting(Doc, 'FirstStart', 'True'), True);
  finally
    Doc.Free;
  end;
end;

class procedure TAppSettings.Init;
begin
  FLang := 'DefaultLanguageCode';
  FFirstStart := True;
  Save;
end;

end.

