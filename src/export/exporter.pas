unit Exporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, datamodel, CookieBannerGeneratorLogicExport, FileSystemUtils;

type
  { Exporter class }
  TExporter = class
  private
    FDataModel: TProject;

    function GenerateBlocks(Blocks: TBlockList): string;
  public
    constructor Create(dataModel: TProject);
    function GenerateJavaScript: string;
    function GenerateLanguages: string;
    class function Encode(const s: string): string;
    procedure ExportJavaScript(const fileName: string);
    procedure ExportAdditionalFiles(const fileName: string);
  end;

implementation

{ TExporter }

constructor TExporter.Create(dataModel: TProject);
begin
  FDataModel := dataModel;
end;

function TExporter.GenerateJavaScript: string;
var
  s, blocks, lang: string;
begin
  s := TFileSystemUtils.LoadTextFromAppPath('cookieconsent-init.js');

  // Replace the placeholders with the actual values
  s := StringReplace(s, '#cookie_name#', Encode(FDataModel.CommonSettings.CookieName), [rfReplaceAll]);
//TODO  s := StringReplace(s, '#current_lang#', Encode(FDataModel.), [rfReplaceAll]);
  s := StringReplace(s, '#cookie_expiration#', '365', [rfReplaceAll]); //TODO parameter
  s := StringReplace(s, '#revision#', '0', [rfReplaceAll]); //TODO parameter
  s := StringReplace(s, '#remove_cookie_tables#', BoolToStr(FDataModel.CommonSettings.RemoveCookieTables, True).ToLower, [rfReplaceAll]);
  s := StringReplace(s, '#delay#', FDataModel.CommonSettings.Delay, [rfReplaceAll]);

  // Consent modal
  s := StringReplace(s, '#consent_layout#', FDataModel.Design.ConsentLayout, [rfReplaceAll]);
  s := StringReplace(s, '#consent_position#', FDataModel.Design.ConsentPos, [rfReplaceAll]);
  s := StringReplace(s, '#consent_transition#', FDataModel.Design.ConsentTransition, [rfReplaceAll]);

  lang:= GenerateLanguages();
  s := StringReplace(s, '#languages#', GenerateLanguages(), [rfReplaceAll]);

end;

function TExporter.GenerateLanguages(): string;
var
  Template, Html, CurrentHtml, Blocks: String;
  CurrentCookieLang: TCookieLanguage;
  I: Integer;
begin
  Template:= TFileSystemUtils.LoadTextFromAppPath('lang.js');
  Html:= '';
  CurrentHtml:= '';
  Blocks:= '';

  for I:= 0 to FDataModel.CookieLanguages.Count - 1 do
  begin
    CurrentHtml:= Template;
    CurrentCookieLang:= FDataModel.CookieLanguages[I];


    // Blocks
    //Blocks := GenerateBlocks;
    //CurrentHtml := StringReplace(CurrentHtml, '{#blocks#}', blocks, [rfReplaceAll]);
    //Result := Html;


    Html:= Html + CurrentHtml + #13#10;
  end;
end;

function TExporter.GenerateBlocks(Blocks: TBlockList): string;
var
  block: TBlock;
  generator: TABlockGenerator;
begin
  Result := '';

  {
  for block in FDataModel.Blocks do
  begin
    generator := TBlockGeneratorFactory.CreateBlockGenerator(block);
    try
      Result := Result + generator.GenerateBlock(block);
    finally
      generator.Free;
    end;
  end;
  }
end;

class function TExporter.Encode(const s: string): string;
begin
  // Implement string encoding for JSON content
  // Note: Object Pascal does not have a direct equivalent of HttpUtility.JavaScriptStringEncode
  // You may need to write a custom function for this purpose
end;

procedure TExporter.ExportJavaScript(const fileName: string);
var
  s: string;
  stream: TFileStream;
  stringList: TStringList;
begin
  s := GenerateJavaScript;
  stringList := TStringList.Create;
  try
    stringList.Text := s;
    stream := TFileStream.Create(fileName, fmCreate);
    try
      stringList.SaveToStream(stream);
    finally
      stream.Free;
    end;
  finally
    stringList.Free;
  end;
end;

procedure TExporter.ExportAdditionalFiles(const fileName: string);
var
  folder, cssFile, jsFile, cssSourceFile, jsSourceFile: string;
begin
  folder := ExtractFileDir(fileName);

  if folder = '' then
    Exit;

  // Copy css and js
  // ... (Similar operations as in the C# code)
end;

end.

end.

