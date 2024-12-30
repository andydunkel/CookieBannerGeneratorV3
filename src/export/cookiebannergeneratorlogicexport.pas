unit CookieBannerGeneratorLogicExport;

{$mode objfpc}{$H+}

interface

uses
  FileSystemUtils, datamodel, Classes, SysUtils;

type

  { Base class for all block generators }
  TABlockGenerator = class abstract
  public
    { Generates the block }
    function GenerateBlock(block: TBlock): string; virtual; abstract;

  protected
    { Opens a template file from the res folder }
    function LoadTemplate(fileName: string): string;
  end;

type

  { Standard block generator }
   TBlockGenerator = class(TABlockGenerator)
   public
     function GenerateBlock(block: TBlock): string; override;

   private
     function GenerateCookieTable(block: TBlock): string;
     function GenerateCookieTableEntry(cookieTableEntry: TCookieTableEntry): string;
   end;

type

  { Generator for simple blocks, only text with title }
  TSimpleBlockGenerator = class(TABlockGenerator)
  public
    function GenerateBlock(block: TBlock): string; override;
  end;

type

  { BlockGeneratorFactory }
  TBlockGeneratorFactory = class
  public
    class function CreateBlockGenerator(block: TBlock): TABlockGenerator; static;
  end;



implementation

uses
  Exporter;

{ TABlockGenerator }

function TABlockGenerator.LoadTemplate(fileName: string): string;
var
  filePath: string;
begin
  filePath := ConcatPaths(['res', fileName]);
  Result := TFileSystemUtils.LoadTextFromAppPath(filePath);
end;

{ TBlockGenerator }

function TBlockGenerator.GenerateBlock(block: TBlock): string;
var
  s, cookieTable: string;
begin
  s := LoadTemplate('block.js');
  s := StringReplace(s, '{#title#}', block.Title, [rfReplaceAll]);
  s := StringReplace(s, '{#description#}', TExporter.Encode(block.Description), [rfReplaceAll]);

//  s := StringReplace(s, '{#value#}', block.Value, [rfReplaceAll]);
  s := StringReplace(s, '{#enabled#}', BoolToStr(block.Enabled, true), [rfReplaceAll]);
  s := StringReplace(s, '{#readonly#}', BoolToStr(block.ReadOnly, true), [rfReplaceAll]);

  cookieTable := GenerateCookieTable(block);
  s := StringReplace(s, '{#cookie_table#}', cookieTable, [rfReplaceAll]);

  s := s + LineEnding;

  Result := s;
end;

function TBlockGenerator.GenerateCookieTable(block: TBlock): string;
var
  s, template: string;
  cookieTableEntry: TCookieTableEntry;
begin
  s := '';
  template := LoadTemplate('table.js');

  if block.CookieTableEntries.Count = 0 then
    Exit('');

  for cookieTableEntry in block.CookieTableEntries do
  begin
    s := s + GenerateCookieTableEntry(cookieTableEntry);
  end;

  template := StringReplace(template, '{#entries#}', s, [rfReplaceAll]);
  Result := template;
end;

function TBlockGenerator.GenerateCookieTableEntry(cookieTableEntry: TCookieTableEntry): string;
var
  s: string;
begin
  s := LoadTemplate('tableentry.js');

  {
  if not AppLogic.Instance.Model.SettingsModal.ShowCol3 then
    s := StringReplace(s, "col3: '{#col3#}',", "", [rfReplaceAll]);

  if not AppLogic.Instance.Model.SettingsModal.ShowCol4 then
    s := StringReplace(s, "col4: '{#col4#}',", "", [rfReplaceAll]);

  s := StringReplace(s, '{#col1#}', Exporter.Encode(cookieTableEntry.Col1), [rfReplaceAll]);
  s := StringReplace(s, '{#col2#}', Exporter.Encode(cookieTableEntry.Col2), [rfReplaceAll]);
  s := StringReplace(s, '{#col3#}', Exporter.Encode(cookieTableEntry.Col3), [rfReplaceAll]);
  s := StringReplace(s, '{#col4#}', Exporter.Encode(cookieTableEntry.Col4), [rfReplaceAll]);
  s := s + LineEnding;
   }
  Result := s;
end;

{ TSimpleBlockGenerator }

function TSimpleBlockGenerator.GenerateBlock(block: TBlock): string;
var
  s: string;
begin
  s := LoadTemplate('simpleblock.js');
  s := StringReplace(s, '{#title#}', block.Title, [rfReplaceAll]);
  s := StringReplace(s, '{#description#}', TExporter.Encode(block.Description), [rfReplaceAll]);
  s := s + LineEnding;

  Result := s;
end;


{ TBlockGeneratorFactory }

class function TBlockGeneratorFactory.CreateBlockGenerator(block: TBlock): TABlockGenerator;
begin
  if block.IsToggle then
    Result := TSimpleBlockGenerator.Create
  else
    Result := TBlockGenerator.Create;
end;

end.

