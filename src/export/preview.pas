unit Preview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, datamodel, FileSystemUtils, ProjectLogic, FileUtil, lclintf, Exporter;

type

  { Class for generating the preview to open in a browser }
  TPreview = class
  public
    procedure GeneratePreview(model: TProject; folder: String = '');
  end;

implementation

{ TPreview }

procedure TPreview.GeneratePreview(model: TProject; folder: String);
var
  tempFolder, previewFile, sourceFile, cssFile, jsFile, cssSourceFile, jsSourceFile, jsPath: string;
  Exporter: TExporter;
begin
  tempFolder := folder;

  if tempFolder = '' then
  begin
    tempFolder := IncludeTrailingPathDelimiter(GetTempDir) + PROG_PATH;
  end;

  if not DirectoryExists(tempFolder) then
  begin
    CreateDir(tempFolder);
  end;

  // Copy the preview file
  previewFile := ConcatPaths([tempFolder, 'preview.html']);
  sourceFile := ConcatPaths([TFileSystemUtils.GetApplicationPath, 'preview.html']);
  CopyFile(sourceFile, previewFile, [cffOverwriteFile]);

  // Copy css and js
  cssFile := ConcatPaths([tempFolder, 'cookieconsent.min.css']);
  jsFile := ConcatPaths([tempFolder, 'cookieconsent.min.js']);
  cssSourceFile := ConcatPaths([TFileSystemUtils.GetApplicationPath, 'cookieconsent.min.css']);
  jsSourceFile := ConcatPaths([TFileSystemUtils.GetApplicationPath, 'cookieconsent.min.js']);
  CopyFile(cssSourceFile, cssFile, [cffOverwriteFile]);
  CopyFile(jsSourceFile, jsFile, [cffOverwriteFile]);

  // Generate the javascript
  Exporter := TExporter.Create(model);
  try
    jsPath := ConcatPaths([tempFolder, 'cookieconsent-init.js']);
    Exporter.ExportJavaScript(jsPath);
  finally
    FreeAndNil(Exporter);
  end;

  // Start the process
  if folder = '' then
  begin
    // Open the preview in the browser
    OpenDocument(previewFile);
  end
  else
  begin
    // Regular export, open the explorer
    OpenDocument(tempFolder); // OpenDocument is a cross-platform way to open a folder
  end;
end;

end.

