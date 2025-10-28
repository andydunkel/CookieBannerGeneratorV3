unit datamodel;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, XMLHelper, category, language;

type
  { TProject }
  TProject = class
  private
    FCategories: TCategoryList;
    FLanguages: TLanguageList;
    procedure ClearDocument;
    procedure AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
  public
    ProjectName: UnicodeString;
    Comment: UnicodeString;
    SelectedLanguage: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure Save(const FileName: String);
    procedure Load(const FileName: String);
    procedure Clear;

    property Categories: TCategoryList read FCategories;
    property Languages: TLanguageList read FLanguages;
  end;

implementation

{ TProject }

constructor TProject.Create;
begin
  inherited Create;
  FCategories := TCategoryList.Create(True); // True = owns objects
  FLanguages := TLanguageList.Create; // Already owns objects by default

  ProjectName := '';
  Comment := '';
  SelectedLanguage := 0;
end;

destructor TProject.Destroy;
begin
  FreeAndNil(FCategories);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TProject.ClearDocument;
begin
  ProjectName := '';
  Comment := '';
  SelectedLanguage := 0;
  FCategories.Clear;
  FLanguages.Clear;
  // Reset default language
  FLanguages.DefaultLanguage := 'en';
end;

procedure TProject.Clear;
begin
  ClearDocument;
end;

procedure TProject.AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
var
  NewNode: TDOMNode;
begin
  NewNode := ParentNode.OwnerDocument.CreateElement(NodeName);
  NewNode.TextContent := NodeValue;
  ParentNode.AppendChild(NewNode);
end;

procedure TProject.Save(const FileName: String);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('Project');
    Doc.AppendChild(RootNode);

    AddTextNode(RootNode, 'FileVersion', '1.0');
    AddTextNode(RootNode, 'ProjectName', ProjectName);
    AddTextNode(RootNode, 'Comment', Comment);
    AddTextNode(RootNode, 'SelectedLanguage', IntToStr(SelectedLanguage));

    // Save categories and languages
    FCategories.SaveToXML(RootNode);
    FLanguages.SaveToXML(RootNode);

    WriteXMLFile(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

procedure TProject.Load(const FileName: String);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);

  ReadXMLFile(Doc, FileName);
  try
    RootNode := Doc.DocumentElement;

    if RootNode.NodeName <> 'Project' then
      raise Exception.Create('Invalid project file format');

    // Load all fields
    ProjectName := TXMLHelper.GetXML('ProjectName', RootNode);
    Comment := TXMLHelper.GetXML('Comment', RootNode);

    try
      SelectedLanguage := StrToInt(TXMLHelper.GetXML('SelectedLanguage', RootNode));
    except
      SelectedLanguage := 0;
    end;

    // Load categories and languages
    FCategories.LoadFromXML(RootNode);
    FLanguages.LoadFromXML(RootNode);
  finally
    Doc.Free;
  end;
end;

end.
