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
    
    // Design properties for Consent Modal
    DesignConsentModalLayout: String;
    DesignConsentModalPosition: String;
    DesignConsentModalFlipButtons: Boolean;
    DesignConsentModalEqualWeightButtons: Boolean;
    
    // Design properties for Preference Modal
    DesignPrefModalLayout: String;
    DesignPrefModalPosition: String;
    DesignPrefModalFlipButtons: Boolean;
    DesignPrefModalEqualWeightButtons: Boolean;
    
    // General design properties
    DesignEnableDarkMode: Boolean;
    DesignDisablePageInteraction: Boolean;
    DesignDisableTransitions: Boolean;
    
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
  
  // Initialize design properties with defaults
  DesignConsentModalLayout := '';
  DesignConsentModalPosition := '';
  DesignConsentModalFlipButtons := False;
  DesignConsentModalEqualWeightButtons := False;
  
  DesignPrefModalLayout := '';
  DesignPrefModalPosition := '';
  DesignPrefModalFlipButtons := False;
  DesignPrefModalEqualWeightButtons := False;
  
  DesignEnableDarkMode := False;
  DesignDisablePageInteraction := False;
  DesignDisableTransitions := False;
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
  
  // Reset design properties
  DesignConsentModalLayout := '';
  DesignConsentModalPosition := '';
  DesignConsentModalFlipButtons := False;
  DesignConsentModalEqualWeightButtons := False;
  
  DesignPrefModalLayout := '';
  DesignPrefModalPosition := '';
  DesignPrefModalFlipButtons := False;
  DesignPrefModalEqualWeightButtons := False;
  
  DesignEnableDarkMode := False;
  DesignDisablePageInteraction := False;
  DesignDisableTransitions := False;
  
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
    
    // Save Consent Modal design properties
    AddTextNode(RootNode, 'DesignConsentModalLayout', DesignConsentModalLayout);
    AddTextNode(RootNode, 'DesignConsentModalPosition', DesignConsentModalPosition);
    AddTextNode(RootNode, 'DesignConsentModalFlipButtons', BoolToStr(DesignConsentModalFlipButtons, True));
    AddTextNode(RootNode, 'DesignConsentModalEqualWeightButtons', BoolToStr(DesignConsentModalEqualWeightButtons, True));
    
    // Save Preference Modal design properties
    AddTextNode(RootNode, 'DesignPrefModalLayout', DesignPrefModalLayout);
    AddTextNode(RootNode, 'DesignPrefModalPosition', DesignPrefModalPosition);
    AddTextNode(RootNode, 'DesignPrefModalFlipButtons', BoolToStr(DesignPrefModalFlipButtons, True));
    AddTextNode(RootNode, 'DesignPrefModalEqualWeightButtons', BoolToStr(DesignPrefModalEqualWeightButtons, True));
    
    // Save general design properties
    AddTextNode(RootNode, 'DesignEnableDarkMode', BoolToStr(DesignEnableDarkMode, True));
    AddTextNode(RootNode, 'DesignDisablePageInteraction', BoolToStr(DesignDisablePageInteraction, True));
    AddTextNode(RootNode, 'DesignDisableTransitions', BoolToStr(DesignDisableTransitions, True));
    
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
    
    // Load Consent Modal design properties
    DesignConsentModalLayout := TXMLHelper.GetXML('DesignConsentModalLayout', RootNode);
    DesignConsentModalPosition := TXMLHelper.GetXML('DesignConsentModalPosition', RootNode);
    DesignConsentModalFlipButtons := StrToBoolDef(TXMLHelper.GetXML('DesignConsentModalFlipButtons', RootNode), False);
    DesignConsentModalEqualWeightButtons := StrToBoolDef(TXMLHelper.GetXML('DesignConsentModalEqualWeightButtons', RootNode), False);
    
    // Load Preference Modal design properties
    DesignPrefModalLayout := TXMLHelper.GetXML('DesignPrefModalLayout', RootNode);
    DesignPrefModalPosition := TXMLHelper.GetXML('DesignPrefModalPosition', RootNode);
    DesignPrefModalFlipButtons := StrToBoolDef(TXMLHelper.GetXML('DesignPrefModalFlipButtons', RootNode), False);
    DesignPrefModalEqualWeightButtons := StrToBoolDef(TXMLHelper.GetXML('DesignPrefModalEqualWeightButtons', RootNode), False);
    
    // Load general design properties
    DesignEnableDarkMode := StrToBoolDef(TXMLHelper.GetXML('DesignEnableDarkMode', RootNode), False);
    DesignDisablePageInteraction := StrToBoolDef(TXMLHelper.GetXML('DesignDisablePageInteraction', RootNode), False);
    DesignDisableTransitions := StrToBoolDef(TXMLHelper.GetXML('DesignDisableTransitions', RootNode), False);
    
    // Load categories and languages
    FCategories.LoadFromXML(RootNode);
    FLanguages.LoadFromXML(RootNode);
  finally
    Doc.Free;
  end;
end;

end.