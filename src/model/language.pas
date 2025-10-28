unit language;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, laz2_DOM, XMLHelper;

type
  { Forward declarations }
  TSection = class;
  TSectionList = class;
  TConsentModal = class;
  TPreferencesModal = class;
  TTranslation = class;
  TLanguage = class;
  TLanguageList = class;

  { TSection - Represents a section in the preferences modal }
  TSection = class
  private
    FTitle: String;
    FDescription: String;
    FLinkedCategory: String;
  public
    constructor Create(const ATitle: String = ''; const ADescription: String = ''; const ALinkedCategory: String = '');

    property Title: String read FTitle write FTitle;
    property Description: String read FDescription write FDescription;
    property LinkedCategory: String read FLinkedCategory write FLinkedCategory;
  end;

  { TSectionList - List for managing sections }
  TSectionList = class(specialize TFPGObjectList<TSection>)
  private
    function GetSection(Index: Integer): TSection;
  public
    function Add(const ATitle: String = ''; const ADescription: String = ''; const ALinkedCategory: String = ''): TSection;
    procedure MoveUp(Index: Integer);
    procedure MoveDown(Index: Integer);

    procedure SaveToXML(ParentNode: TDOMNode);
    procedure LoadFromXML(ParentNode: TDOMNode);

    property Sections[Index: Integer]: TSection read GetSection; default;
  end;

  { TConsentModal - Consent modal texts }
  TConsentModal = class
  private
    FTitle: String;
    FDescription: String;
    FAcceptAllBtn: String;
    FAcceptNecessaryBtn: String;
    FShowPreferencesBtn: String;
    FFooter: String;
  public
    constructor Create;

    property Title: String read FTitle write FTitle;
    property Description: String read FDescription write FDescription;
    property AcceptAllBtn: String read FAcceptAllBtn write FAcceptAllBtn;
    property AcceptNecessaryBtn: String read FAcceptNecessaryBtn write FAcceptNecessaryBtn;
    property ShowPreferencesBtn: String read FShowPreferencesBtn write FShowPreferencesBtn;
    property Footer: String read FFooter write FFooter;
  end;

  { TPreferencesModal - Preferences modal texts }
  TPreferencesModal = class
  private
    FTitle: String;
    FAcceptAllBtn: String;
    FAcceptNecessaryBtn: String;
    FSavePreferencesBtn: String;
    FCloseIconLabel: String;
    FServiceCounterLabel: String;
    FSections: TSectionList;
  public
    constructor Create;
    destructor Destroy; override;

    property Title: String read FTitle write FTitle;
    property AcceptAllBtn: String read FAcceptAllBtn write FAcceptAllBtn;
    property AcceptNecessaryBtn: String read FAcceptNecessaryBtn write FAcceptNecessaryBtn;
    property SavePreferencesBtn: String read FSavePreferencesBtn write FSavePreferencesBtn;
    property CloseIconLabel: String read FCloseIconLabel write FCloseIconLabel;
    property ServiceCounterLabel: String read FServiceCounterLabel write FServiceCounterLabel;
    property Sections: TSectionList read FSections;
  end;

  { TTranslation - Translation for a specific language }
  TTranslation = class
  private
    FConsentModal: TConsentModal;
    FPreferencesModal: TPreferencesModal;
  public
    constructor Create;
    destructor Destroy; override;

    property ConsentModal: TConsentModal read FConsentModal;
    property PreferencesModal: TPreferencesModal read FPreferencesModal;
  end;

  { TLanguage - Language with its code and translation }
  TLanguage = class
  private
    FCode: String;
    FTranslation: TTranslation;
  public
    constructor Create(const ACode: String);
    destructor Destroy; override;

    property Code: String read FCode write FCode;
    property Translation: TTranslation read FTranslation;
  end;

  { TLanguageList - List for managing languages }
  TLanguageList = class(specialize TFPGObjectList<TLanguage>)
  private
    FDefaultLanguage: String;
    function GetLanguage(Index: Integer): TLanguage;
  public
    constructor Create;

    function Add(const ACode: String): TLanguage;
    function Find(const ACode: String): TLanguage;
    function Exists(const ACode: String): Boolean;
    procedure Remove(const ACode: String);
    procedure MoveUp(Index: Integer);
    procedure MoveDown(Index: Integer);

    procedure SaveToXML(ParentNode: TDOMNode);
    procedure LoadFromXML(ParentNode: TDOMNode);

    property DefaultLanguage: String read FDefaultLanguage write FDefaultLanguage;
    property Languages[Index: Integer]: TLanguage read GetLanguage; default;
  end;

implementation

{ TSection }

constructor TSection.Create(const ATitle: String = ''; const ADescription: String = ''; const ALinkedCategory: String = '');
begin
  inherited Create;
  FTitle := ATitle;
  FDescription := ADescription;
  FLinkedCategory := ALinkedCategory;
end;

{ TSectionList }

function TSectionList.GetSection(Index: Integer): TSection;
begin
  Result := inherited Items[Index];
end;

function TSectionList.Add(const ATitle: String = ''; const ADescription: String = ''; const ALinkedCategory: String = ''): TSection;
begin
  Result := TSection.Create(ATitle, ADescription, ALinkedCategory);
  inherited Add(Result);
end;

procedure TSectionList.MoveUp(Index: Integer);
begin
  if (Index > 0) and (Index < Count) then
    Exchange(Index, Index - 1);
end;

procedure TSectionList.MoveDown(Index: Integer);
begin
  if (Index >= 0) and (Index < Count - 1) then
    Exchange(Index, Index + 1);
end;

procedure TSectionList.SaveToXML(ParentNode: TDOMNode);
var
  SectionsNode, SectionNode: TDOMNode;
  i: Integer;
  Section: TSection;
begin
  if Count = 0 then
    Exit;

  SectionsNode := ParentNode.OwnerDocument.CreateElement('Sections');
  ParentNode.AppendChild(SectionsNode);

  for i := 0 to Count - 1 do
  begin
    Section := Items[i];
    SectionNode := ParentNode.OwnerDocument.CreateElement('Section');
    SectionsNode.AppendChild(SectionNode);

    with SectionNode do
    begin
      AppendChild(OwnerDocument.CreateElement('Title')).TextContent := Section.Title;
      AppendChild(OwnerDocument.CreateElement('Description')).TextContent := Section.Description;
      if Section.LinkedCategory <> '' then
        AppendChild(OwnerDocument.CreateElement('LinkedCategory')).TextContent := Section.LinkedCategory;
    end;
  end;
end;

procedure TSectionList.LoadFromXML(ParentNode: TDOMNode);
var
  SectionsNode, SectionNode: TDOMNode;
  Title, Description, LinkedCategory: String;
begin
  Clear;

  SectionsNode := ParentNode.FindNode('Sections');
  if not Assigned(SectionsNode) then
    Exit;

  SectionNode := SectionsNode.FirstChild;
  while Assigned(SectionNode) do
  begin
    if SectionNode.NodeName = 'Section' then
    begin
      Title := TXMLHelper.GetXML('Title', SectionNode);
      Description := TXMLHelper.GetXML('Description', SectionNode);
      LinkedCategory := TXMLHelper.GetXML('LinkedCategory', SectionNode);

      Add(Title, Description, LinkedCategory);
    end;

    SectionNode := SectionNode.NextSibling;
  end;
end;

{ TConsentModal }

constructor TConsentModal.Create;
begin
  inherited Create;
  // Initialize with default/empty values
  FTitle := '';
  FDescription := '';
  FAcceptAllBtn := '';
  FAcceptNecessaryBtn := '';
  FShowPreferencesBtn := '';
  FFooter := '';
end;

{ TPreferencesModal }

constructor TPreferencesModal.Create;
begin
  inherited Create;
  FSections := TSectionList.Create(True); // Owns objects

  // Initialize with default/empty values
  FTitle := '';
  FAcceptAllBtn := '';
  FAcceptNecessaryBtn := '';
  FSavePreferencesBtn := '';
  FCloseIconLabel := '';
  FServiceCounterLabel := '';
end;

destructor TPreferencesModal.Destroy;
begin
  FreeAndNil(FSections);
  inherited Destroy;
end;

{ TTranslation }

constructor TTranslation.Create;
begin
  inherited Create;
  FConsentModal := TConsentModal.Create;
  FPreferencesModal := TPreferencesModal.Create;
end;

destructor TTranslation.Destroy;
begin
  FreeAndNil(FConsentModal);
  FreeAndNil(FPreferencesModal);
  inherited Destroy;
end;

{ TLanguage }

constructor TLanguage.Create(const ACode: String);
begin
  inherited Create;
  FCode := ACode;
  FTranslation := TTranslation.Create;
end;

destructor TLanguage.Destroy;
begin
  FreeAndNil(FTranslation);
  inherited Destroy;
end;

{ TLanguageList }

constructor TLanguageList.Create;
begin
  inherited Create(True); // Owns objects
  FDefaultLanguage := 'en';
end;

function TLanguageList.GetLanguage(Index: Integer): TLanguage;
begin
  Result := inherited Items[Index];
end;

function TLanguageList.Add(const ACode: String): TLanguage;
begin
  // Check if language already exists
  Result := Find(ACode);

  if not Assigned(Result) then
  begin
    // Create new language
    Result := TLanguage.Create(ACode);
    inherited Add(Result);
  end;
end;

function TLanguageList.Find(const ACode: String): TLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].Code, ACode) = 0 then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TLanguageList.Exists(const ACode: String): Boolean;
begin
  Result := Assigned(Find(ACode));
end;

procedure TLanguageList.Remove(const ACode: String);
var
  Lang: TLanguage;
begin
  Lang := Find(ACode);
  if Assigned(Lang) then
    inherited Remove(Lang);
end;

procedure TLanguageList.MoveUp(Index: Integer);
begin
  if (Index > 0) and (Index < Count) then
    Exchange(Index, Index - 1);
end;

procedure TLanguageList.MoveDown(Index: Integer);
begin
  if (Index >= 0) and (Index < Count - 1) then
    Exchange(Index, Index + 1);
end;

procedure TLanguageList.SaveToXML(ParentNode: TDOMNode);
var
  LanguagesNode, LanguageNode, TranslationNode: TDOMNode;
  ConsentModalNode, PreferencesModalNode: TDOMNode;
  i: Integer;
  Lang: TLanguage;
begin
  // Save default language
  ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement('DefaultLanguage')).TextContent := FDefaultLanguage;

  if Count = 0 then
    Exit;

  LanguagesNode := ParentNode.OwnerDocument.CreateElement('Languages');
  ParentNode.AppendChild(LanguagesNode);

  for i := 0 to Count - 1 do
  begin
    Lang := Items[i];
    LanguageNode := ParentNode.OwnerDocument.CreateElement('Language');
    LanguagesNode.AppendChild(LanguageNode);

    // Language code
    LanguageNode.AppendChild(LanguageNode.OwnerDocument.CreateElement('Code')).TextContent := Lang.Code;

    // Translation node
    TranslationNode := LanguageNode.OwnerDocument.CreateElement('Translation');
    LanguageNode.AppendChild(TranslationNode);

    // Consent Modal
    ConsentModalNode := TranslationNode.OwnerDocument.CreateElement('ConsentModal');
    TranslationNode.AppendChild(ConsentModalNode);
    with Lang.Translation.ConsentModal do
    begin
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('Title')).TextContent := Title;
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('Description')).TextContent := Description;
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('AcceptAllBtn')).TextContent := AcceptAllBtn;
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('AcceptNecessaryBtn')).TextContent := AcceptNecessaryBtn;
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('ShowPreferencesBtn')).TextContent := ShowPreferencesBtn;
      ConsentModalNode.AppendChild(ConsentModalNode.OwnerDocument.CreateElement('Footer')).TextContent := Footer;
    end;

    // Preferences Modal
    PreferencesModalNode := TranslationNode.OwnerDocument.CreateElement('PreferencesModal');
    TranslationNode.AppendChild(PreferencesModalNode);
    with Lang.Translation.PreferencesModal do
    begin
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('Title')).TextContent := Title;
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('AcceptAllBtn')).TextContent := AcceptAllBtn;
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('AcceptNecessaryBtn')).TextContent := AcceptNecessaryBtn;
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('SavePreferencesBtn')).TextContent := SavePreferencesBtn;
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('CloseIconLabel')).TextContent := CloseIconLabel;
      PreferencesModalNode.AppendChild(PreferencesModalNode.OwnerDocument.CreateElement('ServiceCounterLabel')).TextContent := ServiceCounterLabel;

      // Save sections
      Sections.SaveToXML(PreferencesModalNode);
    end;
  end;
end;

procedure TLanguageList.LoadFromXML(ParentNode: TDOMNode);
var
  LanguagesNode, LanguageNode, TranslationNode: TDOMNode;
  ConsentModalNode, PreferencesModalNode: TDOMNode;
  Code: String;
  Lang: TLanguage;
begin
  Clear;

  // Load default language
  FDefaultLanguage := TXMLHelper.GetXML('DefaultLanguage', ParentNode);
  if FDefaultLanguage = '' then
    FDefaultLanguage := 'en';

  LanguagesNode := ParentNode.FindNode('Languages');
  if not Assigned(LanguagesNode) then
    Exit;

  LanguageNode := LanguagesNode.FirstChild;
  while Assigned(LanguageNode) do
  begin
    if LanguageNode.NodeName = 'Language' then
    begin
      Code := TXMLHelper.GetXML('Code', LanguageNode);

      if Code <> '' then
      begin
        Lang := Add(Code);

        TranslationNode := LanguageNode.FindNode('Translation');
        if Assigned(TranslationNode) then
        begin
          // Load Consent Modal
          ConsentModalNode := TranslationNode.FindNode('ConsentModal');
          if Assigned(ConsentModalNode) then
          begin
            with Lang.Translation.ConsentModal do
            begin
              Title := TXMLHelper.GetXML('Title', ConsentModalNode);
              Description := TXMLHelper.GetXML('Description', ConsentModalNode);
              AcceptAllBtn := TXMLHelper.GetXML('AcceptAllBtn', ConsentModalNode);
              AcceptNecessaryBtn := TXMLHelper.GetXML('AcceptNecessaryBtn', ConsentModalNode);
              ShowPreferencesBtn := TXMLHelper.GetXML('ShowPreferencesBtn', ConsentModalNode);
              Footer := TXMLHelper.GetXML('Footer', ConsentModalNode);
            end;
          end;

          // Load Preferences Modal
          PreferencesModalNode := TranslationNode.FindNode('PreferencesModal');
          if Assigned(PreferencesModalNode) then
          begin
            with Lang.Translation.PreferencesModal do
            begin
              Title := TXMLHelper.GetXML('Title', PreferencesModalNode);
              AcceptAllBtn := TXMLHelper.GetXML('AcceptAllBtn', PreferencesModalNode);
              AcceptNecessaryBtn := TXMLHelper.GetXML('AcceptNecessaryBtn', PreferencesModalNode);
              SavePreferencesBtn := TXMLHelper.GetXML('SavePreferencesBtn', PreferencesModalNode);
              CloseIconLabel := TXMLHelper.GetXML('CloseIconLabel', PreferencesModalNode);
              ServiceCounterLabel := TXMLHelper.GetXML('ServiceCounterLabel', PreferencesModalNode);

              // Load sections
              Sections.LoadFromXML(PreferencesModalNode);
            end;
          end;
        end;
      end;
    end;

    LanguageNode := LanguageNode.NextSibling;
  end;
end;

end.
