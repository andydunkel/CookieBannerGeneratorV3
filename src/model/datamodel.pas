unit datamodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, XMLHelper,  fgl;


type
  TSettings = record
    Title: String;
    ButtonSave: String;
    ButtonDenyAll: String;
    ButtonClose: String;
    ButtonAcceptAll: String;
    Col1: String;
    Col2: String;
    Col3: String;
    Col4: String;
  end;

  TDesign = record
    ConsentLayout: String;
    ConsentPos: String;
    ConsentPos1: String;
    ConsentTransition: String;
    SettingsLayout: String;
    SettingsPosition: String;
    SettingsTransition: String;
  end;

  TApproval = record
    Title: String;
    Description: String;
    PrimaryButton: String;
    SecondaryButton: String;
    SelectButton: String;
  end;

  TCommonSettings = record
    CookieName: string;
    Delay: string;
    HideFromBots: boolean;
    RemoveCookieTables: boolean;
    Col3Active: boolean;
    Col4Active: boolean;
  end;

  type
    
    { TCookieTableEntry }

    TCookieTableEntry = class
      Col1: string;
      Col2: string;
      Col3: string;
      Col4: string;
      IsRegEx: Boolean;
      function Clone: TCookieTableEntry;
      procedure Serialize(ParentNode: TDOMNode);
      procedure Deserialize(ParentNode: TDOMNode);
    end;

  TCookieTableList = specialize TFPGObjectList<TCookieTableEntry>;

  { TBlock }

  TBlock = class
    Title: string;
    Description: string;
    CookieTableEntries: TCookieTableList;
    IsToggle: Boolean;
    BlockType: string;
    Enabled: Boolean;
    ReadOnly: Boolean;
    constructor Create;
    destructor Destroy; override;
    function Clone: TBlock;
    procedure SerializeBlock(ParentNode: TDOMNode);
    procedure DeserializeBlock(ParentNode: TDOMNode);
  end;

  TBlockList = specialize TFPGObjectList<TBlock>;

  { TCookieLanguage }

  TCookieLanguage = class
    Language: String;
    Approval: TApproval;
    Settings: TSettings;
    Blocks: TBlockList;
    constructor Create;
    destructor Destroy; override;
    procedure MoveBlockUp(Index: Integer);
    procedure MoveBlockDown(Index: Integer);
  end;

  TCookieLanguageList = specialize TFPGObjectList<TCookieLanguage>;

  { TProject }

  TProject = class
    public
      ProjectName: UnicodeString;
      Comment: UnicodeString;
      CookieLanguages: TCookieLanguageList;
      ServiceTypes: array of String;
      SelectedLanguage: Integer;
      CommonSettings: TCommonSettings;
      Design: TDesign;
      Doc: TXMLDocument;
      constructor Create;
      destructor Destroy; override;
      procedure Save(FileName: String);
      procedure Load(FileName: String);
      function GetCurrentCookieLanguage(): TCookieLanguage;
      procedure RemoveCookieLanguage(const Language: String);
      procedure AddServiceType(const ServiceType: String);
      procedure RemoveServiceType(const ServiceType: String);
      procedure ClearServiceTypes();
      procedure AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
      function GetTextNodeContent(Node: TDOMNode): String;
      procedure UpdateLanguageList(const ListBoxItems: TStrings);
      procedure SerializeSettings(const Settings: TSettings; ParentNode: TDOMNode);
      procedure SerializeDesign(ParentNode: TDOMNode);
      procedure SerializeApproval(const Approval: TApproval; ParentNode: TDOMNode);
      procedure SerializeCommonSettings(ParentNode: TDOMNode);
      procedure DeserializeCommonSettings(ParentNode: TDOMNode);
      procedure DeserializeDesign(ParentNode: TDOMNode);
      procedure DeserializeSettings(ParentNode: TDOMNode; out Settings: TSettings);
      procedure DeserializeApproval(ParentNode: TDOMNode; out Approval: TApproval);
      procedure SerializeCookieTableEntry(const Entry: TCookieTableEntry; ParentNode: TDOMNode);
      procedure DeserializeCookieTableEntry(Node: TDOMNode; out Entry: TCookieTableEntry);
  end;


implementation


{ TProject }

constructor TProject.Create;
begin
  inherited Create;
  //create List
  CookieLanguages:= TCookieLanguageList.Create(True);
end;

destructor TProject.Destroy;
begin
  inherited Destroy;
  FreeAndNil(CookieLanguages);
end;

procedure TProject.AddTextNode(ParentNode: TDOMNode; const NodeName, NodeValue: String);
begin
  ParentNode.AppendChild(Doc.CreateElement(NodeName)).TextContent := NodeValue;
end;

procedure TProject.ClearServiceTypes();
begin
  SetLength(ServiceTypes, 0);
end;


function TProject.GetCurrentCookieLanguage(): TCookieLanguage;
begin
  Result:= CookieLanguages.Items[SelectedLanguage];
end;

procedure TProject.SerializeSettings(const Settings: TSettings; ParentNode: TDOMNode);
var
  SettingsNode: TDOMNode;
begin
  SettingsNode := Doc.CreateElement('Settings');
  ParentNode.AppendChild(SettingsNode);

  with Settings do
  begin
    AddTextNode(SettingsNode, 'Title', Title);
    AddTextNode(SettingsNode, 'ButtonSave', ButtonSave);
    AddTextNode(SettingsNode, 'ButtonDenyAll', ButtonDenyAll);
    AddTextNode(SettingsNode, 'ButtonClose', ButtonClose);
    AddTextNode(SettingsNode, 'ButtonAcceptAll', ButtonAcceptAll);
    AddTextNode(SettingsNode, 'Col1', Col1);
    AddTextNode(SettingsNode, 'Col2', Col2);
    AddTextNode(SettingsNode, 'Col3', Col3);
    AddTextNode(SettingsNode, 'Col4', Col4);
  end;
end;

procedure TProject.Save(FileName: String);
var
  RootNode, CookieLanguagesNode, CookieLanguageNode, BlocksNode,
    ServiceTypesNode, ServiceTypeNode: TDOMNode;
  i, j: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('Project');
    Doc.AppendChild(RootNode);

    AddTextNode(RootNode, 'FileVersion', '1.0');
    AddTextNode(RootNode, 'ProjectName', ProjectName);
    AddTextNode(RootNode, 'Comment', Comment);
    AddTextNode(RootNode, 'SelectedLanguage', IntToStr(SelectedLanguage));

    SerializeCommonSettings(RootNode);
    SerializeDesign(RootNode);

    // Save Service Types
    ServiceTypesNode := Doc.CreateElement('ServiceTypes');
    RootNode.AppendChild(ServiceTypesNode);

    for i := 0 to High(ServiceTypes) do
    begin
      ServiceTypeNode := Doc.CreateElement('ServiceType');
      ServiceTypesNode.AppendChild(ServiceTypeNode);
      ServiceTypeNode.TextContent := ServiceTypes[i];
    end;

    //save language specific content
    CookieLanguagesNode := Doc.CreateElement('CookieLanguages');
    RootNode.AppendChild(CookieLanguagesNode);

    for i:= 0 to CookieLanguages.Count - 1 do
    begin
      CookieLanguageNode := Doc.CreateElement('CookieLanguage');
      CookieLanguagesNode.AppendChild(CookieLanguageNode);

      with CookieLanguages.Items[i] do
      begin
        AddTextNode(CookieLanguageNode, 'Language', Language);
        SerializeSettings(Settings, CookieLanguageNode);
        SerializeApproval(Approval, CookieLanguageNode);
        BlocksNode:= Doc.CreateElement('Blocks');
        CookieLanguageNode.AppendChild(BlocksNode);

        for j:= 0 to Blocks.Count - 1 do
        begin
          Blocks[j].SerializeBlock(BlocksNode);
        end;
      end;
    end;
    WriteXMLFile(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

procedure TProject.Load(FileName: String);
var
  RootNode, CookieLanguagesNode, CookieLanguageNode, BlocksNode, BlockNode,
    ServiceTypesNode, ServiceTypeNode: TDOMNode;
  CurrentLangSettings: TCookieLanguage;
  Block: TBlock;
begin
  ReadXMLFile(Doc, FileName);
  try
    RootNode := Doc.DocumentElement;

    ProjectName := TXMLHelper.GetXML('ProjectName', RootNode);
    Comment:= TXMLHelper.GetXml('Comment', RootNode);
    SelectedLanguage:= StrToInt(TXMLHelper.GetXML('SelectedLanguage', RootNode, '0'));

    //Load common settings
    DeserializeCommonSettings(RootNode);

    //load design
    DeserializeDesign(RootNode);

    // Load service types
    ServiceTypesNode := RootNode.FindNode('ServiceTypes');
    if Assigned(ServiceTypesNode) then
    begin
      ServiceTypeNode := ServiceTypesNode.FirstChild;
      while Assigned(ServiceTypeNode) do
      begin
        if ServiceTypeNode.NodeName = 'ServiceType' then
        begin
          AddServiceType(GetTextNodeContent(ServiceTypeNode));
        end;
        ServiceTypeNode := ServiceTypeNode.NextSibling;
      end;
    end;

    //load languages
    CookieLanguagesNode:= RootNode.FindNode('CookieLanguages');
    if Assigned(CookieLanguagesNode) then
    begin
      CookieLanguageNode:= CookieLanguagesNode.FirstChild;
      while Assigned(CookieLanguageNode) do
      begin
        if CookieLanguageNode.NodeName = 'CookieLanguage' then
        begin
          //create cookie language and read data
          CurrentLangSettings:= TCookieLanguage.Create;
          CurrentLangSettings.Language:= GetTextNodeContent(CookieLanguageNode.FindNode('Language'));
          DeserializeSettings(CookieLanguageNode, CurrentLangSettings.Settings);
          DeserializeApproval(CookieLanguageNode, CurrentLangSettings.Approval);

          //load blocks
          BlocksNode:= CookieLanguageNode.FindNode('Blocks');
          if Assigned(BlocksNode) then
          begin
            BlockNode:= BlocksNode.FirstChild;
            while Assigned(BlockNode) do
            begin
              Block:= TBlock.Create;
              Block.DeserializeBlock(BlockNode);
              CurrentLangSettings.Blocks.Add(Block);
              BlockNode:= BlockNode.NextSibling;
            end;
          end;

          CookieLanguages.Add(CurrentLangSettings);
        end;
        CookieLanguageNode:= CookieLanguageNode.NextSibling;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TProject.UpdateLanguageList(const ListBoxItems: TStrings);
var
  i: Integer;
  langFound: Boolean;
  CookieLang, NewCookieLang: TCookieLanguage;
begin
  // Add new languages from ListBoxItems to CookieLanguages
  for i := 0 to ListBoxItems.Count - 1 do
  begin
    langFound := False;
    for CookieLang in CookieLanguages do
    begin
      if CookieLang.Language = ListBoxItems[i] then
      begin
        langFound := True;
        Break;
      end;
    end;
    if not langFound then
    begin
      // Initialize a new TCookieLanguage record and add it
      NewCookieLang:= TCookieLanguage.Create;
      NewCookieLang.Language := ListBoxItems[i];
      // Initialize other fields of cookieLang as needed
      CookieLanguages.Add(NewCookieLang);
    end;
  end;

  // Remove languages from CookieLanguages that are not in ListBoxItems
  i := 0;
  while i < CookieLanguages.Count do
  begin
    if ListBoxItems.IndexOf(CookieLanguages[i].Language) = -1 then
    begin
      RemoveCookieLanguage(CookieLanguages[i].Language);
    end
    else
      Inc(i);
  end;
end;

procedure TProject.RemoveCookieLanguage(const Language: String);
var
  Index: Integer;
begin
  Index := -1;

  // Find the index of the TCookieLanguage with the specified Language
  for Index := 0 to CookieLanguages.Count - 1 do
  begin
    if CookieLanguages[Index].Language = Language then
      Break;
  end;

  // If Language found, remove it from the list and free the associated object
  if Index < CookieLanguages.Count then
  begin
    CookieLanguages.Delete(Index);
  end;
end;

procedure TProject.SerializeDesign(ParentNode: TDOMNode);
var
  DesignNode: TDOMNode;
begin
  DesignNode := Doc.CreateElement('Design');
  ParentNode.AppendChild(DesignNode);

  with Design do
  begin
    AddTextNode(DesignNode, 'ConsentLayout', ConsentLayout);
    AddTextNode(DesignNode, 'ConsentPos', ConsentPos);
    AddTextNode(DesignNode, 'ConsentPos1', ConsentPos1);
    AddTextNode(DesignNode, 'ConsentTransition', ConsentTransition);
    AddTextNode(DesignNode, 'SettingsLayout', SettingsLayout);
    AddTextNode(DesignNode, 'SettingsPosition', SettingsPosition);
    AddTextNode(DesignNode, 'SettingsTransition', SettingsTransition);
  end;
end;

procedure TProject.SerializeApproval(const Approval: TApproval; ParentNode: TDOMNode);
var
  ApprovalNode: TDOMNode;
begin
  ApprovalNode := Doc.CreateElement('Approval');
  ParentNode.AppendChild(ApprovalNode);

  with Approval do
  begin
    AddTextNode(ApprovalNode, 'Title', Title);
    AddTextNode(ApprovalNode, 'Description', Description);
    AddTextNode(ApprovalNode, 'PrimaryButton', PrimaryButton);
    AddTextNode(ApprovalNode, 'SecondaryButton', SecondaryButton);
    AddTextNode(ApprovalNode, 'SelectButton', SelectButton);
  end;
end;

procedure TProject.SerializeCommonSettings(ParentNode: TDOMNode);
var
  CommonSettingsNode: TDOMNode;
begin
  CommonSettingsNode := Doc.CreateElement('CommonSettings');
  ParentNode.AppendChild(CommonSettingsNode);

  with CommonSettings do
  begin
    AddTextNode(CommonSettingsNode, 'CookieName', CookieName);
    AddTextNode(CommonSettingsNode, 'Delay', Delay);
    AddTextNode(CommonSettingsNode, 'HideFromBots', BoolToStr(HideFromBots, True));
    AddTextNode(CommonSettingsNode, 'RemoveCookieTables', BoolToStr(RemoveCookieTables, True));
    AddTextNode(CommonSettingsNode, 'Col3Active', BoolToStr(Col3Active, True));
    AddTextNode(CommonSettingsNode, 'Col4Active', BoolToStr(Col4Active, True));
  end;
end;

function TProject.GetTextNodeContent(Node: TDOMNode): String;
  begin
    if Assigned(Node) and Assigned(Node.FirstChild) then
      Result := Node.FirstChild.NodeValue
    else
      Result := '';
  end;

procedure TProject.DeserializeCommonSettings(ParentNode: TDOMNode);
var
  CommonSettingsNode, ChildNode: TDOMNode;
  NodeName: String;
begin
  CommonSettingsNode := ParentNode.FindNode('CommonSettings');
  if Assigned(CommonSettingsNode) then
  begin
    ChildNode := CommonSettingsNode.FirstChild;
    while Assigned(ChildNode) do
    begin
      NodeName := ChildNode.NodeName;

      if NodeName = 'CookieName' then
        CommonSettings.CookieName := GetTextNodeContent(ChildNode)
      else if NodeName = 'Delay' then
        CommonSettings.Delay := GetTextNodeContent(ChildNode)
      else if NodeName = 'HideFromBots' then
        CommonSettings.HideFromBots := StrToBoolDef(GetTextNodeContent(ChildNode), False)
      else if NodeName = 'RemoveCookieTables' then
        CommonSettings.RemoveCookieTables := StrToBoolDef(GetTextNodeContent(ChildNode), False)
      else if NodeName ='Col3Active' then
        CommonSettings.Col3Active := StrToBoolDef(GetTextNodeContent(ChildNode), False)
      else if NodeName ='Col4Active' then
              CommonSettings.Col4Active := StrToBoolDef(GetTextNodeContent(ChildNode), False);

      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

procedure TProject.DeserializeDesign(ParentNode: TDOMNode);
var
  DesignNode, ChildNode: TDOMNode;
  NodeName: String;
begin
  DesignNode := ParentNode.FindNode('Design');
  if Assigned(DesignNode) then
  begin
    ChildNode := DesignNode.FirstChild;
    while Assigned(ChildNode) do
    begin
      NodeName := ChildNode.NodeName;

      if NodeName = 'ConsentLayout' then
        Design.ConsentLayout := GetTextNodeContent(ChildNode)
      else if NodeName = 'ConsentPos' then
        Design.ConsentPos := GetTextNodeContent(ChildNode)
      else if NodeName = 'ConsentPos1' then
        Design.ConsentPos1 := GetTextNodeContent(ChildNode)
      else if NodeName = 'ConsentTransition' then
        Design.ConsentTransition := GetTextNodeContent(ChildNode)
      else if NodeName = 'SettingsLayout' then
        Design.SettingsLayout := GetTextNodeContent(ChildNode)
      else if NodeName = 'SettingsPosition' then
        Design.SettingsPosition := GetTextNodeContent(ChildNode)
      else if NodeName = 'SettingsTransition' then
        Design.SettingsTransition := GetTextNodeContent(ChildNode);

      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

procedure TProject.DeserializeSettings(ParentNode: TDOMNode; out Settings: TSettings);
var
  SettingsNode, ChildNode: TDOMNode;
  NodeName: String;
begin
  SettingsNode := ParentNode.FindNode('Settings');
  if Assigned(SettingsNode) then
  begin
    ChildNode := SettingsNode.FirstChild;
    while Assigned(ChildNode) do
    begin
      NodeName := ChildNode.NodeName;

      if NodeName = 'Title' then
        Settings.Title := GetTextNodeContent(ChildNode)
      else if NodeName = 'ButtonSave' then
        Settings.ButtonSave := GetTextNodeContent(ChildNode)
      else if NodeName = 'ButtonDenyAll' then
        Settings.ButtonDenyAll := GetTextNodeContent(ChildNode)
      else if NodeName = 'ButtonClose' then
        Settings.ButtonClose := GetTextNodeContent(ChildNode)
      else if NodeName = 'ButtonAcceptAll' then
        Settings.ButtonAcceptAll := GetTextNodeContent(ChildNode)
      else if NodeName = 'Col1' then
        Settings.Col1 := GetTextNodeContent(ChildNode)
      else if NodeName = 'Col2' then
        Settings.Col2 := GetTextNodeContent(ChildNode)
      else if NodeName = 'Col3' then
        Settings.Col3 := GetTextNodeContent(ChildNode)
      else if NodeName = 'Col4' then
        Settings.Col4 := GetTextNodeContent(ChildNode);

      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;


procedure TProject.DeserializeApproval(ParentNode: TDOMNode; out Approval: TApproval);
var
  ApprovalNode, ChildNode: TDOMNode;
  NodeName: String;
begin
  ApprovalNode := ParentNode.FindNode('Approval');
  if Assigned(ApprovalNode) then
  begin
    ChildNode := ApprovalNode.FirstChild;
    while Assigned(ChildNode) do
    begin
      NodeName := ChildNode.NodeName;

      if NodeName = 'Title' then
        Approval.Title := GetTextNodeContent(ChildNode)
      else if NodeName = 'Description' then
        Approval.Description := GetTextNodeContent(ChildNode)
      else if NodeName = 'PrimaryButton' then
        Approval.PrimaryButton := GetTextNodeContent(ChildNode)
      else if NodeName = 'SecondaryButton' then
        Approval.SecondaryButton := GetTextNodeContent(ChildNode)
      else if NodeName = 'SelectButton' then
        Approval.SelectButton := GetTextNodeContent(ChildNode);

      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

procedure TProject.AddServiceType(const ServiceType: String);
begin
  SetLength(ServiceTypes, Length(ServiceTypes) + 1);
  ServiceTypes[High(ServiceTypes)] := ServiceType;
end;

procedure TProject.RemoveServiceType(const ServiceType: String);
var
  i, IndexToRemove: Integer;
begin
  IndexToRemove := -1;

  // Find the index of the ServiceType to remove
  for i := Low(ServiceTypes) to High(ServiceTypes) do
  begin
    if ServiceTypes[i] = ServiceType then
    begin
      IndexToRemove := i;
      Break;
    end;
  end;

  // If the ServiceType was found, remove it from the array
  if IndexToRemove >= 0 then
  begin
    for i := IndexToRemove to High(ServiceTypes) - 1 do
      ServiceTypes[i] := ServiceTypes[i + 1];
    SetLength(ServiceTypes, Length(ServiceTypes) - 1);
  end;
end;


procedure TProject.SerializeCookieTableEntry(const Entry: TCookieTableEntry; ParentNode: TDOMNode);
var
  EntryNode: TDOMNode;
begin
  EntryNode := Doc.CreateElement('CookieTableEntry');
  ParentNode.AppendChild(EntryNode);

  with Entry do
  begin
    AddTextNode(EntryNode, 'Col1', Col1);
    AddTextNode(EntryNode, 'Col2', Col2);
    AddTextNode(EntryNode, 'Col3', Col3);
    AddTextNode(EntryNode, 'Col4', Col4);
    AddTextNode(EntryNode, 'IsRegEx', BoolToStr(IsRegEx, True));
  end;
end;


procedure TProject.DeserializeCookieTableEntry(Node: TDOMNode; out Entry: TCookieTableEntry);
var
  ChildNode: TDOMNode;
begin
  ChildNode := Node.FirstChild;
  while Assigned(ChildNode) do
  begin
    if ChildNode.NodeName = 'Col1' then
      Entry.Col1 := GetTextNodeContent(ChildNode)
    else if ChildNode.NodeName = 'Col2' then
      Entry.Col2 := GetTextNodeContent(ChildNode)
    else if ChildNode.NodeName = 'Col3' then
      Entry.Col3 := GetTextNodeContent(ChildNode)
    else if ChildNode.NodeName = 'Col4' then
      Entry.Col4 := GetTextNodeContent(ChildNode)
    else if ChildNode.NodeName = 'IsRegEx' then
      Entry.IsRegEx := StrToBoolDef(GetTextNodeContent(ChildNode), False);

    ChildNode := ChildNode.NextSibling;
  end;
end;


{ TCookieLanguage }

constructor TCookieLanguage.Create;
begin
  Self.Blocks:= TBlockList.Create(true);
end;

destructor TCookieLanguage.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Blocks);
end;


procedure TCookieLanguage.MoveBlockUp(Index: Integer);
begin
  if (Index > 0) and (Index < Blocks.Count) then
  begin
    Blocks.Exchange(Index, Index - 1);
  end;
end;

procedure TCookieLanguage.MoveBlockDown(Index: Integer);
begin
  if (Index >= 0) and (Index < Blocks.Count - 1) then
  begin
    Blocks.Exchange(Index, Index + 1);
  end;
end;


{ TBlock }

constructor TBlock.Create;
begin
  CookieTableEntries:= TCookieTableList.Create(true);
  IsToggle:= True;
end;

destructor TBlock.Destroy;
begin
  inherited Destroy;
  FreeAndNil(CookieTableEntries);
end;

function TBlock.Clone: TBlock;
var
  Entry: TCookieTableEntry;
begin
  Result := TBlock.Create;
  Result.Title := Title;
  Result.Description := Description;
  Result.IsToggle := IsToggle;
  Result.BlockType := BlockType;
  Result.Enabled := Enabled;
  Result.ReadOnly := ReadOnly;

  // Clone the CookieTableEntries if it's a reference type
  if Assigned(CookieTableEntries) then
  begin
    Result.CookieTableEntries := TCookieTableList.Create;
    // Clone each entry within CookieTableEntries
    for Entry in CookieTableEntries do
      Result.CookieTableEntries.Add(Entry.Clone);
  end;
end;

procedure TBlock.SerializeBlock(ParentNode: TDOMNode);
var
  BlockNode, EntryNode: TDOMNode;
  Entry: TCookieTableEntry;
begin
  // Create a new BlockNode under the ParentNode
  BlockNode := ParentNode.OwnerDocument.CreateElement('Block');
  ParentNode.AppendChild(BlockNode);

  // Add properties of the TBlock to the BlockNode as child elements
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Title')).TextContent := Title;
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Description')).TextContent := Description;
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('IsToggle')).TextContent := BoolToStr(IsToggle, True);
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('BlockType')).TextContent := BlockType;
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Enabled')).TextContent := BoolToStr(Enabled, True);
  BlockNode.AppendChild(ParentNode.OwnerDocument.CreateElement('ReadOnly')).TextContent := BoolToStr(ReadOnly, True);

  EntryNode := ParentNode.OwnerDocument.CreateElement('CookieTableEntries');
  BlockNode.AppendChild(EntryNode);

  // Serialize each entry within CookieTableEntries
  for Entry in CookieTableEntries do
  begin
    Entry.Serialize(EntryNode);
  end;
end;

procedure TBlock.DeserializeBlock(ParentNode: TDOMNode);
var
  EntryList, EntryNode: TDOMNode;
  Entry: TCookieTableEntry;
begin
  // Deserialize properties of TBlock from BlockNode
  Title := ParentNode.FindNode('Title').TextContent;
  Description := ParentNode.FindNode('Description').TextContent;
  IsToggle := StrToBool(ParentNode.FindNode('IsToggle').TextContent);
  BlockType := ParentNode.FindNode('BlockType').TextContent;
  Enabled := StrToBool(ParentNode.FindNode('Enabled').TextContent);
  ReadOnly := StrToBool(ParentNode.FindNode('ReadOnly').TextContent);

  // Deserialize each entry within CookieTableEntries
  EntryList:= ParentNode.FindNode('CookieTableEntries');
  if Assigned(EntryList) then
  begin
    EntryNode:= EntryList.FirstChild;
    while Assigned(EntryNode) do
    begin
      Entry:= TCookieTableEntry.Create;
      Entry.Deserialize(EntryNode);
      Self.CookieTableEntries.Add(Entry);
      EntryNode:= EntryNode.NextSibling;
    end;
  end;

end;

{ TCookieTableEntry }

function TCookieTableEntry.Clone: TCookieTableEntry;
begin
  Result := TCookieTableEntry.Create;
  Result.Col1 := Col1;
  Result.Col2 := Col2;
  Result.Col3 := Col3;
  Result.Col4 := Col4;
  Result.IsRegEx := IsRegEx;
end;

procedure TCookieTableEntry.Serialize(ParentNode: TDOMNode);
var
  EntryNode: TDOMNode;
begin
  // Create a new EntryNode under the ParentNode
  EntryNode := ParentNode.OwnerDocument.CreateElement('CookieTableEntry');
  ParentNode.AppendChild(EntryNode);

  // Add properties of the TCookieTableEntry to the EntryNode as child elements
  EntryNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Col1')).TextContent := Col1;
  EntryNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Col2')).TextContent := Col2;
  EntryNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Col3')).TextContent := Col3;
  EntryNode.AppendChild(ParentNode.OwnerDocument.CreateElement('Col4')).TextContent := Col4;
  EntryNode.AppendChild(ParentNode.OwnerDocument.CreateElement('IsRegEx')).TextContent := BoolToStr(IsRegEx, True);
end;

procedure TCookieTableEntry.Deserialize(ParentNode: TDOMNode);
begin
  // Deserialize properties of TCookieTableEntry from ParentNode
  Col1 := ParentNode.FindNode('Col1').TextContent;
  Col2 := ParentNode.FindNode('Col2').TextContent;
  Col3 := ParentNode.FindNode('Col3').TextContent;
  Col4 := ParentNode.FindNode('Col4').TextContent;
  IsRegEx := StrToBool(ParentNode.FindNode('IsRegEx').TextContent);
end;



end.

