unit category;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, laz2_DOM, XMLHelper;

type
  { TCategory }
  TCategory = class
  private
    FName: String;
    FReadOnly: Boolean;
    FPreSelected: Boolean;
  public
    constructor Create(const AName: String; AReadOnly: Boolean = False; APreSelected: Boolean = False);

    property Name: String read FName write FName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property PreSelected: Boolean read FPreSelected write FPreSelected;
  end;

  { TCategoryList - Specialized list for managing categories }
  TCategoryList = class(specialize TFPGObjectList<TCategory>)
  private
    function GetCategory(Index: Integer): TCategory;
  public
    function Add(const AName: String; AReadOnly: Boolean = False; APreSelected: Boolean = False): TCategory;
    function Find(const AName: String): TCategory;
    function Exists(const AName: String): Boolean;
    procedure Remove(const AName: String);

    procedure SaveToXML(ParentNode: TDOMNode);
    procedure LoadFromXML(ParentNode: TDOMNode);

    property Categories[Index: Integer]: TCategory read GetCategory; default;
  end;

implementation

{ TCategory }

constructor TCategory.Create(const AName: String; AReadOnly: Boolean = False; APreSelected: Boolean = False);
begin
  inherited Create;
  FName := AName;
  FReadOnly := AReadOnly;
  FPreSelected := APreSelected;
end;

{ TCategoryList }

function TCategoryList.GetCategory(Index: Integer): TCategory;
begin
  Result := inherited Items[Index];
end;

function TCategoryList.Add(const AName: String; AReadOnly: Boolean = False; APreSelected: Boolean = False): TCategory;
begin
  // Check if category already exists
  Result := Find(AName);

  if Assigned(Result) then
  begin
    // Update existing category
    Result.ReadOnly := AReadOnly;
    Result.PreSelected := APreSelected;
  end
  else
  begin
    // Create new category
    Result := TCategory.Create(AName, AReadOnly, APreSelected);
    inherited Add(Result);
  end;
end;

function TCategoryList.Find(const AName: String): TCategory;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].Name, AName) = 0 then // Case-insensitive comparison
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TCategoryList.Exists(const AName: String): Boolean;
begin
  Result := Assigned(Find(AName));
end;

procedure TCategoryList.Remove(const AName: String);
var
  Cat: TCategory;
begin
  Cat := Find(AName);
  if Assigned(Cat) then
    inherited Remove(Cat); // Will free the object since we own it
end;

procedure TCategoryList.SaveToXML(ParentNode: TDOMNode);
var
  CategoriesNode, CategoryNode: TDOMNode;
  i: Integer;
  Cat: TCategory;
begin
  if Count = 0 then
    Exit;

  CategoriesNode := ParentNode.OwnerDocument.CreateElement('Categories');
  ParentNode.AppendChild(CategoriesNode);

  for i := 0 to Count - 1 do
  begin
    Cat := Items[i];
    CategoryNode := ParentNode.OwnerDocument.CreateElement('Category');
    CategoriesNode.AppendChild(CategoryNode);

    // Add child nodes
    with CategoryNode do
    begin
      AppendChild(OwnerDocument.CreateElement('Name')).TextContent := Cat.Name;
      AppendChild(OwnerDocument.CreateElement('ReadOnly')).TextContent := BoolToStr(Cat.ReadOnly, True);
      AppendChild(OwnerDocument.CreateElement('PreSelected')).TextContent := BoolToStr(Cat.PreSelected, True);
    end;
  end;
end;

procedure TCategoryList.LoadFromXML(ParentNode: TDOMNode);
var
  CategoriesNode, CategoryNode: TDOMNode;
  Name: String;
  ReadOnly, PreSelected: Boolean;
begin
  Clear;

  CategoriesNode := ParentNode.FindNode('Categories');
  if not Assigned(CategoriesNode) then
    Exit;

  CategoryNode := CategoriesNode.FirstChild;
  while Assigned(CategoryNode) do
  begin
    if CategoryNode.NodeName = 'Category' then
    begin
      Name := TXMLHelper.GetXML('Name', CategoryNode);

      try
        ReadOnly := StrToBool(TXMLHelper.GetXML('ReadOnly', CategoryNode));
      except
        ReadOnly := False;
      end;

      try
        PreSelected := StrToBool(TXMLHelper.GetXML('PreSelected', CategoryNode));
      except
        PreSelected := False;
      end;

      if Name <> '' then
        Add(Name, ReadOnly, PreSelected);
    end;

    CategoryNode := CategoryNode.NextSibling;
  end;
end;

end.
