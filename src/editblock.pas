unit editblock;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ProjectLogic, datamodel, tableentry;

type

  { TFormEditBlock }

  TFormEditBlock = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    CheckBoxFixed: TCheckBox;
    CheckBoxReadOnly: TCheckBox;
    CheckBoxActive: TCheckBox;
    ComboBoxType: TComboBox;
    EditTitle: TEdit;
    LabelTitle: TLabel;
    LabelDescription: TLabel;
    LabelType: TLabel;
    ListViewCookieTable: TListView;
    MemoDescription: TMemo;
    PageControl: TPageControl;
    TabSheetCommon: TTabSheet;
    TabSheetTable: TTabSheet;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonUp: TToolButton;
    ToolButtonDown: TToolButton;
    ToolButtonAdd: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonEdit: TToolButton;
    procedure ButtonOkClick(Sender: TObject);
    procedure CheckBoxFixedChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButtonAddClick(Sender: TObject);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonDownClick(Sender: TObject);
    procedure ToolButtonEditClick(Sender: TObject);
    procedure ToolButtonUpClick(Sender: TObject);
  private
    function Validate(): boolean;
    procedure FormToModel();
    procedure ModelToForm();
    procedure UpdateTable();

  public
    Block: TBlock;

  end;

var
  FormEditBlock: TFormEditBlock;

implementation

uses
  mainform, modelhelper;

{$R *.lfm}

{ TFormEditBlock }

procedure TFormEditBlock.FormShow(Sender: TObject);
var
  i: Integer;
begin
  PageControl.ActivePageIndex:= 0;
  EditTitle.SetFocus;

  //update combobox
  for i:= 0 to Length(TProjectLogic.GetInstance.Model.ServiceTypes) - 1 do
  begin
    ComboBoxType.Items.Add(TProjectLogic.GetInstance.Model.ServiceTypes[i]);
  end;

  ModelToForm();
  UpdateTable();

  FormMain.FormToModel();
  if ListViewCookieTable.Columns.Count >= 4 then
  begin
    // Change the captions of the columns
    ListViewCookieTable.Columns[0].Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col1;
    ListViewCookieTable.Columns[1].Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col2;
    ListViewCookieTable.Columns[2].Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col3;
    ListViewCookieTable.Columns[3].Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col4;
  end;

end;

procedure TFormEditBlock.ToolButtonAddClick(Sender: TObject);
var
  CookieTableEntry: TCookieTableEntry;
  DialogResult: Integer;
  FormTableEntry: TFormTableEntry;
begin
  CookieTableEntry:= TCookieTableEntry.Create;
  FormTableEntry:= TFormTableEntry.Create(Self);
  FormTableEntry.CookieTableEntry:= CookieTableEntry;
  DialogResult:= FormTableEntry.ShowModal;

  if DialogResult = mrOK then
  begin
    Block.CookieTableEntries.Add(CookieTableEntry);
    CookieTableEntry:= nil;
    UpdateTable();
  end else
  begin
    FreeAndNil(CookieTableEntry);
  end;

  FreeAndNil(FormTableEntry);
end;


procedure TFormEditBlock.ToolButtonEditClick(Sender: TObject);
var
  CookieTableEntry: TCookieTableEntry;
  DialogResult: Integer;
  FormTableEntry: TFormTableEntry;
begin
  if ListViewCookieTable.ItemIndex = -1 then Exit;

  FormTableEntry:= TFormTableEntry.Create(Self);
  FormTableEntry.CookieTableEntry:= Block.CookieTableEntries[ListViewCookieTable.ItemIndex];
  DialogResult:= FormTableEntry.ShowModal;

  if DialogResult = mrOK then
  begin
    CookieTableEntry:= nil;
    UpdateTable();
  end;

  FreeAndNil(FormTableEntry);
end;

procedure TFormEditBlock.ToolButtonDeleteClick(Sender: TObject);
begin
  if ListViewCookieTable.ItemIndex = -1 then Exit;

  Block.CookieTableEntries.Delete(ListViewCookieTable.ItemIndex);
  UpdateTable();
end;

procedure TFormEditBlock.ToolButtonDownClick(Sender: TObject);
var
  SelectedIndex: Integer;
begin
  SelectedIndex := ListViewCookieTable.ItemIndex;

  if SelectedIndex = -1 then
    Exit;

  // Check if it's possible to move down (not the last item)
  if SelectedIndex < ListViewCookieTable.Items.Count - 1 then
  begin

    // Swap the entries in the underlying data structure
    Block.CookieTableEntries.Exchange(SelectedIndex, SelectedIndex + 1);

    UpdateTable();

    // Update the ListView's selected item
    ListViewCookieTable.ItemIndex := SelectedIndex + 1;
  end;
end;

procedure TFormEditBlock.ToolButtonUpClick(Sender: TObject);
var
  SelectedIndex: Integer;
begin
  SelectedIndex := ListViewCookieTable.ItemIndex;

  if SelectedIndex = -1 then
    Exit;

  // Check if it's possible to move up (not the first item)
  if SelectedIndex > 0 then
  begin
    // Swap the entries in the underlying data structure
    Block.CookieTableEntries.Exchange(SelectedIndex, SelectedIndex - 1);

    UpdateTable();

    // Update the ListView's selected item
    ListViewCookieTable.ItemIndex := SelectedIndex - 1;
  end;
end;


procedure TFormEditBlock.CheckBoxFixedChange(Sender: TObject);
begin
  TabSheetTable.TabVisible:= not CheckBoxFixed.Checked;
end;

procedure TFormEditBlock.ButtonOkClick(Sender: TObject);
begin
  if Self.Validate = true then
  begin
    FormToModel;
    Close;
    ModalResult:= mrOK;
  end;
end;

function TFormEditBlock.Validate: boolean;
begin
  if EditTitle.Text = '' then
  begin
    Result:= false;
    FormMain.ShowWarningMessage('Please enter a title for the block.');
    EditTitle.SetFocus;
    Exit;
  end;

  if ComboBoxType.Text = '' then
  begin
    Result:= false;
    FormMain.ShowWarningMessage('Please selected or enter a type.');
    ComboBoxType.SetFocus;
    Exit;
  end;
end;

procedure TFormEditBlock.FormToModel;
begin
  Block.Title:= EditTitle.Text;
  Block.Description:= MemoDescription.Text;
  Block.Enabled:= CheckBoxActive.Checked;
  Block.BlockType:= ComboBoxType.Text;
  Block.IsToggle:= not CheckBoxFixed.Checked;
  Block.ReadOnly:= CheckBoxReadOnly.Checked;
end;

procedure TFormEditBlock.ModelToForm;
begin
  EditTitle.Text := Block.Title;
  MemoDescription.Text := Block.Description;
  CheckBoxActive.Checked := Block.Enabled;
  ComboBoxType.Text := Block.BlockType;
  CheckBoxFixed.Checked := not Block.IsToggle;
  CheckBoxReadOnly.Checked := Block.ReadOnly;
end;

procedure TFormEditBlock.UpdateTable;
var
  Entry: TCookieTableEntry;
  ListItem: TListItem;
begin
  // Clear the ListViewCookieTable before populating it
  ListViewCookieTable.Clear;

  // Check if Block is assigned and contains CookieTableEntries
  if Assigned(Block) and Assigned(Block.CookieTableEntries) then
  begin
    // Iterate through the TCookieTableEntries and add them to the ListView
    for Entry in Block.CookieTableEntries do
    begin
      ListItem := ListViewCookieTable.Items.Add;
      ListItem.Caption := Entry.Col1;
      ListItem.SubItems.Add(Entry.Col2);
      ListItem.SubItems.Add(Entry.Col3);
      ListItem.SubItems.Add(TruncateMultilineText(Entry.Col4, 100));
    end;
  end;
end;


end.

