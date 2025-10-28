unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, formcategory,
  ActnList, StdCtrls, Buttons, ExtCtrls, SynEdit, SynHighlighterJScript, category,
  formeditlang, language, formedittexts;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionPreview: TAction;
    ActionServiceTypes: TAction;
    ActionAbout: TAction;
    ActionOpen: TAction;
    ActionSaveAs: TAction;
    ActionExit: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionList: TActionList;
    ButtonLangDelete: TToolButton;
    ButtonLangDown: TToolButton;
    ButtonLangEdit: TToolButton;
    ButtonLangNew: TToolButton;
    ButtonLangUp: TToolButton;
    CheckBoxDisableTransitions: TCheckBox;
    CheckBoxDisablePageInteraction: TCheckBox;
    CheckBoxDarkMode: TCheckBox;
    CheckBoxPrefFlipButtons: TCheckBox;
    CheckBoxConsentEqualWeightButtons: TCheckBox;
    CheckBoxConsentFlipButtons: TCheckBox;
    CheckBoxPrefEqualWeightButtons: TCheckBox;
    ComboPrefPosition: TComboBox;
    ComboPrefLayout: TComboBox;
    ComboConsentPosition: TComboBox;
    ComboConsentLayout: TComboBox;
    GroupBoxDesignMisc: TGroupBox;
    GroupBoxDesignConsentModal: TGroupBox;
    GroupBoxPrefModal: TGroupBox;
    ImageList: TImageList;
    LabelPrefPosition: TLabel;
    LabelLayout: TLabel;
    LabelConsentPosition: TLabel;
    LabelConsentLayout: TLabel;
    ListViewCategories: TListView;
    ListViewLang: TListView;
    MainMenu: TMainMenu;
    MainToolbar: TToolBar;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemPreview: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemInfo: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog: TSaveDialog;
    Separator2: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemOpen: TMenuItem;
    Separator1: TMenuItem;
    StatusBar: TStatusBar;
    EditCode: TSynEdit;
    SynJScriptSyn1: TSynJScriptSyn;
    TabSheetDesign: TTabSheet;
    TabSheetTexts: TTabSheet;
    TabSheetCode: TTabSheet;
    TabSheetCategories: TTabSheet;
    ToolBarCat: TToolBar;
    ToolBarLangs: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ButtonCatNew: TToolButton;
    ButtonCatEdit: TToolButton;
    ButtonCatDelete: TToolButton;
    ButtonEditTexts: TToolButton;
    ToolButton3: TToolButton;
    ToolButton7: TToolButton;
    ToolButtonPreview: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSep2: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonNew: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    function ActionSaveExecute(Sender: TObject): boolean;
    procedure ActionServiceTypesExecute(Sender: TObject);
    procedure ButtonCatDeleteClick(Sender: TObject);
    procedure ButtonCatEditClick(Sender: TObject);
    procedure ButtonCatNewClick(Sender: TObject);
    procedure ButtonEditTextsClick(Sender: TObject);
    procedure ButtonLangDeleteClick(Sender: TObject);
    procedure ButtonLangDownClick(Sender: TObject);
    procedure ButtonLangEditClick(Sender: TObject);
    procedure ButtonLangNewClick(Sender: TObject);
    procedure ButtonLangUpClick(Sender: TObject);
    procedure ComboConsentLayoutChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListViewLangSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    function SaveCheck(): boolean;
    procedure FormShow(Sender: TObject);
    procedure UpdateTitleBar();
    procedure ShowErrorMessage(msg: String);
    procedure ShowWarningMessage(msg: String);
    procedure ShowInfoMessage(msg: String);
    procedure ModelToForm();
    procedure FormToModel();
    procedure UpdateCategories();
    procedure UpdateLangList();
    procedure UpdateAll();
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  about, ProjectLogic, datamodel, modelhelper;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
var
  MsgResult: Integer;
begin
  MsgResult := MessageDlg('Do you want to save the file before exiting?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);

  case MsgResult of
    mrYes:
      begin
        //Save file
        ActionSave.Execute;
        Application.Terminate;
      end;
    mrNo:
      begin
        // User chose not to save, so simply close the application
        Application.Terminate;
      end;
    mrCancel:
      begin
        // User canceled the exit operation, do nothing
      end;
  end;
end;

procedure TFormMain.ActionNewExecute(Sender: TObject);
begin
  if Self.SaveCheck() = true then
  begin
    TProjectLogic.GetInstance.New();
    UpdateAll();
  end;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    TProjectLogic.GetInstance.Open(OpenDialog.FileName);
    UpdateAll();
    ModelToForm();
  end;
end;

procedure TFormMain.ActionPreviewExecute(Sender: TObject);
begin
end;

procedure TFormMain.ActionSaveAsExecute(Sender: TObject);
begin
  FormToModel();
  if SaveDialog.Execute then
  begin
    TProjectLogic.GetInstance.SaveAs(SaveDialog.FileName);
    UpdateTitleBar();
  end;
end;

function TFormMain.ActionSaveExecute(Sender: TObject): boolean;
begin
  Result:= false;
  FormToModel();
  if TProjectLogic.GetInstance.ProjectFileName <> '' then
  begin
    TProjectLogic.GetInstance.Save();
    Result:= true;
  end
  else if SaveDialog.Execute then
  begin
    TProjectLogic.GetInstance.SaveAs(SaveDialog.FileName);
    Result:= true;
  end;
  UpdateTitleBar();
end;

procedure TFormMain.ActionServiceTypesExecute(Sender: TObject);
begin
end;

procedure TFormMain.ButtonCatDeleteClick(Sender: TObject);
var
  SelectedItem: TListItem;
  Cat: TCategory;
  UserChoice: Integer;
begin
  // Check if an item is selected
  SelectedItem := ListViewCategories.Selected;

  if SelectedItem = nil then
  begin
    ShowWarningMessage('Please select a category to delete.');
    Exit;
  end;

  // Get the category from the selected item
  Cat := TCategory(SelectedItem.Data);

  // Ask for confirmation
  UserChoice := MessageDlg('Delete Category',
                          'Are you sure you want to delete the category "' + Cat.Name + '"?',
                          mtConfirmation,
                          [mbYes, mbNo],
                          0);

  if UserChoice = mrYes then
  begin
    // Remove the category from the model
    TProjectLogic.GetInstance.Model.Categories.Remove(Cat.Name);

    // Update the list view
    UpdateCategories();
  end;
end;

procedure TFormMain.ButtonCatEditClick(Sender: TObject);
var
  SelectedItem: TListItem;
  Cat: TCategory;
  FormEditCat: TFormEditCategory;
  OldName: String;
begin
  // Check if an item is selected
  SelectedItem := ListViewCategories.Selected;

  if SelectedItem = nil then
  begin
    ShowWarningMessage('Please select a category to edit.');
    Exit;
  end;

  // Get the category from the selected item
  Cat := TCategory(SelectedItem.Data);

  // Create and populate the edit form
  FormEditCat := TFormEditCategory.Create(Self);
  try
    // Load current values into the form
    FormEditCat.ComboBoxCatName.Text := Cat.Name;
    FormEditCat.CheckBoxReadonly.Checked := Cat.ReadOnly;
    FormEditCat.CheckBoxActivated.Checked := Cat.PreSelected;

    // Store the old name in case it changes
    OldName := Cat.Name;

    if FormEditCat.ShowModal = mrOK then
    begin
      // Check if name has changed
      if OldName <> FormEditCat.ComboBoxCatName.Text then
      begin
        // Remove old category and add with new name
        TProjectLogic.GetInstance.Model.Categories.Remove(OldName);
        TProjectLogic.GetInstance.Model.Categories.Add(
          FormEditCat.ComboBoxCatName.Text,
          FormEditCat.CheckBoxReadonly.Checked,
          FormEditCat.CheckBoxActivated.Checked);
      end
      else
      begin
        // Just update the properties (name stays the same)
        Cat.ReadOnly := FormEditCat.CheckBoxReadonly.Checked;
        Cat.PreSelected := FormEditCat.CheckBoxActivated.Checked;
      end;

      // Update the list view
      UpdateCategories();
    end;
  finally
    FreeAndNil(FormEditCat);
  end;
end;

procedure TFormMain.ButtonCatNewClick(Sender: TObject);
var
  FormEditCat : TFormEditCategory;
begin
  FormEditCat:= TFormEditCategory.Create(Self);
  if FormEditCat.ShowModal = mrOK then
  begin
    TProjectLogic.GetInstance.Model.Categories.Add(FormEditCat.ComboBoxCatName.Text,
                                                    FormEditCat.CheckBoxReadonly.Checked,
                                                    FormEditCat.CheckBoxActivated.Checked);
    UpdateCategories();
  end;
  FreeAndNil(FormEditCat);
end;

procedure TFormMain.ButtonEditTextsClick(Sender: TObject);
var
   FormEditTexts : TFormEditLangTexts;
begin
  if ListViewLang.ItemIndex = -1 then exit;
  FormEditTexts:= TFormEditLangTexts.Create(Self);
  FormEditTexts.ShowModal;
  FreeAndNil(FormEditTexts);
end;

procedure TFormMain.ButtonLangDeleteClick(Sender: TObject);
var
  SelectedItem: TListItem;
  SelectedIndex: Integer;
  Languages: TLanguageList;
  LangCode: String;
  UserChoice: Integer;
begin
  // Check if an item is selected
  SelectedItem := ListViewLang.Selected;

  if SelectedItem = nil then
  begin
    ShowWarningMessage('Please select a language to delete.');
    Exit;
  end;

  SelectedIndex := SelectedItem.Index;
  Languages := TProjectLogic.GetInstance.Model.Languages;
  LangCode := Languages[SelectedIndex].Code;

  // Ask for confirmation with a strong warning
  UserChoice := MessageDlg('Delete Language',
                          'Are you sure you want to delete the language "' + LangCode + '"?' + LineEnding + LineEnding +
                          'WARNING: This will permanently delete all sections and texts for this language!',
                          mtWarning,
                          [mbYes, mbNo],
                          0);

  if UserChoice = mrYes then
  begin
    // Remove the language from the model
    Languages.Delete(SelectedIndex);

    // Update the list view
    UpdateLangList();

    // Update the ButtonEditTexts enabled state since selection changed
    ButtonEditTexts.Enabled := ListViewLang.Selected <> nil;
  end;
end;


procedure TFormMain.ButtonLangNewClick(Sender: TObject);
var
  FormLangEdit: TFormEditLanguage;
begin
  FormLangEdit:= TFormEditLanguage.Create(Self);
  if FormLangEdit.ShowModal = mrOK then
  begin
    TProjectLogic.GetInstance.Model.Languages.Add(FormLangEdit.ComboBoxLanguage.Text);
    UpdateLangList();
  end;
  FreeAndNil(FormLangEdit);
end;

procedure TFormMain.ButtonLangUpClick(Sender: TObject);
var
  SelectedIndex: Integer;
  Languages: TLanguageList;
begin
  // Check if an item is selected
  if ListViewLang.Selected = nil then
    Exit;

  SelectedIndex := ListViewLang.Selected.Index;

  // Can't move up if already at the top
  if SelectedIndex <= 0 then
    Exit;

  // Swap items in the model
  Languages := TProjectLogic.GetInstance.Model.Languages;
  Languages.Exchange(SelectedIndex, SelectedIndex - 1);

  // Update the display
  UpdateLangList();

  // Restore selection to the moved item
  ListViewLang.Items[SelectedIndex - 1].Selected := True;
  ListViewLang.Items[SelectedIndex - 1].MakeVisible(False);
end;

procedure TFormMain.ButtonLangDownClick(Sender: TObject);
var
  SelectedIndex: Integer;
  Languages: TLanguageList;
begin
  // Check if an item is selected
  if ListViewLang.Selected = nil then
    Exit;

  SelectedIndex := ListViewLang.Selected.Index;
  Languages := TProjectLogic.GetInstance.Model.Languages;

  // Can't move down if already at the bottom
  if SelectedIndex >= Languages.Count - 1 then
    Exit;

  // Swap items in the model
  Languages.Exchange(SelectedIndex, SelectedIndex + 1);

  // Update the display
  UpdateLangList();

  // Restore selection to the moved item
  ListViewLang.Items[SelectedIndex + 1].Selected := True;
  ListViewLang.Items[SelectedIndex + 1].MakeVisible(False);
end;

procedure TFormMain.ButtonLangEditClick(Sender: TObject);
var
  FormLangEdit: TFormEditLanguage;
  SelectedIndex: Integer;
  Languages: TLanguageList;
begin
  // Check if an item is selected
  if ListViewLang.Selected = nil then
    Exit;

  SelectedIndex := ListViewLang.Selected.Index;
  Languages := TProjectLogic.GetInstance.Model.Languages;

  // Create and configure the edit form
  FormLangEdit := TFormEditLanguage.Create(Self);
  try
    // Pre-fill with current language code
    FormLangEdit.ComboBoxLanguage.Text := Languages[SelectedIndex].Code;

    // Show dialog and update if OK was pressed
    if FormLangEdit.ShowModal = mrOK then
    begin
      Languages[SelectedIndex].Code := FormLangEdit.ComboBoxLanguage.Text;
      UpdateLangList();

      // Restore selection
      ListViewLang.Items[SelectedIndex].Selected := True;
    end;
  finally
    FreeAndNil(FormLangEdit);
  end;
end;

procedure TFormMain.UpdateLangList();
var
  i: Integer;
  ListItem: TListItem;
  Languages: TLanguageList;
begin
  ListViewLang.Items.Clear;

  Languages := TProjectLogic.GetInstance.Model.Languages;

  for i := 0 to Languages.Count - 1 do
  begin
    ListItem := ListViewLang.Items.Add;
    ListItem.Caption := Languages[i].Code;
  end;
end;

procedure TFormMain.UpdateAll();
begin
  UpdateCategories();
  UpdateLangList();
  UpdateTitleBar();
end;

procedure TFormMain.UpdateCategories();
var
  i: Integer;
  Cat: TCategory;
  ListItem: TListItem;
begin
  ListViewCategories.Items.BeginUpdate;
  try
    ListViewCategories.Items.Clear;

    for i := 0 to TProjectLogic.GetInstance.Model.Categories.Count - 1 do
    begin
      Cat := TProjectLogic.GetInstance.Model.Categories[i];
      ListItem := ListViewCategories.Items.Add;
      ListItem.Caption := Cat.Name;

      if Cat.ReadOnly then
        ListItem.SubItems.Add('✓')
      else
        ListItem.SubItems.Add('');

      if Cat.PreSelected then
        ListItem.SubItems.Add('✓')
      else
        ListItem.SubItems.Add('');

      ListItem.Data := Cat;
    end;
  finally
    ListViewCategories.Items.EndUpdate;
  end;
end;

procedure TFormMain.ComboConsentLayoutChange(Sender: TObject);
var
  SelectedLayout: String;
  IsBarLayout: Boolean;
  NeedsUpdate: Boolean;
begin
  SelectedLayout := ComboConsentLayout.Text;
  IsBarLayout := Pos('bar', SelectedLayout) > 0;

  // Check if we need to update the position combo
  NeedsUpdate := False;

  if IsBarLayout then
  begin
    // Bar layout should have 2 items (top, bottom)
    if ComboConsentPosition.Items.Count <> 2 then
      NeedsUpdate := True;
  end
  else
  begin
    // Box/cloud layouts should have 9 items
    if ComboConsentPosition.Items.Count <> 9 then
      NeedsUpdate := True;
  end;

  // Only update if necessary
  if NeedsUpdate then
  begin
    ComboConsentPosition.Items.Clear;

    if IsBarLayout then
    begin
      // Bar layouts only have top and bottom positions
      ComboConsentPosition.Items.Add('top');
      ComboConsentPosition.Items.Add('bottom');
    end
    else
    begin
      // Box and cloud layouts have all 9 positions
      ComboConsentPosition.Items.Add('top left');
      ComboConsentPosition.Items.Add('top center');
      ComboConsentPosition.Items.Add('top right');
      ComboConsentPosition.Items.Add('middle left');
      ComboConsentPosition.Items.Add('middle center');
      ComboConsentPosition.Items.Add('middle right');
      ComboConsentPosition.Items.Add('bottom left');
      ComboConsentPosition.Items.Add('bottom center');
      ComboConsentPosition.Items.Add('bottom right');
    end;

    // Set default selection to first item
    if ComboConsentPosition.Items.Count > 0 then
      ComboConsentPosition.ItemIndex := 0;
  end;
end;


procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SaveCheck() = false then
  begin
    CanClose:= false;
  end;
end;

procedure TFormMain.ListViewLangSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  ButtonEditTexts.Enabled := ListViewLang.Selected <> nil;
end;

//check if the file should be saved
function TFormMain.SaveCheck: boolean;
var
   UserChoice: Integer;
begin
  Result:= false;
  //Saving is only needed when the project is dirty
  if TProjectLogic.GetInstance.Dirty then
  begin
    if TProjectLogic.GetInstance.ProjectFileName <> '' then
    begin
      //project was already saved so we just save it
      TProjectLogic.GetInstance.Save();
      Result:= true;
    end else begin
      //project is not saved yet, we ask the user
      UserChoice:= MessageDlg('The project was modified, save first?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);

      // Handle the user's choice
      case UserChoice of
        mrYes:
          begin
            Self.ActionSaveAs.Execute;
            Result:= true;
          end;
        mrNo:
          Result:= true; //user does not want to save
        mrCancel:
          Result:= false; //already set, but make it clear here
      end;
    end;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  TProjectLogic.GetInstance; //this will create the data model and project logic, load default
  ModelToForm();
  UpdateAll();
  ButtonEditTexts.Enabled := False; // Initially disabled
end;

//Update title bar
procedure TFormMain.UpdateTitleBar;
var
  header, dirty: String;
begin
  dirty:= '';
  if TProjectLogic.GetInstance.Dirty = true then dirty:= '*';

  header:= PROG_NAME + ' - ';
  if TProjectLogic.GetInstance.ProjectFileName <> '' then
    header:= header + TProjectLogic.GetInstance.ProjectFileName
  else
    header:= header + 'untitled.cookiebanner';

  Self.Caption:= header + dirty;
end;

procedure TFormMain.ModelToForm();
begin
  with TProjectLogic.GetInstance.Model do
  begin
    ComboConsentLayout.Text := DesignConsentModalLayout;
    ComboPrefLayout.Text := DesignPrefModalLayout;

    ComboConsentLayoutChange(nil);

    CheckBoxConsentFlipButtons.Checked := DesignConsentModalFlipButtons;
    CheckBoxConsentEqualWeightButtons.Checked := DesignConsentModalEqualWeightButtons;
    CheckBoxPrefEqualWeightButtons.Checked := DesignPrefModalEqualWeightButtons;
    CheckBoxPrefFlipButtons.Checked := DesignConsentModalFlipButtons;
    CheckBoxDisablePageInteraction.Checked := DesignDisablePageInteraction;
    CheckBoxDarkMode.Checked := DesignEnableDarkMode;
    CheckBoxDisableTransitions.Checked := DesignDisableTransitions;

    ComboConsentPosition.Text := DesignConsentModalPosition;
    ComboPrefPosition.Text := DesignPrefModalPosition;
  end;
end;

procedure TFormMain.FormToModel();
begin
  with TProjectLogic.GetInstance.Model do
  begin
    DesignConsentModalLayout:= ComboConsentLayout.Text;
    DesignConsentModalPosition:=  ComboConsentPosition.Text;
    DesignConsentModalFlipButtons:= CheckBoxConsentEqualWeightButtons.Checked;
    DesignConsentModalEqualWeightButtons:= CheckBoxConsentEqualWeightButtons.Checked;

    DesignPrefModalEqualWeightButtons:= CheckBoxPrefEqualWeightButtons.Checked;
    DesignConsentModalFlipButtons:= CheckBoxPrefFlipButtons.Checked;
    DesignConsentModalPosition:= ComboPrefPosition.Text;
    DesignConsentModalLayout:= ComboPrefLayout.Text;

    DesignDisablePageInteraction:= CheckBoxDisablePageInteraction.Checked;
    DesignEnableDarkMode:= CheckBoxDarkMode.Checked;
    DesignDisableTransitions:= CheckBoxDisableTransitions.Checked;
  end;
end;

procedure TFormMain.ShowErrorMessage(msg: String);
begin
  MessageDlg('Error', msg, mtError, [mbOK], 0);
end;

procedure TFormMain.ShowWarningMessage(msg: String);
begin
  MessageDlg('Warning', msg, mtWarning, [mbOK], 0);
end;

procedure TFormMain.ShowInfoMessage(msg: String);
begin
  MessageDlg('Information', msg, mtInformation, [mbOK], 0);
end;

end.

