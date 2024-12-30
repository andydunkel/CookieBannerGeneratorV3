unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList, StdCtrls, Buttons, SynEdit, editblock, Preview;

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
    ButtonBlockDown: TBitBtn;
    ButtonBlockUp: TBitBtn;
    ButtonBlockDelete: TBitBtn;
    ButtonBlockEdit: TBitBtn;
    ButtonAddBlock: TBitBtn;
    ButtonManageLang: TButton;
    CheckBoxCol3Active: TCheckBox;
    CheckBoxCol4Active: TCheckBox;
    CheckBoxHideFromBots: TCheckBox;
    CheckBoxRemoveTables: TCheckBox;
    ComboBoxSettingsLayout: TComboBox;
    ComboBoxSettingsPosition: TComboBox;
    ComboBoxSettingsTransition: TComboBox;
    ComboBoxConsentLayout: TComboBox;
    ComboBoxConsentPosition: TComboBox;
    ComboBoxConsentPosition1: TComboBox;
    ComboBoxConsentTransition: TComboBox;
    ComboBoxLang: TComboBox;
    EditSettingsTitle: TEdit;
    EditSaveButton: TEdit;
    EditButtonDenyAll: TEdit;
    EditCloseButton: TEdit;
    EditButtonAcceptAll: TEdit;
    EditCol1: TEdit;
    EditCol2: TEdit;
    EditCol3: TEdit;
    EditCol4: TEdit;
    EditTitle: TEdit;
    EditPrimaryButton: TEdit;
    EditSecondaryButton: TEdit;
    EditSelectButton: TEdit;
    EditDelay: TEdit;
    EditCookieName: TEdit;
    GroupBoxDesignConsent: TGroupBox;
    GroupBoxDesignSettings: TGroupBox;
    ImageList: TImageList;
    LabelConsentLayout: TLabel;
    LabelConsentPosition: TLabel;
    LabelConsentTransition: TLabel;
    LabelSettingsLayout: TLabel;
    LabelSettingsPosition: TLabel;
    LabelSettingsTransition: TLabel;
    LabelSettingsTitle: TLabel;
    LabelSaveButton: TLabel;
    LabelButtonDenyAll: TLabel;
    LabelCloseButton: TLabel;
    LabelButtonAcceptAll: TLabel;
    LabelCol1: TLabel;
    LabelCol2: TLabel;
    LabelCol3: TLabel;
    LabelCol4: TLabel;
    LabelTitle: TLabel;
    LabelDescription: TLabel;
    LabelPrimaryButton: TLabel;
    LabelSecondaryButton: TLabel;
    LabelSelectButton: TLabel;
    LabelDelayUnit: TLabel;
    LabelLanguage: TLabel;
    LabelCookieName: TLabel;
    LabelDelay: TLabel;
    ListViewBlocks: TListView;
    MainMenu: TMainMenu;
    MainToolbar: TToolBar;
    MemoDescription: TMemo;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemPreview: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemServiceTypes: TMenuItem;
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
    SynEditCode: TSynEdit;
    TabSheetDesign: TTabSheet;
    TabSheetCommon: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet6: TTabSheet;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
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
    procedure ButtonBlockDownClick(Sender: TObject);
    procedure ButtonAddBlockClick(Sender: TObject);
    procedure ButtonBlockDeleteClick(Sender: TObject);
    procedure ButtonBlockEditClick(Sender: TObject);
    procedure ButtonBlockUpClick(Sender: TObject);
    procedure ButtonManageLangClick(Sender: TObject);
    procedure ComboBoxLangChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function SaveCheck(): boolean;
    procedure FormShow(Sender: TObject);
    procedure UpdateTitleBar();
    procedure ShowErrorMessage(msg: String);
    procedure ShowWarningMessage(msg: String);
    procedure ShowInfoMessage(msg: String);
    procedure ModelToForm();
    procedure FormToModel();
  private
    procedure UpdateBlocks();

  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  about, ProjectLogic, datamodel, managelang, servicetypes, modelhelper;

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
  end;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    TProjectLogic.GetInstance.Open(OpenDialog.FileName);
  end;
  ModelToForm();
end;

procedure TFormMain.ActionPreviewExecute(Sender: TObject);
var
  Preview: TPreview;
begin
  Preview:= TPreview.Create;
  Preview.GeneratePreview(TProjectLogic.GetInstance.Model);
  FreeAndNil(Preview);
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
var
  FormServiceTypes: TFormServiceTypes;
begin
  FormServiceTypes:= TFormServiceTypes.Create(Self);
  FormServiceTypes.ShowModal;
  FreeAndNil(FormServiceTypes);
end;

procedure TFormMain.ButtonBlockDownClick(Sender: TObject);
var
  SelectedIndex, LangIndex: Integer;
begin
  if ListViewBlocks.ItemIndex = -1 then exit;
  SelectedIndex:= ListViewBlocks.ItemIndex;
  LangIndex:= TProjectLogic.GetInstance.Model.SelectedLanguage;
  TProjectLogic.GetInstance.Model.CookieLanguages.Items[LangIndex].MoveBlockDown(SelectedIndex);

  if (SelectedIndex <> ListViewBlocks.Items.Count - 1) then Inc(SelectedIndex);
  UpdateBlocks();
  ListViewBlocks.ItemIndex:= SelectedIndex;
end;

procedure TFormMain.ButtonBlockUpClick(Sender: TObject);
var
  SelectedIndex, LangIndex: Integer;
begin
  if ListViewBlocks.ItemIndex = -1 then exit;
  SelectedIndex:= ListViewBlocks.ItemIndex;
  LangIndex:= TProjectLogic.GetInstance.Model.SelectedLanguage;
  TProjectLogic.GetInstance.Model.CookieLanguages[LangIndex].MoveBlockUp(SelectedIndex);

  if (SelectedIndex > 0) then Dec(SelectedIndex);
  UpdateBlocks();
  ListViewBlocks.ItemIndex:= SelectedIndex;
end;

procedure TFormMain.ButtonAddBlockClick(Sender: TObject);
var
  FormEditBlock: TFormEditBlock;
  DialogResult: Integer;
  Block: TBlock;
begin
  FormEditBlock:= TFormEditBlock.Create(Self);
  Block:= TBlock.Create;
  FormEditBlock.Block:= Block;
  DialogResult:= FormEditBlock.ShowModal;

  if DialogResult = mrOK then
  begin
    TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks.Add(FormEditBlock.Block);
    UpdateBlocks();
    Block:= nil;
  end else
  begin
    FreeAndNil(Block);
  end;

  FreeAndNil(FormEditBlock);
end;

procedure TFormMain.ButtonBlockEditClick(Sender: TObject);
var
  FormEditBlock: TFormEditBlock;
  DialogResult: Integer;
begin
  if ListViewBlocks.ItemIndex = -1 then exit;
  FormEditBlock:= TFormEditBlock.Create(Self);

  //clone block
  FormEditBlock.Block:= TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks[ListViewBlocks.ItemIndex].Clone;
  DialogResult:= FormEditBlock.ShowModal;

  if DialogResult = mrOK then
  begin
    TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks[ListViewBlocks.ItemIndex]:= FormEditBlock.Block;
    UpdateBlocks();
  end;

  FreeAndNil(FormEditBlock);
end;

procedure TFormMain.ButtonBlockDeleteClick(Sender: TObject);
var
  UserResponse: Integer;
begin
  if ListViewBlocks.ItemIndex = -1 then exit;

  UserResponse := MessageDlg('Confirm Deletion',
                             'Are you sure you want to delete the selected block?',
                             mtConfirmation,
                             [mbYes, mbNo], 0);

  if UserResponse = mrYes then
  begin
    TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks.Delete(ListViewBlocks.ItemIndex);
    UpdateBlocks();
  end;
end;

procedure TFormMain.ButtonManageLangClick(Sender: TObject);
var
  FormManageLang: TFormManageLang;
  FormResult: Integer;
begin
  FormManageLang:= TFormManageLang.Create(Self);
  FormResult:= FormManageLang.ShowModal;
  if FormResult = mrOK then
  begin
    ModelToForm();
  end;
  FreeAndNil(FormManageLang);
end;

procedure TFormMain.ComboBoxLangChange(Sender: TObject);
begin
  FormToModel();
  TProjectLogic.GetInstance.Model.SelectedLanguage:= ComboBoxLang.ItemIndex;
  ModelToForm();
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SaveCheck() = false then
  begin
    CanClose:= false;
  end;
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
  UpdateTitleBar();
  ModelToForm();
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

//update from the data model
procedure TFormMain.ModelToForm();
var i: Integer;
begin
  with TProjectLogic.GetInstance.Model do
  begin
    // Common settings
    EditCookieName.Text := CommonSettings.CookieName;
    EditDelay.Text := CommonSettings.Delay;
    CheckBoxHideFromBots.Checked := CommonSettings.HideFromBots;
    CheckBoxRemoveTables.Checked := CommonSettings.RemoveCookieTables;
    CheckBoxCol3Active.Checked:= CommonSettings.Col3Active;
    CheckBoxCol4Active.Checked:= CommonSettings.Col4Active;

    // Design
    ComboBoxConsentLayout.Text := Design.ConsentLayout;
    ComboBoxConsentPosition.Text := Design.ConsentPos;
    ComboBoxConsentPosition1.Text := Design.ConsentPos1;
    ComboBoxConsentTransition.Text := Design.ConsentTransition;
    ComboBoxSettingsLayout.Text := Design.SettingsLayout;
    ComboBoxSettingsPosition.Text := Design.SettingsPosition;
    ComboBoxSettingsTransition.Text := Design.SettingsTransition;

    // Language-specific stuff
    with CookieLanguages[SelectedLanguage] do
    begin
      // Approval
      EditTitle.Text := Approval.Title;
      MemoDescription.Text := Approval.Description;
      EditPrimaryButton.Text := Approval.PrimaryButton;
      EditSecondaryButton.Text := Approval.SecondaryButton;
      EditSelectButton.Text := Approval.SelectButton;

      // Settings
      EditSettingsTitle.Text := Settings.Title;
      EditSaveButton.Text := Settings.ButtonSave;
      EditButtonDenyAll.Text := Settings.ButtonDenyAll;
      EditCloseButton.Text := Settings.ButtonClose;
      EditButtonAcceptAll.Text := Settings.ButtonAcceptAll;
      EditCol1.Text := Settings.Col1;
      EditCol2.Text := Settings.Col2;
      EditCol3.Text := Settings.Col3;
      EditCol4.Text := Settings.Col4;
    end;
  end;

  //Update language list
  ComboBoxLang.Items.Clear;
  for i:= 0 to TProjectLogic.GetInstance.Model.CookieLanguages.Count - 1 do
  begin
    ComboBoxLang.Items.Add(TProjectLogic.GetInstance.Model.CookieLanguages[i].Language);
  end;
  ComboBoxLang.ItemIndex:= TProjectLogic.GetInstance.Model.SelectedLanguage;

  UpdateBlocks();
  UpdateTitleBar();
end;

//Update to the data model
procedure TFormMain.FormToModel();
begin
  with TProjectLogic.GetInstance.Model do
  begin
    //Common settings
    CommonSettings.CookieName:= EditCookieName.Text;
    CommonSettings.Delay:= EditDelay.Text;
    CommonSettings.HideFromBots:= CheckBoxHideFromBots.Checked;
    CommonSettings.RemoveCookieTables:= CheckBoxRemoveTables.Checked;
    CommonSettings.Col3Active:= CheckBoxCol3Active.Checked;
    CommonSettings.Col4Active:= CheckBoxCol4Active.Checked;

    //Design
    Design.ConsentLayout:= ComboBoxConsentLayout.Text;
    Design.ConsentPos:= ComboBoxConsentPosition.Text;
    Design.ConsentPos1:= ComboBoxConsentPosition1.Text;
    Design.ConsentTransition:= ComboBoxConsentTransition.Text;
    Design.SettingsLayout:= ComboBoxSettingsLayout.Text;
    Design.SettingsPosition:= ComboBoxSettingsPosition.Text;
    Design.SettingsTransition:= ComboBoxSettingsTransition.Text;

    //language specific stuff
    with CookieLanguages[SelectedLanguage] do
    begin
      //Approval
      Approval.Title:= EditTitle.Text;
      Approval.Description:= MemoDescription.Text;
      Approval.PrimaryButton:= EditPrimaryButton.Text;
      Approval.SecondaryButton:= EditSecondaryButton.Text;
      Approval.SelectButton:= EditSelectButton.Text;

      //Settings
      Settings.Title:= EditSettingsTitle.Text;
      Settings.ButtonSave:= EditSaveButton.Text;
      Settings.ButtonDenyAll:= EditButtonDenyAll.Text;
      Settings.ButtonClose:= EditCloseButton.Text;
      Settings.ButtonAcceptAll:= EditButtonAcceptAll.Text;
      Settings.Col1:= EditCol1.Text;
      Settings.Col2:= EditCol2.Text;
      Settings.Col3:= EditCol3.Text;
      Settings.Col4:= EditCol4.Text;
    end;

    SelectedLanguage:= ComboBoxLang.ItemIndex;
  end;
end;

procedure TFormMain.UpdateBlocks;
var
  i: Integer;
  item: TListItem;
  CurrentBlock: TBlock;
begin
  ListViewBlocks.Items.Clear;

  for i:= 0 to TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks.Count - 1 do
  begin;
    CurrentBlock:= TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage().Blocks[i];
    item:= ListViewBlocks.Items.Add;
    item.Caption:= CurrentBlock.Title;
    item.SubItems.Add(CurrentBlock.Description);
    item.SubItems.Add(BoolToVisibleString(CurrentBlock.IsToggle));
    item.SubItems.Add(CurrentBlock.BlockType);
    item.SubItems.Add(BoolToVisibleString(CurrentBlock.Enabled));
    item.SubItems.Add(BoolToVisibleString(CurrentBlock.ReadOnly));
  end;
end;

end.

