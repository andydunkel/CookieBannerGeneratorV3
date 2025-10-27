unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, formcategory,
  ActnList, StdCtrls, Buttons, ExtCtrls, SynEdit, SynHighlighterJScript;

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
    ListView1: TListView;
    ListView2: TListView;
    MainMenu: TMainMenu;
    MainToolbar: TToolBar;
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
    ToolButton6: TToolButton;
    ButtonCatUp: TToolButton;
    ButtonCatDown: TToolButton;
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
    procedure ButtonCatNewClick(Sender: TObject);
    procedure ComboConsentLayoutChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function SaveCheck(): boolean;
    procedure FormShow(Sender: TObject);
    procedure UpdateTitleBar();
    procedure ShowErrorMessage(msg: String);
    procedure ShowWarningMessage(msg: String);
    procedure ShowInfoMessage(msg: String);
    procedure ModelToForm();
    procedure FormToModel();

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

procedure TFormMain.ButtonCatNewClick(Sender: TObject);
var
  FormEditCat : TFormEditCategory;
begin
  FormEditCat:= TFormEditCategory.Create(Self);
  FormEditCat.ShowModal;
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

procedure TFormMain.ModelToForm();
begin

end;

procedure TFormMain.FormToModel();
begin

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

