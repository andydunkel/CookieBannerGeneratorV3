unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList, StdCtrls, Buttons, SynEdit;

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
    ImageList: TImageList;
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
    TabSheetCommon: TTabSheet;
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

