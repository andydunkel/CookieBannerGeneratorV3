unit managelang;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TFormManageLang }

  TFormManageLang = class(TForm)
    EditLang: TEdit;
    LabelHint: TLabel;
    LabelLang: TLabel;
    Settings: TGroupBox;
    ListBoxLang: TListBox;
    StatusBar1: TStatusBar;
    ToolBarMain: TToolBar;
    ToolButtonExit: TToolButton;
    ToolButtonSep1: TToolButton;
    ToolButtonAdd: TToolButton;
    ToolButtonEdit: TToolButton;
    ToolButtonDelete: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure ListBoxLangSelectionChange(Sender: TObject; User: boolean);
    procedure ToolButtonAddClick(Sender: TObject);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonEditClick(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
  private
    function CheckIfLangExists(Lang: String): boolean;
    procedure UpdateLanguageList();

  public

  end;

var
  FormManageLang: TFormManageLang;

implementation

uses
  mainform, ProjectLogic;

{$R *.lfm}

{ TFormManageLang }

procedure TFormManageLang.ToolButtonExitClick(Sender: TObject);
var
  MsgResult: Integer;
begin
  MsgResult:= MessageDlg('Confirmation', 'Apply settings to the project, this will update all affected languages.', mtConfirmation,
     [mbYes, mbNo, mbCancel ],0);

   if MsgResult = mrYes then
   begin
     TProjectLogic.GetInstance.Model.UpdateLanguageList(ListBoxLang.Items);
     Close;
     Self.ModalResult:= mrOK;
   end;

   if MsgResult = mrNo then
   begin
     Self.ModalResult:= mrCancel;
     Close;
   end;
end;

function TFormManageLang.CheckIfLangExists(Lang: String): boolean;
var
  i: Integer;
  s: String;
begin
  Result:= false;

  for i:= 0 to ListBoxLang.Items.Count - 1 do
  begin
    s:= ListBoxLang.Items[i];
    if s = Lang then Result:= true;
  end;
end;

//Update the language list in the data model
procedure TFormManageLang.UpdateLanguageList;
begin

end;

procedure TFormManageLang.ToolButtonAddClick(Sender: TObject);
begin
  if EditLang.Text <> '' then
  begin
    if not CheckIfLangExists(EditLang.Text) then
    begin
      ListBoxLang.Items.Add(EditLang.Text);
      EditLang.Text:= '';
      EditLang.SetFocus;
      ListBoxLang.ItemIndex:= -1;
    end else
    begin
      FormMain.ShowInfoMessage('Language already exists');
      EditLang.SetFocus;
    end;
  end else begin
    FormMain.ShowInfoMessage('Please enter a short name');
    EditLang.SetFocus;
  end;
end;

procedure TFormManageLang.ToolButtonDeleteClick(Sender: TObject);
begin
  if ListBoxLang.ItemIndex <> -1 then
  begin
    if ListBoxLang.Items.Count <= 1 then
    begin
      FormMain.ShowErrorMessage('Project must contain at least one language.');
      Exit;
    end;

    if MessageDlg('Delete', 'Delete language? Warning, this will delete all data for that language.', mtConfirmation,
     [mbYes, mbNo ],0) = mrYes
    then begin
      ListBoxLang.DeleteSelected;
    end;
  end;
end;

procedure TFormManageLang.ToolButtonEditClick(Sender: TObject);
begin
  if ListBoxLang.ItemIndex <> -1 then
  begin
    if EditLang.Text <> '' then
    begin
      ListBoxLang.Items[ListBoxLang.ItemIndex]:= EditLang.Text;
      EditLang.Text:= '';
      EditLang.SetFocus;
    end else
    begin
      FormMain.ShowInfoMessage('Please enter a short name');
      EditLang.SetFocus;
    end;
  end else
  begin
    FormMain.ShowInfoMessage('Select a language first');
  end;
end;


procedure TFormManageLang.ListBoxLangSelectionChange(Sender: TObject; User: boolean);
begin
   EditLang.Text:= ListBoxLang.GetSelectedText;
end;

procedure TFormManageLang.FormShow(Sender: TObject);
var
  i: Integer;
begin
  EditLang.SetFocus;
  for i := 0 to TProjectLogic.GetInstance.Model.CookieLanguages.Count - 1 do
  begin
    ListBoxLang.Items.Add(TProjectLogic.GetInstance.Model.CookieLanguages[i].Language);
  end;
end;

end.

