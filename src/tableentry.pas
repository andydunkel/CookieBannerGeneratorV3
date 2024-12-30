unit tableentry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, datamodel, ProjectLogic;

type

  { TFormTableEntry }

  TFormTableEntry = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    EditCol1: TEdit;
    EditCol2: TEdit;
    EditCol3: TEdit;
    LabelCol1: TLabel;
    LabelCol2: TLabel;
    LabelCol3: TLabel;
    LabelCol4: TLabel;
    EditCol4: TMemo;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure FormToModel();
    procedure ModelToForm();
    function Validate(): Boolean;

  public
    CookieTableEntry: TCookieTableEntry;

  end;

var
  FormTableEntry: TFormTableEntry;

implementation

uses
  mainform;

{$R *.lfm}

{ TFormTableEntry }

procedure TFormTableEntry.FormCreate(Sender: TObject);
begin
  LabelCol1.Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col1;
  LabelCol2.Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col2;
  LabelCol3.Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col3;
  LabelCol4.Caption := TProjectLogic.GetInstance.Model.GetCurrentCookieLanguage.Settings.Col4;
end;

procedure TFormTableEntry.FormShow(Sender: TObject);
begin
  ModelToForm();
end;

procedure TFormTableEntry.ButtonCancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TFormTableEntry.ButtonOkClick(Sender: TObject);
begin
  if Validate() = True then
  begin
    Self.Close();
    Self.FormToModel();
    Self.ModalResult:= mrOk;
  end else
  begin
    FormMain.ShowWarningMessage('Please enter at least one value in one of the fields.');
  end;
end;

procedure TFormTableEntry.FormToModel;
begin
  CookieTableEntry.Col1:= EditCol1.Text;
  CookieTableEntry.Col2:= EditCol2.Text;
  CookieTableEntry.Col3:= EditCol3.Text;
  CookieTableEntry.Col4:= EditCol4.Text;
end;

procedure TFormTableEntry.ModelToForm;
begin
  EditCol1.Text:= CookieTableEntry.Col1;
  EditCol2.Text:= CookieTableEntry.Col2;
  EditCol3.Text:= CookieTableEntry.Col3;
  EditCol4.Text:= CookieTableEntry.Col4;
end;

function TFormTableEntry.Validate: Boolean;
begin
  // Check if at least one of the edit fields contains text
  Result := (Length(EditCol1.Text) > 0) or
           (Length(EditCol2.Text) > 0) or
           (Length(EditCol3.Text) > 0) or
           (Length(EditCol4.Text) > 0);
end;

end.

