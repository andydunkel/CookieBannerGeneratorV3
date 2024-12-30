unit servicetypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ProjectLogic, inputdialog;

type

  { TFormServiceTypes }

  TFormServiceTypes = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ListBox: TListBox;
    ToolBarMain: TToolBar;
    ToolButtonAdd: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonEdit: TToolButton;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ToolButtonAddClick(Sender: TObject);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonEditClick(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
  private
    FormInputDialog: TFormInputDialog;
    procedure FormToModel();
    procedure ModelToForm();

  public

  end;

var
  FormServiceTypes: TFormServiceTypes;

implementation

{$R *.lfm}

{ TFormServiceTypes }

procedure TFormServiceTypes.ToolButtonExitClick(Sender: TObject);
begin

end;

procedure TFormServiceTypes.FormToModel;
var
  i: Integer;
begin
  TProjectLogic.GetInstance.Model.ClearServiceTypes();
  for i:= 0 to ListBox.Items.Count - 1 do
  begin
    TProjectLogic.GetInstance.Model.AddServiceType(ListBox.Items[i]);
  end;
end;

procedure TFormServiceTypes.ModelToForm;
var
  i: Integer;
begin
  ListBox.Items.Clear;
  for i:= 0 to Length(TProjectLogic.GetInstance.Model.ServiceTypes) - 1 do
  begin
    ListBox.Items.Add(TProjectLogic.GetInstance.Model.ServiceTypes[i]);
  end;
end;

procedure TFormServiceTypes.FormShow(Sender: TObject);
begin
  ModelToForm();

  //Init the input dialog
  FormInputDialog:= TFormInputDialog.Create(Self);
  FormInputDialog.Caption:= 'Edit service type';
  FormInputDialog.ValidationType:= vtStringNotEmpty;
  FormInputDialog.EditInput.Text:= '';
end;

procedure TFormServiceTypes.ListBoxDblClick(Sender: TObject);
begin
  ToolButtonEditClick(Sender);
end;

procedure TFormServiceTypes.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FormInputDialog);
end;

procedure TFormServiceTypes.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormServiceTypes.ButtonOkClick(Sender: TObject);
begin
  FormToModel();
end;

procedure TFormServiceTypes.ToolButtonAddClick(Sender: TObject);
begin
  FormInputDialog.EditInput.Text:= '';
  if FormInputDialog.ShowModal = mrOK then
  begin
    ListBox.Items.Add(FormInputDialog.EditInput.Text);
  end;
end;

procedure TFormServiceTypes.ToolButtonDeleteClick(Sender: TObject);
begin
  if ListBox.ItemIndex = -1 then Exit;
  ListBox.DeleteSelected;
end;

procedure TFormServiceTypes.ToolButtonEditClick(Sender: TObject);
var
  s: String;
begin
  if ListBox.ItemIndex = -1 then Exit;
  s:= ListBox.Items[ListBox.ItemIndex];
  FormInputDialog.EditInput.Text:= s;

  if FormInputDialog.ShowModal = mrOK then
  begin
    ListBox.Items[ListBox.ItemIndex]:= FormInputDialog.EditInput.Text;
  end;
end;

end.

