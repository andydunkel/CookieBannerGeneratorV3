unit inputdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TValidationType = (vtStringNotEmpty, vtInteger);


type

  { TFormInputDialog }

  TFormInputDialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditInput: TEdit;
    LabelError: TLabel;
    LabelCaption: TLabel;
    TimerMessage: TTimer;
    procedure ButtonOkClick(Sender: TObject);
    procedure EditInputKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure TimerMessageTimer(Sender: TObject);
  private
    procedure DisplayError(Msg: String);

  public
    ValidationType: TValidationType;
  end;

var
  FormInputDialog: TFormInputDialog;

implementation

{$R *.lfm}

{ TFormInputDialog }

procedure TFormInputDialog.FormShow(Sender: TObject);
begin
  EditInput.SetFocus;
  LabelError.Visible:= False;
end;

procedure TFormInputDialog.TimerMessageTimer(Sender: TObject);
begin
  LabelError.Visible:=False;
  TimerMessage.Enabled:= False;
end;

procedure TFormInputDialog.DisplayError(Msg: String);
begin
  LabelError.Caption:= Msg;
  LabelError.Visible:= True;
  EditInput.SetFocus;
  TimerMessage.Enabled:= True;
end;

procedure TFormInputDialog.ButtonOkClick(Sender: TObject);
var
  InputValue: String;
  InputInt: Integer;
begin
  InputValue := EditInput.Text;

  case ValidationType of
    vtInteger:
      if not TryStrToInt(InputValue, InputInt) then
      begin
        DisplayError('Please enter a valid integer value.');
        Exit;
      end;

    vtStringNotEmpty:
      if InputValue = '' then
      begin
        DisplayError('The input cannot be empty.');
        Exit;
      end;
  end;

  Self.Close;
  Self.ModalResult:= mrOK;
end;

procedure TFormInputDialog.EditInputKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then // #13 is the ASCII code for Enter
  begin
    Key := #0; // Prevent the beep sound when pressing Enter
    ButtonOkClick(Sender); // Call the ButtonOkClick procedure
  end;
end;

end.

