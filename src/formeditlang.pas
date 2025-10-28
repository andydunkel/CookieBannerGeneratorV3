unit formeditlang;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormEditLanguage }

  TFormEditLanguage = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxLanguage: TComboBox;
    LabelLang: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function ValidateLanguageCode: Boolean;
  public

  end;

var
  FormEditLanguage: TFormEditLanguage;

implementation

{$R *.lfm}

{ TFormEditLanguage }

procedure TFormEditLanguage.FormShow(Sender: TObject);
begin
   ComboBoxLanguage.SetFocus;
end;

procedure TFormEditLanguage.ButtonOKClick(Sender: TObject);
begin
  if ValidateLanguageCode then
    ModalResult := mrOK
  else ModalResult:= mrNone;
end;

function TFormEditLanguage.ValidateLanguageCode: Boolean;
var
  Code: string;
begin
  Result := False;
  Code := Trim(ComboBoxLanguage.Text);

  // Check if exactly 2 characters
  if Length(Code) <> 2 then
  begin
    MessageDlg('Error', 'Language code must be exactly 2 letters.',
               mtError, [mbOK], 0);
    ComboBoxLanguage.SetFocus;
    Exit;
  end;

  // Check if both characters are letters
  if not ((Code[1] in ['a'..'z', 'A'..'Z']) and
          (Code[2] in ['a'..'z', 'A'..'Z'])) then
  begin
    MessageDlg('Error', 'Language code must contain only letters.',
               mtError, [mbOK], 0);
    ComboBoxLanguage.SetFocus;
    Exit;
  end;

  Result := True;
end;

end.

