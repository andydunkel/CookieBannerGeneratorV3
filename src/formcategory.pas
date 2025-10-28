unit formcategory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormEditCategory }

  TFormEditCategory = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxActivated: TCheckBox;
    CheckBoxReadonly: TCheckBox;
    ComboBoxCatName: TComboBox;
    LabelCatName: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormEditCategory: TFormEditCategory;

implementation

{$R *.lfm}

{ TFormEditCategory }

procedure TFormEditCategory.FormCreate(Sender: TObject);
begin
  // Set focus to ComboBox when form opens
  ActiveControl := ComboBoxCatName;

  // Make OK button the default button (Enter key triggers it)
  ButtonOK.Default := True;

  // Make Cancel button respond to Escape key
  ButtonCancel.Cancel := True;
end;

procedure TFormEditCategory.ButtonOKClick(Sender: TObject);
begin
  // Validate that ComboBox is not empty
  if Trim(ComboBoxCatName.Text) = '' then
  begin
    ShowMessage('Please enter a category name.');
    ComboBoxCatName.SetFocus;
    ModalResult := mrNone; // Prevent dialog from closing
    Exit;
  end;

  // If validation passes, close dialog with OK result
  ModalResult := mrOK;
end;

end.

