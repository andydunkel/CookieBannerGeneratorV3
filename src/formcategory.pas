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
  private

  public

  end;

var
  FormEditCategory: TFormEditCategory;

implementation

{$R *.lfm}

end.

