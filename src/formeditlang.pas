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
  private

  public

  end;

var
  FormEditLanguage: TFormEditLanguage;

implementation

{$R *.lfm}

end.

