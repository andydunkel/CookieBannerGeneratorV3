unit formsection;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TFormEditSection }
  TFormEditSection = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxLinkedCategory: TComboBox;
    EditTitle: TEdit;
    LabelDescription: TLabel;
    LabelLinkedCategory: TLabel;
    LabelTitle: TLabel;
    MemoDescription: TMemo;
  private
  public
  end;

var
  FormEditSection: TFormEditSection;

implementation

{$R *.lfm}

end.
