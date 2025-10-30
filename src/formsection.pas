unit formsection;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ProjectLogic, datamodel, category;

type
  { TFormEditSection }
  TFormEditSection = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxLinkedCategory: TComboBox;
    EditTitle: TEdit;
    Label1: TLabel;
    LabelDescription: TLabel;
    LabelLinkedCategory: TLabel;
    LabelTitle: TLabel;
    MemoDescription: TMemo;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  FormEditSection: TFormEditSection;

implementation

{$R *.lfm}

{ TFormEditSection }

procedure TFormEditSection.ButtonOKClick(Sender: TObject);
begin
  // Trim removes leading/trailing whitespace
  if Trim(EditTitle.Text) = '' then
  begin
    MessageDlg('Validation Error', 'Title cannot be empty!', mtError, [mbOK], 0);
    EditTitle.SetFocus;
    Exit;
  end;
  // If validation passes, close the form with OK result
  ModalResult := mrOK;
end;

procedure TFormEditSection.FormCreate(Sender: TObject);
var
  i: Integer;
  Category: TCategory;
begin
  ComboBoxLinkedCategory.Items.Clear;

  // Loop through all categories and add their names
  for i := 0 to TProjectLogic.GetInstance.Model.Categories.Count - 1 do
  begin
    ComboBoxLinkedCategory.Items.Add(
      TProjectLogic.GetInstance.Model.Categories.Items[i].Name
    );
  end;
end;

procedure TFormEditSection.FormShow(Sender: TObject);
begin
end;

end.
