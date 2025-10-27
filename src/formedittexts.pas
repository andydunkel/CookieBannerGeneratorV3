unit formedittexts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TFormEditLangTexts }

  TFormEditLangTexts = class(TForm)
    ButtonCatDelete: TToolButton;
    ButtonCatDown: TToolButton;
    ButtonCatEdit: TToolButton;
    ButtonCatNew: TToolButton;
    ButtonCatUp: TToolButton;
    PageControl1: TPageControl;
    TabSheetConsent: TTabSheet;
    TabSheetPreferences: TTabSheet;
    ToolBarCat: TToolBar;
    ToolButton6: TToolButton;
  private

  public

  end;

var
  FormEditLangTexts: TFormEditLangTexts;

implementation

{$R *.lfm}

end.

