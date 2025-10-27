unit formedittexts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TFormEditLangTexts }

  TFormEditLangTexts = class(TForm)
    PageControl1: TPageControl;
    TabSheetConsent: TTabSheet;
    TabSheetPreferences: TTabSheet;
  private

  public

  end;

var
  FormEditLangTexts: TFormEditLangTexts;

implementation

{$R *.lfm}

end.

