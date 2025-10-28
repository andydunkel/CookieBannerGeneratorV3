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
    
    // Consent Modal controls
    EditConsentTitle: TEdit;
    EditConsentAcceptAll: TEdit;
    EditConsentRejectAll: TEdit;
    EditConsentPrefs: TEdit;
    LabelConsentTitle: TLabel;
    LabelConsentDesc: TLabel;
    LabelConsentAcceptAll: TLabel;
    LabelConsentRejectAll: TLabel;
    LabelConsentPrefs: TLabel;
    LabelConsentFooter: TLabel;
    ListViewBlocks: TListView;
    MemoConsentDesc: TMemo;
    MemoConsentFooter: TMemo;
    
    // Preferences Modal controls
    EditPrefTitle: TEdit;
    EditPrefAcceptAll: TEdit;
    EditPrefRejectAll: TEdit;
    EditPrefSave: TEdit;
    EditPrefClose: TEdit;
    EditPrefServiceCounter: TEdit;
    LabelPrefTitle: TLabel;
    LabelPrefAcceptAll: TLabel;
    LabelPrefRejectAll: TLabel;
    LabelPrefSave: TLabel;
    LabelPrefClose: TLabel;
    LabelPrefServiceCounter: TLabel;
    
    GroupBoxConsentTexts: TGroupBox;
    GroupBoxPrefModal: TGroupBox;
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