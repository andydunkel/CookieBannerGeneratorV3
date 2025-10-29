unit formedittexts;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  datamodel, language, ProjectLogic, formsection;

type
  { TFormEditLangTexts }
  TFormEditLangTexts = class(TForm)
    ButtonDelete: TToolButton;
    ButtonDown: TToolButton;
    ButtonEdit: TToolButton;
    ButtonNew: TToolButton;
    ButtonUp: TToolButton;

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
    StatusBar1: TStatusBar;
    TabSheetConsent: TTabSheet;
    TabSheetPreferences: TTabSheet;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton6: TToolButton;
    ToolButtonClose: TToolButton;

    procedure ButtonNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ToolButtonCloseClick(Sender: TObject);
  private
    FLanguageIndex: Integer;
    FLanguage: TLanguage;
    procedure LoadLanguageData;
    procedure ModelToForm;
    procedure FormToModel;
    procedure UpdateSectionList;
  public
    property LanguageIndex: Integer read FLanguageIndex write FLanguageIndex;
  end;

var
  FormEditLangTexts: TFormEditLangTexts;

implementation

uses
  mainform;

{$R *.lfm}

{ TFormEditLangTexts }

procedure TFormEditLangTexts.FormShow(Sender: TObject);
begin
  LoadLanguageData;
end;

procedure TFormEditLangTexts.ButtonNewClick(Sender: TObject);
var
  FormEditSection: TFormEditSection;
begin
  FormEditSection:= TFormEditSection.Create(Self);
  FormEditSection.ShowModal;
  FreeAndNil(FormEditSection);
end;

procedure TFormEditLangTexts.PageControl1Change(Sender: TObject);
var
  Enable : Boolean;
begin
  Enable := (PageControl1.ActivePage.TabIndex = 0);
  ButtonNew.Enabled := Enable;
  ButtonDelete.Enabled := Enable;
  ButtonEdit.Enabled := Enable;
  ButtonUp.Enabled := Enable;
  ButtonDown.Enabled := Enable;
end;


procedure TFormEditLangTexts.ToolButtonCloseClick(Sender: TObject);
begin
  FormToModel;
  Close;
end;

procedure TFormEditLangTexts.LoadLanguageData;
begin
  // Validate language index
  if (FLanguageIndex < 0) or
     (FLanguageIndex >= TProjectLogic.GetInstance.Model.Languages.Count) then
  begin
    ShowMessage('Invalid language index');
    Exit;
  end;

  // Get the language object
  FLanguage := TProjectLogic.GetInstance.Model.Languages[FLanguageIndex];

  // Load all texts from model to form
  ModelToForm;
  UpdateSectionList;

  // Update form caption
  Caption := 'Edit Texts - ' + FLanguage.Code;
end;

procedure TFormEditLangTexts.ModelToForm;
begin
  // Load Consent Modal texts
  EditConsentTitle.Text := FLanguage.Translation.ConsentModal.Title;
  MemoConsentDesc.Lines.Text := FLanguage.Translation.ConsentModal.Description;
  EditConsentAcceptAll.Text := FLanguage.Translation.ConsentModal.AcceptAllBtn;
  EditConsentRejectAll.Text := FLanguage.Translation.ConsentModal.AcceptNecessaryBtn;
  EditConsentPrefs.Text := FLanguage.Translation.ConsentModal.ShowPreferencesBtn;
  MemoConsentFooter.Lines.Text := FLanguage.Translation.ConsentModal.Footer;

  // Load Preferences Modal texts
  EditPrefTitle.Text := FLanguage.Translation.PreferencesModal.Title;
  EditPrefAcceptAll.Text := FLanguage.Translation.PreferencesModal.AcceptAllBtn;
  EditPrefRejectAll.Text := FLanguage.Translation.PreferencesModal.AcceptNecessaryBtn;
  EditPrefSave.Text := FLanguage.Translation.PreferencesModal.SavePreferencesBtn;
  EditPrefClose.Text := FLanguage.Translation.PreferencesModal.CloseIconLabel;
  EditPrefServiceCounter.Text := FLanguage.Translation.PreferencesModal.ServiceCounterLabel;
end;

procedure TFormEditLangTexts.FormToModel;
begin
  // Save Consent Modal texts
  FLanguage.Translation.ConsentModal.Title := EditConsentTitle.Text;
  FLanguage.Translation.ConsentModal.Description := MemoConsentDesc.Lines.Text;
  FLanguage.Translation.ConsentModal.AcceptAllBtn := EditConsentAcceptAll.Text;
  FLanguage.Translation.ConsentModal.AcceptNecessaryBtn := EditConsentRejectAll.Text;
  FLanguage.Translation.ConsentModal.ShowPreferencesBtn := EditConsentPrefs.Text;
  FLanguage.Translation.ConsentModal.Footer := MemoConsentFooter.Lines.Text;

  // Save Preferences Modal texts
  FLanguage.Translation.PreferencesModal.Title := EditPrefTitle.Text;
  FLanguage.Translation.PreferencesModal.AcceptAllBtn := EditPrefAcceptAll.Text;
  FLanguage.Translation.PreferencesModal.AcceptNecessaryBtn := EditPrefRejectAll.Text;
  FLanguage.Translation.PreferencesModal.SavePreferencesBtn := EditPrefSave.Text;
  FLanguage.Translation.PreferencesModal.CloseIconLabel := EditPrefClose.Text;
  FLanguage.Translation.PreferencesModal.ServiceCounterLabel := EditPrefServiceCounter.Text;
end;

procedure TFormEditLangTexts.UpdateSectionList;
var
  i: Integer;
  Section: TSection;
  Item: TListItem;
begin
  ListViewBlocks.Items.Clear;

  // Load all sections for this language
  for i := 0 to FLanguage.Translation.PreferencesModal.Sections.Count - 1 do
  begin
    Section := FLanguage.Translation.PreferencesModal.Sections[i];
    Item := ListViewBlocks.Items.Add;
    Item.Caption := Section.Title;
    Item.SubItems.Add(Section.Description);
    if Section.LinkedCategory <> '' then
      Item.SubItems.Add(Section.LinkedCategory)
    else
      Item.SubItems.Add('(no category)');
    Item.Data := Section; // Store reference to section object
  end;
end;

end.
