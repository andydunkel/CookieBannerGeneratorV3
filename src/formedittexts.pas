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

    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonUpClick(Sender: TObject);
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
  FormEditSection := TFormEditSection.Create(Self);
  try
    if FormEditSection.ShowModal = mrOK then
    begin
      // Add the new section to the current language
      FLanguage.Translation.PreferencesModal.Sections.Add(
        FormEditSection.EditTitle.Text,
        FormEditSection.MemoDescription.Text,
        FormEditSection.ComboBoxLinkedCategory.Text
      );

      // Refresh the ListView to show the new section
      UpdateSectionList;
    end;
  finally
    FreeAndNil(FormEditSection);
  end;
end;

procedure TFormEditLangTexts.ButtonUpClick(Sender: TObject);
var
  Section: TSection;
  SectionIndex: Integer;
begin
  // Check if a section is selected
  if ListViewBlocks.Selected = nil then
  begin
    Exit;
  end;

  // Get the selected section
  Section := TSection(ListViewBlocks.Selected.Data);
  if not Assigned(Section) then
    Exit;

  // Find the section index
  SectionIndex := FLanguage.Translation.PreferencesModal.Sections.IndexOf(Section);

  // Check if we can move up (not already at the top)
  if SectionIndex > 0 then
  begin
    // Move up in the list
    FLanguage.Translation.PreferencesModal.Sections.MoveUp(SectionIndex);

    // Refresh the ListView
    UpdateSectionList;

    // Reselect the moved item
    if SectionIndex - 1 < ListViewBlocks.Items.Count then
    begin
      ListViewBlocks.Items[SectionIndex - 1].Selected := True;
      ListViewBlocks.Items[SectionIndex - 1].MakeVisible(False);
    end;
  end;
end;

procedure TFormEditLangTexts.ButtonDownClick(Sender: TObject);
var
  Section: TSection;
  SectionIndex: Integer;
begin
  // Check if a section is selected
  if ListViewBlocks.Selected = nil then
  begin
    Exit;
  end;

  // Get the selected section
  Section := TSection(ListViewBlocks.Selected.Data);
  if not Assigned(Section) then
    Exit;

  // Find the section index
  SectionIndex := FLanguage.Translation.PreferencesModal.Sections.IndexOf(Section);

  // Check if we can move down (not already at the bottom)
  if (SectionIndex >= 0) and (SectionIndex < FLanguage.Translation.PreferencesModal.Sections.Count - 1) then
  begin
    // Move down in the list
    FLanguage.Translation.PreferencesModal.Sections.MoveDown(SectionIndex);

    // Refresh the ListView
    UpdateSectionList;

    // Reselect the moved item
    if SectionIndex + 1 < ListViewBlocks.Items.Count then
    begin
      ListViewBlocks.Items[SectionIndex + 1].Selected := True;
      ListViewBlocks.Items[SectionIndex + 1].MakeVisible(False);
    end;
  end;
end;

procedure TFormEditLangTexts.ButtonEditClick(Sender: TObject);
var
  FormEditSection: TFormEditSection;
  Section: TSection;
begin
  // Check if a section is selected
  if ListViewBlocks.Selected = nil then
  begin
    ShowMessage('Please select a section to edit');
    Exit;
  end;

  // Get the selected section
  Section := TSection(ListViewBlocks.Selected.Data);
  if not Assigned(Section) then
    Exit;

  FormEditSection := TFormEditSection.Create(Self);
  try
    // Load current values into the form
    FormEditSection.EditTitle.Text := Section.Title;
    FormEditSection.MemoDescription.Text := Section.Description;
    FormEditSection.ComboBoxLinkedCategory.Text := Section.LinkedCategory;

    if FormEditSection.ShowModal = mrOK then
    begin
      // Update the section with new values
      Section.Title := FormEditSection.EditTitle.Text;
      Section.Description := FormEditSection.MemoDescription.Text;
      Section.LinkedCategory := FormEditSection.ComboBoxLinkedCategory.Text;

      // Refresh the ListView to show the updated section
      UpdateSectionList;
    end;
  finally
    FreeAndNil(FormEditSection);
  end;
end;

procedure TFormEditLangTexts.ButtonDeleteClick(Sender: TObject);
var
  Section: TSection;
  SectionIndex: Integer;
begin
  // Check if a section is selected
  if ListViewBlocks.Selected = nil then
  begin
    MessageDlg('No Selection', 'Please select a section to delete', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Get the selected section
  Section := TSection(ListViewBlocks.Selected.Data);
  if not Assigned(Section) then
    Exit;

  // Confirm deletion
  if MessageDlg('Confirm Delete',
                'Are you sure you want to delete the section "' + Section.Title + '"?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Find the section index in the list
    SectionIndex := FLanguage.Translation.PreferencesModal.Sections.IndexOf(Section);

    if SectionIndex >= 0 then
    begin
      // Delete the section
      FLanguage.Translation.PreferencesModal.Sections.Delete(SectionIndex);

      // Refresh the ListView
      UpdateSectionList;
    end;
  end;
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
  DescriptionText: String;
  MaxLength: Integer;
begin
  ListViewBlocks.Items.Clear;
  MaxLength := 50; // Adjust this value as needed

  // Load all sections for this language
  for i := 0 to FLanguage.Translation.PreferencesModal.Sections.Count - 1 do
  begin
    Section := FLanguage.Translation.PreferencesModal.Sections[i];
    Item := ListViewBlocks.Items.Add;
    Item.Caption := Section.Title;

    // Clean up description: replace newlines with spaces and trim
    DescriptionText := StringReplace(Section.Description, #13#10, ' ', [rfReplaceAll]);
    DescriptionText := StringReplace(DescriptionText, #13, ' ', [rfReplaceAll]);
    DescriptionText := StringReplace(DescriptionText, #10, ' ', [rfReplaceAll]);
    DescriptionText := Trim(DescriptionText);

    // Shorten if too long
    if Length(DescriptionText) > MaxLength then
      DescriptionText := Copy(DescriptionText, 1, MaxLength) + '...';

    Item.SubItems.Add(DescriptionText);

    if Section.LinkedCategory <> '' then
      Item.SubItems.Add(Section.LinkedCategory)
    else
      Item.SubItems.Add('(no category)');
    Item.Data := Section; // Store reference to section object
  end;
end;

end.
