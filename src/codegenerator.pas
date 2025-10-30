unit codegenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, datamodel, category, language, StrUtils;

type
  { TCodeGenerator }
  TCodeGenerator = class
  private
    FProject: TProject;
    FIndentLevel: Integer;

    function Indent: String;
    procedure IncIndent;
    procedure DecIndent;

    function EscapeJavaScript(const S: String): String;
    function BoolToJS(B: Boolean): String;

    function GenerateGuiOptions: String;
    function GenerateCategories: String;
    function GenerateLanguages: String;
    function GenerateLanguageTranslation(Lang: TLanguage): String;
    function GenerateConsentModal(Modal: TConsentModal): String;
    function GeneratePreferencesModal(Modal: TPreferencesModal): String;
    function GenerateSections(Sections: TSectionList): String;

  public
    constructor Create(AProject: TProject);

    function Generate: String;
    function GenerateWithCDN: String;

    property Project: TProject read FProject write FProject;
  end;

implementation

{ TCodeGenerator }

constructor TCodeGenerator.Create(AProject: TProject);
begin
  inherited Create;
  FProject := AProject;
  FIndentLevel := 0;
end;

function TCodeGenerator.Indent: String;
begin
  Result := StringOfChar(' ', FIndentLevel * 4);
end;

procedure TCodeGenerator.IncIndent;
begin
  Inc(FIndentLevel);
end;

procedure TCodeGenerator.DecIndent;
begin
  Dec(FIndentLevel);
end;

function TCodeGenerator.EscapeJavaScript(const S: String): String;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

function TCodeGenerator.BoolToJS(B: Boolean): String;
begin
  if B then
    Result := 'true'
  else
    Result := 'false';
end;

function TCodeGenerator.GenerateGuiOptions: String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + 'guiOptions: {');
    IncIndent;

    // Consent Modal
    SL.Add(Indent + 'consentModal: {');
    IncIndent;

    if FProject.DesignConsentModalLayout <> '' then
      SL.Add(Indent + 'layout: "' + FProject.DesignConsentModalLayout + '",');

    if FProject.DesignConsentModalPosition <> '' then
      SL.Add(Indent + 'position: "' + FProject.DesignConsentModalPosition + '",');

    SL.Add(Indent + 'equalWeightButtons: ' + BoolToJS(FProject.DesignConsentModalEqualWeightButtons) + ',');
    SL.Add(Indent + 'flipButtons: ' + BoolToJS(FProject.DesignConsentModalFlipButtons));

    DecIndent;
    SL.Add(Indent + '},');

    // Preferences Modal
    SL.Add(Indent + 'preferencesModal: {');
    IncIndent;

    if FProject.DesignPrefModalLayout <> '' then
      SL.Add(Indent + 'layout: "' + FProject.DesignPrefModalLayout + '",');

    if FProject.DesignPrefModalPosition <> '' then
      SL.Add(Indent + 'position: "' + FProject.DesignPrefModalPosition + '",');

    SL.Add(Indent + 'equalWeightButtons: ' + BoolToJS(FProject.DesignPrefModalEqualWeightButtons) + ',');
    SL.Add(Indent + 'flipButtons: ' + BoolToJS(FProject.DesignPrefModalFlipButtons));

    DecIndent;
    SL.Add(Indent + '}');

    DecIndent;
    SL.Add(Indent + '},');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateCategories: String;
var
  SL: TStringList;
  i: Integer;
  Cat: TCategory;
  Props: TStringList;
begin
  SL := TStringList.Create;
  Props := TStringList.Create;
  try
    SL.Add(Indent + 'categories: {');
    IncIndent;

    for i := 0 to FProject.Categories.Count - 1 do
    begin
      Cat := FProject.Categories[i];
      Props.Clear;

      if Cat.ReadOnly then
        Props.Add('readOnly: true');

      if Cat.PreSelected then
        Props.Add('enabled: true');

      if Props.Count > 0 then
        SL.Add(Indent + Cat.Name + ': {' + Props.CommaText + '}' +
               IfThen(i < FProject.Categories.Count - 1, ',', ''))
      else
        SL.Add(Indent + Cat.Name + ': {}' +
               IfThen(i < FProject.Categories.Count - 1, ',', ''));
    end;

    DecIndent;
    SL.Add(Indent + '},');

    Result := SL.Text;
  finally
    Props.Free;
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateSections(Sections: TSectionList): String;
var
  SL: TStringList;
  i: Integer;
  Sec: TSection;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + 'sections: [');
    IncIndent;

    for i := 0 to Sections.Count - 1 do
    begin
      Sec := Sections[i];
      SL.Add(Indent + '{');
      IncIndent;

      SL.Add(Indent + 'title: "' + EscapeJavaScript(Sec.Title) + '",');
      SL.Add(Indent + 'description: "' + EscapeJavaScript(Sec.Description) + '"');

      if Sec.LinkedCategory <> '' then
        SL.Add(',' + sLineBreak + Indent + 'linkedCategory: "' + Sec.LinkedCategory + '"');

      DecIndent;
      SL.Add(Indent + '}' + IfThen(i < Sections.Count - 1, ',', ''));
    end;

    DecIndent;
    SL.Add(Indent + ']');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateConsentModal(Modal: TConsentModal): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + 'consentModal: {');
    IncIndent;

    SL.Add(Indent + 'title: "' + EscapeJavaScript(Modal.Title) + '",');
    SL.Add(Indent + 'description: "' + EscapeJavaScript(Modal.Description) + '",');
    SL.Add(Indent + 'acceptAllBtn: "' + EscapeJavaScript(Modal.AcceptAllBtn) + '",');
    SL.Add(Indent + 'acceptNecessaryBtn: "' + EscapeJavaScript(Modal.AcceptNecessaryBtn) + '",');
    SL.Add(Indent + 'showPreferencesBtn: "' + EscapeJavaScript(Modal.ShowPreferencesBtn) + '",');
    SL.Add(Indent + 'footer: "' + EscapeJavaScript(Modal.Footer) + '"');

    DecIndent;
    SL.Add(Indent + '},');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GeneratePreferencesModal(Modal: TPreferencesModal): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + 'preferencesModal: {');
    IncIndent;

    SL.Add(Indent + 'title: "' + EscapeJavaScript(Modal.Title) + '",');
    SL.Add(Indent + 'acceptAllBtn: "' + EscapeJavaScript(Modal.AcceptAllBtn) + '",');
    SL.Add(Indent + 'acceptNecessaryBtn: "' + EscapeJavaScript(Modal.AcceptNecessaryBtn) + '",');
    SL.Add(Indent + 'savePreferencesBtn: "' + EscapeJavaScript(Modal.SavePreferencesBtn) + '",');
    SL.Add(Indent + 'closeIconLabel: "' + EscapeJavaScript(Modal.CloseIconLabel) + '",');
    SL.Add(Indent + 'serviceCounterLabel: "' + EscapeJavaScript(Modal.ServiceCounterLabel) + '",');

    SL.Add(GenerateSections(Modal.Sections));

    DecIndent;
    SL.Add(Indent + '}');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateLanguageTranslation(Lang: TLanguage): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + Lang.Code + ': {');
    IncIndent;

    SL.Add(GenerateConsentModal(Lang.Translation.ConsentModal));
    SL.Add(GeneratePreferencesModal(Lang.Translation.PreferencesModal));

    DecIndent;
    SL.Add(Indent + '}' + IfThen(False, ',', '')); // Comma handling needed in parent

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateLanguages: String;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add(Indent + 'language: {');
    IncIndent;

    SL.Add(Indent + 'default: "' + FProject.Languages.DefaultLanguage + '",');
    SL.Add(Indent + 'translations: {');
    IncIndent;

    for i := 0 to FProject.Languages.Count - 1 do
    begin
      SL.Add(GenerateLanguageTranslation(FProject.Languages[i]));
      if i < FProject.Languages.Count - 1 then
        SL[SL.Count - 1] := SL[SL.Count - 1] + ',';
    end;

    DecIndent;
    SL.Add(Indent + '}');

    DecIndent;
    SL.Add(Indent + '}');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.Generate: String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    FIndentLevel := 0;

    // Dark mode comment
    if FProject.DesignEnableDarkMode then
      SL.Add('// Enable dark mode' + sLineBreak +
             'document.documentElement.classList.add(''cc--darkmode'');' + sLineBreak);

    // Main configuration
    SL.Add('CookieConsent.run({');
    IncIndent;

    SL.Add(GenerateGuiOptions);
    SL.Add(GenerateCategories);
    SL.Add(GenerateLanguages);

    DecIndent;
    SL.Add('});');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TCodeGenerator.GenerateWithCDN: String;
begin
  Result := 'import ''https://cdn.jsdelivr.net/gh/orestbida/cookieconsent@3.1.0/dist/cookieconsent.umd.js'';' +
            sLineBreak + sLineBreak + Generate;
end;

end.
