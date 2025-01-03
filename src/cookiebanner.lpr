program cookiebanner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, about, XMLHelper, globalfunctions, datamodel, projectlogic,
  inputdialog, modelhelper,
  appsettings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='CookieBannerGenerator';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.

