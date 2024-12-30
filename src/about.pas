unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, lclintf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonClose: TButton;
    Image1: TImage;
    LabelFreeware: TLabel;
    LabelAuthor: TLabel;
    LabelTitle: TLabel;
    LabelVersionInfo: TLabel;
    LabelVersion: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TFormAbout.Image1DblClick(Sender: TObject);
begin
  OpenURL('https://schneuse.de/');
end;

end.

