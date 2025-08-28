unit unitAboutProgram;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  LCLIntf,
  math,
  GlobalParameters;

//resourcestring
//  Version = 'Версия: ';
//  Author = 'Автор: ';
//  SourceCode = 'Исходный код:';

type

  { TFormAboutProgram }

  TFormAboutProgram = class(TForm)
    Image1: TImage;
    DisplayNameLabel: TLabel;
    AppVersionLabel: TLabel;
    GithubLabel: TLabel;
    CodebergLabel: TLabel;
    PoweredByImage: TImage;
    AuthorLabel: TLabel;
    SourceCodeLabel: TLabel;
    procedure CodebergLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GithubLabelClick(Sender: TObject);
//    procedure Image2Click(Sender: TObject);
    procedure PoweredByImageClick(Sender: TObject);
  private

  public

  end;

var
  FormAboutProgram: TFormAboutProgram;

implementation

{$R *.lfm}

{ TFormAboutProgram }

procedure TFormAboutProgram.FormCreate(Sender: TObject);
begin
  Caption := GlobalParameters.AppDisplayname + ' - О программе';
  DisplayNameLabel.Caption := AppDisplayName;

  //это тоже съезжает в сторону
  with Image1 do begin
    Left := ceil(FormAboutProgram.Width / 2) - floor(width / 2);
  end;
//  with Image2 do begin
//    Left := ceil(FormAboutProgram.Width / 2) - ceil(width / 2);
//  end;
  with PoweredByImage do begin
    Left := ceil(FormAboutProgram.Width / 2) - floor(width / 2);
  end;
  //AppVersionLabel.Caption := Version + AppVersion;
  //AuthorLabel.Caption := Author + 'edikbrok';
  //SourceCodeLabel.Caption := SourceCFormAboutProgram;
end;

procedure TFormAboutProgram.CodebergLabelClick(Sender: TObject);
begin
  OpenURL('https://codeberg.org/EdwardBrok/yggdrasil-network-gui');
end;

procedure TFormAboutProgram.GithubLabelClick(Sender: TObject);
begin
  OpenURL('https://github.com/EdwardBrok/yggdrasil-network-gui');
end;

//procedure TFormAboutProgram.Image2Click(Sender: TObject);
//begin
//  OpenURL('https://github.com/Revertron');
//end;

procedure TFormAboutProgram.PoweredByImageClick(Sender: TObject);
begin
  OpenUrl('https://www.lazarus-ide.org/');
end;

end.

