unit UnitMain;
{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Menus,
  PopupNotifier,
  StdCtrls,
  Grids,
  System.UITypes,
  {$ifdef MSWINDOWS}
  WinDirs,
  {$endif}

  GlobalParameters,
  UnitGetPeers,
  UnitListPeers,
  UnitListListen,
  UnitSettingsEdit,
  UnitThisNode,
  UnitAboutProgram;

type

  { TFormMain }

  TFormMain = class(TForm)
    AreYouSureFullShutdown: TTaskDialog;
    AreYouSureReboot: TTaskDialog;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    YggdrasilNotFound: TTaskDialog;
    TrayIcon1: TTrayIcon;
    procedure AreYouSureFullShutdownButtonClicked(Sender: TObject;
      AModalResult: TModalResult; var ACanClose: Boolean);
    procedure AreYouSureRebootButtonClicked(Sender: TObject;
      AModalResult: TModalResult; var ACanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure YggdrasilNotFoundButtonClicked(Sender: TObject;
      AModalResult: TModalResult; var ACanClose: Boolean);
    procedure YggdrasilNotFoundClose(Sender: TObject;
      var CloseAction: TCloseAction);
  private

  public

  end;

var
  FormMain: TFormMain;
  output_str: string;

implementation

{$R *.lfm}

{ TFormMain }


procedure TFormMain.FormCreate(Sender: TObject);
var ygg_installed, yggctl_installed: boolean;
//  output: string;
begin
  {$ifdef LINUX}
  ygg_installed := FileExists('/usr/bin/yggdrasil') or
                   DirectoryExists('/usr/lib/yggdrasil') or
                   FileExists('/usr/local/bin/yggdrasil');
  yggctl_installed := FileExists('/usr/bin/yggdrasilctl') or
                      DirectoryExists('/usr/lib/yggdrasilctl') or
                      FileExists('/usr/local/bin/yggdrasilctl');
  if not ygg_installed or not yggctl_installed then
  YggdrasilNotFound.Execute();

  if GetStatusOfYggdrasilService = 'stopped' then
    RestartYggdrasilService;
  {$endif}
  {$ifdef MSWINDOWS}
  ygg_installed := FileExists(GetWindowsSpecialDir(CSIDL_PROGRAM_FILES) + 'Yggdrasil\yggdrasil.exe');
  yggctl_installed := FileExists(GetWindowsSpecialDir(CSIDL_PROGRAM_FILES) + 'Yggdrasil\yggdrasilctl.exe');
  if not ygg_installed or not yggctl_installed then
  YggdrasilNotFound.Execute();

  if GetStatusOfYggdrasilService = 'stopped' then
    RunCommandOverride('sc start yggdrasil');
  {$endif}

end;


procedure TFormMain.MenuItem10Click(Sender: TObject);
var form: TFormGetPeers;
begin
  form := TFormGetPeers.Create(Application);
  form.show();
end;

procedure TFormMain.MenuItem11Click(Sender: TObject);
var form: TFormThisNode;
begin
  form := TFormThisNode.Create(Application);
  form.ShowModal;
  form.free;
end;


procedure TFormMain.MenuItem12Click(Sender: TObject);
begin
  AreYouSureFullShutdown.Execute;
end;


procedure TFormMain.MenuItem3Click(Sender: TObject);
begin
  if FormAboutProgram.Visible then FormAboutProgram.SetFocus
  else FormAboutProgram.ShowModal;
end;


procedure TFormMain.MenuItem5Click(Sender: TObject);
begin
  if FormSettings.Visible then FormSettings.SetFocus
  else FormSettings.ShowModal;
end;

procedure TFormMain.MenuItem6Click(Sender: TObject);
var form: TFormListPeers;
begin
  form := TFormListPeers.Create(Application);
  form.ShowModal;
  form.Free;
end;

procedure TFormMain.MenuItem7Click(Sender: TObject);
begin
  AreYouSureReboot.Execute;
end;

procedure TFormMain.MenuItem8Click(Sender: TObject);
var form: TFormListListen;
begin
  form := TFormListListen.Create(Application);
  form.ShowModal;
  form.free;
end;

procedure TFormMain.MenuItem9Click(Sender: TObject);
begin
  Halt;
end;


procedure TFormMain.AreYouSureFullShutdownButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
begin
  if AModalResult = mrYes then
  begin
    ShutdownYggdrasilService;
    Close;
  end;
end;

procedure TFormMain.AreYouSureRebootButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
begin
  if AModalResult = mrYes then
  begin
    RestartYggdrasilService;
  end;
end;


procedure TFormMain.YggdrasilNotFoundButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
begin
  if AModalResult = mrClose then Halt;
end;


procedure TFormMain.YggdrasilNotFoundClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Close;
end;


end.

