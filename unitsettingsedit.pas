unit unitsettingsedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, EditBtn, GlobalParameters;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    ConfigFilePath: TFileNameEdit;
    LabelUseSudo: TLabel;
    OKButton: TButton;
    ConfigFilePathLabel: TLabel;
    UseCustom: TCheckBox;
    InitSystem: TComboBox; //временно скрыто. если ненужность подтвердится, удалить
    RestartYggdrasilCommand: TEdit;
    ShutdownYggdrasilCommand: TEdit;
    CustomCommands: TGroupBox;
    LabelInitSystem: TLabel;
    LabelRestart: TLabel;
    LabelShutdown: TLabel;
    LabelUseCustom: TLabel;
    Main: TPageControl;
    ServiceControlPage: TTabSheet;
    OtherPage: TTabSheet;
    UseSudo: TCheckBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure ApplySettings;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure UpdateCustomCommandsArea;
    procedure FormCreate(Sender: TObject);
    procedure InitSystemChange(Sender: TObject);
    procedure UseCustomChange(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.ApplySettings;
begin
  with Settings do
  begin
    //InitSystem := FormSettings.InitSystem.Text;
    ConfigFilePath := FormSettings.ConfigFilePath.Text;

    UseSudo := FormSettings.UseSudo.Checked;
    UseCustomCommands := FormSettings.UseCustom.Checked;
    RestartCustomCommand := FormSettings.RestartYggdrasilCommand.Text;
    ShutdownCustomCommand := FormSettings.ShutdownYggdrasilCommand.Text;
  end;

  SaveSettingsRecord
  {$ifdef LINUX}
  (getuserdir + '/.ygg-gui.dat');
  {$endif}
  {$ifdef MSWINDOWS}
  (getuserdir + '\Documents\.ygg-gui.dat');
  {$endif}
end;

procedure TFormSettings.ApplyButtonClick(Sender: TObject);
begin
  ApplySettings;
end;

procedure TFormSettings.CancelButtonClick(Sender: TObject);
begin
  close;
end;

procedure TFormSettings.OKButtonClick(Sender: TObject);
begin
  ApplySettings;
  Close;
end;

procedure TFormSettings.UpdateCustomCommandsArea;
begin
  //временно захардкоджено. нужно более элегантное решение
  UseCustom.Checked := Settings.UseCustomCommands;
  CustomCommands.Enabled := Settings.UseCustomCommands;
  if Settings.UseCustomCommands then
  begin
    RestartYggdrasilCommand.Text := Settings.RestartCustomCommand;
    ShutdownYggdrasilCommand.Text := Settings.ShutdownCustomCommand;
  end
  else case InitSystem.Text of
    'systemd': begin
      RestartYggdrasilCommand.Text := 'systemctl restart yggdrasil';
      ShutdownYggdrasilCommand.Text := 'systemctl stop yggdrasil';
    end;
    'sysvinit': begin
      RestartYggdrasilCommand.Text := 'service yggdrasil restart';
      ShutdownYggdrasilCommand.Text := 'service yggdrasil stop';
    end;
    'openrc': begin
      RestartYggdrasilCommand.Text := 'rc-service yggdrasil restart';
      ShutdownYggdrasilCommand.Text := 'rc-service yggdrasil stop';
    end;
    'unknown': begin
      RestartYggdrasilCommand.Text := '';
      ShutdownYggdrasilCommand.Text := '';
    end;
    'windows': begin
      //InitSystem.ReadOnly := true;
      UseCustom.Enabled := false;
      RestartYggdrasilCommand.Text := 'sc stop yggdrasil; sc start yggdrasil';
      ShutdownYggdrasilCommand.Text := 'sc stop yggdrasil';
    end;
  end;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  InitSystem.Text := Settings.InitSystem;
  UseSudo.Checked := Settings.UseSudo;
  UpdateCustomCommandsArea;
  ConfigFilePath.Text := Settings.ConfigFilePath;
  Caption := GlobalParameters.AppDisplayname + ' - Настройки'; //нужна система локализации
end;

procedure TFormSettings.InitSystemChange(Sender: TObject);
begin
  UpdateCustomCommandsArea;
end;

procedure TFormSettings.UseCustomChange(Sender: TObject);
begin
  CustomCommands.Enabled := UseCustom.Checked;
  if UseCustom.Checked then
  begin
    RestartYggdrasilCommand.Text := Settings.RestartCustomCommand;
    ShutdownYggdrasilCommand.Text := Settings.ShutdownCustomCommand;
  end;
end;

end.

