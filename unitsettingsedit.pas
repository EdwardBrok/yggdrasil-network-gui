unit unitsettingsedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, EditBtn, Spin, GlobalParameters;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    AutostartEnabled: TCheckBox;
    ConfigFilePath: TFileNameEdit;
    LabelLogLevel: TLabel;
    LabelAutostartEnabled: TLabel;
    LabelUpdateFrequency: TLabel;
    LabelUseSudo: TLabel;
    LogLevel: TComboBox;
    OKButton: TButton;
    LabelConfigFilePath: TLabel;
    UpdateFrequency: TSpinEdit;
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
    procedure AutostartEnabledChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ConfigFilePathChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LogLevelChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RestartYggdrasilCommandChange(Sender: TObject);
    procedure ShutdownYggdrasilCommandChange(Sender: TObject);
    procedure UpdateCustomCommandsArea;
    procedure FormCreate(Sender: TObject);
    procedure InitSystemChange(Sender: TObject);
    procedure UpdateFrequencyChange(Sender: TObject);
    procedure UseCustomChange(Sender: TObject);
    procedure SetApplyButtonsState(state: boolean);
    procedure UseSudoChange(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.SetApplyButtonsState(State: boolean);
begin
  log(0, 'FormSettings: setting apply buttons state to:' + booltostr(state, true));
  OKButton.Enabled := state;
  ApplyButton.Enabled := state;
end;

procedure TFormSettings.UseSudoChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;

procedure TFormSettings.ApplySettings;
begin
  log(1, 'Applying new settings...');
  with Settings do
  begin
    //InitSystem := FormSettings.InitSystem.Text;
    ConfigFilePath := FormSettings.ConfigFilePath.Text;

    UseSudo := FormSettings.UseSudo.Checked;
    UseCustomCommands := FormSettings.UseCustom.Checked;
    if FormSettings.AutostartEnabled.Checked <> AutostartEnabled then
    begin
      AutostartEnabled := FormSettings.AutostartEnabled.Checked;
      ToggleAutostart(AutostartEnabled);
    end;
    RestartCustomCommand := FormSettings.RestartYggdrasilCommand.Text;
    ShutdownCustomCommand := FormSettings.ShutdownYggdrasilCommand.Text;
    UpdateFrequency := FormSettings.UpdateFrequency.Value;
    LogLevel := FormSettings.LogLevel.ItemIndex;
  end;

  SaveSettingsRecord(SettingsFilePath);
  SetApplyButtonsState(false);
end;


procedure TFormSettings.AutostartEnabledChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;


procedure TFormSettings.ApplyButtonClick(Sender: TObject);
begin
  ApplySettings;
end;


procedure TFormSettings.CancelButtonClick(Sender: TObject);
begin
  close;
end;

procedure TFormSettings.ConfigFilePathChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;

procedure TFormSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  log(0, 'closing the formSettings window.');
end;

procedure TFormSettings.LogLevelChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;

procedure TFormSettings.OKButtonClick(Sender: TObject);
begin
  ApplySettings;
  log(0, 'Closing FormSettings window');
  Close;
end;

procedure TFormSettings.RestartYggdrasilCommandChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;

procedure TFormSettings.ShutdownYggdrasilCommandChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
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
      UseSudo.Enabled := false;
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
  AutostartEnabled.Checked := Settings.AutostartEnabled;
  UpdateFrequency.Value := Settings.UpdateFrequency;
  LogLevel.ItemIndex := Settings.LogLevel;
  //Caption := GlobalParameters.AppDisplayname + ' - Настройки'; //нужна система локализации
  SetApplyButtonsState(false);
end;


procedure TFormSettings.InitSystemChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
  UpdateCustomCommandsArea;
end;

procedure TFormSettings.UpdateFrequencyChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
end;


procedure TFormSettings.UseCustomChange(Sender: TObject);
begin
  SetApplyButtonsState(true);
  CustomCommands.Enabled := UseCustom.Checked;
  if UseCustom.Checked then
  begin
    RestartYggdrasilCommand.Text := Settings.RestartCustomCommand;
    ShutdownYggdrasilCommand.Text := Settings.ShutdownCustomCommand;
  end;
end;

end.

