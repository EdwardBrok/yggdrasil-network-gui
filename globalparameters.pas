unit GlobalParameters;


{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
  Process,
  RegExpr;

type
  TSettings = Record
    //уже используются
    InitSystem: string[15];            //systemd, sysvinit, openrc, windows или другая
    ConfigFilePath: string[64];        //путь к конфигу игги - изначально берется используемый
    UseSudo: boolean;                  //использование sudo для команд остановки и перезапуска
    UseCustomCommands: boolean;        //использование кастомных (нестандартных) команд
    RestartCustomCommand: string[128];
    ShutdownCustomCommand: string[128];

    //надо внедрить
    SettingsVersion: string[10];       //версия настроек - для обратной совместимости
    //Language: string[2];           //
    //UpdateFrequency: 250 .. 5000;  //частота обновления в FormGetPeers, мс
  end;


const AppDisplayname: string = 'Yggdrasil GUI';
const AppVersion : string    = '1.0.1';           //16-ричное число - повыебываться)
const SettingsVersionStamp   = '1.0.0';

var
  Settings: TSettings; //запись с настройками - кмк проще хранить все
                       //переменные параметров в одном типе

function  RunCommandLinux(const Command: string): string;
function  RunCommandWindows(const Command: string): string;
function  GetInitSystem: string;
procedure CreateSettingsRecord;
procedure LoadSettingsRecord(const Path: string);
procedure SaveSettingsRecord(const Path: string);
function  ReadYggdrasilConf(const Path: string): TStringList;
procedure WriteYggdrasilConf(const Path: string; Data: TStringList; SenderObject: TComponent);
procedure RestartYggdrasilService;
procedure ShutdownYggdrasilService;


implementation

function RunCommandLinux(const Command: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'sh';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Command);
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;


//пока не тестировалась и не используется (v1)
function RunCommandWindows(const Command: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'cmd.exe';
    //Process.Parameters.Add('-c');
    Process.Parameters.Add(Command);
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;


//определение системы инициализации - нужно корректно понять, как дергать иггу
function GetInitSystem: string;
var outputstr: string;
    regex: TRegExpr;
begin
  regex := TRegExpr.Create;
  regex.Expression := '([a-zA-Z0-9\-\_\.]*)';
  regex.ModifierG:=true;
  regex.ModifierM:=true;
  regex.Exec(RunCommandLinux('head -n 1 /proc/1/comm 2>/dev/null || echo "unknown"'));
  outputstr := regex.Match[0];
  case outputstr of
    'init':
      if FileExists('/etc/rc.conf') then
        GetInitSystem := 'openrc'
      else if DirectoryExists('/etc/init.d/') then
        GetInitSystem := 'sysvinit';
    'openrc-init':
      GetInitSystem := 'openrc';
  else
    GetInitSystem := outputstr;
  end;
end;


//создание записи с настройками
procedure CreateSettingsRecord;
var NewSettingsRecord: TSettings;
begin
  with NewSettingsRecord do
  begin
    InitSystem := GetInitSystem;
    ConfigFilePath := '/etc/yggdrasil.conf';

    SettingsVersion := '1.0.0';
    UseSudo := false;
    UseCustomCommands := false;
    RestartCustomCommand := '';
    ShutdownCustomCommand := '';
  end;
  Settings := NewSettingsRecord;
  SaveSettingsRecord(getuserdir + '/.ygg-gui.dat');
end;


//загрузка существующей записи с настройками из файла
procedure LoadSettingsRecord(const Path: string);
var SettingsFile: file of TSettings;
begin
  AssignFile(SettingsFile, Path);
  Reset(SettingsFile);
  Read(SettingsFile, Settings);
  Close(SettingsFile);
end;


procedure SaveSettingsRecord(const Path: string);
var SettingsFile: file of TSettings;
begin
  if not fileexists(path) then filecreate(path);
  AssignFile(SettingsFile, Path);
  try
    Rewrite(SettingsFile);
    Write(SettingsFile, Settings);
  finally
    Close(SettingsFile);
  end;
end;


function ReadYggdrasilConf(const Path: string): TStringList;
begin
  Result := TStringList.Create;
  try
    Result.LoadFromFile(Path);
  except
    on E: Exception do
      ShowMessage('Ошибка чтения файла: ' + E.Message);
  end;
end;

//sender нужен для прикрепления формы с вводом к вызывающей форме
procedure WriteYggdrasilConf(const Path: string; Data: TStringList; SenderObject: TComponent);
var Proc: TProcess;
  TempFile: string;
  Output: TStringList;
begin
  TempFile := GetTempFileName('', 'config_');
  Output := TStringList.Create;

  try
    //запись во временный файл
    with TStringList.Create do
    try
      Text := Data.Text;
      SaveToFile(TempFile);
    finally
      Free;
    end;

    //копирование временного файла в /etc/
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := 'pkexec';
      Proc.Parameters.Add('cp');
      Proc.Parameters.Add(TempFile);
      Proc.Parameters.Add(Path);
      Proc.Options := [poUsePipes, poWaitOnExit];
      Proc.Execute;

      Output.LoadFromStream(Proc.Stderr);

      if Proc.ExitStatus <> 0 then
        showMessage('Не удалось перезаписать конфиг.'' Код ошибки: ' + floattostr(Proc.ExitStatus) + '. Сообщение: ' + Output.Text);
    finally
      Proc.Free;
    end;
  finally
    DeleteFile(TempFile);
  end;
end;


procedure RestartYggdrasilService;
var command, outputstr: string;
begin
  try
    if Settings.UseCustomCommands then
    begin
      if Settings.InitSystem <> 'windows' then
        if Settings.UseSudo then
          RunCommandLinux('sudo '+ Settings.RestartCustomCommand)
        else
          RunCommandLinux(Settings.RestartCustomCommand)

      else
        RunCommandWindows(Settings.RestartCustomCommand);
    end
    else
    begin
      case Settings.InitSystem of
        'systemd':  command := 'systemctl restart yggdrasil';
        'sysvinit': command := 'service yggdrasil restart';
        'openrc':   command := 'rc-service yggdrasil restart';
        'unknown':  command := 'echo initsys-not-implemented';
        'windows':  begin RunCommandWindows('sc start yggdrasil'); exit end  //необходимо протестировать
      end;
      if (Settings.UseSudo) and (Settings.InitSystem <> 'windows') then
        command := 'sudo ' + command;
      showmessage(command);
      outputstr := RunCommandLinux(command);

      if (outputstr = 'initsys-not-implemented')
      then showmessage('К сожалению, для вашей системы инициализации пока нет реализации. Сделайте это вручную. (А лучше помогите проекту:>)');
    end;
  finally
  end;
end;


procedure ShutdownYggdrasilService;
var command, outputstr: string;
begin
  try
    if Settings.UseCustomCommands then
    begin
      if Settings.InitSystem <> 'windows' then
        if Settings.UseSudo then
          RunCommandLinux('sudo '+ Settings.ShutdownCustomCommand)
        else
          RunCommandLinux(Settings.ShutdownCustomCommand)

      else
        RunCommandWindows(Settings.ShutdownCustomCommand);
    end
    else
    begin
      case Settings.InitSystem of
        'systemd':  command := 'systemctl stop yggdrasil';
        'sysvinit': command := 'service yggdrasil stop';
        'openrc':   command := 'rc-service yggdrasil stop';
        'unknown':  command := 'echo "initsys-not-identified"';
        'windows':  begin RunCommandWindows('sc stop yggdrasil'); exit end  //необходимо протестировать
      end;
      if (Settings.UseSudo) and (Settings.InitSystem <> 'windows') then
        command := 'sudo ' + command;
      showmessage(command);

      outputstr := RunCommandLinux(command);
      if outputstr = 'initsys-not-identified'
      then showmessage('Для вашей системы инициализации нет реализации :(\n Сделайте это вручную.');
    end;
  finally
  end;
end;


procedure FirstLaunch;
begin
  CreateSettingsRecord;
end;



initialization

if not FileExists(getuserdir + '/.ygg-gui.dat') then Firstlaunch
else LoadSettingsRecord(getuserdir + '/.ygg-gui.dat');

end.

