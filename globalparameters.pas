unit GlobalParameters;


{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
  Process,
  {$ifdef MSWINDOWS}
  WinDirs,
  StrUtils,
  {$endif}
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

function  RunCommandOverride(const Command: string): string;
function  GetInitSystem: string;
procedure CreateSettingsRecord;
procedure LoadSettingsRecord(const Path: string);
procedure SaveSettingsRecord(const Path: string);
function  ReadYggdrasilConf(const Path: string): TStringList;
procedure WriteYggdrasilConf(const Path: string; Data: TStringList; SenderObject: TComponent);
procedure RestartYggdrasilService;
procedure ShutdownYggdrasilService;
function GetStatusOfYggdrasilService: string;
procedure FirstLaunch;

implementation

function RunCommandOverride(const Command: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  {$ifdef LINUX}
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
  {$endif}
  {$ifdef MSWINDOWS}
  try
    Process.Executable := 'cmd.exe';
    Process.Parameters.Add('/c');
    Process.Parameters.Add(command);
    Process.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
  {$endif}
end;


//определение системы инициализации - нужно корректно понять, как дергать иггу
function GetInitSystem: string;
{$ifdef LINUX}
var outputstr: string;
    regex: TRegExpr;
begin
  regex := TRegExpr.Create;
  regex.Expression := '([a-zA-Z0-9\-\_\.]*)';
  regex.ModifierG:=true;
  regex.ModifierM:=true;
  regex.Exec(RunCommandOverride('head -n 1 /proc/1/comm 2>/dev/null || echo "unknown"'));
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
{$endif}
{$ifdef MSWINDOWS}
begin
  GetInitSystem := 'windows';
end;
{$endif}


//загрузка существующей записи с настройками из файла
procedure LoadSettingsRecord(const Path: string);
var SettingsFile: file of TSettings;
begin
  AssignFile(SettingsFile, Path);
  FileMode := fmOpenReadWrite;
  Reset(SettingsFile);
  Read(SettingsFile, Settings);
  CloseFile(SettingsFile);
end;


procedure SaveSettingsRecord(const Path: string);
{$ifdef LINUX}
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
{$endif}
{$ifdef MSWINDOWS} //assignfile() не работает
var SettingsFile: file of TSettings;
    OutStream: TFileStream;
begin
  try
    if fileexists(path) then OutStream := TFileStream.Create(Path, fmOpenWrite)
    else OutStream := TFileStream.Create(Path, fmCreate);
    OutStream.Write(Settings, sizeof(Settings));
    OutStream.Free;
  except
    on E: EInOutError do
      showmessage('File handling error occurred. Details: ' + E.ClassName + '/' + E.Message);
  end;
end;
{$endif}


//создание записи с настройками
procedure CreateSettingsRecord;
var NewSettingsRecord: TSettings;
begin
  with NewSettingsRecord do
  begin
    InitSystem := GetInitSystem;
    ConfigFilePath :=
    {$ifdef LINUX}
    '/etc/yggdrasil.conf';
    {$endif}
    {$ifdef MSWINDOWS}
    GetWindowsSpecialDir(CSIDL_COMMON_APPDATA) + 'Yggdrasil\yggdrasil.conf';
    {$endif}

    SettingsVersion := '1.0.0';
    UseSudo := false;
    UseCustomCommands := false;
    RestartCustomCommand := '';
    ShutdownCustomCommand := '';
  end;
  Settings := NewSettingsRecord;
  {$ifdef LINUX}
  SaveSettingsRecord(getuserdir + '/.ygg-gui.dat');
  {$endif}
  {$ifdef MSWINDOWS}
  SaveSettingsRecord(getuserdir + 'Documents\.ygg-gui.dat'); //запись в бибилиотеку Документы
  {$endif}
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
{$ifdef LINUX}
var Proc: TProcess;
  TempFile: string;
  Output: TStringList;
begin
  Output := TStringList.Create;
  TempFile := GetTempFileName('', 'config_');

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
{$endif}
{$ifdef MSWINDOWS}
begin
  try
    Data.SaveToFile(Path);
  except
    on E: Exception do
      ShowMessage('Не удалось перезаписать конфиг. Сообщение: ' + E.Message);
  end;
end;
{$endif}


procedure RestartYggdrasilService;
var command, outputstr: string;
begin
  try
    {$ifdef LINUX}
    if Settings.UseCustomCommands then
    begin
        if Settings.UseSudo then
          RunCommandOverride('sudo '+ Settings.RestartCustomCommand)
        else
          RunCommandOverride(Settings.RestartCustomCommand)
    end
    else
    {$endif}
    begin
      {$ifdef LINUX}
      case Settings.InitSystem of
        'systemd':  command := 'systemctl restart yggdrasil';
        'sysvinit': command := 'service yggdrasil restart';
        'openrc':   command := 'rc-service yggdrasil restart';
        'unknown':  command := 'echo initsys-not-implemented';
      end;
      if (Settings.UseSudo) then
        command := 'sudo ' + command;
      //showmessage(command);
      outputstr := RunCommandOverride(command);

      if (outputstr = 'initsys-not-implemented')
      then showmessage('К сожалению, для вашей системы инициализации пока нет реализации. Сделайте это вручную. (А лучше помогите проекту:>)');
      {$endif}
      {$ifdef MSWINDOWS}
      RunCommandOverride('sc stop yggdrasil');
      RunCommandOverride('sc start yggdrasil');
      {$endif}
    end;
  finally
  end;
end;


procedure ShutdownYggdrasilService;
var command, outputstr: string;
begin
  try
    {$ifdef LINUX}
    if Settings.UseCustomCommands then
    begin
        if Settings.UseSudo then
          RunCommandOverride('sudo '+ Settings.ShutdownCustomCommand)
        else
          RunCommandOverride(Settings.ShutdownCustomCommand)
    end
    else
    {$endif}
    begin
      {$ifdef LINUX}
      case Settings.InitSystem of
        'systemd':  command := 'systemctl stop yggdrasil';
        'sysvinit': command := 'service yggdrasil stop';
        'openrc':   command := 'rc-service yggdrasil stop';
        'unknown':  command := 'echo "initsys-not-identified"';
      end;
      if (Settings.UseSudo) then
        command := 'sudo ' + command;
      //showmessage(command);

      outputstr := RunCommandOverride(command);
      if outputstr = 'initsys-not-identified' then
        showmessage('Для вашей системы инициализации нет реализации :(\n Сделайте это вручную.');
      {$endif}
      {$ifdef MSWINDOWS}
      RunCommandOverride('sc stop yggdrasil');
      {$endif}
    end;
  finally
  end;
end;


function GetStatusOfYggdrasilService: string;
{$ifdef LINUX}
var command, output: string;
begin
  case Settings.InitSystem of
    'systemd':  command := 'systemctl is-active yggdrasil >/dev/null && echo running || echo stopped';
    'sysvinit': command := 'service yggdrasil status >/dev/null 2>&1 && echo running || echo stopped';
    'openrc':   command := 'rc-service yggdrasil status >/dev/null && echo running || echo stopped';
    'unknown':  command := 'echo "ISUnknw"';
  end;
  output := copy(RunCommandOverride(command), 1, 7);
  if output = 'ISUnkwn' then
    showmessage('Для вашей системы инициализации нет реализации :(\n Программа требует работающей службы Yggdrasil и хотела ее запустить. Сделайте это вручную.')
  else
    GetStatusOfYggdrasilService := output;
end;
{$endif}
{$ifdef MSWINDOWS}
begin
  GetStatusOfYggdrasilService := copy(RunCommandOverride('sc query yggdrasil | find /c "RUNNING" >nul && echo running || echo stopped'), 1, 7);
end;
{$endif}


procedure FirstLaunch;
begin
  CreateSettingsRecord;
end;


initialization

{$ifdef LINUX}
if not FileExists(getuserdir + '/.ygg-gui.dat') then Firstlaunch
else LoadSettingsRecord(getuserdir + '/.ygg-gui.dat');
{$endif}
{$ifdef MSWINDOWS}
if not FileExists(getuserdir + 'Documents\.ygg-gui.dat') then FirstLaunch
else LoadSettingsRecord(getuserdir + 'Documents\.ygg-gui.dat');
{$endif}

end.

