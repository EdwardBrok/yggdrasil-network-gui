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
  Registry,
  ShlObj,
  ComObj,
  activex,
  windows,
  {$endif}
  RegExpr;


type
  PSettings = ^TSettings;
  TSettings = Record
    //уже используются
    InitSystem: string[15];            //systemd, sysvinit, openrc, windows или другая
    ConfigFilePath: string[64];        //путь к конфигу игги - изначально берется используемый
    UseSudo: boolean;                  //использование sudo для команд остановки и перезапуска
    UseCustomCommands: boolean;        //использование кастомных (нестандартных) команд
    RestartCustomCommand: string[128];
    ShutdownCustomCommand: string[128];
    AutostartEnabled: boolean;
    LogLevel: byte;

    //надо внедрить
    SettingsVersion: string[10];       //версия настроек - для переноса, обратной совместимости и т.д.
    //Language: string[2];               //
    UpdateFrequency: integer;          //частота обновления в FormGetPeers, мс - от 250 до 5000
  end;


const AppDisplayname: string = 'Yggdrasil GUI';
const AppVersion : string    = '1.3.1';
const SettingsVersionStamp   = '1.3';
const DefaultLogLevel = 0;
const LogLevelsInText : Array[0..4] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');


var
  //запись с настройками - проще хранить все
  //переменные параметров в одном типе
  Settings: TSettings;
  SettingsPtr: PSettings = nil;
  SettingsFilePath: string;
  LogFileStream: TFileStream;
  LogFilePath: string;          //не может быть константой - получаем динамически при инициализации


function  RunCommandOverride(const Command: string): string;
function  GetInitSystem: string;
procedure CreateSettingsRecord;
procedure LoadSettingsRecord(const Path: string);
procedure SaveSettingsRecord(const Path: string);
function  ReadYggdrasilConf: TStringList;
procedure WriteYggdrasilConf(const Path: string; Data: TStringList; SenderObject: TComponent);
procedure RestartYggdrasilService;
procedure ShutdownYggdrasilService;
function  GetStatusOfYggdrasilService: string;
procedure ToggleAutostart(Enabled: boolean);
procedure FirstLaunch;

procedure Log(const Level: byte; message: string);
procedure OpenLogFile;
procedure CreateLogFile;


implementation

procedure Log(const Level: byte; message: string);
var ExportMessage: string;
begin
                                      //в случае отсутствия записи
                                      //настроек на момент вызова
                                      //будет использован переданный уровень

  if ((SettingsPtr = nil) or (Level >= Settings.LogLevel))
     and
     (Level < 5) then
  begin
    ExportMessage := '[' + FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', now) + '] '
                     + '[' + LogLevelsInText[Level] + '] '
                     + message + LineEnding;
    try
      LogFileStream.WriteBuffer(Pointer(ExportMessage)^, Length(ExportMessage));
    except
      on E: Exception do
      begin
        showmessage('Ошибка логгирования: ' + E.Message);
      end;
    end;
  end
  else if Level >= 5 then //5 или выше ничего не означают - можно использовать для вывода особых строк
  begin
    ExportMessage := message + lineending;
    try
      LogFileStream.WriteBuffer(Pointer(ExportMessage)^, Length(ExportMessage));
    finally
    end;
  end;
end;

procedure CreateShortcut(LinkPath: string);
{$ifdef LINUX}
begin
  try
    Log(1, 'Creating autostart shortcut at '+LinkPath+'.');
    //запись во временный файл
    with TStringList.Create do
    try
      Text := '[Desktop Entry]'     + LineEnding +
              'Type=Application'    + LineEnding +
              'Name=Yggdrasil GUI'  + LineEnding +
              'Exec=' + ParamStr(0) + LineEnding +
              'StartupNotify=false' + LineEnding +
              'Terminal=false';
      SaveToFile(LinkPath);
    finally
      Free;
    end;
  except
    on E: Exception do begin
      Log(3, 'Error at creating shortcut: '+e.Message);
      showmessage('Ошибка создания ярлыка для автозапуска: ' + E.Message);
    end;
  end;
end;
{$endif}
{$ifdef MSWINDOWS}
var IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetPath(PChar(ParamStr(0)));
  ISLink.SetWorkingDirectory(PChar(ExtractFilePath(ParamStr(0))));

  IPFile.Save(PWideChar(WideString(LinkPath)), False);
end;
{$endif}


function RunCommandOverride(const Command: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Log(0, 'Running command: '+command);
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
  log(0, 'Getting the name of init system...');
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
  log(0, 'Name of init system has been acquired: '+ outputstr);
  regex.Free;
end;
{$endif}
{$ifdef MSWINDOWS}
begin
  GetInitSystem := 'windows';
  log(0, 'Name of init system has been acquired. Init system: windows. there is no other possible initsys on windows :)' );
end;
{$endif}

{%region logFileMethods}
procedure OpenLogFile;
begin
  LogFileStream := TFileStream.Create(LogFilePath, fmOpenWrite or fmShareDenyWrite);
  LogFileStream.Seek(0, soEnd);
end;

procedure CreateLogFile;
begin
  {$ifdef LINUX}
  if not DirectoryExists(getuserdir + '.var') then CreateDir(getuserdir + '.var');
  if not DirectoryExists(getuserdir + '.var/log') then CreateDir(getuserdir + '.var/log');
  {$endif}
  {$ifdef MSWINDOWS}
  if not DirectoryExists(getuserdir + '.var') then CreateDir(getuserdir + '.var');
  if not DirectoryExists(getuserdir + '.var/log') then CreateDir(getuserdir + '.var/log');
  {$endif}
  LogFileStream := TFileStream.Create(LogFilePath, fmCreate);
end;
{%endregion}


{%region settingsFileMethods}
procedure LoadSettingsRecord(const Path: string);
var SettingsFile: file of TSettings;
begin
  try
    Log(0, 'loading settings record...');
    AssignFile(SettingsFile, Path);
    FileMode := fmOpenReadWrite;
    Reset(SettingsFile);
    Read(SettingsFile, Settings);
    CloseFile(SettingsFile);

    SettingsPtr := @Settings;
  except
    on E:Exception do
    begin
      log(3, 'Error at loading existing settings record: '+ e.Message);
    end;
  end;
  log(0, 'settings record loaded.');
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
  except
    on E: Exception do
    begin
      log(3, 'Error at saving settings record: ' + e.Message);
    end;
  end;
  Close(SettingsFile);
end;
{$endif}
{$ifdef MSWINDOWS} //assignfile() не работает корректно в windows
var OutStream: TFileStream;
begin
  try
    if fileexists(path) then OutStream := TFileStream.Create(Path, fmOpenWrite)
    else OutStream := TFileStream.Create(Path, fmCreate);
    OutStream.Write(Settings, sizeof(Settings));
    OutStream.Free;
  except
    on E: EInOutError do
    begin
      log(3, 'Error at saving settings record: ' + E.ClassName + '/' + E.Message);
      showmessage('File handling error occurred. Details: ' + E.ClassName + '/' + E.Message);
    end;
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
    {$ifdef LINUX}
    ConfigFilePath := '/etc/yggdrasil.conf';
    {$endif}
    {$ifdef MSWINDOWS}
    ConfigFilePath := GetWindowsSpecialDir(CSIDL_COMMON_APPDATA) + 'Yggdrasil\yggdrasil.conf';
    {$endif}

    SettingsVersion := SettingsVersionStamp;
    UseSudo := false;
    UseCustomCommands := false;
    RestartCustomCommand := '';
    ShutdownCustomCommand := '';
    AutostartEnabled := false;
    UpdateFrequency := 1000;
    LogLevel := 1; //1 == info
  end;
  Settings := NewSettingsRecord;
  SettingsPtr := @Settings;
  SaveSettingsRecord(SettingsFilePath);
end;
{%endregion}

{%region yggConfFileMethods}
function ReadYggdrasilConf: TStringList;
begin
  Result := TStringList.Create;
  try
    Result.LoadFromFile(Settings.ConfigFilePath);
  except
    on E: Exception do begin
      log(3, 'Error at reading Yggdrasil CONF file: '+ E.Message);
      ShowMessage('Ошибка чтения файла: ' + E.Message);
    end;
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
  TempFile := GetTempFileName('', 'ygg_config_');

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
      begin
        log(3, 'Failed to write new config with error ' + floattostr(Proc.ExitStatus) + ': ' + Output.Text);
        showMessage('Не удалось перезаписать конфиг.'' Код ошибки: ' + floattostr(Proc.ExitStatus) + '. Сообщение: ' + Output.Text);
      end;
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
    begin
      log(3, 'Failed to write new config: ' + E.Message);
      ShowMessage('Не удалось перезаписать конфиг. Сообщение: ' + E.Message);
    end;
  end;
end;
{$endif}
{%endregion}

{%region yggServiceMethods}
procedure RestartYggdrasilService;
var command, outputstr: string;
begin
  try
    {$ifdef LINUX}
    log(0, 'Restarting the service...');
    if Settings.UseCustomCommands then
    begin
        if Settings.UseSudo then
          RunCommandOverride('pkexec '+ Settings.RestartCustomCommand)
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
        command := 'pkexec ' + command;
      //showmessage(command);
      outputstr := RunCommandOverride(command);

      if (outputstr = 'initsys-not-implemented')
      then begin
        showmessage('К сожалению, для вашей системы инициализации пока нет реализации. Сделайте это вручную. (А лучше помогите проекту:>)');
        log(2, 'Unfortunately there was no implementation for your init system. Do it by yourself. Or help the project.');
      end;
      {$endif}
      {$ifdef MSWINDOWS}
      log(0, 'Stopping the service...');
      RunCommandOverride('sc stop yggdrasil');
      log(0, 'Starting the service...');
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
    log(0, 'Stopping the service...');

    {$ifdef LINUX}
    if Settings.UseCustomCommands then
    begin
      if Settings.UseSudo then
        RunCommandOverride('pkexec '+ Settings.ShutdownCustomCommand)
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
        command := 'pkexec ' + command;

      outputstr := RunCommandOverride(command);
      if outputstr = 'initsys-not-identified' then
      begin
        log(2, 'Unfortunately there was no implementation for your init system. Do it by yourself. Or help the project.');
        showmessage('Для вашей системы инициализации нет реализации :(\n Сделайте это вручную.');
      end;
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
  log(1, 'Checking the Yggdrasil service state...');
  case Settings.InitSystem of
    'systemd':  command := 'systemctl is-active yggdrasil >/dev/null && echo running || echo stopped';
    'sysvinit': command := 'service yggdrasil status >/dev/null 2>&1 && echo running || echo stopped';
    'openrc':   command := 'rc-service yggdrasil status >/dev/null && echo running || echo stopped';
    'unknown':  command := 'echo "ISUnknw"';
  end;
  output := copy(RunCommandOverride(command), 1, 7);
  if output = 'ISUnkwn' then
  begin
    showmessage('Для вашей системы инициализации нет реализации :(\n Программа требует работающей службы Yggdrasil в обязательном порядке.');
    log(2, 'Unfortunately there is no implementation for your init system. Please start the Yggdrasil service by yourself.');
  end

  else
    GetStatusOfYggdrasilService := output;
end;
{$endif}
{$ifdef MSWINDOWS}
begin
  log(1, 'Checking the Yggdrasil service state...');
  GetStatusOfYggdrasilService := copy(RunCommandOverride('sc query yggdrasil | find /c "RUNNING" >nul && echo running || echo stopped'), 1, 7);
end;
{$endif}
{%endregion}


procedure ToggleAutostart(Enabled: boolean);
{$ifdef LINUX}
begin
  log(0, 'Setting YggGUI autostart to: ' + booltostr(Enabled));

  if Enabled then
  begin
    log(0, 'Creating the autostart file...');
    CreateShortcut(SysUtils.GetEnvironmentVariable('HOME') + '/.config/autostart/yggdrasil-gui.desktop')
  end
  else
  begin
    log(0, 'Deleting the autostart file...');
    RunCommandOverride('rm $HOME/.config/autostart/yggdrasil-gui.desktop');
  end;
end;
{$endif}
{$ifdef MSWINDOWS}
var LinkPath: string;
begin
  Log(0, 'getting the autostart file path...');
  LinkPath := GetWindowsSpecialDir(CSIDL_STARTUP) + AppDisplayName + '.lnk';
  log(0, 'autostart file path: '+ linkpath);

  log(0, 'Setting YggGUI autostart to: ' + booltostr(Enabled));

  if Enabled then
  begin
    log(0, 'Creating the autostart file...');
    CreateShortcut(LinkPath)
  end
  else
  begin
    log(0, 'Deleting the autostart file...');
    DeleteFile(PChar(LinkPath));
  end;
end;
{$endif}


{$ifdef MSWINDOWS}
//изменение прав доступа к службе
procedure SetYggdrasilServiceRights; //ONLY WINDOWS
var sid: string;
  regex: TRegExpr;
begin
  log(0, 'Setting up the Yggdrasil service rights... (this is a must for right work)');
  regex := tregexpr.Create;
  try
    regex.Expression := 'S-1-5-21-[0-9]{10}-[0-9]{10}-[0-9]{10}-[0-9]{4}';
    regex.exec(RunCommandOverride('whoami /user /fo csv /nh'));
    sid := regex.Match[0];
    showmessage('Сейчас программа изменит необходимые права для управления службой Yggdrasil. Ссылки на исходный код программы доступен во вкладке "О программе".');
    ShellExecute(0, 'runas', PChar('cmd.exe'), PChar('/c sc sdset yggdrasil D:(A;;CCLCSWRPWPDTLOCRRC;;;SY)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;BA)(A;;CCLCSWLOCRRC;;;IU)(A;;CCLCSWRPWPDTLOCRRC;;;'+sid+')'), nil, SW_SHOWNORMAL);
  finally
    regex.free;
  end;
end;
{$endif}


procedure FirstLaunch;
begin
  CreateSettingsRecord;

  {$ifdef MSWINDOWS}
  SetYggdrasilServiceRights;
  {$endif}
end;


initialization

{$ifdef LINUX}
LogFilePath := getuserdir + '.var/log/ygg-gui.log';
SettingsFilePath := getuserdir + '/.ygg-gui.dat';
{$endif}
{$ifdef MSWINDOWS}
LogFilePath := getuserdir + 'Documents\.ygg-gui.log';
SettingsFilePath := getuserdir + 'Documents\.ygg-gui.dat';
{$endif}

if not FileExists(LogFilePath)
then
  CreateLogFile
else
  OpenLogFile;

log(9, lineending +'======= '+ FormatDateTime('YYYY-MM-DD hh:mm', now) +' =========================================');

if not FileExists(SettingsFilePath)
then
  Firstlaunch
else
  LoadSettingsRecord(SettingsFilePath);


end.

