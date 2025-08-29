program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  lazcontrols,
  {$ifdef MSWINDOWS}
  sysutils,
  {$endif}
  unitmain,
  unitgetpeers,
  unitlistpeers,
  globalparameters, 
  unitsettingsedit, 
  unitlistlisten, unitthisnode, unitaboutprogram, unitfindpeers;

{$R *.res}
{$IFDEF MSWINDOWS}
  {$R win_dlls.rc}
{$ENDIF}

begin
  RequireDerivedFormResource:=True;
  Application.Title := 'Yggdrasil GUI';
  Application.Scaled := True;
  Application.Initialize;
  Application.ShowMainForm := FALSE;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAboutProgram, FormAboutProgram);
  Application.Run;
end.

