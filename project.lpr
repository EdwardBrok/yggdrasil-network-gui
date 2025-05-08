program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
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
  unitlistlisten, unitthisnode, unitaboutprogram
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Unnamed Yggdrasil GUI';
  Application.Scaled:=True;
  Application.Initialize;
  Application.ShowMainForm := FALSE;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormAboutProgram, FormAboutProgram);
  Application.Run;
end.

