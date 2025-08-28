unit unitThisNode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Process,
  RegExpr,
  Clipbrd,
  GlobalParameters;

type

  { TFormThisNode }

  TFormThisNode = class(TForm)
    BuildNameEdit: TEdit;
    BuildVerEdit: TEdit;
    AddressEdit: TEdit;
    SubnetEdit: TEdit;
    PubKeyEdit: TEdit;
    RouteTableSizeEdit: TEdit;
    PropertiesLabel: TLabel;
    BuildNameLabel: TLabel;
    BuildVerLabel: TLabel;
    AddressLabel: TLabel;
    SubnetLabel: TLabel;
    RouteTableSizeLabel: TLabel;
    PubKeyLabel: TLabel;
    procedure BuildNameEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormThisNode: TFormThisNode;

implementation

{$R *.lfm}

{ TFormThisNode }


procedure TFormThisNode.FormCreate(Sender: TObject);
var process: TProcess;
  output: TStringList;
  regex: TRegExpr;
  edits : Array of TEdit;
  i: integer;
begin
  log(0, 'showing FormThisNode');
  Caption := GlobalParameters.AppDisplayname + ' - Об этом узле';
  edits := [BuildNameEdit, BuildVerEdit, AddressEdit, SubnetEdit, RouteTableSizeEdit, PubKeyEdit];

  regex := TRegExpr.Create;
  regex.Expression := '^(Build\sname|Build\sversion|IPv6\saddress|IPv6\ssubnet|Routing\stable\ssize|Public\skey):\s*(.+)$';
  regex.ModifierG := true;
  regex.ModifierM := true;

  {$ifdef MSWINDOWS}
  begin
    output := TStringList.Create;
    Process := TProcess.Create(nil);
    try
      Process.Executable := '"C:\Program Files\Yggdrasil\yggdrasilctl.exe"';
      Process.Parameters.Add('getSelf');
      Process.Options := [poUsePipes, poWaitOnExit, poNoConsole];
      Process.Execute;
      output.LoadFromStream(Process.Output);
    finally
      Process.Free;
    end;
  end;
  {$endif}
  {$ifdef LINUX}
  begin //linux
    output := TStringList.Create;
    Process := TProcess.Create(nil);
    try
      Process.Executable := 'yggdrasilctl';
      Process.Parameters.Add('getSelf');
      Process.Options := [poUsePipes, poWaitOnExit];
      Process.Execute;
      output.LoadFromStream(Process.Output);
    finally
      Process.Free;
    end;
  end;
  {$endif}

  for i := 0 to 5 do
  begin
    regex.Exec(output[i]);
    //showmessage(regex.Match[2]);
    edits[i].Text := trim(regex.Match[2]);
  end;

  //дописать
end;

procedure TFormThisNode.BuildNameEditClick(Sender: TObject);
begin
  log(0, 'FormThisNode: clicked ' + (Sender as TEdit).Name + ' and copied its value to clipboard');
  (Sender as TEdit).SelectAll;
  (Sender as TEdit).CopyToClipboard;
end;


end.

