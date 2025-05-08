unit UnitGetPeers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Process,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  RegExpr,
  GlobalParameters;

type
  TYggdrasilPeer = record
    URI: string;
    State: string;
    Direction: string;
    IPAddress: string;
    Uptime: string;
    RTT: string;
    RX: string;
    TX: string;
    Down: string;
    Up: string;
    Priority: string;
    Cost: string;
    LastError: string;
  end;

  TArrayOfYggdrasilPeers = Array of TYggdrasilPeer;


  { TFormGetPeers }

  TFormGetPeers = class(TForm)
    ListView1: TListView;
    UpdatePeersTableTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure UpdatePeersTableTimerTimer(Sender: TObject);
    procedure UpdatePeersTable;
    function GetYggdrasilPeers: TStringList;
    function ParseYggdrasilPeers(Output: TStringList): TArrayOfYggdrasilPeers;
  private

  public

  end;

var
  FormGetPeers: TFormGetPeers;

implementation

{$R *.lfm}

{ TFormGetPeers }

//TODO переписать под использование GlobalParameters.RunCommand*
function TFormGetPeers.GetYggdrasilPeers: TStringList;
var
  Process: TProcess;
begin //неоптимизированный говнокод?
  Result := TStringList.Create;
  Process := TProcess.Create(nil);
  try
    {$ifdef LINUX}
    Process.Executable := 'yggdrasilctl';
    {$endif}
    {$ifdef MSWINDOWS}
    Process.Executable := '"C:\Program Files\Yggdrasil\yggdrasilctl.exe"';
    {$endif}
    Process.Parameters.Add('getPeers');
    Process.Options := [poUsePipes, poWaitOnExit, poNoConsole];
    Process.Execute;
    Result.LoadFromStream(Process.Output);
  finally
    Process.Free;
  end;
end;


function TFormGetPeers.ParseYggdrasilPeers(Output: TStringList): TArrayOfYggdrasilPeers;
var
  i: Integer;
  RegEx: TRegExpr;
  Peer: TYggdrasilPeer;
begin
  SetLength(Result, 0);
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := '^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$';
    for i := 1 to Output.Count - 1 do
    begin
      if RegEx.Exec(Output[i]) then
      begin
        Peer.URI        := RegEx.Match[1];
        Peer.State      := RegEx.Match[2];
        Peer.Direction  := RegEx.Match[3];
        Peer.IPAddress  := RegEx.Match[4];
        Peer.Uptime     := RegEx.Match[5];
        Peer.RTT        := RegEx.Match[6];
        Peer.RX         := RegEx.Match[7];
        Peer.TX         := RegEx.Match[8];
        Peer.Down       := RegEx.Match[9];
        Peer.Up         := RegEx.Match[10];
        Peer.Priority   := RegEx.Match[11];
        Peer.Cost       := RegEx.Match[12];
        Peer.LastError  := RegEx.Match[13];

        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Peer;
      end;
    end;
  finally
    RegEx.Free;
  end;
end;


procedure TFormGetPeers.UpdatePeersTable();
var
  Output: TStringList;
  Peers: TArrayOfYggdrasilPeers;
  i: Integer;
  Item: TListItem;
begin
  Output := GetYggdrasilPeers;
  try
    Peers := ParseYggdrasilPeers(Output);
    ListView1.Items.Clear;
    for i := 0 to High(Peers) do
    begin
      Item := ListView1.Items.Add;
      Item.Caption := Peers[i].URI;
      Item.SubItems.Add(Peers[i].State);
      Item.SubItems.Add(Peers[i].Direction);
      Item.SubItems.Add(Peers[i].IPAddress);
      Item.SubItems.Add(Peers[i].Uptime);
      Item.SubItems.Add(Peers[i].RTT);
      Item.SubItems.Add(Peers[i].RX);
      Item.SubItems.Add(Peers[i].TX);
      Item.SubItems.Add(Peers[i].Down);
      Item.SubItems.Add(Peers[i].Up);
      Item.SubItems.Add(Peers[i].Priority);
      Item.SubItems.Add(Peers[i].Cost);
      Item.SubItems.Add(Peers[i].LastError);
    end;
  finally
    Output.Free
  end;
end;


procedure TFormGetPeers.FormCreate(Sender: TObject);
begin
  Caption := GlobalParameters.AppDisplayname + ' - Все узлы'; //нужна система локализации
end;

procedure TFormGetPeers.UpdatePeersTableTimerTimer(Sender: TObject);
begin
  UpdatePeersTable();
end;


end.

