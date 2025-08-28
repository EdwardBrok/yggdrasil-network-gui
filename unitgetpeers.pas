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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  log(0, 'getting the peers statistic directly from the ygg service...');
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
  except
    on E: Exception do
    begin
      showmessage('Ошибка получения статистики по пирам: ' + E.Message);
      log(3, 'Error at getting peers statistics: '+E.Message);
    end;
  end;
  Process.Free;
end;


function TFormGetPeers.ParseYggdrasilPeers(Output: TStringList): TArrayOfYggdrasilPeers;
var
  i: Integer;
  RegEx: TRegExpr;
  Peer: TYggdrasilPeer;
begin
  log(0, 'Parsing output...');
  SetLength(Result, 0);
  RegEx := TRegExpr.Create;
  try
    //RegEx.Expression := '^(\S+)\s+(\S+)\s+(\S+)\s*\t(\S+\s*|\s*)\t(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$';
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
  except
    on E: Exception do
    begin
      showmessage('Ошибка парсинга статистики по пирам: ' + E.Message);
      log(3, 'Error at parsing peers statistics: '+E.Message);
    end;
  end;
  Regex.Free;
end;


procedure TFormGetPeers.UpdatePeersTable();
var
  Output: TStringList;
  Peers: TArrayOfYggdrasilPeers;
  i: Integer;
  Item: TListItem;
begin
  log(0, 'Updating Peers table...');
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
  except
    on E: Exception do
    begin
      showmessage('Ошибка обновления таблицы статистики по пирам: ' + E.Message);
      log(3, 'Error at updating table of peers statistics: '+E.Message);
    end;
  end;

end;


procedure TFormGetPeers.FormCreate(Sender: TObject);
begin
  log(0, 'FormGetPeers was created');
  //Caption := GlobalParameters.AppDisplayname + ' - Все узлы'; //нужна система локализации
  UpdatePeersTableTimer.Interval := Settings.UpdateFrequency;
  //UpdatePeersTable();
end;

procedure TFormGetPeers.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  log(0, 'closing FormGetPeers...');
  CloseAction := caFree;
end;

procedure TFormGetPeers.UpdatePeersTableTimerTimer(Sender: TObject);
begin
  UpdatePeersTable();
end;


end.

