unit unitfindpeers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Buttons,
  ExtCtrls, ValEdit, PairSplitter,
  FPHTTPClient,
  FPJson,
  JsonParser,
  JsonScanner,
  openssl,
  opensslsockets,
  math,
  GlobalParameters;

type

//  TPeerData = record
//    Up: Boolean;
//    Key: string;
//    Imported: integer;
//    Updated: integer;
//    LastSeen: integer;
//    ProtoMinor: Byte;
//    States: string;
//    ResponseMs: integer;
//  end;

//  PString = ^string;

  { TThreadGetJson}

  TThreadGetJson = class(TThread)
  private
    FStatusText: string;
    FTargetURL: string;
    ThreadStatus: byte;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    Output: rawbytestring;
    constructor Create(URL: string);
  end;


  { TFormFindPeers }

  TFormFindPeers = class(TForm)
    ButtonConfirmAdding: TButton;
    Checkboxes: TImageList;
    CloseButton: TBitBtn;
    LabelCurrentAction: TLabel;
    LabelNoInternetDescription: TLabel;
    LabelPleaseWait: TLabel;
    LabelNoInternetConnection: TLabel;
    LabelSelectCountry: TLabel;
    PanelNoInternetConnection: TPanel;
    PeersList: TTreeView;
    SelectedPeers: TListView;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PanelMain: TPanel;
    PanelPleaseWait: TPanel;
    PeerParamsList: TValueListEditor;
    ProgressBar: TProgressBar;
    SelectCountry: TComboBox;
    Splitter1: TSplitter;
    TimerResponceNotReceived: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure PanelPleaseWaitClick(Sender: TObject);
    procedure ParsePeersByAnyCountry;
    procedure ParsePeersBySelectedCountry;
    procedure PeersListClick(Sender: TObject);
    procedure PeersListSelectionChanged(Sender: TObject);
    procedure RetrievePublicPeersList;
    procedure TryRetrieveList(serverIndex: byte);
    procedure FinishRetrieve(sender: TObject);
    procedure SelectCountryChange(Sender: TObject);
    procedure HideWaitPanel;
    procedure ShowWaitPanel;
    procedure ShowNoInternetPanel;
    procedure TimerResponceNotReceivedTimer(Sender: TObject);
    procedure UpdateSelectedPeersPanel;
    function  FindPeerInJson(country, peer: string): TJSONObject;


    procedure SetTreeViewCheckboxes(Node: TTreeNode; State: boolean);
    procedure ToggleTreeViewCheckboxes(Node: TTreeNode);
    function  NodeChecked(ANode:TTreeNode): Boolean;
  private

  public
    SelectedPeersList: TStringList;
  end;

const
  ImgIndexUnchecked = 0;   // Image index of checked icon
  ImgIndexChecked = 1;  // Image index of unchecked icon

  TimeoutMs = 5000;
  Servers : array of string = (
//  'http://[200:2688:699a:ce30:5897:cd3d:f999:8782]/publicnodes.json', //ygg address
  'https://publicpeers.neilalexander.dev/publicnodes.json',
  'https://peers.yggdrasil.link/publicnodes.json');

var
  FormFindPeers: TFormFindPeers;
  ReceivedJson: TJsonData;
  thread: TThreadGetJson;
  CurrentServerIndex: byte;

implementation

{$R *.lfm}

{%region thread}
constructor TThreadGetJson.Create(URL: string);
begin
  FreeOnTerminate := true;
  FTargetURL := URL;
  inherited Create(true);
  ThreadStatus := 0; //не получил ответа, 1 - получил и может выйти
end;

procedure TThreadGetJson.ShowStatus;
begin
  FormFindPeers.LabelCurrentAction.Caption := FStatusText;
end;

procedure TThreadGetJson.Execute;
begin
  FStatusText := 'Соединяемся с ' + ftargeturl + '...';
  log(0, 'FormFindPeers -> ThreadGetJson: connecting to ' + ftargeturl + '...');
  Synchronize(@showstatus);
  with TFPHttpClient.Create(nil) do
  begin
    try
      ConnectTimeout := TimeoutMs;
      FStatusText := 'Получение пиров...';
      synchronize(@showstatus);

      Output := Get(ftargetUrl);

      with TJsonParser.Create(Output, defaultOptions) do
        receivedjson := Parse;

      Log(0, 'FormFindPeers: Retrieving finished successfully.');
      ThreadStatus := 1;
    except
      on E: Exception do
        if E.ClassName = 'ESocketError' then
        begin
          FStatusText := 'Проблема сети:' + E.Message;
          log(3, 'FormFindPeers -> ThreadGetJson: there is some network issue: ' + E.Message);
          Synchronize(@showstatus);
          Output := 'network_issue';
        end
        else
        begin
          FStatusText := 'Ошибка получения JSON: ' + E.Message;
          log(3, 'FormFindPeers -> ThreadGetJson: error at receiving json: ' + E.Message);
          Synchronize(@showstatus);
          Output := 'network_issue';
        end;
    end;
  end;
end;
{%endregion}



{%region node_methods}
procedure TFormFindPeers.SetTreeViewCheckboxes(Node: TTreeNode; State: boolean);
var i: integer;
begin
  if Assigned(Node) and (Node.StateIndex > -1) then
  begin
    if not state then
      begin
        Node.StateIndex := ImgIndexUnchecked;
        if SelectedPeersList.Find(Node.Text, i) then
          SelectedPeersList.Delete(i);
      end
    else
      begin
        Node.StateIndex := ImgIndexChecked;
        SelectedPeersList.Add(Node.Text);
      end;
  end;
end;

procedure TFormFindPeers.ToggleTreeViewCheckboxes(Node: TTreeNode);
var i: integer;
begin
  if Assigned(Node) and (Node.StateIndex > -1) then
  begin
    if Node.StateIndex = ImgIndexUnchecked then
    begin
      Node.StateIndex := ImgIndexChecked;
      SelectedPeersList.Add(Node.Text);
    end
    else
    if Node.StateIndex = ImgIndexChecked then
    begin
      Node.StateIndex := ImgIndexUnchecked;
      if SelectedPeersList.Find(Node.Text, i) then
        SelectedPeersList.Delete(i);
    end;
    SelectedPeersList.Sort;
    FormFindPeers.UpdateSelectedPeersPanel;
  end;
end;

function TFormFindPeers.NodeChecked(ANode:TTreeNode): Boolean;
begin
  result := (ANode.StateIndex = ImgIndexChecked) and (ANode.StateIndex > -1);
end;
{%endregion}



{%region form}
{ TFormFindPeers }

function TFormFindPeers.FindPeerInJson(country, peer: string): TJSONObject;
begin
  try
    result := ((receivedjson as TJSONObject)
               .Find(country) as TJSONObject)
              .Find(peer) as TJSONObject;
  except
    on e: Exception do begin
      log(3, 'formfindpeers: error at finding specific peer in received json: ' + e.Message);
      result := TJSONObject.create;
    end;
  end;
end;


procedure TFormFindPeers.ShowNoInternetPanel;
begin
  PanelPleaseWait.Hide;
  PanelNoInternetConnection.Show;
end;


procedure TFormFindPeers.ShowWaitPanel;
begin
  PanelPleaseWait.Show;
  SelectCountry.Enabled := false;
  PanelMain.Enabled := false;
end;


procedure TFormFindPeers.TimerResponceNotReceivedTimer(Sender: TObject);
begin
  TimerResponceNotReceived.Enabled := false;
  inc(currentserverindex);
  if currentserverindex < length(servers) then
  begin
    log(1, 'FormFindPeers: connection with previous server was not established, trying another one...');
    tryretrieveList(currentserverindex);
  end
  else
  begin
    log(2, 'FormFindPeers: no other servers left. You have some connection issues.');
    ShowNoInternetPanel; //no connection to the internet
  end;
end;


procedure TFormFindPeers.HideWaitPanel;
begin
  PanelPleaseWait.Hide;
  SelectCountry.Enabled := true;
  PanelMain.Enabled := true;
end;


procedure TFormFindPeers.ParsePeersByAnyCountry;
var i: integer;
begin
  log(0, 'showing peers by any country...');

  for i := 0 to PeersList.Items.TopLvlCount - 1 do
    PeersList.Items.TopLvlItems[i].Visible := true;
end;


procedure TFormFindPeers.ParsePeersBySelectedCountry;
var i: integer;
begin
  log(0, 'showing peers by certain country (' + SelectCountry.Text + ')...');

  for i := 0 to PeersList.Items.TopLvlCount - 1 do
  begin
    if PeersList.Items.TopLvlItems[i].Text = SelectCountry.Text then
    begin
      PeersList.Items.TopLvlItems[i].Visible := true;
      PeersList.Items.TopLvlItems[i].Expand(false);
    end
    else
      PeersList.Items.TopLvlItems[i].Visible := false;
  end;

end;


procedure TFormFindPeers.PeersListClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Mouse.CursorPos;
  P := PeersList.ScreenToClient(P);
  if (htOnStateIcon in PeersList.GetHitTestInfoAt(P.X, P.Y)) then
    ToggleTreeViewCheckboxes(PeersList.GetNodeAt(P.X, P.Y));

  //чтобы не заебывать кнопку (забить ли на эту микрооптимизацию?)
//  if (SelectedPeersList.Count > 0) and (not ButtonConfirmAdding.Enabled) then
//    ButtonConfirmAdding.Enabled := true
//  else
//  if (SelectedPeersList.Count < 1) and (ButtonConfirmAdding.Enabled) then
//    ButtonConfirmAdding.Enabled := false;

  //пока пусть так
  if (SelectedPeersList.Count > 0) then
    ButtonConfirmAdding.Enabled := true
  else
    ButtonConfirmAdding.Enabled := false;
end;


procedure TFormFindPeers.PeersListSelectionChanged(Sender: TObject);
var
  StatsEnum: TBaseJSONEnumerator;
begin
  PeerParamsList.Clear;

  if (PeersList.SelectionCount = 1) and (PeersList.Selections[0].Parent <> nil) then
  begin
    statsEnum := FindPeerInJson(PeersList.Selections[0].Parent.Text, peerslist.Selections[0].Text).GetEnumerator;
    while statsEnum.MoveNext do
      PeerParamsList.InsertRow(statsenum.Current.Key, statsenum.Current.Value.AsString, true);
  end;
end;


procedure TFormFindPeers.UpdateSelectedPeersPanel;
var
  i: integer;
begin
  SelectedPeers.Items.Clear;
  for i := 0 to SelectedPeersList.Count - 1 do
    (SelectedPeers.Items.Add).Caption := SelectedPeersList.Strings[i];
end;


//procedure TFormFindPeers.PeersListItemChecked(Sender: TObject; CurrItem: TListItem);
//begin
//  with SelectedPeers.Items do
//  begin
//
//    if CurrItem.Checked then
//      AddItem(CurrItem)
//    else
//      (FindCaption(Count - 1, CurrItem.Caption, false, true, false)).Delete;
//  end;
//end;


                                       //чтобы не передавать строку
procedure TFormFindPeers.TryRetrieveList(ServerIndex: byte);
begin
  try
    log(0, 'FormFindPeers: Retrieving the public peers list... using ' + Servers[serverIndex]);

    thread := TThreadGetJson.Create(Servers[serverIndex]);
    thread.OnTerminate := @FinishRetrieve;
    thread.Start;

    TimerResponceNotReceived.Enabled := true;
  finally
  end;
end;


procedure TFormFindPeers.RetrievePublicPeersList;
begin
  CurrentServerIndex := 0;
  TryRetrieveList(CurrentServerIndex);
end;


procedure TFormFindPeers.FinishRetrieve(sender: TObject);
var
  CountryJSONEnum, PeerJSONEnum: TBaseJSONEnumerator;
  BaseNode, SubNode: TTreeNode;
begin
  case thread.Output of
    'error', 'network_issue': exit;  //thread was not successful
  end;


  TimerResponceNotReceived.Enabled := false;

  //парсируем единожды, потом просто скрываем ненужные страны
  //(при переключении комбобокса)
  log(0, 'parsing peers by any country...');
  PeersList.Items.Clear;
  CountryJSONEnum := receivedjson.GetEnumerator;
  while CountryJSONEnum.MoveNext do
    with (CountryJsonEnum.GetCurrent) do
    begin
      SelectCountry.Items.Add(key);

      PeerJSONEnum := Value.GetEnumerator;

      BaseNode := PeersList.Items.Add(nil, key);
      BaseNode.StateIndex := -2;

      while PeerJSONEnum.MoveNext do
        with (PeerJSONEnum.GetCurrent) do
        begin
          SubNode := PeersList.Items.AddChild(BaseNode, key);
          SubNode.StateIndex := ImgIndexUnchecked;
        end;
    end;
  FreeAndNil(CountryJSONEnum);
  FreeAndNil(PeerJSONEnum);
  log(0, 'parsing peers by any country is finished');
  //конец парсинга, все пиры по всем странам отобразятся без лишних методов


  HideWaitPanel;
  SetFocus;
end;


procedure TFormFindPeers.SelectCountryChange(Sender: TObject);
begin
  log(0, 'Changed selected country to: '+SelectCountry.Text);
  if SelectCountry.ItemIndex = 0 then
    ParsePeersByAnyCountry
  else
    ParsePeersBySelectedCountry;
end;


procedure TFormFindPeers.FormCreate(Sender: TObject);
begin
  log(0, 'FormFindPeers window has been instanced');

  FormFindPeers := Self;


  //чтобы панели были централизованы во всех сборках
  //(в винде они почему-то сместились)
  with PanelPleaseWait do begin
    Left := ceil(FormFindPeers.Width / 2) - ceil(width / 2);
    Top := ceil(FormFindPeers.height / 2) - ceil(height / 2);
  end;
  with PanelNoInternetConnection do begin
    Left := ceil(FormFindPeers.Width / 2) - ceil(width / 2);
    Top := ceil(FormFindPeers.height / 2) - ceil(height / 2);
  end;


  //для исправления прозрачности в сборках на базе Qt
  PanelPleaseWait.ControlStyle := PanelPleaseWait.ControlStyle + [csOpaque];
  PanelNoInternetConnection.ControlStyle := PanelNoInternetConnection.ControlStyle + [csOpaque];
  ShowWaitPanel;

  TimerResponceNotReceived.Interval := TimeoutMs;
  RetrievePublicPeersList;

  SelectedPeersList := TStringList.Create;
  SelectedPeersList.SortStyle := sslAuto;
end;

procedure TFormFindPeers.PanelPleaseWaitClick(Sender: TObject);
begin

end;

{%endregion}

initialization

end.

