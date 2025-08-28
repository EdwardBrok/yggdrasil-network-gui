unit UnitListPeers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  MaskEdit,
  Buttons,
  ExtCtrls,
  RegExpr,
  System.UITypes,
  GlobalParameters,
  UnitFindPeers;

type

  { TFormListPeers }

  TFormListPeers = class(TForm)
    AddPeerOKButton: TBitBtn;
    AddPeerEdit: TEdit;
    AddPeerLabel: TLabel;
    ApplyButton: TButton;
    AtCloseQueryDlg: TTaskDialog;
    ClearSelection: TButton;
    FindPeers: TButton;
    RemovePeer: TBitBtn;
    CancelButton: TButton;
    DisabledPeersList: TListBox;
    DisablePeer: TButton;
    EnablePeer: TButton;
    EnabledPeersList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    OKButton: TButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    RemovePeerDlg: TTaskDialog;
    procedure ApplyPeersList;
    procedure ClearSelectionClick(Sender: TObject);
    procedure EnabledPeersListSelectionChange(Sender: TObject; User: boolean);
    procedure FindPeersClick(Sender: TObject);
    procedure RemovePeerClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RemovePeerDlgButtonClicked(Sender: TObject;
      AModalResult: TModalResult; var ACanClose: Boolean);
    procedure UpdateButtonsState;
    procedure AddPeerOKButtonClick(Sender: TObject);
    procedure AddPeerEditChange(Sender: TObject);
    procedure DisablePeerClick(Sender: TObject);
    procedure EnablePeerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AtCloseQueryDlgButtonClicked(Sender: TObject;
      AModalResult: TModalResult; var ACanClose: Boolean);
  private

  public

  end;

var
  FormListPeers: TFormListPeers;
  ListChanged: boolean;
  prevPeersField: TStringList;

implementation

{$R *.lfm}

{ TFormListPeers }


procedure TFormListPeers.ApplyPeersList;
var prevConf, newConf, newPeersField: TStringList;
  i: integer;
begin
  log(1, 'Rewriting Peers section in config...');
  newConf := TStringList.Create;
  newPeersField := TStringList.Create;

  prevConf := ReadYggdrasilConf;

  //создание новых строк для значения поля Peers
  newPeersField.Add('  Peers: [');
  for i := 0 to EnabledPeersList.Count - 1 do
  begin
    newPeersField.Add('    ' + EnabledPeersList.Items[i]);
  end;
  for i := 0 to DisabledPeersList.Count - 1 do
  begin
    newPeersField.Add('    #' + DisabledPeersList.Items[i]);
  end;
  newPeersField.Add('  ]');

  newConf.Text := ReplaceStr(prevConf.Text, prevPeersField.Text, newPeersField.Text);
  WriteYggdrasilConf(Settings.ConfigFilePath, newConf, FormListPeers);

  prevPeersField := newPeersField; //обновление предыдущего списка пиров - нет смысла заново читать файл
  log(0, 'Peers section has been rewritten to the config with amount of ' + inttostr(EnabledPeersList.Count + DisabledPeersList.Count) + ' peers.');
end;

procedure TFormListPeers.ClearSelectionClick(Sender: TObject);
begin
  DisabledPeersList.ClearSelection;
  EnabledPeersList.ClearSelection;
  log(0, 'FormListPeers: selection cleared');
end;


procedure TFormListPeers.EnabledPeersListSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateButtonsState;
end;

procedure TFormListPeers.FindPeersClick(Sender: TObject);
var
  form: TFormFindPeers;
  i: integer;
begin
  form := TFormFindPeers.Create(FormListPeers as TComponent);
  if form.ShowModal = mrOK then
  begin
      if form.SelectedPeersList.Count > 1 then
      begin
        i := 0;
        while i < form.SelectedPeersList.Count do
          begin
            EnabledPeersList.Items.Add(form.SelectedPeersList.Strings[i]);
            i := i + 1;
          end
      end
      else
      if form.SelectedPeersList.Count = 1 then
        EnabledPeersList.Items.Add(form.SelectedPeersList.Strings[0]);
  end;
  FreeAndNil(form);
  log(0, 'FormFindPeers closed and freed');
end;


procedure TFormListPeers.RemovePeerClick(Sender: TObject);
begin
  RemovePeerDlg.Execute;
end;


procedure TFormListPeers.ApplyButtonClick(Sender: TObject);
begin
  applypeerslist;
  listchanged := false;
  updateButtonsState;
end;


procedure TFormListPeers.CancelButtonClick(Sender: TObject);
begin
  if not listchanged then close
  else AtCloseQueryDlg.Execute;
end;


procedure TFormListPeers.OKButtonClick(Sender: TObject);
var MsgWindow : TComponent;
begin
  ApplyPeersList;
  RestartYggdrasilService;
  close;
end;


procedure TFormListPeers.RemovePeerDlgButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
var i: integer;
begin
   if AModalResult = mrYes then
   begin
     log(0, 'FormListPeers: removing peers from the columns');
     if EnabledPeersList.Items.Count <> 0 then
       for i := EnabledPeersList.Items.Count - 1 downto 0 do
         if EnabledPeersList.Selected[i] then
           EnabledPeersList.Items.Delete(i);

     if DisabledPeersList.Items.Count <> 0 then
       for i := DisabledPeersList.Items.Count - 1 downto 0 do
         if DisabledPeersList.Selected[i] then
           DisabledPeersList.Items.Delete(i);

     listChanged := true;
     UpdateButtonsState;
   end;
end;


procedure TFormListPeers.UpdateButtonsState;
begin
  log(0, 'updating buttons state in FormListPeers');
  RemovePeer.Enabled := (EnabledPeersList.SelCount > 0) or (DisabledPeersList.SelCount > 0);
  DisablePeer.Enabled := (EnabledPeersList.SelCount > 0) and (EnabledPeersList.Focused);
  EnablePeer.Enabled := (DisabledPeersList.SelCount > 0) and (DisabledPeersList.Focused);
  ApplyButton.Enabled := listchanged;
  OKButton.Enabled := listchanged;
end;


procedure TFormListPeers.FormCreate(Sender: TObject);
var Config: TStringList;
  regex: TRegExpr;
  tempStrArr: TStringArray;
  s, t: string;
begin
  log(0, 'showing FormListPeers');
  Caption := GlobalParameters.AppDisplayname + ' - Изменение пиров для подключения';
  ListChanged := false;
  Config := TStringList.Create;
  prevPeersField := TStringList.Create;

  regex := TRegExpr.Create;
  regex.Expression := '  Peers\:[\s]*\[[\n]*([a-zA-Z0-9\s\,\:\.\/\-\n\#]*[a-zA-Z0-9\s\,\:\.\/\-\#]?)[\n]*\]';
  regex.ModifierG := true;
  regex.ModifierM := true;

  Config := ReadYggdrasilConf;
  try
    if regex.Exec(Config.Text) and (regex.Match[1] <> '') then
    begin
      //showmessage(regex.Match[0]);
      prevPeersField.AddText(regex.Match[0]);
      tempStrArr := splitstring(trim(regex.Match[1]), #$0A);
      for s in tempStrArr do
      begin
        t := trim(s);
        if not StartsStr('#', t) then
          EnabledPeersList.Items.Add(t)
        else
          DisabledPeersList.Items.Add(copy(t, 2, 64)); // := regex.Match[1];
      end;
    end;
    UpdateButtonsState;
  except
    on E: Exception do
    showMessage('Ошибка чтения пиров для подключения: ' + e.Message)
  end;
  Regex.Free;
  Config.Free;
end;



procedure TFormListPeers.AtCloseQueryDlgButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
begin
   if AModalResult = mrNo then Close;
   if AModalResult = mrYes then
   begin
     applypeerslist;
     RestartYggdrasilService;
     close;
   end;
end;


procedure TFormListPeers.AddPeerOKButtonClick(Sender: TObject);
var regex: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    regex.expression := '[a-zA-Z]+\:\/\/([a-zA-Z0-9\.\_\-]+\.[a-zA-Z]+|[0-9]{0,3}\.[0-9]{0,3}\.[0-9]{0,3}\.[0-9]{0,3}|)\:[0-9]{3,5}';
    //пока что без ipv6 - но есть домены и кривой ipv4
    if regex.Exec(AddPeerEdit.Text) then
    begin
      listchanged := true;
      updateButtonsState;
      EnabledPeersList.Items.Add(AddPeerEdit.Text);
      AddPeerEdit.Clear;
    end
    else ShowMessage('Некорректный адрес пира.'); //execute();
  finally
  end;
end;


procedure TFormListPeers.AddPeerEditChange(Sender: TObject);
begin
  AddPeerOKButton.Enabled := trim(AddPeerEdit.Text) <> '';
end;


procedure TFormListPeers.DisablePeerClick(Sender: TObject);
var i: integer;
begin
  if EnabledPeersList.Items.Count <> 0 then
  for i := EnabledPeersList.Items.Count - 1 downto 0 do
  if EnabledPeersList.Selected[i] then
  begin
    DisabledPeersList.Items.Add(EnabledPeersList.Items[i]);
    EnabledPeersList.Items.Delete(i);
  end;
  listChanged := true;
  UpdateButtonsState;
end;


procedure TFormListPeers.EnablePeerClick(Sender: TObject);
var i: integer;
begin
  if DisabledPeersList.Items.Count <> 0 then
  for i := DisabledPeersList.Items.Count - 1 downto 0 do
  if DisabledPeersList.Selected[i] then
  begin
    EnabledPeersList.Items.Add(DisabledPeersList.Items[i]);
    DisabledPeersList.Items.Delete(i);
  end;
  listChanged := true;
  UpdateButtonsState;
end;


end.

