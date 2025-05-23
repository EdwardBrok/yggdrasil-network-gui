unit unitListListen;

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
  CheckLst,
  MaskEdit,
  Buttons,
  ExtCtrls,
  EditBtn,
  Grids,
  ValEdit,
  LvlGraphCtrl,
  RegExpr,
  System.UITypes,
  GlobalParameters;

type

  { TFormListListen }

  TFormListListen = class(TForm)
    AddPeerEdit: TEdit;
    AddPeerLabel: TLabel;
    AddPeerOKButton: TBitBtn;
    ApplyButton: TButton;
    AtCloseQueryDlg: TTaskDialog;
    CancelButton: TButton;
    ClearSelection: TButton;
    DisabledPeersList: TListBox;
    DisablePeer: TButton;
    EnabledPeersList: TListBox;
    EnablePeer: TButton;
    Label1: TLabel;
    Label2: TLabel;
    OKButton: TButton;
    Panel1: TPanel;
    RemovePeer: TBitBtn;
    RemovePeerDlg: TTaskDialog;
    Splitter1: TSplitter;
    procedure AddPeerLabelClick(Sender: TObject);
    procedure ApplyPeersList;
    procedure ClearSelectionClick(Sender: TObject);
    procedure EnabledPeersListSelectionChange(Sender: TObject; User: boolean);
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
  FormListListen: TFormListListen;
  ListChanged: boolean;
  prevPeersField: TStringList;

implementation

{$R *.lfm}

{ TFormListListen }


procedure TFormListListen.ApplyPeersList;
var prevConf, newConf, newPeersField: TStringList;
  i: integer;
begin
  newConf := TStringList.Create;
  newPeersField := TStringList.Create;

  prevConf := ReadYggdrasilConf(Settings.ConfigFilePath);

  //создание новых строк для значения поля Peers
  newPeersField.Add('  Listen: [');
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
  WriteYggdrasilConf(Settings.ConfigFilePath, newConf, FormListListen);

  prevPeersField := newPeersField; //обновление предыдущего списка пиров - нет смысла заново читать файл
end;

procedure TFormListListen.AddPeerLabelClick(Sender: TObject);
begin

end;

procedure TFormListListen.ClearSelectionClick(Sender: TObject);
begin
  DisabledPeersList.ClearSelection;
  EnabledPeersList.ClearSelection;
end;


procedure TFormListListen.EnabledPeersListSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateButtonsState;
end;


procedure TFormListListen.RemovePeerClick(Sender: TObject);
begin
  RemovePeerDlg.Execute;
end;


procedure TFormListListen.ApplyButtonClick(Sender: TObject);
begin
  applypeerslist;
  listchanged := false;
  updateButtonsState;
end;


procedure TFormListListen.CancelButtonClick(Sender: TObject);
begin
  if not listchanged then close
  else AtCloseQueryDlg.Execute;
end;


procedure TFormListListen.OKButtonClick(Sender: TObject);
begin
  ApplyPeersList;
  close;
end;


procedure TFormListListen.RemovePeerDlgButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
var i: integer;
begin
   if AModalResult = mrYes then
   begin
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


procedure TFormListListen.UpdateButtonsState;
begin
  RemovePeer.Enabled := (EnabledPeersList.SelCount > 0) or (DisabledPeersList.SelCount > 0);
  DisablePeer.Enabled := (EnabledPeersList.SelCount > 0) and (EnabledPeersList.Focused);
  EnablePeer.Enabled := (DisabledPeersList.SelCount > 0) and (DisabledPeersList.Focused);
  ApplyButton.Enabled := listchanged;
  OKButton.Enabled := listchanged;
end;


procedure TFormListListen.FormCreate(Sender: TObject);
var Config: TStringList;
  regex: TRegExpr;
  tempStrArr: TStringArray;
  s, t: string;
begin
  Caption := GlobalParameters.AppDisplayname + ' - Изменение прослушиваемых пиров';
  ListChanged := false;
  Config := TStringList.Create;
  prevPeersField := TStringList.Create;

  regex := TRegExpr.Create;
  regex.Expression := '  Listen\:[\s]*\[[\n]*([a-zA-Z0-9\s\,\:\.\/\-\n\#]*[a-zA-Z0-9\s\,\:\.\/\-\#]?)[\n]*\]';
  regex.ModifierG := true;
  regex.ModifierM := true;

  Config := ReadYggdrasilConf(Settings.ConfigFilePath);
  try
    if regex.Exec(Config.Text) then
    begin
      prevPeersField.AddText(regex.Match[0]);
      if regex.Match[1] <> '' then
      begin
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
    end;
    UpdateButtonsState;
  except
    on E: Exception do
    showMessage('Ошибка чтения прослушиваемых пиров: ' + e.Message)
  end;
  Regex.Free;
  Config.Free;
end;



procedure TFormListListen.AtCloseQueryDlgButtonClicked(Sender: TObject;
  AModalResult: TModalResult; var ACanClose: Boolean);
begin
   if AModalResult = mrNo then Close;
   if AModalResult = mrYes then
   begin
     applypeerslist;
     close;
   end;
end;


procedure TFormListListen.AddPeerOKButtonClick(Sender: TObject);
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


procedure TFormListListen.AddPeerEditChange(Sender: TObject);
begin
  AddPeerOKButton.Enabled := trim(AddPeerEdit.Text) <> '';
end;


procedure TFormListListen.DisablePeerClick(Sender: TObject);
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


procedure TFormListListen.EnablePeerClick(Sender: TObject);
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

