unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, StdCtrls, fphttpclient, uItemFrameBase, uMoteMessage, uEventDto,
  uItemDto, uSettingItemUriDto, uSettingNewItemOptionsDto;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAddItem: TAction;
    acShowWeek: TAction;
    acTimerActivate: TAction;
    acAddRemoteItem: TAction;
    ActionList: TActionList;
    Button1: TButton;
    imFundo: TImage;
    ImageList: TImageList;
    ListBoxMessages: TListBox;
    MainMenu: TMainMenu;
    miAtivartimer: TMenuItem;
    miItemPopupDeleteItem: TMenuItem;
    miItemPopupItemText: TMenuItem;
    miUsertCodesEditor: TMenuItem;
    miTools: TMenuItem;
    miAddItem: TMenuItem;
    miItem: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    PanelActions: TPanel;
    pmRemoteItems: TPopupMenu;
    PopupMenuItem: TPopupMenu;
    ScrollBoxMain: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    btnRemoteNewItem: TSpeedButton;
    Splitter1: TSplitter;
    Timer: TTimer;

    procedure AddItemFromJson(const psJSONString: string);
    procedure acAddItemExecute(Sender: TObject);
    procedure acAddRemoteItemExecute(Sender: TObject);
    procedure acTimerActivateExecute(Sender: TObject);
    procedure acShowWeekExecute(Sender: TObject);
    procedure btnRemoteNewItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ControlBarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miItemPopupDeleteItemClick(Sender: TObject);
    procedure miItemPopupItemTextClick(Sender: TObject);
    procedure miUsertCodesEditorClick(Sender: TObject);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure MessagePublish(Sender: Tobject; const poMessage: TMoteMessage);
    function ItemFrameBaseEvent(Sender: TObject; const psEvent: string; const poParam: TObject; var poResponse: TObject): boolean;
    procedure TimerTimer(Sender: TObject);
    function FindItemGui(const psJSONItem: string): TItemFrameBase;
  private
    FnContador: integer;
    FoMessageClient: TMoteMessageClient;
    FoLastFrameInserted: TItemFrameBase;
    FoLastFrameEventHandle: TItemFrameBase;
    FslRemoteItems: TStringList;

    procedure GetRemoteNewItems(poItem: TSettingItemUriDto);
    function TestItemCanBeVisible(const poItem: TItemDto): boolean;

    procedure miRemoteItemsClick(Sender: TObject);
    procedure AddItemToGui(const psJSONItem: string);
    procedure UpdateItemToGui(const psJSONItem: string);
    procedure DeleteItemToGui(const psJSONItem: string);
    procedure ApplySetting(const psJSONItem: string);
    function GetMessageClient: TMoteMessageClient;
    procedure SetMessageClient(AValue: TMoteMessageClient);
  public
    property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  uUserCodesEditor, uMoteUtils, uWeek, uTextEditor;

{ TfrmMain }

procedure TfrmMain.miUsertCodesEditorClick(Sender: TObject);
begin
  Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  frmUserCodesEditor.Show;
end;

procedure TfrmMain.MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
var
  e: TEventDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  try
    ListBoxMessages.Items.Add('received < [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);

    if e.event = 'item_restore' then
    begin
      AddItemToGui(e.payload);
    end
    else
    if (e.event = 'item_update') or (e.event = 'item_refresh') then
    begin
      UpdateItemToGui(e.payload);
    end
    else
    if e.event = 'item_delete' then
    begin
      DeleteItemToGui(e.payload);
    end
    else
    if e.event = 'setting_restore' then
    begin
      ApplySetting(e.payload);
    end;
  finally
    e.Free;
  end;
end;

procedure TfrmMain.MessagePublish(Sender: Tobject; const poMessage: TMoteMessage
  );
begin
  //only to gui log
  ListBoxMessages.Items.Add(' sent > [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);
end;

function TfrmMain.ItemFrameBaseEvent(Sender: TObject; const psEvent: string;
  const poParam: TObject; var poResponse: TObject): boolean;
var
  e: TEventDto;
begin
  FoLastFrameEventHandle := Sender as TItemFrameBase;
  e := TEventDto.CreateEventObject(psEvent, FoLastFrameEventHandle.Item);
  try
    if psEvent = 'item_memo_edition' then
    begin
      frmTextEditorDialog.mmEditor.Lines.Text:= FoLastFrameEventHandle.Item.Description;
      frmTextEditorDialog.FoLastFrameEventHandle := FoLastFrameEventHandle;
      frmTextEditorDialog.Visible := true;
      frmTextEditorDialog.BringToFront;
      exit(true);
    end
    else
    if psEvent = 'item_option' then
    begin
      PopupMenuItem.PopupComponent:=(poParam as TComponent);
      PopupMenuItem.PopUp;
      exit(true);
    end
    else
    if psEvent = 'item_interrupt' then
    begin
      if Timer.Enabled then
      begin
        Timer.Enabled := false;
        miAtivartimer.Checked:=false;
        ShowMessage('Devido a uma restrição, não possível usar interrupção de item em conjunto com timer. O timer foi desabiltiado');
      end;

      FoLastFrameInserted := nil;
      acAddItemExecute(Sender);

      poResponse := FoLastFrameInserted;
      result := (poResponse <>  nil);
      if result then
      begin
        FoLastFrameInserted.ParentItem := FoLastFrameEventHandle;
        FoLastFrameInserted.Update(false);
        FoMessageClient.Publish('item', e.ToString);

        FoLastFrameInserted.Top := FoLastFrameEventHandle.Top + (FoLastFrameEventHandle.Height div 2);

        e.event:='item_init_work';
        e.payload:=FoLastFrameInserted.ToString;
        FoMessageClient.Publish('item', e.ToString);
      end;
      exit;
    end;
    FoMessageClient.Publish('item', e.ToString);

    //force refrash of time
    if (psEvent = 'item_init_work') and (not Timer.enabled) then
      acTimerActivateExecute(sender);
  finally
    e.Free;
  end;
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  i: integer;
  oControl: TControl;
begin
  for i := 0 to ScrollBoxMain.ComponentCount-1 do
  begin
    if ScrollBoxMain.Components[i] is TItemFrameBase then
    begin
      (ScrollBoxMain.Components[i] as TItemFrameBase).RefreshTime;
    end;
  end;
end;

function TfrmMain.GetMessageClient: TMoteMessageClient;
begin
  Result := FoMessageClient;
end;

procedure TfrmMain.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  //event
  if Assigned(FoMessageClient) then
  begin
    FoMessageClient.OnMessage := @MessageEvent;
    FoMessageClient.OnPublish := @MessagePublish;
  end;
end;



procedure TfrmMain.AddItemToGui(const psJSONItem: string);
  var
  oFrame: TItemFrameBase;
begin
  FoLastFrameInserted := FindItemGui(psJSONItem);
  if FoLastFrameInserted <> nil then
  begin
    FoLastFrameInserted.UpdateFromJSON(psJSONItem);
    //temp. action: a hidded item can be showed after init work
    FoLastFrameInserted.Visible := TestItemCanBeVisible(FoLastFrameInserted.Item);
    exit;
  end;

  FnContador := FnContador + 1;

  FoLastFrameInserted := TItemFrameBase.Create(ScrollBoxMain);
  FoLastFrameInserted.Name := 'Item_' + IntToStr(FnContador);
  FoLastFrameInserted.Parent := ScrollBoxMain;
  FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

  FoLastFrameInserted.UpdateFromJSON(psJSONItem);
  //temp. action: hide itens for other dates
  //temp. action: a hidded item can be showed after init work
  FoLastFrameInserted.Visible := TestItemCanBeVisible(FoLastFrameInserted.Item);
end;

function TfrmMain.FindItemGui(const psJSONItem: string): TItemFrameBase;
var
  i: integer;
  oItem: TItemDto;
begin
  result := nil;
  oItem := TItemDto.CreateFromJSON(psJSONItem);
  try
  for i := 0 to ScrollBoxMain.ComponentCount-1 do
  begin
    if ScrollBoxMain.Components[i] is TItemFrameBase then
    begin
      if (ScrollBoxMain.Components[i] as TItemFrameBase).Item.Id =  oItem.Id then
      begin
        result := ScrollBoxMain.Components[i] as TItemFrameBase;
        exit;
      end;
    end;
  end;
  finally
    oItem.Free;
  end;
end;

function TfrmMain.TestItemCanBeVisible(const poItem: TItemDto): boolean;
var
  s: string;
begin
  s := poItem.TimeIntervals;
  if (s <> '') and (Pos(FormatDateTime('dd/mm/yyyy', Date), s) < 1) then
    exit(false)
  else
    exit(true);
end;


procedure TfrmMain.UpdateItemToGui(const psJSONItem: string);
var
  oFrame: TItemFrameBase;
begin
  oFrame := FindItemGui(psJSONItem);
  if oFrame <> nil then
  begin
    oFrame.UpdateFromJSON(psJSONItem);
    oFrame.Visible := TestItemCanBeVisible(oFrame.Item);
  end;
end;

procedure TfrmMain.DeleteItemToGui(const psJSONItem: string);
var
  oControl: TControl;
  oFrame: TItemFrameBase;
begin
  oFrame := FindItemGui(psJSONItem);
  if oFrame = nil then
    exit;
  oControl := oFrame As TControl;
  ScrollBoxMain.RemoveControl(oControl);
  oControl.Parent := nil;
  (oControl As TItemFrameBase).Free;
end;

procedure TfrmMain.GetRemoteNewItems(poItem: TSettingItemUriDto);
var
  s: string;
  oResult: TSettingNewItemOptionsDto;
  oItem: TSettingNewItemOptionsItemDto;
  oCi: TcollectionItem;
  i: integer;
  mi: TMenuItem;
begin
  FslRemoteItems.Clear;
  s := TFPHttpClient.SimpleGet(poItem.Uri);
  oResult := TSettingNewItemOptionsDto.Create(s);
  try
    ListBoxMessages.Items.Add(Format('%d items from remote new items.', [oResult.items.Count]));
    for i := 0 to oResult.items.Count-1 do
    begin
      oCi := oResult.items.Items[i];
      oItem := TSettingNewItemOptionsItemDto(oCi);

      mi := TMenuItem.Create(pmRemoteItems);
      mi.Caption := '&' + IntTostr(i+1) + ' - ' + oItem.title;
      mi.Tag := i;
      mi.OnClick := @miRemoteItemsClick;
      FslRemoteItems.Add(oItem.ToString);
      pmRemoteItems.Items.Add(mi);
    end;
  finally
    oResult.Free;
  end;

end;

procedure TfrmMain.miRemoteItemsClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  i := (Sender as TMenuItem).Tag;
  s := FslRemoteItems[i];
  AddItemFromJson(s);
end;


procedure TfrmMain.ApplySetting(const psJSONItem: string);
var
  oItem: TSettingItemUriDto;
begin
  oItem := TSettingItemUriDto.Create(psJSONItem);
  try
    if oItem.kind = 'new-item-remote-templates' then
    begin
      GetRemoteNewItems(oItem);
    end;
  finally
    oItem.Free;
  end;
end;


procedure TfrmMain.AddItemFromJson(const psJSONString: string);
var
  s: string;
  oItem: TSettingNewItemOptionsItemDto;
  e: TEventDto;
begin
  oItem := TSettingNewItemOptionsItemDto.CreateFromJson(psJSONString);
  try
    FnContador := FnContador + 1;

    FoLastFrameInserted := TItemFrameBase.Create(ScrollBoxMain);
    FoLastFrameInserted.Name := 'Item_' + IntToStr(FnContador);
    FoLastFrameInserted.Parent := ScrollBoxMain;

    FoLastFrameInserted.Id := GetNewId;
    FoLastFrameInserted.CreatedBy:=GetCurrentUserName;
    FoLastFrameInserted.UserName:=GetCurrentUserName;
    FoLastFrameInserted.Item.CreationDateTime:=now;
    FoLastFrameInserted.Item.LastUpdateDateTime:=FoLastFrameInserted.Item.CreationDateTime;

    FoLastFrameInserted.Title := oItem.title;
    FoLastFrameInserted.Description:= oItem.description;
    FoLastFrameInserted.ExternalToolItem := oItem.externalToolItem;

    FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

    if Assigned(FoMessageClient) then
    begin
      e := TEventDto.CreateEventObject('item_create', FoLastFrameInserted.Item);
      try
        FoMessageClient.Publish('item', e.ToString);
      finally
        e.Free;
      end;
    end;
  finally
    oItem.Free;
  end;
end;

procedure TfrmMain.acAddItemExecute(Sender: TObject);
var
  s: string;
  e: TEventDto;
begin
  s := '';
  if InputQuery('Novo item','Informe o Título', s) then
  begin
    FnContador := FnContador + 1;

    FoLastFrameInserted := TItemFrameBase.Create(ScrollBoxMain);
    FoLastFrameInserted.Name := 'Item_' + IntToStr(FnContador);
    FoLastFrameInserted.Parent := ScrollBoxMain;

    FoLastFrameInserted.Id := GetNewId;
    FoLastFrameInserted.CreatedBy:=GetCurrentUserName;
    FoLastFrameInserted.UserName:=GetCurrentUserName;
    FoLastFrameInserted.Item.CreationDateTime:=now;
    FoLastFrameInserted.Item.LastUpdateDateTime:=FoLastFrameInserted.Item.CreationDateTime;

    FoLastFrameInserted.Title:=s;
    FoLastFrameInserted.Description:='';
    FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

    if Assigned(FoMessageClient) then
    begin
      e := TEventDto.CreateEventObject('item_create', FoLastFrameInserted.Item);
      try
        FoMessageClient.Publish('item', e.ToString);
      finally
        e.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.acAddRemoteItemExecute(Sender: TObject);
begin
  pmRemoteItems.PopupComponent:=(Sender as TComponent);
  pmRemoteItems.PopUp;
end;

procedure TfrmMain.acTimerActivateExecute(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  miAtivartimer.Checked:=Timer.Enabled;
  if Timer.Enabled then
    Timer.OnTimer(self);
end;

procedure TfrmMain.acShowWeekExecute(Sender: TObject);
begin
  frmWeek.Show;
end;

procedure TfrmMain.btnRemoteNewItemClick(Sender: TObject);
begin

end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ListBoxMessages.Clear;
end;

procedure TfrmMain.ControlBarClick(Sender: TObject);
begin
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FslRemoteItems := TStringList.Create;
  //force refrash of time
  if not Timer.enabled then
    acTimerActivateExecute(sender);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FslRemoteItems.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Caption := 'show';
end;

procedure TfrmMain.miItemPopupDeleteItemClick(Sender: TObject);
var
  e: TEventDto;
begin
  if FoLastFrameEventHandle = nil then
    exit;

  if Assigned(FoMessageClient) then
  begin
    e := TEventDto.CreateEventObject('item_delete', FoLastFrameEventHandle.Item);
    try
      FoMessageClient.Publish('item', e.ToString);
      DeleteItemToGui(FoLastFrameEventHandle.ToString);
    finally
      e.Free;
    end;
  end;

end;

procedure TfrmMain.miItemPopupItemTextClick(Sender: TObject);
var
  s: string;
begin
  if FoLastFrameEventHandle = nil then
    exit;

  s := FoLastFrameEventHandle.ToString;
  InputQuery('Texto do item','Informe o Título', s);
end;

end.

