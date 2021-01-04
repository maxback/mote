unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, StdCtrls, fphttpclient, uItemFrameBase, uTodaySumaryFrame,
  uMoteMessage, uEventDto, uItemDto, uSettingItemUriDto, uSettingItemBase,
  uSettingNewItemOptionsDto, uDateIntervalParamDto;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAddItem: TAction;
    acShowWeek: TAction;
    acTimerActivate: TAction;
    acAddRemoteItem: TAction;
    acFilterSumary: TAction;
    acMoveTodayToPrev: TAction;
    acMoveTodayToForward: TAction;
    ActionList: TActionList;
    btnRemoteNewItem1: TSpeedButton;
    Button1: TButton;
    imFundo: TImage;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    miLogMessages: TMenuItem;
    mmMessages: TMemo;
    N1: TMenuItem;
    miAtivartimer: TMenuItem;
    miItemPopupDeleteItem: TMenuItem;
    miItemPopupItemText: TMenuItem;
    miUsertCodesEditor: TMenuItem;
    miTools: TMenuItem;
    miAddItem: TMenuItem;
    miItem: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    pnLog: TPanel;
    PanelActions: TPanel;
    pmRemoteItems: TPopupMenu;
    PopupMenuItem: TPopupMenu;
    ScrollBoxMain: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    btnRemoteNewItem: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Splitter: TSplitter;
    Timer: TTimer;
    TimerInitalizationAnimate: TTimer;
    FrameSumary: TTodaySumaryFrame;

    procedure acFilterSumaryExecute(Sender: TObject);
    procedure acMoveTodayToForwardExecute(Sender: TObject);
    procedure acMoveTodayToPrevExecute(Sender: TObject);
    procedure miItemInterruptionClick(Sender: TObject);
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
    procedure miLogMessagesClick(Sender: TObject);
    procedure miUsertCodesEditorClick(Sender: TObject);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure MessagePublish(Sender: Tobject; const poMessage: TMoteMessage);
    function ItemFrameBaseEvent(Sender: TObject; const psEvent: string; const poParam: TObject; var poResponse: TObject): boolean;
    procedure TimerInitalizationAnimateTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    function FindItemGui(const psJSONItem: string): TItemFrameBase;
  private
    FnContador: integer;
    FoMessageClient: TMoteMessageClient;
    FoLastFrameInserted: TItemFrameBase;
    FoLastFrameEventHandle: TItemFrameBase;
    FslRemoteItems: TStringList;
    FslItemInterruptionsItems: TStringList;
    FdtToday: TDateTime;
    FbTimerEnabledOld: boolean;

    procedure SaveTimerEnabled;
    procedure RestoreTimerEnabled;

    procedure GetRemoteNewItems(poItem: TSettingItemUriDto);
    procedure GetRemoteItemInterruptions(poItem: TSettingItemBaseDto);
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
    if miLogMessages.checked then
      mmMessages.Lines.Add('received < [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);

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
    FrameSumary.Update;
  finally
    e.Free;
  end;
end;

procedure TfrmMain.MessagePublish(Sender: Tobject; const poMessage: TMoteMessage
  );
begin
  //only to gui log
  if miLogMessages.checked then
    mmMessages.Lines.Add(' sent > [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);
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
    if (psEvent = 'item_interrupt') or (psEvent = 'item_new_from_timeinterval') then
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
        if psEvent = 'item_interrupt' then
        begin
          e.event:='item_init_work';
          e.payload:=FoLastFrameInserted.ToString;
          FoMessageClient.Publish('item', e.ToString);
        end
        else
        if psEvent = 'item_new_from_timeinterval' then
        begin
          FoLastFrameInserted.TimeIntervals.Text := (poParam as TItemFrameBaseEventParamObject).StringValue;
          FoLastFrameInserted.Description:= 'Item extrat from Time intervals of ' + FoLastFrameEventHandle.Title + #13#10+
          FoLastFrameEventHandle.Description + #13#10 +
          FoLastFrameEventHandle.Id;
          FoLastFrameEventHandle.Update(true);
        end;
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

procedure TfrmMain.TimerInitalizationAnimateTimer(Sender: TObject);
begin
  PanelActions.Visible := true;
  miLogMessages.Checked := false;

  Timer.OnTimer(self);
  miAtivartimer.Checked := false;
  Timer.Enabled := false;

  TimerInitalizationAnimate.Enabled:=false;
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ScrollBoxMain.ComponentCount-1 do
  begin
    if ScrollBoxMain.Components[i] is TItemFrameBase then
    begin
      (ScrollBoxMain.Components[i] as TItemFrameBase).RefreshTime;
    end;
  end;
  FdtToday := now;
  FrameSumary.TodayValue:=FormatdateTime('dd/mm/yyyy', FdtToday);
  FrameSumary.Update;
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

procedure TfrmMain.SaveTimerEnabled;
begin
  FbTimerEnabledOld := Timer.Enabled;
  Timer.Enabled := false;
end;

procedure TfrmMain.RestoreTimerEnabled;
begin
  Timer.Enabled := FbTimerEnabledOld;
end;

function TfrmMain.TestItemCanBeVisible(const poItem: TItemDto): boolean;
var
  s: string;
begin
  s := poItem.TimeIntervals + '|' + FormatDateTime('dd/mm/yyyy', poItem.CreationDateTime);
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
    if miLogMessages.checked then
      mmMessages.Lines.Add(Format('%d items from remote new items.', [oResult.items.Count]));
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

procedure TfrmMain.miItemInterruptionClick(Sender: TObject);
var
  s: string;
  i: integer;
  oItem: TSettingItemBaseDto;
  sStart, sEnd: string;
  sl: TStringList;
begin
  if not Assigned(FoLastFrameEventHandle) then
    exit;
  i := (Sender as TMenuItem).Tag;
  s := FslItemInterruptionsItems[i];
  oItem := TSettingItemBaseDto.CreateFromJSon(s);
  sl := TStringList.Create;
  try
    sStart := Copy(oItem.value, 1, 5);
    sEnd := Copy(oItem.value, 7, 5);

    sl.Text := FoLastFrameEventHandle.Item.TimeIntervals;
    i := sl.Count-1;
    s := sl[i];
    if length(s) > 18 then
      raise Exception.Create('You need to do this only on openned item!');

    if sEnd <> '' then
      sl.Add(Copy(s, 1, 10) + ' ' + sEnd);

    sl[i] := s + ' - ' + sStart;

    FoLastFrameEventHandle.Item.TimeIntervals := sl.Text;
    FoLastFrameEventHandle.Update(true, true);

  finally
    sl.Free;
    oItem.Free;
  end;

end;

procedure TfrmMain.acFilterSumaryExecute(Sender: TObject);
var
  s: string;
begin
  SaveTimerEnabled;
  try
    s := FrameSumary.Filter;
    if InputQuery('Filtro (Adicione ! antes para negar)','Informe o filtro dos intes no sumário', s) then
    begin
      FrameSumary.Filter:=s;
      FrameSumary.Update;
    end;
  finally
    RestoreTimerEnabled;
  end;
end;

procedure TfrmMain.acMoveTodayToForwardExecute(Sender: TObject);
begin
  FdtToday := FdtToday + 1.0;
  FrameSumary.TodayValue:=FormatdateTime('dd/mm/yyyy', FdtToday);
  FrameSumary.Update;
end;

procedure TfrmMain.acMoveTodayToPrevExecute(Sender: TObject);
var
  oInterval: TDateIntervalDto;
begin
  FdtToday := FdtToday - 1.0;
  FrameSumary.TodayValue:=FormatdateTime('dd/mm/yyyy', FdtToday);
  FrameSumary.Update;

  if Assigned(FoMessageClient) then
  begin
    oInterval := TDateIntervalDto.CreateInterval(FdtToday, FdtToday);
    try
      FoMessageClient.Publish('date_interval_select', oInterval.ToString);
    finally
      oInterval.Free;
    end;
  end;
end;

procedure TfrmMain.GetRemoteItemInterruptions(poItem: TSettingItemBaseDto);
var
  mi: TMenuItem;
begin
  if not poItem.Active then
    exit;
  mi := TMenuItem.Create(PopupMenuItem);
  mi.Tag := FslItemInterruptionsItems.Count;
  mi.Caption := '&' + IntTostr(mi.Tag + 1) + ' - ' + poItem.name;
  mi.OnClick := @miItemInterruptionClick;
  FslItemInterruptionsItems.Add(poItem.ToString);
  PopupMenuItem.Items.Add(mi);
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
  oItem: TSettingItemBaseDto;
  oItemUri: TSettingItemUriDto;
begin
  oItem := TSettingItemUriDto.Create(psJSONItem);
  try
    if oItem.kind = 'new-item-remote-templates' then
    begin
      oItemUri := TSettingItemUriDto.Create(psJSONItem);
      GetRemoteNewItems(oItemUri);
    end
    else
    if oItem.kind = 'item-interruption' then
    begin
      GetRemoteItemInterruptions(oItem);
    end;

  finally
    oItemUri.Free;
    oItem.Free;
  end;
end;


procedure TfrmMain.AddItemFromJson(const psJSONString: string);
var

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

    FoLastFrameInserted.TimeIntervals.Text := oItem.timeIntervals;
    FoLastFrameInserted.Item.TimeIntervals := oItem.timeIntervals;

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
  SaveTimerEnabled;
  try
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
  finally
    RestoreTimerEnabled;
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
  mmMessages.Clear;
end;

procedure TfrmMain.ControlBarClick(Sender: TObject);
begin
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FrameSumary.ContainerOfItemsFrames := ScrollBoxMain;
  FrameSumary.CanControlContainerOfItemsFramesVisiblity := true;
  FdtToday := now;
  FrameSumary.TodayValue:=FormatDateTime('dd/mm/yyyy', FdtToday);;
  FslRemoteItems := TStringList.Create;
  FslItemInterruptionsItems := TStringList.Create;
  //force refrash of time
  if not Timer.enabled then
    acTimerActivateExecute(sender);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FslItemInterruptionsItems.Free;
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

procedure TfrmMain.miLogMessagesClick(Sender: TObject);
begin
  miLogMessages.Checked := not miLogMessages.Checked;
  pnLog.Visible := miLogMessages.Checked;
  Splitter.Visible := miLogMessages.Checked;
  Splitter.Top := Height - pnLog.Height - 10;
end;

end.

