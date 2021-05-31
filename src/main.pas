unit main;

{$mode objfpc}{$H+}

interface

uses
  uprcLog,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, StdCtrls, fphttpclient, uItemFrameBase, uTodaySumaryFrame,
  uMoteMessage, uEventDto, uItemDto, uSettingItemUriDto, uSettingItemBase,
  uSettingNewItemOptionsDto, uDateIntervalParamDto, umotelog;

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
    procedure pmRemoteItemsPopup(Sender: TObject);
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
    FsRemoteNewItemsUri: string;
    FdtRemoteNewItemsLastLoadTime: TDateTime;

    procedure AdjustFrameWith(const poItemFrameBase: TItemFrameBase);
    procedure SaveTimerEnabled;
    procedure RestoreTimerEnabled;

    procedure GetRemoteNewItems(poItem: TSettingItemUriDto);
    procedure GetRemoteItemInterruptions(poItem: TSettingItemBaseDto);
    function TestItemCanBeVisible(const poItem: TItemDto): boolean;

    function GetRemoteNewItemsFromUri(const psURI: string): boolean;

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
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miUsertCodesEditorClick', EmptyStr, True);
  try
    Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
    frmUserCodesEditor.Show;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
var
  oContextoLog: IMoteContextoRegistroLog;
  e: TEventDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.MessageEvent', EmptyStr, True);
  try
    motelogfmt('payload: "%s"', [poMessage.Payload]);
    e := TEventDto.CreateFromJSON(poMessage.Payload);
    motelogfmt('****** Event: %s', [e.event]);
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.MessagePublish(Sender: Tobject; const poMessage: TMoteMessage
  );
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.MessagePublish', EmptyStr, True);
  try
    //only to gui log
    if miLogMessages.checked then
      mmMessages.Lines.Add(' sent > [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);
    motelog(' sent > [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

//**************** Método tem mais de 60 linhas *********************
function TfrmMain.ItemFrameBaseEvent(Sender: TObject; const psEvent: string;
  const poParam: TObject; var poResponse: TObject): boolean;
var
  oContextoLog: IMoteContextoRegistroLog;
  e: TEventDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.ItemFrameBaseEvent', EmptyStr, True);
  try
    FoLastFrameEventHandle := Sender as TItemFrameBase;
    e := TEventDto.CreateEventObject(psEvent, FoLastFrameEventHandle.Item);
    motelogfmt('****** Event: %s', [e.event]);
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
          AdjustFrameWith(FoLastFrameInserted);
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
        begin
          oContextoLog.GerarLogRetornoMetodo;
          exit;
        end;
      end;
      FoMessageClient.Publish('item', e.ToString);
  
      //force refrash of time
      if (psEvent = 'item_init_work') and (not Timer.enabled) then
        acTimerActivateExecute(sender);
    finally
      e.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo(Format('%d (bool)', [integer(result)]));
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.pmRemoteItemsPopup(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.pmRemoteItemsPopup', EmptyStr, True);
  try
    if (now - FdtRemoteNewItemsLastLoadTime) > ((1 / 24.0 / 60.0) * 5) then
    begin
      if GetRemoteNewItemsFromUri(FsRemoteNewItemsUri) then
        FdtRemoteNewItemsLastLoadTime := now;;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.TimerInitalizationAnimateTimer(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.TimerInitalizationAnimateTimer', EmptyStr, True);
  try
    PanelActions.Visible := true;
    miLogMessages.Checked := false;
  
    Timer.OnTimer(self);
    miAtivartimer.Checked := false;
    Timer.Enabled := false;
  
    TimerInitalizationAnimate.Enabled:=false;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.AdjustFrameWith(const poItemFrameBase: TItemFrameBase);
var
  oContextoLog: IMoteContextoRegistroLog;
  i, w, ww: integer;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.AdjustFrameWith', EmptyStr, True);
  try
    w := poItemFrameBase.Width;
    ww := (Width - 12);
    if w <> ww then
      poItemFrameBase.Width := ww;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


procedure TfrmMain.TimerTimer(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  i: integer;
  oItemFrameBase: TItemFrameBase;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.TimerTimer', EmptyStr, True);
  try
    for i := 0 to ScrollBoxMain.ComponentCount-1 do
    begin
      if ScrollBoxMain.Components[i] is TItemFrameBase then
      begin
        oItemFrameBase := ScrollBoxMain.Components[i] as TItemFrameBase;
        oItemFrameBase.RefreshTime;
        AdjustFrameWith(oItemFrameBase);
      end;
    end;
    FdtToday := now;
    FrameSumary.TodayValue:=FormatdateTime('dd/mm/yyyy', FdtToday);
    FrameSumary.Update;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TfrmMain.GetMessageClient: TMoteMessageClient;
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.GetMessageClient: TMoteMessageClient', EmptyStr, True);
  try
    Result := FoMessageClient;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.SetMessageClient(AValue: TMoteMessageClient);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.SetMessageClient', EmptyStr, True);
  try
    FoMessageClient := AValue;
    //event
    if Assigned(FoMessageClient) then
    begin
      FoMessageClient.OnMessage := @MessageEvent;
      FoMessageClient.OnPublish := @MessagePublish;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;



procedure TfrmMain.AddItemToGui(const psJSONItem: string);


var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.AddItemToGui', EmptyStr, True);
  try
    FoLastFrameInserted := FindItemGui(psJSONItem);
    if FoLastFrameInserted <> nil then
    begin
      FoLastFrameInserted.UpdateFromJSON(psJSONItem);
      //temp. action: a hidded item can be showed after init work
      FoLastFrameInserted.Visible := TestItemCanBeVisible(FoLastFrameInserted.Item);
      oContextoLog.GerarLogRetornoMetodo;
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TfrmMain.FindItemGui(const psJSONItem: string): TItemFrameBase;
var
  oContextoLog: IMoteContextoRegistroLog;
  i: integer;
  oItem: TItemDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.FindItemGui', EmptyStr, True);
  try
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
          oContextoLog.GerarLogRetornoMetodo;
          exit;
        end;
      end;
    end;
    finally
      oItem.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.SaveTimerEnabled;
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.SaveTimerEnabled', EmptyStr, True);
  try
    FbTimerEnabledOld := Timer.Enabled;
    Timer.Enabled := false;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.RestoreTimerEnabled;
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.RestoreTimerEnabled', EmptyStr, True);
  try
    Timer.Enabled := FbTimerEnabledOld;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TfrmMain.TestItemCanBeVisible(const poItem: TItemDto): boolean;
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.TestItemCanBeVisible', EmptyStr, True);
  try
    s := poItem.TimeIntervals + '|' + FormatDateTime('dd/mm/yyyy', poItem.CreationDateTime);
    if (s <> '') and (Pos(FormatDateTime('dd/mm/yyyy', Date), s) < 1) then
    begin
      oContextoLog.GerarLogRetornoMetodo('false');
      exit(false);
    end
    else
    begin
      oContextoLog.GerarLogRetornoMetodo('true');
      exit(true);
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


procedure TfrmMain.UpdateItemToGui(const psJSONItem: string);
var
  oContextoLog: IMoteContextoRegistroLog;
  oFrame: TItemFrameBase;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.UpdateItemToGui', EmptyStr, True);
  try
    oFrame := FindItemGui(psJSONItem);
    if oFrame <> nil then
    begin
      oFrame.UpdateFromJSON(psJSONItem);
      oFrame.Visible := TestItemCanBeVisible(oFrame.Item);
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.DeleteItemToGui(const psJSONItem: string);
var
  oContextoLog: IMoteContextoRegistroLog;
  oControl: TControl;
  oFrame: TItemFrameBase;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.DeleteItemToGui', EmptyStr, True);
  try
    oFrame := FindItemGui(psJSONItem);
    if oFrame = nil then
      begin
        oContextoLog.GerarLogRetornoMetodo;
        exit;
      end;
    oControl := oFrame As TControl;
    ScrollBoxMain.RemoveControl(oControl);
    oControl.Parent := nil;
    (oControl As TItemFrameBase).Free;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.GetRemoteNewItems(poItem: TSettingItemUriDto);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.GetRemoteNewItems', EmptyStr, True);
  try
    FsRemoteNewItemsUri := poItem.Uri;
    GetRemoteNewItemsFromUri(poItem.Uri);
    FdtRemoteNewItemsLastLoadTime := now;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TfrmMain.GetRemoteNewItemsFromUri(const psURI: string): boolean;
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
  oResult: TSettingNewItemOptionsDto;
  oItem: TSettingNewItemOptionsItemDto;
  oCi: TcollectionItem;
  i: integer;
  mi: TMenuItem;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.GetRemoteNewItemsFromUri', EmptyStr, True);
  try
    s := TFPHttpClient.SimpleGet(psURI);
    oResult := TSettingNewItemOptionsDto.Create(s);
    try
      if miLogMessages.checked then
        mmMessages.Lines.Add(Format('%d items from remote new items.', [oResult.items.Count]));
      if oResult.items.Count = 0 then
        exit(false);
  
      FslRemoteItems.Clear;
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
      exit(true);
    finally
      oResult.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo(Format('%d (bool)', [integer(result)]));
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.miItemInterruptionClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
  i: integer;
  oItem: TSettingItemBaseDto;
  sStart, sEnd: string;
  sl: TStringList;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miItemInterruptionClick', EmptyStr, True);
  try
    if not Assigned(FoLastFrameEventHandle) then
    begin
      oContextoLog.GerarLogRetornoMetodo;
      exit;
    end;
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
  
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acFilterSumaryExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acFilterSumaryExecute', EmptyStr, True);
  try
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acMoveTodayToForwardExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acMoveTodayToForwardExecute', EmptyStr, True);
  try
    FdtToday := FdtToday + 1.0;
    FrameSumary.TodayValue:=FormatdateTime('dd/mm/yyyy', FdtToday);
    FrameSumary.Update;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acMoveTodayToPrevExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  oInterval: TDateIntervalDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acMoveTodayToPrevExecute', EmptyStr, True);
  try
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.GetRemoteItemInterruptions(poItem: TSettingItemBaseDto);
var
  oContextoLog: IMoteContextoRegistroLog;
  mi: TMenuItem;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.GetRemoteItemInterruptions', EmptyStr, True);
  try
    if not poItem.Active then
    begin
      oContextoLog.GerarLogRetornoMetodo;
      exit;
    end;
    mi := TMenuItem.Create(PopupMenuItem);
    mi.Tag := FslItemInterruptionsItems.Count;
    mi.Caption := '&' + IntTostr(mi.Tag + 1) + ' - ' + poItem.name;
    mi.OnClick := @miItemInterruptionClick;
    FslItemInterruptionsItems.Add(poItem.ToString);
    PopupMenuItem.Items.Add(mi);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.miRemoteItemsClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
  i: integer;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miRemoteItemsClick', EmptyStr, True);
  try
    i := (Sender as TMenuItem).Tag;
    s := FslRemoteItems[i];
    AddItemFromJson(s);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


procedure TfrmMain.ApplySetting(const psJSONItem: string);
var
  oContextoLog: IMoteContextoRegistroLog;
  oItem: TSettingItemBaseDto;
  oItemUri: TSettingItemUriDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.ApplySetting', EmptyStr, True);
  try
    motelogfmt('psJSONItem: "%s"', [psJSONItem]);
    oItem := TSettingItemUriDto.Create(psJSONItem);
    motelogfmt('**** Kind: %s', [oItem.kind]);
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


procedure TfrmMain.AddItemFromJson(const psJSONString: string);
var
  oContextoLog: IMoteContextoRegistroLog;

  oItem: TSettingNewItemOptionsItemDto;
  e: TEventDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.AddItemFromJson', EmptyStr, True);
  try
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acAddItemExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
  e: TEventDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acAddItemExecute', EmptyStr, True);
  try
    SaveTimerEnabled;
    try
      s := '';
      if InputQuery('Novo item','Informe o Texto do item', s) then
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acAddRemoteItemExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acAddRemoteItemExecute', EmptyStr, True);
  try
    pmRemoteItems.PopupComponent:=(Sender as TComponent);
    pmRemoteItems.PopUp;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acTimerActivateExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acTimerActivateExecute', EmptyStr, True);
  try
    Timer.Enabled := not Timer.Enabled;
    miAtivartimer.Checked:=Timer.Enabled;
    if Timer.Enabled then
      Timer.OnTimer(self);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.acShowWeekExecute(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.acShowWeekExecute', EmptyStr, True);
  try
    frmWeek.Show;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.btnRemoteNewItemClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.btnRemoteNewItemClick', EmptyStr, True);
  try
  
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.Button1Click', EmptyStr, True);
  try
    mmMessages.Clear;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.ControlBarClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.ControlBarClick', EmptyStr, True);
  try
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.FormCreate', EmptyStr, True);
  try
    FrameSumary.ContainerOfItemsFrames := ScrollBoxMain;
    FrameSumary.CanControlContainerOfItemsFramesVisiblity := true;
    FdtToday := now;
    FrameSumary.TodayValue:=FormatDateTime('dd/mm/yyyy', FdtToday);;
    FslRemoteItems := TStringList.Create;
    FslItemInterruptionsItems := TStringList.Create;
    //force refrash of time
    if not Timer.enabled then
      acTimerActivateExecute(sender);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.FormDestroy', EmptyStr, True);
  try
    FslItemInterruptionsItems.Free;
    FslRemoteItems.Free;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.FormShow', EmptyStr, True);
  try
    //Caption := 'show';
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.miItemPopupDeleteItemClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  e: TEventDto;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miItemPopupDeleteItemClick', EmptyStr, True);
  try
    if FoLastFrameEventHandle = nil then
    begin
      oContextoLog.GerarLogRetornoMetodo;
      exit;
    end;
  
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
  
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.miItemPopupItemTextClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
  s: string;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miItemPopupItemTextClick', EmptyStr, True);
  try
    if FoLastFrameEventHandle = nil then
      begin
        oContextoLog.GerarLogRetornoMetodo;
        exit;
      end;
  
    s := FoLastFrameEventHandle.ToString;
    InputQuery('Texto do item','Informe o Título', s);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TfrmMain.miLogMessagesClick(Sender: TObject);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //07/04/2021 - maxback -  Adicionado log especial
  oContextoLog := motelog(ClassName + '.miLogMessagesClick', EmptyStr, True);
  try
    miLogMessages.Checked := not miLogMessages.Checked;
    pnLog.Visible := miLogMessages.Checked;
    Splitter.Visible := miLogMessages.Checked;
    Splitter.Top := Height - pnLog.Height - 10;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;

end;

end.

