unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, StdCtrls, uItemFrameBase, uMoteMessage, uEventDto,
  uItemDto;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAddItem: TAction;
    acShowWeek: TAction;
    acAtivarTimerAtualizacoesTempos: TAction;
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
    PopupMenuItem: TPopupMenu;
    ScrollBoxMain: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Timer: TTimer;
    procedure acAddItemExecute(Sender: TObject);
    procedure acAtivarTimerAtualizacoesTemposExecute(Sender: TObject);
    procedure acShowWeekExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ControlBarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miItemPopupDeleteItemClick(Sender: TObject);
    procedure miItemPopupItemTextClick(Sender: TObject);
    procedure miUsertCodesEditorClick(Sender: TObject);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure MessagePublish(Sender: Tobject; const poMessage: TMoteMessage);
    function ItemFrameBaseEvent(Sender: TObject; const psEvent: string; const poParam: TObject; var poResponse: TObject): boolean;
    procedure TimerTimer(Sender: TObject);
  private
    FnContador: integer;
    FoMessageClient: TMoteMessageClient;
    FoLastFrameInserted: TItemFrameBase;
    FoLastFrameEventHandle: TItemFrameBase;

    procedure AddItemToGui(const psJSONItem: string);
    procedure UpdateItemToGui(const psJSONItem: string);
    procedure DeleteItemToGui(const psJSONItem: string);
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
  uUserCodesEditor, uMoteUtils, uWeek;

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
    if psEvent = 'item_option' then
    begin
      PopupMenuItem.PopupComponent:=(poParam as TComponent);
      PopupMenuItem.PopUp;
      exit(true);
    end;
    if psEvent = 'item_interrupt' then
    begin
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
begin
  FnContador := FnContador + 1;

  FoLastFrameInserted := TItemFrameBase.Create(ScrollBoxMain);
  FoLastFrameInserted.Name := 'Item_' + IntToStr(FnContador);
  FoLastFrameInserted.Parent := ScrollBoxMain;
  FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

  FoLastFrameInserted.UpdateFromJSON(psJSONItem);
end;


procedure TfrmMain.UpdateItemToGui(const psJSONItem: string);
var
  i: integer;
  oItem: TItemDto;
begin
  oItem := TItemDto.CreateFromJSON(psJSONItem);
  try
  for i := 0 to ScrollBoxMain.ComponentCount-1 do
  begin
    if ScrollBoxMain.Components[i] is TItemFrameBase then
    begin
      if (ScrollBoxMain.Components[i] as TItemFrameBase).Item.Id =  oItem.Id then
      begin
        (ScrollBoxMain.Components[i] as TItemFrameBase).UpdateFromJSON(psJSONItem);
      end;
    end;
  end;
  finally
    oItem.Free;
  end;
end;

procedure TfrmMain.DeleteItemToGui(const psJSONItem: string);
var
  i: integer;
  oItem: TItemDto;
  oControl: TControl;
begin
  oItem := TItemDto.CreateFromJSON(psJSONItem);
  try
  for i := 0 to ScrollBoxMain.ComponentCount-1 do
  begin
    if ScrollBoxMain.Components[i] is TItemFrameBase then
    begin
      if (ScrollBoxMain.Components[i] as TItemFrameBase).Item.Id =  oItem.Id then
      begin
        oControl := ScrollBoxMain.Components[i] as TControl;
        ScrollBoxMain.RemoveControl(oControl);
        oControl.Parent := nil;
        (oControl As TItemFrameBase).Free;
        break;
      end;
    end;
  end;
  finally
    oItem.Free;
  end;
end;



procedure TfrmMain.acAddItemExecute(Sender: TObject);
var
  s: string;
var
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

procedure TfrmMain.acAtivarTimerAtualizacoesTemposExecute(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
  miAtivartimer.Checked:=Timer.Enabled;
end;

procedure TfrmMain.acShowWeekExecute(Sender: TObject);
begin
  frmWeek.Show;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ListBoxMessages.Clear;
end;

procedure TfrmMain.ControlBarClick(Sender: TObject);
begin
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

