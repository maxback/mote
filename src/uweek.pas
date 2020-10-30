unit uWeek;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, uMoteMessage, uEventDto, uItemDto,
  uDateIntervalDto, uItemFrameWeekCompact;

type

  { TfrmWeek }

  TfrmWeek = class(TForm)
    Button1: TButton;
    DateTimePicker: TDateTimePicker;
    GroupBox: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListBoxMessages: TListBox;
    Panel: TPanel;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    ScrollBox4: TScrollBox;
    ScrollBox5: TScrollBox;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure DateTimePickerChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gbClick(Sender: TObject);
    procedure PanelClick(Sender: TObject);
  private
    FnContador: integer;
    FoMessageClient: TMoteMessageClient;
    FoLastFrameInserted: TItemFrameWeekCompact;
    FoLastFrameEventHandle: TItemFrameWeekCompact;
    FaScrollBox: array[2..6] of TScrollBox;
    FaDates: array[2..6] of TDateTime;
    FaGroupBox: array[2..6] of TGroupBox;
    procedure AddItemToScrollBox(const poScrollBox: TScrollBox;
      const psJSONItem: string);
    procedure AjustGroupBoxDayOfWeek(poInterval: TDateIntervalDto);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure MessagePublish(Sender: Tobject; const poMessage: TMoteMessage);
    procedure AddItemsToGui(const psJSONItem: string);
    procedure ClearAllItems;
    procedure UpdateItemToGui(const psJSONItem: string);
    function GetMessageClient: TMoteMessageClient;
    procedure SetMessageClient(AValue: TMoteMessageClient);

    function ItemFrameBaseEvent(Sender: TObject; const psEvent: string; const poParam: TObject; var poResponse: TObject): boolean;

  public
    property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;
    procedure Show;
    procedure InternalHide;
  end;

var
  frmWeek: TfrmWeek;

implementation

{$R *.lfm}

{ TfrmWeek }

procedure TfrmWeek.PanelClick(Sender: TObject);
begin

end;

procedure TfrmWeek.MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
var
  e: TEventDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  try
    ListBoxMessages.Items.Add('received < [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);

    if e.event = 'item_restore' then
    begin
      AddItemsToGui(e.payload);
    end
    else
    if (e.event = 'item_update') or (e.event = 'item_refresh') then
    begin
      UpdateItemToGui(e.payload);
    end;
  finally
    e.Free;
  end;
end;

procedure TfrmWeek.MessagePublish(Sender: Tobject; const poMessage: TMoteMessage
  );
begin
  //only to gui log
  ListBoxMessages.Items.Add(' sent > [' + FormatDateTime('hh:mm:ss.nnn', Now) + ' - ' + poMessage.Orign + ']: ' + poMessage.ToString);
end;


procedure TfrmWeek.AddItemToScrollBox(const poScrollBox: TScrollBox; const psJSONItem: string);
begin
  FnContador := FnContador + 1;

  FoLastFrameInserted := TItemFrameWeekCompact.Create(poScrollBox);
  FoLastFrameInserted.Name := 'Item_' + FnContador.ToString();
  FoLastFrameInserted.Parent := poScrollBox;
  FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

  FoLastFrameInserted.UpdateFromJSON(psJSONItem);
end;


procedure TfrmWeek.AddItemsToGui(const psJSONItem: string);
var
  i: integer;
  oItem: TItemDto;
begin
  //add the same item in each day of week where it exists
  oItem := TItemDto.CreateFromJSON(psJSONItem);
  try
    for i := Low(FaScrollBox) to High(FaScrollBox) do
    begin
      if Pos(FormatDateTime('dd/mm/yyyy', FaDates[i]), oItem.TimeIntervals) > 0 then
        AddItemToScrollBox(FaScrollBox[i], psJSONItem);
    end;
  finally
    oItem.Free;
  end;
end;

procedure TfrmWeek.ClearAllItems;
var
  oControl: TControl;
  n, i: integer;
  sb: TScrollBox;
begin
  //add the same item in each day of week where it exists
  for n := Low(FaScrollBox) to High(FaScrollBox) do
  begin
    sb := FaScrollBox[n];
    i := 0;
    while i < sb.ComponentCount do
    begin
      if sb.Components[i] is TItemFrameWeekCompact then
      begin
        oControl := sb.Components[i] as TControl;
        sb.RemoveControl(oControl);
        oControl.Parent := nil;
        (oControl As TItemFrameWeekCompact).Free;
      end
      else
        i := i + 1;
    end;
  end;
end;

procedure TfrmWeek.UpdateItemToGui(const psJSONItem: string);
var
  n, i: integer;
  oItem: TItemDto;
  sb: TScrollBox;
begin
  //add the same item in each day of week where it exists
  oItem := TItemDto.CreateFromJSON(psJSONItem);
  try
    for n := Low(FaScrollBox) to High(FaScrollBox) do
    begin
      sb := FaScrollBox[n];

      for i := 0 to sb.ComponentCount-1 do
       begin
       if sb.Components[i] is TItemFrameWeekCompact then
       begin
         if (sb.Components[i] as TItemFrameWeekCompact).Item.Id = oItem.Id then
         begin
           (sb.Components[i] as TItemFrameWeekCompact).UpdateFromJSON(psJSONItem);
         end;
       end;
      end;
    end;
  finally
    oItem.Free;
  end;
end;

function TfrmWeek.GetMessageClient: TMoteMessageClient;
begin
  Result := FoMessageClient;
end;

procedure TfrmWeek.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  //event
  if Assigned(FoMessageClient) then
  begin
    FoMessageClient.OnMessage := @MessageEvent;
    FoMessageClient.OnPublish := @MessagePublish;
  end;
end;

function TfrmWeek.ItemFrameBaseEvent(Sender: TObject; const psEvent: string;
  const poParam: TObject; var poResponse: TObject): boolean;
var
  e: TEventDto;
begin
  FoLastFrameEventHandle := Sender as TItemFrameWeekCompact;
  e := TEventDto.CreateEventObject(psEvent, FoLastFrameEventHandle.Item);
  try

    if Assigned(FoMessageClient) then
    FoMessageClient.Publish('item', e.ToString);
  finally
    e.Free;
  end;
end;

procedure TfrmWeek.Show;
begin
  if Assigned(FoMessageClient) then
    FoMessageClient.Subscribe('*');
  Visible := true;
end;

procedure TfrmWeek.InternalHide;
begin
  if Assigned(FoMessageClient) then
    FoMessageClient.UnSubscribe('*');
  Visible := False;
end;

procedure TfrmWeek.FormResize(Sender: TObject);
begin
  GroupBox1.Width:= Width div 5;
  GroupBox2.Width:= Width div 5;
  GroupBox3.Width:= Width div 5;
  GroupBox4.Width:= Width div 5;
  GroupBox5.Width:= Width div 5;
end;

procedure TfrmWeek.gbClick(Sender: TObject);
begin

end;

procedure TfrmWeek.FormCreate(Sender: TObject);
begin
  FaScrollBox[2] := ScrollBox1;
  FaScrollBox[3] := ScrollBox2;
  FaScrollBox[4] := ScrollBox3;
  FaScrollBox[5] := ScrollBox4;
  FaScrollBox[6] := ScrollBox5;

  FaGroupBox[2] := GroupBox1;
  FaGroupBox[3] := GroupBox2;
  FaGroupBox[4] := GroupBox3;
  FaGroupBox[5] := GroupBox4;
  FaGroupBox[6] := GroupBox5;
end;

procedure TfrmWeek.AjustGroupBoxDayOfWeek(poInterval: TDateIntervalDto);
var
  dt: TDateTime;
  i: integer;
begin
  dt := poInterval.StartDate;
  for i := Low(FaGroupBox) to High(FaGroupBox) do
  begin
    FaGroupBox[i].Caption := FormatDateTime('dd/mm/yyyy', dt);
    dt := dt + 1;
  end;
end;

procedure TfrmWeek.DateTimePickerChange(Sender: TObject);
var
  dts, dte, dt: TDateTime;
  i, nDayOfW, nDiff: integer;
  oInterval: TDateIntervalDto;
begin
  nDayOfW := DayOfWeek(DateTimePicker.Date);
  nDiff := nDayOfW - 2;

  dt := DateTimePicker.Date - nDiff;
  dts := dt;
  for i := Low(FaDates) to High(FaDates) do
  begin
    FaDates[i] := dt;
    dte := dt;
    dt := dt + 1;
  end;

  if Assigned(FoMessageClient) then
  begin
    ClearAllItems;
    oInterval := TDateIntervalDto.CreateInterval(dts, dte);
    try
      AjustGroupBoxDayOfWeek(oInterval);
      FoMessageClient.Publish('date_interval_select', oInterval.ToString);
    finally
      oInterval.Free;
    end;
  end;

end;

procedure TfrmWeek.Button1Click(Sender: TObject);
begin
  ListBoxMessages.Clear;
end;

procedure TfrmWeek.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  InternalHide;
end;

end.

