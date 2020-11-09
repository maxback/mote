unit uPomodoro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uMoteMessage, uEventDto, uItemDto, uSettingPoromodoroItemDto;

type
  TMotePomodoroItemStatus = (mpisWorking, mpisBreak, mpisStoped);
  TMotePomodoroItem = class
  public
    Id: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    RestartTime: TDateTime;
    Status: TMotePomodoroItemStatus;
  end;

TMotePomodoroList = specialize TFPGObjectList<TMotePomodoroItem>;


{ TMotePomodoro }

TMotePomodoro = class
private
  FList: TMotePomodoroList;
  FoSetting: TSettingPoromodoroItemDto;
  FoLastMotePomodoroItem: TMotePomodoroItem;
  FoMessageClient: TMoteMessageClient;
  procedure EndIterval(const poItem: TItemDto);
  function CheckIterval(const poItem: TItemDto): boolean;
  function GetMessageClient: TMoteMessageClient;
  procedure SetMessageClient(AValue: TMoteMessageClient);
  procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
  //function ExtractTimes(const psString: string): TTimeEnginePairOfTimes;
  //function GenerateTodayTimeText(const poItem: TItemDto): string;
  //function GenerateTotalTimeText(const poItem: TItemDto): string;
  procedure StartIterval(const poItem: TItemDto);
  procedure ConfigureTimes(poPomodoroItem: TMotePomodoroItem);
  //function TestToday(const psString: string): boolean;
  //function GetTimeStringInExtendedFormat(const pdttime: TDateTime): Extended;
public
  property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;
  constructor Create;
  destructor Destroy; override;
end;
implementation

{ TMotePomodoro }


procedure TMotePomodoro.EndIterval(const poItem: TItemDto);
var
  i: integer;
begin
  for i :=0 to FList.Count-1 do
  begin
    if FList[i].Id = poItem.Id then
    begin
      FList.Delete(i);
      exit;
    end;
  end;
end;

function TMotePomodoro.CheckIterval(const poItem: TItemDto): boolean;
var
  i: integer;
begin
  result := false;
  for i :=0 to FList.Count-1 do
  begin
    if FList[i].Id = poItem.Id then
    begin
      case FList[i].Status of
        mpisWorking:
          if FList[i].EndTime < now then
          begin
            result := true;
            FList[i].Status := mpisBreak;
          end;
        mpisBreak:
          if FList[i].RestartTime < now then
          begin
            result := true;
            ConfigureTimes(FList[i]);
          end;
      end;
    end;
  end;
end;

function TMotePomodoro.GetMessageClient: TMoteMessageClient;
begin
  Result := FoMessageClient;
end;

procedure TMotePomodoro.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  if Assigned(FoMessageClient) then
  begin
    FoMessageClient.OnMessage := @MessageEvent;
    FoMessageClient.Subscribe('setting');
    FoMessageClient.Subscribe('item');
  end;
end;

procedure TMotePomodoro.MessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oItem: TItemDto;
  oSetting: TSettingPoromodoroItemDto;
  oItemInterrpted: TItemDto;
  e: TEventDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  oItem := TItemDto.CreateFromJSON(e.payload);
  oItemInterrpted := nil;
  try

    if e.event = 'setting_restore' then
    begin
      oSetting := TSettingPoromodoroItemDto.Create(e.payload);
      if oSetting.kind = 'local-pomodoro' then
      begin
        oSetting.CopySettingTo(FoSetting);
      end;
    end
    else
    if e.event = 'item_init_work' then
    begin
      if not FoSetting.active then
        exit;

      FoLastMotePomodoroItem := TMotePomodoroItem.Create;
      FList.Add(FoLastMotePomodoroItem);
      StartIterval(oItem);
      e.Payload := '{"status": "start"}';
      //FoMessageClient.Publish('pomodoro', e.ToString);
    end
    else
    if (e.event = 'item_stop') or (e.event = 'item_interrupt') then
    begin
      if not FoSetting.active then
        exit;

      EndIterval(oItem);
      FoLastMotePomodoroItem := nil;
      e.Payload := '{"status": "stop"}';
      //FoMessageClient.Publish('pomodoro', e.ToString);
    end
    else
    if (e.event = 'item_time_edited') or (e.event = 'item_refresh_query') then
    begin
      if not FoSetting.active then
        exit;

      if CheckIterval(oItem) then
      begin
        e.Payload := '{"status": "alarm"}';
        //FoMessageClient.Publish('pomodoro', e.ToString);
      end;
    end;
  finally
    oSetting.Free;
    oItemInterrpted.Free;
    oItem.Free;
    e.Free;
  end;
end;

procedure TMotePomodoro.ConfigureTimes(poPomodoroItem: TMotePomodoroItem);
begin
  poPomodoroItem.StartTime:=now;
  poPomodoroItem.EndTime:=poPomodoroItem.StartTime+25;
  poPomodoroItem.RestartTime:=poPomodoroItem.EndTime+5;
  poPomodoroItem.Status:=mpisWorking;
end;

procedure TMotePomodoro.StartIterval(const poItem: TItemDto);
begin
  FoLastMotePomodoroItem.Id := poItem.Id;
  ConfigureTimes(FoLastMotePomodoroItem);
end;

constructor TMotePomodoro.Create;
begin
  FList := TMotePomodoroList.Create;
  FoSetting := TSettingPoromodoroItemDto.Create;
end;

destructor TMotePomodoro.Destroy;
begin
  FoSetting.Free;
  FList.Free;
  inherited Destroy;
end;

end.

