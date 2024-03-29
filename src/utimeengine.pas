unit uTimeEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMoteMessage, uEventDto, uItemDto;

type

{ TTimeEngine }

TTimeEnginePairOfTimes = record
  dtStart: TDatetime;
  dtEnd: TDatetime;
end;

TTimeEngine = class
private
  FoMessageClient: TMoteMessageClient;
  procedure EndIterval(const poItem: TItemDto);
  function GetMessageClient: TMoteMessageClient;
  procedure SetMessageClient(AValue: TMoteMessageClient);
  procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
  class function ExtractTimes(const psString: string): TTimeEnginePairOfTimes;
  function GenerateTodayTimeText(const poItem: TItemDto): string;
  function GenerateTotalTimeText(const poItem: TItemDto): string;
  function GenerateEspecificDateTimeText(const poItem: TItemDto; const pdtEspecificDate: TDatetime; const psPrefix: string = #13#10): string;
  procedure StartIterval(const poItem: TItemDto);
  function TestToday(const psString: string): boolean;
  function TestDate(const psString: string; const pdtEspecificDate: TDatetime): boolean;
  class function GetTimeStringInExtendedFormat(const pdttime: TDateTime): Extended;
public
  property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;
  constructor Create;
  destructor Destroy; override;
  class function GenerateTotalTimeTextFromStringList(sl: TStringList; const pbOnlyDecimalTimeFormat: boolean): string;
end;

implementation

uses
  uMoteUtils, DateUtils;

{ TTimeEngine }


procedure TTimeEngine.StartIterval(const poItem: TItemDto);
var
  sl: TStringList;
  s: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := poItem.TimeIntervals;
    sl.Add(FormatDateTime('dd/mm/yyyy hh:mm', now));
    s := sl.Text;
    poItem.TimeIntervals := s;
    poItem.Working:=true;
  finally
    sl.Free;
  end;
end;

class function TTimeEngine.ExtractTimes(const psString: string): TTimeEnginePairOfTimes;
var
  nPos1, nPos2: SizeInt;
  sStart, sEnd: string;
begin
  result.dtStart := 0.0;
  result.dtEnd := 0.0;

  nPos1 := Pos(' ', psString);
  nPos2 := Pos(' - ', psString);


  if nPos1 > 0 then
  begin
    sStart := Copy(psString, nPos1+1, 5);
    result.dtStart := StrToTime(sStart);
  end
  else
    exit;
  if nPos2 > nPos1 then
  begin
    sEnd := Copy(psString, nPos2+3, 5);
    if sEnd = '24:00' then
      sEnd := '23:59:59';
    result.dtEnd := StrToTime(sEnd);
  end;
end;

procedure TTimeEngine.EndIterval(const poItem: TItemDto);
var
  sl: TStringList;
  s: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := poItem.TimeIntervals;
    if sl.Count < 1 then
      exit;
    s := sl[sl.Count-1];
    if not TestToday(s) then
    begin
      StartIterval(poItem);
      exit;
    end;
    s := s + FormatDateTime(' - hh:mm', now);
    sl[sl.Count-1] := s;
    poItem.TimeIntervals := sl.Text;
    poItem.Working:=false;
  finally
    sl.Free;
  end;
end;

function TTimeEngine.TestToday(const psString: string): boolean;
var
  sToday: string;
begin
  sToday := Formatdatetime('dd/mm/yyyy', now);
  result := Pos(sToday, psString) = 1;
end;

function TTimeEngine.TestDate(const psString: string;
  const pdtEspecificDate: TDatetime): boolean;
var
  sDate: string;
begin
  sDate := Formatdatetime('dd/mm/yyyy', pdtEspecificDate);
  result := Pos(sDate, psString) = 1;
end;

class function TTimeEngine.GetTimeStringInExtendedFormat(const pdttime: TDateTime
  ): Extended;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  eHour, eMinute, eSecond: Extended;
begin
  DecodeDateTime(pdtTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  eHour := AHour * 1.0;
  eMinute := AMinute * 1.0;
  eSecond := ASecond * 1.0;

  result :=  eHour;
  result := result + (eMinute / 60.0);
  result := result + (eSecond / (60.0 * 60.0));
end;

function TTimeEngine.GenerateTodayTimeText(const poItem: TItemDto): string;
var
  i: integer;
  sl: TStringList;
  dtSum: TDateTime;
  dtLast: TDateTime;
  s: string;
  pair: TTimeEnginePairOfTimes;
begin
  dtSum := 0.0;
  sl := TStringList.Create;
  try
    sl.Text := poItem.TimeIntervals;
    for i := 0 to sl.Count-1 do
    begin
      s := sl[i];
      if not TestToday(s) then
        continue;
      pair := ExtractTimes(s);
      if pair.dtStart = 0.0 then
        continue;
      if pair.dtEnd = 0.0 then
        pair.dtEnd := Time;
      dtLast := (pair.dtEnd - pair.dtStart);
      dtSum := dtSum + dtLast;
    end;

    if dtSum = TDateTime(0.0) then
      exit('');

    Result := FormatDateTime('hh:mm', dtSum)+ ' (' + Format('%0.3f', [GetTimeStringInExtendedFormat(dtSum)])+')';
    //if sl.Count < 2 then
    //  exit;

    Result := Result + #13#10 + '[';
    if poItem.Working then
      Result := Result + 'Actual: '
    else
      Result := Result + 'Last: ';
    if dtLast < 0.0 then
      Result := Result + 'CountDown: -';
    Result := Result + FormatDateTime('hh:mm', dtLast)+ ' (' + Format('%0.3f', [GetTimeStringInExtendedFormat(dtLast)]) +
    ')]';

  finally
    sl.Free;
  end;
end;

function TTimeEngine.GenerateEspecificDateTimeText(const poItem: TItemDto; const pdtEspecificDate: TDatetime; const psPrefix: string): string;
var
  i: integer;
  sl: TStringList;
  dtSum: TDateTime;
  dtLast: TDateTime;
  s: string;
  sEspecificDate: string;
  pair: TTimeEnginePairOfTimes;
begin
  sEspecificDate := FormatDateTime('dd/mm/yyyy', pdtEspecificDate);
  dtSum := 0.0;
  sl := TStringList.Create;
  try
    sl.Text := poItem.TimeIntervals;
    for i := 0 to sl.Count-1 do
    begin
      s := sl[i];
      if not TestDate(s, pdtEspecificDate) then
        continue;
      pair := ExtractTimes(s);
      if pair.dtStart = 0.0 then
        continue;
      if pair.dtEnd = 0.0 then
        pair.dtEnd := Time;
      dtLast := (pair.dtEnd - pair.dtStart);
      dtSum := dtSum + dtLast;
    end;

    if (dtSum = TDateTime(0.0)) and (pdtEspecificDate <> Date) then
      exit('');

    Result := psPrefix + sEspecificDate + ': ' + FormatDateTime('hh:mm', dtSum)+ ' (' + Format('%0.3f', [GetTimeStringInExtendedFormat(dtSum)])+')';
  finally
    sl.Free;
  end;
end;


class function TTimeEngine.GenerateTotalTimeTextFromStringList(sl: TStringList; const pbOnlyDecimalTimeFormat: boolean): string;
var
  i: integer;
  dtSum: TDateTime;
  s: string;
  pair: TTimeEnginePairOfTimes;
begin
  dtSum := 0.0;
  for i := 0 to sl.Count-1 do
  begin
    s := sl[i];
    pair := ExtractTimes(s);
    if pair.dtStart = 0.0 then
      continue;
    if pair.dtEnd = 0.0 then
      pair.dtEnd := Time;
    if pair.dtEnd > pair.dtStart then
      dtSum := dtSum + (pair.dtEnd - pair.dtStart);
  end;

  if dtSum = TDateTime(0.0) then
    exit('0');

  if pbOnlyDecimalTimeFormat then
    Result := Format('%0.3f', [GetTimeStringInExtendedFormat(dtSum)])
  else
    Result := FormatDateTime('hh:mm', dtSum)+ ' (' + Format('%0.3f', [GetTimeStringInExtendedFormat(dtSum)])+')';
end;


function TTimeEngine.GenerateTotalTimeText(const poItem: TItemDto): string;
var
  i: integer;
  sl: TStringList;
  dtSum: TDateTime;
  s: string;
  pair: TTimeEnginePairOfTimes;
begin
  dtSum := 0.0;
  sl := TStringList.Create;
  try
    sl.Text := poItem.TimeIntervals;
    for i := 0 to sl.Count-1 do
    begin
      s := sl[i];
      pair := ExtractTimes(s);
      if pair.dtStart = 0.0 then
        continue;
      if pair.dtEnd = 0.0 then
        pair.dtEnd := Time;
      if pair.dtEnd > pair.dtStart then
        dtSum := dtSum + (pair.dtEnd - pair.dtStart);
    end;

    if dtSum = TDateTime(0.0) then
      exit('');

    Result := FormatDateTime('hh:mm', dtSum)+ ' (' + Format('%0.3f', [GetTimeStringInExtendedFormat(dtSum)])+')';
  finally
    sl.Free;
  end;
end;

procedure TTimeEngine.MessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oItem: TItemDto;
  oItemInterrpted: TItemDto;
  e: TEventDto;
  s: string;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  oItem := TItemDto.CreateFromJSON(e.payload);
  oItemInterrpted := nil;
  try

    if e.event = 'item_init_work' then
    begin
      StartIterval(oItem);
      oItem.Time:= GenerateTotalTimeText(oItem) + ' Today: ' + GenerateTodayTimeText(oItem) +
      GenerateEspecificDateTimeText(oItem, now - 1.0) +
      GenerateEspecificDateTimeText(oItem, now - 2.0) +
      GenerateEspecificDateTimeText(oItem, now - 3.0) +
      GenerateEspecificDateTimeText(oItem, now - 4.0) +
      GenerateEspecificDateTimeText(oItem, now - 5.0) +
      GenerateEspecificDateTimeText(oItem, now - 6.0) +
      GenerateEspecificDateTimeText(oItem, now - 7.0);
      e.event:='item_update';
      e.payload:=oItem.ToString;
      FoMessageClient.Publish('item', e.ToString);
    end
    else
    if (e.event = 'item_stop') or (e.event = 'item_interrupt') then
    begin
      EndIterval(oItem);
      oItem.Time:= GenerateTotalTimeText(oItem) + ' Today: ' + GenerateTodayTimeText(oItem) +
      GenerateEspecificDateTimeText(oItem, now - 1.0) +
      GenerateEspecificDateTimeText(oItem, now - 2.0) +
      GenerateEspecificDateTimeText(oItem, now - 3.0) +
      GenerateEspecificDateTimeText(oItem, now - 4.0) +
      GenerateEspecificDateTimeText(oItem, now - 5.0) +
      GenerateEspecificDateTimeText(oItem, now - 6.0) +
      GenerateEspecificDateTimeText(oItem, now - 7.0);
      e.event:='item_update';
      e.payload:=oItem.ToString;
      FoMessageClient.Publish('item', e.ToString);
      //if this item interrup another, then that item will by resumed
      if oItem.ParentItemId <> '' then
      begin
        oItemInterrpted := TItemDto.Create;
        oItemInterrpted.Id:= oItem.ParentItemId;
        oItemInterrpted.Working:=true;
        e.event:='item_restore';
        FoMessageClient.Publish('item', e.ToString);
      end;
    end
    else
    if (e.event = 'item_time_edited') or (e.event = 'item_refresh_query') then
    begin
      s := GenerateTotalTimeText(oItem) + ' Today: ' + GenerateTodayTimeText(oItem) +
        GenerateEspecificDateTimeText(oItem, now - 1.0) +
        GenerateEspecificDateTimeText(oItem, now - 2.0) +
        GenerateEspecificDateTimeText(oItem, now - 3.0) +
        GenerateEspecificDateTimeText(oItem, now - 4.0) +
        GenerateEspecificDateTimeText(oItem, now - 5.0) +
        GenerateEspecificDateTimeText(oItem, now - 6.0) +
        GenerateEspecificDateTimeText(oItem, now - 7.0);
      if s = oItem.Time then
        exit;
      oItem.Time:= s;
      if e.event = 'item_time_edited' then
        e.event:='item_update'
      else
        e.event:='item_refresh';
      e.payload:=oItem.ToString;
      FoMessageClient.Publish('item', e.ToString);
    end;
  finally
    oItemInterrpted.Free;
    oItem.Free;
    e.Free;
  end;
end;

function TTimeEngine.GetMessageClient: TMoteMessageClient;
begin
  result := FoMessageClient;
end;

procedure TTimeEngine.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  if Assigned(FoMessageClient) then
  begin
    FoMessageClient.OnMessage := @MessageEvent;
    FoMessageClient.Subscribe('item');
    FoMessageClient.Subscribe('app');
  end;
end;


constructor TTimeEngine.Create;
begin
  inherited Create;
  FoMessageClient := nil;
end;

destructor TTimeEngine.Destroy;
begin
  inherited Destroy;
end;


end.

