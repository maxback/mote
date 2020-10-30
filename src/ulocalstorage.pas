unit uLocalStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMoteMessage, Dos, uItemDto, uEventDto;

type

{ TLocalStorage }

TLocalStorage = class
  private
    FsFilesystemStorageDir: string;
    FslLastQueryResult: TStringList;
    FoMessageClient: TMoteMessageClient;
    procedure AppMessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure DefaultMessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    procedure ItemMessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    function GetMessageClient: TMoteMessageClient;
    function GenerateItemFileName(const poItem: TItemDto): string;
    procedure SetMessageClient(AValue: TMoteMessageClient);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    function TestFileContainText(const psFileName: string; const paTextToFind: array of string): boolean;
  protected
    procedure QueryAllItem;
    procedure QueryByDate(const pdtStartDate, pdtEndDate: TDateTime; const pbIncludeWithoutInterval: boolean = false);
    procedure SendMessagesForLastQueryResult;
    procedure InsertItem(const poItem: TItemDto);
    procedure UpdateItem(const poItem: TItemDto);
    procedure DeleteItem(const poItem: TItemDto);
  public
    property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;
    constructor Create(const psFilesystemStorageDir: string);
    destructor Destroy; override;
  end;

implementation

uses
  uDateIntervalParamDto;

{ TLocalStorage }


procedure TLocalStorage.DefaultMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  e: TEventDto;
  di: TDateIntervalDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  try
    if poMessage.Topic = 'date_interval_select' then
    begin
      di := TDateIntervalDto.CreateFromJSON(poMessage.Payload);
      try
        //load all data interval
        QueryByDate(di.StartDate, di.EndDate);
        SendMessagesForLastQueryResult;
      finally
        di.Free;
      end;
    end
  finally
    e.Free;
  end;
end;


procedure TLocalStorage.AppMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  e: TEventDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  try
    if e.event = 'create' then
    begin
      //load all data for today
      QueryByDate(Date, Date, true);
      SendMessagesForLastQueryResult;
    end;

  finally
    e.Free;
  end;
end;

function TLocalStorage.GenerateItemFileName(const poItem: TItemDto): string;
var
  sId: string;
begin
  sId := poItem.Id;
  if Length(sId) < 5 then
    sId := 'invalid_id_' + FormatDatetime('yyyy-mm-dd-hh-mm-ss-nnn', now);
  result := FsFilesystemStorageDir+sId+'.json';
end;

procedure TLocalStorage.InsertItem(const poItem: TItemDto);
var
  sl: TStringList;
  sFileName: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := poItem.ToString;
    sFileName := GenerateItemFileName(poItem);
    sl.SaveToFile(sFileName);
  finally
    sl.Free;
  end;
end;

procedure TLocalStorage.UpdateItem(const poItem: TItemDto);
var
  sl: TStringList;
  sFileName: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := poItem.ToString;
    sFileName := GenerateItemFileName(poItem);
    sl.SaveToFile(sFileName);
  finally
    sl.Free;
  end;
end;

procedure TLocalStorage.DeleteItem(const poItem: TItemDto);
var
  sFileName: string;
begin
  sFileName := GenerateItemFileName(poItem);
  DeleteFile(sFileName);
end;

procedure TLocalStorage.ItemMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oItem: TItemDto;
var
  e: TEventDto;
begin
  e := TEventDto.CreateFromJSON(poMessage.Payload);
  oItem := TItemDto.CreateFromJSON(e.payload);
  try
    //check the action to create or update data
    if e.event = 'item_create' then
       InsertItem(oItem);
    if e.event = 'item_update' then
       UpdateItem(oItem);
    if e.event = 'item_delete' then
       DeleteItem(oItem);
  finally
    oItem.Free;
    e.Free;
  end;
end;

procedure TLocalStorage.MessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
begin
  //if poMessage.Orign = FoMessageClient.Orign then
  //  exit;
  FoMessageClient.Publish('debug', '{"event": "TLocalStorage.MessageEvent", "message": '+poMessage.ToString+'}');
  try
  if poMessage.Orign = 'app' then
    AppMessageEvent(Sender, poMessage);
  if poMessage.Topic = 'item' then
    ItemMessageEvent(Sender, poMessage)
  else
    DefaultMessageEvent(Sender, poMessage)


  except
    on e:exception do
      FoMessageClient.Publish('debug', '{"event": "exception", "value": "'+E.Message+'"}');
  end;
end;

procedure TLocalStorage.QueryAllItem;
var
  sr: SearchRec;
  Att: Word;
begin
  FslLastQueryResult.Clear;
  Att := archive or readonly;

  FindFirst(FsFilesystemStorageDir+'*.*', Att, sr);
  while DosError = 0 do
  begin
    FslLastQueryResult.Add(sr.Name);
    FindNext(sr);
  end;
  FindClose(sr);
end;

procedure TLocalStorage.QueryByDate(const pdtStartDate, pdtEndDate: TDateTime; const pbIncludeWithoutInterval: boolean);
var
  sr: SearchRec;
  Att: Word;
  dtDate: TDateTime;
  sDate: string;
  i: integer;
  sText: string;
begin
  FslLastQueryResult.Clear;
  Att := archive or readonly;

  dtDate := pdtStartDate;
  while dtDate <= pdtEndDate do
  begin
    sDate := FormatDateTime('dd/mm/yyyy', dtDate);
    FindFirst(FsFilesystemStorageDir+'*.*', Att, sr);
    while DosError = 0 do
    begin
      if pbIncludeWithoutInterval and (dtDate = pdtEndDate) then
        sText := '"TimeIntervals" : ""'
      else
        sText := '';
      if TestFileContainText(FsFilesystemStorageDir + sr.Name, [sDate, sText]) then
      begin
        i := FslLastQueryResult.IndexOf(sr.Name);
        if i >= 0 then
        begin
          FslLastQueryResult.Delete(i);
        end;
        FslLastQueryResult.Add(sr.Name);
      end;
      FindNext(sr);
    end;
    FindClose(sr);
    dtDate := dtDate + 1;
  end;
end;

procedure TLocalStorage.SendMessagesForLastQueryResult;
var
  i: integer;
  sl: TStringList;
  e: TEventDto;
begin
  sl := TStringList.Create;
  e := TEventDto.Create;
  try
    for i:=0 to FslLastQueryResult.Count-1 do
    begin
      sl.LoadFromFile(FsFilesystemStorageDir+FslLastQueryResult[i]);
      e.event := 'item_restore';
      e.payload:=sl.Text;
      FoMessageClient.Publish('item', e.ToString);
    end;
  finally
    e.Free;
    sl.Free;
  end;
end;

function TLocalStorage.TestFileContainText(const psFileName: string; const paTextToFind: array of string): boolean;
var
  sl: TStringList;
  nPos: SizeInt;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(psFileName);
    for i := Low(paTextToFind) to High(paTextToFind) do
    begin
      if paTextToFind[i] = '' then
        continue;
      nPos := Pos(paTextToFind[i], sl.Text);
      Result := npos > 0;
      if Result then
        exit;
    end;
  finally
    sl.Free;
  end;
end;


function TLocalStorage.GetMessageClient: TMoteMessageClient;
begin
  result := FoMessageClient;
end;

procedure TLocalStorage.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  if Assigned(FoMessageClient) then
  begin
    FoMessageClient.OnMessage := @MessageEvent;
    FoMessageClient.Subscribe('*');

    FoMessageClient.Publish('debug', '{"event": "TLocalStorage.SetMessageClient"}');
  end;
end;


constructor TLocalStorage.Create(const psFilesystemStorageDir: string);
begin
  inherited Create;
  FoMessageClient := nil;
  FsFilesystemStorageDir := psFilesystemStorageDir;
  FslLastQueryResult := TStringList.Create;
end;

destructor TLocalStorage.Destroy;
begin
  FslLastQueryResult.Free;
  inherited Destroy;
end;


end.

