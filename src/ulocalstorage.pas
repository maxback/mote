unit uLocalStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMoteMessage, Dos, uItemDto, uEventDto;

type

{ TLocalStorage }
TLocalStorageKind = (lskData, lskSettings);
TLocalStorage = class
  private
    FsItemName: string;
    FsFilesystemStorageDir: string;
    FenKind: TLocalStorageKind;
    FslLastQueryResult: TStringList;
    FoMessageClient: TMoteMessageClient;
    FsBackUpDir: string;
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
    procedure SaveBackUp(const psFileName: string);
    procedure QueryByDate(const pdtStartDate, pdtEndDate: TDateTime; const pbIncludeWithoutInterval: boolean = false);
    procedure SendMessagesForLastQueryResult;
    procedure InsertItem(const poItem: TItemDto);
    procedure UpdateItem(const poItem: TItemDto);
    procedure DeleteItem(const poItem: TItemDto);
  public
    property BackUpDir: string read FsBackUpDir write FsBackUpDir;
    property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;
    constructor Create(const psFilesystemStorageDir: string;
      const psItemName: string; const penKind: TLocalStorageKind);
    destructor Destroy; override;
  end;

implementation

uses
  uDateIntervalParamDto, umotelog;

{ TLocalStorage }


procedure TLocalStorage.DefaultMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oContextoLog: IMoteContextoRegistroLog;
  e: TEventDto;
  di: TDateIntervalDto;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.DefaultMessageEvent', EmptyStr, True);
  try
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


procedure TLocalStorage.AppMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oContextoLog: IMoteContextoRegistroLog;
  e: TEventDto;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.AppMessageEvent', EmptyStr, True);
  try
    e := TEventDto.CreateFromJSON(poMessage.Payload);
    try
      if e.event = 'create' then
      begin
        //load all data for today
        if FenKind = lskData then
          QueryByDate(Date, Date, true)
        else
        if FenKind = lskSettings then
          QueryAllItem
        else
        begin
          oContextoLog.GerarLogRetornoMetodo;
          exit;
        end;
        SendMessagesForLastQueryResult;
      end;

    finally
      e.Free;
    end;
  oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TLocalStorage.GenerateItemFileName(const poItem: TItemDto): string;
var
  oContextoLog: IMoteContextoRegistroLog;
  sId: string;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.GenerateItemFileName', EmptyStr, True);
  try
    sId := poItem.Id;
    if Length(sId) < 5 then
      sId := 'invalid_id_' + FormatDatetime('yyyy-mm-dd-hh-mm-ss-nnn', now);
    result := FsFilesystemStorageDir+sId+'.json';
    oContextoLog.GerarLogRetornoMetodo('"' + result + '"');
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.InsertItem(const poItem: TItemDto);
var
  oContextoLog: IMoteContextoRegistroLog;
  sl: TStringList;
  sFileName: string;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.InsertItem', EmptyStr, True);
  try
    sl := TStringList.Create;
    try
      sl.Text := poItem.ToString;
      sFileName := GenerateItemFileName(poItem);
      sl.SaveToFile(sFileName);
    finally
      sl.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.UpdateItem(const poItem: TItemDto);
var
  oContextoLog: IMoteContextoRegistroLog;
  sl: TStringList;
  sFileName: string;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.UpdateItem', EmptyStr, True);
  try
    sl := TStringList.Create;
    try
      sl.Text := poItem.ToString;
      poItem.LastUpdateDateTime:=now;
      sFileName := GenerateItemFileName(poItem);
      SaveBackUp(sFileName);
      sl.SaveToFile(sFileName);
    finally
      sl.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.DeleteItem(const poItem: TItemDto);
var
  oContextoLog: IMoteContextoRegistroLog;
  sFileName: string;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.DeleteItem', EmptyStr, True);
  try
    sFileName := GenerateItemFileName(poItem);
    SaveBackUp(sFileName);
    DeleteFile(sFileName);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.ItemMessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oContextoLog: IMoteContextoRegistroLog;
  oItem: TItemDto;
var
  e: TEventDto;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.ItemMessageEvent', EmptyStr, True);
  try
    e := TEventDto.CreateFromJSON(poMessage.Payload);
    oItem := TItemDto.CreateFromJSON(e.payload);
    try
      //check the action to create or update data
      if e.event = FsItemName + '_create' then
         InsertItem(oItem);
      if e.event = FsItemName + '_update' then
         UpdateItem(oItem);
      if e.event = FsItemName + '_delete' then
         DeleteItem(oItem);
    finally
      oItem.Free;
      e.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.MessageEvent(Sender: Tobject;
  const poMessage: TMoteMessage);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.MessageEvent', EmptyStr, True);
  try
    //if poMessage.Orign = FoMessageClient.Orign then
    //  exit;
    try
    if poMessage.Orign = 'app' then
      AppMessageEvent(Sender, poMessage);
    if poMessage.Topic = FsItemName then
      ItemMessageEvent(Sender, poMessage)
    else
      DefaultMessageEvent(Sender, poMessage)


    except
      on e:exception do
        FoMessageClient.Publish('error', '{"event": "exception", "method": "MessageEvent", "value": "'+E.Message+'"}');
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.QueryAllItem;
var
  oContextoLog: IMoteContextoRegistroLog;
  sr: SearchRec;
  Att: Word;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.QueryAllItem', EmptyStr, True);
  try
    FslLastQueryResult.Clear;
    Att := archive or readonly;

    FindFirst(FsFilesystemStorageDir+'*.*', Att, sr);
    while DosError = 0 do
    begin
      FslLastQueryResult.Add(sr.Name);
      FindNext(sr);
    end;
    FindClose(sr);
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.SaveBackUp(const psFileName: string);
var
  oContextoLog: IMoteContextoRegistroLog;
  sl: TStringList;
  oItem: TItemDto;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.SaveBackUp', EmptyStr, True);
  try
    if BackUpDir = '' then
    begin
      oContextoLog.GerarLogRetornoMetodo;
      exit;
    end;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(psFileName);
      oItem := TItemDto.Create(sl.Text);
      sl.SavetoFile(BackUpDir + oItem.Id + '(' + FormatDateTime('yyyy-mm-dd-hh-mm-ss-nnn', Now) + ').json');
    finally
      oItem.Free;
      sl.free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.QueryByDate(const pdtStartDate, pdtEndDate: TDateTime; const pbIncludeWithoutInterval: boolean);
var
  oContextoLog: IMoteContextoRegistroLog;
  sr: SearchRec;
  Att: Word;
  dtDate: TDateTime;
  sDate: string;
  i: integer;
  sText: string;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.QueryByDate', EmptyStr, True);
  try
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
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

procedure TLocalStorage.SendMessagesForLastQueryResult;
var
  oContextoLog: IMoteContextoRegistroLog;
  i: integer;
  sl: TStringList;
  e: TEventDto;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.SendMessagesForLastQueryResult', EmptyStr, True);
  try
    sl := TStringList.Create;
    e := TEventDto.Create;
    try
      for i:=0 to FslLastQueryResult.Count-1 do
      begin
        sl.LoadFromFile(FsFilesystemStorageDir+FslLastQueryResult[i]);
        e.event := FsItemName + '_restore';
        e.payload:=sl.Text;
        FoMessageClient.Publish(FsItemName, e.ToString);
      end;
    finally
      e.Free;
      sl.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

function TLocalStorage.TestFileContainText(const psFileName: string; const paTextToFind: array of string): boolean;
var
  oContextoLog: IMoteContextoRegistroLog;
  sl: TStringList;
  nPos: SizeInt;
  i: integer;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.TestFileContainText', EmptyStr, True);
  try
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
        begin
          oContextoLog.GerarLogRetornoMetodo;
          exit;
        end;
      end;
    finally
      sl.Free;
    end;
    oContextoLog.GerarLogRetornoMetodo(Format('%d (bool)', [integer(result)]));
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


function TLocalStorage.GetMessageClient: TMoteMessageClient;
begin
  result := FoMessageClient;
end;

procedure TLocalStorage.SetMessageClient(AValue: TMoteMessageClient);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.SetMessageClient', EmptyStr, True);
  try
    FoMessageClient := AValue;
    if Assigned(FoMessageClient) then
    begin
      FoMessageClient.OnMessage := @MessageEvent;
      FoMessageClient.Subscribe('*');

    end;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


constructor TLocalStorage.Create(const psFilesystemStorageDir: string;
  const psItemName: string; const penKind: TLocalStorageKind);
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.Create', EmptyStr, True);
  try
    inherited Create;
    FoMessageClient := nil;
    FsFilesystemStorageDir := psFilesystemStorageDir;
    FsItemName := psItemName;
    FenKind := penKind;
    FslLastQueryResult := TStringList.Create;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;

destructor TLocalStorage.Destroy;
var
  oContextoLog: IMoteContextoRegistroLog;
begin
  //14/04/2021 - maxback -  - Adicionado log especial
  oContextoLog := motelog(ClassName + '.Destroy', EmptyStr, True);
  try
    FslLastQueryResult.Free;
    inherited Destroy;
    oContextoLog.GerarLogRetornoMetodo;
  except
    oContextoLog.GerarLogRetornoMetodoComErro;
    raise;
  end;
end;


end.

