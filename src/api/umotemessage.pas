unit uMoteMessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  /// Class of generic massage (basade in queue schemas, like MQTT)

  { TMoteMessage }

  TMoteMessageError = Exception;

  TMoteMessage = class(TPersistent)
  private
    FsOrign: AnsiString;
    FsTopic: AnsiString;
    FsPayload: AnsiString;
  published
    property Orign: AnsiString read FsOrign;
    property Topic: AnsiString read FsTopic;
    property Payload: AnsiString read FsPayload;

    constructor Create(const psOrign: AnsiString; const psTopic: AnsiString; const psPayload: AnsiString); overload;
    constructor CreateCopy(const poMessage: TMoteMessage); overload;

    function ToString: string; override;

  end;




  TMoteMessageClientMessageEvent = procedure(Sender: Tobject; const poMessage: TMoteMessage) of object;

  TMoteMessageList = specialize TFPGObjectList<TMoteMessage>;
 
  { TMoteMessageClient }

  TMoteMessageBus = class;

  TMoteMessageClient = class(TObject)
  private
    FsOrign: AnsiString;
    FoIncommingQueue: TMoteMessageList;
    FnMaxIncommingQueueSize: integer;
    FoOnMessage: TMoteMessageClientMessageEvent;
    FoOnPublish: TMoteMessageClientMessageEvent;
    FoMessageBus: TMoteMessageBus;
    FslSuportedTopics: TStringList;
  public
    property MessageBus: TMoteMessageBus read FoMessageBus write FoMessageBus;
    property IncommingQueue: TMoteMessageList read FoIncommingQueue;
    property OnMessage: TMoteMessageClientMessageEvent read FoOnMessage write FoOnMessage;
    property OnPublish: TMoteMessageClientMessageEvent read FoOnPublish write FoOnPublish;
    property Orign: AnsiString read FsOrign;

    //to bus test if a especifc topic will by add on current object
    function TestTopicSuported(const psTopic: AnsiString): boolean;

    //this means that the topc suported will return true in TestTopicSuported()
    procedure Subscribe(const psTopic: AnsiString);
    //this means that the toct suported will return true in TestTopicSuported()
    procedure UnSubscribe(const psTopic: AnsiString);
    function getMessage: TMoteMessage;
    procedure Publish(const psTopic: AnsiString; const psPayload: AnsiString); overload;
    procedure Publish(const poMessage: TMoteMessage); overload;

    procedure Receive(const poMessage: TMoteMessage);

    constructor Create(const psOrign: AnsiString; const pnMaxIncommingQueueSize: integer);
    destructor Destroy; override;

  end;

 TMoteMessageClientList = specialize TFPGObjectList<TMoteMessageClient>;
 
  { TMoteMessageBus }

  TMoteMessageBus = class(TObject)
  private
    FoMessageClientList: TMoteMessageClientList;
  public
    procedure Add(const poClient: TMoteMessageClient);
    procedure remove(const poClient: TMoteMessageClient);
    procedure Publish(const poMessage: TMoteMessage);
    procedure clear;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  uMoteUtils;

{ TMoteMessageBus }

procedure TMoteMessageBus.Add(const poClient: TMoteMessageClient);
begin
  FoMessageClientList.Add(poClient);
  poClient.MessageBus := self;
//  poClient.
end;

procedure TMoteMessageBus.remove(const poClient: TMoteMessageClient);
begin
  poClient.MessageBus := nil;
  FoMessageClientList.Remove(poClient);
end;

procedure TMoteMessageBus.Publish(const poMessage: TMoteMessage);
var
  i: integer;
  oMessage: TMoteMessage;
begin
  //make a copy to each client that suports it
  for i := 0 to FoMessageClientList.Count-1 do
  begin
    try
      if FoMessageClientList.Items[i].Orign = poMessage.Orign then
        continue;
      if FoMessageClientList.Items[i].TestTopicSuported(poMessage.Topic) then
      begin
        oMessage := TMoteMessage.CreateCopy(poMessage);
        FoMessageClientList.Items[i].Receive(oMessage);
      end;
    except
    end;
  end;

end;

procedure TMoteMessageBus.clear;
var
  i: integer;
begin
  for i := 0 to FoMessageClientList.Count-1 do
  begin
    FoMessageClientList.Items[i].MessageBus := nil;
  end;

  FoMessageClientList.Clear;

end;

constructor TMoteMessageBus.Create;
begin
  FoMessageClientList := TMoteMessageClientList.Create(false);
end;

destructor TMoteMessageBus.Destroy;
begin
  FoMessageClientList.Free;
  inherited Destroy;
end;

{ TMoteMessage }

constructor TMoteMessage.Create(const psOrign: AnsiString;
  const psTopic: AnsiString; const psPayload: AnsiString);
begin
  inherited Create;
  FsOrign:=psOrign;
  FsPayload:=psPayload;
  FsTopic:=psTopic;
end;

constructor TMoteMessage.CreateCopy(const poMessage: TMoteMessage);
begin
  Create(poMessage.Orign, poMessage.Topic, poMessage.Payload);
end;

function TMoteMessage.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

{ TMoteMessageClient }

function TMoteMessageClient.TestTopicSuported(const psTopic: AnsiString
  ): boolean;
begin
  result := (FslSuportedTopics.IndexOf('*') >= 0) or
    (FslSuportedTopics.IndexOf(psTopic) >= 0);

end;

procedure TMoteMessageClient.Subscribe(const psTopic: AnsiString);
begin
  FslSuportedTopics.Add(psTopic);
end;

procedure TMoteMessageClient.uNSubscribe(const psTopic: AnsiString);
var
  i: integer;
begin
  i := FslSuportedTopics.IndexOf(psTopic);
  if i >= 0 then
    FslSuportedTopics.Delete(i);
end;

function TMoteMessageClient.getMessage: TMoteMessage;
begin

end;

procedure TMoteMessageClient.Publish(const psTopic: AnsiString;
  const psPayload: AnsiString);
var
  oMessage: TMoteMessage;
begin
  oMessage := TMoteMessage.Create(FsOrign, psTopic, psPayload);
  try
    Publish(oMessage);
  finally
    oMessage.Free;
  end;
end;

procedure TMoteMessageClient.Publish(const poMessage: TMoteMessage);
begin
  if not Assigned(FoMessageBus) then
    TMoteMessageError.CreateFmt('Não é possível publicar a mensagem do tópico "%s" pois o cliente está desconectado do barramento. Payload: "%s".',
      [poMessage.Topic, poMessage.Payload]);
  if Assigned(FoOnPublish) then
    FoOnPublish(Self, poMessage);
  FoMessageBus.Publish(poMessage);
end;

procedure TMoteMessageClient.Receive(const poMessage: TMoteMessage);
begin
  if FoIncommingQueue.Count > FnMaxIncommingQueueSize then
    TMoteMessageError.Create('Não é possível publicar a mensagem pois o cliente está com sua fila cheia.');

  FoIncommingQueue.Add(poMessage);
  if FoOnMessage = nil then
    exit;

  FoOnMessage(Self, poMessage);
  FoIncommingQueue.Remove(poMessage);
end;

constructor TMoteMessageClient.Create(const psOrign: AnsiString;
  const pnMaxIncommingQueueSize: integer);
begin
  FsOrign := psOrign;
  FoIncommingQueue := TMoteMessageList.Create(false);
  FnMaxIncommingQueueSize := pnMaxIncommingQueueSize;
  FslSuportedTopics := TStringList.Create;
end;

destructor TMoteMessageClient.Destroy;
begin
  FoIncommingQueue.Free;
  FslSuportedTopics.Free;
  inherited Destroy;
end;

end.

