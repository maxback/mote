unit message;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  /// Class of generic massage (basade in queue schemas, like MQTT)

  { TMoteMessage }

  TMoteMessage = class(TObject)
  private
    FsOrign: AnsiString;
    FsTopic: AnsiString;
    FsPayload: AnsiString;
  public
    property Orign: AnsiString read FsOrign;
    property Topic: AnsiString read FsTopic;
    property Payload: AnsiString read FsPayload;

    constructor Create(const psOrign: AnsiString; const psTopic: AnsiString; const psPayload: AnsiString);
  end;




  TMoteMessageClientMessageEvent = procedure(Sender: Tobject; const poMessage: TMoteMessage) of object;

  TMoteMessageList = specialize TFPGObjectList<TMoteMessage>;
 
  { TMoteMessageClient }

  TMoteMessageClient = class(TObject)
  private
    FsOrign: AnsiString;
    FoIncommingQueue: TMoteMessageList;
    FnMaxIncommingQueueSize: integer;
    FoOnMessage: TMoteMessageClientMessageEvent;
  public
    property IncommingQueue: TMoteMessageList read FoIncommingQueue;
    property OnMessage: TMoteMessageClientMessageEvent read FoOnMessage write FoOnMessage;

    //to bus test if a especifc topic will by add on current object
    function TestTopicSuported(const psTopic: AnsiString): boolean;

    //this means that the topc suported will return true in TestTopicSuported()
    procedure Subscribe(const psTopic: AnsiString);
    //this means that the toct suported will return true in TestTopicSuported()
    procedure uNSubscribe(const psTopic: AnsiString);
    function getMessage: TMoteMessage;
    procedure Publish(const psTopic: AnsiString; const psPayload: AnsiString); overload;
    procedure Publish(const poMessage: TMoteMessage); overload;

    constructor Create(const Orign: AnsiString; const pnMaxIncommingQueueSize: integer);

  end;

 TMoteMessageClientList = specialize TFPGObjectList<TMoteMessageClient>;
 
  { TMoteMessageBus }

  TMoteMessageBus = class(TObject)
  private
    FoMessageClientList: TMoteMessageClientList;
  public
    procedure Add(const poClient: TMoteMessageClient);
    procedure remove(const poClient: TMoteMessageClient);
    procedure clear;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMoteMessageBus }

procedure TMoteMessageBus.Add(const poClient: TMoteMessageClient);
begin

end;

procedure TMoteMessageBus.remove(const poClient: TMoteMessageClient);
begin

end;

procedure TMoteMessageBus.clear;
begin

end;

constructor TMoteMessageBus.Create;
begin

end;

destructor TMoteMessageBus.Destroy;
begin
  inherited Destroy;
end;

{ TMoteMessage }

constructor TMoteMessage.Create(const psOrign: AnsiString;
  const psTopic: AnsiString; const psPayload: AnsiString);
begin

end;

{ TMoteMessageClient }

function TMoteMessageClient.TestTopicSuported(const psTopic: AnsiString
  ): boolean;
begin

end;

procedure TMoteMessageClient.Subscribe(const psTopic: AnsiString);
begin

end;

procedure TMoteMessageClient.uNSubscribe(const psTopic: AnsiString);
begin

end;

function TMoteMessageClient.getMessage: TMoteMessage;
begin

end;

procedure TMoteMessageClient.Publish(const psTopic: AnsiString;
  const psPayload: AnsiString);
begin

end;

procedure TMoteMessageClient.Publish(const poMessage: TMoteMessage);
begin

end;

constructor TMoteMessageClient.Create(const Orign: AnsiString;
  const pnMaxIncommingQueueSize: integer);
begin

end;

end.

