unit uEventDto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEventDto }

  TEventDto = class(TPersistent)
  private
    FsEvent: string;
    FsMessage: string;
    FsPayload: string;
  published
    property event: string read FsEvent write FsEvent;
    property message: string read FsMessage write FsMessage;
    property payload: string read FsPayload write FsPayload;

    constructor CreateExceptionMessage(const psMessage: string);
    constructor CreateEventObject(const psEvent: string; const poPayloadObject: TObject);
    function ToString: string; override;
    constructor CreateFromJSON(const psJSONString: string);
  end;

implementation

uses
  uMoteUtils;

{ TEventDto }

constructor TEventDto.CreateExceptionMessage(const psMessage: string);
begin
  FsEvent := 'exception';
  FsMessage := psMessage;
  FsPayload := '';
  inherited Create;
end;

constructor TEventDto.CreateEventObject(const psEvent: string;
  const poPayloadObject: TObject);
begin
  FsEvent := psEvent;
  FsMessage := '';
  FsPayload := ConvertObjectToJSONString(poPayloadObject);
  inherited Create;
end;

function TEventDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TEventDto.CreateFromJSON(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, Self);
end;


end.

