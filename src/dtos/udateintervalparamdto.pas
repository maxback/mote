unit uDateIntervalParamDto;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  { TDateIntervalDto }

  TDateIntervalDto = class(TPersistent)
  private
    FdtStartDate: TDateTime;
    FdtEndDate: TDateTime;
  published
    property StartDate: TDateTime read FdtStartDate write FdtStartDate;
    property EndDate: TDateTime read FdtEndDate write FdtEndDate;

    constructor CreateInterval(const pdtStartDate, pdtEndDate: TDateTime);
    function ToString: string; override;
    constructor CreateFromJSON(const psJSONString: string);
  end;

implementation

uses
  uMoteUtils;

{ TDateIntervalDto }


constructor TDateIntervalDto.CreateInterval(const pdtStartDate,
  pdtEndDate: TDateTime);
begin
  FdtStartDate := pdtStartDate;
  FdtEndDate := pdtEndDate;
  inherited Create;
end;

function TDateIntervalDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TDateIntervalDto.CreateFromJSON(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, Self);
end;


end.
