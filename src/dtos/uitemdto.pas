unit uItemDto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TItemDto }

  TItemDto = class(TPersistent)
  private
    FsId: string;
    FsDescription: string;
    FsExternalToolItem: string;
    FsTime: string;
    FsTitle: string;
    FbWorking: boolean;
    FsTimeIntervals: string;
    FsParentItemId: string;
    FsUserName: string;
    FsCreatedBy: string;
  published
    property Id: string read FsId write FsId;
    property ExternalToolItem: string read FsExternalToolItem write FsExternalToolItem;
    property ParentItemId: string read FsParentItemId write FsParentItemId;
    property Title: string read FsTitle write FsTitle;
    property Time: string read FsTime write FsTime;
    property Description: string read FsDescription write FsDescription;
    property Working: boolean read FbWorking write FbWorking;
    property TimeIntervals: string read FsTimeIntervals write FsTimeIntervals;
    property UserName: string read FsUserName write FsUserName;
    property CreatedBy: string read FsCreatedBy write FsCreatedBy;

    function ToString: string; override;
    constructor Create; overload;
    constructor Create(const psJSONString: string); overload;
    constructor CreateFromJSON(const psJSONString: string);
    destructor Destroy; override;
  end;

implementation
uses
  uMoteUtils;

{ TItemDto }

function TItemDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TItemDto.Create;
begin
  inherited Create;
end;

constructor TItemDto.Create(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, self);
end;

constructor TItemDto.CreateFromJSON(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, Self);
end;

destructor TItemDto.Destroy;
begin
  inherited Destroy;
end;

end.

