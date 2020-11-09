unit uSettingItemBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

{ TSettingItemBaseDto }

TSettingItemBaseDto = class(TPersistent)
private
  FbActive: boolean;
  FsKind: string;
  FsName: string;
published
  property kind: string read FsKind write FsKind;
  property name: string read FsName write FsName;
  property active: boolean read FbActive write FbActive;

  function ToString: string; override;
  constructor Create; overload;
  constructor Create(const psJSONString: string); overload;
  constructor CreateFromJSON(const psJSONString: string);
  destructor Destroy; override;
  procedure CopySettingTo(const poDest: TSettingItemBaseDto);
end;

implementation

uses
  uMoteUtils;

{ TSettingItemBaseDto }

function TSettingItemBaseDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TSettingItemBaseDto.Create;
begin
  inherited Create;
end;

constructor TSettingItemBaseDto.Create(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, self);
end;

constructor TSettingItemBaseDto.CreateFromJSON(const psJSONString: string);
begin
  inherited Create;
  ConvertJSONStringObject(psJSONString, Self);
end;

destructor TSettingItemBaseDto.Destroy;
begin
  inherited Destroy;
end;

procedure TSettingItemBaseDto.CopySettingTo(const poDest: TSettingItemBaseDto);
begin
  ConvertJSONStringObject(Self.ToString, poDest);
end;

end.

