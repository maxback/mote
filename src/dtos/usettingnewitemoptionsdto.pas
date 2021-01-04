unit uSettingNewItemOptionsDto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSettingNewItemOptionsItemDto }

  TSettingNewItemOptionsItemDto = class (TCollectionItem)
  private
    FsDescription: string;
    FsExternalToolItem: string;
    FsTitle: string;
    FsTimeIntervals: string;
  public
    function ToString: string; override;
    constructor CreateFromJSON(const psJSONString: string);
  published
    property title: string read FsTitle write FsTitle;
    property description: string read FsDescription write FsDescription;
    property externalToolItem: string read FsExternalToolItem write FsExternalToolItem;
    property timeIntervals: string read FsTimeIntervals write FsTimeIntervals;
  end;

  { TSettingNewItemOptionsItemDto }

  { TSettingNewItemOptionsDto }

  TSettingNewItemOptionsDto = class(TPersistent)
   private
     FItems: TCollection;
   public
     constructor Create;
     destructor Destroy; override;
     function ToString: string; override;
     constructor Create(const psJSONString: string); overload;
     constructor CreateFromJSON(const psJSONString: string);
   published
     property items:  TCollection read FItems;
   end;

implementation

uses
  uMoteUtils;

{ TSettingNewItemOptionsItemDto }

function TSettingNewItemOptionsItemDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TSettingNewItemOptionsItemDto.CreateFromJSON(
  const psJSONString: string);
begin
  inherited Create(nil);
  ConvertJSONStringObject(psJSONString, self);

end;

{ TSettingNewItemOptionsDto }

constructor TSettingNewItemOptionsDto.Create;
begin
  inherited Create;
  FItems := TCollection.Create(TSettingNewItemOptionsItemDto);
end;

destructor TSettingNewItemOptionsDto.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TSettingNewItemOptionsDto.ToString: string;
begin
  Result := ConvertObjectToJSONString(self);
end;

constructor TSettingNewItemOptionsDto.Create(const psJSONString: string);
begin
  inherited Create;
  FItems := TCollection.Create(TSettingNewItemOptionsItemDto);
  ConvertJSONStringObject(psJSONString, self);
end;

constructor TSettingNewItemOptionsDto.CreateFromJSON(const psJSONString: string
  );
begin
  inherited Create;
  FItems := TCollection.Create(TSettingNewItemOptionsItemDto);
  ConvertJSONStringObject(psJSONString, self);
end;


end.

