unit uMoteUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrtti;

function GetNewId: string;
function ConvertObjectToJSONString(const poObject: TObject): string;
procedure ConvertJSONStringObject(const psJSONString: string; const poObject: TObject);

implementation

function GetNewId: string;
Var
  G : TGUID;
begin
  CreateGUID(G);
  Result:=GuiDToString(G);
  Result:=Copy(Result,2,36);
end;


function ConvertObjectToJSONString(const poObject: TObject): string;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    result := '{}';
    if Assigned(poObject) then
      result := Streamer.ObjectToJSONString(poObject);
  finally
    Streamer.Free;
  end;
end;

procedure ConvertJSONStringObject(const psJSONString: string; const poObject: TObject);
var
  Streamer: TJSONDeStreamer;
begin
  Streamer := TJSONDeStreamer.Create(nil);
  try
    if Assigned(poObject) then
      Streamer.JSONToObject(psJSONString, poObject);
  finally
    Streamer.Free;
  end;
end;


end.

