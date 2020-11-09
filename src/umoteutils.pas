unit uMoteUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrtti, TypInfo, fpjson
  {$IFDEF MSWINDOWS}
  , Windows, LazUTF8
  {$ENDIF}
  ;

type
  TJSONHelper = class
  public
    procedure OnStreamProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; var Res: TJSONData);
    procedure OnRestoreProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
  end;


function GetNewId: string;
function GetCurrentUserName: String;
function ConvertObjectToJSONString(const poObject: TObject): string;
procedure ConvertJSONStringObject(const psJSONString: string; const poObject: TObject);

implementation
var
  goJSONHelper: TJSONHelper;

function GetNewId: string;
Var
  G : TGUID;
begin
  CreateGUID(G);
  Result:=GuiDToString(G);
  Result:=Copy(Result,2,36);
end;


procedure TJSONHelper.OnStreamProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; var Res: TJSONData);
var
  sDateTime: string;
begin
  if (Info^.PropType^.Name = 'TDateTime') then
  begin
    sDateTime := FormatDateTime('dd/mm/yyyy hh:mm:ss', TDateTime(GetPropValue(AObject, Info, False)));
    Res.Free;
    Res := TJSONString.Create(sDateTime);
  end;
end;

procedure TJSONHelper.OnRestoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  dtDateTime: TDateTime;
  sDate, sTime: string;
begin
  Handled := False;
  if (Info^.PropType^.Name = 'TDateTime') then
  begin
    Handled := True;
    sDate := Copy(AValue.AsString, 1, 10);
    sTime := Copy(AValue.AsString, 12, 8);
    if sTime = '24:00' then
      dtDateTime := StrToDate(sDate) + 1.0
    else
      dtDateTime := StrToDate(sDate) + StrToTime(sTime);
    SetPropValue(AObject, Info, dtDateTime);
  end;
end;

function ConvertObjectToJSONString(const poObject: TObject): string;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    result := '{}';
    Streamer.OnStreamProperty := @goJSONHelper.OnStreamProperty;
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
    Streamer.OnRestoreProperty := @goJSONHelper.OnRestoreProperty;
    if Assigned(poObject) then
      Streamer.JSONToObject(psJSONString, poObject);
  finally
    Streamer.Free;
  end;
end;



//solutinon published in https://forum.lazarus.freepascal.org/index.php/topic,23171.msg138057.html#msg138057 by Bart
function GetCurrentUserName: String;

{$IFDEF WINDOWS}
const
  MaxLen = 256;
var
  Len: DWORD;
  WS: WideString;
  Res: windows.BOOL;
{$ENDIF}
begin
  Result := '';
  {$IFDEF UNIX}
  {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
  Result := SysToUtf8(GetUserName(fpgetuid));   //GetUsername in unit Users, fpgetuid in unit BaseUnix
  {$ELSE Linux/BSD}
  Result := GetEnvironmentVariableUtf8('USER');
  {$ENDIF UNIX}
  {$ELSE}
  {$IFDEF WINDOWS}
  Len := MaxLen;
  {$IFnDEF WINCE}
  if Win32MajorVersion <= 4 then
  begin
    SetLength(Result,MaxLen);
    Res := Windows.GetuserName(@Result[1], Len);
    //writeln('GetUserNameA = ',Res);
    if Res then
    begin
      SetLength(Result,Len-1);
      Result := SysToUtf8(Result);
    end
    else SetLength(Result,0);
  end
  else
  {$ENDIF NOT WINCE}
  begin
    SetLength(WS, MaxLen-1);
    Res := Windows.GetUserNameW(@WS[1], Len);
    //writeln('GetUserNameW = ',Res);
    if Res then
    begin
      SetLength(WS, Len - 1);
      Result := Utf16ToUtf8(WS);
    end
    else SetLength(Result,0);
  end;
  {$ENDIF WINDOWS}
  {$ENDIF UNIX}
end;

initialization
  goJSONHelper := TJSONHelper.Create;
end.

