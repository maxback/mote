unit uSettingPoromodoroItemDto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSettingItemBase;
type

  { TSettingPoromodoroItemDto }

  TSettingPoromodoroItemDto = class(TSettingItemBaseDto)
  private
    FnShortBreakMinutes: integer;
    FnWorkintervalMinutes: integer;
    FsAlarmFileName: string;
  published
    property workintervalMinutes: integer read FnWorkintervalMinutes write FnWorkintervalMinutes;
    property shortBreakMinutes: integer read FnShortBreakMinutes write FnShortBreakMinutes;
    property alarmFileName: string read FsAlarmFileName write FsAlarmFileName;
  end;

implementation

end.

