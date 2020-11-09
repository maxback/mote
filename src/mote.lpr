program mote;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysUtils, main, uMoteMessage, uUserCodesEditor, uItemFrameBase,
  uTimeEngine, uMoteUtils, uLocalStorage, uEventDto, uItemDto, uWeek,
  uItemFrameWeekCompact, uDateIntervalParamDto, uPomodoro, uSettingItemBase,
  uSettingPoromodoroItemDto, uTextEditor, usettingitemuridto, uSettingNewItemOptionsDto;

{$R *.res}
var
  mcgui, mcte, mclsi, mclss, mcapp, mcweek, mcp: TMoteMessageClient;
  te: TTimeEngine;
  lsi: TLocalStorage;
  lss: TLocalStorage;
  p: TMotePomodoro;
  mb: TMoteMessageBus;
  e: TEventDto;
begin
  RequireDerivedFormResource:=True;
  //Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmWeek, frmWeek);

  te := TTimeEngine.Create;
  lsi := TLocalStorage.Create(ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'items'+PathDelim, 'item', lskData);
  lsi.BackUpDir:= ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'items'+PathDelim+'bkp'+PathDelim;

  lss := TLocalStorage.Create(ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'settings'+PathDelim, 'setting', lskSettings);

  p := TMotePomodoro.Create;

  //Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  mcapp := TMoteMessageClient.Create('app', 1024);
  mcgui := TMoteMessageClient.Create('gui', 1024);
  mcte := TMoteMessageClient.Create('time_engine', 1024);
  mclsi := TMoteMessageClient.Create('local_storage', 1024);
  mclss := TMoteMessageClient.Create('local_storage_settings', 1024);
  mcweek := TMoteMessageClient.Create('gui_week', 1024);
  mcp := TMoteMessageClient.Create('pomodoro_local', 10);

  mb := TMoteMessageBus.Create;

  e := TEventDto.Create;
  try
    mb.Add(mcapp);
    mb.Add(mcgui);
    mb.Add(mcte);
    mb.Add(mclss);
    mb.Add(mclsi);
    mb.Add(mcweek);
    //mb.Add(mcp); //i will refactory the message event x bus to because i found a problem


    mcapp.Subscribe('*');

    te.MessageClient:=mcte;
    lsi.MessageClient:=mclsi;
    lss.MessageClient:=mclss;
    p.MessageClient:=mcp;

    frmMain.MessageClient := mcgui;
    mcgui.Subscribe('*');

    frmWeek.MessageClient := mcweek;

    e.event:= 'create';
    mcapp.Publish('app', e.ToString);
  Application.CreateForm(TfrmTextEditorDialog, frmTextEditorDialog);
    Application.Run;
  finally
    frmMain.MessageClient := nil;

    e.Free;
    p.Free;
    lss.Free;
    lsi.Free;
    te.Free;

    mcweek.Free;
    mclss.Free;
    mclsi.Free;
    mcte.Free;
    mcgui.Free;
    mcapp.Free;
    mb.Free;
  end;
end.

