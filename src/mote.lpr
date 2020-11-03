program mote;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysUtils, main, uMoteMessage, uUserCodesEditor,
  uItemFrameBase, uTimeEngine, uMoteUtils, uLocalStorage, uEventDto, uItemDto,
  uWeek, uItemFrameWeekCompact, uDateIntervalParamDto;

{$R *.res}
var
  mcgui, mcte, mclsi, mclss, mcapp, mcweek: TMoteMessageClient;
  te: TTimeEngine;
  lsi: TLocalStorage;
  lss: TLocalStorage;
  mb: TMoteMessageBus;

begin
  RequireDerivedFormResource:=True;
  //Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmWeek, frmWeek);

  te := TTimeEngine.Create;
  lsi := TLocalStorage.Create(ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'items'+PathDelim, 'item', lskData);
  lss := TLocalStorage.Create(ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'settings'+PathDelim, 'setting', lskSettings);

  //Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  mcapp := TMoteMessageClient.Create('app', 1024);
  mcgui := TMoteMessageClient.Create('gui', 1024);
  mcte := TMoteMessageClient.Create('time_engine', 1024);
  mclsi := TMoteMessageClient.Create('local_storage', 1024);
  mclss := TMoteMessageClient.Create('local_storage_settings', 1024);
  mcweek := TMoteMessageClient.Create('gui_week', 1024);

  mb := TMoteMessageBus.Create;
  try
    mb.Add(mcapp);
    mb.Add(mcgui);
    mb.Add(mcte);
    mb.Add(mclss);
    mb.Add(mclsi);
    mb.Add(mcweek);


    mcapp.Subscribe('*');

    te.MessageClient:=mcte;
    lsi.MessageClient:=mclsi;
    lss.MessageClient:=mclss;

    frmMain.MessageClient := mcgui;
    mcgui.Subscribe('*');

    frmWeek.MessageClient := mcweek;

    mcapp.Publish('app', '{"event": "create"}');


    Application.Run;
  finally
    frmMain.MessageClient := nil;
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

