program mote;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, sysUtils, main, uMoteMessage, uUserCodesEditor,
  uItemFrameBase, uTimeEngine, uMoteUtils, uLocalStorage, uEventDto, uItemDto,
  uWeek, uItemFrameWeekCompact;

{$R *.res}
var
  mcgui, mcte, mcls, mcapp, mcweek: TMoteMessageClient;
  te: TTimeEngine;
  ls: TLocalStorage;
  mb: TMoteMessageBus;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmWeek, frmWeek);

  te := TTimeEngine.Create;
  ls := TLocalStorage.Create(ExtractFilePath(ParamStr(0))+'localstorage'+PathDelim+'items'+PathDelim);

  //Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  mcapp := TMoteMessageClient.Create('app', 1024);
  mcgui := TMoteMessageClient.Create('gui', 1024);
  mcte := TMoteMessageClient.Create('time_engine', 1024);
  mcls := TMoteMessageClient.Create('local_storage', 1024);
  mcweek := TMoteMessageClient.Create('gui_week', 1024);

  mb := TMoteMessageBus.Create;
  try
    mb.Add(mcapp);
    mb.Add(mcgui);
    mb.Add(mcte);
    mb.Add(mcls);
    mb.Add(mcweek);


    mcapp.Subscribe('*');

    te.MessageClient:=mcte;
    ls.MessageClient:=mcls;

    frmMain.MessageClient := mcgui;
    mcgui.Subscribe('*');

    frmWeek.MessageClient := mcweek;

    mcapp.Publish('app', '{"event": "create"}');


    Application.Run;
  finally
    frmMain.MessageClient := nil;
    ls.Free;
    te.Free;

    mcweek.Free;
    mcls.Free;
    mcte.Free;
    mcgui.Free;
    mcapp.Free;
    mb.Free;
  end;
end.

