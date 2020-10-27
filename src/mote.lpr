program mote;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, uMoteMessage, uUserCodesEditor, uItemFrameBase
  { you can add units after this };

{$R *.res}
var
  mcgui: TMoteMessageClient;
  mcapp: TMoteMessageClient;
  mb: TMoteMessageBus;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  mcapp := TMoteMessageClient.Create('app', 1024);
  mcgui := TMoteMessageClient.Create('gui', 1024);
  mb := TMoteMessageBus.Create;
  try
    mb.Add(mcapp);
    mb.Add(mcgui);
    mcapp.Subscribe('*');
    mcgui.Subscribe('*');
    frmMain.MessageClient := mcgui;
    mcapp.Publish('global', '{"event": "create"}');
    Application.Run;
  finally
    frmMain.MessageClient := nil;
    mcgui.Free;
    mcapp.Free;
    mb.Free;
  end;
end.

