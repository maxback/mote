unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, StdCtrls, uItemFrameBase, uMoteMessage;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAddItem: TAction;
    ActionList: TActionList;
    ControlBar1: TControlBar;
    ImageList: TImageList;
    ListBoxMessages: TListBox;
    MainMenu: TMainMenu;
    miUsertCodesEditor: TMenuItem;
    miTools: TMenuItem;
    miAddItem: TMenuItem;
    miItem: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    ScrollBoxMain: TScrollBox;
    cpbtnAddItem: TSpeedButton;
    Splitter1: TSplitter;
    procedure acAddItemExecute(Sender: TObject);
    procedure ControlBar1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miUsertCodesEditorClick(Sender: TObject);
    procedure MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
    function ItemFrameBaseEvent(Sender: TObject; const psEvent: string; const poParam: TObject; var poResponse: TObject): boolean;
  private
    FnContador: integer;
    FoMessageClient: TMoteMessageClient;
    FoLastFrameInserted: TItemFrameBase;

    function GetMessageClient: TMoteMessageClient;
    procedure SetMessageClient(AValue: TMoteMessageClient);
    function GetNewId: string;
  public
    property MessageClient: TMoteMessageClient read GetMessageClient write SetMessageClient;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  uUserCodesEditor;

{ TfrmMain }

procedure TfrmMain.miUsertCodesEditorClick(Sender: TObject);
begin
  Application.CreateForm(TfrmUserCodesEditor, frmUserCodesEditor);
  frmUserCodesEditor.Show;
end;

procedure TfrmMain.MessageEvent(Sender: Tobject; const poMessage: TMoteMessage);
begin
  ListBoxMessages.Items.Add(FormatDateTime('hh:mm:ss.nnn', Now) + ' ' + poMessage.ToString);
end;

function TfrmMain.ItemFrameBaseEvent(Sender: TObject; const psEvent: string;
  const poParam: TObject; var poResponse: TObject): boolean;
begin
  if psEvent = 'item_interrupt' then
  begin
    FoLastFrameInserted := nil;
    acAddItemExecute(Sender);

    poResponse := FoLastFrameInserted;
    result := (poResponse <>  nil);
    if result then
    begin
      FoLastFrameInserted.ParentItem := Sender as TItemFrameBase;
      FoLastFrameInserted.Update;
      FoMessageClient.Publish('item',
        Format('{"event": "%s", "value": "%s"}', [psEvent, (Sender As TItemFrameBase).Id]));
    end;
    exit;
  end;

  FoMessageClient.Publish('item',
    Format('{"event": "%s", "value": "%s"}', [psEvent, (Sender As TItemFrameBase).Id]));
end;

function TfrmMain.GetMessageClient: TMoteMessageClient;
begin
  Result := FoMessageClient;
end;

procedure TfrmMain.SetMessageClient(AValue: TMoteMessageClient);
begin
  FoMessageClient := AValue;
  //event
  if Assigned(FoMessageClient) then
    FoMessageClient.OnMessage := @MessageEvent;
end;

function TfrmMain.GetNewId: string;
Var
  G : TGUID;
begin
  CreateGUID(G);
  Result:=GuiDToString(G);
  Result:=Copy(Result,2,36);
end;


procedure TfrmMain.acAddItemExecute(Sender: TObject);
var
  s: string;
begin
  if InputQuery('Novo item','Informe o Título', s) then
  begin
    FnContador := FnContador + 1;

    FoLastFrameInserted := TItemFrameBase.Create(ScrollBoxMain);
    FoLastFrameInserted.Name := 'Item_' + FnContador.ToString();
    FoLastFrameInserted.Id := GetNewId; //'B9488A15-4F11-4DEA-8C91-9037D5FF954D';
    FoLastFrameInserted.Parent := ScrollBoxMain;
    FoLastFrameInserted.Title:=s;
    FoLastFrameInserted.OnEvent:=@ItemFrameBaseEvent;

    if Assigned(FoMessageClient) then
    begin

      s := Format('{"id": "%s", "title": "%s", "description": "%s"}', [
        FoLastFrameInserted.Id, FoLastFrameInserted.Title, FoLastFrameInserted.Description]);
      FoMessageClient.Publish('gui',
        Format('{"event": "create_item", "value": %s}', [s]));
    end;
  end;
end;

procedure TfrmMain.ControlBar1Click(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Caption := 'show';
end;

end.

