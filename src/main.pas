unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Buttons, ActnList, uItemFrameBase;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acAddItem: TAction;
    ActionList: TActionList;
    ControlBar1: TControlBar;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    miUsertCodesEditor: TMenuItem;
    miTools: TMenuItem;
    miAddItem: TMenuItem;
    miItem: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    ScrollBoxMain: TScrollBox;
    cpbtnAddItem: TSpeedButton;
    procedure acAddItemExecute(Sender: TObject);
    procedure ControlBar1Click(Sender: TObject);
    procedure miAddItemClick(Sender: TObject);
    procedure miUsertCodesEditorClick(Sender: TObject);
  private
    FnContador: integer;
  public

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

procedure TfrmMain.miAddItemClick(Sender: TObject);
begin

end;

procedure TfrmMain.acAddItemExecute(Sender: TObject);
var
  Frame: TItemFrameBase;
  s: string;
begin
  if InputQuery('Novo item','Informe o Título', s) then
  begin
    FnContador := FnContador + 1;

    Frame := TItemFrameBase.Create(ScrollBoxMain);
    Frame.Name := 'Item_' + FnContador.ToString();
    Frame.Parent := ScrollBoxMain;
    Frame.Title:=s;

  end;
end;

procedure TfrmMain.ControlBar1Click(Sender: TObject);
begin

end;

end.

