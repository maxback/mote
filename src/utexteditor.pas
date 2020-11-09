unit uTextEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ActnList, uItemDto, uItemFrameBase;

type

  { TfrmTextEditorDialog }

  TfrmTextEditorDialog = class(TForm)
    acConfirmEdition: TAction;
    ActionList: TActionList;
    mmEditor: TMemo;
    PanelActions: TPanel;
    SpeedButton1: TSpeedButton;
    procedure acConfirmEditionExecute(Sender: TObject);
  private

  public
    FoLastFrameEventHandle: TItemFrameBase;
  end;

var
  frmTextEditorDialog: TfrmTextEditorDialog;

implementation

{$R *.lfm}

{ TfrmTextEditorDialog }

procedure TfrmTextEditorDialog.acConfirmEditionExecute(Sender: TObject);
var
  oItem: TItemDto;
begin
  oItem := FoLastFrameEventHandle.Item;
  oItem.Description := mmEditor.Lines.Text;
  FoLastFrameEventHandle.Update(true);
  visible := false;
end;

end.

