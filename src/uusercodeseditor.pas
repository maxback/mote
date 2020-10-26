unit uUserCodesEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, SynHighlighterPas, SynEdit, SynHighlighterVB;

type

  { TfrmUserCodesEditor }

  TfrmUserCodesEditor = class(TForm)
    btnSave: TBitBtn;
    cbCode: TComboBox;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
  private
    FsDirBase: string;
  public

  end;

var
  frmUserCodesEditor: TfrmUserCodesEditor;

implementation

{$R *.lfm}

{ TfrmUserCodesEditor }

procedure TfrmUserCodesEditor.FormCreate(Sender: TObject);
begin
  //FsDirBase := ExtractfilePath(
  //psTemplates
end;

end.

