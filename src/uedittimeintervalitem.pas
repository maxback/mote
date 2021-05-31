unit uEditTimeIntervalItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TfrmEditTimeIntervalItem }

  TfrmEditTimeIntervalItem = class(TForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    edtDate: TEdit;
    edtIniTime: TEdit;
    edtEndTime: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmComment: TMemo;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    function SetFocusOnMinuts(const poEdit: TEdit): boolean;

  public
    class function Execute(const psCaption, psPrompt: string; var psValue: string): boolean;
  end;

var
  frmEditTimeIntervalItem: TfrmEditTimeIntervalItem;

implementation

{$R *.lfm}

{ TfrmEditTimeIntervalItem }


function TfrmEditTimeIntervalItem.SetFocusOnMinuts(const poEdit: TEdit): boolean;
begin
  if (poEdit.Text <> '') and poEdit.CanFocus then
  begin
    poEdit.SetFocus;
    poEdit.SelStart := Length(poEdit.Text) - 2;
    poEdit.SelLength := 2;
    exit(true);
  end;
  exit(false);
end;

procedure TfrmEditTimeIntervalItem.FormShow(Sender: TObject);
begin
  if not SetFocusOnMinuts(edtEndTime) then
    SetFocusOnMinuts(edtIniTime);
end;

class function TfrmEditTimeIntervalItem.Execute(const psCaption,
  psPrompt: string; var psValue: string): boolean;
begin
  //
  Application.CreateForm(TfrmEditTimeIntervalItem, frmEditTimeIntervalItem);
  frmEditTimeIntervalItem.Caption := psCaption;
  frmEditTimeIntervalItem.edtDate.Text := Copy(psValue, 1, 10);
  frmEditTimeIntervalItem.edtIniTime.Text := Copy(psValue, 12, 5);
  frmEditTimeIntervalItem.edtEndTime.Text := Copy(psValue, 20, 5);
  frmEditTimeIntervalItem.mmComment.Lines.Text := StringReplace(Copy(psValue, 26, Length(psValue)), '\r\n', #13#10,  [rfReplaceAll]);

  frmEditTimeIntervalItem.ShowModal;

  result := frmEditTimeIntervalItem.ModalResult = mrOK;
  if result then
  begin
    psValue := Format('%-10s ', [Formatdatetime('dd/mm/yyyy', StrToDate(frmEditTimeIntervalItem.edtDate.Text))]);
    psValue := psValue + Format('%-5s', [FormatDateTime('hh:mm', StrToTime(frmEditTimeIntervalItem.edtIniTime.Text))]);
    if frmEditTimeIntervalItem.edtEndTime.Text <> '' then
      psValue := psValue + Format(' - %-5s', [FormatDateTime('hh:mm', StrToTime(frmEditTimeIntervalItem.edtEndTime.Text))]);
    if frmEditTimeIntervalItem.mmComment.Lines.Text <> '' then
    psValue := psValue + ' ' +   StringReplace(frmEditTimeIntervalItem.mmComment.Lines.Text, #13#10, '\r\n',  [rfReplaceAll]);
  end;

end;

end.

