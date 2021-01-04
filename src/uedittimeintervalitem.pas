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
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private

  public
    class function Execute(const psCaption, psPrompt: string; var psValue: string): boolean;
  end;

var
  frmEditTimeIntervalItem: TfrmEditTimeIntervalItem;

implementation

{$R *.lfm}

{ TfrmEditTimeIntervalItem }

procedure TfrmEditTimeIntervalItem.FormShow(Sender: TObject);
begin
  if (edtEndTime.Text <> '') and edtEndTime.CanFocus then
    edtEndTime.SetFocus
  else if (edtIniTime.Text <> '') and edtIniTime.CanFocus then
    edtIniTime.SetFocus;
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

  frmEditTimeIntervalItem.ShowModal;

  result := frmEditTimeIntervalItem.ModalResult = mrOK;
  if result then
  begin
    psValue := Format('%-10s ', [Formatdatetime('dd/mm/yyyy', StrToDate(frmEditTimeIntervalItem.edtDate.Text))]);
    psValue := psValue + Format('%-5s', [FormatDateTime('hh:mm', StrToTime(frmEditTimeIntervalItem.edtIniTime.Text))]);
    if frmEditTimeIntervalItem.edtEndTime.Text <> '' then
      psValue := psValue + Format(' - %-5s', [FormatDateTime('hh:mm', StrToTime(frmEditTimeIntervalItem.edtEndTime.Text))]);
  end;
end;

end.

