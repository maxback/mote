unit uEditTimeIntervalItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Clipbrd;

type

  { TfrmEditTimeIntervalItem }

  TfrmEditTimeIntervalItem = class(TForm)
    btnCancel: TBitBtn;
    btnCopyTotalOfDayAndClose: TBitBtn;
    btnOk: TBitBtn;
    edtDate: TEdit;
    edtIniTime: TEdit;
    edtEndTime: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblTotalOfDay: TLabel;
    lbTimeIntervals: TListBox;
    mmSelectedDaySumary: TMemo;
    mmComment: TMemo;
    Panel1: TPanel;
    pnlTop: TPanel;
    procedure btnCopyTotalOfDayAndCloseClick(Sender: TObject);
    procedure edtDateChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbTimeIntervalsSelectionChange(Sender: TObject; User: boolean);
  private
    FsDay: string;
    function SetFocusOnMinuts(const poEdit: TEdit): boolean;
    function FilterSelectedDay(const s: string): boolean;

  public
    class function Execute(const psCaption, psPrompt: string; const psTotalOfDay, psTimeIntervals: string; var psValue: string): boolean;
  end;

var
  frmEditTimeIntervalItem: TfrmEditTimeIntervalItem;

implementation

{$R *.lfm}

{ TfrmEditTimeIntervalItem }

uses
  uMoteUtils;

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

function TfrmEditTimeIntervalItem.FilterSelectedDay(const s: string): boolean;
begin
  result := Pos(FsDay, s) > 0;
end;


procedure TfrmEditTimeIntervalItem.lbTimeIntervalsSelectionChange(Sender: TObject;
  User: boolean);
var
  sl: TStringList;
  s: string;
begin
  FsDay := Copy(lbTimeIntervals.Items.Strings[lbTimeIntervals.ItemIndex], 1, 10);

  sl := TStringList.Create;
  try
     lbTimeIntervals.Items.Filter(@FilterSelectedDay, sl);
     mmSelectedDaySumary.Lines.Clear;

     s := GenerateTotalTimeTextFromStringList(sl, true);

     mmSelectedDaySumary.Lines.Add('Date:');
     mmSelectedDaySumary.Lines.Add(FsDay);
     mmSelectedDaySumary.Lines.Add('Sum:');
     mmSelectedDaySumary.Lines.Add(s);

  finally
    sl.Free;
  end;
end;

procedure TfrmEditTimeIntervalItem.btnCopyTotalOfDayAndCloseClick(
  Sender: TObject);
var
  s: string;
begin
  if mmSelectedDaySumary.Lines.Count  < 4 then
    exit;

  s := mmSelectedDaySumary.Lines.Strings[3];
  Clipboard.AsText := s;
  ModalResult := mrcancel;
  Close;
end;

procedure TfrmEditTimeIntervalItem.edtDateChange(Sender: TObject);
var
  i: integer;
begin
  lbTimeIntervals.ItemIndex := -1;
  for i := 0 to lbTimeIntervals.items.Count-1 do
      if Pos(edtDate.Text, lbTimeIntervals.items.Strings[i]) = 1 then
      begin
        lbTimeIntervals.ItemIndex := i;
        break;
      end;
end;

class function TfrmEditTimeIntervalItem.Execute(const psCaption,
  psPrompt: string; const psTotalOfDay, psTimeIntervals: string; var psValue: string): boolean;
begin
  //
  Application.CreateForm(TfrmEditTimeIntervalItem, frmEditTimeIntervalItem);
  frmEditTimeIntervalItem.Caption := psCaption;
  frmEditTimeIntervalItem.lbTimeIntervals.Items.Text := psTimeIntervals;
  frmEditTimeIntervalItem.lblTotalOfDay.Caption := psTotalOfDay;
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

