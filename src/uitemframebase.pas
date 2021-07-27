unit uItemFrameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Grids, ComCtrls,
  Dialogs, ExtCtrls, uItemDto, LCLIntf, Clipbrd, Menus, Graphics;

{$J+}
const
  nMAX_TIMES_SIZE_FRAME: integer = 5;
{$J-}

type

  { TItemFrameCustomControls }

  TItemFrameCustomControls = class
  public
    ControlClassName: string;
    Properties: TStringList;
    UserCodeEventHandle: string;
    constructor CreateItem(const psControlClassName: string;
      pslProperties: TStringList; const psUserCodeEventHandle: string);
    destructor Destroy; override;
  end;

  { TItemFrameBase }

  TItemFrameBaseEvent = function(Sender: TObject; const psEvent: string;
    const poParam: TObject; var poResponse: TObject): boolean of object;

  TItemFrameBaseEventParamObject = class
    StringValue: string;
  end;

  TItemFrameBase = class(TFrame)
    btnChangeSizeLess: TBitBtn;
    btnEditDescription: TBitBtn;
    btnNewItemwithSelectTimeInterval: TBitBtn;
    btnDeleteTimeInterval: TBitBtn;
    btnDuplicateTimeInterval: TBitBtn;
    edtEditTimeInterval: TBitBtn;
    edtOptions: TBitBtn;
    btnEditTitle: TBitBtn;
    btnEditExternalToolItem: TBitBtn;
    btnStop: TBitBtn;
    btnInitWork: TBitBtn;
    btnInitUnexpectedWork: TBitBtn;
    edtExternalToolItem: TEdit;
    btnChangeSizePluss: TBitBtn;
    edtPutOkOnItemInterval: TBitBtn;
    edtTitle: TEdit;
    edtEstimatedTime: TEdit;
    gb: TGroupBox;
    lbTimeElapsed: TLabel;
    lblTime: TLabel;
    lblDescription: TLabel;
    lblExternalToolItem: TLabel;
    lblTitle: TLabel;
    lbTime: TListBox;
    micCreateTextFileAndInsertLocalFileRef: TMenuItem;
    miEditDescription: TMenuItem;
    miInsertLocalFileRef: TMenuItem;
    mmDescription: TMemo;
    mmTimeOfLabel: TMemo;
    OpenDialog: TOpenDialog;
    pnTimeButton: TPanel;
    PopupMenuDescOptions: TPopupMenu;
    pbTimeElapsed: TProgressBar;
    SaveDialog: TSaveDialog;
    Shape: TShape;
    procedure btnChangeSizeLessClick(Sender: TObject);
    procedure btnChangeSizePlussClick(Sender: TObject);
    procedure btnDuplicateTimeIntervalClick(Sender: TObject);
    procedure btnEditDescExternalClick(Sender: TObject);
    procedure btnEditDescriptionClick(Sender: TObject);
    procedure btnNewItemwithSelectTimeIntervalClick(Sender: TObject);
    procedure edtExternalToolItemDblClick(Sender: TObject);
    procedure edtPutOkOnItemIntervalClick(Sender: TObject);
    procedure edtEstimatedTimeExit(Sender: TObject);
    procedure gbClick(Sender: TObject);
    procedure GenericOnEnter(Sender: TObject);
    procedure lbDescriptionDbClick(Sender: TObject);
    procedure btnEditDescriptionExit(Sender: TObject);
    procedure btnEditExternalToolItemClick(Sender: TObject);
    procedure btnEditTitleClick(Sender: TObject);
    procedure btnInitUnexpectedWorkClick(Sender: TObject);
    procedure btnInitWorkClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnDeleteTimeIntervalClick(Sender: TObject);
    procedure edtEditTimeIntervalClick(Sender: TObject);
    procedure edtExternalToolItemExit(Sender: TObject);
    procedure edtOptionsClick(Sender: TObject);
    procedure edtTitleEditingDone(Sender: TObject);
    procedure edtTitleExit(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure lblTimeDblClick(Sender: TObject);
    procedure lbTimeClick(Sender: TObject);
    procedure lbTimeDblClick(Sender: TObject);
    procedure lbTimeExit(Sender: TObject);
    procedure micCreateTextFileAndInsertLocalFileRefClick(Sender: TObject);
    procedure miInsertLocalFileRefClick(Sender: TObject);
    procedure mmDescriptionDblClick(Sender: TObject);
    procedure mmDescriptionExit(Sender: TObject);
    procedure mmDescriptionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mmTimeOfLabelExit(Sender: TObject);
    procedure pbTimeElapsedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TratarMouseSobreTextos(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
  private
    FnOriginalHeight: integer;
    FoItem: TItemDto;
    FslCustomControls: TList;
    FoParentItem: TItemFrameBase;
    FoItemThatInterruptThis: TItemFrameBase;
    FOnvent: TItemFrameBaseEvent;
    FbCopyLimeToClipboardBeforeOpenLink: boolean;
    function GetDescription: string;
    function GetEstimatedTime: string;
    function GetExternalToolItem: string;
    function GetId: string;
    function GetParentItem: TItemFrameBase;
    function GetTime: string;
    function GetTimeIntervals: TStrings;
    function GetTitle: string;
    function GetWorking: boolean;
    function GetUserName: string;
    function GetCreatedBy: string;
    procedure SetDescription(AValue: string);
    procedure SetEstimatedTime(AValue: string);
    procedure SetExternalToolItem(AValue: string);
    procedure SetId(AValue: string);
    procedure SetOnvent(AValue: TItemFrameBaseEvent);
    procedure SetParentItem(AValue: TItemFrameBase);
    procedure SetTime(AValue: string);
    procedure SetTimeIntervals(AValue: TStrings);
    procedure SetTitle(AValue: string);
    procedure SetWorking(AValue: boolean);
    procedure SetUserName(AValue: string);
    procedure SetCreatedBy(AValue: string);
    function GetCopyLimeToClipboardBeforeOpenLink: boolean;
    procedure SetCopyLimeToClipboardBeforeOpenLink(const value: boolean);
  protected
    FoEventResponseObject: TObject;
    function InterruptWork: boolean; virtual;
    function DoOnvent(const psEvent: string; const poParam: TObject): boolean; virtual;
    function TestarCampoTotalmenteVisivel(Sender: TObject): boolean;
    procedure OpenURLFromDescription; virtual;
    function GetLastTimeTextItem(const psDelimmiterBegin, psDelimmiterEnd: char): string; virtual;
    function GetTodayTimeTextItem(const psDelimmiterBegin, psDelimmiterEnd: char): string; virtual;
  public
    property Id: string read GetId write SetId;
    property ExternalToolItem: string read GetExternalToolItem write SetExternalToolItem;
    property ParentItem: TItemFrameBase read GetParentItem write SetParentItem;
    property Title: string read GetTitle write SetTitle;
    property Time: string read GetTime write SetTime;
    property Description: string read GetDescription write SetDescription;
    property Working: boolean read GetWorking write SetWorking;
    property TimeIntervals: TStrings read GetTimeIntervals write SetTimeIntervals;
    property OnEvent: TItemFrameBaseEvent read FOnvent write SetOnvent;
    property UserName: string read GetUserName write SetUserName;
    property CreatedBy: string read GetCreatedBy write SetCreatedBy;
    property CopyLimeToClipboardBeforeOpenLink: boolean read GetCopyLimeToClipboardBeforeOpenLink write SetCopyLimeToClipboardBeforeOpenLink;
    property EstimatedTime: string read GetEstimatedTime write SetEstimatedTime;


    property Item: TItemDto read FoItem write FoItem;

    procedure UpdateTimeElapsed;
    function ToString: string; override;
    procedure Update(const pbHandleInternal: boolean; const pbForceHasUpdated: boolean = false); virtual;
    procedure UpdateFromJSON(const psJSONItem: string); virtual;
    procedure AddControl(const psControlClassName: string;
      pslProperties: TStringList; const psUserCodeEventHandle: string);
    procedure RefreshTime;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  uMoteUtils, uEditTimeIntervalItem;

{$R *.lfm}

{ TItemFrameCustomControls }

constructor TItemFrameCustomControls.CreateItem(const psControlClassName: string;
  pslProperties: TStringList; const psUserCodeEventHandle: string);
begin
  ControlClassName := PsControlClassName;

  Properties := TStringList.Create;
  Properties.AddStrings(pslProperties);
  UserCodeEventHandle := psUserCodeEventHandle;

end;

destructor TItemFrameCustomControls.Destroy;
begin
  properties.Free;
  inherited Destroy;
end;

{ TItemFrameBase }

procedure TItemFrameBase.btnEditTitleClick(Sender: TObject);
begin
  GenericOnEnter(Sender);
  edtTitle.Text := FoItem.Title;
  lblTitle.Visible := False;
  edtTitle.Visible := True;
  edtTitle.SetFocus();
end;

procedure TItemFrameBase.btnEditExternalToolItemClick(Sender: TObject);
begin
  edtExternalToolItem.Text := FoItem.ExternalToolItem;
  lblExternalToolItem.Visible := False;
  edtExternalToolItem.Visible := True;
  edtExternalToolItem.SetFocus();
end;

procedure TItemFrameBase.lbDescriptionDbClick(Sender: TObject);
begin
  mmDescription.Lines.Text := FoItem.Description;
  mmDescription.Visible := True;
  mmDescription.SetFocus;
end;

procedure TItemFrameBase.btnEditDescriptionClick(Sender: TObject);
begin
  if mmDescription.Visible then
  begin
    FoItem.Description:=mmDescription.Lines.Text;
    Update(true);
    mmDescription.Visible := False;
  end;

  //add new option, db click no label open local memo, in button send a message to gui open a dialog
  if not DoOnvent('item_memo_edition', mmDescription) then
    //but if not return true, this default editor will be opend
    lbDescriptionDbClick(Sender);
end;

procedure TItemFrameBase.btnNewItemwithSelectTimeIntervalClick(Sender: TObject);
var
  sl: TStringList;
  po: TItemFrameBaseEventParamObject;
begin
  sl := TStringList.Create;
  po := TItemFrameBaseEventParamObject.Create;
  try
    sl.Text := FoItem.TimeIntervals;
    if lbTime.ItemIndex >= 0 then
      po.StringValue := sl[lbTime.ItemIndex]
    else
      po.StringValue := FoItem.TimeIntervals;
    if DoOnvent('item_new_from_timeinterval', po) and (lbTime.ItemIndex >= 0) then
    begin
      sl.Delete(lbTime.ItemIndex);
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
    end;
  finally
    po.Free;
    sl.Free;
  end;
end;

procedure TItemFrameBase.edtExternalToolItemDblClick(Sender: TObject);
begin
  if FbCopyLimeToClipboardBeforeOpenLink then
  begin
    //Clipboard.AsText := GetLastTimeTextItem(' ', ' ');
    Clipboard.AsText := GetLastTimeTextItem('(', ')');
    Clipboard.AsText := GetTodayTimeTextItem('(', ')');
  end;

  OpenURL(edtExternalToolItem.Text);
end;

procedure TItemFrameBase.edtPutOkOnItemIntervalClick(Sender: TObject);
var
  sl: TStringList;
  s: string;
begin
  if lbTime.ItemIndex >= 0 then
  begin
    sl := TStringList.Create;
    try
      sl.Text := FoItem.TimeIntervals;
      s := sl[lbTime.ItemIndex];
      if length(s) < 24 then
        exit;

      if Pos(' Ok', s) > 24 then
        s := StringReplace(s, ' Ok', '', [])
      else
        s := s + ' Ok';
      sl[lbTime.ItemIndex] := s;
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
    finally
      sl.Free;
    end;
  end;
end;

procedure TItemFrameBase.edtEstimatedTimeExit(Sender: TObject);
begin
  edtEstimatedTime.Visible := False;
  FoItem.EstimatedTime := edtEstimatedTime.Text;
  Update(true, true);
end;

procedure TItemFrameBase.gbClick(Sender: TObject);
begin

end;

procedure TItemFrameBase.GenericOnEnter(Sender: TObject);
begin
  pnTimeButton.Visible := false;
end;

procedure TItemFrameBase.btnEditDescExternalClick(Sender: TObject);
begin

end;

procedure TItemFrameBase.btnChangeSizePlussClick(Sender: TObject);
begin
  if Height > (FnOriginalHeight * nMAX_TIMES_SIZE_FRAME) then
    exit;
  if Height < FnOriginalHeight then
    Height := FnOriginalHeight
  else
   Height := Height + FnOriginalHeight;
lblDescription.ShowHint := Height = FnOriginalHeight;
end;

procedure TItemFrameBase.btnDuplicateTimeIntervalClick(Sender: TObject);
var
  sl: TStringList;
  ii: integer;
begin
  if lbTime.ItemIndex >= 0 then
  begin
    ii := lbTime.ItemIndex;
    sl := TStringList.Create;
    try
      sl.Text := FoItem.TimeIntervals;
      sl.Insert(ii + 1, sl[ii]);
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
      lbTime.ItemIndex := ii;
    finally
      sl.Free;
    end;
  end;
end;

procedure TItemFrameBase.btnChangeSizeLessClick(Sender: TObject);
begin
  if Height <= (FnOriginalHeight div 2) then
    exit;
  if Height <= FnOriginalHeight then
    Height := FnOriginalHeight div 2
  else
    Height := Height - FnOriginalHeight;
  lblDescription.ShowHint := Height = FnOriginalHeight;
end;

procedure TItemFrameBase.btnEditDescriptionExit(Sender: TObject);
begin
  mmDescription.Visible := False;
  FoItem.Description := mmDescription.Lines.Text;
  Update(true);
end;

procedure TItemFrameBase.btnInitUnexpectedWorkClick(Sender: TObject);
begin
  if InterruptWork then
  begin
    FoItem.Working := False;
    Update(true);
  end;
end;

procedure TItemFrameBase.btnInitWorkClick(Sender: TObject);
begin
  FoItem.Working := True;
  DoOnvent('item_init_work', nil);
  Update(true);
end;

procedure TItemFrameBase.btnStopClick(Sender: TObject);
begin
  FoItem.Working := False;
  DoOnvent('item_stop', nil);
  Update(true);
end;

procedure TItemFrameBase.btnDeleteTimeIntervalClick(Sender: TObject);
var
  sl: TStringList;
begin
  if lbTime.ItemIndex >= 0 then
  begin
    sl := TStringList.Create;
    try
      sl.Text := FoItem.TimeIntervals;
      sl.Delete(lbTime.ItemIndex);
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
    finally
      sl.Free;
    end;
  end;
end;

procedure TItemFrameBase.edtEditTimeIntervalClick(Sender: TObject);
var
  sl: TStringList;
  slToday: TStringList;
  s: string;
begin
  if lbTime.ItemIndex >= 0 then
  begin
    sl := TStringList.Create;
    slToday := TStringList.Create;
    try
      sl.Text := FoItem.TimeIntervals;
      slToday.Text := FoItem.TimeIntervals;
      mmTimeOfLabel.Lines.Text := lblTime.Caption;

      s := sl[lbTime.ItemIndex];

      if not TfrmEditTimeIntervalItem.Execute('Editar intervalo', 'Respeite o formato dd/mm/yyyy hh:mm - hh:mm, senão...!',
        mmTimeOfLabel.Lines.Text + '------' + #13#10 + slToday.Text, FoItem.TimeIntervals, s) then
        exit;
      sl[lbTime.ItemIndex] := s;
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
    finally
      slToday.Free;
      sl.Free;
    end;
  end;
end;


procedure TItemFrameBase.edtExternalToolItemExit(Sender: TObject);
begin
  lblExternalToolItem.Visible := True;
  edtExternalToolItem.Visible := False;
  FoItem.ExternalToolItem := edtExternalToolItem.Text;
  Update(true);
end;

procedure TItemFrameBase.edtOptionsClick(Sender: TObject);
begin
  DoOnvent('item_option', Sender);
end;

procedure TItemFrameBase.edtTitleEditingDone(Sender: TObject);
begin
  FoItem.Title := edtTitle.Text;
  Update(true);
end;

procedure TItemFrameBase.edtTitleExit(Sender: TObject);
begin
  lblTitle.Visible := True;
  edtTitle.Visible := False;
  FoItem.Title:=edtTitle.Text;
  Update(true);
end;

procedure TItemFrameBase.FrameExit(Sender: TObject);
begin
  pnTimeButton.Visible := false;
end;

procedure TItemFrameBase.lblTimeDblClick(Sender: TObject);
begin
  mmTimeOfLabel.Lines.Text := lblTime.Caption;
  mmTimeOfLabel.Left := lblTime.Left;
  mmTimeOfLabel.Visible := true;
  mmTimeOfLabel.SetFocus;
end;

procedure TItemFrameBase.lbTimeClick(Sender: TObject);
begin
  GenericOnEnter(Sender);
  //start edit mode to time inervals
  pnTimeButton.Visible := true;
end;

procedure TItemFrameBase.lbTimeDblClick(Sender: TObject);
begin
  edtEditTimeIntervalClick(Sender);
end;

procedure TItemFrameBase.lbTimeExit(Sender: TObject);
begin
  mmTimeOfLabel.Visible := false;
end;

procedure TItemFrameBase.micCreateTextFileAndInsertLocalFileRefClick(
  Sender: TObject);
var
  s: string;
  sl: TStringList;
begin
  SaveDialog.FileName := edtTitle.text + ' - extra text file ' + FormatDateTime('yyyy-mm-dd--hh-mm-ss', now) + '.txt';
  if not SaveDialog.Execute then
    exit;
  sl := TStringList.Create;
  try
    s := 'Text file create from item ' + edtTitle.text + #13#10 +
      'Clipbroard: ' + Clipboard.AsText;
    sl.TExt := s;
    sl.SaveToFile(SaveDialog.FileName);
  finally
    sl.free;
  end;
  s := #13#10 + 'file://' + SaveDialog.FileName + #13#10;
  //bad, but today we have ctrl+c ad ctrl+v :-)
  Clipboard.AsText := s;
  mmDescription.PasteFromClipboard;
  OpenURL(s);
end;

procedure TItemFrameBase.miInsertLocalFileRefClick(Sender: TObject);
var
  s: string;
begin
  if not opendialog.Execute then
    exit;

  s := #13#10 + 'file://' + opendialog.FileName + #13#10;
  //bad, but today we have ctrl+c ad ctrl+v :-)
  Clipboard.AsText := s;
  mmDescription.PasteFromClipboard;

end;


function TItemFrameBase.GetTodayTimeTextItem(const psDelimmiterBegin, psDelimmiterEnd: char): string;
var
  nPos1, nPos2: integer;
  sToday: string;
begin
  nPos1 := Pos('Today', lblTime.Caption);
  sToday := Copy(lblTime.Caption, nPos1, Length(lblTime.Caption) - nPos1 + 1);
  nPos2 := Pos(')', sToday);
  sToday := Copy(sToday, 1, nPos2 + 1);
  result := GetTimeTextItem(sToday, psDelimmiterBegin, psDelimmiterEnd);
end;

procedure TItemFrameBase.UpdateTimeElapsed;
var
  nPercent, nElapsedTime, nEstimatedTime: Double;
  nPosition: integer;
  s: string;
begin
  lbTimeElapsed.Color:=clNone;
  lbTimeElapsed.Font.Color:= clBlack;
  lbTimeElapsed.Caption := EmptyStr;
  lbTimeElapsed.Hint := EmptyStr;
  try
    nEstimatedTime := 0;
    if edtEstimatedTime.Text <> EmptyStr then
      nEstimatedTime := StrToFloat(edtEstimatedTime.Text);

    nElapsedTime := 0;
    //s := GetTimeTextItem(Copy(lblTime.Caption, 1, 15), '(', ')');
    s := GetTimeTextItem(lblTime.Caption, '(', ')');
    if s <> EmptyStr then
      nElapsedTime := StrToFloat(s);

    nPosition := Trunc(nElapsedTime * 1000);
    pbTimeElapsed.Max:=Trunc(nEstimatedTime * 1000);
    pbTimeElapsed.Min:=0;
    if nEstimatedTime <= 0 then
    begin
      pbTimeElapsed.Position:= 0;
      pbTimeElapsed.Hint:= 'Estimated time not found';
      exit;
    end;
    if nPosition >= pbTimeElapsed.Max then
    begin
      lbTimeElapsed.Font.Color := clRed;
      nPosition := pbTimeElapsed.Max;
    end;
    if nPosition >= ((pbTimeElapsed.Max div 100) * 75)  then
      lbTimeElapsed.Font.Color := clYellow
    else
      lbTimeElapsed.Font.Color := clAqua;

    lbTimeElapsed.Color := clBlack;
    pbTimeElapsed.Position:= nPosition;

    nPercent := 0;
    if nEstimatedTime > 0 then
      nPercent := (nEstimatedTime / 100.0) * nElapsedTime;

    pbTimeElapsed.Hint:= Format('nElapsed %0.3f of %s (estimated)', [nElapsedTime, edtEstimatedTime.Text]);

    lbTimeElapsed.Caption := Format(' %0.2f%%  (%0.3f / %0.3f)', [nPercent * 100, nElapsedTime, nEstimatedTime]);
    lbTimeElapsed.Hint := lbTimeElapsed.Caption;

  except
    pbTimeElapsed.Max:=1;
    pbTimeElapsed.Min:=0;
    pbTimeElapsed.Position:=0;
    pbTimeElapsed.Hint:= 'Error uppindanting progress';

  end;
end;


function TItemFrameBase.GetLastTimeTextItem(const psDelimmiterBegin, psDelimmiterEnd: char): string;
begin
  result := GetTimeTextItem(lblTime.Caption, psDelimmiterBegin, psDelimmiterEnd);
end;



procedure TItemFrameBase.OpenURLFromDescription;
const
  Protocols : array[0..2] of string = ('http://', 'https://', 'file://');
var
  n, n2, i1, i2: integer;
  s: string;
  i: integer;
begin
  if FbCopyLimeToClipboardBeforeOpenLink then
  begin
    //Clipboard.AsText := GetLastTimeTextItem(' ', ' ');
    Clipboard.AsText := GetLastTimeTextItem('(', ')');
  end;
  //obtain a link in corrient item
  n2 := 0;
  i1 := mmDescription.SelStart;
  for n := 0 to mmDescription.Lines.Count-1 do
  begin
    n2 := n2 + Length(mmDescription.Lines.Strings[n]) + 2; ///r/n
    if n2 >= i1 then
    begin
      for i := low(Protocols) to high(Protocols) do
      begin
        i2 := Pos(Protocols[i], mmDescription.Lines.Strings[n]);
        if i2 < 1 then
          continue;

        s := Copy(mmDescription.Lines.Strings[n], i2, Length(mmDescription.Lines.Strings[n]));
        OpenURL(s);
        exit;
      end;

    end;
  end;

  s := mmDescription.SelText;
  if Pos('http://', s) = 1 then
    OpenURL(s);
  if Pos('file://', s) = 1 then
    OpenURL(s);
end;

procedure TItemFrameBase.mmDescriptionDblClick(Sender: TObject);
begin
  OpenURLFromDescription;
end;

procedure TItemFrameBase.mmDescriptionExit(Sender: TObject);
begin
  mmDescription.Visible := False;
  FoItem.Description:=mmDescription.Lines.Text;
  Update(true);
end;

procedure TItemFrameBase.mmDescriptionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and (mmDescription.SelLength > 0) then
    OpenURLFromDescription;

  if (Key = 83) and (ssCtrl in Shift) then
  begin
    FoItem.Description:=mmDescription.Lines.Text;
    Update(true);
  end;
end;

procedure TItemFrameBase.mmTimeOfLabelExit(Sender: TObject);
begin
  mmTimeOfLabel.visible := false;
end;

procedure TItemFrameBase.pbTimeElapsedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  edtEstimatedTime.Visible := true;
  edtEstimatedTime.SetFocus;
end;

function TItemFrameBase.TestarCampoTotalmenteVisivel(Sender: TObject): boolean;
begin
  if not (Sender is TControl) then
    exit(false);
  result := Height > ((Sender as TControl).Top + (Sender as TControl).Height);

end;

procedure TItemFrameBase.TratarMouseSobreTextos(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  //de acordo com o texto, msotra botão associado
  btnEditTitle.Visible := (Sender = lblTitle) and TestarCampoTotalmenteVisivel(Sender);
  btnEditExternalToolItem.Visible := (Sender = lblExternalToolItem) and TestarCampoTotalmenteVisivel(Sender);
  btnEditDescription.Visible := (Sender = lblDescription) and TestarCampoTotalmenteVisivel(Sender);
end;

procedure TItemFrameBase.SetTitle(AValue: string);
begin
  if FoItem.Title = AValue then
    Exit;
  FoItem.Title := AValue;
  Update(true);
end;

procedure TItemFrameBase.SetUserName(AValue: string);
begin
  FoItem.UserName:=AValue;
end;

procedure TItemFrameBase.SetWorking(AValue: boolean);
begin
  if FoItem.Working = AValue then
    Exit;
  FoItem.Working := AValue;
  Update(true);
end;

function TItemFrameBase.InterruptWork: boolean;
begin
  Result := False;
  if DoOnvent('item_interrupt', nil) and Assigned(FoEventResponseObject) and
    (FoEventResponseObject is TItemFrameBase) then
  begin
    FoItemThatInterruptThis := FoEventResponseObject as TItemFrameBase;
    Result := True;
  end;

end;

function TItemFrameBase.DoOnvent(const psEvent: string; const poParam: TObject): boolean;
begin
  if FOnvent = nil then
    exit(False);

  Result := FOnvent(Self, psEvent, poParam, FoEventResponseObject);
end;


procedure TItemFrameBase.SetTime(AValue: string);
begin
  if FoItem.Time = AValue then
    Exit;
  FoItem.Time := AValue;
  Update(true);
end;

procedure TItemFrameBase.SetTimeIntervals(AValue: TStrings);
begin
  lbTime.Items.Text := AValue.Text;
end;

procedure TItemFrameBase.SetDescription(AValue: string);
begin
  if FoItem.Description = AValue then
    Exit;
  FoItem.Description := AValue;
  Update(true);
end;

procedure TItemFrameBase.SetEstimatedTime(AValue: string);
begin
  if FoItem.EstimatedTime = AValue then
    Exit;
  FoItem.EstimatedTime := AValue;
  Update(true);

end;

function TItemFrameBase.GetTimeIntervals: TStrings;
begin
  Result := lbTime.Items;
end;

function TItemFrameBase.GetDescription: string;
begin
  result := FoItem.Description;
end;

function TItemFrameBase.GetEstimatedTime: string;
begin
  result := FoItem.EstimatedTime;
end;

function TItemFrameBase.GetCreatedBy: string;
begin
  result := FoItem.CreatedBy;
end;

function TItemFrameBase.GetExternalToolItem: string;
begin
  result := FoItem.ExternalToolItem;
end;

function TItemFrameBase.GetId: string;
begin
  result := FoItem.Id;
end;

function TItemFrameBase.GetParentItem: TItemFrameBase;
begin
  result := FoParentItem;
end;

function TItemFrameBase.GetUserName: string;
begin
  result := FoItem.UserName;
end;

function TItemFrameBase.GetTime: string;
begin
  result := FoItem.Time;
end;

function TItemFrameBase.GetTitle: string;
begin
  result := FoItem.Title;
end;

function TItemFrameBase.GetWorking: boolean;
begin
  result := FoItem.Working;
end;

procedure TItemFrameBase.SetCreatedBy(AValue: string);
begin
  FoItem.CreatedBy := AValue;
end;

function TItemFrameBase.GetCopyLimeToClipboardBeforeOpenLink: boolean;
begin
  result := FbCopyLimeToClipboardBeforeOpenLink;
end;

procedure TItemFrameBase.SetCopyLimeToClipboardBeforeOpenLink(
  const value: boolean);
begin
  FbCopyLimeToClipboardBeforeOpenLink := value;

end;

procedure TItemFrameBase.SetExternalToolItem(AValue: string);
begin
  if FoItem.ExternalToolItem = AValue then
    Exit;
  FoItem.ExternalToolItem := AValue;
  Update(true);
end;

procedure TItemFrameBase.SetId(AValue: string);
begin
  if FoItem.Id = AValue then
    Exit;
  FoItem.Id := AValue;
  Update(true);
end;

procedure TItemFrameBase.SetOnvent(AValue: TItemFrameBaseEvent);
begin
  if FOnvent = AValue then
    Exit;
  FOnvent := AValue;
end;

procedure TItemFrameBase.SetParentItem(AValue: TItemFrameBase);
begin
  if FoParentItem = AValue then
    Exit;
  FoParentItem := AValue;
  if Assigned(FoParentItem) then
    FoItem.Id := FoParentItem.Id;
  Update(true);
end;

procedure TItemFrameBase.Update(const pbHandleInternal: boolean; const pbForceHasUpdated: boolean);
var
  bUpdated: boolean;
begin
  bUpdated := (lblTitle.Caption <> FoItem.Title);
  lblTitle.Caption := FoItem.Title;

  bUpdated := bUpdated or (edtEstimatedTime.Text <> FoItem.EstimatedTime);
  edtEstimatedTime.Text := FoItem.EstimatedTime;

  UpdateTimeElapsed;

  bUpdated := bUpdated or (lblTitle.Caption <> FoItem.Title);
  lblTitle.Caption := FoItem.Title;
  bUpdated := bUpdated or (edtTitle.Text <> FoItem.Title);
  edtTitle.Text := FoItem.Title;

  bUpdated := bUpdated or (lblTime.Caption <> FoItem.Time);
  lblTime.Caption := FoItem.Time;
  if pos(#13, lblTime.Caption) < 1 then
  begin
    lblTime.Font.Size:=10;
    //lblTime.Caption := ' ' + #13#10 + lblTime.Caption;
  end
  else
    lblTime.Font.Size:=8;
  lblTime.Hint := lblTime.Caption;

  bUpdated := bUpdated or (lbTime.Items.Text <> FoItem.TimeIntervals);
  lbTime.Items.Text := FoItem.TimeIntervals;

  bUpdated := bUpdated or (lblDescription.Caption <> FoItem.Description);
  lblDescription.Caption := FoItem.Description;
  lblDescription.Hint := FoItem.Description;

  bUpdated := bUpdated or (lblExternalToolItem.Caption <> FoItem.ExternalToolItem);
  lblExternalToolItem.Caption := FoItem.ExternalToolItem;
  edtExternalToolItem.Caption := FoItem.ExternalToolItem;

  btnInitWork.Visible := not FoItem.Working;
  btnInitUnexpectedWork.Visible := FoItem.Working;
  btnStop.Visible := FoItem.Working;
  gb.Caption := ' ';

  if pbHandleInternal and (bUpdated or pbForceHasUpdated) then
  begin
    DoOnvent('item_update', nil);
  end;


  gb.Color := $00EBEBEB; //white + 1
  if Working then
  begin
    gb.Color := $00D0FBD6; //green
    if Pos('CountDown:', FoItem.Time) > 0 then
    begin
      gb.Color := $00BEF9FA; //welow
    end;

  end;

  if FoParentItem <> nil then
  begin
    gb.Caption := 'Filho de  ' + FoParentItem.title + '. ';
    if Working then
      gb.Color := $001A7B68; //DARK green
  end;
  if FoItemThatInterruptThis <> nil then
  begin
    gb.Caption := 'Interrompido por ' + FoItemThatInterruptThis.Title;
    if not Working then
      gb.Color := $004254F9; //red
  end;
end;

procedure TItemFrameBase.UpdateFromJSON(const psJSONItem: string);
begin
  ConvertJSONStringObject(psJSONItem, FoItem);
  Update(false);
  UpdateTimeElapsed;
end;

procedure TItemFrameBase.AddControl(const psControlClassName: string;
  pslProperties: TStringList; const psUserCodeEventHandle: string);
begin

end;

procedure TItemFrameBase.RefreshTime;
begin
  DoOnvent('item_refresh_query', nil);
end;

constructor TItemFrameBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FbCopyLimeToClipboardBeforeOpenLink := true;
  FnOriginalHeight:=Height;
  FslCustomControls := TList.Create;
  FoItem := TItemDto.Create;
  Update(true);
end;

destructor TItemFrameBase.Destroy;
var
  i: integer;
begin
  FoItem.Free;
  for i := 0 to FslCustomControls.Count - 1 do
  begin
    TObject(FslCustomControls.Items[i]).Free;
  end;

  inherited Destroy;
end;

function TItemFrameBase.ToString: string;
var
  s: string;
  w: string;
begin
  result := FoItem.ToString;
end;


end.
