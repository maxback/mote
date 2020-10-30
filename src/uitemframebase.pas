unit uItemFrameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Grids, ComCtrls,
  Dialogs, uItemDto;

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

  TItemFrameBase = class(TFrame)
    btnEditDescription: TBitBtn;
    edtEditTimeInterval: TBitBtn;
    edtOptions: TBitBtn;
    btnEditTitle: TBitBtn;
    btnEditExternalToolItem: TBitBtn;
    btnStop: TBitBtn;
    btnInitWork: TBitBtn;
    btnInitUnexpectedWork: TBitBtn;
    edtExternalToolItem: TEdit;
    edtDeleteTimeInterval: TBitBtn;
    edtTitle: TEdit;
    gb: TGroupBox;
    lblTime: TLabel;
    lblDescription: TLabel;
    lblExternalToolItem: TLabel;
    lblTitle: TLabel;
    lbTime: TListBox;
    mmDescription: TMemo;
    procedure btnEditDescriptionClick(Sender: TObject);
    procedure btnEditDescriptionExit(Sender: TObject);
    procedure btnEditExternalToolItemClick(Sender: TObject);
    procedure btnEditTitleClick(Sender: TObject);
    procedure btnInitUnexpectedWorkClick(Sender: TObject);
    procedure btnInitWorkClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure edtDeleteTimeIntervalClick(Sender: TObject);
    procedure edtEditTimeIntervalClick(Sender: TObject);
    procedure edtExternalToolItemExit(Sender: TObject);
    procedure edtOptionsClick(Sender: TObject);
    procedure edtTitleEditingDone(Sender: TObject);
    procedure edtTitleExit(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure lbTimeClick(Sender: TObject);
    procedure lbTimeDblClick(Sender: TObject);
    procedure lbTimeExit(Sender: TObject);
    procedure mmDescriptionExit(Sender: TObject);
    procedure TratarMouseSobreTextos(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
  private
    FoItem: TItemDto;

    FslCustomControls: TList;
    FoParentItem: TItemFrameBase;
    FoItemThatInterruptThis: TItemFrameBase;
    FOnvent: TItemFrameBaseEvent;
    function GetDescription: string;
    function GetExternalToolItem: string;
    function GetId: string;
    function GetParentItem: TItemFrameBase;
    function GetTime: string;
    function GetTimeIntervals: TStrings;
    function GetTitle: string;
    function GetWorking: boolean;
    procedure SetDescription(AValue: string);
    procedure SetExternalToolItem(AValue: string);
    procedure SetId(AValue: string);
    procedure SetOnvent(AValue: TItemFrameBaseEvent);
    procedure SetParentItem(AValue: TItemFrameBase);
    procedure SetTime(AValue: string);
    procedure SetTimeIntervals(AValue: TStrings);
    procedure SetTitle(AValue: string);
    procedure SetWorking(AValue: boolean);
  protected
    FoEventResponseObject: TObject;
    function InterruptWork: boolean; virtual;
    function DoOnvent(const psEvent: string; const poParam: TObject): boolean; virtual;
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

    property Item: TItemDto read FoItem write FoItem;

    function ToString: string; override;
    procedure Update(const pbHandleInternal: boolean); virtual;
    procedure UpdateFromJSON(const psJSONItem: string); virtual;
    procedure AddControl(const psControlClassName: string;
      pslProperties: TStringList; const psUserCodeEventHandle: string);
    procedure RefreshTime;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  uMoteUtils;

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

procedure TItemFrameBase.btnEditDescriptionClick(Sender: TObject);
begin
  mmDescription.Lines.Text := FoItem.Description;
  mmDescription.Visible := True;
  mmDescription.SetFocus;
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

procedure TItemFrameBase.edtDeleteTimeIntervalClick(Sender: TObject);
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
  s: string;
begin
  if lbTime.ItemIndex >= 0 then
  begin
    sl := TStringList.Create;
    try
      sl.Text := FoItem.TimeIntervals;
      s := sl[lbTime.ItemIndex];
      if not InputQuery('Editar intervalo', 'Respeite o formato dd/mm/yyyy hh:mm - hh:mm, senão...!', s) then
        exit;
      sl[lbTime.ItemIndex] := s;
      FoItem.TimeIntervals := sl.Text;
      Update(true);
      DoOnvent('item_time_edited', nil);
    finally
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
  edtDeleteTimeInterval.Visible := false;
  edtEditTimeInterval.Visible := false;
end;

procedure TItemFrameBase.lbTimeClick(Sender: TObject);
begin
  //start edit mode to time inervals
  edtDeleteTimeInterval.Visible := true;
  edtEditTimeInterval.Visible := true;
end;

procedure TItemFrameBase.lbTimeDblClick(Sender: TObject);
begin
  edtEditTimeIntervalClick(Sender);
end;

procedure TItemFrameBase.lbTimeExit(Sender: TObject);
begin
end;

procedure TItemFrameBase.mmDescriptionExit(Sender: TObject);
begin
  mmDescription.Visible := False;
  FoItem.Description:=mmDescription.Lines.Text;
  Update(true);
end;

procedure TItemFrameBase.TratarMouseSobreTextos(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  //de acordo com o texto, msotra botão associado
  btnEditTitle.Visible := Sender = lblTitle;
  btnEditExternalToolItem.Visible := Sender = lblExternalToolItem;
  btnEditDescription.Visible := Sender = lblDescription;
end;

procedure TItemFrameBase.SetTitle(AValue: string);
begin
  if FoItem.Title = AValue then
    Exit;
  FoItem.Title := AValue;
  Update(true);
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

function TItemFrameBase.GetTimeIntervals: TStrings;
begin
  Result := lbTime.Items;
end;

function TItemFrameBase.GetDescription: string;
begin
  result := FoItem.Description;
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

procedure TItemFrameBase.Update(const pbHandleInternal: boolean);
var
  bUpdated: boolean;
begin
  bUpdated := (lblTitle.Caption <> FoItem.Title);
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

  if pbHandleInternal and bUpdated then
  begin
    DoOnvent('item_update', nil);
  end;


  gb.Color := $00EBEBEB; //white + 1
  if Working then
    gb.Color := $00D0FBD6; //green

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
