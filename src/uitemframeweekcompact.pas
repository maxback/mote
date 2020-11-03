unit uItemFrameWeekCompact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Grids, ComCtrls,
  Dialogs, uItemDto, uItemFrameBase;

type

  { TItemFrameWeekCompact }

  TItemFrameWeekCompact = class(TFrame)
    btnEditDescription: TBitBtn;
    btnEditExternalToolItem: TBitBtn;
    btnEditTitle: TBitBtn;
    btnInitUnexpectedWork: TBitBtn;
    btnInitWork: TBitBtn;
    btnStop: TBitBtn;
    edtDeleteTimeInterval: TBitBtn;
    edtEditTimeInterval: TBitBtn;
    edtExternalToolItem: TEdit;
    edtOptions: TBitBtn;
    edtTitle: TEdit;
    gb: TGroupBox;
    lblDescription: TLabel;
    lblExternalToolItem: TLabel;
    lblTime: TLabel;
    lblTitle: TLabel;
    lbTime: TListBox;
    mmDescription: TMemo;
  private
    FoItem: TItemDto;

    FslCustomControls: TList;
    FoParentItem: TItemFrameWeekCompact;
    FoItemThatInterruptThis: TItemFrameWeekCompact;
    FOnvent: TItemFrameBaseEvent;
    function GetDescription: string;
    function GetExternalToolItem: string;
    function GetId: string;
    function GetParentItem: TItemFrameWeekCompact;
    function GetTime: string;
    function GetTimeIntervals: TStrings;
    function GetTitle: string;
    function GetWorking: boolean;
    function GetUserName: string;
    function GetCreatedBy: string;
    procedure SetDescription(AValue: string);
    procedure SetExternalToolItem(AValue: string);
    procedure SetId(AValue: string);
    procedure SetOnvent(AValue: TItemFrameBaseEvent);
    procedure SetParentItem(AValue: TItemFrameWeekCompact);
    procedure SetTime(AValue: string);
    procedure SetTimeIntervals(AValue: TStrings);
    procedure SetTitle(AValue: string);
    procedure SetWorking(AValue: boolean);
    procedure SetUserName(AValue: string);
    procedure SetCreatedBy(AValue: string);

  protected
    FoEventResponseObject: TObject;
    function InterruptWork: boolean; virtual;
    function DoOnvent(const psEvent: string; const poParam: TObject): boolean; virtual;
  public
    property Id: string read GetId write SetId;
    property ExternalToolItem: string read GetExternalToolItem write SetExternalToolItem;
    property ParentItem: TItemFrameWeekCompact read GetParentItem write SetParentItem;
    property Title: string read GetTitle write SetTitle;
    property Time: string read GetTime write SetTime;
    property Description: string read GetDescription write SetDescription;
    property Working: boolean read GetWorking write SetWorking;
    property TimeIntervals: TStrings read GetTimeIntervals write SetTimeIntervals;
    property UserName: string read GetUserName write SetUserName;
    property CreatedBy: string read GetCreatedBy write SetCreatedBy;

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

{$R *.lfm}

uses
  uMoteUtils;
{ TItemFrameWeekCompact }

function TItemFrameWeekCompact.GetDescription: string;
begin
  result := FoItem.Description;
end;

function TItemFrameWeekCompact.GetExternalToolItem: string;
begin
  result := FoItem.ExternalToolItem;
end;

function TItemFrameWeekCompact.GetId: string;
begin
  result := FoItem.Id;
end;

function TItemFrameWeekCompact.GetParentItem: TItemFrameWeekCompact;
begin
  result := FoParentItem;
end;

function TItemFrameWeekCompact.GetTime: string;
begin
  result := FoItem.Time;
end;

function TItemFrameWeekCompact.GetTimeIntervals: TStrings;
begin
  Result := lbTime.Items;
end;

function TItemFrameWeekCompact.GetTitle: string;
begin
  result := FoItem.Title;
end;

function TItemFrameWeekCompact.GetWorking: boolean;
begin
  result := FoItem.Working;
end;

function TItemFrameWeekCompact.GetUserName: string;
begin
  Result:=FoItem.UserName;
end;

function TItemFrameWeekCompact.GetCreatedBy: string;
begin
  Result:=FoItem.CreatedBy;
end;

procedure TItemFrameWeekCompact.SetDescription(AValue: string);
begin
  if FoItem.Description = AValue then
    Exit;
  FoItem.Description := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetExternalToolItem(AValue: string);
begin
  if FoItem.ExternalToolItem = AValue then
    Exit;
  FoItem.ExternalToolItem := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetId(AValue: string);
begin
  if FoItem.Id = AValue then
    Exit;
  FoItem.Id := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetOnvent(AValue: TItemFrameBaseEvent);
begin
  if FOnvent = AValue then
    Exit;
  FOnvent := AValue;
end;

procedure TItemFrameWeekCompact.SetParentItem(AValue: TItemFrameWeekCompact);
begin
  if FoParentItem = AValue then
    Exit;
  FoParentItem := AValue;
  if Assigned(FoParentItem) then
    FoItem.Id := FoParentItem.Id;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetTime(AValue: string);
begin
  if FoItem.Time = AValue then
    Exit;
  FoItem.Time := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetTimeIntervals(AValue: TStrings);
begin
  lbTime.Items.Text := AValue.Text;
end;

procedure TItemFrameWeekCompact.SetTitle(AValue: string);
begin
  if FoItem.Title = AValue then
    Exit;
  FoItem.Title := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetWorking(AValue: boolean);
begin
  if FoItem.Working = AValue then
    Exit;
  FoItem.Working := AValue;
  Update(true);
end;

procedure TItemFrameWeekCompact.SetUserName(AValue: string);
begin
  FoItem.UserName:=AValue;
end;

procedure TItemFrameWeekCompact.SetCreatedBy(AValue: string);
begin
  FoItem.CreatedBy:=AValue;
end;

function TItemFrameWeekCompact.InterruptWork: boolean;
begin
  Result := False;
  if DoOnvent('item_interrupt', nil) and Assigned(FoEventResponseObject) and
    (FoEventResponseObject is TItemFrameWeekCompact) then
  begin
    FoItemThatInterruptThis := FoEventResponseObject as TItemFrameWeekCompact;
    Result := True;
  end;
end;

function TItemFrameWeekCompact.DoOnvent(const psEvent: string;
  const poParam: TObject): boolean;
begin
  if FOnvent = nil then
    exit(False);

  Result := FOnvent(Self, psEvent, poParam, FoEventResponseObject);
end;

function TItemFrameWeekCompact.ToString: string;
begin
  result := FoItem.ToString;
end;

procedure TItemFrameWeekCompact.Update(const pbHandleInternal: boolean);
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

procedure TItemFrameWeekCompact.UpdateFromJSON(const psJSONItem: string);
begin
  ConvertJSONStringObject(psJSONItem, FoItem);
  Update(false);
end;

procedure TItemFrameWeekCompact.AddControl(const psControlClassName: string;
  pslProperties: TStringList; const psUserCodeEventHandle: string);
begin

end;

procedure TItemFrameWeekCompact.RefreshTime;
begin
  DoOnvent('item_refresh_query', nil);
end;

constructor TItemFrameWeekCompact.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FslCustomControls := TList.Create;
  FoItem := TItemDto.Create;
  Update(true);
end;

destructor TItemFrameWeekCompact.Destroy;
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

end.

