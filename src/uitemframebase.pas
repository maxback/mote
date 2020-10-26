unit uItemFrameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons;

type

  { TItemFrameCustomControls }

  TItemFrameCustomControls = class
  public
    ControlClassName: string;
    Properties: TStringList;
    UserCodeEventHandle: string;
    constructor CreateItem(const psControlClassName: string; pslProperties: TStringList; const psUserCodeEventHandle: string);
    destructor Destroy; override;
  end;

  { TItemFrameBase }

  TItemFrameBaseEvent = function(Sender: TObject; const psEvent; const poParam: TObject; const poResponse: TObject): boolean;

  TItemFrameBase = class(TFrame)
    btnEditDescription: TBitBtn;
    btnEditTitle: TBitBtn;
    btnEditTitle1: TBitBtn;
    btnStop: TBitBtn;
    btnInitWork: TBitBtn;
    btnInitUnexpectedWork: TBitBtn;
    edtExternalToolItem: TEdit;
    edtTitle: TEdit;
    gb: TGroupBox;
    lblTime: TLabel;
    lblDescription: TLabel;
    lblExternalToolItem: TLabel;
    lblTitle: TLabel;
    procedure btnEditTitle1Click(Sender: TObject);
    procedure btnEditTitleClick(Sender: TObject);
    procedure btnInitUnexpectedWorkClick(Sender: TObject);
    procedure btnInitWorkClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure edtExternalToolItemEditingDone(Sender: TObject);
    procedure edtExternalToolItemExit(Sender: TObject);
    procedure edtTitleEditingDone(Sender: TObject);
    procedure edtTitleExit(Sender: TObject);
  private
    FId: string;
    FsDescription: string;
    FsExternalToolItem: string;
    FslCustomControls: TList;
    FsTime: string;
    FsTitle: string;
    FbWorking: boolean;
    FoParentItem: TItemFrameBase;
    FoItemThatInterruptThis: TItemFrameBase;
    FOnvent: TItemFrameBaseEvent;
    procedure SetDescription(AValue: string);
    procedure SetExternalToolItem(AValue: string);
    procedure SetId(AValue: string);
    procedure SetOnvent(AValue: TItemFrameBaseEvent);
    procedure SetParentItem(AValue: TItemFrameBase);
    procedure SetTime(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetWorking(AValue: boolean);
  protected
    FoEventResponseObject: TObject;
    function InterruptWork: boolean; virtual;
    function DoOnvent(const psEvent: string; const poParam: TObject): boolean; virtual;
  public
    property Id: string read FId write SetId;
    property ExternalToolItem: string read FsExternalToolItem write SetExternalToolItem;
    property ParentItem: TItemFrameBase read FoParentItem write SetParentItem;
    property Title: string read FsTitle write SetTitle;
    property Time: string read FsTime write SetTime;
    property Description: string read FsDescription write SetDescription;
    property Working: boolean read FbWorking write SetWorking;
    property Onvent: TItemFrameBaseEvent read FOnvent write SetOnvent;
    procedure Update; virtual;
    procedure AddControl(const psControlClassName: string; pslProperties: TStringList; const psUserCodeEventHandle: string);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TItemFrameCustomControls }

constructor TItemFrameCustomControls.CreateItem(const psControlClassName: string; pslProperties: TStringList; const psUserCodeEventHandle: string);
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
  lblTitle.Visible:=false;
  edtTitle.Visible:=true;
  edtTitle.SetFocus();
end;

procedure TItemFrameBase.btnEditTitle1Click(Sender: TObject);
begin
  lblExternalToolItem.Visible:=false;
  edtExternalToolItem.Visible:=true;
  edtExternalToolItem.SetFocus();
end;

procedure TItemFrameBase.btnInitUnexpectedWorkClick(Sender: TObject);
begin
  if InterruptWork then
  begin
    FbWorking := False;
    Update;
  end;
end;

procedure TItemFrameBase.btnInitWorkClick(Sender: TObject);
begin
  FbWorking := True;
  Update;
end;

procedure TItemFrameBase.btnStopClick(Sender: TObject);
begin
  FbWorking := False;
  Update;
end;

procedure TItemFrameBase.edtExternalToolItemEditingDone(Sender: TObject);
begin
  FsExternalToolItem := edtExternalToolItem.Text;
  Update;
end;

procedure TItemFrameBase.edtExternalToolItemExit(Sender: TObject);
begin
  lblExternalToolItem.Visible:=true;
  edtExternalToolItem.Visible:=false;
end;

procedure TItemFrameBase.edtTitleEditingDone(Sender: TObject);
begin
  FsTitle := edtTitle.Text;
  Update;
end;

procedure TItemFrameBase.edtTitleExit(Sender: TObject);
begin
  lblTitle.Visible:=true;
  edtTitle.Visible:=false;
end;

procedure TItemFrameBase.SetTitle(AValue: string);
begin
  if FsTitle=AValue then Exit;
  FsTitle:=AValue;
  Update;
end;

procedure TItemFrameBase.SetWorking(AValue: boolean);
begin
  if FbWorking=AValue then Exit;
  FbWorking:=AValue;
  Update;
end;

function TItemFrameBase.InterruptWork: boolean;
begin
  result := false;
  if DoOnvent('interrupt', nil) and Assigned(FoEventResponseObject) and (FoEventResponseObject is TItemFrameBase) then
  begin
     FoItemThatInterruptThis := FoEventResponseObject As TItemFrameBase;
     result := true;
  end;

end;

function TItemFrameBase.DoOnvent(const psEvent: string; const poParam: TObject): boolean;
begin
  if FOnvent = nil then
    exit(false);

  result := FOnvent(Self, psEvent, poParam, FoEventResponseObject);
end;

procedure TItemFrameBase.SetTime(AValue: string);
begin
  if FsTime=AValue then Exit;
  FsTime:=AValue;
  Update;
end;

procedure TItemFrameBase.SetDescription(AValue: string);
begin
  if FsDescription=AValue then Exit;
  FsDescription:=AValue;
  Update;
end;

procedure TItemFrameBase.SetExternalToolItem(AValue: string);
begin
  if FsExternalToolItem=AValue then Exit;
  FsExternalToolItem:=AValue;
  Update;
end;

procedure TItemFrameBase.SetId(AValue: string);
begin
  if FId=AValue then Exit;
  FId:=AValue;
  Update;
end;

procedure TItemFrameBase.SetOnvent(AValue: TItemFrameBaseEvent);
begin
  if FOnvent=AValue then Exit;
  FOnvent:=AValue;
end;

procedure TItemFrameBase.SetParentItem(AValue: TItemFrameBase);
begin
  if FoParentItem=AValue then Exit;
  FoParentItem:=AValue;
  Update;
end;

procedure TItemFrameBase.Update;
begin
  lblTitle.Caption := FsTitle;
  lblTime.Caption := FsTime;
  lblDescription.Caption := FsDescription;
  lblExternalToolItem.Caption := FsExternalToolItem;
  btnInitWork.Visible := not FbWorking;
  btnInitUnexpectedWork.Visible := FbWorking;
  btnStop.Visible := FbWorking;
  gb.Caption := ' ';

  if FoParentItem <> nil then
    gb.Caption := 'Filho de  ' + FoParentItem.title + '. ';
  if FoItemThatInterruptThis <> nil then
    gb.Caption := 'Interrompido por ' + FoItemThatInterruptThis.Title;
end;

procedure TItemFrameBase.AddControl(const psControlClassName: string; pslProperties: TStringList; const psUserCodeEventHandle: string);
begin

end;

constructor TItemFrameBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FslCustomControls := TList.Create;
  Update;
end;

destructor TItemFrameBase.Destroy;
var
  i: integer;
begin
  for i := 0 to FslCustomControls.Count -1 do
  begin
    TObject(FslCustomControls.Items[i]).Free;
  end;

  inherited Destroy;
end;

end.

