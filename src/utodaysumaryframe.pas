unit uTodaySumaryFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, uItemFrameBase;

type

  { TTodaySumaryFrame }

  TTodaySumaryFrame = class(TFrame)
    lbTop: TLabel;
    lbList: TLabel;
    pnClient: TPanel;
  private
    FoContainerOfItemsFrames: TComponent;
    FsFilter: string;
    FsTodayValue: string;
    FbCanControlContainerOfItemsFramesVisiblity: boolean;
    procedure SetTodayValue(AValue: string);
  public
    property TodayValue: string read FsTodayValue write SetTodayValue;
    property ContainerOfItemsFrames: TComponent read FoContainerOfItemsFrames write FoContainerOfItemsFrames;
    property Filter: string read FsFilter write FsFilter;
    property CanControlContainerOfItemsFramesVisiblity: boolean read FbCanControlContainerOfItemsFramesVisiblity write FbCanControlContainerOfItemsFramesVisiblity;
    procedure Update; override;
  end;

implementation

{$R *.lfm}

{ TTodaySumaryFrame }

procedure TTodaySumaryFrame.SetTodayValue(AValue: string);
begin
  if FsTodayValue=AValue then Exit;
  FsTodayValue:=AValue;
  Update;
end;

procedure TTodaySumaryFrame.Update;
var
  i: integer;
  j: integer;
  f: TItemFrameBase;
  nCount: integer;
  sToday, sLinesToday: string;
  sl:TStringList;
begin
  sl := TStringList.Create;
  try
    sToday := FsTodayValue;
    nCount := 0;
    lbList.Caption := '';
    for i := 0 to FoContainerOfItemsFrames.ComponentCount-1 do
    begin
      if FoContainerOfItemsFrames.Components[i] is TItemFrameBase then
      begin
        f := FoContainerOfItemsFrames.Components[i] as TItemFrameBase;
        if ((FsFilter = '') or (Pos(FsFilter, f.Item.ToString)>0)) then
        begin
          if (not FbCanControlContainerOfItemsFramesVisiblity) and (not f.Visible) then
            continue;
           f.Visible := Pos(sToday, f.Item.TimeIntervals) > 0;
          Inc(nCount);
          sLinesToday := '';
          sl.Text := f.Item.TimeIntervals;
          for j := 0 to sl.Count-1 do
          begin
            if Pos(sToday, sl[j]) = 1 then
            begin
              if sLinesToday <> '' then sLinesToday := sLinesToday + ',';
              sLinesToday := sLinesToday + sl[j];
            end;
          end;
          if lbList.Caption <> '' then lbList.Caption := lbList.Caption + ' | ';
          if sLinesToday <> '' then
            lbList.Caption := lbList.Caption + Copy(f.Item.Title, 1, 15) + ': ' + sLinesToday;
        end
        else
        if FbCanControlContainerOfItemsFramesVisiblity then
          f.Visible := false;
      end;
    end;
    lbTop.Caption := sToday + ' ->  ' + Format('%d items', [nCount]);
    if FsFilter <> '' then
      lbTop.Caption := lbTop.Caption +  ' (Filter: "' + FsFilter + '").';
  finally
    sl.Free;
  end;
end;

end.

