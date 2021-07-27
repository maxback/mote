unit uTodaySumaryFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, uItemFrameBase, DateUtils,
  Graphics, Buttons;

type

  { TTodaySumaryFrame }

  TTimeTrackItem = record
    nStartTime: single;
    nEndTime: single;
    sStartTime: string;
    sEndTime: string;
    sTitle: string;
    l1: integer;
    l2: integer;
    oItemFrameBase: TItemFrameBase;
  end;

  TTodaySumaryFrameTimeTrackKind = (ttkCompact, ttkMultiLines);
  TTodaySumaryFrame = class(TFrame)
    lbTop: TLabel;
    lbList: TLabel;
    lb: TListBox;
    PanelTimeTrack: TPanel;
    pbTimeTrack: TPaintBox;
    pnClient: TPanel;
    sbPositionLess: TSpeedButton;
    sbPositionPlus: TSpeedButton;
    procedure pbTimeTrackClick(Sender: TObject);
    procedure pbTimeTrackDblClick(Sender: TObject);
    procedure pbTimeTrackMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbTimeTrackPaint(Sender: TObject);
    procedure sbPositionLessClick(Sender: TObject);
    procedure sbPositionPlusClick(Sender: TObject);
  private
    FoContainerOfItemsFrames: TComponent;
    FsFilter: string;
    FsTodayValue: string;
    FbCanControlContainerOfItemsFramesVisiblity: boolean;
    FnMinTimeDec, FnMaxTimeDec: single;
    FaTimeTrack: array of TTimeTrackItem;
    FnTimeTrackScale: single;
    FnTimeTrackScroolPosition: integer;
    FoTimeTrackKind: TTodaySumaryFrameTimeTrackKind;

    procedure SetTodayValue(AValue: string);
    procedure UpdateTimeTrackPaintBox;
    procedure SortTimeTrack;
    function FilterString(const psText: string): boolean;
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

procedure TTodaySumaryFrame.pbTimeTrackPaint(Sender: TObject);
begin
  UpdateTimeTrackPaintBox;
end;

procedure TTodaySumaryFrame.sbPositionLessClick(Sender: TObject);
begin
  if FnTimeTrackScale <= 1.0 then
    exit;

  if FnTimeTrackScroolPosition <= 1 then
  exit;

  FnTimeTrackScroolPosition := FnTimeTrackScroolPosition - 1;
  UpdateTimeTrackPaintBox;
end;

procedure TTodaySumaryFrame.sbPositionPlusClick(Sender: TObject);
begin
  if FnTimeTrackScale <= 1.0 then
    exit;

  FnTimeTrackScroolPosition := FnTimeTrackScroolPosition + 1;
  UpdateTimeTrackPaintBox;
end;

procedure TTodaySumaryFrame.pbTimeTrackDblClick(Sender: TObject);
begin
end;

procedure TTodaySumaryFrame.pbTimeTrackMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  l, i: integer;
begin
  {
  if FnTimeTrackScale > 1.0 then
  begin
    //pbTimeTrack.Hint := '???';
    lbList.Caption := '';
    exit;
  end;
  }
  lbList.Caption := '';
  l := X;
  for i := Low(FaTimeTrack) to High(FaTimeTrack) do
  begin
    if (l >= FaTimeTrack[i].l1) and (l <= FaTimeTrack[i].l2) then
    begin
      //pbTimeTrack.Hint := FaTimeTrack[i].sTitle + ' (' + FaTimeTrack[i].sStartTime + ' - ' + FaTimeTrack[i].sEndTime + ')';
      //lbList.Caption := Format('%d <= %d <= %d - %s', [FaTimeTrack[i].l1, l , FaTimeTrack[i].l2, pbTimeTrack.Hint]);
      lbList.Caption := FaTimeTrack[i].sStartTime + ' - ' + FaTimeTrack[i].sEndTime + ' - ' + FaTimeTrack[i].sTitle;
      break;
    end;
  end;

end;

procedure TTodaySumaryFrame.pbTimeTrackClick(Sender: TObject);
begin
  if FnTimeTrackScale = 0 then
    FnTimeTrackScale := 1.0;

  if FoTimeTrackKind = ttkCompact then
  begin
    FoTimeTrackKind := ttkMultiLines;
    UpdateTimeTrackPaintBox;
    exit;
  end;

  FnTimeTrackScale := FnTimeTrackScale + 1;
  UpdateTimeTrackPaintBox;

  if FnTimeTrackScale > 5 then
  begin
    FnTimeTrackScale := 1.0;
    FnTimeTrackScroolPosition := 1;
    FoTimeTrackKind := ttkCompact;
  end;

  sbPositionLess.Visible := FnTimeTrackScale > 1.0;
  sbPositionPlus.Visible := FnTimeTrackScale > 1.0;

  UpdateTimeTrackPaintBox;
end;

procedure TTodaySumaryFrame.SetTodayValue(AValue: string);
begin
  if FsTodayValue=AValue then Exit;
  FsTodayValue:=AValue;
  Update;
end;


procedure TTodaySumaryFrame.UpdateTimeTrackPaintBox;
const
//  clCOLORS: array[0..13] of TColor = (clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
  //clCOLORS: array[0..13] of TColor = (clWhite, clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);
  clCOLORS: array[0..12] of TColor = (clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);

var
  i, i2, l1, l2, t, iC, iColorActual, H, W1, W2: integer;
  oTimeTrackItem: TTimeTrackItem;
  nHour, nPixesPerMiliHour, nIntervalMileOur: single;
  s, sKey, sTitleOld: string;
  slColorByTitle: TStringList;
begin
  if FnTimeTrackScale < 1.0 then
    FnTimeTrackScale := 1.0;

  try
    try
      lb.Items.Clear;
      //lbList.Caption := '';
      nIntervalMileOur := (FnMaxTimeDec * 1000.0) - (FnMinTimeDec * 1000.0);
      nPixesPerMiliHour :=  nIntervalMileOur /  ( ( (pbTimeTrack.Width) * ( FnTimeTrackScale) ) - 150);
      //lbList.Caption := lbList.Caption + format('(pbtimeTrack.Width - 150): %d, nIntervalMileOur: %f, nPixesPerMiliHour: %f', [pbTimeTrack.Width - 150, nIntervalMileOur, nPixesPerMiliHour]);
      lb.Items.Add(lbList.Caption);

      pbTimeTrack.Canvas.Brush.Color := clWhite;
      pbTimeTrack.Canvas.Pen.Color:= clWhite;
      pbTimeTrack.Canvas.Rectangle(1, 1, pbTimeTrack.Width, pbTimeTrack.Height);
      pbTimeTrack.Canvas.Pen.Color:= clBlack;

      iC := 0;
      sTitleOld := '';
      slColorByTitle := TStringList.Create;

      for i := Low(FaTimeTrack) to High(FaTimeTrack) do
      begin
        //lbList.Caption := lbList.Caption + ', ' + IntToStr(i);
        oTimeTrackItem := FaTimeTrack[i];
        l1 := Trunc(
          (oTimeTrackItem.nStartTime - FnMinTimeDec) * 1000.0 / nPixesPerMiliHour
          );

        if l1 = 0.0 then
          l1 := 1;
        l2 := Trunc(
          (oTimeTrackItem.nEndTime - FnMinTimeDec) * 1000.0 / nPixesPerMiliHour
          );

        if l2 = 0.0 then
          l2 := 1;

        if FnTimeTrackScroolPosition > 1 then
          l1 := l1 - (100 * FnTimeTrackScroolPosition);

        if FnTimeTrackScroolPosition > 1 then
          l2 := l2 - (100 * FnTimeTrackScroolPosition);

        lb.Items.Add(Format('{%d} - entre l1: %d, %s (%0.3f) e l2: %d, %s (%0.3f): %s', [
          i, l1, oTimeTrackItem.sStartTime, oTimeTrackItem.nStartTime,
          l2, oTimeTrackItem.sEndTime, oTimeTrackItem.nEndTime, oTimeTrackItem.sTitle]));

        sKey := StringReplace(oTimeTrackItem.sTitle, ' ', '', [rfReplaceall]);
        if (i>0) and (oTimeTrackItem.sTitle <> sTitleOld) then
        begin
          s := slColorByTitle.Values[sKey];
          if s = EmptyStr then
          begin
            iC := (iC + 1) mod Length(clCOLORS);
            iColorActual := iC;
          end
          else
          begin
            iColorActual := StrToIntDef(s, -1);
            if iColorActual = -1 then
            begin
              iC := (iC + 1) mod Length(clCOLORS);
              iColorActual := iC;
            end;
          end;
          slColorByTitle.Values[sKey] := IntToStr(iColorActual);
        end
        else if i = 0 then
        begin
          slColorByTitle.Values[sKey] := IntToStr(iC);
          iColorActual := iC;
        end;

        //if oTimeTrackItem.oItemFrameBase.Shape.Brush.Color = clWhite then
        oTimeTrackItem.oItemFrameBase.Shape.Brush.Color := clCOLORS[iColorActual];

        sTitleOld := oTimeTrackItem.sTitle;
    //    pbTimeTrack.Canvas.Pen.Color := clCOLORS[iC];
        pbTimeTrack.Canvas.Brush.Color := clCOLORS[iColorActual];

        H := pbTimeTrack.Canvas.TextHeight('H') + 3;

        if FoTimeTrackKind = ttkCompact then
        begin
          t := 2;
          H := H * 2;
          pbTimeTrack.Canvas.Rectangle(l1, t - 1, l2, t + H);
        end
        else
        begin
          if pbTimeTrack.Height > (3 * H) then
            H := pbTimeTrack.Height div 3;
          t := 2;
          if (i mod 3) = 1 then
            t := t + 4 + H
          else if (i mod 3) = 2 then
            t := t + 4 + ( 2 * H);
          pbTimeTrack.Canvas.Rectangle(l1, t - 1, l2, t + H);
        end;

        pbTimeTrack.Canvas.Brush.Color := clWhite;
        pbTimeTrack.Canvas.Pen.Color := clBlack;

        if FoTimeTrackKind = ttkCompact then
        begin
          pbTimeTrack.Canvas.TextOut(l1 + 2, t, oTimeTrackItem.sTitle);

          W1 := pbTimeTrack.Canvas.TextWidth('WW:WW - WW:WW');
          W2 := pbTimeTrack.Canvas.TextWidth('W');

          if (l2 - l1) >= W1 then
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H  + 5, oTimeTrackItem.sStartTime + ' - ' + oTimeTrackItem.sEndTime)
          else if (l2 - l1) >= pbTimeTrack.Canvas.TextWidth('WW:WW')  then
          begin
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H  + 5, oTimeTrackItem.sStartTime + ' -');
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H + pbTimeTrack.Canvas.TextHeight('H') + 7, oTimeTrackItem.sEndTime);
          end
          else if (l2 - l1) >= W2 then
          begin
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H  + 5, Copy(oTimeTrackItem.sStartTime, 1, 3));
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H + pbTimeTrack.Canvas.TextHeight('H') + 7, Copy(oTimeTrackItem.sStartTime, 4, 2));
          end
          else
          begin
            for i2 := 0 to 1 do
              pbTimeTrack.Canvas.TextOut(l1 + 5, t + H + (i2 * pbTimeTrack.Canvas.TextHeight('H')) + 5, Copy(oTimeTrackItem.sStartTime, i2+1, 1));
            pbTimeTrack.Canvas.TextOut(l1 + 5, t + H + (2 * pbTimeTrack.Canvas.TextHeight('H')) + 5, Copy(oTimeTrackItem.sStartTime, 3, 3));
          end;
        end
        else
        begin
          pbTimeTrack.Canvas.TextOut(l1 + 2, t, oTimeTrackItem.sStartTime + ' - ' + oTimeTrackItem.sEndTime + ' - ' + oTimeTrackItem.sTitle);
        end;

        oTimeTrackItem.l1 := l1;
        oTimeTrackItem.l2 := l2;
        FaTimeTrack[i] := oTimeTrackItem;
      end;

      //poe esca embaixo
      nHour := Trunc(FnMinTimeDec) - 1.0;
      if FoTimeTrackKind = ttkCompact then
      begin
        pbTimeTrack.Canvas.Brush.Color := clWhite;
        pbTimeTrack.Canvas.Pen.Color := clBlack;

        H := pbTimeTrack.Canvas.TextHeight('H') + 3;
        t := (H * 2) + 3;

        while nHour <= (FnMaxTimeDec + 2.0) do
        begin
          l1 := Trunc( (nHour - FnMinTimeDec) * 1000.0 / nPixesPerMiliHour );

          if l1 = 0.0 then
            l1 := 1;

          if FnTimeTrackScroolPosition > 1 then
            l1 := l1 - (100 * FnTimeTrackScroolPosition);

          pbTimeTrack.Canvas.Rectangle(l1, t, l1+3, t + (2 * H));

          pbTimeTrack.Canvas.TextOut(l1, t + (2 * H) + 3, FloattoStr(nHour) + 'h');

          nHour := nHour + 1.0;
        end;

        pbTimeTrack.Canvas.Rectangle(1, t, pbTimeTrack.Width , t + 3);

      end;

    except
    end;
  finally
    slColorByTitle.Free;
  end;

end;
procedure TTodaySumaryFrame.SortTimeTrack;
var
  i, j: integer;
  oTimeTrackItem: TTimeTrackItem;
begin
  for j := Low(FaTimeTrack) to High(FaTimeTrack) do
    for i := Low(FaTimeTrack) to High(FaTimeTrack)-1 do
    begin
      oTimeTrackItem := FaTimeTrack[i];
      if oTimeTrackItem.nStartTime > FaTimeTrack[i+1].nStartTime then
      begin
        FaTimeTrack[i] := FaTimeTrack[i+1];
        FaTimeTrack[i+1] := oTimeTrackItem;
      end;
    end;

end;


function TTodaySumaryFrame.FilterString(const psText: string): boolean;
var
  s: string;
  bNotFilter: boolean;
begin
  if FsFilter = '' then
    exit(true);

  bNotFilter := Copy(FsFilter, 1, 1) = '!';
  s := FsFilter;
  if bNotFilter then
  begin
    Delete(s, 1, 1);
    exit(Pos(s, psText) < 1);
  end;

  exit(Pos(s, psText) > 0);
end;

procedure TTodaySumaryFrame.Update;
var
  i: integer;
  j: integer;
  f: TItemFrameBase;
  nCount: integer;
  sToday, sLinesToday: string;
  sl:TStringList;
  nTimeDec, nTotalTimeDec : single;
  sH, sMinTimeDec, sMaxTimeDec: string;
  dt: TdateTime;
  Y,Mo,D,H,Mi,S,MS : Word;
  oTimeTrackItem: TTimeTrackItem;
begin
  nTotalTimeDec := 0.0;
  SetLength(FaTimeTrack, 0);
  //lb.Items.Clear;
  sl := TStringList.Create;
  try
    FnMinTimeDec := 24.0;
    FnMaxTimeDec := 0.0;
    sToday := FsTodayValue;
    nCount := 0;
    //lbList.Caption := '';
    for i := 0 to FoContainerOfItemsFrames.ComponentCount-1 do
    begin
      if FoContainerOfItemsFrames.Components[i] is TItemFrameBase then
      begin
        f := FoContainerOfItemsFrames.Components[i] as TItemFrameBase;
        if FilterString(f.Item.ToString) then
        begin
          if (not FbCanControlContainerOfItemsFramesVisiblity) and (not f.Visible) then
            continue;
          f.Visible := Pos(sToday, f.Item.TimeIntervals) > 0;
          f.Visible := f.Visible or (Pos(sToday, DateTimeToStr(f.Item.CreationDateTime)) > 0);

          Inc(nCount);
          sLinesToday := '';
          sl.Text := f.Item.TimeIntervals;
          for j := 0 to sl.Count-1 do
          begin
            if Pos(sToday, sl[j]) = 1 then
            begin
              try
                sH := Copy(sl[j], 12, 5);
                dt := StrToDateTime(sH);
                DecodeDateTime(dt,Y,Mo,D,H,Mi,S,MS);

                nTimeDec := H + (Mi / 60.0) + (S / 60.0 / 60.0) + (MS / 60.0 / 1000.0);
                if nTimeDec < FnMinTimeDec then
                begin
                  FnMinTimeDec := nTimeDec;
                  sMinTimeDec := sH;
                end;

                oTimeTrackItem.oItemFrameBase := f;
                oTimeTrackItem.nStartTime := nTimeDec;
                oTimeTrackItem.sStartTime:= sH;

                if Length(sl[j]) > 20 then
                  sH := Copy(sl[j], 20, 5)
                else
                  sH := FormatDateTime('hh:mm', time);

                dt := StrToDateTime(sH);
                DecodeDateTime(dt,Y,Mo,D,H,Mi,S,MS);
                nTimeDec := H + (Mi / 60.0) + (S / 60.0 / 60.0) + (MS / 60.0 / 1000.0);

                oTimeTrackItem.nEndTime := nTimeDec;
                oTimeTrackItem.sEndTime := sH;

                nTotalTimeDec := nTotalTimeDec + (oTimeTrackItem.nEndTime - oTimeTrackItem.nStartTime);

                oTimeTrackItem.sTitle:= f.Item.Title;


                if nTimeDec > FnMaxTimeDec then
                begin
                  FnMaxTimeDec := nTimeDec;
                  sMaxTimeDec := sH;
                end;

                SetLength(FaTimeTrack, Length(FaTimeTrack)+1);
                FaTimeTrack[High(FaTimeTrack)] := oTimeTrackItem;
//                lb.Items.Add(Format('[%d] - entre %s (%0.3f) e %s (%0.3f): %s', [
//                  High(FaTimeTrack), oTimeTrackItem.sStartTime, oTimeTrackItem.nStartTime,
//                  oTimeTrackItem.sEndTime, oTimeTrackItem.nEndTime, oTimeTrackItem.sTitle]));
              except
              end;

              if sLinesToday <> '' then sLinesToday := sLinesToday + ',';
              sLinesToday := sLinesToday + sl[j];
            end;
          end;
          //if lbList.Caption <> '' then lbList.Caption := lbList.Caption + ' | ';
          //if sLinesToday <> '' then
          //  lbList.Caption := lbList.Caption + Copy(f.Item.Title, 1, 15) + ': ' + sLinesToday;
        end
        else
        if FbCanControlContainerOfItemsFramesVisiblity then
          f.Visible := false;
      end;
    end;
    lbTop.Caption := sToday + ' ->  ' + Format('%d items', [nCount]);
    if FsFilter <> '' then
      lbTop.Caption := lbTop.Caption +  ' (Filter: "' + FsFilter + Format('"). Total time dec: %0.3f', [nTotalTimeDec]) ;

    lbTop.Caption := lbTop.Caption + Format(' (entre %s (%0.3f) e %s (%0.3f)).  Total time dec: %0.3f', [sMinTimeDec, FnMinTimeDec, sMaxTimeDec, FnMaxTimeDec, nTotalTimeDec]);
    SortTimeTrack;
    UpdateTimeTrackPaintBox;
  finally
    sl.Free;
  end;
end;

end.

