// Component serialized as Pascal - Begin
// Format version 1
WebPanel1:=TWebPanel.Create(Self);
WebLabel1:=TWebLabel.Create(Self);
WebTimer1:=TWebTimer.Create(Self);
WebPanel1.BeginUpdate;
WebLabel1.BeginUpdate;
try
  Name:='WebForm1';
  Left:=315;
  Height:=240;
  Top:=114;
  Width:=320;
  Caption:='WebForm1';
  ClientHeight:=240;
  ClientWidth:=320;
  LCLVersion:='1.9.0.0';
  with WebPanel1 do begin
    Name:='WebPanel1';
    Parent:=Self;
    Left:=8;
    Height:=57;
    Top:=16;
    Width:=142;
    Caption:='WebPanel1';
    with WebLabel1 do begin
      Name:='WebLabel1';
      Parent:=WebPanel1;
      Left:=8;
      Height:=39;
      Top:=8;
      Width:=124;
      Caption:='00:00:00';
      Font.CharSet:=ANSI_CHARSET;
      Font.Color:=clRed;
      Font.Height:=-32;
      Font.Name:='Tahoma';
      Font.Pitch:=fpVariable;
      Font.Quality:=fqDraft;
      ParentColor:=False;
      ParentFont:=False;
      AlignWithMargins:=False;
      EllipsisPosition:=epEndEllipsis;
    end;
  end;
  with WebTimer1 do begin
    Name:='WebTimer1';
    OnTimer:=@WebTimer1Timer;
    WebTimer1.DesignInfo:=1310930;
  end;
  // Component serialized as Pascal - End
finally
  WebPanel1.EndUpdate;
  WebLabel1.EndUpdate;
end;
