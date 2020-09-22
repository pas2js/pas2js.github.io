  WebLabel1        := TLabel.Create(Self);
  WebPaintBox1     := TPaintBox.Create(Self);
  WebPanel1        := TPanel.Create(Self);
  WebLabel2        := TLabel.Create(WebPanel1);
  WebImageControl1 := TImageControl.Create(WebPanel1);
  WebPaintBox2     := TPaintBox.Create(Self);
  WebLabel1.BeginUpdate();
  WebPaintBox1.BeginUpdate();
  WebPanel1.BeginUpdate();
  WebLabel2.BeginUpdate();
  WebImageControl1.BeginUpdate();
  WebPaintBox2.BeginUpdate();

  try
    Name                     := 'Form1';
    Left                     := 0;
    Top                      := 0;
    Width                    := 640;
    Height                   := 480;
    Font.Charset             := 1;
    Font.Color               := 0;
    Font.Height              := -13;
    Font.Name                := 'Tahoma';
    Font.Style               := [];
    FFormContainer            := 'appcontent';
    TabOrder                 := 1;

    WebLabel1.Parent         := Self;
    WebLabel1.Name           := 'WebLabel1';
    WebLabel1.Left           := 64;
    WebLabel1.Top            := 40;
    WebLabel1.Width          := 533;
    WebLabel1.Height         := 32;
    WebLabel1.Caption        := 'Say good-bye to the world of native clients only. Welcome in the wonderful TMS Web world, bringing Delphi developers everywhere: native clients and web clients!';
    WebLabel1.WordWrap       := true;

    WebPaintBox1.Parent      := Self;
    WebPaintBox1.Name        := 'WebPaintBox1';
    WebPaintBox1.Left        := 64;
    WebPaintBox1.Top         := 87;
    WebPaintBox1.Width       := 235;
    WebPaintBox1.Height      := 137;
    WebPaintBox1.OnPaint     := @WebPaintBox1Paint;

    WebPanel1.Parent          := Self;
    WebPanel1.Name            := 'WebPanel1';
    WebPanel1.Left            := 64;
    WebPanel1.Top             := 272;
    WebPanel1.Width           := 513;
    WebPanel1.Height          := 89;
    WebPanel1.WidthStyle      := TSizeStyle.ssPercent;
    WebPanel1.WidthPercent    := 80;
    WebPanel1.BorderStyle     := TBorderStyle.bsSingle;

    WebLabel2.Parent           := WebPanel1;
    WebLabel2.Name             := 'WebLabel2';
    WebLabel2.Left             := 3;
    WebLabel2.Top              := 29;
    WebLabel2.Width            := 428;
    WebLabel2.Height           := 32;
    WebLabel2.Caption          :='With Pascal code you can draw on the TPaintBox control using the familiar TCanvas object';
    WebLabel2.WordWrap         := true;
    WebLabel2.WidthStyle       := TSizeStyle.ssPercent;

    WebImageControl1.Parent    := WebPanel1;
    WebImageControl1.Name      := 'WebImageControl1';
    WebImageControl1.Left      := 6;
    WebImageControl1.Top       := 7;
    WebImageControl1.Width     := 16;
    WebImageControl1.Height    := 16;
    WebImageControl1.AutoSize  := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');

    WebPaintBox2.Parent        := Self;
    WebPaintBox2.Name          := 'WebPaintBox2';
    WebPaintBox2.Left          := 342;
    WebPaintBox2.Top           := 87;
    WebPaintBox2.Width         := 235;
    WebPaintBox2.Height        := 137;
    WebPaintBox2.OnPaint       := @WebPaintBox2Paint;
  finally
    WebLabel1.EndUpdate;
    WebPaintBox1.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebImageControl1.EndUpdate;
    WebPaintBox2.EndUpdate;
  end;