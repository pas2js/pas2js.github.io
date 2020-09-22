  WebLabel1 := TLabel.Create(Self);
  WebButton1 := TButton.Create(Self);
  WebEdit1 := TEdit.Create(Self);
  WebCheckBox1 := TCheckBox.Create(Self);
  WebPanel1 := TPanel.Create(Self);
  WebLabel6 := TLabel.Create(WebPanel1);
  WebImageControl1 := TImageControl.Create(WebPanel1);

  WebLabel1.BeginUpdate;
  WebButton1.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebCheckBox1.BeginUpdate;
  WebPanel1.BeginUpdate;
  WebLabel6.BeginUpdate;
  WebImageControl1.BeginUpdate;
  try
    Name := 'Form1';
    Left := 0;
    Top := 0;
    Width := 640;
    Height := 480;
    Caption := 'Hello world';
    Font.Charset := 1;
    Font.Color := 0;
    Font.Height := -13;
    Font.Name := 'Tahoma';
    Font.Style := [];
    FFormContainer := 'appcontent';
    TabOrder := 1;

    WebLabel1.Parent := Self;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 40;
    WebLabel1.Top := 32;
    WebLabel1.Width := 90;
    WebLabel1.Height := 35;
    WebLabel1.Caption := 'Form 1';
    WebLabel1.Font.Charset := 1;
    WebLabel1.Font.Color := 255;
    WebLabel1.Font.Height := -29;
    WebLabel1.Font.Name := 'Tahoma';
    WebLabel1.Font.Style := [];
    WebLabel1.ParentFont := false;

    WebButton1.Parent := Self;
    WebButton1.Name := 'WebButton1';
    WebButton1.Left := 40;
    WebButton1.Top := 184;
    WebButton1.Width := 281;
    WebButton1.Height := 57;
    WebButton1.Caption := 'Click here to open the second form';
    WebButton1.OnClick := @WebButton1Click;
    WebButton1.TabOrder := 0;

    WebEdit1.Parent := Self;
    WebEdit1.Name := 'WebEdit1';
    WebEdit1.Left := 40;
    WebEdit1.Top := 88;
    WebEdit1.Width := 177;
    WebEdit1.Height := 24;
    WebEdit1.TabOrder := 1;
    WebEdit1.Text := 'sample text edit';

    WebCheckBox1.Parent := Self;
    WebCheckBox1.Name := 'WebCheckBox1';
    WebCheckBox1.Left := 40;
    WebCheckBox1.Top := 154;
    WebCheckBox1.Width := 177;
    WebCheckBox1.Height := 17;
    WebCheckBox1.Caption := 'Show form as popup';
    WebCheckBox1.State := cbUnchecked;
    WebCheckBox1.TabOrder := 2;

    WebPanel1.Parent := Self;
    WebPanel1.Name := 'WebPanel1';
    WebPanel1.Left := 40;
    WebPanel1.Top := 287;
    WebPanel1.Width := 513;
    WebPanel1.Height := 89;
    WebPanel1.WidthStyle := ssPercent;
    WebPanel1.WidthPercent := 80;
    WebPanel1.BorderStyle := bsSingle;

    WebLabel6.Parent := WebPanel1;
    WebLabel6.Name := 'WebLabel6';
    WebLabel6.Left := 6;
    WebLabel6.Top := 29;
    WebLabel6.Width := 479;
    WebLabel6.Height := 32;
    WebLabel6.Caption :=
      'Demo showing a web application consisting of two form files and showing the form either on top of the main form or launching it as a new web page';
    WebLabel6.WordWrap := true;
    WebLabel6.WidthStyle := ssPercent;

    WebImageControl1.Parent := (WebPanel1);
    WebImageControl1.Name := 'WebImageControl1';
    WebImageControl1.Left := 6;
    WebImageControl1.Top := 7;
    WebImageControl1.Width := 16;
    WebImageControl1.Height := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');
  finally
    WebLabel1.EndUpdate;
    WebButton1.EndUpdate;
    WebEdit1.EndUpdate;
    WebCheckBox1.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel6.EndUpdate;
    WebImageControl1.EndUpdate;
  end;