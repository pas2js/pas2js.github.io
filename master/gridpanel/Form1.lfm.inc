  WebLabel5 := TLabel.Create(Self);
  WebGridPanel1 := TGridPanel.Create(Self);
  WebLabel1 := TLabel.Create(WebGridPanel1);
  WebLabel2 := TLabel.Create(WebGridPanel1);
  WebLabel3 := TLabel.Create(WebGridPanel1);
  WebLabel4 := TLabel.Create(WebGridPanel1);
  WebEdit1 := TEdit.Create(WebGridPanel1);
  WebButton1 := TButton.Create(WebGridPanel1);
  WebEdit2 := TEdit.Create(WebGridPanel1);
  WebButton2 := TButton.Create(WebGridPanel1);
  WebEdit3 := TEdit.Create(WebGridPanel1);
  WebButton3 := TButton.Create(WebGridPanel1);
  WebEdit4 := TEdit.Create(WebGridPanel1);
  WebButton4 := TButton.Create(WebGridPanel1);
  WebPanel1 := TPanel.Create(Self);
  WebLabel6 := TLabel.Create(WebPanel1);
  WebImageControl1 := TImageControl.Create(WebPanel1);

  WebLabel5.BeginUpdate;
  WebGridPanel1.BeginUpdate;
  WebLabel1.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebLabel3.BeginUpdate;
  WebLabel4.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebButton1.BeginUpdate;
  WebEdit2.BeginUpdate;
  WebButton2.BeginUpdate;
  WebEdit3.BeginUpdate;
  WebButton3.BeginUpdate;
  WebEdit4.BeginUpdate;
  WebButton4.BeginUpdate;
  WebPanel1.BeginUpdate;
  WebLabel6.BeginUpdate;
  WebImageControl1.BeginUpdate;
  try
    Name := 'Form1';
    Left := 0;
    Top := 0;
    Width := 640;
    Height := 480;
    Font.CharSet := 1;
    Font.Color := 0;
    Font.Height := -13;
    Font.Name := 'Tahoma';
    Font.Style := [];
    FFormContainer := 'appcontent';
    TabOrder := 1;

    WebLabel5.Parent := Self;
    WebLabel5.Name := 'WebLabel5';
    WebLabel5.Left := 70;
    WebLabel5.Top := 32;
    WebLabel5.Width := 186;
    WebLabel5.Height := 23;
    WebLabel5.Caption := 'Grid Panel component';
    WebLabel5.Font.CharSet := 1;
    WebLabel5.Font.Color := 0;
    WebLabel5.Font.Height := -19;
    WebLabel5.Font.Name := 'Tahoma';
    WebLabel5.Font.Style := [];
    WebLabel5.ParentFont := false;

    with WebGridPanel1 do
    begin
      Parent := Self;
      Name := 'WebGridPanel1';
      Left := 67;
      Top := 64;
      Width := 510;
      Height := 217;
      Color := 16777215;
      GridLineWidth := 1;
      GridLineColor := 12632256;

      ColumnCollection.Clear;
      with TGridPanelColumn(ColumnCollection.Add) do
      begin
        MarginLeft := 3;
        MarginRight := 3;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;

      with TGridPanelColumn(ColumnCollection.Add) do
      begin
        Alignment := TAlignment.taCenter;
        MarginLeft := 3;
        MarginRight := 3;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 50;
      end;

      with TGridPanelColumn(ColumnCollection.Add) do
      begin
        Alignment := TAlignment.taRightJustify;
        MarginLeft := 3;
        MarginRight := 3;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;

      ControlCollection.Clear;
      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebLabel1;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebEdit1;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebButton1;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebLabel2;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebEdit2;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebButton2;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebLabel3;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebEdit3;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebButton3;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebLabel4;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebEdit4;
      end;

      with TControlCollectionItem(ControlCollection.Add) do
      begin
        Column := 0;
        Row := 0;
        Control := WebButton4;
      end;

      RowCollection.Clear;
      with TGridPanelRow(RowCollection.Add) do
      begin
        Alignment := TVerticalAlignment.vaCenter;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;
      with TGridPanelRow(RowCollection.Add) do
      begin
        Alignment := TVerticalAlignment.vaCenter;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;
      with TGridPanelRow(RowCollection.Add) do
      begin
        Alignment := TVerticalAlignment.vaCenter;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;
      with TGridPanelRow(RowCollection.Add) do
      begin
        Alignment := TVerticalAlignment.vaCenter;
        SizeStyle := TSizeStyle.ssPercent;
        Value := 25;
      end;
    end;

    WebLabel1.Parent := WebGridPanel1;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 5;
    WebLabel1.Top := 19;
    WebLabel1.Width := 66;
    WebLabel1.Height := 16;
    WebLabel1.Caption := 'First name:';

    WebLabel2.Parent := WebGridPanel1;
    WebLabel2.Name := 'WebLabel2';
    WebLabel2.Left := 5;
    WebLabel2.Top := 73;
    WebLabel2.Width := 64;
    WebLabel2.Height := 16;
    WebLabel2.Caption := 'Last name:';

    WebLabel3.Parent := WebGridPanel1;
    WebLabel3.Name := 'WebLabel3';
    WebLabel3.Left := 5;
    WebLabel3.Top := 127;
    WebLabel3.Width := 36;
    WebLabel3.Height := 16;
    WebLabel3.Caption := 'Email:';

    WebLabel4.Parent := WebGridPanel1;
    WebLabel4.Name := 'WebLabel4';
    WebLabel4.Left := 5;
    WebLabel4.Top := 181;
    WebLabel4.Width := 49;
    WebLabel4.Height := 16;
    WebLabel4.Caption := 'Country:';

    WebEdit1.Parent := WebGridPanel1;
    WebEdit1.Name := 'WebEdit1';
    WebEdit1.Left := 156;
    WebEdit1.Top := 15;
    WebEdit1.Width := 196;
    WebEdit1.Height := 24;
    WebEdit1.TabOrder := 0;
    WebEdit1.WidthStyle := TSizeStyle.ssPercent;
    WebEdit1.WidthPercent := 80;

    WebButton1.Parent := WebGridPanel1;
    WebButton1.Name := 'WebButton1';
    WebButton1.Left := 434;
    WebButton1.Top := 12;
    WebButton1.Width := 70;
    WebButton1.Height := 30;
    WebButton1.Caption := 'Edit';
    WebButton1.TabOrder := 1;
    WebButton1.WidthStyle := TSizeStyle.ssPercent;
    WebButton1.WidthPercent := 60;

    WebEdit2.Parent := WebGridPanel1;
    WebEdit2.Name := 'WebEdit2';
    WebEdit2.Left := 156;
    WebEdit2.Top := 69;
    WebEdit2.Width := 196;
    WebEdit2.Height := 24;
    WebEdit2.TabOrder := 2;
    WebEdit2.WidthStyle := TSizeStyle.ssPercent;
    WebEdit2.WidthPercent := 80;

    WebButton2.Parent := WebGridPanel1;
    WebButton2.Name := 'WebButton2';
    WebButton2.Left := 434;
    WebButton2.Top := 66;
    WebButton2.Width := 70;
    WebButton2.Height := 30;
    WebButton2.Caption := 'Edit';
    WebButton2.TabOrder := 3;
    WebButton2.WidthStyle := TSizeStyle.ssPercent;
    WebButton2.WidthPercent := 60;

    WebEdit3.Parent := WebGridPanel1;
    WebEdit3.Name := 'WebEdit3';
    WebEdit3.Left := 156;
    WebEdit3.Top := 123;
    WebEdit3.Width := 196;
    WebEdit3.Height := 24;
    WebEdit3.TabOrder := 4;
    WebEdit3.WidthStyle := TSizeStyle.ssPercent;
    WebEdit3.WidthPercent := 80;

    WebButton3.Parent := WebGridPanel1;
    WebButton3.Name := 'WebButton3';
    WebButton3.Left := 434;
    WebButton3.Top := 120;
    WebButton3.Width := 70;
    WebButton3.Height := 30;
    WebButton3.Caption := 'Edit';
    WebButton3.TabOrder := 5;
    WebButton3.WidthStyle := TSizeStyle.ssPercent;
    WebButton3.WidthPercent := 60;

    WebEdit4.Parent := WebGridPanel1;
    WebEdit4.Name := 'WebEdit4';
    WebEdit4.Left := 156;
    WebEdit4.Top := 177;
    WebEdit4.Width := 196;
    WebEdit4.Height := 24;
    WebEdit4.TabOrder := 6;
    WebEdit4.WidthStyle := TSizeStyle.ssPercent;
    WebEdit4.WidthPercent := 80;

    WebButton4.Parent := WebGridPanel1;
    WebButton4.Name := 'WebButton4';
    WebButton4.Left := 434;
    WebButton4.Top := 174;
    WebButton4.Width := 70;
    WebButton4.Height := 30;
    WebButton4.Caption := 'Edit';
    WebButton4.TabOrder := 7;
    WebButton4.WidthStyle := TSizeStyle.ssPercent;
    WebButton4.WidthPercent := 60;

    WebPanel1.Parent := Self;
    WebPanel1.Name := 'WebPanel1';
    WebPanel1.Left := 64;
    WebPanel1.Top := 304;
    WebPanel1.Width := 513;
    WebPanel1.Height := 89;
    WebPanel1.WidthStyle := TSizeStyle.ssPercent;
    WebPanel1.WidthPercent := 80;
    WebPanel1.BorderStyle := TBorderStyle.bsSingle;

    WebLabel6.Parent := WebPanel1;
    WebLabel6.Name := 'WebLabel6';
    WebLabel6.Left := 3;
    WebLabel6.Top := 29;
    WebLabel6.Width := 397;
    WebLabel6.Height := 32;
    WebLabel6.Caption :=
      'The GridPanel allows easy control of alignment and sizing of controls within the panel.';
    WebLabel6.WordWrap := true;
    WebLabel6.WidthStyle := TSizeStyle.ssPercent;

    WebImageControl1.Parent := WebPanel1;
    WebImageControl1.Name := 'WebImageControl1';
    WebImageControl1.Left := 6;
    WebImageControl1.Top := 7;
    WebImageControl1.Width := 16;
    WebImageControl1.Height := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');
  finally
    WebLabel5.EndUpdate;
    WebGridPanel1.EndUpdate;
    WebLabel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebLabel3.EndUpdate;
    WebLabel4.EndUpdate;
    WebEdit1.EndUpdate;
    WebButton1.EndUpdate;
    WebEdit2.EndUpdate;
    WebButton2.EndUpdate;
    WebEdit3.EndUpdate;
    WebButton3.EndUpdate;
    WebEdit4.EndUpdate;
    WebButton4.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel6.EndUpdate;
    WebImageControl1.EndUpdate;
  end;