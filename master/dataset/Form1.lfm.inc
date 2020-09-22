  WebDBLabel1:=           TDBLabel.Create(Self);
  WebLabel1:=             TLabel.Create(Self);
  WebLabel2:=             TLabel.Create(Self);
  WebLabel3:=             TLabel.Create(Self);
  WebLabel4:=             TLabel.Create(Self);
  WebLabel6:=             TLabel.Create(Self);
  WebLabel7:=             TLabel.Create(Self);
  WebLabel5:=             TLabel.Create(Self);
  WebButton1:=            TButton.Create(Self);
  WebDBNavigator1:=       TDBNavigator.Create(Self);
  WebDBEdit1:=            TDBEdit.Create(Self);
  WebDBEdit2:=            TDBEdit.Create(Self);
  WebDBEdit3:=            TDBEdit.Create(Self);
  WebDBEdit4:=            TDBEdit.Create(Self);
  WebPanel1:=             TPanel.Create(Self);
  WebLabel9:=             TLabel.Create(WebPanel1);
  WebImageControl1:=      TImageControl.Create(WebPanel1);
  WebDBMemo1:=            TDBMemo.Create(Self);
  WebDBSpinEdit1:=        TDBSpinEdit.Create(Self);

  WebClientConnection1:=  TClientConnection.Create(Self);
  WebClientDataSet1:=     TClientDataSet.Create(Self);
  WebClientDataSource1:=  TClientDataSource.Create(Self);

  WebDBLabel1.BeginUpdate;
  WebLabel1.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebLabel3.BeginUpdate;
  WebLabel4.BeginUpdate;
  WebLabel6.BeginUpdate;
  WebLabel7.BeginUpdate;
  WebLabel5.BeginUpdate;
  WebButton1.BeginUpdate;
  WebDBNavigator1.BeginUpdate;
  WebDBEdit1.BeginUpdate;
  WebDBEdit2.BeginUpdate;
  WebDBEdit3.BeginUpdate;
  WebDBEdit4.BeginUpdate;
  WebPanel1.BeginUpdate;
  WebLabel9.BeginUpdate;
  WebImageControl1.BeginUpdate;
  WebDBMemo1.BeginUpdate;
  WebDBSpinEdit1.BeginUpdate;

  try
    Self.Name := 'Form1';
    Self.Left := 0;
    Self.Top := 0;
    Self.Width := 775;
    Self.Height := 575;
    Self.Font.Charset := 1;
    Self.Font.Color := 0;
    Self.Font.Height := -13;
    Self.Font.Name := 'Tahoma';
    Self.Font.Style := [];
    Self.FFormContainer := 'appcontent';
    Self.TabOrder := 1;

    WebDBLabel1.Parent := Self;
    WebDBLabel1.Name := 'WebDBLabel1';
    WebDBLabel1.Left := 132;
    WebDBLabel1.Top := 257;
    WebDBLabel1.Width := 457;
    WebDBLabel1.Height := 22;
    WebDBLabel1.AutoSize := false;
    WebDBLabel1.Caption := 'WebDBLabel1';
    WebDBLabel1.EllipsisPosition := epEndEllipsis;
    WebDBLabel1.DataField := '_Length_In';
    WebDBLabel1.DataSource := WebClientDataSource1;

    WebLabel1.Parent := Self;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 16;
    WebLabel1.Top := 98;
    WebLabel1.Width := 68;
    WebLabel1.Height := 16;
    WebLabel1.Caption := 'Species No:';

    WebLabel2.Parent := Self;
    WebLabel2.Name := 'WebLabel2';
    WebLabel2.Left := 16;
    WebLabel2.Top := 130;
    WebLabel2.Width := 56;
    WebLabel2.Height := 16;
    WebLabel2.Caption := 'Category:';

    WebLabel3.Parent := Self;
    WebLabel3.Name := 'WebLabel3';
    WebLabel3.Left := 16;
    WebLabel3.Top := 162;
    WebLabel3.Width := 93;
    WebLabel3.Height := 16;
    WebLabel3.Caption := 'Common Name:';

    WebLabel4.Parent := Self;
    WebLabel4.Name := 'WebLabel4';
    WebLabel4.Left := 16;
    WebLabel4.Top := 195;
    WebLabel4.Width := 86;
    WebLabel4.Height := 16;
    WebLabel4.Caption := 'Species Name:';

    WebLabel6.Parent := Self;
    WebLabel6.Name := 'WebLabel6';
    WebLabel6.Left := 16;
    WebLabel6.Top := 226;
    WebLabel6.Width := 64;
    WebLabel6.Height := 16;
    WebLabel6.Caption := 'Length cm:';

    WebLabel7.Parent := Self;
    WebLabel7.Name := 'WebLabel7';
    WebLabel7.Left := 16;
    WebLabel7.Top := 257;
    WebLabel7.Width := 58;
    WebLabel7.Height := 16;
    WebLabel7.Caption := 'Length In:';

    WebLabel5.Parent := Self;
    WebLabel5.Name := 'WebLabel5';
    WebLabel5.Left := 16;
    WebLabel5.Top := 288;
    WebLabel5.Width := 37;
    WebLabel5.Height := 16;
    WebLabel5.Caption := 'Notes:';

    WebButton1.Parent := Self;
    WebButton1.Name := 'WebButton1';
    WebButton1.Left := 16;
    WebButton1.Top := 16;
    WebButton1.Width := 153;
    WebButton1.Height := 25;
    WebButton1.Caption := 'Connect to DB';
    WebButton1.OnClick := @WebButton1Click;
    WebButton1.TabOrder := 0;

    WebDBNavigator1.Parent := Self;
    WebDBNavigator1.Name := 'WebDBNavigator1';
    WebDBNavigator1.Left := 132;
    WebDBNavigator1.Top := 60;
    WebDBNavigator1.Width := 288;
    WebDBNavigator1.Height := 25;
    WebDBNavigator1.DataSource := WebClientDataSource1;

    WebDBEdit1.Parent := Self;
    WebDBEdit1.Name := 'WebDBEdit1';
    WebDBEdit1.Left := 132;
    WebDBEdit1.Top := 95;
    WebDBEdit1.Width := 457;
    WebDBEdit1.Height := 24;
    WebDBEdit1.AutoSelect := false;
    WebDBEdit1.Color := 16777215;
    WebDBEdit1.HideSelection := false;
    WebDBEdit1.TabOrder := 2;
    WebDBEdit1.Text := 'WebDBEdit1';
    WebDBEdit1.DataField := '_Species_No';
    WebDBEdit1.DataSource := WebClientDataSource1;

    WebDBEdit2.Parent := Self;
    WebDBEdit2.Name := 'WebDBEdit2';
    WebDBEdit2.Left := 132;
    WebDBEdit2.Top := 127;
    WebDBEdit2.Width := 457;
    WebDBEdit2.Height := 24;
    WebDBEdit2.AutoSelect := false;
    WebDBEdit2.Color := 16777215;
    WebDBEdit2.HideSelection := false;
    WebDBEdit2.TabOrder := 3;
    WebDBEdit2.Text := 'WebDBEdit1';
    WebDBEdit2.DataField := '_Category';
    WebDBEdit2.DataSource := WebClientDataSource1;

    WebDBEdit3.Parent := Self;
    WebDBEdit3.Name := 'WebDBEdit3';
    WebDBEdit3.Left := 132;
    WebDBEdit3.Top := 159;
    WebDBEdit3.Width := 457;
    WebDBEdit3.Height := 24;
    WebDBEdit3.AutoSelect := false;
    WebDBEdit3.Color := 16777215;
    WebDBEdit3.HideSelection := false;
    WebDBEdit3.TabOrder := 4;
    WebDBEdit3.Text := 'WebDBEdit1';
    WebDBEdit3.DataField := '_Common_Name';
    WebDBEdit3.DataSource := WebClientDataSource1;

    WebDBEdit4.Parent := Self;
    WebDBEdit4.Name := 'WebDBEdit4';
    WebDBEdit4.Left := 132;
    WebDBEdit4.Top := 192;
    WebDBEdit4.Width := 457;
    WebDBEdit4.Height := 24;
    WebDBEdit4.AutoSelect := false;
    WebDBEdit4.Color := 16777215;
    WebDBEdit4.HideSelection := false;
    WebDBEdit4.TabOrder := 5;
    WebDBEdit4.Text := 'WebDBEdit1';
    WebDBEdit4.DataField := '_Species_Name';
    WebDBEdit4.DataSource := WebClientDataSource1;

    WebPanel1.Parent := Self;
    WebPanel1.Name := 'WebPanel1';
    WebPanel1.Left := 16;
    WebPanel1.Top := 465;
    WebPanel1.Width := 541;
    WebPanel1.Height := 89;
    WebPanel1.WidthStyle := ssPercent;
    WebPanel1.WidthPercent := 80;
    WebPanel1.BorderStyle := bsSingle;

    WebLabel9.Parent := WebPanel1;
    WebLabel9.Name := 'WebLabel9';
    WebLabel9.Left := 3;
    WebLabel9.Top := 29;
    WebLabel9.Width := 460;
    WebLabel9.Height := 48;
    WebLabel9.Caption :=
      'Self demo shows a web client dataset connected to DB controls. The web client dataset gets the information from an Client server but for demo purposes all editing in the dataset is local in the web client only!';
    WebLabel9.WordWrap := true;
    WebLabel9.WidthStyle := ssPercent;

    WebImageControl1.Parent := WebPanel1;
    WebImageControl1.Name := 'WebImageControl1';
    WebImageControl1.Left := 6;
    WebImageControl1.Top := 7;
    WebImageControl1.Width := 16;
    WebImageControl1.Height := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');

    WebDBMemo1.Parent := Self;
    WebDBMemo1.Name := 'WebDBMemo1';
    WebDBMemo1.Left := 132;
    WebDBMemo1.Top := 285;
    WebDBMemo1.Width := 457;
    WebDBMemo1.Height := 140;
    WebDBMemo1.AutoSize := false;
    WebDBMemo1.Lines.BeginUpdate;
    try
      WebDBMemo1.Lines.Clear;
      WebDBMemo1.Lines.Add('WebDBMemo1');
    finally
      WebDBMemo1.Lines.EndUpdate;
    end;
    WebDBMemo1.SelLength := 0;
    WebDBMemo1.SelStart := 0;
    WebDBMemo1.TabOrder := 7;
    WebDBMemo1.DataField := '_Notes';
    WebDBMemo1.DataSource := WebClientDataSource1;

    WebDBSpinEdit1.Parent := Self;
    WebDBSpinEdit1.Name := 'WebDBSpinEdit1';
    WebDBSpinEdit1.Left := 132;
    WebDBSpinEdit1.Top := 223;
    WebDBSpinEdit1.Width := 150;
    WebDBSpinEdit1.Height := 22;
    WebDBSpinEdit1.AutoSize := false;
    WebDBSpinEdit1.BorderStyle := bsSingle;
    WebDBSpinEdit1.Color := 16777215;
    WebDBSpinEdit1.Increment := 1;
    WebDBSpinEdit1.MaxValue := 100;
    WebDBSpinEdit1.MinValue := 0;
    WebDBSpinEdit1.TabOrder := 8;
    WebDBSpinEdit1.Value := 0;
    WebDBSpinEdit1.DataField := '_Length__cm_';
    WebDBSpinEdit1.DataSource := WebClientDataSource1;

    WebClientConnection1.Name := 'WebClientConnection1';
    WebClientConnection1.Active := false;
    WebClientDataSet1.Name := 'WebClientDataSet1';
    WebClientDataSet1.Connection := WebClientConnection1;

    WebClientDataSource1.Name := 'WebClientDataSource1';
    WebClientDataSource1.DataSet := WebClientDataSet1;
  finally
    WebDBLabel1.EndUpdate;
    WebLabel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebLabel3.EndUpdate;
    WebLabel4.EndUpdate;
    WebLabel6.EndUpdate;
    WebLabel7.EndUpdate;
    WebLabel5.EndUpdate;
    WebButton1.EndUpdate;
    WebDBNavigator1.EndUpdate;
    WebDBEdit1.EndUpdate;
    WebDBEdit2.EndUpdate;
    WebDBEdit3.EndUpdate;
    WebDBEdit4.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel9.EndUpdate;
    WebImageControl1.EndUpdate;
    WebDBMemo1.EndUpdate;
    WebDBSpinEdit1.EndUpdate;
  end;