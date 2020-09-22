  WebLabel4        := TLabel.Create(Self);
  WebLabel1        := TLabel.Create(Self);
  WebLabel2        := TLabel.Create(Self);
  WebListBox2      := TListBox.Create(Self);
  WebButton1       := TButton.Create(Self);
  WebEdit1         := TEdit.Create(Self);
  WebPanel1        := TPanel.Create(Self);
  WebLabel6        := TLabel.Create(WebPanel1);
  WebImageControl1 := TImageControl.Create(WebPanel1);

  WebLabel4.BeginUpdate;
  WebLabel1.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebListBox2.BeginUpdate;
  WebButton1.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebPanel1.BeginUpdate;
  WebLabel6.BeginUpdate;
  WebImageControl1.BeginUpdate;
  try
    Self.Name           := 'Form1';
    Self.Left           := 0;
    Self.Top            := 0;
    Self.Width          := 640;
    Self.Height         := 480;
    Self.Font.Charset   := 1;
    Self.Font.Color     := 0;
    Self.Font.Height    := -13;
    Self.Font.Name      := 'Tahoma';
    Self.Font.Style     := [fsBold];
    Self.FFormContainer := 'appcontent';
    Self.TabOrder       := 1;

    WebLabel4.Parent := Self;
    WebLabel4.Name := 'WebLabel4';
    WebLabel4.Left := 48;
    WebLabel4.Top := 16;
    WebLabel4.Width := 179;
    WebLabel4.Height := 23;
    WebLabel4.Caption := 'MessageDialog Demo';
    WebLabel4.Font.Charset := 1;
    WebLabel4.Font.Color := 0;
    WebLabel4.Font.Height := -19;
    WebLabel4.Font.Name := 'Tahoma';
    WebLabel4.Font.Style := [];
    WebLabel4.ParentFont := false;

    WebLabel1.Parent := Self;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 187;
    WebLabel1.Top := 114;
    WebLabel1.Width := 40;
    WebLabel1.Height := 16;
    WebLabel1.Caption := 'Result:';
    WebLabel1.Font.Charset := 1;
    WebLabel1.Font.Color := 0;
    WebLabel1.Font.Height := -13;
    WebLabel1.Font.Name := 'Tahoma';
    WebLabel1.Font.Style := [];
    WebLabel1.ParentFont := false;

    WebLabel2.Parent := Self;
    WebLabel2.Name := 'WebLabel2';
    WebLabel2.Left := 243;
    WebLabel2.Top := 114;
    WebLabel2.Width := 134;
    WebLabel2.Height := 16;
    WebLabel2.AutoSize := false;
    WebLabel2.Font.Charset := 1;
    WebLabel2.Font.Color := 0;
    WebLabel2.Font.Height := -13;
    WebLabel2.Font.Name := 'Tahoma';
    WebLabel2.Font.Style := [];
    WebLabel2.ParentFont := false;

    WebListBox2.Parent := Self;
    WebListBox2.Name := 'WebListBox2';
    WebListBox2.Left := 48;
    WebListBox2.Top := 114;
    WebListBox2.Width := 121;
    WebListBox2.Height := 97;
    WebListBox2.ItemIndex := 0;
    WebListBox2.Items.BeginUpdate;
    try
      WebListBox2.Items.Clear;
      WebListBox2.Items.Add('Warning');
      WebListBox2.Items.Add('Error');
      WebListBox2.Items.Add('Information');
      WebListBox2.Items.Add('Confirmation');
      WebListBox2.Items.Add('Custom');
    finally
      WebListBox2.Items.EndUpdate;
    end;
    WebListBox2.TabOrder := 0;

    WebButton1.Parent := Self;
    WebButton1.Name := 'WebButton1';
    WebButton1.Left := 48;
    WebButton1.Top := 217;
    WebButton1.Width := 121;
    WebButton1.Height := 25;
    WebButton1.Caption := 'Show message';
    WebButton1.OnClick := @WebButton1Click;
    WebButton1.TabOrder := 1;

    WebEdit1.Parent := Self;
    WebEdit1.Name := 'WebEdit1';
    WebEdit1.Left := 48;
    WebEdit1.Top := 87;
    WebEdit1.Width := 329;
    WebEdit1.Height := 24;
    WebEdit1.TabOrder := 2;
    WebEdit1.Text := 'Sample message text';
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
    WebLabel6.Width := 297;
    WebLabel6.Height := 32;
    WebLabel6.Caption :=
      'Demo showing various custom dialog types and the dialog response codes';
    WebLabel6.WordWrap := true;
    WebLabel6.WidthStyle := ssPercent;

    WebImageControl1.Parent := WebPanel1;
    WebImageControl1.Name := 'WebImageControl1';
    WebImageControl1.Left := 6;
    WebImageControl1.Top := 7;
    WebImageControl1.Width := 16;
    WebImageControl1.Height := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');
  finally
    WebLabel4.EndUpdate;
    WebLabel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebListBox2.EndUpdate;
    WebButton1.EndUpdate;
    WebEdit1.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel6.EndUpdate;
    WebImageControl1.EndUpdate;
  end;