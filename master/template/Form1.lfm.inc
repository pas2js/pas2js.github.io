  WebLabel1 := TLabel.Create('lbl');
  WebEdit1 := TEdit.Create('edt');
  WebButton1 := TButton.Create('btn');
  WebMemo1 := TMemo.Create('mem');
  WebComboBox1 := TComboBox.Create('sel');

  WebLabel1.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebButton1.BeginUpdate;
  WebMemo1.BeginUpdate;
  WebComboBox1.BeginUpdate;
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
    TabOrder := 1;

    WebLabel1.Parent := Self;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 64;
    WebLabel1.Top := 232;
    WebLabel1.Width := 63;
    WebLabel1.Height := 16;
    WebLabel1.Caption := 'WebLabel1';

    WebEdit1.Parent := Self;
    WebEdit1.Name := 'WebEdit1';
    WebEdit1.Left := 64;
    WebEdit1.Top := 42;
    WebEdit1.Width := 121;
    WebEdit1.Height := 24;
    WebEdit1.CharCase := TEditCharCase.wecLowerCase;
    WebEdit1.TabOrder := 0;

    WebButton1.Parent := Self;
    WebButton1.Name := 'WebButton1';
    WebButton1.Left := 200;
    WebButton1.Top := 40;
    WebButton1.Width := 75;
    WebButton1.Height := 25;
    WebButton1.Caption := 'Add';
    WebButton1.OnClick := @WebButton1Click;
    WebButton1.TabOrder := 1;

    WebMemo1.Parent := Self;
    WebMemo1.Name := 'WebMemo1';
    WebMemo1.Left := 64;
    WebMemo1.Top := 88;
    WebMemo1.Width := 211;
    WebMemo1.Height := 89;
    WebMemo1.AutoSize := false;
    WebMemo1.SelLength := 0;
    WebMemo1.SelStart := 0;
    WebMemo1.TabOrder := 2;

    WebComboBox1.Parent := Self;
    WebComboBox1.Name := 'WebComboBox1';
    WebComboBox1.Left := 64;
    WebComboBox1.Top := 191;
    WebComboBox1.Width := 211;
    WebComboBox1.Height := 24;
    WebComboBox1.ItemIndex := -1;
    WebComboBox1.TabOrder := 3;
    WebComboBox1.Text := 'WebComboBox1';
    WebComboBox1.OnChange := @WebComboBox1Change;
  finally
    WebLabel1.EndUpdate;
    WebEdit1.EndUpdate;
    WebButton1.EndUpdate;
    WebMemo1.EndUpdate;
    WebComboBox1.EndUpdate;
  end;