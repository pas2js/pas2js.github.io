  WebLabel1 := TLabel.Create(Self);
  WebLabel2 := TLabel.Create(Self);
  WebButton2 := TButton.Create(Self);
  frm2Edit := TEdit.Create(Self);
  WebLabel1.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebButton2.BeginUpdate;
  frm2Edit.BeginUpdate;

  try
    Name := 'Form2';
    Left := 0;
    Top := 0;
    Width := 640;
    Height := 480;
    Font.Charset := 1;
    Font.Color := 0;
    Font.Height := -13;
    Font.Name := 'Tahoma';
    Font.Style := [];
    TabOrder := 1;

    WebLabel1.Parent := Self;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 40;
    WebLabel1.Top := 32;
    WebLabel1.Width := 90;
    WebLabel1.Height := 35;
    WebLabel1.Caption := 'Form 2';
    WebLabel1.Font.Charset := 1;
    WebLabel1.Font.Color := 255;
    WebLabel1.Font.Height := -29;
    WebLabel1.Font.Name := 'Tahoma';
    WebLabel1.Font.Style := [];
    WebLabel1.ParentFont := false;

    WebLabel2.Parent := (Self);
    WebLabel2.Name := 'WebLabel2';
    WebLabel2.Left := 40;
    WebLabel2.Top := 123;
    WebLabel2.Width := 75;
    WebLabel2.Height := 16;
    WebLabel2.Caption := 'Preset value:';

    WebButton2.Parent := (Self);
    WebButton2.Name := 'WebButton2';
    WebButton2.Left := 40;
    WebButton2.Top := 184;
    WebButton2.Width := 281;
    WebButton2.Height := 57;
    WebButton2.Caption := 'Close form';
    WebButton2.OnClick := @WebButton2Click;
    WebButton2.TabOrder := 0;

    frm2Edit.Parent := Self;
    frm2Edit.Name := 'frm2Edit';
    frm2Edit.Left := 121;
    frm2Edit.Top := 120;
    frm2Edit.Width := 209;
    frm2Edit.Height := 24;
    frm2Edit.TabOrder := 1;
  finally
    WebLabel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebButton2.EndUpdate;
    frm2Edit.EndUpdate;
  end;