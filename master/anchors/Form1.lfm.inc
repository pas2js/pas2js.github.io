  WebPanel1 := TPanel.Create(Self);
  WebLabel1 := TLabel.Create(WebPanel1);
  WebLabel2 := TLabel.Create(WebPanel1);
  WebEdit1 := TEdit.Create(WebPanel1);
  WebEdit2 := TEdit.Create(WebPanel1);
  WebMemo1 := TMemo.Create(WebPanel1);
  WebPanel2 := TPanel.Create(Self);
  WebLabel6 := TLabel.Create(WebPanel2);
  WebImageControl1 := TImageControl.Create(WebPanel2);

  WebPanel1.BeginUpdate;
  WebLabel1.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebEdit2.BeginUpdate;
  WebMemo1.BeginUpdate;
  WebPanel2.BeginUpdate;
  WebLabel6.BeginUpdate;
  WebImageControl1.BeginUpdate;

  try
    Self.Name := 'Form1';
    Self.Left := 0;
    Self.Top := 0;
    Self.Width := 675;
    Self.Height := 500;
    Self.Font.Charset := 1;
    Self.Font.Color := 0;
    Self.Font.Height := -13;
    Self.Font.Name := 'Tahoma';
    Self.Font.Style := [];
    Self.FFormContainer := 'appcontent';
    Self.TabOrder := 1;

    WebPanel1.Parent := Self;
    WebPanel1.Name := 'WebPanel1';
    WebPanel1.Left := 24;
    WebPanel1.Top := 24;
    WebPanel1.Width := 410;
    WebPanel1.Height := 281;
    WebPanel1.Anchors := [akLeft,akTop,akRight,akBottom];
    WebPanel1.BorderStyle := bsSingle;
    WebPanel1.Caption := 'WebPanel1';

    WebLabel1.Parent := WebPanel1;
    WebLabel1.Name := 'WebLabel1';
    WebLabel1.Left := 24;
    WebLabel1.Top := 24;
    WebLabel1.Width := 120;
    WebLabel1.Height := 16;
    WebLabel1.Caption := 'Left anchored label 1';

    WebLabel2.Parent := WebPanel1;
    WebLabel2.Name := 'WebLabel2';
    WebLabel2.Left := 24;
    WebLabel2.Top := 64;
    WebLabel2.Width := 120;
    WebLabel2.Height := 16;
    WebLabel2.Caption := 'Left anchored label 2';

    WebEdit1.Parent := WebPanel1;
    WebEdit1.Name := 'WebEdit1';
    WebEdit1.Left := 268;
    WebEdit1.Top := 21;
    WebEdit1.Width := 121;
    WebEdit1.Height := 24;
    WebEdit1.Anchors := [akTop,akRight];
    WebEdit1.TabOrder := 0;
    WebEdit1.Text := 'Anchor right';

    WebEdit2.Parent := WebPanel1;
    WebEdit2.Name := 'WebEdit2';
    WebEdit2.Left := 268;
    WebEdit2.Top := 61;
    WebEdit2.Width := 121;
    WebEdit2.Height := 24;
    WebEdit2.Anchors := [akLeft,akTop,akRight];
    WebEdit2.TabOrder := 1;
    WebEdit2.Text := 'Anchor left,right';

    WebMemo1.Parent := WebPanel1;
    WebMemo1.Name := 'WebMemo1';
    WebMemo1.Left := 24;
    WebMemo1.Top := 120;
    WebMemo1.Width := 365;
    WebMemo1.Height := 142;
    WebMemo1.Anchors := [akLeft,akTop,akRight,akBottom];
    WebMemo1.AutoSize := false;
    WebMemo1.Lines.BeginUpdate;
    try
      WebMemo1.Lines.Clear;
      WebMemo1.Lines.Add('Left,Top \/ Right,Bottom anchored memo');
    finally
      WebMemo1.Lines.EndUpdate;
    end;
    WebMemo1.SelLength := 0;
    WebMemo1.SelStart := 39;
    WebMemo1.TabOrder := 2;

    WebPanel2.Parent := Self;
    WebPanel2.Name := 'WebPanel2';
    WebPanel2.Left := 24;
    WebPanel2.Top := 404;
    WebPanel2.Width := 513;
    WebPanel2.Height := 89;
    WebPanel2.WidthStyle := ssPercent;
    WebPanel2.WidthPercent := 80;
    WebPanel2.BorderStyle := bsSingle;

    WebLabel6.Parent := WebPanel2;
    WebLabel6.Name := 'WebLabel6';
    WebLabel6.Left := 3;
    WebLabel6.Top := 29;
    WebLabel6.Width := 445;
    WebLabel6.Height := 32;
    WebLabel6.Caption :=  'Self demo shows anchoring of controls on the web form. Resize the browser window to see the effect.';
    WebLabel6.WordWrap := true;
    WebLabel6.WidthStyle := ssPercent;

    WebImageControl1.Parent := WebPanel2;
    WebImageControl1.Name := 'WebImageControl1';
    WebImageControl1.Left := 6;
    WebImageControl1.Top := 7;
    WebImageControl1.Width := 16;
    WebImageControl1.Height := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');
  finally
    WebPanel1.EndUpdate;
    WebLabel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebEdit1.EndUpdate;
    WebEdit2.EndUpdate;
    WebMemo1.EndUpdate;
    WebPanel2.EndUpdate;
    WebLabel6.EndUpdate;
    WebImageControl1.EndUpdate;
  end;