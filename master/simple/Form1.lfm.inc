  WebLabel1        := TLabel.Create(Self);
  WebEdit1         := TEdit.Create(Self);
  WebButton1       := TButton.Create(Self);
  WebMemo1         := TMemo.Create(Self);
  WebComboBox1     := TComboBox.Create(Self);
  WebPanel1        := TPanel.Create(Self);
  WebLabel2        := TLabel.Create(WebPanel1);
  WebImageControl1 := TImageControl.Create(WebPanel1);

  WebLabel1.BeginUpdate;
  WebEdit1.BeginUpdate;
  WebButton1.BeginUpdate;
  WebMemo1.BeginUpdate;
  WebComboBox1.BeginUpdate;
  WebPanel1.BeginUpdate;
  WebLabel2.BeginUpdate;
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

    WebLabel1.Parent  := Self;
    WebLabel1.Name    := 'WebLabel1';
    WebLabel1.Left    := 64;
    WebLabel1.Top     := 232;
    WebLabel1.Width   := 40;
    WebLabel1.Height  := 16;
    WebLabel1.Caption := '-Label-';
    WebEdit1.Parent   := Self;

    WebEdit1.Name     := 'WebEdit1';
    WebEdit1.Left     := 64;
    WebEdit1.Top      := 42;
    WebEdit1.Width    := 121;
    WebEdit1.Height   := 24;
    WebEdit1.TabOrder := 0;
    WebEdit1.TextHint := 'Add some text ...';

    WebButton1.Parent   := Self;
    WebButton1.Name     := 'WebButton1';
    WebButton1.Left     := 204;
    WebButton1.Top      := 40;
    WebButton1.Width    := 75;
    WebButton1.Height   := 25;
    WebButton1.Caption  := 'Add';
    WebButton1.OnClick := @WebButton1Click;
    WebButton1.TabOrder := 1;

    WebMemo1.Parent     := Self;
    WebMemo1.Name       := 'WebMemo1';
    WebMemo1.Left       := 64;
    WebMemo1.Top        := 88;
    WebMemo1.Width      := 215;
    WebMemo1.Height     := 89;
    WebMemo1.AutoSize   := false;
    WebMemo1.SelLength  := 0;
    WebMemo1.SelStart   := 0;
    WebMemo1.TabOrder   := 2;

    WebComboBox1.Parent     := Self;
    WebComboBox1.Name       := 'WebComboBox1';
    WebComboBox1.Left       := 64;
    WebComboBox1.Top        := 197;
    WebComboBox1.Width      := 215;
    WebComboBox1.Height     := 24;
    WebComboBox1.ItemIndex  := -1;
    WebComboBox1.TabOrder   := 3;
    WebComboBox1.Text       := 'WebComboBox1';
    WebComboBox1.OnChange  := @WebComboBox1Change;

    WebPanel1.Parent      := Self;
    WebPanel1.Name        := 'WebPanel1';
    WebPanel1.Left        := 64;
    WebPanel1.Top         := 272;
    WebPanel1.Width       := 513;
    WebPanel1.Height      := 89;
    WebPanel1.WidthStyle  := TSizeStyle.ssPercent;
    WebPanel1.WidthPercent:= 80;
    WebPanel1.BorderStyle := TBorderStyle.bsSingle;

    WebLabel2.Parent      := WebPanel1;
    WebLabel2.Name        := 'WebLabel2';
    WebLabel2.Left        := 3;
    WebLabel2.Top         := 29;
    WebLabel2.Width       := 411;
    WebLabel2.Height      := 32;
    WebLabel2.Caption     := 'This demo shows the use of basic controls like TEdit, TButton, TComboBox, TMemo and TLabel.';
    WebLabel2.WordWrap   := true;
    WebLabel2.WidthStyle  := TSizeStyle.ssPercent;

    WebImageControl1.Parent    := WebPanel1;
    WebImageControl1.Name      := 'WebImageControl1';
    WebImageControl1.Left      := 6;
    WebImageControl1.Top       := 7;
    WebImageControl1.Width     := 16;
    WebImageControl1.Height    := 16;
    WebImageControl1.AutoSize := true;
    WebImageControl1.Picture.LoadFromFile('Picture.png');
  finally
    //Button1.EndUpdate;
    WebLabel1.EndUpdate;
    WebEdit1.EndUpdate;
    WebButton1.EndUpdate;
    WebMemo1.EndUpdate;
    WebComboBox1.EndUpdate;
    WebPanel1.EndUpdate;
    WebLabel2.EndUpdate;
    WebImageControl1.EndUpdate;
  end;