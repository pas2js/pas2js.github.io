  LeftPanel       := TPanel.Create(Self);
  WebLabel1       := TLabel.Create(LeftPanel);
  WebButton1      := TButton.Create(LeftPanel);
  ClientPanel     := TPanel.Create(Self);
  ListChildPanel  := TPanel.Create(ClientPanel);
  WebLabel2       := TLabel.Create(ClientPanel);
  WebListBox1     := TListBox.Create(ClientPanel);
  WebListBox2     := TListBox.Create(ClientPanel);
  WebSplitter1    := TSplitter.Create(Self);

  LeftPanel.BeginUpdate;
  WebLabel1.BeginUpdate;
  WebButton1.BeginUpdate;
  ClientPanel.BeginUpdate;
  ListChildPanel.BeginUpdate;
  WebLabel2.BeginUpdate;
  WebListBox1.BeginUpdate;
  WebListBox2.BeginUpdate;
  WebSplitter1.BeginUpdate;

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

    LeftPanel.Parent  := Self;
    LeftPanel.Name    := 'LeftPanel';
    LeftPanel.Left    := 0;
    LeftPanel.Top     := 0;
    LeftPanel.Width   := 150;
    LeftPanel.Height  := 480;
    LeftPanel.Align   := TAlign.alLeft;
    LeftPanel.BorderStyle := TBorderStyle.bsSingle;
    LeftPanel.Caption := 'LeftPanel';

    WebLabel1.Parent  := LeftPanel;
    WebLabel1.Name    := 'WebLabel1';
    WebLabel1.Left    := 0;
    WebLabel1.Top     := 0;
    WebLabel1.Width   := 150;
    WebLabel1.Height  := 16;
    WebLabel1.Align   := TAlign.alTop;
    WebLabel1.Caption := 'Left aligned';

    WebButton1.Parent          := LeftPanel;
    WebButton1.Name            := 'WebButton1';
    WebButton1.Left            := 0;
    WebButton1.Top             := 455;
    WebButton1.Width           := 150;
    WebButton1.Height          := 25;
    WebButton1.Align           := TAlign.alBottom;
    WebButton1.Caption         := 'Bottom button';
    WebButton1.TabOrder        :=  0;
    WebButton1.OnClick         := @WebButton1Click;

    ClientPanel.Parent         := Self;
    ClientPanel.Name           := 'ClientPanel';
    ClientPanel.Left           := 155;
    ClientPanel.Top            := 0;
    ClientPanel.Width          := 485;
    ClientPanel.Height         := 480;
    ClientPanel.Align          := TAlign.alClient;
    ClientPanel.BorderStyle    := TBorderStyle.bsSingle;
    ClientPanel.Caption        := 'ClientPanel';

    ListChildPanel.Parent      := ClientPanel;
    ListChildPanel.Name        := 'ListChildPanel';
    ListChildPanel.Left        := 0;
    ListChildPanel.Top         := 0;
    ListChildPanel.Width       := 485;
    ListChildPanel.Height      := 249;
    ListChildPanel.Align       := TAlign.alTop;
    ListChildPanel.BorderStyle := TBorderStyle.bsSingle;
    ListChildPanel.Caption     := 'ListChildPanel';

    WebLabel2.Parent           := ListChildPanel;
    WebLabel2.Name             := 'WebLabel2';
    WebLabel2.Left             := 0;
    WebLabel2.Top              := 0;
    WebLabel2.Width            := 485;
    WebLabel2.Height           := 16;
    WebLabel2.Align            := TAlign.alTop;
    WebLabel2.Alignment        := taCenter;
    WebLabel2.Caption          := 'Top aligned child panel';

    WebListBox1.Parent         := ListChildPanel;
    WebListBox1.Name           := 'WebListBox1';
    WebListBox1.Left           := 364;
    WebListBox1.Top            := 16;
    WebListBox1.Width          := 121;
    WebListBox1.Height         := 233;
    WebListBox1.Align          := TAlign.alRight;
    WebListBox1.ItemIndex      := -1;

    WebListBox1.Items.BeginUpdate ;
    try
      WebListBox1.Items.Clear ;
      WebListBox1.Items.Add('Right aligned');
    finally
      WebListBox1.Items.EndUpdate ;
    end;
    WebListBox1.TabOrder    :=  0;

    WebListBox2.Parent      := ListChildPanel;
    WebListBox2.Name        := 'WebListBox2';
    WebListBox2.Left        := 0;
    WebListBox2.Top         := 16;
    WebListBox2.Width       := 121;
    WebListBox2.Height      := 233;
    WebListBox2.Align       := TAlign.alLeft;
    WebListBox2.ItemIndex   := -1;
    WebListBox2.Items.BeginUpdate ;
    try
      WebListBox2.Items.Clear ;
      WebListBox2.Items.Add('Left aligned');
    finally
      WebListBox2.Items.EndUpdate ;
    end;
    WebListBox2.TabOrder :=  1;

    WebSplitter1.Parent  := Self;
    WebSplitter1.Name    := 'WebSplitter1';
    WebSplitter1.Left    := 150;
    WebSplitter1.Top     := 0;
    WebSplitter1.Width   := 5;
    WebSplitter1.Height  := 480;
    WebSplitter1.Align   := TAlign.alLeft;
    WebSplitter1.Color   := 15790320;
  finally
    LeftPanel.EndUpdate;
    WebLabel1.EndUpdate;
    WebButton1.EndUpdate;
    ClientPanel.EndUpdate;
    ListChildPanel.EndUpdate;
    WebLabel2.EndUpdate;
    WebListBox1.EndUpdate;
    WebListBox2.EndUpdate;
    WebSplitter1.EndUpdate;
  end;