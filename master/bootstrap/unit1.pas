unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, JS, Web,
  WEBLib.Graphics, WEBLIB.Controls, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms,
  WEBLib.Dialogs;

type
  TForm1 = class(TForm)
  private
    { Private declarations }
    //Button1        : TButton;
    WebLabel1        : TLabel;
    WebEdit1         : TEdit;
    WebButton1       : TButton;
    WebMemo1         : TMemo;
    WebComboBox1     : TComboBox;
    WebPanel1        : TPanel;
    WebLabel2        : TLabel;
    WebImageControl1 : TImageControl;
  public
    { Public declarations }
    procedure LoadDFMValues; override;
    procedure WebButton1Click(Sender: TObject);
    procedure WebComboBox1Change(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LoadDFMValues;
begin
  inherited;
  {$I Form1.lfm.inc}  
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  console.log('button clicked');
  WebComboBox1.Items.Add(WebEdit1.Text);
  WebComboBox1.ItemIndex := WebComboBox1.Items.Count - 1;
  WebMemo1.Lines.Add(WebEdit1.Text);
end;

procedure TForm1.WebComboBox1Change(Sender: TObject);
begin
  WebLabel1.Caption := WebComboBox1.Items[WebComboBox1.ItemIndex];
end;

end.


