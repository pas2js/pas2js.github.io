unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, JS, Web, WEBLib.Forms, WEBLIB.Controls, WEBLib.Graphics,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Dialogs
  { you can add units after this };

type

  { TWebForm1 }

  TWebForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebComboBox1: TWebComboBox;
    WebEdit1: TWebEdit;
    WebImageControl1: TWebImageControl;
    WebLabel1: TWebLabel;
    WebLabel2: TWebLabel;
    WebMemo1: TWebMemo;
    WebPanel1: TWebPanel;
    procedure WebButton1Click(Sender: TObject);
    procedure WebComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure LoadDFMValues; override;
  public
    { Public declarations }
  end;

var
  WebForm1: TWebForm1;

implementation

{$R *.lfm}

procedure TWebForm1.WebButton1Click(Sender: TObject);
begin
  console.log('button1 clicked');
  WebComboBox1.Items.Add(WebEdit1.Text);
  WebComboBox1.ItemIndex:= WebComboBox1.Items.Count - 1;
  WebMemo1.Lines.Add(WebEdit1.Text);
end;

procedure TWebForm1.WebComboBox1Change(Sender: TObject);
begin
  WebLabel2.Caption:= WebComboBox1.Items[WebComboBox1.ItemIndex];
end;

procedure TWebForm1.LoadDFMValues;
begin
inherited;
  {$I WebForm1.lfm.inc}
end;


end.

