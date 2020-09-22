unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, JS, Web,
  WEBLib.Graphics, WEBLIB.Controls, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms,
  WEBLib.Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
  private
    { Private declarations }
    WebPanel1: TPanel;
    WebLabel1: TLabel;
    WebLabel2: TLabel;
    WebEdit1: TEdit;
    WebEdit2: TEdit;
    WebMemo1: TMemo;
    WebPanel2: TPanel;
    WebLabel6: TLabel;
    WebImageControl1: TImageControl;

  public
    { Public declarations }
    procedure LoadDFMValues; override;
    procedure WebButton1Click(Sender: TObject);
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

end;

end.


