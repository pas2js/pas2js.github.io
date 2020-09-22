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
    WebLabel1        : TLabel;
    WebPaintBox1     : TPaintBox;
    WebPanel1        : TPanel;
    WebLabel2        : TLabel;
    WebImageControl1 : TImageControl;
    WebPaintBox2     : TPaintBox;
    { methods }
    procedure WebPaintBox1Paint(Sender: TObject);
    procedure WebPaintBox2Paint(Sender: TObject);
  protected
    procedure PaintSign(Control: TCustomControl; AText: String; Cross: Boolean);
  public
    { Public declarations }
    procedure LoadDFMValues; override;
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

procedure TForm1.PaintSign(Control: TCustomControl; AText: String; Cross: Boolean);
begin
  Control.Canvas.Pen.Width   := 3;
  Control.Canvas.Pen.Color   := 255;
  Control.Canvas.Brush.Color := 65535;
  Control.Canvas.Brush.Style := TBrushStyle.bsSolid;
  Control.Canvas.Rectangle(10,10,200,100);
  if (Cross) then
  begin
    Control.Canvas.MoveTo(10,100);
    Control.Canvas.LineTo(200,10);
    Control.Canvas.Font.Size := 14;
  end;
  Control.Canvas.Font.Name := 'Arial';
  Control.Canvas.Font.Size := 14;
  Control.Canvas.TextOut(20,40,AText);
end;

procedure TForm1.WebPaintBox1Paint(Sender: TObject);
begin
  PaintSign(WebPaintBox1, 'Native clients only', true);
end;

procedure TForm1.WebPaintBox2Paint(Sender: TObject);
begin
  PaintSign(WebPaintBox2, 'Web + Native clients!', false);
end;


end.


