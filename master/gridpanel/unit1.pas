unit Unit1;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, JS, Web,
  WEBLib.Graphics, WEBLIB.Controls, WEBLib.StdCtrls, WEBLib.ExtCtrls,
  WEBLib.Forms, WEBLib.Dialogs;

type
  { TForm1 }
  TForm1 = class(TForm)
  private
    { Private declarations }
    WebLabel5: TLabel;
    WebGridPanel1: TGridPanel;
    WebLabel1: TLabel;
    WebLabel2: TLabel;
    WebLabel3: TLabel;
    WebLabel4: TLabel;
    WebEdit1: TEdit;
    WebButton1: TButton;
    WebEdit2: TEdit;
    WebButton2: TButton;
    WebEdit3: TEdit;
    WebButton3: TButton;
    WebEdit4: TEdit;
    WebButton4: TButton;
    WebPanel1: TPanel;
    WebLabel6: TLabel;
    WebImageControl1: TImageControl;
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

end;

procedure TForm1.WebComboBox1Change(Sender: TObject);
begin

end;

end.

