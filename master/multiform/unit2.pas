unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, JS, Web,
  WEBLib.Graphics, WEBLIB.Controls, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms,
  WEBLib.Dialogs;

type
  TForm2 = class(TForm)
  private
    { Private declarations }
    WebLabel1: TLabel;
    WebLabel2: TLabel;
    WebButton2: TButton;
  public
    { Protected declarations }
    frm2Edit: TEdit;

  public
    { Public declarations }
    procedure LoadDFMValues; override;
    procedure WebButton2Click(Sender: TObject);

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm2.LoadDFMValues;
begin
  inherited;
  {$I Form2.lfm.inc}    
end;

procedure TForm2.WebButton2Click(Sender: TObject);
begin
  Close;
end;


end.


