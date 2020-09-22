unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, JS, Web,
  WEBLib.Forms, WEBLIB.Controls, WEBLib.Graphics, WEBLib.StdCtrls,
  WEBLib.ExtCtrls, WEBLib.Dialogs
  { you can add units after this };

type

  { TWebForm1 }

  TWebForm1 = class(TWebForm)
    WebLabel1: TWebLabel;
    WebPanel1: TWebPanel;
    WebTimer1: TWebTimer;
    procedure WebTimer1Timer(Sender: TObject);
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

procedure TWebForm1.WebTimer1Timer(Sender: TObject);
begin
  WebLabel1.Caption:= TimeToStr(time);
end;

procedure TWebForm1.LoadDFMValues;
begin
inherited;
  {$I WebForm1.lfm.inc}
end;


end.

