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
    WebLabel4        : TLabel;
    WebLabel1        : TLabel;
    WebLabel2        : TLabel;
    WebListBox2      : TListBox;
    WebButton1       : TButton;
    WebEdit1         : TEdit;
    WebPanel1        : TPanel;
    WebLabel6        : TLabel;
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

     procedure DialogProc(AValue: Integer);
     var
       s: String;
     begin
       case AValue of
         1: s := 'OK clicked';
         6: s := 'Yes clicked';
         7: s := 'No clicked';
         3: s := 'Abort clicked';
         4: s := 'Retry clicked';
         8: s := 'Close clicked';
         2: s := 'Cancelled';
       end;
       Self.WebLabel2.Caption := s;
     end;


begin
  console.log('button clicked');
  case Self.WebListBox2.ItemIndex of
    0: MessageDlg(WebEdit1.Text, mtWarning,[], @DialogProc);
    1: MessageDlg(WebEdit1.Text, mtError,[], @DialogProc);
    2: MessageDlg(WebEdit1.Text, mtInformation,[], @DialogProc);
    3: MessageDlg(WebEdit1.Text, mtConfirmation, [mbYes, mbNo, mbCancel], @DialogProc);
    4: MessageDlg(WebEdit1.Text, mtCustom, [mbAbort, mbRetry, mbClose], @DialogProc);
  end;
end;

procedure TForm1.WebComboBox1Change(Sender: TObject);
begin

end;

end.


