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
    WebLabel1 : TLabel;
    WebButton1 : TButton;
    WebEdit1 : TEdit;
    WebCheckBox1 : TCheckBox;
    WebPanel1 : TPanel;
    WebLabel6 : TLabel;
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

uses Unit2;

{ TForm1 }

procedure TForm1.LoadDFMValues;
begin
  inherited;
  {$I Form1.lfm.inc}    
end;

procedure TForm1.WebButton1Click(Sender: TObject);
var
  newform: TForm2;

  procedure AfterShowModal(AValue: TModalResult);
  begin
    WriteLn('Form 2 closed with new value:' + newform.frm2Edit.Text);
    WebEdit1.Text := newform.frm2Edit.Text;
  end;

  // async called OnCreate for TForm2
  procedure AfterCreate(AForm: {TObject}TCustomForm);
  begin
    (AForm as TForm2).frm2Edit.Text := WebEdit1.Text;
  end;

begin
  //newform = pas.Unit2.TForm2.$create("CreateNew$3",[AfterCreate]);
  newform := TForm2.CreateNew(@AfterCreate);
  //newform := TForm2.CreateNew;
  newform.Popup := WebCheckBox1.Checked;
  newform.ShowModal(@AfterShowModal);
end;

procedure TForm1.WebComboBox1Change(Sender: TObject);
begin
//  WebLabel1.Caption := WebComboBox1.Items[WebComboBox1.ItemIndex];
end;

end.


