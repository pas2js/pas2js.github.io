unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, JS, Web, DB,
  WEBLib.Graphics, WEBLIB.Controls, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Forms,
  WEBLib.Dialogs, WEBLib.DBCtrls, WEBLib.CDS;

type
  { TForm1 }
  TForm1 = class(TForm)
  private
    { Private declarations }
    WebDBLabel1: TDBLabel;
    WebLabel1: TLabel;
    WebLabel2: TLabel;
    WebLabel3: TLabel;
    WebLabel4: TLabel;
    WebLabel6: TLabel;
    WebLabel7: TLabel;
    WebLabel5: TLabel;
    WebButton1: TButton;
    WebDBNavigator1: TDBNavigator;
    WebDBEdit1: TDBEdit;
    WebDBEdit2: TDBEdit;
    WebDBEdit3: TDBEdit;
    WebDBEdit4: TDBEdit;
    WebPanel1: TPanel;
    WebLabel9: TLabel;
    WebImageControl1: TImageControl;
    WebDBMemo1: TDBMemo;
    WebDBSpinEdit1: TDBSpinEdit;
    WebClientConnection1: TClientConnection;
    WebClientDataSet1: TClientDataSet;
    WebClientDataSource1: TClientDataSource;

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
  WebClientConnection1.URI := 'fishfacti.json';
  WebClientConnection1.DataNode := 'ROW';
  WebClientDataSet1.FieldDefs.Clear();
  WebClientDataSet1.FieldDefs.Add('_Species_No', TFieldType.ftString,0);
  WebClientDataSet1.FieldDefs.Add('_Category',  TFieldType.ftString,50);
  WebClientDataSet1.FieldDefs.Add('_Common_Name', TFieldType.ftString,50);
  WebClientDataSet1.FieldDefs.Add('_Species_Name', TFieldType.ftString,50);
  WebClientDataSet1.FieldDefs.Add('_Length__cm_', TFieldType.ftInteger,0);
  WebClientDataSet1.FieldDefs.Add('_Length_In', TFieldType.ftString,30);
  WebClientDataSet1.FieldDefs.Add('_Notes', TFieldType.ftString,255);
  WebClientConnection1.Active := true;
end;

end.


