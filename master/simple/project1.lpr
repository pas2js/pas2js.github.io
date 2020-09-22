program project1;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web,
  WEBLib.Forms, WEBLib.Runner,
  Unit1 in 'Unit1.pas' {Form1: TWebForm} {*.html};

{$R *.res}

begin
  // Your code here
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  TTMSWebRunner.Execute(tbnFirefox);
end.
