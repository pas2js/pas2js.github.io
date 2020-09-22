program project1;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, WEBLib.Forms, WEBLib.Runner, unit1;

begin
  // Your code here
  Application.Initialize;
  Application.CreateForm(TWebForm1, WebForm1);
  Application.Run;
  TTMSWebRunner.Execute(tbnFirefox);
end.
