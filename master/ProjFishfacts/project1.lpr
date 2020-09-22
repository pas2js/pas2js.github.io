program project1;

{$mode objfpc}

uses
  uMain, Web, Classes, SysUtils;

var
  Application: TApplication;

begin
  try
    Application := TApplication.Create;
    Application.RunApp;
  except
    on e: Exception do
      console.log(e.Message);
  end;

end.
