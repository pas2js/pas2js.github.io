program project1;

{$mode objfpc}

uses
   SysUtils, Classes, uFormView;

var
  FMainView: TJCripto;
begin
  // Your code here
  FMainView:= TJCripto.Create;
  FMainView.InitializeObject;
end.

