program project1;

{$mode objfpc}

uses
   SysUtils, JS, Web, uFishFacts;

var
  FishFacts: TJFishFacts;
begin
  // Your code here
  FishFacts := TJFishFacts.Create;
  FishFacts.InitializeObject;
  //console.log('init');
end.

