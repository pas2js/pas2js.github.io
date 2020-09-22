unit uMain;

{$MODE objfpc}{$H+}

interface

uses
  uFishfacts, Classes, SysUtils;

type
  TApplication = class(TObject)
  private
    {  private declarations  }
    FishFacts: TJFishFacts;
    Application: TApplication;
  public
    procedure RunApp; virtual;
  end;

implementation

procedure TApplication.RunApp;
begin
  FishFacts := TJFishFacts.Create;
  FishFacts.InitializeObject;
end;

end.
