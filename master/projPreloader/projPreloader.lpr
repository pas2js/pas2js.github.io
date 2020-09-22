
program projPreloader;

{$mode objfpc}
{$modeswitch externalclass}

uses
  SysUtils, Classes, JS, Web, uLoader;

type
  TFunc = reference to function: Double;

  TCircle = class external name 'Object' (TJSObject)
    radius: Integer;
    area: TFunc;
  end;

var
  circ : TCircle;

begin
Preloader._then(
  function(aStatus: JSValue): JSValue
  begin
    if aStatus then
    begin
    { ╔══════════════════════════════════════════════════╗
      ║ <<< AFTER ALL RESOURCES HAVE BEEN LOADED! >>>    ║
      ╚══════════════════════════════════════════════════╝ }
      // Your code begin here
      circ := TCircle.new;
      circ.radius:= 10;
      circ.area := function(): Double
      begin
        Result := PI * circ.radius * circ.radius;
      end;

      console.log(circ.area);

    end;
  end);

end.
