unit System.Diagnostics;
{ ╔═════════════════════════════════════════════════════════════════════════════╗
  ║ Provides a high-resolution stopwatch implementation.                        ║
  ║ Use TStopwatch to obtain access to high-resolution timers that can be used  ║
  ║ to monitor the time spent performing some operations.                       ║
  ╚═════════════════════════════════════════════════════════════════════════════╝ }
(* Examples:
    var
      TickStart, TickEnd: Double;
      StopWatch: TStopWatch;
      I: Integer;
      Aux: Integer;
      Res: Double;

    begin
     { teste i }
      TickStart := GetTickCount;
       try
         for I := 0 to 999999999 do
         begin
           Aux := Trunc(I / 2);
         end;
       finally
         TickEnd := GetTickCount;
       end;

       console.log( FloatToStr((TickEnd - TickStart) / 1000) + ' secs' );

     { teste ii }
      StopWatch := TStopWatch.Create;
      StopWatch.Start;
       try
         for I := 0 to 999999999 do
         begin
           Aux := Trunc(I / 2);
         end;
       finally
         StopWatch.Stop;
       end;

       Res := StopWatch.Elapsed;
       console.log(FloatToStr(Res/1000) + ' secs');

     end;
*)

{$mode objfpc}

interface

uses
  JS;

type
  { TStopWatch }
  TStopWatch = class
  strict private
    FElapsed: Double;
    FRunning: Boolean;
    FStartTimeStamp: Double;
    FStopTimeStamp: Double;
  protected
    function GetHighResolution: Boolean;
    function GetElapsed: Double;
    procedure SetStartTime(const aValue: Double);
    procedure SetStopTime(const aValue: Double);
    property StartTime: Double read FStartTimeStamp write SetStartTime;
    property StopTime: Double read FStopTimeStamp write SetStopTime;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    property Elapsed: Double read GetElapsed;
    property IsHighResolution: Boolean read GetHighResolution;
    property IsRunning: Boolean read FRunning;
  end;

  function GetTickCount: Double;

implementation

type
  { T }
  T = class abstract
  strict private
    class var x: JSValue;
    class var b: Boolean;
    class procedure n;
  public
    class function Y: Double; static;
    class property H: Boolean read b;
  end;

{ T }
class function T.Y : Double;
begin
  if not x then n;
  asm
    Result = Number(this.x.now());
  end;
end;

class procedure T.n;
begin
  asm
    if (window.performance && performance.now) {
      this.x = performance;
      this.b = true;
    } else {
      this.b =false;
      if (!Date.now){ Date.now=function(){return +(new Date)}};
      this.x = Date;
    }
  end;
end;

function GetTickCount: Double;
begin
  Result:= T.Y;
end;

{ TStopWatch }

function TStopWatch.GetHighResolution: Boolean;
begin
  Result:= T.H;
end;

function TStopWatch.GetElapsed: Double;
begin
  if IsRunning then
    FElapsed:= T.Y-StartTime else
    FElapsed:= StopTime-StartTime;
  Result:= FElapsed;
end;

procedure TStopWatch.SetStartTime(const aValue: Double);
begin
  if FStartTimeStamp= aValue then
    Exit;
  FStartTimeStamp:= aValue;
end;

procedure TStopWatch.SetStopTime(const aValue: Double);
begin
  if FStopTimeStamp= aValue then
    Exit;
  FStopTimeStamp:= aValue;
end;

constructor TStopWatch.Create;
begin
  Start;
end;

procedure TStopWatch.Start;
begin
  FRunning := True;
  StartTime := T.Y;
end;

procedure TStopWatch.Stop;
begin
  StopTime := T.Y;
  FRunning := False;
end;

end.

