unit uLoader;

{$MODE objfpc}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, Math, JS, Web;

function Preloader: TJSPromise;

implementation

type
  JParamsData = class;
  TRrcLoader_on_handler = reference to procedure(stats: JParamsData);

type
  JSMSLoader = class external name 'rscLoader'
    constructor New;
    function listen(eventName: string; handler: TRrcLoader_on_handler = nil):
      JSMSLoader; overload;
    function load(resourceName: string; resourceType: string): JSMSLoader;
      overload;
  end;

type
  JParamsData = class external name 'JParamsData'
    overall: Double;
    ready: Double;
  end;

function BounceOut(T, B, C, D: Float): Float;
begin
  T := T / D;
  if (T<(1 / 2.75)) then
    Result := C * (7.5625 * T * T) + B
  else if (T<(2 / 2.75)) then
  begin
    T := T - (1.5 / 2.75);
    Result := C * (7.5625 * T * T + 0.75) + B;
  end
  else if (T<(2.5 / 2.75)) then
  begin
    T := T - (2.25 / 2.75);
    Result := C * (7.5625 * T * T + 0.9375) + B;
  end
  else
  begin
    T := T - (2.625 / 2.75);
    Result := C * (7.5625 * T * T + 0.984375) + B;
  end;
end;

function BoolToInt(aValue: Boolean): Integer; assembler;
asm
  if (aValue){return 1}else{return  0};
end;

function NumberTo(const aNumber: Double): TJSNumber; external name ' ';

function Preloader: TJSPromise;
var
  preLoader, mainLoader: JSMSLoader;
  URI: string;
  i: integer;
  ring, text: TJSNode;
  r: integer;
  rv: Integer;
  degrees, rad: double;
  x, y: string;
  lenghty: Integer;
  descriptions: array of JSValue;

begin
  Result := TJSPromise.new(
    procedure(resolve, reject: TJSPromiseResolver)
    begin
      URI := TJSString(window.document.location.href).substring(0,
        TJSString(window.document.location.href).lastIndexOf('/'));
      preLoader := JSMSLoader.New;
      preLoader.load(URI + '/images/download.png', 'image');
      //preLoader.load(URI+'/res/app.css', 'stylesheet');

      preLoader.listen('all', procedure(stats: JParamsData)
        begin
          if (stats.overall > stats.ready) then
            exit;

          mainLoader := JSMSLoader.New;
          for i := 0 to 6 do
            mainLoader.load(URI + '/images/serverdelay.css?delay=2&' +
              string(TJSDate.New().valueOf()) + IntToStr(i) + IntToStr(i) +
              '&type=stylesheet', 'stylesheet');

          for i := 0 to 21 do
            mainLoader.load(URI + '/images/serverdelay.js?delay=1&' +
              string(TJSDate.New().valueOf()) + IntToStr(i) + IntToStr(i) +
              '&type=javascript', 'javascript');

          for i := 0 to 22 do
            mainLoader.load(URI + '/images/download.png?delay=1&' +
              string(TJSDate.New().valueOf()) + IntToStr(i) + IntToStr(i) +
              '&type=image', 'image');

          mainLoader.load(URI + '/css/photo-1516646255117-f9f933680173.jpg',
            'image');

          mainLoader.listen('all', procedure(stats: JParamsData)
            begin
              ring := document.getElementsByTagName('path')[0];
              text := document.getElementsByTagName('text')[0];
              r := 50;
              rv := Round(100 * stats.ready / stats.overall / 1);
               (* -------------------------------------------------------------*)
                // Update the wheel giving to it a value in degrees,
                // getted from the percentage of the input value
                // a.k.a. (value * 360) / 100
              degrees := BounceOut(rv / 100, 0.5, 0.5, 1) * 100 * 3.5999;
              // Convert the degrees value to radians
              rad := degrees * (PI / 180);
              // Determine X and cut to 2 decimals
              x := NumberTo(sin(rad) * r).toFixed(2);
              // Determine Y and cut to 2 decimals
              y := NumberTo(-cos(rad) * r).toFixed(2);
              // The another half ring. Same as (deg > 180) ? 1 : 0
              lenghty := BoolToInt(degrees > 180);
              // Moveto + Arcto
              descriptions := (['M', 0, 0, 'v', -r, 'A', r, r, 1, lenghty, 1, x,
                y, 'z']);
              // Apply changes to the path
              TJSElement(ring).setAttribute('d',
                TJSArray(descriptions).join(' '));
              // Update the numeric display
              text.textContent := FloatToStr(round(bounceOut(rv / 100, 0.5, 0.5,
                1) * 100)) + '%';
              // Translate the center axis to a half of total size
              //TJSElement(ring).setAttribute('transform', 'translate(' + IntToStr(r) + ', ' + IntToStr(r) + ')');
              TJSElement(ring).setAttribute('transform', 'translate(185,240)');
              if (stats.ready = stats.overall) then
              begin
                resolve(true);
                console.log('Ready!');
                document.querySelector('.pas2js_loader').setAttribute('style', 'display:none;');
              end;
             end); // end listen all
         end); // end listen all
     end); // end promise
end;

end.

