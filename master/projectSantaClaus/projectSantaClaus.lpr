program projAudio;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, uSanta;

function startBtn(Event: TEventListenerEvent): boolean;
var
  i: Longint;
  v: Integer;
  parentEl: TJSElement;
  PlaySound: TJSAudio;

begin
  { hidden some elements }
  TJSHTMLElement(document.querySelector('#el1')).style.Properties['display'] := 'none';
  TJSHTMLElement(document.querySelector('#el2')).style.Properties['display'] := 'none';

  { code here! }
    parentEl := document.querySelector('.pas2js-stage');

    c := TJSHTMLCanvasElement(document.createElement('CANVAS'));
    c.width:=640;
    c.height:=480;
    ctx := TJSCanvasRenderingContext2D(c.getContext('2d'));
    //document.body.appendChild(c);
    parentEl.appendChild(c);

    ctx.beginPath();
    ctx.arc(95,50,40,0,2*3.1416);
    ctx.stroke();
    // Is requestAnimationFrame better than setInterval?
    //  interval := window.setInterval(@Update, 20);
    window.requestAnimationFrame(@Frame);

    // Attach keyboard event handler
    window.addEventListener('keydown', TJSEventHandler(@MyKeyDown));

    a := 0;
    v := 2;
    speed := 0;
    jump := 0;
    SetLength(arr, 20);
    for i := 0 to High(arr) do
    begin
      arr[i].y := Random(300);
      arr[i].x := Random(640);
    end;

    // Direct references to files, much nicer than going through the HTML file!
    img := GetPicture('assets/sled.png');
    img2 := GetPicture('assets/sled2.png');
    flake := GetPicture('assets/snowflake.png');
    bg := GetPicture('assets/bg.png');
    clouds := GetPicture('assets/clouds.png');
    fence := GetPicture('assets/fence.png');
    playMp3('assets/jingle_bells.mp3', true);
end;


var
  el: TJSElement;
begin
  // start Button event Handler
  el := document.querySelector('#el2');
  el.addEventListener('click', @startBtn);
end.
