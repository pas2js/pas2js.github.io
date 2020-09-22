unit uSanta;

{$mode objfpc}
{$modeswitch externalclass}

Interface

uses
  Classes, SysUtils, Types, Math, JS, Web;

type
  TJSAudio = class external name 'Audio' (TJSHTMLAudioElement);

{ global methods }
  procedure Frame(timeStamp : double);
  procedure MyKeyDown(e: TJSKeyboardEvent); // TJSEventHandler
  function GetPicture(fileName: String): TJSHTMLImageElement;
  function playMp3(mp3: String; loop: Boolean): TJSAudio;

{ global variables }
var
  ctx : TJSCanvasRenderingContext2D;
  arr: array of TPoint;
  c : TJSHTMLCanvasElement;
  a: Integer;
  speed, jump: Longint;
  img, flake, img2, bg, clouds, fence: TJSObject;
  jingleSnd, boingSnd: TJSHTMLAudioElement;

implementation

function GetPicture(fileName: String): TJSHTMLImageElement;
var
  img: TJSHTMLImageElement;
begin
  img := TJSHTMLImageElement.new();
  img.src := fileName;
  Result := img;
end;

procedure DrawPicture(pic: TJSHTMLImageElement; x, y: Real);
begin
  ctx.drawImage(pic, x, y);
end;

function playMp3(mp3: String; loop: Boolean): TJSAudio;
var
  snd: TJSAudio;
begin
  snd:= TJSAudio.new;
  snd.src:= mp3;
  snd.loop:= loop;

  if snd.paused then
    snd.play()
  else
    snd.currentTime := 0;
  Result := snd;
end;

procedure MyKeyDown(e: TJSKeyboardEvent);
begin
  if e.keyCode = 27 then
  begin
    a := 0; // Reset Canvas
  end
  else
  begin
    speed := 20; // Jump
    playMp3('assets/boing.mp3', false);
  end;
end;

procedure Update();
var
  i, a1, a2, a3, a4: Longint;
begin
  ctx.clearRect(0, 0, c.width, c.height);

  if (jump > 0) or (speed > 0) then
  begin
    jump := jump + speed;
    speed := speed - 1;
    if jump < 0 then
      jump := 0;
  end;

  a1 := a mod 640;
  a2 := (a div 2) mod 640;
  a3 := (a div 3) mod 640;
  a4 := (a div 4) mod 640;
  ctx.drawImage(bg, -a2, 0);
  ctx.drawImage(bg, -a2 + 640, 0);
  ctx.drawImage(clouds, -a4, 50);
  ctx.drawImage(clouds, -a4 + 640, 50);

  if jump > 0 then
    ctx.drawImage(img2, 100, 130 - jump)
  else
    ctx.drawImage(img2, 100, 130 + 5*sin(a/10));

  ctx.drawImage(fence, -a1, 400);
  ctx.drawImage(fence, -a1 + 640, 400);

  a := a + 2;

  for i := 0 to High(arr) do
  begin
    arr[i].y := arr[i].y + Random(5);
    arr[i].x := arr[i].x + Random(5)-2;
    ctx.drawImage(flake, arr[i].x, arr[i].y);
    if arr[i].y > 480 then
    begin
      arr[i].y := -10;
      arr[i].x := Random(640);
    end;
  end;

  if (a mod 2000) > 200 then
  begin
    a1 := a mod 2000;
  ctx.font := '60px Princess Sofia';
  if a1 < 640 then
    ctx.fillText('Powered by pas2js 1.5.1', 640 - (a1-50),50)
  else
  if a1 < 1000 then
    ctx.fillText('Powered by pas2js 1.5.1',50,50);
  end;

end;

procedure Frame(timeStamp : Double);
begin
  Update();
  window.requestAnimationFrame(@Frame);
end;

end.

