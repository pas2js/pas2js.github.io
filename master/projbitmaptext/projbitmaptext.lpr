program projbitmaptext;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, Types, Math, uMain ;

procedure bomb;
begin
  window.setTimeout(procedure
  var
    txt: String;
    time: Integer;
    tmp: Integer;
    c, m, i: integer;
    s: String;
    sep: String;
  begin
    document.querySelector('.bottomStuff').classList.add('active');
    time := 2 * 60;
    tmp := time;
    i:= window.setInterval(procedure
    begin
      Dec(tmp);
      c := tmp;
      m := Trunc(c / 60) shr 0;
      s := IntToStr(c - m * 60) + '';
      if (TJSString(s).length) > 1 then txt := '' else txt := '0';
      txt := 'pas2js in ' + IntToStr(m) + ':' + txt + s;
       bitmapText(document.getElementById('img1'), 'Desyrel', txt, 160, 137, 1, false);
      if (tmp = -1) then begin
	bitmapText(document.getElementById('img1'), 'Desyrel', 'puff', 160, 137, 1, false);
      window.clearInterval(i);
      end;
    end, 100);
  end, 1000);

end;

begin
// Your code here
  console.log('starting...');
  bomb;
end.
