{
	This is free and unencumbered software released into the public domain.

	Anyone is free to copy, modify, publish, use, compile, sell, or
	distribute this software, either in source code form or as a compiled
	binary, for any purpose, commercial or non-commercial, and by any
	means.

	In jurisdictions that recognize copyright laws, the author or authors
	of this software dedicate any and all copyright interest in the
	software to the public domain. We make this dedication for the benefit
	of the public at large and to the detriment of our heirs and
	successors. We intend this dedication to be an overt act of
	relinquishment in perpetuity of all present and future rights to this
	software under copyright law.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
	OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
	ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
}
unit pas2js.StreetView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.IFrame;

type
  TWStreetView = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure SetLocation(s1,s2,s3: String);    //(lat/long/api)
    ifr : TWIFrame;
  end;

implementation

{ TWStreetView }

constructor TWStreetView.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);
  ifr := TWIframe.Create(self);
end;

Procedure TWStreetView.SetLocation(s1,s2,s3: String);
var
  s: String;

begin
  s := 'https://www.google.com/maps/embed/v1/streetview?location=' +
                    s1 + ',' + s2 + '&key=' + s3;
//                    'AIzaSyCWiDqHr-ME74FlTd40x2yoLgVA6Qod-Tk';

  ifr.SetAttribute('src',s);
  ifr.SetProperty('width',inttostr(width-60)+'px');
  ifr.SetProperty('height',inttostr(height-60)+'px');
end;

end.

