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
unit pas2js.FieldSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element;

type
  TWFieldSet = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    Legend: String;
    Title : TCustomControl;
  end;

implementation



{ TWFieldSet }

constructor TWFieldSet.Create(parent: TCustomControl);

  procedure doFieldSetOnReadyExecute(sender: TObject);
  var
    i: integer;
    d: TJSHTMLCollection;
  begin
    //construct legend when applicable
    If self.legend <> '' then
    begin
      Title := TCustomControl.Create('legend',self);
      Title.Handle.innerHTML := self.Legend;
      Title.Handle.removeAttribute('style');
    end;

    d := self.Handle.children;
    for i := 0 to d.length -1 do begin
      If TJSHTMLElement(d[i]).style.getPropertyValue('height') = '0px' then
      begin
        TJSHTMLElement(d[i]).style.setProperty('left', '10px');
        TJSHTMLElement(d[i]).style.setProperty('top', inttostr(30 + (i*34)) + 'px');
        TJSHTMLElement(d[i]).style.setProperty('width', inttostr(self.width-4) + 'px');
        TJSHTMLElement(d[i]).style.setProperty('height', '30px');
      end;
    end;

  end;

begin
  inherited Create('fieldset', parent);
  SetProperty('border','1px solid silver');

  //self.Observe;
  self.OnReadyExecute := @doFieldSetOnReadyExecute;
end;

end.

