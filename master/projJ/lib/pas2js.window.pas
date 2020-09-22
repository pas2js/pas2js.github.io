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
unit pas2js.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.Panel, pas2js.Button;

type
  TWWindow = class(TCustomControl)
  protected
    WindowArea : TWPanel;
    procedure ArrangeElements;
  public
    CloseButton: TWButton;
    constructor Create(parent: TCustomControl); virtual;
    procedure OpenWindow;
    procedure CloseWIndow;
  end;


implementation



{ TWWindow }

constructor TWWindow.Create(parent: TCustomControl);

  procedure doCloseButtonOnClick(sender: TObject);
  begin
    CloseWindow;
  end;

Begin
  inherited Create('div', parent);
  self.SetProperty('display','none');
  self.SetProperty('background-color', 'rgb(255,255,255)');
  self.SetProperty('background-color', 'rgba(0,0,0,0.4)');
  TJSHTMLElement(self.Handle).style.setProperty('width', '100%');
  TJSHTMLElement(self.Handle).style.setProperty('height', '100%');

  WindowArea := TWPanel.Create(self);
  WindowArea.SetProperty('background-color', 'whitesmoke');
  WindowArea.SetProperty('margin', '10% 5% 5% 10%');
  WindowArea.SetProperty('border', '1px solid #888');
  WindowArea.SetProperty('width', '80%');
  WindowArea.SetProperty('height', '30%');

  CloseButton := TWButton.Create(WindowArea);
  CloseButton.SetinnerHTML('x');
  CloseButton.SetAttribute('style', 'margin: 2px 2px; float: right; cursor: pointer;');

  CloseButton.OnClick := @doCloseButtonOnClick;
end;

procedure TWWindow.OpenWindow;
begin
  ArrangeElements;
  self.SetProperty('display','inline-block');
end;

procedure TWWindow.CloseWindow;
begin
  //self.SetProperty('display','none');
  //Handle.parentNode.removeChild(Handle);
  self.Destroy;
end;

procedure TWWindow.ArrangeElements;
var
  d: TJSHTMLCollection;
  i, j, x, y, z: Integer;
  TempArray : Array of JSValue;

begin
  //move all children of self, except WindowArea, to WindowArea
  //so this component can be invoked as if it is a normal form
  d := self.Handle.children;
  for i := 0 to d.length -1 do
    TJSArray(TempArray).push(d[i]);

  z := 0;
  for j := 0 to TJSArray(TempArray).length -1 do
    If TJSElement(TempArray[j]).id <> WindowArea.Handle.id then begin  //omit WindowArea

      // set child.top at least to CloseButton.height so as not to obscure close button
      x := parseInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px'));
      If x <= 30 then
        TJSHTMLElement(TempArray[j]).style.setProperty('top', Inttostr(x+30) + 'px');

      // set height of WindowArea depending on lowest child-bottom
      y := parseInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px')) +
               parseInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('height'),'px')) +
               parseInt(StrBefore(TJSHTMLElement(WindowArea.Handle).style.getPropertyValue('margin-bottom'),'px'));
      If y > z then z := y;

      // move all elements (if any) from self to WindowArea
      WindowArea.Handle.appendChild(TJSNode(TempArray[j]));
    end;

  If z > 0 then
    TJSHTMLElement(WindowArea.Handle).style.setProperty('height', inttostr(z) + 'px');
end;

end.

