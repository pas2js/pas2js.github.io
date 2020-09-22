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
unit pas2js.Dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.Panel, pas2js.Button;

type
  TWDialog = class(TCustomControl)
  protected
    DialogBox : TWPanel;
    CloseButton: TWButton;
    procedure ArrangeElements;
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure OpenDialog(DialogMessage: String);
  end;

implementation



{ TWDialog }

constructor TWDialog.Create(parent: TCustomControl);
  procedure doCloseButtonOnClick(sender:TObject);
  begin
    self.SetProperty('display','none');
  end;

begin
  inherited Create('div', parent);
  self.SetProperty('display','none');
  self.SetProperty('background-color', 'rgb(0,0,0)');
  self.SetProperty('background-color', 'rgba(0,0,0,0.4)');
  TJSHTMLElement(self.Handle).style.setProperty('width', '100%');
  TJSHTMLElement(self.Handle).style.setProperty('height', '100%');

  DialogBox := TWPanel.Create(self);
  DialogBox.SetProperty('background-color', 'whitesmoke');
  DialogBox.SetProperty('margin', '10% 5% 0% 10%');
  DialogBox.SetProperty('border', '1px solid #888');
  DialogBox.SetProperty('width', '80%');
  DialogBox.SetProperty('height', '30%');

  CloseButton := TWButton.Create(DialogBox);
  CloseButton.SetinnerHTML('x');
  CloseButton.SetAttribute('style', 'margin: 2px 2px; float: right; cursor: pointer;');

  CloseButton.OnClick :=  @doCloseButtonOnClick;
end;

procedure TWDialog.OpenDialog(DialogMessage: String);
begin
  ArrangeElements;
  // todo : set title
  self.SetProperty('display','inline-block');
end;

procedure TWDialog.ArrangeElements;
var
  d: TJSHTMLCollection;
  i, j, x, y, z: Integer;
  TempArray : Array of JSValue;
begin
  //move all children of self, except dialogbox, to dialogbox
  //so this component can be invoked as if it is a normal form
  d := self.Handle.children;
  for i := 0 to d.length -1 do
    TJSArray(TempArray).push(d[i]);

  z := 0;
  for j := 0 to TJSArray(TempArray).length -1 do
    If TJSElement(TempArray[j]).id <> DialogBox.Handle.id then begin  //omit DialogBox

      // set child.top at least to CloseButton.height so as not to obscure close button
      x := strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px'));
      If x <= 30 then
        TJSHTMLElement(TempArray[j]).style.setProperty('top', inttostr(x+30) + 'px');

      // set height of dialogbox depending on lowest child-bottom
      y := strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px')) +
               strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('height'),'px'));
      If y > z then z := y;

      // move all elements (if any) from self to DialogBox
      DialogBox.Handle.appendChild(TJSNode(TempArray[j]));
    end;

  If z > 0 then
    TJSHTMLElement(DialogBox.Handle).style.setProperty('height', inttostr(z) + 'px');
end;

end.

