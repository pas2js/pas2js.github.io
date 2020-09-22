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
unit pas2js.ToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.Panel;

type
  TWToolBar = class(TCustomControl)
  private
    ToolBarItems: Array of TWPanel;
  public
    constructor Create(parent: TCustomControl); virtual;
    Procedure AddMenu(menuText, GotoForm, color: string);
    Procedure SetActiveMenu(formname: string);
  end;

implementation

uses pas2js.Application;

{ TWMenuBar }

constructor TWToolBar.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);
end;

procedure TWToolBar.AddMenu(menuText, GotoForm, color: string);
  var
    Panel0 : TWPanel;

  procedure Panel0Click(Sender:TObject);
    procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  begin
  if TJSArray(Application.FormNames).IndexOf((Sender as TWPanel).tag) > -1 then     //if form
    Application.GoToForm((Sender as TWPanel).tag) else               //then gotoform
    postMessage([self.Handle.id,'click',GoToForm],'*');   //else send message
  end;

begin
//
  Panel0 := TWPanel.Create(self);
  Panel0.SetBounds(20 + ((TJSArray(ToolBarItems).Length) * 100), 14, 90, 26);
  Panel0.SetinnerHtml(menuText);
  Panel0.setProperty('color', color);
  Panel0.setProperty('cursor','pointer');
  Panel0.SetProperty('font-size', '0.9em');

  TJSArray(ToolBarItems).push(Panel0);
  Panel0.Tag := GotoForm;
  Panel0.OnClick := @Panel0Click;
end;

procedure TWToolBar.SetActiveMenu(FormName: String);
var
  i: integer;

begin
//
  for i := 0 to TJSArray(ToolBarItems).Length - 1 do begin
    ToolBarItems[i].setProperty('font-weight', 'normal');
    ToolBarItems[i].setProperty('text-decoration', 'none');
    If ToolBarItems[i].Tag = FormName then
    begin
      ToolBarItems[i].setProperty('font-weight', 'bold');
      ToolBarItems[i].setProperty('text-decoration', 'underline');
    end;
  end;
end;

end.

