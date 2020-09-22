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
unit pas2js.Select;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.ListBox, pas2js.Panel, pas2js.Image;

type
  TWSelect = class(TCustomControl)
  private
    ListBox: TWListBox;
    Panel: TWPanel;
    Chevron: TWImage;
  public
    constructor Create(parent: TCustomControl); virtual;
    Procedure Add(item: TCustomControl);
    Value : String;
  end;

implementation



{ TWSelect }

constructor TWSelect.Create(parent: TCustomControl);
  procedure doPanelClick(sender: TObject);
  begin
    ListBox.SetProperty('display','inline-block');
  end;

  procedure doOnReadyExecute(sender: TObject);
  begin
    Panel.SetBounds(0,0,width-2,20);
    Chevron.SetBounds(width-22,2,16,16);
    Chevron.SetProperty('max-height','16px');
    Chevron.SetProperty('max-width','16px');
    ListBox.Width := self.Width;
    ListBox.Height := self.Height - 22;
  end;

begin
  inherited Create('div', parent);

  Panel := TWPanel.Create(self);
  Panel.OnClick := @doPanelClick;
  Panel.SetProperty('border','1px solid silver');
  Panel.SetinnerHTML('select...');

  Chevron := TWImage.Create(self);
  Chevron.SetAttribute('src','images/chevron-down.png');
  Chevron.OnClick := Panel.OnClick;

  ListBox := TWListBox.Create(self);
  Listbox.SetProperty('display','none');
  ListBox.Top := 22;

  //self.Observe;
  self.OnReadyExecute := @doOnReadyExecute;
end;

procedure TWSelect.Add(item: TCustomControl);
  procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  procedure doSelectOnClick(Sender:TObject);
  begin
    Panel.SetInnerHTML( (Sender as TCustomControl).tag);
    Value := (Sender as TCustomControl).tag;
    postMessage([self.Handle.id,'click',value],'*');
    Listbox.SetProperty('display','none');
  end;
begin
//
  ListBox.Add(item);
  Item.OnClick := @doSelectOnClick;

end;

end.

