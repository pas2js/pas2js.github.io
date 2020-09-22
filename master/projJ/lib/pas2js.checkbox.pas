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
unit pas2js.CheckBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.Panel;

type
  TWCheckBox = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    Checked: Boolean;
    &Label: String;
    CheckBoxDimension: integer; // := 20;
    Box : TCustomControl;
  end;

var
  CheckImage: String;

implementation



{ TWCheckBox }

constructor TWCheckBox.Create(parent: TCustomControl);

  procedure doBoxOnClick(sender: TObject);
    procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  begin
    Checked := not Checked;
    If Checked then
      self.Box.SetProperty('background-image',CheckImage) else
      self.Box.SetProperty('background-image','none');
    postMessage([self.Handle.id,'click',checked],'*');
  end;

  procedure doCheckBoxOnClick(sender: TObject);
  var
    Label1 : TWPanel;
  begin
    If Checked then
      self.Box.SetProperty('background-image',CheckImage);

    Label1 := TWPanel.Create(self);
    Label1.SetinnerHTML(&Label);
    Label1.OnClick := self.Box.OnClick;
    Label1.SetProperty('cursor','pointer');
    Label1.SetProperty('font-size', '0.9em');
    TJSHTMLElement(Label1.Handle).style.setProperty('width' ,'auto');
    TJSHTMLElement(Label1.Handle).style.setProperty('height' ,'auto');

    self.Box.SetBounds(0,0,CheckBoxDimension,CheckBoxDimension);
    Label1.SetBounds(trunc(CheckBoxDimension*1.5),
                     self.Box.top+CheckBoxDimension-Label1.Handle.clientHeight,
                     Label1.Handle.clientWidth+2,
                     CheckBoxDimension-CheckBoxDimension+Label1.Handle.clientHeight);

  end;

begin
  inherited Create('div', parent);                //background for checkbox & label
  CheckBoxDimension := 20;

  //create checkbox
  self.Box := TCustomControl.Create('div', self);

  self.Box.SetProperty('border','1px solid silver');
  self.Box.SetProperty('border-radius','5px');
  self.Box.setProperty('background-size', 'cover');
  self.Box.SetProperty('cursor','pointer');
  self.Box.Width  := CheckBoxDimension;
  self.Box.Height := CheckBoxDimension;

  self.Box.OnClick := @doBoxOnClick;

  self.OnClick := self.Box.OnClick;

  //self.Observe;
  self.OnReadyExecute := @doCheckBoxOnClick;

end;

initialization

  CheckImage := 'url(data:image/jpeg;base64,'+
    '/9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAHgAA/+4ADkFkb2JlAGTAAAAAAf/bAIQ' +
    'AEAsLCwwLEAwMEBcPDQ8XGxQQEBQbHxcXFxcXHx4XGhoaGhceHiMlJyUjHi8vMzMvL0BAQEBAQEBAQEB' +
    'AQEBAQAERDw8RExEVEhIVFBEUERQaFBYWFBomGhocGhomMCMeHh4eIzArLicnJy4rNTUwMDU1QEA/QEB' +
    'AQEBAQEBAQEBA/8AAEQgAMgAyAwEiAAIRAQMRAf/EAH4AAQADAQEBAAAAAAAAAAAAAAABAgMFBAYBAAM' +
    'BAQAAAAAAAAAAAAAAAAABAwIFEAACAgEDAAcHBQAAAAAAAAABAgADBBEhMVFhIjJCMwVBsdESUmITcYG' +
    'hkgYRAAIBAwIGAwAAAAAAAAAAAAACARESAzFRIUFhccEiMhME/9oADAMBAAIRAxEAPwCCzMSzEszbsx3' +
    'JJ5JiQOJM75wBERABE1x8bIyX/Hj1mxwNSB7B1k7TN0et2R1Kup0ZTsQRFWK0rFdh0mlaTTfkV0HREmI' +
    'xEDibLiZTUHTWpjQvesA225mI4nf9B9WrRFwMjRV1Iqc8do6/K37naTzO6Jci30njHTmUwojta7WVjhP' +
    'XkcGb4eHfm3iigasd2Y8KvSZ1/UP865yFbBAFVh7aHYV9Y+3qnvTWvQcLbtO3s8dr/AfxIv8AqWVj6vd' +
    '30XbuWT8rQ0/b6Imrb9gTheg4Wg7TtwPHa/w90+WyL3yb3vs0+ew6nTiWy8u/Mva+9tWPAHCj6V6pjN4' +
    'MNlWabsj/ACnwYz5r6KsW40+MeRERLkCBxJkDgSYAdrA/0b49H4slGuKDStwRr+j6++cvLy78y833tqx' +
    '2AHCj6VmMSa4catLKtJYo2bIywrNMwoiIlCYiIgBe7z7e7328vy+fB9vRKREUaR2CdZEREYCIiAFP7cx' +
    'ERmT/2Q==)';

end.

