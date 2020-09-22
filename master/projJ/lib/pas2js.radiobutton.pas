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
unit pas2js.RadioButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.Panel;

type
  TWRadioButton = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    Checked: Boolean;
    &Label: String;
    RadioButtonDimension: integer; // := 16;
    Button : TCustomControl;
  end;

var
  CheckImage: String;

implementation



{ TWRadioButton }

constructor TWRadioButton.Create(parent: TCustomControl);
    procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
    procedure doButtonOnClick(sender: TObject);
    var
      i, j: Integer;
      d: TJSHTMLCollection;
      e: TJSHTMLCollection;

    begin
      //set all radiobuttons to un-selected
      d := TJSHTMLElement(TJSNode(self.Handle).parentNode).children;
      for i := 0 to d.length -1 do begin
        e := TJSElement(d[i]).children;
        for j := 0 to e.length -1 do begin
          TJSHTMLElement(e[j]).style.setProperty('background-image', 'none');
        end;
      end;
      //set clicked one to selected
      self.Button.SetProperty('background-image',CheckImage);
      postMessage([self.Button.Handle.id,'click',checked],'*');
    end;

    procedure doRadioButtonOnClick(sender: TObject);
    var
      Label1 : TWPanel;

    begin
      If Checked then
        self.Button.SetProperty('background-image',CheckImage);

      Label1 := TWPanel.Create(self);
      Label1.SetinnerHTML(&Label);
      Label1.OnClick := Button.OnClick;
      Label1.SetProperty('cursor','pointer');
      Label1.SetProperty('font-size', '0.85em');
      TJSHTMLElement(Label1.Handle).style.setProperty('width', 'auto');
      TJSHTMLElement(Label1.Handle).style.setProperty('height', 'auto');

      self.Button.SetBounds(0,0,RadioButtonDimension,RadioButtonDimension);
      Label1.SetBounds(trunc(RadioButtonDimension*1.5),
                       self.Button.top+RadioButtonDimension-Label1.Handle.clientHeight+2,
                       Label1.Handle.clientWidth+2,
                       RadioButtonDimension-RadioButtonDimension+Label1.Handle.clientHeight);

    end;


begin
  inherited Create('div', parent);                //background for RadioButton & label
  RadioButtonDimension := 16;

  //create RadioButton
  self.Button := TCustomControl.Create('div', self);

  self.Button.SetProperty('border','1px solid #0066cc');
  self.Button.SetProperty('border-radius','50%');
  self.Button.setProperty('background-size', 'cover');
  self.Button.SetProperty('cursor','pointer');
  self.Button.Width  := RadioButtonDimension;
  self.Button.Height := RadioButtonDimension;

  self.Button.OnClick := @doButtonOnClick;

  self.OnClick := self.Button.OnClick;

  //self.Observe;
  self.OnReadyExecute := @doRadioButtonOnClick;

end;

initialization
  CheckImage := 'url(data:image/jpeg;base64,'+
     '/9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAHgAA/+4ADkFkb2JlAGTAAAAAAf/bAIQ' +
     'AEAsLCwwLEAwMEBcPDQ8XGxQQEBQbHxcXFxcXHx4XGhoaGhceHiMlJyUjHi8vMzMvL0BAQEBAQEBAQE' +
     'BAQEBAQAERDw8RExEVEhIVFBEUERQaFBYWFBomGhocGhomMCMeHh4eIzArLicnJy4rNTUwMDU1QEA/Q' +
     'EBAQEBAQEBAQEBA/8AAEQgAOQA5AwEiAAIRAQMRAf/EAI8AAAICAwEAAAAAAAAAAAAAAAUGAAQBAgcD' +
     'AQEBAQEBAAAAAAAAAAAAAAAFAwQBAhAAAQMDAAUHCgcAAAAAAAAAAQACAxEEBSExQRITUWGBkSJCBnG' +
     'hscHRMlJyIzPhYpKyUxQWEQACAQMCAwcFAQAAAAAAAAABAhEAEgMTBCFBUWGBocEiQhQxcZEycpL/2g' +
     'AMAwEAAhEDEQA/AOgKnf5O1sGVldV592Nulx9imUyDLC2Mp0yO7MTeV3sCTZppZ5HSyuL5HGrnFattt' +
     'tT1NwQeNZdzudP0rxc+FE7nxHfykiHdgZsoN53W72KmcpkSam5k/UR6FVWKhIrhxqICL+KObNkYyXb8' +
     '0RhzuTiNeNxB8LwHfiTWP8Q29yRFcAQSnQDXsOPl2dKVVF4ybbE4/UKeq8K949zlQ/sWHRuNdAWUA8P' +
     '5VzyLG4dV1PovOug7h9SPo74z6ul3zyt60j8lNLV7o53dKUvENy6bIGLuQANHlPacULVjIEm+uCdfFf' +
     '8AuKrpXEoXGijkoorKxbI7HmxoxhcK27b/AGbmvArRjBo36ayTyJjjs7WJgZHCxrRs3QvPGtY3H24j9' +
     '3htPWKnzq0is+Z3dpJgGAKVwYURFgCSJJoRkcBbXDHSWzRDONIA0NceQjYlUgtJaRQg0I5wugpLzLWt' +
     'ydwG6t4HpIBK1bLMzEoxugSJrLvcKqA6i2TBiqkcj4pGyxmj2EOaecJj/wBNb/xuS0taBbbRcG5gR3G' +
     'sVxtK8iZ7xRPPW7oMlIaUbLSRvTr86HJvzWNN9bAx/fiqWfmB1t6UoEFpIcKEaCDoIIUNrlD4wPcgtP' +
     'lV91iKZCfa5uHnTDgMtCyIWVy4MLT9J50NIPdJR/XpC5+t2zztG62R7W8gcQFPNsg7Fla2eJETVMO9K' +
     'KFZbo4AzFOOQydvYxEvcHTU7EQ1k8/IEmySPlkdLIaveS5x5ytSSTUmpOslRWwbdcQMG4n6mo59w2Ui' +
     'RaB9BUAJIDRUnQANpRr/ADE/xt6lPD2NdLML2UUijP069542+QelM65rrrjFPtP+uld0G0Dlj3D/AD1' +
     'qIXk8HBekyxnhXB1u7rvmHrRRRGYdS8aU3dnnSebTsOrFvb5Uk3OKv7YniQuLR32Deb1hVCCDQihXQV' +
     'TuPvMSqnNHqGM/ZiPKimGGfScg+6g+dJ8NrczmkMT3k8gNOtGsf4bcSJL40GyFp1/M4epMLdSypZ/k2' +
     'mwKP5Mt4xVcHxrheWP9CF8JrVrWsaGMAa1ooANAAC2UURfGe2lOEdlf/9k=)';

end.

