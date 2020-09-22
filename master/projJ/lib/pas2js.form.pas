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
unit pas2js.Form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element;

type
  TWForm = class(TCustomControl)
  public
    procedure InitializeForm; virtual;
    procedure InitializeObject; virtual;
    procedure ReSize; virtual;
    constructor Create(parent: TCustomControl); virtual;
    Caption : String;
  end;

  TFormClass = class of TWForm;

implementation

uses
  pas2js.Globals;

{ TW3Form }

constructor TWForm.Create(parent: TCustomControl);

  procedure doOnResize(sender: TObject);
  begin
    screenwidth := window.innerWidth;
    ReSize;
  end;

begin
  inherited Create('div', parent);
  SetProperty('border','1px double #2196f3');
  Left := 5; Top := 5;
  setProperty('width','calc(100% - 12px)');
  setProperty('height','calc(100% - 12px)');
  setProperty('background-color','white');

  (* This forces the browsers that support it to
     use the GPU rather than CPU for movement *)
  self.setProperty('will-change','transform');
  self.setProperty('-webkit-transform','translateZ(0px)');
  self.setProperty(   '-moz-transform','translateZ(0px)');
  self.setProperty(    '-ms-transform','translateZ(0px)');
  self.setProperty(     '-o-transform','translateZ(0px)');
  self.setProperty(        'transform','translateZ(0px)');

  OnResize := @doOnResize;

end;

Procedure TWForm.InitializeForm;
begin
  //clear form
  self.Clear;
end;

Procedure TWForm.InitializeObject;
begin
//
end;

Procedure TWForm.ReSize;
begin
//
end;

end.

