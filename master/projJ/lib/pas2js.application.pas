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
unit pas2js.Application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.Form;

type
  TWApplication = class(TCustomControl)
  public
    FormNames: Array of String;
    FormsClasses: Array of TFormClass;       //TFormClass = class of TWForm;
    FormsInstances: Array of TWForm;
    constructor Create(parent: TCustomControl); virtual;
    procedure CreateForm(FormName: String; aClassType: TFormClass);
    procedure GoToForm(FormName: String);
  end;

var
  Application : TWApplication;

implementation

{ TWApplication }

constructor TWApplication.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);

  setProperty('width','100%');
  setProperty('height','100%');
  setProperty('background-color','white');

end;

procedure TWApplication.CreateForm(FormName: String; aClassType: TFormClass);
begin
  TJSArray(FormNames).push(FormName);
  TJSArray(FormsClasses).push(aClassType);
  TJSArray(FormsInstances).push(nil);
end;

(*
,Show:function(Self, FormName$1) {
   var i = 0;
   var $temp1;
   for(i=0,$temp1=Self.FormNames.length;i<$temp1;i++) {
      if (Self.FormsInstances[i]!==null) {
         TCustomControl.SetProperty(Self.FormsInstances[i],"display","none");
      }
      if (Self.FormNames[i]==FormName$1) {
         if (Self.FormsInstances[i]===null) {
            Self.FormsInstances[i]=TWForm.Create$5($NewDyn(Self.FormsClasses[i],""),Self);
            TWForm.ShowForm$(Self.FormsInstances[i]);
         } else {
            TWForm.ClearForm(Self.FormsInstances[i]);
            TWForm.ShowForm$(Self.FormsInstances[i]);
            TCustomControl.SetProperty(Self.FormsInstances[i],"display","inline-block");
         }
      }
   }
}
*)
procedure TWApplication.GoToForm(FormName: String);
var
  i: integer;
begin
//
  For i := 0 to TJSArray(FormNames).length -1 do begin
    If FormsInstances[i] <> nil then
      FormsInstances[i].SetProperty('display','none');
    If FormNames[i] = FormName then begin
      If FormsInstances[i] = nil then       //form has never been displayed yet
        FormsInstances[i] := FormsClasses[i].Create(self) else
        FormsInstances[i].SetProperty('display','inline-block');

      //TW3Form(FormsInstances[i]).InitializeForm;    //ClearForm;
      //console.log(FormsClasses[i]);    //ClearForm;
      { TODO warleyalex }
      (*
      this.Dt$ = function ($) {
  		return $.ClassType.Dt($)
  	},
  	this.YC$ = function ($) {
  		return $.ClassType.YC($)
  	},
      *)
      //*this.FormsClasses[i](this.FormsInstances[i])
      TWForm(FormsInstances[i]).InitializeForm;
      TWForm(FormsInstances[i]).InitializeObject;
        //(FormsInstances[i] as FormsClasses[i]).InitializeForm;    //ClearForm;
        //(FormsInstances[i] as FormsClasses[i]).InitializeObject;  //ShowForm;
    end;
  end;
end;

initialization
Application := TWApplication.Create(nil);

end.

