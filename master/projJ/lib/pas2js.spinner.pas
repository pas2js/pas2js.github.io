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
unit pas2js.Spinner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element;

type
  TWSpinner = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation



{ TWSpinner }

constructor TWSpinner.Create(parent: TCustomControl);
var
  s: String;

begin
  inherited Create('div', parent);
  setProperty('-webkit-animation','sk-rotate 2.0s infinite linear');
  setProperty('animation','sk-rotate 2.0s infinite linear');
(*
  s :=
  '.dot1'+
  '{'+
  'width: 60%;'+
  'height: 60%;'+
  'display: inline-block;'+
  'position: absolute;'+
  'top: 0;'+
  'background-color: #699BCE;'+
  'border-radius: 100%;'+
  '-webkit-animation: sk-bounce 2.0s infinite ease-in-out;'+
  'animation: sk-bounce 2.0s infinite ease-in-out;'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '.dot2'+
  '{'+
  'width: 60%;'+
  'height: 60%;'+
  'display: inline-block;'+
  'position: absolute;'+
  'top: 0;'+
  'background-color: #699BCE;'+
  'border-radius: 100%;'+
  '-webkit-animation: sk-bounce 2.0s infinite ease-in-out;'+
  'animation: sk-bounce 2.0s infinite ease-in-out;'+
  'top: auto;'+
  'bottom: 0;'+
  '-webkit-animation-delay: -1.0s;'+
  'animation-delay: -1.0s;'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);

//
  s :=
  '@-webkit-keyframes sk-rotate {'+
  '100% {'+
  '-webkit-transform: rotate(360deg)'+
  '}'+
  '}';

  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@keyframes sk-rotate {'+
  '100% {'+
  'transform: rotate(360deg);'+
  '-webkit-transform: rotate(360deg)'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@-webkit-keyframes sk-bounce {'+
  '0%,'+
  '100% {'+
  '-webkit-transform: scale(0.0)'+
  '}'+
  '50% {'+
  '-webkit-transform: scale(1.0)'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@keyframes sk-bounce {'+
  '0%,'+
  '100% {'+
  'transform: scale(0.0);'+
  '-webkit-transform: scale(0.0);'+
  '}'+
  '50% {'+
  'transform: scale(1.0);'+
  '-webkit-transform: scale(1.0);'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
*)
//
  self.SetinnerHTML('<div class="dot1"></div><div class="dot2"></div>');

end;

end.

