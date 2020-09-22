program projPromises;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, uMain;

var
  This: TJSHTMLElement; external name 'this';

procedure onBtnGroupClick;
var i: integer;
    x: TJSNodeList;

begin
   x := document.querySelectorAll('.tab-link');
  for i := 0 to x.length - 1 do
    TJSHTMLElement(x[i]).classList.remove('active');

  if (document.querySelector('.tab-link').classList.contains('active')) then
    this.classList.remove('active')
  else
    this.classList.toggle('active');
end;

var i: integer;
    x: TJSNodeList;

begin
  // Your code here
  x := document.querySelectorAll('.tab-link');
  for i := 0 to x.length - 1 do
    TJSHTMLElement(x[i]).addEventListener('click', @onBtnGroupClick);

  document.querySelector('.tab1').addEventListener('click', @fetchImgSequentially);
  document.querySelector('.tab2').addEventListener('click', @FetchImgAtOnce);
  document.querySelector('.tab3').addEventListener('click', @FetchImgAtAll);
end.
