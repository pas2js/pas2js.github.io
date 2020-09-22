program project1;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, i18next;

var
  opt: JOptions;
  i18nList: TJSNodeList;
  select: TJSElement;

  procedure vCallback(currentValue : TJSNode; currentIndex: NativeInt; list : TJSNodeList);
  begin
    TJSHTMLOptionElement(currentValue).innerText :=   i18n.t( JI18n(TJSHTMLElement(currentValue).dataset).i18n );
  end;

  procedure aCallBack(error: JSValue; t: TJSFunction);
  begin
    i18nList.forEach(@vCallback);
  end;

  var
    this: TJSObject; external name 'this';

  procedure EventHandler(Event: TJSEvent);
  var
    selecionada: TJSNode;
    lng: string;

  begin
    selecionada :=  TJSHTMLSelectElement(this).options[TJSHTMLSelectElement(this).selectedIndex];
    lng := TJSElement(selecionada).getAttribute('value');
    opt:= JOptions.New;
    opt.lng:= lng;
    i18n.init(opt, @aCallBack);
  end;

begin
  // Your code here
  console.log('Program startup...');

  opt:= JOptions.New;
  opt.lng:= 'en';
  opt.resGetPath:= 'locales/__lng__.jrs';
  // initializate the i18next
  i18n.init( opt );

  i18nList := document.querySelectorAll('[data-i18n]');
  i18nList.forEach(@vCallback);

  select := document.querySelector('select');
  select.addEventListener('change', @EventHandler);

  // Set language after init:
  i18n.setLng('en', procedure(err, t: JSValue)
  begin
  (* loading done *)
     i18nList.forEach(@vCallback);
  end);

end.

