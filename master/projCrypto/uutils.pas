unit uUtils;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web, Types;

type
  JEventListenerHandler = reference to procedure(event: TJSEvent);

type
  JElement = class external name 'Element' (TJSElement)
  Public
    addEventListenerExists: boolean; external name 'addEventListener';
    attachEventExists : boolean; external name 'attachEvent';
    procedure addEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure removeEventListener(aname: String; callback: JEventListenerHandler; capture : Boolean = false);
    procedure attachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
    procedure detachEvent(eventNameWithOn: String; callback: JEventListenerHandler);
  end;

  procedure bindEvent(element: TJSElement; EventType: String;
    handler: JEventListenerHandler);

implementation

procedure bindEvent(element: TJSElement; EventType: String;
  handler: JEventListenerHandler);
var
  events : TStringDynArray;
   i: Integer;

begin
  events := TJSString(EventType).split(' ');

  (* check if addeventlistener exists / For all major browsers *)
  if (JElement(element).addEventListenerExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      JElement(element).addEventListener(events[i], handler, false);
  end else
  (* check if attachEvent exists / For IE 8 and earlier *)
  if (JElement(element).attachEventExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      JElement(element).attachEvent('on'+events[i], handler);
  end;
end;

end.

