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
unit pas2js.Element;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.MutationObserver;

type
  TMouseClickEv = procedure(sender: TObject);
  TMouseClickEvent = procedure(sender: TObject) of object;
  TResizeEvent = procedure(sender: TObject) of object;
  TReadyExecuteEvent = procedure(sender: TObject) of object;

type
  TCustomControl = class  //fake TCustomControl is the base class.
  private
    { private declarations }
    FTag: String;
    FName: String;
    FHandle: TJSHTMLElement;
    FOnClick: TMouseClickEvent;
    FOnResize: TResizeEvent;
    FOnReadyExecute: TReadyExecuteEvent;
    procedure SetLeft(aLeft: integer);
    function GetLeft: Integer;
    procedure SetTop(aTop: integer);
    function GetTop: Integer;
    procedure SetWidth(aWidth: integer);
    function GetWidth: Integer;
    procedure SetHeight(aHeight: integer);
    function GetHeight: Integer;
    procedure _setMouseClick(const aValue: TMouseClickEvent);
    procedure _setOnResize(const aValue: TResizeEvent);
    procedure _setOnReadyExecute(const aValue: TReadyExecuteEvent);
  public
    { public declarations }
    constructor Create(element: string; parent: TCustomControl);
    destructor Destroy; override;

    procedure SetProperty(S1: string; S2: string);
    procedure SetAttribute(S1: string; S2: string);
    procedure SetBounds(aleft, atop, awidth, aheight: integer);
    procedure SetinnerHTML(S1: string);
    function GetinnerHTML: string;
    procedure CBClick(eventObj: TJSEvent); virtual;
    procedure CBResize(eventObj: TJSEvent); virtual;
    property OnReadyExecute: TReadyExecuteEvent read FOnReadyExecute write
      _setOnReadyExecute;
    procedure CBReadyExecute(eventObj: TJSEvent); virtual;
    procedure Observe;
    procedure Clear;
    procedure touch2Mouse(e: TJSEvent);
    property Handle: TJSHTMLElement read FHandle write FHandle;
  published
    { published properties }
    property Name: String read FName write FName stored False;
    property Tag: String read FTag write FTag {default 0};

    property Left: Integer read getLeft write setLeft;
    property Top: Integer read getTop write setTop;
    property Width: Integer read getWidth write setWidth;
    property Height: Integer read getHeight write setHeight;
    property OnClick: TMouseClickEvent read FOnClick write _setMouseClick;
    property OnReSize: TResizeEvent read FOnResize write _setOnResize;
  end;

  { global methods }
  function StrBefore(s: String; d: String): String;
  function StrEndsWith(s: string; e: JSValue): JSValue;

var
  ScreenWidth: Integer;

implementation

var
  uniqueNum: Integer;

function generateID: string;
begin
  Inc(uniqueNum);
  Result := 'OBJ' + IntToStr(uniqueNum);
end;

function StrEndsWith(s: string; e: JSValue): JSValue;
begin
  asm
  { Result=s.substr(s.length-e.length)==e }
  end;
end;

function StrBefore(s: String; d: String): String;
begin
  asm
    if(!d)Result=s;var p=s.indexOf(d);Result=(p<0)?s:s.substr(0,p)
  end;
end;


{ TCustomControl }

constructor TCustomControl.Create(element: string; parent: TCustomControl);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  // cache element
  Handle := TJSHTMLElement(document.createElement(element));
  Handle.className := Self.ClassName;
  Handle.id := generateID;

  HandleStyle := TJSHTMLElement(Handle).style;
  HandleStyle.setProperty('visibility', 'visible');
  HandleStyle.setProperty('display', 'inline-block');
  HandleStyle.setProperty('position', 'absolute');
  HandleStyle.setProperty('overflow', 'auto');

  if parent = nil then
    TJSNode(Self.Handle) := document.body.appendChild(Self.Handle)
  else
    TJSNode(Self.Handle) := parent.Handle.appendChild(Self.Handle);

  SetBounds(0, 0, 0, 0);

  Handle.addEventListener('click', @CBClick);
  window.addEventListener('resize', @CBResize);
  Handle.addEventListener('readyexecute', @CBReadyExecute);
  Observe;
end;

destructor TCustomControl.Destroy;
begin
  Self.Handle.parentNode.removeChild(Handle);
  inherited Destroy;
end;

procedure TCustomControl.SetProperty(S1: string; S2: string);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  HandleStyle := TJSHTMLElement(Handle).style;
  HandleStyle.setProperty(S1, S2);
end;

procedure TCustomControl.SetAttribute(S1: string; S2: string);
begin
  Handle.setAttribute(S1, S2);
end;

procedure TCustomControl.SetBounds(aleft, atop, awidth, aheight: integer);
begin
  left:= aleft;
  top:= atop;
  width:= awidth;
  height:= aheight;
end;

procedure TCustomControl.SetinnerHTML(S1: string);
begin
  Handle.innerHTML:= S1;
end;

function TCustomControl.GetinnerHTML: string;
begin
  Result := Handle.innerHTML;
end;

procedure TCustomControl._setMouseClick(const aValue: TMouseClickEvent);
begin
  FOnClick := aValue;
end;

procedure TCustomControl.CBClick(eventObj: TJSEvent);
begin
  eventObj.stopPropagation;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCustomControl._setOnResize(const aValue: TResizeEvent);
begin
  FOnResize := aValue;
end;

procedure TCustomControl._setOnReadyExecute(const aValue: TReadyExecuteEvent);
begin
  FOnReadyExecute := aValue;
end;

procedure TCustomControl.CBResize(eventObj: TJSEvent);
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TCustomControl.CBReadyExecute(eventObj: TJSEvent);
begin
  //  eventObj.stopPropagation;
  if Assigned(FOnReadyExecute) then
    FOnReadyExecute(Self);
end;

procedure TCustomControl.SetLeft(aLeft: integer);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  HandleStyle := TJSHTMLElement(Handle).style;
  HandleStyle.setProperty('left', inttostr(aLeft) + 'px');
end;

function TCustomControl.GetLeft: Integer;
var
  HandleStyle: TJSCSSStyleDeclaration;
  S: string;
begin
  HandleStyle := TJSHTMLElement(Handle).style;

  S := HandleStyle.getPropertyValue('left');
  if StrEndsWith(S, 'px') then
    SetLength(S, TJSString(S).Length - 2);
  //  alternatively : if Pos('px',S) > 0 then SetLength(S, TJSString(S).Length-2);
  Result := parseInt(S, 10);
end;

procedure TCustomControl.SetTop(aTop: integer);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  HandleStyle := TJSHTMLElement(Handle).style;
  HandleStyle.setProperty('top', inttostr(aTop) + 'px');
end;

function TCustomControl.GetTop: Integer;
var
  HandleStyle: TJSCSSStyleDeclaration;
  S: string;
begin
  HandleStyle := TJSHTMLElement(Handle).style;

  S := HandleStyle.getPropertyValue('top');
  if StrEndsWith(S, 'px') then
    SetLength(S, TJSString(S).Length - 2);

  Result := parseInt(S, 10);
end;

procedure TCustomControl.SetWidth(aWidth: integer);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  HandleStyle := TJSHTMLElement(Handle).style;
  if aWidth = screenwidth then
    HandleStyle.setProperty('width', 'calc(100%)')
  else
    HandleStyle.setProperty('width', inttostr(aWidth) + 'px');
end;

function TCustomControl.GetWidth: Integer;
var
  HandleStyle: TJSCSSStyleDeclaration;
  S: string;
begin
  HandleStyle := TJSHTMLElement(Handle).style;

  S := HandleStyle.getPropertyValue('width');
  if StrEndsWith(S, 'px') then
    SetLength(S, TJSString(S).Length - 2);
  Result := parseInt(S, 10);
end;

procedure TCustomControl.SetHeight(aHeight: integer);
var
  HandleStyle: TJSCSSStyleDeclaration;
begin
  HandleStyle := Handle.style;
  HandleStyle.setProperty('height', inttostr(aHeight) + 'px');
end;

function TCustomControl.GetHeight: Integer;
var
  HandleStyle: TJSCSSStyleDeclaration;
  S: string;
begin
  HandleStyle := TJSHTMLElement(Handle).style;

  S := HandleStyle.getPropertyValue('height');
  if StrEndsWith(S, 'px') then
    SetLength(S, TJSString(S).Length - 2);
  Result := parseInt(S, 10);
end;

procedure TCustomControl.Clear;
begin
  while assigned(Handle.firstChild) do
    Handle.removeChild(Handle.firstChild);
end;

type
  TMutationObserver = class
  protected
    constructor Create; virtual;
    procedure CBMutationChange(mutationRecordsList: TJSMutationRecordArray);
      virtual;
  public
    Handle: JSValue;
  end;

  //#############################################################################
  // TMutationObserver
  //#############################################################################

constructor TMutationObserver.Create;
var
  mRef: TJSSubscribeCallback;

  procedure subscribe(mutationRecordsList: TJSMutationRecordArray);
  begin
    mRef(mutationRecordsList);
  end;

begin
  inherited Create;
  mRef := @CBMutationChange;
  Handle := TJSMutationObserver.new(@subscribe);
end;

procedure TMutationObserver.CBMutationChange(mutationRecordsList:
  TJSMutationRecordArray);
var
  //  LEvent: JSValue;
  LEvent: TJSEvent;
begin
  TJSMutationObserver(Handle).disconnect();
  // edit Web.pas and adds to TJSEvent external class the following constructor:
  // constructor new (aType : String; const aInit : TJSObject); overload;
  LEvent := TJSEvent.new('readyexecute', nil);
  //asm LEvent = new Event('readyexecute'); end;
  //mutationRecordsList[length(mutationRecordsList)-1].target.dispatchEvent(LEvent);
  mutationRecordsList[TJSArray(mutationRecordsList).Length - 1].target.dispatchEvent(LEvent);
end;

function observerConfig: TJSObject;
{ ╔════Initialize paramater═════════════════════════════════════════════════╗
  ║ Options for the observer (which mutations to observe)                   ║
  ║ by default all false. However, you can pick as many as you want,        ║
  ║ but at least one of - attributes, characterData, or childList           ║
  ║                                                                         ║
  ║ var optParams = {"childList": true, "attributes": true}                 ║
  ╚═════════════════════════════════════════════════════════════════════════╝}
begin
  Result :=
    new([
    'attributes', true
      { attribute changes will be observed | on add/remove/change attributes }
     ,'attributeOldValue', true
      { will show oldValue of attribute | on add/remove/change attributes | default: null }
     //,'characterData', true         { data changes will be observed | on add/remove/change characterData }
     //,'characterDataOldValue', true { will show OldValue of characterData | on add/remove/change characterData | default: null }
    ,'childList', true { target childs will be observed | on add/remove }
    //,'subtree', true               { target childs will be observed | on attributes/characterData changes if they observed on target }
    //,'attributeFilter', TJSArray.new('style')  { filter for attributes | array of attributes that should be observed, in this case only style }
    ]);
end;

function observerConfiguration: TJSMutationObserverInit;
begin
  result := TJSMutationObserverInit.new;
  result.attributes := true;
  result.attributeOldValue := true;
  result.childList := true;
end;

procedure TCustomControl.Observe;
var
  MyObserver: TMutationObserver;
begin
  //console.log('Observe was called');
  MyObserver := TMutationObserver.Create;
  TJSMutationObserver(MyObserver.Handle).observe(TJSNode(Handle),
    observerConfiguration);
end;

procedure TCustomControl.touch2Mouse(e: TJSEvent);
var
  theTouch: TJSTouch;
  mouseEv: string;
  mouseEvent: TJSEvent;
  eventOptions: TJSObject;
  op: TJSEventInit;
  event: TJSMouseEvent;
begin
  //mapping touch events to mouse events. See JSplitter for example
  //https://www.codicode.com/art/easy_way_to_add_touch_support_to_your_website.aspx

  theTouch := TJSTouchEvent(e).changedTouches[0];

  case e._type of
    'touchstart': mouseEv := 'mousedown';
    'touchend': mouseEv := 'mouseup';
    'touchmove': mouseEv := 'mousemove';
  else
    exit;
  end;

  mouseEvent := document.createEvent('MouseEvent');
  // Edit Web.pas and adds the following:
  (* procedure initMouseEvent(const typeArg: string; bubblesArg, cancelableArg: boolean;
        viewArg: TJSAbstractView; detailArg, screenXArg, screenYArg, clientXArg,
        clientYArg: Integer; ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg: boolean;
        buttonArg: Integer; relatedTargetArg: TJSEventTarget); overload;
     procedure initMouseEvent(const typeArg: string; bubblesArg, cancelableArg: boolean;
        viewArg: TJSWindow; detailArg, screenXArg, screenYArg, clientXArg,
        clientYArg: Integer; ctrlKeyArg, altKeyArg, shiftKeyArg, metaKeyArg: boolean;
        buttonArg: Integer; relatedTargetArg: TJSEventTarget); overload;
  *)
  TJSMouseEvent(mouseEvent).initMouseEvent(mouseEv, true, true, window, 1,
    theTouch.screenX, theTouch.screenY, theTouch.clientX, theTouch.clientY, false,
    false, false, false, 0, nil);
  theTouch.target.dispatchEvent(mouseEvent);
  e.preventDefault();

  (*
    //op.bubbles:= true;
    //op.cancelable:= true;
    //op.composed:=;
    //op.scoped:=;

      eventOptions := new([
      'view', window,
      'bubbles', true,
      'cancelable', true,
      'screenX', theTouch.screenX,
      'screenY', theTouch.screenY,
      'clientX', theTouch.clientX,
      'clientY', theTouch.clientY,
      'button', 0
      //'offsetX': mouseX - player.offset().left,
      //'offsetY': mouseY - player.offset().top,
      //'pageX': mouseX,
      //'pageY': mouseY
    ]);

    (* Though the MouseEvent.initMouseEvent() method is kept for backward compatibility,
       creating of a MouseEvent object should be done using the MouseEvent() constructor. *)
    //event := TJSMouseEvent.new(mouseEv, eventOptions);
  theTouch.target.dispatchEvent(event);
  *)

(*
var event = new MouseEvent('mousedown', {
    'view': window,
    'bubbles': true,
    'cancelable': true,
    'screenX': datePosition.left,
    'screenY': datePosition.top
});

dateTile.dispatchEvent(event);
*)
end;

initialization
  ScreenWidth := window.innerWidth;

end.

(*
procedure initMouseEvent(const
      typeArg: string;                 = mouseEv
      bubblesArg: boolean;             = true
      cancelableArg: boolean;          = true
      viewArg: TJSWindow;              = window
      detailArg: Integer;              = 1
      screenXArg: Integer;             = theTouch.screenX
      screenYArg: Integer;             = theTouch.screenY
      clientXArg: Integer;             = theTouch.clientX
      clientYArg: Integer;             = theTouch.clientY
      ctrlKeyArg: boolean;             = false
      altKeyArg: boolean;              = false
      shiftKeyArg: boolean;            = false
      metaKeyArg: boolean;             = false
      buttonArg: Integer;              = 0
      relatedTargetArg: TJSEventTarget = nil
      );
*)

