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
unit pas2js.Splitter;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.Panel;

type
  TJSEvtHandler = function(event: TJSEvent): boolean of object;

  JTouchEventHandlers = class external name 'TouchEventHandlers'
  public
    ontouchstart: TJSEvtHandler;
    ontouchend: TJSEvtHandler;
    ontouchmove: TJSEvtHandler;
    ontouchenter: TJSEvtHandler;
    ontouchleave: TJSEvtHandler;
    ontouchcancel: TJSEvtHandler;
  end;

type
  TWSplitter = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    PanelLeft, PanelRight, ReSizer: TWPanel;
  end;

implementation

{ TWSplitter }

constructor TWSplitter.create(parent: TCustomControl);

  procedure doSplitterOnReadyExecute(sender: TObject);

    function doReSizerOnTouchStart(event : TJSEvent): Boolean;
    begin
      touch2Mouse(event);
    end;

    function doReSizerOnMouseDown(e : TJSMouseEvent): Boolean;
    var
      saveX : NativeInt;
      function doOnMouseMove(e : TJSMouseEvent): Boolean;
      begin
        PanelRight.left := PanelRight.Left - Trunc(saveX - e.clientX);
        saveX := Trunc(e.clientX);
        PanelRight.width := self.Width - PanelRight.Left;
        PanelLeft.SetProperty ('cursor','w-resize');
        PanelRight.SetProperty('cursor','w-resize');
      end;
    Begin
      saveX := Trunc(e.clientX);
      TJSHTMLElement(self.Handle).onmousemove := @doOnMouseMove;
    end;

    function doSplitterOnMouseUp(event : TJSMouseEvent): Boolean;
      function doOnMouseMove(e : TJSMouseEvent): Boolean;
      begin
        //nullify mousemove
      end;
    Begin
      PanelLeft.SetProperty ('cursor','default');
      PanelRight.SetProperty('cursor','default');
      TJSHTMLElement(self.Handle).onmousemove := @doOnMouseMove;
    end;

  Begin
    console.log('OnReadyExecute');

    PanelLeft.SetProperty('height','100%');
    PanelLeft.SetProperty('width','100%');

    PanelRight.SetProperty('height','100%');
    PanelRight.Width := trunc(self.width/2);
    PanelRight.Left  := trunc(self.width/2);

    ReSizer.SetProperty('height','100%');

  //
  // event handling splitter movement
  //
    //mapping touchstart to mousedown, touchend to mouseup and touchmove to mousemove
    //see touch2Mouse in pas2js.Element.
    JTouchEventHandlers(ReSizer.Handle).ontouchstart := @doReSizerOnTouchStart;
    JTouchEventHandlers(ReSizer.Handle).ontouchmove  := JTouchEventHandlers(ReSizer.Handle).ontouchstart;
    JTouchEventHandlers(ReSizer.Handle).ontouchend   := JTouchEventHandlers(ReSizer.Handle).ontouchstart;

    TJSHTMLElement(ReSizer.Handle).onmousedown := @doReSizerOnMouseDown;

    TJSHTMLElement(self.Handle).onmouseup := @doSplitterOnMouseUp;

  end;


Begin
  inherited Create('div', parent);

  PanelLeft := TWPanel.Create(self);
  PanelRight := TWPanel.Create(self);

  ReSizer := TWPanel.Create(PanelRight);
  ReSizer.SetProperty('background-color','#ccc');
  ReSizer.SetProperty('cursor','w-resize');
  ReSizer.width := 4;

  //self.Observe;
  self.OnReadyExecute := @doSplitterOnReadyExecute;
end;

end.

