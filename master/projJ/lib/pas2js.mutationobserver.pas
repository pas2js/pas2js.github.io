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
unit pas2js.MutationObserver;

{$MODE objfpc}{$H+}
{$MODESWITCH externalclass}

interface

uses
  Classes, SysUtils, JS, Web;

type
  TJSMutationObserver = class;
  TJSMutationObserverInit = class;
  TJSMutationRecord = class;
  TJSMutationRecordArray = array of TJSMutationRecord;
  TStringDynArray = array of string;
  TJSMutationCallback = procedure(mutations: TJSMutationRecordArray;
      observer: TJSMutationObserver);
  TJSSubscribeCallback = procedure(mutationRecordsList: TJSMutationRecordArray)
      of object;

{ ╔════════Constructor for instantiating new DOM mutation observers═══════════════════╗
  ║                                                                                   ║
  ║ More info here: https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver ║
  ║ Objects list:   https://developer.mozilla.org/en-US/docs/Web/API/MutationRecord   ║
  ╚═══════════════════════════════════════════════════════════════════════════════════╝ }

type
  TJSMutationObserverInit = class external name 'Object'(TJSObject)
  private
    FAttributes: boolean; external name 'attributes';
    FAttributeOldValue: boolean; external name 'attributeOldValue';
    FCharacterData: boolean; external name 'characterData';
    FCharacterDataOldValue: boolean; external name 'characterDataOldValue';
    FChildList: boolean; external name 'childList';
    FSubTree: boolean; external name 'subtree';
    FAttributeFilter: TJSArray; external name 'attributeFilter';
  public
    constructor new; reintroduce;
    { public properties }
    property attributes: boolean read FAttributes write FAttributes;
    property attributeOldValue: boolean read FAttributeOldValue write FAttributeOldValue;
    property characterData: boolean read FCharacterData write FCharacterData;
    property characterDataOldValue: boolean read FCharacterDataOldValue write FCharacterDataOldValue;
    property childList: boolean read FChildList write FChildList;
    property subTree: boolean read FSubTree write FSubTree;
    property attributeFilter: TJSArray read FAttributeFilter write FAttributeFilter;
 end;

type
  TJSMutationObserver = class external name 'MutationObserver'
  public
    { constructor }
    constructor new(mutationCallback: TJSMutationCallback); overload;
    constructor new(mutationCallback: TJSSubscribeCallback); overload;
    { public methods }
    procedure observe(target: TJSNode); overload;
    procedure observe(target: TJSNode; options: TJSMutationObserverInit); overload;
    procedure observe(target: TJSNode; options: TJSObject); overload;
    procedure disconnect;
    function takeRecords: TJSMutationRecordArray;
  end;

type
  TJSMutationRecord = class external name 'MutationRecord'
    private
      Ftype: string; external name 'type';
      Ftarget: TJSNode; external name 'target';
      FaddedNodes: TJSNodeList; external name 'addedNodes';
      FremovedNodes: TJSNodeList; external name 'removedNodes';
      FpreviousSibling: TJSNode; external name 'previousSibling';
      FnextSibling: TJSNode; external name 'nextSibling';
      FattributeName: string; external name 'attributeName';
      FattributeNamespace: string; external name 'attributeNamespace';
      FoldValue: string; external name 'oldValue';
    public
      { public properties }
      property &type: string read Ftype;
      property target: TJSNode read Ftarget;
      property addedNodes: TJSNodeList read FaddedNodes;
      property removedNodes: TJSNodeList read FremovedNodes;
      property previousSibling: TJSNode read FpreviousSibling;
      property nextSibling: TJSNode read FnextSibling;
      property attributeName: string read FattributeName;
      property attributeNamespace: string read FattributeNamespace;
      property oldValue: string read FoldValue;
    end;

type
  TJSMutationEvent = class external name 'MutationEvent'(TJSEvent)
  private
    FrelatedNode: TJSNode; external name 'relatedNode';
    FprevValue: string; external name 'prevValue';
    FnewValue: string; external name 'newValue';
    FattrName: string; external name 'attrName';
    FattrChange: Integer; external name 'attrChange';
  public
    { public methods }
    procedure initMutationEvent(&type: string; canBubble,cancelable: Boolean;
      relatedNode: TJSNode; prevValue, newValue, attrName: string;
      attrChange: Integer);
    { public properties }
    property relatedNode: TJSNode read FrelatedNode;
    property prevValue: string read FprevValue;
    property newValue: string read FnewValue;
    property attrName: string read FattrName;
    property attrChange: Integer read FattrChange;
  end;


function beginMethod: JSValue; external name 'function(){//';
function endMethod: JSValue; external name '}//';

implementation

(* BASIC EXAMPLE
function observerConfig: TJSMutationObserverInit;
{ ╔════Config paramater═════════════════════════════════════════════════════╗
  ║ Options for the observer (which mutations to observe)                   ║
  ║ by default all false. However, you can pick as many as you want,        ║
  ║ but at least one of - attributes, characterData, or childList           ║
  ║                                                                         ║
  ║ var optParams = {"childList": true, "attributes": true}                 ║
  ╚═════════════════════════════════════════════════════════════════════════╝}
begin
  result := TJSMutationObserverInit.new;
  result.attributes           := true; { attribute changes will be observed | on add/remove/change attributes }
  result.attributeOldValue    := true; { will show oldValue of attribute | on add/remove/change attributes | default: null }
  result.characterData        := true; { data changes will be observed | on add/remove/change characterData }
  result.characterDataOldValue:= true; { will show OldValue of characterData | on add/remove/change characterData | default: null }
  result.childList            := true; { target childs will be observed | on add/remove }
  result.subtree              := true; { target childs will be observed | on attributes/characterData changes if they observed on target }
  result.attributeFilter      := TJSArray.new('style'); { filter for attributes | array of attributes that should be observed, in this case only style }

end;

procedure Subscribe(mutationList: TJSMutationRecordArray; observer: TJSMutationObserver);
var
  mutation: TJSMutationRecord;

begin
  for mutation in mutationList do
  begin
    if (mutation.&type = 'childList') then
    begin
      console.log('A child node has been added or removed.');
    end
    else if (mutation.&type = 'attributes') then
    begin
      console.log('The ' + mutation.attributeName + ' attribute was modified.');
    end;
  end;
end;

// Select the node that will be observed for mutations
var
  targetNode: TJSElement;
  observer: TJSMutationObserver;
begin
  // Select the node that will be observed for mutations
  targetNode := document.getElementById('OBJA');

  // Create an observer instance linked to the callback function
  observer := TJSMutationObserver.new(@Subscribe);
  // Start observing the target node for configured mutations
  observer.observe(targetNode, observerConfig);

  // Later, you can stop observing
  //observer.disconnect();
end.

*)

end.

