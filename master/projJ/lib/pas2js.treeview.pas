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
unit pas2js.TreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, pas2js.Element, pas2js.ListBox, pas2js.Panel;

type
  TWTreeNode = class
  public
    Node, ParentNode, NodeDescription : String;
    Level : Integer;
    Children : array of TWTreeNode;
    Expanded: Boolean;
    Showing: Boolean;
  end;

  TWTreeView = class(TCustomControl)
  private
    ListBox: TWListBox;
    Title : TWPanel;
    Function  FindNode(ThisNode: string):TWTreeNode;
    Procedure HideAllChildren(node: TWTreeNode);
    Procedure Order(node: TWTreeNode);
    Node : TWTreeNode;
    Root : TWTreeNode;
  public
    constructor Create(parent: TCustomControl); virtual;
    Procedure Add(NewNode, ParentNode, NodeDescription: String);
    Procedure ShowTree;
    Subject: String; // := 'TreeView...';
  end;

implementation



{ TWTreeView }

constructor TWTreeView.Create(parent: TCustomControl);

  procedure doTreeViewOnReadyExecute(sender: TObject);
  begin
    Title.SetBounds(0,0,width-2,20);
    Title.SetinnerHTML(Subject);
    Title.SetProperty('font-size', '0.95em');
    ListBox.Width := self.Width;
    ListBox.Height := self.Height - 20;

    ShowTree;
  end;

begin
  inherited Create('div', parent);
  Subject := 'TreeView...';

  Title := TWPanel.Create(self);
  Title.SetProperty('border','1px solid white');
  Title.SetProperty('background-color','#699BCE');
  Title.SetProperty('color','white');

  ListBox := TWListBox.Create(self);
  ListBox.Top := 20;

  //self.Observe;
  self.OnReadyExecute := @doTreeViewOnReadyExecute;

end;

Procedure TWTreeView.Add(NewNode, ParentNode, NodeDescription: string);
var
  Parent: TWTreeNode;
  temp: TWTreeNode;
begin
//
  Node := TWTreeNode.Create;
  Node.Node := NewNode;
  Node.ParentNode := ParentNode;
  Node.NodeDescription := NodeDescription;
  Node.Expanded := false;
  Node.Showing := false;
  If ParentNode = '' then
  begin
    Root := Node;
    Node.Level := 1;
    Node.Expanded := true;
    Node.Showing := True;
  end;
  Parent := TWTreeNode.Create;
  Parent := FindNode(ParentNode);
  If assigned(Parent) then
  begin
    temp := TWTreeNode.Create;            //  eliminate unwanted double ups
    Temp := FindNode(NewNode);                                                //
    If (not assigned(temp)) or (temp.ParentNode <> parent.node) then begin    //
      TJSArray(Parent.Children).push(Node);
      Node.Level := Parent.Level + 1;
      If node.level = 2 then node.Showing := true;
    end;                                                                      //
  end;

end;

(*
this.FindNode = function (ThisNode) {
  var Result = null;
  var queue = [];
  var node = null;
  var i = 0;
  queue = [this.Root];
  while (queue.length > 0) {
    node = queue[0];
    //queue.slice(0);
    	queue.shift();
    if (node.Node === ThisNode) Result = node;
    for (var $l1 = 0, $end2 = node.Children.length - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      queue.push(node.Children[i]);
    };
  };
  return Result;
};
*)

Function TWTreeView.FindNode(ThisNode:string):TWTreeNode;
var
  queue: TJSArray; //Array of TWTreeNode;
  node: TWTreeNode;
  i: Integer;

begin
  { TODO warleyalex }
//  queue := [Root];

  queue := TJSArray.new(Root);

  while (TJSArray(queue).length > 0) do
  begin
    node := TWTreeNode(queue.Elements[0]);
    TJSArray(queue).shift;
    if node.Node = ThisNode then
      result := node;
    for i := 0 to TJSArray(node.Children).length - 1 do
    begin
      TJSArray(queue).push(node.Children[i]);
    end;
  end;
end;

Procedure TWTreeView.HideAllChildren(node: TWTreeNode);
var
  i: integer;
begin

  Node.Showing := false;
  Node.Expanded := false;

  for i := 0 to TJSArray(node.Children).length -1 do
  begin
    HideAllChildren(node.Children[i]);
  end;
end;

Procedure TWTreeView.ShowTree;
begin
  ListBox.Clear;
  Order(Root);
end;

Procedure TWTreeView.Order(node: TWTreeNode);
var
  Item: TWPanel;
  prefix, s: string;
  i, j: integer;

  procedure doItemOnClick(Sender:TObject);
  begin
    Title.SetInnerHTML( (Sender as TCustomControl).tag );
    Subject := (Sender as TCustomControl).tag;

    node.expanded := not node.expanded;
    For j := 0 to TJSArray(Node.Children).length -1 do
      Node.Children[j].Showing := Node.Expanded;
    If Node.Expanded = false then
    begin
      HideAllChildren(Node);
      Node.Showing := true;
    end;
    ShowTree;
  end;

  function doOnDblClick(event: TJSMouseEvent): boolean;
    procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  begin
    postMessage([self.Handle.id,'dblclick',subject],'*');
  end;

begin

  if Node.Showing then begin
    Item := TWPanel.Create(Self);
    Item.setProperty('background-color', 'whitesmoke');
    Item.SetProperty('font-size', '0.85em');
    Item.Height := 21;

    prefix := '';
    If TJSArray(node.children).Length > 0
      then prefix := '&#9656;&nbsp;'               //triangle right
      else prefix := '&nbsp;&nbsp;&#9643;&nbsp;';  //white square
    If TJSArray(node.children).length > 0 then
      If node.Children[0].Showing then
        prefix := '&#9662;&nbsp;';                 //triangle down

    s := '';
    For i := 1 to node.Level do begin
      S := S + '&nbsp;&nbsp;';
    end;
    S := S + prefix;

    Item.SetinnerHTML(S + node.NodeDescription);
    Item.tag := node.NodeDescription;


    ListBox.Add(Item);

    Item.OnClick := @doItemOnClick;

    TJSHTMLElement(Item.Handle).ondblclick := @doOnDblClick;

  end;

  for i := 0 to TJSArray(node.Children).length -1 do
  begin
    Order(node.Children[i]);
  end;
end;

end.

