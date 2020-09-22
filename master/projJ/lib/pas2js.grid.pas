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
unit pas2js.Grid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.ListBox, pas2js.Panel;

type
  TWGrid = class(TCustomControl)
  private
    ListBox: TWListBox;
    Item: TWPanel;
    ItemHeight: integer;
    ColumnCount : integer;
    ColumnWidths : array of integer;
    Columns : array of TWPanel;
    Procedure HandleColumnResize(columnTitle: TWPanel);
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure AddColumn(title: string; colwidth: integer);
    procedure AddCell(row, column: integer; cell: TCustomControl);
    CanResize : boolean;
  end;

implementation



{ TWGrid }

constructor TWGrid.Create(parent: TCustomControl);

  procedure gridOnReadyExecute(sender: TObject);
  begin
    //set ListBox position relative to Grid dimensions
    ListBox.SetBounds(0,28,self.width-2,self.height-28);
  end;

begin
  inherited Create('div', parent);

  ListBox := TWListBox.Create(self);
  Item := TWPanel.Create(ListBox);
  ColumnCount := 0;

  //self.Observe;
  self.OnReadyExecute := @gridOnReadyExecute;
end;

procedure TWGrid.AddColumn(title: string; colwidth: integer);
var
  columnTitle: TWPanel;
  CurLength: integer;
  i: Integer;

begin
//add columnwidth to array
  TJSArray(ColumnWidths).push(colwidth);

//create column title
  columnTitle := TWPanel.Create(self);
  columnTitle.SetinnerHTML(title);
  columnTitle.SetBounds(0,0,colwidth,24);
  columnTitle.SetProperty('border','1px solid grey');
  columnTitle.SetProperty('background-color','lightgrey');

//compute offset of column title
  CurLength := 2;
  For i := 0 to ColumnCount-1 do begin
    CurLength := CurLength + ColumnWidths[i] + 6;
  end;
  columnTitle.Left := CurLength;

//make columns resizeable if required (default = non resizeable)
  if CanResize then
    HandleColumnResize(columnTitle);

//doubled up, ColumnCount is same as ColumnsWidths.Count
  Inc(ColumnCount);
end;

procedure TWGrid.AddCell(row, column: integer; cell: TCustomControl);
var
  CurLength: integer;
  i: Integer;
  c: TJSHTMLCollection;

begin
//
//when inserting the first cell in a row, create the row (a panel) first
  If Column = 1 then
  begin
    Item := TWPanel.Create(ListBox);
    Item.SetProperty('border-bottom','none');
    Item.SetProperty('width',inttostr(self.width-2)+'px');
    Item.SetProperty('height',inttostr(cell.height+6)+'px');
    ItemHeight := Cell.Height;
  end;

//keep track of largest height of any cell in a row
  If Cell.Height > ItemHeight then
    ItemHeight := Cell.Height;

//compute offset for the cell
  CurLength := 0;
  For i := 1 to Column-1 do
  begin
    CurLength := CurLength + ColumnWidths[i-1] + 6;
  end;
  Cell.Left := CurLength;

//set some cell properties and attach cell to the listbox row
  Cell.Top := 0;
  Cell.SetProperty('width',inttostr(ColumnWidths[column-1])+'px');
  Cell.setProperty('border', '1px solid lightgrey');
  Item.Handle.appendchild(Cell.Handle);

//when inserting the last cell in a row,
// - set the height of the row to largest cell height
// - and add row to listbox
  If Column = ColumnCount then
  begin
    c := Item.Handle.children;
    for i := 0 to c.length -1 do begin
      TJSHTMLElement(c[i]).style.setProperty('height', inttostr(Itemheight+6)+'px');
    end;
    Item.SetProperty('height',inttostr(ItemHeight+10)+'px');
    ListBox.Add(Item);
  end;
end;

procedure TWGrid.HandleColumnReSize(columnTitle: TWPanel);
var
  ReSizer: TWPanel;
  saveX: Double;

  procedure DoOnTouchStart(e: TJSEvent);
  begin
    touch2Mouse(e);
  end;

  function DoOnMouseDown(e: TJSMouseEvent): boolean;
    function DoOnMouseMove(e: TJSMouseEvent): boolean;
    begin
      TJSHTMLElement(columnTitle.Handle).style.setProperty('zIndex', '999');   //BringToFront
      columnTitle.Width := columnTitle.Width - Trunc(saveX - e.clientX);
      saveX := e.clientX;
      ReSizer.Left := columnTitle.Width - 4;
    end;

  begin
    saveX := e.clientX;
    TJSHTMLElement(self.Handle).onmousemove := @DoOnMouseMove;
  end;

  function DoOnMouseUp(e: TJSMouseEvent): boolean;
  var
    i, j, k, diff: Integer;
    c, d: TJSHTMLCollection;

    function DoOnMouseMove(e: TJSMouseEvent): boolean;
    begin
    // nullify mousemove
    end;

  begin
  ColumnWidths[ColumnCount] := columnTitle.Width;
  //get all rows
  c := ListBox.Handle.children;
  for i := 0 to c.length -1 do
  begin
    //get all cells
    d := TJSElement(c[i]).children;
    for j := 0 to d.length -1 do
    begin
      //set new cell widths for the resized column
      if j = StrToInt(ReSizer.Tag) then
      begin
        TJSHTMLElement(d[j]).style.setProperty('width', inttostr(ColumnWidths[ColumnCount]) + 'px');
        diff := ColumnWidths[j] - columnTitle.Width;
        //shift all columns on the right hand side
        for k := j+1 to d.length -1 do
        begin
           { TODO warleyalex }
          //d[k].style.left := IntToStr(StrToInt(StrBefore(d[k].style.left, 'px')) - diff) + 'px';
          //Columns[k].Left := StrToInt(StrBefore(d[k].style.left, 'px'))+2;
        end;
      end;
    end;
  end;
  ColumnWidths[StrToInt(ReSizer.Tag)] := columnTitle.Width;
  TJSHTMLElement(columnTitle.Handle).style.setProperty('zIndex', '0');

  TJSHTMLElement(self.Handle).onmousemove := @DoOnMouseMove;

  end;

begin
//set column titles & listbox to non-selectable
//as it interferes somewhat visually while dragging
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + columnTitle.Handle.id + ' { user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + columnTitle.Handle.id + ' { -webkit-user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + columnTitle.Handle.id + ' { -moz-user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + columnTitle.Handle.id + ' { -ms-user-select:none}', 0);

 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + ListBox.Handle.id + ' { user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + ListBox.Handle.id + ' { -webkit-user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + ListBox.Handle.id + ' { -moz-user-select:none}', 0);
 TJSCSSStyleSheet( document.styleSheets[0] ).insertRule('#' + ListBox.Handle.id + ' { -ms-user-select:none}', 0);

//create resizers
  ReSizer := TWPanel.Create(columnTitle);
  ReSizer.SetProperty('background-color','gold');
  ReSizer.SetBounds(0,1,4,22);
  ReSizer.Left := columnTitle.Width - 4;
  ReSizer.SetProperty('cursor','w-resize');
  ReSizer.tag := IntToStr(ColumnCount);

  //map touchstart to mousedown, touchend to mouseup and touchmove to mousemove
  { TODO warleyalex }
//  TJSHTMLElement(ReSizer.Handle).ontouchstart := @DoOntouchstart;
//  TJSHTMLElement(ReSizer.Handle).ontouchmove  := ReSizer.Handle.ontouchstart;
//  TJSHTMLElement(ReSizer.Handle).ontouchend   := ReSizer.Handle.ontouchstart;

  //adjust width of columnTitle while dragging
  TJSHTMLElement(ReSizer.Handle).onmousedown := @DoOnMouseDown;

  TJSArray(Columns).push(columnTitle);

  //mouseUp = end of dragging
  TJSHTMLElement(columnTitle.Handle).onmouseup := @DoOnMouseUp;

end;

end.

(*
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
unit pas2js.Grid;

interface

uses
  pas2js.Element, pas2js.ListBox, pas2js.Panel;

type
  TWGrid = class(TCustomControl)
  private
    ListBox: TWListBox;
    Item: TWPanel;
    ItemHeight: integer;
    ColumnCount : integer;
    ColumnWidths : array of integer;
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure AddColumn(title: string; colwidth: integer);
    procedure AddCell(row, column: integer; cell: TCustomControl);
  end;

implementation

uses Globals;

{ TWGrid }

constructor TWGrid.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);

  ListBox := TWListBox.Create(self);
  Item := TWPanel.Create(ListBox);
  ColumnCount := 0;

  //self.Observe;
  self.OnReadyExecute := procedure(sender: TObject)
  begin
    //set ListBox position relative to Grid dimensions
    ListBox.SetBounds(0,26,self.width-2,self.height-28);
  end;
end;

procedure TWGrid.AddColumn(title: string; colwidth: integer);
begin

//add columnwidth to array
  ColumnWidths.Add(colwidth);

//create column title
  var columnTitle := TWPanel.Create(self);
  columnTitle.SetinnerHTML(title);
  columnTitle.SetBounds(0,0,colwidth,24);
  columnTitle.SetProperty('border','1px solid grey');
  columnTitle.SetProperty('background-color','lightgrey');

//compute offset of column title
  var CurLength : integer := 2;
  For var i := 0 to ColumnCount-1 do begin
    CurLength := CurLength + ColumnWidths[i] + 6;
  end;
  columnTitle.Left := CurLength;

//doubled up, ColumnCount is same as ColumnsWidths.Count
  Inc(ColumnCount);
end;

procedure TWGrid.AddCell(row, column: integer; cell: TCustomControl);
begin
//
//when inserting the first cell in a row, create the listbox line-item
  If Column = 1 then begin
    Item := TWPanel.Create(ListBox);
    Item.SetProperty('border-bottom','none');
    Item.SetProperty('width',inttostr(self.width-2)+'px');
    Item.SetProperty('height',inttostr(cell.height+6)+'px');
    ItemHeight := Cell.Height;
  end;

//keep track of largest height of any cell in a row
  If Cell.Height > ItemHeight then ItemHeight := Cell.Height;

//compute offset for the cell
  var CurLength : integer := 2;
  For var i := 1 to Column-1 do begin
    CurLength := CurLength + ColumnWidths[i-1] + 6;
  end;
  Cell.Left := CurLength;

//set some cell properties and attach cell to the listbox line-item
  Cell.Top := 2;
  Cell.SetProperty('width',inttostr(ColumnWidths[column-1]-4)+'px');
  Cell.setProperty('border', '1px solid lightgrey');
  Item.Handle.appendchild(Cell.Handle);

//when inserting the last cell in a row,
// - set the height of the listbox line-item to largest cell height
// - and add listbox line-item to listbox
  If Column = ColumnCount then
  begin
    var c := Item.Handle.children; //document.getElementById(Item.Handle.id).children;
    for var i := 0 to c.length -1 do begin
      c[i].style.height  := inttostr(Itemheight+6)+'px';
    end;
    Item.SetProperty('height',inttostr(ItemHeight+10)+'px');
    ListBox.Add(Item);
  end;
end;

end.
*)
