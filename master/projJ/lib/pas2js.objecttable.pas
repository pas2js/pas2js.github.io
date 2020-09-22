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
unit pas2js.ObjectTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.Panel;

type
  TWObjTable = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure AddColumn(title: string; colwidth: integer);
    procedure AddCell(row, column: integer; cell: TCustomControl);
    ColumnCount : integer;
    ColumnWidths : array of integer;
    ItemTop: integer;
    ItemHeight: Integer; //= 20;
    tr : TCustomControl;
  end;

implementation



{ TWTable }

constructor TWObjTable.Create(parent: TCustomControl);
begin
  inherited Create('table', parent);
  ItemHeight := 20;

  tr := TCustomControl.Create('tr',self);
  ColumnCount := 0;
  setProperty('cursor','pointer');
  setProperty('overflow-x','hidden');
end;

procedure TWObjTable.AddColumn(title: string; colwidth: integer);
var
  th: TCustomControl;
  i, CurLength: Integer;

begin
//
//add columnwidth to array
  TJSArray(ColumnWidths).push(colwidth);

//create column title
  If ColumnCount = 0 then
  begin
    tr.width  := self.width;
    tr.height := 34;
//  same as  tr.instance.style.height := '34px';
  end;

  th := TCustomControl.Create('th',tr);
  th.SetInnerHtml(title);
  th.width  := colwidth-2;
  th.height := 22;
  th.setProperty('border','1px solid grey');
  th.setProperty('background-color','lightgrey');

//compute offset of column title
  CurLength := 2;
  For i := 0 to ColumnCount-1 do
  begin
    CurLength := CurLength + ColumnWidths[i] + 6;
  end;
  th.left := CurLength;

  Inc(ColumnCount);
end;

procedure TWObjTable.AddCell(row, column: integer; cell: TCustomControl);
var
  td: TCustomControl;
  i, j, CurLength: Integer;
  temp1, temp2: TJSHTMLCollection;

begin
//
//when inserting the first cell in a row, create a tr row element
  If Column = 1 then
  begin
    tr.destroy;
    tr := TCustomControl.Create('tr',self);
    tr.width := self.width;
    tr.height := self.height;
    ItemTop := ItemTop + ItemHeight + 10;
    tr.top := ItemTop;
    ItemHeight := Cell.Height;
  end;

//keep track of largest height of any cell in a row
  If Cell.Height > ItemHeight then ItemHeight := Cell.Height;

//now create the cell contents in td elements
  td := TCustomControl.Create('td',tr);
  td.width  := ColumnWidths[column-1]+20;
  td.height := Itemheight;

//compute offset for the cell
  CurLength := 2;
  For i := 1 to Column-1 do
  begin
    CurLength := CurLength + ColumnWidths[i-1] + 6;
  end;
  td.left := CurLength;

  Cell.Width := ColumnWidths[column-1];
  Cell.Height := ItemHeight;
  cell.setproperty('border','1px solid lightgrey');
  td.Handle.appendchild(Cell.Handle);

//set the height of the row to itemheight
  tr.height := ItemHeight+20;
//td's
  temp1 := TJSHTMLElement(tr.Handle).children;
  for i := 0 to temp1.length -1 do begin
    TJSHTMLElement(temp1[i]).style.setProperty('height', inttostr(Itemheight)+'px');
//div's
    temp2 := TJSHTMLElement(temp1[i]).children;
    for j := 0 to temp2.length -1 do
    begin
      TJSHTMLElement(temp2[j]).style.setProperty('height', inttostr(Itemheight)+'px');
    end;
  end;

  td.height := Itemheight;

end;

end.

