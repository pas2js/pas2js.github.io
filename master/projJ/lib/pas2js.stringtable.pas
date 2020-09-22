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
unit pas2js.StringTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, pas2js.Element, pas2js.Panel;

type
  TWStrTable = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure AddColumn(title: string; colwidth: integer);
    procedure AddCell(row, column: integer; cell: String);
    ColumnCount : integer;
    RowCount: integer;
    ColumnWidths : array of integer;
    TableRow : String;
    TitleRow : String;
  end;

implementation



{ TWTable }

constructor TWStrTable.Create(parent: TCustomControl);
begin
  inherited Create('table', parent);
  ColumnCount := 0;
  RowCount := 0;
  setProperty('transform','none');
end;

procedure TWStrTable.AddColumn(title: string; colwidth: integer);
begin
//
//add columnwidth to array
  TJSArray(ColumnWidths).push(colwidth);

  Inc(ColumnCount);
//when inserting the first column, create a tr row element
  If ColumnCount = 1 then
    TitleRow := '<tr>';

//delete tr closure if it exists
  TitleRow := StrBefore(TitleRow, '</tr>');

  TitleRow := TitleRow + '<th style="width:' + inttostr(colwidth)
                       + 'px;text-align:left;border:1px solid lightgrey">' + title + '</th></tr>';

  self.SetInnerHtml(TitleRow);

end;

procedure TWStrTable.AddCell(row, column: integer; cell: String);
begin
//
//when inserting the first cell in a row, create a tr row element
  If Column = 1 then begin
    TableRow := '<tr>';
    Inc(RowCount);
  end;

  If odd(RowCount)
    then TableRow := TableRow + '<td style="background-color:whitesmoke">' + cell + '</td>'
    else TableRow := TableRow + '<td style="background-color:white">' + cell + '</td>';

//when inserting the last cell in a row,
  If Column = ColumnCount then
  begin
    TableRow := TableRow + '</tr>';
    self.SetInnerHtml(self.GetinnerHTML + TableRow);
  end;

end;

end.

