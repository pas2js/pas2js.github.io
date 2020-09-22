unit uMain;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  JS, Web, Types, Math, Classes, SysUtils,
  uDatabase, uSQLite;

type
  JEventListenerHandler = procedure(event: TJSEvent) of Object;

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

type
  { TJSMain }
  TJSMain = class(TObject)
  private
    (* Private declarations *)
    MasterDetailDatabase: TDatabase;
    procedure AfterConn(Sender: TObject);
  end;

type
  TApplication  = class(TJSMain)
  private
    procedure onBtn1Click(event: TJSEvent);
    procedure onBtn2Click(event: TJSEvent);
    function sqliteResultSet: TJSQueryResults;
    function configSQLDB: TJSObject;
  protected
    procedure bindEvent(element: TJSElement; EventType: String;
        handler: JEventListenerHandler);
    procedure ShowMain(contents: TJSSqliteQueryResults);
    procedure AfterConn(Sender: TObject);
    procedure OnConnectError(Sender: TObject; fDatabase: String);
  public
    procedure RunApp; virtual;
  end;


implementation

var
   body: TJSElement; external name 'document.body';
  _BTN1: TJSElement; external name 'document.querySelector("#btn1")';
  _BTN2: TJSElement; external name 'document.querySelector("#btn2")';

{ TJSMain }
procedure TJSMain.AfterConn(Sender: TObject);
begin
  console.log('AfterConn was called');
  console.log(MasterDetailDatabase.DB);
end;

{ TApplication }
procedure TApplication.bindEvent(element: TJSElement; EventType: String;
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

{  Show the main screen output }
procedure TApplication.ShowMain(contents: TJSSqliteQueryResults);
// Create an HTML table
function CreateTable(columns, values: TJSValueDynArray): JSValue;
  function valconcat(vals: JSValue; tagName: String): String;
  var
     open, close: String;
  begin
    if (TJSArray(vals).length = 0) then Result := '';
    open  := '<'+tagName+'>';
    close := '</'+tagName+'>';
    Result := open + TJSArray(vals).join(close + open) + close;
  end;

var
   tbl: TJSElement;
   html: String;
   tableHtml : String;
   rows: JSValue;
begin
  tbl  := document.createElement('table');
  tbl.setAttribute('id', 'tableId');
  tbl.setAttribute('class', 'table table-striped table-bordered table-hover');

  html := '<thead class="thead-default">' + UpperCase(valconcat(columns, 'th class="text"')) + '</thead>';
  rows := TJSArray(values).map(function(element : JSValue; index: NativeInt; anArray : TJSArray): JSValue
  begin
     Result := valconcat((element), 'td');
  end);

  html += '<tbody>' + valconcat(rows, 'tr class="success"')+'</tbody>';
  tbl.innerHTML := html;
  Result :=  tbl;
end;

var
  i: integer;
  outputElm: TJSElement;
  table: TJSElement;
  rows: TJSHTMLCollection;
  row: TJSNode;

begin
   outputElm := document.getElementById('view.home.list');
   outputElm.innerHTML := '';
   for i:=0 to TJSArray(contents).length-1 do
   begin
     outputElm.appendChild(
        TJSNode(CreateTable(contents[i].columns, contents[i].values))
     );
  end;

   table := document.getElementById('tableId');
   rows := table.getElementsByTagName('tr');
   for i:=1 to rows.length-1 do
   begin
     row := TJSHTMLTableElement(table).rows[i];
     TJSElement(row).setAttribute('id', row.firstChild.innerText);
     //row.addEventListener('click', function(event: TEventListenerEvent): boolean begin end);
     //row.addEventListener('click', procedure(event: TJSEvent) begin end);

     { onRowTableCellClick event}
     row.addEventListener('click', procedure(event: {JSValue}TJSEvent)   // ---> probably a bug!
     var
       contents: TJSSqliteQueryResults;
       sqlStatement: String;
     begin
       sqlStatement:=
         ' SELECT * FROM '+ TJSElement(TJSElement(event.targetElement).parentNode).id;

       contents := MasterDetailDatabase.DB.exec( sqlStatement );
       ShowQuery( contents );
     end);
   end;
end;


procedure TApplication.onBtn1Click(event: TJSEvent);
var
  selectRecordStatement: String;
  contents: TJSSqliteQueryResults;
begin
  console.log('btn8');
  // Prepare an SQL statement
  selectRecordStatement:=
    ' SELECT "artist" AS Table_Name, COUNT(*) AS Record_Count FROM artist'+
    ' UNION '+
    ' SELECT "track" AS Table_Name, COUNT(*) AS Record_Count FROM track'+
    ' UNION '+
    ' SELECT "genre" AS Table_Name, COUNT(*) AS Record_Count FROM genre'+
    ' UNION '+
    ' SELECT "album" AS Table_Name, COUNT(*) AS Record_Count FROM album';

    contents := MasterDetailDatabase.DB.exec( selectRecordStatement );
    console.log(contents);
    ShowMain( contents );
end;

procedure TApplication.onBtn2Click(event: TJSEvent);
var
  outputElm: TJSElement;
  dataTable: TJSSimpleDatatables;

begin
  console.log('btn2');
  outputElm := document.querySelector('#myTable');
  dataTable := TJSSimpleDatatables.New(outputElm, configSQLDB);
end;

function TApplication.sqliteResultSet: TJSQueryResults;
var
  contents: TJSSqliteQueryResults;
  sqlStatement: String;
begin
  sqlStatement:=
    ' SELECT * FROM Album';

  contents := MasterDetailDatabase.DB.exec( sqlStatement );
  console.log(contents);
  result := contents[0];
end;

function TApplication.configSQLDB: TJSObject;
begin
  Result:= CreateObject;{}
  Result['data']      := sqliteResultSet;
  Result['filters']   := new(['job', TJSArray.new('Assistant','Manager') ]);
  Result['columns']   := new(['select', 4, 'type', 'date', 'format', 'MM/DD/YYYY']);
end;

procedure TApplication.AfterConn(Sender: TObject);
begin
  console.log( 'AfterConn' );
  console.log( MasterDetailDatabase.DB );
end;

procedure TApplication.OnConnectError(Sender: TObject; fDatabase: String);
begin
  console.log('The "' + fDatabase + '" database was not found!');
end;


procedure TApplication.RunApp;
begin
  WriteLn('starting app');
  bindEvent(_BTN1, 'click', @onBtn1Click);
  bindEvent(_BTN2, 'click', @onBtn2Click);

  // Open Chinook_Sqlite database
  MasterDetailDatabase:=  TDatabase.Create;
  // Check for existence of the Chinook_Sqlite database.
  MasterDetailDatabase.Connected:= False;
  Try
     MasterDetailDatabase.DatabaseName:= 'Chinook_Sqlite.sqlite';
     MasterDetailDatabase.AfterConnect:= @AfterConn;
     MasterDetailDatabase.OnConnectError:= @OnConnectError;
     MasterDetailDatabase.Connected:= True;
  Except
    On E: Exception do
    begin
      MasterDetailDatabase.Connected:= False;
    end;
  end;

end;

end.

