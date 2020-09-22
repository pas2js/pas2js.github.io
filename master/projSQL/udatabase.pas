unit uDatabase;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Types, JS, Web, DB, JSONDataSet, USQLite;

type
  TProcedureSQLDB = reference to procedure(localDB: TJSSqliteDatabase); // of object;

type
  TConnectErrorEvent = procedure(Sender: TObject; fDatabase: String) of object;

  { TDatabase }
  TDatabase = class(TComponent)
  private
  { private declarations }
    FResponse: TJSResponse;
    data: TJSPromise;
    FDS: TDataSet;
    FLeft: Integer;
    FTop: Integer;
    FConnected: Boolean;
    FDataNode: String;
    FDatabaseName: String;
    FAfterConnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FOnConnectError: TConnectErrorEvent;
  protected
  { protected declarations }
    procedure SetConnected(AValue: boolean);
  public
  { public declarations }
    DB: TJSSqliteDatabase;
    procedure RegisterDataSet(value: TDataSet);
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoError(fDatabase: String{ErrorCode: Integer});
    constructor Create(AOwner: TComponent); override;
  published
  { published declarations }
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Connected: Boolean read FConnected write SetConnected;
    property DataNode: String read FDataNode write FDataNode;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property OnConnectError: TConnectErrorEvent read FOnConnectError write FOnConnectError;
  end;


  function Sleep(ms: NativeInt): TJSPromise;
  procedure ShowQuery(contents: TJSSqliteQueryResults);

implementation

function Sleep(ms: NativeInt): TJSPromise;
begin
  Result := TJSPromise.New(
    procedure(resolve,reject : TJSPromiseResolver)
    begin
      window.setTimeout(
      procedure()
      begin
        console.log('Done waiting');
        resolve(ms);
      end, ms);
    end);
end;

//------------------------------------------------------------------------------
// Create an HTML table
function tableCreate(columns, values: TJSValueDynArray): JSValue;
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
   rows: JSValue;
begin
  tbl  := document.createElement('table');
  tbl.setAttribute('class', 'tableSection');

  html := '<thead>' + UpperCase(valconcat(columns, 'th class="text"')) + '</thead>';
  rows := TJSArray(values).map(function(element : JSValue; index: NativeInt; anArray : TJSArray): JSValue
  begin
   Result := valconcat((element), 'td');
  end);

  html += '<tbody>' + valconcat(rows, 'tr') + '</tbody>';
  tbl.innerHTML := html;
  Result :=  tbl;
end;

{  Show the query output }
procedure ShowQuery(contents: TJSSqliteQueryResults);
var
  i: integer;
  outputElm: TJSElement;
begin
   outputElm := document.getElementById('output');
   outputElm.innerHTML := '';
   for i:=0 to TJSArray(contents).length-1 do
   begin
     outputElm.appendChild(
        TJSNode(TableCreate(contents[i].columns, contents[i].values))
     );
  end;
end;

{  Open Async local Sqlite Database file }
procedure OpenSQLiteDBFile(path: String; callBackDB: TProcedureSQLDB); async;
var
  fetched: TJSResponse;
  buf: TJSPromise; //TJSArrayBuffer;
  dataDB: TJSUint8Array;
begin
  fetched := await(window.fetch( path ));
  buf := await (fetched.arrayBuffer());
  dataDB:= TJSUint8Array.New( TJSArrayBuffer(buf) );
  //await (Sleep(25));
  callBackDB( TJSSqliteDatabase.New( dataDB )); //new SQL.Database(new Uint8Array(buf));
end;

{ TDatabase }
procedure TDatabase.SetConnected(AValue: boolean);
var
  J: JSValue;
 JA: JSValue;

begin
  if ((FDatabaseName <> '') and AValue) then
  begin
    DoBeforeConnect();
    FConnected := AValue;

    OpenSQLiteDBFile(FDatabaseName, procedure(localDB: TJSSqliteDatabase)
    begin
      { global database }
      DB:= localDB;
      DoAfterConnect();
    end);
  end;
end;

procedure TDatabase.RegisterDataSet(value: TDataSet);
begin
  FDS := value;
end;

procedure TDatabase.DoBeforeConnect;
begin
  if (assigned(FBeforeConnect)) then
    FBeforeConnect(Self);
end;

procedure TDatabase.DoAfterConnect;
begin
  if (assigned(FAfterConnect)) then
    FAfterConnect(Self)
   else
      DoError(FDatabaseName);
end;

procedure TDatabase.DoError(fDatabase: String {ErrorCode: Integer});
begin
  if (assigned(FOnConnectError)) then
  FOnConnectError(Self, fDatabase);
end;

constructor TDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDS := nil;
end;

end.


