unit uCDS;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, Types, JS, Web, DB, JSONDataSet;

type
  { TWDataSource }
  TWDataSource = class(TDataSource)
  private
  { private declarations }
    FLeft: Integer;
    FTop: Integer;
  protected
  { protected declarations }
  published
  { published declarations }
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
  end;

type
  TConnectErrorEvent = procedure(Sender: TObject; ErrorCode: Integer);
  TDataSet = class;

  { TConnection }
  TConnection = class(TComponent)
  private
  { private declarations }
//    FReq: TJSXMLHttpRequest;
    FResponse: TJSResponse;
    data: TJSPromise;
    FDS: TDataSet;
    FLeft: Integer;
    FTop: Integer;
    FActive: Boolean;
    FDataNode: String;
    FURI: String;
    FAfterConnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FOnConnectError: TConnectErrorEvent;
  protected
  { protected declarations }
    function fetchAsync(url: String): {TJSResponse} TJSPromise; {$ifndef InLazIDE}async;{$endif}
    function fetchJSON(url: String): TJSObject; {$ifndef InLazIDE}async;{$endif}
    procedure SetActive(AValue: boolean);
  public
  { public declarations }
//    function onLoad(Event: TEventListenerEvent): boolean;
    procedure RegisterDataSet(value: TDataSet);
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoError(ErrorCode: Integer);
    constructor Create(AOwner: TComponent); override;
  published
  { published declarations }
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Active: Boolean read FActive write SetActive;
    property DataNode: String read FDataNode write FDataNode;
    property URI: String read FURI write FURI;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property OnConnectError: TConnectErrorEvent read FOnConnectError write FOnConnectError;
  end;

type
  { TDataSet }
  TDataSet = class(TBaseJSONDataSet)
  private
  { private declarations }
    FConnection: TConnection;
    FLeft: Integer;
    FTop: Integer;
  protected
  { protected declarations }
    procedure SetConnection(Value: TConnection);
    Function CreateFieldMapper : TJSONFieldMapper; override;
  public
  { public declarations }
  published
  { published declarations }
    // Can be set directly if the dataset is closed. If metadata is set, it must match the data.
    Property Rows;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Connection: TConnection read FConnection write SetConnection;
  end;

  function Sleep(ms: NativeInt): TJSPromise;

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

{ TConnection }

function TConnection.fetchAsync(url: String): {TJSResponse} TJSPromise; {$ifndef InLazIDE}async;{$endif}
begin
  FResponse := await(window.fetch( url ));
  Result := await(FResponse.json());
end;

function TConnection.fetchJSON(url: String): TJSObject; {$ifndef InLazIDE}async;{$endif}
//var
//  response: TJSResponse;
//  data: TJSPromise;
begin
  //  MyDataset.JSON:=await(TJSobject,fetchasync('myurl'))
  FResponse := await(TJSResponse,fetchasync(url));
end;

procedure TConnection.SetActive(AValue: boolean);
var
  J: JSValue;
 JA: JSValue;

begin
  if ((FURI <> '') and AValue) then
  begin
    DoBeforeConnect();
    FActive := AValue;
    fetchAsync(FURI)
    ._then(function(res: JSValue): JSValue
    begin
      if (FResponse.status = 200) then
      begin
        JA := TJSObject(res).Properties[FDataNode];
//        DoAfterConnect();
        if (assigned(FDS)) then
        begin
          FDS.Rows := TJSArray(JA);
          FDS.Open();
          DoAfterConnect();
        end else
        DoError(FResponse.status);
      end;
    end);

(*
    FReq := TJSXMLHttpRequest.new;
    FReq.addEventListener('load', @onLoad);
    FReq.open('GET',FURI, FALSE {TRUE}); //  if ASYNC = FALSE ==> [Deprecation] Synchronous XMLHttpRequest on the main thread is deprecated because of its detrimental effects to the end user's experience. For more help, check https://xhr.spec.whatwg.org/.
    FReq.send();
*)
  end;
end;

(*
function TConnection.onLoad(Event: TEventListenerEvent): boolean;
var
  J: JSValue;
 JA: JSValue;
begin
  if (FReq.status = 200) then
  begin
    J := TJSJSON.parse(FReq.responseText);
    JA := TJSObject(J).Properties[FDataNode];
    DoAfterConnect();
    if (assigned(FDS)) then
    begin
      FDS.Rows := TJSArray(JA);
      FDS.Open();
    end else
    DoError(FReq.status);
  end;
end;
*)

procedure TConnection.RegisterDataSet(value: TDataSet);
begin
  FDS := value;
end;

procedure TConnection.DoBeforeConnect;
begin
  if (assigned(FBeforeConnect)) then
    FBeforeConnect(Self);
end;

procedure TConnection.DoAfterConnect;
begin
  if (assigned(FAfterConnect)) then
    FAfterConnect(Self);
end;

procedure TConnection.DoError(ErrorCode: Integer);
begin
  if (assigned(FOnConnectError)) then
    FOnConnectError(Self, ErrorCode);
end;

constructor TConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDS := nil;
end;

{ TDataSet }
procedure TDataSet.SetConnection(Value: TConnection);
begin
  if (assigned(Value)) then
    Value.RegisterDataSet(Self);
end;

Function TDataSet.CreateFieldMapper : TJSONFieldMapper;
begin
  Result := TJSONObjectFieldMapper.Create;
end;

end.


