program project1;

{$mode objfpc}
{$modeswitch externalclass}

uses
   JS, Web, uGoogleMaps;

(*
function loadMap(event: TJSEvent): boolean;
var
  mapOptions: TGoogleMapOptions;
  latlng: TGoogleLatLng;
  map: TGoogleMap;
  beachMarker: TGoogleMarker;
  markerOptions: TGoogleMarkerOptions;
begin
  console.log('Seven Lakes Map');
  latlng:= TGoogleLatLng.New(-19.4574, -44.2417);

  mapOptions := TGoogleMapOptions.new;
  mapOptions.zoom:= 16;
  mapOptions.center := latlng;
  mapOptions.mapTypeId := TGoogleMapTypeID.ROADMAP;

  map := TGoogleMap.New(document.querySelector('#sample'), mapOptions);

  markerOptions:= TGoogleMarkerOptions.New;
  markerOptions.position := TGoogleLatLng.New(-19.4574, -44.2417);

  beachMarker:= TGoogleMarker.New(markerOptions);
  beachMarker.setMap(map);
  beachMarker.setIcon('beachflag.png');
end;*)

//* approach i
//  assuming we have already this tag in the main html
//  <script src = "https://maps.googleapis.com/maps/api/js"></script>
// this approach does not work with pas2js, just because pas2js is started on the 'load' event;

(*
procedure initializeGM;
var
  FHandler: TJSGoogleMapsEventHandler;
begin
  FHandler := TGoogleEvents.addDomListener(window, 'load', @loadMap );
//  FHandler := TGoogleEvents.addDomListener(window, 'DOMContentLoaded', @loadMap );
end;
*)


function CreateObjecto(obj: String): JSValue; assembler;
asm
 return eval("(" + obj + ")");
end;

function CreateObj: JSValue; assembler;
asm
 return {};
end;


var  CreateObject: TJSObject; external name '{}';
(*
type
  TGM = class
  private
    DisableDefaultUI: Boolean;
    DisableDblClickZoom: Boolean;
    Draggable: Boolean;
    Heading: Double;
    mapTypeId: String;
    center: TGoogleLatLng;
  protected
    procedure SetMapOptions(Value: TGoogleMapOptions);
    function GetMapOptions: TGoogleMapOptions;
  public
    property MapOptions: TGoogleMapOptions read GetMapOptions write SetMapOptions;
  end;

procedure TGM.SetMapOptions(Value: TGoogleMapOptions);
begin
   Value.disableDefaultUI:= DisableDefaultUI;
   Value.disableDoubleClickZoom:= DisableDblClickZoom;
   Value.draggable:= Draggable;
   Value.heading:= Heading;
   Value.mapTypeId:= mapTypeId;
   Value.center := center;
end;

function TGM.GetMapOptions: TGoogleMapOptions;
begin
   //Result:=TGoogleMapOptions(CreateObject('{}'));
   Result:=TGoogleMapOptions(CreateObjecto);
   SetMapOptions(Result);
end;
*)

type
TOption = class
private
  DisableDefaultUI: Boolean;
  DisableDblClickZoom: Boolean;
  Draggable: Boolean;
  Heading: Double;
  mapTypeId: String;
  center: TGoogleLatLng;
protected
//  procedure SetMapOptions(Value: TGoogleMapOptions);
//  function GetMapOptions: TGoogleMapOptions;
public
//  property MapOptions: TGoogleMapOptions read GetMapOptions write SetMapOptions;
end;

{ TMap }

TMap = class
  private
    FOptions: TOption;
    FGoogleMap: TGoogleMap;
    DisableDefaultUI: Boolean;
    DisableDblClickZoom: Boolean;
    Draggable: Boolean;
    Heading: Double;
    mapTypeId: String;
    center: TGoogleLatLng;
  protected
    procedure SetMapOptions(Value: TGoogleMapOptions);
    function GetMapOptions: TGoogleMapOptions;
  public
    constructor Create;
    property GoogleMap: TGoogleMap read FGoogleMap write FGoogleMap;
    property MapOptions: TGoogleMapOptions read GetMapOptions write SetMapOptions;
end;

{ TMap }

constructor TMap.Create;
begin
  inherited Create;
end;

procedure TMap.SetMapOptions(Value: TGoogleMapOptions);
begin
   Value.disableDefaultUI:= DisableDefaultUI;
   Value.disableDoubleClickZoom:= DisableDblClickZoom;
   Value.draggable:= Draggable;
   Value.heading:= Heading;
   Value.mapTypeId:= mapTypeId;
   Value.center := center;
end;

function TMap.GetMapOptions: TGoogleMapOptions;
begin
   //Result:=TGoogleMapOptions(CreateObject('{}'));
   Result:=TGoogleMapOptions(CreateObject);
   SetMapOptions(Result);
end;

function loadMap(event: TJSEvent): boolean;
(*
var
  gm: TMap;
  beachMarker: TGoogleMarker;
  markerOptions: TGoogleMarkerOptions;
begin
  gm := TMap.Create;
  //gm.Options.disableDefaultUI:=TRUE;
  //gm.Options.disableDoubleClickZoom:=true;
  //gm.Options.draggable:=true;
  //gm.Options.heading:= 123;
  gm.MapOptions.zoom:= 16;
  gm.MapOptions.mapTypeId := TGoogleMapTypeID.ROADMAP;
  gm.MapOptions.center := TGoogleLatLng.New(-19.4574, -44.2417);
  gm.GoogleMap := TGoogleMap.New(document.querySelector('#sample'), gm.MapOptions);

  markerOptions:= TGoogleMarkerOptions.New;
   markerOptions.position := TGoogleLatLng.New(-19.4574, -44.2417);

   beachMarker:= TGoogleMarker.New(markerOptions);
   beachMarker.setMap(gm.GoogleMap);
   beachMarker.setIcon('beachflag.png');
*)


var
  mapOptions: TGoogleMapOptions;

  map: TGoogleMap;
  beachMarker: TGoogleMarker;
  markerOptions: TGoogleMarkerOptions;
begin
  console.log('Seven Lakes Map');

  mapOptions := TGoogleMapOptions(CreateObject);
  mapOptions.zoom:= 16;
  mapOptions.center := TGoogleLatLng.New(-19.4574, -44.2417);
  mapOptions.mapTypeId := TGoogleMapTypeID.ROADMAP;

  map := TGoogleMap.New(document.querySelector('#sample'), mapOptions);

  //markerOptions:= TGoogleMarkerOptions.New;
  markerOptions:= TGoogleMarkerOptions(CreateObject);
  markerOptions.position := TGoogleLatLng.New(-19.4574, -44.2417);

  beachMarker:= TGoogleMarker.New(markerOptions);
  beachMarker.setMap(map);
  beachMarker.setIcon('beachflag.png');

end;

//  inject the Google maps script
procedure InjectJS;
var
  id: string;
  script: TJSHTMLScriptElement;
begin
  script := TJSHTMLScriptElement(document.createElement('script'));
  script.src := 'https://maps.googleapis.com/maps/api/js?v=3&signed_in=true';
  script.type_ := 'text/javascript';
  script.addEventListener('load', @loadMap);
  document.body.appendChild(script);
end;

//var
 // r: JSValue;
 // GoogleMapTypeControlOptions: TGoogleMapTypeControlOptions;
 // Value: TGoogleMapOptions;
//  GM: TGM;

begin
  // Your code here
  InjectJS;


  (*
  GM := TGM.Create;
  GM.MapOptions.disableDefaultUI:=TRUE;
  GM.MapOptions.disableDoubleClickZoom:=true;
  GM.MapOptions.draggable:=true;
  GM.MapOptions.heading:= 123;
  GM.mapTypeId := TGoogleMapTypeID.ROADMAP;
  GM.center := TGoogleLatLng.New(-19.4574, -44.2417);
  *)



  // initialize Google Maps
  //InjectJS;

//  r:=   CreateObjecto;
//    console.log(r);


//r:=   CreateObj;
//  console.log(r);

//r:= CreateObject('{hello: "world", key: "value"}');
//console.log(r);

// Value := TGoogleMapOptions(CreateObject('{}'));
//  Value.mapTypeControlOptions:= TGoogleMapTypeControlOptions(CreateObject('{}'));
//  Value.mapTypeControlOptions:= TGoogleMapTypeControlOptions(CreateObject('{}'));
//  console.log(Value);


//Value := TGoogleMapOptions(CreateObjecto);
//Value.mapTypeControlOptions := TGoogleMapTypeControlOptions(CreateObjecto);
//TGoogleMapOptions(Value).mapTypeControlOptions.position:='1';

//console.log(Value);

//GoogleMapTypeControlOptions: TGoogleMapTypeControlOptions;

//console.log(GetMapOptions)
end.

