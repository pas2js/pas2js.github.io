unit uGoogleMaps;
{ ╔═════════════════════════════════════════════╗
  ║ Google Maps API                             ║
  ╚═════════════════════════════════════════════╝ }

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, JS, Web;

type
  TJSGoogleMapsEventHandler = reference to function (event: TJSEvent): boolean;

{ forward declarations }
  TJSStyleMedia = class;
  TJSAbstractView = class;

  TJSStyleMedia = class external name 'Object'
  public
    &type: string;
    function matchMedium(mediaquery: string): boolean;
  end;

  TJSAbstractView = class external name 'Object'
  public
    styleMedia: TJSStyleMedia;
    document: TJSDocument;
  end;

  TMaps = class external name 'maps'
  end;

  TGoogle = class external name 'google'
  private
    Fmaps: TMaps; external name 'maps';
  public
    property maps: TMaps read Fmaps;
  end;

  TGoogleMapsEventListener = class external name 'TGoogleMapsEventListener'
  end;

  TGoogleMapsEvent = class external name 'TGoogleMapsEvent'
  end;

  TGoogleEvents = class external name 'google.maps.event'
  public
    class function addDomListener(instance: TJSWindow; const eventName: String;
                                  handler: TJSGoogleMapsEventHandler {TJSEventHandler}; capture: Boolean = false): TJSGoogleMapsEventHandler {TJSEventHandler};
    class function addDomListenerOnce(instance: TJSWindow; const eventName: String;
                                      handler: TJSGoogleMapsEventHandler{TJSEventHandler}; capture: Boolean = false): TJSGoogleMapsEventHandler {TJSEventHandler};
    class function addListener(instance: JSValue; const eventName: String;
                               handler: TJSGoogleMapsEventHandler {TJSEventHandler}): TJSGoogleMapsEventHandler {TJSEventHandler};
    class function addListenerOnce(instance: JSValue; const eventName: String;
                                   handler: TJSGoogleMapsEventHandler {TJSEventHandler}): TJSGoogleMapsEventHandler {TJSEventHandler};
    class procedure clearInstanceListeners(instance: JSValue);
    class procedure clearListeners(instance: JSValue; const eventName:string);
    class procedure removeListener(listener: TJSGoogleMapsEventHandler {TJSEventHandler});
    class procedure trigger(instance: JSValue; const eventName: String; var_args: JSValue = nil);
   end;

type
  TGoogleMVCObject = class external name 'google.maps.MVCObject'
  public
    function addListener(const eventName: String; handler: TJSEventHandler): TJSEventHandler;
    procedure bindTo(const key: String; target: TGoogleMVCObject; targetKey: String=''; noNotify: Boolean=False);
    procedure changed(const key: String);
    function get(const key: String): JSValue;
    procedure notify(const key: String);
    procedure &set(const key: String; value: JSValue);
    procedure setValues(values: JSValue);
    procedure unbind(const key: String);
    procedure unbindAll;
  end;

  TGooglePoint = class external name 'google.maps.Point'
  private
    fx: Double; external name 'x';
    fy: Double; external name 'y';
  public
    constructor New(x,y: Double);
    property x: Double read fx write fx;
    property y: Double read fy write fy;
    function equals(other: TGooglePoint): Boolean;
    function toString: String;
  end;

  TGoogleSize = class external name 'google.maps.Size'
  private
    fheight: Double; external name 'height';
    fwidth: Double; external name 'width';
   public
     constructor New(width,height: Double; widthUnit: string; heightUnit: String='');
     property height: Double read fheight write fheight;
     property width: Double read fwidth write fwidth;
     function equals(other: TGoogleSize): Boolean;
     function toString: String;
   end;

  (*
  TGoogleMapTypeID = class external name 'google.maps.MapTypeId'
  public
    class var HYBRID: String; external name 'google.maps.MapTypeId.HYBRID';
    class var ROADMAP: String; external name 'google.maps.MapTypeId.ROADMAP';
    class var SATELLITE: String; external name 'google.maps.MapTypeId.SATELLITE';
    class var TERRAIN: String; external name 'google.maps.MapTypeId.TERRAIN';
    class var view: TJSAbstractView; external name 'view';
  end;
  *)

  TGoogleMapTypeID = class external name 'google.maps.MapTypeId'
  private
    class var fHYBRID: String; external name 'HYBRID';
    class var fROADMAP: String; external name 'ROADMAP';
    class var fSATELLITE: String; external name 'SATELLITE';
    class var fTERRAIN: String; external name 'TERRAIN';
    class var fview: TJSAbstractView; external name 'view';
   public
    class property HYBRID: String read fHYBRID;
    class property ROADMAP: String read fROADMAP;
    class property SATELLITE: String read fSATELLITE;
    class property TERRAIN: String read fTERRAIN;
    class property view: TJSAbstractView read fview;
  end;

  TGoogleControlPosition = class external name 'google.maps.ControlPosition'
  private
    class var fBOTTOM_CENTER: String; external name 'BOTTOM_CENTER';
    class var fBOTTOM_LEFT: String; external name 'BOTTOM_LEFT';
    class var fBOTTOM_RIGHT: String; external name 'BOTTOM_RIGHT';
    class var fLEFT_BOTTOM: String; external name 'LEFT_BOTTOM';
    class var fLEFT_CENTER: String; external name 'LEFT_CENTER';
    class var fLEFT_TOP: String; external name 'LEFT_TOP';
    class var fRIGHT_BOTTOM: String; external name 'RIGHT_BOTTOM';
    class var fRIGHT_CENTER: String; external name 'RIGHT_CENTER';
    class var fRIGHT_TOP: String; external name 'RIGHT_TOP';
    class var fTOP_CENTER: String; external name 'TOP_CENTER';
    class var fTOP_LEFT: String; external name 'TOP_LEFT';
    class var fTOP_RIGHT: String; external name 'TOP_RIGHT';
   public
     class property BOTTOM_CENTER: String read fBOTTOM_CENTER;
     class property BOTTOM_LEFT: String read fBOTTOM_LEFT;
     class property BOTTOM_RIGHT: String read fBOTTOM_RIGHT;
     class property LEFT_BOTTOM: String read fLEFT_BOTTOM;
     class property LEFT_CENTER: String read fLEFT_CENTER;
     class property LEFT_TOP: String read fLEFT_TOP;
     class property RIGHT_BOTTOM: String read fRIGHT_BOTTOM;
     class property RIGHT_CENTER: String read fRIGHT_CENTER;
     class property RIGHT_TOP: String read fRIGHT_TOP;
     class property TOP_CENTER: String read fTOP_CENTER;
     class property TOP_LEFT: String read fTOP_LEFT;
     class property TOP_RIGHT: String read fTOP_RIGHT;
   end;

  TGoogleMapTypeControlStyle = class external name 'google.maps.MapTypeControlStyle'
  private
    class var fDEFAULT: String; external name 'DEFAULT';
    class var fDROPDOWN_MENU: String; external name 'DROPDOWN_MENU';
    class var fHORIZONTAL_BAR: String; external name 'HORIZONTAL_BAR';
   public
     class property &DEFAULT: String read fDEFAULT;
     class property DROPDOWN_MENU: String read fDROPDOWN_MENU;
     class property HORIZONTAL_BAR: String read fHORIZONTAL_BAR;
   end;

  TGoogleScaleControlStyle = class external name 'google.maps.ScaleControlStyle'
  private
    class var fDEFAULT: String; external name 'DEFAULT';
   public
     class property DEFAULT: String read fDEFAULT;
  end;

  TGoogleZoomControlStyle = class external name 'google.maps.ZoomControlStyle'
  private
    class var fDEFAULT: String; external name 'DEFAULT';
    class var fLARGE: String; external name 'LARGE';
    class var fSMALL: String; external name 'SMALL';
  public
    class property DEFAULT: String read fDEFAULT;
    class property LARGE: String read fLARGE;
    class property SMALL: String read fSMALL;
   end;

  TGoogleMapTypeControlOptions = class external name 'google.maps.MapTypeControlOptions'
  private
    fmapTypeIds: TStringDynArray; external name 'mapTypeIds';
    fposition: String; external name 'position';
    fstyle: String; external name 'style';
   public
    property mapTypeIds: TStringDynArray read fmapTypeIds write fmapTypeIds;
    property position: String read fposition write fposition;
    property style: String read fstyle write fstyle;
   end;

  TGoogleLatLng = class external name 'google.maps.LatLng'
  public
    constructor New(lat,lng: Double; noWrap: Boolean=False);
    function equals(other: TGoogleLatLng): Boolean;
    function lat: Double;
    function lng: Double;
    function toString: String;
    function toUrlValue(precision: Double): String;
  end;

  TGoogleOverviewMapControlOptions = class external name 'google.maps.OverviewMapControlOptions'
  private
    fopened: Boolean; external name 'opened';
  public
    property opened: Boolean read fopened write fopened;
   end;

  TGooglePanControlOptions = class external name 'google.maps.PanControlOptions'
  private
    fposition: String; external name 'position';
  public
    property position: String read fposition write fposition;
   end;

  TGoogleRotateControlOptions = class external name 'google.maps.RotateControlOptions'
  private
    fposition: String; external name 'position';
  public
    property position: String read fposition write fposition;
  end;

  TGoogleScaleControlOptions = class external name 'google.maps.ScaleControlOptions'
  private
    fstyle: String; external name 'style';
  public
    property style: String read fstyle write fstyle;
  end;

  TGoogleStreetViewControlOptions = class external name 'google.maps.StreetViewControlOptions'
  private
    fposition: String; external name 'position';
  public
    property position: String read fposition write fposition;
  end;

  TGoogleZoomControlOptions = class external name 'google.maps.ZoomControlOptions'
  private
    fposition: String; external name 'position';
    fstyle: String; external name 'style';
  public
    property position: String read fposition write fposition;
    property style: String read fstyle write fstyle;
  end;

  TGoogleMapOptions = class external name 'google.maps.MapOptions'
  //TGoogleMapOptions = class external name 'Object' (TJSObject)
  private
    fbackgroundColor: String; external name 'backgroundColor';
    fcenter: TGoogleLatLng; external name 'center';
    fdisableDefaultUI: Boolean; external name 'disableDefaultUI';
    fdisableDoubleClickZoom: Boolean; external name 'disableDoubleClickZoom';
    fdraggable: Boolean; external name 'draggable';
    fdraggableCursor: String; external name 'draggableCursor';
    fdraggingCursor: String; external name 'draggingCursor';
    fheading: Double; external name 'heading';
    fkeyboardShortcuts: Boolean; external name 'keyboardShortcuts';
    fmapMaker: Boolean; external name 'mapMaker';
    fmapTypeControl: Boolean; external name 'mapTypeControl';
    fmapTypeControlOptions: TGoogleMapTypeControlOptions; external name 'mapTypeControlOptions';
    fmapTypeId: String; external name 'mapTypeId';
    fmaxZoom: Integer; external name 'maxZoom';
    fminZoom: Integer; external name 'minZoom';
    fnoClear: Boolean; external name 'noClear';
    foverviewMapControl: Boolean; external name 'overviewMapControl';
    foverviewMapControlOptions: TGoogleOverviewMapControlOptions; external name 'overviewMapControlOptions';
    fpanControl: Boolean; external name 'panControl';
    fpanControlOptions: TGooglePanControlOptions; external name 'panControlOptions';
    frotateControl: Boolean; external name 'rotateControl';
    frotateControlOptions: TGoogleRotateControlOptions; external name 'rotateControlOptions';
    fscaleControl: Boolean; external name 'scaleControl';
    fscaleControlOptions: TGoogleScaleControlOptions; external name 'scaleControlOptions';
    fscrollwheel: Boolean; external name 'scrollwheel';
    fstreetViewControl: Boolean; external name 'streetViewControl';
    fstreetViewControlOptions: TGoogleStreetViewControlOptions; external name 'streetViewControlOptions';
    fstyles: TStringDynArray; external name 'styles';
    ftilt: Integer; external name 'tilt';
    fzoom: Integer; external name 'zoom';
    fzoomControl: Boolean; external name 'zoomControl';
    fzoomControlOptions: TGoogleZoomControlOptions; external name 'zoomControlOptions';
  public
     property backgroundColor: String read fbackgroundColor write fbackgroundColor;
     property center: TGoogleLatLng read fcenter write fcenter;
     property disableDefaultUI: Boolean read fdisableDefaultUI write fdisableDefaultUI;
     property disableDoubleClickZoom: Boolean read fdisableDoubleClickZoom write fdisableDoubleClickZoom;
     property draggable: Boolean read fdraggable write fdraggable;
     property draggableCursor: String read fdraggableCursor write fdraggableCursor;
     property draggingCursor: String read fdraggingCursor write fdraggingCursor;
     property heading: Double read fheading write fheading;
     property keyboardShortcuts: Boolean read fkeyboardShortcuts write fkeyboardShortcuts;
     property mapMaker: Boolean read fmapMaker write fmapMaker;
     property mapTypeControl: Boolean read fmapTypeControl write fmapTypeControl;
     property mapTypeControlOptions: TGoogleMapTypeControlOptions read fmapTypeControlOptions write fmapTypeControlOptions;
     property mapTypeId: String read fmapTypeId write fmapTypeId;
     property maxZoom: Integer read fmaxZoom write fmaxZoom;
     property minZoom: Integer read fminZoom write fminZoom;
     property noClear: Boolean read fnoClear write fnoClear;
     property overviewMapControl: Boolean read foverviewMapControl write foverviewMapControl;
     property overviewMapControlOptions: TGoogleOverviewMapControlOptions read foverviewMapControlOptions write foverviewMapControlOptions;
     property panControl: Boolean read fpanControl write fpanControl;
     property panControlOptions: TGooglePanControlOptions read fpanControlOptions write fpanControlOptions;
     property rotateControl: Boolean read frotateControl write frotateControl;
     property rotateControlOptions: TGoogleRotateControlOptions read frotateControlOptions write frotateControlOptions;
     property scaleControl: Boolean read fscaleControl write fscaleControl;
     property scaleControlOptions: TGoogleScaleControlOptions read fscaleControlOptions write fscaleControlOptions;
     property scrollwheel: Boolean read fscrollwheel write fscrollwheel;
     property streetViewControl: Boolean read fstreetViewControl write fstreetViewControl;
     property streetViewControlOptions: TGoogleStreetViewControlOptions read fstreetViewControlOptions write fstreetViewControlOptions;
     property styles: TStringDynArray read fstyles write fstyles;
     property tilt: Integer read ftilt write ftilt;
     property zoom: Integer read fzoom write fzoom;
     property zoomControl: Boolean read fzoomControl write fzoomControl;
     property zoomControlOptions: TGoogleZoomControlOptions read fzoomControlOptions write fzoomControlOptions;
  end;

  TGoogleLatLngBounds = class external name 'google.maps.LatLngBounds'
  public
    constructor New(sw: TGoogleLatLng; ne: TGoogleLatLng=nil);
    function contains(latLng: TGoogleLatLng): Boolean;
    function equals(other: TGoogleLatLngBounds): Boolean;
    function extend(point: TGoogleLatLng): TGoogleLatLngBounds;
    function getCenter: TGoogleLatLng;
    function getNorthEast: TGoogleLatLng;
    function getSouthWest: TGoogleLatLng;
    function intersects(other: TGoogleLatLngBounds): Boolean;
    function isEmpty: Boolean;
    function toSpan: TGoogleLatLng;
    function toString: String;
    function toUrlValue(precision: Double=0): String;
    function union(other: TGoogleLatLngBounds): TGoogleLatLngBounds;
  end;

  TGoogleProjection = class external name 'google.maps.Projection'
  public
    function fromLatLngToPoint(latLng: TGoogleLatLng; point: TGooglePoint=nil): TGooglePoint;
    function fromPointToLatLng(pixel: TGooglePoint; nowrap: Boolean=False): TGoogleLatLng;
  end;

  TGoogleMapType = class external name 'google.maps.MapType'
  private
    falt: String; external name 'alt';
    fmaxZoom: Integer; external name 'maxZoom';
    fminZoom: Integer; external name 'minZoom';
    fname: String; external name 'name';
    fprojection: TGoogleProjection; external name 'projection';
    fradius: Double; external name 'radius';
    ftileSize: TGoogleSize; external name 'tileSize';
  public
    property alt: String read falt write falt;
    property maxZoom: Integer read fmaxZoom write fmaxZoom;
    property minZoom: Integer read fminZoom write fminZoom;
    property name: String read fname write fname;
    property projection: TGoogleProjection read fprojection write fprojection;
    property radius: Double read fradius write fradius;
    property tileSize: TGoogleSize read ftileSize write ftileSize;

    function getTile(tileCoord: TGooglePoint; zoom: Integer; ownerDocument: TJSDocument): TJSNode;
    procedure releaseTile(tile: TJSNode);
  end;

  TGoogleMapTypeRegistry = class external name 'google.maps.MapTypeRegistry' (TGoogleMVCObject)
  public
    procedure &set(const id: String; mapType: TGoogleMapType=nil);
  end;

  TGoogleMap = class external name 'google.maps.Map' (TGoogleMVCObject)
  private
    fmapTypes: TGoogleMapTypeRegistry; external name 'mapTypes';
  public
    constructor New(mapDiv: TJSNode; opts: TGoogleMapOptions);
    property mapTypes: TGoogleMapTypeRegistry read fmapTypes;
    procedure fitBounds(bounds: TGoogleLatLngBounds);
    function getBounds: TGoogleLatLngBounds;
    function getCenter: TGoogleLatLng;
    function getDiv: TJSNode;
    function getHeading: Double;
    function getMapTypeId: String;
    function getProjection: TGoogleProjection;
    function getTilt: Integer;
    function getZoom: Integer;
    procedure panBy(x,y: Double);
    procedure panTo(latLng: TGoogleLatLng);
    procedure panToBounds(latLngBounds: TGoogleLatLngBounds);
    procedure setCenter(latlng: TGoogleLatLng);
    procedure setHeading(heading: Double);
    procedure setMapTypeId(mapTypeId: String);
    procedure setOptions(options: TGoogleMapOptions);
    procedure setTilt(tilt: Integer);
    procedure setZoom(zoom: Integer);
  end;

  TGoogleGeocoderAddressComponent = class external name 'google.maps.GeocoderAddressComponent'
  private
    flong_name: String; external name 'long_name';
    fshort_name: String; external name 'short_name';
    ftypes: TStringDynArray; external name 'types';
  public
    property long_name: String read flong_name;
    property short_name: String read fshort_name;
    property types: TStringDynArray read ftypes;
  end;

  TGoogleGeocoderStatus = class external name 'google.maps.GeocoderStatus'
  private
    class var fERROR: String; external name 'ERROR';
    class var fINVALID_REQUEST: String; external name 'INVALID_REQUEST';
    class var fOK: String; external name 'OK';
    class var fOVER_QUERY_LIMIT: String; external name 'OVER_QUERY_LIMIT';
    class var fREQUEST_DENIED: String; external name 'REQUEST_DENIED';
    class var fUNKNOWN_ERROR: String; external name 'UNKNOWN_ERROR';
    class var fZERO_RESULTS: String; external name 'ZERO_RESULTS';
  public
    class property ERROR: String read fERROR;
    class property INVALID_REQUEST: String read fINVALID_REQUEST;
    class property OK: String read fOK;
    class property OVER_QUERY_LIMIT: String read fOVER_QUERY_LIMIT;
    class property REQUEST_DENIED: String read fREQUEST_DENIED;
    class property UNKNOWN_ERROR: String read fUNKNOWN_ERROR;
    class property ZERO_RESULTS: String read fZERO_RESULTS;
  end;

  TGoogleGeocoderLocationType = class external name 'google.maps.GeocoderLocationType'
  private
    class var fAPPROXIMATE: String; external name 'APPROXIMATE';
    class var fGEOMETRIC_CENTER: String; external name 'GEOMETRIC_CENTER';
    class var fRANGE_INTERPOLATED: String; external name 'RANGE_INTERPOLATED';
    class var fROOFTOP: String; external name 'ROOFTOP';
  public
    class property APPROXIMATE: String read fAPPROXIMATE;
    class property GEOMETRIC_CENTER: String read fGEOMETRIC_CENTER;
    class property RANGE_INTERPOLATED: String read fRANGE_INTERPOLATED;
    class property ROOFTOP: String read fROOFTOP;
  end;

  TGoogleGeocoderGeometry = class external name 'google.maps.GeocoderGeometry'
  private
    fbounds: TGoogleLatLngBounds; external name 'bounds';
    flocation: TGoogleLatLng; external name 'location';
    flocation_type: TGoogleGeocoderLocationType; external name 'location_type';
    fviewport: TGoogleLatLngBounds; external name 'viewport';
  public
    property bounds: TGoogleLatLngBounds read fbounds;
    property location: TGoogleLatLng read flocation;
    property location_type: TGoogleGeocoderLocationType read flocation_type;
    property viewport: TGoogleLatLngBounds read fviewport;
  end;

  type
    TGoogleGeocoderAddressComponents = array of TGoogleGeocoderAddressComponent;

  type
  TGoogleGeocoderResult = class external name 'google.maps.GeocoderResult'
  private
    faddress_components: TGoogleGeocoderAddressComponents; external name 'address_components';
    fformatted_address: String; external name 'formatted_address';
    fgeometry: TGoogleGeocoderGeometry; external name 'geometry';
    fpartial_match: Boolean; external name 'partial_match';
    fpostcode_localities: TStringDynArray; external name 'postcode_localities';
    ftypes: TStringDynArray; external name 'types';
  public
    property address_components: TGoogleGeocoderAddressComponents read faddress_components;
    property formatted_address: String read fformatted_address;
    property geometry: TGoogleGeocoderGeometry read fgeometry;
    property partial_match: Boolean read fpartial_match;
    property postcode_localities: TStringDynArray read fpostcode_localities;
    property types: TStringDynArray read ftypes;
  end;

  TGoogleGeocoderResults = class external name 'TGoogleGeocoderResults'
  private
    flength: Integer; external name 'length';
  public
    function fitems(index: Integer): TGoogleGeocoderResult;
    property items[index: Integer]: TGoogleGeocoderResult read fitems; default;
    property length: Integer read flength;
  end;

  TGoogleGeocoderCallback = procedure (results: TGoogleGeocoderResults;
                                              const status: String) of object;

  TGoogleGeocoderComponentRestrictions = class external name 'google.maps.GeocoderComponentRestrictions'
  private
    fadministrativeArea: String; external name 'administrativeArea';
    fcountry: String; external name 'country';
    flocality: String; external name 'locality';
    fpostalCode: String; external name 'postalCode';
    froute: String; external name 'route';
  public
    property administrativeArea: String read fadministrativeArea write fadministrativeArea;
    property country: String read fcountry write fcountry;
    property locality: String read flocality write flocality;
    property postalCode: String read fpostalCode write fpostalCode;
    property route: String read froute write froute;
  end;

  TGoogleGeocoderRequest = class external name 'google.maps.GeocoderRequest'
  private
    faddress: String; external name 'address';
    fbounds: TGoogleLatLngBounds; external name 'bounds';
    fcomponentRestrictions: TGoogleGeocoderComponentRestrictions; external name 'componentRestrictions';
    flocation: TGoogleLatLng; external name 'location';
    fregion: String; external name 'region';
  public
    property address: String read faddress write faddress;
    property bounds: TGoogleLatLngBounds read fbounds write fbounds;
    property componentRestrictions: TGoogleGeocoderComponentRestrictions read fcomponentRestrictions write fcomponentRestrictions;
    property location: TGoogleLatLng read flocation write flocation;
    property region: String read fregion write fregion;
  end;

  TGoogleGeocoder = class external name 'google.maps.Geocoder'
  public
    constructor New;
    procedure geocode(request: TGoogleGeocoderRequest; callback: TGoogleGeocoderCallback);
  end;

  TGoogleAnimation = class external name 'google.maps.Animation'
  private
    class var fBOUNCE: String; external name 'BOUNCE';
    class var fDROP: String; external name 'DROP';
  public
    class property BOUNCE: String read fBOUNCE;
    class property DROP: String read fDROP;
  end;

  TGoogleAttribution = class external name 'google.maps.Attribution'
  private
    fiosDeepLinkId: String; external name 'iosDeepLinkId';
    fsource: String; external name 'source';
    fwebUrl: String; external name 'webUrl';
  public
    property iosDeepLinkId: String read fiosDeepLinkId write fiosDeepLinkId;
    property source: String read fsource write fsource;
    property webUrl: String read fwebUrl write fwebUrl;
  end;

  TGooglePlace = class external name 'google.maps.Place'
  private
    flocation: TGoogleLatLng; external name 'location';
    fplaceId: String; external name 'placeId';
    fquery: String; external name 'query';
  public
    property location: TGoogleLatLng read flocation write flocation;
    property placeId: String read fplaceId write fplaceId;
    property query: String read fquery write fquery;
  end;

  TGoogleMarkerOptions = class external name 'google.maps.MarkerOptions'
  //TGoogleMarkerOptions = class external name 'Object' (TJSObject)
  private
    fanchorPoint: TGooglePoint; external name 'anchorPoint';
    fanimation: String; external name 'animation';
    fattribution: TGoogleAttribution; external name 'attribution';
    fclickable: Boolean; external name 'clickable';
    fcrossOnDrag: Boolean; external name 'crossOnDrag';
    fcursor: String; external name 'cursor';
    fdraggable: Boolean; external name 'draggable';
    ficon: String; external name 'icon';
    fmap: TGoogleMap; external name 'map';
    fopacity: Double; external name 'opacity';
    foptimized: Boolean; external name 'optimized';
    fplace: TGooglePlace; external name 'place';
    fposition: TGoogleLatLng; external name 'position';
    ftitle: String; external name 'title';
    fvisible: Boolean; external name 'visible';
    fzIndex: Integer; external name 'zIndex';
  public
   property anchorPoint: TGooglePoint read fanchorPoint write fanchorPoint;
   property animation: String read fanimation write fanimation;
   property attribution: TGoogleAttribution read fattribution write fattribution;
   property clickable: Boolean read fclickable write fclickable;
   property crossOnDrag: Boolean read fcrossOnDrag write fcrossOnDrag;
   property cursor: String read fcursor write fcursor;
   property draggable: Boolean read fdraggable write fdraggable;
   property icon: String read ficon write ficon;
   property map: TGoogleMap read fmap write fmap;
   property opacity: Double read fopacity write fopacity;
   property optimized: Boolean read foptimized write foptimized;
   property place: TGooglePlace read fplace write fplace;
   property position: TGoogleLatLng read fposition write fposition;
   property title: String read ftitle write ftitle;
   property visible: Boolean read fvisible write fvisible;
   property zIndex: Integer read fzIndex write fzIndex;
 end;

  TGoogleMarker = class external name 'google.maps.Marker' (TGoogleMVCObject)
  public
    constructor New(opts: TGoogleMarkerOptions=nil);
    function getAnimation: TGoogleAnimation;
    function getAttribution: TGoogleAttribution;
    function getClickable: Boolean;
    function getCursor: String;
    function getDraggable: Boolean;
    function getIcon: String;
    function getMap: TGoogleMap;
    function getOpacity: Double;
    function getPlace: TGooglePlace;
    function getPosition: TGoogleLatLng;
    function getTitle: String;
    function getVisible: Boolean;
    function getZIndex: Integer;
    procedure setAnimation(animation: TGoogleAnimation);
    procedure setAttribution(attribution: TGoogleAttribution);
    procedure setClickable(flag: Boolean);
    procedure setCursor(const cursor: String);
    procedure setDraggable(flag: Boolean);
    procedure setIcon(const icon: String);
    procedure setMap(map: TGoogleMap);
    procedure setOpacity(opacity: Double);
    procedure setOptions(options: TGoogleMarkerOptions);
    procedure setPlace(place: TGooglePlace);
    procedure setPosition(latlng: TGoogleLatLng);
    procedure setTitle(const title: String);
    procedure setVisible(visible: Boolean);
    procedure setZIndex(zIndex: Integer);
   end;

implementation

end.

