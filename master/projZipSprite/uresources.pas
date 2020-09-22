unit uResources;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, Web;

type
  TZipSprite = class external name 'window.RES'
  Public
    constructor New(const blob: JSValue);
    function createURL(const filename: string): String;
  end;

var
  RT_RCDATA: TJSPromise;
  { global methods }
  function LoadFromResourceName(const resourceName: string): String;

implementation

var
  Sprite: TZipSprite;
  queue: TJSPromise; external name 'Promise.resolve()';
  { global external functions }
  function LoadResourcesFromZipFile(const url: String): TJSPromise; external name 'LoadResourcesFromZipFile';

function LoadFromResourceName(const resourceName: string): String;
begin
  Result := Sprite.createURL(resourceName);
end;

initialization

(* //** BLOCK TO ADD RT_RCDATA RESOURCES

  {╔═════════════════════════════════════╗}
  {║} RT_RCDATA._then(procedure() Begin {║} {>>> begin RT_RCDATA :: LoadFromResourceName('rcdata.ext') <<<}
  {╚═════════════════════════════════════╝}

  // LoadFromResourceName('rcdata.ext')

  {╔═════════════════════════════════════╗}
  {║} End);                             {║} {>>> end RT_RCDATA block <<<}
  {╚═════════════════════════════════════╝}
*)

RT_RCDATA := queue
  ._then(function(blob: JSValue): JSValue
  begin
    console.log('RESOURCES HAS BEEN LOADED!');
    Result := LoadResourcesFromZipFile('projZipSprite.res');
  end)
  ._then(function(blob: JSValue): JSValue
  begin
    Sprite:= TZipSprite.New(blob);
    Result := Sprite;
  end);

end.