unit uMain;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  uPromises,
  JS, Web, Types, Math, Classes, SysUtils;

procedure fetchImgSequentially;
procedure FetchImgAtAll;
procedure FetchImgAtOnce;


implementation

(*type
  JEventListenerHandler = procedure(Event: TJSEvent) of object;


procedure bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
var
  events : TStringDynArray;
   i: Integer;

begin
  events := TJSString(EventType).split(' ');

  (* check if addeventlistener exists / For all major browsers *)
  if (element.addEventListenerExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      element.addEventListener(events[i], handler, false);
  end else
  (* check if attachEvent exists / For IE 8 and earlier *)
  if (element.attachEventExists) then
  begin
    for i:= 0 to TJSArray(events).length - 1 do
      element.attachEvent('on'+events[i], handler);
  end;
end;
*)

type
  JArray = class;

  TPromiseCallBack = function (element : JSValue; index: NativeInt; anArray : JArray) : JPromise;

  JArray = Class external name 'Array' (TJSArray)
  public
    Function map(const aCallBack : TPromiseCallBack) : JArray;
  end;


function getImage(url: JSValue): JPromise;

  procedure p(resolve: TJPromiseCallback; reject: TJPromiseCallback);
  var img: TJSElement;
     rand: Integer;


    procedure proc1;
      procedure r;
      begin
        resolve(url);
      end;

    begin
      window.setTimeout(@r, rand);
    end;

    procedure proc2;
      procedure r;
      begin
        reject(url);
      end;
    begin
      window.setTimeout(@r, rand);
    end;

  begin
    rand := round( random() * 1000 ); // add random delay to resolve and reject to accentuate asynchronicity
    img := document.createElement('img');
    img.addEventListener('load', @proc1);
    img.addEventListener('error', @proc2);
    img.setAttribute('src', String(url));
  end;

begin
  Result := JPromise.new(@p);
end;


procedure fetchImgSequentially;
var
  doggies : JArray;
  doggyplayground: TJSHTMLElement;
  targetimage: JSValue;

  procedure displayimages(images: JArray);

    function p(url: JSValue): JSValue;  // load image then...
    var docFragment : TJSDocumentFragment;
        div0, div_0, img, div_1: TJSElement;
        text: TJSNode;
    begin
      docFragment := document.createDocumentFragment(); // contains all gathered nodes
      div0 := document.createElement('DIV');
      div0.setAttribute('class', 'box');
      docFragment.appendChild(div0);

      div_0 := document.createElement('DIV');
      div_0.setAttribute('class', 'boxInner');
      div0.appendChild(div_0);

      img := document.createElement('IMG');
      img.setAttribute('src', String(url));
      div_0.appendChild(img);

      div_1 := document.createElement('DIV');
      div_1.setAttribute('class', 'titleBox');
      div_0.appendChild(div_1);
      text := document.createTextNode('Butterfly');
      div_1.appendChild(text);

      doggyplayground.appendChild(docFragment); // add image to DIV
      displayimages(images); // recursion- call displayimages() again to process next image/doggy
    end;

    function c(url: String): JSValue; // handle an image not loading
    begin
      console.log('Error loading ' + url);
      displayimages(images); // recursion- call displayimages() again to process next image/doggy
    end;

  begin
    targetimage := images.shift(); // process doggies images one at a time
    if (targetimage) then begin // if not end of array

    getImage(targetimage)
       .&then(@p)
       .catch(@c)
    end;
  end;


begin
  doggies := JArray.new('images/g01.jpg', 'images/g02.jpg', 'images/g03.jpg', 'images/g04.jpg', 'images/g05.jpg','images/g06.jpg', 'images/g07.jpg', 'images/g08.jpg', 'images/g09.jpg', 'images/g10.jpg','images/g11.jpg', 'images/g12.jpg', 'images/g13.jpg', 'images/g14.jpg', 'images/g15.jpg','images/g16.jpg');

  doggyplayground := TJSHTMLElement( document.getElementsByClassName('wrap').Items[0]); // document.getElementsByClassName('wrap')[0];
  doggyplayground.innerHTML := '';
  displayimages(doggies);
end;


procedure FetchImgAtAll;
var
  doggies : JArray;
  doggyplayground: TJSHTMLElement;

  procedure displayimagesAtAll(images: JArray);
  var
    doggypromises: JArray;

             function r(urls: JSValue): JSValue;
             var
               i: Integer;
               docFragment : TJSDocumentFragment;
               div0, div_0, img, div_1: TJSElement;
               text: TJSNode;

             begin
               doggyplayground.innerHTML := '';
               for i :=0 to TJSArray(urls).length - 1 do
               begin
                 docFragment := document.createDocumentFragment(); // contains all gathered nodes
                 div0 := document.createElement('DIV');
                 div0.setAttribute('class', 'box');
                 docFragment.appendChild(div0);

                 div_0 := document.createElement('DIV');
                 div_0.setAttribute('class', 'boxInner');
                 div0.appendChild(div_0);

                 img := document.createElement('IMG');
                 img.setAttribute('src', String( TJSArray(urls)[i]) );
                 div_0.appendChild(img);

                 div_1 := document.createElement('DIV');
                 div_1.setAttribute('class', 'titleBox');
                 div_0.appendChild(div_1);
                 text := document.createTextNode('Butterfly');
                 div_1.appendChild(text);
                 doggyplayground.appendChild(docFragment); // add image to DIV
       	       end;
             end;

             function c(urls: String): JSValue;
             begin
               console.log('Error fetching some images: ' + urls)
             end;

             function n(element : JSValue; index: NativeInt; anArray : JArray) : JPromise;
             begin
               result := getImage(element);
             end;

  begin
 // doggypromises := images.map(getImage); // call getImage on each array element and return array of promises
    doggypromises := images.map(@n); // call getImage on each array element and return array of promises

    Promise
     .all(doggypromises)
     .&then(@r)
     .catch(@c);
  end;

begin
  doggies := JArray.new('images/g01.jpg', 'images/g02.jpg', 'images/g03.jpg', 'images/g04.jpg', 'images/g05.jpg','images/g06.jpg', 'images/g07.jpg', 'images/g08.jpg', 'images/g09.jpg', 'images/g10.jpg','images/g11.jpg', 'images/g12.jpg', 'images/g13.jpg', 'images/g14.jpg', 'images/g15.jpg','images/g16.jpg');
  doggyplayground := TJSHTMLElement( document.getElementsByClassName('wrap').Items[0]); //document.getElementsByClassName('wrap')[0];
  doggyplayground.innerHTML := 'Fetching the girls...';

  displayimagesAtAll(doggies);
end;


procedure FetchImgAtOnce;
var
  doggies : JArray;
  doggyplayground: TJSHTMLElement;

  procedure displayimagesBestofBothWorlds(images: JArray);
  var
    doggypromises: JArray;
    resolvedPromise: JPromise;

      procedure p(curPromise: JSValue);

        function p1(aValue: JSValue): JSValue;
        begin
          exit( curPromise );
        end;

        function p2(url: JSValue): JSValue;
        var
          docFragment : TJSDocumentFragment;
          div0, div_0, img, div_1: TJSElement;
          text: TJSNode;

        begin
          docFragment := document.createDocumentFragment(); // contains all gathered nodes
          div0 := document.createElement('DIV');
          div0.setAttribute('class', 'box');
          docFragment.appendChild(div0);

          div_0 := document.createElement('DIV');
          div_0.setAttribute('class', 'boxInner');
          div0.appendChild(div_0);

          img := document.createElement('IMG');
          img.setAttribute('src', String(url));
          div_0.appendChild(img);

          div_1 := document.createElement('DIV');
          div_1.setAttribute('class', 'titleBox');
          div_0.appendChild(div_1);
          text := document.createTextNode('Butterfly');
          div_1.appendChild(text);
          doggyplayground.appendChild(docFragment); // add image to DIV
        end;

        function p3(err: JSValue): JSValue;
        begin
          console.log(String(err) + ' failed to load!');
        end;

      begin
        resolvedPromise := resolvedPromise
          .&then(@p1)
          .&then(@p2)
          .catch(@p3);
      end;

      function n(element : JSValue; index: NativeInt; anArray : JArray) : JPromise;
      begin
        result := getImage(element);
      end;

      function q(element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;
      begin
        p(element);
      end;

  Begin
  doggyplayground := TJSHTMLElement( document.getElementsByClassName('wrap').Items[0]);
//  doggypromises := images.map(getImage); // call getImage on each array element and return array of promises
  doggypromises := images.map(@n); // call getImage on each array element and return array of promises

  resolvedPromise := Promise.resolve();

  doggypromises.forEach(@q);
  End;

BEGIN
  doggies := JArray.new('images/g01.jpg', 'images/g02.jpg', 'images/g03.jpg', 'images/g04.jpg', 'images/g05.jpg','images/g06.jpg', 'images/g07.jpg', 'images/g08.jpg', 'images/g09.jpg', 'images/g10.jpg','images/g11.jpg', 'images/g12.jpg', 'images/g13.jpg', 'images/g14.jpg', 'images/g15.jpg','images/g16.jpg');
  doggyplayground := TJSHTMLElement( document.getElementsByClassName('wrap').Items[0]); //document.getElementsByClassName('wrap')[0];
  doggyplayground.innerHTML := '';

  displayimagesBestofBothWorlds(doggies);
END;

end.


