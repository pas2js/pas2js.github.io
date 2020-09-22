program projhtmltemplateresource;

{$mode objfpc}

{$R testres.html}

uses
  browserconsole, JS, Classes, SysUtils, types, p2jsres, unita, unitb, web;

procedure OnLoaded(const html: String);
var
  El: TJSElement;
begin
  console.log('onLoaded');
  El := document.getElementById('playarea');
  El.innerHTML := html;
end;

procedure OnLoadFailed(const aError: string);
begin
  console.log('Failed to load resources : '+AError)
end;

function DoOnClick(aEvent: TJSMouseEvent): boolean;
var
  el : TJSHTMLElement;
  aInfo : TResourceInfo;
begin
  Result:=False;
  el:=TJSHTMLElement(document.getelementByid('playarea'));
  (*
  if not GetResourceInfo('testres',aInfo) then
    el.innerhtml:='resource testres not found !'
  else
    el.innerhtml:=window.atob(ainfo.Data);
  *)
  getHTMLTemplateResource('htmlloadlinkdemo-res.html','interface', 'pas2js2', @OnLoaded,@OnLoadFailed)
end;

Var
  RL : TStringDynArray;
  aInfo : TResourceInfo;
  el : TJSHTMLElement;
  S : String;
begin
  Writeln('Javascript embedded resources:');
  SetResourceSource(rsJS);
  RL:=GetResourceNames;
  For S in RL do
    begin
    Writeln('--- Found resource name: ',S,' : ');
    if not GetResourceInfo(S,aInfo) then
      Writeln('No extra information for resource ',S,' available !')
    else
      begin
      Writeln('Name: ',aInfo.Name);
      Writeln('Format: ',aInfo.Format);
      Writeln('encoding: ',aInfo.Encoding);
      Writeln('unit: ',aInfo.resourceunit);
      Writeln('data length: ',Length(aInfo.data));
      end;
    end;
  el:=TJSHTMLButtonElement(document.getelementByid('doinsert'));
  el.onclick:=@DoOnClick;
end.
