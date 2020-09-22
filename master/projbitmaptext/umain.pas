unit uMain;

  {$mode objfpc}
  {$R fonts/desyrel.png}

interface

uses
  Classes, SysUtils, Types, Math, Contnrs, JS, Web, p2jsres;

procedure bitmapText(TargetRoot: TJSElement; FontName: String; Text: String;
                     Xaxis: Integer; Yaxis: Integer; Scale: Integer; kerning: boolean);

implementation

procedure bitmapText(TargetRoot: TJSElement; FontName: String; Text: String;
                     Xaxis: Integer; Yaxis: Integer; Scale: Integer; kerning: boolean);
var
  aInfo : TResourceInfo;
  CNT_IMAGEDATA: String;

begin
  if not GetResourceInfo(FontName, aInfo) then
    console.log('font '+FontName+' resource not found!')
  else
   CNT_IMAGEDATA :='data:'+aInfo.format+';'+aInfo.encoding+','+ainfo.Data;
  ASM
  var Result=new function(TargetRoot,FontName,Text){this.g=TargetRoot,this.i=FontName,this.h=Text||0,this.a=new Image,this.a.setAttribute("crossOrigin","anonymous"),this.a.src=CNT_IMAGEDATA;this.b=[]}("fonts/"+FontName.toLowerCase()+".png","fonts/"+FontName.toLowerCase()+".xml",kerning);(function(TargetRoot,FontName,Text){var w={};return new Promise(function(C){w.c={};var D,kerning=0,Result=TargetRoot.i;(async function(url){var g;return g=await fetch(url),await g.text()})(Result).then(function(str){return(new DOMParser).parseFromString(str,"text/xml")}).then(function(El){Result=El.getElementsByTagName("info")[0],D=El.getElementsByTagName("common")[0],w.font=Result.getAttribute("face"),w.size=parseInt(Result.getAttribute("size"),10),w.lineHeight=parseInt(D.getAttribute("lineHeight"),10)+0,Result=$mod.frame?$mod.frame.x:0,D=$mod.frame?$mod.frame.y:0,El=El.getElementsByTagName("character");for(var e=0;e<El.length;e++){var I=FontName.charCodeAt(e)-32;e<FontName.length&&(TargetRoot.b[e]={x:Result+parseInt(El[I].getAttribute("x"),10),y:D+parseInt(El[I].getAttribute("y"),10),width:parseInt(El[I].getAttribute("width"),10),height:parseInt(El[I].getAttribute("height"),10),j:parseInt(El[I].getAttribute("xoffset"),10)/1,l:parseInt(El[I].getAttribute("yoffset"),10)/1,f:(parseInt(El[I].getAttribute("xadvance"),10)+1)/1},kerning+=TargetRoot.b[e].f)}w.lineWidth=(kerning+TargetRoot.h+10)*Text,w.c=TargetRoot.b,C(w)})})})(Result,Text,Scale).then(function(A){var g=A.lineHeight,Text=A.lineWidth,Xaxis=A.c;return new Promise(function(A,C){var Q=new Image;Q.setAttribute("crossOrigin","anonymous"),Q.onload=function(){var C=0,Q=document.createElement("canvas");Q.width=Text,Q.height=g;var E=Q.getContext("2d");for(E.scale(Scale,Scale),Q=0;Q<Xaxis.length;Q++){var e=parseInt(Xaxis[Q].x),I=parseInt(Xaxis[Q].y),s=parseInt(Xaxis[Q].width),t=parseInt(Xaxis[Q].height),r=parseInt(Xaxis[Q].j),n=parseInt(Xaxis[Q].l),x=parseInt(Xaxis[Q].f);E.drawImage(Result.a,e,I,s,t,C+r,n,s,t),C+=x}A(E.canvas.toDataURL("image/png",1))},Q.onerror=function(A){C(A)},Q.src="data:image/gif;base64,R0lGODlhAQABAIAAAP///wAAACwAAAAAAQABAAACAkQBADs="})}).then(function(res){setTimeout(function(){TargetRoot.src=res,TargetRoot.setAttribute("style","position:absolute; left:"+Xaxis+"px; top:"+Yaxis+"px;")},0)});
  END;
end;

end.

