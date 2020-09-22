unit MainView;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web, uSJCL;

type
  JEventListenerHandler = procedure(event: TJSEvent) of object;

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
  { Stanford Javascript Crypto Library }
  TSJCL = class
  private
    (* Private declarations *)
    param: JSCipherParams;
    parsedMessage: TJSCipherEncrypted;
    encryptedMessage: TJSCipherEncrypted;
    procedure bindEvent(element: TJSElement; EventType: String; handler: JEventListenerHandler);
  protected
    (* Protected declarations *)
    procedure doEncrypt(e: TJSEvent);
    procedure doDecrypt(e: TJSEvent);
    procedure doRunSimpleTest(e: TJSEvent);
    function getListParsed: TJSCipherEncrypted;
    function getListToString: String;
  public
    (* Public declarations *)
    constructor Create;
  published
    (* Published declarations *)
    procedure InitializeObject;
  end;

implementation

var
  BODY: TJSElement; external name 'document.body';
  _ENCRYPT: TJSElement; external name 'document.querySelector("#encrypt")';
  _DECRYPT: TJSElement; external name 'document.querySelector("#decrypt")';
  _RUNTEXT: TJSElement; external name 'document.querySelector("#RunTest")';
  _PLAINTEXT: String; external name 'document.querySelector("#plaintext").value';
  _PLAINPASSWORD: String; external name 'document.querySelector("#password").value';
  _LCIPHERTEXT: String; external name 'document.querySelector("#ciphertext").value';
  _SCIPHERTEXT: String; external name 'document.querySelector("#shortciphertext").value';

{ TSJCL }
{ ╔══════════════════════════════════════════════════╗
  ║ Insert here the (generated code)  TODO           ║
  ╚══════════════════════════════════════════════════╝ }

procedure TSJCL.bindEvent(element: TJSElement; EventType: String;
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

constructor TSJCL.Create;
var
  i: Longint;
  docFragment : TJSDocumentFragment;
  text, text_0, text_1, text_2, text_3, text_4, text_5, text_6, text_7, text_8,
  text_9, text_10, text_11, text_12, text_13: TJSNode;
  h1, theform, ckey, ppassword, h2, div_, label_, password,
  p, cmode, ctexts, pplaintext, h2_0, div_0, label_0,
  plaintext,  div_1, buttons, encrypt,
  span, decrypt, span_0, pciphertext, h2_1,
  label_1, ciphertext, div_2, sciphertext,
  h2_2, label_2, shortciphertext, btn : TJSElement;

//begin
{ ╔═══════════════════════════════════════════════════════════════════════════╗
  ║ Since the document fragment is in memory and not part of the main DOM     ║
  ║ tree, appending children to it does not cause page reflow (computation    ║
  ║ of elements position and geometry). Consequently, using documentfragments ║
  ║ often results in better performance.                                      ║
  ╚═══════════════════════════════════════════════════════════════════════════╝ }
begin
  docFragment := document.createDocumentFragment(); // contains all gathered nodes

  h1 := document.createElement('H1');
  docFragment.appendChild(h1);

  text := document.createTextNode('Stanford Javascript Crypto Library - pas2js demo');
  h1.appendChild(text);

  theform := document.createElement('FORM');
  theform.setAttribute('id', 'theForm');
  theform.setAttribute('onsubmit', 'return false;');
  docFragment.appendChild(theform);
(*
  btn := document.createElement('BUTTON');
  btn.setAttribute('id', 'RunTest');
  btn.setAttribute('data-icon', '|');
  btn.setAttribute('title', 'Turn ON');
  btn.setAttribute('class', 'btn btn-default');
  btn.textContent:= 'Run simple test';
  docFragment.appendChild(btn);*)

  ckey := document.createElement('DIV');
  ckey.setAttribute('class', 'column');
  ckey.setAttribute('id', 'ckey');
  theform.appendChild(ckey);

  ppassword := document.createElement('DIV');
  ppassword.setAttribute('class', 'box');
  ppassword.setAttribute('id', 'ppassword');
  ckey.appendChild(ppassword);

  h2 := document.createElement('H2');
  ppassword.appendChild(h2);
  text_0 := document.createTextNode('Password');
  h2.appendChild(text_0);

  div_ := document.createElement('DIV');
  div_.setAttribute('class', 'section');
  ppassword.appendChild(div_);

  label_ := document.createElement('LABEL');
  label_.setAttribute('for', 'password');
  div_.appendChild(label_);
  text_1 := document.createTextNode('Password:');
  label_.appendChild(text_1);

  password := document.createElement('INPUT');
  password.setAttribute('type', 'password');
  password.setAttribute('class', 'wide');
  password.setAttribute('name', 'password');
  password.setAttribute('id', 'password');
  password.setAttribute('autocomplete', 'off');
  password.setAttribute('tabindex', '1');
//  password.setAttribute('value', 'myPassword');
  div_.appendChild(password);

  p := document.createElement('P');
  p.setAttribute('class', 'explanation');
  div_.appendChild(p);
  text_2 := document.createTextNode(' Choose a strong, random password.\n        ');
  p.appendChild(text_2);

  cmode := document.createElement('DIV');
  cmode.setAttribute('class', 'column');
  cmode.setAttribute('id', 'cmode');
  theform.appendChild(cmode);

  ctexts := document.createElement('DIV');
  ctexts.setAttribute('class', 'column');
  ctexts.setAttribute('id', 'ctexts');
  theform.appendChild(ctexts);

  pplaintext := document.createElement('DIV');
  pplaintext.setAttribute('id', 'pplaintext');
  pplaintext.setAttribute('class', 'box');
  ctexts.appendChild(pplaintext);

  h2_0 := document.createElement('H2');
  pplaintext.appendChild(h2_0);
  text_3 := document.createTextNode('Plaintext');
  h2_0.appendChild(text_3);

  div_0 := document.createElement('DIV');
  div_0.setAttribute('class', 'section');
  pplaintext.appendChild(div_0);


  label_0 := document.createElement('LABEL');
  label_0.setAttribute('for', 'plaintext');
  div_0.appendChild(label_0);
//  text_4 := document.createTextNode('Secret message:');
//  label_0.appendChild(text_4);

  plaintext := document.createElement('TEXTAREA');
  plaintext.setAttribute('id', 'plaintext');
  plaintext.setAttribute('autocomplete', 'off');
  plaintext.setAttribute('rows', '5');
  plaintext.setAttribute('tabindex', '2');
  div_0.appendChild(plaintext);
//  text_5 := document.createTextNode('myMessage');
//  plaintext.appendChild(text_5);

  div_1 := document.createElement('DIV');
  div_1.setAttribute('class', 'explanation');
  div_0.appendChild(div_1);
  text_6 := document.createTextNode(' This message will be encrypted, so that nobody can read it or change it\n          without your password.\n        ');
  div_1.appendChild(text_6);

  buttons := document.createElement('DIV');
  buttons.setAttribute('id', 'buttons');
  ctexts.appendChild(buttons);

  encrypt := document.createElement('A');
//  encrypt.setAttribute('href', 'javascript:doEncrypt()');
  encrypt.setAttribute('id', 'encrypt');
  encrypt.setAttribute('tabindex', '4');
  buttons.appendChild(encrypt);

  span := document.createElement('SPAN');
  span.setAttribute('class', 'turnDown');
  encrypt.appendChild(span);
  text_7 := document.createTextNode('encrypt');
  span.appendChild(text_7);

  decrypt := document.createElement('A');
///  decrypt.setAttribute('href', 'javascript:doDecrypt()');
  decrypt.setAttribute('id', 'decrypt');
  decrypt.setAttribute('tabindex', '6');
  buttons.appendChild(decrypt);

  span_0 := document.createElement('SPAN');
  span_0.setAttribute('class', 'turnUp');
  decrypt.appendChild(span_0);
  text_8 := document.createTextNode('decrypt');
  span_0.appendChild(text_8);

  pciphertext := document.createElement('DIV');
  pciphertext.setAttribute('id', 'pciphertext');
  pciphertext.setAttribute('class', 'box');
  ctexts.appendChild(pciphertext);

  h2_1 := document.createElement('H2');
  pciphertext.appendChild(h2_1);
  text_9 := document.createTextNode('Ciphertext');
  h2_1.appendChild(text_9);

  label_1 := document.createElement('LABEL');
  label_1.setAttribute('for', 'ciphertext');
  pciphertext.appendChild(label_1);
  text_10 := document.createTextNode('Ciphertext:');
  label_1.appendChild(text_10);

  ciphertext := document.createElement('TEXTAREA');
  ciphertext.setAttribute('id', 'ciphertext');
  ciphertext.setAttribute('autocomplete', 'off');
  ciphertext.setAttribute('rows', '7');
  ciphertext.setAttribute('tabindex', '5');
  pciphertext.appendChild(ciphertext);

  div_2 := document.createElement('DIV');
  div_2.setAttribute('class', 'explanation');
  pciphertext.appendChild(div_2);
  text_11 := document.createTextNode(' Your message, encrypted and authenticated so that nobody can read it\n        or change it without your password.');
  div_2.appendChild(text_11);

  sciphertext := document.createElement('DIV');
  sciphertext.setAttribute('id', 'sciphertext');
  sciphertext.setAttribute('class', 'box');
  sciphertext.setAttribute('style', 'width: 440px;');
  ///sciphertext.setAttribute(';', '');
  ctexts.appendChild(sciphertext);

  h2_2 := document.createElement('H2');
  sciphertext.appendChild(h2_2);
  text_12 := document.createTextNode('Only send short ciphertext parameters to the receiver.');

  h2_2.appendChild(text_12);

  label_2 := document.createElement('LABEL');
  label_2.setAttribute('for', 'ciphertext');
  sciphertext.appendChild(label_2);
  text_13 := document.createTextNode('Short Ciphertext:');
  label_2.appendChild(text_13);

  shortciphertext := document.createElement('TEXTAREA');
  shortciphertext.setAttribute('id', 'shortciphertext');
  shortciphertext.setAttribute('autocomplete', 'off');
  shortciphertext.setAttribute('rows', '2');
  shortciphertext.setAttribute('tabindex', '5');
  sciphertext.appendChild(shortciphertext);
  body.appendChild( docFragment );
end;

function JSExtend(const a: JSValue; const b: JSValue): JSValue; assembler;
asm
  for(var key in b)
    if(b.hasOwnProperty(key))
      a[key] = b[key];
  return a;
end;

function TSJCL.getListParsed: TJSCipherEncrypted;
  function _JSONPARSE(obj: TJSCipherEncrypted): TJSCipherEncrypted; external name 'JSON.parse';
begin
  result := _JSONPARSE(encryptedMessage);
end;

function TSJCL.getListToString: String;
  function _JSONSTRINGIFY(obj: TJSCipherEncrypted): String; external name 'JSON.stringify';
begin
  result := _JSONSTRINGIFY(parsedMessage);
end;

procedure TSJCL.doEncrypt(e: TJSEvent);
var
  properties: String;
  fieldnames: array [0 .. 7] of String = ('mode', 'iter', 'ks', 'ts', 'v', 'cipher', 'adata', 'salt');
  encryptedMessageWithoutParameters: string;

begin
  if (_PLAINTEXT) = '' then
    exit;

  (* Stanford Javascript Crypto Library. (SJCL) encrypt and decrypt without sending parameters *)

  // Set cipher parameter
    param.mode := 'ccm';
    param.iter := 1000;
    param.ks := 128;
    param.ts := 64;
    param.v := 1;
    param.cipher := 'aes';
    param.adata := '';
    param.salt :=  'myGeneratedSalt';

  // Encrypt string
    encryptedMessage := encrypt(_PLAINPASSWORD, _PLAINTEXT, param );

  // I want a very short object, we need to delete some properties.
    parsedMessage := getListParsed;   { parse the Obj message }
    _LCIPHERTEXT := getListToString;  { stringify the obj to display it at the textarea }

  // for instance, if the receiver already knows the parameters such as iv, salt. so we don't need to be sent
    for properties in fieldnames do
      JSDelete(parsedMessage, properties);

    encryptedMessageWithoutParameters := getListToString;

    _PLAINTEXT :=  '';
    _SCIPHERTEXT := encryptedMessageWithoutParameters;
end;

procedure TSJCL.doDecrypt(e: TJSEvent);
var
  messageWithParameters: String;
begin
//Decrypt string
  JSExtend(parsedMessage, param); // we need to extend the object before decrypt it
  messageWithParameters := getListToString;
  decrypt(_PLAINPASSWORD, messageWithParameters);
  _PLAINTEXT := decrypt(_PLAINPASSWORD, messageWithParameters);
end;

procedure TSJCL.doRunSimpleTest(e: TJSEvent);
var
  cp: TJSCipherEncrypted;

begin
  // Set cipher parameter
  param.mode := 'ccm';
  param.iter := 1000;
  param.ks := 128;
  param.ts := 64;
  param.v := 1;
  param.cipher := 'aes';
  param.adata := '';
  param.salt :=  'myGeneratedSalt';

  { test 1 }
  // Encrypt the message
  cp := encrypt(_PLAINPASSWORD, _PLAINTEXT);
  // show the encrypted JSON
  console.log(cp);
  // Decrypt the message
  console.log(
    decrypt(_PLAINPASSWORD, cp)
    );
  console.log('>>--------------------------<<');
  {test 2}
  cp := encrypt(_PLAINPASSWORD, _PLAINTEXT, param);
  console.log(
    decrypt(_PLAINPASSWORD, cp)
   );
  console.log('>>--------------------------<<');
end;

procedure TSJCL.InitializeObject;
begin
  _PLAINPASSWORD := 'myPassword';
  _PLAINTEXT := 'myMessage';

  bindEvent(_ENCRYPT, 'click', @doEncrypt);
  bindEvent(_DECRYPT, 'click', @doDecrypt);
//  bindEvent(_RUNTEXT, 'click', @doRunSimpleTest);
end;

end.

