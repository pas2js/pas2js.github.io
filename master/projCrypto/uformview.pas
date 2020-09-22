unit uFormView;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  SysUtils, JS, Web, System.Diagnostics, SynCrossPlatformCrypto, uCryptoJS, uUtils;

type
  { CipherParams }
  CipherParams = record
    mode: JSValue;  external name 'mode'; //external name 'CryptoJS.mode.CBC';
    padding: JSValue; external name 'padding'; //external name 'CryptoJS.pad.Pkcs7';
  end;

type
  { TJCripto }
  TJCripto = class
  private
    (* Private declarations *)
    param : CipherParams;
    json: JCipherParams;
    decrypt: JWordArray;
  protected
    (* Protected declarations *)
    procedure OnClickButton1(e: TJSEvent);
    procedure OnClickButton2(e: TJSEvent);
    procedure OnClickButton3(e: TJSEvent);
    procedure OnClickButton4(e: TJSEvent);
    procedure OnClickButton5(e: TJSEvent);
  public
    (* Public declarations *)
    constructor Create;
  published
    (* Published declarations *)
    procedure InitializeObject;
  end;

implementation

var
  _MODE: JSValue; external name 'CryptoJS.mode.CBC';
  _PADDING: JSValue; external name 'CryptoJS.pad.Pkcs7';

  _ENCRYPTBTN: TJSElement; external name 'document.querySelector(".encrypt")';
  _ENCRYPTEXT: String; external name 'document.querySelector("#en").value';
  _RESTXT: String; external name 'document.querySelector("#resultado").innerText';

  _DECRYPTBTN: TJSElement; external name 'document.querySelector(".decrypt")';
  _DECRYPTEXT: String; external name 'document.querySelector("#de").value';

  _GENKEYBTN: TJSElement; external name 'document.querySelector(".genkey")';
  _GENKEYTXT: String; external name 'document.querySelector("#key").value';

  _GENUUID1BTN: TJSElement; external name 'document.querySelector(".uuid1")';
  _GENUUID1TXT: String; external name 'document.querySelector("#d1").value';

  _GENUUID2BTN: TJSElement; external name 'document.querySelector(".uuid2")';
  _GENUUID2TXT: String; external name 'document.querySelector("#d2").value';


  function GenerateUUID:string;
  // The procedure to generate a version 4 UUID is as follows:
  // 1. Generate 16 random bytes (=128 bits)
  // 2. Adjust certain bits according to RFC 4122 section 4.4 as follows:
  //   a. set the four most significant bits of the 7th byte to 0100'B, so the high nibble is '4'
  //   b. set the two most significant bits of the 9th byte to 10'B, so the high nibble will be one of '8', '9', 'A', or 'B'.
  // 3. Convert the adjusted bytes to 32 hexadecimal digits
  // 4. Add four hyphen '-' characters to obtain blocks of 8, 4, 4, 4 and 12 hex digits
  // 5. Output the resulting 36-character string "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
  var
    d0, d1, d2, d3: integer;
  begin
    d0 := Round(Now + GetTickCount) + Trunc(Random * $ffffffff);
    d1 := d0 + Trunc(Random * $ffffffff);
    d2 := d1 + Trunc(Random * $ffffffff);
    d3 := d2 + Trunc(Random * $ffffffff);
    d1 := (d1 and $ff0fffff) or $00400000;
    d2 := (d2 and $ffffff3f) or $00000080;
    Result := IntToHex(d0 and $ff, 2) + IntToHex((d0 shr 8) and $ff, 2) +
      IntToHex((d0 shr 16) and $ff, 2) + IntToHex((d0 shr 24) and $ff, 2) + '-' +
      IntToHex(d1 and $ff, 2) + IntToHex((d1 shr 8) and $ff, 2) + '-' +
      IntToHex((d1 shr 16) and $ff, 2) + IntToHex((d1 shr 24) and $ff, 2) + '-' +
      IntToHex(d2 and $ff, 2) + IntToHex((d2 shr 8) and $ff, 2) + '-' +
      IntToHex((d2 shr 16) and $ff, 2) + IntToHex((d2 shr 24) and $ff, 2) +
      IntToHex(d3 and $ff, 2) + IntToHex((d3 shr 8) and $ff, 2) +
      IntToHex((d3 shr 16) and $ff, 2) + IntToHex((d3 shr 24) and $ff, 2);
  end;

  function GenerateUUIDNew: string;
  // The procedure to generate a version 4 UUID is as follows:
  // 1. Generate 16 random bytes (=128 bits)
  // 2. Adjust certain bits according to RFC 4122 section 4.4 as follows:
  //   a. set the four most significant bits of the 7th byte to 0100'B, so the high nibble is '4'
  //   b. set the two most significant bits of the 9th byte to 10'B, so the high nibble will be one of '8', '9', 'A', or 'B'.
  // 3. Convert the adjusted bytes to 32 hexadecimal digits
  // 4. Add four hyphen '-' characters to obtain blocks of 8, 4, 4, 4 and 12 hex digits
  // 5. Output the resulting 36-character string "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
  var
    data: TJSUint8Array;
  begin
    data := TJSUint8Array.New(16);
    Window.Crypto.getRandomValues(data);
    //TJSCrypto.getRandomValues(data);
    data[6] := (data[6] and $0f) or $40;
    data[8] := (data[8] and $3f) or $80;
    Result := IntToHex(data[0], 2) + IntToHex(data[1], 2) +
      IntToHex(data[2], 2) + IntToHex(data[3], 2) + '-' +
      IntToHex(data[4], 2) + IntToHex(data[5], 2) + '-' +
      IntToHex(data[6], 2) + IntToHex(data[7], 2) + '-' +
      IntToHex(data[8], 2) + IntToHex(data[9], 2) + '-' +
      IntToHex(data[10], 2) + IntToHex(data[11], 2) +
      IntToHex(data[12], 2) + IntToHex(data[13], 2) +
      IntToHex(data[14], 2) + IntToHex(data[15], 2);
  end;

function troncaNum(c: String;a: Integer): String;
var
  b: String;
begin
  b:=  TJSString(c).toString;
  Result := TJSString(b).subString(0, a);
end;

function getHash(c: Integer;e: String): String;
var
  d : String;
  a, b: Integer;
begin
  b := parseInt(e);
  if (b = 128) then
  begin
    d := SHA256(IntToStr(c));
    a := Trunc(128 / 4);
    d := troncaNum(d, a)
  end else
  begin
    if (b = 192) then
    begin
      d := SHA256(IntToStr(c));
      a := Trunc(192 / 4);
      d := troncaNum(d, a)
    end else
    begin
      if (b = 256) then
      begin
        d := SHA256(IntToStr(c))
      end;
    end;
  end;

  Result := d;
end;


function randomPassphrase(c: String): String;
var
  d: Integer;
  b: String;
begin
  try
    d := Random(10000);
    b := getHash(d, c);
    result := b;
  except
    on E : Exception do
//    result := -1
  end;
end;

procedure genPassphrase;
var
  a: JSValue;
  _keySizeInBits: String;
  keysize: String;
begin
    _keySizeInBits := TJSHTMLOptionElement(document.querySelector('#keysize')).value;
  a := randomPassphrase(_keySizeInBits);

  keysize := String(a);
  _GENKEYTXT := String(keysize);
end;

procedure OnClickButton(e: TJSEvent);
begin

end;

{ ╔═══════════════════════════════════════╗
  ║ TJCripto                              ║
  ╚═══════════════════════════════════════╝ }
constructor TJCripto.Create;
begin
  param.mode:= _MODE;
  param.padding:= _PADDING;
end;


procedure TJCripto.OnClickButton1(e: TJSEvent);
begin
  // encrypt
  json := CryptoJS.AES.encrypt(_ENCRYPTEXT, _GENKEYTXT, param);
  _RESTXT := json.ciphertext.toString(CryptoJS.enc.Base64);
end;

procedure TJCripto.OnClickButton2(e: TJSEvent);
begin
  // decrypt
  decrypt := CryptoJS.AES.decrypt(json, _GENKEYTXT, param);
  _DECRYPTEXT := decrypt.toString(CryptoJS.enc.Utf8);
end;

procedure TJCripto.OnClickButton3(e: TJSEvent);
begin
  // generate password phrase
 genPassphrase;
end;

procedure TJCripto.OnClickButton4(e: TJSEvent);
begin
  _GENUUID1TXT := GenerateUUIDNew;
end;

procedure TJCripto.OnClickButton5(e: TJSEvent);
begin
  _GENUUID2TXT := GenerateUUID;
end;

procedure TJCripto.InitializeObject;
var
  Sender: TJSEvent;
begin
  _ENCRYPTEXT := 'myPassword';
  genPassphrase;   // generate key
  OnClickButton1(Sender);  // call encrypt

  bindEvent(_ENCRYPTBTN, 'click', @OnClickButton1);
  bindEvent(_DECRYPTBTN, 'click', @OnClickButton2);
  bindEvent(_GENKEYBTN, 'click', @OnClickButton3);
  bindEvent(_GENUUID1BTN, 'click', @OnClickButton4);
  bindEvent(_GENUUID2BTN, 'click', @OnClickButton5);
end;

end.

