unit uSJCL;
{ ╔══════════════════════════════════════════════════╗
  ║ Stanford Javascript Crypto Library               ║
  ║ https://github.com/bitwiseshiftleft/sjcl         ║
  ╚══════════════════════════════════════════════════╝ }

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, JS, Web;

type
  TProcCallback = procedure(obj: JSValue);

type
//  not sure these definitions are useful
  TJSSjcl = class external name 'sjcl'
    hash: JSValue; external name 'sjcl.hash';
    cipher: JSValue; external name 'sjcl.cipher';
    mode: JSValue; external name 'sjcl.mode';
    misc: JSValue; external name 'sjcl.misc';
    keyexchange: JSValue; external name 'sjcl.keyexchange';
    codec: JSValue; external name 'sjcl.codec';
  end;

  // TJSBitArray is a Javascript array of Doubles.
  // Create a TJSBitArray using the codec functions below.
  TJSBitArray = class external name 'sjcl.bitArray'
    class function bitLength(a: TJSBitArray): Double;
    class function bitSlice(a: TJSBitArray; bstart: Double; bend: Double): TJSBitArray; overload;
    class function bitSlice(a: TJSBitArray; bstart: Double): TJSBitArray; overload;
    class function clamp(a: TJSBitArray; len: Double): TJSBitArray;
    class function concat(a1: TJSBitArray; a2: TJSBitArray): TJSBitArray;
    class function equal(a: TJSBitArray; b: TJSBitArray): Boolean;
    class function extract(a: TJSBitArray; bstart: Double; blenth: Double): Double;
    class function getPartial(x: Double): Double;
    class function &partial(len: Double; x: Double): Double; overload;
    class function &partial(len: Double; x: Double; _end: Double): Double; overload;
  end;

  TJSCodecUtf8 = class external name 'sjcl.codec.utf8String'
    class function fromBits(bits: TJSBitArray): String;
    class function toBits(value: String): TJSBitArray;
  end;

  TJSCodecHex = class external name 'sjcl.codec.hex'
    class function fromBits(bits: TJSBitArray): String;
    class function toBits(value: String): TJSBitArray;
  end;

  TJSCodecBase32 = class external name 'sjcl.codec.base32'
    class function fromBits(bits: TJSBitArray; _noEquals, _hex: boolean): String;
    class function toBits(value: String; _hex: boolean): TJSBitArray;
  end;

  TJSCodecBase32Hex = class external name 'sjcl.codec.base32hex'
    class function fromBits(bits: TJSBitArray): String;
    class function toBits(value: String): TJSBitArray;
  end;

  TJSCodecBase64 = class external name 'sjcl.codec.base64'
    class function fromBits(bits: TJSBitArray; _noEquals, _url: boolean): String;
    class function toBits(value: String; _url: boolean): TJSBitArray;
  end;

  TJSCodecBase64URL = class external name 'sjcl.codec.base64url'
    class function fromBits(bits: TJSBitArray): String;
    class function toBits(value: String): TJSBitArray;
  end;

  TJSCodecBytes = class external name 'sjcl.codec.bytes'
    class function fromBits(bits: TJSBitArray): JSValue; {array of bytes}
    class function toBits(value: JSValue {array of bytes}): TJSBitArray;
  end;

  TJSCodecArrayBuffer = class external name 'sjcl.codec.arrayBuffer'
    class function fromBits(bits: TJSBitArray): TJSArrayBuffer; overload;
    class function fromBits(bits: TJSBitArray; padding: boolean; padding_count: integer): TJSArrayBuffer; overload;
    class function toBits(buffer: TJSArrayBuffer): TJSBitArray;
    class procedure hexDumpBuffer(buffer: TJSArrayBuffer);
  end;

  // Random
  TJSRandom = class external name 'sjcl.random'
    class function randomWords(nwords: Double): TJSBitArray; overload;
    class function randomWords(nwords: Double; paranoia: Double): TJSBitArray; overload;
    class procedure setDefaultParanoia(paranoia: Double; allowZeroParanoia: String);
    class procedure addEntropy(data: JSValue {Double or array of Double or String}; estimatedEntropy: Double; source: String);
    class function isReady: Boolean; overload;
    class function isReady(paranoia: Double): Boolean; overload;
    class function getProgress: Double; overload;
    class function getProgress(paranoia: Double): Double; overload;
    class procedure startCollectors;
    class procedure stopCollectors;
    class procedure addEventListener(name: String; cb: TProcCallback);
    class procedure removeEventListener(name: String; cb: TProcCallback);
  end;

  // Hash
  TJSHash = class external name 'TJSHash'
    //class var
    name: String; external name 'name';
    function reset: TJSHash;
    function update(data: String): TJSHash; overload;
    function update(data: TJSBitArray): TJSHash; overload;
    function finalize: TJSBitArray;
  end;

  TJSSHA256 = class external name 'sjcl.hash.sha256' (TJSHash)
    constructor New; //overload; external name 'sha256';
    constructor New(key: JSValue); //overload; external name 'sha256';
    class function hash(data: TJSBitArray): TJSBitArray; overload;
    class function hash(data: String): TJSBitArray; overload;
  end;

type
  TJSHmac = class external name 'sjcl.misc.hmac'
    constructor New(key: TJSBitArray); //overload; external name 'hmac';
    constructor New(key: TJSBitArray; hash: TJSHash); //overload; external name 'hmac';
    function encrypt(data: TJSBitArray): TJSBitArray; overload;
    function encrypt(data: string): TJSBitArray; overload;
    procedure reset;
    procedure update;
    function digest: TJSBitArray;
  end;

// PBKDF2
// defaults:
// iterations = 1000
// keyLength = output size of hash function
// Prff (pseudorandom function family) = HMAC

function pbkdf2(password: string; salt: string): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: string; salt: TJSBitArray): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: string): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: TJSBitArray): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';

function pbkdf2(password: string; salt: string; iterations: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: string; salt: TJSBitArray; iterations: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: string; iterations: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: TJSBitArray; iterations: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';

function pbkdf2(password: string; salt: string; iterations, keyLength: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: string; salt: TJSBitArray; iterations, keyLength: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: string; iterations, keyLength: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: TJSBitArray; iterations, keyLength: integer): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';

function pbkdf2(password: string; salt: string; iterations, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: string; salt: TJSBitArray; iterations, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: string; iterations, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';
function pbkdf2(password: TJSBitArray; salt: TJSBitArray; iterations, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.pbkdf2';

function scrypt(password: string; salt: string): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: string; salt: TJSBitArray): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: TJSBitArray; salt: string): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: TJSBitArray; salt: TJSBitArray): TJSBitArray; overload; external name 'sjcl.misc.scrypt';

function scrypt(password: string; salt: string; N, r, p, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: string; salt: TJSBitArray; N, r, p, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: TJSBitArray; salt: string; N, r, p, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.scrypt';
function scrypt(password: TJSBitArray; salt: TJSBitArray; N, r, p, keyLength: integer; Prff: JSValue): TJSBitArray; overload; external name 'sjcl.misc.scrypt';

// AES
type
  TJSCipher = class external name 'TJSCipher'
    function encrypt(data: array of Double): TDoubleDynArray;
    function decrypt(data: array of Double): TDoubleDynArray;
  end;

  JAES = class external name 'sjcl.cipher.aes' (TJSCipher)
    // The key is an array of 4, 6 or 8 32-bit words.
    constructor New(key: JSValue); //external name 'aes';
    //class procedure create(key: JSValue); external name 'aes';
  end;

// Cipher
  type
  JSCipherParams = record
    v: Double; external name 'v'; // nullable
    iter: Double; external name 'iter'; // nullable (iterations)
    ks: Double; external name 'ks'; // nullable (key size, in bits)
    ts: Double; external name 'ts'; // nullable (tag size, in bits)
    mode: String; external name 'mode'; // nullable
    adata: String; external name 'adata'; // nullable (authenticated data)
    cipher: String; external name 'cipher'; // nullable
    raw: integer; external name 'raw'; // nullable
    salt: string; external name 'salt'; //  TJSBitArray
  end;

  TJSCipherParams = class (TObject)
  public
    v: Double; external name 'v'; // nullable
    iter: Double; external name 'iter'; // nullable (iterations)
    ks: Double; external name 'ks'; // nullable (key size, in bits)
    ts: Double; external name 'ts'; // nullable (tag size, in bits)
    mode: String; external name 'mode'; // nullable
    adata: String; external name 'adata'; // nullable (authenticated data)
    cipher: String; external name 'cipher';// nullable
    raw: integer; external name 'raw';// nullable
  end;

  TJSCipherEncryptParams = class (TJSCipherParams)
    salt: TJSBitArray;
    iv: TJSBitArray; // (initialization vector)
  end;

  TJSCipherDecryptParams = class (TJSCipherParams)
    salt: TJSBitArray; // nullable
    iv: TJSBitArray; // nullable
  end;

  TJSCipherEncrypted = class (TJSCipherEncryptParams)
    kemtag: TJSBitArray; // nullable (used in ECC)
    ct: TJSBitArray;
  end;

  TJSCipherDecrypted = class (TJSCipherEncrypted)
    key: TJSBitArray;
  end;

// convenience/json
// NOTES:
// 1. If params.Salt is a string, it's assumed to be a base64 encoding of TJSBitArray.
// 2. If params.IV is a string, it's assumed to be a base64 encoding of TJSBitArray.
// 3. If password is a string, a key will generated with pbkdf2.
function encrypt(password: String; plaintext: String): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: String; plaintext: String; params: TJSCipherEncryptParams): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: String; plaintext: String; params: JSCipherParams): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: String; plaintext: String; params: JSCipherParams; rp: TJSCipherEncrypted): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: TJSBitArray; plaintext: String): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: TJSBitArray; plaintext: String; params: TJSCipherEncryptParams): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';
function encrypt(password: TJSBitArray; plaintext: String; params: TJSCipherEncryptParams; rp: TJSCipherEncrypted): TJSCipherEncrypted; overload; external name 'sjcl.json.encrypt';


function decrypt(password: String; ciphertext: String): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: String; ciphertext: TJSCipherEncrypted): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: String; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: String; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams; rp: TJSCipherDecrypted): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: TJSBitArray; ciphertext: TJSCipherEncrypted): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: TJSBitArray; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: TJSBitArray; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams; rp: TJSCipherDecrypted): String; overload; external name 'sjcl.json.decrypt';
function decrypt(password: TJSBitArray; params: TJSCipherDecryptParams): String; overload; external name 'sjcl.json.decrypt';

// set params.raw := 1 for this version of decrypt
function decryptRaw(password: String; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams): TJSBitArray; overload; external name 'sjcl.json.decrypt';
function decryptRaw(password: String; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams; rp: TJSCipherDecrypted): TJSBitArray; overload; external name 'sjcl.json.decrypt';
function decryptRaw(password: TJSBitArray; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams): TJSBitArray; overload; external name 'sjcl.json.decrypt';
function decryptRaw(password: TJSBitArray; ciphertext: TJSCipherEncrypted; params: TJSCipherDecryptParams; rp: TJSCipherDecrypted): TJSBitArray; overload; external name 'sjcl.json.decrypt';

function encode(obj: TJSObject): String; external name 'sjcl.json.encode';
function decode(obj: String): TJSObject; external name 'sjcl.json.decode';

//mode
type
  TJSMode = class external name 'TJSMode'
    //class var
    name: String; external name 'name';
  end;

  // GCM mode defaults:
  // aData (authenticated data) = []
  // tlen (desired tag length, in bits) = 128
  TJSModeGCM = class external name 'sjcl.mode.gcm' (TJSMode)
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
  end;

  TJSModeCBC = class external name 'sjcl.mode.cbc' (TJSMode)
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
  end;

  TJSModeCCM = class external name 'sjcl.mode.ccm' (TJSMode)
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
  end;

  TJSModeCCMArrayBuffer = class external name 'sjcl.arrayBuffer.ccm' (TJSMode)
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
  end;

  TJSModeOCB2 = class external name 'sjcl.mode.ocb2' (TJSMode)
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double; premac: Boolean): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray; tlen: Double; premac: Boolean): TJSBitArray; overload;
    class function pmac(prf: TJSCipher; adata: TJSBitArray): TDoubleDynArray;
  end;

  TJSModeCTR = class external name 'sjcl.mode.ctr' (TJSMode)
  private
    FName: String; external name 'name';
  public
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function encrypt(prf: TJSCipher; plaintext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray): TJSBitArray; overload;
    class function decrypt(prf: TJSCipher; ciphertext: TJSBitArray; iv: TJSBitArray; adata: TJSBitArray): TJSBitArray; overload;
    property name: String read FName write FName;
  end;

  //function parse(aJSON: TJSCipherEncrypted): TJSCipherEncrypted; external name 'JSON.parse';
  //function stringify(aOBJ: TJSCipherEncrypted): String; external name 'JSON.stringify';

implementation

end.
