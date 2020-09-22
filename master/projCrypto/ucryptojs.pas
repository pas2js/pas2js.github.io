unit uCryptoJS;
{ ╔════════════════════════════════════════════════════════════════════════╗
  ║ From https://cdnjs.com/libraries/crypto-js                             ║
  ║ https://cdnjs.cloudflare.com/ajax/libs/crypto-js/3.1.2/rollups/aes.js  ║
  ╚════════════════════════════════════════════════════════════════════════╝ }
{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, JS, Web;

{ fowards declarations }
type
  JC = JSValue;
  JLibStatic = class;
  JEncStatic = class;
  JKdfStatic = class;
  JFormatStatic = class;
  JAlgoStatic = class;
  JModeStatic = class;
  JPadStatic = class;
  JX64Static = class;
  JICipherHelper = class;
  JHasherHelper = class;
  JCipherHelper = class;
  JIHasherHmacHelper = class;
  JIHasherHelper = class;
  JIEvpKDFHelper = class;
  JIEncoder = class;
  JHasher = class;
  JCipher = class;
  JIBlockCipherEncryptor = class;
  JIBlockCipherDecryptor = class;
  JIBlockCipherModeImpl = class;
  JIPaddingImpl = class;
  JWordArray = class;
  JIFormatter = class;
  JIKdfImpl = class;
  JCipherParamsData = class;
  JAES = class;
  JDES = class;
  JTripleDES = class;
  JRabbitLegacy = class;
  JRabbit = class;
  JRC4 = class;
  JMD5 = class;
  JRIPEMD160 = class;
  JSHA1 = class;
  JSHA256 = class;
  JSHA224 = class;
  JSHA384 = class;
  JSHA512 = class;
  JSHA3 = class;
  JHMAC = class;
  JEvpKDF = class;
  JPBKDF2 = class;
  JRC4Drop = class;
  JIEvpKDFCfg = class;
  JCBC = class;
  JCFB = class;
  JCTR = class;
  JCTRGladman = class;
  JECB = class;
  JOFB = class;
  JPkcs7 = class;
  JAnsiX923 = class;
  JIso10126 = class;
  JIso97971 = class;
  JZeroPadding = class;
  JNoPadding = class;
  JIEncoderDecoder = class;
  JWord = class;

type
  JCryptoJS = class external name 'CryptoJS'
    lib: JLibStatic;
    enc: JEncStatic;
    kdf: JKdfStatic;
    format: JFormatStatic;
    algo: JAlgoStatic;
    mode: JModeStatic;
    pad: JPadStatic;
    x64: JX64Static;
    AES: JICipherHelper;
    DES:  JICipherHelper;
    TripleDES: JICipherHelper;
    RabbitLegacy: JCipherHelper;
    Rabbit: JCipherHelper;
    RC4: JCipherHelper;
    RC4Drop: JICipherHelper{<TJSObject>};
    MD5: JHasherHelper;
    HmacMD5: JIHasherHmacHelper;
    RIPEMD160: JHasherHelper;
    HmacRIPEMD160: JIHasherHmacHelper;
    SHA1: JHasherHelper;
    HmacSHA1: JIHasherHmacHelper;
    SHA256: JHasherHelper;
    HmacSHA256: JIHasherHmacHelper;
    SHA224: JHasherHelper;
    HmacSHA224: JIHasherHmacHelper;
    SHA512: JHasherHelper;
    HmacSHA512: JIHasherHmacHelper;
    SHA384: JHasherHelper;
    HmacSHA384: JIHasherHmacHelper;
    SHA3: JIHasherHelper;
    HmacSHA3: JIHasherHmacHelper;
    EvpKDF: JIEvpKDFHelper;
    PBKDF2: JIEvpKDFHelper;
  end;

{ ╔═════════════════════════════╗
  ║ lib                         ║
  ╚═════════════════════════════╝ }
type
  JBase = class external name 'JBase'
    function extend(overrides: TJSObject): TJSObject;
    procedure init(args: TJSValueDynArray);
    function create(args: TJSValueDynArray): JBase;
    procedure mixIn(properties: TJSObject);
    function clone: JBase;
  end;

  JWordArray = class external name 'JWordArray' (JBase)
    words: TDoubleDynArray;
    sigBytes: Double;
    procedure init; overload;
    procedure init(words: array of Double); overload;
    procedure init(words: array of Double; sigBytes: Double); overload;
    function create: JWordArray; overload;
    function create(words: array of Double): JWordArray; overload;
    function create(words: array of Double; sigBytes: Double): JWordArray; overload;
    procedure init(typedArray: TJSArrayBuffer); overload;
    procedure init(typedArray: TJSInt8Array); overload;
    function create(typedArray: TJSArrayBuffer): JWordArray; overload;
    function create(typedArray: TJSInt8Array): JWordArray; overload;
    function toString: String; overload;
    function toString(encoder: JIEncoderDecoder): String; overload;
    function concat(wordArray: JWordArray): JWordArray;
    procedure clamp;
    function clone: JWordArray;
    function random(nBytes: Double): JWordArray;
  end;

(*
  JWordArray = class external name 'JWordArray' (JBase)
    words: array of JWord;
    sigBytes: Double;
    procedure init; overload;
    procedure init(words: array of JWord); overload;
    procedure init(words: array of JWord; sigBytes: Double); overload;
    function create: JWordArray; overload;
    function create(words: array of JWord): JWordArray; overload;
    function create(words: array of JWord; sigBytes: Double): JWordArray; overload;
    function toX32: JWordArray;
    function clone: JWordArray;
  end;
*)

  JBufferedBlockAlgorithm = class external name 'JBufferedBlockAlgorithm' (JBase)
    procedure reset;
    function clone: JBufferedBlockAlgorithm;
  end;

  JIHasher = class external name 'JIHasher' (JBufferedBlockAlgorithm)
    cfg: JC;
    procedure init; overload;
    procedure init(cfg: JC); overload;
    function create: JIHasher; overload;
    function create(cfg: JC): JIHasher; overload;
    function update(messageUpdate: String):  JHasher; overload;
    function update(messageUpdate: JWordArray): JHasher; overload;
    function finalize: JWordArray; overload;
    function finalize(messageUpdate: String): JWordArray; overload;
    function finalize(messageUpdate: JWordArray): JWordArray; overload;
    blockSize: Double;
    function _createHelper(hasher: JHasher): JIHasherHelper;
    function _createHmacHelper(hasher: JHasher): JIHasherHmacHelper;
    function clone: JIHasher;
  end;

  JHasher = class external name 'JHasher' (JIHasher {TJSObject} )
  end;

  JIHasherHelper = class external name 'JIHasherHelper'
    //function(message: String; cfg: JC): JWordArray;;
    //function(message: JWordArray; cfg: JC): JWordArray;;
  end;

  JHasherHelper = class external name 'JHasherHelper' (JIHasherHelper {TJSObject} )
  end;

  JIHasherHmacHelper = class external name 'JIHasherHmacHelper'
    //function(message: String; key: String): JWordArray;;
    //function(message: String; key: JWordArray): JWordArray;;
    //function(message: JWordArray; key: String): JWordArray;;
    //function(message: JWordArray; key: JWordArray): JWordArray;;
  end;

  JICipher = class external name 'JICipher' (JBufferedBlockAlgorithm)
    cfg: JC;
    function createEncryptor(key: JWordArray): JICipher; overload;
    function createEncryptor(key: JWordArray; cfg: JC): JICipher; overload;
    function createDecryptor(key: JWordArray): JICipher; overload;
    function createDecryptor(key: JWordArray; cfg: JC): JICipher; overload;
    function create: JICipher; overload;
    function create(xformMode: Double): JICipher; overload;
    function create(xformMode: Double; key: JWordArray): JICipher; overload;
    function create(xformMode: Double; key: JWordArray; cfg: JC): JICipher; overload;
    procedure init; overload;
    procedure init(xformMode: Double); overload;
    procedure init(xformMode: Double; key: JWordArray); overload;
    procedure init(xformMode: Double; key: JWordArray; cfg: JC); overload;
    function process(dataUpdate: String): JWordArray; overload;
    function process(dataUpdate: JWordArray): JWordArray; overload;
    function finalize: JWordArray; overload;
    function finalize(dataUpdate: String): JWordArray; overload;
    function finalize(dataUpdate: JWordArray): JWordArray; overload;
    keySize: Double;
    ivSize: Double;
    function _createHelper(cipher: JCipher): JICipherHelper;
    function clone: JICipher;
  end;

  JCipher = class external name 'JCipher' (JICipher {TJSObject} )
  end;

  JIStreamCipher = class external name 'JIStreamCipher' (JICipher)
    drop: Double; // nullable
    function createEncryptor(key: JWordArray): JIStreamCipher; overload;
    function createEncryptor(key: JWordArray; cfg: JC): JIStreamCipher; overload;
    function createDecryptor(key: JWordArray): JIStreamCipher; overload;
    function createDecryptor(key: JWordArray; cfg: JC): JIStreamCipher; overload;
    function create: JIStreamCipher; overload;
    function create(xformMode: Double): JIStreamCipher; overload;
    function create(xformMode: Double; key: JWordArray): JIStreamCipher; overload;
    function create(xformMode: Double; key: JWordArray; cfg: JC): JIStreamCipher; overload;
    blockSize: Double;
  end;

  JStreamCipher = class external name 'JStreamCipher' (JIStreamCipher {TJSObject} )
  end;

  JBlockCipherMode = class external name 'JBlockCipherMode' (JBase)
    function createEncryptor(cipher: JCipher; iv: array of Double): JIBlockCipherEncryptor;
    function createDecryptor(cipher: JCipher; iv: array of Double): JIBlockCipherDecryptor;
    procedure init; overload;
    procedure init(cipher: JCipher); overload;
    procedure init(cipher: JCipher; iv: array of Double); overload;
    function create: JBlockCipherMode; overload;
    function create(cipher: JCipher): JBlockCipherMode; overload;
    function create(cipher: JCipher; iv: array of Double): JBlockCipherMode; overload;
  end;

  JBlockCipher = class external name 'JBlockCipher' (JIStreamCipher)
  end;

  JIBlockCipherCfg = class external name 'JIBlockCipherCfg'
    iv: JWordArray; // nullable
    mode: JIBlockCipherModeImpl; // nullable
    padding: JIPaddingImpl; // nullable
  end;

  JCipherParamsData = class external name 'JCipherParamsData'
    ciphertext: JWordArray; // nullable
    key: JWordArray; // nullable
    iv: JWordArray; // nullable
    salt: JWordArray; // nullable
    algorithm: JCipher; // nullable
    mode: JIBlockCipherModeImpl; // nullable
    padding: JIPaddingImpl; // nullable
    blockSize: Double; // nullable
    formatter: JIFormatter; // nullable
  end;

  JCipherParams = class external name 'JCipherParams' (JBase)
    ciphertext: JWordArray; // nullable
    key: JWordArray; // nullable
    iv: JWordArray; // nullable
    salt: JWordArray; // nullable
    algorithm: JCipher; // nullable
    mode: JIBlockCipherModeImpl; // nullable
    padding: JIPaddingImpl; // nullable
    blockSize: Double; // nullable
    formatter: JIFormatter; // nullable
    procedure init; overload;
    procedure init(cipherParams: JCipherParamsData); overload;
    function create: JCipherParams; overload;
    function create(cipherParams: JCipherParamsData): JCipherParams; overload;
    function toString: String; overload;
    function toString(formatter: JIFormatter): String; overload;
  end;

  JISerializableCipher = class external name 'JISerializableCipher' (JBase)
    cfg: JC;
    function encrypt(cipher: JCipher; message: JWordArray; key: JWordArray): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: JWordArray; key: JWordArray; cfg: JC): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: String; key: JWordArray): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: String; key: JWordArray; cfg: JC): JCipherParams; overload;
    function decrypt(cipher: JCipher; ciphertext: JCipherParamsData; key: JWordArray): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: JCipherParamsData; key: JWordArray; cfg: JC): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: String; key: JWordArray): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: String; key: JWordArray; cfg: JC): JWordArray; overload;
  end;

  JSerializableCipher = class external name 'JSerializableCipher' (JISerializableCipher)
  end;

  JISerializableCipherCfg = class external name 'JISerializableCipherCfg'
    format: JIFormatter; // nullable
    iv: JWordArray; // nullable
    mode: JIBlockCipherModeImpl; // nullable
    padding: JIPaddingImpl; // nullable
  end;

  JIPasswordBasedCipher = class external name 'JIPasswordBasedCipher' (JBase)
    cfg: JC;
    function encrypt(cipher: JCipher; message: JWordArray; password: String): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: JWordArray; password: String; cfg: JC): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: String; password: String): JCipherParams; overload;
    function encrypt(cipher: JCipher; message: String; password: String; cfg: JC): JCipherParams; overload;
    function decrypt(cipher: JCipher; ciphertext: JCipherParamsData; password: String): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: JCipherParamsData; password: String; cfg: JC): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: String; password: String): JWordArray; overload;
    function decrypt(cipher: JCipher; ciphertext: String; password: String; cfg: JC): JWordArray; overload;
  end;

  JPasswordBasedCipher = class external name 'JPasswordBasedCipher'(JIPasswordBasedCipher)
  end;

  JIPasswordBasedCipherCfg = class external name 'JIPasswordBasedCipherCfg' (JISerializableCipherCfg)
    kdf: JIKdfImpl; // nullable
    //mode: JIBlockCipherModeImpl; // nullable
    //padding: JIPaddingImpl; // nullable
  end;

  JICipherHelper = class external name 'JICipherHelper'
    function encrypt(message: String; password: String): JCipherParams; overload;
    function encrypt(message: String; password: String; cfg: JC): JCipherParams; overload;
    function encrypt(message: String; key: JWordArray): JCipherParams; overload;
    function encrypt(message: String; key: JWordArray; cfg: JC): JCipherParams; overload;
    function encrypt(message: JWordArray; password: String): JCipherParams; overload;
    function encrypt(message: JWordArray; password: String; cfg: JC): JCipherParams; overload;
    function encrypt(message: JWordArray; key: JWordArray): JCipherParams; overload;
    function encrypt(message: JWordArray; key: JWordArray; cfg: JC): JCipherParams; overload;
    function decrypt(ciphertext: JSValue; password: String): JWordArray; overload;
    function decrypt(ciphertext: JSValue; password: String; cfg: JC): JWordArray; overload;
    function decrypt(ciphertext: JSValue; key: JWordArray): JWordArray; overload;
    function decrypt(ciphertext: JSValue; key: JWordArray; cfg: JC): JWordArray; overload;
    function decrypt(ciphertext: JCipherParamsData; password: String): JWordArray; overload;
    function decrypt(ciphertext: JCipherParamsData; password: String; cfg: JC): JWordArray; overload;
    function decrypt(ciphertext: JCipherParamsData; key: JWordArray): JWordArray; overload;
    function decrypt(ciphertext: JCipherParamsData; key: JWordArray; cfg: JC): JWordArray; overload;
  end;

  JCipherHelper = class external name 'JCipherHelper' (JICipherHelper {TJSObject} )
  end;

  JLibStatic = class external name 'JLibStatic'
    Base: JBase;
    WordArray: JWordArray;
    CipherParams: JCipherParams;
    SerializableCipher: JSerializableCipher;
    PasswordBasedCipher: JPasswordBasedCipher;
  end;


{ ╔═════════════════════════════╗
  ║ enc                         ║
  ╚═════════════════════════════╝ }
type
  JIEncoder = class external name 'JIEncoder'
    function stringify(wordArray: JWordArray): String;
  end;

  JIDecoder = class external name 'JIDecoder'
    function parse(s: String): JWordArray;
  end;

type
  JIEncoderDecoder = class external name 'JIEncoderDecoder'
    function stringify(wordArray: JWordArray): String;
    function parse(s: String): JWordArray;
  end;

  JICoder = class external name 'JICoder' (JIEncoderDecoder)
  end;

  JEncStatic = class external name 'JEncStatic'
    Hex: JICoder;
    Latin1: JICoder;
    Utf8: JICoder;
    Base64: JICoder;
    Utf16: JICoder;
    Utf16BE: JICoder;
    Utf16LE: JICoder;
  end;


{ ╔═════════════════════════════╗
  ║ kdf                         ║
  ╚═════════════════════════════╝ }
type
  JKdfStatic = class external name 'JKdfStatic'
    OpenSSL: JIKdfImpl;
  end;

  JIKdfImpl = class external name 'JIKdfImpl'
    function execute(password: String; keySize: Double; ivSize: Double): JCipherParams; overload;
    function execute(password: String; keySize: Double; ivSize: Double; salt: String): JCipherParams; overload;
    function execute(password: String; keySize: Double; ivSize: Double; salt: JWordArray): JCipherParams; overload;
  end;

{ ╔═════════════════════════════╗
  ║ format                      ║
  ╚═════════════════════════════╝ }
type
  JFormatStatic = class external name 'JFormatStatic'
    OpenSSL: JIFormatter;
    Hex: JIFormatter;
  end;

  JIFormatter = class external name 'JIFormatter'
    function stringify(cipherParams: JCipherParamsData): String;
    function parse(s: String): JCipherParams;
  end;


{ ╔═════════════════════════════╗
  ║ algo                        ║
  ╚═════════════════════════════╝ }
type
  JAlgoStatic = class external name 'JAlgoStatic'
    AES: JAES;
    DES: JDES;
    TripleDES: JTripleDES;
    RabbitLegacy: JRabbitLegacy;
    Rabbit: JRabbit;
    RC4: JRC4;
    MD5: JMD5;
    RIPEMD160: JRIPEMD160;
    SHA1: JSHA1;
    SHA256: JSHA256;
    SHA224: JSHA224;
    SHA384: JSHA384;
    SHA512: JSHA512;
    SHA3: JSHA3;
    HMAC: JHMAC;
    EvpKDF: JEvpKDF;
    PBKDF2: JPBKDF2;
    RC4Drop: JRC4Drop;
  end;

  JIBlockCipherImpl = class external name 'JIBlockCipherImpl' (JBlockCipher)
    procedure encryptBlock(M: array of Double; offset: Double);
    procedure decryptBlock(M: array of Double; offset: Double);
    function createEncryptor(key: JWordArray): JIBlockCipherImpl; overload;
    function createEncryptor(key: JWordArray; cfg: JCipherParamsData): JIBlockCipherImpl; overload;
    function createDecryptor(key: JWordArray): JIBlockCipherImpl; overload;
    function createDecryptor(key: JWordArray; cfg: JCipherParamsData): JIBlockCipherImpl; overload;
    function create: JIBlockCipherImpl; overload;
    function create(xformMode: Double): JIBlockCipherImpl; overload;
    function create(xformMode: Double; key: JWordArray): JIBlockCipherImpl; overload;
    function create(xformMode: Double; key: JWordArray; cfg: JIBlockCipherCfg): JIBlockCipherImpl; overload;
  end;

  JAES = class external name 'JAES' (JIBlockCipherImpl)
  end;

  JDES = class external name 'JDES' (JIBlockCipherImpl)
  end;

  JTripleDES = class external name 'JTripleDES' (JIBlockCipherImpl)
  end;

  JRabbitLegacy = class external name 'JRabbitLegacy' (JStreamCipher)
  end;

  JRabbit = class external name 'JRabbit' (JStreamCipher)
  end;

  JRC4 = class external name 'JRC4' (JStreamCipher)
  end;

  JMD5 = class external name 'JMD5' (JHasher)
  end;

  JRIPEMD160 = class external name 'JRIPEMD160' (JHasher)
  end;

  JSHA1 = class external name 'JSHA1' (JHasher)
  end;

  JSHA256 = class external name 'JSHA256' (JHasher)
  end;

  JSHA224 = class external name 'JSHA224' (JHasher)
  end;

  JSHA384 = class external name 'JSHA384' (JHasher)
  end;

  JSHA512 = class external name 'JSHA512' (JHasher)
  end;

  JSHA3 = class external name 'JSHA3' (JIHasher)
  end;

  JISHA3Cfg = class external name 'JISHA3Cfg'
    outputLength: Double; // nullable
  end;

  JHMAC = class external name 'JHMAC' (JBase)
    procedure init; overload;
    procedure init(hasher: JHasher); overload;
    procedure init(hasher: JHasher; key: String); overload;
    procedure init(hasher: JHasher; key: JWordArray); overload;
    function create: JHMAC; overload;
    function create(hasher: JHasher): JHMAC; overload;
    function create(hasher: JHasher; key: String): JHMAC; overload;
    function create(hasher: JHasher; key: JWordArray): JHMAC; overload;
    function update(messageUpdate: String): JHMAC; overload;
    function update(messageUpdate: JWordArray): JHMAC; overload;
    function finalize: JWordArray; overload;
    function finalize(messageUpdate: String): JWordArray; overload;
    function finalize(messageUpdate: JWordArray): JWordArray; overload;
  end;

  JEvpKDF = class external name 'JEvpKDF' (JBase)
    cfg: JIEvpKDFCfg;
    procedure init; overload;
    procedure init(cfg: JIEvpKDFCfg); overload;
    function create: JEvpKDF; overload;
    function create(cfg: JIEvpKDFCfg): JEvpKDF; overload;
    function compute(password: String; salt: String): JWordArray; overload;
    function compute(password: String; salt: JWordArray): JWordArray; overload;
    function compute(password: JWordArray; salt: String): JWordArray; overload;
    function compute(password: JWordArray; salt: JWordArray): JWordArray; overload;
  end;

  JIEvpKDFCfg = class external name 'JIEvpKDFCfg'
    keySize: Double; // nullable
    hasher: JHasher; // nullable
    iterations: Double; // nullable
  end;

  JIEvpKDFHelper = class external name 'JIEvpKDFHelper'
    //function(password: String; salt: String; cfg: JIEvpKDFCfg): JWordArray;
    //function(password: String; salt: JWordArray; cfg: JIEvpKDFCfg): JWordArray;
    //function(password: JWordArray; salt: String; cfg: JIEvpKDFCfg): JWordArray;
    //function(password: JWordArray; salt: JWordArray; cfg: JIEvpKDFCfg): JWordArray;
  end;

  JPBKDF2 = class external name 'JPBKDF2' (JEvpKDF)
  end;

  JRC4Drop = class external name 'JRC4Drop' (JRC4)
  end;


  { ╔═════════════════════════════╗
    ║ mode                        ║
    ╚═════════════════════════════╝ }
type
  JModeStatic = class external name 'JModeStatic'
    CBC: JCBC;
    CFB: JCFB;
    CTR: JCTR;
    CTRGladman: JCTRGladman;
    ECB: JECB;
    OFB: JOFB;
  end;

  JIBlockCipherEncryptor = class external name 'JIBlockCipherEncryptor' (JBlockCipherMode)
    procedure processBlock(words: array of Double; offset: Double);
  end;

  JIBlockCipherDecryptor = class external name 'JIBlockCipherDecryptor' (JBlockCipherMode)
    procedure processBlock(words: array of Double; offset: Double);
  end;

  JIBlockCipherModeImpl = class external name 'JIBlockCipherModeImpl' (JBlockCipherMode)
    Encryptor: JIBlockCipherEncryptor;
    Decryptor: JIBlockCipherDecryptor;
  end;

  JCBC = class external name 'JCBC' (JIBlockCipherModeImpl)
  end;

  JCFB = class external name 'JCFB' (JIBlockCipherModeImpl)
  end;

  JCTR = class external name 'JCTR' (JIBlockCipherModeImpl)
  end;

  JCTRGladman = class external name 'JCTRGladman' (JIBlockCipherModeImpl)
  end;

  JECB = class external name 'JECB' (JIBlockCipherModeImpl)
  end;

  JOFB = class external name 'JOFB' (JIBlockCipherModeImpl)
  end;


  { ╔═════════════════════════════╗
    ║ pad                         ║
    ╚═════════════════════════════╝ }
type
  JPadStatic = class external name 'JPadStatic'
    Pkcs7: JPkcs7;
    AnsiX923: JAnsiX923;
    Iso10126: JIso10126;
    Iso97971: JIso97971;
    ZeroPadding: JZeroPadding;
    NoPadding: JNoPadding;
  end;

  JIPaddingImpl = class external name 'JIPaddingImpl'
    procedure pad(data: JWordArray; blockSize: Double);
    procedure unpad(data: JWordArray);
  end;

  JPkcs7 = class external name 'JPkcs7' (JIPaddingImpl)
  end;

  JAnsiX923 = class external name 'JAnsiX923' (JIPaddingImpl)
  end;

  JIso10126 = class external name 'JIso10126' (JIPaddingImpl)
  end;

  JIso97971 = class external name 'JIso97971' (JIPaddingImpl)
  end;

  JZeroPadding = class external name 'JZeroPadding' (JIPaddingImpl)
  end;

  JNoPadding = class external name 'JNoPadding' (JIPaddingImpl)
  end;


//x64

type
  JX64Static = class external name 'JX64Static'
    Word: JWord;
    WordArray: JWordArray;
  end;

  JWord = class external name 'JWord' (JBase)
    &high: Double;
    &low: Double;
    procedure init; overload;
    procedure init(highvar: Double); overload;
    procedure init(highvar: Double; lowvar: Double); overload;
    function create: JWord; overload;
    function create(highvar: Double): JWord; overload;
    function create(highvar: Double; lowvar: Double): JWord; overload;
  end;

//function cryptoJS: JCryptoJS; external name 'CryptoJS';
var
  cryptoJS: JCryptoJS; external name 'CryptoJS';

implementation

end.

