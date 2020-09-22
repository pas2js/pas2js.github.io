unit uSQLite;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, Web, JS, Classes, SysUtils;

type
  TJSOptionsSQLDB = reference to function: TJSObject;

type
  TJSSimpleDatatables = class external name 'simpleDatatables.DataTable'
    constructor New; overload;
    constructor New(target: TJSElement; config: TJSObject); overload;
    constructor New(target: TJSElement; config: TJSOptionsSQLDB); overload;
  end;

var
  CreateObject: TJSObject; external name '{}';

type
  { forward declarations }
  TJSStatement = class;

  TJSValueDynArrays = array of TJSValueDynArray;

  TSQLiteRowValues = Record
    values:  Array of JSValue; external name 'values';  // --> this is trick, obfuscation don't change the field
  End;

  TJSQueryResults = class external name 'QueryResults'
    columns: array of JSValue;  external name 'columns';  // --> this is trick, obfuscation don't change the field
    values: TJSValueDynArrays {Double or String or TJSUint8Array};  external name 'values';  // --> this is trick, obfuscation don't change the field
  end;

  TJSSqliteQueryResults = array of TJSQueryResults;

  TSQLRecordDB = record
    i:integer;
  end;

  TJDB = class external name 'TJDB' (TJSObject)
   date: String;
end;


type
  TVarArray = array of JSValue;
//  TProcColumnNameParams = procedure(columnName: JConfig{string});
  TProcColumnNameParam = reference to procedure(columnName: JSValue{string});
  TProcColumnName = procedure();

  TCustomFunctionA = function(a: integer; b: integer): integer;

  TJSSqliteDatabase = class external name 'SQL.Database'
    constructor New; overload;

    //constructor Create(data: JBuffer);
    constructor New(data: TJSUint8Array); overload;
    //constructor Create(data: array of Double);
    function run(sql: String): TJSSqliteDatabase; overload;
    //function run(sql: String; params: record
      // property Item[key: String]: JSValue {Double or String or TJSUint8Array};
    //end): TJSSqliteDatabase;
    //function run(sql: String; params: array of JSValue {Double or String or TJSUint8Array}): TJSSqliteDatabase;
    function run(sql: String; params: TVarArray): TJSSqliteDatabase; overload;
    function run(sql: String; params: TJSObject{JSValue}): TJSSqliteDatabase; overload;
    //function exec(sql: String): TJSQueryResults; overload;
    function exec(sql: String): TJSSqliteQueryResults; overload;
    procedure each(sql: String; callback: TProcColumnNameParam); overload;
//    procedure each(sql: String; callback: TProcColumnNameParams); overload;
    procedure each(sql: String; callback: TProcColumnNameParam; doneProc: TProcColumnName); overload;
//    procedure each(sql: String; callback: TProcColumnNameParams; doneProc: TProcColumnName); overload;
    procedure each(sql: String; params: JSValue; callback: TProcColumnNameParam; doneProc: TProcColumnName); overload;
    procedure each(sql: String; params: TVarArray; callback: TProcColumnNameParam; doneProc: TProcColumnName); overload;
    //procedure each(sql: String; callback: procedure(obj: record
      // property Item[columnName: String]: JSValue {Double or String or TJSUint8Array};
      //end); done: procedure);

    //procedure each(sql: String; params: record
      // property Item[key: String]: JSValue {Double or String or TJSUint8Array};
      //end; callback: procedure(obj: record
      // property Item[columnName: String]: JSValue {Double or String or TJSUint8Array};
      //end); done: procedure);

    //procedure each(sql: String; params: array of JSValue {Double or String or TJSUint8Array}; callback: procedure(obj: record
      // property Item[columnName: String]: JSValue {Double or String or TJSUint8Array};
      //end); done: procedure);
    function prepare(sql: String): TJSStatement; overload;
    function prepare(sql: String; params: TVarArray): TJSStatement; overload;

    //function prepare(sql: String; params: record
      // property Item[key: String]: JSValue {Double or String or TJSUint8Array};
    //end): TJSStatement;
    //function prepare(sql: String; params: array of JSValue {Double or String or TJSUint8Array}): TJSStatement;
    function &export: TJSUint8Array;
    procedure close;
    function getRowsModified: Double;
    procedure create_function(name: String; func: TCustomFunctionA {JFunction}); overload;
  end;

  TJSStatement = class external name 'Statement'
    function bind: Boolean; overload;
    function bind(key: JSValue): Boolean; overload;
    //function bind(values: record
      // property Item[key: String]: JSValue {Double or String or TJSUint8Array};
    //end): Boolean;
    //function bind(values: array of JSValue {Double or String or TJSUint8Array}): Boolean;
    function step: Boolean;
    function get: TJSValueDynArray {array of JSValue} {Double or String or TJSUint8Array}; overload;
    function get(params: JSValue): JSValue; overload;
    //function get(params: record
      // property Item[key: String]: JSValue {Double or String or TJSUint8Array};
    //end): array of JSValue {Double or String or TJSUint8Array};
    function get(params: array of JSValue {Double or String or TJSUint8Array}): TJSValueDynArray {array of JSValue} {Double or String or TJSUint8Array};  overload;
    //function getColumnNames: array of String;
    function getAsObject(): JSValue; overload;
    function getAsObject(columnName: JSValue): JSValue; overload;
    //function getAsObject: record
      // property Item[columnName: String]: JSValue {Float or String or TJSUint8Array};
    //end;

    //function getAsObject: record
      // property Item[columnName: String]: JSValue {Float or String or TJSUint8Array};
    //end;

    //function getAsObject(params: record
      // property Item[key: String]: JSValue {Float or String or TJSUint8Array};
    //end): record
      // property Item[columnName: String]: JSValue {Float or String or TJSUint8Array};
    //end;

    //function getAsObject(params: array of JSValue {Float or String or TJSUint8Array}): record
      // property Item[columnName: String]: JSValue {Float or String or TJSUint8Array};
    //end;

    procedure run;
    //procedure run(values: record
      // property Item[key: String]: JSValue {Float or String or TJSUint8Array};
    //end);
    //procedure run(values: array of JSValue {Float or String or TJSUint8Array});
    procedure reset;
    procedure freemem;
    function free: Boolean;
  end;

implementation

(*
{ ╔════════════════════════════════════════════════╗
  ║ External Global Functions                      ║
  ╚════════════════════════════════════════════════╝ }

*)
end.



