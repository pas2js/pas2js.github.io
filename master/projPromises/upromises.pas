unit uPromises;

{$MODE objfpc}
{$MODESWITCH externalclass}

interface

  uses JS, Web;

type
  TJSValueDynArray = array of JSValue;
  JDeferred = class;
  TJPromiseCallback = procedure(Value: JSValue);
  TJDeferredObject_fn = function(d: TJPromiseCallback): JSValue;
  TJDeferredObject = procedure(resolve: TJPromiseCallback; reject: TJPromiseCallback);
  TJPromiseCallback_fn = function(Value: JSValue): JSValue;
  TJDeferredEventHandler = function(event: JSValue): JSValue;

  JPromise = class external name 'Promise'
    constructor new(fn: TJDeferredObject_fn { = nil}); overload;
    constructor new(resolve: TJDeferredObject_fn; reject: TJDeferredObject_fn); overload;
    constructor new(fn: TJDeferredObject); overload;
    function always(alwaysCallbacks: TJSValueDynArray): JPromise;
    function done(doneCallbacks: TJSValueDynArray): JPromise; overload;
    function done(doneCallbacks: JSValue): JPromise; overload;
    function fail(failCallbacks: TJSValueDynArray): JPromise;
    function progress(progressCallbacks: TJSValueDynArray): JPromise;
    function state(): string;
    function &then(doneCallbacks: JSValue; failCallbacks: JSValue{ = undefined};
      progressCallbacks: JSValue { = undefined}): JPromise; external name 'then';
    function &then(onFulfilled: TJPromiseCallback_fn = nil): JPromise; overload;
      external name 'then';
    function &then(onFulfilled: TJPromiseCallback_fn; onRejected:
      TJPromiseCallback_fn): JPromise; overload; external name 'then';
    function &then(onFulfilled: TJPromiseCallback; onRejected:
      TJPromiseCallback): JPromise; overload; external name 'then';
    function catch(rejecTJPromiseCallback: JSValue = nil): JPromise; overload;
    function catch(rejecTJPromiseCallback: TJPromiseCallback_fn): JPromise; overload;
    class function promise(target: JSValue): JPromise;
  end;

type
  JDeferred = class external name 'Promise'(JPromise)
    function notify(args: TJSValueDynArray): JDeferred;
    function notifyWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function reject(args: TJSValueDynArray): JDeferred; overload;
    function reject(args: JSValue): JDeferred; overload;
    function reject(args: TJDeferredEventHandler): JDeferred; overload;
    function rejectWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function resolve(args: TJSValueDynArray): JDeferred; overload;
    function resolve(value: JSValue = nil): JPromise; overload;
    function resolveWith(context: JSValue; args: TJSValueDynArray): JDeferred;
    function all(iterable: TJSValueDynArray): JPromise; overload;
    function all(iterable: TJSArray): JPromise; overload;
    function race(iterable: TJSValueDynArray): JPromise;
  end;

  { global external functions }
var Promise: JDeferred; external name 'Promise';// property;
function Error(message: JSValue): JSValue; external name 'Error';

implementation

end.

