unit i18next;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web;

(*
type
Float = Double;
FallbackLng = JSValue {String or array of String or JFallbackLngObjList};
JFormatFunction = reference to function(value: JSValue; format: String; lng: String): String;
//StringMap =     // property Item[key: String]: JSValue;
//end;
//Callback = reference to procedure(error: JSValue; t: JTFunction);
//Newable =     CreateJT.New;
//end;

type
  JFallbackLngObjList = class external name 'FallbackLngObjList' (TJSObject)
    // property Item[language: String]: array of String;
  end;

  JInterpolationOptions = class external name 'InterpolationOptions'
    format: JFormatFunction; // nullable
    formatSeparator: String; // nullable
    function escape(str: String): String;
    escapeValue: Boolean; // nullable
    useRawValueToEscape: Boolean; // nullable
    prefix: String; // nullable
    suffix: String; // nullable
    prefixEscaped: String; // nullable
    suffixEscaped: String; // nullable
    unescapeSuffix: String; // nullable
    unescapePrefix: String; // nullable
    nestingPrefix: String; // nullable
    nestingSuffix: String; // nullable
    nestingPrefixEscaped: String; // nullable
    nestingSuffixEscaped: String; // nullable
//    defaultVariables: record
      // property Item[index: String]: JSValue;
//    end; // nullable
    maxReplaces: Float; // nullable
  end;

  JReactOptions = class external
    wait: Boolean; // nullable
    nsMode: JSValue {Jdefault or Jfallback}; // nullable
    defaultTransParent: String; // nullable
    bindI18n: String; // nullable
    bindStore: String; // nullable
    transEmptyNodeValue: String; // nullable
    useSuspense: Boolean; // nullable
    transSupportBasicHtmlNodes: Boolean; // nullable
    transKeepBasicHtmlNodesFor: array of String; // nullable
  end;

  JInitOptions = class external
    debug: Boolean; // nullable
    resources: JResource; // nullable
    partialBundledLanguages: Boolean; // nullable
    lng: String; // nullable
    fallbackLng: JFallbackLng; // nullable
    whitelist: array of String; // nullable
    nonExplicitWhitelist: Boolean; // nullable
    load: JSValue {Jall or JcurrentOnly or JlanguageOnly}; // nullable
    preload: array of String; // nullable
    lowerCaseLng: Boolean; // nullable
    ns: JSValue {String or array of String}; // nullable
    defaultNS: String; // nullable
    fallbackNS: JSValue {String or array of String}; // nullable
    saveMissing: Boolean; // nullable
    updateMissing: Boolean; // nullable
    saveMissingTo: JSValue {Jcurrent or Jall or Jfallback}; // nullable
    missingKeyHandler: (lngs: array of String; ns: String; key: String; fallbackValue: String); // nullable
    function parseMissingKeyHandler(key: String): JSValue;
    appendNamespaceToMissingKey: Boolean; // nullable
    missingInterpolationHandler: (text: String; value: JSValue; options: JInitOptions): JSValue; // nullable
    simplifyPluralSuffix: Boolean; // nullable
    postProcess: JSValue {String or array of String}; // nullable
    returnNull: Boolean; // nullable
    returnEmptyString: Boolean; // nullable
    returnObjects: Boolean; // nullable
    procedure returnedObjectHandler(key: String; value: String; options: JSValue);
    joinArrays: String; // nullable
    function overloadTranslationOptionHandler(args: array of String): JTOptions;
    interpolation: JInterpolationOptions; // nullable
    detection: Jobject; // nullable
    backend: Jobject; // nullable
    cache: Jobject; // nullable
    i18nFormat: Jobject; // nullable
    react: JReactOptions; // nullable
    initImmediate: Boolean; // nullable
    keySeparator: String; // nullable
    nsSeparator: String; // nullable
    pluralSeparator: String; // nullable
    contextSeparator: String; // nullable
    appendNamespaceToCIMode: Boolean; // nullable
    compatibilityJSON: JSValue {Jv1 or Jv2 or Jv3}; // nullable
    editor: record
      enabled: Boolean; // nullable
      autoOpen: Boolean; // nullable
      enableByQS: String; // nullable
      toggleKeyModifier: JSValue {JctrlKey or JmetaKey or JaltKey or JshiftKey}; // nullable
      toggleKeyCode: Float; // nullable
      lngOverrideQS: String; // nullable
      lngOverride: String; // nullable
      mode: JSValue {Jiframe or Jwindow}; // nullable
      iframeContainerStyle: String; // nullable
      iframeStyle: String; // nullable
      bodyStyle: String; // nullable
      onEditorSaved: (lng: Jnil; ns: JSValue {String or array of String}); // nullable
    end; // nullable
    locizeLastUsed: record
      projectId: String;
      apiKey: String; // nullable
      referenceLng: String; // nullable
      version: String; // nullable
      debounceSubmit: Float; // nullable
      allowedHosts: array of String; // nullable
    end; // nullable
  end;

  JTOptionsBase = class external
    defaultValue: JSValue; // nullable
    count: Float; // nullable
    context: JSValue; // nullable
    replace: JSValue; // nullable
    lng: String; // nullable
    lngs: array of String; // nullable
    fallbackLng: JFallbackLng; // nullable
    ns: JSValue {String or array of String}; // nullable
    keySeparator: String; // nullable
    nsSeparator: String; // nullable
    returnObjects: Boolean; // nullable
    joinArrays: String; // nullable
    postProcess: JSValue {String or array of String}; // nullable
    interpolation: JInterpolationOptions; // nullable
  end;

  JWithT = class external
    t: JTFunction;
  end;

  JResource = class external
    // property Item[language: String]: JResourceLanguage;
  end;

  JResourceLanguage = class external
    // property Item[namespace: String]: JResourceKey;
  end;

  JInterpolator = class external
    function init(options: JInterpolationOptions; reset: Boolean): Jundefined;
    function reset: Jundefined;
    function resetRegExp: Jundefined;
    function interpolate(str: String; data: Jobject; lng: String; options: JInterpolationOptions): String;
    function nest(str: String; fc: function(args: array of JSValue): JSValue; options: JInterpolationOptions): String;
  end;

  JServices = class external
    backendConnector: JSValue;
    i18nFormat: JSValue;
    interpolator: JInterpolator;
    languageDetector: JSValue;
    languageUtils: JSValue;
    logger: JSValue;
    pluralResolver: JSValue;
    resourceStore: JResource;
  end;

  JModule = class external
    &type: JSValue {Jbackend or Jlogger or JlanguageDetector or JpostProcessor or Ji18nFormat or J3rdParty};
  end;

  JLanguageDetectorModule = class external(JModule)
    &type: JlanguageDetector;
    procedure init(services: JServices; detectorOptions: Jobject; i18nextOptions: JInitOptions);
    function detect: JSValue {String or Jundefined};
    procedure cacheUserLanguage(lng: String);
  end;

  JPostProcessorModule = class external(JModule)
    name: String;
    &type: JpostProcessor;
    function process(value: String; key: String; options: JTOptions; translator: JSValue): String;
  end;

  JLoggerModule = class external(JModule)
    &type: Jlogger;
    procedure log(args: array of JSValue);
    procedure warn(args: array of JSValue);
    procedure error(args: array of JSValue);
  end;

  JI18nFormatModule = class external(JModule)
    &type: Ji18nFormat;
  end;

  JThirdPartyModule = class external(JModule)
    &type: J3rdParty;
    procedure init(i18next: Ji18n);
  end;

  JModules = class external
    backend: JBackendModule; // nullable
    logger: JLoggerModule; // nullable
    languageDetector: JSValue {JLanguageDetectorModule or JLanguageDetectorAsyncModule}; // nullable
    i18nFormat: JI18nFormatModule; // nullable
    &external: array of JThirdPartyModule;
  end;

  Ji18n = class external
    t: JTFunction;
    function init: JPromise{<JTFunction>}; overload;
    function init(callback: JCallback): JPromise{<JTFunction>}; overload;
    function init(options: JInitOptions): JPromise{<JTFunction>}; overload;
    function init(options: JInitOptions; callback: JCallback): JPromise{<JTFunction>}; overload;
    procedure loadResources; overload;
    procedure loadResources(callback: procedure(err: JSValue)); overload;
    function use(module: JSValue {JT or JNewable{<JT>} or array of JThirdPartyModule or array of JNewable{<JThirdPartyModule>}}): Ji18n;
    modules: JModules;
    services: JServices;
    exists: JExistsFunction;
    function getFixedT(lng: JSValue {String or array of String}): JTFunction; overload;
    function getFixedT(lng: JSValue {String or array of String}; ns: JSValue {String or array of String}): JTFunction; overload;
    function changeLanguage(lng: String): JPromise{<JTFunction>}; overload;
    function changeLanguage(lng: String; callback: JCallback): JPromise{<JTFunction>}; overload;
    language: String;
    languages: array of String;
    function loadNamespaces(ns: JSValue {String or array of String}): JPromise{<JSValue>}; overload;
    function loadNamespaces(ns: JSValue {String or array of String}; callback: JCallback): JPromise{<JSValue>}; overload;
    function loadLanguages(lngs: JSValue {String or array of String}): JPromise{<JSValue>}; overload;
    function loadLanguages(lngs: JSValue {String or array of String}; callback: JCallback): JPromise{<JSValue>}; overload;
    function reloadResources: JPromise{<JSValue>}; overload;
    function reloadResources(lngs: JSValue {String or array of String}): JPromise{<JSValue>}; overload;
    function reloadResources(lngs: JSValue {String or array of String}; ns: JSValue {String or array of String}): JPromise{<JSValue>}; overload;
    function reloadResources(lngs: JSValue {String or array of String}; ns: JSValue {String or array of String}; callback: procedure): JPromise{<JSValue>}; overload;
    procedure setDefaultNamespace(ns: String);
    function dir: JSValue {Jltr or Jrtl}; overload;
    function dir(lng: String): JSValue {Jltr or Jrtl}; overload;
    format: JFormatFunction;
    function createInstance: Ji18n; overload;
    function createInstance(options: JInitOptions): Ji18n; overload;
    function createInstance(options: JInitOptions; callback: JCallback): Ji18n; overload;
    function cloneInstance: Ji18n; overload;
    function cloneInstance(options: JInitOptions): Ji18n; overload;
    function cloneInstance(options: JInitOptions; callback: JCallback): Ji18n; overload;
    procedure on(event: Jinitialized; callback: procedure(options: JInitOptions));
    procedure on(event: Jloaded; callback: procedure(loaded: Boolean));
    procedure on(event: JfailedLoading; callback: procedure(lng: String; ns: String; msg: String));
    procedure on(event: JmissingKey; callback: procedure(lngs: array of String; namespace: String; key: String; res: String));
    procedure on(event: JSValue {Jadded or Jremoved}; callback: procedure(lng: String; ns: String));
    procedure on(event: JlanguageChanged; callback: procedure(lng: String));
    procedure on(event: String; listener: procedure(args: array of JSValue));
    procedure off(event: String; listener: procedure(args: array of JSValue));
    function getResource(lng: String; ns: String; key: String): JSValue; overload;
    function getResource(lng: String; ns: String; key: String; options: record
      keySeparator: String; // nullable
    end): JSValue; overload;
    procedure addResource(lng: String; ns: String; key: String; value: String); overload;
    procedure addResource(lng: String; ns: String; key: String; value: String; options: record
      keySeparator: String; // nullable
      silent: Boolean; // nullable
    end); overload;
    procedure addResources(lng: String; ns: String; resources: JSValue);
    procedure addResourceBundle(lng: String; ns: String; resources: JSValue); overload;
    procedure addResourceBundle(lng: String; ns: String; resources: JSValue; deep: Boolean); overload;
    procedure addResourceBundle(lng: String; ns: String; resources: JSValue; deep: Boolean; overwrite: Boolean); overload;
    function hasResourceBundle(lng: String; ns: String): Boolean;
    function getResourceBundle(lng: String; ns: String): JSValue;
    procedure removeResourceBundle(lng: String; ns: String);
    options: JInitOptions;
    isInitialized: Boolean;
    procedure emit(eventName: String);
  end;


i18next: Ji18next.i18n;
*)

type
JOptions = class external name 'Object'
  constructor new;
  debug: Boolean; // nullable
//  resources: JResourceStore; // nullable
  lng: String; // nullable
//  fallbackLng: JFallbackLng; // nullable
//  ns: Variant {String or array of String}; // nullable
  defaultNS: String; // nullable
//  fallbackNS: Variant {String or array of String}; // nullable
  whitelist: array of String; // nullable
  lowerCaseLng: Boolean; // nullable
  load: String; // nullable
  preload: array of String; // nullable
  keySeparator: String; // nullable
  nsSeparator: String; // nullable
  pluralSeparator: String; // nullable
  contextSeparator: String; // nullable
  saveMissing: Boolean; // nullable
  saveMissingTo: String; // nullable
  procedure missingKeyHandler(lng: String; ns: String; key: String; fallbackValue: String);
  procedure parseMissingKeyHandler(key: String);
  appendNamespaceToMissingKey: Boolean; // nullable
//  postProcess: Variant {String or array of Variant}; // nullable
  returnNull: Boolean; // nullable
  returnEmptyString: Boolean; // nullable
  returnObjects: Boolean; // nullable
//  procedure returnedObjectHandler(key: String; value: String; options: Variant);
  joinArrays: String; // nullable
//  function overloadTranslationOptionHandler(args: array of Variant): JTranslationOptions;
//  interpolation: JInterpolationOptions; // nullable
//  detection: Variant; // nullable
//  backend: Variant; // nullable
//  cache: Variant; // nullable
resGetPath: String;
end;

JTranslationOptions = record
    defaultValue: String; // nullable
    count: Double; // nullable
    context: JSValue; // nullable
    replace: JSValue; // nullable
    lng: String; // nullable
    lngs: array of String; // nullable
//    fallbackLng: JFallbackLng; // nullable
//    ns: Variant {String or array of String}; // nullable
    keySeparator: String; // nullable
    nsSeparator: String; // nullable
    returnObjects: Boolean; // nullable
    joinArrays: String; // nullable
//    postProcess: Variant {String or array of Variant}; // nullable
//    interpolation: JInterpolationOptions; // nullable
    // property Item[x: String]: Variant;
    //dataset
  end;

type
  TProc18n = reference to procedure(err, t: JSValue);
  JCallback = reference to procedure(error: JSValue; t: TJSFunction);

type
JI18n = class external name 'i18n'
var i18n: TJSNode;
    function init: JI18n; overload;
    function init(options: JOptions): JI18n; overload;
    function init(options: JOptions; callback: JCallback): TJSPromise{<JTFunction>}; overload;
    procedure setLng(language: string; aProc: TProc18n);
    function t(key: String): JSValue {String or Variant or array of Variant}; overload;
    function t(key: String; options: JTranslationOptions): JSValue {String or Variant or array of Variant}; overload;
    function t(options: TJSNode): String {String or Variant or array of Variant}; overload;


(*
    function init(options: JOptions; callback: procedure(err: Variant; t: JTranslationFunction)): JI18n; overload;
    procedure loadResources; overload;
    procedure loadResources(callback: procedure(err: Variant)); overload;
    language: String;
    languages: array of String;
    function use(module: Variant): JI18n;
    procedure changeLanguage(lng: String); overload;
    procedure changeLanguage(lng: String; callback: procedure(err: Variant; t: JTranslationFunction)); overload;
    function getFixedT: JTranslationFunction; overload;
    function getFixedT(lng: String): JTranslationFunction; overload;
    function getFixedT(lng: String; ns: Variant {String or array of String}): JTranslationFunction; overload;
    function t(key: String): Variant {String or Variant or array of Variant}; overload;
    function t(key: String; options: JTranslationOptions): Variant {String or Variant or array of Variant}; overload;
    function exists(key: String): Boolean; overload;
    function exists(key: String; options: JTranslationOptions): Boolean; overload;
    procedure setDefaultNamespace(ns: String);
    procedure loadNamespaces(ns: array of String); overload;
    procedure loadNamespaces(ns: array of String; callback: procedure); overload;
    procedure loadLanguages(lngs: array of String); overload;
    procedure loadLanguages(lngs: array of String; callback: procedure); overload;
    function dir: Variant {Jltr or Jrtl}; overload;
    function dir(lng: String): Variant {Jltr or Jrtl}; overload;
    function createInstance: JI18n; overload;
    function createInstance(options: JOptions): JI18n; overload;
    function createInstance(options: JOptions; callback: procedure(err: Variant; t: JTranslationFunction)): JI18n; overload;
    function cloneInstance: JI18n; overload;
    function cloneInstance(options: JOptions): JI18n; overload;
    function cloneInstance(options: JOptions; callback: procedure(err: Variant; t: JTranslationFunction)): JI18n; overload;
    procedure on(event: String; listener: procedure);
    procedure on(initialized: Jinitialized; listener: procedure(options: JOptions));
    procedure on(loaded: Jloaded; listener: procedure(loaded: Variant));
    procedure on(failedLoading: JfailedLoading; listener: procedure(lng: String; ns: String; msg: String));
    procedure on(missingKey: JmissingKey; listener: procedure(lngs: Variant; namespace: String; key: String; res: Variant));
    procedure on(action: Variant {Jremoved or Jadded}; listener: procedure(lng: String; ns: String));
    procedure on(languageChanged: JlanguageChanged; listener: procedure(lng: String));
    procedure off(event: String; listener: procedure);
    options: JOptions;
    *)
  end;

  var i18n: JI18n; external name 'i18n';


implementation

end.
