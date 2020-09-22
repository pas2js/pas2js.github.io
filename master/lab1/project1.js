var pas = {};

var rtl = {

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist, implcode){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist+' hasimplcode='+rtl.isFunction(implcode));
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');
    if (!(implcode==undefined) && !rtl.isFunction(implcode)) rtl.error('invalid implementation code of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: implcode,
      $impl: null,
      $rtti: Object.create(rtl.tSectionRTTI)
    };
    module.$rtti.$module = module;
    if (implcode) module.$impl = {
      $module: module,
      $rtti: module.$rtti
    };
  },

  exitcode: 0,

  run: function(module_name){
  
    function doRun(){
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if (module_name=='program'){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas.program.$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    }
    
    if (rtl.showUncaughtExceptions) {
      try{
        doRun();
      } catch(re) {
        var errMsg = re.hasOwnProperty('$class') ? re.$class.$classname : '';
	    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
        alert('Uncaught Exception : '+errMsg);
        rtl.exitCode = 216;
      }
    } else {
      doRun();
    }
    return rtl.exitcode;
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    for (var i in useslist){
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  cloneCallback: function(cb){
    return rtl.createCallback(cb.scope,cb.fn);
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a==b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn==b.fn);
    }
  },

  initClass: function(c,parent,name,initfn){
    parent[name] = c;
    c.$classname = name;
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    c.$fullname = parent.$name+'.'+name;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.name+'.'+name;
    };
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$name,{ "class": c, module: parent });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = {};
      c.$create = function(fnname,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
        o.$init();
        try{
          o[fnname].apply(o,args);
          o.AfterConstruction();
        } catch($e){
          o.$destroy;
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        this[fnname]();
        this.$final;
      };
    };
    rtl.initClass(c,parent,name,initfn);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var c = null;
    c = Object.create(ancestor);
    c.$create = function(fnname,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fnname,args);
      } else {
        o = Object.create(this);
      }
      o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
      o.$init();
      try{
        o[fnname].apply(o,args);
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        o.$destroy;
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      this[fnname]();
      this.$final;
    };
    rtl.initClass(c,parent,name,initfn);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = name;
    i.$fullname = module.$name+'.'+name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,new TGuid());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.queryIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      this[id]=intf;
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          //console.log('rtl.intfRefs.free: id='+id+' '+this[id].$name+' $o='+this[id].$o.$classname);
          this[id]._Release();
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    // multi dim: (arr,defaultvalue,dim1,dim2,...)
    if (arr == null) arr = [];
    var p = arguments;
    function setLength(a,argNo){
      var oldlen = a.length;
      var newlen = p[argNo];
      if (oldlen!==newlength){
        a.length = newlength;
        if (argNo === p.length-1){
          if (rtl.isArray(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
          } else if (rtl.isFunction(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=new defaultvalue(); // e.g. record
          } else if (rtl.isObject(defaultvalue)) {
            for (var i=oldlen; i<newlen; i++) a[i]={}; // e.g. set
          } else {
            for (var i=oldlen; i<newlen; i++) a[i]=defaultvalue;
          }
        } else {
          for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
        }
      }
      if (argNo < p.length-1){
        // multi argNo
        for (var i=0; i<newlen; i++) a[i]=setLength(a[i],argNo+1);
      }
      return a;
    }
    return setLength(arr,2);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if (rtl.isFunction(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = new type(src[srcpos]); // clone record
    } else if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=src; // Note: concat(a) does not clone
      } else {
        a=a.concat(src);
      }
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    s.$shared = true;
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    delete r.$shared;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
    };
  },

  floatToStr : function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (ad<1.0e+10) {
		pad='00';
	  } else if (ad<1.0e+100) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=9;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo" };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = {};
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params);
      t.procsig.resulttype = result?result:null;
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      if (rtl.isArray(t.params)) t.params = rtl.newTIParams(t.params);
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",15 /* tkInterface */,rtl.tTypeInfoStruct);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result,
      flags: flags
    };
    return s;
  }
}
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  this.LineEnding = "\n";
  this.sLineBreak = $mod.LineEnding;
  this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
  });
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.DefaultTextLineBreakStyle = $mod.TTextLineBreakStyle.tlbsLF;
  this.Int = function (A) {
    var Result = 0.0;
    Result = Math.trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if (((Index < 1) || (Index > S.get().length)) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set(($mod.Copy(t,1,Index - 1) + Insertion) + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else NI.set($mod.Trunc(x));
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    Result = "";
    for (var $l1 = 1, $end2 = l; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + c;
    };
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SListCapacityError = "List capacity (%s) exceeded.";
  this.SListCountError = "List count (%s) out of bounds.";
  this.SListIndexError = "List index (%s) out of bounds";
  this.SSortedListError = "Operation not allowed on sorted list";
  this.SDuplicateString = "String list does not allow duplicates";
  this.SErrFindNeedsSortedList = "Cannot use find on unsorted list";
  this.SInvalidName = 'Invalid component name: "%s"';
  this.SDuplicateName = 'Duplicate component name: "%s"';
  this.SErrInvalidInteger = 'Invalid integer value: "%s"';
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
  this.TPoint = function (s) {
    if (s) {
      this.x = s.x;
      this.y = s.y;
    } else {
      this.x = 0;
      this.y = 0;
    };
    this.$equal = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
  };
  this.TRect = function (s) {
    if (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
    } else {
      this.Left = 0;
      this.Top = 0;
      this.Right = 0;
      this.Bottom = 0;
    };
    this.$equal = function (b) {
      return (this.Left === b.Left) && ((this.Top === b.Top) && ((this.Right === b.Right) && (this.Bottom === b.Bottom)));
    };
  };
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = new $mod.TRect();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = new $mod.TPoint();
    Result.x = x;
    Result.y = y;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.TJSValueType = {"0": "jvtNull", jvtNull: 0, "1": "jvtBoolean", jvtBoolean: 1, "2": "jvtInteger", jvtInteger: 2, "3": "jvtFloat", jvtFloat: 3, "4": "jvtString", jvtString: 4, "5": "jvtObject", jvtObject: 5, "6": "jvtArray", jvtArray: 6};
  this.GetValueType = function (JS) {
    var Result = 0;
    var t = "";
    if ($mod.isNull(JS)) {
      Result = $mod.TJSValueType.jvtNull}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = $mod.TJSValueType.jvtString}
       else if (t === "boolean") {
        Result = $mod.TJSValueType.jvtBoolean}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = $mod.TJSValueType.jvtArray}
         else Result = $mod.TJSValueType.jvtObject;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = $mod.TJSValueType.jvtInteger}
       else Result = $mod.TJSValueType.jvtFloat;
    };
    return Result;
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
    };
    this.CreateFmt = function (Msg, Args) {
      this.fMessage = $mod.Format(Msg,Args);
    };
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while (((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9")) && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === -1) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          if (rtl.isNumber(Args[ArgN]) && pas.JS.isInteger(Args[ArgN])) {
            Value = Math.floor(Args[ArgN])}
           else $impl.DoFormatError(1,Fmt);
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = -1;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === -1) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (pas.JS.GetValueType(Args[DoArg]) !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp1 = Fchar;
        if ($tmp1 === "D") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp1 === "U") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffGeneral,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffCurrency,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(pas.JS.TJSValueType.jvtString,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          vq = Math.floor(Args[DoArg]);
          Index = 31;
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while (((1 << (Index * 4)) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp1 === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if ($mod.TStringReplaceFlag.rfReplaceAll in Flags) REFlags = "g";
    if ($mod.TStringReplaceFlag.rfIgnoreCase in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    var Radix = 10;
    var F = "";
    var N = "";
    var J = undefined;
    N = S;
    F = pas.System.Copy(N,1,1);
    if (F === "$") {
      Radix = 16}
     else if (F === "&") {
      Radix = 8}
     else if (F === "%") Radix = 2;
    if (Radix !== 10) pas.System.Delete({get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }},1,1);
    J = parseInt(N,Radix);
    Result = !isNaN(J);
    if (Result) res.set(Math.floor(J));
    return Result;
  };
  this.StrToInt = function (S) {
    var Result = 0;
    var R = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = R;
    return Result;
  };
  var HexDigits = "0123456789ABCDEF";
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    if (Digits === 0) Digits = 1;
    Result = "";
    while (Value > 0) {
      Result = HexDigits.charAt(((Value & 15) + 1) - 1) + Result;
      Value = Value >>> 4;
    };
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === $mod.TFloatFormat.ffGeneral) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffExponent) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffFixed) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffNumber) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator)}
     else if ($tmp1 === $mod.TFloatFormat.ffCurrency) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,DS,$mod.ThousandSeparator);
    if (((format !== $mod.TFloatFormat.ffCurrency) && (Result.length > 1)) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    return Result;
  };
  this.TTimeStamp = function (s) {
    if (s) {
      this.Time = s.Time;
      this.Date = s.Date;
    } else {
      this.Time = 0;
      this.Date = 0;
    };
    this.$equal = function (b) {
      return (this.Time === b.Time) && (this.Date === b.Date);
    };
  };
  this.TimeSeparator = ":";
  this.DateSeparator = "-";
  this.ShortDateFormat = "yyyy-mm-dd";
  this.LongDateFormat = "ddd, yyyy-mm-dd";
  this.ShortTimeFormat = "hh:nn";
  this.LongTimeFormat = "hh:nn:ss";
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  this.TimeAMString = "AM";
  this.TimePMString = "PM";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.DateDelta = 693594;
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  this.LongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
  this.ShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
  this.LongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.GetDecimalSeparator = function () {
      var Result = "";
      Result = $mod.DecimalSeparator;
      return Result;
    };
    this.GetShortDateFormat = function () {
      var Result = "";
      Result = $mod.ShortDateFormat;
      return Result;
    };
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
    this.SetDateSeparator = function (Value) {
      $mod.DateSeparator = Value;
    };
    this.SetDecimalSeparator = function (Value) {
      $mod.DecimalSeparator = Value;
    };
    this.SetShortDateFormat = function (Value) {
      $mod.ShortDateFormat = Value;
    };
    this.SetThousandSeparator = function (Value) {
      $mod.ThousandSeparator = Value;
    };
    this.SetTimeSeparator = function (Value) {
      $mod.TimeSeparator = Value;
    };
  });
  this.FormatSettings = null;
  this.JSDateToDateTime = function (aDate) {
    var Result = 0.0;
    Result = $mod.EncodeDate(aDate.getFullYear(),aDate.getMonth() + 1,aDate.getDate()) + $mod.EncodeTime(aDate.getHours(),aDate.getMinutes(),aDate.getSeconds(),aDate.getMilliseconds());
    return Result;
  };
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = new $mod.TTimeStamp();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = pas.System.Trunc(Math.abs(pas.System.Trunc(D)) % 86400000);
    Result.Date = 693594 + Math.floor(pas.System.Trunc(D) / 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (((((Year > 0) && (Year < 10000)) && (Month >= 1)) && (Month <= 12)) && (Day > 0)) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = Math.floor(Year / 100);
      ya = Year - (100 * c);
      date.set(((((146097 * c) >>> 2) + ((1461 * ya) >>> 2)) + Math.floor(((153 * Month) + 2) / 5)) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (((Hour < 24) && (Min < 60)) && (Sec < 60)) && (MSec < 1000);
    if (Result) Time.set(((((Hour * 3600000) + (Min * 60000)) + (Sec * 1000)) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s-%s-%s is not a valid date specification",[$mod.IntToStr(Year),$mod.IntToStr(Month),$mod.IntToStr(Day)]]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",[$mod.IntToStr(Hour),$mod.IntToStr(Minute),$mod.IntToStr(Second),$mod.IntToStr(MilliSecond)]]);
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = ((pas.System.Trunc(date) + 693900) << 2) - 1;
      ly = Math.floor(j / 146097);
      j = j - (146097 * ly);
      ld = j >>> 2;
      j = Math.floor(((ld << 2) + 3) / 1461);
      ld = (((ld << 2) + 7) - (1461 * j)) >>> 2;
      lm = Math.floor(((5 * ld) - 3) / 153);
      ld = Math.floor((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(Math.floor(l / 3600000));
    l = l % 3600000;
    Minute.set(Math.floor(l / 60000));
    l = l % 60000;
    Second.set(Math.floor(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + ((pas.System.Trunc(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.Date = function () {
    var Result = 0.0;
    Result = pas.System.Trunc($mod.Now());
    return Result;
  };
  this.Time = function () {
    var Result = 0.0;
    Result = $mod.Now() - $mod.Date();
    return Result;
  };
  this.Now = function () {
    var Result = 0.0;
    Result = $mod.JSDateToDateTime(new Date());
    return Result;
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.TimeToStr = function (Time) {
    var Result = "";
    Result = $mod.FormatDateTime("tt",Time);
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + pas.System.Copy(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp1 === "A") || ($tmp1 === "a")) {
          if ((($mod.CompareText(pas.System.Copy(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,4),"AMPM") === 0)) || ($mod.CompareText(pas.System.Copy(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp2 = Token;
        if (($tmp2 === "'") || ($tmp2 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp2 === "A") {
          if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString($mod.TimeAMString)}
             else StoreString($mod.TimePMString);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $mod.EConvertError.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp2 === "\/") {
          StoreString($mod.DateSeparator);
        } else if ($tmp2 === ":") {
          StoreString($mod.TimeSeparator)}
         else if ((((((((((($tmp2 === " ") || ($tmp2 === "C")) || ($tmp2 === "D")) || ($tmp2 === "H")) || ($tmp2 === "M")) || ($tmp2 === "N")) || ($tmp2 === "S")) || ($tmp2 === "T")) || ($tmp2 === "Y")) || ($tmp2 === "Z")) || ($tmp2 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp3 = Token;
          if ($tmp3 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp3 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp3 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp4 = Count;
              if ($tmp4 === 1) {
                StoreInt(Month,0)}
               else if ($tmp4 === 2) {
                StoreInt(Month,2)}
               else if ($tmp4 === 3) {
                StoreString($mod.ShortMonthNames[Month - 1])}
               else {
                StoreString($mod.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp3 === "D") {
            var $tmp5 = Count;
            if ($tmp5 === 1) {
              StoreInt(Day,0)}
             else if ($tmp5 === 2) {
              StoreInt(Day,2)}
             else if ($tmp5 === 3) {
              StoreString($mod.ShortDayNames[DayOfWeek])}
             else if ($tmp5 === 4) {
              StoreString($mod.LongDayNames[DayOfWeek])}
             else if ($tmp5 === 5) {
              StoreFormat($mod.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat($mod.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp3 === "H") {
            if (isInterval) {
              StoreInt(Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp3 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp3 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp3 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp3 === "T") {
            if (Count === 1) {
              StoreFormat($mod.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat($mod.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp3 === "C") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            if (((Hour !== 0) || (Minute !== 0)) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp3 === "F") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 2;
  this.CurrencyString = "$";
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.feInvalidFormat = 1;
  $impl.feMissingArgument = 2;
  $impl.feInvalidArgIndex = 3;
  $impl.DoFormatError = function (ErrCode, fmt) {
    var $tmp1 = ErrCode;
    if ($tmp1 === 1) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidFormat,[fmt]])}
     else if ($tmp1 === 2) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SArgumentMissing,[fmt]])}
     else if ($tmp1 === 3) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidArgIndex,[fmt]]);
  };
  $impl.maxdigits = 15;
  $impl.ReplaceDecimalSep = function (S, DS) {
    var Result = "";
    var P = 0;
    P = pas.System.Pos(".",S);
    if (P > 0) {
      Result = (pas.System.Copy(S,1,P - 1) + DS) + pas.System.Copy(S,P + 1,S.length - P)}
     else Result = S;
    return Result;
  };
  $impl.FormatGeneralFloat = function (Value, Precision, DS) {
    var Result = "";
    var P = 0;
    var PE = 0;
    var Q = 0;
    var Exponent = 0;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    Result = $mod.TrimLeft(Result);
    P = pas.System.Pos(".",Result);
    if (P === 0) return Result;
    PE = pas.System.Pos("E",Result);
    if (PE === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    Q = PE + 2;
    Exponent = 0;
    while (Q <= Result.length) {
      Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - "0".charCodeAt();
      Q += 1;
    };
    if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
    if (((P + Exponent) < PE) && (Exponent > -6)) {
      Result = rtl.strSetLength(Result,PE - 1);
      if (Exponent >= 0) {
        for (var $l1 = 0, $end2 = Exponent - 1; $l1 <= $end2; $l1++) {
          Q = $l1;
          Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
          P += 1;
        };
        Result = rtl.setCharAt(Result,P - 1,".");
        P = 1;
        if (Result.charAt(P - 1) === "-") P += 1;
        while (((Result.charAt(P - 1) === "0") && (P < Result.length)) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
      } else {
        pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 1);
        Result = rtl.setCharAt(Result,(P - Exponent) - 1,Result.charAt(((P - Exponent) - 1) - 1));
        Result = rtl.setCharAt(Result,P - 1,".");
        if (Exponent !== -1) Result = rtl.setCharAt(Result,((P - Exponent) - 1) - 1,"0");
      };
      Q = Result.length;
      while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
      if (Result.charAt(Q - 1) === ".") Q -= 1;
      if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
        Result = "0"}
       else Result = rtl.strSetLength(Result,Q);
    } else {
      while (Result.charAt((PE - 1) - 1) === "0") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE - 1) - 1) === DS) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE + 1) - 1) === "+") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1)}
       else PE += 1;
      while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},PE + 1,1);
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
    var Result = "";
    var P = 0;
    DS = $mod.DecimalSeparator;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos("E",Result);
    if (P === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    P += 2;
    if (Digits > 4) Digits = 4;
    Digits = ((Result.length - P) - Digits) + 1;
    if (Digits < 0) {
      pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P)}
     else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
      pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P,1);
      if (P > Result.length) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 2,2);
        break;
      };
      Digits -= 1;
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatFixedFloat = function (Value, Digits, DS) {
    var Result = "";
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
    var Result = "";
    var P = 0;
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 15) Digits = 15;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    P -= 3;
    if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
      if (Result.charAt((P - 1) - 1) !== "-") pas.System.Insert(TS,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    return Result;
  };
  $impl.RemoveLeadingNegativeSign = function (AValue, DS) {
    var Result = false;
    var i = 0;
    var TS = "";
    var StartPos = 0;
    Result = false;
    StartPos = 2;
    TS = $mod.ThousandSeparator;
    for (var $l1 = StartPos, $end2 = AValue.get().length; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get() === TS);
      if (!Result) break;
    };
    if (Result) pas.System.Delete(AValue,1,1);
    return Result;
  };
  $impl.FormatNumberCurrency = function (Value, Digits, DS, TS) {
    var Result = "";
    var Negative = false;
    var P = 0;
    if (Digits === -1) {
      Digits = $mod.CurrencyDecimals}
     else if (Digits > 18) Digits = 18;
    Result = rtl.spaceLeft("" + Value,0);
    Negative = Result.charAt(0) === "-";
    if (Negative) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    if (P !== 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS)}
     else P = Result.length + 1;
    P -= 3;
    while (P > 1) {
      if ($mod.ThousandSeparator !== "\x00") pas.System.Insert($mod.FormatSettings.GetThousandSeparator(),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    if ((Result.length > 1) && Negative) Negative = !$impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    if (!Negative) {
      var $tmp1 = $mod.CurrencyFormat;
      if ($tmp1 === 0) {
        Result = $mod.CurrencyString + Result}
       else if ($tmp1 === 1) {
        Result = Result + $mod.CurrencyString}
       else if ($tmp1 === 2) {
        Result = ($mod.CurrencyString + " ") + Result}
       else if ($tmp1 === 3) Result = (Result + " ") + $mod.CurrencyString;
    } else {
      var $tmp2 = $mod.NegCurrFormat;
      if ($tmp2 === 0) {
        Result = (("(" + $mod.CurrencyString) + Result) + ")"}
       else if ($tmp2 === 1) {
        Result = ("-" + $mod.CurrencyString) + Result}
       else if ($tmp2 === 2) {
        Result = ($mod.CurrencyString + "-") + Result}
       else if ($tmp2 === 3) {
        Result = ($mod.CurrencyString + Result) + "-"}
       else if ($tmp2 === 4) {
        Result = (("(" + Result) + $mod.CurrencyString) + ")"}
       else if ($tmp2 === 5) {
        Result = ("-" + Result) + $mod.CurrencyString}
       else if ($tmp2 === 6) {
        Result = (Result + "-") + $mod.CurrencyString}
       else if ($tmp2 === 7) {
        Result = (Result + $mod.CurrencyString) + "-"}
       else if ($tmp2 === 8) {
        Result = (("-" + Result) + " ") + $mod.CurrencyString}
       else if ($tmp2 === 9) {
        Result = (("-" + $mod.CurrencyString) + " ") + Result}
       else if ($tmp2 === 10) {
        Result = ((Result + " ") + $mod.CurrencyString) + "-"}
       else if ($tmp2 === 11) {
        Result = (($mod.CurrencyString + " ") + Result) + "-"}
       else if ($tmp2 === 12) {
        Result = (($mod.CurrencyString + " ") + "-") + Result}
       else if ($tmp2 === 13) {
        Result = ((Result + "-") + " ") + $mod.CurrencyString}
       else if ($tmp2 === 14) {
        Result = ((("(" + $mod.CurrencyString) + " ") + Result) + ")"}
       else if ($tmp2 === 15) Result = ((("(" + Result) + " ") + $mod.CurrencyString) + ")";
    };
    if (TS === "") ;
    return Result;
  };
  $impl.RESpecials = "([\\[\\]\\(\\)\\\\\\.\\*])";
});
rtl.module("Classes",["System","RTLConsts","Types","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass($mod,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass($mod,"EStringListError",$mod.EListError,function () {
  });
  rtl.createClass($mod,"EComponentError",pas.SysUtils.Exception,function () {
  });
  this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  $mod.$rtti.$Enum("TAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass($mod,"TFPList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.Error(pas.RTLConsts.SListCapacityError,"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.Error(pas.RTLConsts.SListCountError,"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.Error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,[Data]]);
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === pas.Types.TDirection.FromBeginning) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.Error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass($mod,"TList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Notify = function (aValue, Action) {
      if (pas.System.Assigned(aValue)) ;
      if (Action === $mod.TListNotification.lnExtracted) ;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FList = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,$mod.TListNotification.lnAdded);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if (pas.System.Assigned(V)) this.Notify(V,$mod.TListNotification.lnDeleted);
    };
    this.Insert = function (Index, Item) {
      this.FList.Insert(Index,Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,$mod.TListNotification.lnAdded);
    };
  });
  rtl.createClass($mod,"TPersistent",pas.System.TObject,function () {
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw pas.SysUtils.EConvertError.$create("Create$1",[((("Cannot assign a " + SourceName) + " to a ") + this.$classname) + "."]);
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass($mod,"TStrings",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FSpecialCharsInited = false;
      this.FAlwaysQuote = false;
      this.FQuoteChar = "";
      this.FDelimiter = "";
      this.FNameValueSeparator = "";
      this.FUpdateCount = 0;
      this.FLBS = 0;
      this.FStrictDelimiter = false;
      this.FLineBreak = "";
    };
    this.GetValue = function (Name) {
      var Result = "";
      var L = 0;
      var N = "";
      Result = "";
      L = this.IndexOfName(Name);
      if (L !== -1) this.GetNameValue(L,{get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.SetDelimiter = function (c) {
      this.CheckSpecialChars();
      this.FDelimiter = c;
    };
    this.Error = function (Msg, Data) {
      throw $mod.EStringListError.$create("CreateFmt",[Msg,[pas.SysUtils.IntToStr(Data)]]);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.GetCount();
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      if (Index === 0) ;
      Result = null;
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) ;
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.SetDelimitedText = function (AValue) {
      var i = 0;
      var j = 0;
      var aNotFirst = false;
      this.CheckSpecialChars();
      this.BeginUpdate();
      i = 1;
      j = 1;
      aNotFirst = false;
      try {
        this.Clear();
        if (this.FStrictDelimiter) {
          while (i <= AValue.length) {
            if ((aNotFirst && (i <= AValue.length)) && (AValue.charAt(i - 1) === this.FDelimiter)) i += 1;
            if (i <= AValue.length) {
              if (AValue.charAt(i - 1) === this.FQuoteChar) {
                j = i + 1;
                while ((j <= AValue.length) && ((AValue.charAt(j - 1) !== this.FQuoteChar) || (((j + 1) <= AValue.length) && (AValue.charAt((j + 1) - 1) === this.FQuoteChar)))) {
                  if ((j <= AValue.length) && (AValue.charAt(j - 1) === this.FQuoteChar)) {
                    j += 2}
                   else j += 1;
                };
                this.Add(pas.SysUtils.StringReplace(pas.System.Copy(AValue,i + 1,(j - i) - 1),this.FQuoteChar + this.FQuoteChar,this.FQuoteChar,rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll)));
                i = j + 1;
              } else {
                j = i;
                while ((j <= AValue.length) && (AValue.charAt(j - 1) !== this.FDelimiter)) j += 1;
                this.Add(pas.System.Copy(AValue,i,j - i));
                i = j;
              };
            } else {
              if (aNotFirst) this.Add("");
            };
            aNotFirst = true;
          };
        } else {
          while (i <= AValue.length) {
            if ((aNotFirst && (i <= AValue.length)) && (AValue.charAt(i - 1) === this.FDelimiter)) i += 1;
            while ((i <= AValue.length) && (AValue.charCodeAt(i - 1) <= " ".charCodeAt())) i += 1;
            if (i <= AValue.length) {
              if (AValue.charAt(i - 1) === this.FQuoteChar) {
                j = i + 1;
                while ((j <= AValue.length) && ((AValue.charAt(j - 1) !== this.FQuoteChar) || (((j + 1) <= AValue.length) && (AValue.charAt((j + 1) - 1) === this.FQuoteChar)))) {
                  if ((j <= AValue.length) && (AValue.charAt(j - 1) === this.FQuoteChar)) {
                    j += 2}
                   else j += 1;
                };
                this.Add(pas.SysUtils.StringReplace(pas.System.Copy(AValue,i + 1,(j - i) - 1),this.FQuoteChar + this.FQuoteChar,this.FQuoteChar,rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll)));
                i = j + 1;
              } else {
                j = i;
                while (((j <= AValue.length) && (AValue.charCodeAt(j - 1) > " ".charCodeAt())) && (AValue.charAt(j - 1) !== this.FDelimiter)) j += 1;
                this.Add(pas.System.Copy(AValue,i,j - i));
                i = j;
              };
            } else {
              if (aNotFirst) this.Add("");
            };
            while ((i <= AValue.length) && (AValue.charCodeAt(i - 1) <= " ".charCodeAt())) i += 1;
            aNotFirst = true;
          };
        };
      } finally {
        this.EndUpdate();
      };
    };
    this.CheckSpecialChars = function () {
      if (!this.FSpecialCharsInited) {
        this.FQuoteChar = '"';
        this.FDelimiter = ",";
        this.FNameValueSeparator = "=";
        this.FLBS = pas.System.DefaultTextLineBreakStyle;
        this.FSpecialCharsInited = true;
        this.FLineBreak = pas.System.sLineBreak;
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FAlwaysQuote = false;
    };
    this.Destroy = function () {
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      Result = this.GetCount();
      this.Insert(this.GetCount(),S);
      return Result;
    };
    this.AddObject = function (S, AObject) {
      var Result = 0;
      Result = this.Add(S);
      this.PutObject(Result,AObject);
      return Result;
    };
    this.AddStrings = function (TheStrings) {
      var Runner = 0;
      for (var $l1 = 0, $end2 = TheStrings.GetCount() - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        this.AddObject(TheStrings.Get(Runner),TheStrings.GetObject(Runner));
      };
    };
    this.Assign = function (Source) {
      var S = null;
      if ($mod.TStrings.isPrototypeOf(Source)) {
        S = Source;
        this.BeginUpdate();
        try {
          this.Clear();
          this.FSpecialCharsInited = S.FSpecialCharsInited;
          this.FQuoteChar = S.FQuoteChar;
          this.FDelimiter = S.FDelimiter;
          this.FNameValueSeparator = S.FNameValueSeparator;
          this.FLBS = S.FLBS;
          this.FLineBreak = S.FLineBreak;
          this.AddStrings(S);
        } finally {
          this.EndUpdate();
        };
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      if (this.FUpdateCount === 0) this.SetUpdateState(true);
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.SetUpdateState(false);
    };
    this.IndexOfName = function (Name) {
      var Result = 0;
      var len = 0;
      var S = "";
      this.CheckSpecialChars();
      Result = 0;
      while (Result < this.GetCount()) {
        S = this.Get(Result);
        len = pas.System.Pos(this.FNameValueSeparator,S) - 1;
        if ((len >= 0) && (this.DoCompareText(Name,pas.System.Copy(S,1,len)) === 0)) return Result;
        Result += 1;
      };
      Result = -1;
      return Result;
    };
    this.GetNameValue = function (Index, AName, AValue) {
      var L = 0;
      this.CheckSpecialChars();
      AValue.set(this.Get(Index));
      L = pas.System.Pos(this.FNameValueSeparator,AValue.get());
      if (L !== 0) {
        AName.set(pas.System.Copy(AValue.get(),1,L - 1));
        AValue.set(pas.System.Copy(AValue.get(),L + 1,AValue.get().length - L));
      } else AName.set("");
    };
  });
  this.TStringItem = function (s) {
    if (s) {
      this.FString = s.FString;
      this.FObject = s.FObject;
    } else {
      this.FString = "";
      this.FObject = null;
    };
    this.$equal = function (b) {
      return (this.FString === b.FString) && (this.FObject === b.FObject);
    };
  };
  this.TStringsSortStyle = {"0": "sslNone", sslNone: 0, "1": "sslUser", sslUser: 1, "2": "sslAuto", sslAuto: 2};
  rtl.createClass($mod,"TStringList",$mod.TStrings,function () {
    this.$init = function () {
      $mod.TStrings.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FOnChange = null;
      this.FOnChanging = null;
      this.FDuplicates = 0;
      this.FCaseSensitive = false;
      this.FOwnsObjects = false;
      this.FSortStyle = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FOnChange = undefined;
      this.FOnChanging = undefined;
      $mod.TStrings.$final.call(this);
    };
    this.GetSorted = function () {
      var Result = false;
      Result = this.FSortStyle in rtl.createSet($mod.TStringsSortStyle.sslUser,$mod.TStringsSortStyle.sslAuto);
      return Result;
    };
    this.Grow = function () {
      var NC = 0;
      NC = this.GetCapacity();
      if (NC >= 256) {
        NC = NC + Math.floor(NC / 4)}
       else if (NC === 0) {
        NC = 4}
       else NC = NC * 4;
      this.SetCapacity(NC);
    };
    this.InternalClear = function (FromIndex, ClearOnly) {
      var I = 0;
      if (FromIndex < this.FCount) {
        if (this.FOwnsObjects) {
          for (var $l1 = FromIndex, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
            I = $l1;
            this.FList[I].FString = "";
            pas.SysUtils.FreeAndNil({p: this.FList[I], get: function () {
                return this.p.FObject;
              }, set: function (v) {
                this.p.FObject = v;
              }});
          };
        } else {
          for (var $l3 = FromIndex, $end4 = this.FCount - 1; $l3 <= $end4; $l3++) {
            I = $l3;
            this.FList[I].FString = "";
          };
        };
        this.FCount = FromIndex;
      };
      if (!ClearOnly) this.SetCapacity(0);
    };
    this.CheckIndex = function (AIndex) {
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.Error(pas.RTLConsts.SListIndexError,AIndex);
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) {
        if (this.FOnChange != null) this.FOnChange(this);
      };
    };
    this.Changing = function () {
      if (this.FUpdateCount === 0) if (this.FOnChanging != null) this.FOnChanging(this);
    };
    this.Get = function (Index) {
      var Result = "";
      this.CheckIndex(Index);
      Result = this.FList[Index].FString;
      return Result;
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = rtl.length(this.FList);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FCount;
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      this.CheckIndex(Index);
      Result = this.FList[Index].FObject;
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FObject = AObject;
      this.Changed();
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < 0) this.Error(pas.RTLConsts.SListCapacityError,NewCapacity);
      if (NewCapacity !== this.GetCapacity()) this.FList = rtl.arraySetLength(this.FList,$mod.TStringItem,NewCapacity);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) {
        this.Changing()}
       else this.Changed();
    };
    this.InsertItem = function (Index, S) {
      this.InsertItem$1(Index,S,null);
    };
    this.InsertItem$1 = function (Index, S, O) {
      var It = new $mod.TStringItem();
      this.Changing();
      if (this.FCount === this.GetCapacity()) this.Grow();
      It.FString = S;
      It.FObject = O;
      this.FList.splice(Index,0,It);
      this.FCount += 1;
      this.Changed();
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      if (this.FCaseSensitive) {
        Result = pas.SysUtils.CompareStr(s1,s2)}
       else Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.Destroy = function () {
      this.InternalClear(0,false);
      $mod.TStrings.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      if (!(this.FSortStyle === $mod.TStringsSortStyle.sslAuto)) {
        Result = this.FCount}
       else if (this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) {
        var $tmp1 = this.FDuplicates;
        if ($tmp1 === pas.Types.TDuplicates.dupIgnore) {
          return Result}
         else if ($tmp1 === pas.Types.TDuplicates.dupError) this.Error(pas.RTLConsts.SDuplicateString,0);
      };
      this.InsertItem(Result,S);
      return Result;
    };
    this.Clear = function () {
      if (this.FCount === 0) return;
      this.Changing();
      this.InternalClear(0,false);
      this.Changed();
    };
    this.Find = function (S, Index) {
      var Result = false;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      Result = false;
      Index.set(-1);
      if (!this.GetSorted()) throw $mod.EListError.$create("Create$1",[pas.RTLConsts.SErrFindNeedsSortedList]);
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + Math.floor((R - L) / 2);
        CompareRes = this.DoCompareText(S,this.FList[I].FString);
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            Result = true;
            if (this.FDuplicates !== pas.Types.TDuplicates.dupAccept) L = I;
          };
        };
      };
      Index.set(L);
      return Result;
    };
    this.Insert = function (Index, S) {
      if (this.FSortStyle === $mod.TStringsSortStyle.sslAuto) {
        this.Error(pas.RTLConsts.SSortedListError,0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.Error(pas.RTLConsts.SListIndexError,Index);
        this.InsertItem(Index,S);
      };
    };
  });
  rtl.createClass($mod,"TCollectionItem",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection !== null) this.FCollection.RemoveItem(this);
        if (Value !== null) Value.InsertItem(this);
      };
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.SetCollection(ACollection);
    };
    this.Destroy = function () {
      this.SetCollection(null);
      pas.System.TObject.Destroy.call(this);
    };
  });
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass($mod,"TCollection",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,$mod.TCollectionNotification.cnAdded);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,$mod.TCollectionNotification.cnExtracting);
      I = this.FItems.IndexOfItem(Item,pas.Types.TDirection.FromEnd);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === $mod.TCollectionNotification.cnAdded) ;
    };
    this.Create$1 = function (AItemClass) {
      pas.System.TObject.Create.call(this);
      this.FItemClass = AItemClass;
      this.FItems = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($mod.TCollection.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l1 = 0, $end2 = Source.GetCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
  });
  rtl.createClass($mod,"TOwnedCollection",$mod.TCollection,function () {
    this.$init = function () {
      $mod.TCollection.$init.call(this);
      this.FOwner = null;
    };
    this.$final = function () {
      this.FOwner = undefined;
      $mod.TCollection.$final.call(this);
    };
    this.Create$2 = function (AOwner, AItemClass) {
      this.FOwner = AOwner;
      $mod.TCollection.Create$1.call(this,AItemClass);
    };
  });
  this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass($mod,"TComponent",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FDesignInfo = 0;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetComponent = function (AIndex) {
      var Result = null;
      if (!(this.FComponents != null)) {
        Result = null}
       else Result = rtl.getObject(this.FComponents.Get(AIndex));
      return Result;
    };
    this.GetComponentCount = function () {
      var Result = 0;
      if (!(this.FComponents != null)) {
        Result = 0}
       else Result = this.FComponents.FCount;
      return Result;
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $mod.TFPList.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csFreeNotification);
        };
      };
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.Loaded = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csLoading);
    };
    this.Loading = function () {
      this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csLoading);
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === $mod.TOperation.opRemove) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csDesigning)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csDesigning);
      if ((this.FComponents != null) && SetChildren) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !pas.SysUtils.IsValidIdent(NewName,false,false)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SInvalidName,[NewName]]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.ChangeName(NewName);
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((((AComponent !== null) && (pas.SysUtils.CompareText(CurName,NewName) !== 0)) && (AComponent.FOwner === this)) && (this.FindComponent(NewName) !== null)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SDuplicateName,[NewName]]);
      if (($mod.TComponentStateItem.csDesigning in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet($mod.TComponentStyleItem.csInheritable);
      if (AOwner != null) AOwner.InsertComponent(this);
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,$mod.TOperation.opRemove);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      pas.System.TObject.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!($mod.TComponentStateItem.csDestroying in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if ($mod.TComponentStateItem.csDestroying in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csDestroying);
      if (this.FComponents != null) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (pas.SysUtils.CompareText(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      this.Insert(AComponent);
      if ($mod.TComponentStateItem.csDesigning in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,$mod.TOperation.opInsert);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,$mod.TOperation.opRemove);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag");
  });
  $mod.$init = function () {
    $impl.ClassList = Object.create(null);
  };
},["JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ClassList = null;
});
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TJSKeyNames",pas.System.TObject,function () {
    this.Alt = "Alt";
    this.CapsLock = "CapsLock";
    this.Control = "Control";
    this.Shift = "Shift";
    this.Enter = "Enter";
    this.Tab = "Tab";
    this.ArrowDown = "ArrowDown";
    this.ArrowLeft = "ArrowLeft";
    this.ArrowRight = "ArrowRight";
    this.ArrowUp = "ArrowUp";
    this._End = "End";
    this.Home = "Home";
    this.PageDown = "PageDown";
    this.PageUp = "PageUp";
    this.BackSpace = "Backspace";
    this.Delete = "Delete";
    this.Insert = "Insert";
    this.Escape = "Escape";
    this.Pause = "Pause";
    this.F1 = "F1";
    this.F2 = "F2";
    this.F3 = "F3";
    this.F4 = "F4";
    this.F5 = "F5";
    this.F6 = "F6";
    this.F7 = "F7";
    this.F8 = "F8";
    this.F9 = "F9";
    this.F10 = "F10";
    this.F11 = "F11";
    this.F12 = "F12";
    this.F13 = "F13";
    this.F14 = "F14";
    this.F15 = "F15";
    this.F16 = "F16";
    this.F17 = "F17";
    this.F18 = "F18";
    this.F19 = "F19";
    this.F20 = "F20";
  });
});
rtl.module("contnrs",["System","SysUtils","Classes"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TObjectList",pas.Classes.TList,function () {
    this.$init = function () {
      pas.Classes.TList.$init.call(this);
      this.FFreeObjects = false;
    };
    this.Notify = function (Ptr, Action) {
      var O = null;
      if (this.FFreeObjects) if (Action === pas.Classes.TListNotification.lnDeleted) {
        O = rtl.getObject(Ptr);
        O = rtl.freeLoc(O);
      };
      pas.Classes.TList.Notify.call(this,Ptr,Action);
    };
    this.Create$2 = function () {
      pas.Classes.TList.Create$1.call(this);
      this.FFreeObjects = true;
    };
  });
},["JS"]);
rtl.module("WEBLib.Graphics",["System","Classes","Types","Web","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.clBlack = 0x0;
  this.clSilver = 0xC0C0C0;
  this.clRed = 0xFF;
  this.clWhite = 0xFFFFFF;
  this.clBtnFace = 0xF0F0F0;
  this.ANSI_CHARSET = 0;
  this.TFontPitch = {"0": "fpDefault", fpDefault: 0, "1": "fpVariable", fpVariable: 1, "2": "fpFixed", fpFixed: 2};
  $mod.$rtti.$Enum("TFontPitch",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TFontPitch});
  this.TFontQuality = {"0": "fqDefault", fqDefault: 0, "1": "fqDraft", fqDraft: 1, "2": "fqProof", fqProof: 2, "3": "fqNonAntialiased", fqNonAntialiased: 3, "4": "fqAntialiased", fqAntialiased: 4, "5": "fqCleartype", fqCleartype: 5, "6": "fqCleartypeNatural", fqCleartypeNatural: 6};
  $mod.$rtti.$Enum("TFontQuality",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TFontQuality});
  this.TPenStyle = {"0": "psSolid", psSolid: 0, "1": "psDash", psDash: 1, "2": "psDot", psDot: 2, "3": "psDashDot", psDashDot: 3, "4": "psDashDotDot", psDashDotDot: 4, "5": "psClear", psClear: 5, "6": "psInsideFrame", psInsideFrame: 6, "7": "psUserStyle", psUserStyle: 7, "8": "psAlternate", psAlternate: 8};
  $mod.$rtti.$Enum("TPenStyle",{minvalue: 0, maxvalue: 8, ordtype: 1, enumtype: this.TPenStyle});
  this.TBrushStyle = {"0": "bsSolid", bsSolid: 0, "1": "bsClear", bsClear: 1, "2": "bsHorizontal", bsHorizontal: 2, "3": "bsVertical", bsVertical: 3, "4": "bsFDiagonal", bsFDiagonal: 4, "5": "bsBDiagonal", bsBDiagonal: 5, "6": "bsCross", bsCross: 6, "7": "bsDiagCross", bsDiagCross: 7};
  $mod.$rtti.$Enum("TBrushStyle",{minvalue: 0, maxvalue: 7, ordtype: 1, enumtype: this.TBrushStyle});
  this.TFontStyle = {"0": "fsBold", fsBold: 0, "1": "fsItalic", fsItalic: 1, "2": "fsStrikeOut", fsStrikeOut: 2, "3": "fsUnderline", fsUnderline: 3};
  $mod.$rtti.$Enum("TFontStyle",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFontStyle});
  $mod.$rtti.$Set("TFontStyles",{comptype: $mod.$rtti["TFontStyle"]});
  $mod.$rtti.$Int("TFontCharset",{minvalue: 0, maxvalue: 255, ordtype: 3});
  rtl.createClass($mod,"TPen",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FWidth = 0;
      this.FColor = 0;
      this.FStyle = 0;
    };
    this.SetColor = function (Value) {
      this.FColor = Value;
    };
    this.Create$1 = function () {
      this.FColor = 0;
      this.FWidth = 1;
      this.FStyle = $mod.TPenStyle.psSolid;
    };
    this.Assign = function (Source) {
      if ($mod.TPen.isPrototypeOf(Source)) {
        this.FColor = rtl.as(Source,$mod.TPen).FColor;
        this.FStyle = rtl.as(Source,$mod.TPen).FStyle;
        this.FWidth = rtl.as(Source,$mod.TPen).FWidth;
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Width",0,rtl.longint,"FWidth","FWidth");
    $r.addProperty("Style",0,$mod.$rtti["TPenStyle"],"FStyle","FStyle");
  });
  rtl.createClass($mod,"TBrush",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
    };
    this.Create$1 = function () {
      this.FColor = 16777215;
      this.FStyle = $mod.TBrushStyle.bsSolid;
    };
    this.Assign = function (Source) {
      if ($mod.TBrush.isPrototypeOf(Source)) {
        this.FColor = rtl.as(Source,$mod.TBrush).FColor;
        this.FStyle = rtl.as(Source,$mod.TBrush).FStyle;
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",0,rtl.longint,"FColor","FColor");
    $r.addProperty("Style",0,$mod.$rtti["TBrushStyle"],"FStyle","FStyle");
  });
  rtl.createClass($mod,"TFont",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FName = "";
      this.FSize = 0;
      this.FColor = 0;
      this.FStyle = {};
      this.FOnChange = null;
      this.FHeight = 0;
      this.FCharset = 0;
      this.Orientation = 0;
      this.Pitch = 0;
      this.Quality = 0;
    };
    this.$final = function () {
      this.FStyle = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetHeight = function (Value) {
      var d = 0.0;
      this.FHeight = Value;
      d = (-this.FHeight * 72) / 96;
      this.FSize = Math.round(d);
      this.DoChange();
    };
    this.SetName = function (AName) {
      if (this.FName !== AName) {
        this.FName = AName;
        this.DoChange();
      };
    };
    this.SetSize = function (ASize) {
      if (this.FSize !== ASize) {
        this.FSize = ASize;
        this.DoChange();
      };
    };
    this.SetColor = function (AColor) {
      if (this.FColor !== AColor) {
        this.FColor = AColor;
        this.DoChange();
      };
    };
    this.SetStyle = function (AStyle) {
      this.FStyle = rtl.refSet(AStyle);
      this.DoChange();
    };
    this.DoChange = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Assign = function (Source) {
      if ($mod.TFont.isPrototypeOf(Source)) {
        this.FName = rtl.as(Source,$mod.TFont).FName;
        this.FColor = rtl.as(Source,$mod.TFont).FColor;
        this.FSize = rtl.as(Source,$mod.TFont).FSize;
        this.FStyle = rtl.refSet(rtl.as(Source,$mod.TFont).FStyle);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.apply(this,arguments);
      this.FName = "Tahoma";
      this.FSize = 8;
      this.FStyle = {};
      this.FColor = 0;
    };
    var $r = this.$rtti;
    $r.addProperty("Charset",0,$mod.$rtti["TFontCharset"],"FCharset","FCharset");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Height",2,rtl.longint,"FHeight","SetHeight");
    $r.addProperty("Style",2,$mod.$rtti["TFontStyles"],"FStyle","SetStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addField("Orientation",rtl.longint);
    $r.addField("Pitch",$mod.$rtti["TFontPitch"]);
    $r.addField("Quality",$mod.$rtti["TFontQuality"]);
  });
  rtl.createClass($mod,"TCanvas",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FElementCanvas = null;
      this.FContext = null;
      this.FPen = null;
      this.FBrush = null;
      this.FFont = null;
      this.FPathOpen = false;
    };
    this.$final = function () {
      this.FElementCanvas = undefined;
      this.FContext = undefined;
      this.FPen = undefined;
      this.FBrush = undefined;
      this.FFont = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AControl) {
      this.FElementCanvas = AControl;
      this.FContext = AControl.getContext("2d");
      this.FPen = $mod.TPen.$create("Create$1");
      this.FBrush = $mod.TBrush.$create("Create$1");
      this.FPathOpen = false;
      this.FFont = $mod.TFont.$create("Create$1");
    };
    this.Destroy = function () {
      pas.System.TObject.Destroy.apply(this,arguments);
    };
    this.Clear = function () {
      if ((this.FContext != null) && (this.FElementCanvas != null)) this.FContext.clearRect(0,0,this.FElementCanvas.width,this.FElementCanvas.height);
    };
  });
  this.ColorToHex = function (c) {
    var Result = "";
    var s = "";
    s = c.toString(16);
    
        while (s.length < 6)
        {
          s = "0" + s;
        };
    Result = (pas.System.Copy(s,5,2) + pas.System.Copy(s,3,2)) + pas.System.Copy(s,1,2);
    return Result;
  };
  this.ColorToHTML = function (c) {
    var Result = "";
    Result = "#" + $mod.ColorToHex(c);
    return Result;
  };
  $mod.$init = function () {
    $impl.FCache = $impl.TGraphicCacheList.$create("Create$2");
    $impl.FQueue = pas.Classes.TStringList.$create("Create$1");
  };
},["WEBLib.Forms","SysUtils","contnrs"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($impl,"TGraphicCacheList",pas.contnrs.TObjectList,function () {
  });
  $impl.FCache = null;
  $impl.FQueue = null;
});
rtl.module("WEBLib.StdCtrls",["System","Classes","WEBLib.Controls","SysUtils","Web","WEBLib.Graphics","Types"],function () {
  "use strict";
  var $mod = this;
  this.TEllipsisPosition = {"0": "epEndEllipsis", epEndEllipsis: 0, "1": "epNone", epNone: 1};
  $mod.$rtti.$Enum("TEllipsisPosition",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TEllipsisPosition});
  this.TTextLayout = {"0": "tlTop", tlTop: 0, "1": "tlCenter", tlCenter: 1, "2": "tlBottom", tlBottom: 2};
  $mod.$rtti.$Enum("TTextLayout",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TTextLayout});
  rtl.createClass($mod,"TCustomLabel",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FContent = null;
      this.FAutoSize = false;
      this.FCaption = "";
      this.FEllipsisPosition = 0;
      this.FWordWrap = false;
      this.FAlignment = 0;
      this.FLayout = 0;
      this.FTransparent = false;
    };
    this.$final = function () {
      this.FContent = undefined;
      pas["WEBLib.Controls"].TCustomControl.$final.call(this);
    };
    this.SetLayout = function (Value) {
      if (this.FLayout !== Value) {
        this.FLayout = Value;
        this.UpdateElement();
      };
    };
    this.SetAlignment = function (Value) {
      if (this.FAlignment !== Value) {
        this.FAlignment = Value;
        this.UpdateElement();
      };
    };
    this.GetContentHandle = function () {
      var Result = null;
      Result = this.FContent;
      return Result;
    };
    this.GetOuterWidth = function () {
      var Result = 0;
      Result = this.GetWidth();
      Result = Result + 4;
      return Result;
    };
    this.GetOuterHeight = function () {
      var Result = 0;
      Result = this.GetHeight();
      if (this.FAutoSize) Result = Result + 4;
      return Result;
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("DIV");
      this.FContent = document.createElement("SPAN");
      Result.appendChild(this.FContent);
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = this.FCaption;
      return Result;
    };
    this.CanShowFocus = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.BindElement = function () {
      this.FContent = this.FContainer.firstElementChild;
    };
    this.UpdateElementData = function () {
      pas["WEBLib.Controls"].TControl.UpdateElementData.apply(this,arguments);
      if (this.GetContentHandle() != null) {
        this.GetContentHandle().innerHTML = this.GetDisplayText()}
       else this.GetElementHandle().innerHTML = this.GetDisplayText();
    };
    this.UpdateElementVisual = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElementVisual.apply(this,arguments);
      if (this.IsUpdating()) return;
      if (this.GetElementHandle() != null) {
        this.GetElementHandle().style.setProperty("display","table");
        if (this.FAutoSize) {
          this.GetElementHandle().style.setProperty("overflow","")}
         else this.GetElementHandle().style.setProperty("overflow","hidden");
      };
      if (this.GetContentHandle() != null) {
        var $tmp1 = this.FLayout;
        if ($tmp1 === $mod.TTextLayout.tlTop) {
          this.GetContentHandle().style.setProperty("vertical-align","top")}
         else if ($tmp1 === $mod.TTextLayout.tlCenter) {
          this.GetContentHandle().style.setProperty("vertical-align","middle")}
         else if ($tmp1 === $mod.TTextLayout.tlBottom) this.GetContentHandle().style.setProperty("vertical-align","bottom");
        var $tmp2 = this.FAlignment;
        if ($tmp2 === pas.Classes.TAlignment.taCenter) {
          this.GetContentHandle().setAttribute("align","center")}
         else if ($tmp2 === pas.Classes.TAlignment.taRightJustify) this.GetContentHandle().setAttribute("align","right");
        if (!this.FTransparent) this.GetContentHandle().style.setProperty("background-color",pas["WEBLib.Graphics"].ColorToHTML(this.FColor));
        this.GetContentHandle().style.setProperty("display","table-cell");
        if (this.FEnabled) this.GetContentHandle().style.setProperty("color",pas["WEBLib.Graphics"].ColorToHTML(this.FFont.FColor));
        if (this.FElementFont === pas["WEBLib.Controls"].TElementFont.efProperty) this.SetHTMLElementFont(this.GetContentHandle(),this.FFont);
        if (this.FEllipsisPosition === $mod.TEllipsisPosition.epNone) {
          this.GetContentHandle().style.setProperty("text-overflow","clip")}
         else this.GetContentHandle().style.setProperty("text-overflow","ellipsis");
        if (this.FWordWrap) {
          this.GetContentHandle().style.setProperty("white-space","normal")}
         else this.GetContentHandle().style.setProperty("white-space","nowrap");
      };
    };
    this.SetAutoSize = function (AValue) {
      if (this.FAutoSize !== AValue) {
        this.FAutoSize = AValue;
        if (this.FAutoSize) {
          this.SetWidth(-1);
          this.SetHeight(-1);
          this.FEllipsisPosition = $mod.TEllipsisPosition.epNone;
        };
        this.UpdateElement();
      };
    };
    this.SetCaption = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.UpdateElement();
      };
    };
    this.SetEllipsisPosition = function (AValue) {
      if (this.FEllipsisPosition !== AValue) {
        this.FEllipsisPosition = AValue;
        if (this.FEllipsisPosition !== $mod.TEllipsisPosition.epNone) this.FAutoSize = false;
        this.UpdateElement();
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FAutoSize = true;
      this.FLayout = $mod.TTextLayout.tlTop;
      this.FCaption = "";
      this.FEllipsisPosition = $mod.TEllipsisPosition.epNone;
      this.FTransparent = true;
      this.SetColor(16777215);
      this.FAlignment = pas.Classes.TAlignment.taLeftJustify;
      this.SetTabStop(false);
    };
  });
  rtl.createClass($mod,"TLabel",$mod.TCustomLabel,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchors"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("EllipsisPosition",2,$mod.$rtti["TEllipsisPosition"],"FEllipsisPosition","SetEllipsisPosition");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("ElementFont",2,pas["WEBLib.Controls"].$rtti["TElementFont"],"FElementFont","SetElementFont",{Default: pas["WEBLib.Controls"].TElementFont.efProperty});
    $r.addProperty("ElementPosition",2,pas["WEBLib.Controls"].$rtti["TElementPosition"],"FElementPosition","SetElementPosition",{Default: pas["WEBLib.Controls"].TElementPosition.epAbsolute});
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Layout",2,$mod.$rtti["TTextLayout"],"FLayout","SetLayout");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Margins",2,pas["WEBLib.Controls"].$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Transparent",0,rtl.boolean,"FTransparent","FTransparent");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("WordWrap",0,rtl.boolean,"FWordWrap","FWordWrap");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
  });
  rtl.createClass($mod,"TWebLabel",$mod.TLabel,function () {
  });
});
rtl.module("WEBLib.Controls",["System","Classes","WEBLib.Graphics","Types","SysUtils","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.crDefault = 0;
  this.crNone = 1;
  this.crArrow = 2;
  this.crCross = 3;
  this.crIBeam = 4;
  this.crSizeNESW = 6;
  this.crSizeNS = 7;
  this.crSizeNWSE = 8;
  this.crSizeWE = 9;
  this.crUpArrow = 10;
  this.crHourGlass = 11;
  this.crDrag = 12;
  this.crNoDrop = 13;
  this.crHSplit = 14;
  this.crVSplit = 15;
  this.crMultIDrag = 16;
  this.crSQLWait = 17;
  this.crNo = 18;
  this.crAppStart = 19;
  this.crHelp = 20;
  this.crHandPoint = 21;
  this.crSizeAll = 22;
  this.TAlign = {"0": "alNone", alNone: 0, "1": "alTop", alTop: 1, "2": "alBottom", alBottom: 2, "3": "alLeft", alLeft: 3, "4": "alRight", alRight: 4, "5": "alClient", alClient: 5, "6": "alCustom", alCustom: 6};
  $mod.$rtti.$Enum("TAlign",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TAlign});
  this.TMouseButton = {"0": "mbLeft", mbLeft: 0, "1": "mbRight", mbRight: 1, "2": "mbMiddle", mbMiddle: 2};
  $mod.$rtti.$Enum("TMouseButton",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TMouseButton});
  this.TBorderStyle = {"0": "bsNone", bsNone: 0, "1": "bsSingle", bsSingle: 1};
  $mod.$rtti.$Enum("TBorderStyle",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TBorderStyle});
  this.TSizeStyle = {"0": "ssPercent", ssPercent: 0, "1": "ssAbsolute", ssAbsolute: 1, "2": "ssAuto", ssAuto: 2};
  $mod.$rtti.$Enum("TSizeStyle",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TSizeStyle});
  this.TAnchorKind = {"0": "akLeft", akLeft: 0, "1": "akTop", akTop: 1, "2": "akRight", akRight: 2, "3": "akBottom", akBottom: 3};
  $mod.$rtti.$Enum("TAnchorKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TAnchorKind});
  $mod.$rtti.$Set("TAnchors",{comptype: $mod.$rtti["TAnchorKind"]});
  this.TElementFont = {"0": "efProperty", efProperty: 0, "1": "efCSS", efCSS: 1};
  $mod.$rtti.$Enum("TElementFont",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TElementFont});
  this.TElementPosition = {"0": "epAbsolute", epAbsolute: 0, "1": "epRelative", epRelative: 1, "2": "epNone", epNone: 2};
  $mod.$rtti.$Enum("TElementPosition",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TElementPosition});
  this.TShiftState$a = {"0": "ssShift", ssShift: 0, "1": "ssAlt", ssAlt: 1, "2": "ssCtrl", ssCtrl: 2, "3": "ssLeft", ssLeft: 3, "4": "ssRight", ssRight: 4, "5": "ssMIDdle", ssMIDdle: 5, "6": "ssDouble", ssDouble: 6, "7": "ssTouch", ssTouch: 7, "8": "ssPen", ssPen: 8, "9": "ssCommand", ssCommand: 9};
  $mod.$rtti.$Enum("TShiftState$a",{minvalue: 0, maxvalue: 9, ordtype: 1, enumtype: this.TShiftState$a});
  $mod.$rtti.$Set("TShiftState",{comptype: $mod.$rtti["TShiftState$a"]});
  $mod.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",$mod.$rtti["TMouseButton"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseMoveEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TKeyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["AKey",rtl.word,1],["Shift",$mod.$rtti["TShiftState"]]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TKeyPressEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["AChar",rtl.char,1]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TTouchEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  this.TControlStyleValue = {"0": "csAcceptsControls", csAcceptsControls: 0};
  rtl.createClass($mod,"TMargins",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FOnChange = null;
      this.FLeft = 0;
      this.FTop = 0;
      this.FRight = 0;
      this.FBottom = 0;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetLeft = function (Value) {
      if (Value !== this.FLeft) {
        this.FLeft = Value;
        this.DoChange();
      };
    };
    this.SetTop = function (Value) {
      if (Value !== this.FTop) {
        this.FTop = Value;
        this.DoChange();
      };
    };
    this.SetRight = function (Value) {
      if (Value !== this.FRight) {
        this.FRight = Value;
        this.DoChange();
      };
    };
    this.SetBottom = function (Value) {
      if (Value !== this.FBottom) {
        this.FBottom = Value;
        this.DoChange();
      };
    };
    this.DoChange = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Create$1 = function () {
      this.FLeft = 3;
      this.FTop = 3;
      this.FBottom = 3;
      this.FRight = 3;
    };
    var $r = this.$rtti;
    $r.addProperty("Left",2,rtl.longint,"FLeft","SetLeft",{Default: 3});
    $r.addProperty("Top",2,rtl.longint,"FTop","SetTop",{Default: 3});
    $r.addProperty("Right",2,rtl.longint,"FRight","SetRight",{Default: 3});
    $r.addProperty("Bottom",2,rtl.longint,"FBottom","SetBottom",{Default: 3});
  });
  rtl.createClass($mod,"TControl",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FLayer = null;
      this.FCaptured = false;
      this.FControlCreated = false;
      this.FUpdateCount = 0;
      this.FBlockUpdateElement = false;
      this.FElement = null;
      this.FID = "";
      this.FNew = false;
      this.FContainer = null;
      this.FElementEvent = null;
      this.FElementClassName = "";
      this.FColor = 0;
      this.FFont = null;
      this.FParent = null;
      this.FPrevParent = null;
      this.FControls = [];
      this.FOnClick = null;
      this.FOnDblClick = null;
      this.FOnMouseDown = null;
      this.FOnMouseUp = null;
      this.FOnMouseMove = null;
      this.FOnKeyDown = null;
      this.FOnKeyUp = null;
      this.FOnKeyPress = null;
      this.FOnEnter = null;
      this.FOnExit = null;
      this.FEnabled = false;
      this.FHint = "";
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = false;
      this.FVisible = false;
      this.FWidth = 0;
      this.FHeight = 0;
      this.FTag$1 = 0;
      this.FLeft = 0;
      this.FTop = 0;
      this.FAlign = 0;
      this.FAnchors = {};
      this.FAlignWithMargins = false;
      this.FIsAligning = false;
      this.FOnMouseEnter = null;
      this.FOnMouseLeave = null;
      this.FCursor = 0;
      this.FControlStyle = {};
      this.FMargins = null;
      this.FOnMouseWheel = null;
      this.FParentColor = false;
      this.FParentFont = false;
      this.FOnTouchMove = null;
      this.FOnTouchStart = null;
      this.FOnTouchEnd = null;
      this.FLinkTouchEvents = false;
      this.FWidthStyle = 0;
      this.FHeightStyle = 0;
      this.FWidthPercent = 0;
      this.FHeightPercent = 0;
      this.FOrigRect = new pas.Types.TRect();
      this.FOrigParentRect = new pas.Types.TRect();
      this.FIsResizing = false;
      this.FShowFocus = false;
      this.FBorderWidth = 0;
      this.FOrigTop = 0;
      this.FOrigLeft = 0;
      this.FUpdateTopLeft = false;
      this.FEnablePropagation = false;
      this.FScriptLoaded = false;
      this.FRequiredScripts = null;
      this.FElementFont = 0;
      this.FElementPosition = 0;
    };
    this.$final = function () {
      this.FLayer = undefined;
      this.FElement = undefined;
      this.FContainer = undefined;
      this.FElementEvent = undefined;
      this.FFont = undefined;
      this.FParent = undefined;
      this.FPrevParent = undefined;
      this.FControls = undefined;
      this.FOnClick = undefined;
      this.FOnDblClick = undefined;
      this.FOnMouseDown = undefined;
      this.FOnMouseUp = undefined;
      this.FOnMouseMove = undefined;
      this.FOnKeyDown = undefined;
      this.FOnKeyUp = undefined;
      this.FOnKeyPress = undefined;
      this.FOnEnter = undefined;
      this.FOnExit = undefined;
      this.FAnchors = undefined;
      this.FOnMouseEnter = undefined;
      this.FOnMouseLeave = undefined;
      this.FControlStyle = undefined;
      this.FMargins = undefined;
      this.FOnMouseWheel = undefined;
      this.FOnTouchMove = undefined;
      this.FOnTouchStart = undefined;
      this.FOnTouchEnd = undefined;
      this.FOrigRect = undefined;
      this.FOrigParentRect = undefined;
      this.FRequiredScripts = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetControlsCount = function () {
      var Result = 0;
      Result = rtl.length(this.FControls);
      return Result;
    };
    this.GetControls = function (Index) {
      var Result = null;
      Result = this.FControls[Index];
      return Result;
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.VisibleChanging();
        this.FVisible = AValue;
        this.UpdateElement();
        this.DoRealign();
      };
    };
    this.SetWidth = function (AValue) {
      if (this.FWidth !== AValue) {
        this.FWidth = AValue;
        this.DoBoundsChange();
      };
    };
    this.SetHeight = function (AValue) {
      var dr = null;
      if (this.FHeight !== AValue) {
        if ((this.FAlign in rtl.createSet($mod.TAlign.alLeft,$mod.TAlign.alRight,$mod.TAlign.alClient)) && (this.GetElementHandle() != null)) {
          dr = this.GetElementHandle().getBoundingClientRect();
          if ((dr.top + AValue) >= window.innerHeight) {
            AValue = Math.round(window.innerHeight - dr.top);
            if (this.FAlignWithMargins) AValue = (AValue - this.FMargins.FBottom) - this.FMargins.FTop;
          };
        };
        this.FHeight = AValue;
        this.DoBoundsChange();
      };
    };
    this.SetHint = function (AValue) {
      if (this.FHint !== AValue) {
        this.FHint = AValue;
        this.UpdateElement();
      };
    };
    this.SetShowHint = function (AValue) {
      if (this.FShowHint !== AValue) {
        this.FShowHint = AValue;
        this.UpdateElement();
      };
    };
    this.SetTabOrder = function (AValue) {
      if (this.FTabOrder !== AValue) {
        this.FTabOrder = AValue;
        this.UpdateElement();
      };
    };
    this.SetTabStop = function (AValue) {
      if (this.FTabStop !== AValue) {
        this.FTabStop = AValue;
        this.UpdateElement();
      };
    };
    this.SetAlign = function (Value) {
      if (this.FAlign !== Value) {
        this.FAlign = Value;
        this.DoRealign();
      };
    };
    this.SetAlignWithMargins = function (Value) {
      if (this.FAlignWithMargins !== Value) {
        this.FAlignWithMargins = Value;
        this.DoRealign();
      };
    };
    this.SetCursor = function (Value) {
      this.FCursor = Value;
      this.UpdateElement();
    };
    this.GetBoundsRect = function () {
      var Result = new pas.Types.TRect();
      Result.Left = this.GetLeft();
      Result.Top = this.GetTop();
      Result.Right = this.GetLeft() + this.GetWidth();
      Result.Bottom = this.GetTop() + this.GetHeight();
      return Result;
    };
    this.SetID = function (Value) {
      this.FID = Value;
      if (this.FContainer != null) this.FContainer.setAttribute("id",Value);
    };
    this.SetMargins = function (Value) {
      this.FMargins.Assign(Value);
    };
    this.SetAnchors = function (Value) {
      if (rtl.neSet(this.FAnchors,Value)) {
        this.FAnchors = rtl.refSet(Value);
        this.UpdateAnchoring();
      };
    };
    this.GetElementEvent = function () {
      var Result = null;
      Result = this.FElementEvent;
      return Result;
    };
    this.SetHeightStyle = function (Value) {
      if (this.FHeightStyle !== Value) {
        this.FHeightStyle = Value;
        this.UpdateElementSize();
      };
    };
    this.SetWidthStyle = function (Value) {
      if (this.FWidthStyle !== Value) {
        this.FWidthStyle = Value;
        this.UpdateElementSize();
      };
    };
    this.GetIsLinked = function () {
      var Result = false;
      Result = !this.FNew;
      return Result;
    };
    this.SetElementFont = function (Value) {
      if (this.FElementFont !== Value) {
        this.FElementFont = Value;
        this.UpdateElement();
      };
    };
    this.SetElementPosition = function (Value) {
      if (this.FElementPosition !== Value) {
        this.FElementPosition = Value;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.SetEnabled = function (Value) {
      if (this.FEnabled !== Value) {
        this.FEnabled = Value;
        this.UpdateElement();
      };
    };
    this.SetBounds = function (X, Y, AWidth, AHeight) {
      if ((((X !== this.GetLeft()) || (Y !== this.GetTop())) || (AWidth !== this.GetWidth())) || (AHeight !== this.GetHeight())) {
        this.FBlockUpdateElement = true;
        this.SetLeft(X);
        this.SetTop(Y);
        this.SetWidth(AWidth);
        this.SetHeight(AHeight);
        this.Realign();
        this.FBlockUpdateElement = false;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.RecreateCanvas = function () {
    };
    this.VisibleChanging = function () {
    };
    this.GetWidth = function () {
      var Result = 0;
      Result = this.FWidth;
      if (((Result === -1) && (this.GetElementHandle() != null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) {
        Result = Math.round(this.GetElementHandle().offsetWidth);
      };
      return Result;
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = this.FHeight;
      if (((Result === -1) && (this.GetElementHandle() != null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) {
        Result = Math.round(this.GetElementHandle().offsetHeight);
      };
      return Result;
    };
    this.GetOuterWidth = function () {
      var Result = 0;
      Result = this.FWidth - this.FBorderWidth;
      return Result;
    };
    this.GetOuterHeight = function () {
      var Result = 0;
      Result = this.FHeight - this.FBorderWidth;
      return Result;
    };
    this.GetLeft = function () {
      var Result = 0;
      Result = this.FLeft;
      if (((Result === -1) && (this.GetElementHandle() != null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetLeft);
      return Result;
    };
    this.GetTop = function () {
      var Result = 0;
      Result = this.FTop;
      if (((Result === -1) && (this.GetElementHandle() != null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetTop);
      return Result;
    };
    this.CreateElement = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.ContainerElement = function () {
      var Result = null;
      Result = document.body;
      return Result;
    };
    this.BindElement = function () {
    };
    this.CreateInitialize = function () {
      this.FRequiredScripts = pas.Classes.TStringList.$create("Create$1");
    };
    this.ClearControls = function () {
      this.FControls = rtl.arraySetLength(this.FControls,null,0);
    };
    this.GetMouseEventButton = function (Event) {
      var Result = 0;
      Result = $mod.TMouseButton.mbLeft;
      var $tmp1 = Event.button;
      if ($tmp1 === 0) {
        Result = $mod.TMouseButton.mbLeft}
       else if ($tmp1 === 1) {
        Result = $mod.TMouseButton.mbMiddle}
       else if ($tmp1 === 2) Result = $mod.TMouseButton.mbRight;
      return Result;
    };
    this.GetMouseEventShiftState = function (Event) {
      var Result = {};
      Result = {};
      if (Event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssShift));
      if (Event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssCtrl));
      if (Event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssAlt));
      return Result;
    };
    this.GetKeyBoardEventShiftState = function (Event) {
      var Result = {};
      Result = {};
      if (Event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssShift));
      if (Event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssCtrl));
      if (Event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssAlt));
      return Result;
    };
    this.GetMouseWheelEventShiftState = function (Event) {
      var Result = {};
      Result = {};
      if (Event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssShift));
      if (Event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssCtrl));
      if (Event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssAlt));
      return Result;
    };
    this.GetTouchEventShiftState = function (Event) {
      var Result = {};
      Result = {};
      if (Event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssShift));
      if (Event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssCtrl));
      if (Event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftState$a.ssAlt));
      return Result;
    };
    this.HandleDoClick = function (Event) {
      var Result = false;
      this.FElementEvent = Event;
      this.StopPropagation();
      this.Click();
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoWheel = function (Event) {
      var Result = false;
      var ss = {};
      var h = false;
      this.FElementEvent = Event;
      this.StopPropagation();
      ss = rtl.refSet(this.GetMouseWheelEventShiftState(Event));
      h = true;
      this.MouseWheel(rtl.refSet(ss),pas.System.Trunc(-Event.deltaY),{get: function () {
          return h;
        }, set: function (v) {
          h = v;
        }});
      if (this.FOnMouseWheel != null) this.FOnMouseWheel(this,rtl.refSet(ss),pas.System.Trunc(-Event.deltaY),new pas.Types.TPoint(pas.Types.Point(0,0)),{get: function () {
          return h;
        }, set: function (v) {
          h = v;
        }});
      Result = h;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoDblClick = function (Event) {
      var Result = false;
      this.FElementEvent = Event;
      this.StopPropagation();
      this.DblClick();
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseDown = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      var mb = 0;
      this.FElementEvent = Event;
      this.StopPropagation();
      l = ((Event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((Event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p != null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(Event));
      mb = this.GetMouseEventButton(Event);
      this.MouseDown(mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseDown != null) this.FOnMouseDown(this,mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseUp = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      var mb = 0;
      this.FElementEvent = Event;
      this.StopPropagation();
      l = ((Event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((Event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p != null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(Event));
      mb = this.GetMouseEventButton(Event);
      this.MouseUp(mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseUp != null) this.FOnMouseUp(this,mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseMove = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      this.FElementEvent = Event;
      this.StopPropagation();
      l = ((Event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((Event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p != null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(Event));
      this.MouseMove(rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseMove != null) this.FOnMouseMove(this,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseLeave = function (Event) {
      var Result = false;
      if (this.Captured()) return Result;
      Event.stopPropagation();
      this.DoMouseLeave();
      if (this.FOnMouseLeave != null) this.FOnMouseLeave(this);
      Result = true;
      return Result;
    };
    this.HandleDoMouseEnter = function (Event) {
      var Result = false;
      if (this.Captured()) return Result;
      Event.stopPropagation();
      this.DoMouseEnter();
      if (this.FOnMouseEnter != null) this.FOnMouseEnter(this);
      Result = true;
      return Result;
    };
    this.HandleDoKeyDown = function (Event) {
      var Result = false;
      var k = 0;
      var ss = {};
      this.FElementEvent = Event;
      this.StopPropagation();
      k = this.GetKeyCode(Event.key);
      ss = rtl.refSet(this.GetKeyBoardEventShiftState(Event));
      this.KeyDown({get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      if (this.FOnKeyDown != null) this.FOnKeyDown(this,{get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoKeyUp = function (Event) {
      var Result = false;
      var k = 0;
      var ss = {};
      var c = "";
      this.FElementEvent = Event;
      this.StopPropagation();
      k = this.GetKeyCode(Event.key);
      ss = rtl.refSet(this.GetKeyBoardEventShiftState(Event));
      if (k === 27) {
        c = "\x1B";
        if (this.FOnKeyPress != null) this.FOnKeyPress(this,{get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
      };
      this.KeyUp({get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      if (this.FOnKeyUp != null) this.FOnKeyUp(this,{get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoKeyPress = function (Event) {
      var Result = false;
      var c = "";
      this.FElementEvent = Event;
      this.StopPropagation();
      if (this.IsKeyCharacter(Event.key)) {
        c = String.fromCharCode(this.GetKeyCode(Event.key));
        this.KeyPress({get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        if (this.FOnKeyPress != null) this.FOnKeyPress(this,{get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
      };
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoExit = function (Event) {
      var Result = false;
      this.DoExit();
      if (this.FOnExit != null) this.FOnExit(this);
      Result = true;
      return Result;
    };
    this.HandleDoEnter = function (Event) {
      var Result = false;
      this.FElementEvent = Event;
      this.StopPropagation();
      this.DoEnter();
      if (this.FOnEnter != null) this.FOnEnter(this);
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoTouchStart = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = Event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (Event.touches.length > 0) {
        touch = Event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p != null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(Event));
        if (this.FLinkTouchEvents) {
          this.MouseDown($mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseDown != null) this.FOnMouseDown(this,$mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchStart(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchStart != null) this.FOnTouchStart(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoTouchMove = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = Event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (Event.touches.length > 0) {
        touch = Event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p != null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(Event));
        if (this.FLinkTouchEvents) {
          this.MouseMove(rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseMove != null) this.FOnMouseMove(this,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchMove(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchMove != null) this.FOnTouchMove(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      this.FElementEvent = null;
      Result = true;
      return Result;
    };
    this.HandleDoTouchEnd = function (Event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = Event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (Event.touches.length > 0) {
        touch = Event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p != null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(Event));
        if (this.FLinkTouchEvents) {
          this.MouseUp($mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseUp != null) this.FOnMouseUp(this,$mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchEnd(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchEnd != null) this.FOnTouchEnd(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      this.FElementEvent = null;
      Result = true;
      return Result;
    };
    this.HandleFontChanged = function (Sender) {
      var i = 0;
      if (this.FUpdateCount === 0) {
        this.FParentFont = false;
        for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if (this.GetControls(i).FParentFont) {
            this.GetControls(i).FFont.Assign(this.FFont);
            this.GetControls(i).FontChanged();
          };
        };
      };
      this.FontChanged();
    };
    this.DoExit = function () {
    };
    this.DoEnter = function () {
    };
    this.Click = function () {
      if (this.FOnClick != null) this.FOnClick(this);
    };
    this.UpdateElement = function () {
      if (this.FBlockUpdateElement || (this.FUpdateCount > 0)) return;
      if (((this.GetElementHandle() != null) && (this.GetElementHandle() !== this.ContainerElement())) && !this.IsUpdating()) {
        if (!this.GetIsLinked()) this.UpdateElementVisual();
        this.UpdateElementData();
      };
    };
    this.UpdateElementSize = function () {
      var eh = null;
      if (this.FBlockUpdateElement || (this.FUpdateCount > 0)) return;
      if ((this.GetElementHandle() != null) && (this.GetElementHandle() !== this.ContainerElement())) {
        eh = this.GetElementHandle();
        if (this.FElementPosition === $mod.TElementPosition.epAbsolute) {
          if (this.FTop !== -1) {
            eh.style.setProperty("top",pas.SysUtils.IntToStr(this.FTop) + "px")}
           else eh.style.setProperty("top","");
          if (this.FLeft !== -1) {
            eh.style.setProperty("left",pas.SysUtils.IntToStr(this.FLeft) + "px")}
           else eh.style.setProperty("left","");
        };
        if (this.FWidthStyle === $mod.TSizeStyle.ssAbsolute) {
          if (this.FWidth !== -1) {
            eh.style.setProperty("width",pas.SysUtils.IntToStr(this.GetOuterWidth()) + "px")}
           else eh.style.setProperty("width","");
        };
        if (this.FWidthStyle === $mod.TSizeStyle.ssAuto) {
          eh.style.setProperty("width","");
        };
        if (this.FHeightStyle === $mod.TSizeStyle.ssAbsolute) {
          if (this.FHeight !== -1) {
            eh.style.setProperty("height",pas.SysUtils.IntToStr(this.GetOuterHeight()) + "px")}
           else eh.style.setProperty("height","");
        };
        if (this.FHeightStyle === $mod.TSizeStyle.ssAuto) {
          eh.style.setProperty("height","");
        };
        if (this.FWidthStyle === $mod.TSizeStyle.ssPercent) {
          if (this.FWidth !== -1) {
            eh.style.setProperty("width",pas.SysUtils.IntToStr(this.FWidthPercent) + "%")}
           else eh.style.setProperty("width","");
        };
        if (this.FHeightStyle === $mod.TSizeStyle.ssPercent) {
          if (this.FHeight !== -1) {
            eh.style.setProperty("height",pas.SysUtils.IntToStr(this.FHeightPercent) + "%")}
           else eh.style.setProperty("height","");
        };
        if (this.FElementPosition === $mod.TElementPosition.epAbsolute) {
          eh.style.setProperty("position","absolute")}
         else if (this.FElementPosition === $mod.TElementPosition.epRelative) {
          eh.style.setProperty("position","relative")}
         else eh.style.setProperty("position","");
      };
    };
    this.UpdateElementVisual = function () {
      var eh = null;
      eh = this.GetElementHandle();
      if (this.FParentFont && (this.FParent != null)) this.FFont.Assign(this.FParent.FFont);
      eh.style.setProperty("overflow","hidden");
      var $tmp1 = this.FCursor;
      if ($tmp1 === 0) {
        eh.style.setProperty("cursor","default")}
       else if ($tmp1 === 2) {
        eh.style.setProperty("cursor","auto")}
       else if ($tmp1 === 1) {
        eh.style.setProperty("cursor","none")}
       else if ($tmp1 === 3) {
        eh.style.setProperty("cursor","crosshair")}
       else if ($tmp1 === 4) {
        eh.style.setProperty("cursor","text")}
       else if ($tmp1 === 6) {
        eh.style.setProperty("cursor","nesw-resize")}
       else if ($tmp1 === 7) {
        eh.style.setProperty("cursor","ns-resize")}
       else if ($tmp1 === 8) {
        eh.style.setProperty("cursor","nwse-resize")}
       else if ($tmp1 === 9) {
        eh.style.setProperty("cursor","ew-resize")}
       else if ($tmp1 === 10) {
        eh.style.setProperty("cursor","")}
       else if ($tmp1 === 11) {
        eh.style.setProperty("cursor","wait")}
       else if ($tmp1 === 12) {
        eh.style.setProperty("cursor","")}
       else if ($tmp1 === 13) {
        eh.style.setProperty("cursor","no-drop")}
       else if ($tmp1 === 14) {
        eh.style.setProperty("cursor","col-resize")}
       else if ($tmp1 === 15) {
        eh.style.setProperty("cursor","row-resize")}
       else if ($tmp1 === 16) {
        eh.style.setProperty("cursor","")}
       else if ($tmp1 === 17) {
        eh.style.setProperty("cursor","progress")}
       else if ($tmp1 === 18) {
        eh.style.setProperty("cursor","not-allowed")}
       else if ($tmp1 === 19) {
        eh.style.setProperty("cursor","wait")}
       else if ($tmp1 === 20) {
        eh.style.setProperty("cursor","help")}
       else if ($tmp1 === 21) {
        eh.style.setProperty("cursor","pointer")}
       else if ($tmp1 === 22) eh.style.setProperty("cursor","move");
      if (this.Captured()) this.FLayer.style.setProperty("cursor",eh.style.getPropertyValue("cursor"));
      if ((this.FElementClassName !== "") || this.CanShowFocus()) {
        eh.style.setProperty("outline","")}
       else eh.style.setProperty("outline","none");
      this.UpdateElementSize();
      eh.style.setProperty("webkit-user-select","none");
      eh.style.setProperty("moz-user-select","none");
      eh.style.setProperty("khtml-user-select","none");
      eh.style.setProperty("ms-user-select","none");
      eh.style.setProperty("user-select","none");
      if ((this.FElementClassName === "") && (this.FElementFont === $mod.TElementFont.efProperty)) {
        if (this.FEnabled) eh.style.setProperty("color",pas["WEBLib.Graphics"].ColorToHTML(this.FFont.FColor));
        this.SetHTMLElementFont(eh,this.FFont);
      } else {
        eh.style.setProperty("color","");
        eh.style.setProperty("font-family","");
        eh.style.setProperty("font-style","");
        eh.style.setProperty("font-size","");
      };
    };
    this.UpdateElementData = function () {
      var eh = null;
      eh = this.GetElementHandle();
      if (this.FVisible) {
        eh.style.setProperty("visibility","")}
       else eh.style.setProperty("visibility","hidden");
      if (this.FTabStop) {
        this.FContainer.setAttribute("tabindex",pas.SysUtils.IntToStr(this.FTabOrder + 1))}
       else this.FContainer.setAttribute("tabindex","");
      if (this.FEnabled) {
        this.FContainer.removeAttribute("disabled")}
       else this.FContainer.setAttribute("disabled","disabled");
      if (this.FShowHint && (this.FHint !== "")) {
        this.FContainer.setAttribute("title",this.FHint)}
       else this.FContainer.setAttribute("title","");
    };
    this.UpdateParent = function () {
      this.InternalUpdateParent();
      if (((pas.Classes.TComponentStateItem.csLoading in this.FComponentState) && (this.FParent != null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FParent.FComponentState)) this.Loaded();
      this.UpdateChildren(this.FPrevParent);
      this.UpdateChildren(this.FParent);
    };
    this.InternalUpdateParent = function () {
      var p = null;
      p = this.FPrevParent;
      if (this.FNew) {
        this.CreateControl();
        if (this.FContainer != null) {
          if ((this.FParent != null) && !(this.FParent.FContainer != null)) this.FParent.CreateControl();
          if (((this.FParent !== p) && (p != null)) && (p.FContainer != null)) p.FContainer.removeChild(this.FContainer);
          if ((this.FParent != null) && (this.FParent.FContainer != null)) this.FParent.FContainer.appendChild(this.FContainer);
        };
      };
    };
    this.UpdateChildren = function (AControl) {
      var I = 0;
      var c = null;
      if (AControl != null) {
        AControl.DoRealign();
        if ($mod.TCustomControl.isPrototypeOf(AControl)) {
          rtl.as(AControl,$mod.TCustomControl).RecreateCanvas();
          rtl.as(AControl,$mod.TCustomControl).Invalidate();
        };
        for (var $l1 = 0, $end2 = AControl.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          c = AControl.GetControls(I);
          this.UpdateChildren(c);
        };
      };
    };
    this.PersistinHTML = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).PersistinHTML();
      };
    };
    this.InitFromHTML = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).InitFromHTML();
      };
    };
    this.DisableTab = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).DisableTab();
      };
    };
    this.SetElementClassName = function (AValue) {
      this.FElementClassName = AValue;
      if (this.FContainer != null) this.FContainer.setAttribute("class",AValue);
    };
    this.SetColor = function (AValue) {
      this.FColor = AValue;
      this.UpdateElement();
    };
    this.SetFont = function (AValue) {
      this.FFont.SetName(AValue.FName);
      this.FFont.SetSize(AValue.FSize);
      this.FFont.SetStyle(rtl.refSet(AValue.FStyle));
      this.FFont.SetColor(AValue.FColor);
    };
    this.SetParent = function (AValue) {
      if (this.FParent !== AValue) {
        if (this.FParent != null) this.FParent.UnRegisterParent(this);
        this.FPrevParent = this.FParent;
        this.FParent = AValue;
        if (this.FParent != null) this.FParent.RegisterParent(this);
        this.UpdateParent();
        this.UpdateElement();
        this.InitScript();
      };
    };
    this.RegisterParent = function (AValue) {
      this.FControls = rtl.arraySetLength(this.FControls,null,rtl.length(this.FControls) + 1);
      this.FControls[rtl.length(this.FControls) - 1] = AValue;
    };
    this.UnRegisterParent = function (AValue) {
      var i = 0;
      var flg = false;
      flg = false;
      for (var $l1 = 0, $end2 = rtl.length(this.FControls) - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (this.FControls[i] === AValue) {
          flg = true;
        };
        if (flg && (i < (rtl.length(this.FControls) - 1))) this.FControls[i] = this.FControls[i + 1];
      };
      if (flg) this.FControls = rtl.arraySetLength(this.FControls,null,rtl.length(this.FControls) - 1);
    };
    this.SetLeft = function (AValue) {
      if (this.FLeft !== AValue) {
        this.FLeft = AValue;
        if (!this.FUpdateTopLeft) this.FOrigLeft = AValue;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.SetTop = function (AValue) {
      if (this.FTop !== AValue) {
        this.FTop = AValue;
        if (!this.FUpdateTopLeft) this.FOrigTop = AValue;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.MouseUp = function (Button, Shift, X, Y) {
    };
    this.MouseDown = function (Button, Shift, X, Y) {
    };
    this.MouseMove = function (Shift, X, Y) {
    };
    this.TouchStart = function (X, Y) {
    };
    this.TouchMove = function (X, Y) {
    };
    this.TouchEnd = function (X, Y) {
    };
    this.DoMouseEnter = function () {
    };
    this.DoMouseLeave = function () {
    };
    this.MouseWheel = function (Shift, WheelDelta, Handled) {
    };
    this.DblClick = function () {
      if (this.FOnDblClick != null) this.FOnDblClick(this);
    };
    this.KeyDown = function (Key, Shift) {
    };
    this.KeyPress = function (Key) {
    };
    this.KeyUp = function (Key, Shift) {
    };
    this.DoMarginsChanged = function (Sender) {
      this.DoRealign();
    };
    this.DoRealign = function () {
      if ((this.FParent != null) && !this.FParent.IsUpdating()) {
        this.FParent.AlignControl(this.FParent);
      };
    };
    this.DoBoundsChange = function () {
      var b = false;
      var frm = null;
      this.UpdateElementSize();
      this.RecreateCanvas();
      if (this.FIsResizing) return;
      this.FIsResizing = true;
      this.InternalResize();
      this.DoRealign();
      b = false;
      frm = pas["WEBLib.Forms"].GetParentForm(this);
      if (frm != null) {
        b = frm.FIsResizing$1;
        b = b || (frm.FUpdateCount > 0);
      };
      b = b || (this.FUpdateCount > 0);
      b = b || this.FIsAligning;
      if (!b) this.UpdateChildAnchoring();
      this.FIsResizing = false;
    };
    this.IsFocused = function () {
      var Result = false;
      Result = this.FContainer === document.activeElement;
      return Result;
    };
    this.GetID = function () {
      var Result = "";
      Result = this.FID;
      return Result;
    };
    this.GetElementHandle = function () {
      var Result = null;
      Result = null;
      if ((this.FContainer != null) && this.FControlCreated) Result = this.FContainer;
      return Result;
    };
    this.GetElementBindHandle = function () {
      var Result = null;
      Result = this.GetElementHandle();
      return Result;
    };
    this.GetKeyCode = function (AValue) {
      var Result = 0;
      var i = 0;
      i = -1;
      var $tmp1 = AValue;
      if ($tmp1 === "Up") {
        i = 38}
       else if ($tmp1 === "Down") {
        i = 40}
       else if ($tmp1 === "Left") {
        i = 37}
       else if ($tmp1 === "Right") {
        i = 39}
       else if ($tmp1 === pas.Web.TJSKeyNames.BackSpace) {
        i = 8}
       else if ($tmp1 === pas.Web.TJSKeyNames.Tab) {
        i = 9}
       else if ($tmp1 === pas.Web.TJSKeyNames.Enter) {
        i = 13}
       else if ($tmp1 === pas.Web.TJSKeyNames.Shift) {
        i = 16}
       else if ($tmp1 === pas.Web.TJSKeyNames.Control) {
        i = 17}
       else if ($tmp1 === pas.Web.TJSKeyNames.Alt) {
        i = 18}
       else if ($tmp1 === pas.Web.TJSKeyNames.Pause) {
        i = 19}
       else if ($tmp1 === pas.Web.TJSKeyNames.CapsLock) {
        i = 20}
       else if ($tmp1 === pas.Web.TJSKeyNames.Escape) {
        i = 27}
       else if ($tmp1 === pas.Web.TJSKeyNames.PageUp) {
        i = 33}
       else if ($tmp1 === pas.Web.TJSKeyNames.PageDown) {
        i = 34}
       else if ($tmp1 === pas.Web.TJSKeyNames._End) {
        i = 35}
       else if ($tmp1 === pas.Web.TJSKeyNames.Home) {
        i = 36}
       else if ($tmp1 === pas.Web.TJSKeyNames.ArrowLeft) {
        i = 37}
       else if ($tmp1 === pas.Web.TJSKeyNames.ArrowUp) {
        i = 38}
       else if ($tmp1 === pas.Web.TJSKeyNames.ArrowRight) {
        i = 39}
       else if ($tmp1 === pas.Web.TJSKeyNames.ArrowDown) {
        i = 40}
       else if ($tmp1 === pas.Web.TJSKeyNames.Insert) {
        i = 45}
       else if ($tmp1 === pas.Web.TJSKeyNames.Delete) {
        i = 46}
       else if ($tmp1 === pas.Web.TJSKeyNames.F1) {
        i = 112}
       else if ($tmp1 === pas.Web.TJSKeyNames.F2) {
        i = 113}
       else if ($tmp1 === pas.Web.TJSKeyNames.F3) {
        i = 114}
       else if ($tmp1 === pas.Web.TJSKeyNames.F4) {
        i = 115}
       else if ($tmp1 === pas.Web.TJSKeyNames.F5) {
        i = 116}
       else if ($tmp1 === pas.Web.TJSKeyNames.F6) {
        i = 117}
       else if ($tmp1 === pas.Web.TJSKeyNames.F7) {
        i = 118}
       else if ($tmp1 === pas.Web.TJSKeyNames.F8) {
        i = 119}
       else if ($tmp1 === pas.Web.TJSKeyNames.F9) {
        i = 120}
       else if ($tmp1 === pas.Web.TJSKeyNames.F10) {
        i = 121}
       else if ($tmp1 === pas.Web.TJSKeyNames.F11) {
        i = 122}
       else if ($tmp1 === pas.Web.TJSKeyNames.F12) {
        i = 123}
       else if ($tmp1 === pas.Web.TJSKeyNames.F13) {
        i = 124}
       else if ($tmp1 === pas.Web.TJSKeyNames.F14) {
        i = 125}
       else if ($tmp1 === pas.Web.TJSKeyNames.F15) {
        i = 126}
       else if ($tmp1 === pas.Web.TJSKeyNames.F16) {
        i = 127}
       else if ($tmp1 === pas.Web.TJSKeyNames.F17) {
        i = 128}
       else if ($tmp1 === pas.Web.TJSKeyNames.F18) {
        i = 129}
       else if ($tmp1 === pas.Web.TJSKeyNames.F19) {
        i = 130}
       else if ($tmp1 === pas.Web.TJSKeyNames.F20) {
        i = 131}
       else {
        i = AValue.charCodeAt(1 - 1);
      };
      Result = i;
      return Result;
    };
    this.IsKeyCharacter = function (AValue) {
      var Result = false;
      var $tmp1 = AValue;
      if ((((((((((((((((((((((((((((((((((((((($tmp1 === "Up") || ($tmp1 === "Down")) || ($tmp1 === "Left")) || ($tmp1 === "Right")) || ($tmp1 === pas.Web.TJSKeyNames.Shift)) || ($tmp1 === pas.Web.TJSKeyNames.Control)) || ($tmp1 === pas.Web.TJSKeyNames.Alt)) || ($tmp1 === pas.Web.TJSKeyNames.Pause)) || ($tmp1 === pas.Web.TJSKeyNames.CapsLock)) || ($tmp1 === pas.Web.TJSKeyNames.PageUp)) || ($tmp1 === pas.Web.TJSKeyNames.PageDown)) || ($tmp1 === pas.Web.TJSKeyNames._End)) || ($tmp1 === pas.Web.TJSKeyNames.Home)) || ($tmp1 === pas.Web.TJSKeyNames.ArrowLeft)) || ($tmp1 === pas.Web.TJSKeyNames.ArrowUp)) || ($tmp1 === pas.Web.TJSKeyNames.ArrowRight)) || ($tmp1 === pas.Web.TJSKeyNames.ArrowDown)) || ($tmp1 === pas.Web.TJSKeyNames.Insert)) || ($tmp1 === pas.Web.TJSKeyNames.Delete)) || ($tmp1 === pas.Web.TJSKeyNames.F1)) || ($tmp1 === pas.Web.TJSKeyNames.F2)) || ($tmp1 === pas.Web.TJSKeyNames.F3)) || ($tmp1 === pas.Web.TJSKeyNames.F4)) || ($tmp1 === pas.Web.TJSKeyNames.F5)) || ($tmp1 === pas.Web.TJSKeyNames.F6)) || ($tmp1 === pas.Web.TJSKeyNames.F7)) || ($tmp1 === pas.Web.TJSKeyNames.F8)) || ($tmp1 === pas.Web.TJSKeyNames.F9)) || ($tmp1 === pas.Web.TJSKeyNames.F10)) || ($tmp1 === pas.Web.TJSKeyNames.F11)) || ($tmp1 === pas.Web.TJSKeyNames.F12)) || ($tmp1 === pas.Web.TJSKeyNames.F13)) || ($tmp1 === pas.Web.TJSKeyNames.F14)) || ($tmp1 === pas.Web.TJSKeyNames.F15)) || ($tmp1 === pas.Web.TJSKeyNames.F16)) || ($tmp1 === pas.Web.TJSKeyNames.F17)) || ($tmp1 === pas.Web.TJSKeyNames.F18)) || ($tmp1 === pas.Web.TJSKeyNames.F19)) || ($tmp1 === pas.Web.TJSKeyNames.F20)) {
        Result = false}
       else {
        Result = true;
      };
      return Result;
    };
    this.GetClientRect = function () {
      var Result = new pas.Types.TRect();
      Result = new pas.Types.TRect(pas.Types.Rect(0,0,this.GetWidth(),this.GetHeight()));
      return Result;
    };
    this.CreateControl = function () {
      if (!(this.FElement != null)) {
        this.FElement = this.CreateElement();
        if (this.FElement != null) {
          this.FControlCreated = true;
          this.FContainer = this.FElement;
          this.FContainer.setAttribute("id",this.GetID());
          this.FContainer.setAttribute("zindex","0");
          this.BindEvents();
          this.UpdateElement();
        };
      };
    };
    this.BindEvents = function () {
      var eh = null;
      if (this.GetElementBindHandle() != null) {
        eh = this.GetElementBindHandle();
        eh.addEventListener("wheel",rtl.createCallback(this,"HandleDoWheel"));
        eh.addEventListener("click",rtl.createCallback(this,"HandleDoClick"));
        eh.addEventListener("dblclick",rtl.createCallback(this,"HandleDoDblClick"));
        eh.addEventListener("mousedown",rtl.createCallback(this,"HandleDoMouseDown"));
        eh.addEventListener("mouseup",rtl.createCallback(this,"HandleDoMouseUp"));
        eh.addEventListener("mousemove",rtl.createCallback(this,"HandleDoMouseMove"));
        eh.addEventListener("mouseleave",rtl.createCallback(this,"HandleDoMouseLeave"));
        eh.addEventListener("mouseenter",rtl.createCallback(this,"HandleDoMouseEnter"));
        eh.addEventListener("keydown",rtl.createCallback(this,"HandleDoKeyDown"));
        eh.addEventListener("keyup",rtl.createCallback(this,"HandleDoKeyUp"));
        eh.addEventListener("keypress",rtl.createCallback(this,"HandleDoKeyPress"));
        eh.addEventListener("focus",rtl.createCallback(this,"HandleDoEnter"));
        eh.addEventListener("blur",rtl.createCallback(this,"HandleDoExit"));
        eh.addEventListener("touchstart",rtl.createCallback(this,"HandleDoTouchStart"));
        eh.addEventListener("touchmove",rtl.createCallback(this,"HandleDoTouchMove"));
        eh.addEventListener("touchend",rtl.createCallback(this,"HandleDoTouchEnd"));
      };
    };
    this.UnbindEvents = function () {
      var eh = null;
      if (this.GetElementBindHandle() != null) {
        eh = this.GetElementBindHandle();
        eh.removeEventListener("wheel",rtl.createCallback(this,"HandleDoWheel"));
        eh.removeEventListener("click",rtl.createCallback(this,"HandleDoClick"));
        eh.removeEventListener("dblclick",rtl.createCallback(this,"HandleDoDblClick"));
        eh.removeEventListener("mousedown",rtl.createCallback(this,"HandleDoMouseDown"));
        eh.removeEventListener("mouseup",rtl.createCallback(this,"HandleDoMouseUp"));
        eh.removeEventListener("mousemove",rtl.createCallback(this,"HandleDoMouseMove"));
        eh.removeEventListener("mouseleave",rtl.createCallback(this,"HandleDoMouseLeave"));
        eh.removeEventListener("mouseenter",rtl.createCallback(this,"HandleDoMouseEnter"));
        eh.removeEventListener("keydown",rtl.createCallback(this,"HandleDoKeyDown"));
        eh.removeEventListener("keyup",rtl.createCallback(this,"HandleDoKeyUp"));
        eh.removeEventListener("keypress",rtl.createCallback(this,"HandleDoKeyPress"));
        eh.removeEventListener("focus",rtl.createCallback(this,"HandleDoEnter"));
        eh.removeEventListener("blur",rtl.createCallback(this,"HandleDoExit"));
        eh.removeEventListener("touchstart",rtl.createCallback(this,"HandleDoTouchStart"));
        eh.removeEventListener("touchmove",rtl.createCallback(this,"HandleDoTouchMove"));
        eh.removeEventListener("touchend",rtl.createCallback(this,"HandleDoTouchEnd"));
      };
    };
    this.SetHTMLElementFont = function (he, Font) {
      var s = "";
      he.style.setProperty("font-family",Font.FName);
      he.style.setProperty("font-style","normal");
      if (pas["WEBLib.Graphics"].TFontStyle.fsBold in Font.FStyle) {
        he.style.setProperty("font-weight","bold")}
       else he.style.setProperty("font-weight","");
      if (pas["WEBLib.Graphics"].TFontStyle.fsItalic in Font.FStyle) he.style.setProperty("font-style","italic");
      s = "";
      if (pas["WEBLib.Graphics"].TFontStyle.fsUnderline in Font.FStyle) s = "underline";
      if (pas["WEBLib.Graphics"].TFontStyle.fsStrikeOut in Font.FStyle) {
        if (s !== "") s = s + " ";
        s = s + "line-through";
      };
      if (s !== "") he.style.setProperty("text-decoration",s);
      he.style.setProperty("font-size",pas.SysUtils.IntToStr(Font.FSize) + "pt");
    };
    this.AlignControls = function (AControl, Rect) {
      var Self = this;
      var j = 0;
      function DoPosition(Control, AAlign) {
        var dl = 0;
        var dt = 0;
        var dr = 0;
        var db = 0;
        Control.FUpdateTopLeft = true;
        if (Control.FAlignWithMargins) {
          dl = Control.FMargins.FLeft;
          dt = Control.FMargins.FTop;
          db = Control.FMargins.FBottom;
          dr = Control.FMargins.FRight;
        } else {
          dl = 0;
          dt = 0;
          db = 0;
          dr = 0;
        };
        var $tmp1 = AAlign;
        if ($tmp1 === $mod.TAlign.alTop) {
          Control.SetWidth(((Rect.get().Right - Rect.get().Left) - dl) - dr);
          Control.SetTop(Rect.get().Top + dt);
          Control.SetLeft(Rect.get().Left + dl);
          Rect.get().Top = ((Rect.get().Top + Control.GetHeight()) + dt) + db;
        } else if ($tmp1 === $mod.TAlign.alBottom) {
          Control.SetWidth((((Rect.get().Right - Rect.get().Left) - dl) - dr) - 1);
          Control.SetTop(((Rect.get().Bottom - Control.GetHeight()) - db) - 1);
          Control.SetLeft(Rect.get().Left + dl);
          Rect.get().Bottom = ((Rect.get().Bottom - Control.GetHeight()) - dt) - db;
        } else if ($tmp1 === $mod.TAlign.alLeft) {
          Control.SetHeight(((Rect.get().Bottom - Rect.get().Top) - dt) - db);
          Control.SetLeft(Rect.get().Left + dl);
          Control.SetTop(Rect.get().Top + dt);
          Rect.get().Left = ((Rect.get().Left + Control.GetWidth()) + dl) + dr;
        } else if ($tmp1 === $mod.TAlign.alRight) {
          Control.SetHeight((((Rect.get().Bottom - Rect.get().Top) - db) - dt) - 1);
          Control.SetLeft(((Rect.get().Right - Control.GetWidth()) - dr) - 2);
          Control.SetTop(Rect.get().Top + dt);
          Rect.get().Right = ((Rect.get().Right - Control.GetWidth()) - dr) - dl;
        } else if ($tmp1 === $mod.TAlign.alClient) {
          Control.SetTop(Rect.get().Top + dt);
          Control.SetLeft(Rect.get().Left + dl);
          Control.SetWidth(((Rect.get().Right - Rect.get().Left) - dl) - dr);
          Control.SetHeight(((Rect.get().Bottom - Rect.get().Top) - db) - dt);
          Rect.get().Left = 0;
          Rect.get().Top = 0;
          Rect.get().Right = 0;
          Rect.get().Bottom = 0;
        };
        Control.FUpdateTopLeft = false;
      };
      function DoAlign(AAlign) {
        var i = 0;
        var j = 0;
        var ins = 0;
        var il = null;
        il = pas.Classes.TList.$create("Create$1");
        for (var $l1 = 0, $end2 = Self.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if ((Self.GetControls(i).FAlign === AAlign) && Self.GetControls(i).FVisible) {
            ins = il.GetCount();
            for (var $l3 = il.GetCount() - 1; $l3 >= 0; $l3--) {
              j = $l3;
              var $tmp4 = AAlign;
              if ($tmp4 === $mod.TAlign.alTop) {
                if (Self.GetControls(i).FOrigTop < rtl.getObject(il.Get(j)).FOrigTop) ins = j}
               else if ($tmp4 === $mod.TAlign.alLeft) {
                if (Self.GetControls(i).FOrigLeft < rtl.getObject(il.Get(j)).FOrigLeft) ins = j}
               else if ($tmp4 === $mod.TAlign.alRight) {
                if ((Self.GetControls(i).FOrigLeft + Self.GetControls(i).GetWidth()) > (rtl.getObject(il.Get(j)).FOrigLeft + rtl.getObject(il.Get(j)).GetWidth())) ins = j}
               else if ($tmp4 === $mod.TAlign.alBottom) if ((Self.GetControls(i).FOrigTop + Self.GetControls(i).GetHeight()) > (rtl.getObject(il.Get(j)).FOrigTop + rtl.getObject(il.Get(j)).GetHeight())) ins = j;
            };
            il.Insert(ins,Self.GetControls(i));
          };
        };
        for (var $l5 = 0, $end6 = il.GetCount() - 1; $l5 <= $end6; $l5++) {
          i = $l5;
          DoPosition(rtl.getObject(il.Get(i)),AAlign);
        };
        il = rtl.freeLoc(il);
      };
      DoAlign($mod.TAlign.alTop);
      DoAlign($mod.TAlign.alBottom);
      DoAlign($mod.TAlign.alLeft);
      DoAlign($mod.TAlign.alRight);
      DoAlign($mod.TAlign.alClient);
      DoAlign($mod.TAlign.alCustom);
      for (var $l1 = 0, $end2 = Self.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        j = $l1;
        Self.GetControls(j).AlignControl(Self.GetControls(j));
      };
    };
    this.AlignControl = function (AControl) {
      var r = new pas.Types.TRect();
      var cr = null;
      var ovf = "";
      var ovfx = "";
      var ovfy = "";
      var eh = null;
      var el = null;
      var frm = null;
      var ctop = 0;
      if (this.FIsAligning) return;
      if (this.IsUpdating()) return;
      frm = pas["WEBLib.Forms"].GetParentForm(this);
      if ((frm != null) && frm.IsUpdating()) return;
      ctop = 0;
      if (((frm != null) && (frm.FFormContainer !== "")) && pas["WEBLib.Forms"].TCustomForm.isPrototypeOf(AControl)) {
        el = document.getElementById(frm.FFormContainer);
        if (el != null) {
          cr = el.getBoundingClientRect();
          ctop = Math.round(cr.top);
        };
      };
      this.FIsAligning = true;
      if (!(AControl != null)) return;
      eh = AControl.GetElementHandle();
      if (eh != null) {
        ovf = eh.style.getPropertyValue("overflow");
        ovfx = eh.style.getPropertyValue("overflow-x");
        ovfy = eh.style.getPropertyValue("overflow-y");
        eh.style.setProperty("overflow","hidden");
      };
      r = new pas.Types.TRect(this.GetClientRect());
      if ((r.Bottom + ctop) > window.innerHeight) r.Bottom = window.innerHeight - ctop;
      this.AlignControls(AControl,{get: function () {
          return r;
        }, set: function (v) {
          r = v;
        }});
      if (eh != null) {
        eh.style.setProperty("overflow",ovf);
        eh.style.setProperty("overflow-x",ovfx);
        eh.style.setProperty("overflow-y",ovfy);
      };
      this.FIsAligning = false;
    };
    this.InitAnchoring = function () {
      var i = 0;
      this.FOrigRect = new pas.Types.TRect(this.GetBoundsRect());
      if (this.FParent != null) this.FOrigParentRect = new pas.Types.TRect(this.FParent.GetBoundsRect());
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).InitAnchoring();
      };
    };
    this.InitScript = function () {
    };
    this.Realign = function () {
      this.AlignControl(this);
    };
    this.Loaded = function () {
      var i = 0;
      pas.Classes.TComponent.Loaded.apply(this,arguments);
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).Loaded();
      };
      var $tmp3 = this.FAlign;
      if ($tmp3 === $mod.TAlign.alClient) {
        if (this.FParent != null) this.SetBounds(0,0,this.FParent.FWidth,this.FParent.FHeight);
      };
      this.Resize();
      this.UpdateElement();
    };
    this.InternalResize = function () {
      if (pas.Classes.TComponentStateItem.csLoading in this.FComponentState) return;
      this.Resize();
    };
    this.FontChanged = function () {
      this.UpdateElement();
    };
    this.HookElement = function () {
      var el = null;
      var i = 0;
      el = document.getElementById(this.FID);
      this.FContainer = el;
      this.BindElement();
      this.BindEvents();
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).HookElement();
      };
    };
    this.CreateWithID = function (AID) {
      var el = null;
      this.FUpdateCount = 0;
      this.FControlCreated = false;
      this.FLinkTouchEvents = true;
      this.FIsResizing = false;
      if (((this.FOwner != null) && (pas.Classes.TComponentStateItem.csLoading in this.FOwner.FComponentState)) || !(this.FOwner != null)) this.Loading();
      this.FElementPosition = $mod.TElementPosition.epAbsolute;
      this.FWidthStyle = $mod.TSizeStyle.ssAbsolute;
      this.FHeightStyle = $mod.TSizeStyle.ssAbsolute;
      el = document.getElementById(AID);
      if (!(el != null)) {
        this.FContainer = null;
        this.FNew = true;
      } else {
        this.FContainer = el;
        this.FNew = false;
        this.FControlCreated = true;
        this.FElementPosition = $mod.TElementPosition.epRelative;
        this.BindElement();
        this.BindEvents();
      };
      this.FID = AID;
      this.FFont = pas["WEBLib.Graphics"].TFont.$create("Create$1");
      this.FFont.FOnChange = rtl.createCallback(this,"HandleFontChanged");
      this.FEnabled = true;
      this.FVisible = true;
      this.FLeft = 0;
      this.FTop = 0;
      this.FTabStop = true;
      this.FAlign = $mod.TAlign.alNone;
      this.FAlignWithMargins = false;
      this.FIsAligning = false;
      this.FParentFont = true;
      this.FAnchors = rtl.createSet($mod.TAnchorKind.akLeft,$mod.TAnchorKind.akTop);
      this.FOrigRect = new pas.Types.TRect(pas.Types.Rect(-1,-1,-1,-1));
      this.FShowFocus = false;
      this.FBorderWidth = 0;
      this.FEnablePropagation = false;
      this.FColor = 16777215;
      this.FMargins = $mod.TMargins.$create("Create$1");
      this.FMargins.FOnChange = rtl.createCallback(this,"DoMarginsChanged");
      this.FParent = null;
      this.FPrevParent = null;
      this.ClearControls();
      this.CreateInitialize();
    };
    this.UpdateAnchoring = function () {
      var dxr = 0;
      var dyr = 0;
      var dxo = 0;
      var dyo = 0;
      var dxw = 0;
      var dyw = 0;
      var br = new pas.Types.TRect();
      var r = new pas.Types.TRect();
      if (pas.Classes.TComponentStateItem.csLoading in this.FComponentState) return;
      if (!this.FControlCreated) return;
      if (this.FAlign !== $mod.TAlign.alNone) return;
      if ((this.FOrigRect.Left === -1) && (this.FOrigRect.Top === -1)) this.InitAnchoring();
      if ((this.FParent != null) && !pas["WEBLib.Forms"].TForm.isPrototypeOf(this)) {
        r = new pas.Types.TRect(this.FParent.GetBoundsRect());
        dxr = (r.Right - r.Left) - (this.FOrigParentRect.Right - this.FOrigParentRect.Left);
        dyr = (r.Bottom - r.Top) - (this.FOrigParentRect.Bottom - this.FOrigParentRect.Top);
        br = new pas.Types.TRect(this.FOrigRect);
        dxo = 0;
        dyo = 0;
        dxw = 0;
        dyw = 0;
        if ($mod.TAnchorKind.akRight in this.FAnchors) {
          if ($mod.TAnchorKind.akLeft in this.FAnchors) {
            dxw = dxr}
           else dxo = dxr;
        };
        if ($mod.TAnchorKind.akBottom in this.FAnchors) {
          if ($mod.TAnchorKind.akTop in this.FAnchors) {
            dyw = dyr}
           else dyo = dyr;
        };
        if (($mod.TAnchorKind.akBottom in this.FAnchors) || ($mod.TAnchorKind.akRight in this.FAnchors)) this.SetBounds(br.Left + dxo,br.Top + dyo,(br.Right - br.Left) + dxw,(br.Bottom - br.Top) + dyw);
      };
      this.UpdateChildAnchoring();
    };
    this.UpdateChildAnchoring = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).UpdateAnchoring();
      };
    };
    this.IsUpdating = function () {
      var Result = false;
      Result = this.FUpdateCount > 0;
      return Result;
    };
    this.CanShowFocus = function () {
      var Result = false;
      Result = this.FShowFocus;
      return Result;
    };
    this.Create$2 = function (ID) {
      this.CreateWithID(ID);
    };
    this.Create$1 = function (AOwner) {
      var s = "";
      var prefix = "";
      var frm = null;
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FScriptLoaded = false;
      this.FElementFont = $mod.TElementFont.efProperty;
      this.FElementPosition = $mod.TElementPosition.epAbsolute;
      s = this.$classname;
      pas.System.Delete({get: function () {
          return s;
        }, set: function (v) {
          s = v;
        }},1,1);
      prefix = "";
      frm = pas["WEBLib.Forms"].GetParentForm(this);
      if (frm != null) prefix = frm.$classname;
      this.CreateWithID((prefix + "_") + $impl.FindUniqueName(s));
    };
    this.Destroy = function () {
      var i = 0;
      var ctrl = null;
      this.UnbindEvents();
      for (var $l1 = this.GetControlsCount() - 1; $l1 >= 0; $l1--) {
        i = $l1;
        ctrl = this.GetControls(i);
        ctrl = rtl.freeLoc(ctrl);
      };
      if (((this.FContainer != null) && (this.FParent != null)) && (this.FParent.FContainer != null)) {
        this.FParent.FContainer.removeChild(this.FContainer);
        this.FContainer = null;
        this.FControlCreated = false;
      };
      rtl.free(this,"FRequiredScripts");
      rtl.free(this,"FMargins");
      rtl.free(this,"FFont");
      pas.Classes.TComponent.Destroy.apply(this,arguments);
    };
    this.Assign = function (Source) {
    };
    this.Resize = function () {
    };
    this.PreventDefault = function () {
      if (this.GetElementEvent() != null) this.GetElementEvent().preventDefault();
    };
    this.StopPropagation = function () {
      if ((this.GetElementEvent() != null) && !this.FEnablePropagation) this.GetElementEvent().stopPropagation();
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) this.UpdateElement();
      };
    };
    this.Invalidate = function () {
    };
    this.Captured = function () {
      var Result = false;
      Result = this.FCaptured && (this.FLayer != null);
      return Result;
    };
  });
  rtl.createClass($mod,"TWinControl",$mod.TControl,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,$mod.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,$mod.$rtti["TAnchors"],"FAnchors","SetAnchors");
    $r.addProperty("Cursor",2,rtl.longint,"FCursor","SetCursor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("HeightStyle",2,$mod.$rtti["TSizeStyle"],"FHeightStyle","SetHeightStyle");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Tag",0,rtl.longint,"FTag$1","FTag$1");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("WidthStyle",2,$mod.$rtti["TSizeStyle"],"FWidthStyle","SetWidthStyle");
    $r.addProperty("OnMouseDown",0,$mod.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,$mod.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,$mod.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseEnter",0,$mod.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,$mod.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnKeyUp",0,$mod.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnKeyDown",0,$mod.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,$mod.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnEnter",0,$mod.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,$mod.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
  rtl.createClass($mod,"TCustomControl",$mod.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FPixelRatio = 0.0;
      this.FPainting = false;
      this.FCanvas = null;
      this.FElementCanvas = null;
      this.FBorderStyle = 0;
      this.FBorderColor = 0;
      this.FCustomBorder = false;
    };
    this.$final = function () {
      this.FCanvas = undefined;
      this.FElementCanvas = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.GetPixelRatio = function () {
      var Result = 0.0;
      var res = 0.0;
      var ctx = document.createElement("canvas").getContext("2d"),
          dpr = window.devicePixelRatio || 1,
          bsr = ctx.webkitBackingStorePixelRatio ||
                ctx.mozBackingStorePixelRatio ||
                ctx.msBackingStorePixelRatio ||
                ctx.oBackingStorePixelRatio ||
                ctx.backingStorePixelRatio || 1;
      res = dpr / bsr;
      Result = res;
      return Result;
    };
    this.RecreateCanvas = function () {
      var px = 0.0;
      var el = null;
      var h = 0;
      var w = 0;
      if (this.FElementCanvas != null) {
        if (!this.FPainting) {
          el = document.getElementById(this.GetID() + "_Canvas");
          if (el != null) {
            this.FElementCanvas = el;
            rtl.free(this,"FCanvas");
            this.FCanvas = null;
          };
          px = this.GetPixelRatio();
          h = this.GetHeight() - this.GetCanvasHeightOffset();
          w = this.GetWidth() - this.GetCanvasWidthOffset();
          this.FElementCanvas.style.setProperty("height",pas.SysUtils.IntToStr(h) + "px");
          this.FElementCanvas.style.setProperty("width",pas.SysUtils.IntToStr(w) + "px");
          this.FElementCanvas.height = Math.round(h * px);
          this.FElementCanvas.width = Math.round(w * px);
          this.FElementCanvas.getContext("2d").scale(px,px);
        };
        if (!(this.FCanvas != null)) this.FCanvas = pas["WEBLib.Graphics"].TCanvas.$create("Create$1",[this.FElementCanvas]);
      };
    };
    this.SetBorderStyle = function (AValue) {
      if (this.FBorderStyle !== AValue) {
        this.FBorderStyle = AValue;
        this.UpdateElement();
      };
    };
    this.SetBorderColor = function (AValue) {
      if (this.FBorderColor !== AValue) {
        this.FBorderColor = AValue;
        this.UpdateElement();
      };
    };
    this.CreateControl = function () {
      $mod.TControl.CreateControl.apply(this,arguments);
      this.RecreateCanvas();
    };
    this.Loaded = function () {
      $mod.TControl.Loaded.apply(this,arguments);
      this.Invalidate();
    };
    this.UpdateElementVisual = function () {
      $mod.TControl.UpdateElementVisual.apply(this,arguments);
      if (this.GetElementHandle() != null) {
        if (this.FBorderStyle === $mod.TBorderStyle.bsSingle) {
          if (this.FCustomBorder) {
            this.GetElementHandle().style.setProperty("border-style","solid");
            this.GetElementHandle().style.setProperty("border-width","1px");
            this.GetElementHandle().style.setProperty("border-color",pas["WEBLib.Graphics"].ColorToHTML(this.FBorderColor));
          } else this.GetElementHandle().style.setProperty("border-style","");
        } else this.GetElementHandle().style.setProperty("border-style","none");
      };
    };
    this.CreateElement = function () {
      var Result = null;
      this.FElementCanvas = document.createElement("CANVAS");
      if ($mod.TControlStyleValue.csAcceptsControls in this.FControlStyle) {
        Result = document.createElement("SPAN");
        Result.appendChild(this.FElementCanvas);
        this.FElementCanvas.setAttribute("id",this.GetID() + "_Canvas");
        this.FElementCanvas.setAttribute("zindex","-1");
      } else Result = this.FElementCanvas;
      return Result;
    };
    this.GetCanvasHeightOffset = function () {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.GetCanvasWidthOffset = function () {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.BindEvents = function () {
      $mod.TControl.BindEvents.apply(this,arguments);
    };
    this.Paint = function () {
    };
    this.CreateInitialize = function () {
      $mod.TControl.CreateInitialize.apply(this,arguments);
      this.FBorderStyle = $mod.TBorderStyle.bsSingle;
      this.FBorderColor = 12632256;
      this.FPainting = false;
      this.FWidth = 100;
      this.FHeight = 25;
      this.FWidthPercent = 100;
      this.FHeightPercent = 100;
      this.FCustomBorder = false;
    };
    this.Destroy = function () {
      if (this.FCanvas != null) rtl.free(this,"FCanvas");
      $mod.TControl.Destroy.apply(this,arguments);
    };
    this.Resize = function () {
      $mod.TControl.Resize.apply(this,arguments);
      this.Invalidate();
    };
    this.Invalidate = function () {
      var px = 0.0;
      $mod.TControl.Invalidate.apply(this,arguments);
      if (this.FParent === null) return;
      px = this.GetPixelRatio();
      if (px !== this.FPixelRatio) this.RecreateCanvas();
      this.FPixelRatio = this.GetPixelRatio();
      this.FPainting = true;
      if (this.FCanvas != null) this.FCanvas.Clear();
      this.Paint();
      this.FPainting = false;
    };
  });
  rtl.createClass($mod,"TControlManager",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FInstanceCount = 0;
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.apply(this,arguments);
      this.FInstanceCount = 0;
    };
    this.GetInstanceNumber = function () {
      var Result = 0;
      this.FInstanceCount += 1;
      Result = this.FInstanceCount;
      return Result;
    };
  });
  rtl.createClass($mod,"TCSSCodeFragment",pas.Classes.TCollectionItem,function () {
    this.$init = function () {
      pas.Classes.TCollectionItem.$init.call(this);
      this.FControlClassname = "";
      this.FCSS = null;
    };
    this.$final = function () {
      this.FCSS = undefined;
      pas.Classes.TCollectionItem.$final.call(this);
    };
    this.SetCSS = function (Value) {
      this.FCSS.Assign(Value);
    };
    this.Create$1 = function (Collection) {
      pas.Classes.TCollectionItem.Create$1.apply(this,arguments);
      this.FCSS = pas.Classes.TStringList.$create("Create$1");
    };
    this.Destroy = function () {
      rtl.free(this,"FCSS");
      pas.Classes.TCollectionItem.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("ControlClassname",0,rtl.string,"FControlClassname","FControlClassname");
    $r.addProperty("CSS",2,pas.Classes.$rtti["TStringList"],"FCSS","SetCSS");
  });
  rtl.createClass($mod,"TCSSCodeFragments",pas.Classes.TOwnedCollection,function () {
    this.Create$3 = function (AOwner) {
      pas.Classes.TOwnedCollection.Create$2.call(this,AOwner,$mod.TCSSCodeFragment);
    };
  });
  rtl.createClass($mod,"TCSSCodeManager",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FCSSFragments = null;
    };
    this.$final = function () {
      this.FCSSFragments = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetCSSFragments = function (Value) {
      this.FCSSFragments.Assign(Value);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.apply(this,arguments);
      this.FCSSFragments = $mod.TCSSCodeFragments.$create("Create$3",[this]);
    };
    this.Destroy = function () {
      rtl.free(this,"FCSSFragments");
      pas.Classes.TComponent.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("CSSFragments",2,$mod.$rtti["TCSSCodeFragments"],"FCSSFragments","SetCSSFragments");
  });
  $mod.$init = function () {
    $impl.ControlManager = $mod.TControlManager.$create("Create$1",[null]);
  };
},["WEBLib.Forms","WEBLib.StdCtrls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ControlManager = null;
  $impl.FindUniqueName = function (Name) {
    var Result = "";
    Result = Name + pas.SysUtils.IntToStr($impl.ControlManager.GetInstanceNumber());
    return Result;
  };
});
rtl.module("WEBLib.ExtCtrls",["System","Classes","SysUtils","Types","WEBLib.Controls","WEBLib.StdCtrls","WEBLib.Graphics","Web"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TTimer",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FEnabled = false;
      this.FInterval = 0;
      this.FOnTimer = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FOnTimer = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetEnabled = function (Value) {
      if (Value !== this.FEnabled) {
        if (Value) {
          this.DoUpdateTimer()}
         else this.DoClearTimer();
        this.FEnabled = Value;
      };
    };
    this.SetInterval = function (Value) {
      if (Value !== this.FInterval) {
        if (this.FEnabled) {
          this.SetEnabled(false);
          try {
            this.FInterval = Value;
          } finally {
            this.SetEnabled(true);
          };
        } else this.FInterval = Value;
      };
    };
    this.DoUpdateTimer = function () {
      this.FID = window.setInterval(rtl.createCallback(this,"DoTimer"),this.FInterval);
    };
    this.DoClearTimer = function () {
      if (this.FID !== 0) {
        window.clearInterval(this.FID);
        this.FID = 0;
      };
    };
    this.DoTimer = function () {
      if (this.FOnTimer != null) this.FOnTimer(this);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FEnabled = true;
      this.FInterval = 1000;
      this.DoUpdateTimer();
    };
    this.Destroy = function () {
      this.SetEnabled(false);
      pas.Classes.TComponent.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("Interval",2,rtl.longint,"FInterval","SetInterval",{Default: 1000});
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag");
    $r.addProperty("OnTimer",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnTimer","FOnTimer");
  });
  rtl.createClass($mod,"TWebTimer",$mod.TTimer,function () {
  });
  rtl.createClass($mod,"TCustomPanel",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FAutoSize = false;
      this.FCaption = "";
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("SPAN");
      return Result;
    };
    this.SetBorderStyle = function (AValue) {
      pas["WEBLib.Controls"].TCustomControl.SetBorderStyle.apply(this,arguments);
      this.UpdateElement();
    };
    this.SetAutoSize = function (AValue) {
      if (this.FAutoSize !== AValue) {
        this.FAutoSize = AValue;
        if (this.FAutoSize) {
          this.SetWidth(-1);
          this.SetHeight(-1);
        };
        this.UpdateElement();
      };
    };
    this.UpdateElementVisual = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElementVisual.apply(this,arguments);
      if (this.GetElementHandle() != null) {
        if (this.FAutoSize) {
          this.GetElementHandle().style.setProperty("overflow","");
          this.GetElementHandle().style.setProperty("white-space","normal");
          this.GetElementHandle().style.setProperty("display","inline");
        } else {
          this.GetElementHandle().style.setProperty("overflow","hidden");
          this.GetElementHandle().style.setProperty("white-space","nowrap");
          this.GetElementHandle().style.setProperty("display","inline-block");
        };
        this.GetElementHandle().style.setProperty("background-color",pas["WEBLib.Graphics"].ColorToHTML(this.FColor));
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FAutoSize = false;
      this.SetColor(15790320);
      this.SetTabStop(false);
      this.FCustomBorder = true;
    };
  });
  rtl.createClass($mod,"TPanel",$mod.TCustomPanel,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchors"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("BorderColor",2,rtl.longint,"FBorderColor","SetBorderColor",{Default: 12632256});
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle",{Default: pas["WEBLib.Controls"].TBorderStyle.bsSingle});
    $r.addProperty("Caption",0,rtl.string,"FCaption","FCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("ElementFont",2,pas["WEBLib.Controls"].$rtti["TElementFont"],"FElementFont","SetElementFont",{Default: pas["WEBLib.Controls"].TElementFont.efProperty});
    $r.addProperty("ElementPosition",2,pas["WEBLib.Controls"].$rtti["TElementPosition"],"FElementPosition","SetElementPosition",{Default: pas["WEBLib.Controls"].TElementPosition.epAbsolute});
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
  });
  rtl.createClass($mod,"TWebPanel",$mod.TPanel,function () {
  });
});
rtl.module("WEBLib.WebTools",["System"],function () {
  "use strict";
  var $mod = this;
  this.GetLocaleShortDateFormat = function (ALocale) {
    var Result = "";
    var res = "";
    var formats = {
         "ar-SA" : "dd/MM/yy",
         "bg-BG" : "dd.M.yyyy",
         "ca-ES" : "dd/MM/yyyy",
         "zh-TW" : "yyyy/M/d",
         "cs-CZ" : "d.M.yyyy",
         "da-DK" : "dd-MM-yyyy",
         "de-DE" : "dd.MM.yyyy",
         "el-GR" : "d/M/yyyy",
         "en-US" : "M/d/yyyy",
         "fi-FI" : "d.M.yyyy",
         "fr-FR" : "dd/MM/yyyy",
         "he-IL" : "dd/MM/yyyy",
         "hu-HU" : "yyyy. MM. dd.",
         "is-IS" : "d.M.yyyy",
         "it-IT" : "dd/MM/yyyy",
         "ja-JP" : "yyyy/MM/dd",
         "ko-KR" : "yyyy-MM-dd",
         "nl-NL" : "d-M-yyyy",
         "nb-NO" : "dd.MM.yyyy",
         "pl-PL" : "yyyy-MM-dd",
         "pt-BR" : "d/M/yyyy",
         "ro-RO" : "dd.MM.yyyy",
         "ru-RU" : "dd.MM.yyyy",
         "hr-HR" : "d.M.yyyy",
         "sk-SK" : "d. M. yyyy",
         "sq-AL" : "yyyy-MM-dd",
         "sv-SE" : "yyyy-MM-dd",
         "th-TH" : "d/M/yyyy",
         "tr-TR" : "dd.MM.yyyy",
         "ur-PK" : "dd/MM/yyyy",
         "id-ID" : "dd/MM/yyyy",
         "uk-UA" : "dd.MM.yyyy",
         "be-BY" : "dd.MM.yyyy",
         "sl-SI" : "d.M.yyyy",
         "et-EE" : "d.MM.yyyy",
         "lv-LV" : "yyyy.MM.dd.",
         "lt-LT" : "yyyy.MM.dd",
         "fa-IR" : "MM/dd/yyyy",
         "vi-VN" : "dd/MM/yyyy",
         "hy-AM" : "dd.MM.yyyy",
         "az-Latn-AZ" : "dd.MM.yyyy",
         "eu-ES" : "yyyy/MM/dd",
         "mk-MK" : "dd.MM.yyyy",
         "af-ZA" : "yyyy/MM/dd",
         "ka-GE" : "dd.MM.yyyy",
         "fo-FO" : "dd-MM-yyyy",
         "hi-IN" : "dd-MM-yyyy",
         "ms-MY" : "dd/MM/yyyy",
         "kk-KZ" : "dd.MM.yyyy",
         "ky-KG" : "dd.MM.yy",
         "sw-KE" : "M/d/yyyy",
         "uz-Latn-UZ" : "dd/MM yyyy",
         "tt-RU" : "dd.MM.yyyy",
         "pa-IN" : "dd-MM-yy",
         "gu-IN" : "dd-MM-yy",
         "ta-IN" : "dd-MM-yyyy",
         "te-IN" : "dd-MM-yy",
         "kn-IN" : "dd-MM-yy",
         "mr-IN" : "dd-MM-yyyy",
         "sa-IN" : "dd-MM-yyyy",
         "mn-MN" : "yy.MM.dd",
         "gl-ES" : "dd/MM/yy",
         "kok-IN" : "dd-MM-yyyy",
         "syr-SY" : "dd/MM/yyyy",
         "dv-MV" : "dd/MM/yy",
         "ar-IQ" : "dd/MM/yyyy",
         "zh-CN" : "yyyy/M/d",
         "de-CH" : "dd.MM.yyyy",
         "en-GB" : "dd/MM/yyyy",
         "es-MX" : "dd/MM/yyyy",
         "fr-BE" : "d/MM/yyyy",
         "it-CH" : "dd.MM.yyyy",
         "nl-BE" : "d/MM/yyyy",
         "nn-NO" : "dd.MM.yyyy",
         "pt-PT" : "dd-MM-yyyy",
         "sr-Latn-CS" : "d.M.yyyy",
         "sv-FI" : "d.M.yyyy",
         "az-Cyrl-AZ" : "dd.MM.yyyy",
         "ms-BN" : "dd/MM/yyyy",
         "uz-Cyrl-UZ" : "dd.MM.yyyy",
         "ar-EG" : "dd/MM/yyyy",
         "zh-HK" : "d/M/yyyy",
         "de-AT" : "dd.MM.yyyy",
         "en-AU" : "d/MM/yyyy",
         "es-ES" : "dd/MM/yyyy",
         "fr-CA" : "yyyy-MM-dd",
         "sr-Cyrl-CS" : "d.M.yyyy",
         "ar-LY" : "dd/MM/yyyy",
         "zh-SG" : "d/M/yyyy",
         "de-LU" : "dd.MM.yyyy",
         "en-CA" : "dd/MM/yyyy",
         "es-GT" : "dd/MM/yyyy",
         "fr-CH" : "dd.MM.yyyy",
         "ar-DZ" : "dd-MM-yyyy",
         "zh-MO" : "d/M/yyyy",
         "de-LI" : "dd.MM.yyyy",
         "en-NZ" : "d/MM/yyyy",
         "es-CR" : "dd/MM/yyyy",
         "fr-LU" : "dd/MM/yyyy",
         "ar-MA" : "dd-MM-yyyy",
         "en-IE" : "dd/MM/yyyy",
         "es-PA" : "MM/dd/yyyy",
         "fr-MC" : "dd/MM/yyyy",
         "ar-TN" : "dd-MM-yyyy",
         "en-ZA" : "yyyy/MM/dd",
         "es-DO" : "dd/MM/yyyy",
         "ar-OM" : "dd/MM/yyyy",
         "en-JM" : "dd/MM/yyyy",
         "es-VE" : "dd/MM/yyyy",
         "ar-YE" : "dd/MM/yyyy",
         "en-029" : "MM/dd/yyyy",
         "es-CO" : "dd/MM/yyyy",
         "ar-SY" : "dd/MM/yyyy",
         "en-BZ" : "dd/MM/yyyy",
         "es-PE" : "dd/MM/yyyy",
         "ar-JO" : "dd/MM/yyyy",
         "en-TT" : "dd/MM/yyyy",
         "es-AR" : "dd/MM/yyyy",
         "ar-LB" : "dd/MM/yyyy",
         "en-ZW" : "M/d/yyyy",
         "es-EC" : "dd/MM/yyyy",
         "ar-KW" : "dd/MM/yyyy",
         "en-PH" : "M/d/yyyy",
         "es-CL" : "dd-MM-yyyy",
         "ar-AE" : "dd/MM/yyyy",
         "es-UY" : "dd/MM/yyyy",
         "ar-BH" : "dd/MM/yyyy",
         "es-PY" : "dd/MM/yyyy",
         "ar-QA" : "dd/MM/yyyy",
         "es-BO" : "dd/MM/yyyy",
         "es-SV" : "dd/MM/yyyy",
         "es-HN" : "dd/MM/yyyy",
         "es-NI" : "dd/MM/yyyy",
         "es-PR" : "dd/MM/yyyy",
         "am-ET" : "d/M/yyyy",
         "tzm-Latn-DZ" : "dd-MM-yyyy",
         "iu-Latn-CA" : "d/MM/yyyy",
         "sma-NO" : "dd.MM.yyyy",
         "mn-Mong-CN" : "yyyy/M/d",
         "gd-GB" : "dd/MM/yyyy",
         "en-MY" : "d/M/yyyy",
         "prs-AF" : "dd/MM/yy",
         "bn-BD" : "dd-MM-yy",
         "wo-SN" : "dd/MM/yyyy",
         "rw-RW" : "M/d/yyyy",
         "qut-GT" : "dd/MM/yyyy",
         "sah-RU" : "MM.dd.yyyy",
         "gsw-FR" : "dd/MM/yyyy",
         "co-FR" : "dd/MM/yyyy",
         "oc-FR" : "dd/MM/yyyy",
         "mi-NZ" : "dd/MM/yyyy",
         "ga-IE" : "dd/MM/yyyy",
         "se-SE" : "yyyy-MM-dd",
         "br-FR" : "dd/MM/yyyy",
         "smn-FI" : "d.M.yyyy",
         "moh-CA" : "M/d/yyyy",
         "arn-CL" : "dd-MM-yyyy",
         "ii-CN" : "yyyy/M/d",
         "dsb-DE" : "d. M. yyyy",
         "ig-NG" : "d/M/yyyy",
         "kl-GL" : "dd-MM-yyyy",
         "lb-LU" : "dd/MM/yyyy",
         "ba-RU" : "dd.MM.yy",
         "nso-ZA" : "yyyy/MM/dd",
         "quz-BO" : "dd/MM/yyyy",
         "yo-NG" : "d/M/yyyy",
         "ha-Latn-NG" : "d/M/yyyy",
         "fil-PH" : "M/d/yyyy",
         "ps-AF" : "dd/MM/yy",
         "fy-NL" : "d-M-yyyy",
         "ne-NP" : "M/d/yyyy",
         "se-NO" : "dd.MM.yyyy",
         "iu-Cans-CA" : "d/M/yyyy",
         "sr-Latn-RS" : "d.M.yyyy",
         "si-LK" : "yyyy-MM-dd",
         "sr-Cyrl-RS" : "d.M.yyyy",
         "lo-LA" : "dd/MM/yyyy",
         "km-KH" : "yyyy-MM-dd",
         "cy-GB" : "dd/MM/yyyy",
         "bo-CN" : "yyyy/M/d",
         "sms-FI" : "d.M.yyyy",
         "as-IN" : "dd-MM-yyyy",
         "ml-IN" : "dd-MM-yy",
         "en-IN" : "dd-MM-yyyy",
         "or-IN" : "dd-MM-yy",
         "bn-IN" : "dd-MM-yy",
         "tk-TM" : "dd.MM.yy",
         "bs-Latn-BA" : "d.M.yyyy",
         "mt-MT" : "dd/MM/yyyy",
         "sr-Cyrl-ME" : "d.M.yyyy",
         "se-FI" : "d.M.yyyy",
         "zu-ZA" : "yyyy/MM/dd",
         "xh-ZA" : "yyyy/MM/dd",
         "tn-ZA" : "yyyy/MM/dd",
         "hsb-DE" : "d. M. yyyy",
         "bs-Cyrl-BA" : "d.M.yyyy",
         "tg-Cyrl-TJ" : "dd.MM.yy",
         "sr-Latn-BA" : "d.M.yyyy",
         "smj-NO" : "dd.MM.yyyy",
         "rm-CH" : "dd/MM/yyyy",
         "smj-SE" : "yyyy-MM-dd",
         "quz-EC" : "dd/MM/yyyy",
         "quz-PE" : "dd/MM/yyyy",
         "hr-BA" : "d.M.yyyy.",
         "sr-Latn-ME" : "d.M.yyyy",
         "sma-SE" : "yyyy-MM-dd",
         "en-SG" : "d/M/yyyy",
         "ug-CN" : "yyyy-M-d",
         "sr-Cyrl-BA" : "d.M.yyyy",
         "es-US" : "M/d/yyyy"
      };
        var loc = "";
        if (ALocale == '') { loc = navigator.language; }
        else { loc = ALocale; }
    
        res = formats[loc] || 'dd/MM/yyyy';
    Result = res;
    return Result;
  };
  this.GetLocaleLongDayName = function (DayOfWeek, ALocale) {
    var Result = "";
    var res = "";
    var baseDate = new Date(Date.UTC(2017, 0, 1)); // Sunday
        baseDate.setDate(baseDate.getDate() + DayOfWeek - 1);
    
        var loc = "";
        if (ALocale == '') { loc = navigator.language; }
        else { loc = ALocale; }
    
        res = baseDate.toLocaleDateString(loc, { weekday: 'long' });
    Result = res;
    return Result;
  };
  this.GetLocaleShortDayName = function (DayOfWeek, ALocale) {
    var Result = "";
    var res = "";
    var baseDate = new Date(Date.UTC(2017, 0, 1)); // Sunday
        baseDate.setDate(baseDate.getDate() + DayOfWeek - 1);
    
        var loc = "";
        if (ALocale == '') { loc = navigator.language; }
        else { loc = ALocale; }
    
        res = baseDate.toLocaleDateString(loc, { weekday: 'short' });
    Result = res;
    return Result;
  };
  this.GetLocaleLongMonthName = function (Month, ALocale) {
    var Result = "";
    var res = "";
    var baseDate = new Date(Date.UTC(2017, 0, 1)); // January
        baseDate.setMonth(baseDate.getMonth() + Month - 1);
    
        var loc = "";
        if (ALocale == '') { loc = navigator.language; }
        else { loc = ALocale; }
    
        res = baseDate.toLocaleDateString(loc, { month: 'long' });
    Result = res;
    return Result;
  };
  this.GetLocaleShortMonthName = function (Month, ALocale) {
    var Result = "";
    var res = "";
    var baseDate = new Date(Date.UTC(2017, 0, 1)); // January
        baseDate.setMonth(baseDate.getMonth() + Month - 1);
    
        var loc = "";
        if (ALocale == '') { loc = navigator.language; }
        else { loc = ALocale; }
    
        res = baseDate.toLocaleDateString(loc, { month: 'short' });
    Result = res;
    return Result;
  };
  this.GetLocaleDecimalSeparator = function () {
    var Result = "";
    var res = "";
    var n = 1.1;
    n = n.toLocaleString().substring(1, 2);
    res = n;
    Result = res;
    return Result;
  };
  this.GetBrowserLocale = function () {
    var Result = "";
    var res = "";
    res = navigator.language;
    Result = res;
    return Result;
  };
},["SysUtils"]);
rtl.module("WEBLib.Dialogs",["System","Classes","WEBLib.Controls","Web","SysUtils","WEBLib.ExtCtrls","WEBLib.WebTools"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TMsgDlgType = {"0": "mtWarning", mtWarning: 0, "1": "mtError", mtError: 1, "2": "mtInformation", mtInformation: 2, "3": "mtConfirmation", mtConfirmation: 3, "4": "mtCustom", mtCustom: 4};
  rtl.createClass($mod,"TMessageDlg",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FOpacity = 0.0;
      this.FMessage = "";
      this.FMsgDlgType = 0;
      this.FOnButtonClick = null;
      this.FOnClose = null;
    };
    this.$final = function () {
      this.FOnButtonClick = undefined;
      this.FOnClose = undefined;
      pas["WEBLib.Controls"].TCustomControl.$final.call(this);
    };
    this.BindEvents = function () {
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FOpacity = 0.2;
      this.FMessage = "";
      this.FMsgDlgType = $mod.TMsgDlgType.mtInformation;
    };
    var $r = this.$rtti;
    $r.addProperty("Opacity",0,rtl.double,"FOpacity","FOpacity");
    $r.addProperty("OnButtonClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnButtonClick","FOnButtonClick");
    $r.addProperty("OnClose",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClose","FOnClose");
  });
  $mod.$init = function () {
    $impl.WebLibDlg = null;
  };
},["WEBLib.Forms"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.WebLibDlg = null;
});
rtl.module("WEBLib.Forms",["System","Classes","SysUtils","WEBLib.Graphics","WEBLib.Controls","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.mrNone = 0;
  this.TCloseAction = {"0": "caNone", caNone: 0, "1": "caHide", caHide: 1, "2": "caFree", caFree: 2, "3": "caMinimize", caMinimize: 3};
  $mod.$rtti.$Enum("TCloseAction",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TCloseAction});
  this.TFormStyle = {"0": "fsNormal", fsNormal: 0, "1": "fsStayOnTop", fsStayOnTop: 1};
  $mod.$rtti.$Enum("TFormStyle",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TFormStyle});
  $mod.$rtti.$MethodVar("TCloseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Action",$mod.$rtti["TCloseAction"],1]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TCloseQueryEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CanClose",rtl.boolean,1]]), methodkind: 0});
  rtl.createClass($mod,"TCustomForm",pas["WEBLib.Controls"].TWinControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TWinControl.$init.call(this);
      this.FLayer$1 = null;
      this.FPopup = false;
      this.FFormFileName = "";
      this.FFormContent = "";
      this.FFormContainer = "";
      this.FFormElement = "";
      this.FFormStyle = 0;
      this.FOnCreate = null;
      this.FOnResize = null;
      this.FOnShow = null;
      this.FModalResult = 0;
      this.FOnPaint = null;
      this.FOnDeactivate = null;
      this.FOnCloseQuery = null;
      this.FOnClose = null;
      this.FModalProc = null;
      this.FCaption = "";
      this.FIsResizing$1 = false;
      this.FOnScroll = null;
      this.FOnUnload = null;
      this.LCLVersion = "";
      this.ClientHeight = 0;
      this.ClientWidth = 0;
    };
    this.$final = function () {
      this.FLayer$1 = undefined;
      this.FOnCreate = undefined;
      this.FOnResize = undefined;
      this.FOnShow = undefined;
      this.FOnPaint = undefined;
      this.FOnDeactivate = undefined;
      this.FOnCloseQuery = undefined;
      this.FOnClose = undefined;
      this.FModalProc = undefined;
      this.FOnScroll = undefined;
      this.FOnUnload = undefined;
      pas["WEBLib.Controls"].TWinControl.$final.call(this);
    };
    this.DoResize = function (Event) {
      var Result = false;
      this.CreateControl();
      this.Resize$1();
      Result = true;
      return Result;
    };
    this.DoLoaded = function (Event) {
      var Result = false;
      this.Resize$1();
      this.InitAnchoring();
      Result = true;
      return Result;
    };
    this.DoScroll = function (Event) {
      var Result = false;
      if (this.FOnScroll != null) this.FOnScroll(this);
      Result = true;
      return Result;
    };
    this.DoUnload = function (Event) {
      var Result = false;
      if (this.FOnUnload != null) this.FOnUnload(this);
      Result = true;
      return Result;
    };
    this.GetFormStyle = function () {
      var Result = 0;
      Result = this.FFormStyle;
      return Result;
    };
    this.SetFormStyle = function (Value) {
      this.FFormStyle = Value;
    };
    this.SetCaption = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.UpdateElement();
      };
    };
    this.Resize$1 = function () {
      var i = 0;
      this.FIsResizing$1 = true;
      this.AlignControl(this);
      if (this.FOnResize != null) this.FOnResize(this);
      this.FIsResizing$1 = false;
      if (!this.IsUpdating()) {
        if ((this.FOrigRect.Left === -1) && (this.FOrigRect.Top === -1)) this.InitAnchoring();
        for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          this.GetControls(i).UpdateAnchoring();
        };
      };
    };
    this.Loaded = function () {
      pas["WEBLib.Controls"].TControl.Loaded.apply(this,arguments);
    };
    this.DoClose = function (CloseAction) {
      if ((this.FModalProc != null) && (CloseAction.get() !== $mod.TCloseAction.caNone)) {
        this.FModalProc(this.FModalResult);
        this.FModalProc = null;
      };
    };
    this.BindEvents = function () {
      pas["WEBLib.Controls"].TControl.BindEvents.apply(this,arguments);
      window.addEventListener("resize",rtl.createCallback(this,"DoResize"));
      window.addEventListener("load",rtl.createCallback(this,"DoLoaded"));
      document.addEventListener("scroll",rtl.createCallback(this,"DoScroll"));
      window.addEventListener("unload",rtl.createCallback(this,"DoUnload"));
    };
    this.UnbindEvents = function () {
      pas["WEBLib.Controls"].TControl.UnbindEvents.apply(this,arguments);
      if (this.FLayer$1 != null) this.FLayer$1.removeEventListener("click",rtl.createCallback(this,"HandleDoClick$1"));
      window.removeEventListener("resize",rtl.createCallback(this,"DoResize"));
      window.removeEventListener("load",rtl.createCallback(this,"DoLoaded"));
      document.removeEventListener("scroll",rtl.createCallback(this,"DoScroll"));
      window.removeEventListener("unload",rtl.createCallback(this,"DoUnload"));
    };
    this.DoExit = function () {
      pas["WEBLib.Controls"].TControl.DoExit.apply(this,arguments);
      if (this.FPopup) this.Close();
    };
    this.DoCreate = function () {
      this.BeginUpdate();
      this.LoadDFMValues();
      this.Loaded();
      if (this.FOnCreate != null) this.FOnCreate(this);
      this.EndUpdate();
      this.AlignControl(this);
      if (this.FOnShow != null) this.FOnShow(this);
    };
    this.HandleDoClick$1 = function (Event) {
      var Result = false;
      Event.stopPropagation();
      this.Close();
      Result = true;
      return Result;
    };
    this.GetWidth = function () {
      var Result = 0;
      var d = 0;
      var dr = null;
      var s = "";
      var el = null;
      var css = null;
      if (this.FFormElement !== "") {
        el = document.getElementById(this.FFormElement);
        if (el != null) {
          dr = el.getBoundingClientRect();
          Result = Math.round(dr.right - dr.left);
          return Result;
        };
      };
      if (this.GetElementHandle() === document.body) {
        css = window.getComputedStyle(this.GetElementHandle());
        s = css.getPropertyValue("margin-left");
        s = pas.System.Copy(s,1,s.length - 2);
        d = pas.SysUtils.StrToInt(s) - 2;
        Result = (window.innerWidth - d) + 4;
      } else {
        if (this.FPopup) {
          Result = pas["WEBLib.Controls"].TControl.GetWidth.call(this)}
         else {
          Result = Math.round(this.GetElementHandle().offsetWidth);
          if (Result === 0) Result = window.innerWidth;
        };
      };
      return Result;
    };
    this.GetHeight = function () {
      var Result = 0;
      var d = 0;
      var s = "";
      var css = null;
      var el = null;
      var dr = null;
      if (this.FFormElement !== "") {
        el = document.getElementById(this.FFormElement);
        if (el != null) {
          dr = el.getBoundingClientRect();
          Result = Math.round(dr.bottom - dr.top);
          return Result;
        };
      };
      if (this.GetElementHandle() === document.body) {
        css = window.getComputedStyle(this.GetElementHandle());
        s = css.getPropertyValue("margin-top");
        s = pas.System.Copy(s,1,s.length - 2);
        d = pas.SysUtils.StrToInt(s) - 2;
        Result = window.innerHeight - d;
      } else {
        if (this.FPopup) {
          Result = pas["WEBLib.Controls"].TControl.GetHeight.call(this)}
         else {
          Result = Math.round(this.GetElementHandle().offsetHeight);
          if (Result === 0) Result = window.innerHeight;
        };
      };
      return Result;
    };
    this.GetLeft = function () {
      var Result = 0;
      if (this.FPopup) {
        Result = pas["WEBLib.Controls"].TControl.GetLeft.call(this)}
       else Result = Math.round(this.GetElementHandle().offsetLeft);
      return Result;
    };
    this.GetTop = function () {
      var Result = 0;
      if (this.FPopup) {
        Result = pas["WEBLib.Controls"].TControl.GetTop.call(this)}
       else Result = Math.round(this.GetElementHandle().offsetTop);
      return Result;
    };
    this.CreateElement = function () {
      var Result = null;
      var eh = null;
      if (this.FPopup) {
        Result = document.createElement("SPAN");
        this.FLayer$1 = document.createElement("SPAN");
        document.body.appendChild(this.FLayer$1);
        eh = this.FLayer$1;
        eh.addEventListener("click",rtl.createCallback(this,"HandleDoClick$1"));
        eh.style.setProperty("top","0");
        eh.style.setProperty("left","0");
        eh.style.setProperty("right","0");
        eh.style.setProperty("bottom","0");
        eh.style.setProperty("webkit-user-select","none");
        eh.style.setProperty("moz-user-select","none");
        eh.style.setProperty("khtml-user-select","none");
        eh.style.setProperty("ms-user-select","none");
        eh.style.setProperty("user-select","none");
        eh.style.setProperty("position","absolute");
      } else {
        Result = this.FormContainerElement();
      };
      return Result;
    };
    this.UpdateElement = function () {
      var clr = "";
      var i = 0;
      pas["WEBLib.Controls"].TControl.UpdateElement.apply(this,arguments);
      if (this.IsUpdating()) return;
      if (this.GetElementHandle() != null) {
        if (this.FCaption !== "") window.document.title = this.FCaption;
        clr = pas["WEBLib.Graphics"].ColorToHTML(this.FColor);
        this.GetElementHandle().style.setProperty("background-Color",clr);
        for (var $l1 = 0, $end2 = this.GetComponentCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if (pas["WEBLib.Controls"].TCSSCodeManager.isPrototypeOf(this.GetComponent(i))) ;
        };
      };
    };
    this.ContainerElement = function () {
      var Result = null;
      Result = this.FormContainerElement();
      return Result;
    };
    this.FormContainerElement = function () {
      var Result = null;
      if (this.FFormContainer !== "") {
        this.SetID(this.FFormContainer);
        Result = document.getElementById(this.FFormContainer);
        if (!(Result != null)) Result = document.body;
      } else {
        Result = document.body;
      };
      return Result;
    };
    this.GetElementBindHandle = function () {
      var Result = null;
      Result = window;
      return Result;
    };
    this.GetElementHandle = function () {
      var Result = null;
      if (this.FPopup) {
        Result = pas["WEBLib.Controls"].TControl.GetElementHandle.call(this)}
       else Result = this.FormContainerElement();
      return Result;
    };
    this.CloseQuery = function () {
      var Result = false;
      Result = true;
      if (this.FOnCloseQuery != null) this.FOnCloseQuery(this,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.LoadDFMValues = function () {
    };
    this.Init = function () {
    };
    this.IsFocused = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TControl.CreateInitialize.apply(this,arguments);
      this.FModalResult = 0;
      this.FFormStyle = $mod.TFormStyle.fsNormal;
      this.SetColor(16777215);
      window.addEventListener("load",rtl.createCallback(this,"DoLoaded"));
    };
    this.Create$2 = function (id) {
      pas["WEBLib.Controls"].TControl.Create$2.call(this,id);
      this.FFormElement = "";
      this.FModalResult = 0;
      this.FFormStyle = $mod.TFormStyle.fsNormal;
      this.DoCreate();
    };
    this.Create$1 = function (AOwner) {
      pas["WEBLib.Controls"].TControl.Create$1.apply(this,arguments);
      this.FFormFileName = "";
      this.FPopup = true;
    };
    this.Destroy = function () {
      if (this.FVisible) this.Close();
      pas["WEBLib.Controls"].TControl.Destroy.apply(this,arguments);
      if (this.FLayer$1 != null) {
        this.FormContainerElement().removeChild(this.FLayer$1);
        this.FLayer$1 = null;
      };
    };
    this.Close = function () {
      var lAction = 0;
      if (this.CloseQuery()) {
        this.ClearControls();
        this.UnbindEvents();
        lAction = $mod.TCloseAction.caHide;
        if (this.FOnClose != null) this.FOnClose(this,{get: function () {
            return lAction;
          }, set: function (v) {
            lAction = v;
          }});
        if (lAction !== $mod.TCloseAction.caNone) {
          if (this.FFormFileName === "") {
            if (this.FLayer$1 != null) {
              this.FormContainerElement().removeChild(this.FLayer$1);
              this.FLayer$1 = null;
            };
            if (this.FFormElement !== "") {
              this.FContainer.innerHTML = "";
              this.FContainer = null;
            } else if (this.FContainer != null) {
              this.FormContainerElement().removeChild(this.FContainer);
              this.FContainer = null;
            };
          } else {
            if (this.FFormElement !== "") {
              this.FContainer.innerHTML = "";
              this.FContainer = null;
            };
          };
          if (this.FFormElement === "") $mod.Application.PopForm();
          this.DoClose({get: function () {
              return lAction;
            }, set: function (v) {
              lAction = v;
            }});
          if ((lAction === $mod.TCloseAction.caFree) && !(pas.Classes.TComponentStateItem.csDestroying in this.FComponentState)) $impl.ReleaseForm(this);
          this.SetVisible(false);
        };
      };
    };
  });
  rtl.createClass($mod,"TForm",$mod.TCustomForm,function () {
    var $r = this.$rtti;
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnResize",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnResize","FOnResize");
    $r.addProperty("OnShow",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnShow","FOnShow");
    $r.addProperty("OnScroll",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnScroll","FOnScroll");
    $r.addProperty("OnPaint",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnPaint","FOnPaint");
    $r.addProperty("FormStyle",3,$mod.$rtti["TFormStyle"],"GetFormStyle","SetFormStyle");
    $r.addProperty("OnDeactivate",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDeactivate","FOnDeactivate");
    $r.addProperty("OnClose",0,$mod.$rtti["TCloseEvent"],"FOnClose","FOnClose");
    $r.addProperty("OnCloseQuery",0,$mod.$rtti["TCloseQueryEvent"],"FOnCloseQuery","FOnCloseQuery");
    $r.addProperty("OnTouchStart",0,pas["WEBLib.Controls"].$rtti["TTouchEvent"],"FOnTouchStart","FOnTouchStart");
    $r.addProperty("OnTouchMove",0,pas["WEBLib.Controls"].$rtti["TTouchEvent"],"FOnTouchMove","FOnTouchMove");
    $r.addProperty("OnTouchEnd",0,pas["WEBLib.Controls"].$rtti["TTouchEvent"],"FOnTouchEnd","FOnTouchEnd");
    $r.addProperty("OnUnload",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnUnload","FOnUnload");
  });
  rtl.createClass($mod,"TApplication",pas["WEBLib.Controls"].TControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TControl.$init.call(this);
      this.FLastReq = null;
      this.FMainForm = null;
      this.FFormStack = null;
      this.FParameters = null;
      this.FIsRedirect = false;
    };
    this.$final = function () {
      this.FLastReq = undefined;
      this.FMainForm = undefined;
      this.FFormStack = undefined;
      this.FParameters = undefined;
      pas["WEBLib.Controls"].TControl.$final.call(this);
    };
    this.PushForm = function (AForm) {
      var i = 0;
      for (var $l1 = 0, $end2 = AForm.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        AForm.GetControls(i).PersistinHTML();
      };
      AForm.FFormContent = document.body.innerHTML;
      for (var $l3 = 0, $end4 = AForm.GetControlsCount() - 1; $l3 <= $end4; $l3++) {
        i = $l3;
        AForm.GetControls(i).DisableTab();
      };
      this.FFormStack.Add(AForm);
    };
    this.PopForm = function () {
      var Result = null;
      var i = 0;
      var frm = "";
      if (this.FFormStack.GetCount() > 0) {
        Result = rtl.getObject(this.FFormStack.Get(this.FFormStack.GetCount() - 1));
        frm = Result.FFormContent;
        this.FFormStack.Delete(this.FFormStack.GetCount() - 1);
        document.body.innerHTML = frm;
        Result.InitFromHTML();
        Result.BindEvents();
        for (var $l1 = 0, $end2 = Result.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          Result.GetControls(i).HookElement();
        };
        for (var $l3 = 0, $end4 = Result.GetControlsCount() - 1; $l3 <= $end4; $l3++) {
          i = $l3;
          Result.GetControls(i).RecreateCanvas();
          Result.GetControls(i).InternalResize();
        };
        $mod.Application.FMainForm = Result;
      };
      return Result;
    };
    this.Create$1 = function (AOwner) {
      this.FFormStack = pas.Classes.TList.$create("Create$1");
      this.FParameters = pas.Classes.TStringList.$create("Create$1");
      this.FMainForm = null;
      this.FIsRedirect = false;
    };
    this.Destroy = function () {
      rtl.free(this,"FFormStack");
      rtl.free(this,"FParameters");
      pas["WEBLib.Controls"].TControl.Destroy.call(this);
    };
    this.CreateForm = function (AInstanceClass, AReference) {
      var Self = this;
      var lFileName = "";
      function DoStatusCreate(Event) {
        var Result = false;
        document.body.innerHTML = Self.FLastReq.responseText;
        if (Self.FMainForm != null) Self.PushForm(Self.FMainForm);
        Self.FMainForm = AInstanceClass.$create("Create$2",[$impl.cBodyTag]);
        Self.FMainForm.FFormFileName = lFileName;
        Self.FMainForm.CreateControl();
        Self.FMainForm.Init();
        AReference.set(Self.FMainForm);
        Result = true;
        return Result;
      };
      if (Self.FIsRedirect) return;
      lFileName = AInstanceClass.$module.$name + $impl.cHTMLExt;
      Self.FLastReq = new XMLHttpRequest();
      Self.FLastReq.addEventListener("load",DoStatusCreate);
      Self.FLastReq.open("GET",lFileName);
      Self.FLastReq.setRequestHeader("Cache-Control","no-cache");
      Self.FLastReq.send();
    };
    this.Initialize = function () {
      var query = "";
      var token = "";
      this.FParameters.Clear();
      query = window.location.href;
      this.InitFormatSettings(pas["WEBLib.WebTools"].GetBrowserLocale());
      this.FParameters.SetDelimiter("&");
      this.FParameters.FStrictDelimiter = true;
      this.FParameters.SetDelimitedText(query);
      if (this.FParameters.IndexOfName("code") !== -1) {
        this.FIsRedirect = true;
        token = this.FParameters.GetValue("code");
        window.opener.processAuthData(token);
        window.close();
      };
      if (this.FParameters.IndexOfName("access_token") !== -1) {
        this.FIsRedirect = true;
        token = this.FParameters.GetValue("access_token");
        window.opener.processAuthData(token);
        window.close();
      };
    };
    this.InitFormatSettings = function (BrowserLocale) {
      var timestr = "";
      var i = 0;
      var locale = "";
      locale = BrowserLocale;
      pas.SysUtils.FormatSettings.SetShortDateFormat(pas["WEBLib.WebTools"].GetLocaleShortDateFormat(locale));
      if (pas.System.Pos("\/",pas.SysUtils.FormatSettings.GetShortDateFormat()) > 0) pas.SysUtils.FormatSettings.SetDateSeparator("\/");
      if (pas.System.Pos(".",pas.SysUtils.FormatSettings.GetShortDateFormat()) > 0) pas.SysUtils.FormatSettings.SetDateSeparator(".");
      if (pas.System.Pos("-",pas.SysUtils.FormatSettings.GetShortDateFormat()) > 0) pas.SysUtils.FormatSettings.SetDateSeparator("-");
      var event = new Date('Jan 1, 1980 06:07:08 GMT+00:00');
      var loc = "";
      if (locale == "") { loc = navigator.language; } else
      { loc = locale; }
      timestr = event.toLocaleTimeString(loc);
      if (pas.System.Pos(":",timestr) > 0) pas.SysUtils.FormatSettings.SetTimeSeparator(":");
      if (pas.System.Pos("-",timestr) > 0) pas.SysUtils.FormatSettings.SetTimeSeparator(".");
      if (pas.System.Pos("-",timestr) > 0) pas.SysUtils.FormatSettings.SetTimeSeparator("-");
      for (i = 1; i <= 7; i++) {
        pas.SysUtils.ShortDayNames[i] = pas["WEBLib.WebTools"].GetLocaleShortDayName(i,locale);
        pas.SysUtils.LongDayNames[i] = pas["WEBLib.WebTools"].GetLocaleLongDayName(i,locale);
      };
      for (i = 1; i <= 12; i++) {
        pas.SysUtils.ShortMonthNames[i - 1] = pas["WEBLib.WebTools"].GetLocaleShortMonthName(i,locale);
        pas.SysUtils.LongMonthNames[i - 1] = pas["WEBLib.WebTools"].GetLocaleLongMonthName(i,locale);
      };
      pas.SysUtils.FormatSettings.SetDecimalSeparator(pas["WEBLib.WebTools"].GetLocaleDecimalSeparator());
      if (pas.SysUtils.FormatSettings.GetDecimalSeparator() === ".") {
        pas.SysUtils.FormatSettings.SetThousandSeparator(",")}
       else pas.SysUtils.FormatSettings.SetThousandSeparator(".");
    };
    this.Run = function () {
    };
  });
  this.GetParentForm = function (AControl) {
    var Result = null;
    var FOwner = null;
    Result = null;
    FOwner = AControl;
    while ((FOwner != null) && !$mod.TCustomForm.isPrototypeOf(FOwner)) {
      FOwner = FOwner.FOwner;
    };
    if ((FOwner != null) && $mod.TCustomForm.isPrototypeOf(FOwner)) Result = rtl.as(FOwner,$mod.TCustomForm);
    return Result;
  };
  this.Application = null;
  $mod.$init = function () {
    $mod.Application = $mod.TApplication.$create("Create$1",[null]);
  };
},["WEBLib.Dialogs","WEBLib.WebTools"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.cBodyTag = "body";
  $impl.cHTMLExt = ".html";
  $impl.ReleaseForm = function (AForm) {
    AForm = rtl.freeLoc(AForm);
    AForm = null;
  };
});
rtl.module("WEBLib.Runner",["System"],function () {
  "use strict";
  var $mod = this;
  this.TTMSBrowserEnum = {"0": "tbnNull", tbnNull: 0, "1": "tbnDefault", tbnDefault: 1, "2": "tbnAll", tbnAll: 2, "3": "tbnChrome", tbnChrome: 3, "4": "tbnFirefox", tbnFirefox: 4, "5": "tbnEdge", tbnEdge: 5, "6": "tbnIExplore", tbnIExplore: 6, "7": "tbnOpera", tbnOpera: 7};
  rtl.createClass($mod,"TTMSWebRunner",pas.System.TObject,function () {
    this.Execute$4 = function (ABrowser) {
    };
  });
});
rtl.module("Unit1",["System","SysUtils","Classes","JS","Web","WEBLib.Forms","WEBLib.Controls","WEBLib.Graphics","WEBLib.StdCtrls","WEBLib.ExtCtrls","WEBLib.Dialogs"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TWebForm1",pas["WEBLib.Forms"].TForm,function () {
    this.$init = function () {
      pas["WEBLib.Forms"].TForm.$init.call(this);
      this.WebLabel1 = null;
      this.WebPanel1 = null;
      this.WebTimer1 = null;
    };
    this.$final = function () {
      this.WebLabel1 = undefined;
      this.WebPanel1 = undefined;
      this.WebTimer1 = undefined;
      pas["WEBLib.Forms"].TForm.$final.call(this);
    };
    this.WebTimer1Timer = function (Sender) {
      this.WebLabel1.SetCaption(pas.SysUtils.TimeToStr(pas.SysUtils.Time()));
    };
    this.LoadDFMValues = function () {
      pas["WEBLib.Forms"].TCustomForm.LoadDFMValues.apply(this,arguments);
      this.WebPanel1 = pas["WEBLib.ExtCtrls"].TWebPanel.$create("Create$1",[this]);
      this.WebLabel1 = pas["WEBLib.StdCtrls"].TWebLabel.$create("Create$1",[this]);
      this.WebTimer1 = pas["WEBLib.ExtCtrls"].TWebTimer.$create("Create$1",[this]);
      this.WebPanel1.BeginUpdate();
      this.WebLabel1.BeginUpdate();
      try {
        this.SetName("WebForm1");
        this.SetLeft(315);
        this.SetHeight(240);
        this.SetTop(114);
        this.SetWidth(320);
        this.SetCaption("WebForm1");
        this.ClientHeight = 240;
        this.ClientWidth = 320;
        this.LCLVersion = "1.9.0.0";
        var $with1 = this.WebPanel1;
        $with1.SetName("WebPanel1");
        $with1.SetParent(this);
        $with1.SetLeft(8);
        $with1.SetHeight(57);
        $with1.SetTop(16);
        $with1.SetWidth(142);
        $with1.FCaption = "WebPanel1";
        var $with2 = this.WebLabel1;
        $with2.SetName("WebLabel1");
        $with2.SetParent(this.WebPanel1);
        $with2.SetLeft(8);
        $with2.SetHeight(39);
        $with2.SetTop(8);
        $with2.SetWidth(124);
        $with2.SetCaption("00:00:00");
        $with2.FFont.FCharset = 0;
        $with2.FFont.SetColor(255);
        $with2.FFont.SetHeight(-32);
        $with2.FFont.SetName("Tahoma");
        $with2.FFont.Pitch = pas["WEBLib.Graphics"].TFontPitch.fpVariable;
        $with2.FFont.Quality = pas["WEBLib.Graphics"].TFontQuality.fqDraft;
        $with2.FParentColor = false;
        $with2.FParentFont = false;
        $with2.SetAlignWithMargins(false);
        $with2.SetEllipsisPosition(pas["WEBLib.StdCtrls"].TEllipsisPosition.epEndEllipsis);
        var $with3 = this.WebTimer1;
        $with3.SetName("WebTimer1");
        $with3.FOnTimer = rtl.createCallback(this,"WebTimer1Timer");
        this.WebTimer1.FDesignInfo = 1310930;
      } finally {
        this.WebPanel1.EndUpdate();
        this.WebLabel1.EndUpdate();
      };
    };
  });
  this.WebForm1 = null;
});
rtl.module("program",["System","Classes","SysUtils","JS","Web","WEBLib.Forms","WEBLib.Runner","Unit1"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    pas["WEBLib.Forms"].Application.Initialize();
    pas["WEBLib.Forms"].Application.CreateForm(pas.Unit1.TWebForm1,{p: pas.Unit1, get: function () {
        return this.p.WebForm1;
      }, set: function (v) {
        this.p.WebForm1 = v;
      }});
    pas["WEBLib.Forms"].Application.Run();
    pas["WEBLib.Runner"].TTMSWebRunner.Execute$4(pas["WEBLib.Runner"].TTMSBrowserEnum.tbnFirefox);
  };
});
//# sourceMappingURL=project1.js.map
