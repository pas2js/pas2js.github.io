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
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" proc='+typeof(fn));
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
    if (typeof item === 'function') return item.call(obj); // COM: contains _AddRef
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
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null'));
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
        if (this.hasOwnProperty(id)) this[id]._Release;
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

  arrayClone: function(type,src,srcpos,end,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if (rtl.isFunction(type)){
      for (; srcpos<end; srcpos++) dst[dstpos++] = new type(src[srcpos]); // clone record
    } else if((typeof(type)==="string") && (type === 'refSet')) {
      for (; srcpos<end; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    }  else {
      for (; srcpos<end; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++) l+=arguments[i].length;
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src == null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray == null) return [];
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
        t.module = this.module;
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
  this.MaxLongint = 0x7fffffff;
  this.Maxint = 2147483647;
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
  this.Frac = function (A) {
    return A % 1;
  };
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
  this.val$5 = function (S, I, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else if (x > 2147483647) {
      Code.set(2)}
     else I.set($mod.Trunc(x));
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
  this.isBoolean = function (v) {
    return typeof(v) == 'boolean';
  };
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.isUndefined = function (v) {
    return v == undefined;
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
  this.SErrInvalidDate = 'Invalid date: "%s"';
  this.SErrInvalidTimeFormat = 'Invalid time format: "%s"';
  this.SInvalidDateFormat = 'Invalid date format: "%s"';
  this.SErrInvalidInteger = 'Invalid integer value: "%s"';
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
  this.TFloatRec = function (s) {
    if (s) {
      this.Exponent = s.Exponent;
      this.Negative = s.Negative;
      this.Digits = s.Digits;
    } else {
      this.Exponent = 0;
      this.Negative = false;
      this.Digits = [];
    };
    this.$equal = function (b) {
      return (this.Exponent === b.Exponent) && ((this.Negative === b.Negative) && (this.Digits === b.Digits));
    };
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
  rtl.createClass($mod,"EAbort",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EVariantError",$mod.Exception,function () {
  });
  this.CharInSet = function (Ch, CSet) {
    var Result = false;
    var I = 0;
    Result = false;
    I = rtl.length(CSet) - 1;
    while (!Result && (I >= 0)) {
      Result = Ch === CSet[I];
      I -= 1;
    };
    return Result;
  };
  this.Trim = function (S) {
    return S.trim();
  };
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
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
  this.AnsiSameText = function (s1, s2) {
    return s1.localeCompare(s2) == 0;
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
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
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
  var Rounds = "234567890";
  this.FloatToDecimal = function (Value, Precision, Decimals) {
    var Result = new $mod.TFloatRec();
    var Buffer = "";
    var InfNan = "";
    var error = 0;
    var N = 0;
    var L = 0;
    var Start = 0;
    var C = 0;
    var GotNonZeroBeforeDot = false;
    var BeforeDot = false;
    if (Value === 0) ;
    Result.Digits = rtl.arraySetLength(Result.Digits,"",19);
    Buffer=Value.toPrecision(21); // Double precision;
    N = 1;
    L = Buffer.length;
    while (Buffer.charAt(N - 1) === " ") N += 1;
    Result.Negative = Buffer.charAt(N - 1) === "-";
    if (Result.Negative) {
      N += 1}
     else if (Buffer.charAt(N - 1) === "+") N += 1;
    if (L >= (N + 2)) {
      InfNan = pas.System.Copy(Buffer,N,3);
      if (InfNan === "Inf") {
        Result.Digits[0] = "\x00";
        Result.Exponent = 32767;
        return Result;
      };
      if (InfNan === "Nan") {
        Result.Digits[0] = "\x00";
        Result.Exponent = -32768;
        return Result;
      };
    };
    Start = N;
    Result.Exponent = 0;
    BeforeDot = true;
    GotNonZeroBeforeDot = false;
    while ((L >= N) && (Buffer.charAt(N - 1) !== "E")) {
      if (Buffer.charAt(N - 1) === ".") {
        BeforeDot = false}
       else {
        if (BeforeDot) {
          Result.Exponent += 1;
          Result.Digits[N - Start] = Buffer.charAt(N - 1);
          if (Buffer.charAt(N - 1) !== "0") GotNonZeroBeforeDot = true;
        } else Result.Digits[(N - Start) - 1] = Buffer.charAt(N - 1);
      };
      N += 1;
    };
    N += 1;
    if (N <= L) {
      pas.System.val$5(pas.System.Copy(Buffer,N,(L - N) + 1),{get: function () {
          return C;
        }, set: function (v) {
          C = v;
        }},{get: function () {
          return error;
        }, set: function (v) {
          error = v;
        }});
      Result.Exponent += C;
    };
    if (BeforeDot) {
      N = (N - Start) - 1}
     else N = (N - Start) - 2;
    L = rtl.length(Result.Digits);
    if (N < L) Result.Digits[N] = "0";
    if ((Decimals + Result.Exponent) < Precision) {
      N = Decimals + Result.Exponent}
     else N = Precision;
    if (N >= L) N = L - 1;
    if (N === 0) {
      if (Result.Digits[0] >= "5") {
        Result.Digits[0] = "1";
        Result.Digits[1] = "\x00";
        Result.Exponent += 1;
      } else Result.Digits[0] = "\x00";
    } else if (N > 0) {
      if (Result.Digits[N] >= "5") {
        do {
          Result.Digits[N] = "\x00";
          N -= 1;
          Result.Digits[N] = Rounds.charAt($mod.StrToInt(Result.Digits[N]) - 1);
        } while (!((N === 0) || (Result.Digits[N] < ":")));
        if (Result.Digits[0] === ":") {
          Result.Digits[0] = "1";
          Result.Exponent += 1;
        };
      } else {
        Result.Digits[N] = "0";
        while ((N > -1) && (Result.Digits[N] === "0")) {
          Result.Digits[N] = "\x00";
          N -= 1;
        };
      };
    } else Result.Digits[0] = "\x00";
    if ((Result.Digits[0] === "\x00") && !GotNonZeroBeforeDot) {
      Result.Exponent = 0;
      Result.Negative = false;
    };
    return Result;
  };
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,$mod.TFloatFormat.ffGeneral,15,0);
    return Result;
  };
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
  this.TryStrToFloat = function (S, res) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if ($mod.ThousandSeparator !== "") N = $mod.StringReplace(N,$mod.ThousandSeparator,"",rtl.createSet($mod.TStringReplaceFlag.rfReplaceAll));
    if ($mod.DecimalSeparator !== ".") N = $mod.StringReplace(N,$mod.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  var MaxPrecision = 18;
  this.FormatFloat = function (Fmt, aValue) {
    var Result = "";
    var E = 0.0;
    var FV = new $mod.TFloatRec();
    var Section = "";
    var SectionLength = 0;
    var ThousandSep = false;
    var IsScientific = false;
    var DecimalPos = 0;
    var FirstDigit = 0;
    var LastDigit = 0;
    var RequestedDigits = 0;
    var ExpSize = 0;
    var Available = 0;
    var Current = 0;
    var PadZeroes = 0;
    var DistToDecimal = 0;
    function InitVars() {
      E = aValue;
      Section = "";
      SectionLength = 0;
      ThousandSep = false;
      IsScientific = false;
      DecimalPos = 0;
      FirstDigit = 2147483647;
      LastDigit = 0;
      RequestedDigits = 0;
      ExpSize = 0;
      Available = -1;
    };
    function ToResult(AChar) {
      Result = Result + AChar;
    };
    function AddToResult(AStr) {
      Result = Result + AStr;
    };
    function WriteDigit(ADigit) {
      if (ADigit === "\x00") return;
      DistToDecimal -= 1;
      if (DistToDecimal === -1) {
        AddToResult($mod.DecimalSeparator);
        ToResult(ADigit);
      } else {
        ToResult(ADigit);
        if ((ThousandSep && ((DistToDecimal % 3) === 0)) && (DistToDecimal > 1)) AddToResult($mod.ThousandSeparator);
      };
    };
    function GetDigit() {
      var Result = "";
      Result = "\x00";
      if (Current <= Available) {
        Result = FV.Digits[Current];
        Current += 1;
      } else if (DistToDecimal <= LastDigit) {
        DistToDecimal -= 1}
       else Result = "0";
      return Result;
    };
    function CopyDigit() {
      if (PadZeroes === 0) {
        WriteDigit(GetDigit())}
       else if (PadZeroes < 0) {
        PadZeroes += 1;
        if (DistToDecimal <= FirstDigit) {
          WriteDigit("0")}
         else DistToDecimal -= 1;
      } else {
        while (PadZeroes > 0) {
          WriteDigit(GetDigit());
          PadZeroes -= 1;
        };
        WriteDigit(GetDigit());
      };
    };
    function GetSections(SP) {
      var Result = 0;
      var FL = 0;
      var i = 0;
      var C = "";
      var Q = "";
      var inQuote = false;
      Result = 1;
      SP.get()[1] = -1;
      SP.get()[2] = -1;
      SP.get()[3] = -1;
      inQuote = false;
      Q = "\x00";
      i = 1;
      FL = Fmt.length;
      while (i <= FL) {
        C = Fmt.charAt(i - 1);
        var $tmp1 = C;
        if ($tmp1 === ";") {
          if (!inQuote) {
            if (Result > 3) throw $mod.Exception.$create("Create$1",["Invalid float format"]);
            SP.get()[Result] = i + 1;
            Result += 1;
          };
        } else if (($tmp1 === '"') || ($tmp1 === "'")) {
          if (inQuote) {
            inQuote = C !== Q}
           else {
            inQuote = true;
            Q = C;
          };
        };
        i += 1;
      };
      if (SP.get()[Result] === -1) SP.get()[Result] = FL + 1;
      return Result;
    };
    function AnalyzeFormat() {
      var I = 0;
      var Len = 0;
      var Q = "";
      var C = "";
      var InQuote = false;
      Len = Section.length;
      I = 1;
      InQuote = false;
      Q = "\x00";
      while (I <= Len) {
        C = Section.charAt(I - 1);
        if (C.charCodeAt() in rtl.createSet(34,39)) {
          if (InQuote) {
            InQuote = C !== Q}
           else {
            InQuote = true;
            Q = C;
          };
        } else if (!InQuote) {
          var $tmp1 = C;
          if ($tmp1 === ".") {
            if (DecimalPos === 0) DecimalPos = RequestedDigits + 1}
           else if ($tmp1 === ",") {
            ThousandSep = $mod.ThousandSeparator !== "\x00"}
           else if (($tmp1 === "e") || ($tmp1 === "E")) {
            I += 1;
            if (I < Len) {
              C = Section.charAt(I - 1);
              IsScientific = C.charCodeAt() in rtl.createSet(45,43);
              if (IsScientific) while ((I < Len) && (Section.charAt((I + 1) - 1) === "0")) {
                ExpSize += 1;
                I += 1;
              };
              if (ExpSize > 4) ExpSize = 4;
            };
          } else if ($tmp1 === "#") {
            RequestedDigits += 1}
           else if ($tmp1 === "0") {
            if (RequestedDigits < FirstDigit) FirstDigit = RequestedDigits + 1;
            RequestedDigits += 1;
            LastDigit = RequestedDigits + 1;
          };
        };
        I += 1;
      };
      if (DecimalPos === 0) DecimalPos = RequestedDigits + 1;
      LastDigit = DecimalPos - LastDigit;
      if (LastDigit > 0) LastDigit = 0;
      FirstDigit = DecimalPos - FirstDigit;
      if (FirstDigit < 0) FirstDigit = 0;
    };
    function ValueOutSideScope() {
      var Result = false;
      Result = (((FV.Exponent >= 18) && !IsScientific) || (FV.Exponent === 0x7FF)) || (FV.Exponent === 0x800);
      return Result;
    };
    function CalcRunVars() {
      var D = 0;
      var P = 0;
      if (IsScientific) {
        P = RequestedDigits;
        D = 9999;
      } else {
        P = 18;
        D = (RequestedDigits - DecimalPos) + 1;
      };
      FV = new $mod.TFloatRec($mod.FloatToDecimal(aValue,P,D));
      DistToDecimal = DecimalPos - 1;
      if (IsScientific) {
        PadZeroes = 0}
       else {
        PadZeroes = FV.Exponent - (DecimalPos - 1);
        if (PadZeroes >= 0) DistToDecimal = FV.Exponent;
      };
      Available = -1;
      while ((Available < (rtl.length(FV.Digits) - 1)) && (FV.Digits[Available + 1] !== "\x00")) Available += 1;
    };
    function FormatExponent(ASign, aExponent) {
      var Result = "";
      Result = $mod.IntToStr(aExponent);
      Result = pas.System.StringOfChar("0",ExpSize - Result.length) + Result;
      if (aExponent < 0) {
        Result = "-" + Result}
       else if ((aExponent > 0) && (ASign === "+")) Result = ASign + Result;
      return Result;
    };
    var I = 0;
    var S = 0;
    var C = "";
    var Q = "";
    var PA = [];
    var InLiteral = false;
    PA = rtl.arraySetLength(PA,0,4);
    Result = "";
    InitVars();
    if (E > 0) {
      S = 1}
     else if (E < 0) {
      S = 2}
     else S = 3;
    PA[0] = 0;
    I = GetSections({get: function () {
        return PA;
      }, set: function (v) {
        PA = v;
      }});
    if ((I < S) || ((PA[S] - PA[S - 1]) === 0)) S = 1;
    SectionLength = (PA[S] - PA[S - 1]) - 1;
    Section = pas.System.Copy(Fmt,PA[S - 1] + 1,SectionLength);
    Section = rtl.strSetLength(Section,SectionLength);
    AnalyzeFormat();
    CalcRunVars();
    if ((SectionLength === 0) || ValueOutSideScope()) {
      Section=E.toPrecision(15);
      Result = Section;
    };
    I = 1;
    Current = 0;
    Q = " ";
    InLiteral = false;
    if (FV.Negative && (S === 1)) ToResult("-");
    while (I <= SectionLength) {
      C = Section.charAt(I - 1);
      if (C.charCodeAt() in rtl.createSet(34,39)) {
        if (InLiteral) {
          InLiteral = C !== Q}
         else {
          InLiteral = true;
          Q = C;
        };
      } else if (InLiteral) {
        ToResult(C)}
       else {
        var $tmp1 = C;
        if (($tmp1 === "0") || ($tmp1 === "#")) {
          CopyDigit()}
         else if (($tmp1 === ".") || ($tmp1 === ",")) {}
        else if (($tmp1 === "e") || ($tmp1 === "E")) {
          ToResult(C);
          I += 1;
          if (I <= Section.length) {
            C = Section.charAt(I - 1);
            if (C.charCodeAt() in rtl.createSet(43,45)) {
              AddToResult(FormatExponent(C,(FV.Exponent - DecimalPos) + 1));
              while ((I < SectionLength) && (Section.charAt((I + 1) - 1) === "0")) I += 1;
            };
          };
        } else {
          ToResult(C);
        };
      };
      I += 1;
    };
    return Result;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = "Application raised an exception " + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = (S + " : ") + ExceptObject.fMessage;
    window.alert(S);
    if (ExceptAddr === null) ;
  };
  this.Abort = function () {
    throw $mod.EAbort.$create("Create$1",[$impl.SAbortError]);
  };
  this.TTimeStamp = function (s) {
    if (s) {
      this.Time = s.Time;
      this.date = s.date;
    } else {
      this.Time = 0;
      this.date = 0;
    };
    this.$equal = function (b) {
      return (this.Time === b.Time) && (this.date === b.date);
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
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
  });
  this.FormatSettings = null;
  this.TwoDigitYearCenturyWindow = 50;
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
    Result.date = 693594 + Math.floor(pas.System.Trunc(D) / 86400000);
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
  this.ComposeDateTime = function (date, Time) {
    var Result = 0.0;
    if (date < 0) {
      Result = pas.System.Trunc(date) - Math.abs(pas.System.Frac(Time))}
     else Result = pas.System.Trunc(date) + Math.abs(pas.System.Frac(Time));
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
  this.date = function () {
    var Result = 0.0;
    Result = pas.System.Trunc($mod.Now());
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
  this.StrToDate = function (S) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.StrToDate$2 = function (S, useformat, separator) {
    var Result = 0.0;
    var MSg = "";
    Result = $impl.IntStrToDate({get: function () {
        return MSg;
      }, set: function (v) {
        MSg = v;
      }},S,useformat,separator);
    if (MSg !== "") throw $mod.EConvertError.$create("Create$1",[MSg]);
    return Result;
  };
  this.StrToTime = function (S) {
    var Result = 0.0;
    Result = $mod.StrToTime$1(S,$mod.TimeSeparator);
    return Result;
  };
  this.StrToTime$1 = function (S, separator) {
    var Result = 0.0;
    var Msg = "";
    Result = $impl.IntStrToTime({get: function () {
        return Msg;
      }, set: function (v) {
        Msg = v;
      }},S,S.length,separator);
    if (Msg !== "") throw $mod.EConvertError.$create("Create$1",[Msg]);
    return Result;
  };
  this.StrToDateTime = function (S) {
    var Result = 0.0;
    var TimeStr = "";
    var DateStr = "";
    var PartsFound = 0;
    PartsFound = $impl.SplitDateTimeStr(S,{get: function () {
        return DateStr;
      }, set: function (v) {
        DateStr = v;
      }},{get: function () {
        return TimeStr;
      }, set: function (v) {
        TimeStr = v;
      }});
    var $tmp1 = PartsFound;
    if ($tmp1 === 0) {
      Result = $mod.StrToDate("")}
     else if ($tmp1 === 1) {
      if (DateStr.length > 0) {
        Result = $mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator)}
       else Result = $mod.StrToTime(TimeStr)}
     else if ($tmp1 === 2) Result = $mod.ComposeDateTime($mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator),$mod.StrToTime(TimeStr));
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
  this.TryStrToDate = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToDate$2(S,Value,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.TryStrToDate$2 = function (S, Value, useformat, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToDate({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,useformat,separator));
      Result = Msg === "";
    };
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
  $impl.SAbortError = "Operation aborted";
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
    return Result;
  };
  $impl.RESpecials = "([\\[\\]\\(\\)\\\\\\.\\*])";
  var WhiteSpace = " \b\t\n\f\r";
  var Digits = "0123456789";
  $impl.IntStrToDate = function (ErrorMsg, S, useformat, separator) {
    var Result = 0.0;
    function FixErrorMsg(errmarg) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SInvalidDateFormat,[errmarg]));
    };
    var df = "";
    var d = 0;
    var m = 0;
    var y = 0;
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var n = 0;
    var i = 0;
    var len = 0;
    var c = 0;
    var dp = 0;
    var mp = 0;
    var yp = 0;
    var which = 0;
    var s1 = "";
    var values = [];
    var YearMoreThenTwoDigits = false;
    values = rtl.arraySetLength(values,0,4);
    Result = 0;
    len = S.length;
    ErrorMsg.set("");
    while ((len > 0) && (pas.System.Pos(S.charAt(len - 1),WhiteSpace) > 0)) len -= 1;
    if (len === 0) {
      FixErrorMsg(S);
      return Result;
    };
    YearMoreThenTwoDigits = false;
    if (separator === "\x00") if ($mod.DateSeparator !== "\x00") {
      separator = $mod.DateSeparator}
     else separator = "-";
    df = $mod.UpperCase(useformat);
    yp = 0;
    mp = 0;
    dp = 0;
    which = 0;
    i = 0;
    while ((i < df.length) && (which < 3)) {
      i += 1;
      var $tmp1 = df.charAt(i - 1);
      if ($tmp1 === "Y") {
        if (yp === 0) {
          which += 1;
          yp = which;
        }}
       else if ($tmp1 === "M") {
        if (mp === 0) {
          which += 1;
          mp = which;
        }}
       else if ($tmp1 === "D") if (dp === 0) {
        which += 1;
        dp = which;
      };
    };
    for (i = 1; i <= 3; i++) values[i] = 0;
    s1 = "";
    n = 0;
    for (var $l2 = 1, $end3 = len; $l2 <= $end3; $l2++) {
      i = $l2;
      if (pas.System.Pos(S.charAt(i - 1),Digits) > 0) s1 = s1 + S.charAt(i - 1);
      if ((separator !== " ") && (S.charAt(i - 1) === " ")) continue;
      if ((S.charAt(i - 1) === separator) || ((i === len) && (pas.System.Pos(S.charAt(i - 1),Digits) > 0))) {
        n += 1;
        if (n > 3) {
          FixErrorMsg(S);
          return Result;
        };
        if ((n === yp) && (s1.length > 2)) YearMoreThenTwoDigits = true;
        pas.System.val$5(s1,{a: n, p: values, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }},{get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        if (c !== 0) {
          FixErrorMsg(S);
          return Result;
        };
        s1 = "";
      } else if (pas.System.Pos(S.charAt(i - 1),Digits) === 0) {
        FixErrorMsg(S);
        return Result;
      };
    };
    if ((which < 3) && (n > which)) {
      FixErrorMsg(S);
      return Result;
    };
    $mod.DecodeDate($mod.date(),{get: function () {
        return ly;
      }, set: function (v) {
        ly = v;
      }},{get: function () {
        return lm;
      }, set: function (v) {
        lm = v;
      }},{get: function () {
        return ld;
      }, set: function (v) {
        ld = v;
      }});
    if (n === 3) {
      y = values[yp];
      m = values[mp];
      d = values[dp];
    } else {
      y = ly;
      if (n < 2) {
        d = values[1];
        m = lm;
      } else if (dp < mp) {
        d = values[1];
        m = values[2];
      } else {
        d = values[2];
        m = values[1];
      };
    };
    if (((y >= 0) && (y < 100)) && !YearMoreThenTwoDigits) {
      ly = ly - $mod.TwoDigitYearCenturyWindow;
      y += Math.floor(ly / 100) * 100;
      if (($mod.TwoDigitYearCenturyWindow > 0) && (y < ly)) y += 100;
    };
    if (!$mod.TryEncodeDate(y,m,d,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set(pas.RTLConsts.SErrInvalidDate);
    return Result;
  };
  var AMPM_None = 0;
  var AMPM_AM = 1;
  var AMPM_PM = 2;
  var tiHour = 0;
  var tiMin = 1;
  var tiSec = 2;
  var tiMSec = 3;
  var Digits$1 = "0123456789";
  $impl.IntStrToTime = function (ErrorMsg, S, Len, separator) {
    var Result = 0.0;
    var AmPm = 0;
    var TimeValues = [];
    function SplitElements(TimeValues, AmPm) {
      var Result = false;
      var Cur = 0;
      var Offset = 0;
      var ElemLen = 0;
      var Err = 0;
      var TimeIndex = 0;
      var FirstSignificantDigit = 0;
      var Value = 0;
      var DigitPending = false;
      var MSecPending = false;
      var AmPmStr = "";
      var CurChar = "";
      var I = 0;
      var allowedchars = "";
      Result = false;
      AmPm.set(0);
      MSecPending = false;
      TimeIndex = 0;
      for (I = 0; I <= 3; I++) TimeValues.get()[I] = 0;
      Cur = 1;
      while ((Cur < Len) && (S.charAt(Cur - 1) === " ")) Cur += 1;
      Offset = Cur;
      if (((Cur > (Len - 1)) || (S.charAt(Cur - 1) === separator)) || (S.charAt(Cur - 1) === $mod.DecimalSeparator)) {
        return Result;
      };
      DigitPending = pas.System.Pos(S.charAt(Cur - 1),Digits$1) > 0;
      while (Cur <= Len) {
        CurChar = S.charAt(Cur - 1);
        if (pas.System.Pos(CurChar,Digits$1) > 0) {
          if (!DigitPending || (TimeIndex > 3)) {
            return Result;
          };
          Offset = Cur;
          if (CurChar !== "0") {
            FirstSignificantDigit = Offset}
           else FirstSignificantDigit = -1;
          while ((Cur < Len) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) > 0)) {
            if ((FirstSignificantDigit === -1) && (S.charAt(Cur - 1) !== "0")) FirstSignificantDigit = Cur;
            Cur += 1;
          };
          if (FirstSignificantDigit === -1) FirstSignificantDigit = Cur;
          ElemLen = (1 + Cur) - FirstSignificantDigit;
          if ((ElemLen <= 2) || ((ElemLen <= 3) && (TimeIndex === 3))) {
            pas.System.val$5(pas.System.Copy(S,FirstSignificantDigit,ElemLen),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Err;
              }, set: function (v) {
                Err = v;
              }});
            TimeValues.get()[TimeIndex] = Value;
            TimeIndex += 1;
            DigitPending = false;
          } else {
            return Result;
          };
        } else if (CurChar === " ") {}
        else if (CurChar === separator) {
          if (DigitPending || (TimeIndex > 2)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = false;
        } else if (CurChar === $mod.DecimalSeparator) {
          if ((DigitPending || MSecPending) || (TimeIndex !== 3)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = true;
        } else {
          if ((AmPm.get() !== 0) || DigitPending) {
            return Result;
          };
          Offset = Cur;
          allowedchars = $mod.DecimalSeparator + " ";
          if (separator !== "\x00") allowedchars = allowedchars + separator;
          while (((Cur < (Len - 1)) && (pas.System.Pos(S.charAt((Cur + 1) - 1),allowedchars) === 0)) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) === 0)) Cur += 1;
          ElemLen = (1 + Cur) - Offset;
          AmPmStr = pas.System.Copy(S,1 + Offset,ElemLen);
          if ($mod.CompareText(AmPmStr,$mod.TimeAMString) === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,$mod.TimePMString) === 0) {
            AmPm.set(2)}
           else if ($mod.CompareText(AmPmStr,"AM") === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,"PM") === 0) {
            AmPm.set(2)}
           else {
            return Result;
          };
          if (TimeIndex === 0) {
            DigitPending = true;
          } else {
            TimeIndex = 3 + 1;
            DigitPending = false;
          };
        };
        Cur += 1;
      };
      if (((TimeIndex === 0) || ((AmPm.get() !== 0) && ((TimeValues.get()[0] > 12) || (TimeValues.get()[0] === 0)))) || DigitPending) return Result;
      Result = true;
      return Result;
    };
    TimeValues = rtl.arraySetLength(TimeValues,0,4);
    if (separator === "\x00") if ($mod.TimeSeparator !== "\x00") {
      separator = $mod.TimeSeparator}
     else separator = ":";
    AmPm = 0;
    if (!SplitElements({get: function () {
        return TimeValues;
      }, set: function (v) {
        TimeValues = v;
      }},{get: function () {
        return AmPm;
      }, set: function (v) {
        AmPm = v;
      }})) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
      return Result;
    };
    if ((AmPm === 2) && (TimeValues[0] !== 12)) {
      TimeValues[0] += 12}
     else if ((AmPm === 1) && (TimeValues[0] === 12)) TimeValues[0] = 0;
    if (!$mod.TryEncodeTime(TimeValues[0],TimeValues[1],TimeValues[2],TimeValues[3],{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
    return Result;
  };
  var WhiteSpace$1 = "\t\n\r ";
  $impl.SplitDateTimeStr = function (DateTimeStr, DateStr, TimeStr) {
    var Result = 0;
    var p = 0;
    var DummyDT = 0.0;
    Result = 0;
    DateStr.set("");
    TimeStr.set("");
    DateTimeStr = $mod.Trim(DateTimeStr);
    if (DateTimeStr.length === 0) return Result;
    if ((($mod.DateSeparator === " ") && ($mod.TimeSeparator === " ")) && (pas.System.Pos(" ",DateTimeStr) > 0)) {
      DateStr.set(DateTimeStr);
      return 1;
    };
    p = 1;
    if ($mod.DateSeparator !== " ") {
      while ((p < DateTimeStr.length) && !(pas.System.Pos(DateTimeStr.charAt((p + 1) - 1),WhiteSpace$1) > 0)) p += 1;
    } else {
      p = pas.System.Pos($mod.TimeSeparator,DateTimeStr);
      if (p !== 0) do {
        p -= 1;
      } while (!((p === 0) || (pas.System.Pos(DateTimeStr.charAt(p - 1),WhiteSpace$1) > 0)));
    };
    if (p === 0) p = DateTimeStr.length;
    DateStr.set(pas.System.Copy(DateTimeStr,1,p));
    TimeStr.set($mod.Trim(pas.System.Copy(DateTimeStr,p + 1,100)));
    if (TimeStr.get().length !== 0) {
      Result = 2}
     else {
      Result = 1;
      if ((($mod.DateSeparator !== $mod.TimeSeparator) && (pas.System.Pos($mod.TimeSeparator,DateStr.get()) > 0)) || (($mod.DateSeparator === $mod.TimeSeparator) && !$mod.TryStrToDate(DateStr.get(),{get: function () {
          return DummyDT;
        }, set: function (v) {
          DummyDT = v;
        }}))) {
        TimeStr.set(DateStr.get());
        DateStr.set("");
      };
    };
    return Result;
  };
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
    this.Put = function (Index, Item) {
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      this.FList[Index] = Item;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.error(pas.RTLConsts.SListCapacityError,"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.error(pas.RTLConsts.SListCountError,"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
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
      if ((Index < 0) || (Index >= this.FCount)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.error = function (Msg, Data) {
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
      if ((Index < 0) || (Index > this.FCount)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
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
    this.IndexOf = function (Item) {
      var Result = 0;
      Result = this.FList.IndexOf(Item);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
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
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
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
      this.FSkipLastLineBreak = false;
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
    this.DoSetTextStr = function (Value, DoClear) {
      var S = "";
      var P = 0;
      try {
        this.BeginUpdate();
        if (DoClear) this.Clear();
        P = 1;
        while (this.GetNextLinebreak(Value,{get: function () {
            return S;
          }, set: function (v) {
            S = v;
          }},{get: function () {
            return P;
          }, set: function (v) {
            P = v;
          }})) this.Add(S);
      } finally {
        this.EndUpdate();
      };
    };
    this.GetLineBreak = function () {
      var Result = "";
      this.CheckSpecialChars();
      Result = this.FLineBreak;
      return Result;
    };
    this.GetSkipLastLineBreak = function () {
      var Result = false;
      this.CheckSpecialChars();
      Result = this.FSkipLastLineBreak;
      return Result;
    };
    this.error = function (Msg, Data) {
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
    this.GetTextStr = function () {
      var Result = "";
      var I = 0;
      var S = "";
      var NL = "";
      this.CheckSpecialChars();
      if (this.FLineBreak !== pas.System.sLineBreak) {
        NL = this.FLineBreak}
       else {
        var $tmp1 = this.FLBS;
        if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsLF) {
          NL = "\n"}
         else if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsCRLF) {
          NL = "\r\n"}
         else if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsCR) NL = "\r";
      };
      Result = "";
      for (var $l2 = 0, $end3 = this.GetCount() - 1; $l2 <= $end3; $l2++) {
        I = $l2;
        S = this.Get(I);
        Result = Result + S;
        if ((I < (this.GetCount() - 1)) || !this.GetSkipLastLineBreak()) Result = Result + NL;
      };
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetTextStr = function (Value) {
      this.CheckSpecialChars();
      this.DoSetTextStr(Value,true);
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
    this.GetNextLinebreak = function (Value, S, P) {
      var Result = false;
      var PP = 0;
      S.set("");
      Result = false;
      if ((Value.length - P.get()) < 0) return Result;
      PP = Value.indexOf(this.GetLineBreak(),P.get() - 1) + 1;
      if (PP < 1) PP = Value.length + 1;
      S.set(pas.System.Copy(Value,P.get(),PP - P.get()));
      P.set(PP + this.GetLineBreak().length);
      Result = true;
      return Result;
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
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.error(pas.RTLConsts.SListIndexError,AIndex);
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
      if (NewCapacity < 0) this.error(pas.RTLConsts.SListCapacityError,NewCapacity);
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
         else if ($tmp1 === pas.Types.TDuplicates.dupError) this.error(pas.RTLConsts.SDuplicateString,0);
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
        this.error(pas.RTLConsts.SSortedListError,0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.error(pas.RTLConsts.SListIndexError,Index);
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
    this.GetIndex = function () {
      var Result = 0;
      if (this.FCollection !== null) {
        Result = this.FCollection.FItems.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection !== null) this.FCollection.RemoveItem(this);
        if (Value !== null) Value.InsertItem(this);
      };
    };
    this.Changed = function (AllItems) {
      if ((this.FCollection !== null) && (this.FCollection.FUpdateCount === 0)) {
        if (AllItems) {
          this.FCollection.Update(null)}
         else this.FCollection.Update(this);
      };
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FCollection;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      this.Changed(false);
      if (Value === "") ;
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
    this.Owner = function () {
      var Result = null;
      Result = this.GetOwner();
      return Result;
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
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
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
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
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
    this.FreeNotification = function (AComponent) {
      if ((this.FOwner !== null) && (AComponent === this.FOwner)) return;
      if (!(this.FFreeNotifies != null)) this.FFreeNotifies = $mod.TFPList.$create("Create");
      if (this.FFreeNotifies.IndexOf(AComponent) === -1) {
        this.FFreeNotifies.Add(AComponent);
        AComponent.FreeNotification(this);
      };
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
rtl.module("WEBLib.Graphics",["System","Classes","SysUtils","Types","contnrs","Web","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.clBlack = 0x000000;
  this.clWhite = 0xFFFFFF;
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
      this.FColor = 0;
      this.FWidth = 0;
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
    };
    this.$final = function () {
      this.FStyle = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetHeight = function (aValue) {
      var d = 0.0;
      this.FHeight = aValue;
      d = (-this.FHeight * 72) / 96;
      this.FSize = Math.round(d);
      this.DoChange();
    };
    this.DoChange = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.apply(this,arguments);
      this.FName = "Tahoma";
      this.FSize = 8;
      this.FStyle = {};
      this.FColor = 0;
    };
    this.Assign = function (Source) {
      if ($mod.TFont.isPrototypeOf(Source)) {
        this.FName = rtl.as(Source,$mod.TFont).FName;
        this.FColor = rtl.as(Source,$mod.TFont).FColor;
        this.FSize = rtl.as(Source,$mod.TFont).FSize;
        this.FStyle = rtl.refSet(rtl.as(Source,$mod.TFont).FStyle);
      };
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
    this.SetStyle = function (AStyle) {
      this.FStyle = rtl.refSet(AStyle);
      this.DoChange();
    };
    this.SetColor = function (AColor) {
      if (this.FColor !== AColor) {
        this.FColor = AColor;
        this.DoChange();
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Charset",0,$mod.$rtti["TFontCharset"],"FCharset","FCharset");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Height",2,rtl.longint,"FHeight","SetHeight");
    $r.addProperty("Style",2,$mod.$rtti["TFontStyles"],"FStyle","SetStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
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
    var $r = this.$rtti;
    $r.addProperty("Font",0,$mod.$rtti["TFont"],"FFont","FFont");
    $r.addProperty("Brush",0,$mod.$rtti["TBrush"],"FBrush","FBrush");
    $r.addProperty("Pen",0,$mod.$rtti["TPen"],"FPen","FPen");
  });
  this.ColorToHex = function (aValue) {
    var Result = "";
    var s = "";
    s = Number(aValue).toString(16);
    while (s.length < 6) s = "0" + s;
    Result = (pas.System.Copy(s,5,2) + pas.System.Copy(s,3,2)) + pas.System.Copy(s,1,2);
    return Result;
  };
  this.ColorToHTML = function (aValue) {
    var Result = "";
    Result = "#" + $mod.ColorToHex(aValue);
    return Result;
  };
  $mod.$init = function () {
    $impl.FCache = $impl.TGraphicCacheList.$create("Create$2");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($impl,"TGraphicCacheList",pas.contnrs.TObjectList,function () {
  });
  $impl.FCache = null;
});
rtl.module("WEBLib.StdCtrls",["System","Classes","SysUtils","Types","Web","WEBLib.Controls","WEBLib.Graphics"],function () {
  "use strict";
  var $mod = this;
  this.TEditCharCase = {"0": "wecLowerCase", wecLowerCase: 0, "1": "wecNormal", wecNormal: 1, "2": "wecMixedCase", wecMixedCase: 2, "3": "wecUpperCase", wecUpperCase: 3};
  $mod.$rtti.$Enum("TEditCharCase",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TEditCharCase});
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
    this.BindElement = function () {
      this.FContent = this.FContainer.firstElementChild;
    };
    this.UpdateElement = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
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
        this.GetContentHandle().innerHTML = this.GetDisplayText();
        if (this.FEnabled) this.GetContentHandle().style.setProperty("color","#" + pas["WEBLib.Graphics"].ColorToHex(this.FFont.FColor));
        this.SetElementFont(this.GetContentHandle(),this.FFont);
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
      this.FAutoSize = false;
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
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("EllipsisPosition",2,$mod.$rtti["TEllipsisPosition"],"FEllipsisPosition","SetEllipsisPosition");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
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
  rtl.createClass($mod,"TCustomInput",pas["WEBLib.Controls"].TCustomControl,function () {
    this.GetInputType = function () {
      var Result = "";
      Result = "EDIT";
      return Result;
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("INPUT");
      Result.setAttribute("type",this.GetInputType());
      return Result;
    };
    this.UpdateElement = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
      if ((this.FContainer != null) && !this.IsUpdating()) {
        this.FContainer.style.setProperty("-moz-box-sizing","border-box");
        this.FContainer.style.setProperty("-webkit-box-sizing","border-box");
        this.FContainer.style.setProperty("box-sizing","border-box");
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.SetShowFocus(true);
    };
  });
  rtl.createClass($mod,"TCustomEdit",$mod.TCustomInput,function () {
    this.$init = function () {
      $mod.TCustomInput.$init.call(this);
      this.FCharCase = 0;
      this.FMaxLength = 0;
      this.FReadOnly = false;
      this.FText = "";
      this.FTextHint = "";
      this.FSelStart = 0;
      this.FAlignment = 0;
      this.FHideSelection = false;
      this.FPasswordChar = "";
      this.FOnChange = null;
      this.FAutoSize = false;
      this.FAutoSelect = false;
      this.FSelLength = 0;
      this.FNumeric = false;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      $mod.TCustomInput.$final.call(this);
    };
    this.SetAlignment = function (Value) {
      this.FAlignment = Value;
      this.UpdateElement();
    };
    this.SetHideSelection = function (Value) {
      this.FHideSelection = Value;
      this.UpdateElement();
    };
    this.SetAutoSelect = function (Value) {
      this.FAutoSelect = Value;
      this.UpdateElement();
    };
    this.SetAutoSize = function (Value) {
      this.FAutoSize = Value;
      this.UpdateElement();
    };
    this.GetElementInputHandle = function () {
      var Result = null;
      Result = this.FContainer;
      return Result;
    };
    this.SetPasswordChar = function (Value) {
      this.FPasswordChar = Value;
      this.UpdateElement();
    };
    this.GetInputType$1 = function () {
      var Result = "";
      if (this.FPasswordChar !== "") {
        Result = "PASSWORD"}
       else if (this.FNumeric) {
        Result = "NUMBER"}
       else Result = "TEXT";
      return Result;
    };
    this.PersistinHTML = function () {
      this.GetElementInputHandle().setAttribute("value",this.GetText());
    };
    this.DisableTab = function () {
      pas["WEBLib.Controls"].TControl.DisableTab.apply(this,arguments);
      this.FContainer.setAttribute("tabindex","-1");
    };
    this.GetText = function () {
      var Result = "";
      Result = this.FText;
      if (this.GetElementInputHandle() != null) Result = this.GetElementInputHandle().value;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = this.FText;
      return Result;
    };
    this.DoHandleChange = function (Event) {
      var Result = false;
      this.Change();
      Result = true;
      return Result;
    };
    this.IsReadOnly = function () {
      var Result = false;
      Result = this.FReadOnly;
      return Result;
    };
    this.BindEvents = function () {
      pas["WEBLib.Controls"].TCustomControl.BindEvents.apply(this,arguments);
      if (this.GetElementInputHandle() != null) this.GetElementInputHandle().oninput = rtl.createCallback(this,"DoHandleChange");
    };
    this.UpdateElement = function () {
      $mod.TCustomInput.UpdateElement.apply(this,arguments);
      if ((this.GetElementInputHandle() != null) && !this.IsUpdating()) {
        var $tmp1 = this.FCharCase;
        if ($tmp1 === $mod.TEditCharCase.wecUpperCase) {
          this.GetElementInputHandle().style.setProperty("text-transform","uppercase")}
         else if ($tmp1 === $mod.TEditCharCase.wecLowerCase) {
          this.GetElementInputHandle().style.setProperty("text-transform","lowercase")}
         else if ($tmp1 === $mod.TEditCharCase.wecMixedCase) {
          this.GetElementInputHandle().style.setProperty("text-transform","capitalize")}
         else if ($tmp1 === $mod.TEditCharCase.wecNormal) this.GetElementInputHandle().style.setProperty("text-transform","initial");
        this.GetElementInputHandle().readOnly = this.IsReadOnly();
        this.GetElementInputHandle().placeholder = this.FTextHint;
        this.GetElementInputHandle().setAttribute("type",this.GetInputType$1());
        if (this.FMaxLength <= 0) {
          this.GetElementInputHandle().removeAttribute("maxLength")}
         else this.GetElementInputHandle().maxLength = this.FMaxLength;
        this.GetElementInputHandle().value = this.GetDisplayText();
        if (!this.FNumeric) this.GetElementInputHandle().setSelectionRange(this.FSelStart,this.FSelStart + this.FSelLength);
      };
    };
    this.SetCharCase = function (AValue) {
      this.FCharCase = AValue;
      this.UpdateElement();
    };
    this.SetMaxLength = function (AValue) {
      this.FMaxLength = AValue;
      this.UpdateElement();
    };
    this.SetReadOnly = function (AValue) {
      this.FReadOnly = AValue;
      this.UpdateElement();
    };
    this.SetText = function (AValue) {
      this.FText = AValue;
      this.UpdateElement();
    };
    this.SetTextHint = function (AValue) {
      this.FTextHint = AValue;
      this.UpdateElement();
    };
    this.CreateInitialize = function () {
      $mod.TCustomInput.CreateInitialize.apply(this,arguments);
      this.FText = "";
      this.FCharCase = $mod.TEditCharCase.wecNormal;
      this.FMaxLength = 0;
      this.FReadOnly = false;
      this.FTextHint = "";
    };
    this.Change = function () {
      if (this.GetElementHandle() != null) this.FText = this.GetElementInputHandle().value;
      if (this.FOnChange != null) this.FOnChange(this);
    };
  });
  rtl.createClass($mod,"TEdit",$mod.TCustomEdit,function () {
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("AutoSelect",2,rtl.boolean,"FAutoSelect","SetAutoSelect");
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("CharCase",2,$mod.$rtti["TEditCharCase"],"FCharCase","SetCharCase");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("HideSelection",2,rtl.boolean,"FHideSelection","SetHideSelection");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("Margins",2,pas["WEBLib.Controls"].$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("MaxLength",2,rtl.longint,"FMaxLength","SetMaxLength");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowFocus",2,rtl.boolean,"FShowFocus","SetShowFocus");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Text",3,rtl.string,"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnKeyDown",0,pas["WEBLib.Controls"].$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas["WEBLib.Controls"].$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas["WEBLib.Controls"].$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas["WEBLib.Controls"].$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,pas["WEBLib.Controls"].$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,pas["WEBLib.Controls"].$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseLeave",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
  rtl.createClass($mod,"TButton",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FCaption = "";
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("BUTTON");
      Result.setAttribute("type","BUTTON");
      return Result;
    };
    this.SetCaption = function (AValue) {
      this.FCaption = AValue;
      if (this.FContainer != null) this.GetElementHandle().innerHTML = AValue;
    };
    this.DisableTab = function () {
      pas["WEBLib.Controls"].TControl.DisableTab.apply(this,arguments);
      if (this.FContainer != null) this.FContainer.setAttribute("tabindex","-1");
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FCaption = this.GetID();
    };
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Margins",2,pas["WEBLib.Controls"].$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnKeyDown",0,pas["WEBLib.Controls"].$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas["WEBLib.Controls"].$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas["WEBLib.Controls"].$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas["WEBLib.Controls"].$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,pas["WEBLib.Controls"].$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,pas["WEBLib.Controls"].$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseLeave",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
  rtl.createClass($mod,"TCustomMemo",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FBlockChange = false;
      this.FLines = null;
      this.FSelStart = 0;
      this.FSelLength = 0;
      this.FAutoSize = false;
      this.FOnChange = null;
      this.FReadOnly = false;
    };
    this.$final = function () {
      this.FLines = undefined;
      this.FOnChange = undefined;
      pas["WEBLib.Controls"].TCustomControl.$final.call(this);
    };
    this.GetText = function () {
      var Result = "";
      if (this.GetElementInputHandle() != null) {
        this.FBlockChange = true;
        this.FLines.SetTextStr(this.GetElementInputHandle().value);
        this.FBlockChange = false;
      };
      Result = this.FLines.GetTextStr();
      return Result;
    };
    this.SetText = function (Value) {
      this.FLines.SetTextStr(Value);
    };
    this.SetSelLength = function (Value) {
      if (this.FSelLength !== Value) {
        this.FSelLength = Value;
        this.UpdateElement();
      };
    };
    this.SetSelStart = function (Value) {
      if (this.FSelStart !== Value) {
        this.FSelStart = Value;
        this.UpdateElement();
      };
    };
    this.SetAutoSize = function (Value) {
      if (this.FAutoSize !== Value) {
        this.FAutoSize = Value;
        this.UpdateElement();
      };
    };
    this.GetElementInputHandle = function () {
      var Result = null;
      Result = this.FContainer;
      return Result;
    };
    this.SetReadOnly = function (Value) {
      if (this.FReadOnly !== Value) {
        this.FReadOnly = Value;
        this.UpdateElement();
      };
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("TEXTAREA");
      return Result;
    };
    this.DoHandleInput = function (Event) {
      var Result = false;
      this.GetText();
      this.Change();
      Result = true;
      return Result;
    };
    this.DoHandleChange = function (Event) {
      var Result = false;
      this.Change();
      Result = true;
      return Result;
    };
    this.IsReadOnly = function () {
      var Result = false;
      Result = this.FReadOnly;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = this.FLines.GetTextStr();
      return Result;
    };
    this.BindEvents = function () {
      pas["WEBLib.Controls"].TCustomControl.BindEvents.apply(this,arguments);
      if (this.GetElementInputHandle() != null) this.GetElementInputHandle().oninput = rtl.createCallback(this,"DoHandleInput");
      if (this.GetElementInputHandle() != null) this.GetElementInputHandle().onchange = rtl.createCallback(this,"DoHandleChange");
    };
    this.UpdateElement = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
      if (((this.GetElementInputHandle() != null) && !this.IsUpdating()) && !this.FBlockChange) {
        this.GetElementInputHandle().value = this.GetDisplayText();
        this.GetElementInputHandle().setSelectionRange(this.FSelStart,this.FSelStart + this.FSelLength);
        this.GetElementInputHandle().style.setProperty("resize","none");
        this.GetElementInputHandle().readOnly = this.IsReadOnly();
      };
    };
    this.SetLines = function (ALines) {
      this.FLines.Assign(ALines);
    };
    this.DoLinesChange = function (Sender) {
      this.UpdateElement();
    };
    this.Change = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FLines = pas.Classes.TStringList.$create("Create$1");
      this.FLines.FOnChange = rtl.createCallback(this,"DoLinesChange");
      this.SetShowFocus(true);
    };
    this.Destroy = function () {
      rtl.free(this,"FLines");
      pas["WEBLib.Controls"].TCustomControl.Destroy.apply(this,arguments);
    };
  });
  rtl.createClass($mod,"TMemo",$mod.TCustomMemo,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Lines",2,pas.Classes.$rtti["TStringList"],"FLines","SetLines");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("SelStart",2,rtl.longint,"FSelStart","SetSelStart");
    $r.addProperty("SelLength",2,rtl.longint,"FSelLength","SetSelLength");
    $r.addProperty("ShowFocus",2,rtl.boolean,"FShowFocus","SetShowFocus");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Text",3,rtl.string,"GetText","SetText");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnKeyDown",0,pas["WEBLib.Controls"].$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas["WEBLib.Controls"].$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas["WEBLib.Controls"].$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas["WEBLib.Controls"].$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,pas["WEBLib.Controls"].$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,pas["WEBLib.Controls"].$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseLeave",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
  rtl.createClass($mod,"TSpinEdit",$mod.TCustomInput,function () {
    this.$init = function () {
      $mod.TCustomInput.$init.call(this);
      this.FIncrement = 0;
      this.FMaxValue = 0;
      this.FMinValue = 0;
      this.FShowFocus$1 = false;
      this.FValue = 0;
      this.FAutoSize = false;
      this.FOnChange = null;
      this.FReadOnly = false;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      $mod.TCustomInput.$final.call(this);
    };
    this.DoHandleChange = function (Event) {
      var Result = false;
      this.Change();
      Result = true;
      return Result;
    };
    this.IsReadOnly = function () {
      var Result = false;
      Result = this.FReadOnly;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = pas.SysUtils.IntToStr(this.FValue);
      return Result;
    };
    this.SetText = function (Value) {
    };
    this.GetElementInputHandle = function () {
      var Result = null;
      Result = this.FContainer;
      return Result;
    };
    this.SetReadOnly = function (Value) {
      if (this.FReadOnly !== Value) {
        this.FReadOnly = Value;
        this.UpdateElement();
      };
    };
    this.PersistinHTML = function () {
      this.GetElementInputHandle().setAttribute("value",pas.SysUtils.IntToStr(this.GetValue()));
    };
    this.DisableTab = function () {
      pas["WEBLib.Controls"].TControl.DisableTab.apply(this,arguments);
      this.FContainer.setAttribute("tabindex","-1");
    };
    this.UpdateElement = function () {
      $mod.TCustomInput.UpdateElement.apply(this,arguments);
      if ((this.FContainer != null) && !this.IsUpdating()) {
        this.FContainer.setAttribute("inputmode","numeric");
        this.FContainer.setAttribute("pattern","[0-9]*");
        this.FContainer.value = this.GetDisplayText();
        this.GetElementInputHandle().readOnly = this.IsReadOnly();
      };
    };
    this.GetValue = function () {
      var Result = 0;
      var s = "";
      Result = this.FValue;
      if (!(this.FContainer != null)) return Result;
      s = this.FContainer.value;
      if (s !== "") Result = pas.SysUtils.StrToInt(s);
      return Result;
    };
    this.SetIncrement = function (AValue) {
      this.FIncrement = AValue;
      if (this.FContainer != null) this.FContainer.setAttribute("step",pas.SysUtils.IntToStr(AValue));
    };
    this.SetMaxValue = function (AValue) {
      this.FMaxValue = AValue;
      if (this.FContainer != null) this.FContainer.setAttribute("max",pas.SysUtils.IntToStr(AValue));
    };
    this.SetMinValue = function (AValue) {
      this.FMinValue = AValue;
      if (this.FContainer != null) this.FContainer.setAttribute("min",pas.SysUtils.IntToStr(AValue));
    };
    this.SetValue = function (AValue) {
      this.FValue = AValue;
      this.UpdateElement();
    };
    this.BindEvents = function () {
      if (this.GetElementInputHandle() != null) this.GetElementInputHandle().oninput = rtl.createCallback(this,"DoHandleChange");
    };
    this.Change = function () {
      this.FValue = this.GetValue();
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.CreateInitialize = function () {
      $mod.TCustomInput.CreateInitialize.apply(this,arguments);
      this.FIncrement = 1;
      this.FMaxValue = 0;
      this.FMinValue = 0;
      this.SetShowFocus(true);
    };
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",0,rtl.boolean,"FAutoSize","FAutoSize");
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Increment",2,rtl.longint,"FIncrement","SetIncrement");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Margins",2,pas["WEBLib.Controls"].$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("MaxValue",2,rtl.longint,"FMaxValue","SetMaxValue");
    $r.addProperty("MinValue",2,rtl.longint,"FMinValue","SetMinValue");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowFocus",2,rtl.boolean,"FShowFocus$1","SetShowFocus");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("Value",3,rtl.longint,"GetValue","SetValue");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnKeyDown",0,pas["WEBLib.Controls"].$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas["WEBLib.Controls"].$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas["WEBLib.Controls"].$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas["WEBLib.Controls"].$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,pas["WEBLib.Controls"].$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,pas["WEBLib.Controls"].$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseLeave",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
});
rtl.module("WEBLib.Controls",["System","Classes","SysUtils","JS","Types","Web","WEBLib.Graphics"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TAlign = {"0": "alNone", alNone: 0, "1": "alTop", alTop: 1, "2": "alBottom", alBottom: 2, "3": "alLeft", alLeft: 3, "4": "alRight", alRight: 4, "5": "alClient", alClient: 5, "6": "alCustom", alCustom: 6};
  $mod.$rtti.$Enum("TAlign",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TAlign});
  this.TMouseButton = {"0": "mbLeft", mbLeft: 0, "1": "mbRight", mbRight: 1, "2": "mbMiddle", mbMiddle: 2};
  $mod.$rtti.$Enum("TMouseButton",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TMouseButton});
  this.TBorderStyle = {"0": "bsNone", bsNone: 0, "1": "bsSingle", bsSingle: 1};
  $mod.$rtti.$Enum("TBorderStyle",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TBorderStyle});
  this.TControlPosition = {"0": "cpAbsolute", cpAbsolute: 0, "1": "cpRelative", cpRelative: 1, "2": "cpNone", cpNone: 2};
  this.TSizeStyle = {"0": "ssPercent", ssPercent: 0, "1": "ssAbsolute", ssAbsolute: 1, "2": "ssAuto", ssAuto: 2};
  $mod.$rtti.$Enum("TSizeStyle",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TSizeStyle});
  this.TAnchorKind = {"0": "akLeft", akLeft: 0, "1": "akTop", akTop: 1, "2": "akRight", akRight: 2, "3": "akBottom", akBottom: 3};
  $mod.$rtti.$Enum("TAnchorKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TAnchorKind});
  this.TShiftStateEnum = {"0": "ssShift", ssShift: 0, "1": "ssAlt", ssAlt: 1, "2": "ssCtrl", ssCtrl: 2, "3": "ssLeft", ssLeft: 3, "4": "ssRight", ssRight: 4, "5": "ssMIDdle", ssMIDdle: 5, "6": "ssDouble", ssDouble: 6, "7": "ssTouch", ssTouch: 7, "8": "ssPen", ssPen: 8, "9": "ssCommand", ssCommand: 9, "10": "ssHorizontal", ssHorizontal: 10};
  $mod.$rtti.$Enum("TShiftStateEnum",{minvalue: 0, maxvalue: 10, ordtype: 1, enumtype: this.TShiftStateEnum});
  this.TControlStyleValue = {"0": "csAcceptsControls", csAcceptsControls: 0};
  $mod.$rtti.$Set("TAnchorKindSet",{comptype: $mod.$rtti["TAnchorKind"]});
  $mod.$rtti.$Set("TShiftState",{comptype: $mod.$rtti["TShiftStateEnum"]});
  $mod.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
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
    this.SetLeft = function (aValue) {
      if (aValue !== this.FLeft) {
        this.FLeft = aValue;
        this.DoChange();
      };
    };
    this.SetTop = function (aValue) {
      if (aValue !== this.FTop) {
        this.FTop = aValue;
        this.DoChange();
      };
    };
    this.SetRight = function (aValue) {
      if (aValue !== this.FRight) {
        this.FRight = aValue;
        this.DoChange();
      };
    };
    this.SetBottom = function (aValue) {
      if (aValue !== this.FBottom) {
        this.FBottom = aValue;
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
  $mod.$rtti.$MethodVar("TMouseDownEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",$mod.$rtti["TMouseButton"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseUpEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",$mod.$rtti["TMouseButton"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TMouseMoveEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",$mod.$rtti["TShiftState"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TTouchEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["X",rtl.longint],["Y",rtl.longint]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TKeyDownEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.longint,1],["Shift",$mod.$rtti["TShiftState"]]],rtl.boolean), methodkind: 1});
  $mod.$rtti.$MethodVar("TKeyPressEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.char,1]],rtl.boolean), methodkind: 1});
  $mod.$rtti.$MethodVar("TKeyUpEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.longint,1],["Shift",$mod.$rtti["TShiftState"]]]), methodkind: 0});
  $mod.$rtti.$Class("TControl");
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
      this.FElementEvent = null;
      this.FElementClassName = "";
      this.FColor = 0;
      this.FFont = null;
      this.FParent = null;
      this.FPrevParent = null;
      this.FControls = [];
      this.FOnDragDrop = null;
      this.FOnStartDrag = null;
      this.FOnClick = null;
      this.FOnDblClick = null;
      this.FOnMouseDown = null;
      this.FOnMouseEnter = null;
      this.FOnMouseLeave = null;
      this.FOnMouseMove = null;
      this.FOnMouseUp = null;
      this.FOnMouseWheel = null;
      this.FOnTouchStart = null;
      this.FOnTouchMove = null;
      this.FOnTouchEnd = null;
      this.FOnEnter = null;
      this.FOnExit = null;
      this.FOnKeyDown = null;
      this.FOnKeyPress = null;
      this.FOnKeyUp = null;
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
      this.FControlPosition = 0;
      this.FAlign = 0;
      this.FAnchors = {};
      this.FAlignWithMargins = false;
      this.FIsAligning = false;
      this.FCursor = 0;
      this.FControlStyle = {};
      this.FMargins = null;
      this.FParentFont = false;
      this.FLinkTouchEvents = false;
      this.FWidthStyle = 0;
      this.FHeightStyle = 0;
      this.FWidthPercent = 0;
      this.FHeightPercent = 0;
      this.FOrigParentRect = new pas.Types.TRect();
      this.FIsResizing = false;
      this.FShowFocus = false;
      this.FBorderWidth = 0;
      this.FOrigRect = new pas.Types.TRect();
      this.FContainer = null;
    };
    this.$final = function () {
      this.FLayer = undefined;
      this.FElement = undefined;
      this.FElementEvent = undefined;
      this.FFont = undefined;
      this.FParent = undefined;
      this.FPrevParent = undefined;
      this.FControls = undefined;
      this.FOnDragDrop = undefined;
      this.FOnStartDrag = undefined;
      this.FOnClick = undefined;
      this.FOnDblClick = undefined;
      this.FOnMouseDown = undefined;
      this.FOnMouseEnter = undefined;
      this.FOnMouseLeave = undefined;
      this.FOnMouseMove = undefined;
      this.FOnMouseUp = undefined;
      this.FOnMouseWheel = undefined;
      this.FOnTouchStart = undefined;
      this.FOnTouchMove = undefined;
      this.FOnTouchEnd = undefined;
      this.FOnEnter = undefined;
      this.FOnExit = undefined;
      this.FOnKeyDown = undefined;
      this.FOnKeyPress = undefined;
      this.FOnKeyUp = undefined;
      this.FAnchors = undefined;
      this.FControlStyle = undefined;
      this.FMargins = undefined;
      this.FOrigParentRect = undefined;
      this.FOrigRect = undefined;
      this.FContainer = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.Create$2 = function (ID) {
      this.CreateWithID(ID);
    };
    this.Create$1 = function (AOwner) {
      var s = "";
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      s = this.$classname;
      pas.System.Delete({get: function () {
          return s;
        }, set: function (v) {
          s = v;
        }},1,1);
      this.CreateWithID($impl.FindUniqueName(s));
    };
    this.Destroy = function () {
      this.UnbindEvents();
      if (((this.FContainer !== null) && (this.FParent !== null)) && (this.FParent.FContainer !== null)) {
        this.FParent.FContainer.removeChild(this.FContainer);
        this.FControlCreated = false;
      };
      rtl.free(this,"FMargins");
      rtl.free(this,"FFont");
      pas.Classes.TComponent.Destroy.apply(this,arguments);
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
    this.SetVisible = function (aValue) {
      if (this.FVisible !== aValue) {
        this.VisibleChanging();
        this.FVisible = aValue;
        this.UpdateElement();
        this.DoRealign();
      };
    };
    this.SetHint = function (aValue) {
      if (this.FHint !== aValue) {
        this.FHint = aValue;
        this.UpdateElement();
      };
    };
    this.SetShowHint = function (aValue) {
      if (this.FShowHint !== aValue) {
        this.FShowHint = aValue;
        this.UpdateElement();
      };
    };
    this.SetTabOrder = function (aValue) {
      if (this.FTabOrder !== aValue) {
        this.FTabOrder = aValue;
        this.UpdateElement();
      };
    };
    this.SetTabStop = function (aValue) {
      if (this.FTabStop !== aValue) {
        this.FTabStop = aValue;
        this.UpdateElement();
      };
    };
    this.SetTop = function (aValue) {
      if (this.FTop !== aValue) {
        this.FTop = aValue;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.SetLeft = function (aValue) {
      if (this.FLeft !== aValue) {
        this.FLeft = aValue;
        this.UpdateElementSize();
        this.RecreateCanvas();
        this.InternalResize();
      };
    };
    this.SetHeight = function (aValue) {
      var dr = null;
      if (this.FHeight !== aValue) {
        if ((this.FAlign in rtl.createSet($mod.TAlign.alLeft,$mod.TAlign.alRight,$mod.TAlign.alClient)) && (this.GetElementHandle() !== null)) {
          dr = this.GetElementHandle().getBoundingClientRect();
          if ((dr.top + aValue) >= window.innerHeight) {
            aValue = window.innerHeight - pas.System.Trunc(dr.top);
            if (this.FAlignWithMargins) aValue = (aValue - this.FMargins.FBottom) - this.FMargins.FTop;
          };
        };
        this.FHeight = aValue;
        this.DoBoundsChange();
      };
    };
    this.SetWidth = function (aValue) {
      if (this.FWidth !== aValue) {
        this.FWidth = aValue;
        this.DoBoundsChange();
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
      if (this.FContainer !== null) this.FContainer.setAttribute("id",Value);
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
    this.SetWidthPercent = function (Value) {
      if (this.FWidthPercent !== Value) {
        this.FWidthPercent = Value;
        this.UpdateElementSize();
      };
    };
    this.SetShowFocus = function (Value) {
      if (this.FShowFocus !== Value) {
        this.FShowFocus = Value;
        this.UpdateElement();
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
      if (((Result === -1) && (this.GetElementHandle() !== null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetWidth);
      return Result;
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = this.FHeight;
      if (((Result === -1) && (this.GetElementHandle() !== null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetHeight);
      return Result;
    };
    this.GetLeft = function () {
      var Result = 0;
      Result = this.FLeft;
      if (((Result === -1) && (this.GetElementHandle() !== null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetLeft);
      return Result;
    };
    this.GetTop = function () {
      var Result = 0;
      Result = this.FTop;
      if (((Result === -1) && (this.GetElementHandle() !== null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FComponentState)) Result = Math.round(this.GetElementHandle().offsetTop);
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
    this.RecreateElement = function () {
      if (this.FContainer != null) {
        this.UnbindEvents();
        this.FContainer.parentNode.removeChild(this.FContainer);
        this.FElement = null;
        this.CreateControl();
        if ((this.FParent != null) && (this.FParent.FContainer != null)) this.FParent.FContainer.appendChild(this.FContainer);
      };
    };
    this.BindElement = function () {
    };
    this.CreateInitialize = function () {
    };
    this.ClearControls = function () {
      this.FControls = rtl.arraySetLength(this.FControls,null,0);
    };
    this.GetMouseEventButton = function (event) {
      var Result = 0;
      Result = $mod.TMouseButton.mbLeft;
      var $tmp1 = event.button;
      if ($tmp1 === 0) {
        Result = $mod.TMouseButton.mbLeft}
       else if ($tmp1 === 1) {
        Result = $mod.TMouseButton.mbMiddle}
       else if ($tmp1 === 2) Result = $mod.TMouseButton.mbRight;
      return Result;
    };
    this.GetMouseEventShiftState = function (event) {
      var Result = {};
      if (event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssShift));
      if (event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssCtrl));
      if (event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssAlt));
      return Result;
    };
    this.GetKeyBoardEventShiftState = function (event) {
      var Result = {};
      if (event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssShift));
      if (event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssCtrl));
      if (event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssAlt));
      return Result;
    };
    this.GetMouseWheelEventShiftState = function (event) {
      var Result = {};
      if (event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssShift));
      if (event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssCtrl));
      if (event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssAlt));
      return Result;
    };
    this.GetTouchEventShiftState = function (event) {
      var Result = {};
      if (event.shiftKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssShift));
      if (event.ctrlKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssCtrl));
      if (event.altKey) Result = rtl.unionSet(Result,rtl.createSet($mod.TShiftStateEnum.ssAlt));
      return Result;
    };
    this.HandleDoClick = function (event) {
      var Result = false;
      this.FElementEvent = event;
      this.StopPropagation();
      this.Click();
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoWheel = function (event) {
      var Result = false;
      var ss = {};
      var h = false;
      this.FElementEvent = event;
      this.StopPropagation();
      ss = rtl.refSet(this.GetMouseWheelEventShiftState(event));
      h = true;
      this.MouseWheel(rtl.refSet(ss),pas.System.Trunc(-event.deltaY),{get: function () {
          return h;
        }, set: function (v) {
          h = v;
        }});
      if (this.FOnMouseWheel != null) this.FOnMouseWheel(this,rtl.refSet(ss),pas.System.Trunc(-event.deltaY),new pas.Types.TPoint(pas.Types.Point(0,0)),{get: function () {
          return h;
        }, set: function (v) {
          h = v;
        }});
      Result = h;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoDblClick = function (event) {
      var Result = false;
      this.FElementEvent = event;
      this.StopPropagation();
      this.DblClick();
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseDown = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      var mb = 0;
      this.FElementEvent = event;
      this.StopPropagation();
      l = ((event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p !== null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(event));
      mb = this.GetMouseEventButton(event);
      this.MouseDown(mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseDown !== null) this.FOnMouseDown(this,mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseUp = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      var mb = 0;
      this.FElementEvent = event;
      this.StopPropagation();
      l = ((event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p !== null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(event));
      mb = this.GetMouseEventButton(event);
      this.MouseUp(mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseUp !== null) this.FOnMouseUp(this,mb,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseMove = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var p = null;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      l = ((event.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
      t = ((event.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
      p = this.FParent;
      while (p !== null) {
        l = l - p.GetLeft();
        t = t - p.GetTop();
        p = p.FParent;
      };
      ss = rtl.refSet(this.GetMouseEventShiftState(event));
      this.MouseMove(rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      if (this.FOnMouseMove !== null) this.FOnMouseMove(this,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoMouseLeave = function (event) {
      var Result = false;
      if (this.Captured()) return Result;
      event.stopPropagation();
      this.DoMouseLeave();
      if (this.FOnMouseLeave !== null) this.FOnMouseLeave(this);
      Result = true;
      return Result;
    };
    this.HandleDoMouseEnter = function (event) {
      var Result = false;
      if (this.Captured()) return Result;
      event.stopPropagation();
      this.DoMouseEnter();
      if (this.FOnMouseEnter !== null) this.FOnMouseEnter(this);
      Result = true;
      return Result;
    };
    this.HandleDoKeyDown = function (event) {
      var Result = false;
      var k = 0;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      k = this.GetKeyCode(event.key);
      ss = rtl.refSet(this.GetKeyBoardEventShiftState(event));
      this.KeyDown({get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      if (this.FOnKeyDown !== null) this.FOnKeyDown(this,{get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoKeyUp = function (event) {
      var Result = false;
      var k = 0;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      k = this.GetKeyCode(event.key);
      ss = rtl.refSet(this.GetKeyBoardEventShiftState(event));
      this.KeyUp({get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      if (this.FOnKeyUp !== null) this.FOnKeyUp(this,{get: function () {
          return k;
        }, set: function (v) {
          k = v;
        }},rtl.refSet(ss));
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoKeyPress = function (event) {
      var Self = this;
      var Result = false;
      var c = "";
      Self.FElementEvent = event;
      Self.StopPropagation();
      c = String.fromCharCode(Self.GetKeyCode(event.key));
      Self.KeyPress({get: function () {
          return c;
        }, set: function (v) {
          c = v;
        }});
      if (Self.FOnKeyPress !== null) Self.FOnKeyPress(Self,{get: function () {
          return c;
        }, set: function (v) {
          c = v;
        }});
      Result = true;
      Self.FElementEvent = null;
      return Result;
    };
    this.HandleDoExit = function (event) {
      var Result = false;
      this.DoExit();
      if (this.FOnExit !== null) this.FOnExit(this);
      Result = true;
      return Result;
    };
    this.HandleDoEnter = function (event) {
      var Result = false;
      this.FElementEvent = event;
      this.StopPropagation();
      this.DoEnter();
      if (this.FOnEnter !== null) this.FOnEnter(this);
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoTouchStart = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (event.touches.length > 0) {
        touch = event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p !== null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(event));
        if (this.FLinkTouchEvents) {
          this.MouseDown($mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseDown !== null) this.FOnMouseDown(this,$mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchStart(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchStart !== null) this.FOnTouchStart(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      Result = true;
      this.FElementEvent = null;
      return Result;
    };
    this.HandleDoTouchMove = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (event.touches.length > 0) {
        touch = event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p !== null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(event));
        if (this.FLinkTouchEvents) {
          this.MouseMove(rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseMove !== null) this.FOnMouseMove(this,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchMove(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchMove !== null) this.FOnTouchMove(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      this.FElementEvent = null;
      Result = true;
      return Result;
    };
    this.HandleDoTouchEnd = function (event) {
      var Result = false;
      var l = 0.0;
      var t = 0.0;
      var touch = null;
      var p = null;
      var ss = {};
      this.FElementEvent = event;
      this.StopPropagation();
      if (this.IsFocused()) this.PreventDefault();
      if (event.touches.length > 0) {
        touch = event.touches.item(0);
        l = ((touch.clientX - this.GetLeft()) + document.body.scrollLeft) + document.documentElement.scrollLeft;
        t = ((touch.clientY - this.GetTop()) + document.body.scrollTop) + document.documentElement.scrollTop;
        p = this.FParent;
        while (p !== null) {
          l = l - p.GetLeft();
          t = t - p.GetTop();
          p = p.FParent;
        };
        ss = rtl.refSet(this.GetTouchEventShiftState(event));
        if (this.FLinkTouchEvents) {
          this.MouseUp($mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
          if (this.FOnMouseUp !== null) this.FOnMouseUp(this,$mod.TMouseButton.mbLeft,rtl.refSet(ss),pas.System.Trunc(l),pas.System.Trunc(t));
        };
        this.TouchEnd(pas.System.Trunc(l),pas.System.Trunc(t));
        if (this.FOnTouchEnd !== null) this.FOnTouchEnd(this,pas.System.Trunc(l),pas.System.Trunc(t));
      };
      this.FElementEvent = null;
      Result = true;
      return Result;
    };
    this.DoExit = function () {
    };
    this.DoEnter = function () {
    };
    this.Click = function () {
      if (this.FOnClick !== null) this.FOnClick(this);
    };
    this.UpdateElement = function () {
      var eh = null;
      if (this.FBlockUpdateElement || (this.FUpdateCount > 0)) return;
      if ((this.GetElementHandle() !== null) && (this.GetElementHandle() !== this.ContainerElement())) {
        eh = this.GetElementHandle();
        if (this.FParentFont && (this.FParent !== null)) this.FFont.Assign(this.FParent.FFont);
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
        if (this.FVisible) {
          eh.style.setProperty("visibility","")}
         else eh.style.setProperty("visibility","hidden");
        if (this.FTabStop) {
          this.FContainer.setAttribute("tabindex",pas.SysUtils.IntToStr(this.FTabOrder + 1))}
         else this.FContainer.setAttribute("tabindex","-1");
        if (this.FEnabled) {
          this.FContainer.removeAttribute("disabled")}
         else this.FContainer.setAttribute("disabled","disabled");
        if (this.FElementClassName === "") {
          if (this.FEnabled) {
            eh.style.setProperty("color","#" + pas["WEBLib.Graphics"].ColorToHex(this.FFont.FColor));
            this.SetElementFont(eh,this.FFont);
          } else {
            eh.style.setProperty("color","");
            eh.style.setProperty("font-family","");
            eh.style.setProperty("font-style","");
            eh.style.setProperty("font-size","");
          };
        };
        if (this.FShowHint && (this.FHint !== "")) {
          this.FContainer.setAttribute("title",this.FHint)}
         else this.FContainer.setAttribute("title","");
      };
    };
    this.UpdateElementSize = function () {
      var eh = null;
      if (this.FBlockUpdateElement || (this.FUpdateCount > 0)) return;
      if ((this.GetElementHandle() !== null) && (this.GetElementHandle() !== this.ContainerElement())) {
        eh = this.GetElementHandle();
        if (this.FControlPosition === $mod.TControlPosition.cpAbsolute) {
          if (this.FTop !== -1) {
            eh.style.setProperty("top",pas.SysUtils.IntToStr(this.FTop) + "px")}
           else eh.style.setProperty("top","");
          if (this.FLeft !== -1) {
            eh.style.setProperty("left",pas.SysUtils.IntToStr(this.FLeft) + "px")}
           else eh.style.setProperty("left","");
        };
        if (this.FWidthStyle === $mod.TSizeStyle.ssAbsolute) {
          if (this.FWidth !== -1) {
            eh.style.setProperty("width",pas.SysUtils.IntToStr(this.FWidth - this.FBorderWidth) + "px")}
           else eh.style.setProperty("width","");
        };
        if (this.FHeightStyle === $mod.TSizeStyle.ssAbsolute) {
          if (this.FHeight !== -1) {
            eh.style.setProperty("height",pas.SysUtils.IntToStr(this.FHeight - this.FBorderWidth) + "px")}
           else eh.style.setProperty("height","");
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
        if (this.FControlPosition === $mod.TControlPosition.cpAbsolute) {
          eh.style.setProperty("position","absolute")}
         else if (this.FControlPosition === $mod.TControlPosition.cpRelative) {
          eh.style.setProperty("position","relative")}
         else eh.style.setProperty("position","");
      };
    };
    this.UpdateParent = function () {
      this.InternalUpdateParent();
      if (((pas.Classes.TComponentStateItem.csLoading in this.FComponentState) && (this.FParent !== null)) && !(pas.Classes.TComponentStateItem.csLoading in this.FParent.FComponentState)) this.Loaded();
      this.UpdateChildren(this.FPrevParent);
      this.UpdateChildren(this.FParent);
    };
    this.InternalUpdateParent = function () {
      var p = null;
      p = this.FPrevParent;
      if (this.FNew) {
        this.CreateControl();
        if (this.FContainer !== null) {
          if ((this.FParent !== null) && !(this.FParent.FContainer !== null)) this.FParent.CreateControl();
          if (((this.FParent !== p) && (p != null)) && (p.FContainer !== null)) p.FContainer.removeChild(this.FContainer);
          if ((this.FParent !== null) && (this.FParent.FContainer !== null)) this.FParent.FContainer.appendChild(this.FContainer);
        };
      };
    };
    this.UpdateChildren = function (AControl) {
      var I = 0;
      var c = null;
      if (AControl !== null) {
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
    this.DisableTab = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).DisableTab();
      };
    };
    this.SetElementClassName = function (aValue) {
      this.FElementClassName = aValue;
      if (this.FContainer !== null) this.FContainer.setAttribute("class",aValue);
    };
    this.SetColor = function (aValue) {
      this.FColor = aValue;
      this.UpdateElement();
    };
    this.SetFont = function (aValue) {
      this.FFont.SetName(aValue.FName);
      this.FFont.SetSize(aValue.FSize);
      this.FFont.SetStyle(rtl.refSet(aValue.FStyle));
      this.FFont.SetColor(aValue.FColor);
    };
    this.SetParent = function (aValue) {
      if (this.FParent !== aValue) {
        if (this.FParent !== null) this.FParent.UnRegisterParent(this);
        this.FPrevParent = this.FParent;
        this.FParent = aValue;
        if (this.FParent !== null) this.FParent.RegisterParent(this);
        this.UpdateParent();
        this.UpdateElement();
      };
    };
    this.RegisterParent = function (aValue) {
      this.FControls = rtl.arraySetLength(this.FControls,null,rtl.length(this.FControls) + 1);
      this.FControls[rtl.length(this.FControls) - 1] = aValue;
    };
    this.UnRegisterParent = function (aValue) {
      var i = 0;
      var flg = false;
      flg = false;
      for (var $l1 = 0, $end2 = rtl.length(this.FControls) - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (this.FControls[i] === aValue) flg = true;
        if (flg && (i < (rtl.length(this.FControls) - 1))) this.FControls[i] = this.FControls[i + 1];
      };
      if (flg) this.FControls = rtl.arraySetLength(this.FControls,null,rtl.length(this.FControls) - 1);
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
      if ((this.FParent != null) && !this.FParent.IsUpdating()) this.FParent.AlignControl(this.FParent);
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
      if (frm !== null) {
        b = frm.FIsResizing;
        b = b || (frm.FUpdateCount > 0);
      };
      b = b || (this.FUpdateCount > 0);
      b = b || this.FIsAligning;
      if (!b) this.UpdateChildAnchoring();
      this.FIsResizing = false;
    };
    this.DoStartDrag = function (DragObject) {
      if (this.FOnStartDrag != null) this.FOnStartDrag(this,DragObject);
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
      if ((this.FContainer !== null) && this.FControlCreated) Result = this.FContainer;
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
      if ($tmp1 === pas.Web.TJSKeyNames.BackSpace) {
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
        i = AValue.charCodeAt(0);
      };
      return Result;
    };
    this.GetClientRect = function () {
      var Result = new pas.Types.TRect();
      Result = new pas.Types.TRect(pas.Types.Rect(0,0,this.GetWidth(),this.GetHeight()));
      return Result;
    };
    this.CreateControl = function () {
      if (!(this.FElement !== null)) {
        this.FElement = this.CreateElement();
        if (this.FElement !== null) {
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
      if (this.GetElementBindHandle() !== null) {
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
      if (this.GetElementBindHandle() !== null) {
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
    this.SetElementFont = function (he, Font) {
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
          Control.SetWidth(((Rect.get().Right - Rect.get().Left) - dl) - dr);
          Control.SetTop(((Rect.get().Bottom - Control.GetHeight()) - dt) - db);
          Control.SetLeft(Rect.get().Left + dl);
          Rect.get().Bottom = ((Rect.get().Bottom - Control.GetHeight()) - dt) - db;
        } else if ($tmp1 === $mod.TAlign.alLeft) {
          Control.SetHeight(((Rect.get().Bottom - Rect.get().Top) - dt) - db);
          Control.SetLeft(Rect.get().Left + dl);
          Control.SetTop(Rect.get().Top + dt);
          Rect.get().Left = ((Rect.get().Left + Control.GetWidth()) + dl) + dr;
        } else if ($tmp1 === $mod.TAlign.alRight) {
          Control.SetHeight(((Rect.get().Bottom - Rect.get().Top) - db) - dt);
          Control.SetLeft((Rect.get().Right - Control.GetWidth()) - dr);
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
      };
      function DoAlign(AAlign) {
        var i = 0;
        for (var $l1 = 0, $end2 = Self.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if ((Self.GetControls(i).FAlign === AAlign) && Self.GetControls(i).FVisible) DoPosition(Self.GetControls(i),AAlign);
        };
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
      var ovf = "";
      var ovfx = "";
      var ovfy = "";
      var eh = null;
      var frm = null;
      if (this.FIsAligning) return;
      if (this.IsUpdating()) return;
      frm = pas["WEBLib.Forms"].GetParentForm(this);
      if ((frm !== null) && frm.IsUpdating()) return;
      this.FIsAligning = true;
      if (!(AControl !== null)) return;
      eh = AControl.GetElementHandle();
      if (eh !== null) {
        ovf = eh.style.getPropertyValue("overflow");
        ovfx = eh.style.getPropertyValue("overflow-x");
        ovfy = eh.style.getPropertyValue("overflow-y");
        eh.style.setProperty("overflow","hidden");
      };
      r = new pas.Types.TRect(this.GetClientRect());
      this.AlignControls(AControl,{get: function () {
          return r;
        }, set: function (v) {
          r = v;
        }});
      if (eh !== null) {
        eh.style.setProperty("overflow",ovf);
        eh.style.setProperty("overflow-x",ovfx);
        eh.style.setProperty("overflow-y",ovfy);
      };
      this.FIsAligning = false;
    };
    this.InitAnchoring = function () {
      var i = 0;
      this.FOrigRect = new pas.Types.TRect(this.GetBoundsRect());
      if (this.FParent !== null) this.FOrigParentRect = new pas.Types.TRect(this.FParent.GetBoundsRect());
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).InitAnchoring();
      };
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
      if ($tmp3 === $mod.TAlign.alClient) if (this.FParent != null) this.SetBounds(0,0,this.FParent.FWidth,this.FParent.FHeight);
      this.Resize();
      this.UpdateElement();
    };
    this.InternalResize = function () {
      if (pas.Classes.TComponentStateItem.csLoading in this.FComponentState) return;
      this.Resize();
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
      if (((this.FOwner !== null) && (pas.Classes.TComponentStateItem.csLoading in this.FOwner.FComponentState)) || !(this.FOwner !== null)) this.Loading();
      this.FControlPosition = $mod.TControlPosition.cpAbsolute;
      this.FWidthStyle = $mod.TSizeStyle.ssAbsolute;
      this.FHeightStyle = $mod.TSizeStyle.ssAbsolute;
      el = document.getElementById(AID);
      if (!(el !== null)) {
        this.FContainer = null;
        this.FNew = true;
      } else {
        this.FContainer = el;
        this.FNew = false;
        this.FControlCreated = true;
        this.FControlPosition = $mod.TControlPosition.cpRelative;
        this.BindElement();
        this.BindEvents();
      };
      this.FID = AID;
      this.FFont = pas["WEBLib.Graphics"].TFont.$create("Create$1");
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
      this.FColor = 0xFFFFFF;
      this.FMargins = $mod.TMargins.$create("Create$1");
      this.FMargins.FOnChange = rtl.createCallback(this,"DoMarginsChanged");
      this.FParent = null;
      this.FPrevParent = null;
      this.ClearControls();
      this.CreateInitialize();
    };
    this.AddControlLink = function (linkid, link) {
      var Self = this;
      function writeLinkOnce(linkName, linkText) {
        var Result = "";
        var linkElement = null;
        linkElement = document.getElementById(linkName);
        if (linkElement != null) return Result;
        linkElement = document.createElement("link");
        linkElement.id = linkName;
        linkElement.setAttribute("rel","stylesheet");
        linkElement.setAttribute("type","text\/css");
        linkElement.setAttribute("href",linkText);
        document.getElementsByTagName("head").item(0).appendChild(linkElement);
        return Result;
      };
      writeLinkOnce(linkid,link);
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
      if ((this.FParent !== null) && !pas["WEBLib.Forms"].TForm.isPrototypeOf(this)) {
        r = new pas.Types.TRect(this.FParent.FOrigRect);
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
    this.Assign = function (Source) {
    };
    this.DragDrop = function (Source, X, Y) {
      if (this.FOnDragDrop != null) this.FOnDragDrop(this,Source,X,Y);
    };
    this.Resize = function () {
    };
    this.PreventDefault = function () {
      if (this.GetElementEvent() !== null) this.GetElementEvent().preventDefault();
    };
    this.StopPropagation = function () {
      if (this.GetElementEvent() !== null) this.GetElementEvent().stopPropagation();
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
      Result = this.FCaptured && (this.FLayer !== null);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Align",2,$mod.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Parent",2,$r,"FParent","SetParent");
    $r.addProperty("Font",2,pas["WEBLib.Graphics"].$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("ParentFont",0,rtl.boolean,"FParentFont","FParentFont");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
  });
  rtl.createClass($mod,"TWinControl",$mod.TControl,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,$mod.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,$mod.$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("Cursor",2,rtl.longint,"FCursor","SetCursor");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("HeightStyle",2,$mod.$rtti["TSizeStyle"],"FHeightStyle","SetHeightStyle");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Margins",2,$mod.$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.longint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Tag",0,rtl.longint,"FTag$1","FTag$1");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("WidthStyle",2,$mod.$rtti["TSizeStyle"],"FWidthStyle","SetWidthStyle");
    $r.addProperty("WidthPercent",2,rtl.longint,"FWidthPercent","SetWidthPercent");
    $r.addProperty("OnClick",0,$mod.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,$mod.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,$mod.$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,$mod.$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,$mod.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseEnter",0,$mod.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,$mod.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnKeyUp",0,$mod.$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnKeyDown",0,$mod.$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
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
    };
    this.$final = function () {
      this.FCanvas = undefined;
      this.FElementCanvas = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.GetPixelRatio = function () {
      var Result = 0.0;
      var ctx = document.createElement("canvas").getContext("2d"),
                dpr = window.devicePixelRatio || 1,
                bsr = ctx.webkitBackingStorePixelRatio ||
                      ctx.mozBackingStorePixelRatio ||
                      ctx.msBackingStorePixelRatio ||
                      ctx.oBackingStorePixelRatio ||
                      ctx.backingStorePixelRatio || 1;
      
            Result = dpr / bsr;
      return Result;
    };
    this.RecreateCanvas = function () {
      var px = 0.0;
      var el = null;
      if (this.FElementCanvas != null) {
        if (!this.FPainting) {
          el = document.getElementById(this.GetID() + "_Canvas");
          if (el != null) {
            this.FElementCanvas = el;
            rtl.free(this,"FCanvas");
            this.FCanvas = null;
          };
          px = this.GetPixelRatio();
          this.FElementCanvas.style.setProperty("height",pas.SysUtils.IntToStr(this.GetHeight() - this.GetCanvasHeightOffset()) + "px");
          this.FElementCanvas.style.setProperty("width",pas.SysUtils.IntToStr(this.GetWidth() - this.GetCanvasWidthOffset()) + "px");
          this.FElementCanvas.width = Math.round(this.FElementCanvas.offsetWidth * px);
          this.FElementCanvas.height = Math.round(this.FElementCanvas.offsetHeight * px);
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
    this.CreateControl = function () {
      $mod.TControl.CreateControl.apply(this,arguments);
      this.RecreateCanvas();
    };
    this.Loaded = function () {
      $mod.TControl.Loaded.apply(this,arguments);
      this.Invalidate();
    };
    this.UpdateElement = function () {
      $mod.TControl.UpdateElement.apply(this,arguments);
      if (this.GetElementHandle() != null) {
        if (this.FBorderStyle === $mod.TBorderStyle.bsSingle) {
          this.GetElementHandle().style.setProperty("border-style","")}
         else this.GetElementHandle().style.setProperty("border-style","none");
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
      return Result;
    };
    this.GetCanvasWidthOffset = function () {
      var Result = 0;
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
      this.FPainting = false;
      this.FWidth = 100;
      this.FHeight = 25;
      this.FWidthPercent = 100;
      this.FHeightPercent = 100;
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
    this.GetCanvas = function () {
      var Result = null;
      this.CreateControl();
      Result = this.FCanvas;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Canvas",1,pas["WEBLib.Graphics"].$rtti["TCanvas"],"GetCanvas","");
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
  rtl.createClass($mod,"TCSSCodeFragments",pas.Classes.TOwnedCollection,function () {
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
    this.SetCSSFragments = function (AValue) {
      this.FCSSFragments.Assign(AValue);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.apply(this,arguments);
      this.FCSSFragments = $mod.TCSSCodeFragments.$create("Create");
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
rtl.module("WEBLib.Forms",["System","Classes","SysUtils","JS","Types","Web","WEBLib.Graphics","WEBLib.Controls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TCloseAction = {"0": "caNone", caNone: 0, "1": "caHide", caHide: 1, "2": "caFree", caFree: 2, "3": "caMinimize", caMinimize: 3};
  $mod.$rtti.$Enum("TCloseAction",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TCloseAction});
  this.TFormStyle = {"0": "fsNormal", fsNormal: 0, "1": "fsStayOnTop", fsStayOnTop: 1};
  $mod.$rtti.$Enum("TFormStyle",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TFormStyle});
  this.TModalResult = {"0": "mrNone", mrNone: 0, "1": "mrOk", mrOk: 1, "2": "mrCancel", mrCancel: 2, "3": "mrAbort", mrAbort: 3, "4": "mrRetry", mrRetry: 4, "5": "mrIgnore", mrIgnore: 5, "6": "mrYes", mrYes: 6, "7": "mrNo", mrNo: 7, "8": "mrAll", mrAll: 8, "9": "mrNoToAll", mrNoToAll: 9, "10": "mrYesToAll", mrYesToAll: 10, "11": "mrClose", mrClose: 11};
  $mod.$rtti.$MethodVar("TCloseQueryEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Hnd",rtl.boolean,1]],rtl.boolean), methodkind: 1});
  $mod.$rtti.$MethodVar("TCloseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Hnd",$mod.$rtti["TCloseAction"],1]]), methodkind: 0});
  rtl.createClass($mod,"TCustomForm",pas["WEBLib.Controls"].TWinControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TWinControl.$init.call(this);
      this.FLayer$1 = null;
      this.FPopup = false;
      this.FFormFileName = "";
      this.FFormContent = "";
      this.FFormStyle = 0;
      this.FOnCreate = null;
      this.FOnResize = null;
      this.FOnShow$1 = null;
      this.FModalResult = 0;
      this.FOnPaint = null;
      this.FOnDeactivate = null;
      this.FOnCloseQuery = null;
      this.FOnClose = null;
      this.FModalProc = null;
      this.FCaption = "";
      this.FIsResizing$1 = false;
      this.FOnScroll$1 = null;
      this.FOnUnload = null;
      this.FFormContainer = "";
    };
    this.$final = function () {
      this.FLayer$1 = undefined;
      this.FOnCreate = undefined;
      this.FOnResize = undefined;
      this.FOnShow$1 = undefined;
      this.FOnPaint = undefined;
      this.FOnDeactivate = undefined;
      this.FOnCloseQuery = undefined;
      this.FOnClose = undefined;
      this.FModalProc = undefined;
      this.FOnScroll$1 = undefined;
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
      this.BeginUpdate();
      this.Loaded();
      this.EndUpdate();
      this.Resize$1();
      this.InitAnchoring();
      Result = true;
      return Result;
    };
    this.DoScroll = function (Event) {
      var Result = false;
      if (this.FOnScroll$1 != null) this.FOnScroll$1(this);
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
      var i = 0;
      pas["WEBLib.Controls"].TControl.Loaded.apply(this,arguments);
      for (var $l1 = 0, $end2 = this.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetControls(i).Loaded();
      };
      if (this.FOnShow$1 != null) this.FOnShow$1(this);
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
      var s = "";
      var css = null;
      if (this.GetElementHandle() === document.body) {
        css = window.getComputedStyle(this.GetElementHandle());
        s = css.getPropertyValue("margin-left");
        s = pas.System.Copy(s,1,s.length - 2);
        d = pas.SysUtils.StrToInt(s) - 2;
        Result = window.innerWidth - d;
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
      } else Result = this.FormContainerElement();
      return Result;
    };
    this.UpdateElement = function () {
      var clr = "";
      var i = 0;
      pas["WEBLib.Controls"].TControl.UpdateElement.apply(this,arguments);
      if (this.IsUpdating()) return;
      if (this.GetElementHandle() != null) {
        if (this.FCaption !== "") window.document.title = this.FCaption;
        clr = "#" + pas["WEBLib.Graphics"].ColorToHex(this.FColor);
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
      } else Result = document.body;
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
          this.SetVisible(false);
          if (this.FFormFileName === "") {
            this.FormContainerElement().removeChild(this.FLayer$1);
            this.FormContainerElement().removeChild(this.FContainer);
          };
          $mod.Application.PopForm();
          this.DoClose({get: function () {
              return lAction;
            }, set: function (v) {
              lAction = v;
            }});
        };
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TControl.CreateInitialize.apply(this,arguments);
      this.FModalResult = $mod.TModalResult.mrNone;
      this.FFormStyle = $mod.TFormStyle.fsNormal;
      this.SetColor(0xFFFFFF);
      window.addEventListener("load",rtl.createCallback(this,"DoLoaded"));
    };
    this.Create$2 = function (ID) {
      pas["WEBLib.Controls"].TControl.Create$2.call(this,ID);
      this.FModalResult = $mod.TModalResult.mrNone;
      this.FFormStyle = $mod.TFormStyle.fsNormal;
      this.DoCreate();
    };
    this.Create$1 = function (AOwner) {
      pas["WEBLib.Controls"].TControl.Create$1.apply(this,arguments);
      this.FFormFileName = "";
      this.FPopup = true;
    };
    this.Destroy = function () {
      pas["WEBLib.Controls"].TControl.Destroy.apply(this,arguments);
      this.FormContainerElement().removeChild(this.FLayer$1);
      this.FLayer$1 = null;
    };
    var $r = this.$rtti;
    $r.addProperty("Popup",0,rtl.boolean,"FPopup","FPopup");
  });
  rtl.createClass($mod,"TForm",$mod.TCustomForm,function () {
    var $r = this.$rtti;
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnResize",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnResize","FOnResize");
    $r.addProperty("OnShow",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnShow$1","FOnShow$1");
    $r.addProperty("OnScroll",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnScroll$1","FOnScroll$1");
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
        if (Self.FMainForm !== null) Self.PushForm(Self.FMainForm);
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
      var Self = this;
      var query = "";
      var token = "";
      Self.FParameters.Clear();
      query = window.location.href;
      Self.FParameters.SetDelimiter("&");
      Self.FParameters.FStrictDelimiter = true;
      Self.FParameters.SetDelimitedText(query);
      if (Self.FParameters.IndexOfName("code") !== -1) {
        Self.FIsRedirect = true;
        token = Self.FParameters.GetValue("code");
        window.opener.processAuthData(token);
        window.close();
      };
      if (Self.FParameters.IndexOfName("access_token") !== -1) {
        Self.FIsRedirect = true;
        token = Self.FParameters.GetValue("access_token");
        window.opener.processAuthData(token);
        window.close();
      };
    };
    this.Run = function () {
    };
    var $r = this.$rtti;
    $r.addProperty("MainForm",0,$mod.$rtti["TForm"],"FMainForm","");
  });
  this.Application = null;
  this.GetParentForm = function (AControl) {
    var Result = null;
    var FOwner = null;
    Result = null;
    FOwner = AControl;
    while ((FOwner !== null) && !$mod.TCustomForm.isPrototypeOf(FOwner)) {
      FOwner = FOwner.FOwner;
    };
    if ((FOwner != null) && $mod.TCustomForm.isPrototypeOf(FOwner)) Result = rtl.as(FOwner,$mod.TCustomForm);
    return Result;
  };
  $mod.$init = function () {
    $mod.Application = $mod.TApplication.$create("Create$1",[null]);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.cBodyTag = "body";
  $impl.cHTMLExt = ".html";
});
rtl.module("WEBLib.Runner",["System","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.TTMSBrowserEnum = {"0": "tbnNull", tbnNull: 0, "1": "tbnDefault", tbnDefault: 1, "2": "tbnChrome", tbnChrome: 2, "3": "tbnFirefox", tbnFirefox: 3, "4": "tbnEdge", tbnEdge: 4, "5": "tbnIExplore", tbnIExplore: 5, "6": "tbnOpera", tbnOpera: 6};
  rtl.createClass($mod,"TTMSWebRunner",pas.System.TObject,function () {
    this.Execute$1 = function (ABrowser) {
    };
  });
});
rtl.module("DateUtils",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.DateTimeToRFC3339 = function (ADate) {
    var Result = "";
    Result = pas.SysUtils.FormatDateTime('yyyy-mm-dd"T"hh":"nn":"ss"."zzz"Z"',ADate);
    return Result;
  };
  var P = [11,1,6,9,12,15,18];
  this.TryRFC3339ToDateTime = function (Avalue, ADateTime) {
    var Result = false;
    this.TPartPos = {"0": "ppTime", ppTime: 0, "1": "ppYear", ppYear: 1, "2": "ppMonth", ppMonth: 2, "3": "ppDay", ppDay: 3, "4": "ppHour", ppHour: 4, "5": "ppMinute", ppMinute: 5, "6": "ppSec", ppSec: 6};
    var lY = 0;
    var lM = 0;
    var lD = 0;
    var lH = 0;
    var lMi = 0;
    var lS = 0;
    if (pas.SysUtils.Trim(Avalue) === "") {
      Result = true;
      ADateTime.set(0);
    };
    lY = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppYear],4),-1);
    lM = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppMonth],2),-1);
    lD = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppDay],2),-1);
    if (Avalue.length >= P[$mod.TPartPos.ppTime]) {
      lH = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppHour],2),-1);
      lMi = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppMinute],2),-1);
      lS = pas.SysUtils.StrToIntDef(pas.System.Copy(Avalue,P[$mod.TPartPos.ppSec],2),-1);
    } else {
      lH = 0;
      lMi = 0;
      lS = 0;
    };
    Result = (((((lY >= 0) && (lM >= 0)) && (lD >= 0)) && (lH >= 0)) && (lMi >= 0)) && (lS >= 0);
    if (!Result) {
      ADateTime.set(0)}
     else if (((lY === 0) || (lM === 0)) || (lD === 0)) {
      ADateTime.set(pas.SysUtils.EncodeTime(lH,lMi,lS,0))}
     else ADateTime.set(pas.SysUtils.EncodeDate(lY,lM,lD) + pas.SysUtils.EncodeTime(lH,lMi,lS,0));
    return Result;
  };
},["JS","RTLConsts"]);
rtl.module("DBConst",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SActiveDataset: {org: "Operation cannot be performed on an active dataset"}, SDatasetReadOnly: {org: "Dataset is read-only."}, SDuplicateFieldName: {org: 'Duplicate fieldname : "%s"'}, SFieldNotFound: {org: 'Field not found : "%s"'}, SInactiveDataset: {org: "Operation cannot be performed on an inactive dataset"}, SInvalidDisplayValues: {org: '"%s" are not valid boolean displayvalues'}, SInvalidFieldSize: {org: "Invalid field size : %d"}, SInvalidTypeConversion: {org: "Invalid type conversion to %s in field %s"}, SNeedField: {org: "Field %s is required, but not supplied."}, SNeedFieldName: {org: "Field needs a name"}, SNoDataset: {org: 'No dataset asssigned for field : "%s"'}, SNoSuchRecord: {org: "Could not find the requested record."}, SNotABoolean: {org: '"%s" is not a valid boolean'}, SNotAFloat: {org: '"%s" is not a valid float'}, SNotAninteger: {org: '"%s" is not a valid integer'}, SNotEditing: {org: 'Operation not allowed, dataset "%s" is not in an edit or insert state.'}, SRangeError: {org: "%f is not between %f and %f for %s"}, SUniDirectional: {org: "Operation cannot be performed on an unidirectional dataset"}, SUnknownFieldType: {org: "Unknown field type : %s"}, SFieldValueError: {org: "Invalid value for field '%s'"}, SDuplicateName: {org: "Duplicate name '%s' in %s"}, SLookupInfoError: {org: "Lookup information for field '%s' is incomplete"}, SDatasetEmpty: {org: "The dataset is empty"}, SErrInvalidDateTime: {org: 'Invalid date\/time value : "%s"'}, SErrInsertingSameRecordtwice: {org: "Attempt to insert the same record twice."}};
});
rtl.module("DB",["System","Classes","SysUtils","JS","Types","DateUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TDataSetState = {"0": "dsInactive", dsInactive: 0, "1": "dsBrowse", dsBrowse: 1, "2": "dsEdit", dsEdit: 2, "3": "dsInsert", dsInsert: 3, "4": "dsSetKey", dsSetKey: 4, "5": "dsCalcFields", dsCalcFields: 5, "6": "dsFilter", dsFilter: 6, "7": "dsNewValue", dsNewValue: 7, "8": "dsOldValue", dsOldValue: 8, "9": "dsCurValue", dsCurValue: 9, "10": "dsBlockRead", dsBlockRead: 10, "11": "dsInternalCalc", dsInternalCalc: 11, "12": "dsOpening", dsOpening: 12, "13": "dsRefreshFields", dsRefreshFields: 13};
  this.TDataEvent = {"0": "deFieldChange", deFieldChange: 0, "1": "deRecordChange", deRecordChange: 1, "2": "deDataSetChange", deDataSetChange: 2, "3": "deDataSetScroll", deDataSetScroll: 3, "4": "deLayoutChange", deLayoutChange: 4, "5": "deUpdateRecord", deUpdateRecord: 5, "6": "deUpdateState", deUpdateState: 6, "7": "deCheckBrowseMode", deCheckBrowseMode: 7, "8": "dePropertyChange", dePropertyChange: 8, "9": "deFieldListChange", deFieldListChange: 9, "10": "deFocusControl", deFocusControl: 10, "11": "deParentScroll", deParentScroll: 11, "12": "deConnectChange", deConnectChange: 12, "13": "deReconcileError", deReconcileError: 13, "14": "deDisabledStateChange", deDisabledStateChange: 14};
  this.TUpdateStatus = {"0": "usUnmodified", usUnmodified: 0, "1": "usModified", usModified: 1, "2": "usInserted", usInserted: 2, "3": "usDeleted", usDeleted: 3, "4": "usResolved", usResolved: 4, "5": "usResolveFailed", usResolveFailed: 5};
  this.TProviderFlag = {"0": "pfInUpdate", pfInUpdate: 0, "1": "pfInWhere", pfInWhere: 1, "2": "pfInKey", pfInKey: 2, "3": "pfHidden", pfHidden: 3, "4": "pfRefreshOnInsert", pfRefreshOnInsert: 4, "5": "pfRefreshOnUpdate", pfRefreshOnUpdate: 5};
  $mod.$rtti.$Enum("TProviderFlag",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TProviderFlag});
  $mod.$rtti.$Set("TProviderFlags",{comptype: $mod.$rtti["TProviderFlag"]});
  $mod.$rtti.$Class("TField");
  $mod.$rtti.$Class("TDataSet");
  $mod.$rtti.$Class("TDataSource");
  rtl.createClass($mod,"EDatabaseError",pas.SysUtils.Exception,function () {
  });
  this.TFieldType = {"0": "ftUnknown", ftUnknown: 0, "1": "ftString", ftString: 1, "2": "ftInteger", ftInteger: 2, "3": "ftLargeInt", ftLargeInt: 3, "4": "ftBoolean", ftBoolean: 4, "5": "ftFloat", ftFloat: 5, "6": "ftDate", ftDate: 6, "7": "ftTime", ftTime: 7, "8": "ftDateTime", ftDateTime: 8, "9": "ftAutoInc", ftAutoInc: 9, "10": "ftBlob", ftBlob: 10, "11": "ftMemo", ftMemo: 11, "12": "ftFixedChar", ftFixedChar: 12, "13": "ftVariant", ftVariant: 13};
  $mod.$rtti.$Enum("TFieldType",{minvalue: 0, maxvalue: 13, ordtype: 1, enumtype: this.TFieldType});
  this.TFieldAttribute = {"0": "faHiddenCol", faHiddenCol: 0, "1": "faReadonly", faReadonly: 1, "2": "faRequired", faRequired: 2, "3": "faLink", faLink: 3, "4": "faUnNamed", faUnNamed: 4, "5": "faFixed", faFixed: 5};
  $mod.$rtti.$Enum("TFieldAttribute",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TFieldAttribute});
  $mod.$rtti.$Set("TFieldAttributes",{comptype: $mod.$rtti["TFieldAttribute"]});
  rtl.createClass($mod,"TNamedItem",pas.Classes.TCollectionItem,function () {
    this.$init = function () {
      pas.Classes.TCollectionItem.$init.call(this);
      this.FName = "";
    };
    this.GetDisplayName = function () {
      var Result = "";
      Result = this.FName;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      var TmpInd = 0;
      if (this.FName === Value) return;
      if ((Value !== "") && $mod.TFieldDefs.isPrototypeOf(this.FCollection)) {
        TmpInd = this.FCollection.IndexOf(Value);
        if ((TmpInd >= 0) && (TmpInd !== this.GetIndex())) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SDuplicateName"),[Value,this.FCollection.$classname]);
      };
      this.FName = Value;
      pas.Classes.TCollectionItem.SetDisplayName.call(this,Value);
    };
    var $r = this.$rtti;
    $r.addProperty("Name",2,rtl.string,"FName","SetDisplayName");
  });
  rtl.createClass($mod,"TDefCollection",pas.Classes.TOwnedCollection,function () {
    this.$init = function () {
      pas.Classes.TOwnedCollection.$init.call(this);
      this.FDataset = null;
    };
    this.$final = function () {
      this.FDataset = undefined;
      pas.Classes.TOwnedCollection.$final.call(this);
    };
    this.SetItemName = function (Item) {
      var N = null;
      var TN = "";
      N = rtl.as(Item,$mod.TNamedItem);
      if (N.FName === "") {
        TN = pas.System.Copy(this.$classname,2,5) + pas.SysUtils.IntToStr(N.FID + 1);
        if (this.FDataset != null) TN = this.FDataset.FName + TN;
        N.SetDisplayName(TN);
      } else pas.Classes.TCollection.SetItemName.call(this,Item);
    };
    this.create$3 = function (ADataset, AOwner, AClass) {
      pas.Classes.TOwnedCollection.Create$2.call(this,AOwner,AClass);
      this.FDataset = ADataset;
    };
    this.IndexOf = function (AName) {
      var Result = 0;
      var i = 0;
      Result = -1;
      for (var $l1 = 0, $end2 = this.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (pas.SysUtils.AnsiSameText(this.GetItem(i).FName,AName)) {
          Result = i;
          break;
        };
      };
      return Result;
    };
  });
  rtl.createClass($mod,"TFieldDef",$mod.TNamedItem,function () {
    this.$init = function () {
      $mod.TNamedItem.$init.call(this);
      this.FAttributes = {};
      this.FDataType = 0;
      this.FFieldNo = 0;
      this.FInternalCalcField = false;
      this.FPrecision = 0;
      this.FRequired = false;
      this.FSize = 0;
    };
    this.$final = function () {
      this.FAttributes = undefined;
      $mod.TNamedItem.$final.call(this);
    };
    this.GetFieldClass = function () {
      var Result = null;
      if (((this.FCollection != null) && $mod.TFieldDefs.isPrototypeOf(this.FCollection)) && (this.FCollection.FDataset != null)) {
        Result = this.FCollection.FDataset.GetFieldClass(this.FDataType)}
       else Result = null;
      return Result;
    };
    this.SetAttributes = function (AValue) {
      this.FAttributes = rtl.refSet(AValue);
      this.Changed(false);
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
      this.Changed(false);
    };
    this.SetPrecision = function (AValue) {
      this.FPrecision = AValue;
      this.Changed(false);
    };
    this.SetSize = function (AValue) {
      this.FSize = AValue;
      this.Changed(false);
    };
    this.Create$1 = function (ACollection) {
      pas.Classes.TCollectionItem.Create$1.call(this,ACollection);
      this.FFieldNo = this.GetIndex() + 1;
    };
    this.Create$3 = function (AOwner, AName, ADataType, ASize, ARequired, AFieldNo) {
      pas.Classes.TCollectionItem.Create$1.call(this,AOwner);
      this.SetDisplayName(AName);
      this.FDataType = ADataType;
      this.FSize = ASize;
      this.FRequired = ARequired;
      this.FPrecision = -1;
      this.FFieldNo = AFieldNo;
    };
    this.Destroy = function () {
      pas.Classes.TCollectionItem.Destroy.call(this);
    };
    this.Assign = function (Source) {
      var fd = null;
      fd = null;
      if ($mod.TFieldDef.isPrototypeOf(Source)) fd = rtl.as(Source,$mod.TFieldDef);
      if (fd != null) {
        this.FCollection.BeginUpdate();
        try {
          this.SetDisplayName(fd.FName);
          this.SetDataType(fd.FDataType);
          this.SetSize(fd.FSize);
          this.SetPrecision(fd.FPrecision);
          this.FRequired = fd.FRequired;
        } finally {
          this.FCollection.EndUpdate();
        };
      } else pas.Classes.TPersistent.Assign.call(this,Source);
    };
    this.CreateField = function (AOwner) {
      var Result = null;
      var TheField = null;
      TheField = this.GetFieldClass();
      if (TheField === null) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SUnknownFieldType"),[this.FName]);
      Result = TheField.$create("Create$1",[AOwner]);
      try {
        Result.FFieldDef = this;
        Result.SetSize(this.FSize);
        Result.FRequired = this.FRequired;
        Result.FFieldName = this.FName;
        Result.FDisplayLabel = this.GetDisplayName();
        Result.FFieldNo = this.FFieldNo;
        Result.SetFieldType(this.FDataType);
        Result.FReadOnly = $mod.TFieldAttribute.faReadonly in this.FAttributes;
        Result.SetDataset(this.FCollection.FDataset);
        if ($mod.TFloatField.isPrototypeOf(Result)) Result.SetPrecision(this.FPrecision);
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Attributes",2,$mod.$rtti["TFieldAttributes"],"FAttributes","SetAttributes",{Default: {}});
    $r.addProperty("DataType",2,$mod.$rtti["TFieldType"],"FDataType","SetDataType");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 0});
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass($mod,"TFieldDefs",$mod.TDefCollection,function () {
    this.GetItem$1 = function (Index) {
      var Result = null;
      Result = this.GetItem(Index);
      return Result;
    };
    this.FieldDefClass = function () {
      var Result = null;
      Result = $mod.TFieldDef;
      return Result;
    };
    this.Create$4 = function (ADataSet) {
      $mod.TDefCollection.create$3.call(this,ADataSet,this.Owner(),this.$class.FieldDefClass());
    };
    this.Add$2 = function (AName, ADataType, ASize, ARequired, AFieldNo) {
      var Result = null;
      Result = this.$class.FieldDefClass().$create("Create$3",[this,AName,ADataType,ASize,ARequired,AFieldNo]);
      return Result;
    };
    this.Add$3 = function (AName, ADataType, ASize, ARequired) {
      if (AName.length === 0) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SNeedFieldName"),this.FDataset);
      this.BeginUpdate();
      try {
        this.Add$2(AName,ADataType,ASize,ARequired,this.GetCount() + 1);
      } finally {
        this.EndUpdate();
      };
    };
    this.Add$4 = function (AName, ADataType, ASize) {
      this.Add$3(AName,ADataType,ASize,false);
    };
  });
  this.TFieldKind = {"0": "fkData", fkData: 0, "1": "fkCalculated", fkCalculated: 1, "2": "fkLookup", fkLookup: 2, "3": "fkInternalCalc", fkInternalCalc: 3};
  $mod.$rtti.$Enum("TFieldKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFieldKind});
  $mod.$rtti.$MethodVar("TFieldNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TFieldGetTextEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]],["aText",rtl.string,1],["DisplayText",rtl.boolean]]), methodkind: 0});
  $mod.$rtti.$MethodVar("TFieldSetTextEvent",{procsig: rtl.newTIProcSig([["Sender",$mod.$rtti["TField"]],["aText",rtl.string,2]]), methodkind: 0});
  rtl.createClass($mod,"TLookupList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function () {
      this.FList = pas.Classes.TFPList.$create("Create");
    };
    this.Destroy = function () {
      this.Clear();
      this.FList.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      this.FList.Clear();
    };
  });
  rtl.createClass($mod,"TField",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAlignment = 0;
      this.FConstraintErrorMessage = "";
      this.FCustomConstraint = "";
      this.FDataSet = null;
      this.FDataType = 0;
      this.FDefaultExpression = "";
      this.FDisplayLabel = "";
      this.FDisplayWidth = 0;
      this.FFieldDef = null;
      this.FFieldKind = 0;
      this.FFieldName = "";
      this.FFieldNo = 0;
      this.FFields = null;
      this.FHasConstraints = false;
      this.FImportedConstraint = "";
      this.FKeyFields = "";
      this.FLookupCache = false;
      this.FLookupDataSet = null;
      this.FLookupKeyfields = "";
      this.FLookupresultField = "";
      this.FLookupList = null;
      this.FOffset = 0;
      this.FOnChange = null;
      this.FOnGetText = null;
      this.FOnSetText = null;
      this.FOnValidate = null;
      this.FOrigin = "";
      this.FReadOnly = false;
      this.FRequired = false;
      this.FSize = 0;
      this.FValidChars = [];
      this.FValueBuffer = undefined;
      this.FValidating = false;
      this.FVisible = false;
      this.FProviderFlags = {};
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FFieldDef = undefined;
      this.FFields = undefined;
      this.FLookupDataSet = undefined;
      this.FLookupList = undefined;
      this.FOnChange = undefined;
      this.FOnGetText = undefined;
      this.FOnSetText = undefined;
      this.FOnValidate = undefined;
      this.FValidChars = undefined;
      this.FProviderFlags = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FDataSet != null) {
        Result = this.FDataSet.FFieldList.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.PropertyChanged(false);
      };
    };
    this.SetIndex = function (AValue) {
      if (this.FFields !== null) this.FFields.SetFieldIndex(this,AValue);
    };
    this.GetDisplayText = function () {
      var Result = "";
      Result = rtl.strSetLength(Result,0);
      if (this.FOnGetText != null) {
        this.FOnGetText(this,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},true)}
       else this.GetText({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},true);
      return Result;
    };
    this.SetDisplayLabel = function (AValue) {
      if (this.FDisplayLabel !== AValue) {
        this.FDisplayLabel = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetDisplayWidth = function (AValue) {
      if (this.FDisplayWidth !== AValue) {
        this.FDisplayWidth = AValue;
        this.PropertyChanged(true);
      };
    };
    this.GetDisplayWidth = function () {
      var Result = 0;
      if (this.FDisplayWidth === 0) {
        Result = this.GetDefaultWidth()}
       else Result = this.FDisplayWidth;
      return Result;
    };
    this.SetReadOnly = function (AValue) {
      if (this.FReadOnly !== AValue) {
        this.FReadOnly = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.PropertyChanged(true);
      };
    };
    this.IsDisplayLabelStored = function () {
      var Result = false;
      Result = this.GetDisplayName() !== this.FFieldName;
      return Result;
    };
    this.IsDisplayWidthStored = function () {
      var Result = false;
      Result = this.FDisplayWidth !== 0;
      return Result;
    };
    this.GetLookupList = function () {
      var Result = null;
      if (!(this.FLookupList != null)) this.FLookupList = $mod.TLookupList.$create("Create$1");
      Result = this.FLookupList;
      return Result;
    };
    this.CalcLookupValue = function () {
    };
    this.RaiseAccessError = function (TypeName) {
      var E = null;
      E = this.AccessError(TypeName);
      throw E;
    };
    this.AccessError = function (TypeName) {
      var Result = null;
      throw $mod.EDatabaseError.$create("CreateFmt",[rtl.getResStr(pas.DBConst,"SInvalidTypeConversion"),[TypeName,this.FFieldName]]);
      return Result;
    };
    this.CheckInactive = function () {
      if (this.FDataSet != null) this.FDataSet.CheckInactive();
    };
    this.CheckTypeSize = function (AValue) {
      if ((AValue !== 0) && !this.IsBlob()) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.Change = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Bind = function (Binding) {
      if (Binding && (this.FFieldKind === $mod.TFieldKind.fkLookup)) {
        if ((((this.FLookupDataSet === null) || (this.FLookupKeyfields === "")) || (this.FLookupresultField === "")) || (this.FKeyFields === "")) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SLookupInfoError"),[this.GetDisplayName()]);
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.Open();
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        if (this.FLookupCache) this.RefreshLookupList();
      };
    };
    this.GetAsBytes = function () {
      var Result = [];
      this.RaiseAccessError($impl.SBytes);
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      this.RaiseAccessError($impl.SInteger);
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      Result = this.GetClassDesc();
      return Result;
    };
    this.GetClassDesc = function () {
      var Result = "";
      var ClassN = "";
      ClassN = pas.System.Copy(this.$classname,2,pas.System.Pos("Field",this.$classname) - 2);
      if (this.GetIsNull()) {
        Result = ("(" + pas.SysUtils.LowerCase(ClassN)) + ")"}
       else Result = ("(" + pas.SysUtils.UpperCase(ClassN)) + ")";
      return Result;
    };
    this.GetDataSize = function () {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = 10;
      return Result;
    };
    this.GetDisplayName = function () {
      var Result = "";
      if (this.FDisplayLabel !== "") {
        Result = this.FDisplayLabel}
       else Result = this.FFieldName;
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      Result = pas.JS.isNull(this.GetData());
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set(this.GetAsString());
    };
    this.Notification = function (AComponent, Operation) {
      pas.Classes.TComponent.Notification.call(this,AComponent,Operation);
      if ((Operation === pas.Classes.TOperation.opRemove) && (AComponent === this.FLookupDataSet)) this.FLookupDataSet = null;
    };
    this.PropertyChanged = function (LayoutAffected) {
      if ((this.FDataSet !== null) && this.FDataSet.GetActive()) if (LayoutAffected) {
        this.FDataSet.DataEvent($mod.TDataEvent.deLayoutChange,0)}
       else this.FDataSet.DataEvent($mod.TDataEvent.deDataSetChange,0);
    };
    this.SetAsInteger = function (AValue) {
      this.RaiseAccessError($impl.SInteger);
    };
    this.SetAsJSValue = function (AValue) {
      if (pas.JS.isNull(AValue)) {
        this.Clear()}
       else try {
        this.SetVarValue(AValue);
      } catch ($e) {
        if (pas.SysUtils.EVariantError.isPrototypeOf($e)) {
          $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SFieldValueError"),[this.GetDisplayName()])}
         else throw $e
      };
    };
    this.SetAsString = function (AValue) {
      this.RaiseAccessError($impl.SString);
    };
    this.SetDataset = function (AValue) {
      if (AValue === this.FDataSet) return;
      if (this.FDataSet != null) {
        this.FDataSet.CheckInactive();
        this.FDataSet.FFieldList.Remove(this);
      };
      if (AValue != null) {
        AValue.CheckInactive();
        AValue.FFieldList.Add(this);
      };
      this.FDataSet = AValue;
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
    };
    this.SetSize = function (AValue) {
      this.CheckInactive();
      this.$class.CheckTypeSize(AValue);
      this.FSize = AValue;
    };
    this.SetVarValue = function (AValue) {
      this.RaiseAccessError($impl.SJSValue);
    };
    this.SetAsBytes = function (AValue) {
      this.RaiseAccessError($impl.SBytes);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FVisible = true;
      this.FValidChars = rtl.arraySetLength(this.FValidChars,"",255);
      this.FProviderFlags = rtl.createSet($mod.TProviderFlag.pfInUpdate,$mod.TProviderFlag.pfInWhere);
    };
    this.Destroy = function () {
      if (this.FDataSet != null) {
        this.FDataSet.SetActive(false);
        if (this.FFields != null) this.FFields.Remove(this);
      };
      rtl.free(this,"FLookupList");
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.Assign = function (Source) {
      if (Source === null) {
        this.Clear()}
       else if ($mod.TField.isPrototypeOf(Source)) {
        this.SetAsJSValue(Source.GetAsJSValue());
      } else pas.Classes.TPersistent.Assign.call(this,Source);
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.GetData = function () {
      var Result = undefined;
      if (this.FDataSet === null) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNoDataset"),[this.FFieldName]);
      if (this.FValidating) {
        Result = this.FValueBuffer}
       else Result = this.FDataSet.GetFieldData(this);
      return Result;
    };
    this.IsBlob = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.IsValidChar = function (InputChar) {
      var Result = false;
      Result = pas.SysUtils.CharInSet(InputChar,this.FValidChars);
      return Result;
    };
    this.RefreshLookupList = function () {
      var tmpActive = false;
      if (((!(this.FLookupDataSet != null) || (this.FLookupKeyfields.length === 0)) || (this.FLookupresultField.length === 0)) || (this.FKeyFields.length === 0)) return;
      tmpActive = this.FLookupDataSet.GetActive();
      try {
        this.FLookupDataSet.SetActive(true);
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        this.GetLookupList().Clear();
        this.FLookupDataSet.DisableControls();
        try {
          this.FLookupDataSet.First();
          while (!this.FLookupDataSet.FEOF) {
            this.FLookupDataSet.Next();
          };
        } finally {
          this.FLookupDataSet.EnableControls();
        };
      } finally {
        this.FLookupDataSet.SetActive(tmpActive);
      };
    };
    this.SetData = function (Buffer) {
      if (!(this.FDataSet != null)) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNoDataset"),[this.FFieldName]);
      this.FDataSet.SetFieldData(this,Buffer);
    };
    this.SetFieldType = function (AValue) {
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taLeftJustify});
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ConstraintErrorMessage",0,rtl.string,"FConstraintErrorMessage","FConstraintErrorMessage");
    $r.addProperty("DefaultExpression",0,rtl.string,"FDefaultExpression","FDefaultExpression");
    $r.addProperty("DisplayLabel",15,rtl.string,"GetDisplayName","SetDisplayLabel",{stored: "IsDisplayLabelStored"});
    $r.addProperty("DisplayWidth",15,rtl.longint,"GetDisplayWidth","SetDisplayWidth",{stored: "IsDisplayWidthStored"});
    $r.addProperty("FieldKind",0,$mod.$rtti["TFieldKind"],"FFieldKind","FFieldKind");
    $r.addProperty("FieldName",0,rtl.string,"FFieldName","FFieldName");
    $r.addProperty("HasConstraints",0,rtl.boolean,"FHasConstraints","");
    $r.addProperty("Index",3,rtl.longint,"GetIndex","SetIndex");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
    $r.addProperty("KeyFields",0,rtl.string,"FKeyFields","FKeyFields");
    $r.addProperty("LookupCache",0,rtl.boolean,"FLookupCache","FLookupCache");
    $r.addProperty("LookupDataSet",0,$mod.$rtti["TDataSet"],"FLookupDataSet","FLookupDataSet");
    $r.addProperty("LookupKeyFields",0,rtl.string,"FLookupKeyfields","FLookupKeyfields");
    $r.addProperty("LookupResultField",0,rtl.string,"FLookupresultField","FLookupresultField");
    $r.addProperty("Origin",0,rtl.string,"FOrigin","FOrigin");
    $r.addProperty("ProviderFlags",0,$mod.$rtti["TProviderFlags"],"FProviderFlags","FProviderFlags");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("Required",0,rtl.boolean,"FRequired","FRequired");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible",{Default: true});
    $r.addProperty("OnChange",0,$mod.$rtti["TFieldNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnGetText",0,$mod.$rtti["TFieldGetTextEvent"],"FOnGetText","FOnGetText");
    $r.addProperty("OnSetText",0,$mod.$rtti["TFieldSetTextEvent"],"FOnSetText","FOnSetText");
    $r.addProperty("OnValidate",0,$mod.$rtti["TFieldNotifyEvent"],"FOnValidate","FOnValidate");
  });
  rtl.createClass($mod,"TStringField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FFixedChar = false;
      this.FTransliterate = false;
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue < 0) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = pas.SysUtils.StrToInt(this.GetAsString());
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if (rtl.isString(V)) {
        Result = "" + V}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FSize;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set(this.GetAsString());
    };
    this.SetAsInteger = function (AValue) {
      this.SetAsString(pas.SysUtils.IntToStr(AValue));
    };
    this.SetAsString = function (AValue) {
      this.SetData(AValue);
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError(rtl.getResStr(pas.DBConst,"SFieldValueError"));
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftString);
      this.FFixedChar = false;
      this.FTransliterate = false;
      this.FSize = 20;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in rtl.createSet($mod.TFieldType.ftString,$mod.TFieldType.ftFixedChar)) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 20});
  });
  rtl.createClass($mod,"TNumericField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayFormat = "";
      this.FEditFormat = "";
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue > 16) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.rangeError = function (AValue, Min, Max) {
      $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SRangeError"),[AValue,Min,Max,this.FFieldName]);
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetEditFormat = function (AValue) {
      if (this.FEditFormat !== AValue) {
        this.FEditFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetAlignment(pas.Classes.TAlignment.taRightJustify);
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taRightJustify});
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
    $r.addProperty("EditFormat",2,rtl.string,"FEditFormat","SetEditFormat");
  });
  rtl.createClass($mod,"TIntegerField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.rangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.rangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsInteger = function () {
      var Result = 0;
      if (!this.GetValue({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = 0;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = pas.SysUtils.IntToStr(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var l = 0;
      var fmt = "";
      AText.set("");
      if (!this.GetValue({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }})) return;
      if (ADisplayText || (this.FEditFormat === "")) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      if (fmt.length !== 0) {
        AText.set(pas.SysUtils.FormatFloat(fmt,l))}
       else AText.set("" + l);
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var V = undefined;
      V = this.GetData();
      Result = pas.JS.isInteger(V);
      if (Result) AValue.set(Math.floor(V));
      return Result;
    };
    this.SetAsInteger = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        this.rangeError(AValue,this.FMinValue,this.FMaxValue)}
       else this.rangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetAsString = function (AValue) {
      var L = 0;
      var Code = 0;
      if (AValue.length === 0) {
        this.Clear()}
       else {
        pas.System.val$5(AValue,{get: function () {
            return L;
          }, set: function (v) {
            L = v;
          }},{get: function () {
            return Code;
          }, set: function (v) {
            Code = v;
          }});
        if (Code === 0) {
          this.SetAsInteger(L)}
         else $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNotAninteger"),[AValue]);
      };
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isInteger(AValue)) {
        this.SetAsInteger(Math.floor(AValue))}
       else this.RaiseAccessError($impl.SInteger);
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftInteger);
      this.FMinRange = -2147483648;
      this.FMaxRange = 2147483647;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.longint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.longint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass($mod,"TLargeintField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.rangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.rangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = this.GetAsLargeInt();
      return Result;
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      if (!this.GetValue({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = 0;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = pas.SysUtils.IntToStr(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var l = 0;
      var fmt = "";
      AText.set("");
      if (!this.GetValue({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }})) return;
      if (ADisplayText || (this.FEditFormat === "")) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      if (fmt.length !== 0) {
        AText.set(pas.SysUtils.FormatFloat(fmt,l))}
       else AText.set("" + l);
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var P = undefined;
      P = this.GetData();
      Result = pas.JS.isInteger(P);
      if (Result) AValue.set(Math.floor(P));
      return Result;
    };
    this.SetAsInteger = function (AValue) {
      this.SetAsLargeInt(AValue);
    };
    this.SetAsLargeInt = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.rangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetAsString = function (AValue) {
      var L = 0;
      var code = 0;
      if (AValue.length === 0) {
        this.Clear()}
       else {
        pas.System.val(AValue,{get: function () {
            return L;
          }, set: function (v) {
            L = v;
          }},{get: function () {
            return code;
          }, set: function (v) {
            code = v;
          }});
        if (code === 0) {
          this.SetAsLargeInt(L)}
         else $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNotAninteger"),[AValue]);
      };
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isInteger(AValue)) {
        this.SetAsLargeInt(Math.floor(AValue))}
       else this.RaiseAccessError($impl.SLargeInt);
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftLargeInt);
      this.FMinRange = -4503599627370496;
      this.FMaxRange = 4503599627370495;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.nativeint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.nativeint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass($mod,"TAutoIncField",$mod.TIntegerField,function () {
    this.SetAsInteger = function (AValue) {
      $mod.TIntegerField.SetAsInteger.apply(this,arguments);
    };
    this.Create$1 = function (AOwner) {
      $mod.TIntegerField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftAutoInc);
    };
  });
  rtl.createClass($mod,"TFloatField",$mod.TNumericField,function () {
    this.$init = function () {
      $mod.TNumericField.$init.call(this);
      this.FCurrency = false;
      this.FMaxValue = 0.0;
      this.FMinValue = 0.0;
      this.FPrecision = 0;
    };
    this.SetCurrency = function (AValue) {
      if (this.FCurrency === AValue) return;
      this.FCurrency = AValue;
    };
    this.SetPrecision = function (AValue) {
      if ((AValue === -1) || (AValue > 1)) {
        this.FPrecision = AValue}
       else this.FPrecision = 2;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = rtl.getNumber(P)}
       else Result = 0.0;
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = Math.round(this.GetAsFloat());
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = P}
       else Result = null;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = pas.SysUtils.FloatToStr(rtl.getNumber(P))}
       else Result = "";
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var fmt = "";
      var E = 0.0;
      var Digits = 0;
      var ff = 0;
      var P = undefined;
      AText.set("");
      P = this.GetData();
      if (!rtl.isNumber(P)) return;
      E = rtl.getNumber(P);
      if (ADisplayText || (this.FEditFormat.length === 0)) {
        fmt = this.FDisplayFormat}
       else fmt = this.FEditFormat;
      Digits = 0;
      if (!this.FCurrency) {
        ff = pas.SysUtils.TFloatFormat.ffGeneral}
       else {
        Digits = 2;
        ff = pas.SysUtils.TFloatFormat.ffFixed;
      };
      if (fmt !== "") {
        AText.set(pas.SysUtils.FormatFloat(fmt,E))}
       else AText.set(pas.SysUtils.FloatToStrF(E,ff,this.FPrecision,Digits));
    };
    this.SetAsFloat = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.rangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetAsInteger = function (AValue) {
      this.SetAsFloat(AValue);
    };
    this.SetAsString = function (AValue) {
      var f = 0.0;
      if (AValue === "") {
        this.Clear()}
       else {
        if (!pas.SysUtils.TryStrToFloat(AValue,{get: function () {
            return f;
          }, set: function (v) {
            f = v;
          }})) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNotAFloat"),[AValue]);
        this.SetAsFloat(f);
      };
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isNumber(AValue)) {
        this.SetAsFloat(rtl.getNumber(AValue))}
       else this.RaiseAccessError("Float");
    };
    this.Create$1 = function (AOwner) {
      $mod.TNumericField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftFloat);
      this.FPrecision = 15;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = true;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Currency",2,rtl.boolean,"FCurrency","SetCurrency",{Default: false});
    $r.addProperty("MaxValue",0,rtl.double,"FMaxValue","FMaxValue");
    $r.addProperty("MinValue",0,rtl.double,"FMinValue","FMinValue");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 15});
  });
  rtl.createClass($mod,"TBooleanField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayValues = "";
      this.FDisplays = rtl.arraySetLength(null,"",2,2);
    };
    this.$final = function () {
      this.FDisplays = undefined;
      $mod.TField.$final.call(this);
    };
    this.SetDisplayValues = function (AValue) {
      var I = 0;
      if (this.FDisplayValues !== AValue) {
        I = pas.System.Pos(";",AValue);
        if ((I < 2) || (I === AValue.length)) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidDisplayValues"),[AValue]);
        this.FDisplayValues = AValue;
        this.FDisplays[0][1] = pas.System.Copy(AValue,1,I - 1);
        this.FDisplays[1][1] = pas.SysUtils.UpperCase(this.FDisplays[0][1]);
        this.FDisplays[0][0] = pas.System.Copy(AValue,I + 1,AValue.length - I);
        this.FDisplays[1][0] = pas.SysUtils.UpperCase(this.FDisplays[0][0]);
        this.PropertyChanged(true);
      };
    };
    this.GetAsBoolean = function () {
      var Result = false;
      var P = undefined;
      P = this.GetData();
      if (pas.JS.isBoolean(P)) {
        Result = !(P == false)}
       else Result = false;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if (pas.JS.isBoolean(P)) {
        Result = this.FDisplays[0][+!(P == false)]}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if (pas.JS.isBoolean(P)) {
        Result = !(P == false)}
       else Result = null;
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = this.GetAsBoolean() + 0;
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FDisplays[0][0].length;
      if (Result < this.FDisplays[0][1].length) Result = this.FDisplays[0][1].length;
      return Result;
    };
    this.SetAsBoolean = function (AValue) {
      this.SetData(AValue);
    };
    this.SetAsString = function (AValue) {
      var Temp = "";
      Temp = pas.SysUtils.UpperCase(AValue);
      if (Temp === "") {
        this.Clear()}
       else if (pas.System.Pos(Temp,this.FDisplays[1][1]) === 1) {
        this.SetAsBoolean(true)}
       else if (pas.System.Pos(Temp,this.FDisplays[1][0]) === 1) {
        this.SetAsBoolean(false)}
       else $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SNotABoolean"),[AValue]);
    };
    this.SetAsInteger = function (AValue) {
      this.SetAsBoolean(AValue !== 0);
    };
    this.SetVarValue = function (AValue) {
      if (pas.JS.isBoolean(AValue)) {
        this.SetAsBoolean(!(AValue == false))}
       else if (rtl.isNumber(AValue)) this.SetAsBoolean(rtl.getNumber(AValue) !== 0);
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftBoolean);
      this.SetDisplayValues("True;False");
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayValues",2,rtl.string,"FDisplayValues","SetDisplayValues");
  });
  rtl.createClass($mod,"TDateTimeField",$mod.TField,function () {
    this.$init = function () {
      $mod.TField.$init.call(this);
      this.FDisplayFormat = "";
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.ConvertToDateTime = function (aValue, aRaiseError) {
      var Result = 0.0;
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertToDateTime(aValue,aRaiseError)}
       else Result = $mod.TDataSet.DefaultConvertToDateTime(aValue,aRaiseError);
      return Result;
    };
    this.DateTimeToNativeDateTime = function (aValue) {
      var Result = undefined;
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertDateTimeToNative(aValue)}
       else Result = $mod.TDataSet.DefaultConvertDateTimeToNative(aValue);
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      this.GetText({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},false);
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      if (!rtl.isString(Result)) Result = null;
      return Result;
    };
    this.GetDataSize = function () {
      var Result = 0;
      Result = $mod.TField.GetDataSize.call(this);
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var R = 0.0;
      var F = "";
      R = this.ConvertToDateTime(this.GetData(),false);
      if (R === 0) {
        AText.set("")}
       else {
        if (ADisplayText && (this.FDisplayFormat.length !== 0)) {
          F = this.FDisplayFormat}
         else {
          var $tmp1 = this.FDataType;
          if ($tmp1 === $mod.TFieldType.ftTime) {
            F = pas.SysUtils.LongTimeFormat}
           else if ($tmp1 === $mod.TFieldType.ftDate) {
            F = pas.SysUtils.ShortDateFormat}
           else {
            F = "c";
          };
        };
        AText.set(pas.SysUtils.FormatDateTime(F,R));
      };
    };
    this.SetAsDateTime = function (AValue) {
      this.SetData(this.DateTimeToNativeDateTime(AValue));
    };
    this.SetAsString = function (AValue) {
      var R = 0.0;
      if (AValue !== "") {
        R = pas.SysUtils.StrToDateTime(AValue);
        this.SetData(this.DateTimeToNativeDateTime(R));
      } else this.SetData(null);
    };
    this.SetVarValue = function (AValue) {
      this.SetAsDateTime(this.ConvertToDateTime(AValue,true));
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftDateTime);
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
  });
  rtl.createClass($mod,"TDateField",$mod.TDateTimeField,function () {
    this.Create$1 = function (AOwner) {
      $mod.TDateTimeField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftDate);
    };
  });
  rtl.createClass($mod,"TTimeField",$mod.TDateTimeField,function () {
    this.SetAsString = function (AValue) {
      var R = 0.0;
      if (AValue !== "") {
        R = pas.SysUtils.StrToTime(AValue);
        this.SetData(this.DateTimeToNativeDateTime(R));
      } else this.SetData(null);
    };
    this.Create$1 = function (AOwner) {
      $mod.TDateTimeField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftTime);
    };
  });
  rtl.createClass($mod,"TBinaryField",$mod.TField,function () {
    this.CheckTypeSize = function (AValue) {
      if (AValue < 1) $mod.DatabaseErrorFmt(rtl.getResStr(pas.DBConst,"SInvalidFieldSize"),[AValue]);
    };
    this.BlobToBytes = function (aValue) {
      var Result = [];
      if (this.FDataSet != null) {
        Result = this.FDataSet.BlobDataToBytes(aValue)}
       else Result = $mod.TDataSet.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      var S = [];
      var I = 0;
      Result = "";
      V = this.GetData();
      if (V != null) {
        S = this.BlobToBytes(V);
        for (var $l1 = 0, $end2 = rtl.length(S); $l1 <= $end2; $l1++) {
          I = $l1;
          Result.concat(String.fromCharCode(S[I]));
        };
      };
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetAsString = function (AValue) {
      var B = [];
      var i = 0;
      B = rtl.arraySetLength(B,0,AValue.length);
      for (var $l1 = 1, $end2 = AValue.length; $l1 <= $end2; $l1++) {
        i = $l1;
        B[i - 1] = AValue.charCodeAt(i - 1);
      };
      this.SetAsBytes(B);
    };
    this.SetVarValue = function (AValue) {
      var B = [];
      var I = 0;
      var Len = 0;
      if (rtl.isArray(AValue)) {
        Len = rtl.length(AValue);
        B = rtl.arraySetLength(B,0,Len);
        for (var $l1 = 1, $end2 = Len - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          B[I] = AValue[I];
        };
        this.SetAsBytes(B);
      } else if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError("Blob");
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 16});
  });
  rtl.createClass($mod,"TBlobField",$mod.TBinaryField,function () {
    this.$init = function () {
      $mod.TBinaryField.$init.call(this);
      this.FModified = false;
    };
    this.GetBlobSize = function () {
      var Result = 0;
      var B = [];
      B = this.GetAsBytes();
      Result = rtl.length(B);
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      if (!this.FModified) {
        Result = $mod.TField.GetIsNull.call(this)}
       else Result = this.GetBlobSize() === 0;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      AText.set($mod.TBinaryField.GetAsString.call(this));
    };
    this.Create$1 = function (AOwner) {
      $mod.TBinaryField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftBlob);
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.IsBlob = function () {
      var Result = false;
      Result = true;
      return Result;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in $mod.ftBlobTypes) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass($mod,"TMemoField",$mod.TBlobField,function () {
    this.Create$1 = function (AOwner) {
      $mod.TBlobField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftMemo);
    };
  });
  rtl.createClass($mod,"TVariantField",$mod.TField,function () {
    this.CheckTypeSize = function (aValue) {
    };
    this.GetAsInteger = function () {
      var Result = 0;
      var V = undefined;
      V = this.GetData();
      if (pas.JS.isInteger(V)) {
        Result = Math.floor(V)}
       else if (rtl.isString(V)) {
        Result = parseInt("" + V)}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.SetAsInteger = function (AValue) {
      this.SetVarValue(AValue);
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if (pas.JS.isInteger(V)) {
        Result = pas.SysUtils.IntToStr(Math.floor(V))}
       else if (rtl.isNumber(V)) {
        Result = pas.SysUtils.FloatToStr(rtl.getNumber(V))}
       else if (rtl.isString(V)) {
        Result = "" + V}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.SetAsString = function (aValue) {
      this.SetVarValue(aValue);
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetVarValue = function (aValue) {
      this.SetData(aValue);
    };
    this.Create$1 = function (AOwner) {
      $mod.TField.Create$1.call(this,AOwner);
      this.SetDataType($mod.TFieldType.ftVariant);
    };
  });
  rtl.createClass($mod,"TCheckConstraint",pas.Classes.TCollectionItem,function () {
    this.$init = function () {
      pas.Classes.TCollectionItem.$init.call(this);
      this.FCustomConstraint = "";
      this.FErrorMessage = "";
      this.FFromDictionary = false;
      this.FImportedConstraint = "";
    };
    this.Assign = function (Source) {
    };
    var $r = this.$rtti;
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ErrorMessage",0,rtl.string,"FErrorMessage","FErrorMessage");
    $r.addProperty("FromDictionary",0,rtl.boolean,"FFromDictionary","FFromDictionary");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
  });
  rtl.createClass($mod,"TCheckConstraints",pas.Classes.TCollection,function () {
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Create$2 = function (AOwner) {
      pas.Classes.TCollection.Create$1.call(this,$mod.TCheckConstraint);
    };
  });
  rtl.createClass($mod,"TFields",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FDataset = null;
      this.FFieldList = null;
      this.FOnChange = null;
      this.FValidFieldKinds = {};
    };
    this.$final = function () {
      this.FDataset = undefined;
      this.FFieldList = undefined;
      this.FOnChange = undefined;
      this.FValidFieldKinds = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.ClearFieldDefs = function () {
      var i = 0;
      for (var $l1 = 0, $end2 = this.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        this.GetField(i).FFieldDef = null;
      };
    };
    this.Changed = function () {
      if ((this.FDataset !== null) && !(pas.Classes.TComponentStateItem.csDestroying in this.FDataset.FComponentState)) this.FDataset.DataEvent($mod.TDataEvent.deFieldListChange,0);
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FFieldList.FCount;
      return Result;
    };
    this.GetField = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FFieldList.Get(Index));
      return Result;
    };
    this.SetFieldIndex = function (Field, Value) {
      var Old = 0;
      Old = this.FFieldList.IndexOf(Field);
      if (Old === -1) return;
      if (Value < 0) Value = 0;
      if (Value >= this.GetCount()) Value = this.GetCount() - 1;
      if (Value !== Old) {
        this.FFieldList.Delete(Old);
        this.FFieldList.Insert(Value,Field);
        Field.PropertyChanged(true);
        this.Changed();
      };
    };
    this.Create$1 = function (ADataset) {
      this.FDataset = ADataset;
      this.FFieldList = pas.Classes.TFPList.$create("Create");
      this.FValidFieldKinds = rtl.createSet(null,$mod.TFieldKind.fkData,$mod.TFieldKind.fkInternalCalc);
    };
    this.Destroy = function () {
      if (this.FFieldList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldList;
        }, set: function (v) {
          this.p.FFieldList = v;
        }});
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Field) {
      this.CheckFieldName(Field.FFieldName);
      this.FFieldList.Add(Field);
      Field.FFields = this;
      this.Changed();
    };
    this.CheckFieldName = function (Value) {
      if (this.FindField(Value) !== null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SDuplicateFieldName"),[Value],this.FDataset);
    };
    this.CheckFieldNames = function (Value) {
      var N = "";
      var StrPos = 0;
      if (Value === "") return;
      StrPos = 1;
      do {
        N = $mod.ExtractFieldName(Value,{get: function () {
            return StrPos;
          }, set: function (v) {
            StrPos = v;
          }});
        this.FieldByName(N);
      } while (!(StrPos > Value.length));
    };
    this.Clear = function () {
      var AField = null;
      while (this.FFieldList.FCount > 0) {
        AField = rtl.getObject(this.FFieldList.Last());
        AField.FDataSet = null;
        AField = rtl.freeLoc(AField);
        this.FFieldList.Delete(this.FFieldList.FCount - 1);
      };
      this.Changed();
    };
    this.FindField = function (Value) {
      var Result = null;
      var S = "";
      var I = 0;
      S = pas.SysUtils.UpperCase(Value);
      for (var $l1 = 0, $end2 = this.FFieldList.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        Result = rtl.getObject(this.FFieldList.Get(I));
        if (S === pas.SysUtils.UpperCase(Result.FFieldName)) {
          return Result;
        };
      };
      Result = null;
      return Result;
    };
    this.FieldByName = function (Value) {
      var Result = null;
      Result = this.FindField(Value);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[Value],this.FDataset);
      return Result;
    };
    this.IndexOf = function (Field) {
      var Result = 0;
      Result = this.FFieldList.IndexOf(Field);
      return Result;
    };
    this.Remove = function (Value) {
      this.FFieldList.Remove(Value);
      Value.FFields = null;
      this.Changed();
    };
  });
  this.TBookmarkFlag = {"0": "bfCurrent", bfCurrent: 0, "1": "bfBOF", bfBOF: 1, "2": "bfEOF", bfEOF: 2, "3": "bfInserted", bfInserted: 3};
  this.TBookmark = function (s) {
    if (s) {
      this.Data = s.Data;
    } else {
      this.Data = undefined;
    };
    this.$equal = function (b) {
      return this.Data === b.Data;
    };
  };
  this.TGetMode = {"0": "gmCurrent", gmCurrent: 0, "1": "gmNext", gmNext: 1, "2": "gmPrior", gmPrior: 2};
  this.TGetResult = {"0": "grOK", grOK: 0, "1": "grBOF", grBOF: 1, "2": "grEOF", grEOF: 2, "3": "grError", grError: 3};
  this.TResyncMode$a = {"0": "rmExact", rmExact: 0, "1": "rmCenter", rmCenter: 1};
  this.TDataAction = {"0": "daFail", daFail: 0, "1": "daAbort", daAbort: 1, "2": "daRetry", daRetry: 2};
  this.TLoadOption = {"0": "loNoOpen", loNoOpen: 0, "1": "loNoEvents", loNoEvents: 1, "2": "loAtEOF", loAtEOF: 2};
  this.TRecordState = {"0": "rsNew", rsNew: 0, "1": "rsClean", rsClean: 1, "2": "rsUpdate", rsUpdate: 2, "3": "rsDelete", rsDelete: 3};
  this.TDataRecord = function (s) {
    if (s) {
      this.data = s.data;
      this.state = s.state;
      this.bookmark = s.bookmark;
      this.bookmarkFlag = s.bookmarkFlag;
    } else {
      this.data = undefined;
      this.state = 0;
      this.bookmark = undefined;
      this.bookmarkFlag = 0;
    };
    this.$equal = function (b) {
      return (this.data === b.data) && ((this.state === b.state) && ((this.bookmark === b.bookmark) && (this.bookmarkFlag === b.bookmarkFlag)));
    };
  };
  rtl.createClass($mod,"TDataSet",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAfterLoad = null;
      this.FBeforeLoad = null;
      this.FBlockReadSize = 0;
      this.FCalcBuffer = new $mod.TDataRecord();
      this.FCalcFieldsSize = 0;
      this.FOnLoadFail = null;
      this.FOpenAfterRead = false;
      this.FActiveRecord = 0;
      this.FAfterCancel = null;
      this.FAfterClose = null;
      this.FAfterDelete = null;
      this.FAfterEdit = null;
      this.FAfterInsert = null;
      this.FAfterOpen = null;
      this.FAfterPost = null;
      this.FAfterScroll = null;
      this.FAutoCalcFields = false;
      this.FBOF = false;
      this.FBeforeCancel = null;
      this.FBeforeClose = null;
      this.FBeforeDelete = null;
      this.FBeforeEdit = null;
      this.FBeforeInsert = null;
      this.FBeforeOpen = null;
      this.FBeforePost = null;
      this.FBeforeScroll = null;
      this.FBlobFieldCount = 0;
      this.FBuffers = [];
      this.FBufferCount = 0;
      this.FConstraints = null;
      this.FDisableControlsCount = 0;
      this.FDisableControlsState = 0;
      this.FCurrentRecord = 0;
      this.FDataSources = null;
      this.FDefaultFields = false;
      this.FEOF = false;
      this.FEnableControlsEvent = 0;
      this.FFieldList = null;
      this.FFieldDefs = null;
      this.FInternalCalcFields = false;
      this.FModified = false;
      this.FOnCalcFields = null;
      this.FOnDeleteError = null;
      this.FOnEditError = null;
      this.FOnNewRecord = null;
      this.FOnPostError = null;
      this.FRecordCount = 0;
      this.FIsUniDirectional = false;
      this.FState = 0;
      this.FInternalOpenComplete = false;
      this.FDataProxy = null;
      this.FDataRequestID = 0;
      this.FChangeList = null;
    };
    this.$final = function () {
      this.FAfterLoad = undefined;
      this.FBeforeLoad = undefined;
      this.FCalcBuffer = undefined;
      this.FOnLoadFail = undefined;
      this.FAfterCancel = undefined;
      this.FAfterClose = undefined;
      this.FAfterDelete = undefined;
      this.FAfterEdit = undefined;
      this.FAfterInsert = undefined;
      this.FAfterOpen = undefined;
      this.FAfterPost = undefined;
      this.FAfterScroll = undefined;
      this.FBeforeCancel = undefined;
      this.FBeforeClose = undefined;
      this.FBeforeDelete = undefined;
      this.FBeforeEdit = undefined;
      this.FBeforeInsert = undefined;
      this.FBeforeOpen = undefined;
      this.FBeforePost = undefined;
      this.FBeforeScroll = undefined;
      this.FBuffers = undefined;
      this.FConstraints = undefined;
      this.FDataSources = undefined;
      this.FFieldList = undefined;
      this.FFieldDefs = undefined;
      this.FOnCalcFields = undefined;
      this.FOnDeleteError = undefined;
      this.FOnEditError = undefined;
      this.FOnNewRecord = undefined;
      this.FOnPostError = undefined;
      this.FDataProxy = undefined;
      this.FChangeList = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.DoInsertAppend = function (DoAppend) {
      var Self = this;
      function DoInsert(DoAppend) {
        var BookBeforeInsert = new $mod.TBookmark();
        var TempBuf = new $mod.TDataRecord();
        var I = 0;
        if (Self.FRecordCount > 0) BookBeforeInsert = new $mod.TBookmark(Self.GetBookmark());
        if (!DoAppend) {
          if (Self.FRecordCount > 0) {
            TempBuf = new $mod.TDataRecord(Self.FBuffers[Self.FBufferCount]);
            for (var $l1 = Self.FBufferCount, $end2 = Self.FActiveRecord + 1; $l1 >= $end2; $l1--) {
              I = $l1;
              Self.FBuffers[I] = new $mod.TDataRecord(Self.FBuffers[I - 1]);
            };
            Self.FBuffers[Self.FActiveRecord] = new $mod.TDataRecord(TempBuf);
          };
        } else if (Self.FRecordCount === Self.FBufferCount) {
          Self.ShiftBuffersBackward()}
         else {
          if (Self.FRecordCount > 0) Self.FActiveRecord += 1;
        };
        Self.InitRecord({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
        Self.CursorPosChanged();
        if (Self.FRecordCount === 0) {
          Self.SetBookmarkFlag({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
              return this.p[this.a];
            }, set: function (v) {
              this.p[this.a] = v;
            }},$mod.TBookmarkFlag.bfEOF)}
         else {
          Self.FBOF = false;
          if (Self.FRecordCount > 0) {
            Self.SetBookmarkData({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
                return this.p[this.a];
              }, set: function (v) {
                this.p[this.a] = v;
              }},new $mod.TBookmark(BookBeforeInsert));
            Self.FreeBookmark(new $mod.TBookmark(BookBeforeInsert));
          };
        };
        Self.InternalInsert();
        if (Self.FRecordCount < Self.FBufferCount) Self.FRecordCount += 1;
      };
      Self.CheckBrowseMode();
      if (!Self.GetCanModify()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SDatasetReadOnly"),Self);
      Self.DoBeforeInsert();
      Self.DoBeforeScroll();
      if (!DoAppend) {
        DoInsert(false);
      } else {
        Self.ClearBuffers();
        Self.InternalLast();
        Self.GetPriorRecords();
        if (Self.FRecordCount > 0) Self.FActiveRecord = Self.FRecordCount - 1;
        DoInsert(true);
        Self.SetBookmarkFlag({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }},$mod.TBookmarkFlag.bfEOF);
        Self.FBOF = false;
        Self.FEOF = true;
      };
      Self.SetState($mod.TDataSetState.dsInsert);
      try {
        Self.DoOnNewRecord();
      } catch ($e) {
        Self.SetCurrentRecord(Self.FActiveRecord);
        Self.Resync({});
        throw $e;
      };
      Self.FModified = false;
      Self.DataEvent($mod.TDataEvent.deDataSetChange,0);
      Self.DoAfterInsert();
      Self.DoAfterScroll();
    };
    this.DoInternalOpen = function () {
      this.InternalOpen();
      this.FInternalOpenComplete = true;
      this.FRecordCount = 0;
      this.RecalcBufListSize();
      this.FBOF = true;
      this.FEOF = this.FRecordCount === 0;
      if (this.GetDataProxy() != null) this.InitChangeList();
    };
    this.GetBufferCount = function () {
      var Result = 0;
      Result = rtl.length(this.FBuffers);
      return Result;
    };
    this.GetDataProxy = function () {
      var Result = null;
      if (this.FDataProxy === null) this.SetDataProxy(this.DoGetDataProxy());
      Result = this.FDataProxy;
      return Result;
    };
    this.RegisterDataSource = function (ADataSource) {
      this.FDataSources.Add(ADataSource);
      this.RecalcBufListSize();
    };
    this.SetDataProxy = function (AValue) {
      if (AValue === this.FDataProxy) return;
      if (this.FDataProxy != null) this.FDataProxy.RemoveFreeNotification(this);
      this.FDataProxy = AValue;
      if (this.FDataProxy != null) this.FDataProxy.FreeNotification(this);
    };
    this.ShiftBuffersForward = function () {
      var TempBuf = new $mod.TDataRecord();
      var I = 0;
      TempBuf = new $mod.TDataRecord(this.FBuffers[this.FBufferCount]);
      for (var $l1 = this.FBufferCount; $l1 >= 1; $l1--) {
        I = $l1;
        this.FBuffers[I] = new $mod.TDataRecord(this.FBuffers[I - 1]);
      };
      this.FBuffers[0] = new $mod.TDataRecord(TempBuf);
    };
    this.ShiftBuffersBackward = function () {
      var TempBuf = new $mod.TDataRecord();
      var I = 0;
      TempBuf = new $mod.TDataRecord(this.FBuffers[0]);
      for (var $l1 = 1, $end2 = this.FBufferCount; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FBuffers[I - 1] = new $mod.TDataRecord(this.FBuffers[I]);
      };
      this.FBuffers[this.GetBufferCount()] = new $mod.TDataRecord(TempBuf);
    };
    this.TryDoing = function (P, Ev) {
      var Result = false;
      var Retry = 0;
      Result = true;
      Retry = $mod.TDataAction.daRetry;
      while (Retry === $mod.TDataAction.daRetry) try {
        this.UpdateCursorPos();
        P();
        return Result;
      } catch ($e) {
        if ($mod.EDatabaseError.isPrototypeOf($e)) {
          var E = $e;
          Retry = $mod.TDataAction.daFail;
          if (Ev != null) Ev(this,E,{get: function () {
              return Retry;
            }, set: function (v) {
              Retry = v;
            }});
          var $tmp1 = Retry;
          if ($tmp1 === $mod.TDataAction.daFail) {
            throw $e}
           else if ($tmp1 === $mod.TDataAction.daAbort) pas.SysUtils.Abort();
        } else {
          throw $e;
        }
      };
      return Result;
    };
    this.GetActive = function () {
      var Result = false;
      Result = (this.FState !== $mod.TDataSetState.dsInactive) && (this.FState !== $mod.TDataSetState.dsOpening);
      return Result;
    };
    this.UnRegisterDataSource = function (ADataSource) {
      this.FDataSources.Remove(ADataSource);
    };
    this.HandleRequestresponse = function (ARequest) {
      var DataAdded = false;
      if (!(ARequest != null)) return;
      var $tmp1 = ARequest.FSuccess;
      if ($tmp1 === $mod.TDataRequestResult.rrFail) {
        if (this.FOnLoadFail != null) this.FOnLoadFail(this,ARequest.FRequestID,ARequest.FErrorMsg);
      } else if (($tmp1 === $mod.TDataRequestResult.rrEOF) || ($tmp1 === $mod.TDataRequestResult.rrOK)) {
        DataAdded = false;
        if (ARequest.FEvent != null) ARequest.FEvent(this,ARequest.FData);
        if (ARequest.FSuccess !== $mod.TDataRequestResult.rrEOF) DataAdded = this.DataPacketReceived(ARequest);
        if (!(this.GetActive() || ($mod.TLoadOption.loNoOpen in ARequest.FLoadOptions))) {
          if (!($mod.TLoadOption.loNoEvents in ARequest.FLoadOptions)) this.DoAfterLoad();
          this.Open();
        } else {
          if (($mod.TLoadOption.loAtEOF in ARequest.FLoadOptions) && DataAdded) this.FEOF = false;
          if (!($mod.TLoadOption.loNoEvents in ARequest.FLoadOptions)) this.DoAfterLoad();
        };
      };
      ARequest.$destroy("Destroy");
    };
    this.DataPacketReceived = function (ARequest) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.DoLoad = function (aOptions, aAfterLoad) {
      var Result = false;
      var Request = null;
      if (!($mod.TLoadOption.loNoEvents in aOptions)) this.DoBeforeLoad();
      Result = this.GetDataProxy() !== null;
      if (!Result) return Result;
      Request = this.GetDataProxy().GetDataRequest(rtl.refSet(aOptions),rtl.createCallback(this,"HandleRequestresponse"),aAfterLoad);
      Request.FDataset = this;
      if (this.GetActive()) Request.FBookmark = new $mod.TBookmark(this.GetBookmark());
      this.FDataRequestID += 1;
      Request.FRequestID = this.FDataRequestID;
      this.GetDataProxy().DoGetData(Request);
      return Result;
    };
    this.DoGetDataProxy = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InitChangeList = function () {
      this.DoneChangeList();
      this.FChangeList = pas.Classes.TFPList.$create("Create");
    };
    this.DoneChangeList = function () {
      this.ClearChangeList();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FChangeList;
        }, set: function (v) {
          this.p.FChangeList = v;
        }});
    };
    this.ClearChangeList = function () {
      var I = 0;
      if (!(this.FChangeList != null)) return;
      for (var $l1 = 0, $end2 = this.FChangeList.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        rtl.getObject(this.FChangeList.Get(I)).$destroy("Destroy");
        this.FChangeList.Put(I,null);
      };
    };
    this.IndexInChangeList = function (aBookmark) {
      var Result = 0;
      Result = -1;
      if (!(this.FChangeList != null)) return Result;
      Result = this.FChangeList.FCount - 1;
      while ((Result >= 0) && (this.CompareBookmarks(new $mod.TBookmark(aBookmark),new $mod.TBookmark(rtl.getObject(this.FChangeList.Get(Result)).FBookmark)) !== 0)) Result -= 1;
      return Result;
    };
    this.AddToChangeList = function (aChange) {
      var Result = null;
      var B = new $mod.TBookmark();
      var I = 0;
      Result = null;
      if (!(this.FChangeList != null)) return Result;
      B = new $mod.TBookmark(this.GetBookmark());
      I = this.IndexInChangeList(new $mod.TBookmark(B));
      if (I === -1) {
        if (this.GetDataProxy() != null) {
          Result = this.GetDataProxy().GetUpdateDescriptor(this,new $mod.TBookmark(B),this.ActiveBuffer().data,aChange)}
         else Result = $mod.TRecordUpdateDescriptor.$create("Create$1",[null,this,new $mod.TBookmark(B),this.ActiveBuffer().data,aChange]);
        this.FChangeList.Add(Result);
      } else {
        Result = rtl.getObject(this.FChangeList.Get(I));
        var $tmp1 = aChange;
        if ($tmp1 === $mod.TUpdateStatus.usDeleted) {
          Result.FStatus = $mod.TUpdateStatus.usDeleted}
         else if ($tmp1 === $mod.TUpdateStatus.usInserted) {
          $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SErrInsertingSameRecordtwice"),this)}
         else if ($tmp1 === $mod.TUpdateStatus.usModified) Result.FData = this.ActiveBuffer().data;
      };
      return Result;
    };
    this.RemoveFromChangeList = function (R) {
      if (!((R != null) && (this.FChangeList != null))) return;
    };
    this.RecalcBufListSize = function () {
      var i = 0;
      var j = 0;
      var ABufferCount = 0;
      var DataLink = null;
      if (!this.IsCursorOpen()) return;
      if (this.FIsUniDirectional) {
        ABufferCount = 1}
       else ABufferCount = 10;
      for (var $l1 = 0, $end2 = this.FDataSources.FCount - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        for (var $l3 = 0, $end4 = rtl.getObject(this.FDataSources.Get(i)).FDataLinks.GetCount() - 1; $l3 <= $end4; $l3++) {
          j = $l3;
          DataLink = rtl.getObject(rtl.getObject(this.FDataSources.Get(i)).FDataLinks.Get(j));
          if (ABufferCount < DataLink.GetBufferCount()) ABufferCount = DataLink.GetBufferCount();
        };
      };
      if (this.FBufferCount === ABufferCount) return;
      this.SetBufListSize(ABufferCount);
      this.GetNextRecords();
      if ((this.FRecordCount < this.FBufferCount) && !this.FIsUniDirectional) {
        this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
        this.CursorPosChanged();
      };
    };
    this.ActivateBuffers = function () {
      this.FBOF = false;
      this.FEOF = false;
      this.FActiveRecord = 0;
    };
    this.BindFields = function (Binding) {
      var i = 0;
      var FieldIndex = 0;
      var FieldDef = null;
      var Field = null;
      this.FCalcFieldsSize = 0;
      this.FBlobFieldCount = 0;
      for (var $l1 = 0, $end2 = this.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        Field = this.FFieldList.GetField(i);
        Field.FFieldDef = null;
        if (!Binding) {
          Field.FFieldNo = 0}
         else if (Field.FFieldKind in rtl.createSet($mod.TFieldKind.fkCalculated,$mod.TFieldKind.fkLookup)) {
          Field.FFieldNo = -1;
          Field.FOffset = this.FCalcFieldsSize;
          this.FCalcFieldsSize += Field.GetDataSize() + 1;
        } else {
          FieldIndex = this.FFieldDefs.IndexOf(Field.FFieldName);
          if (FieldIndex === -1) {
            $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[Field.FFieldName],this)}
           else {
            FieldDef = this.FFieldDefs.GetItem$1(FieldIndex);
            Field.FFieldDef = FieldDef;
            Field.FFieldNo = FieldDef.FFieldNo;
            if (FieldDef.FInternalCalcField) this.FInternalCalcFields = true;
            if (Field.$class.IsBlob()) {
              Field.FSize = FieldDef.FSize;
              Field.FOffset = this.FBlobFieldCount;
              this.FBlobFieldCount += 1;
            };
          };
        };
        Field.Bind(Binding);
      };
    };
    this.BlockReadNext = function () {
      this.MoveBy(1);
    };
    var BookmarkStates = rtl.createSet($mod.TDataSetState.dsBrowse,$mod.TDataSetState.dsEdit,$mod.TDataSetState.dsInsert);
    this.BookmarkAvailable = function () {
      var Result = false;
      Result = ((!this.IsEmpty() && !this.FIsUniDirectional) && (this.FState in BookmarkStates)) && (this.GetBookmarkFlag(new $mod.TDataRecord(this.ActiveBuffer())) === $mod.TBookmarkFlag.bfCurrent);
      return Result;
    };
    this.CalculateFields = function (Buffer) {
      var i = 0;
      var OldState = 0;
      this.FCalcBuffer = new $mod.TDataRecord(Buffer.get());
      if (this.FState !== $mod.TDataSetState.dsInternalCalc) {
        OldState = this.FState;
        this.FState = $mod.TDataSetState.dsCalcFields;
        try {
          this.ClearCalcFields({p: this, get: function () {
              return this.p.FCalcBuffer;
            }, set: function (v) {
              this.p.FCalcBuffer = v;
            }});
          if (!this.FIsUniDirectional) for (var $l1 = 0, $end2 = this.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
            i = $l1;
            if (this.FFieldList.GetField(i).FFieldKind === $mod.TFieldKind.fkLookup) this.FFieldList.GetField(i).CalcLookupValue();
          };
        } finally {
          this.DoOnCalcFields();
          this.FState = OldState;
        };
      };
    };
    this.CheckActive = function () {
      if (!this.GetActive()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SInactiveDataset"),this);
    };
    this.CheckInactive = function () {
      if (this.GetActive()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SActiveDataset"),this);
    };
    this.CheckBiDirectional = function () {
      if (this.FIsUniDirectional) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SUniDirectional"),this);
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.apply(this,arguments);
      try {
        if (this.FOpenAfterRead) this.SetActive(true);
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var E = $e;
          if (pas.Classes.TComponentStateItem.csDesigning in this.FComponentState) this.InternalHandleException(E);
        } else {
          throw $e;
        }
      };
    };
    this.ClearBuffers = function () {
      this.FRecordCount = 0;
      this.FActiveRecord = 0;
      this.FCurrentRecord = -1;
      this.FBOF = true;
      this.FEOF = true;
    };
    this.ClearCalcFields = function (Buffer) {
    };
    this.CloseCursor = function () {
      this.ClearBuffers();
      this.SetBufListSize(0);
      this.FFieldList.ClearFieldDefs();
      this.InternalClose();
      this.FInternalOpenComplete = false;
    };
    this.CreateFields = function () {
      var I = 0;
      for (var $l1 = 0, $end2 = this.FFieldDefs.GetCount() - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        var $with3 = this.FFieldDefs.GetItem$1(I);
        if ($with3.FDataType !== $mod.TFieldType.ftUnknown) {
          $with3.CreateField(this);
        };
      };
    };
    this.DataEvent = function (Event, Info) {
      var Self = this;
      function HandleFieldChange(aField) {
        if (aField.FFieldKind in rtl.createSet($mod.TFieldKind.fkData,$mod.TFieldKind.fkInternalCalc)) Self.SetModified(true);
        if (Self.FState !== $mod.TDataSetState.dsSetKey) {
          if (aField.FFieldKind === $mod.TFieldKind.fkData) {
            if (Self.FInternalCalcFields) {
              Self.RefreshInternalCalcFields({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
                  return this.p[this.a];
                }, set: function (v) {
                  this.p[this.a] = v;
                }})}
             else if (Self.FAutoCalcFields && (Self.FCalcFieldsSize !== 0)) Self.CalculateFields({a: Self.FActiveRecord, p: Self.FBuffers, get: function () {
                return this.p[this.a];
              }, set: function (v) {
                this.p[this.a] = v;
              }});
          };
          aField.Change();
        };
      };
      function HandleScrollOrChange() {
        if (Self.FState !== $mod.TDataSetState.dsInsert) Self.UpdateCursorPos();
      };
      var i = 0;
      var $tmp1 = Event;
      if ($tmp1 === $mod.TDataEvent.deFieldChange) {
        HandleFieldChange(rtl.getObject(Info))}
       else if (($tmp1 === $mod.TDataEvent.deDataSetChange) || ($tmp1 === $mod.TDataEvent.deDataSetScroll)) {
        HandleScrollOrChange()}
       else if ($tmp1 === $mod.TDataEvent.deLayoutChange) Self.FEnableControlsEvent = $mod.TDataEvent.deLayoutChange;
      if (!Self.ControlsDisabled() && (Self.FState !== $mod.TDataSetState.dsBlockRead)) {
        for (var $l2 = 0, $end3 = Self.FDataSources.FCount - 1; $l2 <= $end3; $l2++) {
          i = $l2;
          rtl.getObject(Self.FDataSources.Get(i)).ProcessEvent(Event,Info);
        };
      };
    };
    this.DestroyFields = function () {
      this.FFieldList.Clear();
    };
    this.DoAfterCancel = function () {
      if (this.FAfterCancel != null) this.FAfterCancel(this);
    };
    this.DoAfterClose = function () {
      if ((this.FAfterClose != null) && !(pas.Classes.TComponentStateItem.csDestroying in this.FComponentState)) this.FAfterClose(this);
    };
    this.DoAfterDelete = function () {
      if (this.FAfterDelete != null) this.FAfterDelete(this);
    };
    this.DoAfterEdit = function () {
      if (this.FAfterEdit != null) this.FAfterEdit(this);
    };
    this.DoAfterInsert = function () {
      if (this.FAfterInsert != null) this.FAfterInsert(this);
    };
    this.DoAfterOpen = function () {
      if (this.FAfterOpen != null) this.FAfterOpen(this);
    };
    this.DoAfterPost = function () {
      if (this.FAfterPost != null) this.FAfterPost(this);
    };
    this.DoAfterScroll = function () {
      if (this.FAfterScroll != null) this.FAfterScroll(this);
    };
    this.DoBeforeCancel = function () {
      if (this.FBeforeCancel != null) this.FBeforeCancel(this);
    };
    this.DoBeforeClose = function () {
      if ((this.FBeforeClose != null) && !(pas.Classes.TComponentStateItem.csDestroying in this.FComponentState)) this.FBeforeClose(this);
    };
    this.DoBeforeDelete = function () {
      if (this.FBeforeDelete != null) this.FBeforeDelete(this);
    };
    this.DoBeforeEdit = function () {
      if (this.FBeforeEdit != null) this.FBeforeEdit(this);
    };
    this.DoBeforeInsert = function () {
      if (this.FBeforeInsert != null) this.FBeforeInsert(this);
    };
    this.DoBeforeOpen = function () {
      if (this.FBeforeOpen != null) this.FBeforeOpen(this);
    };
    this.DoBeforePost = function () {
      if (this.FBeforePost != null) this.FBeforePost(this);
    };
    this.DoBeforeScroll = function () {
      if (this.FBeforeScroll != null) this.FBeforeScroll(this);
    };
    this.DoOnCalcFields = function () {
      if (this.FOnCalcFields != null) this.FOnCalcFields(this);
    };
    this.DoOnNewRecord = function () {
      if (this.FOnNewRecord != null) this.FOnNewRecord(this);
    };
    this.DoBeforeLoad = function () {
      if (this.FBeforeLoad != null) this.FBeforeLoad(this);
    };
    this.DoAfterLoad = function () {
      if (this.FAfterLoad != null) this.FAfterLoad(this);
    };
    this.GetCalcFields = function (Buffer) {
      if ((this.FCalcFieldsSize > 0) || this.FInternalCalcFields) this.CalculateFields(Buffer);
    };
    this.GetCanModify = function () {
      var Result = false;
      Result = !this.FIsUniDirectional;
      return Result;
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      Result = $mod.DefaultFieldClasses[FieldType];
      return Result;
    };
    this.GetfieldCount = function () {
      var Result = 0;
      Result = this.FFieldList.GetCount();
      return Result;
    };
    this.GetNextRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetNextRecord()) Result += 1;
      return Result;
    };
    this.GetNextRecord = function () {
      var Result = false;
      var T = new $mod.TDataRecord();
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FRecordCount - 1);
      Result = this.GetRecord({a: this.FBufferCount, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},$mod.TGetMode.gmNext,true) === $mod.TGetResult.grOK;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        if (this.FRecordCount === this.FBufferCount) {
          this.ShiftBuffersBackward()}
         else {
          this.FRecordCount += 1;
          this.FCurrentRecord = this.FRecordCount - 1;
          T = new $mod.TDataRecord(this.FBuffers[this.FCurrentRecord]);
          this.FBuffers[this.FCurrentRecord] = new $mod.TDataRecord(this.FBuffers[this.FBufferCount]);
          this.FBuffers[this.FBufferCount] = new $mod.TDataRecord(T);
        };
      } else this.CursorPosChanged();
      return Result;
    };
    this.GetPriorRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetPriorRecord()) Result += 1;
      return Result;
    };
    this.GetPriorRecord = function () {
      var Result = false;
      this.CheckBiDirectional();
      if (this.FRecordCount > 0) this.SetCurrentRecord(0);
      Result = this.GetRecord({a: this.FBufferCount, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},$mod.TGetMode.gmPrior,true) === $mod.TGetResult.grOK;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        this.ShiftBuffersForward();
        if (this.FRecordCount < this.FBufferCount) this.FRecordCount += 1;
      } else this.CursorPosChanged();
      return Result;
    };
    this.InitRecord = function (Buffer) {
      this.InternalInitRecord(Buffer);
      this.ClearCalcFields(Buffer);
    };
    this.InternalCancel = function () {
    };
    this.InternalEdit = function () {
    };
    this.InternalInsert = function () {
    };
    this.OpenCursor = function (InfoQuery) {
      if (InfoQuery) {
        this.InternalInitFieldDefs()}
       else if (this.FState !== $mod.TDataSetState.dsOpening) this.DoInternalOpen();
    };
    this.OpenCursorcomplete = function () {
      try {
        if (this.FState === $mod.TDataSetState.dsOpening) this.DoInternalOpen();
      } finally {
        if (this.FInternalOpenComplete) {
          this.SetState($mod.TDataSetState.dsBrowse);
          this.DoAfterOpen();
          if (!this.IsEmpty()) this.DoAfterScroll();
        } else {
          this.SetState($mod.TDataSetState.dsInactive);
          this.CloseCursor();
        };
      };
    };
    this.RefreshInternalCalcFields = function (Buffer) {
    };
    this.SetActive = function (Value) {
      if (Value && (this.FState === $mod.TDataSetState.dsInactive)) {
        if (pas.Classes.TComponentStateItem.csLoading in this.FComponentState) {
          this.FOpenAfterRead = true;
          return;
        } else {
          this.DoBeforeOpen();
          this.FEnableControlsEvent = $mod.TDataEvent.deLayoutChange;
          this.FInternalCalcFields = false;
          try {
            this.FDefaultFields = this.GetfieldCount() === 0;
            this.OpenCursor(false);
          } finally {
            if (this.FState !== $mod.TDataSetState.dsOpening) this.OpenCursorcomplete();
          };
        };
        this.FModified = false;
      } else if (!Value && (this.FState !== $mod.TDataSetState.dsInactive)) {
        this.DoBeforeClose();
        this.SetState($mod.TDataSetState.dsInactive);
        this.FDataRequestID = 0;
        this.DoneChangeList();
        this.CloseCursor();
        this.DoAfterClose();
        this.FModified = false;
      };
    };
    this.SetBufListSize = function (Value) {
      var I = 0;
      if (Value < 0) Value = 0;
      if (Value === this.FBufferCount) return;
      if (Value > this.GetBufferCount()) {
        for (var $l1 = this.FBufferCount, $end2 = Value; $l1 <= $end2; $l1++) {
          I = $l1;
          this.FBuffers[I] = new $mod.TDataRecord(this.AllocRecordBuffer());
        };
      } else if (Value < this.GetBufferCount()) if ((Value >= 0) && (this.FActiveRecord > (Value - 1))) {
        for (var $l3 = 0, $end4 = this.FActiveRecord - Value; $l3 <= $end4; $l3++) {
          I = $l3;
          this.ShiftBuffersBackward();
        };
        this.FActiveRecord = Value - 1;
      };
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,Value + 1);
      this.FBufferCount = Value;
      if (this.FRecordCount > this.FBufferCount) this.FRecordCount = this.FBufferCount;
    };
    this.SetCurrentRecord = function (Index) {
      if (this.FCurrentRecord !== Index) {
        if (!this.FIsUniDirectional) {
          var $tmp1 = this.GetBookmarkFlag(new $mod.TDataRecord(this.FBuffers[Index]));
          if ($tmp1 === $mod.TBookmarkFlag.bfCurrent) {
            this.InternalSetToRecord(new $mod.TDataRecord(this.FBuffers[Index]))}
           else if ($tmp1 === $mod.TBookmarkFlag.bfBOF) {
            this.InternalFirst()}
           else if ($tmp1 === $mod.TBookmarkFlag.bfEOF) this.InternalLast();
        };
        this.FCurrentRecord = Index;
      };
    };
    this.SetModified = function (Value) {
      this.FModified = Value;
    };
    this.SetName = function (NewName) {
      var Self = this;
      function CheckName(FieldName) {
        var Result = "";
        var i = 0;
        var j = 0;
        Result = FieldName;
        i = 0;
        j = 0;
        while (i < Self.FFieldList.GetCount()) {
          if (Result === Self.FFieldList.GetField(i).FFieldName) {
            j += 1;
            Result = FieldName + pas.SysUtils.IntToStr(j);
          } else i += 1;
        };
        return Result;
      };
      var i = 0;
      var nm = "";
      var old = "";
      if (Self.FName === NewName) return;
      old = Self.FName;
      pas.Classes.TComponent.SetName.call(Self,NewName);
      if (pas.Classes.TComponentStateItem.csDesigning in this.FComponentState) for (var $l1 = 0, $end2 = Self.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        nm = old + Self.FFieldList.GetField(i).FFieldName;
        if (pas.System.Copy(Self.FFieldList.GetField(i).FName,1,nm.length) === nm) Self.FFieldList.GetField(i).SetName(CheckName(NewName + Self.FFieldList.GetField(i).FFieldName));
      };
    };
    this.SetState = function (Value) {
      if (Value !== this.FState) {
        this.FState = Value;
        if (Value === $mod.TDataSetState.dsBrowse) this.FModified = false;
        this.DataEvent($mod.TDataEvent.deUpdateState,0);
      };
    };
    this.AllocRecordBuffer = function () {
      var Result = new $mod.TDataRecord();
      Result.data = null;
      Result.state = $mod.TRecordState.rsNew;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
    };
    this.GetBookmarkData = function (Buffer, Data) {
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = $mod.TBookmarkFlag.bfCurrent;
      return Result;
    };
    this.InternalDelete = function () {
    };
    this.InternalFirst = function () {
    };
    this.InternalHandleException = function (E) {
      pas.SysUtils.ShowException(E,null);
    };
    this.InternalInitRecord = function (Buffer) {
    };
    this.InternalLast = function () {
    };
    this.InternalPost = function () {
      var Self = this;
      function CheckRequiredFields() {
        var I = 0;
        for (var $l1 = 0, $end2 = Self.FFieldList.GetCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          var $with3 = Self.FFieldList.GetField(I);
          if (((($with3.FRequired && !$with3.FReadOnly) && ($with3.FFieldKind === $mod.TFieldKind.fkData)) && !($with3.FDataType === $mod.TFieldType.ftAutoInc)) && $with3.GetIsNull()) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNeedField"),[$with3.GetDisplayName()],Self);
        };
      };
      CheckRequiredFields();
    };
    this.InternalSetToRecord = function (Buffer) {
    };
    this.SetBookmarkFlag = function (Buffer, Value) {
    };
    this.SetBookmarkData = function (Buffer, Data) {
    };
    this.Notification = function (AComponent, Operation) {
      pas.Classes.TComponent.Notification.call(this,AComponent,Operation);
      if ((Operation === pas.Classes.TOperation.opRemove) && (AComponent === this.FDataProxy)) this.FDataProxy = null;
    };
    this.GetFieldData = function (Field) {
      var Result = undefined;
      Result = this.GetFieldData$1(Field,new $mod.TDataRecord(this.ActiveBuffer()));
      return Result;
    };
    this.SetFieldData = function (Field, AValue) {
      this.SetFieldData$1(Field,{a: this.FActiveRecord, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},AValue);
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      Result = rtl.getObject(Buffer.data)[Field.FFieldName];
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      rtl.getObject(Buffer.get().data)[Field.FFieldName] = AValue;
    };
    this.FieldDefsClass = function () {
      var Result = null;
      Result = $mod.TFieldDefs;
      return Result;
    };
    this.FieldsClass = function () {
      var Result = null;
      Result = $mod.TFields;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FFieldDefs = this.$class.FieldDefsClass().$create("Create$4",[this]);
      this.FFieldList = this.$class.FieldsClass().$create("Create$1",[this]);
      this.FDataSources = pas.Classes.TFPList.$create("Create");
      this.FConstraints = $mod.TCheckConstraints.$create("Create$2",[this]);
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,1);
      this.FActiveRecord = 0;
      this.FEOF = true;
      this.FBOF = true;
      this.FIsUniDirectional = false;
      this.FAutoCalcFields = true;
      this.FDataRequestID = 0;
    };
    this.Destroy = function () {
      var i = 0;
      this.SetActive(false);
      rtl.free(this,"FFieldDefs");
      rtl.free(this,"FFieldList");
      var $with1 = this.FDataSources;
      while ($with1.FCount > 0) rtl.getObject($with1.Get($with1.FCount - 1)).SetDataSet(null);
      $with1.$destroy("Destroy");
      for (var $l2 = 0, $end3 = this.FBufferCount; $l2 <= $end3; $l2++) {
        i = $l2;
        this.FreeRecordBuffer({a: i, p: this.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
      };
      rtl.free(this,"FConstraints");
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$mod.TDataRecord,1);
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.ActiveBuffer = function () {
      var Result = new $mod.TDataRecord();
      Result = new $mod.TDataRecord(this.FBuffers[this.FActiveRecord]);
      return Result;
    };
    this.Append = function () {
      this.DoInsertAppend(true);
    };
    this.ConvertToDateTime = function (aValue, ARaiseException) {
      var Result = 0.0;
      Result = this.$class.DefaultConvertToDateTime(aValue,ARaiseException);
      return Result;
    };
    this.ConvertDateTimeToNative = function (aValue) {
      var Result = undefined;
      Result = this.$class.DefaultConvertDateTimeToNative(aValue);
      return Result;
    };
    this.DefaultConvertToDateTime = function (aValue, ARaiseException) {
      var Result = 0.0;
      Result = 0;
      if (rtl.isString(aValue)) {
        if (!pas.DateUtils.TryRFC3339ToDateTime("" + aValue,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})) throw pas.SysUtils.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.DBConst,"SErrInvalidDateTime"),["" + aValue]]);
      } else if (rtl.isNumber(aValue)) Result = rtl.getNumber(aValue);
      return Result;
    };
    this.DefaultConvertDateTimeToNative = function (aValue) {
      var Result = undefined;
      Result = pas.DateUtils.DateTimeToRFC3339(aValue);
      return Result;
    };
    this.BlobDataToBytes = function (aValue) {
      var Result = [];
      Result = this.$class.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.DefaultBlobDataToBytes = function (aValue) {
      var Result = [];
      var S = "";
      var I = 0;
      var J = 0;
      var L = 0;
      Result = rtl.arraySetLength(Result,0,0);
      if (rtl.isString(aValue)) {
        S = "" + aValue;
        L = S.length;
        Result = rtl.arraySetLength(Result,0,Math.floor((L + 1) / 2));
        I = 1;
        J = 0;
        while (I < L) {
          Result[J] = pas.SysUtils.StrToInt("$" + pas.System.Copy(S,I,2));
          I += 2;
          J += 1;
        };
      };
      return Result;
    };
    this.Cancel = function () {
      if (this.FState in rtl.createSet($mod.TDataSetState.dsEdit,$mod.TDataSetState.dsInsert)) {
        this.DataEvent($mod.TDataEvent.deCheckBrowseMode,0);
        this.DoBeforeCancel();
        this.UpdateCursorPos();
        this.InternalCancel();
        if ((this.FState === $mod.TDataSetState.dsInsert) && (this.FRecordCount === 1)) {
          this.FEOF = true;
          this.FBOF = true;
          this.FRecordCount = 0;
          this.InitRecord({a: this.FActiveRecord, p: this.FBuffers, get: function () {
              return this.p[this.a];
            }, set: function (v) {
              this.p[this.a] = v;
            }});
          this.SetState($mod.TDataSetState.dsBrowse);
          this.DataEvent($mod.TDataEvent.deDataSetChange,0);
        } else {
          this.SetState($mod.TDataSetState.dsBrowse);
          this.SetCurrentRecord(this.FActiveRecord);
          this.Resync({});
        };
        this.DoAfterCancel();
      };
    };
    this.CheckBrowseMode = function () {
      this.CheckActive();
      this.DataEvent($mod.TDataEvent.deCheckBrowseMode,0);
      var $tmp1 = this.FState;
      if (($tmp1 === $mod.TDataSetState.dsEdit) || ($tmp1 === $mod.TDataSetState.dsInsert)) {
        this.UpdateRecord();
        if (this.FModified) {
          this.Post()}
         else this.Cancel();
      } else if ($tmp1 === $mod.TDataSetState.dsSetKey) this.Post();
    };
    this.ControlsDisabled = function () {
      var Result = false;
      Result = this.FDisableControlsCount > 0;
      return Result;
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.CursorPosChanged = function () {
      this.FCurrentRecord = -1;
    };
    this.Delete = function () {
      var R = null;
      if (!this.GetCanModify()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SDatasetReadOnly"),this);
      if (this.IsEmpty()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SDatasetEmpty"),this);
      if (this.FState in rtl.createSet($mod.TDataSetState.dsInsert)) {
        this.Cancel();
      } else {
        this.DataEvent($mod.TDataEvent.deCheckBrowseMode,0);
        this.DoBeforeDelete();
        this.DoBeforeScroll();
        R = this.AddToChangeList($mod.TUpdateStatus.usDeleted);
        if (!this.TryDoing(rtl.createCallback(this,"InternalDelete"),this.FOnDeleteError)) {
          if (R != null) this.RemoveFromChangeList(R);
          return;
        };
        this.SetState($mod.TDataSetState.dsBrowse);
        this.SetCurrentRecord(this.FActiveRecord);
        this.Resync({});
        this.DoAfterDelete();
        this.DoAfterScroll();
      };
    };
    this.DisableControls = function () {
      if (this.FDisableControlsCount === 0) {
        this.FDisableControlsState = this.FState;
        this.FEnableControlsEvent = $mod.TDataEvent.deDataSetChange;
      };
      this.FDisableControlsCount += 1;
    };
    this.Edit = function () {
      if (this.FState in rtl.createSet($mod.TDataSetState.dsEdit,$mod.TDataSetState.dsInsert)) return;
      this.CheckBrowseMode();
      if (!this.GetCanModify()) $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SDatasetReadOnly"),this);
      if (this.FRecordCount === 0) {
        this.Append();
        return;
      };
      this.DoBeforeEdit();
      if (!this.TryDoing(rtl.createCallback(this,"InternalEdit"),this.FOnEditError)) return;
      this.GetCalcFields({a: this.FActiveRecord, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }});
      this.SetState($mod.TDataSetState.dsEdit);
      this.DataEvent($mod.TDataEvent.deRecordChange,0);
      this.DoAfterEdit();
    };
    this.EnableControls = function () {
      if (this.FDisableControlsCount > 0) this.FDisableControlsCount -= 1;
      if (this.FDisableControlsCount === 0) {
        if (this.FState !== this.FDisableControlsState) this.DataEvent($mod.TDataEvent.deUpdateState,0);
        if ((this.FState !== $mod.TDataSetState.dsInactive) && (this.FDisableControlsState !== $mod.TDataSetState.dsInactive)) this.DataEvent(this.FEnableControlsEvent,0);
      };
    };
    this.FieldByName = function (FieldName) {
      var Result = null;
      Result = this.FindField(FieldName);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SFieldNotFound"),[FieldName],this);
      return Result;
    };
    this.FindField = function (FieldName) {
      var Result = null;
      Result = this.FFieldList.FindField(FieldName);
      return Result;
    };
    this.First = function () {
      this.CheckBrowseMode();
      this.DoBeforeScroll();
      if (!this.FIsUniDirectional) {
        this.ClearBuffers()}
       else if (!this.FBOF) {
        this.SetActive(false);
        this.SetActive(true);
      };
      try {
        this.InternalFirst();
        if (!this.FIsUniDirectional) this.GetNextRecords();
      } finally {
        this.FBOF = true;
        this.DataEvent($mod.TDataEvent.deDataSetChange,0);
        this.DoAfterScroll();
      };
    };
    this.FreeBookmark = function (ABookmark) {
    };
    this.GetBookmark = function () {
      var Result = new $mod.TBookmark();
      if (this.BookmarkAvailable()) {
        this.GetBookmarkData(new $mod.TDataRecord(this.ActiveBuffer()),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})}
       else Result.Data = null;
      return Result;
    };
    this.Insert$1 = function () {
      this.DoInsertAppend(false);
    };
    this.IsEmpty = function () {
      var Result = false;
      Result = (this.FBOF && this.FEOF) && !(this.FState === $mod.TDataSetState.dsInsert);
      return Result;
    };
    this.Last = function () {
      this.CheckBiDirectional();
      this.CheckBrowseMode();
      this.DoBeforeScroll();
      this.ClearBuffers();
      try {
        this.InternalLast();
        this.GetPriorRecords();
        if (this.FRecordCount > 0) this.FActiveRecord = this.FRecordCount - 1;
      } finally {
        this.FEOF = true;
        this.DataEvent($mod.TDataEvent.deDataSetChange,0);
        this.DoAfterScroll();
      };
    };
    this.MoveBy = function (Distance) {
      var Self = this;
      var Result = 0;
      var TheResult = 0;
      function ScrollForward() {
        var Result = 0;
        Result = 0;
        Self.FBOF = false;
        while ((Distance > 0) && !Self.FEOF) {
          if (Self.FActiveRecord < (Self.FRecordCount - 1)) {
            Self.FActiveRecord += 1;
            Distance -= 1;
            TheResult += 1;
          } else {
            if (Self.GetNextRecord()) {
              Distance -= 1;
              Result -= 1;
              TheResult += 1;
            } else {
              Self.FEOF = true;
              Self.DoLoad(rtl.createSet($mod.TLoadOption.loNoOpen,$mod.TLoadOption.loAtEOF),null);
            };
          };
        };
        return Result;
      };
      function ScrollBackward() {
        var Result = 0;
        Self.CheckBiDirectional();
        Result = 0;
        Self.FEOF = false;
        while ((Distance < 0) && !Self.FBOF) {
          if (Self.FActiveRecord > 0) {
            Self.FActiveRecord -= 1;
            Distance += 1;
            TheResult -= 1;
          } else {
            if (Self.GetPriorRecord()) {
              Distance += 1;
              Result += 1;
              TheResult -= 1;
            } else Self.FBOF = true;
          };
        };
        return Result;
      };
      var Scrolled = 0;
      Self.CheckBrowseMode();
      Result = 0;
      TheResult = 0;
      Self.DoBeforeScroll();
      if (((Distance === 0) || ((Distance > 0) && Self.FEOF)) || ((Distance < 0) && Self.FBOF)) return Result;
      try {
        Scrolled = 0;
        if (Distance > 0) {
          Scrolled = ScrollForward()}
         else Scrolled = ScrollBackward();
      } finally {
        Self.DataEvent($mod.TDataEvent.deDataSetScroll,Scrolled);
        Self.DoAfterScroll();
        Result = TheResult;
      };
      return Result;
    };
    this.Next = function () {
      if (this.FBlockReadSize > 0) {
        this.BlockReadNext()}
       else this.MoveBy(1);
    };
    this.Open = function () {
      this.SetActive(true);
    };
    var UpdateStates = [$mod.TUpdateStatus.usModified,$mod.TUpdateStatus.usInserted];
    this.Post = function () {
      var R = null;
      var WasInsert = false;
      this.UpdateRecord();
      if (this.FState in rtl.createSet($mod.TDataSetState.dsEdit,$mod.TDataSetState.dsInsert)) {
        this.DataEvent($mod.TDataEvent.deCheckBrowseMode,0);
        this.DoBeforePost();
        WasInsert = this.FState === $mod.TDataSetState.dsInsert;
        if (!this.TryDoing(rtl.createCallback(this,"InternalPost"),this.FOnPostError)) return;
        this.CursorPosChanged();
        this.SetState($mod.TDataSetState.dsBrowse);
        this.Resync({});
        R = this.AddToChangeList(UpdateStates[+WasInsert]);
        if (R != null) R.FBookmark = new $mod.TBookmark(this.GetBookmark());
        this.DoAfterPost();
      } else if (this.FState !== $mod.TDataSetState.dsSetKey) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNotEditing"),[this.FName],this);
    };
    this.Prior = function () {
      this.MoveBy(-1);
    };
    this.Resync = function (Mode) {
      var i = 0;
      var count = 0;
      if (this.FIsUniDirectional) return;
      if (this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},$mod.TGetMode.gmCurrent,false) !== $mod.TGetResult.grOK) if ($mod.TResyncMode$a.rmExact in Mode) {
        $mod.DatabaseError$1(rtl.getResStr(pas.DBConst,"SNoSuchRecord"),this)}
       else if ((this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},$mod.TGetMode.gmNext,true) !== $mod.TGetResult.grOK) && (this.GetRecord({a: 0, p: this.FBuffers, get: function () {
          return this.p[this.a];
        }, set: function (v) {
          this.p[this.a] = v;
        }},$mod.TGetMode.gmPrior,true) !== $mod.TGetResult.grOK)) {
        this.ClearBuffers();
        this.InternalInitRecord({a: this.FActiveRecord, p: this.FBuffers, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
        this.DataEvent($mod.TDataEvent.deDataSetChange,0);
        return;
      };
      this.FCurrentRecord = 0;
      this.FEOF = false;
      this.FBOF = false;
      if ($mod.TResyncMode$a.rmCenter in Mode) {
        count = Math.floor(this.FRecordCount / 2)}
       else count = this.FActiveRecord;
      i = 0;
      this.FRecordCount = 1;
      this.FActiveRecord = 0;
      while ((i < count) && this.GetPriorRecord()) i += 1;
      this.FActiveRecord = i;
      this.GetNextRecords();
      if (this.FRecordCount < this.FBufferCount) this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
      this.DataEvent($mod.TDataEvent.deDataSetChange,0);
    };
    this.UpdateCursorPos = function () {
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FActiveRecord);
    };
    this.UpdateRecord = function () {
      if (!(this.FState in $mod.dsEditModes)) $mod.DatabaseErrorFmt$1(rtl.getResStr(pas.DBConst,"SNotEditing"),[this.FName],this);
      this.DataEvent($mod.TDataEvent.deUpdateRecord,0);
    };
  });
  rtl.createClass($mod,"TDataLink",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FFirstRecord = 0;
      this.FBufferCount = 0;
      this.FActive = false;
      this.FDataSourceFixed = false;
      this.FEditing = false;
      this.FReadOnly = false;
      this.FUpdatingRecord = false;
      this.FVisualControl = false;
      this.FDataSource = null;
    };
    this.$final = function () {
      this.FDataSource = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.CalcFirstRecord = function (Index) {
      var Result = 0;
      if (this.FDataSource.FDataSet.FActiveRecord > (((this.FFirstRecord + Index) + this.FBufferCount) - 1)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - (((this.FFirstRecord + Index) + this.FBufferCount) - 1)}
       else if (this.FDataSource.FDataSet.FActiveRecord < (this.FFirstRecord + Index)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - (this.FFirstRecord + Index)}
       else Result = 0;
      this.FFirstRecord += Index + Result;
      return Result;
    };
    this.CalcRange = function () {
      var aMax = 0;
      var aMin = 0;
      aMin = (this.GetDataset().FActiveRecord - this.FBufferCount) + 1;
      if (aMin < 0) aMin = 0;
      aMax = this.GetDataset().FBufferCount - this.FBufferCount;
      if (aMax < 0) aMax = 0;
      if (aMax > this.GetDataset().FActiveRecord) aMax = this.GetDataset().FActiveRecord;
      if (this.FFirstRecord < aMin) this.FFirstRecord = aMin;
      if (this.FFirstRecord > aMax) this.FFirstRecord = aMax;
      if ((this.FFirstRecord !== 0) && ((this.GetDataset().FActiveRecord - this.FFirstRecord) < (this.FBufferCount - 1))) this.FFirstRecord -= 1;
    };
    this.CheckActiveAndEditing = function () {
      var B = false;
      B = (this.FDataSource != null) && !(this.FDataSource.FState in rtl.createSet($mod.TDataSetState.dsInactive,$mod.TDataSetState.dsOpening));
      if (B !== this.FActive) {
        this.FActive = B;
        this.ActiveChanged();
      };
      B = ((this.FDataSource != null) && (this.FDataSource.FState in $mod.dsEditModes)) && !this.FReadOnly;
      if (B !== this.FEditing) {
        this.FEditing = B;
        this.EditingChanged();
      };
    };
    this.GetDataset = function () {
      var Result = null;
      if (this.FDataSource != null) {
        Result = this.FDataSource.FDataSet}
       else Result = null;
      return Result;
    };
    this.SetActive = function (AActive) {
      if (this.FActive !== AActive) {
        this.FActive = AActive;
        this.ActiveChanged();
      };
    };
    this.SetDataSource = function (Value) {
      if (this.FDataSource === Value) return;
      if (!this.FDataSourceFixed) {
        if (this.FDataSource != null) {
          this.FDataSource.UnregisterDataLink(this);
          this.FDataSource = null;
          this.CheckActiveAndEditing();
        };
        this.FDataSource = Value;
        if (this.FDataSource != null) {
          this.FDataSource.RegisterDataLink(this);
          this.CheckActiveAndEditing();
        };
      };
    };
    this.ActiveChanged = function () {
      this.FFirstRecord = 0;
    };
    this.CheckBrowseMode = function () {
    };
    this.DataEvent = function (Event, Info) {
      var $tmp1 = Event;
      if (($tmp1 === $mod.TDataEvent.deFieldChange) || ($tmp1 === $mod.TDataEvent.deRecordChange)) {
        if (!this.FUpdatingRecord) this.RecordChanged(rtl.getObject(Info))}
       else if ($tmp1 === $mod.TDataEvent.deDataSetChange) {
        this.SetActive(this.FDataSource.FDataSet.GetActive());
        this.CalcRange();
        this.CalcFirstRecord(Math.floor(Info));
        this.DataSetChanged();
      } else if ($tmp1 === $mod.TDataEvent.deDataSetScroll) {
        this.DataSetScrolled(this.CalcFirstRecord(Math.floor(Info)))}
       else if ($tmp1 === $mod.TDataEvent.deLayoutChange) {
        this.CalcFirstRecord(Math.floor(Info));
        this.LayoutChanged();
      } else if ($tmp1 === $mod.TDataEvent.deUpdateRecord) {
        this.UpdateRecord()}
       else if ($tmp1 === $mod.TDataEvent.deUpdateState) {
        this.CheckActiveAndEditing()}
       else if ($tmp1 === $mod.TDataEvent.deCheckBrowseMode) {
        this.CheckBrowseMode()}
       else if ($tmp1 === $mod.TDataEvent.deFocusControl) this.FocusControl(Info);
    };
    this.DataSetChanged = function () {
      this.RecordChanged(null);
    };
    this.DataSetScrolled = function (Distance) {
      this.DataSetChanged();
    };
    this.EditingChanged = function () {
    };
    this.FocusControl = function (Field) {
    };
    this.GetBufferCount = function () {
      var Result = 0;
      Result = this.FBufferCount;
      return Result;
    };
    this.LayoutChanged = function () {
      this.DataSetChanged();
    };
    this.RecordChanged = function (Field) {
    };
    this.UpdateData = function () {
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FBufferCount = 1;
      this.FFirstRecord = 0;
      this.FDataSource = null;
      this.FDataSourceFixed = false;
    };
    this.Destroy = function () {
      this.FActive = false;
      this.FEditing = false;
      this.FDataSourceFixed = false;
      this.SetDataSource(null);
      pas.System.TObject.Destroy.call(this);
    };
    this.Edit = function () {
      var Result = false;
      if (!this.FReadOnly) this.FDataSource.Edit();
      Result = this.FEditing;
      return Result;
    };
    this.UpdateRecord = function () {
      this.FUpdatingRecord = true;
      try {
        this.UpdateData();
      } finally {
        this.FUpdatingRecord = false;
      };
    };
  });
  $mod.$rtti.$MethodVar("TDataChangeEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Field",$mod.$rtti["TField"]]]), methodkind: 0});
  rtl.createClass($mod,"TDataSource",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FDataSet = null;
      this.FDataLinks = null;
      this.FEnabled = false;
      this.FAutoEdit = false;
      this.FState = 0;
      this.FOnStateChange = null;
      this.FOnDataChange = null;
      this.FOnUpdateData = null;
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FDataLinks = undefined;
      this.FOnStateChange = undefined;
      this.FOnDataChange = undefined;
      this.FOnUpdateData = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.DistributeEvent = function (Event, Info) {
      var i = 0;
      var $with1 = this.FDataLinks;
      for (var $l2 = 0, $end3 = $with1.GetCount() - 1; $l2 <= $end3; $l2++) {
        i = $l2;
        var $with4 = rtl.getObject($with1.Get(i));
        if (!$with4.FVisualControl) $with4.DataEvent(Event,Info);
      };
      for (var $l5 = 0, $end6 = $with1.GetCount() - 1; $l5 <= $end6; $l5++) {
        i = $l5;
        var $with7 = rtl.getObject($with1.Get(i));
        if ($with7.FVisualControl) $with7.DataEvent(Event,Info);
      };
    };
    this.RegisterDataLink = function (DataLink) {
      this.FDataLinks.Add(DataLink);
      if (this.FDataSet != null) this.FDataSet.RecalcBufListSize();
    };
    var OnDataChangeEvents = rtl.createSet($mod.TDataEvent.deRecordChange,$mod.TDataEvent.deDataSetChange,$mod.TDataEvent.deDataSetScroll,$mod.TDataEvent.deLayoutChange,$mod.TDataEvent.deUpdateState);
    this.ProcessEvent = function (Event, Info) {
      var NeedDataChange = false;
      var FLastState = 0;
      if (Event === $mod.TDataEvent.deUpdateState) {
        NeedDataChange = this.FState === $mod.TDataSetState.dsInactive;
        FLastState = this.FState;
        if (this.FDataSet != null) {
          this.FState = this.FDataSet.FState}
         else this.FState = $mod.TDataSetState.dsInactive;
        if (this.FState === FLastState) return;
      } else NeedDataChange = true;
      this.DistributeEvent(Event,Info);
      if (!(pas.Classes.TComponentStateItem.csDestroying in this.FComponentState)) {
        if (Event === $mod.TDataEvent.deUpdateState) this.DoStateChange();
        if ((Event in OnDataChangeEvents) && NeedDataChange) this.DoDataChange(null);
        if (Event === $mod.TDataEvent.deFieldChange) this.DoDataChange(Info);
        if (Event === $mod.TDataEvent.deUpdateRecord) this.DoUpdateData();
      };
    };
    this.SetDataSet = function (ADataSet) {
      if (this.FDataSet !== null) {
        this.FDataSet.UnRegisterDataSource(this);
        this.FDataSet = null;
        this.ProcessEvent($mod.TDataEvent.deUpdateState,0);
      };
      if (ADataSet !== null) {
        ADataSet.RegisterDataSource(this);
        this.FDataSet = ADataSet;
        this.ProcessEvent($mod.TDataEvent.deUpdateState,0);
      };
    };
    this.SetEnabled = function (Value) {
      this.FEnabled = Value;
    };
    this.UnregisterDataLink = function (DataLink) {
      this.FDataLinks.Remove(DataLink);
      if (this.FDataSet !== null) this.FDataSet.RecalcBufListSize();
    };
    this.DoDataChange = function (Info) {
      if (this.FOnDataChange != null) this.FOnDataChange(this,Info);
    };
    this.DoStateChange = function () {
      if (this.FOnStateChange != null) this.FOnStateChange(this);
    };
    this.DoUpdateData = function () {
      if (this.FOnUpdateData != null) this.FOnUpdateData(this);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FDataLinks = pas.Classes.TList.$create("Create$1");
      this.FEnabled = true;
      this.FAutoEdit = true;
    };
    this.Destroy = function () {
      this.FOnStateChange = null;
      this.SetDataSet(null);
      var $with1 = this.FDataLinks;
      while ($with1.GetCount() > 0) rtl.getObject($with1.Get($with1.GetCount() - 1)).SetDataSource(null);
      rtl.free(this,"FDataLinks");
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.Edit = function () {
      if ((this.FState === $mod.TDataSetState.dsBrowse) && this.FAutoEdit) this.FDataSet.Edit();
    };
    var $r = this.$rtti;
    $r.addProperty("AutoEdit",0,rtl.boolean,"FAutoEdit","FAutoEdit",{Default: true});
    $r.addProperty("DataSet",2,$mod.$rtti["TDataSet"],"FDataSet","SetDataSet");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("OnStateChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStateChange","FOnStateChange");
    $r.addProperty("OnDataChange",0,$mod.$rtti["TDataChangeEvent"],"FOnDataChange","FOnDataChange");
    $r.addProperty("OnUpdateData",0,pas.Classes.$rtti["TNotifyEvent"],"FOnUpdateData","FOnUpdateData");
  });
  this.TDataRequestResult = {"0": "rrFail", rrFail: 0, "1": "rrEOF", rrEOF: 1, "2": "rrOK", rrOK: 2};
  rtl.createClass($mod,"TDataRequest",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FBookmark = new $mod.TBookmark();
      this.FDataset = null;
      this.FErrorMsg = "";
      this.FEvent = null;
      this.FLoadOptions = {};
      this.FRequestID = 0;
      this.FSuccess = 0;
      this.FData = undefined;
      this.FAfterRequest = null;
      this.FDataProxy = null;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FEvent = undefined;
      this.FLoadOptions = undefined;
      this.FAfterRequest = undefined;
      this.FDataProxy = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (aDataProxy, aOptions, aAfterRequest, aAfterLoad) {
      this.FDataProxy = aDataProxy;
      this.FLoadOptions = rtl.refSet(aOptions);
      this.FEvent = aAfterLoad;
      this.FAfterRequest = aAfterRequest;
    };
  });
  rtl.createClass($mod,"TRecordUpdateDescriptor",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FBookmark = new $mod.TBookmark();
      this.FData = undefined;
      this.FDataset = null;
      this.FProxy = null;
      this.FStatus = 0;
      this.FOriginalStatus = 0;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FProxy = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (aProxy, aDataset, aBookmark, AData, AStatus) {
      this.FDataset = aDataset;
      this.FBookmark = new $mod.TBookmark(aBookmark);
      this.FData = AData;
      this.FStatus = AStatus;
      this.FOriginalStatus = AStatus;
      this.FProxy = aProxy;
    };
  });
  rtl.createClass($mod,"TDataProxy",pas.Classes.TComponent,function () {
    this.GetDataRequestClass = function () {
      var Result = null;
      Result = $mod.TDataRequest;
      return Result;
    };
    this.GetUpdateDescriptorClass = function () {
      var Result = null;
      Result = $mod.TRecordUpdateDescriptor;
      return Result;
    };
    this.GetDataRequest = function (aOptions, aAfterRequest, aAfterLoad) {
      var Result = null;
      Result = this.GetDataRequestClass().$create("Create$1",[this,rtl.refSet(aOptions),aAfterRequest,aAfterLoad]);
      return Result;
    };
    this.GetUpdateDescriptor = function (aDataset, aBookmark, AData, AStatus) {
      var Result = null;
      Result = this.GetUpdateDescriptorClass().$create("Create$1",[this,aDataset,new $mod.TBookmark(aBookmark),AData,AStatus]);
      return Result;
    };
  });
  this.DefaultFieldClasses = [$mod.TField,$mod.TStringField,$mod.TIntegerField,$mod.TLargeintField,$mod.TBooleanField,$mod.TFloatField,$mod.TDateField,$mod.TTimeField,$mod.TDateTimeField,$mod.TAutoIncField,$mod.TBlobField,$mod.TMemoField,$mod.TStringField,$mod.TVariantField];
  this.dsEditModes = rtl.createSet($mod.TDataSetState.dsEdit,$mod.TDataSetState.dsInsert,$mod.TDataSetState.dsSetKey);
  this.ftBlobTypes = rtl.createSet($mod.TFieldType.ftBlob,$mod.TFieldType.ftMemo);
  this.DatabaseError$1 = function (Msg, Comp) {
    if ((Comp != null) && (Comp.FName !== "")) throw $mod.EDatabaseError.$create("CreateFmt",["%s : %s",[Comp.FName,Msg]]);
  };
  this.DatabaseErrorFmt = function (Fmt, Args) {
    throw $mod.EDatabaseError.$create("CreateFmt",[Fmt,Args]);
  };
  this.DatabaseErrorFmt$1 = function (Fmt, Args, Comp) {
    if (Comp != null) throw $mod.EDatabaseError.$create("CreateFmt",[pas.SysUtils.Format("%s : %s",[Comp.FName,Fmt]),Args]);
  };
  this.ExtractFieldName = function (Fields, Pos) {
    var Result = "";
    var i = 0;
    var FieldsLength = 0;
    i = Pos.get();
    FieldsLength = Fields.length;
    while ((i <= FieldsLength) && (Fields.charAt(i - 1) !== ";")) i += 1;
    Result = pas.SysUtils.Trim(pas.System.Copy(Fields,Pos.get(),i - Pos.get()));
    if ((i <= FieldsLength) && (Fields.charAt(i - 1) === ";")) i += 1;
    Pos.set(i);
    return Result;
  };
  $mod.$init = function () {
  };
},["DBConst"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.DefaultBufferCount = 10;
  $impl.SInteger = "Integer";
  $impl.SLargeInt = "NativeInt";
  $impl.SJSValue = "JSValue";
  $impl.SString = "String";
  $impl.SBytes = "Bytes";
});
rtl.module("WEBLib.ExtCtrls",["System","Classes","SysUtils","Types","Web","WEBLib.Controls","WEBLib.StdCtrls","WEBLib.Graphics"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TURLPicture",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FOnChange = null;
      this.FFilename = "";
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.LoadFromFile = function (AFileName) {
      this.FFilename = AFileName;
      if (this.FOnChange != null) this.FOnChange(this);
    };
    var $r = this.$rtti;
    $r.addProperty("OnChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass($mod,"TImageControl",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FAlign$1 = 0;
      this.FAlignWithMargins$1 = false;
      this.FAnchors$1 = {};
      this.FHint$1 = "";
      this.FShowHint$1 = false;
      this.FURL = "";
      this.FPicture = null;
      this.FAutoSize = false;
      this.FVisible$1 = false;
    };
    this.$final = function () {
      this.FAnchors$1 = undefined;
      this.FPicture = undefined;
      pas["WEBLib.Controls"].TCustomControl.$final.call(this);
    };
    this.SetURL = function (AURL) {
      this.FContainer.setAttribute("src",AURL);
    };
    this.HandleDoDrag = function (aEvent) {
      var Result = false;
      Result = true;
      return Result;
    };
    this.HandleDoDragEnd = function (aEvent) {
      var Result = false;
      Result = true;
      return Result;
    };
    this.HandleDoDragExit = function (aEvent) {
      var Result = false;
      Result = true;
      return Result;
    };
    this.HandleDoDragOver = function (aEvent) {
      var Result = false;
      Result = true;
      return Result;
    };
    this.HandleDoDragStart = function (aEvent) {
      var Result = false;
      var obj = null;
      aEvent.dataTransfer.effectAllowed = "copy";
      aEvent.dataTransfer.dropEffect = "copy";
      aEvent.dataTransfer.setData("text","Hello World");
      this.DoStartDrag(obj);
      Result = true;
      return Result;
    };
    this.HandleDoDrop = function (aEvent) {
      var Result = false;
      this.DragDrop(null,aEvent.clientX,aEvent.clientY);
      Result = true;
      return Result;
    };
    this.SetPicture = function (Value) {
      this.FPicture.Assign(Value);
    };
    this.PictureChanged = function (Sender) {
      this.SetURL(this.FPicture.FFilename);
    };
    this.BindEvents = function () {
      pas["WEBLib.Controls"].TCustomControl.BindEvents.apply(this,arguments);
      if (this.GetElementHandle() != null) {
        this.FContainer.setAttribute("draggable","true");
        this.FContainer.setAttribute("droppable","true");
        this.GetElementHandle().ondrag = rtl.createCallback(this,"HandleDoDrag");
        this.GetElementHandle().ondragend = rtl.createCallback(this,"HandleDoDragEnd");
        this.GetElementHandle().ondragexit = rtl.createCallback(this,"HandleDoDragExit");
        this.GetElementHandle().ondragover = rtl.createCallback(this,"HandleDoDragOver");
        this.GetElementHandle().ondragstart = rtl.createCallback(this,"HandleDoDragStart");
        this.GetElementHandle().ondrop = rtl.createCallback(this,"HandleDoDrop");
      };
    };
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("IMG");
      return Result;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FPicture = $mod.TURLPicture.$create("Create");
      this.FPicture.FOnChange = rtl.createCallback(this,"PictureChanged");
      this.SetColor(-1);
      this.SetTabStop(false);
    };
    this.Destroy = function () {
      rtl.free(this,"FPicture");
      pas["WEBLib.Controls"].TCustomControl.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign$1","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins$1","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors$1","SetAnchors");
    $r.addProperty("AutoSize",0,rtl.boolean,"FAutoSize","FAutoSize");
    $r.addProperty("Hint",2,rtl.string,"FHint$1","SetHint");
    $r.addProperty("Picture",2,$mod.$rtti["TURLPicture"],"FPicture","SetPicture");
    $r.addProperty("URL",2,rtl.string,"FURL","SetURL");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint$1","SetShowHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible$1","SetVisible");
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
    this.UpdateElement = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
      if ((this.GetElementHandle() != null) && !this.IsUpdating()) {
        if (this.FAutoSize) {
          this.GetElementHandle().style.setProperty("overflow","");
          this.GetElementHandle().style.setProperty("white-space","normal");
          this.GetElementHandle().style.setProperty("display","inline");
        } else {
          this.GetElementHandle().style.setProperty("overflow","hidden");
          this.GetElementHandle().style.setProperty("white-space","nowrap");
          this.GetElementHandle().style.setProperty("display","inline-block");
        };
        if (this.FBorderStyle === pas["WEBLib.Controls"].TBorderStyle.bsSingle) {
          this.GetElementHandle().style.setProperty("border-style","solid");
          this.GetElementHandle().style.setProperty("border-width","1px");
          this.GetElementHandle().style.setProperty("border-color","lightgray");
        } else {
          this.GetElementHandle().style.setProperty("border-style","none");
          this.GetElementHandle().style.setProperty("border-width","");
          this.GetElementHandle().style.setProperty("border-color","");
        };
        this.GetElementHandle().style.setProperty("background-color",pas["WEBLib.Graphics"].ColorToHTML(this.FColor));
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FAutoSize = false;
      this.SetColor(0xF0F0F0);
      this.SetTabStop(false);
    };
    var $r = this.$rtti;
    $r.addMethod("CreateElement",1,null,pas.Web.$rtti["TJSElement"]);
    $r.addMethod("SetBorderStyle",0,[["AValue",pas["WEBLib.Controls"].$rtti["TBorderStyle"]]]);
    $r.addMethod("SetAutoSize",0,[["AValue",rtl.boolean]]);
    $r.addMethod("UpdateElement",0,null);
    $r.addMethod("CreateInitialize",0,null);
  });
  rtl.createClass($mod,"TPanel",$mod.TCustomPanel,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Anchors",2,pas["WEBLib.Controls"].$rtti["TAnchorKindSet"],"FAnchors","SetAnchors");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize");
    $r.addProperty("BorderStyle",2,pas["WEBLib.Controls"].$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Caption",0,rtl.string,"FCaption","FCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
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
});
rtl.module("WEBLib.Buttons",["System","Classes","SysUtils","Types","Web","WEBLib.Controls","WEBLib.Graphics","WEBLib.StdCtrls","WEBLib.ExtCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TCustomSpeedButton",pas["WEBLib.Controls"].TCustomControl,function () {
    this.$init = function () {
      pas["WEBLib.Controls"].TCustomControl.$init.call(this);
      this.FCaption = "";
      this.FGlyph = "";
      this.FColor$1 = 0;
      this.FIntColor = 0;
      this.FDown = false;
      this.FFlat = false;
      this.FGroupIndex = 0;
      this.FAllowAllUp = false;
    };
    this.SetCaption = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.UpdateElement();
      };
    };
    this.SetGlyph = function (AValue) {
      if (this.FGlyph !== AValue) {
        this.FGlyph = AValue;
        this.UpdateElement();
      };
    };
    this.SetIntColor = function (AValue) {
      if (this.FIntColor !== AValue) {
        this.FIntColor = AValue;
        this.UpdateElement();
      };
    };
    this.SetGroupIndex = function (AValue) {
      this.FGroupIndex = AValue;
    };
    this.SetDown = function (AValue) {
      if (this.FDown !== AValue) {
        this.FDown = AValue;
        this.UpdateElement();
      };
    };
    this.SetFlat = function (Value) {
      if (this.FFlat !== Value) {
        this.FFlat = Value;
        this.RecreateElement();
      };
    };
    this.CreateElement = function () {
      var Result = null;
      if (this.FFlat) {
        Result = document.createElement("SPAN")}
       else Result = document.createElement("BUTTON");
      return Result;
    };
    this.UpdateElement = function () {
      var s = "";
      var inactive = "";
      var noptr = "";
      var clr = 0;
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
      if (this.GetElementHandle() != null) {
        if (this.FDown) {
          clr = 8421504}
         else clr = this.FIntColor;
        if (this.FFlat) {
          if (clr !== -1) {
            this.GetElementHandle().style.setProperty("background-color",pas["WEBLib.Graphics"].ColorToHTML(clr))}
           else this.GetElementHandle().style.setProperty("background-color","");
        };
        this.GetElementHandle().style.setProperty("text-align","left");
        this.GetElementHandle().style.setProperty("vertical-align","middle");
        this.GetElementHandle().style.setProperty("display","table");
        this.GetElementHandle().style.setProperty("overflow","hidden");
        this.GetElementHandle().style.setProperty("padding","0px");
        this.GetElementHandle().style.setProperty("text-align","center");
        s = "";
        inactive = "";
        noptr = "";
        if (!this.FEnabled) inactive = " md-light md-inactive";
        noptr = "pointer-events:none";
        if (this.FGlyph !== "") s = ((((('<i class="material-icons' + inactive) + '" style="') + noptr) + '">') + this.FGlyph) + "<\/i>";
        if ((this.FGlyph !== "") && (this.FCaption !== "")) s = s + "&nbsp;";
        if (this.FCaption !== "") s = ((((s + '<span style="display: table-cell;vertical-align:middle;width:80%;') + noptr) + '">') + this.FCaption) + "<\/span>";
        this.GetElementHandle().innerHTML = s;
      };
    };
    this.UpdateGroup = function () {
      var i = 0;
      if (this.FParent != null) {
        for (var $l1 = 0, $end2 = this.FParent.GetControlsCount() - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          if (((this.FParent.GetControls(i) !== this) && $mod.TSpeedButton.isPrototypeOf(this.FParent.GetControls(i))) && (rtl.as(this.FParent.GetControls(i),$mod.TSpeedButton).FGroupIndex === this.FGroupIndex)) rtl.as(this.FParent.GetControls(i),$mod.TSpeedButton).SetDown(false);
        };
      };
    };
    this.MouseUp = function (Button, Shift, X, Y) {
      if (this.FEnabled) this.SetIntColor(12632256);
    };
    this.MouseDown = function (Button, Shift, X, Y) {
      if (!this.FEnabled) return;
      this.SetIntColor(8421504);
      if (this.FGroupIndex > 0) {
        if (this.FDown && this.FAllowAllUp) {
          this.SetDown(false)}
         else {
          this.SetDown(true);
          this.UpdateGroup();
        };
      };
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.FColor$1 = -1;
      this.FIntColor = -1;
      this.FGroupIndex = 0;
      this.FDown = false;
      this.FFlat = false;
    };
  });
  rtl.createClass($mod,"TSpeedButton",$mod.TCustomSpeedButton,function () {
    var $r = this.$rtti;
    $r.addProperty("AllowAllUp",0,rtl.boolean,"FAllowAllUp","FAllowAllUp");
    $r.addProperty("Caption",2,rtl.string,"FCaption","SetCaption");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Flat",2,rtl.boolean,"FFlat","SetFlat");
    $r.addProperty("Glyph",2,rtl.string,"FGlyph","SetGlyph");
    $r.addProperty("GroupIndex",2,rtl.longint,"FGroupIndex","SetGroupIndex");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnKeyDown",0,pas["WEBLib.Controls"].$rtti["TKeyDownEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas["WEBLib.Controls"].$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas["WEBLib.Controls"].$rtti["TKeyUpEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas["WEBLib.Controls"].$rtti["TMouseDownEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseUp",0,pas["WEBLib.Controls"].$rtti["TMouseUpEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseMove",0,pas["WEBLib.Controls"].$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseLeave",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnEnter",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnExit","FOnExit");
  });
  rtl.createClass($mod,"TCustomToolBar",pas["WEBLib.Controls"].TCustomControl,function () {
    this.CreateElement = function () {
      var Result = null;
      Result = document.createElement("SPAN");
      return Result;
    };
    this.UpdateElement = function () {
      pas["WEBLib.Controls"].TCustomControl.UpdateElement.apply(this,arguments);
      if (this.GetElementHandle() != null) this.GetElementHandle().style.setProperty("background-color","#CFCFCF");
    };
  });
  rtl.createClass($mod,"TToolBar",$mod.TCustomToolBar,function () {
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas["WEBLib.Controls"].$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlignWithMargins",2,rtl.boolean,"FAlignWithMargins","SetAlignWithMargins");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("ElementClassName",2,rtl.string,"FElementClassName","SetElementClassName");
    $r.addProperty("ElementID",3,rtl.string,"GetID","SetID");
    $r.addProperty("Height",3,rtl.longint,"GetHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Left",3,rtl.longint,"GetLeft","SetLeft");
    $r.addProperty("Margins",2,pas["WEBLib.Controls"].$rtti["TMargins"],"FMargins","SetMargins");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Top",3,rtl.longint,"GetTop","SetTop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("Width",3,rtl.longint,"GetWidth","SetWidth");
    $r.addProperty("OnClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
  });
});
rtl.module("WEBLib.DBCtrls",["System","Classes","SysUtils","DB","WEBLib.Controls","WEBLib.StdCtrls","WEBLib.ExtCtrls","WEBLib.Buttons"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TFieldDataLink",pas.DB.TDataLink,function () {
    this.$init = function () {
      pas.DB.TDataLink.$init.call(this);
      this.FOnDataChange = null;
      this.FOnUpdateData = null;
      this.FOnActiveChange = null;
      this.FField = null;
      this.FFieldName = "";
      this.FModified = false;
    };
    this.$final = function () {
      this.FOnDataChange = undefined;
      this.FOnUpdateData = undefined;
      this.FOnActiveChange = undefined;
      this.FField = undefined;
      pas.DB.TDataLink.$final.call(this);
    };
    this.UpdateField = function () {
      this.FField = null;
      if (((this.FDataSource != null) && (this.FDataSource.FDataSet != null)) && (this.FFieldName !== "")) if (this.FDataSource.FDataSet.GetActive()) this.FField = this.FDataSource.FDataSet.FieldByName(this.FFieldName);
    };
    this.ActiveChanged = function () {
      if (this.FOnActiveChange != null) this.FOnActiveChange(this);
    };
    this.RecordChanged = function (Field) {
      if ((Field === null) || (Field === this.FField)) if (this.FOnDataChange != null) this.FOnDataChange(this);
    };
    this.UpdateData = function () {
      if (this.FModified) {
        if ((this.FField != null) && (this.FOnUpdateData != null)) this.FOnUpdateData(this);
        this.FModified = false;
      };
    };
    this.Modified = function () {
      this.FModified = true;
    };
    var $r = this.$rtti;
    $r.addProperty("FieldName",0,rtl.string,"FFieldName","FFieldName");
    $r.addProperty("OnDataChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnDataChange","FOnDataChange");
    $r.addProperty("OnUpdateData",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnUpdateData","FOnUpdateData");
    $r.addProperty("OnActiveChange",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FOnActiveChange","FOnActiveChange");
  });
  rtl.createClass($mod,"TDBEdit",pas["WEBLib.StdCtrls"].TEdit,function () {
    this.$init = function () {
      pas["WEBLib.StdCtrls"].TEdit.$init.call(this);
      this.FDataLink = null;
      this.FEditChange = false;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      pas["WEBLib.StdCtrls"].TEdit.$final.call(this);
    };
    this.DataUpdate = function (Sender) {
      if (this.FDataLink.FField != null) {
        if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
        this.FDataLink.FField.SetAsString(this.GetText());
      };
    };
    this.DataChange = function (Sender) {
      if (this.FEditChange) return;
      if (!(this.FDataLink.GetDataset() != null)) return;
      if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
      if (this.FDataLink.FField != null) this.SetText(this.FDataLink.FField.GetDisplayText());
    };
    this.ActiveChange = function (Sender) {
      if (this.FDataLink.GetDataset() != null) {
        if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
          this.SetText("")}
         else this.DataChange(this);
      };
    };
    this.GetDataField = function () {
      var Result = "";
      Result = this.FDataLink.FFieldName;
      return Result;
    };
    this.SetDataField = function (Value) {
      this.FDataLink.FFieldName = Value;
      this.FDataLink.UpdateField();
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.KeyDown = function (Key, Shift) {
      var canedit = false;
      if (!this.CheckDataSet()) return;
      if (((Key.get() === 46) || (Key.get() === 8)) || ((Key.get() === 45) && (pas["WEBLib.Controls"].TShiftStateEnum.ssShift in Shift))) {
        this.FEditChange = true;
        canedit = this.EditCanModify();
        this.FEditChange = false;
        if (!canedit) {
          Key.set(0);
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          return;
        } else {
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          this.FDataLink.Edit();
        };
      };
    };
    this.KeyPress = function (Key) {
      var canedit = false;
      if (!(this.GetDataSource() != null)) {
        pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
        return;
      };
      this.FEditChange = true;
      canedit = this.EditCanModify();
      this.FEditChange = false;
      if (!canedit) return;
      if ((((Key.get() >= " ") && (this.FDataLink.FField != null)) && !this.FDataLink.FField.IsValidChar(Key.get())) || this.FDataLink.FReadOnly) Key.set("\x00");
      pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
    };
    this.CheckDataSet = function () {
      var Result = false;
      Result = false;
      if (((this.GetDataSource() != null) && this.GetDataSource().FEnabled) && (this.GetDataField() !== "")) {
        if ((this.FDataLink.GetDataset() != null) && this.FDataLink.GetDataset().GetActive()) Result = true;
      };
      return Result;
    };
    this.EditCanModify = function () {
      var Result = false;
      if (this.GetDataSource() != null) {
        Result = this.FDataLink.Edit()}
       else Result = true;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      if (!this.CheckDataSet()) {
        Result = ""}
       else Result = pas["WEBLib.StdCtrls"].TCustomEdit.GetDisplayText.call(this);
      return Result;
    };
    this.Change = function () {
      pas["WEBLib.StdCtrls"].TCustomEdit.Change.apply(this,arguments);
      this.FDataLink.Modified();
    };
    this.DoExit = function () {
      if (!this.FDataLink.FReadOnly) {
        this.FEditChange = true;
        this.FDataLink.UpdateRecord();
      };
      pas["WEBLib.Controls"].TControl.DoExit.apply(this,arguments);
      this.FEditChange = false;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.StdCtrls"].TCustomEdit.CreateInitialize.apply(this,arguments);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnUpdateData = rtl.createCallback(this,"DataUpdate");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
      this.FEditChange = false;
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas["WEBLib.Controls"].TCustomControl.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataField",3,rtl.string,"GetDataField","SetDataField");
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
  rtl.createClass($mod,"TDBLabel",pas["WEBLib.StdCtrls"].TLabel,function () {
    this.$init = function () {
      pas["WEBLib.StdCtrls"].TLabel.$init.call(this);
      this.FDataLink = null;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      pas["WEBLib.StdCtrls"].TLabel.$final.call(this);
    };
    this.DataUpdate = function (Sender) {
    };
    this.DataChange = function (Sender) {
      if (!(this.FDataLink.GetDataset() != null)) return;
      if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
      this.SetCaption(this.FDataLink.FField.GetDisplayText());
    };
    this.ActiveChange = function (Sender) {
      if (this.FDataLink.GetDataset() != null) {
        if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
          this.SetCaption("")}
         else this.DataChange(this);
      };
    };
    this.SetDataField = function (Value) {
      this.FDataLink.FFieldName = Value;
      this.FDataLink.UpdateField();
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.GetDataField = function () {
      var Result = "";
      Result = this.FDataLink.FFieldName;
      return Result;
    };
    this.CheckDataSet = function () {
      var Result = false;
      Result = false;
      if (((this.GetDataSource() != null) && this.GetDataSource().FEnabled) && (this.GetDataField() !== "")) {
        if ((this.FDataLink.GetDataset() != null) && this.FDataLink.GetDataset().GetActive()) Result = true;
      };
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      if (!this.CheckDataSet()) {
        Result = ""}
       else Result = pas["WEBLib.StdCtrls"].TCustomLabel.GetDisplayText.call(this);
      return Result;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.StdCtrls"].TCustomLabel.CreateInitialize.apply(this,arguments);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnUpdateData = rtl.createCallback(this,"DataUpdate");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas["WEBLib.Controls"].TCustomControl.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataField",3,rtl.string,"GetDataField","SetDataField");
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
  rtl.createClass($mod,"TDBSpinEdit",pas["WEBLib.StdCtrls"].TSpinEdit,function () {
    this.$init = function () {
      pas["WEBLib.StdCtrls"].TSpinEdit.$init.call(this);
      this.FDataLink = null;
      this.FEditChange = false;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      pas["WEBLib.StdCtrls"].TSpinEdit.$final.call(this);
    };
    this.DataUpdate = function (Sender) {
      if (this.FDataLink.FField != null) {
        if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
        this.FDataLink.FField.SetAsInteger(this.GetValue());
      };
    };
    this.DataChange = function (Sender) {
      if (this.FEditChange) return;
      if (!(this.FDataLink.GetDataset() != null)) return;
      if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
      this.SetValue(this.FDataLink.FField.GetAsInteger());
    };
    this.ActiveChange = function (Sender) {
      if (this.FDataLink.GetDataset() != null) {
        if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
          this.SetText("")}
         else this.DataChange(this);
      };
    };
    this.SetDataField = function (Value) {
      this.FDataLink.FFieldName = Value;
      this.FDataLink.UpdateField();
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.GetDataField = function () {
      var Result = "";
      Result = this.FDataLink.FFieldName;
      return Result;
    };
    this.KeyDown = function (Key, Shift) {
      var canedit = false;
      if (!(this.GetDataSource() != null)) {
        pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
        return;
      };
      if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
        pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
        return;
      };
      if (((Key.get() === 46) || (Key.get() === 8)) || ((Key.get() === 45) && (pas["WEBLib.Controls"].TShiftStateEnum.ssShift in Shift))) {
        this.FEditChange = true;
        canedit = this.EditCanModify();
        this.FEditChange = false;
        if (!canedit) {
          Key.set(0);
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          return;
        } else {
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          this.FDataLink.Edit();
        };
      };
    };
    this.KeyPress = function (Key) {
      var canedit = false;
      pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
      if (!(this.GetDataSource() != null)) {
        pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
        return;
      };
      if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
        pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
        return;
      };
      this.FEditChange = true;
      canedit = this.EditCanModify();
      this.FEditChange = false;
      if (!canedit) return;
      if ((((Key.get() >= " ") && (this.FDataLink.FField != null)) && !this.FDataLink.FField.IsValidChar(Key.get())) || this.FDataLink.FReadOnly) Key.set("\x00");
      pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
    };
    this.CheckDataSet = function () {
      var Result = false;
      Result = false;
      if ((this.GetDataSource() != null) && this.GetDataSource().FEnabled) {
        if ((this.FDataLink.GetDataset() != null) && this.FDataLink.GetDataset().GetActive()) Result = true;
      };
      return Result;
    };
    this.EditCanModify = function () {
      var Result = false;
      if (this.GetDataSource() != null) {
        Result = this.FDataLink.Edit()}
       else Result = true;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      if (!this.CheckDataSet()) {
        Result = ""}
       else Result = pas["WEBLib.StdCtrls"].TSpinEdit.GetDisplayText.call(this);
      return Result;
    };
    this.Change = function () {
      var canedit = false;
      pas["WEBLib.StdCtrls"].TSpinEdit.Change.apply(this,arguments);
      if (!this.CheckDataSet()) return;
      this.FEditChange = true;
      canedit = this.EditCanModify();
      this.FEditChange = false;
      if (canedit) {
        this.FDataLink.Edit();
        this.FDataLink.Modified();
      };
    };
    this.DoExit = function () {
      if (!this.FDataLink.FReadOnly) {
        this.FEditChange = true;
        this.FDataLink.UpdateRecord();
      };
      pas["WEBLib.Controls"].TControl.DoExit.apply(this,arguments);
      this.FEditChange = false;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.StdCtrls"].TSpinEdit.CreateInitialize.apply(this,arguments);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnUpdateData = rtl.createCallback(this,"DataUpdate");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
      this.FEditChange = false;
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas["WEBLib.Controls"].TCustomControl.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataField",3,rtl.string,"GetDataField","SetDataField");
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
  rtl.createClass($mod,"TDBMemo",pas["WEBLib.StdCtrls"].TMemo,function () {
    this.$init = function () {
      pas["WEBLib.StdCtrls"].TMemo.$init.call(this);
      this.FDataLink = null;
      this.FEditChange = false;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      pas["WEBLib.StdCtrls"].TMemo.$final.call(this);
    };
    this.DataUpdate = function (Sender) {
      if (this.FDataLink.FField != null) {
        if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
        this.FDataLink.FField.SetAsString(this.FLines.GetTextStr());
      };
    };
    this.DataChange = function (Sender) {
      if (this.FEditChange) return;
      if (!(this.FDataLink.GetDataset() != null)) return;
      if (!(this.FDataLink.FField != null)) this.FDataLink.UpdateField();
      if (this.FDataLink.FField != null) this.FLines.SetTextStr(this.FDataLink.FField.GetDisplayText());
    };
    this.ActiveChange = function (Sender) {
      if (this.FDataLink.GetDataset() != null) {
        if (!this.FDataLink.GetDataset().GetActive() || !this.GetDataSource().FEnabled) {
          this.FLines.SetTextStr("")}
         else this.DataChange(this);
      };
    };
    this.SetDataField = function (Value) {
      this.FDataLink.FFieldName = Value;
      this.FDataLink.UpdateField();
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.GetDataField = function () {
      var Result = "";
      Result = this.FDataLink.FFieldName;
      return Result;
    };
    this.KeyDown = function (Key, Shift) {
      var canedit = false;
      if (!this.CheckDataSet()) return;
      if (((Key.get() === 46) || (Key.get() === 8)) || ((Key.get() === 45) && (pas["WEBLib.Controls"].TShiftStateEnum.ssShift in Shift))) {
        this.FEditChange = true;
        canedit = this.EditCanModify();
        this.FEditChange = false;
        if (!canedit) {
          Key.set(0);
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          return;
        } else {
          pas["WEBLib.Controls"].TControl.KeyDown.apply(this,arguments);
          this.FDataLink.Edit();
        };
      };
    };
    this.KeyPress = function (Key) {
      var canedit = false;
      if (!(this.GetDataSource() != null)) {
        pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
        return;
      };
      this.FEditChange = true;
      canedit = this.EditCanModify();
      this.FEditChange = false;
      if (!canedit) return;
      if ((((Key.get() >= " ") && (this.FDataLink.FField != null)) && !this.FDataLink.FField.IsValidChar(Key.get())) || this.FDataLink.FReadOnly) Key.set("\x00");
      pas["WEBLib.Controls"].TControl.KeyPress.apply(this,arguments);
    };
    this.CheckDataSet = function () {
      var Result = false;
      Result = false;
      if ((this.GetDataSource() != null) && this.GetDataSource().FEnabled) if ((this.FDataLink.GetDataset() != null) && this.FDataLink.GetDataset().GetActive()) Result = true;
      return Result;
    };
    this.EditCanModify = function () {
      var Result = false;
      if (this.GetDataSource() != null) {
        Result = this.FDataLink.Edit()}
       else Result = true;
      return Result;
    };
    this.GetDisplayText = function () {
      var Result = "";
      if (!this.CheckDataSet()) {
        Result = ""}
       else Result = pas["WEBLib.StdCtrls"].TCustomMemo.GetDisplayText.call(this);
      return Result;
    };
    this.Change = function () {
      pas["WEBLib.StdCtrls"].TCustomMemo.Change.apply(this,arguments);
      this.FDataLink.Modified();
    };
    this.DoExit = function () {
      if (!this.FDataLink.FReadOnly) {
        this.FEditChange = true;
        this.FDataLink.UpdateRecord();
      };
      pas["WEBLib.Controls"].TControl.DoExit.apply(this,arguments);
      this.FEditChange = false;
    };
    this.CreateInitialize = function () {
      pas["WEBLib.StdCtrls"].TCustomMemo.CreateInitialize.apply(this,arguments);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnUpdateData = rtl.createCallback(this,"DataUpdate");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
      this.FEditChange = false;
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas["WEBLib.StdCtrls"].TCustomMemo.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataField",3,rtl.string,"GetDataField","SetDataField");
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
  rtl.createClass($mod,"TDBNavigator",pas["WEBLib.Buttons"].TToolBar,function () {
    this.$init = function () {
      pas["WEBLib.Buttons"].TToolBar.$init.call(this);
      this.FDataLink = null;
      this.FFirstBtn = null;
      this.FPriorBtn = null;
      this.FNextBtn = null;
      this.FLastBtn = null;
      this.FEditBtn = null;
      this.FPostBtn = null;
      this.FInsertBtn = null;
      this.FDeleteBtn = null;
      this.FCancelBtn = null;
    };
    this.$final = function () {
      this.FDataLink = undefined;
      this.FFirstBtn = undefined;
      this.FPriorBtn = undefined;
      this.FNextBtn = undefined;
      this.FLastBtn = undefined;
      this.FEditBtn = undefined;
      this.FPostBtn = undefined;
      this.FInsertBtn = undefined;
      this.FDeleteBtn = undefined;
      this.FCancelBtn = undefined;
      pas["WEBLib.Buttons"].TToolBar.$final.call(this);
    };
    this.DataChange = function (Sender) {
      this.UpdateButtons();
    };
    this.ActiveChange = function (Sender) {
      this.UpdateButtons();
    };
    this.SetDataSource = function (Value) {
      this.FDataLink.SetDataSource(Value);
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = this.FDataLink.FDataSource;
      return Result;
    };
    this.UpdateElement = function () {
      pas["WEBLib.Buttons"].TCustomToolBar.UpdateElement.apply(this,arguments);
      if (!this.IsUpdating()) this.AddControlLink("googlematerial","https:\/\/fonts.googleapis.com\/icon?family=Material+Icons");
    };
    this.CreateInitialize = function () {
      pas["WEBLib.Controls"].TCustomControl.CreateInitialize.apply(this,arguments);
      this.SetWidth(24 * 9);
      this.FDataLink = $mod.TFieldDataLink.$create("Create$1");
      this.FDataLink.FOnDataChange = rtl.createCallback(this,"DataChange");
      this.FDataLink.FOnActiveChange = rtl.createCallback(this,"ActiveChange");
      this.CreateButton({p: this, get: function () {
          return this.p.FFirstBtn;
        }, set: function (v) {
          this.p.FFirstBtn = v;
        }},this.GetID() + "_first","&#xE5DC;","First");
      this.CreateButton({p: this, get: function () {
          return this.p.FPriorBtn;
        }, set: function (v) {
          this.p.FPriorBtn = v;
        }},this.GetID() + "_prior","&#xE5CB;","Prior");
      this.CreateButton({p: this, get: function () {
          return this.p.FNextBtn;
        }, set: function (v) {
          this.p.FNextBtn = v;
        }},this.GetID() + "_next","&#xE5CC;","Next");
      this.CreateButton({p: this, get: function () {
          return this.p.FLastBtn;
        }, set: function (v) {
          this.p.FLastBtn = v;
        }},this.GetID() + "_last","&#xE5DD;","Last");
      this.CreateButton({p: this, get: function () {
          return this.p.FEditBtn;
        }, set: function (v) {
          this.p.FEditBtn = v;
        }},this.GetID() + "_edit","&#xE5C7;","Edit");
      this.CreateButton({p: this, get: function () {
          return this.p.FPostBtn;
        }, set: function (v) {
          this.p.FPostBtn = v;
        }},this.GetID() + "_post","&#xE5CA;","Post");
      this.CreateButton({p: this, get: function () {
          return this.p.FInsertBtn;
        }, set: function (v) {
          this.p.FInsertBtn = v;
        }},this.GetID() + "_insert","&#xE145;","Insert");
      this.CreateButton({p: this, get: function () {
          return this.p.FDeleteBtn;
        }, set: function (v) {
          this.p.FDeleteBtn = v;
        }},this.GetID() + "_delete","&#xE15B;","Delete");
      this.CreateButton({p: this, get: function () {
          return this.p.FCancelBtn;
        }, set: function (v) {
          this.p.FCancelBtn = v;
        }},this.GetID() + "_cancel","&#xE14C;","Cancel");
      this.UpdateButtons();
    };
    this.HandleSpeedButtonClick = function (Sender) {
      var BID = "";
      if ((this.GetDataSource() != null) && (this.GetDataSource().FDataSet != null)) {
        BID = Sender.GetID();
        if (BID === (this.GetID() + "_first")) this.FDataLink.GetDataset().First();
        if (BID === (this.GetID() + "_prior")) this.FDataLink.GetDataset().Prior();
        if (BID === (this.GetID() + "_next")) this.FDataLink.GetDataset().Next();
        if (BID === (this.GetID() + "_last")) this.FDataLink.GetDataset().Last();
        if (BID === (this.GetID() + "_edit")) this.FDataLink.GetDataset().Edit();
        if (BID === (this.GetID() + "_insert")) this.FDataLink.GetDataset().Insert$1();
        if (BID === (this.GetID() + "_delete")) this.FDataLink.GetDataset().Delete();
        if (BID === (this.GetID() + "_post")) this.FDataLink.GetDataset().Post();
        if (BID === (this.GetID() + "_cancel")) this.FDataLink.GetDataset().Cancel();
      };
    };
    this.UpdateButtons = function () {
      if ((((this.FDataLink != null) && (this.FDataLink.FDataSource != null)) && (this.FDataLink.FDataSource.FDataSet != null)) && pas.System.Assigned(this.FDataLink.FDataSource.FDataSet.GetActive())) {
        this.FFirstBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && !this.FDataLink.GetDataset().FBOF);
        this.FPriorBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && !this.FDataLink.GetDataset().FBOF);
        this.FNextBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && !this.FDataLink.GetDataset().FEOF);
        this.FLastBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && !this.FDataLink.GetDataset().FEOF);
        this.FPostBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && (this.FDataLink.GetDataset().FState in rtl.createSet(pas.DB.TDataSetState.dsEdit,pas.DB.TDataSetState.dsInsert)));
        this.FEditBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && (this.FDataLink.GetDataset().FState === pas.DB.TDataSetState.dsBrowse));
        this.FInsertBtn.SetEnabled(this.FDataLink.GetDataset().GetActive() && !(this.FDataLink.GetDataset().FState === pas.DB.TDataSetState.dsInsert));
        this.FDeleteBtn.SetEnabled(this.FDataLink.GetDataset().GetActive());
      } else {
        this.FFirstBtn.SetEnabled(false);
        this.FPriorBtn.SetEnabled(false);
        this.FNextBtn.SetEnabled(false);
        this.FLastBtn.SetEnabled(false);
        this.FEditBtn.SetEnabled(false);
        this.FPostBtn.SetEnabled(false);
        this.FInsertBtn.SetEnabled(false);
        this.FDeleteBtn.SetEnabled(false);
        this.FCancelBtn.SetEnabled(false);
      };
    };
    this.CreateButton = function (btn, BtnID, Glyph, Hint) {
      btn.set(pas["WEBLib.Buttons"].TSpeedButton.$create("Create$2",[BtnID]));
      btn.get().SetFlat(true);
      btn.get().SetParent(this);
      btn.get().SetGlyph(Glyph);
      btn.get().SetWidth(32);
      btn.get().SetHeight(24);
      btn.get().SetAlign(pas["WEBLib.Controls"].TAlign.alLeft);
      btn.get().SetHint(Hint);
      btn.get().SetShowHint(Hint !== "");
      btn.get().FOnClick = rtl.createCallback(this,"HandleSpeedButtonClick");
    };
    this.Destroy = function () {
      rtl.free(this,"FDataLink");
      pas["WEBLib.Controls"].TCustomControl.Destroy.apply(this,arguments);
    };
    var $r = this.$rtti;
    $r.addProperty("DataSource",3,pas.DB.$rtti["TDataSource"],"GetDataSource","SetDataSource");
  });
});
rtl.module("JSONDataset",["System","Types","JS","DB","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TJSONFieldMapper",pas.System.TObject,function () {
    this.GetJSONDataForField$1 = function (F, Row) {
      var Result = undefined;
      Result = this.GetJSONDataForField(F.FFieldName,F.GetIndex(),Row);
      return Result;
    };
    this.SetJSONDataForField$1 = function (F, Row, Data) {
      this.SetJSONDataForField(F.FFieldName,F.GetIndex(),Row,Data);
    };
  });
  rtl.createClass($mod,"TJSONDateField",pas.DB.TDateField,function () {
    this.$init = function () {
      pas.DB.TDateField.$init.call(this);
      this.FDateFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateFormat",0,rtl.string,"FDateFormat","FDateFormat");
  });
  rtl.createClass($mod,"TJSONTimeField",pas.DB.TTimeField,function () {
    this.$init = function () {
      pas.DB.TTimeField.$init.call(this);
      this.FTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("TimeFormat",0,rtl.string,"FTimeFormat","FTimeFormat");
  });
  rtl.createClass($mod,"TJSONDateTimeField",pas.DB.TDateTimeField,function () {
    this.$init = function () {
      pas.DB.TDateTimeField.$init.call(this);
      this.FDateTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateTimeFormat",0,rtl.string,"FDateTimeFormat","FDateTimeFormat");
  });
  rtl.createClass($mod,"TJSONIndex",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
      this.FRows = null;
      this.FDataset = null;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FRows = undefined;
      this.FDataset = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.GetRecordIndex = function (aListIndex) {
      var Result = 0;
      if (pas.JS.isUndefined(this.FList[aListIndex])) {
        Result = -1}
       else Result = Math.floor(this.FList[aListIndex]);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.length;
      return Result;
    };
    this.Create$1 = function (aDataset, aRows) {
      this.FRows = aRows;
      this.FList = new Array(this.FRows.length);
      this.FDataset = aDataset;
      this.CreateIndex();
    };
    this.Delete = function (aListIndex) {
      var Result = 0;
      var a = null;
      a = this.FList.splice(aListIndex,1);
      if (a.length > 0) {
        Result = Math.floor(a[0])}
       else Result = -1;
      return Result;
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      Result = this.Append(aRecordIndex);
      return Result;
    };
  });
  rtl.createClass($mod,"TDefaultJSONIndex",$mod.TJSONIndex,function () {
    this.CreateIndex = function () {
      var I = 0;
      for (var $l1 = 0, $end2 = this.FRows.length - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FList[I] = I;
      };
    };
    this.AppendToIndex = function () {
      var I = 0;
      var L = 0;
      L = this.FList.length;
      this.FList.length = this.FRows.length;
      for (var $l1 = L, $end2 = this.FRows.length - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        this.FList[I] = I;
      };
    };
    this.Append = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.push(aRecordIndex) - 1;
      return Result;
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      this.FList.splice(aCurrentIndex,0,aRecordIndex);
      Result = aCurrentIndex;
      return Result;
    };
    this.FindRecord = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.indexOf(aRecordIndex);
      return Result;
    };
    this.Update = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      if (this.GetRecordIndex(aCurrentIndex) !== aRecordIndex) pas.DB.DatabaseErrorFmt$1("Inconsistent record index in default index, expected %d, got %d.",[aCurrentIndex,this.GetRecordIndex(aCurrentIndex)],this.FDataset);
      return Result;
    };
  });
  rtl.createClass($mod,"TBaseJSONDataSet",pas.DB.TDataSet,function () {
    this.$init = function () {
      pas.DB.TDataSet.$init.call(this);
      this.FOwnsData = false;
      this.FDefaultIndex = null;
      this.FCurrentIndex = null;
      this.FCurrent = 0;
      this.FMetaData = null;
      this.FRows = null;
      this.FDeletedRows = null;
      this.FFieldMapper = null;
      this.FEditIdx = 0;
      this.FEditRow = undefined;
      this.FUseDateTimeFormatFields = false;
    };
    this.$final = function () {
      this.FDefaultIndex = undefined;
      this.FCurrentIndex = undefined;
      this.FMetaData = undefined;
      this.FRows = undefined;
      this.FDeletedRows = undefined;
      this.FFieldMapper = undefined;
      pas.DB.TDataSet.$final.call(this);
    };
    this.SetRows = function (AValue) {
      if (AValue === this.FRows) return;
      this.CheckInactive();
      this.FRows = null;
      this.AddToRows(AValue);
    };
    this.AllocRecordBuffer = function () {
      var Result = new pas.DB.TDataRecord();
      Result.data = new Object();
      Result.bookmark = null;
      Result.state = pas.DB.TRecordState.rsNew;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
      Buffer.get().data = null;
      Buffer.get().bookmark = null;
      Buffer.get().state = pas.DB.TRecordState.rsNew;
    };
    this.InternalInitRecord = function (Buffer) {
      Buffer.get().data = this.FFieldMapper.CreateRow();
      Buffer.get().bookmark = null;
      Buffer.get().state = pas.DB.TRecordState.rsNew;
    };
    this.GetRecord = function (Buffer, GetMode, DoCheck) {
      var Result = 0;
      var BkmIdx = 0;
      Result = pas.DB.TGetResult.grOK;
      var $tmp1 = GetMode;
      if ($tmp1 === pas.DB.TGetMode.gmNext) {
        if (this.FCurrent < (this.FCurrentIndex.GetCount() - 1)) {
          this.FCurrent += 1}
         else Result = pas.DB.TGetResult.grEOF}
       else if ($tmp1 === pas.DB.TGetMode.gmPrior) {
        if (this.FCurrent > 0) {
          this.FCurrent -= 1}
         else Result = pas.DB.TGetResult.grBOF}
       else if ($tmp1 === pas.DB.TGetMode.gmCurrent) if (this.FCurrent >= this.FCurrentIndex.GetCount()) Result = pas.DB.TGetResult.grEOF;
      if (Result === pas.DB.TGetResult.grOK) {
        BkmIdx = this.FCurrentIndex.GetRecordIndex(this.FCurrent);
        Buffer.get().data = this.FRows[BkmIdx];
        Buffer.get().bookmarkFlag = pas.DB.TBookmarkFlag.bfCurrent;
        Buffer.get().bookmark = BkmIdx;
      };
      return Result;
    };
    this.AddToRows = function (AValue) {
      if (this.FRows === null) {
        this.FRows = AValue}
       else {
        this.FRows = this.FRows.concat(AValue);
        this.AppendToIndexes();
      };
    };
    this.InternalClose = function () {
      this.BindFields(false);
      if (this.FDefaultFields) this.DestroyFields();
      this.FreeData();
    };
    this.InternalDelete = function () {
      var Idx = 0;
      Idx = this.FCurrentIndex.Delete(this.FCurrent);
      if (Idx !== -1) {
        if (!(this.FDeletedRows != null)) {
          this.FDeletedRows = new Array(this.FRows[Idx])}
         else this.FDeletedRows.push(this.FRows[Idx]);
        this.FRows[Idx] = undefined;
      };
    };
    this.InternalFirst = function () {
      this.FCurrent = -1;
    };
    this.InternalLast = function () {
      this.FCurrent = this.FCurrentIndex.GetCount();
    };
    this.InternalOpen = function () {
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FFieldMapper = this.CreateFieldMapper();
      if (this.FRows === null) {
        this.FRows = new Array();
        this.FOwnsData = true;
      };
      this.CreateIndexes();
      this.InternalInitFieldDefs();
      if (this.FDefaultFields) this.CreateFields();
      this.BindFields(true);
      this.InitDateTimeFields();
      this.FCurrent = -1;
    };
    this.InternalPost = function () {
      var Idx = 0;
      var B = new pas.DB.TBookmark();
      this.GetBookmarkData(new pas.DB.TDataRecord(this.ActiveBuffer()),{get: function () {
          return B;
        }, set: function (v) {
          B = v;
        }});
      if (this.FState === pas.DB.TDataSetState.dsInsert) {
        Idx = this.FRows.push(this.FEditRow) - 1;
        if (this.GetBookmarkFlag(new pas.DB.TDataRecord(this.ActiveBuffer())) === pas.DB.TBookmarkFlag.bfEOF) {
          this.FDefaultIndex.Append(Idx);
          if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrentIndex.Append(Idx);
        } else {
          this.FCurrent = this.FDefaultIndex.Insert(this.FCurrent,Idx);
          if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrent = this.FCurrentIndex.Insert(this.FCurrent,Idx);
        };
      } else {
        if (this.FEditIdx === -1) pas.DB.DatabaseErrorFmt("Failed to retrieve record index for record %d",[this.FCurrent]);
        Idx = this.FEditIdx;
        this.FRows[Idx] = this.FEditRow;
        this.FDefaultIndex.Update(this.FCurrent,Idx);
        if (this.FCurrentIndex !== this.FDefaultIndex) this.FCurrentIndex.Update(this.FCurrent,Idx);
      };
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalInsert = function () {
      var I = 0;
      var D = null;
      this.FEditRow = this.ActiveBuffer().data;
      for (var $l1 = 0, $end2 = this.FFieldDefs.GetCount() - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        D = this.FFieldDefs.GetItem$1(I);
        this.FFieldMapper.SetJSONDataForField(D.FName,D.GetIndex(),this.FEditRow,null);
      };
    };
    this.InternalEdit = function () {
      this.FEditIdx = this.FCurrentIndex.GetRecordIndex(this.FCurrent);
      if (!pas.JS.isUndefined(this.FRows[this.FEditIdx])) {
        this.FEditRow = JSON.parse(JSON.stringify(this.FRows[this.FEditIdx]))}
       else this.FEditRow = new Object();
    };
    this.InternalCancel = function () {
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalInitFieldDefs = function () {
      if (this.FMetaData != null) this.MetaDataToFieldDefs();
      if (this.FFieldDefs.GetCount() === 0) throw $mod.EJSONDataset.$create("Create$1",["No fields found"]);
    };
    this.InternalSetToRecord = function (Buffer) {
      this.FCurrent = this.FCurrentIndex.FindRecord(Math.floor(Buffer.bookmark));
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      if (this.FUseDateTimeFormatFields && (FieldType in rtl.createSet(pas.DB.TFieldType.ftDate,pas.DB.TFieldType.ftDateTime,pas.DB.TFieldType.ftTime))) {
        var $tmp1 = FieldType;
        if ($tmp1 === pas.DB.TFieldType.ftDate) {
          Result = $mod.TJSONDateField}
         else if ($tmp1 === pas.DB.TFieldType.ftDateTime) {
          Result = $mod.TJSONDateTimeField}
         else if ($tmp1 === pas.DB.TFieldType.ftTime) Result = $mod.TJSONTimeField;
      } else Result = pas.DB.TDataSet.GetFieldClass.call(this,FieldType);
      return Result;
    };
    this.IsCursorOpen = function () {
      var Result = false;
      Result = this.FDefaultIndex != null;
      return Result;
    };
    this.GetBookmarkData = function (Buffer, Data) {
      Data.get().Data = Buffer.bookmark;
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = Buffer.bookmarkFlag;
      return Result;
    };
    this.SetBookmarkFlag = function (Buffer, Value) {
      Buffer.get().bookmarkFlag = Value;
    };
    this.SetBookmarkData = function (Buffer, Data) {
      Buffer.get().bookmark = Data.Data;
    };
    this.FreeData = function () {
      if (this.FOwnsData) {
        this.FRows = null;
        this.FMetaData = null;
      };
      if (this.FCurrentIndex !== this.FDefaultIndex) {
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FCurrentIndex;
          }, set: function (v) {
            this.p.FCurrentIndex = v;
          }})}
       else this.FCurrentIndex = null;
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FDefaultIndex;
        }, set: function (v) {
          this.p.FDefaultIndex = v;
        }});
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FCurrentIndex = null;
      this.FDeletedRows = null;
    };
    this.AppendToIndexes = function () {
      this.FDefaultIndex.AppendToIndex();
    };
    this.CreateIndexes = function () {
      this.FDefaultIndex = $mod.TDefaultJSONIndex.$create("Create$1",[this,this.FRows]);
      this.AppendToIndexes();
      this.FCurrentIndex = this.FDefaultIndex;
    };
    this.InitDateTimeFields = function () {
    };
    this.Create$1 = function (AOwner) {
      pas.DB.TDataSet.Create$1.apply(this,arguments);
      this.FOwnsData = true;
      this.FUseDateTimeFormatFields = false;
      this.FEditIdx = -1;
    };
    this.Destroy = function () {
      this.FEditIdx = -1;
      this.FreeData();
      pas.DB.TDataSet.Destroy.apply(this,arguments);
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      var R = undefined;
      if (this.FEditIdx == Buffer.bookmark) {
        R = this.FEditRow}
       else R = Buffer.data;
      Result = this.FFieldMapper.GetJSONDataForField$1(Field,R);
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      this.FFieldMapper.SetJSONDataForField$1(Field,this.FEditRow,AValue);
      this.SetModified(true);
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      if (rtl.isNumber(Bookmark1.Data) && rtl.isNumber(Bookmark2.Data)) {
        Result = Math.floor(Bookmark2.Data) - Math.floor(Bookmark1.Data)}
       else {
        if (rtl.isNumber(Bookmark1.Data)) {
          Result = -1}
         else if (rtl.isNumber(Bookmark2.Data)) {
          Result = 1}
         else Result = 0;
      };
      return Result;
    };
  });
  rtl.createClass($mod,"TJSONObjectFieldMapper",$mod.TJSONFieldMapper,function () {
    this.SetJSONDataForField = function (FieldName, FieldIndex, Row, Data) {
      rtl.getObject(Row)[FieldName] = Data;
    };
    this.GetJSONDataForField = function (FieldName, FieldIndex, Row) {
      var Result = undefined;
      Result = rtl.getObject(Row)[FieldName];
      return Result;
    };
    this.CreateRow = function () {
      var Result = undefined;
      Result = new Object();
      return Result;
    };
  });
  rtl.createClass($mod,"EJSONDataset",pas.DB.EDatabaseError,function () {
  });
},["DateUtils"]);
rtl.module("WEBLib.CDS",["System","Classes","SysUtils","Types","JS","Web","DB","JSONDataset","WEBLib.Controls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TClientDataSource",pas.DB.TDataSource,function () {
    this.$init = function () {
      pas.DB.TDataSource.$init.call(this);
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
    };
    this.$final = function () {
      this.FParent = undefined;
      pas.DB.TDataSource.$final.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Parent",0,pas["WEBLib.Controls"].$rtti["TControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
  });
  $mod.$rtti.$ProcVar("TConnectErrorEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["ErrorCode",rtl.longint]])});
  rtl.createClass($mod,"TClientConnection",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FReq = null;
      this.FDS = null;
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
      this.FActive = false;
      this.FDataNode = "";
      this.FURI = "";
      this.FAfterConnect = null;
      this.FBeforeConnect = null;
      this.FOnConnectError = null;
    };
    this.$final = function () {
      this.FReq = undefined;
      this.FDS = undefined;
      this.FParent = undefined;
      this.FAfterConnect = undefined;
      this.FBeforeConnect = undefined;
      this.FOnConnectError = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetActive = function (AValue) {
      if ((this.FURI !== "") && AValue) {
        this.DoBeforeConnect();
        this.FActive = AValue;
        this.FReq = new XMLHttpRequest();
        this.FReq.addEventListener("load",rtl.createCallback(this,"onLoad"));
        this.FReq.open("GET",this.FURI,true);
        this.FReq.send();
      };
    };
    this.onLoad = function (Event) {
      var Result = false;
      var J = undefined;
      var JA = undefined;
      if (this.FReq.status === 200) {
        J = JSON.parse(this.FReq.responseText);
        JA = rtl.getObject(J)[this.FDataNode];
        this.DoAfterConnect();
        if (this.FDS != null) {
          this.FDS.SetRows(rtl.getObject(JA));
          this.FDS.Open();
        } else this.DoError(this.FReq.status);
      };
      return Result;
    };
    this.RegisterDataSet = function (value) {
      this.FDS = value;
    };
    this.DoBeforeConnect = function () {
      if (this.FBeforeConnect != null) this.FBeforeConnect(this);
    };
    this.DoAfterConnect = function () {
      if (this.FAfterConnect != null) this.FAfterConnect(this);
    };
    this.DoError = function (ErrorCode) {
      if (this.FOnConnectError != null) this.FOnConnectError(this,ErrorCode);
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.apply(this,arguments);
      this.FDS = null;
    };
    var $r = this.$rtti;
    $r.addProperty("Parent",0,pas["WEBLib.Controls"].$rtti["TControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
    $r.addProperty("Active",2,rtl.boolean,"FActive","SetActive");
    $r.addProperty("DataNode",0,rtl.string,"FDataNode","FDataNode");
    $r.addProperty("URI",0,rtl.string,"FURI","FURI");
    $r.addProperty("AfterConnect",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FAfterConnect","FAfterConnect");
    $r.addProperty("BeforeConnect",0,pas["WEBLib.Controls"].$rtti["TNotifyEvent"],"FBeforeConnect","FBeforeConnect");
    $r.addProperty("OnConnectError",0,$mod.$rtti["TConnectErrorEvent"],"FOnConnectError","FOnConnectError");
  });
  rtl.createClass($mod,"TClientDataSet",pas.JSONDataset.TBaseJSONDataSet,function () {
    this.$init = function () {
      pas.JSONDataset.TBaseJSONDataSet.$init.call(this);
      this.FConnection = null;
      this.FParent = null;
      this.FLeft = 0;
      this.FTop = 0;
    };
    this.$final = function () {
      this.FConnection = undefined;
      this.FParent = undefined;
      pas.JSONDataset.TBaseJSONDataSet.$final.call(this);
    };
    this.SetConnection = function (Value) {
      if (Value != null) Value.RegisterDataSet(this);
    };
    this.CreateFieldMapper = function () {
      var Result = null;
      Result = pas.JSONDataset.TJSONObjectFieldMapper.$create("Create");
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Rows",2,pas.JS.$rtti["TJSArray"],"FRows","SetRows");
    $r.addProperty("Parent",0,pas["WEBLib.Controls"].$rtti["TControl"],"FParent","FParent");
    $r.addProperty("Left",0,rtl.longint,"FLeft","FLeft");
    $r.addProperty("Top",0,rtl.longint,"FTop","FTop");
    $r.addProperty("Connection",2,$mod.$rtti["TClientConnection"],"FConnection","SetConnection");
  });
});
rtl.module("Unit1",["System","SysUtils","Classes","JS","Web","DB","WEBLib.Graphics","WEBLib.Controls","WEBLib.StdCtrls","WEBLib.ExtCtrls","WEBLib.Forms","WEBLib.DBCtrls","WEBLib.CDS"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TForm1",pas["WEBLib.Forms"].TForm,function () {
    this.$init = function () {
      pas["WEBLib.Forms"].TForm.$init.call(this);
      this.WebDBLabel1 = null;
      this.WebLabel1 = null;
      this.WebLabel2 = null;
      this.WebLabel3 = null;
      this.WebLabel4 = null;
      this.WebLabel6 = null;
      this.WebLabel7 = null;
      this.WebLabel5 = null;
      this.WebButton1 = null;
      this.WebDBNavigator1 = null;
      this.WebDBEdit1 = null;
      this.WebDBEdit2 = null;
      this.WebDBEdit3 = null;
      this.WebDBEdit4 = null;
      this.WebPanel1 = null;
      this.WebLabel9 = null;
      this.WebImageControl1 = null;
      this.WebDBMemo1 = null;
      this.WebDBSpinEdit1 = null;
      this.WebClientConnection1 = null;
      this.WebClientDataSet1 = null;
      this.WebClientDataSource1 = null;
    };
    this.$final = function () {
      this.WebDBLabel1 = undefined;
      this.WebLabel1 = undefined;
      this.WebLabel2 = undefined;
      this.WebLabel3 = undefined;
      this.WebLabel4 = undefined;
      this.WebLabel6 = undefined;
      this.WebLabel7 = undefined;
      this.WebLabel5 = undefined;
      this.WebButton1 = undefined;
      this.WebDBNavigator1 = undefined;
      this.WebDBEdit1 = undefined;
      this.WebDBEdit2 = undefined;
      this.WebDBEdit3 = undefined;
      this.WebDBEdit4 = undefined;
      this.WebPanel1 = undefined;
      this.WebLabel9 = undefined;
      this.WebImageControl1 = undefined;
      this.WebDBMemo1 = undefined;
      this.WebDBSpinEdit1 = undefined;
      this.WebClientConnection1 = undefined;
      this.WebClientDataSet1 = undefined;
      this.WebClientDataSource1 = undefined;
      pas["WEBLib.Forms"].TForm.$final.call(this);
    };
    this.LoadDFMValues = function () {
      pas["WEBLib.Forms"].TCustomForm.LoadDFMValues.apply(this,arguments);
      this.WebDBLabel1 = pas["WEBLib.DBCtrls"].TDBLabel.$create("Create$1",[this]);
      this.WebLabel1 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel2 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel3 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel4 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel6 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel7 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebLabel5 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this]);
      this.WebButton1 = pas["WEBLib.StdCtrls"].TButton.$create("Create$1",[this]);
      this.WebDBNavigator1 = pas["WEBLib.DBCtrls"].TDBNavigator.$create("Create$1",[this]);
      this.WebDBEdit1 = pas["WEBLib.DBCtrls"].TDBEdit.$create("Create$1",[this]);
      this.WebDBEdit2 = pas["WEBLib.DBCtrls"].TDBEdit.$create("Create$1",[this]);
      this.WebDBEdit3 = pas["WEBLib.DBCtrls"].TDBEdit.$create("Create$1",[this]);
      this.WebDBEdit4 = pas["WEBLib.DBCtrls"].TDBEdit.$create("Create$1",[this]);
      this.WebPanel1 = pas["WEBLib.ExtCtrls"].TPanel.$create("Create$1",[this]);
      this.WebLabel9 = pas["WEBLib.StdCtrls"].TLabel.$create("Create$1",[this.WebPanel1]);
      this.WebImageControl1 = pas["WEBLib.ExtCtrls"].TImageControl.$create("Create$1",[this.WebPanel1]);
      this.WebDBMemo1 = pas["WEBLib.DBCtrls"].TDBMemo.$create("Create$1",[this]);
      this.WebDBSpinEdit1 = pas["WEBLib.DBCtrls"].TDBSpinEdit.$create("Create$1",[this]);
      this.WebClientConnection1 = pas["WEBLib.CDS"].TClientConnection.$create("Create$1",[this]);
      this.WebClientDataSet1 = pas["WEBLib.CDS"].TClientDataSet.$create("Create$1",[this]);
      this.WebClientDataSource1 = pas["WEBLib.CDS"].TClientDataSource.$create("Create$1",[this]);
      this.WebDBLabel1.BeginUpdate();
      this.WebLabel1.BeginUpdate();
      this.WebLabel2.BeginUpdate();
      this.WebLabel3.BeginUpdate();
      this.WebLabel4.BeginUpdate();
      this.WebLabel6.BeginUpdate();
      this.WebLabel7.BeginUpdate();
      this.WebLabel5.BeginUpdate();
      this.WebButton1.BeginUpdate();
      this.WebDBNavigator1.BeginUpdate();
      this.WebDBEdit1.BeginUpdate();
      this.WebDBEdit2.BeginUpdate();
      this.WebDBEdit3.BeginUpdate();
      this.WebDBEdit4.BeginUpdate();
      this.WebPanel1.BeginUpdate();
      this.WebLabel9.BeginUpdate();
      this.WebImageControl1.BeginUpdate();
      this.WebDBMemo1.BeginUpdate();
      this.WebDBSpinEdit1.BeginUpdate();
      try {
        this.SetName("Form1");
        this.SetLeft(0);
        this.SetTop(0);
        this.SetWidth(775);
        this.SetHeight(575);
        this.FFont.FCharset = 1;
        this.FFont.SetColor(0);
        this.FFont.SetHeight(-13);
        this.FFont.SetName("Tahoma");
        this.FFont.SetStyle({});
        this.FFormContainer = "appcontent";
        this.SetTabOrder(1);
        this.WebDBLabel1.SetParent(this);
        this.WebDBLabel1.SetName("WebDBLabel1");
        this.WebDBLabel1.SetLeft(132);
        this.WebDBLabel1.SetTop(257);
        this.WebDBLabel1.SetWidth(457);
        this.WebDBLabel1.SetHeight(22);
        this.WebDBLabel1.SetAutoSize(false);
        this.WebDBLabel1.SetCaption("WebDBLabel1");
        this.WebDBLabel1.SetEllipsisPosition(pas["WEBLib.StdCtrls"].TEllipsisPosition.epEndEllipsis);
        this.WebDBLabel1.SetDataField("_Length_In");
        this.WebDBLabel1.SetDataSource(this.WebClientDataSource1);
        this.WebLabel1.SetParent(this);
        this.WebLabel1.SetName("WebLabel1");
        this.WebLabel1.SetLeft(16);
        this.WebLabel1.SetTop(98);
        this.WebLabel1.SetWidth(68);
        this.WebLabel1.SetHeight(16);
        this.WebLabel1.SetCaption("Species No:");
        this.WebLabel2.SetParent(this);
        this.WebLabel2.SetName("WebLabel2");
        this.WebLabel2.SetLeft(16);
        this.WebLabel2.SetTop(130);
        this.WebLabel2.SetWidth(56);
        this.WebLabel2.SetHeight(16);
        this.WebLabel2.SetCaption("Category:");
        this.WebLabel3.SetParent(this);
        this.WebLabel3.SetName("WebLabel3");
        this.WebLabel3.SetLeft(16);
        this.WebLabel3.SetTop(162);
        this.WebLabel3.SetWidth(93);
        this.WebLabel3.SetHeight(16);
        this.WebLabel3.SetCaption("Common Name:");
        this.WebLabel4.SetParent(this);
        this.WebLabel4.SetName("WebLabel4");
        this.WebLabel4.SetLeft(16);
        this.WebLabel4.SetTop(195);
        this.WebLabel4.SetWidth(86);
        this.WebLabel4.SetHeight(16);
        this.WebLabel4.SetCaption("Species Name:");
        this.WebLabel6.SetParent(this);
        this.WebLabel6.SetName("WebLabel6");
        this.WebLabel6.SetLeft(16);
        this.WebLabel6.SetTop(226);
        this.WebLabel6.SetWidth(64);
        this.WebLabel6.SetHeight(16);
        this.WebLabel6.SetCaption("Length cm:");
        this.WebLabel7.SetParent(this);
        this.WebLabel7.SetName("WebLabel7");
        this.WebLabel7.SetLeft(16);
        this.WebLabel7.SetTop(257);
        this.WebLabel7.SetWidth(58);
        this.WebLabel7.SetHeight(16);
        this.WebLabel7.SetCaption("Length In:");
        this.WebLabel5.SetParent(this);
        this.WebLabel5.SetName("WebLabel5");
        this.WebLabel5.SetLeft(16);
        this.WebLabel5.SetTop(288);
        this.WebLabel5.SetWidth(37);
        this.WebLabel5.SetHeight(16);
        this.WebLabel5.SetCaption("Notes:");
        this.WebButton1.SetParent(this);
        this.WebButton1.SetName("WebButton1");
        this.WebButton1.SetLeft(16);
        this.WebButton1.SetTop(16);
        this.WebButton1.SetWidth(153);
        this.WebButton1.SetHeight(25);
        this.WebButton1.SetCaption("Connect to DB");
        this.WebButton1.FOnClick = rtl.createCallback(this,"WebButton1Click");
        this.WebButton1.SetTabOrder(0);
        this.WebDBNavigator1.SetParent(this);
        this.WebDBNavigator1.SetName("WebDBNavigator1");
        this.WebDBNavigator1.SetLeft(132);
        this.WebDBNavigator1.SetTop(60);
        this.WebDBNavigator1.SetWidth(288);
        this.WebDBNavigator1.SetHeight(25);
        this.WebDBNavigator1.SetDataSource(this.WebClientDataSource1);
        this.WebDBEdit1.SetParent(this);
        this.WebDBEdit1.SetName("WebDBEdit1");
        this.WebDBEdit1.SetLeft(132);
        this.WebDBEdit1.SetTop(95);
        this.WebDBEdit1.SetWidth(457);
        this.WebDBEdit1.SetHeight(24);
        this.WebDBEdit1.SetAutoSelect(false);
        this.WebDBEdit1.SetColor(16777215);
        this.WebDBEdit1.SetHideSelection(false);
        this.WebDBEdit1.SetTabOrder(2);
        this.WebDBEdit1.SetText("WebDBEdit1");
        this.WebDBEdit1.SetDataField("_Species_No");
        this.WebDBEdit1.SetDataSource(this.WebClientDataSource1);
        this.WebDBEdit2.SetParent(this);
        this.WebDBEdit2.SetName("WebDBEdit2");
        this.WebDBEdit2.SetLeft(132);
        this.WebDBEdit2.SetTop(127);
        this.WebDBEdit2.SetWidth(457);
        this.WebDBEdit2.SetHeight(24);
        this.WebDBEdit2.SetAutoSelect(false);
        this.WebDBEdit2.SetColor(16777215);
        this.WebDBEdit2.SetHideSelection(false);
        this.WebDBEdit2.SetTabOrder(3);
        this.WebDBEdit2.SetText("WebDBEdit1");
        this.WebDBEdit2.SetDataField("_Category");
        this.WebDBEdit2.SetDataSource(this.WebClientDataSource1);
        this.WebDBEdit3.SetParent(this);
        this.WebDBEdit3.SetName("WebDBEdit3");
        this.WebDBEdit3.SetLeft(132);
        this.WebDBEdit3.SetTop(159);
        this.WebDBEdit3.SetWidth(457);
        this.WebDBEdit3.SetHeight(24);
        this.WebDBEdit3.SetAutoSelect(false);
        this.WebDBEdit3.SetColor(16777215);
        this.WebDBEdit3.SetHideSelection(false);
        this.WebDBEdit3.SetTabOrder(4);
        this.WebDBEdit3.SetText("WebDBEdit1");
        this.WebDBEdit3.SetDataField("_Common_Name");
        this.WebDBEdit3.SetDataSource(this.WebClientDataSource1);
        this.WebDBEdit4.SetParent(this);
        this.WebDBEdit4.SetName("WebDBEdit4");
        this.WebDBEdit4.SetLeft(132);
        this.WebDBEdit4.SetTop(192);
        this.WebDBEdit4.SetWidth(457);
        this.WebDBEdit4.SetHeight(24);
        this.WebDBEdit4.SetAutoSelect(false);
        this.WebDBEdit4.SetColor(16777215);
        this.WebDBEdit4.SetHideSelection(false);
        this.WebDBEdit4.SetTabOrder(5);
        this.WebDBEdit4.SetText("WebDBEdit1");
        this.WebDBEdit4.SetDataField("_Species_Name");
        this.WebDBEdit4.SetDataSource(this.WebClientDataSource1);
        this.WebPanel1.SetParent(this);
        this.WebPanel1.SetName("WebPanel1");
        this.WebPanel1.SetLeft(16);
        this.WebPanel1.SetTop(465);
        this.WebPanel1.SetWidth(541);
        this.WebPanel1.SetHeight(89);
        this.WebPanel1.SetWidthStyle(pas["WEBLib.Controls"].TSizeStyle.ssPercent);
        this.WebPanel1.SetWidthPercent(80);
        this.WebPanel1.SetBorderStyle(pas["WEBLib.Controls"].TBorderStyle.bsSingle);
        this.WebLabel9.SetParent(this.WebPanel1);
        this.WebLabel9.SetName("WebLabel9");
        this.WebLabel9.SetLeft(3);
        this.WebLabel9.SetTop(29);
        this.WebLabel9.SetWidth(460);
        this.WebLabel9.SetHeight(48);
        this.WebLabel9.SetCaption("Self demo shows a web client dataset connected to DB controls. The web client dataset gets the information from an Client server but for demo purposes all editing in the dataset is local in the web client only!");
        this.WebLabel9.FWordWrap = true;
        this.WebLabel9.SetWidthStyle(pas["WEBLib.Controls"].TSizeStyle.ssPercent);
        this.WebImageControl1.SetParent(this.WebPanel1);
        this.WebImageControl1.SetName("WebImageControl1");
        this.WebImageControl1.SetLeft(6);
        this.WebImageControl1.SetTop(7);
        this.WebImageControl1.SetWidth(16);
        this.WebImageControl1.SetHeight(16);
        this.WebImageControl1.FAutoSize = true;
        this.WebImageControl1.FPicture.LoadFromFile("Picture.png");
        this.WebDBMemo1.SetParent(this);
        this.WebDBMemo1.SetName("WebDBMemo1");
        this.WebDBMemo1.SetLeft(132);
        this.WebDBMemo1.SetTop(285);
        this.WebDBMemo1.SetWidth(457);
        this.WebDBMemo1.SetHeight(140);
        this.WebDBMemo1.SetAutoSize(false);
        this.WebDBMemo1.FLines.BeginUpdate();
        try {
          this.WebDBMemo1.FLines.Clear();
          this.WebDBMemo1.FLines.Add("WebDBMemo1");
        } finally {
          this.WebDBMemo1.FLines.EndUpdate();
        };
        this.WebDBMemo1.SetSelLength(0);
        this.WebDBMemo1.SetSelStart(0);
        this.WebDBMemo1.SetTabOrder(7);
        this.WebDBMemo1.SetDataField("_Notes");
        this.WebDBMemo1.SetDataSource(this.WebClientDataSource1);
        this.WebDBSpinEdit1.SetParent(this);
        this.WebDBSpinEdit1.SetName("WebDBSpinEdit1");
        this.WebDBSpinEdit1.SetLeft(132);
        this.WebDBSpinEdit1.SetTop(223);
        this.WebDBSpinEdit1.SetWidth(150);
        this.WebDBSpinEdit1.SetHeight(22);
        this.WebDBSpinEdit1.FAutoSize = false;
        this.WebDBSpinEdit1.SetBorderStyle(pas["WEBLib.Controls"].TBorderStyle.bsSingle);
        this.WebDBSpinEdit1.SetColor(16777215);
        this.WebDBSpinEdit1.SetIncrement(1);
        this.WebDBSpinEdit1.SetMaxValue(100);
        this.WebDBSpinEdit1.SetMinValue(0);
        this.WebDBSpinEdit1.SetTabOrder(8);
        this.WebDBSpinEdit1.SetValue(0);
        this.WebDBSpinEdit1.SetDataField("_Length__cm_");
        this.WebDBSpinEdit1.SetDataSource(this.WebClientDataSource1);
        this.WebClientConnection1.SetName("WebClientConnection1");
        this.WebClientConnection1.SetActive(false);
        this.WebClientDataSet1.SetName("WebClientDataSet1");
        this.WebClientDataSet1.SetConnection(this.WebClientConnection1);
        this.WebClientDataSource1.SetName("WebClientDataSource1");
        this.WebClientDataSource1.SetDataSet(this.WebClientDataSet1);
      } finally {
        this.WebDBLabel1.EndUpdate();
        this.WebLabel1.EndUpdate();
        this.WebLabel2.EndUpdate();
        this.WebLabel3.EndUpdate();
        this.WebLabel4.EndUpdate();
        this.WebLabel6.EndUpdate();
        this.WebLabel7.EndUpdate();
        this.WebLabel5.EndUpdate();
        this.WebButton1.EndUpdate();
        this.WebDBNavigator1.EndUpdate();
        this.WebDBEdit1.EndUpdate();
        this.WebDBEdit2.EndUpdate();
        this.WebDBEdit3.EndUpdate();
        this.WebDBEdit4.EndUpdate();
        this.WebPanel1.EndUpdate();
        this.WebLabel9.EndUpdate();
        this.WebImageControl1.EndUpdate();
        this.WebDBMemo1.EndUpdate();
        this.WebDBSpinEdit1.EndUpdate();
      };
    };
    this.WebButton1Click = function (Sender) {
      window.console.log("button clicked");
      this.WebClientConnection1.FURI = "fishfacti.json";
      this.WebClientConnection1.FDataNode = "ROW";
      this.WebClientDataSet1.FFieldDefs.Clear();
      this.WebClientDataSet1.FFieldDefs.Add$4("_Species_No",pas.DB.TFieldType.ftString,0);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Category",pas.DB.TFieldType.ftString,50);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Common_Name",pas.DB.TFieldType.ftString,50);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Species_Name",pas.DB.TFieldType.ftString,50);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Length__cm_",pas.DB.TFieldType.ftInteger,0);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Length_In",pas.DB.TFieldType.ftString,30);
      this.WebClientDataSet1.FFieldDefs.Add$4("_Notes",pas.DB.TFieldType.ftString,255);
      this.WebClientConnection1.SetActive(true);
    };
  });
  this.Form1 = null;
});
rtl.module("program",["System","JS","Classes","SysUtils","Web","WEBLib.Forms","WEBLib.Runner","Unit1"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    pas["WEBLib.Forms"].Application.Initialize();
    pas["WEBLib.Forms"].Application.CreateForm(pas.Unit1.TForm1,{p: pas.Unit1, get: function () {
        return this.p.Form1;
      }, set: function (v) {
        this.p.Form1 = v;
      }});
    pas["WEBLib.Forms"].Application.Run();
    pas["WEBLib.Runner"].TTMSWebRunner.Execute$1(pas["WEBLib.Runner"].TTMSBrowserEnum.tbnFirefox);
  };
});
//# sourceMappingURL=project1.js.map
