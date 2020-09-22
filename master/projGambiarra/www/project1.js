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
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
  });
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = rtl.length(arguments) - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l1 = 0, $end2 = l; $l1 <= $end2; $l1++) {
        i = $l1;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l3 = 0, $end4 = l; $l3 <= $end4; $l3++) {
        i = $l3;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.WriteBuf = "";
  $impl.WriteCallBack = null;
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"EJS",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.FMessage = Msg;
    };
  });
  this.New = function (aElements) {
    var Result = null;
    var L = 0;
    var I = 0;
    var S = "";
    L = rtl.length(aElements);
    if ((L % 2) === 1) throw $mod.EJS.$create("Create$1",["Number of arguments must be even"]);
    I = 0;
    while (I < L) {
      if (!rtl.isString(aElements[I])) {
        S = String(I);
        throw $mod.EJS.$create("Create$1",[("Argument " + S) + " must be a string."]);
      };
      I += 2;
    };
    I = 0;
    Result = new Object();
    while (I < L) {
      S = "" + aElements[I];
      Result[S] = aElements[I + 1];
      I += 2;
    };
    return Result;
  };
});
rtl.module("SysUtils",["System","JS"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
  });
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
  });
  this.FormatSettings = null;
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
  };
});
rtl.module("Classes",["System","Types","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
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
});
rtl.module("uPromises",["System","JS","Web"],function () {
  "use strict";
  var $mod = this;
  this.wait = function (ms) {
    var Result = null;
    function p(resolve, reject) {
      window.setTimeout(resolve,ms);
    };
    Result = new Promise(p);
    return Result;
  };
});
rtl.module("uDOM",["System","Types","Web","JS","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("uLogin",["System","Classes","SysUtils","JS","Web","uPromises","uDOM"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TLogin",pas.System.TObject,function () {
    this.onLogout = function () {
      var Self = this;
      function LoadInitialPage() {
        function _doTask1(aResponse) {
          var Result = undefined;
          pas.uMain.Application.FmainView.router.loadPage("index.html");
          Result = pas.uPromises.wait(600);
          return Result;
        };
        function _doTask2(aResponse) {
          var Result = undefined;
          var j = 0;
          for (j = 1; j <= 3; j++) window.Dom7(".toolbar .link").eq(j).hide();
          Result = pas.uPromises.wait(2000);
          return Result;
        };
        Promise.resolve().then(_doTask1).then(_doTask2);
      };
      function confirmYES() {
        pas.uMain.Application.FmyApp.closeModal();
        LoadInitialPage();
      };
      function confirmCANCEL() {
        pas.uMain.Application.FmainView.router.back();
      };
      function onLogoutCallback(e) {
        var Result = undefined;
        window.Dom7(".toolbar .link").eq(0).removeAttr("style");
        pas.uMain.Application.FmyApp.confirm("Are you sure Logout?",confirmYES,confirmCANCEL);
        return Result;
      };
      window.Dom7(".logout-button").on("click",onLogoutCallback);
    };
    this.checkLogin = function () {
      var username = undefined;
      var password = undefined;
      var j = 0;
      var wrongLogin = false;
      var modalText = "";
      if (!wrongLogin) {
        modalText = "Wrong username or password"}
       else modalText = "Login with username and password";
      pas.uMain.Application.FmyApp.showIndicator();
      username = window.Dom7($impl.user).val();
      password = window.Dom7($impl.pass).val();
      if ((username == this.DecryptStr("àÂædÔæ",2)) && (password == this.DecryptStr("bdfh",2))) {
        window.console.log("Login successfully");
        window.Dom7(".toolbar .link").eq(0).hide();
        for (j = 1; j <= 3; j++) window.Dom7(".toolbar .link").eq(j).removeAttr("style");
        pas.uMain.Application.FmyApp.closeModal();
        pas.uMain.Application.FmyApp.hideIndicator();
        pas.uMain.Application.FmainView.router.loadPage("pages\/protected.html");
        pas.uMain.Application.FmyApp.hideIndicator();
        return;
      } else {
        window.console.log("Login failed - Please enter correct Username and Password");
        pas.uMain.Application.FmyApp.alert(modalText);
        pas.uMain.Application.FmyApp.hideIndicator();
      };
    };
    this.DecryptStr = function (S, Key) {
      var Result = "";
      var I = 0;
      var fuck = "";
      for (var $l1 = 1, $end2 = S.length; $l1 <= $end2; $l1++) {
        I = $l1;
        fuck = fuck + String.fromCharCode(Math.floor(S.charCodeAt(I - 1) / Key));
      };
      Result = fuck;
      return Result;
    };
    this.InitializeObject = function () {
      var Self = this;
      var Result = null;
      function bindEvents() {
        pas.System.Writeln("login was fired");
        Self.checkLogin();
        Self.onLogout();
      };
      window.Dom7($impl.BtnSign).on("click",bindEvents,null);
      return Result;
    };
  });
},["uMain"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.user = 'input[name="username"]';
  $impl.pass = 'input[name="password"]';
  $impl.BtnSign = ".sign-login-screen";
});
rtl.module("uFW7",["System","Classes","SysUtils","Types","JS","Web"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("uMain",["System","Classes","SysUtils","JS","Web","uLogin","uFW7","uDOM","uPromises"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TApplication",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FmyApp = null;
      this.FmainView = null;
      this.FLogin = null;
    };
    this.$final = function () {
      this.FmyApp = undefined;
      this.FmainView = undefined;
      this.FLogin = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.InitApp = function () {
      var Self = this;
      function ProcessGoAnimation() {
        function _doTask1(aResponse) {
          var Result = undefined;
          window.Dom7("#cover").addClass("go");
          Result = pas.uPromises.wait(660);
          return Result;
        };
        function _doTask2(aResponse) {
          var Result = undefined;
          window.Dom7("#cover").removeClass("go");
          Result = pas.uPromises.wait(3300);
          return Result;
        };
        Promise.resolve().then(_doTask1).then(_doTask2);
      };
      function toggleClassGo(ev) {
        window.Dom7(this).toggleClass("go");
      };
      function indexInit(page) {
        window.console.log("Index page beforeInit");
        ProcessGoAnimation();
        window.Dom7("#cover").mouseout(toggleClassGo);
        Self.GetLogin().InitializeObject();
      };
      function aboutInit(page) {
        window.console.log("onPageInit Option1 ABOUT");
      };
      function pageAboutInit(event) {
        var Result = undefined;
        window.console.log("onPageInit Option3 ABOUT");
        return Result;
      };
      function pageBeforeInit(event) {
        var Result = undefined;
        var page = null;
        var j = 0;
        function blogPageHTML() {
          var html = "";
          var i = 0;
          i += 1;
          if (i < 5) {
            html = "";
            html += '<div class="card post-card"><div class="card-header"><div class="post-avatar"><img src="res\/images\/photos\/avatar.jpg" width="34" height="34"><\/div><div class="post-name">John Doe<\/div><div class="post-date">Monday at 2:15 PM<\/div><\/div><div class="card-content"><div class="card-content-inner"><h4 class="title-post"><a href="single.html">The eye should learn to listen before it looks.<\/a><\/h4><img src="res\/images\/photos\/5.jpg" width="100%" class="img"><p>What a nice photo i took yesterday! Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Proin et augue nec ex facilisis pulvinar.<\/p><p class="color-gray infos">Likes: 112  -  Comments: 43<\/p><\/div><\/div><div class="card-footer no-border"><a href="#" class="link">Like<\/a><a href="#" class="link">Comment<\/a><a href="#" class="link">Share<\/a><\/div><\/div>';
            window.Dom7("#blog-page").append(html);
          } else window.Dom7("#loadmore").hide();
        };
        page = event.detail.page;
        var $tmp1 = page.name;
        if ($tmp1 === "index") {
          window.console.log("index page initialized");
          for (j = 0; j <= 3; j++) {
            window.Dom7(".toolbar .link").eq(j).show();
          };
        } else if ($tmp1 === "protected") {
          window.console.log("protected page initialized");
        } else if ($tmp1 === "features") {
          window.console.log("features page initialized");
        } else if ($tmp1 === "gallery") {
          window.console.log("gallery page initialized");
        } else if ($tmp1 === "blog") {
          window.console.log("blog page initialized");
          window.Dom7("#loadmore").click(blogPageHTML);
        } else if ($tmp1 === "about") {
          pas.System.Writeln("about page initialized");
        } else if ($tmp1 === "tabs") {
          pas.System.Writeln("tabs page initialized");
        } else if ($tmp1 === "toggle") {
          pas.System.Writeln("toggle page initialized");
        } else if ($tmp1 === "contact") {
          pas.System.Writeln("contact page initialized");
        } else if ($tmp1 === "videos") {
          pas.System.Writeln("videos page initialized");
        } else if ($tmp1 === "socials") {
          pas.System.Writeln("socials page initialized");
        };
        return Result;
      };
      window.console.log("App initialization");
      Self.FmyApp = new Framework7($impl.optParams());
      Self.FmainView = Self.FmyApp.addView(".view-main",$impl.addViewParams());
      Self.FmyApp.onPageBeforeInit("index",indexInit).trigger();
      Self.FmyApp.onPageInit("about",aboutInit);
      window.Dom7(document).on("pageInit",'.page[data-page="about"]',pageAboutInit);
      window.Dom7(document).on("pageBeforeInit",pageBeforeInit,null);
    };
    this.GetLogin = function () {
      var Result = null;
      if (this.FLogin === null) this.FLogin = pas.uLogin.TLogin.$create("Create");
      Result = this.FLogin;
      return Result;
    };
    this.RunApp = function () {
      var Self = this;
      $impl.buildHTML();
      Self.InitApp();
    };
    var $r = this.$rtti;
    $r.addMethod("RunApp",0,null);
    $r.addProperty("mainView",0,pas.uFW7.$rtti["JFW7"],"FmainView","FmainView");
    $r.addProperty("myApp",0,pas.uFW7.$rtti["JFW7"],"FmyApp","FmyApp");
    $r.addProperty("Login",1,pas.uLogin.$rtti["TLogin"],"GetLogin","");
  });
  this.Application = null;
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.optParams = function () {
    var Result = null;
    Result = pas.JS.New(["modalTitle","My App","animateNavBackIcon","true"]);
    return Result;
  };
  $impl.addViewParams = function () {
    var Result = null;
    Result = pas.JS.New(["dynamicNavbar",true,"domCache","true"]);
    return Result;
  };
  $impl.buildHTML = function () {
    var docFragment = null;
    var comment = null;
    var comment_0 = null;
    var comment_1 = null;
    var comment_2 = null;
    var comment_3 = null;
    var comment_4 = null;
    var comment_5 = null;
    var comment_6 = null;
    var comment_7 = null;
    var comment_8 = null;
    var comment_9 = null;
    var comment_10 = null;
    var comment_11 = null;
    var comment_12 = null;
    var comment_13 = null;
    var comment_14 = null;
    var comment_15 = null;
    var comment_16 = null;
    var comment_17 = null;
    var text = null;
    var text_0 = null;
    var text_1 = null;
    var text_2 = null;
    var text_3 = null;
    var text_4 = null;
    var text_5 = null;
    var text_6 = null;
    var text_7 = null;
    var text_8 = null;
    var text_9 = null;
    var text_10 = null;
    var text_11 = null;
    var text_12 = null;
    var text_13 = null;
    var text_14 = null;
    var text_15 = null;
    var text_16 = null;
    var text_17 = null;
    var text_18 = null;
    var text_19 = null;
    var text_20 = null;
    var text_21 = null;
    var text_22 = null;
    var text_23 = null;
    var text_24 = null;
    var text_25 = null;
    var text_26 = null;
    var text_27 = null;
    var text_28 = null;
    var text_29 = null;
    var text_30 = null;
    var text_31 = null;
    var text_32 = null;
    var div1 = null;
    var div_0 = null;
    var div_1 = null;
    var div_2 = null;
    var div_3 = null;
    var div_4 = null;
    var div_5 = null;
    var div_6 = null;
    var div_7 = null;
    var div_8 = null;
    var div_9 = null;
    var div_10 = null;
    var div_11 = null;
    var div_12 = null;
    var div_13 = null;
    var div_14 = null;
    var div_15 = null;
    var div_16 = null;
    var div_17 = null;
    var div_18 = null;
    var div_19 = null;
    var div_20 = null;
    var div_21 = null;
    var div_22 = null;
    var div_23 = null;
    var div_24 = null;
    var div_25 = null;
    var div_26 = null;
    var div_27 = null;
    var div_28 = null;
    var div_29 = null;
    var div_30 = null;
    var div_31 = null;
    var div_32 = null;
    var div_33 = null;
    var div_34 = null;
    var div_35 = null;
    var div_36 = null;
    var div_37 = null;
    var div_38 = null;
    var div_39 = null;
    var div_40 = null;
    var div_41 = null;
    var div_42 = null;
    var div_43 = null;
    var div_44 = null;
    var div_45 = null;
    var div_46 = null;
    var div_47 = null;
    var div_48 = null;
    var div_49 = null;
    var div_50 = null;
    var div_51 = null;
    var div_52 = null;
    var div_53 = null;
    var div_54 = null;
    var div_55 = null;
    var div_56 = null;
    var div_57 = null;
    var div_58 = null;
    var div_59 = null;
    var div_60 = null;
    var div_61 = null;
    var div_62 = null;
    var ul = null;
    var ul_0 = null;
    var ul_1 = null;
    var ul_2 = null;
    var ul_3 = null;
    var ul_4 = null;
    var ul_5 = null;
    var ul_6 = null;
    var ul_7 = null;
    var li = null;
    var li_0 = null;
    var li_1 = null;
    var li_2 = null;
    var li_3 = null;
    var li_4 = null;
    var li_5 = null;
    var li_6 = null;
    var li_7 = null;
    var li_8 = null;
    var li_9 = null;
    var li_10 = null;
    var li_11 = null;
    var li_12 = null;
    var li_13 = null;
    var li_14 = null;
    var li_15 = null;
    var li_16 = null;
    var li_17 = null;
    var li_18 = null;
    var li_19 = null;
    var li_20 = null;
    var br = null;
    var br_0 = null;
    var br_1 = null;
    var br_2 = null;
    var a = null;
    var a_0 = null;
    var a_1 = null;
    var a_2 = null;
    var a_3 = null;
    var a_4 = null;
    var a_5 = null;
    var a_6 = null;
    var a_7 = null;
    var a_8 = null;
    var a_9 = null;
    var a_10 = null;
    var a_11 = null;
    var a_12 = null;
    var a_13 = null;
    var a_14 = null;
    var a_15 = null;
    var a_16 = null;
    var a_17 = null;
    var a_18 = null;
    var a_19 = null;
    var a_20 = null;
    var a_21 = null;
    var a_22 = null;
    var a_23 = null;
    var a_24 = null;
    var img = null;
    var img_0 = null;
    var img_1 = null;
    var img_2 = null;
    var img_3 = null;
    var img_4 = null;
    var img_5 = null;
    var img_6 = null;
    var img_7 = null;
    var img_8 = null;
    var img_9 = null;
    var img_10 = null;
    var img_11 = null;
    var img_12 = null;
    var form = null;
    var form_0 = null;
    var form_1 = null;
    var form_2 = null;
    var input = null;
    var input_0 = null;
    var input_1 = null;
    var input_2 = null;
    var input_3 = null;
    var input_4 = null;
    var p = null;
    var p_0 = null;
    var cover = null;
    var h6 = null;
    var home = null;
    var span = null;
    var span_0 = null;
    var span_1 = null;
    var span_2 = null;
    var span_3 = null;
    var span_4 = null;
    var span_5 = null;
    var span_6 = null;
    docFragment = document.createDocumentFragment();
    div1 = document.createElement("DIV");
    div1.setAttribute("class","statusbar-overlay");
    docFragment.appendChild(div1);
    comment = document.createComment(" Panels overlay");
    docFragment.appendChild(comment);
    div_0 = document.createElement("DIV");
    div_0.setAttribute("class","panel-overlay");
    docFragment.appendChild(div_0);
    comment_0 = document.createComment(" Left panel with reveal effect");
    docFragment.appendChild(comment_0);
    div_1 = document.createElement("DIV");
    div_1.setAttribute("class","panel panel-left panel-cover");
    docFragment.appendChild(div_1);
    div_2 = document.createElement("DIV");
    div_2.setAttribute("class","content-block");
    div_1.appendChild(div_2);
    ul = document.createElement("UL");
    ul.setAttribute("class","panel-content");
    div_2.appendChild(ul);
    li = document.createElement("LI");
    li.setAttribute("class","panel-link");
    ul.appendChild(li);
    a = document.createElement("A");
    a.setAttribute("href","pages\/about.html");
    a.setAttribute("class","close-panel");
    li.appendChild(a);
    img = document.createElement("IMG");
    img.setAttribute("src","res\/images\/icons\/white\/user.png");
    img.setAttribute("alt","about");
    a.appendChild(img);
    text = document.createTextNode("About");
    a.appendChild(text);
    li_0 = document.createElement("LI");
    li_0.setAttribute("class","panel-link");
    ul.appendChild(li_0);
    a_0 = document.createElement("A");
    a_0.setAttribute("href","pages\/features.html");
    a_0.setAttribute("class","close-panel");
    li_0.appendChild(a_0);
    img_0 = document.createElement("IMG");
    img_0.setAttribute("src","res\/images\/icons\/white\/features.png");
    img_0.setAttribute("alt","about");
    a_0.appendChild(img_0);
    text_0 = document.createTextNode("Features");
    a_0.appendChild(text_0);
    li_1 = document.createElement("LI");
    li_1.setAttribute("class","panel-link");
    ul.appendChild(li_1);
    a_1 = document.createElement("A");
    a_1.setAttribute("href","pages\/gallery.html");
    a_1.setAttribute("class","close-panel");
    li_1.appendChild(a_1);
    img_1 = document.createElement("IMG");
    img_1.setAttribute("src","res\/images\/icons\/white\/gallery.png");
    img_1.setAttribute("alt","about");
    a_1.appendChild(img_1);
    text_1 = document.createTextNode("Gallery");
    a_1.appendChild(text_1);
    li_2 = document.createElement("LI");
    li_2.setAttribute("class","panel-link");
    ul.appendChild(li_2);
    a_2 = document.createElement("A");
    a_2.setAttribute("href","pages\/videos.html");
    a_2.setAttribute("class","close-panel");
    li_2.appendChild(a_2);
    img_2 = document.createElement("IMG");
    img_2.setAttribute("src","res\/images\/icons\/white\/video.png");
    img_2.setAttribute("alt","about");
    a_2.appendChild(img_2);
    text_2 = document.createTextNode("Videos");
    a_2.appendChild(text_2);
    li_3 = document.createElement("LI");
    li_3.setAttribute("class","panel-link");
    ul.appendChild(li_3);
    a_3 = document.createElement("A");
    a_3.setAttribute("href","pages\/blog.html");
    a_3.setAttribute("class","close-panel");
    li_3.appendChild(a_3);
    img_3 = document.createElement("IMG");
    img_3.setAttribute("src","res\/images\/icons\/white\/blog.png");
    img_3.setAttribute("alt","about");
    a_3.appendChild(img_3);
    text_3 = document.createTextNode("blog");
    a_3.appendChild(text_3);
    li_4 = document.createElement("LI");
    li_4.setAttribute("class","panel-link");
    ul.appendChild(li_4);
    a_4 = document.createElement("A");
    a_4.setAttribute("href","pages\/contact.html");
    a_4.setAttribute("class","close-panel");
    li_4.appendChild(a_4);
    img_4 = document.createElement("IMG");
    img_4.setAttribute("src","res\/images\/icons\/white\/contact.png");
    img_4.setAttribute("alt","about");
    a_4.appendChild(img_4);
    text_4 = document.createTextNode("contact");
    a_4.appendChild(text_4);
    comment_1 = document.createComment(" Right panel with cover effect");
    docFragment.appendChild(comment_1);
    div_3 = document.createElement("DIV");
    div_3.setAttribute("class","panel panel-right panel-cover");
    docFragment.appendChild(div_3);
    div_4 = document.createElement("DIV");
    div_4.setAttribute("class","content-block");
    div_3.appendChild(div_4);
    div_5 = document.createElement("DIV");
    div_5.setAttribute("class","panel-cover");
    div_4.appendChild(div_5);
    div_6 = document.createElement("DIV");
    div_6.setAttribute("class","cover");
    div_5.appendChild(div_6);
    div_7 = document.createElement("DIV");
    div_7.setAttribute("class","cover-text");
    div_5.appendChild(div_7);
    text_5 = document.createTextNode("Hi, Pas2JS\\n          ");
    div_7.appendChild(text_5);
    span = document.createElement("SPAN");
    div_7.appendChild(span);
    text_6 = document.createTextNode("warleyalex designer");
    span.appendChild(text_6);
    ul_0 = document.createElement("UL");
    ul_0.setAttribute("class","panel-content");
    div_4.appendChild(ul_0);
    li_5 = document.createElement("LI");
    li_5.setAttribute("class","panel-link");
    ul_0.appendChild(li_5);
    a_5 = document.createElement("A");
    a_5.setAttribute("href","pages\/about.html");
    a_5.setAttribute("class","close-panel");
    li_5.appendChild(a_5);
    text_7 = document.createTextNode("Profile");
    a_5.appendChild(text_7);
    li_6 = document.createElement("LI");
    li_6.setAttribute("class","panel-link");
    ul_0.appendChild(li_6);
    a_6 = document.createElement("A");
    a_6.setAttribute("href","#");
    a_6.setAttribute("class","close-panel");
    li_6.appendChild(a_6);
    text_8 = document.createTextNode("Settings");
    a_6.appendChild(text_8);
    li_7 = document.createElement("LI");
    li_7.setAttribute("class","panel-link");
    ul_0.appendChild(li_7);
    a_7 = document.createElement("A");
    a_7.setAttribute("href","#");
    a_7.setAttribute("class","close-panel");
    li_7.appendChild(a_7);
    text_9 = document.createTextNode("Account");
    a_7.appendChild(text_9);
    li_8 = document.createElement("LI");
    li_8.setAttribute("class","panel-link");
    ul_0.appendChild(li_8);
    a_8 = document.createElement("A");
    a_8.setAttribute("href","#");
    a_8.setAttribute("class","close-panel");
    li_8.appendChild(a_8);
    text_10 = document.createTextNode("Messages ");
    a_8.appendChild(text_10);
    span_0 = document.createElement("SPAN");
    span_0.setAttribute("class","badge bg-red");
    a_8.appendChild(span_0);
    text_11 = document.createTextNode("5");
    span_0.appendChild(text_11);
    li_9 = document.createElement("LI");
    li_9.setAttribute("class","panel-link");
    ul_0.appendChild(li_9);
    a_9 = document.createElement("A");
    a_9.setAttribute("href","#");
    a_9.setAttribute("class","close-panel");
    li_9.appendChild(a_9);
    text_12 = document.createTextNode("Favourites");
    a_9.appendChild(text_12);
    li_10 = document.createElement("LI");
    li_10.setAttribute("class","panel-link");
    ul_0.appendChild(li_10);
    a_10 = document.createElement("A");
    a_10.setAttribute("href","#");
    a_10.setAttribute("class","close-panel");
    li_10.appendChild(a_10);
    text_13 = document.createTextNode("Logout");
    a_10.appendChild(text_13);
    comment_2 = document.createComment(" Views");
    docFragment.appendChild(comment_2);
    div_8 = document.createElement("DIV");
    div_8.setAttribute("class","views");
    docFragment.appendChild(div_8);
    comment_3 = document.createComment(" Your main view, should have 'view-main' class");
    div_8.appendChild(comment_3);
    div_9 = document.createElement("DIV");
    div_9.setAttribute("class","view view-main");
    div_8.appendChild(div_9);
    comment_4 = document.createComment(" Top Navbar");
    div_9.appendChild(comment_4);
    div_10 = document.createElement("DIV");
    div_10.setAttribute("class","navbar navbar-hidden");
    div_9.appendChild(div_10);
    div_11 = document.createElement("DIV");
    div_11.setAttribute("class","navbar-inner");
    div_10.appendChild(div_11);
    comment_5 = document.createComment(" Pages, because we need fixed-through navbar and toolbar, it has additional appropriate classes");
    div_9.appendChild(comment_5);
    div_12 = document.createElement("DIV");
    div_12.setAttribute("class","pages navbar-through toolbar-through");
    div_9.appendChild(div_12);
    comment_6 = document.createComment(" Page, data-page contains page name");
    div_12.appendChild(comment_6);
    home = document.createElement("DIV");
    home.setAttribute("data-page","index");
    home.setAttribute("class","page no-navbar");
    home.setAttribute("id","home");
    div_12.appendChild(home);
    comment_7 = document.createComment(" Scrollable page content");
    home.appendChild(comment_7);
    div_13 = document.createElement("DIV");
    div_13.setAttribute("class","page-content");
    home.appendChild(div_13);
    cover = document.createElement("DIV");
    cover.setAttribute("id","cover");
    div_13.appendChild(cover);
    h6 = document.createElement("H6");
    cover.appendChild(h6);
    span_1 = document.createElement("SPAN");
    span_1.setAttribute("class","e");
    h6.appendChild(span_1);
    span_2 = document.createElement("SPAN");
    span_2.setAttribute("class","e");
    h6.appendChild(span_2);
    span_3 = document.createElement("SPAN");
    span_3.setAttribute("class","e");
    h6.appendChild(span_3);
    span_4 = document.createElement("SPAN");
    span_4.setAttribute("class","e");
    h6.appendChild(span_4);
    span_5 = document.createElement("SPAN");
    span_5.setAttribute("class","e");
    h6.appendChild(span_5);
    span_6 = document.createElement("SPAN");
    span_6.setAttribute("class","e");
    h6.appendChild(span_6);
    div_14 = document.createElement("DIV");
    div_14.setAttribute("class","content");
    div_13.appendChild(div_14);
    comment_8 = document.createComment(" Bottom Toolbar");
    div_9.appendChild(comment_8);
    div_15 = document.createElement("DIV");
    div_15.setAttribute("class","toolbar tabbar");
    div_9.appendChild(div_15);
    div_16 = document.createElement("DIV");
    div_16.setAttribute("class","toolbar-inner");
    div_15.appendChild(div_16);
    a_11 = document.createElement("A");
    a_11.setAttribute("href","#");
    a_11.setAttribute("class","link open-login-screen");
    div_16.appendChild(a_11);
    img_5 = document.createElement("IMG");
    img_5.setAttribute("src","res\/images\/icons\/white\/login.png");
    img_5.setAttribute("alt","login");
    a_11.appendChild(img_5);
    a_12 = document.createElement("A");
    a_12.setAttribute("href","pages\/features.html");
    a_12.setAttribute("class","link");
    a_12.setAttribute("style","display:none;");
    div_16.appendChild(a_12);
    img_6 = document.createElement("IMG");
    img_6.setAttribute("src","res\/images\/icons\/white\/features.png");
    img_6.setAttribute("alt","features");
    a_12.appendChild(img_6);
    a_13 = document.createElement("A");
    a_13.setAttribute("href","pages\/gallery.html");
    a_13.setAttribute("class","link");
    a_13.setAttribute("style","display:none;");
    div_16.appendChild(a_13);
    img_7 = document.createElement("IMG");
    img_7.setAttribute("src","res\/images\/icons\/white\/gallery.png");
    img_7.setAttribute("alt","gallery");
    a_13.appendChild(img_7);
    a_14 = document.createElement("A");
    a_14.setAttribute("href","pages\/blog.html");
    a_14.setAttribute("style","display:none;");
    a_14.setAttribute("class","link");
    div_16.appendChild(a_14);
    img_8 = document.createElement("IMG");
    img_8.setAttribute("src","res\/images\/icons\/white\/blog.png");
    img_8.setAttribute("alt","blog");
    a_14.appendChild(img_8);
    comment_9 = document.createComment(" Login Screen ");
    docFragment.appendChild(comment_9);
    div_17 = document.createElement("DIV");
    div_17.setAttribute("class","login-screen");
    docFragment.appendChild(div_17);
    comment_10 = document.createComment(" Default view-page layout ");
    div_17.appendChild(comment_10);
    div_18 = document.createElement("DIV");
    div_18.setAttribute("class","view");
    div_17.appendChild(div_18);
    div_19 = document.createElement("DIV");
    div_19.setAttribute("class","page");
    div_18.appendChild(div_19);
    comment_11 = document.createComment(" page-content has additional login-screen content ");
    div_19.appendChild(comment_11);
    div_20 = document.createElement("DIV");
    div_20.setAttribute("class","page-content login-screen-content");
    div_19.appendChild(div_20);
    div_21 = document.createElement("DIV");
    div_21.setAttribute("class","login-screen-title");
    div_20.appendChild(div_21);
    img_9 = document.createElement("IMG");
    img_9.setAttribute("src","res\/images\/logo-black.png");
    img_9.setAttribute("alt","MyApplication");
    div_21.appendChild(img_9);
    comment_12 = document.createComment(" Login form ");
    div_20.appendChild(comment_12);
    form = document.createElement("FORM");
    div_20.appendChild(form);
    div_22 = document.createElement("DIV");
    div_22.setAttribute("class","list-block");
    form.appendChild(div_22);
    ul_1 = document.createElement("UL");
    div_22.appendChild(ul_1);
    li_11 = document.createElement("LI");
    li_11.setAttribute("class","item-content");
    ul_1.appendChild(li_11);
    div_23 = document.createElement("DIV");
    div_23.setAttribute("class","item-inner");
    li_11.appendChild(div_23);
    div_24 = document.createElement("DIV");
    div_24.setAttribute("class","item-title label");
    div_23.appendChild(div_24);
    text_14 = document.createTextNode("Username");
    div_24.appendChild(text_14);
    div_25 = document.createElement("DIV");
    div_25.setAttribute("class","item-input");
    div_23.appendChild(div_25);
    input = document.createElement("INPUT");
    input.setAttribute("type","text");
    input.setAttribute("name","username");
    input.setAttribute("placeholder","Username");
    div_25.appendChild(input);
    li_12 = document.createElement("LI");
    li_12.setAttribute("class","item-content");
    ul_1.appendChild(li_12);
    div_26 = document.createElement("DIV");
    div_26.setAttribute("class","item-inner");
    li_12.appendChild(div_26);
    div_27 = document.createElement("DIV");
    div_27.setAttribute("class","item-title label");
    div_26.appendChild(div_27);
    text_15 = document.createTextNode("Password ");
    div_27.appendChild(text_15);
    p = document.createElement("P");
    p.setAttribute("class","forget-password");
    div_27.appendChild(p);
    a_15 = document.createElement("A");
    a_15.setAttribute("href","#");
    a_15.setAttribute("data-popup",".popup-forgot-password");
    a_15.setAttribute("class","open-popup");
    p.appendChild(a_15);
    text_16 = document.createTextNode("Forgot Password?");
    a_15.appendChild(text_16);
    div_28 = document.createElement("DIV");
    div_28.setAttribute("class","item-input");
    div_26.appendChild(div_28);
    input_0 = document.createElement("INPUT");
    input_0.setAttribute("type","password");
    input_0.setAttribute("name","password");
    input_0.setAttribute("placeholder","Password");
    div_28.appendChild(input_0);
    div_29 = document.createElement("DIV");
    div_29.setAttribute("class","list-block");
    form.appendChild(div_29);
    ul_2 = document.createElement("UL");
    div_29.appendChild(ul_2);
    li_13 = document.createElement("LI");
    ul_2.appendChild(li_13);
    div_30 = document.createElement("DIV");
    div_30.setAttribute("class","content-block");
    li_13.appendChild(div_30);
    a_16 = document.createElement("A");
    a_16.setAttribute("href","#");
    a_16.setAttribute("class","item-link sign-login-screen button button-fill color-blue");
    div_30.appendChild(a_16);
    text_17 = document.createTextNode("Sign In");
    a_16.appendChild(text_17);
    div_31 = document.createElement("DIV");
    div_31.setAttribute("class","list-block-label");
    div_29.appendChild(div_31);
    text_18 = document.createTextNode("Don't have an account?\\n                ");
    div_31.appendChild(text_18);
    p_0 = document.createElement("P");
    div_31.appendChild(p_0);
    a_17 = document.createElement("A");
    a_17.setAttribute("href","#");
    a_17.setAttribute("data-popup",".popup-register");
    a_17.setAttribute("class","open-popup");
    p_0.appendChild(a_17);
    text_19 = document.createTextNode("Sign up");
    a_17.appendChild(text_19);
    br = document.createElement("BR");
    div_31.appendChild(br);
    a_18 = document.createElement("A");
    a_18.setAttribute("href","#");
    a_18.setAttribute("class","close-login-screen button button-fill color-red");
    div_31.appendChild(a_18);
    text_20 = document.createTextNode("Close");
    a_18.appendChild(text_20);
    comment_13 = document.createComment(" Register Popup ");
    docFragment.appendChild(comment_13);
    div_32 = document.createElement("DIV");
    div_32.setAttribute("class","popup popup-register");
    docFragment.appendChild(div_32);
    div_33 = document.createElement("DIV");
    div_33.setAttribute("class","page-content login-screen-content");
    div_32.appendChild(div_33);
    div_34 = document.createElement("DIV");
    div_34.setAttribute("class","login-screen-title");
    div_33.appendChild(div_34);
    img_10 = document.createElement("IMG");
    img_10.setAttribute("src","res\/images\/logo-black.png");
    img_10.setAttribute("alt","MyApplication");
    div_34.appendChild(img_10);
    comment_14 = document.createComment(" Login form ");
    div_33.appendChild(comment_14);
    form_0 = document.createElement("FORM");
    div_33.appendChild(form_0);
    div_35 = document.createElement("DIV");
    div_35.setAttribute("class","list-block");
    form_0.appendChild(div_35);
    ul_3 = document.createElement("UL");
    div_35.appendChild(ul_3);
    li_14 = document.createElement("LI");
    li_14.setAttribute("class","item-content");
    ul_3.appendChild(li_14);
    div_36 = document.createElement("DIV");
    div_36.setAttribute("class","item-inner");
    li_14.appendChild(div_36);
    div_37 = document.createElement("DIV");
    div_37.setAttribute("class","item-title label");
    div_36.appendChild(div_37);
    text_21 = document.createTextNode("Username");
    div_37.appendChild(text_21);
    div_38 = document.createElement("DIV");
    div_38.setAttribute("class","item-input");
    div_36.appendChild(div_38);
    input_1 = document.createElement("INPUT");
    input_1.setAttribute("type","text");
    input_1.setAttribute("name","username");
    input_1.setAttribute("placeholder","Username");
    div_38.appendChild(input_1);
    li_15 = document.createElement("LI");
    li_15.setAttribute("class","item-content");
    ul_3.appendChild(li_15);
    div_39 = document.createElement("DIV");
    div_39.setAttribute("class","item-inner");
    li_15.appendChild(div_39);
    div_40 = document.createElement("DIV");
    div_40.setAttribute("class","item-title label");
    div_39.appendChild(div_40);
    text_22 = document.createTextNode("Email");
    div_40.appendChild(text_22);
    div_41 = document.createElement("DIV");
    div_41.setAttribute("class","item-input");
    div_39.appendChild(div_41);
    input_2 = document.createElement("INPUT");
    input_2.setAttribute("type","email");
    input_2.setAttribute("name","email");
    input_2.setAttribute("placeholder","Email");
    div_41.appendChild(input_2);
    li_16 = document.createElement("LI");
    li_16.setAttribute("class","item-content");
    ul_3.appendChild(li_16);
    div_42 = document.createElement("DIV");
    div_42.setAttribute("class","item-inner");
    li_16.appendChild(div_42);
    div_43 = document.createElement("DIV");
    div_43.setAttribute("class","item-title label");
    div_42.appendChild(div_43);
    text_23 = document.createTextNode("Password");
    div_43.appendChild(text_23);
    div_44 = document.createElement("DIV");
    div_44.setAttribute("class","item-input");
    div_42.appendChild(div_44);
    input_3 = document.createElement("INPUT");
    input_3.setAttribute("type","password");
    input_3.setAttribute("name","password");
    input_3.setAttribute("placeholder","Password");
    div_44.appendChild(input_3);
    div_45 = document.createElement("DIV");
    div_45.setAttribute("class","list-block");
    form_0.appendChild(div_45);
    ul_4 = document.createElement("UL");
    div_45.appendChild(ul_4);
    li_17 = document.createElement("LI");
    ul_4.appendChild(li_17);
    div_46 = document.createElement("DIV");
    div_46.setAttribute("class","content-block");
    li_17.appendChild(div_46);
    a_19 = document.createElement("A");
    a_19.setAttribute("href","#");
    a_19.setAttribute("class","item-link button button-fill color-blue");
    div_46.appendChild(a_19);
    text_24 = document.createTextNode("Register now");
    a_19.appendChild(text_24);
    br_0 = document.createElement("BR");
    div_46.appendChild(br_0);
    a_20 = document.createElement("A");
    a_20.setAttribute("href","#");
    a_20.setAttribute("class","button button-fill color-red close-popup");
    div_46.appendChild(a_20);
    text_25 = document.createTextNode("Close");
    a_20.appendChild(text_25);
    comment_15 = document.createComment(" Forgot password Popup ");
    docFragment.appendChild(comment_15);
    div_47 = document.createElement("DIV");
    div_47.setAttribute("class","popup popup-forgot-password");
    docFragment.appendChild(div_47);
    div_48 = document.createElement("DIV");
    div_48.setAttribute("class","page-content login-screen-content");
    div_47.appendChild(div_48);
    div_49 = document.createElement("DIV");
    div_49.setAttribute("class","login-screen-title");
    div_48.appendChild(div_49);
    img_11 = document.createElement("IMG");
    img_11.setAttribute("src","res\/images\/logo-black.png");
    img_11.setAttribute("alt","MyApplication");
    div_49.appendChild(img_11);
    comment_16 = document.createComment(" Login form ");
    div_48.appendChild(comment_16);
    form_1 = document.createElement("FORM");
    div_48.appendChild(form_1);
    div_50 = document.createElement("DIV");
    div_50.setAttribute("class","list-block");
    form_1.appendChild(div_50);
    ul_5 = document.createElement("UL");
    div_50.appendChild(ul_5);
    li_18 = document.createElement("LI");
    li_18.setAttribute("class","item-content");
    ul_5.appendChild(li_18);
    div_51 = document.createElement("DIV");
    div_51.setAttribute("class","item-inner");
    li_18.appendChild(div_51);
    div_52 = document.createElement("DIV");
    div_52.setAttribute("class","item-title label");
    div_51.appendChild(div_52);
    text_26 = document.createTextNode("Your Email");
    div_52.appendChild(text_26);
    div_53 = document.createElement("DIV");
    div_53.setAttribute("class","item-input");
    div_51.appendChild(div_53);
    input_4 = document.createElement("INPUT");
    input_4.setAttribute("type","email");
    input_4.setAttribute("name","email");
    input_4.setAttribute("placeholder","Email");
    div_53.appendChild(input_4);
    div_54 = document.createElement("DIV");
    div_54.setAttribute("class","list-block");
    form_1.appendChild(div_54);
    ul_6 = document.createElement("UL");
    div_54.appendChild(ul_6);
    li_19 = document.createElement("LI");
    ul_6.appendChild(li_19);
    div_55 = document.createElement("DIV");
    div_55.setAttribute("class","content-block");
    li_19.appendChild(div_55);
    a_21 = document.createElement("A");
    a_21.setAttribute("href","#");
    a_21.setAttribute("class","item-link button button-fill color-blue");
    div_55.appendChild(a_21);
    text_27 = document.createTextNode("Resend password");
    a_21.appendChild(text_27);
    br_1 = document.createElement("BR");
    div_55.appendChild(br_1);
    a_22 = document.createElement("A");
    a_22.setAttribute("href","#");
    a_22.setAttribute("class","button button-fill color-red close-popup");
    div_55.appendChild(a_22);
    text_28 = document.createTextNode("Close");
    a_22.appendChild(text_28);
    div_56 = document.createElement("DIV");
    div_56.setAttribute("class","list-block-label");
    div_54.appendChild(div_56);
    text_29 = document.createTextNode("Check your email and follow the instructions to reset your password.");
    div_56.appendChild(text_29);
    comment_17 = document.createComment(" Logout Popup ");
    docFragment.appendChild(comment_17);
    div_57 = document.createElement("DIV");
    div_57.setAttribute("class","popup popup-logout");
    docFragment.appendChild(div_57);
    div_58 = document.createElement("DIV");
    div_58.setAttribute("class","page-content login-screen-content");
    div_57.appendChild(div_58);
    div_59 = document.createElement("DIV");
    div_59.setAttribute("class","login-screen-title");
    div_58.appendChild(div_59);
    img_12 = document.createElement("IMG");
    img_12.setAttribute("src","res\/images\/logo-black.png");
    img_12.setAttribute("alt","MyApplication");
    div_59.appendChild(img_12);
    form_2 = document.createElement("FORM");
    div_58.appendChild(form_2);
    div_60 = document.createElement("DIV");
    div_60.setAttribute("class","list-block");
    form_2.appendChild(div_60);
    ul_7 = document.createElement("UL");
    div_60.appendChild(ul_7);
    li_20 = document.createElement("LI");
    ul_7.appendChild(li_20);
    div_61 = document.createElement("DIV");
    div_61.setAttribute("class","content-block");
    li_20.appendChild(div_61);
    a_23 = document.createElement("A");
    a_23.setAttribute("href","#");
    a_23.setAttribute("class","logout-button item-link button button-fill color-blue");
    div_61.appendChild(a_23);
    text_30 = document.createTextNode("Logout");
    a_23.appendChild(text_30);
    br_2 = document.createElement("BR");
    div_61.appendChild(br_2);
    a_24 = document.createElement("A");
    a_24.setAttribute("href","#");
    a_24.setAttribute("class","button button-fill color-red close-popup");
    div_61.appendChild(a_24);
    text_31 = document.createTextNode("Close");
    a_24.appendChild(text_31);
    div_62 = document.createElement("DIV");
    div_62.setAttribute("class","list-block-label");
    div_60.appendChild(div_62);
    text_32 = document.createTextNode("Logout your App.");
    div_62.appendChild(text_32);
    document.body.appendChild(docFragment);
  };
});
rtl.module("program",["System","Classes","SysUtils","JS","Web","uMain"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    try {
      pas.uMain.Application = pas.uMain.TApplication.$create("Create");
      pas.uMain.Application.RunApp();
    } catch ($e) {
      if (pas.SysUtils.Exception.isPrototypeOf($e)) {
        var e = $e;
        window.console.log(e.fMessage);
      } else throw $e
    };
  };
});
//# sourceMappingURL=project1.js.map
