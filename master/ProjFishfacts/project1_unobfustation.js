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
      $rtti: Object.create(rtl.tSectionRTTI),
    };
    module.$rtti.$module = module;
    if (implcode) module.$impl = {
      $module: module,
      $rtti: module.$rtti,
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

  is: function(descendant,type){
    return type.isPrototypeOf(descendant) || (descendant===type);
  },

  isExt: function(instance,type){
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type) return true;
    if (type.isPrototypeOf && type.isPrototypeOf(instance)) return true;
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  EInvalidCast: null,

  as: function(instance,type){
    if(rtl.is(instance,type)) return instance;
    throw rtl.EInvalidCast.$create("create");
  },

  asExt: function(instance,type){
    if(rtl.isExt(instance,type)) return instance;
    throw rtl.EInvalidCast.$create("create");
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

  arrayClone: function(type,src,srcpos,end,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if (rtl.isFunction(type)){
      for (; srcpos<end; srcpos++) dst[dstpos++] = new type(src[srcpos]); // clone record
    } else if(isString(type) && (type === 'refSet')) {
      for (; srcpos<end; srcpos++) dst[dstpos++] = refSet(src[srcpos]); // ref set
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
    if (end>scrarray.length) end = scrarray.length;
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
    for (var key in s) if (s.hasOwnProperty(key)) r[key]=true;
    return r;
  },

  refSet: function(s){
    s.$shared = true;
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && !t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key)) r[key]=true;
    for (var key in t) if (t.hasOwnProperty(key)) r[key]=true;
    delete r.$shared;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && !t[key]) r[key]=true;
    for (var key in t) if (t.hasOwnProperty(key) && !s[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (s.hasOwnProperty(key) && !t[key] && (key!='$shared')) return false;
    for (var key in t) if (t.hasOwnProperty(key) && !s[key] && (key!='$shared')) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (s.hasOwnProperty(key) && !t[key] && (key!='$shared')) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (t.hasOwnProperty(key) && !s[key] && (key!='$shared')) return false;
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

    // tTypeInfoStruct - base object for tTypeInfoClass and tTypeInfoRecord
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
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0),
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
  },
}
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
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
     else NI.set(Math.trunc(x));
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
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
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
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SErrInvalidFloat = 'Invalid floating-point value: "%s"';
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.CreateFmt = function (Msg, Args) {
      this.fMessage = $mod.Format(Msg,Args);
    };
  });
  rtl.createClass($mod,"EInvalidCast",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
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
        if (ChPos > Len) $impl.DoFormatError($impl.feInvalidFormat,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === -1) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError($impl.feInvalidFormat,Fmt);
          ArgPos = ArgN + 1;
          if (rtl.isNumber(Args[ArgN]) && pas.JS.isInteger(Args[ArgN])) {
            Value = Math.floor(Args[ArgN])}
           else $impl.DoFormatError($impl.feInvalidFormat,Fmt);
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
            if (Code > 0) $impl.DoFormatError($impl.feInvalidFormat,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError($impl.feMissingArgument,Fmt);
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
        if (err) $impl.DoFormatError($impl.feInvalidArgIndex,Fmt);
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
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError($impl.feInvalidArgIndex,Fmt);
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
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
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
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3};
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,$mod.TFloatFormat.ffGeneral,15,0);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    function RemoveLeadingNegativeSign(AValue) {
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
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === $mod.TFloatFormat.ffGeneral) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffExponent) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffFixed) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffNumber) Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator);
    if ((Result.length > 1) && (Result.charAt(0) === "-")) RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }});
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
  this.StrToFloat = function (S) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidFloat,[S]]);
    return Result;
  };
  this.TimeSeparator = "";
  this.DateSeparator = "";
  this.ShortDateFormat = "";
  this.LongDateFormat = "";
  this.ShortTimeFormat = "";
  this.LongTimeFormat = "";
  this.DecimalSeparator = "";
  this.ThousandSeparator = "";
  this.TimeAMString = "";
  this.TimePMString = "";
  this.MonthDays = rtl.arraySetLength(null,0,2,12);
  this.ShortMonthNames = rtl.arraySetLength(null,"",12);
  this.LongMonthNames = rtl.arraySetLength(null,"",12);
  this.ShortDayNames = rtl.arraySetLength(null,"",7);
  this.LongDayNames = rtl.arraySetLength(null,"",7);
  this.TwoDigitYearCenturyWindow = 0;
  $mod.$init = function () {
    $mod.LongDayNames[0] = "Sunday";
    $mod.LongDayNames[1] = "Monday";
    $mod.LongDayNames[2] = "Tuesday";
    $mod.LongDayNames[3] = "Wednesday";
    $mod.LongDayNames[4] = "Thursday";
    $mod.LongDayNames[5] = "Friday";
    $mod.LongDayNames[6] = "Saturday";
    $mod.ShortDayNames[0] = "Sun";
    $mod.ShortDayNames[1] = "Mon";
    $mod.ShortDayNames[2] = "Tue";
    $mod.ShortDayNames[3] = "Wed";
    $mod.ShortDayNames[4] = "Thu";
    $mod.ShortDayNames[5] = "Fri";
    $mod.ShortDayNames[6] = "Sat";
    $mod.ShortMonthNames[0] = "Jan";
    $mod.ShortMonthNames[1] = "Feb";
    $mod.ShortMonthNames[2] = "Mar";
    $mod.ShortMonthNames[3] = "Apr";
    $mod.ShortMonthNames[4] = "May";
    $mod.ShortMonthNames[5] = "Jun";
    $mod.ShortMonthNames[6] = "Jul";
    $mod.ShortMonthNames[7] = "Aug";
    $mod.ShortMonthNames[8] = "Sep";
    $mod.ShortMonthNames[9] = "Oct";
    $mod.ShortMonthNames[10] = "Nov";
    $mod.ShortMonthNames[11] = "Dec";
    $mod.LongMonthNames[0] = "January";
    $mod.LongMonthNames[1] = "February";
    $mod.LongMonthNames[2] = "March";
    $mod.LongMonthNames[3] = "April";
    $mod.LongMonthNames[4] = "May";
    $mod.LongMonthNames[5] = "June";
    $mod.LongMonthNames[6] = "July";
    $mod.LongMonthNames[7] = "August";
    $mod.LongMonthNames[8] = "September";
    $mod.LongMonthNames[9] = "October";
    $mod.LongMonthNames[10] = "November";
    $mod.LongMonthNames[11] = "December";
    $mod.MonthDays[1][0] = 31;
    $mod.MonthDays[1][1] = 29;
    $mod.MonthDays[1][2] = 31;
    $mod.MonthDays[1][3] = 30;
    $mod.MonthDays[1][4] = 31;
    $mod.MonthDays[1][5] = 30;
    $mod.MonthDays[1][6] = 31;
    $mod.MonthDays[1][7] = 31;
    $mod.MonthDays[1][8] = 30;
    $mod.MonthDays[1][9] = 31;
    $mod.MonthDays[1][10] = 30;
    $mod.MonthDays[1][11] = 31;
    $mod.MonthDays[0][0] = 31;
    $mod.MonthDays[0][1] = 28;
    $mod.MonthDays[0][2] = 31;
    $mod.MonthDays[0][3] = 30;
    $mod.MonthDays[0][4] = 31;
    $mod.MonthDays[0][5] = 30;
    $mod.MonthDays[0][6] = 31;
    $mod.MonthDays[0][7] = 31;
    $mod.MonthDays[0][8] = 30;
    $mod.MonthDays[0][9] = 31;
    $mod.MonthDays[0][10] = 30;
    $mod.MonthDays[0][11] = 31;
    $impl.DateTimeToStrFormat[0] = "c";
    $impl.DateTimeToStrFormat[1] = "f";
    rtl.EInvalidCast = $mod.EInvalidCast;
    $mod.DateSeparator = "-";
    $mod.TimeSeparator = ":";
    $mod.ShortDateFormat = "yyyy-mm-dd";
    $mod.LongDateFormat = "ddd, yyyy-mm-dd";
    $mod.ShortTimeFormat = "hh:nn";
    $mod.LongTimeFormat = "hh:nn:ss";
    $mod.DecimalSeparator = ".";
    $mod.TimeAMString = "AM";
    $mod.TimePMString = "PM";
    $mod.TwoDigitYearCenturyWindow = 50;
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
    if ($tmp1 === $impl.feInvalidFormat) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidFormat,[fmt]])}
     else if ($tmp1 === $impl.feMissingArgument) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SArgumentMissing,[fmt]])}
     else if ($tmp1 === $impl.feInvalidArgIndex) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidArgIndex,[fmt]]);
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
    if ((Precision === -1) || (Precision > $impl.maxdigits)) Precision = $impl.maxdigits;
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
      if (Result.charAt(Q - 1) === DS) Q -= 1;
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
    if ((Precision === -1) || (Precision > $impl.maxdigits)) Precision = $impl.maxdigits;
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
     else if (Digits > $impl.maxdigits) Digits = $impl.maxdigits;
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
  $impl.RESpecials = "([\\[\\]\\(\\)\\\\\\.\\*])";
  $impl.DateTimeToStrFormat = rtl.arraySetLength(null,"",2);
});
rtl.module("math",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Classes",["System","RTLConsts","Types","SysUtils"],function () {
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
rtl.module("uFishFacts",["System","JS","Web","Types","math","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TFishRecord = function (s) {
    if (s) {
      this.Category = s.Category;
      this.Common_Name = s.Common_Name;
      this.Length_Cm = s.Length_Cm;
      this.Length_In = s.Length_In;
      this.Notes = s.Notes;
      this.Species_Name = s.Species_Name;
      this.Species_No = s.Species_No;
    } else {
      this.Category = "";
      this.Common_Name = "";
      this.Length_Cm = "";
      this.Length_In = "";
      this.Notes = "";
      this.Species_Name = "";
      this.Species_No = "";
    };
    this.$equal = function (b) {
      return (this.Category === b.Category) && ((this.Common_Name === b.Common_Name) && ((this.Length_Cm === b.Length_Cm) && ((this.Length_In === b.Length_In) && ((this.Notes === b.Notes) && ((this.Species_Name === b.Species_Name) && (this.Species_No === b.Species_No))))));
    };
  };
  rtl.createClass($mod,"TJFishFacts",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fishRecord = new $mod.TFishRecord();
      this.list = null;
      this.selectedIndex = 0;
    };
    this.$final = function () {
      this.fishRecord = undefined;
      this.list = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.getList = function () {
      var Result = null;
      Result = JSON.parse($impl.fishJsonData);
      return Result;
    };
    this.JSON2TFishRecord = function (Value) {
      var Result = new $mod.TFishRecord();
      Result.Category = "" + Value["Category"];
      Result.Common_Name = "" + Value["Common_Name"];
      Result.Length_Cm = "" + Value["Length_Cm"];
      Result.Length_In = "" + Value["Length_In"];
      Result.Notes = "" + Value["Notes"];
      Result.Species_Name = "" + Value["Species_Name"];
      Result.Species_No = "" + Value["Species_No"];
      return Result;
    };
    this.roundNumber = function (num, decNumber) {
      var Result = "";
      Result = pas.SysUtils.FloatToStr(Math.round(num * Math.pow(10,decNumber)) / Math.pow(10,decNumber));
      return Result;
    };
    this.bindEvent = function (element, EventType, handler) {
      var events = [];
      var i = 0;
      events = EventType.split(" ");
      if (element.addEventListener) {
        for (var $l1 = 0, $end2 = events.length - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          element.addEventListener(events[i],handler,false);
        };
      } else if (element.attachEvent) {
        for (var $l3 = 0, $end4 = events.length - 1; $l3 <= $end4; $l3++) {
          i = $l3;
          element.attachEvent("on" + events[i],handler);
        };
      };
    };
    this.callbackA = function (e) {
      document.getElementById("smsfish-fishDetails").classList.remove("nm");
    };
    this.callbackB = function (e) {
      var btnLiga = null;
      btnLiga = document.querySelector("#smsfish-LIGA");
      document.getElementById("smsfish-ligar").classList.remove("nm");
      document.getElementById("smsfish-hideTable").classList.remove("paused");
      btnLiga.setAttribute("disabled","true");
    };
    this.callbackC = function (e) {
      this.upClick(this);
    };
    this.callbackD = function (e) {
      this.downClick(this);
    };
    this.Create$1 = function () {
      var docFragment = null;
      var smsfishananim = null;
      var smsfishanscene0 = null;
      var div_ = null;
      var smsfishmainscreen = null;
      var div_0 = null;
      var smsfishon = null;
      var a_ = null;
      var smsfishanobj3 = null;
      var smsfishligar = null;
      var smsfishanobj5 = null;
      var a_0 = null;
      var smsfishanobj6 = null;
      var a_1 = null;
      var smsfishpanelsub = null;
      var smsfishpeixea = null;
      var div_1 = null;
      var img_ = null;
      var img_0 = null;
      var smsfishpeixeb = null;
      var div_2 = null;
      var img_1 = null;
      var smsfishcreatedby = null;
      var smsfishsombra = null;
      var smsfishpas2js = null;
      var div_3 = null;
      var img_2 = null;
      var smsfishpaneltop = null;
      var smsfishundersea = null;
      var div_4 = null;
      var img_3 = null;
      var smsfishfishfacts = null;
      var div_5 = null;
      var img_4 = null;
      var smsfishanobj16 = null;
      var smsfishstatuson = null;
      var smsfishaboutme = null;
      var div_6 = null;
      var img_5 = null;
      var smsfishhidegrupo = null;
      var smsfishhidetable = null;
      var smsfishfishdetails = null;
      var smsfishpicture = null;
      var div_7 = null;
      var img_6 = null;
      var smsfishmemo = null;
      var smsfishabout = null;
      var smsfishcategory = null;
      var smsfishspeciename = null;
      var smsfishlencm = null;
      var smsfishlenin = null;
      docFragment = document.createDocumentFragment();
      smsfishananim = document.createElement("DIV");
      smsfishananim.setAttribute("id","smsfish-an-anim");
      smsfishananim.setAttribute("id","smsfish-an-anim");
      docFragment.appendChild(smsfishananim);
      smsfishanscene0 = document.createElement("DIV");
      smsfishanscene0.setAttribute("id","smsfish-an-scene-0");
      smsfishanscene0.setAttribute("class","run t-0 paused");
      smsfishananim.appendChild(smsfishanscene0);
      div_ = document.createElement("DIV");
      div_.setAttribute("class","smsfish-an-stage");
      smsfishanscene0.appendChild(div_);
      smsfishmainscreen = document.createElement("DIV");
      smsfishmainscreen.setAttribute("id","smsfish-mainScreen");
      div_.appendChild(smsfishmainscreen);
      div_0 = document.createElement("DIV");
      smsfishmainscreen.appendChild(div_0);
      img_ = document.createElement("IMG");
      img_.setAttribute("height","665");
      img_.setAttribute("width","648");
      img_.setAttribute("src","assets\/nintendo1.svg");
      div_0.appendChild(img_);
      smsfishon = document.createElement("DIV");
      smsfishon.setAttribute("id","smsfish-ON");
      div_.appendChild(smsfishon);
      a_ = document.createElement("BUTTON");
      a_.setAttribute("id","smsfish-LIGA");
      a_.setAttribute("data-icon","|");
      a_.setAttribute("title","Turn ON");
      a_.setAttribute("class","button green oval icon");
      smsfishon.appendChild(a_);
      smsfishanobj3 = document.createElement("DIV");
      smsfishanobj3.setAttribute("id","smsfish-an-obj-3");
      div_.appendChild(smsfishanobj3);
      smsfishligar = document.createElement("DIV");
      smsfishligar.setAttribute("id","smsfish-ligar");
      smsfishligar.setAttribute("class","nm");
      smsfishanobj3.appendChild(smsfishligar);
      smsfishanobj5 = document.createElement("DIV");
      smsfishanobj5.setAttribute("id","smsfish-R");
      smsfishligar.appendChild(smsfishanobj5);
      a_0 = document.createElement("BUTTON");
      a_0.setAttribute("id","smsfish-RIGHT");
      a_0.setAttribute("class","button pink oval icon");
      a_0.setAttribute("title","Love");
      a_0.setAttribute("data-icon","R");
      smsfishanobj5.appendChild(a_0);
      smsfishanobj6 = document.createElement("DIV");
      smsfishanobj6.setAttribute("id","smsfish-L");
      smsfishligar.appendChild(smsfishanobj6);
      a_1 = document.createElement("BUTTON");
      a_1.setAttribute("id","smsfish-LEFT");
      a_1.setAttribute("class","button blue oval icon");
      a_1.setAttribute("title","Love");
      a_1.setAttribute("data-icon","L");
      smsfishanobj6.appendChild(a_1);
      smsfishpanelsub = document.createElement("DIV");
      smsfishpanelsub.setAttribute("id","smsfish-panelSub");
      smsfishligar.appendChild(smsfishpanelsub);
      smsfishpeixea = document.createElement("DIV");
      smsfishpeixea.setAttribute("id","smsfish-peixeA");
      smsfishpanelsub.appendChild(smsfishpeixea);
      div_1 = document.createElement("DIV");
      smsfishpeixea.appendChild(div_1);
      img_0 = document.createElement("IMG");
      img_0.setAttribute("height","225");
      img_0.setAttribute("width","225");
      img_0.setAttribute("src","assets\/peixeA.png");
      div_1.appendChild(img_0);
      smsfishpeixeb = document.createElement("DIV");
      smsfishpeixeb.setAttribute("id","smsfish-peixeB");
      smsfishpanelsub.appendChild(smsfishpeixeb);
      div_2 = document.createElement("DIV");
      smsfishpeixeb.appendChild(div_2);
      img_1 = document.createElement("IMG");
      img_1.setAttribute("height","225");
      img_1.setAttribute("width","225");
      img_1.setAttribute("src","assets\/peixeB.png");
      div_2.appendChild(img_1);
      smsfishcreatedby = document.createElement("DIV");
      smsfishcreatedby.setAttribute("id","smsfish-createdby");
      smsfishligar.appendChild(smsfishcreatedby);
      smsfishsombra = document.createElement("DIV");
      smsfishsombra.setAttribute("id","smsfish-sombra");
      smsfishcreatedby.appendChild(smsfishsombra);
      smsfishpas2js = document.createElement("DIV");
      smsfishpas2js.setAttribute("id","smsfish-pas2js");
      smsfishcreatedby.appendChild(smsfishpas2js);
      div_3 = document.createElement("DIV");
      smsfishpas2js.appendChild(div_3);
      img_2 = document.createElement("IMG");
      img_2.setAttribute("height","162");
      img_2.setAttribute("width","404");
      img_2.setAttribute("src","assets\/pas2js.png");
      div_3.appendChild(img_2);
      smsfishpaneltop = document.createElement("DIV");
      smsfishpaneltop.setAttribute("id","smsfish-panelTop");
      smsfishligar.appendChild(smsfishpaneltop);
      smsfishundersea = document.createElement("DIV");
      smsfishundersea.setAttribute("id","smsfish-undersea");
      smsfishpaneltop.appendChild(smsfishundersea);
      div_4 = document.createElement("DIV");
      smsfishundersea.appendChild(div_4);
      img_3 = document.createElement("IMG");
      img_3.setAttribute("height","170");
      img_3.setAttribute("width","790");
      img_3.setAttribute("src","assets\/undersea.jpg");
      div_4.appendChild(img_3);
      smsfishfishfacts = document.createElement("DIV");
      smsfishfishfacts.setAttribute("id","smsfish-fishfacts");
      smsfishpaneltop.appendChild(smsfishfishfacts);
      div_5 = document.createElement("DIV");
      smsfishfishfacts.appendChild(div_5);
      img_4 = document.createElement("IMG");
      img_4.setAttribute("height","83");
      img_4.setAttribute("width","232");
      img_4.setAttribute("src","assets\/fishfacts.png");
      div_5.appendChild(img_4);
      smsfishanobj16 = document.createElement("DIV");
      smsfishanobj16.setAttribute("id","smsfish-an-obj-16");
      smsfishligar.appendChild(smsfishanobj16);
      smsfishstatuson = document.createElement("DIV");
      smsfishstatuson.setAttribute("id","smsfish-statusON");
      smsfishanobj16.appendChild(smsfishstatuson);
      smsfishaboutme = document.createElement("DIV");
      smsfishaboutme.setAttribute("id","smsfish-aboutMe");
      smsfishanobj16.appendChild(smsfishaboutme);
      div_6 = document.createElement("DIV");
      smsfishaboutme.appendChild(div_6);
      img_5 = document.createElement("IMG");
      img_5.setAttribute("height","72");
      img_5.setAttribute("width","75");
      img_5.setAttribute("src","assets\/tomate.png");
      div_6.appendChild(img_5);
      smsfishhidegrupo = document.createElement("DIV");
      smsfishhidegrupo.setAttribute("id","smsfish-hideGrupo");
      smsfishanobj3.appendChild(smsfishhidegrupo);
      smsfishhidetable = document.createElement("DIV");
      smsfishhidetable.setAttribute("id","smsfish-hideTable");
      smsfishhidetable.setAttribute("class","paused");
      smsfishhidegrupo.appendChild(smsfishhidetable);
      smsfishfishdetails = document.createElement("DIV");
      smsfishfishdetails.setAttribute("id","smsfish-fishDetails");
      smsfishfishdetails.setAttribute("class","nm");
      div_.appendChild(smsfishfishdetails);
      smsfishpicture = document.createElement("DIV");
      smsfishpicture.setAttribute("id","smsfish-picture");
      smsfishfishdetails.appendChild(smsfishpicture);
      div_7 = document.createElement("DIV");
      div_7.setAttribute("style","position: initial");
      smsfishpicture.appendChild(div_7);
      img_6 = document.createElement("IMG");
      img_6.setAttribute("height","100%");
      img_6.setAttribute("width","100%");
      img_6.setAttribute("src","");
      div_7.appendChild(img_6);
      smsfishmemo = document.createElement("TEXTAREA");
      smsfishmemo.setAttribute("id","smsfish-memo");
      smsfishmemo.setAttribute("style","background-color: rgb(188, 188, 222)");
      smsfishfishdetails.appendChild(smsfishmemo);
      smsfishabout = document.createElement("DIV");
      smsfishabout.setAttribute("id","smsfish-about");
      smsfishabout.setAttribute("style","color: rgb(0, 0, 255); font-size: 20px; text-align: center;");
      smsfishfishdetails.appendChild(smsfishabout);
      smsfishcategory = document.createElement("DIV");
      smsfishcategory.setAttribute("id","smsfish-category");
      smsfishcategory.setAttribute("style","font-size: 15px; font-weight: bold; color: brown;");
      smsfishfishdetails.appendChild(smsfishcategory);
      smsfishspeciename = document.createElement("DIV");
      smsfishspeciename.setAttribute("id","smsfish-specieName");
      smsfishspeciename.setAttribute("style","font-size: 15px; font-weight: bold; color: brown;");
      smsfishfishdetails.appendChild(smsfishspeciename);
      smsfishlencm = document.createElement("DIV");
      smsfishlencm.setAttribute("id","smsfish-lenCm");
      smsfishlencm.setAttribute("style","font-size: 15px; font-weight: bold; color: brown;");
      smsfishfishdetails.appendChild(smsfishlencm);
      smsfishlenin = document.createElement("DIV");
      smsfishlenin.setAttribute("id","smsfish-lenIn");
      smsfishlenin.setAttribute("style","font-size: 15px; font-weight: bold; color: brown;");
      smsfishfishdetails.appendChild(smsfishlenin);
      document.body.appendChild(docFragment);
    };
    this.downClick = function (Sender) {
      if (this.selectedIndex > 0) {
        this.selectedIndex -= 1;
        this.fishRecord = new $mod.TFishRecord(this.JSON2TFishRecord(rtl.getObject(this.getList()[pas.SysUtils.IntToStr(this.selectedIndex)])));
        this.selectionChange();
      };
    };
    this.upClick = function (Sender) {
      if (this.selectedIndex < (this.list.length - 1)) {
        this.selectedIndex += 1;
        this.fishRecord = new $mod.TFishRecord(this.JSON2TFishRecord(rtl.getObject(this.getList()[pas.SysUtils.IntToStr(this.selectedIndex)])));
        this.selectionChange();
      };
    };
    this.refreshFacts = function () {
      this.list = this.getList();
      if (this.list.length > 0) this.selectedIndex = 0;
      this.selectionChange();
    };
    this.selectionChange = function () {
      var rightBtn = null;
      var leftBtn = null;
      var pictureImg = null;
      var about = null;
      var category = null;
      var specieName = null;
      var lenCm = null;
      var lenIn = null;
      var memo = null;
      rightBtn = document.querySelector("#smsfish-RIGHT");
      leftBtn = document.querySelector("#smsfish-LEFT");
      pictureImg = document.querySelector("#smsfish-picture img");
      about = document.querySelector("#smsfish-about");
      category = document.querySelector("#smsfish-category");
      specieName = document.querySelector("#smsfish-specieName");
      lenCm = document.querySelector("#smsfish-lenCm");
      lenIn = document.querySelector("#smsfish-lenIn");
      memo = document.querySelector("#smsfish-memo");
      if (!((this.list.length === 0) || (this.selectedIndex === (this.list.length - 1)))) {
        rightBtn.removeAttribute("disabled")}
       else rightBtn.setAttribute("disabled","true");
      if (!((this.list.length === 0) || (this.selectedIndex === 0))) {
        leftBtn.removeAttribute("disabled")}
       else leftBtn.setAttribute("disabled","true");
      pictureImg.setAttribute("src",("pics\/" + this.fishRecord.Species_No) + ".png");
      about.innerHTML = ("<b>About the " + this.fishRecord.Common_Name) + "<\/b>";
      category.textContent = this.fishRecord.Category;
      specieName.textContent = this.fishRecord.Species_Name;
      lenCm.textContent = this.roundNumber(pas.SysUtils.StrToFloat(this.fishRecord.Length_Cm),2);
      lenIn.textContent = this.roundNumber(pas.SysUtils.StrToFloat(this.fishRecord.Length_In),2);
      memo.textContent = this.fishRecord.Notes;
    };
    this.InitializeObject = function () {
      document.querySelector("#smsfish-an-scene-0").style.setProperty("webkitTransition","none");
      document.querySelector("#smsfish-an-scene-0").classList.add("paused");
      this.bindEvent(document.querySelector("#smsfish-hideTable"),"webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend",rtl.createCallback(this,"callbackA"));
      this.bindEvent(document.querySelector("#smsfish-ON"),"click",rtl.createCallback(this,"callbackB"));
      this.bindEvent(document.querySelector("#smsfish-R"),"click",rtl.createCallback(this,"callbackC"));
      this.bindEvent(document.querySelector("#smsfish-L"),"click",rtl.createCallback(this,"callbackD"));
      this.fishRecord = new $mod.TFishRecord(this.JSON2TFishRecord(rtl.getObject(this.getList()["0"])));
      this.refreshFacts();
    };
    var $r = this.$rtti;
    $r.addMethod("InitializeObject",0,null);
  });
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.fishJsonData = (((((((((((((((((((((((((((("[" + '{"Species_No":"90020","Category":"Triggerfishy","Common_Name":"Clown Triggerfish","Species_Name":"Ballistoides conspicillum","Length_Cm":"50","Length_In":"19.6850393700787","Notes":"Also known as the big spotted triggerfish.  Inhabits outer reef areas and feeds upon crustaceans and mollusks by crushing them with powerful teeth.  They are voracious eaters, and divers report seeing the clown triggerfish devour beds of pearl oysters.\\r\\n\\r\\nDo not eat this fish.  According to an 1878 account, \\"the poisonous flesh acts primarily upon the nervous tissue of the stomach, occasioning violent spasms of that organ, and shortly afterwards all the muscles of the body.  The frame becomes rocked with spasms, the tongue thickened, the eye fixed, the breathing laborious, and the patient expires in a paroxysm of extreme suffering.\\"\\r\\n\\r\\nNot edible.\\r\\n\\r\\nRange is Indo-Pacific and East Africa to Somoa."},') + '{"Species_No":"90030","Category":"Snapper","Common_Name":"Red Emperor","Species_Name":"Lutjanus sebae","Length_Cm":"60","Length_In":"23.6220472440945","Notes":"Called seaperch in Australia.  Inhabits the areas around lagoon coral reefs and sandy bottoms.\\r\\n\\r\\nThe red emperor is a valuable food fish and considered a great sporting fish that fights with fury when hooked.  The flesh of an old fish is just as tender to eat as that of the very young.\\r\\n\\r\\nRange is from the Indo-Pacific to East Africa."},') + '{"Species_No":"90050","Category":"Wrasse","Common_Name":"Giant Maori Wrasse","Species_Name":"Cheilinus undulatus","Length_Cm":"229","Length_In":"90.15748031496059","Notes":"This is the largest of all the wrasse.  It is found in dense reef areas, feeding on a wide variety of mollusks, fishes, sea urchins, crustaceans, and other invertebrates. In spite of its immense size, divers find it a very wary fish.\\r\\n\\r\\nEdibility is considered poor.\\r\\n\\r\\nRange is the Indo-Pacific and the Red Sea."},') + '{"Species_No":"90070","Category":"Angelfish","Common_Name":"Blue Angelfish","Species_Name":"Pomacanthus nauarchus","Length_Cm":"30","Length_In":"11.8110236220472","Notes":"Habitat is around boulders, caves, coral ledges and crevices in shallow waters.  Swims alone or in groups.\\r\\n\\r\\nIts color changes dramatically from juvenile to adult.  The mature adult fish can startle divers by producing a powerful drumming or thumping sound intended to warn off predators.\\r\\n\\r\\nEdibility is good.\\r\\n\\r\\nRange is the entire Indo-Pacific region."},') + '{"Species_No":"90080","Category":"Cod","Common_Name":"Lunartail Rockcod","Species_Name":"Variola louti","Length_Cm":"80","Length_In":"31.496062992126","Notes":"Also known as the coronation trout.  It is found around coral reefs from shallow to very deep waters.  Feeds primarily on small fishes.\\r\\n\\r\\nAlthough this rockcod is considered a good game and food fish, the large ones may contain a toxin and should not be eaten.  There is no way to discern whether the fish contains toxin.\\r\\n\\r\\nRange is the Indo-Pacific and the Red Sea."},') + '{"Species_No":"90090","Category":"Scorpionfish","Common_Name":"Firefish","Species_Name":"Pterois volitans","Length_Cm":"38","Length_In":"14.9606299212598","Notes":"Also known as the turkeyfish.  Inhabits reef caves and crevices.  The firefish is usually stationary during the day, but feeds actively at night.  Favorite foods are crustaceans.\\r\\n\\r\\nThe dorsal spines of the firefish are needle-like and contain venom.  They can inflict an extremely painful wound.\\r\\n\\r\\nEdibility is poor.\\r\\n\\r\\nRange is from Western Australia to Malaysia."},') + '{"Species_No":"90100","Category":"Butterflyfish","Common_Name":"Ornate Butterflyfish","Species_Name":"Chaetodon Ornatissimus","Length_Cm":"19","Length_In":"7.48031496062992","Notes":"Normally seen in pairs around dense coral areas from very shallow to moderate depths.  The butterflyfish feeds mainly on coral polyps and anemones.\\r\\n\\r\\nEdibility is poor.\\r\\n\\r\\nRange is Indo-Pacific from Sri Lanka to Polynesia."},') + '{"Species_No":"90110","Category":"Shark","Common_Name":"Swell Shark","Species_Name":"Cephaloscyllium ventriosum","Length_Cm":"102","Length_In":"40.15748031496063","Notes":"Inhabits shallow reef caves and crevices and kelp beds along the coast and offshore islands.  This shark feeds at night on fishes and crustaceans and is totally harmless to divers.\\n\\nFor defense, the swell shark inflates its stomach with water to tightly lodge itself in a reef crevice.  \\n\\nEdibility is poor.\\n\\nRange is from Monterey Bay to Acapulco.  Also found in Chile."},') + '{"Species_No":"90120","Category":"Ray","Common_Name":"Bat Ray","Species_Name":"Myliobatis californica","Length_Cm":"56","Length_In":"22.04724409448819","Notes":"Also know as the grinder ray because of its flat grinding teeth used to crush its meal of crustaceans or invertebrates.  Inhabits bays, sloughs, and kelp beds with sandy bottoms.\\n\\nThe bat ray digs up food with its wings and snout, and will even bite off overhanging ledges to get at prey.  It hunts singly or in groups.  When resting, it buries itself in sand with just the eyes protruding.\\n\\nEdibility is poor.\\n\\nRange is from Oregon to the Gulf of California."},') + '{"Species_No":"90130","Category":"Eel","Common_Name":"California Moray","Species_Name":"Gymnothorax mordax","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"This fish hides in a shallow-water lair with just its head protruding during the day.  At night it feeds on octopuses, crustaceans, and small fish close by.\\n\\nIf caught, it will bite anything nearby with its large fang-like teeth.  Divers can be bitten by a moray eel when sticking their hands into crevices or holes in search of lobster or abalone.\\n\\nEdibility is good.\\n\\nRange is from Southern California to Southern Baja."},') + '{"Species_No":"90140","Category":"Cod","Common_Name":"Lingcod","Species_Name":"Ophiodon elongatus","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Widely found from near the shore to very deep waters.  Young fish stay on sand or mud bottoms of bays and inshore areas.  The lingcod is a voracious predator, eating many different fishes and octopuses.\\n\\nThis fish changes color when stressed.  The flesh color also changes, from a greenish hue when caught to white when cooked.\\n\\nEdibility is good; Lingcod is a popular sport and commercial fish.\\n\\nRange is Alaska to Northern Baja California."},') + '{"Species_No":"90150","Category":"Sculpin","Common_Name":"Cabezon","Species_Name":"Scorpaenichthys marmoratus","Length_Cm":"99","Length_In":"38.9763779527559","Notes":"Often called the great marbled sculpin.  Found over rocky or shell-encrusted bottoms from shallow to moderately deep waters.  It feeds primarily on crustaceans and mollusks.\\n\\nThe male cabezon will not budge while guarding its nest and can even be touched by divers.\\n\\nEdibility is good; the flesh is bluish-green but turns white when cooked.  The eggs of the cabezon are poisonous.\\n\\nRange is from Alaska to Central Baja."},') + '{"Species_No":"90160","Category":"Spadefish","Common_Name":"Atlantic Spadefish","Species_Name":"Chaetodiperus faber","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Found in mid-water areas around reefs, wrecks and bridges.  The tiny, all-black juveniles drift motionless in the shallows, looking like leaves and pods of mangrove.\\n\\nEdibility is good.\\n\\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},') + '{"Species_No":"90170","Category":"Shark","Common_Name":"Nurse Shark","Species_Name":"Ginglymostoma cirratum","Length_Cm":"400","Length_In":"157.4803149606299","Notes":"Frequently found under rock or reef ledges.  Carnivorous with well-developed organs for scent and vibration detection.\\n\\nLike all sharks, the nurse shark has a skeleton of cartilage rather than bone.  Instead of scales, its body is covered with tiny razor-sharp denticles.  The teeth are specialized forms of denticles.  Sharks must continually swim or will slowly sink because they have no air bladder.\\n\\nEdibility is poor.\\n\\nRange is from Rhode Island to Brazil, including the Gulf of Mexico."},') + '{"Species_No":"90180","Category":"Ray","Common_Name":"Spotted Eagle Ray","Species_Name":"Aetobatus narinari","Length_Cm":"200","Length_In":"78.74015748031496","Notes":"Found in reef areas and sandy bottoms.  The spotted eagle ray has a poisonous spine on its tail and incredibly powerful jaws to crush oysters, clams, and numerous crustaceans.  Divers report large schools during breeding season.\\n\\nThis ray is an active swimmer and often leaps into the air.  The slapping sound it makes on the water is thought to mark a territory.\\n\\nEdibility is poor.\\n\\nRange is throughout the tropics."},') + '{"Species_No":"90190","Category":"Snapper","Common_Name":"Yellowtail Snapper","Species_Name":"Ocyurus chrysurus","Length_Cm":"75","Length_In":"29.52755905511811","Notes":"Prefers to congregate in loose groups in the open water above reef areas.  Has well-developed teeth and usually feeds at night on small fishes, crustaceans, and plankton.\\n\\nThe yellowtail snapper repeatedly snaps its jaws after it has been caught.  Divers have been injured by these fish.\\n\\nThis is an excellent game fish with tenacious fighting ability and tasty flesh.\\n\\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},') + '{"Species_No":"90200","Category":"Parrotfish","Common_Name":"Redband Parrotfish","Species_Name":"Sparisoma Aurofrenatum","Length_Cm":"28","Length_In":"11.02362204724409","Notes":"Inhabits reef areas.  The parrotfish\'s teeth are fused together, enabling them to scrape away hard coral outer skeletons to get at polyps inside.  These fish are thought to be a major factor in reef recycling.\\n\\nOccasionally a female will change sex, increase in size, and take on a distinct appearance  as a terminal-phase male.  This is usually done to replace a missing male.\\n\\nEdibility is poor. \\n\\nRange is Bermuda and Florida to Brazil."},') + '{"Species_No":"90210","Category":"Barracuda","Common_Name":"Great Barracuda","Species_Name":"Sphyraena barracuda","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Young barracuda live in inshore seagrass beds, while adults range from inshore channels to the open ocean.  The barracuda feeds on a wide variety of fishes.\\n\\nIt frequently drifts just below the surface and is known to approach divers at very close range.  The long underslung jaw with its very sharp teeth can be disconcerting.  Attacks on humans have reportedly been in cloudy water when the victim is wearing bright diving gear or attempting to spear the fish.\\n\\nEdibility is good for small specimens, but  large barracuda can carry a fatal toxin.  There is no visible way to tell if the fish is harmful to eat.\\n\\nRange is worldwide."},') + '{"Species_No":"90220","Category":"Grunt","Common_Name":"French Grunt","Species_Name":"Haemulon flavolineatum","Length_Cm":"30","Length_In":"11.81102362204724","Notes":"The French grunt drifts in large groups in sheltered reef areas during the day.  It forages nearby for other fish at night.\\n\\nThe fish produces a grunt-like sound by grinding teeth located in the throat.  The sound is amplified by the adjacent swim bladder.  During territorial skirmishes, male grunts will face and push each other with open mouths.\\n\\nEdibility is excellent.\\n\\nRange is Bermuda, South Carolina to Brazil, and the Gulf of Mexico."},') + '{"Species_No":"90230","Category":"Snapper","Common_Name":"Dog Snapper","Species_Name":"Lutjanus jocu","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"This fish is named for its elongated canine teeth at the front of the upper jaw.  It is solitary and wary and stays in the deep reef or submerged wreck areas.  Not very common anywhere.\\n\\nEdibility is good if the fish is small.  However, a large dog snapper may contain a fatal toxin.  These fish repeatedly snap their jaws shut after removal from a hook or net.\\n\\nRange is New England to Brazil and the Gulf of Mexico."},') + '{"Species_No":"90240","Category":"Grouper","Common_Name":"Nassau Grouper","Species_Name":"Epinephelus striatus","Length_Cm":"91","Length_In":"35.8267716535433","Notes":"Found around shallow coral reefs and seagrass beds, feeding mainly on fishes.\\n\\nThis is the most friendly of all groupers.  If offered food, it will return again and again, looking for more. \\n\\nAs a defense, the Nassau grouper can change colors to blend perfectly into any background, from white to solid black.\\n\\nRange is Bermuda, North Carolina to Brazil, and the Gulf of Mexico."},') + '{"Species_No":"90250","Category":"Wrasse","Common_Name":"Bluehead Wrasse","Species_Name":"Thalassoma bifasciatum","Length_Cm":"15","Length_In":"5.905511811023622","Notes":"Found in coral reefs, rocky flats, reef sand, and seagrass habitats.  This is one of the most successful \\"cleaner fish\\" in the tropical West Atlantic.  It feeds on the parasites of other fish, who come to the wrasse to be cleaned.\\n\\nMost bluehead wrasses are yellow.  The head of the terminal-phase male (about 4% of the population) is blue.\\n\\nEdibility is poor.\\n\\nRange is large, including both sides of the Atlantic, Bermuda, Bahamas, and Florida to Curacao, plus the Gulf of Mexico."},') + '{"Species_No":"90260","Category":"Jack","Common_Name":"Yellow Jack","Species_Name":"Gnathanodon speciousus","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Inhabits reef and mid-water areas, feeding on invertebrates and small fishes.  The adult is one of the few jacks without teeth.\\n\\nThe young fish seek out larger predators, such as sharks, for protection.  Divers have reported young jacks wanting to join up with them!\\n\\nEdibility is excellent.\\n\\nRange is Indo-Pacific and Southern California to Panama."},') + '{"Species_No":"90270","Category":"Surfperch","Common_Name":"Redtail Surfperch","Species_Name":"Amphistichus rhodoterus","Length_Cm":"40","Length_In":"15.74803149606299","Notes":"Inhabits exposed sandy shorelines to shallow depths.  Feeds on sand-dwelling crustaceans and mollusks.\\n\\nWhile almost all other marine fishes fertilize and scatter large numbers of eggs outside the body, the surfperch nourishes offspring inside the ovary and spawns them live and sexually active into the surf.\\n\\nA favorite sport fish for surf anglers.  Edibility is very good.\\n\\nRange is from Vancouver Island to Central California."},') + '{"Species_No":"90280","Category":"Croaker","Common_Name":"White Sea Bass","Species_Name":"Atractoscion nobilis","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Schools are found over rocky bottoms and around kelp forest canopies.  Not a true bass, this is the largest of the croakers on the Pacific Coast.  It feeds in mid-water on squid, anchovies, and sardines.  \\n\\nCroakers make a remarkable \\"boop-boop-boop\\" sound, and submarine commanders discovered they could hide the sound of their engines behind the racket.  \\n\\nThe large calcareous \\"earstones\\" in this fish\'s inner ear canals were considered good luck charms by early Europeans and were used by American Indians in jewelry.\\n\\nExcellent edibility if you can find one.  White sea bass were heavily fished in the 1950s but are now rarely caught.\\n\\nRange is from Alaska to Southern Baja."},') + '{"Species_No":"90290","Category":"Greenling","Common_Name":"Rock Greenling","Species_Name":"Hexagrammos lagocephalus","Length_Cm":"60","Length_In":"23.62204724409449","Notes":"Inhabits rocky areas along shallow exposed coast line.\\n\\nGreenlings can change their color to blend with the surrounding sunlit rock and seaweed.  Their scales are very rough and give the body a sandpaper-like texture.\\n\\nAn 1886 description of a greenling comes from naturalist J.K. Lord.  He was overcome by its beauty, and said \\"its sides...rival in beauty many a tropical flower...[and are] adorned with colors not only conspicuous for their brilliancy, but grouped and blended in a manner one sees only represented in the plumage of a bird, the wing of a butterfly, or the petals of an orchid...red, blue, orange, and green are so mingled that the only thing I can think of as a comparison is a floating flower bed, and even the gardener\'s art, in grouping, is but a bungle contrasted with nature\'s painting.\\"\\n\\nEdibility is good.\\n\\nRange is from the Bering Sea to Southern California."},') + '{"Species_No":"90300","Category":"Wrasse","Common_Name":"Senorita","Species_Name":"Oxyjulis californica","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Found almost everywhere by divers, this fish lives either in schools or alone.  It is a voracious eater that feeds constantly.  It is also a very successful \\"cleaner fish\\", and a single Senorita may be surrounded by dozens of fishes waiting to be cleaned of parasites.  Divers report them teaming up to clean a large sea bass or Mola.\\n\\nThis fish does not reverse sex as most wrasses do.  When disturbed, it burrows in the bottom sediment.  It also sleeps there with its head protruding from the sand.\\n\\nEdibility is poor.\\n\\nRange is Northern California to Central Baja."},') + '{"Species_No":"90310","Category":"Smelt","Common_Name":"Surf Smelt","Species_Name":"Hypomesus pretiosus","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Also called the day smelt because of the timing of its spawning runs.  Inhabits the surf line, feeding on small fishes and invertebrates.  \\n\\nSurf smelt ride a wave onto the beach, lay and fertilize their eggs, and catch a return wave to the sea.  The fry hatch approximately two weeks later during high tide. \\n\\nThis fish is a favorite among surf anglers.  Edibility is good.\\n\\nRange is from Alaska to Southern California."}') + "]";
});
rtl.module("uMain",["System","uFishFacts","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TApplication",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FishFacts = null;
    };
    this.$final = function () {
      this.FishFacts = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.RunApp = function () {
      this.FishFacts = pas.uFishFacts.TJFishFacts.$create("Create$1");
      this.FishFacts.InitializeObject();
    };
  });
});
rtl.module("program",["System","uMain","Web","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.Application = null;
  $mod.$main = function () {
    try {
      $mod.Application = pas.uMain.TApplication.$create("Create");
      $mod.Application.RunApp();
    } catch ($e) {
      if (pas.SysUtils.Exception.isPrototypeOf($e)) {
        var e = $e;
        window.console.log(e.fMessage);
      } else throw $e
    };
  };
});
//# sourceMappingURL=project1.js.map
