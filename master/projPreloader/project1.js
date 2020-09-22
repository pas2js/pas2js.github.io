var pas={},rtl={version:10501,quiet:!1,debug_load_units:!1,debug_rtti:!1,debug:function(){!rtl.quiet&&console&&console.log&&console.log(arguments)},error:function(t){throw rtl.debug("Error: ",t),t},warn:function(t){rtl.debug("Warn: ",t)},checkVersion:function(t){if(rtl.version!=t)throw"expected rtl version "+t+", but found "+rtl.version},hiInt:Math.pow(2,53),hasString:function(t){return rtl.isString(t)&&0<t.length},isArray:function(t){return Array.isArray(t)},isFunction:function(t){return"function"==typeof t},isModule:function(t){return rtl.isObject(t)&&rtl.hasString(t.$name)&&pas[t.$name]===t},isImplementation:function(t){return rtl.isObject(t)&&rtl.isModule(t.$module)&&t.$module.$impl===t},isNumber:function(t){return"number"==typeof t},isObject:function(t){return"object"==typeof t&&null!=t},isString:function(t){return"string"==typeof t},getNumber:function(t){return"number"==typeof t?t:NaN},getChar:function(t){return"string"==typeof t&&1===t.length?t:""},getObject:function(t){return"object"==typeof t||"function"==typeof t?t:null},isTRecord:function(t){return rtl.isObject(t)&&t.hasOwnProperty("$new")&&"function"==typeof t.$new},isPasClass:function(t){return rtl.isObject(t)&&t.hasOwnProperty("$classname")&&rtl.isObject(t.$module)},isPasClassInstance:function(t){return rtl.isObject(t)&&rtl.isPasClass(t.$class)},hexStr:function(t,r){return("000000000000000"+t.toString(16).toUpperCase()).slice(-r)},m_loading:0,m_loading_intf:1,m_intf_loaded:2,m_loading_impl:3,m_initializing:4,m_initialized:5,module:function(t,r,e,n,i){rtl.debug_load_units&&rtl.debug('rtl.module name="'+t+'" intfuses='+r+" impluses="+n+" hasimplcode="+rtl.isFunction(i)),rtl.hasString(t)||rtl.error('invalid module name "'+t+'"'),rtl.isArray(r)||rtl.error('invalid interface useslist of "'+t+'"'),rtl.isFunction(e)||rtl.error('invalid interface code of "'+t+'"'),null==n||rtl.isArray(n)||rtl.error('invalid implementation useslist of "'+t+'"'),null==i||rtl.isFunction(i)||rtl.error('invalid implementation code of "'+t+'"'),pas[t]&&rtl.error('module "'+t+'" is already registered'),(t=pas[t]={$name:t,$intfuseslist:r,$impluseslist:n,$state:rtl.m_loading,$intfcode:e,$implcode:i,$impl:null,$rtti:Object.create(rtl.tSectionRTTI)}).$rtti.$module=t,i&&(t.$impl={$module:t,$rtti:t.$rtti})},exitcode:0,run:function(t){function r(){rtl.hasString(t)||(t="program"),rtl.debug_load_units&&rtl.debug('rtl.run module="'+t+'"'),rtl.initRTTI();var r=pas[t];r||rtl.error('rtl.run module "'+t+'" missing'),rtl.loadintf(r),rtl.loadimpl(r),"program"==t&&(rtl.debug_load_units&&rtl.debug("running $main"),r=pas.program.$main(),rtl.isNumber(r)&&(rtl.exitcode=r))}if(rtl.showUncaughtExceptions)try{r()}catch(t){var e=rtl.hasString(t.$classname)?t.$classname:"";e+=(e?": ":"")+(t.hasOwnProperty("fMessage")?t.fMessage:t),alert("Uncaught Exception : "+e),rtl.exitCode=216}else r();return rtl.exitcode},loadintf:function(t){t.$state>rtl.m_loading_intf||(rtl.debug_load_units&&rtl.debug('loadintf: "'+t.$name+'"'),t.$state===rtl.m_loading_intf&&rtl.error('unit cycle detected "'+t.$name+'"'),t.$state=rtl.m_loading_intf,rtl.loaduseslist(t,t.$intfuseslist,rtl.loadintf),rtl.debug_load_units&&rtl.debug('loadintf: run intf of "'+t.$name+'"'),t.$intfcode(t.$intfuseslist),t.$state=rtl.m_intf_loaded)},loaduseslist:function(t,r,e){if(null!=r)for(var n in r){var i=r[n];rtl.debug_load_units&&rtl.debug('loaduseslist of "'+t.$name+'" uses="'+i+'"'),null==pas[i]&&rtl.error('module "'+t.$name+'" misses "'+i+'"'),e(pas[i])}},loadimpl:function(t){t.$state>=rtl.m_loading_impl||(t.$state<rtl.m_intf_loaded&&rtl.error('loadimpl: interface not loaded of "'+t.$name+'"'),rtl.debug_load_units&&rtl.debug('loadimpl: load uses of "'+t.$name+'"'),t.$state=rtl.m_loading_impl,rtl.loaduseslist(t,t.$impluseslist,rtl.loadintf),rtl.loaduseslist(t,t.$intfuseslist,rtl.loadimpl),rtl.loaduseslist(t,t.$impluseslist,rtl.loadimpl),rtl.debug_load_units&&rtl.debug('loadimpl: run impl of "'+t.$name+'"'),rtl.isFunction(t.$implcode)&&t.$implcode(t.$impluseslist),rtl.debug_load_units&&rtl.debug('loadimpl: run init of "'+t.$name+'"'),t.$state=rtl.m_initializing,rtl.isFunction(t.$init)&&t.$init(),t.$state=rtl.m_initialized)},createCallback:function(t,r){var e="string"==typeof r?function(){return t[r].apply(t,arguments)}:function(){return r.apply(t,arguments)};return e.scope=t,e.fn=r,e},cloneCallback:function(t){return rtl.createCallback(t.scope,t.fn)},eqCallback:function(t,r){return t==r||null!=t&&null!=r&&t.fn&&t.scope===r.scope&&t.fn==r.fn},initStruct:function(t,r,e){return r.$module&&r.$module.$impl===r&&(r=r.$module),t.$parent=r,rtl.isModule(r)?(t.$module=r,t.$name=e):(t.$module=r.$module,t.$name=r.$name+"."+e),r},initClass:function(t,r,e,n){r[e]=t,t.$class=t,t.$classname=e,r=rtl.initStruct(t,r,e),t.$fullname=r.$name+"."+e,rtl.debug_rtti&&rtl.debug("initClass "+t.$fullname),r=t.$module.$rtti.$Class(t.$name,{class:t}),t.$rtti=r,rtl.isObject(t.$ancestor)&&(r.ancestor=t.$ancestor.$rtti),r.ancestor||(r.ancestor=null),n.call(t)},createClass:function(t,r,e,n){var i=null;null!=e?(i=Object.create(e)).$ancestor=e:i={$create:function(t,r){null==r&&(r=[]);var e=Object.create(this);e.$init();try{"string"==typeof t?e[t].apply(e,r):t.apply(e,r),e.AfterConstruction()}catch(t){throw e.Destroy&&e.Destroy(),e.$final(),t}return e},$destroy:function(t){this.BeforeDestruction(),this[t]&&this[t](),this.$final()}},rtl.initClass(i,t,r,n)},createClassExt:function(t,r,e,n,i){(e=Object.create(e)).$create=function(t,r){null==r&&(r=[]);var e=0<n.length?this[n](t,r):Object.create(this);e.$init&&e.$init();try{"string"==typeof t?e[t].apply(e,r):t.apply(e,r),e.AfterConstruction&&e.AfterConstruction()}catch(t){throw e.Destroy&&e.Destroy(),e.$final&&this.$final(),t}return e},e.$destroy=function(t){this.BeforeDestruction&&this.BeforeDestruction(),this[t]&&this[t](),this.$final&&this.$final()},rtl.initClass(e,t,r,i)},createHelper:function(t,r,e,n){if(null!=e){var i=Object.create(e);i.$ancestor=e}else i={};t[r]=i,i.$class=i,i.$classname=r,t=rtl.initStruct(i,t,r),i.$fullname=t.$name+"."+r,t=i.$module.$rtti.$Helper(i.$name,{helper:i}),i.$rtti=t,rtl.isObject(e)&&(t.ancestor=e.$rtti),t.ancestor||(t.ancestor=null),n.call(i)},tObjectDestroy:"Destroy",free:function(t,r){if(null==t[r])return null;t[r].$destroy(rtl.tObjectDestroy),t[r]=null},freeLoc:function(t){return null==t?null:(t.$destroy(rtl.tObjectDestroy),null)},recNewT:function(t,r,e,n){function i(t){Object.defineProperty(l,t,{enumerable:!1})}var l={};return t&&(t[r]=l),n&&(rtl.initStruct(l,t,r),l.$record=l,i("$record"),i("$name"),i("$parent"),i("$module")),e.call(l),l.$new||(l.$new=function(){return Object.create(this)}),l.$clone=function(t){return this.$new().$assign(t)},i("$new"),i("$clone"),i("$eq"),i("$assign"),l},is:function(t,r){return r.isPrototypeOf(t)||t===r},isExt:function(t,r,e){return null!=t&&("object"==typeof r||"function"==typeof r)&&(t===r?1!==e&&(2!==e||rtl.isPasClass(t)):r.isPrototypeOf&&r.isPrototypeOf(t)?1===e?rtl.isPasClassInstance(t):2!==e||rtl.isPasClass(t):"function"==typeof r&&t instanceof r)},Exception:null,EInvalidCast:null,EAbstractError:null,ERangeError:null,EIntOverflow:null,EPropWriteOnly:null,raiseE:function(t){var r=rtl[t];if(null==r){var e=pas.SysUtils;e||(e=pas.sysutils),e&&((r=e[t])||(r=e[t.toLowerCase()]),r||(r=e.Exception),r||(r=e.exception))}if(r){if(r.Create)throw r.$create("Create");if(r.create)throw r.$create("create")}if("EInvalidCast"===t)throw"invalid type cast";if("EAbstractError"===t)throw"Abstract method called";if("ERangeError"===t)throw"range error";throw t},as:function(t,r){if(null===t||rtl.is(t,r))return t;rtl.raiseE("EInvalidCast")},asExt:function(t,r,e){if(null===t||rtl.isExt(t,r,e))return t;rtl.raiseE("EInvalidCast")},createInterface:function(t,r,e,n,i,l){var o=i?Object.create(i):{};return t[r]=o,o.$module=t,o.$name=r,o.$fullname=t.$name+"."+r,o.$guid=e,o.$guidr=null,o.$names=n||[],rtl.isFunction(l)&&(rtl.debug_rtti&&rtl.debug("createInterface "+o.$fullname),t=o.$module.$rtti.$Interface(r,{interface:o,module:t}),o.$rtti=t,i&&(t.ancestor=i.$rtti),t.ancestor||(t.ancestor=null),l.call(o)),o},strToGUIDR:function(t,r){function e(r){var e=t.substr(n,r);return n+=r,parseInt(e,16)}var n=0;n+=1,r.D1=e(8),n+=1,r.D2=e(4),n+=1,r.D3=e(4),n+=1,r.D4||(r.D4=[]),r.D4[0]=e(2),r.D4[1]=e(2),n+=1;for(var i=2;8>i;i++)r.D4[i]=e(2);return r},guidrToStr:function(t){if(t.$intf)return t.$intf.$guid;for(var r=rtl.hexStr,e="{"+r(t.D1,8)+"-"+r(t.D2,4)+"-"+r(t.D3,4)+"-"+r(t.D4[0],2)+r(t.D4[1],2)+"-",n=2;8>n;n++)e+=r(t.D4[n],2);return e+"}"},createTGUID:function(t){return rtl.strToGUIDR(t,(pas.System?pas.System.TGuid:pas.system.tguid).$new())},getIntfGUIDR:function(t){if(!t)return null;if(!t.$guidr){var r=rtl.createTGUID(t.$guid);t.hasOwnProperty("$guid")||(t=Object.getPrototypeOf(t)),r.$intf=t,t.$guidr=r}return t.$guidr},addIntf:function(t,r,e){function n(t){return"function"==typeof t?function(){return t.apply(this.$o,arguments)}:function(){rtl.raiseE("EAbstractError")}}e||(e={});var i=r,l=Object.create(i);t.hasOwnProperty("$intfmaps")||(t.$intfmaps={}),t.$intfmaps[r.$guid]=l;do{if(!(r=i.$names))break;for(var o=0;o<r.length;o++){var u=r[o],a=e[u];a||(a=u),l[u]=n(t[a])}i=Object.getPrototypeOf(i)}while(null!=i)},getIntfG:function(t,r,e){if(!t)return null;var n=t.$intfmaps;if(!n||!(n=n[r]))return null;if("function"==typeof n)return n.call(t);var i=null;if(t.$interfaces&&(i=t.$interfaces[r]),i||((i=Object.create(n)).$o=t,t.$interfaces||(t.$interfaces={}),t.$interfaces[r]=i),"object"==typeof e){var l=null;return 0===i.QueryInterface(rtl.getIntfGUIDR(e),{get:function(){return l},set:function(t){l=t}})?l:null}return i},getIntfT:function(t,r){return rtl.getIntfG(t,r.$guid)},queryIntfT:function(t,r){return rtl.getIntfG(t,r.$guid,r)},queryIntfIsT:function(t,r){return!!rtl.queryIntfG(t,r.$guid)},asIntfT:function(t,r){var e=rtl.getIntfG(t,r.$guid);if(null!==e)return e;rtl.raiseEInvalidCast()},intfIsClass:function(t,r){return null!=t&&rtl.is(t.$o,r)},intfAsClass:function(t,r){return null==t?null:rtl.as(t.$o,r)},intfToClass:function(t,r){return null!==t&&rtl.is(t.$o,r)?t.$o:null},intfRefs:{ref:function(t,r){return this[t]&&delete this[t],this[t]=r},free:function(){for(var t in this)this.hasOwnProperty(t)}},createIntfRefs:function(){return Object.create(rtl.intfRefs)},setIntfP:function(t,r,e){var n=t[r];n!==e&&(null!==n&&(t[r]=null),null!==e&&(t[r]=e))},setIntfL:function(t,r){return r},_AddRef:function(t){return t},_Release:function(t){return t},checkMethodCall:function(t,r){rtl.isObject(t)&&rtl.is(t,r)||rtl.raiseE("EInvalidCast")},oc:function(t){if(Math.floor(t)===t&&-9007199254740991<=t&&9007199254740991>=t)return t;rtl.raiseE("EIntOverflow")},rc:function(t,r,e){if(Math.floor(t)===t&&t>=r&&t<=e)return t;rtl.raiseE("ERangeError")},rcc:function(t,r,e){if("string"==typeof t&&1===t.length){var n=t.charCodeAt(0);if(n>=r&&n<=e)return t}rtl.raiseE("ERangeError")},rcSetCharAt:function(t,r,e){return("string"!=typeof t||0>r||r>=t.length)&&rtl.raiseE("ERangeError"),rtl.setCharAt(t,r,e)},rcCharAt:function(t,r){return("string"!=typeof t||0>r||r>=t.length)&&rtl.raiseE("ERangeError"),t.charAt(r)},rcArrR:function(t,r){if(Array.isArray(t)&&"number"==typeof r&&0<=r&&r<t.length){if(2<arguments.length){t=t[r];for(var e=2;e<arguments.length;e++)t=rtl.rcArrR(t,arguments[e]);return t}return t[r]}rtl.raiseE("ERangeError")},rcArrW:function(t,r,e){for(var n=3;n<arguments.length;n++)t=rtl.rcArrR(t,r),r=arguments[n-1],e=arguments[n];if(Array.isArray(t)&&"number"==typeof r&&0<=r&&r<t.length)return t[r]=e;rtl.raiseE("ERangeError")},length:function(t){return null==t?0:t.length},arraySetLength:function(t,r,e){var n=arguments;return function t(i,l){var o=n[l],u=[];if(u.length=e,l===n.length-1){var a=i?i.length:0;if(rtl.isArray(r))for(var s=0;s<o;s++)u[s]=s<a?i[s]:[];else if(rtl.isObject(r))if(rtl.isTRecord(r))for(s=0;s<o;s++)u[s]=s<a?r.$clone(i[s]):r.$new();else for(s=0;s<o;s++)u[s]=s<a?rtl.refSet(i[s]):{};else for(s=0;s<o;s++)u[s]=s<a?i[s]:r}else for(s=0;s<o;s++)u[s]=t(i?i[s]:null,l+1);return u}(t,2)},arrayEq:function(t,r){if(null===t)return null===r;if(null===r||t.length!==r.length)return!1;for(var e=0;e<t.length;e++)if(t[e]!==r[e])return!1;return!0},arrayClone:function(t,r,e,n,i,l){if("refSet"===t)for(;e<n;e++)i[l++]=rtl.refSet(r[e]);else if(rtl.isTRecord(t))for(;e<n;e++)i[l++]=t.$clone(r[e]);else for(;e<n;e++)i[l++]=r[e]},arrayConcat:function(t){for(var r=[],e=0,n=1;n<arguments.length;n++){var i=arguments[n];null!==i&&(e+=i.length)}for(r.length=e,e=0,n=1;n<arguments.length;n++)null!==(i=arguments[n])&&(rtl.arrayClone(t,i,0,i.length,r,e),e+=i.length);return r},arrayConcatN:function(){for(var t=null,r=1;r<arguments.length;r++){var e=arguments[r];null!==e&&(t=null===t?e:t.concat(e))}return t},arrayCopy:function(t,r,e,n){if(null===r||(0>e&&(e=0),void 0===n&&(n=r.length),(n=e+n)>r.length&&(n=r.length),e>=n))return[];if(0===t)return r.slice(e,n);var i=[];return i.length=n-e,rtl.arrayClone(t,r,e,n,i,0),i},setCharAt:function(t,r,e){return t.substr(0,r)+e+t.substr(r+1)},getResStr:function(t,r){var e=t.$resourcestrings[r];return e.current?e.current:e.org},createSet:function(){for(var t={},r=0;r<arguments.length;r++)if(null!=arguments[r])t[arguments[r]]=!0;else for(var e=arguments[r+=1],n=arguments[r+=1];e<=n;e++)t[e]=!0;return t},cloneSet:function(t){var r,e={};for(r in t)e[r]=!0;return e},refSet:function(t){return Object.defineProperty(t,"$shared",{enumerable:!1,configurable:!0,writable:!0,value:!0}),t},includeSet:function(t,r){return t.$shared&&(t=rtl.cloneSet(t)),t[r]=!0,t},excludeSet:function(t,r){return t.$shared&&(t=rtl.cloneSet(t)),delete t[r],t},diffSet:function(t,r){var e,n={};for(e in t)r[e]||(n[e]=!0);return n},unionSet:function(t,r){var e,n={};for(e in t)n[e]=!0;for(e in r)n[e]=!0;return n},intersectSet:function(t,r){var e,n={};for(e in t)r[e]&&(n[e]=!0);return n},symDiffSet:function(t,r){var e,n={};for(e in t)r[e]||(n[e]=!0);for(e in r)t[e]||(n[e]=!0);return n},eqSet:function(t,r){for(var e in t)if(!r[e])return!1;for(e in r)if(!t[e])return!1;return!0},neSet:function(t,r){return!rtl.eqSet(t,r)},leSet:function(t,r){for(var e in t)if(!r[e])return!1;return!0},geSet:function(t,r){for(var e in r)if(!t[e])return!1;return!0},strSetLength:function(t,r){var e=t.length;if(e>r)return t.substring(0,r);if(t.repeat)return t+" ".repeat(r-e);for(;e<r;)t+=" ",e++;return t},spaceLeft:function(t,r){var e=t.length;if(e>=r)return t;if(t.repeat)return" ".repeat(r-e)+t;for(;e<r;)t=" "+t,e++},floatToStr:function(t,r,e){if(2<arguments.length)return rtl.spaceLeft(t.toFixed(e),r);var n="",i=Math.abs(t);return 1e10>i?n="00":1e100>i&&(n="0"),2>arguments.length?r=9:9>r&&(r=9),i=(i=(0<t?" ":"")+t.toExponential(r-8)).replace(/e(.)/,"E$1"+n),rtl.spaceLeft(i,r)},valEnum:function(t,r,e){for(var n in t=t.toLowerCase(),r)if("string"==typeof n&&n.toLowerCase()===t)return e(0),r[n];return e(1),0},and:function(t,r){return 2147483648*(t/2147483648&r/2147483648)+(2147483647&t&r&2147483647)},or:function(t,r){return 2147483648*(t/2147483648|r/2147483648)+(2147483647&t|2147483647&r)},xor:function(t,r){return 2147483648*(t/2147483648^r/2147483648)+(2147483647&t^2147483647&r)},shr:function(t,r){return 0>t&&(t+=rtl.hiInt),2147483648>t?t>>r:0>=r?t:54<r?0:Math.floor(t/Math.pow(2,r))},shl:function(t,r){if(0>t&&(t+=rtl.hiInt),0>=r)return t;if(54<r)return 0;var e=t*Math.pow(2,r);return e<=rtl.hiInt?e:e%rtl.hiInt},initRTTI:function(){function t(t,r,e){return e||(e=rtl.tTypeInfo),rtl.debug_rtti&&rtl.debug('initRTTI.newBaseTI "'+t+'" '+r+' ("'+e.name+'")'),(e=Object.create(e)).name=t,e.kind=r,rtl[t]=e}function r(r,e,n,i){return(r=t(r,1,rtl.tTypeInfoInteger)).minvalue=e,r.maxvalue=n,r.ordtype=i,r}function e(t,r){var e=Object.create(rtl.tTypeMember);e.name=t,e.kind=r,rtl[t]=e}rtl.debug_rtti&&rtl.debug("initRTTI"),rtl.tTypeInfo={name:"tTypeInfo"},t("tTypeInfoInteger",1),r("shortint",-128,127,0),r("byte",0,255,1),r("smallint",-32768,32767,2),r("word",0,65535,3),r("longint",-2147483648,2147483647,4),r("longword",0,4294967295,5),r("nativeint",-4503599627370496,0xfffffffffffff,6),r("nativeuint",0,0xfffffffffffff,7),t("char",2),t("string",3),t("tTypeInfoEnum",4,rtl.tTypeInfoInteger),t("tTypeInfoSet",5),t("double",6),t("boolean",7),t("tTypeInfoProcVar",8),t("tTypeInfoMethodVar",9,rtl.tTypeInfoProcVar),t("tTypeInfoArray",10),t("tTypeInfoDynArray",11),t("tTypeInfoPointer",15),t("pointer",15,rtl.tTypeInfoPointer).reftype=null,t("jsvalue",16),t("tTypeInfoRefToProcVar",17,rtl.tTypeInfoProcVar),rtl.tTypeMember={},e("tTypeMemberField",1),e("tTypeMemberMethod",2),e("tTypeMemberProperty",3),rtl.tTypeMembers={};var n=t("tTypeInfoStruct",0);n.$addMember=function(t,r,e){if(rtl.debug_rtti){if(!rtl.hasString(t)||"$"===t.charAt())throw'invalid member "'+t+'", this="'+this.name+'"';if(!rtl.is(r,rtl.tTypeMember))throw'invalid ancestor "'+r+":"+r.name+'", "'+this.name+"."+t+'"';if(null!=e&&"object"!=typeof e)throw'invalid options "'+e+'", "'+this.name+"."+t+'"'}if((r=Object.create(r)).name=t,this.members[t]=r,this.names.push(t),rtl.isObject(e))for(var n in e)e.hasOwnProperty(n)&&(r[n]=e[n]);return r},n.addField=function(t,r,e){if(e=this.$addMember(t,rtl.tTypeMemberField,e),rtl.debug_rtti&&!rtl.is(r,rtl.tTypeInfo))throw'invalid type "'+r+'", "'+this.name+"."+t+'"';return e.typeinfo=r,this.fields.push(t),e},n.addFields=function(){for(var t=0;t<arguments.length;){var r=arguments[t++],e=arguments[t++];t<arguments.length&&"object"==typeof arguments[t]?this.addField(r,e,arguments[t++]):this.addField(r,e)}},n.addMethod=function(t,r,e,n,i){return(i=this.$addMember(t,rtl.tTypeMemberMethod,i)).methodkind=r,i.procsig=rtl.newTIProcSig(e),i.procsig.resulttype=n||null,this.methods.push(t),i},n.addProperty=function(t,r,e,n,i,l){return(l=this.$addMember(t,rtl.tTypeMemberProperty,l)).flags=r,l.typeinfo=e,l.getter=n,l.setter=i,rtl.isArray(l.params)&&(l.params=rtl.newTIParams(l.params)),this.properties.push(t),rtl.isString(l.stored)||(l.stored=""),l},n.getField=function(t){return this.members[this.fields[t]]},n.getMethod=function(t){return this.members[this.methods[t]]},n.getProperty=function(t){return this.members[this.properties[t]]},t("tTypeInfoRecord",12,rtl.tTypeInfoStruct),t("tTypeInfoClass",13,rtl.tTypeInfoStruct),t("tTypeInfoClassRef",14),t("tTypeInfoInterface",18,rtl.tTypeInfoStruct),t("tTypeInfoHelper",19,rtl.tTypeInfoStruct)},tSectionRTTI:{$module:null,$inherited:function(t,r,e){rtl.debug_rtti&&rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")+'"."'+t+'" ('+r.name+") "+(e?"init":"forward"));var n=this[t];if(n){if(!n.$forward)throw'duplicate type "'+t+'"';if(!r.isPrototypeOf(n))throw'typeinfo ancestor mismatch "'+t+'" ancestor="'+r.name+'" t.name="'+n.name+'"'}else(n=Object.create(r)).name=t,n.$module=this.$module,this[t]=n;if(e)for(var i in delete n.$forward,e)e.hasOwnProperty(i)&&(n[i]=e[i]);else n.$forward=!0;return n},$Scope:function(t,r,e){return(t=this.$inherited(t,r,e)).members={},t.names=[],t.fields=[],t.methods=[],t.properties=[],t},$TI:function(t,r,e){return(t=this.$inherited(t,rtl.tTypeInfo,e)).kind=r,t},$Int:function(t,r){return this.$inherited(t,rtl.tTypeInfoInteger,r)},$Enum:function(t,r){return this.$inherited(t,rtl.tTypeInfoEnum,r)},$Set:function(t,r){return this.$inherited(t,rtl.tTypeInfoSet,r)},$StaticArray:function(t,r){return this.$inherited(t,rtl.tTypeInfoArray,r)},$DynArray:function(t,r){return this.$inherited(t,rtl.tTypeInfoDynArray,r)},$ProcVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoProcVar,r)},$RefToProcVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoRefToProcVar,r)},$MethodVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoMethodVar,r)},$Record:function(t,r){return this.$Scope(t,rtl.tTypeInfoRecord,r)},$Class:function(t,r){return this.$Scope(t,rtl.tTypeInfoClass,r)},$ClassRef:function(t,r){return this.$inherited(t,rtl.tTypeInfoClassRef,r)},$Pointer:function(t,r){return this.$inherited(t,rtl.tTypeInfoPointer,r)},$Interface:function(t,r){return this.$Scope(t,rtl.tTypeInfoInterface,r)},$Helper:function(t,r){return this.$Scope(t,rtl.tTypeInfoHelper,r)}},newTIParam:function(t){return{name:t[0],typeinfo:t[1],flags:rtl.isNumber(t[2])?t[2]:0}},newTIParams:function(t){var r=[];if(rtl.isArray(t))for(var e=0;e<t.length;e++)r.push(rtl.newTIParam(t[e]));return r},newTIProcSig:function(t,r,e){return{params:rtl.newTIParams(t),resulttype:r,flags:e}}};rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
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
    if ((Index < 1) || (Index > S.get().length) || (Size <= 0)) return;
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
     else Target.set($mod.Copy(t,1,Index - 1) + Insertion + $mod.Copy(t,Index,t.length));
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SArgumentMissing: {org: 'Missing argument in format "%s"'}, SInvalidFormat: {org: 'Invalid format specifier : "%s"'}, SInvalidArgIndex: {org: 'Invalid argument index in format: "%s"'}, SListCapacityError: {org: "List capacity (%s) exceeded."}, SListCountError: {org: "List count (%s) out of bounds."}, SListIndexError: {org: "List index (%s) out of bounds"}, SSortedListError: {org: "Operation not allowed on sorted list"}, SDuplicateString: {org: "String list does not allow duplicates"}, SErrFindNeedsSortedList: {org: "Cannot use find on unsorted list"}, SInvalidName: {org: 'Invalid component name: "%s"'}, SInvalidBoolean: {org: '"%s" is not a valid boolean.'}, SDuplicateName: {org: 'Duplicate component name: "%s"'}, SErrInvalidDate: {org: 'Invalid date: "%s"'}, SErrInvalidTimeFormat: {org: 'Invalid time format: "%s"'}, SInvalidDateFormat: {org: 'Invalid date format: "%s"'}, SCantReadPropertyS: {org: 'Cannot read property "%s"'}, SCantWritePropertyS: {org: 'Cannot write property "%s"'}, SErrPropertyNotFound: {org: 'Unknown property: "%s"'}, SIndexedPropertyNeedsParams: {org: 'Indexed property "%s" needs parameters'}, SErrInvalidTypecast: {org: "Invalid class typecast"}, SErrInvalidInteger: {org: 'Invalid integer value: "%s"'}, SErrInvalidFloat: {org: 'Invalid floating-point value: "%s"'}, SInvalidDateTime: {org: "Invalid date-time value: %s"}, SInvalidCurrency: {org: "Invalid currency value: %s"}, SErrInvalidDayOfWeek: {org: "%d is not a valid day of the week"}, SErrInvalidTimeStamp: {org: 'Invalid date\/timestamp : "%s"'}, SErrInvalidDateWeek: {org: "%d %d %d is not a valid dateweek"}, SErrInvalidDayOfYear: {org: "Year %d does not have a day number %d"}, SErrInvalidDateMonthWeek: {org: "Year %d, month %d, Week %d and day %d is not a valid date."}, SErrInvalidDayOfWeekInMonth: {org: "Year %d Month %d NDow %d DOW %d is not a valid date"}, SInvalidJulianDate: {org: "%f Julian cannot be represented as a DateTime"}, SErrInvalidHourMinuteSecMsec: {org: "%d:%d:%d.%d is not a valid time specification"}, SInvalidGUID: {org: '"%s" is not a valid GUID value'}, SEmptyStreamIllegalReader: {org: "Illegal Nil stream for TReader constructor"}, SInvalidPropertyValue: {org: "Invalid value for property"}, SInvalidImage: {org: "Invalid stream format"}, SUnknownProperty: {org: 'Unknown property: "%s"'}, SUnknownPropertyType: {org: "Unknown property type %d"}, SAncestorNotFound: {org: 'Ancestor class for "%s" not found.'}, SUnsupportedPropertyVariantType: {org: "Unsupported property variant type %d"}, SPropertyException: {org: "Error reading %s%s%s: %s"}, SInvalidPropertyPath: {org: "Invalid property path"}, SReadOnlyProperty: {org: "Property is read-only"}, SClassNotFound: {org: 'Class "%s" not found'}, SEmptyStreamIllegalWriter: {org: "Illegal Nil stream for TWriter constructor"}, SErrInvalidPropertyType: {org: "Invalid property type from streamed property: %d"}, SParserExpected: {org: "Wrong token type: %s expected"}, SParserInvalidFloat: {org: "Invalid floating point number: %s"}, SParserInvalidInteger: {org: "Invalid integer number: %s"}, SParserUnterminatedString: {org: "Unterminated string"}, SParserWrongTokenType: {org: "Wrong token type: %s expected but %s found"}, SParserWrongTokenSymbol: {org: "Wrong token symbol: %s expected but %s found"}, SParserLocInfo: {org: " (at %d,%d, stream offset %.8x)"}, SParserUnterminatedBinValue: {org: "Unterminated byte value"}, SParserInvalidProperty: {org: "Invalid property"}};
});
rtl.module("JS",["System"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,1,15,0);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === 1) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === 2) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === 0) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === 3) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator)}
     else if ($tmp1 === 4) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,DS,$mod.ThousandSeparator);
    if ((format !== 4) && (Result.length > 1) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    return Result;
  };
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 2;
  this.CurrencyString = "$";
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.maxdigits = 15;
  $impl.ReplaceDecimalSep = function (S, DS) {
    var Result = "";
    var P = 0;
    P = pas.System.Pos(".",S);
    if (P > 0) {
      Result = pas.System.Copy(S,1,P - 1) + DS + pas.System.Copy(S,P + 1,S.length - P)}
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
      Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - 48;
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
        while ((Result.charAt(P - 1) === "0") && (P < Result.length) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
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
        Result = rtl.setCharAt(Result,P - Exponent - 1,Result.charAt(P - Exponent - 1 - 1));
        Result = rtl.setCharAt(Result,P - 1,".");
        if (Exponent !== -1) Result = rtl.setCharAt(Result,P - Exponent - 1 - 1,"0");
      };
      Q = Result.length;
      while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
      if (Result.charAt(Q - 1) === ".") Q -= 1;
      if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
        Result = "0"}
       else Result = rtl.strSetLength(Result,Q);
    } else {
      while (Result.charAt(PE - 1 - 1) === "0") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt(PE - 1 - 1) === DS) {
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
    Digits = (Result.length - P - Digits) + 1;
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
      if (Result.charAt(P - 1 - 1) !== "-") pas.System.Insert(TS,{get: function () {
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
      Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get().charAt(i - 1) === TS);
      if (!Result) break;
    };
    if (Result && (AValue.get().charAt(0) === "-")) pas.System.Delete(AValue,1,1);
    return Result;
  };
  $impl.FormatNumberCurrency = function (Value, Digits, DS, TS) {
    var Result = "";
    var Negative = false;
    var P = 0;
    if (Digits === -1) {
      Digits = $mod.CurrencyDecimals}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value / 10000,0,Digits);
    Negative = Result.charAt(0) === "-";
    if (Negative) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    if (TS !== "") {
      if (P !== 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS)}
       else P = Result.length + 1;
      P -= 3;
      while (P > 1) {
        pas.System.Insert(TS,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P);
        P -= 3;
      };
    };
    if (Negative) $impl.RemoveLeadingNegativeSign({get: function () {
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
        Result = $mod.CurrencyString + " " + Result}
       else if ($tmp1 === 3) Result = Result + " " + $mod.CurrencyString;
    } else {
      var $tmp2 = $mod.NegCurrFormat;
      if ($tmp2 === 0) {
        Result = "(" + $mod.CurrencyString + Result + ")"}
       else if ($tmp2 === 1) {
        Result = "-" + $mod.CurrencyString + Result}
       else if ($tmp2 === 2) {
        Result = $mod.CurrencyString + "-" + Result}
       else if ($tmp2 === 3) {
        Result = $mod.CurrencyString + Result + "-"}
       else if ($tmp2 === 4) {
        Result = "(" + Result + $mod.CurrencyString + ")"}
       else if ($tmp2 === 5) {
        Result = "-" + Result + $mod.CurrencyString}
       else if ($tmp2 === 6) {
        Result = Result + "-" + $mod.CurrencyString}
       else if ($tmp2 === 7) {
        Result = Result + $mod.CurrencyString + "-"}
       else if ($tmp2 === 8) {
        Result = "-" + Result + " " + $mod.CurrencyString}
       else if ($tmp2 === 9) {
        Result = "-" + $mod.CurrencyString + " " + Result}
       else if ($tmp2 === 10) {
        Result = Result + " " + $mod.CurrencyString + "-"}
       else if ($tmp2 === 11) {
        Result = $mod.CurrencyString + " " + Result + "-"}
       else if ($tmp2 === 12) {
        Result = $mod.CurrencyString + " " + "-" + Result}
       else if ($tmp2 === 13) {
        Result = Result + "-" + " " + $mod.CurrencyString}
       else if ($tmp2 === 14) {
        Result = "(" + $mod.CurrencyString + " " + Result + ")"}
       else if ($tmp2 === 15) Result = "(" + Result + " " + $mod.CurrencyString + ")";
    };
    return Result;
  };
});
rtl.module("Classes",["System","SysUtils","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$init = function () {
    $impl.ClassList = Object.create(null);
  };
},[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ClassList = null;
});
rtl.module("Web",["System","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Math",["System"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("uLoader",["System","Classes","SysUtils","Math","JS","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.Preloader = function () {
    var Result = null;
    var preLoader = null;
    var mainLoader = null;
    var URI = "";
    var i = 0;
    var ring = null;
    var text = null;
    var r = 0;
    var rv = 0;
    var degrees = 0.0;
    var rad = 0.0;
    var x = "";
    var y = "";
    var lenghty = 0;
    var descriptions = [];
    Result = new Promise(function (resolve, reject) {
      URI = window.document.location.href.substring(0,window.document.location.href.lastIndexOf("\/"));
      preLoader = new rscLoader();
      preLoader.load(URI + "\/images\/download.png","image");
      preLoader.listen("all",function (stats) {
        if (stats.overall > stats.ready) return;
        mainLoader = new rscLoader();
        for (i = 0; i <= 6; i++) mainLoader.load(URI + "\/images\/serverdelay.css?delay=2&" + ("" + (new Date()).valueOf()) + pas.SysUtils.IntToStr(i) + pas.SysUtils.IntToStr(i) + "&type=stylesheet","stylesheet");
        for (i = 0; i <= 21; i++) mainLoader.load(URI + "\/images\/serverdelay.js?delay=1&" + ("" + (new Date()).valueOf()) + pas.SysUtils.IntToStr(i) + pas.SysUtils.IntToStr(i) + "&type=javascript","javascript");
        for (i = 0; i <= 22; i++) mainLoader.load(URI + "\/images\/download.png?delay=1&" + ("" + (new Date()).valueOf()) + pas.SysUtils.IntToStr(i) + pas.SysUtils.IntToStr(i) + "&type=image","image");
        mainLoader.load(URI + "\/css\/photo-1516646255117-f9f933680173.jpg","image");
        mainLoader.listen("all",function (stats) {
          ring = document.getElementsByTagName("path").item(0);
          text = document.getElementsByTagName("text").item(0);
          r = 50;
          rv = Math.round(((100 * stats.ready) / stats.overall) / 1);
          degrees = $impl.BounceOut(rv / 100,0.5,0.5,1) * 100 * 3.5999;
          rad = degrees * (Math.PI / 180);
          x =  (Math.sin(rad) * r).toFixed(2);
          y =  (-Math.cos(rad) * r).toFixed(2);
          lenghty = $impl.BoolToInt(degrees > 180);
          descriptions = ["M",0,0,"v",-r,"A",r,r,1,lenghty,1,x,y,"z"];
          ring.setAttribute("d",descriptions.join(" "));
          text.textContent = pas.SysUtils.FloatToStr(Math.round($impl.BounceOut(rv / 100,0.5,0.5,1) * 100)) + "%";
          ring.setAttribute("transform","translate(185,240)");
          if (stats.ready === stats.overall) {
            resolve(true);
            window.console.log("Ready!");
            document.querySelector(".pas2js_loader").setAttribute("style","display:none;");
          };
        });
      });
    });
    return Result;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.BounceOut = function (T, B, C, D) {
    var Result = 0.0;
    T = T / D;
    if (T < (1 / 2.75)) {
      Result = (C * (7.5625 * T * T)) + B}
     else if (T < (2 / 2.75)) {
      T = T - (1.5 / 2.75);
      Result = (C * ((7.5625 * T * T) + 0.75)) + B;
    } else if (T < (2.5 / 2.75)) {
      T = T - (2.25 / 2.75);
      Result = (C * ((7.5625 * T * T) + 0.9375)) + B;
    } else {
      T = T - (2.625 / 2.75);
      Result = (C * ((7.5625 * T * T) + 0.984375)) + B;
    };
    return Result;
  };
  $impl.BoolToInt = function (aValue) {
    if (aValue){return 1}else{return  0};
  };
});
rtl.module("program",["System","SysUtils","Classes","JS","Web","uLoader"],function () {
  "use strict";
  var $mod = this;
  this.circ = null;
  $mod.$main = function () {
    pas.uLoader.Preloader().then(function (aStatus) {
      var Result = undefined;
      if (aStatus) {
        $mod.circ = new Object();
        $mod.circ.radius = 10;
        $mod.circ.area = function () {
          var Result = 0.0;
          Result = Math.PI * $mod.circ.radius * $mod.circ.radius;
          return Result;
        };
        window.console.log($mod.circ.area());
      };
      return Result;
    });
  };
});
//# sourceMappingURL=project1.js.map
