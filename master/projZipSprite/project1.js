var pas={},rtl={version:10501,quiet:!1,debug_load_units:!1,debug_rtti:!1,debug:function(){!rtl.quiet&&console&&console.log&&console.log(arguments)},error:function(t){throw rtl.debug("Error: ",t),t},warn:function(t){rtl.debug("Warn: ",t)},checkVersion:function(t){if(rtl.version!=t)throw"expected rtl version "+t+", but found "+rtl.version},hiInt:Math.pow(2,53),hasString:function(t){return rtl.isString(t)&&0<t.length},isArray:function(t){return Array.isArray(t)},isFunction:function(t){return"function"==typeof t},isModule:function(t){return rtl.isObject(t)&&rtl.hasString(t.$name)&&pas[t.$name]===t},isImplementation:function(t){return rtl.isObject(t)&&rtl.isModule(t.$module)&&t.$module.$impl===t},isNumber:function(t){return"number"==typeof t},isObject:function(t){return"object"==typeof t&&null!=t},isString:function(t){return"string"==typeof t},getNumber:function(t){return"number"==typeof t?t:NaN},getChar:function(t){return"string"==typeof t&&1===t.length?t:""},getObject:function(t){return"object"==typeof t||"function"==typeof t?t:null},isTRecord:function(t){return rtl.isObject(t)&&t.hasOwnProperty("$new")&&"function"==typeof t.$new},isPasClass:function(t){return rtl.isObject(t)&&t.hasOwnProperty("$classname")&&rtl.isObject(t.$module)},isPasClassInstance:function(t){return rtl.isObject(t)&&rtl.isPasClass(t.$class)},hexStr:function(t,r){return("000000000000000"+t.toString(16).toUpperCase()).slice(-r)},m_loading:0,m_loading_intf:1,m_intf_loaded:2,m_loading_impl:3,m_initializing:4,m_initialized:5,module:function(t,r,e,n,i){rtl.debug_load_units&&rtl.debug('rtl.module name="'+t+'" intfuses='+r+" impluses="+n+" hasimplcode="+rtl.isFunction(i)),rtl.hasString(t)||rtl.error('invalid module name "'+t+'"'),rtl.isArray(r)||rtl.error('invalid interface useslist of "'+t+'"'),rtl.isFunction(e)||rtl.error('invalid interface code of "'+t+'"'),null==n||rtl.isArray(n)||rtl.error('invalid implementation useslist of "'+t+'"'),null==i||rtl.isFunction(i)||rtl.error('invalid implementation code of "'+t+'"'),pas[t]&&rtl.error('module "'+t+'" is already registered'),(t=pas[t]={$name:t,$intfuseslist:r,$impluseslist:n,$state:rtl.m_loading,$intfcode:e,$implcode:i,$impl:null,$rtti:Object.create(rtl.tSectionRTTI)}).$rtti.$module=t,i&&(t.$impl={$module:t,$rtti:t.$rtti})},exitcode:0,run:function(t){function r(){rtl.hasString(t)||(t="program"),rtl.debug_load_units&&rtl.debug('rtl.run module="'+t+'"'),rtl.initRTTI();var r=pas[t];r||rtl.error('rtl.run module "'+t+'" missing'),rtl.loadintf(r),rtl.loadimpl(r),"program"==t&&(rtl.debug_load_units&&rtl.debug("running $main"),r=pas.program.$main(),rtl.isNumber(r)&&(rtl.exitcode=r))}if(rtl.showUncaughtExceptions)try{r()}catch(t){var e=rtl.hasString(t.$classname)?t.$classname:"";e+=(e?": ":"")+(t.hasOwnProperty("fMessage")?t.fMessage:t),alert("Uncaught Exception : "+e),rtl.exitCode=216}else r();return rtl.exitcode},loadintf:function(t){t.$state>rtl.m_loading_intf||(rtl.debug_load_units&&rtl.debug('loadintf: "'+t.$name+'"'),t.$state===rtl.m_loading_intf&&rtl.error('unit cycle detected "'+t.$name+'"'),t.$state=rtl.m_loading_intf,rtl.loaduseslist(t,t.$intfuseslist,rtl.loadintf),rtl.debug_load_units&&rtl.debug('loadintf: run intf of "'+t.$name+'"'),t.$intfcode(t.$intfuseslist),t.$state=rtl.m_intf_loaded)},loaduseslist:function(t,r,e){if(null!=r)for(var n in r){var i=r[n];rtl.debug_load_units&&rtl.debug('loaduseslist of "'+t.$name+'" uses="'+i+'"'),null==pas[i]&&rtl.error('module "'+t.$name+'" misses "'+i+'"'),e(pas[i])}},loadimpl:function(t){t.$state>=rtl.m_loading_impl||(t.$state<rtl.m_intf_loaded&&rtl.error('loadimpl: interface not loaded of "'+t.$name+'"'),rtl.debug_load_units&&rtl.debug('loadimpl: load uses of "'+t.$name+'"'),t.$state=rtl.m_loading_impl,rtl.loaduseslist(t,t.$impluseslist,rtl.loadintf),rtl.loaduseslist(t,t.$intfuseslist,rtl.loadimpl),rtl.loaduseslist(t,t.$impluseslist,rtl.loadimpl),rtl.debug_load_units&&rtl.debug('loadimpl: run impl of "'+t.$name+'"'),rtl.isFunction(t.$implcode)&&t.$implcode(t.$impluseslist),rtl.debug_load_units&&rtl.debug('loadimpl: run init of "'+t.$name+'"'),t.$state=rtl.m_initializing,rtl.isFunction(t.$init)&&t.$init(),t.$state=rtl.m_initialized)},createCallback:function(t,r){var e="string"==typeof r?function(){return t[r].apply(t,arguments)}:function(){return r.apply(t,arguments)};return e.scope=t,e.fn=r,e},cloneCallback:function(t){return rtl.createCallback(t.scope,t.fn)},eqCallback:function(t,r){return t==r||null!=t&&null!=r&&t.fn&&t.scope===r.scope&&t.fn==r.fn},initStruct:function(t,r,e){return r.$module&&r.$module.$impl===r&&(r=r.$module),t.$parent=r,rtl.isModule(r)?(t.$module=r,t.$name=e):(t.$module=r.$module,t.$name=r.$name+"."+e),r},initClass:function(t,r,e,n){r[e]=t,t.$class=t,t.$classname=e,r=rtl.initStruct(t,r,e),t.$fullname=r.$name+"."+e,rtl.debug_rtti&&rtl.debug("initClass "+t.$fullname),r=t.$module.$rtti.$Class(t.$name,{class:t}),t.$rtti=r,rtl.isObject(t.$ancestor)&&(r.ancestor=t.$ancestor.$rtti),r.ancestor||(r.ancestor=null),n.call(t)},createClass:function(t,r,e,n){var i=null;null!=e?(i=Object.create(e)).$ancestor=e:i={$create:function(t,r){null==r&&(r=[]);var e=Object.create(this);e.$init();try{"string"==typeof t?e[t].apply(e,r):t.apply(e,r),e.AfterConstruction()}catch(t){throw e.Destroy&&e.Destroy(),e.$final(),t}return e},$destroy:function(t){this.BeforeDestruction(),this[t]&&this[t](),this.$final()}},rtl.initClass(i,t,r,n)},createClassExt:function(t,r,e,n,i){(e=Object.create(e)).$create=function(t,r){null==r&&(r=[]);var e=0<n.length?this[n](t,r):Object.create(this);e.$init&&e.$init();try{"string"==typeof t?e[t].apply(e,r):t.apply(e,r),e.AfterConstruction&&e.AfterConstruction()}catch(t){throw e.Destroy&&e.Destroy(),e.$final&&this.$final(),t}return e},e.$destroy=function(t){this.BeforeDestruction&&this.BeforeDestruction(),this[t]&&this[t](),this.$final&&this.$final()},rtl.initClass(e,t,r,i)},createHelper:function(t,r,e,n){if(null!=e){var i=Object.create(e);i.$ancestor=e}else i={};t[r]=i,i.$class=i,i.$classname=r,t=rtl.initStruct(i,t,r),i.$fullname=t.$name+"."+r,t=i.$module.$rtti.$Helper(i.$name,{helper:i}),i.$rtti=t,rtl.isObject(e)&&(t.ancestor=e.$rtti),t.ancestor||(t.ancestor=null),n.call(i)},tObjectDestroy:"Destroy",free:function(t,r){if(null==t[r])return null;t[r].$destroy(rtl.tObjectDestroy),t[r]=null},freeLoc:function(t){return null==t?null:(t.$destroy(rtl.tObjectDestroy),null)},recNewT:function(t,r,e,n){function i(t){Object.defineProperty(l,t,{enumerable:!1})}var l={};return t&&(t[r]=l),n&&(rtl.initStruct(l,t,r),l.$record=l,i("$record"),i("$name"),i("$parent"),i("$module")),e.call(l),l.$new||(l.$new=function(){return Object.create(this)}),l.$clone=function(t){return this.$new().$assign(t)},i("$new"),i("$clone"),i("$eq"),i("$assign"),l},is:function(t,r){return r.isPrototypeOf(t)||t===r},isExt:function(t,r,e){return null!=t&&("object"==typeof r||"function"==typeof r)&&(t===r?1!==e&&(2!==e||rtl.isPasClass(t)):r.isPrototypeOf&&r.isPrototypeOf(t)?1===e?rtl.isPasClassInstance(t):2!==e||rtl.isPasClass(t):"function"==typeof r&&t instanceof r)},Exception:null,EInvalidCast:null,EAbstractError:null,ERangeError:null,EIntOverflow:null,EPropWriteOnly:null,raiseE:function(t){var r=rtl[t];if(null==r){var e=pas.SysUtils;e||(e=pas.sysutils),e&&((r=e[t])||(r=e[t.toLowerCase()]),r||(r=e.Exception),r||(r=e.exception))}if(r){if(r.Create)throw r.$create("Create");if(r.create)throw r.$create("create")}if("EInvalidCast"===t)throw"invalid type cast";if("EAbstractError"===t)throw"Abstract method called";if("ERangeError"===t)throw"range error";throw t},as:function(t,r){if(null===t||rtl.is(t,r))return t;rtl.raiseE("EInvalidCast")},asExt:function(t,r,e){if(null===t||rtl.isExt(t,r,e))return t;rtl.raiseE("EInvalidCast")},createInterface:function(t,r,e,n,i,l){var o=i?Object.create(i):{};return t[r]=o,o.$module=t,o.$name=r,o.$fullname=t.$name+"."+r,o.$guid=e,o.$guidr=null,o.$names=n||[],rtl.isFunction(l)&&(rtl.debug_rtti&&rtl.debug("createInterface "+o.$fullname),t=o.$module.$rtti.$Interface(r,{interface:o,module:t}),o.$rtti=t,i&&(t.ancestor=i.$rtti),t.ancestor||(t.ancestor=null),l.call(o)),o},strToGUIDR:function(t,r){function e(r){var e=t.substr(n,r);return n+=r,parseInt(e,16)}var n=0;n+=1,r.D1=e(8),n+=1,r.D2=e(4),n+=1,r.D3=e(4),n+=1,r.D4||(r.D4=[]),r.D4[0]=e(2),r.D4[1]=e(2),n+=1;for(var i=2;8>i;i++)r.D4[i]=e(2);return r},guidrToStr:function(t){if(t.$intf)return t.$intf.$guid;for(var r=rtl.hexStr,e="{"+r(t.D1,8)+"-"+r(t.D2,4)+"-"+r(t.D3,4)+"-"+r(t.D4[0],2)+r(t.D4[1],2)+"-",n=2;8>n;n++)e+=r(t.D4[n],2);return e+"}"},createTGUID:function(t){return rtl.strToGUIDR(t,(pas.System?pas.System.TGuid:pas.system.tguid).$new())},getIntfGUIDR:function(t){if(!t)return null;if(!t.$guidr){var r=rtl.createTGUID(t.$guid);t.hasOwnProperty("$guid")||(t=Object.getPrototypeOf(t)),r.$intf=t,t.$guidr=r}return t.$guidr},addIntf:function(t,r,e){function n(t){return"function"==typeof t?function(){return t.apply(this.$o,arguments)}:function(){rtl.raiseE("EAbstractError")}}e||(e={});var i=r,l=Object.create(i);t.hasOwnProperty("$intfmaps")||(t.$intfmaps={}),t.$intfmaps[r.$guid]=l;do{if(!(r=i.$names))break;for(var o=0;o<r.length;o++){var u=r[o],a=e[u];a||(a=u),l[u]=n(t[a])}i=Object.getPrototypeOf(i)}while(null!=i)},getIntfG:function(t,r,e){if(!t)return null;var n=t.$intfmaps;if(!n||!(n=n[r]))return null;if("function"==typeof n)return n.call(t);var i=null;if(t.$interfaces&&(i=t.$interfaces[r]),i||((i=Object.create(n)).$o=t,t.$interfaces||(t.$interfaces={}),t.$interfaces[r]=i),"object"==typeof e){var l=null;return 0===i.QueryInterface(rtl.getIntfGUIDR(e),{get:function(){return l},set:function(t){l=t}})?l:null}return i},getIntfT:function(t,r){return rtl.getIntfG(t,r.$guid)},queryIntfT:function(t,r){return rtl.getIntfG(t,r.$guid,r)},queryIntfIsT:function(t,r){return!!rtl.queryIntfG(t,r.$guid)},asIntfT:function(t,r){var e=rtl.getIntfG(t,r.$guid);if(null!==e)return e;rtl.raiseEInvalidCast()},intfIsClass:function(t,r){return null!=t&&rtl.is(t.$o,r)},intfAsClass:function(t,r){return null==t?null:rtl.as(t.$o,r)},intfToClass:function(t,r){return null!==t&&rtl.is(t.$o,r)?t.$o:null},intfRefs:{ref:function(t,r){return this[t]&&delete this[t],this[t]=r},free:function(){for(var t in this)this.hasOwnProperty(t)}},createIntfRefs:function(){return Object.create(rtl.intfRefs)},setIntfP:function(t,r,e){var n=t[r];n!==e&&(null!==n&&(t[r]=null),null!==e&&(t[r]=e))},setIntfL:function(t,r){return r},_AddRef:function(t){return t},_Release:function(t){return t},checkMethodCall:function(t,r){rtl.isObject(t)&&rtl.is(t,r)||rtl.raiseE("EInvalidCast")},oc:function(t){if(Math.floor(t)===t&&-9007199254740991<=t&&9007199254740991>=t)return t;rtl.raiseE("EIntOverflow")},rc:function(t,r,e){if(Math.floor(t)===t&&t>=r&&t<=e)return t;rtl.raiseE("ERangeError")},rcc:function(t,r,e){if("string"==typeof t&&1===t.length){var n=t.charCodeAt(0);if(n>=r&&n<=e)return t}rtl.raiseE("ERangeError")},rcSetCharAt:function(t,r,e){return("string"!=typeof t||0>r||r>=t.length)&&rtl.raiseE("ERangeError"),rtl.setCharAt(t,r,e)},rcCharAt:function(t,r){return("string"!=typeof t||0>r||r>=t.length)&&rtl.raiseE("ERangeError"),t.charAt(r)},rcArrR:function(t,r){if(Array.isArray(t)&&"number"==typeof r&&0<=r&&r<t.length){if(2<arguments.length){t=t[r];for(var e=2;e<arguments.length;e++)t=rtl.rcArrR(t,arguments[e]);return t}return t[r]}rtl.raiseE("ERangeError")},rcArrW:function(t,r,e){for(var n=3;n<arguments.length;n++)t=rtl.rcArrR(t,r),r=arguments[n-1],e=arguments[n];if(Array.isArray(t)&&"number"==typeof r&&0<=r&&r<t.length)return t[r]=e;rtl.raiseE("ERangeError")},length:function(t){return null==t?0:t.length},arraySetLength:function(t,r,e){var n=arguments;return function t(i,l){var o=n[l],u=[];if(u.length=e,l===n.length-1){var a=i?i.length:0;if(rtl.isArray(r))for(var s=0;s<o;s++)u[s]=s<a?i[s]:[];else if(rtl.isObject(r))if(rtl.isTRecord(r))for(s=0;s<o;s++)u[s]=s<a?r.$clone(i[s]):r.$new();else for(s=0;s<o;s++)u[s]=s<a?rtl.refSet(i[s]):{};else for(s=0;s<o;s++)u[s]=s<a?i[s]:r}else for(s=0;s<o;s++)u[s]=t(i?i[s]:null,l+1);return u}(t,2)},arrayEq:function(t,r){if(null===t)return null===r;if(null===r||t.length!==r.length)return!1;for(var e=0;e<t.length;e++)if(t[e]!==r[e])return!1;return!0},arrayClone:function(t,r,e,n,i,l){if("refSet"===t)for(;e<n;e++)i[l++]=rtl.refSet(r[e]);else if(rtl.isTRecord(t))for(;e<n;e++)i[l++]=t.$clone(r[e]);else for(;e<n;e++)i[l++]=r[e]},arrayConcat:function(t){for(var r=[],e=0,n=1;n<arguments.length;n++){var i=arguments[n];null!==i&&(e+=i.length)}for(r.length=e,e=0,n=1;n<arguments.length;n++)null!==(i=arguments[n])&&(rtl.arrayClone(t,i,0,i.length,r,e),e+=i.length);return r},arrayConcatN:function(){for(var t=null,r=1;r<arguments.length;r++){var e=arguments[r];null!==e&&(t=null===t?e:t.concat(e))}return t},arrayCopy:function(t,r,e,n){if(null===r||(0>e&&(e=0),void 0===n&&(n=r.length),(n=e+n)>r.length&&(n=r.length),e>=n))return[];if(0===t)return r.slice(e,n);var i=[];return i.length=n-e,rtl.arrayClone(t,r,e,n,i,0),i},setCharAt:function(t,r,e){return t.substr(0,r)+e+t.substr(r+1)},getResStr:function(t,r){var e=t.$resourcestrings[r];return e.current?e.current:e.org},createSet:function(){for(var t={},r=0;r<arguments.length;r++)if(null!=arguments[r])t[arguments[r]]=!0;else for(var e=arguments[r+=1],n=arguments[r+=1];e<=n;e++)t[e]=!0;return t},cloneSet:function(t){var r,e={};for(r in t)e[r]=!0;return e},refSet:function(t){return Object.defineProperty(t,"$shared",{enumerable:!1,configurable:!0,writable:!0,value:!0}),t},includeSet:function(t,r){return t.$shared&&(t=rtl.cloneSet(t)),t[r]=!0,t},excludeSet:function(t,r){return t.$shared&&(t=rtl.cloneSet(t)),delete t[r],t},diffSet:function(t,r){var e,n={};for(e in t)r[e]||(n[e]=!0);return n},unionSet:function(t,r){var e,n={};for(e in t)n[e]=!0;for(e in r)n[e]=!0;return n},intersectSet:function(t,r){var e,n={};for(e in t)r[e]&&(n[e]=!0);return n},symDiffSet:function(t,r){var e,n={};for(e in t)r[e]||(n[e]=!0);for(e in r)t[e]||(n[e]=!0);return n},eqSet:function(t,r){for(var e in t)if(!r[e])return!1;for(e in r)if(!t[e])return!1;return!0},neSet:function(t,r){return!rtl.eqSet(t,r)},leSet:function(t,r){for(var e in t)if(!r[e])return!1;return!0},geSet:function(t,r){for(var e in r)if(!t[e])return!1;return!0},strSetLength:function(t,r){var e=t.length;if(e>r)return t.substring(0,r);if(t.repeat)return t+" ".repeat(r-e);for(;e<r;)t+=" ",e++;return t},spaceLeft:function(t,r){var e=t.length;if(e>=r)return t;if(t.repeat)return" ".repeat(r-e)+t;for(;e<r;)t=" "+t,e++},floatToStr:function(t,r,e){if(2<arguments.length)return rtl.spaceLeft(t.toFixed(e),r);var n="",i=Math.abs(t);return 1e10>i?n="00":1e100>i&&(n="0"),2>arguments.length?r=9:9>r&&(r=9),i=(i=(0<t?" ":"")+t.toExponential(r-8)).replace(/e(.)/,"E$1"+n),rtl.spaceLeft(i,r)},valEnum:function(t,r,e){for(var n in t=t.toLowerCase(),r)if("string"==typeof n&&n.toLowerCase()===t)return e(0),r[n];return e(1),0},and:function(t,r){return 2147483648*(t/2147483648&r/2147483648)+(2147483647&t&r&2147483647)},or:function(t,r){return 2147483648*(t/2147483648|r/2147483648)+(2147483647&t|2147483647&r)},xor:function(t,r){return 2147483648*(t/2147483648^r/2147483648)+(2147483647&t^2147483647&r)},shr:function(t,r){return 0>t&&(t+=rtl.hiInt),2147483648>t?t>>r:0>=r?t:54<r?0:Math.floor(t/Math.pow(2,r))},shl:function(t,r){if(0>t&&(t+=rtl.hiInt),0>=r)return t;if(54<r)return 0;var e=t*Math.pow(2,r);return e<=rtl.hiInt?e:e%rtl.hiInt},initRTTI:function(){function t(t,r,e){return e||(e=rtl.tTypeInfo),rtl.debug_rtti&&rtl.debug('initRTTI.newBaseTI "'+t+'" '+r+' ("'+e.name+'")'),(e=Object.create(e)).name=t,e.kind=r,rtl[t]=e}function r(r,e,n,i){return(r=t(r,1,rtl.tTypeInfoInteger)).minvalue=e,r.maxvalue=n,r.ordtype=i,r}function e(t,r){var e=Object.create(rtl.tTypeMember);e.name=t,e.kind=r,rtl[t]=e}rtl.debug_rtti&&rtl.debug("initRTTI"),rtl.tTypeInfo={name:"tTypeInfo"},t("tTypeInfoInteger",1),r("shortint",-128,127,0),r("byte",0,255,1),r("smallint",-32768,32767,2),r("word",0,65535,3),r("longint",-2147483648,2147483647,4),r("longword",0,4294967295,5),r("nativeint",-4503599627370496,0xfffffffffffff,6),r("nativeuint",0,0xfffffffffffff,7),t("char",2),t("string",3),t("tTypeInfoEnum",4,rtl.tTypeInfoInteger),t("tTypeInfoSet",5),t("double",6),t("boolean",7),t("tTypeInfoProcVar",8),t("tTypeInfoMethodVar",9,rtl.tTypeInfoProcVar),t("tTypeInfoArray",10),t("tTypeInfoDynArray",11),t("tTypeInfoPointer",15),t("pointer",15,rtl.tTypeInfoPointer).reftype=null,t("jsvalue",16),t("tTypeInfoRefToProcVar",17,rtl.tTypeInfoProcVar),rtl.tTypeMember={},e("tTypeMemberField",1),e("tTypeMemberMethod",2),e("tTypeMemberProperty",3),rtl.tTypeMembers={};var n=t("tTypeInfoStruct",0);n.$addMember=function(t,r,e){if(rtl.debug_rtti){if(!rtl.hasString(t)||"$"===t.charAt())throw'invalid member "'+t+'", this="'+this.name+'"';if(!rtl.is(r,rtl.tTypeMember))throw'invalid ancestor "'+r+":"+r.name+'", "'+this.name+"."+t+'"';if(null!=e&&"object"!=typeof e)throw'invalid options "'+e+'", "'+this.name+"."+t+'"'}if((r=Object.create(r)).name=t,this.members[t]=r,this.names.push(t),rtl.isObject(e))for(var n in e)e.hasOwnProperty(n)&&(r[n]=e[n]);return r},n.addField=function(t,r,e){if(e=this.$addMember(t,rtl.tTypeMemberField,e),rtl.debug_rtti&&!rtl.is(r,rtl.tTypeInfo))throw'invalid type "'+r+'", "'+this.name+"."+t+'"';return e.typeinfo=r,this.fields.push(t),e},n.addFields=function(){for(var t=0;t<arguments.length;){var r=arguments[t++],e=arguments[t++];t<arguments.length&&"object"==typeof arguments[t]?this.addField(r,e,arguments[t++]):this.addField(r,e)}},n.addMethod=function(t,r,e,n,i){return(i=this.$addMember(t,rtl.tTypeMemberMethod,i)).methodkind=r,i.procsig=rtl.newTIProcSig(e),i.procsig.resulttype=n||null,this.methods.push(t),i},n.addProperty=function(t,r,e,n,i,l){return(l=this.$addMember(t,rtl.tTypeMemberProperty,l)).flags=r,l.typeinfo=e,l.getter=n,l.setter=i,rtl.isArray(l.params)&&(l.params=rtl.newTIParams(l.params)),this.properties.push(t),rtl.isString(l.stored)||(l.stored=""),l},n.getField=function(t){return this.members[this.fields[t]]},n.getMethod=function(t){return this.members[this.methods[t]]},n.getProperty=function(t){return this.members[this.properties[t]]},t("tTypeInfoRecord",12,rtl.tTypeInfoStruct),t("tTypeInfoClass",13,rtl.tTypeInfoStruct),t("tTypeInfoClassRef",14),t("tTypeInfoInterface",18,rtl.tTypeInfoStruct),t("tTypeInfoHelper",19,rtl.tTypeInfoStruct)},tSectionRTTI:{$module:null,$inherited:function(t,r,e){rtl.debug_rtti&&rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")+'"."'+t+'" ('+r.name+") "+(e?"init":"forward"));var n=this[t];if(n){if(!n.$forward)throw'duplicate type "'+t+'"';if(!r.isPrototypeOf(n))throw'typeinfo ancestor mismatch "'+t+'" ancestor="'+r.name+'" t.name="'+n.name+'"'}else(n=Object.create(r)).name=t,n.$module=this.$module,this[t]=n;if(e)for(var i in delete n.$forward,e)e.hasOwnProperty(i)&&(n[i]=e[i]);else n.$forward=!0;return n},$Scope:function(t,r,e){return(t=this.$inherited(t,r,e)).members={},t.names=[],t.fields=[],t.methods=[],t.properties=[],t},$TI:function(t,r,e){return(t=this.$inherited(t,rtl.tTypeInfo,e)).kind=r,t},$Int:function(t,r){return this.$inherited(t,rtl.tTypeInfoInteger,r)},$Enum:function(t,r){return this.$inherited(t,rtl.tTypeInfoEnum,r)},$Set:function(t,r){return this.$inherited(t,rtl.tTypeInfoSet,r)},$StaticArray:function(t,r){return this.$inherited(t,rtl.tTypeInfoArray,r)},$DynArray:function(t,r){return this.$inherited(t,rtl.tTypeInfoDynArray,r)},$ProcVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoProcVar,r)},$RefToProcVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoRefToProcVar,r)},$MethodVar:function(t,r){return this.$inherited(t,rtl.tTypeInfoMethodVar,r)},$Record:function(t,r){return this.$Scope(t,rtl.tTypeInfoRecord,r)},$Class:function(t,r){return this.$Scope(t,rtl.tTypeInfoClass,r)},$ClassRef:function(t,r){return this.$inherited(t,rtl.tTypeInfoClassRef,r)},$Pointer:function(t,r){return this.$inherited(t,rtl.tTypeInfoPointer,r)},$Interface:function(t,r){return this.$Scope(t,rtl.tTypeInfoInterface,r)},$Helper:function(t,r){return this.$Scope(t,rtl.tTypeInfoHelper,r)}},newTIParam:function(t){return{name:t[0],typeinfo:t[1],flags:rtl.isNumber(t[2])?t[2]:0}},newTIParams:function(t){var r=[];if(rtl.isArray(t))for(var e=0;e<t.length;e++)r.push(rtl.newTIParam(t[e]));return r},newTIProcSig:function(t,r,e){return{params:rtl.newTIParams(t),resulttype:r,flags:e}}};rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
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
  this.Int = function (A) {
    var Result = 0.0;
    Result = $mod.Trunc(A);
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
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    NI.set($impl.valint(S,-9007199254740991,9007199254740991,Code));
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
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
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.valint = function (S, MinVal, MaxVal, Code) {
    var Result = 0;
    var x = 0.0;
    x = Number(S);
    if (isNaN(x)) {
      var $tmp1 = $mod.Copy(S,1,1);
      if ($tmp1 === "$") {
        x = Number("0x" + $mod.Copy$1(S,2))}
       else if ($tmp1 === "&") {
        x = Number("0o" + $mod.Copy$1(S,2))}
       else if ($tmp1 === "%") {
        x = Number("0b" + $mod.Copy$1(S,2))}
       else {
        Code.set(1);
        return Result;
      };
    };
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else if ((x < MinVal) || (x > MaxVal)) {
      Code.set(2)}
     else {
      Result = $mod.Trunc(x);
      Code.set(0);
    };
    return Result;
  };
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SArgumentMissing: {org: 'Missing argument in format "%s"'}, SInvalidFormat: {org: 'Invalid format specifier : "%s"'}, SInvalidArgIndex: {org: 'Invalid argument index in format: "%s"'}, SListCapacityError: {org: "List capacity (%s) exceeded."}, SListCountError: {org: "List count (%s) out of bounds."}, SListIndexError: {org: "List index (%s) out of bounds"}, SSortedListError: {org: "Operation not allowed on sorted list"}, SDuplicateString: {org: "String list does not allow duplicates"}, SErrFindNeedsSortedList: {org: "Cannot use find on unsorted list"}, SInvalidName: {org: 'Invalid component name: "%s"'}, SInvalidBoolean: {org: '"%s" is not a valid boolean.'}, SDuplicateName: {org: 'Duplicate component name: "%s"'}, SErrInvalidDate: {org: 'Invalid date: "%s"'}, SErrInvalidTimeFormat: {org: 'Invalid time format: "%s"'}, SInvalidDateFormat: {org: 'Invalid date format: "%s"'}, SCantReadPropertyS: {org: 'Cannot read property "%s"'}, SCantWritePropertyS: {org: 'Cannot write property "%s"'}, SErrPropertyNotFound: {org: 'Unknown property: "%s"'}, SIndexedPropertyNeedsParams: {org: 'Indexed property "%s" needs parameters'}, SErrInvalidTypecast: {org: "Invalid class typecast"}, SErrInvalidInteger: {org: 'Invalid integer value: "%s"'}, SErrInvalidFloat: {org: 'Invalid floating-point value: "%s"'}, SInvalidDateTime: {org: "Invalid date-time value: %s"}, SInvalidCurrency: {org: "Invalid currency value: %s"}, SErrInvalidDayOfWeek: {org: "%d is not a valid day of the week"}, SErrInvalidTimeStamp: {org: 'Invalid date\/timestamp : "%s"'}, SErrInvalidDateWeek: {org: "%d %d %d is not a valid dateweek"}, SErrInvalidDayOfYear: {org: "Year %d does not have a day number %d"}, SErrInvalidDateMonthWeek: {org: "Year %d, month %d, Week %d and day %d is not a valid date."}, SErrInvalidDayOfWeekInMonth: {org: "Year %d Month %d NDow %d DOW %d is not a valid date"}, SInvalidJulianDate: {org: "%f Julian cannot be represented as a DateTime"}, SErrInvalidHourMinuteSecMsec: {org: "%d:%d:%d.%d is not a valid time specification"}, SInvalidGUID: {org: '"%s" is not a valid GUID value'}, SEmptyStreamIllegalReader: {org: "Illegal Nil stream for TReader constructor"}, SInvalidPropertyValue: {org: "Invalid value for property"}, SInvalidImage: {org: "Invalid stream format"}, SUnknownProperty: {org: 'Unknown property: "%s"'}, SUnknownPropertyType: {org: "Unknown property type %d"}, SAncestorNotFound: {org: 'Ancestor class for "%s" not found.'}, SUnsupportedPropertyVariantType: {org: "Unsupported property variant type %d"}, SPropertyException: {org: "Error reading %s%s%s: %s"}, SInvalidPropertyPath: {org: "Invalid property path"}, SReadOnlyProperty: {org: "Property is read-only"}, SClassNotFound: {org: 'Class "%s" not found'}, SEmptyStreamIllegalWriter: {org: "Illegal Nil stream for TWriter constructor"}, SErrInvalidPropertyType: {org: "Invalid property type from streamed property: %d"}, SParserExpected: {org: "Wrong token type: %s expected"}, SParserInvalidFloat: {org: "Invalid floating point number: %s"}, SParserInvalidInteger: {org: "Invalid integer number: %s"}, SParserUnterminatedString: {org: "Unterminated string"}, SParserWrongTokenType: {org: "Wrong token type: %s expected but %s found"}, SParserWrongTokenSymbol: {org: "Wrong token symbol: %s expected but %s found"}, SParserLocInfo: {org: " (at %d,%d, stream offset %.8x)"}, SParserUnterminatedBinValue: {org: "Unterminated byte value"}, SParserInvalidProperty: {org: "Invalid property"}};
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
      Result = 0}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = 4}
       else if (t === "boolean") {
        Result = 1}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = 6}
         else Result = 5;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = 2}
       else Result = 3;
    };
    return Result;
  };
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
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      return this;
    };
    this.CreateFmt = function (Msg, Args) {
      this.Create$1($mod.Format(Msg,Args));
      return this;
    };
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
        while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9") && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
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
          Checkarg(2,true);
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
          Checkarg(2,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),0,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),0,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),1,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),3,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(3,false) || Checkarg(2,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),4,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(4,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(2,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(2,true);
          vq = Math.floor(Args[DoArg]);
          Index = 31;
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while ((rtl.shl(1,Index * 4) <= vq) && (Index < 16)) Index += 1;
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
    if (0 in Flags) REFlags = "g";
    if (1 in Flags) REFlags = REFlags + "i";
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
      Value = Math.floor(Value / 16);
    };
    while (Result.length < Digits) Result = "0" + Result;
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
  this.TryStrToFloat$1 = function (S, res) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if ($mod.ThousandSeparator !== "") N = $mod.StringReplace(N,$mod.ThousandSeparator,"",rtl.createSet(0));
    if ($mod.DecimalSeparator !== ".") N = $mod.StringReplace(N,$mod.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  this.StrToFloat = function (S) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SErrInvalidFloat"),[S]]);
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
  $impl.feInvalidFormat = 1;
  $impl.feMissingArgument = 2;
  $impl.feInvalidArgIndex = 3;
  $impl.DoFormatError = function (ErrCode, fmt) {
    var $tmp1 = ErrCode;
    if ($tmp1 === 1) {
      throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidFormat"),[fmt]])}
     else if ($tmp1 === 2) {
      throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SArgumentMissing"),[fmt]])}
     else if ($tmp1 === 3) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidArgIndex"),[fmt]]);
  };
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
  $impl.RESpecials = "([\\+\\[\\]\\(\\)\\\\\\.\\*])";
});
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Classes",["System","SysUtils","RTLConsts","Types","JS"],function () {
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
rtl.module("Math",["System"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("uMain",["System","Classes","SysUtils","JS","Web","Types"],function () {
  "use strict";
  var $mod = this;
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
});
rtl.module("uResources",["System","JS","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.RT_RCDATA = null;
  this.LoadFromResourceName = function (resourceName) {
    var Result = "";
    Result = $impl.Sprite.createURL(resourceName);
    return Result;
  };
  $mod.$init = function () {
    $mod.RT_RCDATA = Promise.resolve().then(function (blob) {
      var Result = undefined;
      window.console.log("RESOURCES HAS BEEN LOADED!");
      Result = LoadResourcesFromZipFile("projZipSprite.res");
      return Result;
    }).then(function (blob) {
      var Result = undefined;
      $impl.Sprite = new window.RES(blob);
      Result = $impl.Sprite;
      return Result;
    });
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.Sprite = null;
});
rtl.module("uFishFacts",["System","SysUtils","Classes","JS","Web","Types","Math","uMain","uResources"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.recNewT($mod,"TFishRecord",function () {
    this.$eq = function (b) {
      return (this.Category === b.Category) && (this.Common_Name === b.Common_Name) && (this.Length_Cm === b.Length_Cm) && (this.Length_In === b.Length_In) && (this.Notes === b.Notes) && (this.Species_Name === b.Species_Name) && (this.Species_No === b.Species_No);
    };
    this.$assign = function (s) {
      this.Category = s.Category;
      this.Common_Name = s.Common_Name;
      this.Length_Cm = s.Length_Cm;
      this.Length_In = s.Length_In;
      this.Notes = s.Notes;
      this.Species_Name = s.Species_Name;
      this.Species_No = s.Species_No;
      return this;
    };
  });
  rtl.createClass($mod,"TJFishFacts",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fishRecord = $mod.TFishRecord.$new();
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
      Result = rtl.getObject(JSON.parse($impl.fishJsonData));
      return Result;
    };
    this.JSON2TFishRecord = function (Value) {
      var Result = $mod.TFishRecord.$new();
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
      var $Self = this;
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
      pas.System.TObject.Create.call($Self);
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
      pas.uResources.RT_RCDATA.then(function () {
        img_ = document.createElement("IMG");
        img_.setAttribute("height","665");
        img_.setAttribute("width","648");
        img_.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/nintendo1.svg"));
        div_0.appendChild(img_);
      });
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
      pas.uResources.RT_RCDATA.then(function () {
        img_0 = document.createElement("IMG");
        img_0.setAttribute("height","225");
        img_0.setAttribute("width","225");
        img_0.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/peixeA.png"));
        div_1.appendChild(img_0);
      });
      smsfishpeixeb = document.createElement("DIV");
      smsfishpeixeb.setAttribute("id","smsfish-peixeB");
      smsfishpanelsub.appendChild(smsfishpeixeb);
      div_2 = document.createElement("DIV");
      smsfishpeixeb.appendChild(div_2);
      pas.uResources.RT_RCDATA.then(function () {
        img_1 = document.createElement("IMG");
        img_1.setAttribute("height","225");
        img_1.setAttribute("width","225");
        img_1.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/peixeB.png"));
        div_2.appendChild(img_1);
      });
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
      pas.uResources.RT_RCDATA.then(function () {
        img_2 = document.createElement("IMG");
        img_2.setAttribute("height","162");
        img_2.setAttribute("width","404");
        img_2.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/pas2js.png"));
        div_3.appendChild(img_2);
      });
      smsfishpaneltop = document.createElement("DIV");
      smsfishpaneltop.setAttribute("id","smsfish-panelTop");
      smsfishligar.appendChild(smsfishpaneltop);
      smsfishundersea = document.createElement("DIV");
      smsfishundersea.setAttribute("id","smsfish-undersea");
      smsfishpaneltop.appendChild(smsfishundersea);
      div_4 = document.createElement("DIV");
      smsfishundersea.appendChild(div_4);
      pas.uResources.RT_RCDATA.then(function () {
        img_3 = document.createElement("IMG");
        img_3.setAttribute("height","170");
        img_3.setAttribute("width","790");
        img_3.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/undersea.jpg"));
        div_4.appendChild(img_3);
      });
      smsfishfishfacts = document.createElement("DIV");
      smsfishfishfacts.setAttribute("id","smsfish-fishfacts");
      smsfishpaneltop.appendChild(smsfishfishfacts);
      div_5 = document.createElement("DIV");
      smsfishfishfacts.appendChild(div_5);
      pas.uResources.RT_RCDATA.then(function () {
        img_4 = document.createElement("IMG");
        img_4.setAttribute("height","83");
        img_4.setAttribute("width","232");
        img_4.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/fishfacts.png"));
        div_5.appendChild(img_4);
      });
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
      pas.uResources.RT_RCDATA.then(function () {
        img_5 = document.createElement("IMG");
        img_5.setAttribute("height","72");
        img_5.setAttribute("width","75");
        img_5.setAttribute("src",pas.uResources.LoadFromResourceName("assets\/tomate.png"));
        div_6.appendChild(img_5);
      });
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
      return this;
    };
    this.downClick = function (Sender) {
      if (this.selectedIndex > 0) {
        this.selectedIndex -= 1;
        this.fishRecord.$assign(this.JSON2TFishRecord(rtl.getObject(this.getList()[pas.SysUtils.IntToStr(this.selectedIndex)])));
        this.selectionChange();
      };
    };
    this.upClick = function (Sender) {
      if (this.selectedIndex < (this.list.length - 1)) {
        this.selectedIndex += 1;
        this.fishRecord.$assign(this.JSON2TFishRecord(rtl.getObject(this.getList()[pas.SysUtils.IntToStr(this.selectedIndex)])));
        this.selectionChange();
      };
    };
    this.refreshFacts = function () {
      this.list = this.getList();
      if (this.list.length > 0) this.selectedIndex = 0;
      this.selectionChange();
    };
    this.selectionChange = function () {
      var $Self = this;
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
      if (!(($Self.list.length === 0) || ($Self.selectedIndex === ($Self.list.length - 1)))) {
        rightBtn.removeAttribute("disabled")}
       else rightBtn.setAttribute("disabled","true");
      if (!(($Self.list.length === 0) || ($Self.selectedIndex === 0))) {
        leftBtn.removeAttribute("disabled")}
       else leftBtn.setAttribute("disabled","true");
      pas.uResources.RT_RCDATA.then(function () {
        pictureImg.setAttribute("src",pas.uResources.LoadFromResourceName("pics\/" + $Self.fishRecord.Species_No + ".png"));
      });
      about.innerHTML = "<b>About the " + $Self.fishRecord.Common_Name + "<\/b>";
      category.textContent = $Self.fishRecord.Category;
      specieName.textContent = $Self.fishRecord.Species_Name;
      lenCm.textContent = $Self.roundNumber(pas.SysUtils.StrToFloat($Self.fishRecord.Length_Cm),2);
      lenIn.textContent = $Self.roundNumber(pas.SysUtils.StrToFloat($Self.fishRecord.Length_In),2);
      memo.textContent = $Self.fishRecord.Notes;
    };
    this.InitializeObject = function () {
      document.querySelector("#smsfish-an-scene-0").style.setProperty("webkitTransition","none");
      document.querySelector("#smsfish-an-scene-0").classList.add("paused");
      pas.uMain.bindEvent(document.querySelector("#smsfish-hideTable"),"webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend",rtl.createCallback(this,"callbackA"));
      pas.uMain.bindEvent(document.querySelector("#smsfish-ON"),"click",rtl.createCallback(this,"callbackB"));
      pas.uMain.bindEvent(document.querySelector("#smsfish-R"),"click",rtl.createCallback(this,"callbackC"));
      pas.uMain.bindEvent(document.querySelector("#smsfish-L"),"click",rtl.createCallback(this,"callbackD"));
      this.fishRecord.$assign(this.JSON2TFishRecord(rtl.getObject(this.getList()["0"])));
      this.refreshFacts();
    };
    var $r = this.$rtti;
    $r.addMethod("InitializeObject",0,null);
  });
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.fishJsonData = "[" + '{"Species_No":"90020","Category":"Triggerfishy","Common_Name":"Clown Triggerfish","Species_Name":"Ballistoides conspicillum","Length_Cm":"50","Length_In":"19.6850393700787","Notes":"Also known as the big spotted triggerfish.  Inhabits outer reef areas and feeds upon crustaceans and mollusks by crushing them with powerful teeth.  They are voracious eaters, and divers report seeing the clown triggerfish devour beds of pearl oysters.\\r\\n\\r\\nDo not eat this fish.  According to an 1878 account, \\"the poisonous flesh acts primarily upon the nervous tissue of the stomach, occasioning violent spasms of that organ, and shortly afterwards all the muscles of the body.  The frame becomes rocked with spasms, the tongue thickened, the eye fixed, the breathing laborious, and the patient expires in a paroxysm of extreme suffering.\\"\\r\\n\\r\\nNot edible.\\r\\n\\r\\nRange is Indo-Pacific and East Africa to Somoa."},' + '{"Species_No":"90030","Category":"Snapper","Common_Name":"Red Emperor","Species_Name":"Lutjanus sebae","Length_Cm":"60","Length_In":"23.6220472440945","Notes":"Called seaperch in Australia.  Inhabits the areas around lagoon coral reefs and sandy bottoms.\\r\\n\\r\\nThe red emperor is a valuable food fish and considered a great sporting fish that fights with fury when hooked.  The flesh of an old fish is just as tender to eat as that of the very young.\\r\\n\\r\\nRange is from the Indo-Pacific to East Africa."},' + '{"Species_No":"90050","Category":"Wrasse","Common_Name":"Giant Maori Wrasse","Species_Name":"Cheilinus undulatus","Length_Cm":"229","Length_In":"90.15748031496059","Notes":"This is the largest of all the wrasse.  It is found in dense reef areas, feeding on a wide variety of mollusks, fishes, sea urchins, crustaceans, and other invertebrates. In spite of its immense size, divers find it a very wary fish.\\r\\n\\r\\nEdibility is considered poor.\\r\\n\\r\\nRange is the Indo-Pacific and the Red Sea."},' + '{"Species_No":"90070","Category":"Angelfish","Common_Name":"Blue Angelfish","Species_Name":"Pomacanthus nauarchus","Length_Cm":"30","Length_In":"11.8110236220472","Notes":"Habitat is around boulders, caves, coral ledges and crevices in shallow waters.  Swims alone or in groups.\\r\\n\\r\\nIts color changes dramatically from juvenile to adult.  The mature adult fish can startle divers by producing a powerful drumming or thumping sound intended to warn off predators.\\r\\n\\r\\nEdibility is good.\\r\\n\\r\\nRange is the entire Indo-Pacific region."},' + '{"Species_No":"90080","Category":"Cod","Common_Name":"Lunartail Rockcod","Species_Name":"Variola louti","Length_Cm":"80","Length_In":"31.496062992126","Notes":"Also known as the coronation trout.  It is found around coral reefs from shallow to very deep waters.  Feeds primarily on small fishes.\\r\\n\\r\\nAlthough this rockcod is considered a good game and food fish, the large ones may contain a toxin and should not be eaten.  There is no way to discern whether the fish contains toxin.\\r\\n\\r\\nRange is the Indo-Pacific and the Red Sea."},' + '{"Species_No":"90090","Category":"Scorpionfish","Common_Name":"Firefish","Species_Name":"Pterois volitans","Length_Cm":"38","Length_In":"14.9606299212598","Notes":"Also known as the turkeyfish.  Inhabits reef caves and crevices.  The firefish is usually stationary during the day, but feeds actively at night.  Favorite foods are crustaceans.\\r\\n\\r\\nThe dorsal spines of the firefish are needle-like and contain venom.  They can inflict an extremely painful wound.\\r\\n\\r\\nEdibility is poor.\\r\\n\\r\\nRange is from Western Australia to Malaysia."},' + '{"Species_No":"90100","Category":"Butterflyfish","Common_Name":"Ornate Butterflyfish","Species_Name":"Chaetodon Ornatissimus","Length_Cm":"19","Length_In":"7.48031496062992","Notes":"Normally seen in pairs around dense coral areas from very shallow to moderate depths.  The butterflyfish feeds mainly on coral polyps and anemones.\\r\\n\\r\\nEdibility is poor.\\r\\n\\r\\nRange is Indo-Pacific from Sri Lanka to Polynesia."},' + '{"Species_No":"90110","Category":"Shark","Common_Name":"Swell Shark","Species_Name":"Cephaloscyllium ventriosum","Length_Cm":"102","Length_In":"40.15748031496063","Notes":"Inhabits shallow reef caves and crevices and kelp beds along the coast and offshore islands.  This shark feeds at night on fishes and crustaceans and is totally harmless to divers.\\n\\nFor defense, the swell shark inflates its stomach with water to tightly lodge itself in a reef crevice.  \\n\\nEdibility is poor.\\n\\nRange is from Monterey Bay to Acapulco.  Also found in Chile."},' + '{"Species_No":"90120","Category":"Ray","Common_Name":"Bat Ray","Species_Name":"Myliobatis californica","Length_Cm":"56","Length_In":"22.04724409448819","Notes":"Also know as the grinder ray because of its flat grinding teeth used to crush its meal of crustaceans or invertebrates.  Inhabits bays, sloughs, and kelp beds with sandy bottoms.\\n\\nThe bat ray digs up food with its wings and snout, and will even bite off overhanging ledges to get at prey.  It hunts singly or in groups.  When resting, it buries itself in sand with just the eyes protruding.\\n\\nEdibility is poor.\\n\\nRange is from Oregon to the Gulf of California."},' + '{"Species_No":"90130","Category":"Eel","Common_Name":"California Moray","Species_Name":"Gymnothorax mordax","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"This fish hides in a shallow-water lair with just its head protruding during the day.  At night it feeds on octopuses, crustaceans, and small fish close by.\\n\\nIf caught, it will bite anything nearby with its large fang-like teeth.  Divers can be bitten by a moray eel when sticking their hands into crevices or holes in search of lobster or abalone.\\n\\nEdibility is good.\\n\\nRange is from Southern California to Southern Baja."},' + '{"Species_No":"90140","Category":"Cod","Common_Name":"Lingcod","Species_Name":"Ophiodon elongatus","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Widely found from near the shore to very deep waters.  Young fish stay on sand or mud bottoms of bays and inshore areas.  The lingcod is a voracious predator, eating many different fishes and octopuses.\\n\\nThis fish changes color when stressed.  The flesh color also changes, from a greenish hue when caught to white when cooked.\\n\\nEdibility is good; Lingcod is a popular sport and commercial fish.\\n\\nRange is Alaska to Northern Baja California."},' + '{"Species_No":"90150","Category":"Sculpin","Common_Name":"Cabezon","Species_Name":"Scorpaenichthys marmoratus","Length_Cm":"99","Length_In":"38.9763779527559","Notes":"Often called the great marbled sculpin.  Found over rocky or shell-encrusted bottoms from shallow to moderately deep waters.  It feeds primarily on crustaceans and mollusks.\\n\\nThe male cabezon will not budge while guarding its nest and can even be touched by divers.\\n\\nEdibility is good; the flesh is bluish-green but turns white when cooked.  The eggs of the cabezon are poisonous.\\n\\nRange is from Alaska to Central Baja."},' + '{"Species_No":"90160","Category":"Spadefish","Common_Name":"Atlantic Spadefish","Species_Name":"Chaetodiperus faber","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Found in mid-water areas around reefs, wrecks and bridges.  The tiny, all-black juveniles drift motionless in the shallows, looking like leaves and pods of mangrove.\\n\\nEdibility is good.\\n\\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},' + '{"Species_No":"90170","Category":"Shark","Common_Name":"Nurse Shark","Species_Name":"Ginglymostoma cirratum","Length_Cm":"400","Length_In":"157.4803149606299","Notes":"Frequently found under rock or reef ledges.  Carnivorous with well-developed organs for scent and vibration detection.\\n\\nLike all sharks, the nurse shark has a skeleton of cartilage rather than bone.  Instead of scales, its body is covered with tiny razor-sharp denticles.  The teeth are specialized forms of denticles.  Sharks must continually swim or will slowly sink because they have no air bladder.\\n\\nEdibility is poor.\\n\\nRange is from Rhode Island to Brazil, including the Gulf of Mexico."},' + '{"Species_No":"90180","Category":"Ray","Common_Name":"Spotted Eagle Ray","Species_Name":"Aetobatus narinari","Length_Cm":"200","Length_In":"78.74015748031496","Notes":"Found in reef areas and sandy bottoms.  The spotted eagle ray has a poisonous spine on its tail and incredibly powerful jaws to crush oysters, clams, and numerous crustaceans.  Divers report large schools during breeding season.\\n\\nThis ray is an active swimmer and often leaps into the air.  The slapping sound it makes on the water is thought to mark a territory.\\n\\nEdibility is poor.\\n\\nRange is throughout the tropics."},' + '{"Species_No":"90190","Category":"Snapper","Common_Name":"Yellowtail Snapper","Species_Name":"Ocyurus chrysurus","Length_Cm":"75","Length_In":"29.52755905511811","Notes":"Prefers to congregate in loose groups in the open water above reef areas.  Has well-developed teeth and usually feeds at night on small fishes, crustaceans, and plankton.\\n\\nThe yellowtail snapper repeatedly snaps its jaws after it has been caught.  Divers have been injured by these fish.\\n\\nThis is an excellent game fish with tenacious fighting ability and tasty flesh.\\n\\nRange is Bermuda, New England to Brazil, and the Gulf of Mexico."},' + '{"Species_No":"90200","Category":"Parrotfish","Common_Name":"Redband Parrotfish","Species_Name":"Sparisoma Aurofrenatum","Length_Cm":"28","Length_In":"11.02362204724409","Notes":"Inhabits reef areas.  The parrotfish\'s teeth are fused together, enabling them to scrape away hard coral outer skeletons to get at polyps inside.  These fish are thought to be a major factor in reef recycling.\\n\\nOccasionally a female will change sex, increase in size, and take on a distinct appearance  as a terminal-phase male.  This is usually done to replace a missing male.\\n\\nEdibility is poor. \\n\\nRange is Bermuda and Florida to Brazil."},' + '{"Species_No":"90210","Category":"Barracuda","Common_Name":"Great Barracuda","Species_Name":"Sphyraena barracuda","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Young barracuda live in inshore seagrass beds, while adults range from inshore channels to the open ocean.  The barracuda feeds on a wide variety of fishes.\\n\\nIt frequently drifts just below the surface and is known to approach divers at very close range.  The long underslung jaw with its very sharp teeth can be disconcerting.  Attacks on humans have reportedly been in cloudy water when the victim is wearing bright diving gear or attempting to spear the fish.\\n\\nEdibility is good for small specimens, but  large barracuda can carry a fatal toxin.  There is no visible way to tell if the fish is harmful to eat.\\n\\nRange is worldwide."},' + '{"Species_No":"90220","Category":"Grunt","Common_Name":"French Grunt","Species_Name":"Haemulon flavolineatum","Length_Cm":"30","Length_In":"11.81102362204724","Notes":"The French grunt drifts in large groups in sheltered reef areas during the day.  It forages nearby for other fish at night.\\n\\nThe fish produces a grunt-like sound by grinding teeth located in the throat.  The sound is amplified by the adjacent swim bladder.  During territorial skirmishes, male grunts will face and push each other with open mouths.\\n\\nEdibility is excellent.\\n\\nRange is Bermuda, South Carolina to Brazil, and the Gulf of Mexico."},' + '{"Species_No":"90230","Category":"Snapper","Common_Name":"Dog Snapper","Species_Name":"Lutjanus jocu","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"This fish is named for its elongated canine teeth at the front of the upper jaw.  It is solitary and wary and stays in the deep reef or submerged wreck areas.  Not very common anywhere.\\n\\nEdibility is good if the fish is small.  However, a large dog snapper may contain a fatal toxin.  These fish repeatedly snap their jaws shut after removal from a hook or net.\\n\\nRange is New England to Brazil and the Gulf of Mexico."},' + '{"Species_No":"90240","Category":"Grouper","Common_Name":"Nassau Grouper","Species_Name":"Epinephelus striatus","Length_Cm":"91","Length_In":"35.8267716535433","Notes":"Found around shallow coral reefs and seagrass beds, feeding mainly on fishes.\\n\\nThis is the most friendly of all groupers.  If offered food, it will return again and again, looking for more. \\n\\nAs a defense, the Nassau grouper can change colors to blend perfectly into any background, from white to solid black.\\n\\nRange is Bermuda, North Carolina to Brazil, and the Gulf of Mexico."},' + '{"Species_No":"90250","Category":"Wrasse","Common_Name":"Bluehead Wrasse","Species_Name":"Thalassoma bifasciatum","Length_Cm":"15","Length_In":"5.905511811023622","Notes":"Found in coral reefs, rocky flats, reef sand, and seagrass habitats.  This is one of the most successful \\"cleaner fish\\" in the tropical West Atlantic.  It feeds on the parasites of other fish, who come to the wrasse to be cleaned.\\n\\nMost bluehead wrasses are yellow.  The head of the terminal-phase male (about 4% of the population) is blue.\\n\\nEdibility is poor.\\n\\nRange is large, including both sides of the Atlantic, Bermuda, Bahamas, and Florida to Curacao, plus the Gulf of Mexico."},' + '{"Species_No":"90260","Category":"Jack","Common_Name":"Yellow Jack","Species_Name":"Gnathanodon speciousus","Length_Cm":"90","Length_In":"35.43307086614173","Notes":"Inhabits reef and mid-water areas, feeding on invertebrates and small fishes.  The adult is one of the few jacks without teeth.\\n\\nThe young fish seek out larger predators, such as sharks, for protection.  Divers have reported young jacks wanting to join up with them!\\n\\nEdibility is excellent.\\n\\nRange is Indo-Pacific and Southern California to Panama."},' + '{"Species_No":"90270","Category":"Surfperch","Common_Name":"Redtail Surfperch","Species_Name":"Amphistichus rhodoterus","Length_Cm":"40","Length_In":"15.74803149606299","Notes":"Inhabits exposed sandy shorelines to shallow depths.  Feeds on sand-dwelling crustaceans and mollusks.\\n\\nWhile almost all other marine fishes fertilize and scatter large numbers of eggs outside the body, the surfperch nourishes offspring inside the ovary and spawns them live and sexually active into the surf.\\n\\nA favorite sport fish for surf anglers.  Edibility is very good.\\n\\nRange is from Vancouver Island to Central California."},' + '{"Species_No":"90280","Category":"Croaker","Common_Name":"White Sea Bass","Species_Name":"Atractoscion nobilis","Length_Cm":"150","Length_In":"59.05511811023622","Notes":"Schools are found over rocky bottoms and around kelp forest canopies.  Not a true bass, this is the largest of the croakers on the Pacific Coast.  It feeds in mid-water on squid, anchovies, and sardines.  \\n\\nCroakers make a remarkable \\"boop-boop-boop\\" sound, and submarine commanders discovered they could hide the sound of their engines behind the racket.  \\n\\nThe large calcareous \\"earstones\\" in this fish\'s inner ear canals were considered good luck charms by early Europeans and were used by American Indians in jewelry.\\n\\nExcellent edibility if you can find one.  White sea bass were heavily fished in the 1950s but are now rarely caught.\\n\\nRange is from Alaska to Southern Baja."},' + '{"Species_No":"90290","Category":"Greenling","Common_Name":"Rock Greenling","Species_Name":"Hexagrammos lagocephalus","Length_Cm":"60","Length_In":"23.62204724409449","Notes":"Inhabits rocky areas along shallow exposed coast line.\\n\\nGreenlings can change their color to blend with the surrounding sunlit rock and seaweed.  Their scales are very rough and give the body a sandpaper-like texture.\\n\\nAn 1886 description of a greenling comes from naturalist J.K. Lord.  He was overcome by its beauty, and said \\"its sides...rival in beauty many a tropical flower...[and are] adorned with colors not only conspicuous for their brilliancy, but grouped and blended in a manner one sees only represented in the plumage of a bird, the wing of a butterfly, or the petals of an orchid...red, blue, orange, and green are so mingled that the only thing I can think of as a comparison is a floating flower bed, and even the gardener\'s art, in grouping, is but a bungle contrasted with nature\'s painting.\\"\\n\\nEdibility is good.\\n\\nRange is from the Bering Sea to Southern California."},' + '{"Species_No":"90300","Category":"Wrasse","Common_Name":"Senorita","Species_Name":"Oxyjulis californica","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Found almost everywhere by divers, this fish lives either in schools or alone.  It is a voracious eater that feeds constantly.  It is also a very successful \\"cleaner fish\\", and a single Senorita may be surrounded by dozens of fishes waiting to be cleaned of parasites.  Divers report them teaming up to clean a large sea bass or Mola.\\n\\nThis fish does not reverse sex as most wrasses do.  When disturbed, it burrows in the bottom sediment.  It also sleeps there with its head protruding from the sand.\\n\\nEdibility is poor.\\n\\nRange is Northern California to Central Baja."},' + '{"Species_No":"90310","Category":"Smelt","Common_Name":"Surf Smelt","Species_Name":"Hypomesus pretiosus","Length_Cm":"25","Length_In":"9.84251968503937","Notes":"Also called the day smelt because of the timing of its spawning runs.  Inhabits the surf line, feeding on small fishes and invertebrates.  \\n\\nSurf smelt ride a wave onto the beach, lay and fertilize their eggs, and catch a return wave to the sea.  The fry hatch approximately two weeks later during high tide. \\n\\nThis fish is a favorite among surf anglers.  Edibility is good.\\n\\nRange is from Alaska to Southern California."}' + "]";
});
rtl.module("program",["System","SysUtils","JS","Web","uFishFacts"],function () {
  "use strict";
  var $mod = this;
  this.FishFacts = null;
  $mod.$main = function () {
    $mod.FishFacts = pas.uFishFacts.TJFishFacts.$create("Create$1");
    $mod.FishFacts.InitializeObject();
  };
});
//# sourceMappingURL=project1.js.map
