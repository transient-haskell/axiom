// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=unCStr("vertical-align:top"),_8=[0,3],_9=unCStr("id"),_a=function(_b){return E(_b);},_c=function(_d,_e,_f,_){var _g=E(_e),_h=A(_d,[_f,_]),_i=A(_1,[_a,_h,_g[1],_g[2],_]);return _h;},_j=function(_k,_l){while(1){var _m=(function(_n,_o){var _p=E(_o);if(!_p[0]){return E(_n);}else{_k=function(_q,_){return _c(_n,_p[1],_q,_);};_l=_p[2];return null;}})(_k,_l);if(_m!=null){return _m;}}},_r=unCStr("span"),_s=function(_t,_u,_){var _v=jsCreateElem(toJSStr(E(_t))),_w=jsAppendChild(_v,E(_u)[1]);return [0,_v];},_x=function(_q,_){return _s(_r,_q,_);},_y=function(_z,_A,_){return [0,_0,_z];},_B=function(_C,_){return [0,_C,_C];},_D=[0,coercionToken],_E=function(_F,_G,_){var _H=A(_F,[_]);return A(_G,[_]);},_I=function(_J,_K,_){return _E(_J,_K,_);},_L=function(_M,_N,_){var _O=A(_M,[_]);return A(_N,[_O,_]);},_P=unCStr("base"),_Q=unCStr("GHC.IO.Exception"),_R=unCStr("IOException"),_S=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_P,_Q,_R],_T=[0],_U=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_S,_T],_V=function(_W){return E(_U);},_X=function(_Y){return E(E(_Y)[1]);},_Z=unCStr("Maybe.fromJust: Nothing"),_10=new T(function(){return err(_Z);}),_11=function(_12,_13,_14){var _15=new T(function(){var _16=A(_12,[_14]),_17=A(_13,[new T(function(){var _18=E(_15);return _18[0]==0?E(_10):E(_18[1]);})]),_19=hs_eqWord64(_16[1],_17[1]);if(!E(_19)){return [0];}else{var _1a=hs_eqWord64(_16[2],_17[2]);return E(_1a)==0?[0]:[1,_14];}});return E(_15);},_1b=function(_1c){var _1d=E(_1c);return _11(_X(_1d[1]),_V,_1d[2]);},_1e=unCStr(": "),_1f=[0,41],_1g=unCStr(" ("),_1h=function(_1i,_1j){var _1k=E(_1i);return _1k[0]==0?E(_1j):[1,_1k[1],new T(function(){return _1h(_1k[2],_1j);})];},_1l=unCStr("already exists"),_1m=unCStr("does not exist"),_1n=unCStr("protocol error"),_1o=unCStr("failed"),_1p=unCStr("invalid argument"),_1q=unCStr("inappropriate type"),_1r=unCStr("hardware fault"),_1s=unCStr("unsupported operation"),_1t=unCStr("timeout"),_1u=unCStr("resource vanished"),_1v=unCStr("interrupted"),_1w=unCStr("resource busy"),_1x=unCStr("resource exhausted"),_1y=unCStr("end of file"),_1z=unCStr("illegal operation"),_1A=unCStr("permission denied"),_1B=unCStr("user error"),_1C=unCStr("unsatisified constraints"),_1D=unCStr("system error"),_1E=function(_1F,_1G){switch(E(_1F)){case 0:return _1h(_1l,_1G);case 1:return _1h(_1m,_1G);case 2:return _1h(_1w,_1G);case 3:return _1h(_1x,_1G);case 4:return _1h(_1y,_1G);case 5:return _1h(_1z,_1G);case 6:return _1h(_1A,_1G);case 7:return _1h(_1B,_1G);case 8:return _1h(_1C,_1G);case 9:return _1h(_1D,_1G);case 10:return _1h(_1n,_1G);case 11:return _1h(_1o,_1G);case 12:return _1h(_1p,_1G);case 13:return _1h(_1q,_1G);case 14:return _1h(_1r,_1G);case 15:return _1h(_1s,_1G);case 16:return _1h(_1t,_1G);case 17:return _1h(_1u,_1G);default:return _1h(_1v,_1G);}},_1H=[0,125],_1I=unCStr("{handle: "),_1J=function(_1K,_1L,_1M,_1N,_1O,_1P){var _1Q=new T(function(){var _1R=new T(function(){return _1E(_1L,new T(function(){var _1S=E(_1N);return _1S[0]==0?E(_1P):_1h(_1g,new T(function(){return _1h(_1S,[1,_1f,_1P]);}));}));}),_1T=E(_1M);return _1T[0]==0?E(_1R):_1h(_1T,new T(function(){return _1h(_1e,_1R);}));}),_1U=E(_1O);if(!_1U[0]){var _1V=E(_1K);if(!_1V[0]){return E(_1Q);}else{var _1W=E(_1V[1]);return _1W[0]==0?_1h(_1I,new T(function(){return _1h(_1W[1],[1,_1H,new T(function(){return _1h(_1e,_1Q);})]);})):_1h(_1I,new T(function(){return _1h(_1W[1],[1,_1H,new T(function(){return _1h(_1e,_1Q);})]);}));}}else{return _1h(_1U[1],new T(function(){return _1h(_1e,_1Q);}));}},_1X=function(_1Y){var _1Z=E(_1Y);return _1J(_1Z[1],_1Z[2],_1Z[3],_1Z[4],_1Z[6],_T);},_20=function(_21,_22){var _23=E(_21);return _1J(_23[1],_23[2],_23[3],_23[4],_23[6],_22);},_24=[0,44],_25=[0,93],_26=[0,91],_27=function(_28,_29,_2a){var _2b=E(_29);return _2b[0]==0?unAppCStr("[]",_2a):[1,_26,new T(function(){return A(_28,[_2b[1],new T(function(){var _2c=function(_2d){var _2e=E(_2d);return _2e[0]==0?E([1,_25,_2a]):[1,_24,new T(function(){return A(_28,[_2e[1],new T(function(){return _2c(_2e[2]);})]);})];};return _2c(_2b[2]);})]);})];},_2f=function(_2g,_2h){return _27(_20,_2g,_2h);},_2i=function(_2j,_2k,_2l){var _2m=E(_2k);return _1J(_2m[1],_2m[2],_2m[3],_2m[4],_2m[6],_2l);},_2n=[0,_2i,_1X,_2f],_2o=new T(function(){return [0,_V,_2n,_2p,_1b];}),_2p=function(_2q){return [0,_2o,_2q];},_2r=[0],_2s=7,_2t=function(_2u){return [0,_2r,_2s,_T,_2u,_2r,_2r];},_2v=function(_2w,_){return die(new T(function(){return _2p(new T(function(){return _2t(_2w);}));}));},_2x=function(_2y,_){return _2v(_2y,_);},_2z=function(_2A,_){return _2A;},_2B=[0,_L,_I,_2z,_2x],_2C=function(_2D){return E(E(_2D)[1]);},_2E=function(_2F,_2G,_2H,_2I){return A(_2C,[_2F,new T(function(){return A(_2G,[_2I]);}),function(_2J){return A(_2H,[new T(function(){return E(E(_2J)[1]);}),new T(function(){return E(E(_2J)[2]);})]);}]);},_2K=function(_2L,_2M,_2N,_2O){return A(_2C,[_2L,new T(function(){return A(_2M,[_2O]);}),function(_2P){return A(_2N,[new T(function(){return E(E(_2P)[2]);})]);}]);},_2Q=function(_2R,_2S,_2T,_2U){return _2K(_2R,_2S,_2T,_2U);},_2V=function(_2W){return E(E(_2W)[4]);},_2X=function(_2Y,_2Z){var _30=new T(function(){return A(_2V,[_2Y,_2Z]);});return function(_31){return E(_30);};},_32=function(_33){return E(E(_33)[3]);},_34=function(_35){var _36=new T(function(){return _32(_35);});return [0,function(_2S,_2T,_2U){return _2E(_35,_2S,_2T,_2U);},function(_2S,_2T,_2U){return _2Q(_35,_2S,_2T,_2U);},function(_37,_38){return A(_36,[[0,_37,_38]]);},function(_2U){return _2X(_35,_2U);}];},_39=new T(function(){return _34(_2B);}),_3a=[0,112],_3b=function(_3c,_3d){var _3e=jsShowI(_3c);return _1h(fromJSStr(_3e),_3d);},_3f=[0,41],_3g=[0,40],_3h=function(_3i,_3j,_3k){return _3j>=0?_3b(_3j,_3k):_3i<=6?_3b(_3j,_3k):[1,_3g,new T(function(){var _3l=jsShowI(_3j);return _1h(fromJSStr(_3l),[1,_3f,_3k]);})];},_3m=function(_3n,_3o,_3p,_3q){var _3r=E(_3o);return A(_3r[1],[new T(function(){var _3s=E(_3n);return E(_3p);}),function(_3t){var _3u=new T(function(){return E(E(_3t)[2]);});return A(_3r[2],[new T(function(){return A(_3q,[new T(function(){var _3v=E(new T(function(){var _3w=E(_3n);return [0,coercionToken];})),_3x=E(_3t);return [0,_3x[1],new T(function(){return [0,E(_3u)[1]+1|0];}),_3x[3],_3x[4],_3x[5]];})]);}),new T(function(){return A(_3r[3],[[1,_3a,new T(function(){return _1h(_3h(0,E(_3u)[1],_T),new T(function(){return E(E(_3t)[1]);}));})]]);})]);}]);},_3y=new T(function(){return _3m(_D,_39,_B,_y);}),_3z=2,_3A=[1],_3B=function(_3C,_){return _2r;},_3D=function(_){return _2r;},_3E=[0,_3D,_3B],_3F=[0,0],_3G=[0,_T,_3F,_3z,_3E,_3A],_3H=function(_){var _=0,_3I=newMVar(),_=putMVar(_3I,_3G);return [0,_3I];},_3J=function(_3K){var _3L=A(_3K,[_]);return E(_3L);},_3M=new T(function(){return _3J(_3H);}),_3N=unCStr(" could be found!"),_3O=function(_3P){return err(unAppCStr("No element with ID ",new T(function(){return _1h(_3P,_3N);})));},_3Q=function(_3R,_3S,_){var _3T=E(_3M)[1],_3U=takeMVar(_3T),_3V=E(_3S),_3W=jsFind(toJSStr(_3V)),_3X=E(_3W);if(!_3X[0]){return _3O(_3V);}else{var _3Y=E(_3X[1]),_3Z=jsClearChildren(_3Y[1]),_40=A(_3R,[_3U,_]),_41=E(_40),_42=E(_41[1]),_=putMVar(_3T,_41[2]),_43=A(_42[1],[_3Y,_]);return _42[2];}},_44=function(_45,_46,_47,_48,_49,_4a,_4b,_4c,_){var _4d=E(_4b);return [0,_4d,[0,_48,_49,_4a,[0,function(_){return _3Q(function(_4e,_){var _4f=A(_45,[new T(function(){var _4g=E(_4e);return [0,_4g[1],_49,_4g[3],_4g[4],_4g[5]];}),_]);return [0,[0,_2z,E(E(_4f)[1])[2]],_4e];},_47,_);},function(_4h,_){var _4i=_3Q(new T(function(){return A(_46,[_4h]);}),_47,_),_4j=E(_4i);return _4j[0]==0?_2r:A(_4d[2],[_4j[1],_]);}],_4c]];},_4k=function(_4l,_4m,_4n,_){var _4o=A(_3y,[_4n,_]),_4p=E(_4o),_4q=_4p[1],_4r=E(_4p[2]),_4s=_44(_4l,_4m,_4q,_4r[1],_4r[2],_4r[3],_4r[4],_4r[5],_),_4t=A(_4l,[new T(function(){return E(E(_4s)[2]);}),_]),_4u=E(_4t),_4v=_4u[2],_4w=E(_4u[1]),_4x=_4w[1],_4y=new T(function(){return _j(_x,[1,[0,_9,_4q],_T]);}),_4z=E(_4w[2]);if(!_4z[0]){return [0,[0,function(_4A,_){var _4B=A(_4x,[_4A,_]),_4C=A(_4y,[_4A,_]);return _4A;},_2r],new T(function(){var _4D=E(_4v);return [0,_4D[1],_4D[2],_4D[3],new T(function(){return E(E(_4s)[1]);}),_4D[5]];})];}else{var _4E=A(_4m,[_4z[1],new T(function(){var _4F=E(_4v);return [0,_4F[1],_4F[2],_4F[3],new T(function(){return E(E(_4s)[1]);}),_4F[5]];}),_]),_4G=E(_4E),_4H=E(_4G[1]);return [0,[0,function(_4I,_){var _4J=A(_4x,[_4I,_]),_4K=A(_4y,[_4I,_]),_4L=A(_4H[1],[_4K,_]);return _4I;},_4H[2]],_4G[2]];}},_4M=function(_4N,_4O,_){var _4P=jsCreateTextNode(toJSStr(E(_4N))),_4Q=jsAppendChild(_4P,E(_4O)[1]);return [0,_4P];},_4R=[0,112],_4S=[1,_4R,_T],_4T=function(_4U,_4V){var _4W=new T(function(){return A(_4U,[_4V]);});return function(_4X,_){var _4Y=jsCreateElem(toJSStr(_4S)),_4Z=jsAppendChild(_4Y,E(_4X)[1]),_50=[0,_4Y],_51=A(_4W,[_50,_]);return _50;};},_52=function(_53){return _3h(0,E(_53)[1],_T);},_54=[0,98],_55=[1,_54,_T],_56=function(_57,_58){var _59=new T(function(){return A(_57,[_58]);});return function(_5a,_){var _5b=jsCreateElem(toJSStr(_55)),_5c=jsAppendChild(_5b,E(_5a)[1]),_5d=[0,_5b],_5e=A(_59,[_5d,_]);return _5d;};},_5f=unCStr("br"),_5g=function(_5h,_){var _5i=jsCreateElem(toJSStr(E(_5f))),_5j=jsAppendChild(_5i,E(_5h)[1]);return [0,_5i];},_5k=[1,_0],_5l=unCStr("result: "),_5m=function(_5n){var _5o=new T(function(){return _56(_4M,new T(function(){return _52(_5n);}));});return function(_5p,_){return [0,[0,function(_5q,_){var _5r=_5g(_5q,_),_5s=_4M(_5l,_5q,_),_5t=A(_5o,[_5q,_]);return _5q;},_5k],_5p];};},_5u=unCStr(" numbers and append the result using a fold"),_5v=[0,0],_5w=[1,_5v],_5x=[0,_2z,_5w],_5y=function(_5z,_){return [0,_5x,_5z];},_5A=function(_5B,_5C,_5D,_){var _5E=_s(_5B,_5D,_),_5F=A(_5C,[_5E,_]);return _5E;},_5G=unCStr("()"),_5H=unCStr("GHC.Tuple"),_5I=unCStr("ghc-prim"),_5J=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5I,_5H,_5G],_5K=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5J,_T],_5L=function(_5M){return E(_5K);},_5N=unCStr("main"),_5O=unCStr("Haste.Perch"),_5P=unCStr("PerchM"),_5Q=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5N,_5O,_5P],_5R=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5Q,_T],_5S=function(_5T){return E(_5R);},_5U=function(_5V){var _5W=E(_5V);return _5W[0]==0?[0]:_1h(_5W[1],new T(function(){return _5U(_5W[2]);}));},_5X=function(_5Y,_5Z){var _60=E(_5Y);if(!_60){return [0,_T,_5Z];}else{var _61=E(_5Z);if(!_61[0]){return [0,_T,_T];}else{var _62=new T(function(){var _63=_5X(_60-1|0,_61[2]);return [0,_63[1],_63[2]];});return [0,[1,_61[1],new T(function(){return E(E(_62)[1]);})],new T(function(){return E(E(_62)[2]);})];}}},_64=[0,120],_65=[0,48],_66=function(_67){var _68=new T(function(){var _69=_5X(8,new T(function(){var _6a=md5(toJSStr(E(_67)));return fromJSStr(_6a);}));return [0,_69[1],_69[2]];}),_6b=parseInt([0,toJSStr([1,_65,[1,_64,new T(function(){return E(E(_68)[1]);})]])]),_6c=new T(function(){var _6d=_5X(8,new T(function(){return E(E(_68)[2]);}));return [0,_6d[1],_6d[2]];}),_6e=parseInt([0,toJSStr([1,_65,[1,_64,new T(function(){return E(E(_6c)[1]);})]])]),_6f=hs_mkWord64(_6b,_6e),_6g=parseInt([0,toJSStr([1,_65,[1,_64,new T(function(){return E(_5X(8,new T(function(){return E(E(_6c)[2]);}))[1]);})]])]),_6h=hs_mkWord64(_6g,_6g);return [0,_6f,_6h];},_6i=function(_6j,_6k){var _6l=E(_6k);return _6l[0]==0?[0]:[1,new T(function(){return A(_6j,[_6l[1]]);}),new T(function(){return _6i(_6j,_6l[2]);})];},_6m=function(_6n,_6o){var _6p=jsShowI(_6n),_6q=md5(_6p);return _1h(fromJSStr(_6q),new T(function(){var _6r=jsShowI(_6o),_6s=md5(_6r);return fromJSStr(_6s);}));},_6t=function(_6u){var _6v=E(_6u);return _6m(_6v[1],_6v[2]);},_6w=function(_6x){var _6y=E(_6x);if(!_6y[0]){return [0];}else{var _6z=E(_6y[1]);return [1,[0,_6z[1],_6z[2]],new T(function(){return _6w(_6y[2]);})];}},_6A=unCStr("Prelude.undefined"),_6B=new T(function(){return err(_6A);}),_6C=function(_6D,_6E){return function(_6F){return E(new T(function(){var _6G=A(_6D,[_6B]),_6H=E(_6G[3]),_6I=_6H[1],_6J=_6H[2],_6K=_1h(_6G[4],[1,new T(function(){return A(_6E,[_6B]);}),_T]);if(!_6K[0]){return [0,_6I,_6J,_6H,_T];}else{var _6L=_66(new T(function(){return _5U(_6i(_6t,[1,[0,_6I,_6J],new T(function(){return _6w(_6K);})]));}));return [0,_6L[1],_6L[2],_6H,_6K];}}));};},_6M=new T(function(){return _6C(_5S,_5L);}),_6N=unCStr("value"),_6O=unCStr("onclick"),_6P=unCStr("checked"),_6Q=[0,_6P,_T],_6R=[1,_6Q,_T],_6S=unCStr("type"),_6T=unCStr("input"),_6U=function(_6V,_){return _s(_6T,_6V,_);},_6W=function(_6X,_6Y,_6Z,_70,_71){var _72=new T(function(){var _73=new T(function(){return _j(_6U,[1,[0,_6S,_6Y],[1,[0,_9,_6X],[1,[0,_6N,_6Z],_T]]]);});return !E(_70)?E(_73):_j(_73,_6R);}),_74=E(_71);return _74[0]==0?E(_72):_j(_72,[1,[0,_6O,_74[1]],_T]);},_75=unCStr("href"),_76=[0,97],_77=[1,_76,_T],_78=function(_79,_){return _s(_77,_79,_);},_7a=function(_7b,_7c){var _7d=new T(function(){return _j(_78,[1,[0,_75,_7b],_T]);});return function(_7e,_){var _7f=A(_7d,[_7e,_]),_7g=A(_7c,[_7f,_]);return _7f;};},_7h=function(_7i){return _7a(_7i,function(_q,_){return _4M(_7i,_q,_);});},_7j=unCStr("option"),_7k=function(_7l,_){return _s(_7j,_7l,_);},_7m=unCStr("selected"),_7n=[0,_7m,_T],_7o=[1,_7n,_T],_7p=function(_7q,_7r,_7s){var _7t=new T(function(){return _j(_7k,[1,[0,_6N,_7q],_T]);}),_7u=function(_7v,_){var _7w=A(_7t,[_7v,_]),_7x=A(_7r,[_7w,_]);return _7w;};return !E(_7s)?E(_7u):_j(_7u,_7o);},_7y=function(_7z,_7A){return _7p(_7z,function(_q,_){return _4M(_7z,_q,_);},_7A);},_7B=unCStr("method"),_7C=unCStr("action"),_7D=unCStr("UTF-8"),_7E=unCStr("acceptCharset"),_7F=[0,_7E,_7D],_7G=unCStr("form"),_7H=function(_7I,_){return _s(_7G,_7I,_);},_7J=function(_7K,_7L,_7M){var _7N=new T(function(){return _j(_7H,[1,_7F,[1,[0,_7C,_7K],[1,[0,_7B,_7L],_T]]]);});return function(_7O,_){var _7P=A(_7N,[_7O,_]),_7Q=A(_7M,[_7P,_]);return _7P;};},_7R=unCStr("select"),_7S=function(_7T,_){return _s(_7R,_7T,_);},_7U=function(_7V,_7W){var _7X=new T(function(){return _j(_7S,[1,[0,_9,_7V],_T]);});return function(_7Y,_){var _7Z=A(_7X,[_7Y,_]),_80=A(_7W,[_7Z,_]);return _7Z;};},_81=unCStr("textarea"),_82=function(_83,_){return _s(_81,_83,_);},_84=function(_85,_86){var _87=new T(function(){return _j(_82,[1,[0,_9,_85],_T]);});return function(_88,_){var _89=A(_87,[_88,_]),_8a=_4M(_86,_89,_);return _89;};},_8b=unCStr("color:red"),_8c=unCStr("style"),_8d=[0,_8c,_8b],_8e=[1,_8d,_T],_8f=[0,98],_8g=[1,_8f,_T],_8h=function(_8i){return _j(function(_8j,_){var _8k=_s(_8g,_8j,_),_8l=A(_8i,[_8k,_]);return _8k;},_8e);},_8m=unCStr("toByteString not defined"),_8n=new T(function(){return err(_8m);}),_8o=function(_8p,_8q,_){var _8r=E(_8p);if(!_8r[0]){return _8q;}else{var _8s=A(_8r[1],[_8q,_]),_8t=_8o(_8r[2],_8q,_);return _8q;}},_8u=function(_8v,_8w,_8x,_){var _8y=A(_8v,[_8x,_]),_8z=A(_8w,[_8x,_]);return _8x;},_8A=[0,_2z,_8u,_8o],_8B=[0,_8A,_6M,_8n,_4M,_4M,_5A,_8h,_7a,_7h,_6W,_84,_7U,_7p,_7y,_7J,_j],_8C=function(_8D,_8E,_){var _8F=A(_8E,[_]);return _8D;},_8G=function(_8H,_8I,_){var _8J=A(_8I,[_]);return new T(function(){return A(_8H,[_8J]);});},_8K=[0,_8G,_8C],_8L=function(_8M){var _8N=E(_8M);return _8N[0]==0?0:E(_8N[1])[1]+_8L(_8N[2])|0;},_8O=function(_8P){return [0,_8L(_8P)];},_8Q=function(_8R,_8S){return [0,E(_8R)[1]+E(_8S)[1]|0];},_8T=[0,_5v,_8Q,_8O],_8U=function(_8V,_8W){var _8X=E(_8W);return _8X[0]==0?[0]:[1,new T(function(){return A(_8V,[_8X[1]]);})];},_8Y=function(_8Z){return E(E(_8Z)[1]);},_90=function(_91){return E(E(_91)[2]);},_92=function(_93,_94,_95,_96,_97,_98){var _99=new T(function(){return _90(_93);});return A(_94,[new T(function(){return A(_96,[_98]);}),function(_9a){var _9b=E(_9a),_9c=E(_9b[1]);return A(_94,[new T(function(){return A(_97,[_9b[2]]);}),function(_9d){var _9e=E(_9d),_9f=E(_9e[1]);return A(_95,[[0,[0,new T(function(){return A(_99,[_9c[1],_9f[1]]);}),new T(function(){var _9g=E(_9c[2]);if(!_9g[0]){return [0];}else{var _9h=E(_9f[2]);return _9h[0]==0?[0]:[1,new T(function(){return A(_9g[1],[_9h[1]]);})];}})],_9e[2]]]);}]);}]);},_9i=function(_9j){return E(E(_9j)[1]);},_9k=function(_9l,_9m,_9n,_9o,_9p,_9q){var _9r=new T(function(){return _8Y(_9l);});return function(_9s){var _9t=E(_9m);return _92(_9r,_9t[1],_9t[3],function(_9u){return A(new T(function(){var _9v=new T(function(){return _90(_9o);});return A(_9i,[_9n,function(_9w){return [0,new T(function(){var _9x=E(E(_9w)[1]);return [0,_9x[1],new T(function(){return _8U(_9v,_9x[2]);})];}),new T(function(){return E(E(_9w)[2]);})];}]);}),[new T(function(){return A(_9p,[_9u]);})]);},_9q,_9s);};},_9y=function(_9z,_9A){while(1){var _9B=(function(_9C,_9D){var _9E=E(_9D);if(!_9E[0]){return E(_9C);}else{_9z=new T(function(){return _9k(_8B,_2B,_8K,_8T,_9C,_9E[1]);});_9A=_9E[2];return null;}})(_9z,_9A);if(_9B!=null){return _9B;}}},_9F=new T(function(){return _9y(_5y,_T);}),_9G=[13,coercionToken],_9H=unCStr("true"),_9I=unCStr("hasevent"),_9J=function(_9K,_9L){while(1){var _9M=E(_9K);if(!_9M[0]){return E(_9L)[0]==0?true:false;}else{var _9N=E(_9L);if(!_9N[0]){return false;}else{if(E(_9M[1])[1]!=E(_9N[1])[1]){return false;}else{_9K=_9M[2];_9L=_9N[2];continue;}}}}},_9O=new T(function(){return [0,"keydown"];}),_9P=new T(function(){return [0,"mousemove"];}),_9Q=new T(function(){return [0,"blur"];}),_9R=new T(function(){return [0,"focus"];}),_9S=new T(function(){return [0,"change"];}),_9T=new T(function(){return [0,"unload"];}),_9U=new T(function(){return [0,"load"];}),_9V=new T(function(){return [0,"keyup"];}),_9W=new T(function(){return [0,"keypress"];}),_9X=new T(function(){return [0,"mouseup"];}),_9Y=new T(function(){return [0,"mousedown"];}),_9Z=new T(function(){return [0,"dblclick"];}),_a0=new T(function(){return [0,"click"];}),_a1=new T(function(){return [0,"mouseout"];}),_a2=new T(function(){return [0,"mouseover"];}),_a3=function(_a4){switch(E(_a4)[0]){case 0:return E(_9U);case 1:return E(_9T);case 2:return E(_9S);case 3:return E(_9R);case 4:return E(_9Q);case 5:return E(_9P);case 6:return E(_a2);case 7:return E(_a1);case 8:return E(_a0);case 9:return E(_9Z);case 10:return E(_9Y);case 11:return E(_9X);case 12:return E(_9W);case 13:return E(_9V);default:return E(_9O);}},_a5=function(_a6,_a7,_a8,_a9,_){var _aa=A(_a6,[_a9,_]),_ab=E(_aa),_ac=_ab[1],_ad=E(_9I),_ae=jsGetAttr(_ac,toJSStr(_ad));if(!_9J(fromJSStr(_ae),_9H)){var _af=E(_a8),_ag=jsSetCB(_ac,_a3(_a7)[1],_a8),_ah=A(_1,[_a,_ab,_ad,_9H,_]);return _ab;}else{return _ab;}},_ai=unCStr("text"),_aj=[0,_2B,_a],_ak=unCStr("base"),_al=unCStr("Control.Exception.Base"),_am=unCStr("PatternMatchFail"),_an=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_ak,_al,_am],_ao=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_an,_T],_ap=function(_aq){return E(_ao);},_ar=function(_as){var _at=E(_as);return _11(_X(_at[1]),_ap,_at[2]);},_au=function(_av){return E(E(_av)[1]);},_aw=function(_ax,_ay){return _1h(E(_ax)[1],_ay);},_az=function(_aA,_aB){return _27(_aw,_aA,_aB);},_aC=function(_aD,_aE,_aF){return _1h(E(_aE)[1],_aF);},_aG=[0,_aC,_au,_az],_aH=new T(function(){return [0,_ap,_aG,_aI,_ar];}),_aI=function(_aJ){return [0,_aH,_aJ];},_aK=unCStr("Non-exhaustive patterns in"),_aL=function(_aM,_aN){return die(new T(function(){return A(_aN,[_aM]);}));},_aO=function(_aP,_aQ){var _aR=E(_aQ);if(!_aR[0]){return [0,_T,_T];}else{var _aS=_aR[1];if(!A(_aP,[_aS])){return [0,_T,_aR];}else{var _aT=new T(function(){var _aU=_aO(_aP,_aR[2]);return [0,_aU[1],_aU[2]];});return [0,[1,_aS,new T(function(){return E(E(_aT)[1]);})],new T(function(){return E(E(_aT)[2]);})];}}},_aV=[0,32],_aW=[0,10],_aX=[1,_aW,_T],_aY=function(_aZ){return E(E(_aZ)[1])==124?false:true;},_b0=function(_b1,_b2){var _b3=_aO(_aY,unCStr(_b1)),_b4=_b3[1],_b5=function(_b6,_b7){return _1h(_b6,new T(function(){return unAppCStr(": ",new T(function(){return _1h(_b2,new T(function(){return _1h(_b7,_aX);}));}));}));},_b8=E(_b3[2]);return _b8[0]==0?_b5(_b4,_T):E(E(_b8[1])[1])==124?_b5(_b4,[1,_aV,_b8[2]]):_b5(_b4,_T);},_b9=function(_ba){return _aL([0,new T(function(){return _b0(_ba,_aK);})],_aI);},_bb=new T(function(){return _b9("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_bc=function(_bd,_be){while(1){var _bf=(function(_bg,_bh){var _bi=E(_bg);switch(_bi[0]){case 0:var _bj=E(_bh);if(!_bj[0]){return [0];}else{_bd=A(_bi[1],[_bj[1]]);_be=_bj[2];return null;}break;case 1:var _bk=A(_bi[1],[_bh]),_bl=_bh;_bd=_bk;_be=_bl;return null;case 2:return [0];case 3:return [1,[0,_bi[1],_bh],new T(function(){return _bc(_bi[2],_bh);})];default:return E(_bi[1]);}})(_bd,_be);if(_bf!=null){return _bf;}}},_bm=function(_bn,_bo){var _bp=new T(function(){var _bq=E(_bo);if(_bq[0]==3){return [3,_bq[1],new T(function(){return _bm(_bn,_bq[2]);})];}else{var _br=E(_bn);if(_br[0]==2){return E(_bq);}else{var _bs=E(_bq);if(_bs[0]==2){return E(_br);}else{var _bt=new T(function(){var _bu=E(_bs);if(_bu[0]==4){return [1,function(_bv){return [4,new T(function(){return _1h(_bc(_br,_bv),_bu[1]);})];}];}else{var _bw=E(_br);if(_bw[0]==1){var _bx=_bw[1],_by=E(_bu);return _by[0]==0?[1,function(_bz){return _bm(A(_bx,[_bz]),_by);}]:[1,function(_bA){return _bm(A(_bx,[_bA]),new T(function(){return A(_by[1],[_bA]);}));}];}else{var _bB=E(_bu);return _bB[0]==0?E(_bb):[1,function(_bC){return _bm(_bw,new T(function(){return A(_bB[1],[_bC]);}));}];}}}),_bD=E(_br);switch(_bD[0]){case 1:var _bE=E(_bs);return _bE[0]==4?[1,function(_bF){return [4,new T(function(){return _1h(_bc(A(_bD[1],[_bF]),_bF),_bE[1]);})];}]:E(_bt);case 4:var _bG=_bD[1],_bH=E(_bs);switch(_bH[0]){case 0:return [1,function(_bI){return [4,new T(function(){return _1h(_bG,new T(function(){return _bc(_bH,_bI);}));})];}];case 1:return [1,function(_bJ){return [4,new T(function(){return _1h(_bG,new T(function(){return _bc(A(_bH[1],[_bJ]),_bJ);}));})];}];default:return [4,new T(function(){return _1h(_bG,_bH[1]);})];}break;default:return E(_bt);}}}}}),_bK=E(_bn);switch(_bK[0]){case 0:var _bL=E(_bo);return _bL[0]==0?[0,function(_bM){return _bm(A(_bK[1],[_bM]),new T(function(){return A(_bL[1],[_bM]);}));}]:E(_bp);case 3:return [3,_bK[1],new T(function(){return _bm(_bK[2],_bo);})];default:return E(_bp);}},_bN=function(_bO,_bP){return E(_bO)[1]!=E(_bP)[1];},_bQ=function(_bR,_bS){return E(_bR)[1]==E(_bS)[1];},_bT=[0,_bQ,_bN],_bU=function(_bV){return E(E(_bV)[1]);},_bW=function(_bX,_bY,_bZ){while(1){var _c0=E(_bY);if(!_c0[0]){return E(_bZ)[0]==0?true:false;}else{var _c1=E(_bZ);if(!_c1[0]){return false;}else{if(!A(_bU,[_bX,_c0[1],_c1[1]])){return false;}else{_bY=_c0[2];_bZ=_c1[2];continue;}}}}},_c2=function(_c3,_c4,_c5){return !_bW(_c3,_c4,_c5)?true:false;},_c6=function(_c7){return [0,function(_c8,_c9){return _bW(_c7,_c8,_c9);},function(_c8,_c9){return _c2(_c7,_c8,_c9);}];},_ca=new T(function(){return _c6(_bT);}),_cb=function(_cc,_cd){var _ce=E(_cc);switch(_ce[0]){case 0:return [0,function(_cf){return _cb(A(_ce[1],[_cf]),_cd);}];case 1:return [1,function(_cg){return _cb(A(_ce[1],[_cg]),_cd);}];case 2:return [2];case 3:return _bm(A(_cd,[_ce[1]]),new T(function(){return _cb(_ce[2],_cd);}));default:var _ch=function(_ci){var _cj=E(_ci);if(!_cj[0]){return [0];}else{var _ck=E(_cj[1]);return _1h(_bc(A(_cd,[_ck[1]]),_ck[2]),new T(function(){return _ch(_cj[2]);}));}},_cl=_ch(_ce[1]);return _cl[0]==0?[2]:[4,_cl];}},_cm=[2],_cn=function(_co){return [3,_co,_cm];},_cp=function(_cq,_cr){var _cs=E(_cq);if(!_cs){return A(_cr,[_0]);}else{var _ct=new T(function(){return _cp(_cs-1|0,_cr);});return [0,function(_cu){return E(_ct);}];}},_cv=function(_cw,_cx,_cy){var _cz=new T(function(){return A(_cw,[_cn]);});return [1,function(_cA){return A(function(_cB,_cC,_cD){while(1){var _cE=(function(_cF,_cG,_cH){var _cI=E(_cF);switch(_cI[0]){case 0:var _cJ=E(_cG);if(!_cJ[0]){return E(_cx);}else{_cB=A(_cI[1],[_cJ[1]]);_cC=_cJ[2];var _cK=_cH+1|0;_cD=_cK;return null;}break;case 1:var _cL=A(_cI[1],[_cG]),_cM=_cG,_cK=_cH;_cB=_cL;_cC=_cM;_cD=_cK;return null;case 2:return E(_cx);case 3:return function(_cN){var _cO=new T(function(){return _cb(_cI,_cN);});return _cp(_cH,function(_cP){return E(_cO);});};default:return function(_cQ){return _cb(_cI,_cQ);};}})(_cB,_cC,_cD);if(_cE!=null){return _cE;}}},[_cz,_cA,0,_cy]);}];},_cR=[6],_cS=unCStr("valDig: Bad base"),_cT=new T(function(){return err(_cS);}),_cU=function(_cV,_cW){var _cX=function(_cY,_cZ){var _d0=E(_cY);if(!_d0[0]){var _d1=new T(function(){return A(_cZ,[_T]);});return function(_d2){return A(_d2,[_d1]);};}else{var _d3=E(_d0[1])[1],_d4=function(_d5){var _d6=new T(function(){return _cX(_d0[2],function(_d7){return A(_cZ,[[1,_d5,_d7]]);});});return function(_d8){var _d9=new T(function(){return A(_d6,[_d8]);});return [0,function(_da){return E(_d9);}];};};switch(E(E(_cV)[1])){case 8:if(48>_d3){var _db=new T(function(){return A(_cZ,[_T]);});return function(_dc){return A(_dc,[_db]);};}else{if(_d3>55){var _dd=new T(function(){return A(_cZ,[_T]);});return function(_de){return A(_de,[_dd]);};}else{return _d4([0,_d3-48|0]);}}break;case 10:if(48>_d3){var _df=new T(function(){return A(_cZ,[_T]);});return function(_dg){return A(_dg,[_df]);};}else{if(_d3>57){var _dh=new T(function(){return A(_cZ,[_T]);});return function(_di){return A(_di,[_dh]);};}else{return _d4([0,_d3-48|0]);}}break;case 16:var _dj=new T(function(){return 97>_d3?65>_d3?[0]:_d3>70?[0]:[1,[0,(_d3-65|0)+10|0]]:_d3>102?65>_d3?[0]:_d3>70?[0]:[1,[0,(_d3-65|0)+10|0]]:[1,[0,(_d3-97|0)+10|0]];});if(48>_d3){var _dk=E(_dj);if(!_dk[0]){var _dl=new T(function(){return A(_cZ,[_T]);});return function(_dm){return A(_dm,[_dl]);};}else{return _d4(_dk[1]);}}else{if(_d3>57){var _dn=E(_dj);if(!_dn[0]){var _do=new T(function(){return A(_cZ,[_T]);});return function(_dp){return A(_dp,[_do]);};}else{return _d4(_dn[1]);}}else{return _d4([0,_d3-48|0]);}}break;default:return E(_cT);}}};return [1,function(_dq){return A(_cX,[_dq,_a,function(_dr){var _ds=E(_dr);return _ds[0]==0?[2]:A(_cW,[_ds]);}]);}];},_dt=[0,10],_du=[0,1],_dv=[0,2147483647],_dw=function(_dx,_dy){while(1){var _dz=E(_dx);if(!_dz[0]){var _dA=_dz[1],_dB=E(_dy);if(!_dB[0]){var _dC=_dB[1],_dD=addC(_dA,_dC);if(!E(_dD[2])){return [0,_dD[1]];}else{_dx=[1,I_fromInt(_dA)];_dy=[1,I_fromInt(_dC)];continue;}}else{_dx=[1,I_fromInt(_dA)];_dy=_dB;continue;}}else{var _dE=E(_dy);if(!_dE[0]){_dx=_dz;_dy=[1,I_fromInt(_dE[1])];continue;}else{return [1,I_add(_dz[1],_dE[1])];}}}},_dF=new T(function(){return _dw(_dv,_du);}),_dG=function(_dH){var _dI=E(_dH);if(!_dI[0]){var _dJ=E(_dI[1]);return _dJ==(-2147483648)?E(_dF):[0, -_dJ];}else{return [1,I_negate(_dI[1])];}},_dK=[0,10],_dL=[0,0],_dM=function(_dN,_dO){while(1){var _dP=E(_dN);if(!_dP[0]){var _dQ=_dP[1],_dR=E(_dO);if(!_dR[0]){var _dS=_dR[1];if(!(imul(_dQ,_dS)|0)){return [0,imul(_dQ,_dS)|0];}else{_dN=[1,I_fromInt(_dQ)];_dO=[1,I_fromInt(_dS)];continue;}}else{_dN=[1,I_fromInt(_dQ)];_dO=_dR;continue;}}else{var _dT=E(_dO);if(!_dT[0]){_dN=_dP;_dO=[1,I_fromInt(_dT[1])];continue;}else{return [1,I_mul(_dP[1],_dT[1])];}}}},_dU=function(_dV,_dW,_dX){while(1){var _dY=E(_dX);if(!_dY[0]){return E(_dW);}else{var _dZ=_dw(_dM(_dW,_dV),_dY[1]);_dX=_dY[2];_dW=_dZ;continue;}}},_e0=function(_e1){var _e2=new T(function(){return _bm(_bm([0,function(_e3){return E(E(_e3)[1])==45?_cU(_dt,function(_e4){return A(_e1,[[1,new T(function(){return _dG(_dU(_dK,_dL,_e4));})]]);}):[2];}],[0,function(_e5){return E(E(_e5)[1])==43?_cU(_dt,function(_e6){return A(_e1,[[1,new T(function(){return _dU(_dK,_dL,_e6);})]]);}):[2];}]),new T(function(){return _cU(_dt,function(_e7){return A(_e1,[[1,new T(function(){return _dU(_dK,_dL,_e7);})]]);});}));});return _bm([0,function(_e8){return E(E(_e8)[1])==101?E(_e2):[2];}],[0,function(_e9){return E(E(_e9)[1])==69?E(_e2):[2];}]);},_ea=function(_eb){return A(_eb,[_2r]);},_ec=function(_ed){return A(_ed,[_2r]);},_ee=function(_ef){var _eg=new T(function(){return _cU(_dt,function(_eh){return A(_ef,[[1,_eh]]);});});return [0,function(_ei){return E(E(_ei)[1])==46?E(_eg):[2];}];},_ej=function(_ek){return _cU(_dt,function(_el){return _cv(_ee,_ea,function(_em){return _cv(_e0,_ec,function(_en){return A(_ek,[[5,[1,_el,_em,_en]]]);});});});},_eo=function(_ep,_eq,_er){while(1){var _es=E(_er);if(!_es[0]){return false;}else{if(!A(_bU,[_ep,_eq,_es[1]])){_er=_es[2];continue;}else{return true;}}}},_et=unCStr("!@#$%&*+./<=>?\\^|:-~"),_eu=function(_ev){return _eo(_bT,_ev,_et);},_ew=[0,8],_ex=[0,16],_ey=function(_ez){var _eA=new T(function(){return _cU(_ex,function(_eB){return A(_ez,[[5,[0,_ex,_eB]]]);});}),_eC=new T(function(){return _cU(_ew,function(_eD){return A(_ez,[[5,[0,_ew,_eD]]]);});}),_eE=new T(function(){return _cU(_ex,function(_eF){return A(_ez,[[5,[0,_ex,_eF]]]);});}),_eG=new T(function(){return _cU(_ew,function(_eH){return A(_ez,[[5,[0,_ew,_eH]]]);});});return [0,function(_eI){return E(E(_eI)[1])==48?E([0,function(_eJ){switch(E(E(_eJ)[1])){case 79:return E(_eG);case 88:return E(_eE);case 111:return E(_eC);case 120:return E(_eA);default:return [2];}}]):[2];}];},_eK=false,_eL=true,_eM=function(_eN){var _eO=new T(function(){return A(_eN,[_ex]);}),_eP=new T(function(){return A(_eN,[_ew]);}),_eQ=new T(function(){return A(_eN,[_ex]);}),_eR=new T(function(){return A(_eN,[_ew]);});return [0,function(_eS){switch(E(E(_eS)[1])){case 79:return E(_eR);case 88:return E(_eQ);case 111:return E(_eP);case 120:return E(_eO);default:return [2];}}];},_eT=function(_eU){return A(_eU,[_dt]);},_eV=function(_eW){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3h(9,_eW,_T);})));},_eX=function(_eY){var _eZ=E(_eY);return _eZ[0]==0?E(_eZ[1]):I_toInt(_eZ[1]);},_f0=function(_f1,_f2){var _f3=E(_f1);if(!_f3[0]){var _f4=_f3[1],_f5=E(_f2);return _f5[0]==0?_f4<=_f5[1]:I_compareInt(_f5[1],_f4)>=0;}else{var _f6=_f3[1],_f7=E(_f2);return _f7[0]==0?I_compareInt(_f6,_f7[1])<=0:I_compare(_f6,_f7[1])<=0;}},_f8=function(_f9){return [2];},_fa=function(_fb){var _fc=E(_fb);if(!_fc[0]){return E(_f8);}else{var _fd=_fc[1],_fe=E(_fc[2]);if(!_fe[0]){return E(_fd);}else{var _ff=new T(function(){return _fa(_fe);});return function(_fg){return _bm(A(_fd,[_fg]),new T(function(){return A(_ff,[_fg]);}));};}}},_fh=unCStr("NUL"),_fi=function(_fj){return [2];},_fk=function(_fl){return _fi(_fl);},_fm=function(_fn,_fo){var _fp=function(_fq,_fr){var _fs=E(_fq);if(!_fs[0]){return function(_ft){return A(_ft,[_fn]);};}else{var _fu=E(_fr);if(!_fu[0]){return E(_fi);}else{if(E(_fs[1])[1]!=E(_fu[1])[1]){return E(_fk);}else{var _fv=new T(function(){return _fp(_fs[2],_fu[2]);});return function(_fw){var _fx=new T(function(){return A(_fv,[_fw]);});return [0,function(_fy){return E(_fx);}];};}}}};return [1,function(_fz){return A(_fp,[_fn,_fz,_fo]);}];},_fA=[0,0],_fB=function(_fC){var _fD=new T(function(){return A(_fC,[_fA]);});return _fm(_fh,function(_fE){return E(_fD);});},_fF=unCStr("STX"),_fG=[0,2],_fH=function(_fI){var _fJ=new T(function(){return A(_fI,[_fG]);});return _fm(_fF,function(_fK){return E(_fJ);});},_fL=unCStr("ETX"),_fM=[0,3],_fN=function(_fO){var _fP=new T(function(){return A(_fO,[_fM]);});return _fm(_fL,function(_fQ){return E(_fP);});},_fR=unCStr("EOT"),_fS=[0,4],_fT=function(_fU){var _fV=new T(function(){return A(_fU,[_fS]);});return _fm(_fR,function(_fW){return E(_fV);});},_fX=unCStr("ENQ"),_fY=[0,5],_fZ=function(_g0){var _g1=new T(function(){return A(_g0,[_fY]);});return _fm(_fX,function(_g2){return E(_g1);});},_g3=unCStr("ACK"),_g4=[0,6],_g5=function(_g6){var _g7=new T(function(){return A(_g6,[_g4]);});return _fm(_g3,function(_g8){return E(_g7);});},_g9=unCStr("BEL"),_ga=[0,7],_gb=function(_gc){var _gd=new T(function(){return A(_gc,[_ga]);});return _fm(_g9,function(_ge){return E(_gd);});},_gf=unCStr("BS"),_gg=[0,8],_gh=function(_gi){var _gj=new T(function(){return A(_gi,[_gg]);});return _fm(_gf,function(_gk){return E(_gj);});},_gl=unCStr("HT"),_gm=[0,9],_gn=function(_go){var _gp=new T(function(){return A(_go,[_gm]);});return _fm(_gl,function(_gq){return E(_gp);});},_gr=unCStr("LF"),_gs=[0,10],_gt=function(_gu){var _gv=new T(function(){return A(_gu,[_gs]);});return _fm(_gr,function(_gw){return E(_gv);});},_gx=unCStr("VT"),_gy=[0,11],_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gy]);});return _fm(_gx,function(_gC){return E(_gB);});},_gD=unCStr("FF"),_gE=[0,12],_gF=function(_gG){var _gH=new T(function(){return A(_gG,[_gE]);});return _fm(_gD,function(_gI){return E(_gH);});},_gJ=unCStr("CR"),_gK=[0,13],_gL=function(_gM){var _gN=new T(function(){return A(_gM,[_gK]);});return _fm(_gJ,function(_gO){return E(_gN);});},_gP=unCStr("SI"),_gQ=[0,15],_gR=function(_gS){var _gT=new T(function(){return A(_gS,[_gQ]);});return _fm(_gP,function(_gU){return E(_gT);});},_gV=unCStr("DLE"),_gW=[0,16],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _fm(_gV,function(_h0){return E(_gZ);});},_h1=unCStr("DC1"),_h2=[0,17],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _fm(_h1,function(_h6){return E(_h5);});},_h7=unCStr("DC2"),_h8=[0,18],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _fm(_h7,function(_hc){return E(_hb);});},_hd=unCStr("DC3"),_he=[0,19],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _fm(_hd,function(_hi){return E(_hh);});},_hj=unCStr("DC4"),_hk=[0,20],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _fm(_hj,function(_ho){return E(_hn);});},_hp=unCStr("NAK"),_hq=[0,21],_hr=function(_hs){var _ht=new T(function(){return A(_hs,[_hq]);});return _fm(_hp,function(_hu){return E(_ht);});},_hv=unCStr("SYN"),_hw=[0,22],_hx=function(_hy){var _hz=new T(function(){return A(_hy,[_hw]);});return _fm(_hv,function(_hA){return E(_hz);});},_hB=unCStr("ETB"),_hC=[0,23],_hD=function(_hE){var _hF=new T(function(){return A(_hE,[_hC]);});return _fm(_hB,function(_hG){return E(_hF);});},_hH=unCStr("CAN"),_hI=[0,24],_hJ=function(_hK){var _hL=new T(function(){return A(_hK,[_hI]);});return _fm(_hH,function(_hM){return E(_hL);});},_hN=unCStr("EM"),_hO=[0,25],_hP=function(_hQ){var _hR=new T(function(){return A(_hQ,[_hO]);});return _fm(_hN,function(_hS){return E(_hR);});},_hT=unCStr("SUB"),_hU=[0,26],_hV=function(_hW){var _hX=new T(function(){return A(_hW,[_hU]);});return _fm(_hT,function(_hY){return E(_hX);});},_hZ=unCStr("ESC"),_i0=[0,27],_i1=function(_i2){var _i3=new T(function(){return A(_i2,[_i0]);});return _fm(_hZ,function(_i4){return E(_i3);});},_i5=unCStr("FS"),_i6=[0,28],_i7=function(_i8){var _i9=new T(function(){return A(_i8,[_i6]);});return _fm(_i5,function(_ia){return E(_i9);});},_ib=unCStr("GS"),_ic=[0,29],_id=function(_ie){var _if=new T(function(){return A(_ie,[_ic]);});return _fm(_ib,function(_ig){return E(_if);});},_ih=unCStr("RS"),_ii=[0,30],_ij=function(_ik){var _il=new T(function(){return A(_ik,[_ii]);});return _fm(_ih,function(_im){return E(_il);});},_in=unCStr("US"),_io=[0,31],_ip=function(_iq){var _ir=new T(function(){return A(_iq,[_io]);});return _fm(_in,function(_is){return E(_ir);});},_it=unCStr("SP"),_iu=[0,32],_iv=function(_iw){var _ix=new T(function(){return A(_iw,[_iu]);});return _fm(_it,function(_iy){return E(_ix);});},_iz=unCStr("DEL"),_iA=[0,127],_iB=function(_iC){var _iD=new T(function(){return A(_iC,[_iA]);});return _fm(_iz,function(_iE){return E(_iD);});},_iF=[1,_iB,_T],_iG=[1,_iv,_iF],_iH=[1,_ip,_iG],_iI=[1,_ij,_iH],_iJ=[1,_id,_iI],_iK=[1,_i7,_iJ],_iL=[1,_i1,_iK],_iM=[1,_hV,_iL],_iN=[1,_hP,_iM],_iO=[1,_hJ,_iN],_iP=[1,_hD,_iO],_iQ=[1,_hx,_iP],_iR=[1,_hr,_iQ],_iS=[1,_hl,_iR],_iT=[1,_hf,_iS],_iU=[1,_h9,_iT],_iV=[1,_h3,_iU],_iW=[1,_gX,_iV],_iX=[1,_gR,_iW],_iY=[1,_gL,_iX],_iZ=[1,_gF,_iY],_j0=[1,_gz,_iZ],_j1=[1,_gt,_j0],_j2=[1,_gn,_j1],_j3=[1,_gh,_j2],_j4=[1,_gb,_j3],_j5=[1,_g5,_j4],_j6=[1,_fZ,_j5],_j7=[1,_fT,_j6],_j8=[1,_fN,_j7],_j9=[1,_fH,_j8],_ja=[1,_fB,_j9],_jb=unCStr("SOH"),_jc=[0,1],_jd=function(_je){var _jf=new T(function(){return A(_je,[_jc]);});return _fm(_jb,function(_jg){return E(_jf);});},_jh=unCStr("SO"),_ji=[0,14],_jj=function(_jk){var _jl=new T(function(){return A(_jk,[_ji]);});return _fm(_jh,function(_jm){return E(_jl);});},_jn=function(_jo){return _cv(_jd,_jj,_jo);},_jp=[1,_jn,_ja],_jq=new T(function(){return _fa(_jp);}),_jr=[0,1114111],_js=[0,34],_jt=[0,_js,_eL],_ju=[0,39],_jv=[0,_ju,_eL],_jw=[0,92],_jx=[0,_jw,_eL],_jy=[0,_ga,_eL],_jz=[0,_gg,_eL],_jA=[0,_gE,_eL],_jB=[0,_gs,_eL],_jC=[0,_gK,_eL],_jD=[0,_gm,_eL],_jE=[0,_gy,_eL],_jF=[0,_fA,_eL],_jG=[0,_jc,_eL],_jH=[0,_fG,_eL],_jI=[0,_fM,_eL],_jJ=[0,_fS,_eL],_jK=[0,_fY,_eL],_jL=[0,_g4,_eL],_jM=[0,_ga,_eL],_jN=[0,_gg,_eL],_jO=[0,_gm,_eL],_jP=[0,_gs,_eL],_jQ=[0,_gy,_eL],_jR=[0,_gE,_eL],_jS=[0,_gK,_eL],_jT=[0,_ji,_eL],_jU=[0,_gQ,_eL],_jV=[0,_gW,_eL],_jW=[0,_h2,_eL],_jX=[0,_h8,_eL],_jY=[0,_he,_eL],_jZ=[0,_hk,_eL],_k0=[0,_hq,_eL],_k1=[0,_hw,_eL],_k2=[0,_hC,_eL],_k3=[0,_hI,_eL],_k4=[0,_hO,_eL],_k5=[0,_hU,_eL],_k6=[0,_i0,_eL],_k7=[0,_i6,_eL],_k8=[0,_ic,_eL],_k9=[0,_ii,_eL],_ka=[0,_io,_eL],_kb=function(_kc){return [0,_kc];},_kd=function(_ke){var _kf=new T(function(){return A(_ke,[_jE]);}),_kg=new T(function(){return A(_ke,[_jD]);}),_kh=new T(function(){return A(_ke,[_jC]);}),_ki=new T(function(){return A(_ke,[_jB]);}),_kj=new T(function(){return A(_ke,[_jA]);}),_kk=new T(function(){return A(_ke,[_jz]);}),_kl=new T(function(){return A(_ke,[_jy]);}),_km=new T(function(){return A(_ke,[_jx]);}),_kn=new T(function(){return A(_ke,[_jv]);}),_ko=new T(function(){return A(_ke,[_jt]);});return _bm([0,function(_kp){switch(E(E(_kp)[1])){case 34:return E(_ko);case 39:return E(_kn);case 92:return E(_km);case 97:return E(_kl);case 98:return E(_kk);case 102:return E(_kj);case 110:return E(_ki);case 114:return E(_kh);case 116:return E(_kg);case 118:return E(_kf);default:return [2];}}],new T(function(){return _bm(_cv(_eM,_eT,function(_kq){var _kr=new T(function(){return _kb(E(_kq)[1]);});return _cU(_kq,function(_ks){var _kt=_dU(_kr,_dL,_ks);return !_f0(_kt,_jr)?[2]:A(_ke,[[0,new T(function(){var _ku=_eX(_kt);return _ku>>>0>1114111?_eV(_ku):[0,_ku];}),_eL]]);});}),new T(function(){var _kv=new T(function(){return A(_ke,[_ka]);}),_kw=new T(function(){return A(_ke,[_k9]);}),_kx=new T(function(){return A(_ke,[_k8]);}),_ky=new T(function(){return A(_ke,[_k7]);}),_kz=new T(function(){return A(_ke,[_k6]);}),_kA=new T(function(){return A(_ke,[_k5]);}),_kB=new T(function(){return A(_ke,[_k4]);}),_kC=new T(function(){return A(_ke,[_k3]);}),_kD=new T(function(){return A(_ke,[_k2]);}),_kE=new T(function(){return A(_ke,[_k1]);}),_kF=new T(function(){return A(_ke,[_k0]);}),_kG=new T(function(){return A(_ke,[_jZ]);}),_kH=new T(function(){return A(_ke,[_jY]);}),_kI=new T(function(){return A(_ke,[_jX]);}),_kJ=new T(function(){return A(_ke,[_jW]);}),_kK=new T(function(){return A(_ke,[_jV]);}),_kL=new T(function(){return A(_ke,[_jU]);}),_kM=new T(function(){return A(_ke,[_jT]);}),_kN=new T(function(){return A(_ke,[_jS]);}),_kO=new T(function(){return A(_ke,[_jR]);}),_kP=new T(function(){return A(_ke,[_jQ]);}),_kQ=new T(function(){return A(_ke,[_jP]);}),_kR=new T(function(){return A(_ke,[_jO]);}),_kS=new T(function(){return A(_ke,[_jN]);}),_kT=new T(function(){return A(_ke,[_jM]);}),_kU=new T(function(){return A(_ke,[_jL]);}),_kV=new T(function(){return A(_ke,[_jK]);}),_kW=new T(function(){return A(_ke,[_jJ]);}),_kX=new T(function(){return A(_ke,[_jI]);}),_kY=new T(function(){return A(_ke,[_jH]);}),_kZ=new T(function(){return A(_ke,[_jG]);}),_l0=new T(function(){return A(_ke,[_jF]);});return _bm([0,function(_l1){return E(E(_l1)[1])==94?E([0,function(_l2){switch(E(E(_l2)[1])){case 64:return E(_l0);case 65:return E(_kZ);case 66:return E(_kY);case 67:return E(_kX);case 68:return E(_kW);case 69:return E(_kV);case 70:return E(_kU);case 71:return E(_kT);case 72:return E(_kS);case 73:return E(_kR);case 74:return E(_kQ);case 75:return E(_kP);case 76:return E(_kO);case 77:return E(_kN);case 78:return E(_kM);case 79:return E(_kL);case 80:return E(_kK);case 81:return E(_kJ);case 82:return E(_kI);case 83:return E(_kH);case 84:return E(_kG);case 85:return E(_kF);case 86:return E(_kE);case 87:return E(_kD);case 88:return E(_kC);case 89:return E(_kB);case 90:return E(_kA);case 91:return E(_kz);case 92:return E(_ky);case 93:return E(_kx);case 94:return E(_kw);case 95:return E(_kv);default:return [2];}}]):[2];}],new T(function(){return A(_jq,[function(_l3){return A(_ke,[[0,_l3,_eL]]);}]);}));}));}));},_l4=function(_l5){return A(_l5,[_0]);},_l6=function(_l7){var _l8=E(_l7);if(!_l8[0]){return E(_l4);}else{var _l9=_l8[2],_la=E(E(_l8[1])[1]);switch(_la){case 9:var _lb=new T(function(){return _l6(_l9);});return function(_lc){var _ld=new T(function(){return A(_lb,[_lc]);});return [0,function(_le){return E(_ld);}];};case 10:var _lf=new T(function(){return _l6(_l9);});return function(_lg){var _lh=new T(function(){return A(_lf,[_lg]);});return [0,function(_li){return E(_lh);}];};case 11:var _lj=new T(function(){return _l6(_l9);});return function(_lk){var _ll=new T(function(){return A(_lj,[_lk]);});return [0,function(_lm){return E(_ll);}];};case 12:var _ln=new T(function(){return _l6(_l9);});return function(_lo){var _lp=new T(function(){return A(_ln,[_lo]);});return [0,function(_lq){return E(_lp);}];};case 13:var _lr=new T(function(){return _l6(_l9);});return function(_ls){var _lt=new T(function(){return A(_lr,[_ls]);});return [0,function(_lu){return E(_lt);}];};case 32:var _lv=new T(function(){return _l6(_l9);});return function(_lw){var _lx=new T(function(){return A(_lv,[_lw]);});return [0,function(_ly){return E(_lx);}];};case 160:var _lz=new T(function(){return _l6(_l9);});return function(_lA){var _lB=new T(function(){return A(_lz,[_lA]);});return [0,function(_lC){return E(_lB);}];};default:var _lD=u_iswspace(_la);if(!E(_lD)){return E(_l4);}else{var _lE=new T(function(){return _l6(_l9);});return function(_lF){var _lG=new T(function(){return A(_lE,[_lF]);});return [0,function(_lH){return E(_lG);}];};}}}},_lI=function(_lJ){var _lK=new T(function(){return _kd(_lJ);}),_lL=new T(function(){return _lI(_lJ);}),_lM=[1,function(_lN){return A(_l6,[_lN,function(_lO){return E([0,function(_lP){return E(E(_lP)[1])==92?E(_lL):[2];}]);}]);}];return _bm([0,function(_lQ){return E(E(_lQ)[1])==92?E([0,function(_lR){var _lS=E(E(_lR)[1]);switch(_lS){case 9:return E(_lM);case 10:return E(_lM);case 11:return E(_lM);case 12:return E(_lM);case 13:return E(_lM);case 32:return E(_lM);case 38:return E(_lL);case 160:return E(_lM);default:var _lT=u_iswspace(_lS);return E(_lT)==0?[2]:E(_lM);}}]):[2];}],[0,function(_lU){var _lV=E(_lU);return E(_lV[1])==92?E(_lK):A(_lJ,[[0,_lV,_eK]]);}]);},_lW=function(_lX,_lY){var _lZ=new T(function(){return A(_lY,[[1,new T(function(){return A(_lX,[_T]);})]]);});return _lI(function(_m0){var _m1=E(_m0),_m2=E(_m1[1]);return E(_m2[1])==34?!E(_m1[2])?E(_lZ):_lW(function(_m3){return A(_lX,[[1,_m2,_m3]]);},_lY):_lW(function(_m4){return A(_lX,[[1,_m2,_m4]]);},_lY);});},_m5=unCStr("_\'"),_m6=function(_m7){var _m8=u_iswalnum(_m7);return E(_m8)==0?_eo(_bT,[0,_m7],_m5):true;},_m9=function(_ma){return _m6(E(_ma)[1]);},_mb=unCStr(",;()[]{}`"),_mc=function(_md){return A(_md,[_T]);},_me=function(_mf,_mg){var _mh=function(_mi){var _mj=E(_mi);if(!_mj[0]){return E(_mc);}else{var _mk=_mj[1];if(!A(_mf,[_mk])){return E(_mc);}else{var _ml=new T(function(){return _mh(_mj[2]);});return function(_mm){var _mn=new T(function(){return A(_ml,[function(_mo){return A(_mm,[[1,_mk,_mo]]);}]);});return [0,function(_mp){return E(_mn);}];};}}};return [1,function(_mq){return A(_mh,[_mq,_mg]);}];},_mr=unCStr(".."),_ms=unCStr("::"),_mt=unCStr("->"),_mu=[0,64],_mv=[1,_mu,_T],_mw=[0,126],_mx=[1,_mw,_T],_my=unCStr("=>"),_mz=[1,_my,_T],_mA=[1,_mx,_mz],_mB=[1,_mv,_mA],_mC=[1,_mt,_mB],_mD=unCStr("<-"),_mE=[1,_mD,_mC],_mF=[0,124],_mG=[1,_mF,_T],_mH=[1,_mG,_mE],_mI=[1,_jw,_T],_mJ=[1,_mI,_mH],_mK=[0,61],_mL=[1,_mK,_T],_mM=[1,_mL,_mJ],_mN=[1,_ms,_mM],_mO=[1,_mr,_mN],_mP=function(_mQ){var _mR=new T(function(){return A(_mQ,[_cR]);});return _bm([1,function(_mS){return E(_mS)[0]==0?E(_mR):[2];}],new T(function(){var _mT=new T(function(){return _kd(function(_mU){var _mV=E(_mU);return (function(_mW,_mX){var _mY=new T(function(){return A(_mQ,[[0,_mW]]);});return !E(_mX)?E(E(_mW)[1])==39?[2]:[0,function(_mZ){return E(E(_mZ)[1])==39?E(_mY):[2];}]:[0,function(_n0){return E(E(_n0)[1])==39?E(_mY):[2];}];})(_mV[1],_mV[2]);});});return _bm([0,function(_n1){return E(E(_n1)[1])==39?E([0,function(_n2){var _n3=E(_n2);switch(E(_n3[1])){case 39:return [2];case 92:return E(_mT);default:var _n4=new T(function(){return A(_mQ,[[0,_n3]]);});return [0,function(_n5){return E(E(_n5)[1])==39?E(_n4):[2];}];}}]):[2];}],new T(function(){var _n6=new T(function(){return _lW(_a,_mQ);});return _bm([0,function(_n7){return E(E(_n7)[1])==34?E(_n6):[2];}],new T(function(){return _bm([0,function(_n8){return !_eo(_bT,_n8,_mb)?[2]:A(_mQ,[[2,[1,_n8,_T]]]);}],new T(function(){return _bm([0,function(_n9){return !_eo(_bT,_n9,_et)?[2]:_me(_eu,function(_na){var _nb=[1,_n9,_na];return !_eo(_ca,_nb,_mO)?A(_mQ,[[4,_nb]]):A(_mQ,[[2,_nb]]);});}],new T(function(){return _bm([0,function(_nc){var _nd=E(_nc),_ne=_nd[1],_nf=u_iswalpha(_ne);return E(_nf)==0?E(_ne)==95?_me(_m9,function(_ng){return A(_mQ,[[3,[1,_nd,_ng]]]);}):[2]:_me(_m9,function(_nh){return A(_mQ,[[3,[1,_nd,_nh]]]);});}],new T(function(){return _cv(_ey,_ej,_mQ);}));}));}));}));}));}));},_ni=function(_nj){var _nk=new T(function(){return _mP(_nj);});return [1,function(_nl){return A(_l6,[_nl,function(_nm){return E(_nk);}]);}];},_nn=[0,0],_no=function(_np,_nq){var _nr=new T(function(){return A(_np,[_nn,function(_ns){var _nt=new T(function(){return A(_nq,[_ns]);});return _ni(function(_nu){var _nv=E(_nu);if(_nv[0]==2){var _nw=E(_nv[1]);return _nw[0]==0?[2]:E(E(_nw[1])[1])==41?E(_nw[2])[0]==0?E(_nt):[2]:[2];}else{return [2];}});}]);});return _ni(function(_nx){var _ny=E(_nx);if(_ny[0]==2){var _nz=E(_ny[1]);return _nz[0]==0?[2]:E(E(_nz[1])[1])==40?E(_nz[2])[0]==0?E(_nr):[2]:[2];}else{return [2];}});},_nA=function(_nB,_nC,_nD){var _nE=function(_nF,_nG){var _nH=new T(function(){return _mP(function(_nI){return A(_nB,[_nI,_nF,function(_nJ){return A(_nG,[new T(function(){return [0, -E(_nJ)[1]];})]);}]);});});return _bm(_ni(function(_nK){var _nL=E(_nK);if(_nL[0]==4){var _nM=E(_nL[1]);return _nM[0]==0?A(_nB,[_nL,_nF,_nG]):E(E(_nM[1])[1])==45?E(_nM[2])[0]==0?E([1,function(_nN){return A(_l6,[_nN,function(_nO){return E(_nH);}]);}]):A(_nB,[_nL,_nF,_nG]):A(_nB,[_nL,_nF,_nG]);}else{return A(_nB,[_nL,_nF,_nG]);}}),new T(function(){return _no(_nE,_nG);}));};return _nE(_nC,_nD);},_nP=function(_nQ,_nR){return [2];},_nS=function(_nT,_nU){return _nP(_nT,_nU);},_nV=function(_nW){var _nX=E(_nW);return _nX[0]==0?[1,new T(function(){return _dU(new T(function(){return _kb(E(_nX[1])[1]);}),_dL,_nX[2]);})]:E(_nX[2])[0]==0?E(_nX[3])[0]==0?[1,new T(function(){return _dU(_dK,_dL,_nX[1]);})]:[0]:[0];},_nY=function(_nZ){var _o0=E(_nZ);if(_o0[0]==5){var _o1=_nV(_o0[1]);if(!_o1[0]){return E(_nP);}else{var _o2=new T(function(){return [0,_eX(_o1[1])];});return function(_o3,_o4){return A(_o4,[_o2]);};}}else{return E(_nS);}},_o5=function(_nT,_nU){return _nA(_nY,_nT,_nU);},_o6=function(_o7,_o8){var _o9=function(_oa,_ob){var _oc=new T(function(){return A(_ob,[_T]);}),_od=new T(function(){return A(_o7,[_nn,function(_oe){return _o9(_eL,function(_of){return A(_ob,[[1,_oe,_of]]);});}]);});return _ni(function(_og){var _oh=E(_og);if(_oh[0]==2){var _oi=E(_oh[1]);if(!_oi[0]){return [2];}else{var _oj=_oi[2];switch(E(E(_oi[1])[1])){case 44:return E(_oj)[0]==0?!E(_oa)?[2]:E(_od):[2];case 93:return E(_oj)[0]==0?E(_oc):[2];default:return [2];}}}else{return [2];}});},_ok=function(_ol){var _om=new T(function(){return _bm(_o9(_eK,_ol),new T(function(){return A(_o7,[_nn,function(_on){return _o9(_eL,function(_oo){return A(_ol,[[1,_on,_oo]]);});}]);}));});return _bm(_ni(function(_op){var _oq=E(_op);if(_oq[0]==2){var _or=E(_oq[1]);return _or[0]==0?[2]:E(E(_or[1])[1])==91?E(_or[2])[0]==0?E(_om):[2]:[2];}else{return [2];}}),new T(function(){return _no(function(_os,_ot){return _ok(_ot);},_ol);}));};return _ok(_o8);},_ou=function(_ov,_ow){return _o6(_o5,_ow);},_ox=new T(function(){return _o6(_o5,_cn);}),_oy=function(_nU){return _bc(_ox,_nU);},_oz=function(_oA){var _oB=new T(function(){return _nA(_nY,_oA,_cn);});return function(_cQ){return _bc(_oB,_cQ);};},_oC=[0,_oz,_oy,_o5,_ou],_oD=function(_oE,_oF){return _3h(0,E(_oE)[1],_oF);},_oG=function(_oH,_oI){return _27(_oD,_oH,_oI);},_oJ=function(_oK,_oL,_oM){return _3h(E(_oK)[1],E(_oL)[1],_oM);},_oN=[0,_oJ,_52,_oG],_oO=unCStr("GHC.Types"),_oP=unCStr("Int"),_oQ=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5I,_oO,_oP],_oR=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oQ,_T],_oS=function(_oT){return E(_oR);},_oU=function(_oV){return E(E(_oV)[1]);},_oW=function(_oX){return E(E(_oX)[2]);},_oY=function(_oZ,_p0){var _p1=new T(function(){return A(_oW,[_oZ,_p0]);}),_p2=new T(function(){return _oU(_oZ);}),_p3=new T(function(){return _32(_p2);}),_p4=new T(function(){return _2C(_p2);});return function(_p5){return A(_p4,[_p1,function(_p6){return A(_p3,[[0,_p6,_p5]]);}]);};},_p7=function(_p8,_p9){return [0,_p8,function(_pa){return _oY(_p9,_pa);}];},_pb=[0,34],_pc=function(_pd,_pe,_){var _pf=jsWriteHandle(E(_pd)[1],toJSStr(E(_pe)));return _0;},_pg=[0,10],_ph=[1,_pg,_T],_pi=function(_pj,_pk,_){var _pl=E(_pj),_pm=jsWriteHandle(_pl[1],toJSStr(E(_pk)));return _pc(_pl,_ph,_);},_pn=function(_po,_pp){return A(_po,[function(_){return jsFind(toJSStr(E(_pp)));}]);},_pq=function(_pr){return E(E(_pr)[4]);},_ps=new T(function(){return E(_6B);}),_pt=function(_pu){return E(E(_pu)[7]);},_pv=unCStr("[]"),_pw=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5I,_oO,_pv],_px=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_pw,_T],_py=function(_pz){return E(_px);},_pA=unCStr("Char"),_pB=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5I,_oO,_pA],_pC=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_pB,_T],_pD=function(_pE){return E(_pC);},_pF=new T(function(){return _6C(_py,_pD);}),_pG=new T(function(){return A(_pF,[_6B]);}),_pH=function(_pI){return E(E(_pI)[1]);},_pJ=[0,0],_pK=[0,32],_pL=[0,10],_pM=function(_pN){var _pO=E(_pN);if(!_pO[0]){return E(_a);}else{var _pP=_pO[1],_pQ=E(_pO[2]);if(!_pQ[0]){return _pR(_pL,_pP);}else{var _pS=new T(function(){return _pM(_pQ);}),_pT=new T(function(){return _pR(_pL,_pP);});return function(_pU){return A(_pT,[[1,_pK,new T(function(){return A(_pS,[_pU]);})]]);};}}},_pV=unCStr("->"),_pW=[1,_pV,_T],_pX=[1,_oO,_pW],_pY=[1,_5I,_pX],_pZ=[0,32],_q0=function(_q1){var _q2=E(_q1);if(!_q2[0]){return [0];}else{var _q3=_q2[1],_q4=E(_q2[2]);return _q4[0]==0?E(_q3):_1h(_q3,[1,_pZ,new T(function(){return _q0(_q4);})]);}},_q5=new T(function(){return _q0(_pY);}),_q6=new T(function(){var _q7=_66(_q5);return [0,_q7[1],_q7[2],_5I,_oO,_pV];}),_q8=function(_q9,_qa){var _qb=E(_q9);return _qb[0]==0?E(_qa):A(_qb[1],[new T(function(){return _q8(_qb[2],_qa);})]);},_qc=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_qd=[1,_5K,_T],_qe=function(_qf){var _qg=E(_qf);if(!_qg[0]){return [0];}else{var _qh=E(_qg[1]);return [1,[0,_qh[1],_qh[2]],new T(function(){return _qe(_qg[2]);})];}},_qi=new T(function(){var _qj=_1h(_T,_qd);if(!_qj[0]){return E(_pw);}else{var _qk=_66(new T(function(){return _5U(_6i(_6t,[1,_qc,new T(function(){return _qe(_qj);})]));}));return E(_pw);}}),_ql=[0,40],_qm=function(_qn){return _pR(_pL,_qn);},_qo=[0,8],_qp=unCStr(" -> "),_qq=[0,9],_qr=[0,93],_qs=[0,91],_qt=[0,41],_qu=[0,44],_qv=function(_qn){return [1,_qu,_qn];},_qw=function(_qx,_qy){var _qz=E(_qy);return _qz[0]==0?[0]:[1,_qx,[1,_qz[1],new T(function(){return _qw(_qx,_qz[2]);})]];},_pR=function(_qA,_qB){var _qC=E(_qB),_qD=_qC[3],_qE=E(_qC[4]);if(!_qE[0]){return function(_qF){return _1h(E(_qD)[5],_qF);};}else{var _qG=_qE[1],_qH=new T(function(){var _qI=E(_qD)[5],_qJ=new T(function(){return _pM(_qE);}),_qK=new T(function(){return E(_qA)[1]<=9?function(_qL){return _1h(_qI,[1,_pK,new T(function(){return A(_qJ,[_qL]);})]);}:function(_qM){return [1,_3g,new T(function(){return _1h(_qI,[1,_pK,new T(function(){return A(_qJ,[[1,_3f,_qM]]);})]);})];};}),_qN=E(_qI);if(!_qN[0]){return E(_qK);}else{if(E(E(_qN[1])[1])==40){var _qO=E(_qN[2]);return _qO[0]==0?E(_qK):E(E(_qO[1])[1])==44?function(_qP){return [1,_ql,new T(function(){return A(new T(function(){var _qQ=_6i(_qm,_qE);if(!_qQ[0]){return E(_a);}else{var _qR=new T(function(){return _qw(_qv,_qQ[2]);});return function(_cQ){return _q8([1,_qQ[1],_qR],_cQ);};}}),[[1,_qt,_qP]]);})];}:E(_qK);}else{return E(_qK);}}}),_qS=E(_qE[2]);if(!_qS[0]){var _qT=E(_qD),_qU=E(_qi),_qV=hs_eqWord64(_qT[1],_qU[1]);if(!E(_qV)){return E(_qH);}else{var _qW=hs_eqWord64(_qT[2],_qU[2]);if(!E(_qW)){return E(_qH);}else{var _qX=new T(function(){return _pR(_pJ,_qG);});return function(_qY){return [1,_qs,new T(function(){return A(_qX,[[1,_qr,_qY]]);})];};}}}else{if(!E(_qS[2])[0]){var _qZ=E(_qD),_r0=E(_q6),_r1=hs_eqWord64(_qZ[1],_r0[1]);if(!E(_r1)){return E(_qH);}else{var _r2=hs_eqWord64(_qZ[2],_r0[2]);if(!E(_r2)){return E(_qH);}else{var _r3=new T(function(){return _pR(_qo,_qS[1]);}),_r4=new T(function(){return _pR(_qq,_qG);});return E(_qA)[1]<=8?function(_r5){return A(_r4,[new T(function(){return _1h(_qp,new T(function(){return A(_r3,[_r5]);}));})]);}:function(_r6){return [1,_3g,new T(function(){return A(_r4,[new T(function(){return _1h(_qp,new T(function(){return A(_r3,[[1,_3f,_r6]]);}));})]);})];};}}}else{return E(_qH);}}}},_r7=function(_r8,_r9,_ra,_rb){var _rc=new T(function(){return _32(_r8);}),_rd=new T(function(){return _pq(_rb);}),_re=new T(function(){return _pt(_rb);}),_rf=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pR,[_pJ,A(_r9,[_ps]),_T]);}));}),_rg=new T(function(){return A(_pH,[_ra,_3F]);});return function(_rh){if(!E(new T(function(){var _ri=A(_r9,[_ps]),_rj=E(_pG),_rk=hs_eqWord64(_ri[1],_rj[1]);if(!E(_rk)){return false;}else{var _rl=hs_eqWord64(_ri[2],_rj[2]);return E(_rl)==0?false:true;}}))){var _rm=new T(function(){return A(_rc,[[1,_rh,new T(function(){return A(_re,[new T(function(){return A(_rd,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1h(_rh,_rf);}));})]);})]);})]]);}),_rn=A(_rg,[_rh]);if(!_rn[0]){return E(_rm);}else{var _ro=E(_rn[1]);return E(_ro[2])[0]==0?E(_rn[2])[0]==0?A(_rc,[[2,_ro[1]]]):E(_rm):E(_rm);}}else{return A(_rc,[[2,_rh]]);}};},_rp=function(_rq){return E(E(_rq)[2]);},_rr=[0],_rs=new T(function(){return [0,"value"];}),_rt=[1,_pb,_T],_ru=unCStr("Prelude.(!!): negative index\n"),_rv=new T(function(){return err(_ru);}),_rw=unCStr("Prelude.(!!): index too large\n"),_rx=new T(function(){return err(_rw);}),_ry=function(_rz,_rA){while(1){var _rB=E(_rz);if(!_rB[0]){return E(_rx);}else{var _rC=E(_rA);if(!_rC){return E(_rB[1]);}else{_rz=_rB[2];_rA=_rC-1|0;continue;}}}},_rD=unCStr("ACK"),_rE=unCStr("BEL"),_rF=unCStr("BS"),_rG=unCStr("SP"),_rH=[1,_rG,_T],_rI=unCStr("US"),_rJ=[1,_rI,_rH],_rK=unCStr("RS"),_rL=[1,_rK,_rJ],_rM=unCStr("GS"),_rN=[1,_rM,_rL],_rO=unCStr("FS"),_rP=[1,_rO,_rN],_rQ=unCStr("ESC"),_rR=[1,_rQ,_rP],_rS=unCStr("SUB"),_rT=[1,_rS,_rR],_rU=unCStr("EM"),_rV=[1,_rU,_rT],_rW=unCStr("CAN"),_rX=[1,_rW,_rV],_rY=unCStr("ETB"),_rZ=[1,_rY,_rX],_s0=unCStr("SYN"),_s1=[1,_s0,_rZ],_s2=unCStr("NAK"),_s3=[1,_s2,_s1],_s4=unCStr("DC4"),_s5=[1,_s4,_s3],_s6=unCStr("DC3"),_s7=[1,_s6,_s5],_s8=unCStr("DC2"),_s9=[1,_s8,_s7],_sa=unCStr("DC1"),_sb=[1,_sa,_s9],_sc=unCStr("DLE"),_sd=[1,_sc,_sb],_se=unCStr("SI"),_sf=[1,_se,_sd],_sg=unCStr("SO"),_sh=[1,_sg,_sf],_si=unCStr("CR"),_sj=[1,_si,_sh],_sk=unCStr("FF"),_sl=[1,_sk,_sj],_sm=unCStr("VT"),_sn=[1,_sm,_sl],_so=unCStr("LF"),_sp=[1,_so,_sn],_sq=unCStr("HT"),_sr=[1,_sq,_sp],_ss=[1,_rF,_sr],_st=[1,_rE,_ss],_su=[1,_rD,_st],_sv=unCStr("ENQ"),_sw=[1,_sv,_su],_sx=unCStr("EOT"),_sy=[1,_sx,_sw],_sz=unCStr("ETX"),_sA=[1,_sz,_sy],_sB=unCStr("STX"),_sC=[1,_sB,_sA],_sD=unCStr("SOH"),_sE=[1,_sD,_sC],_sF=unCStr("NUL"),_sG=[1,_sF,_sE],_sH=[0,92],_sI=unCStr("\\DEL"),_sJ=unCStr("\\a"),_sK=unCStr("\\\\"),_sL=unCStr("\\SO"),_sM=unCStr("\\r"),_sN=unCStr("\\f"),_sO=unCStr("\\v"),_sP=unCStr("\\n"),_sQ=unCStr("\\t"),_sR=unCStr("\\b"),_sS=function(_sT,_sU){if(_sT<=127){var _sV=E(_sT);switch(_sV){case 92:return _1h(_sK,_sU);case 127:return _1h(_sI,_sU);default:if(_sV<32){var _sW=E(_sV);switch(_sW){case 7:return _1h(_sJ,_sU);case 8:return _1h(_sR,_sU);case 9:return _1h(_sQ,_sU);case 10:return _1h(_sP,_sU);case 11:return _1h(_sO,_sU);case 12:return _1h(_sN,_sU);case 13:return _1h(_sM,_sU);case 14:return _1h(_sL,new T(function(){var _sX=E(_sU);return _sX[0]==0?[0]:E(E(_sX[1])[1])==72?unAppCStr("\\&",_sX):E(_sX);}));default:return _1h([1,_sH,new T(function(){var _sY=_sW;return _sY>=0?_ry(_sG,_sY):E(_rv);})],_sU);}}else{return [1,[0,_sV],_sU];}}}else{return [1,_sH,new T(function(){var _sZ=jsShowI(_sT);return _1h(fromJSStr(_sZ),new T(function(){var _t0=E(_sU);if(!_t0[0]){return [0];}else{var _t1=E(_t0[1])[1];return _t1<48?E(_t0):_t1>57?E(_t0):unAppCStr("\\&",_t0);}}));})];}},_t2=unCStr("\\\""),_t3=function(_t4,_t5){var _t6=E(_t4);if(!_t6[0]){return E(_t5);}else{var _t7=_t6[2],_t8=E(E(_t6[1])[1]);return _t8==34?_1h(_t2,new T(function(){return _t3(_t7,_t5);})):_sS(_t8,new T(function(){return _t3(_t7,_t5);}));}},_t9=unCStr("True"),_ta=unCStr("False"),_tb=function(_){var _=0,_tc=jsMkStdout();return [0,_tc];},_td=new T(function(){return _3J(_tb);}),_te=function(_tf,_tg,_th,_ti){var _tj=new T(function(){return _oW(_tf);}),_tk=new T(function(){return _oU(_tf);}),_tl=new T(function(){return A(_32,[_tk,_rr]);}),_tm=new T(function(){return _r7(_tk,_tg,_th,_ti);}),_tn=new T(function(){return _2C(_tk);}),_to=new T(function(){return _rp(_tk);}),_tp=new T(function(){return _2C(_tk);});return function(_tq){return A(_tp,[new T(function(){return _pn(E(_tf)[2],_tq);}),function(_tr){return A(_to,[new T(function(){return A(_tj,[function(_){return _pi(_td,[1,_pb,new T(function(){return _t3(unAppCStr("elemByid ",new T(function(){return _1h(_tq,new T(function(){return unAppCStr(" result= ",new T(function(){return E(_tr)[0]==0?E(_ta):E(_t9);}));}));})),_rt);})],_);}]);}),new T(function(){var _ts=E(_tr);return _ts[0]==0?E(_tl):A(_tn,[new T(function(){return A(E(_tf)[2],[function(_){var _tt=jsGet(E(_ts[1])[1],E(_rs)[1]);return [1,new T(function(){return fromJSStr(_tt);})];}]);}),function(_tu){var _tv=E(_tu);return _tv[0]==0?E(_tl):A(_tm,[_tv[1]]);}]);})]);}]);};},_tw=1,_tx=function(_ty){return E(E(_ty)[10]);},_tz=function(_tA,_tB){return A(_32,[_tA,[0,_tB,_tB]]);},_tC=function(_tD,_tE,_tF){return A(_32,[_tD,[0,_0,_tE]]);},_tG=function(_tH){return E(E(_tH)[2]);},_tI=function(_tJ,_tK,_tL,_tM,_tN){var _tO=new T(function(){return _8Y(_tJ);}),_tP=new T(function(){return _90(_tO);}),_tQ=new T(function(){return _oU(_tK);}),_tR=new T(function(){return _34(_tQ);}),_tS=new T(function(){return _3m([0,coercionToken],_tR,function(_tT){return _tz(_tQ,_tT);},function(_tU,_tV){return _tC(_tQ,_tU,_tV);});}),_tW=new T(function(){return _p7(_tR,_tK);}),_tX=new T(function(){return _te(_tW,_tL,_tN,_tJ);}),_tY=new T(function(){return _32(_tQ);}),_tZ=new T(function(){return _2C(_tQ);}),_u0=new T(function(){return _32(_tQ);}),_u1=new T(function(){return _2C(_tQ);}),_u2=new T(function(){return _32(_tQ);}),_u3=new T(function(){return _2C(_tQ);}),_u4=new T(function(){return _32(_tQ);}),_u5=new T(function(){return _2C(_tQ);}),_u6=new T(function(){return _tG(_tM);}),_u7=new T(function(){return _tx(_tJ);});return function(_u8,_u9,_ua){return function(_ub){return A(_u5,[new T(function(){var _uc=E(_u8);return _uc[0]==0?A(_tS,[_ub]):A(_u4,[[0,_uc[1],_ub]]);}),function(_ud){var _ue=new T(function(){return E(E(_ud)[1]);}),_uf=new T(function(){return A(_tX,[_ue]);}),_ug=new T(function(){return A(_u7,[_ue,_u9,new T(function(){var _uh=E(_ua);if(!_uh[0]){return [0];}else{var _ui=_uh[1],_uj=_11(_tL,_pF,_ui);return _uj[0]==0?A(_u6,[_ui]):E(_uj[1]);}}),_eK,_2r]);});return A(_u3,[new T(function(){var _uk=new T(function(){return E(E(_ud)[2]);});return A(_u2,[[0,_uk,_uk]]);}),function(_ul){return A(_u1,[new T(function(){return A(_u0,[[0,_0,new T(function(){var _um=E(E(_ul)[1]);return [0,_um[1],_um[2],_tw,_um[4],_um[5]];})]]);}),function(_un){return A(_tZ,[new T(function(){return A(_uf,[new T(function(){return E(E(_un)[2]);})]);}),function(_uo){var _up=E(_uo),_uq=_up[2],_ur=E(_up[1]);switch(_ur[0]){case 0:return A(_tY,[[0,[0,_ug,_2r],_uq]]);case 1:return A(_tY,[[0,[0,new T(function(){return A(_tP,[new T(function(){return A(_u7,[_ue,_u9,_ur[1],_eK,_2r]);}),_ur[2]]);}),_2r],_uq]]);default:var _us=_ur[1];return A(_tY,[[0,[0,new T(function(){return A(_u7,[_ue,_u9,new T(function(){var _ut=_11(_tL,_pF,_us);return _ut[0]==0?A(_u6,[_us]):E(_ut[1]);}),_eK,_2r]);}),[1,_us]],_uq]]);}}]);}]);}]);}]);};};},_uu=new T(function(){return _tI(_8B,_aj,_oS,_oN,_oC);}),_uv=new T(function(){return A(_uu,[_2r,_ai,_2r]);}),_uw=function(_ux,_){var _uy=E(_ux),_uz=E(_uy[4]),_uA=A(_uv,[_uy,_]),_uB=E(_uA),_uC=E(_uB[1]);return [0,[0,function(_uD,_){var _uE=_a5(_uC[1],_9G,function(_){var _uF=A(_uz[1],[_]),_uG=E(_uF);if(!_uG[0]){return _0;}else{var _uH=A(_uz[2],[_uG[1],_]);return _0;}},_uD,_),_uI=_5g(_uD,_);return _uD;},_uC[2]],_uB[2]];},_uJ=new T(function(){return [1,_uw,_uJ];}),_uK=function(_uL,_uM){var _uN=E(_uL);if(!_uN){return [0];}else{var _uO=E(_uM);return _uO[0]==0?[0]:[1,_uO[1],new T(function(){return _uK(_uN-1|0,_uO[2]);})];}},_uP=function(_uQ,_uR){return _uQ<0?[0]:_uK(_uQ,_uR);},_uS=function(_uT){return E(_uT);},_uU=function(_uV){var _uW=new T(function(){return _4T(_4M,new T(function(){return unAppCStr("This widget sums ",new T(function(){return _1h(_3h(0,E(_uV)[1],_T),_5u);}));}));});return function(_uX,_){var _uY=_4k(new T(function(){var _uZ=E(_uV)[1];return _uZ>0?_9y(_5y,_uP(_uZ,_uJ)):E(_9F);}),_5m,_uX,_),_v0=E(_uY),_v1=E(_v0[1]),_v2=new T(function(){return _4T(_uS,_v1[1]);});return [0,[0,function(_v3,_){var _v4=A(_uW,[_v3,_]),_v5=A(_v2,[_v3,_]);return _v3;},_v1[2]],_v0[2]];};},_v6=new T(function(){return _uU(_8);}),_v7=[0,3],_v8=[8,coercionToken],_v9=function(_va){while(1){var _vb=E(_va);if(!_vb[0]){_va=[1,I_fromInt(_vb[1])];continue;}else{return I_toString(_vb[1]);}}},_vc=function(_vd,_ve){return _1h(fromJSStr(_v9(_vd)),_ve);},_vf=function(_vg,_vh){var _vi=E(_vg);if(!_vi[0]){var _vj=_vi[1],_vk=E(_vh);return _vk[0]==0?_vj<_vk[1]:I_compareInt(_vk[1],_vj)>0;}else{var _vl=_vi[1],_vm=E(_vh);return _vm[0]==0?I_compareInt(_vl,_vm[1])<0:I_compare(_vl,_vm[1])<0;}},_vn=[0,0],_vo=function(_vp,_vq,_vr){return _vp<=6?_vc(_vq,_vr):!_vf(_vq,_vn)?_vc(_vq,_vr):[1,_3g,new T(function(){return _1h(fromJSStr(_v9(_vq)),[1,_3f,_vr]);})];},_vs=[0,1],_vt=function(_vu){return _bm(_ni(function(_vv){var _vw=E(_vv);return _vw[0]==0?A(_vu,[_vw[1]]):[2];}),new T(function(){return _no(_vx,_vu);}));},_vx=function(_vy,_vz){return _vt(_vz);},_vA=function(_vB){return _bm(_bm(_ni(function(_vC){var _vD=E(_vC);return _vD[0]==1?A(_vB,[_vD[1]]):[2];}),new T(function(){return _o6(_vx,_vB);})),new T(function(){return _no(_vE,_vB);}));},_vE=function(_vF,_vG){return _vA(_vG);},_vH=new T(function(){return _no(_vE,_cn);}),_vI=new T(function(){return _o6(_vx,_cn);}),_vJ=function(_vK){var _vL=E(_vK);return _vL[0]==1?[3,_vL[1],_cm]:[2];},_vM=new T(function(){return _mP(_vJ);}),_vN=function(_vO){return E(_vM);},_vP=function(_vQ){return A(_l6,[_vQ,_vN]);},_vR=[1,_vP],_vS=new T(function(){return _bm(_vR,_vI);}),_vT=new T(function(){return _bm(_vS,_vH);}),_vU=function(_nU){return _bc(_vT,_nU);},_vV=new T(function(){return _vt(_cn);}),_vW=function(_nU){return _bc(_vV,_nU);},_vX=function(_vY){return E(_vW);},_vZ=[0,_vX,_vU,_vx,_vE],_w0=function(_w1){return E(E(_w1)[4]);},_w2=function(_w3,_w4,_w5){return _o6(new T(function(){return _w0(_w3);}),_w5);},_w6=function(_w7){var _w8=new T(function(){return _o6(new T(function(){return _w0(_w7);}),_cn);});return function(_cQ){return _bc(_w8,_cQ);};},_w9=function(_wa,_wb){var _wc=new T(function(){return A(_w0,[_wa,_wb,_cn]);});return function(_cQ){return _bc(_wc,_cQ);};},_wd=function(_we){return [0,function(_nU){return _w9(_we,_nU);},new T(function(){return _w6(_we);}),new T(function(){return _w0(_we);}),function(_nT,_nU){return _w2(_we,_nT,_nU);}];},_wf=new T(function(){return _wd(_vZ);}),_wg=[0,39],_wh=[1,_wg,_T],_wi=unCStr("\'\\\'\'"),_wj=function(_wk){var _wl=E(E(_wk)[1]);return _wl==39?E(_wi):[1,_wg,new T(function(){return _sS(_wl,_wh);})];},_wm=function(_wn,_wo){return [1,_pb,new T(function(){return _t3(_wn,[1,_pb,_wo]);})];},_wp=function(_wq){return _1h(_wi,_wq);},_wr=function(_ws,_wt){var _wu=E(E(_wt)[1]);return _wu==39?E(_wp):function(_wv){return [1,_wg,new T(function(){return _sS(_wu,[1,_wg,_wv]);})];};},_ww=[0,_wr,_wj,_wm],_wx=function(_wy){return E(E(_wy)[3]);},_wz=function(_wA,_wB){return A(_wx,[_wA,_wB,_T]);},_wC=function(_wD,_wE,_wF){return _27(new T(function(){return _wx(_wD);}),_wE,_wF);},_wG=function(_wH){var _wI=new T(function(){return _wx(_wH);});return [0,function(_wJ){return E(_wI);},function(_wq){return _wz(_wH,_wq);},function(_wK,_wq){return _wC(_wH,_wK,_wq);}];},_wL=new T(function(){return _wG(_ww);}),_wM=new T(function(){return _tI(_8B,_aj,_pF,_wL,_wf);}),_wN=unCStr("submit"),_wO=new T(function(){return A(_wM,[_2r,_wN]);}),_wP=[0,43],_wQ=[1,_wP,_T],_wR=[1,_wQ],_wS=new T(function(){return A(_wO,[_wR]);}),_wT=function(_q,_){return _s(_r,_q,_);},_wU=new T(function(){return _3m(_D,_39,_B,_y);}),_wV=new T(function(){return _3m(_D,_39,_B,_y);}),_wW=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_wX=function(_wY){return _3J(function(_){var _=0;return eval(E(_wY)[1]);});},_wZ=new T(function(){return _wX(_wW);}),_x0=function(_x1,_x2,_x3,_){var _x4=A(_wU,[_x3,_]),_x5=A(_wV,[new T(function(){return E(E(_x4)[2]);}),_]),_x6=E(_x5),_x7=_x6[1],_x8=E(_x6[2]),_x9=new T(function(){return E(E(_x4)[1]);}),_xa=function(_xb){var _xc=new T(function(){return A(_x2,[_xb]);});return function(_xd,_){var _xe=A(_xc,[_xd,_]),_xf=E(_xe),_xg=E(_xf[1]);return [0,[0,function(_xh,_){var _xi=A(_xg[1],[_xh,_]),_xj=E(_x9),_xk=jsFind(toJSStr(_xj)),_xl=E(_xk);if(!_xl[0]){return _3O(_xj);}else{var _xm=E(_xl[1]),_xn=A(_wZ,[E(_xm[1]),_]),_xo=jsKillChild(E(_xm)[1],_xn);return _xh;}},_xg[2]],_xf[2]];};},_xp=function(_xq,_){var _xr=A(_x1,[_xq,_]),_xs=E(_xr),_xt=E(_xs[1]);return [0,[0,function(_xu,_){var _xv=_s(_r,_xu,_),_xw=A(_1,[_a,_xv,_9,_x9,_]),_xx=A(_xt[1],[_xv,_]);return _xv;},_xt[2]],_xs[2]];},_xy=_44(_xp,_xa,_x7,_x8[1],_x8[2],_x8[3],_x8[4],_x8[5],_),_xz=_xp(new T(function(){return E(E(_xy)[2]);}),_),_xA=E(_xz),_xB=_xA[2],_xC=E(_xA[1]),_xD=_xC[1],_xE=new T(function(){return _j(_wT,[1,[0,_9,_x7],_T]);}),_xF=E(_xC[2]);if(!_xF[0]){return [0,[0,function(_xG,_){var _xH=A(_xD,[_xG,_]),_xI=A(_xE,[_xG,_]);return _xG;},_2r],new T(function(){var _xJ=E(_xB);return [0,_xJ[1],_xJ[2],_xJ[3],new T(function(){return E(E(_xy)[1]);}),_xJ[5]];})];}else{var _xK=A(_xa,[_xF[1],new T(function(){var _xL=E(_xB);return [0,_xL[1],_xL[2],_xL[3],new T(function(){return E(E(_xy)[1]);}),_xL[5]];}),_]),_xM=E(_xK),_xN=E(_xM[1]);return [0,[0,function(_xO,_){var _xP=A(_xD,[_xO,_]),_xQ=A(_xE,[_xO,_]),_xR=A(_xN[1],[_xQ,_]);return _xO;},_xN[2]],_xM[2]];}},_xS=function(_xT){var _xU=new T(function(){return _vo(0,_xT,_T);}),_xV=new T(function(){return _xS(new T(function(){return _dw(_xT,_vs);}));}),_xW=new T(function(){return _56(_4M,_xU);});return function(_cQ,_xX){return _x0(function(_xY,_){var _xZ=E(_xY),_y0=E(_xZ[4]),_y1=A(_wS,[_xZ,_]),_y2=E(_y1),_y3=E(_y2[1]);return [0,[0,function(_y4,_){var _y5=A(_xW,[_y4,_]),_y6=_a5(_y3[1],_v8,function(_){var _y7=A(_y0[1],[_]),_y8=E(_y7);if(!_y8[0]){return _0;}else{var _y9=A(_y0[2],[_y8[1],_]);return _0;}},_y4,_);return _y4;},_y3[2]],_y2[2]];},function(_ya,_yb,_){return (function(_yb,_){return _4k(function(_yc,_){var _yd=_pi(_td,_xU,_);return [0,[0,_2z,[1,_yd]],_yc];},function(_ye){return E(_xV);},_yb,_);})(_yb,_);},_cQ,_xX);};},_yf=new T(function(){return _xS(_v7);}),_yg=unCStr("bottom"),_yh=new T(function(){return _56(_4M,_yg);}),_yi=unCStr("style"),_yj=unCStr("This widget sums recursively n numbers. When enters 0, present the result"),_yk=new T(function(){return _4T(_4M,_yj);}),_yl=new T(function(){return A(_uu,[_2r,_ai,_2r]);}),_ym=function(_yn,_){var _yo=E(_yn),_yp=E(_yo[4]),_yq=A(_yl,[_yo,_]),_yr=E(_yq),_ys=E(_yr[1]);return [0,[0,function(_yb,_){return _a5(_ys[1],_9G,function(_){var _yt=A(_yp[1],[_]),_yu=E(_yt);if(!_yu[0]){return _0;}else{var _yv=A(_yp[2],[_yu[1],_]);return _0;}},_yb,_);},_ys[2]],_yr[2]];},_yw=function(_yx){var _yy=new T(function(){return _56(_4M,new T(function(){return _52(_yx);}));});return function(_cQ,_xX){return _4k(_ym,function(_yz){var _yA=E(E(_yz)[1]);if(!_yA){return function(_yB,_){return [0,[0,function(_yC,_){var _yD=_5g(_yC,_),_yE=_4M(_5l,_yC,_),_yF=A(_yy,[_yC,_]);return _yC;},_2r],_yB];};}else{var _yG=new T(function(){return _yw(new T(function(){return [0,E(_yx)[1]+_yA|0];}));}),_yH=new T(function(){return _56(_4M,new T(function(){return _3h(0,E(_yx)[1]+_yA|0,_T);}));});return function(_cQ,_xX){return _4k(function(_yI,_){return [0,[0,function(_yJ,_){var _yK=A(_yH,[_yJ,_]),_yL=_5g(_yJ,_);return _yJ;},_5k],_yI];},function(_yM){return E(_yG);},_cQ,_xX);};}},_cQ,_xX);};},_yN=new T(function(){return _yw(_5v);}),_yO=function(_yP){return function(_yQ,_){return [0,[0,new T(function(){var _yR=new T(function(){return _56(_4M,new T(function(){return _52(_yP);}));});return _4T(_uS,function(_yS,_){var _yT=_4M(_5l,_yS,_),_yU=A(_yR,[_yS,_]);return _yS;});}),_5k],_yQ];};},_yV=unCStr("second number "),_yW=unCStr("first number"),_yX=new T(function(){return A(_uu,[_2r,_ai,_2r]);}),_yY=function(_yZ){return E(E(_yZ)[1]);},_z0=function(_z1,_z2,_z3,_z4,_z5){var _z6=new T(function(){return _8Y(_z1);}),_z7=new T(function(){return _yY(_z6);}),_z8=new T(function(){return _32(_z2);}),_z9=new T(function(){return _pt(_z1);}),_za=new T(function(){return _2C(_z2);}),_zb=new T(function(){return _32(_z2);}),_zc=new T(function(){return _2C(_z2);});return A(_z3,[function(_zd){return A(_zc,[new T(function(){return A(_z4,[_zd]);}),function(_ze){var _zf=E(_ze),_zg=E(_zf[1]);return A(_zb,[[0,[0,_zg[1],[1,_zg[2]]],_zf[2]]]);}]);},function(_zh){var _zi=E(_zh);if(!_zi[0]){return function(_zj){return A(_z8,[[0,[0,_z7,_2r],_zj]]);};}else{var _zk=new T(function(){return A(_z5,[_zi[1]]);});return function(_zl){return A(_za,[new T(function(){return A(_zk,[_zl]);}),function(_zm){var _zn=E(_zm),_zo=_zn[2],_zp=E(_zn[1]);return _zp[0]==0?A(_z8,[[0,[0,_z7,_zi],_zo]]):A(_z8,[[0,[0,new T(function(){return A(_z9,[_zp[1]]);}),_2r],_zo]]);}]);};}}]);},_zq=unCStr("more than 2"),_zr=new T(function(){return _56(_4M,_zq);}),_zs=[1,_zr],_zt=function(_zu,_zv,_){return E(_zu)[1]>=3?[0,_zs,_zv]:[0,_2r,_zv];},_zw=new T(function(){return A(_uu,[_2r,_ai,_2r]);}),_zx=function(_zy,_){var _zz=E(_zy),_zA=E(_zz[4]),_zB=A(_zw,[_zz,_]),_zC=E(_zB),_zD=E(_zC[1]);return [0,[0,function(_yb,_){return _a5(_zD[1],_9G,function(_){var _zE=A(_zA[1],[_]),_zF=E(_zE);if(!_zF[0]){return _0;}else{var _zG=A(_zA[2],[_zF[1],_]);return _0;}},_yb,_);},_zD[2]],_zC[2]];},_zH=new T(function(){return _z0(_8B,_2B,_4k,_zx,_zt);}),_zI=function(_zJ,_){var _zK=A(_zH,[_zJ,_]),_zL=E(_zK),_zM=E(_zL[1]),_zN=E(_zL[2]),_zO=E(_zN[4]),_zP=A(_yX,[_zN,_]),_zQ=E(_zP),_zR=E(_zQ[1]);return [0,[0,function(_zS,_){var _zT=_4M(_yW,_zS,_),_zU=_5g(_zS,_),_zV=A(_zM[1],[_zS,_]),_zW=_5g(_zS,_),_zX=_4M(_yV,_zS,_),_zY=_5g(_zS,_),_zZ=_a5(_zR[1],_9G,function(_){var _A0=A(_zO[1],[_]),_A1=E(_A0);if(!_A1[0]){return _0;}else{var _A2=A(_zO[2],[_A1[1],_]);return _0;}},_zS,_),_A3=_5g(_zS,_);return _zS;},new T(function(){var _A4=E(_zM[2]);if(!_A4[0]){return [0];}else{var _A5=E(_zR[2]);return _A5[0]==0?[0]:[1,new T(function(){return _8Q(_A4[1],_A5[1]);})];}})],_zQ[2]];},_A6=unCStr("This widget sums two numbers and append the result. Using applicative and monadic expressions"),_A7=new T(function(){return _4T(_4M,_A6);}),_A8=function(_A9,_){var _Aa=_4k(_zI,_yO,_A9,_),_Ab=E(_Aa),_Ac=E(_Ab[1]),_Ad=new T(function(){return _4T(_uS,_Ac[1]);});return [0,[0,function(_Ae,_){var _Af=A(_A7,[_Ae,_]),_Ag=A(_Ad,[_Ae,_]);return _Ae;},_Ac[2]],_Ab[2]];},_Ah=unCStr("This widget sums recursively n numbers, but remember the previos entries when one entry is edited"),_Ai=new T(function(){return _4T(_4M,_Ah);}),_Aj=[0,0],_Ak=function(_Al){var _Am=new T(function(){return A(_uu,[_2r,_ai,_Al]);});return function(_An,_){var _Ao=E(_An),_Ap=E(_Ao[4]),_Aq=A(_Am,[_Ao,_]),_Ar=E(_Aq),_As=E(_Ar[1]);return [0,[0,function(_yb,_){return _a5(_As[1],_9G,function(_){var _At=A(_Ap[1],[_]),_Au=E(_At);if(!_Au[0]){return _0;}else{var _Av=A(_Ap[2],[_Au[1],_]);return _0;}},_yb,_);},_As[2]],_Ar[2]];};},_Aw=function(_Ax,_Ay){while(1){var _Az=E(_Ax);if(!_Az[0]){return E(_Ay)[0]==0?1:0;}else{var _AA=E(_Ay);if(!_AA[0]){return 2;}else{var _AB=E(_Az[1])[1],_AC=E(_AA[1])[1];if(_AB!=_AC){return _AB>_AC?2:0;}else{_Ax=_Az[2];_Ay=_AA[2];continue;}}}}},_AD=unCStr("Failure in Data.Map.balanceL"),_AE=new T(function(){return err(_AD);}),_AF=function(_AG,_AH,_AI,_AJ){var _AK=E(_AJ);if(!_AK[0]){var _AL=_AK[1],_AM=E(_AI);if(!_AM[0]){var _AN=_AM[1],_AO=_AM[2],_AP=_AM[3];if(_AN<=(imul(3,_AL)|0)){return [0,(1+_AN|0)+_AL|0,E(E(_AG)),_AH,E(_AM),E(_AK)];}else{var _AQ=E(_AM[4]);if(!_AQ[0]){var _AR=_AQ[1],_AS=E(_AM[5]);if(!_AS[0]){var _AT=_AS[1],_AU=_AS[2],_AV=_AS[3],_AW=_AS[4];if(_AT>=(imul(2,_AR)|0)){var _AX=function(_AY){var _AZ=E(_AS[5]);return _AZ[0]==0?[0,(1+_AN|0)+_AL|0,E(_AU),_AV,E([0,(1+_AR|0)+_AY|0,E(_AO),_AP,E(_AQ),E(_AW)]),E([0,(1+_AL|0)+_AZ[1]|0,E(E(_AG)),_AH,E(_AZ),E(_AK)])]:[0,(1+_AN|0)+_AL|0,E(_AU),_AV,E([0,(1+_AR|0)+_AY|0,E(_AO),_AP,E(_AQ),E(_AW)]),E([0,1+_AL|0,E(E(_AG)),_AH,E(_3A),E(_AK)])];},_B0=E(_AW);return _B0[0]==0?_AX(_B0[1]):_AX(0);}else{return [0,(1+_AN|0)+_AL|0,E(_AO),_AP,E(_AQ),E([0,(1+_AL|0)+_AT|0,E(E(_AG)),_AH,E(_AS),E(_AK)])];}}else{return E(_AE);}}else{return E(_AE);}}}else{return [0,1+_AL|0,E(E(_AG)),_AH,E(_3A),E(_AK)];}}else{var _B1=E(_AI);if(!_B1[0]){var _B2=_B1[1],_B3=_B1[2],_B4=_B1[3],_B5=_B1[5],_B6=E(_B1[4]);if(!_B6[0]){var _B7=_B6[1],_B8=E(_B5);if(!_B8[0]){var _B9=_B8[1],_Ba=_B8[2],_Bb=_B8[3],_Bc=_B8[4];if(_B9>=(imul(2,_B7)|0)){var _Bd=function(_Be){var _Bf=E(_B8[5]);return _Bf[0]==0?[0,1+_B2|0,E(_Ba),_Bb,E([0,(1+_B7|0)+_Be|0,E(_B3),_B4,E(_B6),E(_Bc)]),E([0,1+_Bf[1]|0,E(E(_AG)),_AH,E(_Bf),E(_3A)])]:[0,1+_B2|0,E(_Ba),_Bb,E([0,(1+_B7|0)+_Be|0,E(_B3),_B4,E(_B6),E(_Bc)]),E([0,1,E(E(_AG)),_AH,E(_3A),E(_3A)])];},_Bg=E(_Bc);return _Bg[0]==0?_Bd(_Bg[1]):_Bd(0);}else{return [0,1+_B2|0,E(_B3),_B4,E(_B6),E([0,1+_B9|0,E(E(_AG)),_AH,E(_B8),E(_3A)])];}}else{return [0,3,E(_B3),_B4,E(_B6),E([0,1,E(E(_AG)),_AH,E(_3A),E(_3A)])];}}else{var _Bh=E(_B5);return _Bh[0]==0?[0,3,E(_Bh[2]),_Bh[3],E([0,1,E(_B3),_B4,E(_3A),E(_3A)]),E([0,1,E(E(_AG)),_AH,E(_3A),E(_3A)])]:[0,2,E(E(_AG)),_AH,E(_B1),E(_3A)];}}else{return [0,1,E(E(_AG)),_AH,E(_3A),E(_3A)];}}},_Bi=unCStr("Failure in Data.Map.balanceR"),_Bj=new T(function(){return err(_Bi);}),_Bk=function(_Bl,_Bm,_Bn,_Bo){var _Bp=E(_Bn);if(!_Bp[0]){var _Bq=_Bp[1],_Br=E(_Bo);if(!_Br[0]){var _Bs=_Br[1],_Bt=_Br[2],_Bu=_Br[3];if(_Bs<=(imul(3,_Bq)|0)){return [0,(1+_Bq|0)+_Bs|0,E(E(_Bl)),_Bm,E(_Bp),E(_Br)];}else{var _Bv=E(_Br[4]);if(!_Bv[0]){var _Bw=_Bv[1],_Bx=_Bv[2],_By=_Bv[3],_Bz=_Bv[4],_BA=E(_Br[5]);if(!_BA[0]){var _BB=_BA[1];if(_Bw>=(imul(2,_BB)|0)){var _BC=function(_BD){var _BE=E(_Bl),_BF=E(_Bv[5]);return _BF[0]==0?[0,(1+_Bq|0)+_Bs|0,E(_Bx),_By,E([0,(1+_Bq|0)+_BD|0,E(_BE),_Bm,E(_Bp),E(_Bz)]),E([0,(1+_BB|0)+_BF[1]|0,E(_Bt),_Bu,E(_BF),E(_BA)])]:[0,(1+_Bq|0)+_Bs|0,E(_Bx),_By,E([0,(1+_Bq|0)+_BD|0,E(_BE),_Bm,E(_Bp),E(_Bz)]),E([0,1+_BB|0,E(_Bt),_Bu,E(_3A),E(_BA)])];},_BG=E(_Bz);return _BG[0]==0?_BC(_BG[1]):_BC(0);}else{return [0,(1+_Bq|0)+_Bs|0,E(_Bt),_Bu,E([0,(1+_Bq|0)+_Bw|0,E(E(_Bl)),_Bm,E(_Bp),E(_Bv)]),E(_BA)];}}else{return E(_Bj);}}else{return E(_Bj);}}}else{return [0,1+_Bq|0,E(E(_Bl)),_Bm,E(_Bp),E(_3A)];}}else{var _BH=E(_Bo);if(!_BH[0]){var _BI=_BH[1],_BJ=_BH[2],_BK=_BH[3],_BL=_BH[5],_BM=E(_BH[4]);if(!_BM[0]){var _BN=_BM[1],_BO=_BM[2],_BP=_BM[3],_BQ=_BM[4],_BR=E(_BL);if(!_BR[0]){var _BS=_BR[1];if(_BN>=(imul(2,_BS)|0)){var _BT=function(_BU){var _BV=E(_Bl),_BW=E(_BM[5]);return _BW[0]==0?[0,1+_BI|0,E(_BO),_BP,E([0,1+_BU|0,E(_BV),_Bm,E(_3A),E(_BQ)]),E([0,(1+_BS|0)+_BW[1]|0,E(_BJ),_BK,E(_BW),E(_BR)])]:[0,1+_BI|0,E(_BO),_BP,E([0,1+_BU|0,E(_BV),_Bm,E(_3A),E(_BQ)]),E([0,1+_BS|0,E(_BJ),_BK,E(_3A),E(_BR)])];},_BX=E(_BQ);return _BX[0]==0?_BT(_BX[1]):_BT(0);}else{return [0,1+_BI|0,E(_BJ),_BK,E([0,1+_BN|0,E(E(_Bl)),_Bm,E(_3A),E(_BM)]),E(_BR)];}}else{return [0,3,E(_BO),_BP,E([0,1,E(E(_Bl)),_Bm,E(_3A),E(_3A)]),E([0,1,E(_BJ),_BK,E(_3A),E(_3A)])];}}else{var _BY=E(_BL);return _BY[0]==0?[0,3,E(_BJ),_BK,E([0,1,E(E(_Bl)),_Bm,E(_3A),E(_3A)]),E(_BY)]:[0,2,E(E(_Bl)),_Bm,E(_3A),E(_BH)];}}else{return [0,1,E(E(_Bl)),_Bm,E(_3A),E(_3A)];}}},_BZ=function(_C0,_C1,_C2){var _C3=E(_C0),_C4=E(_C2);if(!_C4[0]){var _C5=_C4[2],_C6=_C4[3],_C7=_C4[4],_C8=_C4[5];switch(_Aw(_C3,_C5)){case 0:return _AF(_C5,_C6,_BZ(_C3,_C1,_C7),_C8);case 1:return [0,_C4[1],E(_C3),_C1,E(_C7),E(_C8)];default:return _Bk(_C5,_C6,_C7,_BZ(_C3,_C1,_C8));}}else{return [0,1,E(_C3),_C1,E(_3A),E(_3A)];}},_C9=function(_Ca,_Cb){var _Cc=hs_leWord64(_Ca,_Cb);return E(_Cc)==0?false:true;},_Cd=function(_Ce,_Cf,_Cg,_Ch){var _Ci=hs_eqWord64(_Ce,_Cg);if(!E(_Ci)){var _Cj=hs_leWord64(_Ce,_Cg);return E(_Cj)==0?false:true;}else{return _C9(_Cf,_Ch);}},_Ck=function(_Cl,_Cm){var _Cn=E(_Cl),_Co=_Cn[1],_Cp=_Cn[2],_Cq=E(_Cm),_Cr=_Cq[1],_Cs=_Cq[2],_Ct=hs_eqWord64(_Co,_Cr);if(!E(_Ct)){return !_Cd(_Co,_Cp,_Cr,_Cs)?2:0;}else{var _Cu=hs_eqWord64(_Cp,_Cs);return E(_Cu)==0?!_Cd(_Co,_Cp,_Cr,_Cs)?2:0:1;}},_Cv=function(_Cw,_Cx,_Cy,_Cz,_CA,_CB){var _CC=E(_CB);if(!_CC[0]){var _CD=_CC[2],_CE=_CC[3],_CF=_CC[4],_CG=_CC[5];switch(_Ck([0,_Cw,_Cx,_Cy,_Cz],_CD)){case 0:return _AF(_CD,_CE,_Cv(_Cw,_Cx,_Cy,_Cz,_CA,_CF),_CG);case 1:return [0,_CC[1],E([0,_Cw,_Cx,_Cy,_Cz]),_CA,E(_CF),E(_CG)];default:return _Bk(_CD,_CE,_CF,_Cv(_Cw,_Cx,_Cy,_Cz,_CA,_CG));}}else{return [0,1,E([0,_Cw,_Cx,_Cy,_Cz]),_CA,E(_3A),E(_3A)];}},_CH=function(_CI,_CJ,_CK){var _CL=E(_CI),_CM=_CL[1],_CN=_CL[2],_CO=_CL[3],_CP=_CL[4],_CQ=E(_CK);if(!_CQ[0]){var _CR=_CQ[2],_CS=_CQ[3],_CT=_CQ[4],_CU=_CQ[5];switch(_Ck(_CL,_CR)){case 0:return _AF(_CR,_CS,_Cv(_CM,_CN,_CO,_CP,_CJ,_CT),_CU);case 1:return [0,_CQ[1],E(_CL),_CJ,E(_CT),E(_CU)];default:return _Bk(_CR,_CS,_CT,_Cv(_CM,_CN,_CO,_CP,_CJ,_CU));}}else{return [0,1,E(_CL),_CJ,E(_3A),E(_3A)];}},_CV=function(_CW,_CX){while(1){var _CY=E(_CW),_CZ=E(_CX);if(!_CZ[0]){switch(_Aw(_CY,_CZ[2])){case 0:_CW=_CY;_CX=_CZ[4];continue;case 1:return [1,_CZ[3]];default:_CW=_CY;_CX=_CZ[5];continue;}}else{return [0];}}},_D0=function(_D1,_D2,_D3,_D4,_D5){while(1){var _D6=E(_D5);if(!_D6[0]){switch(_Ck([0,_D1,_D2,_D3,_D4],_D6[2])){case 0:_D5=_D6[4];continue;case 1:return [1,_D6[3]];default:_D5=_D6[5];continue;}}else{return [0];}}},_D7=function(_D8,_D9){var _Da=E(_D8),_Db=_Da[1],_Dc=_Da[2],_Dd=_Da[3],_De=_Da[4],_Df=E(_D9);if(!_Df[0]){switch(_Ck(_Da,_Df[2])){case 0:return _D0(_Db,_Dc,_Dd,_De,_Df[4]);case 1:return [1,_Df[3]];default:return _D0(_Db,_Dc,_Dd,_De,_Df[5]);}}else{return [0];}},_Dg=function(_Dh,_Di,_Dj,_Dk){var _Dl=E(_Di),_Dm=_Dl[1],_Dn=_Dl[3],_Do=new T(function(){return A(_Dk,[_ps]);}),_Dp=new T(function(){return A(_Dn,[_2r]);});return A(_Dm,[new T(function(){return A(_Dm,[_Dj,function(_Dq){return A(_Dn,[new T(function(){var _Dr=E(_Dh);return E(E(_Dq)[5]);})]);}]);}),function(_Ds){var _Dt=_D7(_Do,_Ds);return _Dt[0]==0?E(_Dp):A(_Dn,[[1,_Dt[1]]]);}]);},_Du=unCStr("containers-0.5.5.1"),_Dv=unCStr("Data.Map.Base"),_Dw=unCStr("Map"),_Dx=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Du,_Dv,_Dw],_Dy=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Dx,_T],_Dz=function(_DA){return E(_Dy);},_DB=new T(function(){return _6C(_py,_pD);}),_DC=function(_DD){var _DE=E(_DD);if(!_DE[0]){return [0];}else{var _DF=E(_DE[1]);return [1,[0,_DF[1],_DF[2]],new T(function(){return _DC(_DE[2]);})];}},_DG=function(_DH,_DI){return function(_DJ){return E(new T(function(){var _DK=A(_DH,[_6B]),_DL=E(_DK[3]),_DM=_DL[1],_DN=_DL[2],_DO=_1h(_DK[4],[1,new T(function(){return A(_DI,[_6B]);}),_T]);if(!_DO[0]){return [0,_DM,_DN,_DL,_T];}else{var _DP=_66(new T(function(){return _5U(_6i(_6t,[1,[0,_DM,_DN],new T(function(){return _DC(_DO);})]));}));return [0,_DP[1],_DP[2],_DL,_DO];}}));};},_DQ=new T(function(){return _DG(_Dz,_DB);}),_DR=new T(function(){return _6C(_DQ,_oS);}),_DS=new T(function(){return _Dg(_D,_39,_B,_DR);}),_DT=function(_DU,_){var _DV=A(_DS,[_DU,_]);return [0,[0,_2z,new T(function(){return E(E(_DV)[1]);})],new T(function(){return E(E(_DV)[2]);})];},_DW=[0,_2z,_2r],_DX=function(_DY,_){return [0,_DW,_DY];},_DZ=new T(function(){return _6C(_DQ,_oS);}),_E0=function(_E1,_){return _E1;},_E2=[1,_3A],_E3=new T(function(){return _Dg(_D,_39,_B,_DZ);}),_E4=function(_E5,_){var _E6=A(_E3,[_E5,_]);return [0,[0,_E0,new T(function(){var _E7=E(E(_E6)[1]);return _E7[0]==0?E(_E2):E(_E7);})],new T(function(){return E(E(_E6)[2]);})];},_E8=function(_E9,_){return [0,[0,_2z,[1,_E9]],_E9];},_Ea=[0,_2z,_5k],_Eb=[1,_2r],_Ec=function(_Ed,_Ee){return function(_cQ,_xX){return _4k(function(_Ef,_){var _Eg=_4k(_DT,function(_Eh){var _Ei=_CV(_Ed,_Eh);return _Ei[0]==0?E(_DX):function(_Ej,_){return [0,[0,_2z,_Ei],_Ej];};},_Ef,_),_Ek=E(_Eg),_El=E(_Ek[1]);return [0,[0,function(_Em,_){var _En=A(_El[1],[_Em,_]);return _Em;},new T(function(){var _Eo=E(_El[2]);return _Eo[0]==0?E(_Eb):[1,_Eo];})],_Ek[2]];},function(_Ep){var _Eq=new T(function(){return A(_Ee,[_Ep]);});return function(_cQ,_xX){return _4k(function(_Er,_){var _Es=A(_Eq,[_Er,_]),_Et=E(_Es),_Eu=_Et[2],_Ev=E(_Et[1]),_Ew=_Ev[1],_Ex=_Ev[2],_Ey=E(_Ep);return _Ey[0]==0?[0,[0,function(_Ez,_){var _EA=A(_Ew,[_Ez,_]);return _Ez;},_Ex],_Eu]:[0,[0,function(_EB,_){var _EC=A(_Ew,[_EB,_]);return _EB;},new T(function(){var _ED=E(_Ex);return _ED[0]==0?E(_Ey):E(_ED);})],_Eu];},function(_EE,_EF,_){return _4k(function(_yb,_){return _4k(_E4,function(_EG){var _EH=new T(function(){return _BZ(_Ed,_EE,_EG);}),_EI=new T(function(){return A(_DZ,[_EH]);});return function(_cQ,_xX){return _4k(_E8,function(_EJ){return function(_EK,_){return [0,_Ea,new T(function(){var _EL=E(_EJ);return [0,_EL[1],_EL[2],_EL[3],_EL[4],new T(function(){return _CH(_EI,_EH,_EL[5]);})];})];};},_cQ,_xX);};},_yb,_);},function(_EM,_yb,_){return (function(_EN,_){return [0,[0,_2z,[1,_EE]],_EN];})(_yb,_);},_EF,_);},_cQ,_xX);};},_cQ,_xX);};},_EO=function(_EP,_EQ){var _ER=new T(function(){return _dw(_EP,_vs);}),_ES=new T(function(){return _Ec(new T(function(){return _vo(0,_EP,_T);}),_Ak);});return function(_cQ,_xX){return _4k(_ES,function(_ET){var _EU=new T(function(){return _EO(_ER,new T(function(){return _8Q(_EQ,_ET);}));}),_EV=new T(function(){return _56(_4M,new T(function(){return _3h(0,E(_EQ)[1]+E(_ET)[1]|0,_T);}));});return function(_cQ,_xX){return _4k(function(_EW,_){return [0,[0,function(_EX,_){var _EY=A(_EV,[_EX,_]),_EZ=_5g(_EX,_);return _EX;},_5k],_EW];},function(_F0){return E(_EU);},_cQ,_xX);};},_cQ,_xX);};},_F1=new T(function(){return _EO(_Aj,_5v);}),_F2=unCStr("table"),_F3=unCStr("td"),_F4=unCStr("tr"),_F5=function(_F6,_){var _F7=E(_3M)[1],_F8=takeMVar(_F7),_F9=_A8(_F8,_),_Fa=E(_F9),_Fb=A(_v6,[_Fa[2],_]),_Fc=E(_Fb),_Fd=A(_yN,[_Fc[2],_]),_Fe=E(_Fd),_Ff=A(_yf,[_Fe[2],_]),_Fg=E(_Ff),_Fh=A(_F1,[_Fg[2],_]),_Fi=E(_Fh),_=putMVar(_F7,_Fi[2]),_Fj=_s(_F2,_F6,_),_Fk=_s(_F4,_Fj,_),_Fl=_s(_F3,_Fk,_),_Fm=A(E(_Fa[1])[1],[_Fl,_]),_Fn=_s(_F3,_Fk,_),_Fo=A(E(_Fc[1])[1],[_Fn,_]),_Fp=_s(_F3,_Fk,_),_Fq=A(_yk,[_Fp,_]),_Fr=A(E(_Fe[1])[1],[_Fp,_]),_Fs=A(_1,[_a,_Fk,_yi,_7,_]),_Ft=_s(_F4,_Fj,_),_Fu=_s(_F3,_Ft,_),_Fv=A(E(_Fg[1])[1],[_Fu,_]),_Fw=_s(_F3,_Ft,_),_Fx=A(_Ai,[_Fw,_]),_Fy=A(E(_Fi[1])[1],[_Fw,_]),_Fz=A(_1,[_a,_Ft,_yi,_7,_]),_FA=A(_yh,[_F6,_]);return _5k;},_FB=unCStr("idelem"),_FC=function(_){var _FD=E(_FB),_FE=jsFind(toJSStr(_FD)),_FF=E(_FE);return _FF[0]==0?_3O(_FD):_F5(_FF[1],_);},_FG=function(_){return _FC(_);};
var hasteMain = function() {A(_FG, [0]);};window.onload = hasteMain;