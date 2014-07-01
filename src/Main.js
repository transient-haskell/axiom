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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=function(_q,_r,_){var _s=jsCreateTextNode(toJSStr(E(_q))),_t=jsAppendChild(_s,E(_r)[1]);return [0,_s];},_u=[0,98],_v=[1,_u,_9],_w=function(_x,_y){var _z=new T(function(){return A(_x,[_y]);});return function(_A,_){var _B=jsCreateElem(toJSStr(_v)),_C=jsAppendChild(_B,E(_A)[1]),_D=[0,_B],_E=A(_z,[_D,_]);return _D;};},_F=unCStr("bottom of the page"),_G=new T(function(){return _w(_p,_F);}),_H=unCStr("text-align:center"),_I=unCStr("vertical-align:top"),_J=function(_K){return E(_K);},_L=[0,1],_M=[0,2147483647],_N=function(_O,_P){while(1){var _Q=E(_O);if(!_Q[0]){var _R=_Q[1],_S=E(_P);if(!_S[0]){var _T=_S[1],_U=addC(_R,_T);if(!E(_U[2])){return [0,_U[1]];}else{_O=[1,I_fromInt(_R)];_P=[1,I_fromInt(_T)];continue;}}else{_O=[1,I_fromInt(_R)];_P=_S;continue;}}else{var _V=E(_P);if(!_V[0]){_O=_Q;_P=[1,I_fromInt(_V[1])];continue;}else{return [1,I_add(_Q[1],_V[1])];}}}},_W=new T(function(){return _N(_M,_L);}),_X=function(_Y){var _Z=E(_Y);if(!_Z[0]){var _10=E(_Z[1]);return _10==(-2147483648)?E(_W):_10<0?[0, -_10]:E(_Z);}else{var _11=_Z[1];return I_compareInt(_11,0)>=0?E(_Z):[1,I_negate(_11)];}},_12=function(_13,_14){while(1){var _15=E(_13);if(!_15[0]){var _16=_15[1],_17=E(_14);if(!_17[0]){var _18=_17[1],_19=subC(_16,_18);if(!E(_19[2])){return [0,_19[1]];}else{_13=[1,I_fromInt(_16)];_14=[1,I_fromInt(_18)];continue;}}else{_13=[1,I_fromInt(_16)];_14=_17;continue;}}else{var _1a=E(_14);if(!_1a[0]){_13=_15;_14=[1,I_fromInt(_1a[1])];continue;}else{return [1,I_sub(_15[1],_1a[1])];}}}},_1b=new T(function(){return _N(_M,_L);}),_1c=function(_1d){var _1e=E(_1d);if(!_1e[0]){var _1f=E(_1e[1]);return _1f==(-2147483648)?E(_1b):[0, -_1f];}else{return [1,I_negate(_1e[1])];}},_1g=[0,0],_1h=[0,-1],_1i=function(_1j){var _1k=E(_1j);if(!_1k[0]){var _1l=_1k[1];return _1l>=0?E(_1l)==0?E(_1g):E(_L):E(_1h);}else{var _1m=I_compareInt(_1k[1],0);return _1m<=0?E(_1m)==0?E(_1g):E(_1h):E(_L);}},_1n=function(_1o,_1p){while(1){var _1q=E(_1o);if(!_1q[0]){var _1r=_1q[1],_1s=E(_1p);if(!_1s[0]){var _1t=_1s[1];if(!(imul(_1r,_1t)|0)){return [0,imul(_1r,_1t)|0];}else{_1o=[1,I_fromInt(_1r)];_1p=[1,I_fromInt(_1t)];continue;}}else{_1o=[1,I_fromInt(_1r)];_1p=_1s;continue;}}else{var _1u=E(_1p);if(!_1u[0]){_1o=_1q;_1p=[1,I_fromInt(_1u[1])];continue;}else{return [1,I_mul(_1q[1],_1u[1])];}}}},_1v=[0,_N,_1n,_12,_1c,_X,_1i,_J],_1w=function(_1x,_1y){var _1z=E(_1x);return _1z[0]==0?E(_1y):[1,_1z[1],new T(function(){return _1w(_1z[2],_1y);})];},_1A=function(_1B){while(1){var _1C=E(_1B);if(!_1C[0]){_1B=[1,I_fromInt(_1C[1])];continue;}else{return I_toString(_1C[1]);}}},_1D=function(_1E,_1F){return _1w(fromJSStr(_1A(_1E)),_1F);},_1G=function(_1H,_1I){var _1J=E(_1H);if(!_1J[0]){var _1K=_1J[1],_1L=E(_1I);return _1L[0]==0?_1K<_1L[1]:I_compareInt(_1L[1],_1K)>0;}else{var _1M=_1J[1],_1N=E(_1I);return _1N[0]==0?I_compareInt(_1M,_1N[1])<0:I_compare(_1M,_1N[1])<0;}},_1O=[0,41],_1P=[0,40],_1Q=[0,0],_1R=function(_1S,_1T,_1U){return _1S<=6?_1D(_1T,_1U):!_1G(_1T,_1Q)?_1D(_1T,_1U):[1,_1P,new T(function(){return _1w(fromJSStr(_1A(_1T)),[1,_1O,_1U]);})];},_1V=function(_1W){return _1R(0,_1W,_9);},_1X=[0,44],_1Y=[0,93],_1Z=[0,91],_20=function(_21,_22){var _23=E(_21);return _23[0]==0?unAppCStr("[]",_22):[1,_1Z,new T(function(){return _1R(0,_23[1],new T(function(){var _24=function(_25){var _26=E(_25);return _26[0]==0?E([1,_1Y,_22]):[1,_1X,new T(function(){return _1R(0,_26[1],new T(function(){return _24(_26[2]);}));})];};return _24(_23[2]);}));})];},_27=function(_28,_29,_2a){return _1R(E(_28)[1],_29,_2a);},_2b=[0,_27,_1V,_20],_2c=[13,coercionToken],_2d=function(_2e){return E(E(_2e)[1]);},_2f=unCStr("br"),_2g=function(_2h,_){var _2i=jsCreateElem(toJSStr(E(_2f))),_2j=jsAppendChild(_2i,E(_2h)[1]);return [0,_2i];},_2k=[0,1],_2l=unCStr(" A counter "),_2m=[0,112],_2n=[1,_2m,_9],_2o=function(_2p,_2q){var _2r=new T(function(){return A(_2p,[_2q]);});return function(_2s,_){var _2t=jsCreateElem(toJSStr(_2n)),_2u=jsAppendChild(_2t,E(_2s)[1]),_2v=[0,_2t],_2w=A(_2r,[_2v,_]);return _2v;};},_2x=new T(function(){return _2o(_p,_2l);}),_2y=[0,43],_2z=[1,_2y,_9],_2A=[1,_2z],_2B=function(_2C,_2D,_){var _2E=jsCreateElem(toJSStr(E(_2C))),_2F=jsAppendChild(_2E,E(_2D)[1]);return [0,_2E];},_2G=function(_2H,_2I,_2J,_){var _2K=_2B(_2H,_2J,_),_2L=A(_2I,[_2K,_]);return _2K;},_2M=unCStr("()"),_2N=unCStr("GHC.Tuple"),_2O=unCStr("ghc-prim"),_2P=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_2O,_2N,_2M],_2Q=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_2P,_9],_2R=function(_2S){return E(_2Q);},_2T=unCStr("main"),_2U=unCStr("Haste.Perch"),_2V=unCStr("PerchM"),_2W=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_2T,_2U,_2V],_2X=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_2W,_9],_2Y=function(_2Z){return E(_2X);},_30=function(_31){var _32=E(_31);return _32[0]==0?[0]:_1w(_32[1],new T(function(){return _30(_32[2]);}));},_33=function(_34,_35){var _36=E(_34);if(!_36){return [0,_9,_35];}else{var _37=E(_35);if(!_37[0]){return [0,_9,_9];}else{var _38=new T(function(){var _39=_33(_36-1|0,_37[2]);return [0,_39[1],_39[2]];});return [0,[1,_37[1],new T(function(){return E(E(_38)[1]);})],new T(function(){return E(E(_38)[2]);})];}}},_3a=[0,120],_3b=[0,48],_3c=function(_3d){var _3e=new T(function(){var _3f=_33(8,new T(function(){var _3g=md5(toJSStr(E(_3d)));return fromJSStr(_3g);}));return [0,_3f[1],_3f[2]];}),_3h=parseInt([0,toJSStr([1,_3b,[1,_3a,new T(function(){return E(E(_3e)[1]);})]])]),_3i=new T(function(){var _3j=_33(8,new T(function(){return E(E(_3e)[2]);}));return [0,_3j[1],_3j[2]];}),_3k=parseInt([0,toJSStr([1,_3b,[1,_3a,new T(function(){return E(E(_3i)[1]);})]])]),_3l=hs_mkWord64(_3h,_3k),_3m=parseInt([0,toJSStr([1,_3b,[1,_3a,new T(function(){return E(_33(8,new T(function(){return E(E(_3i)[2]);}))[1]);})]])]),_3n=hs_mkWord64(_3m,_3m);return [0,_3l,_3n];},_3o=function(_3p,_3q){var _3r=E(_3q);return _3r[0]==0?[0]:[1,new T(function(){return A(_3p,[_3r[1]]);}),new T(function(){return _3o(_3p,_3r[2]);})];},_3s=function(_3t,_3u){var _3v=jsShowI(_3t),_3w=md5(_3v);return _1w(fromJSStr(_3w),new T(function(){var _3x=jsShowI(_3u),_3y=md5(_3x);return fromJSStr(_3y);}));},_3z=function(_3A){var _3B=E(_3A);return _3s(_3B[1],_3B[2]);},_3C=function(_3D){var _3E=E(_3D);if(!_3E[0]){return [0];}else{var _3F=E(_3E[1]);return [1,[0,_3F[1],_3F[2]],new T(function(){return _3C(_3E[2]);})];}},_3G=unCStr("Prelude.undefined"),_3H=new T(function(){return err(_3G);}),_3I=function(_3J,_3K){return function(_3L){return E(new T(function(){var _3M=A(_3J,[_3H]),_3N=E(_3M[3]),_3O=_3N[1],_3P=_3N[2],_3Q=_1w(_3M[4],[1,new T(function(){return A(_3K,[_3H]);}),_9]);if(!_3Q[0]){return [0,_3O,_3P,_3N,_9];}else{var _3R=_3c(new T(function(){return _30(_3o(_3z,[1,[0,_3O,_3P],new T(function(){return _3C(_3Q);})]));}));return [0,_3R[1],_3R[2],_3N,_3Q];}}));};},_3S=new T(function(){return _3I(_2Y,_2R);}),_3T=function(_3U,_3V,_3W,_){var _3X=E(_3V),_3Y=A(_3U,[_3W,_]),_3Z=A(_1,[_n,_3Y,_3X[1],_3X[2],_]);return _3Y;},_40=function(_41,_42){while(1){var _43=(function(_44,_45){var _46=E(_45);if(!_46[0]){return E(_44);}else{_41=function(_47,_){return _3T(_44,_46[1],_47,_);};_42=_46[2];return null;}})(_41,_42);if(_43!=null){return _43;}}},_48=unCStr("value"),_49=unCStr("id"),_4a=unCStr("onclick"),_4b=unCStr("checked"),_4c=[0,_4b,_9],_4d=[1,_4c,_9],_4e=unCStr("type"),_4f=unCStr("input"),_4g=function(_4h,_){return _2B(_4f,_4h,_);},_4i=function(_4j,_4k,_4l,_4m,_4n){var _4o=new T(function(){var _4p=new T(function(){return _40(_4g,[1,[0,_4e,_4k],[1,[0,_49,_4j],[1,[0,_48,_4l],_9]]]);});return !E(_4m)?E(_4p):_40(_4p,_4d);}),_4q=E(_4n);return _4q[0]==0?E(_4o):_40(_4o,[1,[0,_4a,_4q[1]],_9]);},_4r=unCStr("href"),_4s=[0,97],_4t=[1,_4s,_9],_4u=function(_4v,_){return _2B(_4t,_4v,_);},_4w=function(_4x,_4y){var _4z=new T(function(){return _40(_4u,[1,[0,_4r,_4x],_9]);});return function(_4A,_){var _4B=A(_4z,[_4A,_]),_4C=A(_4y,[_4B,_]);return _4B;};},_4D=function(_4E){return _4w(_4E,function(_47,_){return _p(_4E,_47,_);});},_4F=unCStr("option"),_4G=function(_4H,_){return _2B(_4F,_4H,_);},_4I=unCStr("selected"),_4J=[0,_4I,_9],_4K=[1,_4J,_9],_4L=function(_4M,_4N,_4O){var _4P=new T(function(){return _40(_4G,[1,[0,_48,_4M],_9]);}),_4Q=function(_4R,_){var _4S=A(_4P,[_4R,_]),_4T=A(_4N,[_4S,_]);return _4S;};return !E(_4O)?E(_4Q):_40(_4Q,_4K);},_4U=function(_4V,_4W){return _4L(_4V,function(_47,_){return _p(_4V,_47,_);},_4W);},_4X=unCStr("method"),_4Y=unCStr("action"),_4Z=unCStr("UTF-8"),_50=unCStr("acceptCharset"),_51=[0,_50,_4Z],_52=unCStr("form"),_53=function(_54,_){return _2B(_52,_54,_);},_55=function(_56,_57,_58){var _59=new T(function(){return _40(_53,[1,_51,[1,[0,_4Y,_56],[1,[0,_4X,_57],_9]]]);});return function(_5a,_){var _5b=A(_59,[_5a,_]),_5c=A(_58,[_5b,_]);return _5b;};},_5d=unCStr("select"),_5e=function(_5f,_){return _2B(_5d,_5f,_);},_5g=function(_5h,_5i){var _5j=new T(function(){return _40(_5e,[1,[0,_49,_5h],_9]);});return function(_5k,_){var _5l=A(_5j,[_5k,_]),_5m=A(_5i,[_5l,_]);return _5l;};},_5n=unCStr("textarea"),_5o=function(_5p,_){return _2B(_5n,_5p,_);},_5q=function(_5r,_5s){var _5t=new T(function(){return _40(_5o,[1,[0,_49,_5r],_9]);});return function(_5u,_){var _5v=A(_5t,[_5u,_]),_5w=_p(_5s,_5v,_);return _5v;};},_5x=unCStr("color:red"),_5y=unCStr("style"),_5z=[0,_5y,_5x],_5A=[1,_5z,_9],_5B=[0,98],_5C=[1,_5B,_9],_5D=function(_5E){return _40(function(_5F,_){var _5G=_2B(_5C,_5F,_),_5H=A(_5E,[_5G,_]);return _5G;},_5A);},_5I=unCStr("toByteString not defined"),_5J=new T(function(){return err(_5I);}),_5K=function(_5L,_5M,_){var _5N=E(_5L);if(!_5N[0]){return _5M;}else{var _5O=A(_5N[1],[_5M,_]),_5P=_5K(_5N[2],_5M,_);return _5M;}},_5Q=function(_5R,_5S,_5T,_){var _5U=A(_5R,[_5T,_]),_5V=A(_5S,[_5T,_]);return _5T;},_5W=function(_5X,_){return _5X;},_5Y=[0,_5W,_5Q,_5K],_5Z=[0,_5Y,_3S,_5J,_p,_p,_2G,_5D,_4w,_4D,_4i,_5q,_5g,_4L,_4U,_55,_40],_60=function(_61,_62,_){var _63=A(_61,[_]);return A(_62,[_]);},_64=function(_65,_66,_){return _60(_65,_66,_);},_67=function(_68,_69,_){var _6a=A(_68,[_]);return A(_69,[_6a,_]);},_6b=unCStr("base"),_6c=unCStr("GHC.IO.Exception"),_6d=unCStr("IOException"),_6e=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_6b,_6c,_6d],_6f=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_6e,_9],_6g=function(_6h){return E(_6f);},_6i=function(_6j){return E(E(_6j)[1]);},_6k=unCStr("Maybe.fromJust: Nothing"),_6l=new T(function(){return err(_6k);}),_6m=function(_6n,_6o,_6p){var _6q=new T(function(){var _6r=A(_6n,[_6p]),_6s=A(_6o,[new T(function(){var _6t=E(_6q);return _6t[0]==0?E(_6l):E(_6t[1]);})]),_6u=hs_eqWord64(_6r[1],_6s[1]);if(!E(_6u)){return [0];}else{var _6v=hs_eqWord64(_6r[2],_6s[2]);return E(_6v)==0?[0]:[1,_6p];}});return E(_6q);},_6w=function(_6x){var _6y=E(_6x);return _6m(_6i(_6y[1]),_6g,_6y[2]);},_6z=unCStr(": "),_6A=[0,41],_6B=unCStr(" ("),_6C=unCStr("already exists"),_6D=unCStr("does not exist"),_6E=unCStr("protocol error"),_6F=unCStr("failed"),_6G=unCStr("invalid argument"),_6H=unCStr("inappropriate type"),_6I=unCStr("hardware fault"),_6J=unCStr("unsupported operation"),_6K=unCStr("timeout"),_6L=unCStr("resource vanished"),_6M=unCStr("interrupted"),_6N=unCStr("resource busy"),_6O=unCStr("resource exhausted"),_6P=unCStr("end of file"),_6Q=unCStr("illegal operation"),_6R=unCStr("permission denied"),_6S=unCStr("user error"),_6T=unCStr("unsatisified constraints"),_6U=unCStr("system error"),_6V=function(_6W,_6X){switch(E(_6W)){case 0:return _1w(_6C,_6X);case 1:return _1w(_6D,_6X);case 2:return _1w(_6N,_6X);case 3:return _1w(_6O,_6X);case 4:return _1w(_6P,_6X);case 5:return _1w(_6Q,_6X);case 6:return _1w(_6R,_6X);case 7:return _1w(_6S,_6X);case 8:return _1w(_6T,_6X);case 9:return _1w(_6U,_6X);case 10:return _1w(_6E,_6X);case 11:return _1w(_6F,_6X);case 12:return _1w(_6G,_6X);case 13:return _1w(_6H,_6X);case 14:return _1w(_6I,_6X);case 15:return _1w(_6J,_6X);case 16:return _1w(_6K,_6X);case 17:return _1w(_6L,_6X);default:return _1w(_6M,_6X);}},_6Y=[0,125],_6Z=unCStr("{handle: "),_70=function(_71,_72,_73,_74,_75,_76){var _77=new T(function(){var _78=new T(function(){return _6V(_72,new T(function(){var _79=E(_74);return _79[0]==0?E(_76):_1w(_6B,new T(function(){return _1w(_79,[1,_6A,_76]);}));}));}),_7a=E(_73);return _7a[0]==0?E(_78):_1w(_7a,new T(function(){return _1w(_6z,_78);}));}),_7b=E(_75);if(!_7b[0]){var _7c=E(_71);if(!_7c[0]){return E(_77);}else{var _7d=E(_7c[1]);return _7d[0]==0?_1w(_6Z,new T(function(){return _1w(_7d[1],[1,_6Y,new T(function(){return _1w(_6z,_77);})]);})):_1w(_6Z,new T(function(){return _1w(_7d[1],[1,_6Y,new T(function(){return _1w(_6z,_77);})]);}));}}else{return _1w(_7b[1],new T(function(){return _1w(_6z,_77);}));}},_7e=function(_7f){var _7g=E(_7f);return _70(_7g[1],_7g[2],_7g[3],_7g[4],_7g[6],_9);},_7h=function(_7i,_7j){var _7k=E(_7i);return _70(_7k[1],_7k[2],_7k[3],_7k[4],_7k[6],_7j);},_7l=function(_7m,_7n,_7o){var _7p=E(_7n);return _7p[0]==0?unAppCStr("[]",_7o):[1,_1Z,new T(function(){return A(_7m,[_7p[1],new T(function(){var _7q=function(_7r){var _7s=E(_7r);return _7s[0]==0?E([1,_1Y,_7o]):[1,_1X,new T(function(){return A(_7m,[_7s[1],new T(function(){return _7q(_7s[2]);})]);})];};return _7q(_7p[2]);})]);})];},_7t=function(_7u,_7v){return _7l(_7h,_7u,_7v);},_7w=function(_7x,_7y,_7z){var _7A=E(_7y);return _70(_7A[1],_7A[2],_7A[3],_7A[4],_7A[6],_7z);},_7B=[0,_7w,_7e,_7t],_7C=new T(function(){return [0,_6g,_7B,_7D,_6w];}),_7D=function(_7E){return [0,_7C,_7E];},_7F=7,_7G=function(_7H){return [0,_a,_7F,_9,_7H,_a,_a];},_7I=function(_7J,_){return die(new T(function(){return _7D(new T(function(){return _7G(_7J);}));}));},_7K=function(_7L,_){return _7I(_7L,_);},_7M=[0,_67,_64,_5W,_7K],_7N=[0,_7M,_n],_7O=function(_7P){return E(E(_7P)[1]);},_7Q=function(_7R){return E(E(_7R)[1]);},_7S=function(_7T){return E(E(_7T)[2]);},_7U=function(_7V){return E(E(_7V)[3]);},_7W=function(_7X,_7Y){var _7Z=new T(function(){return A(_7S,[_7X,_7Y]);}),_80=new T(function(){return _7O(_7X);}),_81=new T(function(){return _7U(_80);}),_82=new T(function(){return _7Q(_80);});return function(_83){return A(_82,[_7Z,function(_84){return A(_81,[[0,_84,_83]]);}]);};},_85=function(_86,_87){return [0,_86,function(_88){return _7W(_87,_88);}];},_89=function(_8a,_8b){return A(_7U,[_8a,[0,_8b,_8b]]);},_8c=function(_8d,_8e,_8f){return A(_7U,[_8d,[0,_0,_8e]]);},_8g=function(_8h,_8i){return [0,_8h,function(_8j){return _89(_8i,_8j);},function(_8k,_8l){return _8c(_8i,_8k,_8l);}];},_8m=function(_8n,_8o,_8p,_8q){return A(_7Q,[_8n,new T(function(){return A(_8o,[_8q]);}),function(_8r){return A(_8p,[new T(function(){return E(E(_8r)[1]);}),new T(function(){return E(E(_8r)[2]);})]);}]);},_8s=function(_8t,_8u,_8v,_8w){return A(_7Q,[_8t,new T(function(){return A(_8u,[_8w]);}),function(_8x){return A(_8v,[new T(function(){return E(E(_8x)[2]);})]);}]);},_8y=function(_8z,_8A,_8B,_8C){return _8s(_8z,_8A,_8B,_8C);},_8D=function(_8E){return E(E(_8E)[4]);},_8F=function(_8G,_8H){var _8I=new T(function(){return A(_8D,[_8G,_8H]);});return function(_8J){return E(_8I);};},_8K=function(_8L){var _8M=new T(function(){return _7U(_8L);});return [0,function(_8A,_8B,_8C){return _8m(_8L,_8A,_8B,_8C);},function(_8A,_8B,_8C){return _8y(_8L,_8A,_8B,_8C);},function(_8N,_8O){return A(_8M,[[0,_8N,_8O]]);},function(_8C){return _8F(_8L,_8C);}];},_8P=function(_8Q){return E(E(_8Q)[1]);},_8R=[0,112],_8S=function(_8T,_8U){var _8V=jsShowI(_8T);return _1w(fromJSStr(_8V),_8U);},_8W=function(_8X,_8Y,_8Z){return _8Y>=0?_8S(_8Y,_8Z):_8X<=6?_8S(_8Y,_8Z):[1,_1P,new T(function(){var _90=jsShowI(_8Y);return _1w(fromJSStr(_90),[1,_1O,_8Z]);})];},_91=function(_92,_93,_94,_95){var _96=E(_93);return A(_96[1],[new T(function(){var _97=E(_92);return E(_94);}),function(_98){var _99=new T(function(){return E(E(_98)[2]);});return A(_96[2],[new T(function(){return A(_95,[new T(function(){var _9a=E(new T(function(){var _9b=E(_92);return [0,coercionToken];})),_9c=E(_98);return [0,_9c[1],new T(function(){return [0,E(_99)[1]+1|0];}),_9c[3],_9c[4],_9c[5]];})]);}),new T(function(){return A(_96[3],[[1,_8R,new T(function(){return _1w(_8W(0,E(_99)[1],_9),new T(function(){return E(E(_98)[1]);}));})]]);})]);}]);},_9d=function(_9e,_9f){return A(_9e,[function(_){return jsFind(toJSStr(E(_9f)));}]);},_9g=function(_9h){return E(E(_9h)[4]);},_9i=unCStr("GHC.Types"),_9j=unCStr("[]"),_9k=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_2O,_9i,_9j],_9l=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_9k,_9],_9m=function(_9n){return E(_9l);},_9o=unCStr("Char"),_9p=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_2O,_9i,_9o],_9q=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_9p,_9],_9r=function(_9s){return E(_9q);},_9t=new T(function(){return _3I(_9m,_9r);}),_9u=new T(function(){return A(_9t,[_3H]);}),_9v=new T(function(){return E(_3H);}),_9w=function(_9x){return E(E(_9x)[7]);},_9y=function(_9z){return E(E(_9z)[1]);},_9A=[0,0],_9B=[0,32],_9C=[0,10],_9D=function(_9E){var _9F=E(_9E);if(!_9F[0]){return E(_n);}else{var _9G=_9F[1],_9H=E(_9F[2]);if(!_9H[0]){return _9I(_9C,_9G);}else{var _9J=new T(function(){return _9D(_9H);}),_9K=new T(function(){return _9I(_9C,_9G);});return function(_9L){return A(_9K,[[1,_9B,new T(function(){return A(_9J,[_9L]);})]]);};}}},_9M=unCStr("->"),_9N=[1,_9M,_9],_9O=[1,_9i,_9N],_9P=[1,_2O,_9O],_9Q=[0,32],_9R=function(_9S){var _9T=E(_9S);if(!_9T[0]){return [0];}else{var _9U=_9T[1],_9V=E(_9T[2]);return _9V[0]==0?E(_9U):_1w(_9U,[1,_9Q,new T(function(){return _9R(_9V);})]);}},_9W=new T(function(){return _9R(_9P);}),_9X=new T(function(){var _9Y=_3c(_9W);return [0,_9Y[1],_9Y[2],_2O,_9i,_9M];}),_9Z=function(_a0,_a1){var _a2=E(_a0);return _a2[0]==0?E(_a1):A(_a2[1],[new T(function(){return _9Z(_a2[2],_a1);})]);},_a3=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_a4=[1,_2Q,_9],_a5=function(_a6){var _a7=E(_a6);if(!_a7[0]){return [0];}else{var _a8=E(_a7[1]);return [1,[0,_a8[1],_a8[2]],new T(function(){return _a5(_a7[2]);})];}},_a9=new T(function(){var _aa=_1w(_9,_a4);if(!_aa[0]){return E(_9k);}else{var _ab=_3c(new T(function(){return _30(_3o(_3z,[1,_a3,new T(function(){return _a5(_aa);})]));}));return E(_9k);}}),_ac=[0,40],_ad=function(_ae){return _9I(_9C,_ae);},_af=[0,8],_ag=unCStr(" -> "),_ah=[0,9],_ai=[0,93],_aj=[0,91],_ak=[0,41],_al=[0,44],_am=function(_ae){return [1,_al,_ae];},_an=function(_ao,_ap){var _aq=E(_ap);return _aq[0]==0?[0]:[1,_ao,[1,_aq[1],new T(function(){return _an(_ao,_aq[2]);})]];},_9I=function(_ar,_as){var _at=E(_as),_au=_at[3],_av=E(_at[4]);if(!_av[0]){return function(_aw){return _1w(E(_au)[5],_aw);};}else{var _ax=_av[1],_ay=new T(function(){var _az=E(_au)[5],_aA=new T(function(){return _9D(_av);}),_aB=new T(function(){return E(_ar)[1]<=9?function(_aC){return _1w(_az,[1,_9B,new T(function(){return A(_aA,[_aC]);})]);}:function(_aD){return [1,_1P,new T(function(){return _1w(_az,[1,_9B,new T(function(){return A(_aA,[[1,_1O,_aD]]);})]);})];};}),_aE=E(_az);if(!_aE[0]){return E(_aB);}else{if(E(E(_aE[1])[1])==40){var _aF=E(_aE[2]);return _aF[0]==0?E(_aB):E(E(_aF[1])[1])==44?function(_aG){return [1,_ac,new T(function(){return A(new T(function(){var _aH=_3o(_ad,_av);if(!_aH[0]){return E(_n);}else{var _aI=new T(function(){return _an(_am,_aH[2]);});return function(_aJ){return _9Z([1,_aH[1],_aI],_aJ);};}}),[[1,_ak,_aG]]);})];}:E(_aB);}else{return E(_aB);}}}),_aK=E(_av[2]);if(!_aK[0]){var _aL=E(_au),_aM=E(_a9),_aN=hs_eqWord64(_aL[1],_aM[1]);if(!E(_aN)){return E(_ay);}else{var _aO=hs_eqWord64(_aL[2],_aM[2]);if(!E(_aO)){return E(_ay);}else{var _aP=new T(function(){return _9I(_9A,_ax);});return function(_aQ){return [1,_aj,new T(function(){return A(_aP,[[1,_ai,_aQ]]);})];};}}}else{if(!E(_aK[2])[0]){var _aR=E(_au),_aS=E(_9X),_aT=hs_eqWord64(_aR[1],_aS[1]);if(!E(_aT)){return E(_ay);}else{var _aU=hs_eqWord64(_aR[2],_aS[2]);if(!E(_aU)){return E(_ay);}else{var _aV=new T(function(){return _9I(_af,_aK[1]);}),_aW=new T(function(){return _9I(_ah,_ax);});return E(_ar)[1]<=8?function(_aX){return A(_aW,[new T(function(){return _1w(_ag,new T(function(){return A(_aV,[_aX]);}));})]);}:function(_aY){return [1,_1P,new T(function(){return A(_aW,[new T(function(){return _1w(_ag,new T(function(){return A(_aV,[[1,_1O,_aY]]);}));})]);})];};}}}else{return E(_ay);}}}},_aZ=function(_b0,_b1,_b2,_b3){var _b4=new T(function(){return _7U(_b0);}),_b5=new T(function(){return _9g(_b3);}),_b6=new T(function(){return _9w(_b3);}),_b7=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_9I,[_9A,A(_b1,[_9v]),_9]);}));}),_b8=new T(function(){return A(_9y,[_b2,_f]);});return function(_b9){if(!E(new T(function(){var _ba=A(_b1,[_9v]),_bb=E(_9u),_bc=hs_eqWord64(_ba[1],_bb[1]);if(!E(_bc)){return false;}else{var _bd=hs_eqWord64(_ba[2],_bb[2]);return E(_bd)==0?false:true;}}))){var _be=new T(function(){return A(_b4,[[1,_b9,new T(function(){return A(_b6,[new T(function(){return A(_b5,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1w(_b9,_b7);}));})]);})]);})]]);}),_bf=A(_b8,[_b9]);if(!_bf[0]){return E(_be);}else{var _bg=E(_bf[1]);return E(_bg[2])[0]==0?E(_bf[2])[0]==0?A(_b4,[[2,_bg[1]]]):E(_be):E(_be);}}else{return A(_b4,[[2,_b9]]);}};},_bh=[0],_bi=new T(function(){return [0,"value"];}),_bj=function(_bk,_bl,_bm,_bn,_bo,_bp){var _bq=E(_bk),_br=_bq[1],_bs=new T(function(){return A(_bq[3],[_bh]);}),_bt=new T(function(){return _aZ(_bq,_bm,_bn,_bo);});return A(_br,[new T(function(){return _9d(_bl,_bp);}),function(_bu){var _bv=E(_bu);return _bv[0]==0?E(_bs):A(_br,[new T(function(){return A(_bl,[function(_){var _bw=jsGet(E(_bv[1])[1],E(_bi)[1]);return [1,new T(function(){return fromJSStr(_bw);})];}]);}),function(_bx){var _by=E(_bx);return _by[0]==0?E(_bs):A(_bt,[_by[1]]);}]);}]);},_bz=false,_bA=1,_bB=function(_bC){return E(E(_bC)[10]);},_bD=function(_bE){return E(E(_bE)[2]);},_bF=function(_bG){return E(E(_bG)[2]);},_bH=function(_bI){return E(E(_bI)[3]);},_bJ=function(_bK){return E(E(_bK)[2]);},_bL=function(_bM,_bN,_bO,_bP,_bQ,_bR,_bS,_bT,_bU,_bV,_bW,_bX){var _bY=_7O(_bR),_bZ=_bY[1],_c0=_bY[3],_c1=new T(function(){return _8P(_bT);}),_c2=new T(function(){return _bF(_c1);}),_c3=new T(function(){return _bH(_bS);}),_c4=new T(function(){return _bJ(_bN);}),_c5=new T(function(){return _bB(_bT);});return A(_bZ,[new T(function(){var _c6=E(_bV);if(!_c6[0]){var _c7=E(_bS);return _91(_bU,_c7[1],_c7[2],_c7[3]);}else{return A(_c0,[_c6[1]]);}}),function(_c8){return A(_bZ,[new T(function(){var _c9=E(_bU);return _bD(_bS);}),function(_ca){return A(_bY[2],[new T(function(){return A(_c3,[new T(function(){var _cb=E(new T(function(){var _cc=E(_bU);return [0,coercionToken];})),_cd=E(_ca);return [0,_cd[1],_cd[2],_bA,_cd[4],_cd[5]];})]);}),new T(function(){var _ce=new T(function(){return A(_c0,[[0,new T(function(){return A(_c5,[_c8,_bW,new T(function(){var _cf=E(_bX);if(!_cf[0]){return [0];}else{var _cg=_cf[1],_ch=_6m(_bQ,_9t,_cg);return _ch[0]==0?A(_bJ,[_bO,_cg]):E(_ch[1]);}}),_bz,_a]);}),_a]]);});return A(_bZ,[new T(function(){var _ci=E(_bR);return _bj(_ci[1],_ci[2],_bP,_bM,_bT,_c8);}),function(_cj){var _ck=E(_cj);switch(_ck[0]){case 0:return E(_ce);case 1:return A(_c0,[[0,new T(function(){return A(_c2,[new T(function(){return A(_c5,[_c8,_bW,_ck[1],_bz,_a]);}),_ck[2]]);}),_a]]);default:var _cl=_ck[1];return A(_c0,[[0,new T(function(){return A(_c5,[_c8,_bW,new T(function(){var _cm=_6m(_bP,_9t,_cl);return _cm[0]==0?A(_c4,[_cl]):E(_cm[1]);}),_bz,_a]);}),[1,_cl]]]);}}]);})]);}]);}]);},_cn=function(_co,_cp,_cq,_cr,_cs){var _ct=new T(function(){return _7O(_cp);}),_cu=new T(function(){return _8K(_ct);}),_cv=new T(function(){return _8g(_cu,_ct);}),_cw=new T(function(){return _85(_cu,_cp);});return function(_aJ,_cx,_cy){return _bL(_cs,_cr,_cr,_cq,_cq,_cw,_cv,_co,[0,coercionToken],_aJ,_cx,_cy);};},_cz=unCStr("base"),_cA=unCStr("Control.Exception.Base"),_cB=unCStr("PatternMatchFail"),_cC=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_cz,_cA,_cB],_cD=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_cC,_9],_cE=function(_cF){return E(_cD);},_cG=function(_cH){var _cI=E(_cH);return _6m(_6i(_cI[1]),_cE,_cI[2]);},_cJ=function(_cK){return E(E(_cK)[1]);},_cL=function(_cM,_cN){return _1w(E(_cM)[1],_cN);},_cO=function(_cP,_cQ){return _7l(_cL,_cP,_cQ);},_cR=function(_cS,_cT,_cU){return _1w(E(_cT)[1],_cU);},_cV=[0,_cR,_cJ,_cO],_cW=new T(function(){return [0,_cE,_cV,_cX,_cG];}),_cX=function(_cY){return [0,_cW,_cY];},_cZ=unCStr("Non-exhaustive patterns in"),_d0=function(_d1,_d2){return die(new T(function(){return A(_d2,[_d1]);}));},_d3=function(_d4,_d5){var _d6=E(_d5);if(!_d6[0]){return [0,_9,_9];}else{var _d7=_d6[1];if(!A(_d4,[_d7])){return [0,_9,_d6];}else{var _d8=new T(function(){var _d9=_d3(_d4,_d6[2]);return [0,_d9[1],_d9[2]];});return [0,[1,_d7,new T(function(){return E(E(_d8)[1]);})],new T(function(){return E(E(_d8)[2]);})];}}},_da=[0,32],_db=[0,10],_dc=[1,_db,_9],_dd=function(_de){return E(E(_de)[1])==124?false:true;},_df=function(_dg,_dh){var _di=_d3(_dd,unCStr(_dg)),_dj=_di[1],_dk=function(_dl,_dm){return _1w(_dl,new T(function(){return unAppCStr(": ",new T(function(){return _1w(_dh,new T(function(){return _1w(_dm,_dc);}));}));}));},_dn=E(_di[2]);return _dn[0]==0?_dk(_dj,_9):E(E(_dn[1])[1])==124?_dk(_dj,[1,_da,_dn[2]]):_dk(_dj,_9);},_do=function(_dp){return _d0([0,new T(function(){return _df(_dp,_cZ);})],_cX);},_dq=new T(function(){return _do("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_dr=function(_ds,_dt){while(1){var _du=(function(_dv,_dw){var _dx=E(_dv);switch(_dx[0]){case 0:var _dy=E(_dw);if(!_dy[0]){return [0];}else{_ds=A(_dx[1],[_dy[1]]);_dt=_dy[2];return null;}break;case 1:var _dz=A(_dx[1],[_dw]),_dA=_dw;_ds=_dz;_dt=_dA;return null;case 2:return [0];case 3:return [1,[0,_dx[1],_dw],new T(function(){return _dr(_dx[2],_dw);})];default:return E(_dx[1]);}})(_ds,_dt);if(_du!=null){return _du;}}},_dB=function(_dC,_dD){var _dE=new T(function(){var _dF=E(_dD);if(_dF[0]==3){return [3,_dF[1],new T(function(){return _dB(_dC,_dF[2]);})];}else{var _dG=E(_dC);if(_dG[0]==2){return E(_dF);}else{var _dH=E(_dF);if(_dH[0]==2){return E(_dG);}else{var _dI=new T(function(){var _dJ=E(_dH);if(_dJ[0]==4){return [1,function(_dK){return [4,new T(function(){return _1w(_dr(_dG,_dK),_dJ[1]);})];}];}else{var _dL=E(_dG);if(_dL[0]==1){var _dM=_dL[1],_dN=E(_dJ);return _dN[0]==0?[1,function(_dO){return _dB(A(_dM,[_dO]),_dN);}]:[1,function(_dP){return _dB(A(_dM,[_dP]),new T(function(){return A(_dN[1],[_dP]);}));}];}else{var _dQ=E(_dJ);return _dQ[0]==0?E(_dq):[1,function(_dR){return _dB(_dL,new T(function(){return A(_dQ[1],[_dR]);}));}];}}}),_dS=E(_dG);switch(_dS[0]){case 1:var _dT=E(_dH);return _dT[0]==4?[1,function(_dU){return [4,new T(function(){return _1w(_dr(A(_dS[1],[_dU]),_dU),_dT[1]);})];}]:E(_dI);case 4:var _dV=_dS[1],_dW=E(_dH);switch(_dW[0]){case 0:return [1,function(_dX){return [4,new T(function(){return _1w(_dV,new T(function(){return _dr(_dW,_dX);}));})];}];case 1:return [1,function(_dY){return [4,new T(function(){return _1w(_dV,new T(function(){return _dr(A(_dW[1],[_dY]),_dY);}));})];}];default:return [4,new T(function(){return _1w(_dV,_dW[1]);})];}break;default:return E(_dI);}}}}}),_dZ=E(_dC);switch(_dZ[0]){case 0:var _e0=E(_dD);return _e0[0]==0?[0,function(_e1){return _dB(A(_dZ[1],[_e1]),new T(function(){return A(_e0[1],[_e1]);}));}]:E(_dE);case 3:return [3,_dZ[1],new T(function(){return _dB(_dZ[2],_dD);})];default:return E(_dE);}},_e2=function(_e3,_e4){return E(_e3)[1]!=E(_e4)[1];},_e5=function(_e6,_e7){return E(_e6)[1]==E(_e7)[1];},_e8=[0,_e5,_e2],_e9=function(_ea){return E(E(_ea)[1]);},_eb=function(_ec,_ed,_ee){while(1){var _ef=E(_ed);if(!_ef[0]){return E(_ee)[0]==0?true:false;}else{var _eg=E(_ee);if(!_eg[0]){return false;}else{if(!A(_e9,[_ec,_ef[1],_eg[1]])){return false;}else{_ed=_ef[2];_ee=_eg[2];continue;}}}}},_eh=function(_ei,_ej,_ek){return !_eb(_ei,_ej,_ek)?true:false;},_el=function(_em){return [0,function(_en,_eo){return _eb(_em,_en,_eo);},function(_en,_eo){return _eh(_em,_en,_eo);}];},_ep=new T(function(){return _el(_e8);}),_eq=function(_er,_es){var _et=E(_er);switch(_et[0]){case 0:return [0,function(_eu){return _eq(A(_et[1],[_eu]),_es);}];case 1:return [1,function(_ev){return _eq(A(_et[1],[_ev]),_es);}];case 2:return [2];case 3:return _dB(A(_es,[_et[1]]),new T(function(){return _eq(_et[2],_es);}));default:var _ew=function(_ex){var _ey=E(_ex);if(!_ey[0]){return [0];}else{var _ez=E(_ey[1]);return _1w(_dr(A(_es,[_ez[1]]),_ez[2]),new T(function(){return _ew(_ey[2]);}));}},_eA=_ew(_et[1]);return _eA[0]==0?[2]:[4,_eA];}},_eB=[2],_eC=function(_eD){return [3,_eD,_eB];},_eE=function(_eF,_eG){var _eH=E(_eF);if(!_eH){return A(_eG,[_0]);}else{var _eI=new T(function(){return _eE(_eH-1|0,_eG);});return [0,function(_eJ){return E(_eI);}];}},_eK=function(_eL,_eM,_eN){var _eO=new T(function(){return A(_eL,[_eC]);});return [1,function(_eP){return A(function(_eQ,_eR,_eS){while(1){var _eT=(function(_eU,_eV,_eW){var _eX=E(_eU);switch(_eX[0]){case 0:var _eY=E(_eV);if(!_eY[0]){return E(_eM);}else{_eQ=A(_eX[1],[_eY[1]]);_eR=_eY[2];var _eZ=_eW+1|0;_eS=_eZ;return null;}break;case 1:var _f0=A(_eX[1],[_eV]),_f1=_eV,_eZ=_eW;_eQ=_f0;_eR=_f1;_eS=_eZ;return null;case 2:return E(_eM);case 3:return function(_f2){var _f3=new T(function(){return _eq(_eX,_f2);});return _eE(_eW,function(_f4){return E(_f3);});};default:return function(_aJ){return _eq(_eX,_aJ);};}})(_eQ,_eR,_eS);if(_eT!=null){return _eT;}}},[_eO,_eP,0,_eN]);}];},_f5=[6],_f6=unCStr("valDig: Bad base"),_f7=new T(function(){return err(_f6);}),_f8=function(_f9,_fa){var _fb=function(_fc,_fd){var _fe=E(_fc);if(!_fe[0]){var _ff=new T(function(){return A(_fd,[_9]);});return function(_fg){return A(_fg,[_ff]);};}else{var _fh=E(_fe[1])[1],_fi=function(_fj){var _fk=new T(function(){return _fb(_fe[2],function(_fl){return A(_fd,[[1,_fj,_fl]]);});});return function(_fm){var _fn=new T(function(){return A(_fk,[_fm]);});return [0,function(_fo){return E(_fn);}];};};switch(E(E(_f9)[1])){case 8:if(48>_fh){var _fp=new T(function(){return A(_fd,[_9]);});return function(_fq){return A(_fq,[_fp]);};}else{if(_fh>55){var _fr=new T(function(){return A(_fd,[_9]);});return function(_fs){return A(_fs,[_fr]);};}else{return _fi([0,_fh-48|0]);}}break;case 10:if(48>_fh){var _ft=new T(function(){return A(_fd,[_9]);});return function(_fu){return A(_fu,[_ft]);};}else{if(_fh>57){var _fv=new T(function(){return A(_fd,[_9]);});return function(_fw){return A(_fw,[_fv]);};}else{return _fi([0,_fh-48|0]);}}break;case 16:var _fx=new T(function(){return 97>_fh?65>_fh?[0]:_fh>70?[0]:[1,[0,(_fh-65|0)+10|0]]:_fh>102?65>_fh?[0]:_fh>70?[0]:[1,[0,(_fh-65|0)+10|0]]:[1,[0,(_fh-97|0)+10|0]];});if(48>_fh){var _fy=E(_fx);if(!_fy[0]){var _fz=new T(function(){return A(_fd,[_9]);});return function(_fA){return A(_fA,[_fz]);};}else{return _fi(_fy[1]);}}else{if(_fh>57){var _fB=E(_fx);if(!_fB[0]){var _fC=new T(function(){return A(_fd,[_9]);});return function(_fD){return A(_fD,[_fC]);};}else{return _fi(_fB[1]);}}else{return _fi([0,_fh-48|0]);}}break;default:return E(_f7);}}};return [1,function(_fE){return A(_fb,[_fE,_n,function(_fF){var _fG=E(_fF);return _fG[0]==0?[2]:A(_fa,[_fG]);}]);}];},_fH=[0,10],_fI=[0,10],_fJ=[0,0],_fK=function(_fL,_fM,_fN){while(1){var _fO=E(_fN);if(!_fO[0]){return E(_fM);}else{var _fP=_N(_1n(_fM,_fL),_fO[1]);_fN=_fO[2];_fM=_fP;continue;}}},_fQ=function(_fR){var _fS=new T(function(){return _dB(_dB([0,function(_fT){return E(E(_fT)[1])==45?_f8(_fH,function(_fU){return A(_fR,[[1,new T(function(){return _1c(_fK(_fI,_fJ,_fU));})]]);}):[2];}],[0,function(_fV){return E(E(_fV)[1])==43?_f8(_fH,function(_fW){return A(_fR,[[1,new T(function(){return _fK(_fI,_fJ,_fW);})]]);}):[2];}]),new T(function(){return _f8(_fH,function(_fX){return A(_fR,[[1,new T(function(){return _fK(_fI,_fJ,_fX);})]]);});}));});return _dB([0,function(_fY){return E(E(_fY)[1])==101?E(_fS):[2];}],[0,function(_fZ){return E(E(_fZ)[1])==69?E(_fS):[2];}]);},_g0=function(_g1){return A(_g1,[_a]);},_g2=function(_g3){return A(_g3,[_a]);},_g4=function(_g5){var _g6=new T(function(){return _f8(_fH,function(_g7){return A(_g5,[[1,_g7]]);});});return [0,function(_g8){return E(E(_g8)[1])==46?E(_g6):[2];}];},_g9=function(_ga){return _f8(_fH,function(_gb){return _eK(_g4,_g0,function(_gc){return _eK(_fQ,_g2,function(_gd){return A(_ga,[[5,[1,_gb,_gc,_gd]]]);});});});},_ge=function(_gf,_gg,_gh){while(1){var _gi=E(_gh);if(!_gi[0]){return false;}else{if(!A(_e9,[_gf,_gg,_gi[1]])){_gh=_gi[2];continue;}else{return true;}}}},_gj=unCStr("!@#$%&*+./<=>?\\^|:-~"),_gk=function(_gl){return _ge(_e8,_gl,_gj);},_gm=[0,8],_gn=[0,16],_go=function(_gp){var _gq=new T(function(){return _f8(_gn,function(_gr){return A(_gp,[[5,[0,_gn,_gr]]]);});}),_gs=new T(function(){return _f8(_gm,function(_gt){return A(_gp,[[5,[0,_gm,_gt]]]);});}),_gu=new T(function(){return _f8(_gn,function(_gv){return A(_gp,[[5,[0,_gn,_gv]]]);});}),_gw=new T(function(){return _f8(_gm,function(_gx){return A(_gp,[[5,[0,_gm,_gx]]]);});});return [0,function(_gy){return E(E(_gy)[1])==48?E([0,function(_gz){switch(E(E(_gz)[1])){case 79:return E(_gw);case 88:return E(_gu);case 111:return E(_gs);case 120:return E(_gq);default:return [2];}}]):[2];}];},_gA=true,_gB=function(_gC){var _gD=new T(function(){return A(_gC,[_gn]);}),_gE=new T(function(){return A(_gC,[_gm]);}),_gF=new T(function(){return A(_gC,[_gn]);}),_gG=new T(function(){return A(_gC,[_gm]);});return [0,function(_gH){switch(E(E(_gH)[1])){case 79:return E(_gG);case 88:return E(_gF);case 111:return E(_gE);case 120:return E(_gD);default:return [2];}}];},_gI=function(_gJ){return A(_gJ,[_fH]);},_gK=function(_gL){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _8W(9,_gL,_9);})));},_gM=function(_gN){var _gO=E(_gN);return _gO[0]==0?E(_gO[1]):I_toInt(_gO[1]);},_gP=function(_gQ,_gR){var _gS=E(_gQ);if(!_gS[0]){var _gT=_gS[1],_gU=E(_gR);return _gU[0]==0?_gT<=_gU[1]:I_compareInt(_gU[1],_gT)>=0;}else{var _gV=_gS[1],_gW=E(_gR);return _gW[0]==0?I_compareInt(_gV,_gW[1])<=0:I_compare(_gV,_gW[1])<=0;}},_gX=function(_gY){return [2];},_gZ=function(_h0){var _h1=E(_h0);if(!_h1[0]){return E(_gX);}else{var _h2=_h1[1],_h3=E(_h1[2]);if(!_h3[0]){return E(_h2);}else{var _h4=new T(function(){return _gZ(_h3);});return function(_h5){return _dB(A(_h2,[_h5]),new T(function(){return A(_h4,[_h5]);}));};}}},_h6=unCStr("NUL"),_h7=function(_h8){return [2];},_h9=function(_ha){return _h7(_ha);},_hb=function(_hc,_hd){var _he=function(_hf,_hg){var _hh=E(_hf);if(!_hh[0]){return function(_hi){return A(_hi,[_hc]);};}else{var _hj=E(_hg);if(!_hj[0]){return E(_h7);}else{if(E(_hh[1])[1]!=E(_hj[1])[1]){return E(_h9);}else{var _hk=new T(function(){return _he(_hh[2],_hj[2]);});return function(_hl){var _hm=new T(function(){return A(_hk,[_hl]);});return [0,function(_hn){return E(_hm);}];};}}}};return [1,function(_ho){return A(_he,[_hc,_ho,_hd]);}];},_hp=[0,0],_hq=function(_hr){var _hs=new T(function(){return A(_hr,[_hp]);});return _hb(_h6,function(_ht){return E(_hs);});},_hu=unCStr("STX"),_hv=[0,2],_hw=function(_hx){var _hy=new T(function(){return A(_hx,[_hv]);});return _hb(_hu,function(_hz){return E(_hy);});},_hA=unCStr("ETX"),_hB=[0,3],_hC=function(_hD){var _hE=new T(function(){return A(_hD,[_hB]);});return _hb(_hA,function(_hF){return E(_hE);});},_hG=unCStr("EOT"),_hH=[0,4],_hI=function(_hJ){var _hK=new T(function(){return A(_hJ,[_hH]);});return _hb(_hG,function(_hL){return E(_hK);});},_hM=unCStr("ENQ"),_hN=[0,5],_hO=function(_hP){var _hQ=new T(function(){return A(_hP,[_hN]);});return _hb(_hM,function(_hR){return E(_hQ);});},_hS=unCStr("ACK"),_hT=[0,6],_hU=function(_hV){var _hW=new T(function(){return A(_hV,[_hT]);});return _hb(_hS,function(_hX){return E(_hW);});},_hY=unCStr("BEL"),_hZ=[0,7],_i0=function(_i1){var _i2=new T(function(){return A(_i1,[_hZ]);});return _hb(_hY,function(_i3){return E(_i2);});},_i4=unCStr("BS"),_i5=[0,8],_i6=function(_i7){var _i8=new T(function(){return A(_i7,[_i5]);});return _hb(_i4,function(_i9){return E(_i8);});},_ia=unCStr("HT"),_ib=[0,9],_ic=function(_id){var _ie=new T(function(){return A(_id,[_ib]);});return _hb(_ia,function(_if){return E(_ie);});},_ig=unCStr("LF"),_ih=[0,10],_ii=function(_ij){var _ik=new T(function(){return A(_ij,[_ih]);});return _hb(_ig,function(_il){return E(_ik);});},_im=unCStr("VT"),_in=[0,11],_io=function(_ip){var _iq=new T(function(){return A(_ip,[_in]);});return _hb(_im,function(_ir){return E(_iq);});},_is=unCStr("FF"),_it=[0,12],_iu=function(_iv){var _iw=new T(function(){return A(_iv,[_it]);});return _hb(_is,function(_ix){return E(_iw);});},_iy=unCStr("CR"),_iz=[0,13],_iA=function(_iB){var _iC=new T(function(){return A(_iB,[_iz]);});return _hb(_iy,function(_iD){return E(_iC);});},_iE=unCStr("SI"),_iF=[0,15],_iG=function(_iH){var _iI=new T(function(){return A(_iH,[_iF]);});return _hb(_iE,function(_iJ){return E(_iI);});},_iK=unCStr("DLE"),_iL=[0,16],_iM=function(_iN){var _iO=new T(function(){return A(_iN,[_iL]);});return _hb(_iK,function(_iP){return E(_iO);});},_iQ=unCStr("DC1"),_iR=[0,17],_iS=function(_iT){var _iU=new T(function(){return A(_iT,[_iR]);});return _hb(_iQ,function(_iV){return E(_iU);});},_iW=unCStr("DC2"),_iX=[0,18],_iY=function(_iZ){var _j0=new T(function(){return A(_iZ,[_iX]);});return _hb(_iW,function(_j1){return E(_j0);});},_j2=unCStr("DC3"),_j3=[0,19],_j4=function(_j5){var _j6=new T(function(){return A(_j5,[_j3]);});return _hb(_j2,function(_j7){return E(_j6);});},_j8=unCStr("DC4"),_j9=[0,20],_ja=function(_jb){var _jc=new T(function(){return A(_jb,[_j9]);});return _hb(_j8,function(_jd){return E(_jc);});},_je=unCStr("NAK"),_jf=[0,21],_jg=function(_jh){var _ji=new T(function(){return A(_jh,[_jf]);});return _hb(_je,function(_jj){return E(_ji);});},_jk=unCStr("SYN"),_jl=[0,22],_jm=function(_jn){var _jo=new T(function(){return A(_jn,[_jl]);});return _hb(_jk,function(_jp){return E(_jo);});},_jq=unCStr("ETB"),_jr=[0,23],_js=function(_jt){var _ju=new T(function(){return A(_jt,[_jr]);});return _hb(_jq,function(_jv){return E(_ju);});},_jw=unCStr("CAN"),_jx=[0,24],_jy=function(_jz){var _jA=new T(function(){return A(_jz,[_jx]);});return _hb(_jw,function(_jB){return E(_jA);});},_jC=unCStr("EM"),_jD=[0,25],_jE=function(_jF){var _jG=new T(function(){return A(_jF,[_jD]);});return _hb(_jC,function(_jH){return E(_jG);});},_jI=unCStr("SUB"),_jJ=[0,26],_jK=function(_jL){var _jM=new T(function(){return A(_jL,[_jJ]);});return _hb(_jI,function(_jN){return E(_jM);});},_jO=unCStr("ESC"),_jP=[0,27],_jQ=function(_jR){var _jS=new T(function(){return A(_jR,[_jP]);});return _hb(_jO,function(_jT){return E(_jS);});},_jU=unCStr("FS"),_jV=[0,28],_jW=function(_jX){var _jY=new T(function(){return A(_jX,[_jV]);});return _hb(_jU,function(_jZ){return E(_jY);});},_k0=unCStr("GS"),_k1=[0,29],_k2=function(_k3){var _k4=new T(function(){return A(_k3,[_k1]);});return _hb(_k0,function(_k5){return E(_k4);});},_k6=unCStr("RS"),_k7=[0,30],_k8=function(_k9){var _ka=new T(function(){return A(_k9,[_k7]);});return _hb(_k6,function(_kb){return E(_ka);});},_kc=unCStr("US"),_kd=[0,31],_ke=function(_kf){var _kg=new T(function(){return A(_kf,[_kd]);});return _hb(_kc,function(_kh){return E(_kg);});},_ki=unCStr("SP"),_kj=[0,32],_kk=function(_kl){var _km=new T(function(){return A(_kl,[_kj]);});return _hb(_ki,function(_kn){return E(_km);});},_ko=unCStr("DEL"),_kp=[0,127],_kq=function(_kr){var _ks=new T(function(){return A(_kr,[_kp]);});return _hb(_ko,function(_kt){return E(_ks);});},_ku=[1,_kq,_9],_kv=[1,_kk,_ku],_kw=[1,_ke,_kv],_kx=[1,_k8,_kw],_ky=[1,_k2,_kx],_kz=[1,_jW,_ky],_kA=[1,_jQ,_kz],_kB=[1,_jK,_kA],_kC=[1,_jE,_kB],_kD=[1,_jy,_kC],_kE=[1,_js,_kD],_kF=[1,_jm,_kE],_kG=[1,_jg,_kF],_kH=[1,_ja,_kG],_kI=[1,_j4,_kH],_kJ=[1,_iY,_kI],_kK=[1,_iS,_kJ],_kL=[1,_iM,_kK],_kM=[1,_iG,_kL],_kN=[1,_iA,_kM],_kO=[1,_iu,_kN],_kP=[1,_io,_kO],_kQ=[1,_ii,_kP],_kR=[1,_ic,_kQ],_kS=[1,_i6,_kR],_kT=[1,_i0,_kS],_kU=[1,_hU,_kT],_kV=[1,_hO,_kU],_kW=[1,_hI,_kV],_kX=[1,_hC,_kW],_kY=[1,_hw,_kX],_kZ=[1,_hq,_kY],_l0=unCStr("SOH"),_l1=[0,1],_l2=function(_l3){var _l4=new T(function(){return A(_l3,[_l1]);});return _hb(_l0,function(_l5){return E(_l4);});},_l6=unCStr("SO"),_l7=[0,14],_l8=function(_l9){var _la=new T(function(){return A(_l9,[_l7]);});return _hb(_l6,function(_lb){return E(_la);});},_lc=function(_ld){return _eK(_l2,_l8,_ld);},_le=[1,_lc,_kZ],_lf=new T(function(){return _gZ(_le);}),_lg=[0,1114111],_lh=[0,34],_li=[0,_lh,_gA],_lj=[0,39],_lk=[0,_lj,_gA],_ll=[0,92],_lm=[0,_ll,_gA],_ln=[0,_hZ,_gA],_lo=[0,_i5,_gA],_lp=[0,_it,_gA],_lq=[0,_ih,_gA],_lr=[0,_iz,_gA],_ls=[0,_ib,_gA],_lt=[0,_in,_gA],_lu=[0,_hp,_gA],_lv=[0,_l1,_gA],_lw=[0,_hv,_gA],_lx=[0,_hB,_gA],_ly=[0,_hH,_gA],_lz=[0,_hN,_gA],_lA=[0,_hT,_gA],_lB=[0,_hZ,_gA],_lC=[0,_i5,_gA],_lD=[0,_ib,_gA],_lE=[0,_ih,_gA],_lF=[0,_in,_gA],_lG=[0,_it,_gA],_lH=[0,_iz,_gA],_lI=[0,_l7,_gA],_lJ=[0,_iF,_gA],_lK=[0,_iL,_gA],_lL=[0,_iR,_gA],_lM=[0,_iX,_gA],_lN=[0,_j3,_gA],_lO=[0,_j9,_gA],_lP=[0,_jf,_gA],_lQ=[0,_jl,_gA],_lR=[0,_jr,_gA],_lS=[0,_jx,_gA],_lT=[0,_jD,_gA],_lU=[0,_jJ,_gA],_lV=[0,_jP,_gA],_lW=[0,_jV,_gA],_lX=[0,_k1,_gA],_lY=[0,_k7,_gA],_lZ=[0,_kd,_gA],_m0=function(_m1){return [0,_m1];},_m2=function(_m3){var _m4=new T(function(){return A(_m3,[_lt]);}),_m5=new T(function(){return A(_m3,[_ls]);}),_m6=new T(function(){return A(_m3,[_lr]);}),_m7=new T(function(){return A(_m3,[_lq]);}),_m8=new T(function(){return A(_m3,[_lp]);}),_m9=new T(function(){return A(_m3,[_lo]);}),_ma=new T(function(){return A(_m3,[_ln]);}),_mb=new T(function(){return A(_m3,[_lm]);}),_mc=new T(function(){return A(_m3,[_lk]);}),_md=new T(function(){return A(_m3,[_li]);});return _dB([0,function(_me){switch(E(E(_me)[1])){case 34:return E(_md);case 39:return E(_mc);case 92:return E(_mb);case 97:return E(_ma);case 98:return E(_m9);case 102:return E(_m8);case 110:return E(_m7);case 114:return E(_m6);case 116:return E(_m5);case 118:return E(_m4);default:return [2];}}],new T(function(){return _dB(_eK(_gB,_gI,function(_mf){var _mg=new T(function(){return _m0(E(_mf)[1]);});return _f8(_mf,function(_mh){var _mi=_fK(_mg,_fJ,_mh);return !_gP(_mi,_lg)?[2]:A(_m3,[[0,new T(function(){var _mj=_gM(_mi);return _mj>>>0>1114111?_gK(_mj):[0,_mj];}),_gA]]);});}),new T(function(){var _mk=new T(function(){return A(_m3,[_lZ]);}),_ml=new T(function(){return A(_m3,[_lY]);}),_mm=new T(function(){return A(_m3,[_lX]);}),_mn=new T(function(){return A(_m3,[_lW]);}),_mo=new T(function(){return A(_m3,[_lV]);}),_mp=new T(function(){return A(_m3,[_lU]);}),_mq=new T(function(){return A(_m3,[_lT]);}),_mr=new T(function(){return A(_m3,[_lS]);}),_ms=new T(function(){return A(_m3,[_lR]);}),_mt=new T(function(){return A(_m3,[_lQ]);}),_mu=new T(function(){return A(_m3,[_lP]);}),_mv=new T(function(){return A(_m3,[_lO]);}),_mw=new T(function(){return A(_m3,[_lN]);}),_mx=new T(function(){return A(_m3,[_lM]);}),_my=new T(function(){return A(_m3,[_lL]);}),_mz=new T(function(){return A(_m3,[_lK]);}),_mA=new T(function(){return A(_m3,[_lJ]);}),_mB=new T(function(){return A(_m3,[_lI]);}),_mC=new T(function(){return A(_m3,[_lH]);}),_mD=new T(function(){return A(_m3,[_lG]);}),_mE=new T(function(){return A(_m3,[_lF]);}),_mF=new T(function(){return A(_m3,[_lE]);}),_mG=new T(function(){return A(_m3,[_lD]);}),_mH=new T(function(){return A(_m3,[_lC]);}),_mI=new T(function(){return A(_m3,[_lB]);}),_mJ=new T(function(){return A(_m3,[_lA]);}),_mK=new T(function(){return A(_m3,[_lz]);}),_mL=new T(function(){return A(_m3,[_ly]);}),_mM=new T(function(){return A(_m3,[_lx]);}),_mN=new T(function(){return A(_m3,[_lw]);}),_mO=new T(function(){return A(_m3,[_lv]);}),_mP=new T(function(){return A(_m3,[_lu]);});return _dB([0,function(_mQ){return E(E(_mQ)[1])==94?E([0,function(_mR){switch(E(E(_mR)[1])){case 64:return E(_mP);case 65:return E(_mO);case 66:return E(_mN);case 67:return E(_mM);case 68:return E(_mL);case 69:return E(_mK);case 70:return E(_mJ);case 71:return E(_mI);case 72:return E(_mH);case 73:return E(_mG);case 74:return E(_mF);case 75:return E(_mE);case 76:return E(_mD);case 77:return E(_mC);case 78:return E(_mB);case 79:return E(_mA);case 80:return E(_mz);case 81:return E(_my);case 82:return E(_mx);case 83:return E(_mw);case 84:return E(_mv);case 85:return E(_mu);case 86:return E(_mt);case 87:return E(_ms);case 88:return E(_mr);case 89:return E(_mq);case 90:return E(_mp);case 91:return E(_mo);case 92:return E(_mn);case 93:return E(_mm);case 94:return E(_ml);case 95:return E(_mk);default:return [2];}}]):[2];}],new T(function(){return A(_lf,[function(_mS){return A(_m3,[[0,_mS,_gA]]);}]);}));}));}));},_mT=function(_mU){return A(_mU,[_0]);},_mV=function(_mW){var _mX=E(_mW);if(!_mX[0]){return E(_mT);}else{var _mY=_mX[2],_mZ=E(E(_mX[1])[1]);switch(_mZ){case 9:var _n0=new T(function(){return _mV(_mY);});return function(_n1){var _n2=new T(function(){return A(_n0,[_n1]);});return [0,function(_n3){return E(_n2);}];};case 10:var _n4=new T(function(){return _mV(_mY);});return function(_n5){var _n6=new T(function(){return A(_n4,[_n5]);});return [0,function(_n7){return E(_n6);}];};case 11:var _n8=new T(function(){return _mV(_mY);});return function(_n9){var _na=new T(function(){return A(_n8,[_n9]);});return [0,function(_nb){return E(_na);}];};case 12:var _nc=new T(function(){return _mV(_mY);});return function(_nd){var _ne=new T(function(){return A(_nc,[_nd]);});return [0,function(_nf){return E(_ne);}];};case 13:var _ng=new T(function(){return _mV(_mY);});return function(_nh){var _ni=new T(function(){return A(_ng,[_nh]);});return [0,function(_nj){return E(_ni);}];};case 32:var _nk=new T(function(){return _mV(_mY);});return function(_nl){var _nm=new T(function(){return A(_nk,[_nl]);});return [0,function(_nn){return E(_nm);}];};case 160:var _no=new T(function(){return _mV(_mY);});return function(_np){var _nq=new T(function(){return A(_no,[_np]);});return [0,function(_nr){return E(_nq);}];};default:var _ns=u_iswspace(_mZ);if(!E(_ns)){return E(_mT);}else{var _nt=new T(function(){return _mV(_mY);});return function(_nu){var _nv=new T(function(){return A(_nt,[_nu]);});return [0,function(_nw){return E(_nv);}];};}}}},_nx=function(_ny){var _nz=new T(function(){return _m2(_ny);}),_nA=new T(function(){return _nx(_ny);}),_nB=[1,function(_nC){return A(_mV,[_nC,function(_nD){return E([0,function(_nE){return E(E(_nE)[1])==92?E(_nA):[2];}]);}]);}];return _dB([0,function(_nF){return E(E(_nF)[1])==92?E([0,function(_nG){var _nH=E(E(_nG)[1]);switch(_nH){case 9:return E(_nB);case 10:return E(_nB);case 11:return E(_nB);case 12:return E(_nB);case 13:return E(_nB);case 32:return E(_nB);case 38:return E(_nA);case 160:return E(_nB);default:var _nI=u_iswspace(_nH);return E(_nI)==0?[2]:E(_nB);}}]):[2];}],[0,function(_nJ){var _nK=E(_nJ);return E(_nK[1])==92?E(_nz):A(_ny,[[0,_nK,_bz]]);}]);},_nL=function(_nM,_nN){var _nO=new T(function(){return A(_nN,[[1,new T(function(){return A(_nM,[_9]);})]]);});return _nx(function(_nP){var _nQ=E(_nP),_nR=E(_nQ[1]);return E(_nR[1])==34?!E(_nQ[2])?E(_nO):_nL(function(_nS){return A(_nM,[[1,_nR,_nS]]);},_nN):_nL(function(_nT){return A(_nM,[[1,_nR,_nT]]);},_nN);});},_nU=unCStr("_\'"),_nV=function(_nW){var _nX=u_iswalnum(_nW);return E(_nX)==0?_ge(_e8,[0,_nW],_nU):true;},_nY=function(_nZ){return _nV(E(_nZ)[1]);},_o0=unCStr(",;()[]{}`"),_o1=function(_o2){return A(_o2,[_9]);},_o3=function(_o4,_o5){var _o6=function(_o7){var _o8=E(_o7);if(!_o8[0]){return E(_o1);}else{var _o9=_o8[1];if(!A(_o4,[_o9])){return E(_o1);}else{var _oa=new T(function(){return _o6(_o8[2]);});return function(_ob){var _oc=new T(function(){return A(_oa,[function(_od){return A(_ob,[[1,_o9,_od]]);}]);});return [0,function(_oe){return E(_oc);}];};}}};return [1,function(_of){return A(_o6,[_of,_o5]);}];},_og=unCStr(".."),_oh=unCStr("::"),_oi=unCStr("->"),_oj=[0,64],_ok=[1,_oj,_9],_ol=[0,126],_om=[1,_ol,_9],_on=unCStr("=>"),_oo=[1,_on,_9],_op=[1,_om,_oo],_oq=[1,_ok,_op],_or=[1,_oi,_oq],_os=unCStr("<-"),_ot=[1,_os,_or],_ou=[0,124],_ov=[1,_ou,_9],_ow=[1,_ov,_ot],_ox=[1,_ll,_9],_oy=[1,_ox,_ow],_oz=[0,61],_oA=[1,_oz,_9],_oB=[1,_oA,_oy],_oC=[1,_oh,_oB],_oD=[1,_og,_oC],_oE=function(_oF){var _oG=new T(function(){return A(_oF,[_f5]);});return _dB([1,function(_oH){return E(_oH)[0]==0?E(_oG):[2];}],new T(function(){var _oI=new T(function(){return _m2(function(_oJ){var _oK=E(_oJ);return (function(_oL,_oM){var _oN=new T(function(){return A(_oF,[[0,_oL]]);});return !E(_oM)?E(E(_oL)[1])==39?[2]:[0,function(_oO){return E(E(_oO)[1])==39?E(_oN):[2];}]:[0,function(_oP){return E(E(_oP)[1])==39?E(_oN):[2];}];})(_oK[1],_oK[2]);});});return _dB([0,function(_oQ){return E(E(_oQ)[1])==39?E([0,function(_oR){var _oS=E(_oR);switch(E(_oS[1])){case 39:return [2];case 92:return E(_oI);default:var _oT=new T(function(){return A(_oF,[[0,_oS]]);});return [0,function(_oU){return E(E(_oU)[1])==39?E(_oT):[2];}];}}]):[2];}],new T(function(){var _oV=new T(function(){return _nL(_n,_oF);});return _dB([0,function(_oW){return E(E(_oW)[1])==34?E(_oV):[2];}],new T(function(){return _dB([0,function(_oX){return !_ge(_e8,_oX,_o0)?[2]:A(_oF,[[2,[1,_oX,_9]]]);}],new T(function(){return _dB([0,function(_oY){return !_ge(_e8,_oY,_gj)?[2]:_o3(_gk,function(_oZ){var _p0=[1,_oY,_oZ];return !_ge(_ep,_p0,_oD)?A(_oF,[[4,_p0]]):A(_oF,[[2,_p0]]);});}],new T(function(){return _dB([0,function(_p1){var _p2=E(_p1),_p3=_p2[1],_p4=u_iswalpha(_p3);return E(_p4)==0?E(_p3)==95?_o3(_nY,function(_p5){return A(_oF,[[3,[1,_p2,_p5]]]);}):[2]:_o3(_nY,function(_p6){return A(_oF,[[3,[1,_p2,_p6]]]);});}],new T(function(){return _eK(_go,_g9,_oF);}));}));}));}));}));}));},_p7=function(_p8){var _p9=new T(function(){return _oE(_p8);});return [1,function(_pa){return A(_mV,[_pa,function(_pb){return E(_p9);}]);}];},_pc=[0,0],_pd=function(_pe,_pf){var _pg=new T(function(){return A(_pe,[_pc,function(_ph){var _pi=new T(function(){return A(_pf,[_ph]);});return _p7(function(_pj){var _pk=E(_pj);if(_pk[0]==2){var _pl=E(_pk[1]);return _pl[0]==0?[2]:E(E(_pl[1])[1])==41?E(_pl[2])[0]==0?E(_pi):[2]:[2];}else{return [2];}});}]);});return _p7(function(_pm){var _pn=E(_pm);if(_pn[0]==2){var _po=E(_pn[1]);return _po[0]==0?[2]:E(E(_po[1])[1])==40?E(_po[2])[0]==0?E(_pg):[2]:[2];}else{return [2];}});},_pp=function(_pq){return _dB(_p7(function(_pr){var _ps=E(_pr);return _ps[0]==0?A(_pq,[_ps[1]]):[2];}),new T(function(){return _pd(_pt,_pq);}));},_pt=function(_pu,_pv){return _pp(_pv);},_pw=function(_px,_py){var _pz=function(_pA,_pB){var _pC=new T(function(){return A(_pB,[_9]);}),_pD=new T(function(){return A(_px,[_pc,function(_pE){return _pz(_gA,function(_pF){return A(_pB,[[1,_pE,_pF]]);});}]);});return _p7(function(_pG){var _pH=E(_pG);if(_pH[0]==2){var _pI=E(_pH[1]);if(!_pI[0]){return [2];}else{var _pJ=_pI[2];switch(E(E(_pI[1])[1])){case 44:return E(_pJ)[0]==0?!E(_pA)?[2]:E(_pD):[2];case 93:return E(_pJ)[0]==0?E(_pC):[2];default:return [2];}}}else{return [2];}});},_pK=function(_pL){var _pM=new T(function(){return _dB(_pz(_bz,_pL),new T(function(){return A(_px,[_pc,function(_pN){return _pz(_gA,function(_pO){return A(_pL,[[1,_pN,_pO]]);});}]);}));});return _dB(_p7(function(_pP){var _pQ=E(_pP);if(_pQ[0]==2){var _pR=E(_pQ[1]);return _pR[0]==0?[2]:E(E(_pR[1])[1])==91?E(_pR[2])[0]==0?E(_pM):[2]:[2];}else{return [2];}}),new T(function(){return _pd(function(_pS,_pT){return _pK(_pT);},_pL);}));};return _pK(_py);},_pU=function(_pV){return _dB(_dB(_p7(function(_pW){var _pX=E(_pW);return _pX[0]==1?A(_pV,[_pX[1]]):[2];}),new T(function(){return _pw(_pt,_pV);})),new T(function(){return _pd(_pY,_pV);}));},_pY=function(_pZ,_q0){return _pU(_q0);},_q1=new T(function(){return _pd(_pY,_eC);}),_q2=new T(function(){return _pw(_pt,_eC);}),_q3=function(_q4){var _q5=E(_q4);return _q5[0]==1?[3,_q5[1],_eB]:[2];},_q6=new T(function(){return _oE(_q3);}),_q7=function(_q8){return E(_q6);},_q9=function(_qa){return A(_mV,[_qa,_q7]);},_qb=[1,_q9],_qc=new T(function(){return _dB(_qb,_q2);}),_qd=new T(function(){return _dB(_qc,_q1);}),_qe=function(_qf){return _dr(_qd,_qf);},_qg=new T(function(){return _pp(_eC);}),_qh=function(_qf){return _dr(_qg,_qf);},_qi=function(_qj){return E(_qh);},_qk=[0,_qi,_qe,_pt,_pY],_ql=function(_qm){return E(E(_qm)[4]);},_qn=function(_qo,_qp,_qq){return _pw(new T(function(){return _ql(_qo);}),_qq);},_qr=function(_qs){var _qt=new T(function(){return _pw(new T(function(){return _ql(_qs);}),_eC);});return function(_aJ){return _dr(_qt,_aJ);};},_qu=function(_qv,_qw){var _qx=new T(function(){return A(_ql,[_qv,_qw,_eC]);});return function(_aJ){return _dr(_qx,_aJ);};},_qy=function(_qz){return [0,function(_qf){return _qu(_qz,_qf);},new T(function(){return _qr(_qz);}),new T(function(){return _ql(_qz);}),function(_qA,_qf){return _qn(_qz,_qA,_qf);}];},_qB=new T(function(){return _qy(_qk);}),_qC=unCStr("Prelude.(!!): negative index\n"),_qD=new T(function(){return err(_qC);}),_qE=unCStr("Prelude.(!!): index too large\n"),_qF=new T(function(){return err(_qE);}),_qG=function(_qH,_qI){while(1){var _qJ=E(_qH);if(!_qJ[0]){return E(_qF);}else{var _qK=E(_qI);if(!_qK){return E(_qJ[1]);}else{_qH=_qJ[2];_qI=_qK-1|0;continue;}}}},_qL=unCStr("ACK"),_qM=unCStr("BEL"),_qN=unCStr("BS"),_qO=unCStr("SP"),_qP=[1,_qO,_9],_qQ=unCStr("US"),_qR=[1,_qQ,_qP],_qS=unCStr("RS"),_qT=[1,_qS,_qR],_qU=unCStr("GS"),_qV=[1,_qU,_qT],_qW=unCStr("FS"),_qX=[1,_qW,_qV],_qY=unCStr("ESC"),_qZ=[1,_qY,_qX],_r0=unCStr("SUB"),_r1=[1,_r0,_qZ],_r2=unCStr("EM"),_r3=[1,_r2,_r1],_r4=unCStr("CAN"),_r5=[1,_r4,_r3],_r6=unCStr("ETB"),_r7=[1,_r6,_r5],_r8=unCStr("SYN"),_r9=[1,_r8,_r7],_ra=unCStr("NAK"),_rb=[1,_ra,_r9],_rc=unCStr("DC4"),_rd=[1,_rc,_rb],_re=unCStr("DC3"),_rf=[1,_re,_rd],_rg=unCStr("DC2"),_rh=[1,_rg,_rf],_ri=unCStr("DC1"),_rj=[1,_ri,_rh],_rk=unCStr("DLE"),_rl=[1,_rk,_rj],_rm=unCStr("SI"),_rn=[1,_rm,_rl],_ro=unCStr("SO"),_rp=[1,_ro,_rn],_rq=unCStr("CR"),_rr=[1,_rq,_rp],_rs=unCStr("FF"),_rt=[1,_rs,_rr],_ru=unCStr("VT"),_rv=[1,_ru,_rt],_rw=unCStr("LF"),_rx=[1,_rw,_rv],_ry=unCStr("HT"),_rz=[1,_ry,_rx],_rA=[1,_qN,_rz],_rB=[1,_qM,_rA],_rC=[1,_qL,_rB],_rD=unCStr("ENQ"),_rE=[1,_rD,_rC],_rF=unCStr("EOT"),_rG=[1,_rF,_rE],_rH=unCStr("ETX"),_rI=[1,_rH,_rG],_rJ=unCStr("STX"),_rK=[1,_rJ,_rI],_rL=unCStr("SOH"),_rM=[1,_rL,_rK],_rN=unCStr("NUL"),_rO=[1,_rN,_rM],_rP=[0,92],_rQ=unCStr("\\DEL"),_rR=unCStr("\\a"),_rS=unCStr("\\\\"),_rT=unCStr("\\SO"),_rU=unCStr("\\r"),_rV=unCStr("\\f"),_rW=unCStr("\\v"),_rX=unCStr("\\n"),_rY=unCStr("\\t"),_rZ=unCStr("\\b"),_s0=function(_s1,_s2){if(_s1<=127){var _s3=E(_s1);switch(_s3){case 92:return _1w(_rS,_s2);case 127:return _1w(_rQ,_s2);default:if(_s3<32){var _s4=E(_s3);switch(_s4){case 7:return _1w(_rR,_s2);case 8:return _1w(_rZ,_s2);case 9:return _1w(_rY,_s2);case 10:return _1w(_rX,_s2);case 11:return _1w(_rW,_s2);case 12:return _1w(_rV,_s2);case 13:return _1w(_rU,_s2);case 14:return _1w(_rT,new T(function(){var _s5=E(_s2);return _s5[0]==0?[0]:E(E(_s5[1])[1])==72?unAppCStr("\\&",_s5):E(_s5);}));default:return _1w([1,_rP,new T(function(){var _s6=_s4;return _s6>=0?_qG(_rO,_s6):E(_qD);})],_s2);}}else{return [1,[0,_s3],_s2];}}}else{return [1,_rP,new T(function(){var _s7=jsShowI(_s1);return _1w(fromJSStr(_s7),new T(function(){var _s8=E(_s2);if(!_s8[0]){return [0];}else{var _s9=E(_s8[1])[1];return _s9<48?E(_s8):_s9>57?E(_s8):unAppCStr("\\&",_s8);}}));})];}},_sa=[0,39],_sb=[1,_sa,_9],_sc=unCStr("\'\\\'\'"),_sd=function(_se){var _sf=E(E(_se)[1]);return _sf==39?E(_sc):[1,_sa,new T(function(){return _s0(_sf,_sb);})];},_sg=[0,34],_sh=unCStr("\\\""),_si=function(_sj,_sk){var _sl=E(_sj);if(!_sl[0]){return E(_sk);}else{var _sm=_sl[2],_sn=E(E(_sl[1])[1]);return _sn==34?_1w(_sh,new T(function(){return _si(_sm,_sk);})):_s0(_sn,new T(function(){return _si(_sm,_sk);}));}},_so=function(_sp,_sq){return [1,_sg,new T(function(){return _si(_sp,[1,_sg,_sq]);})];},_sr=function(_ss){return _1w(_sc,_ss);},_st=function(_su,_sv){var _sw=E(E(_sv)[1]);return _sw==39?E(_sr):function(_sx){return [1,_sa,new T(function(){return _s0(_sw,[1,_sa,_sx]);})];};},_sy=[0,_st,_sd,_so],_sz=function(_sA){return E(E(_sA)[3]);},_sB=function(_sC,_sD){return A(_sz,[_sC,_sD,_9]);},_sE=function(_sF,_sG,_sH){return _7l(new T(function(){return _sz(_sF);}),_sG,_sH);},_sI=function(_sJ){var _sK=new T(function(){return _sz(_sJ);});return [0,function(_sL){return E(_sK);},function(_ss){return _sB(_sJ,_ss);},function(_sM,_ss){return _sE(_sJ,_sM,_ss);}];},_sN=new T(function(){return _sI(_sy);}),_sO=new T(function(){return _cn(_5Z,_7N,_9t,_sN,_qB);}),_sP=unCStr("submit"),_sQ=new T(function(){return A(_sO,[_a,_sP]);}),_sR=new T(function(){return A(_sQ,[_2A]);}),_sS=function(_sT){return E(E(_sT)[7]);},_sU=function(_sV,_sW,_){var _sX=jsWriteHandle(E(_sV)[1],toJSStr(E(_sW)));return _0;},_sY=[0,10],_sZ=[1,_sY,_9],_t0=function(_t1,_t2,_){var _t3=E(_t1),_t4=jsWriteHandle(_t3[1],toJSStr(E(_t2)));return _sU(_t3,_sZ,_);},_t5=unCStr("true"),_t6=unCStr("hasevent"),_t7=function(_t8,_t9){while(1){var _ta=E(_t8);if(!_ta[0]){return E(_t9)[0]==0?true:false;}else{var _tb=E(_t9);if(!_tb[0]){return false;}else{if(E(_ta[1])[1]!=E(_tb[1])[1]){return false;}else{_t8=_ta[2];_t9=_tb[2];continue;}}}}},_tc=new T(function(){return [0,"keydown"];}),_td=new T(function(){return [0,"mousemove"];}),_te=new T(function(){return [0,"blur"];}),_tf=new T(function(){return [0,"focus"];}),_tg=new T(function(){return [0,"change"];}),_th=new T(function(){return [0,"unload"];}),_ti=new T(function(){return [0,"load"];}),_tj=new T(function(){return [0,"keyup"];}),_tk=new T(function(){return [0,"keypress"];}),_tl=new T(function(){return [0,"mouseup"];}),_tm=new T(function(){return [0,"mousedown"];}),_tn=new T(function(){return [0,"dblclick"];}),_to=new T(function(){return [0,"click"];}),_tp=new T(function(){return [0,"mouseout"];}),_tq=new T(function(){return [0,"mouseover"];}),_tr=function(_ts){switch(E(_ts)[0]){case 0:return E(_ti);case 1:return E(_th);case 2:return E(_tg);case 3:return E(_tf);case 4:return E(_te);case 5:return E(_td);case 6:return E(_tq);case 7:return E(_tp);case 8:return E(_to);case 9:return E(_tn);case 10:return E(_tm);case 11:return E(_tl);case 12:return E(_tk);case 13:return E(_tj);default:return E(_tc);}},_tt=function(_tu,_tv,_tw,_tx,_){var _ty=A(_tu,[_tx,_]),_tz=E(_ty),_tA=_tz[1],_tB=E(_t6),_tC=jsGetAttr(_tA,toJSStr(_tB));if(!_t7(fromJSStr(_tC),_t5)){var _tD=E(_tw),_tE=jsSetCB(_tA,_tr(_tv)[1],_tw),_tF=A(_1,[_n,_tz,_tB,_t5,_]);return _tz;}else{return _tz;}},_tG=function(_){return _a;},_tH=function(_){var _=0,_tI=newMVar(),_=putMVar(_tI,_tG);return [0,_tI];},_tJ=new T(function(){return _j(_tH);}),_tK=function(_){var _tL=E(_tJ)[1],_tM=takeMVar(_tL),_=putMVar(_tL,_tM);return _tM;},_tN=function(_){var _tO=0;if(!E(_tO)){var _tP=_tK();return A(_tP,[_]);}else{var _tQ=E(_tJ)[1],_tR=takeMVar(_tQ),_=putMVar(_tQ,_tR);return A(_tR,[_]);}},_tS=[1,_sg,_9],_tT=unCStr("raiseEvent"),_tU=new T(function(){return _si(_tT,_tS);}),_tV=[1,_sg,_tU],_tW=function(_){var _=0,_tX=jsMkStdout();return [0,_tX];},_tY=new T(function(){return _j(_tW);}),_tZ=function(_u0,_u1,_u2,_){var _u3=_t0(_tY,_tV,_),_u4=E(_u2),_u5=E(_u4[4]),_u6=A(_u0,[_u4,_]),_u7=E(_u6),_u8=E(_u7[1]);return [0,[0,function(_47,_){return _tt(_u8[1],_u1,function(_){var _u9=A(_u5[1],[_]),_ua=E(_u9);if(!_ua[0]){var _ub=_tN(_);return _0;}else{var _uc=A(_u5[2],[_ua[1],_]),_ud=_tN(_);return _0;}},_47,_);},_u8[2]],_u7[2]];},_ue=unCStr(" could be found!"),_uf=function(_ug){return err(unAppCStr("No element with ID ",new T(function(){return _1w(_ug,_ue);})));},_uh=function(_ui,_uj,_){var _uk=E(_uj),_ul=jsFind(toJSStr(_uk)),_um=E(_ul);if(!_um[0]){return _uf(_uk);}else{var _un=E(_um[1]),_uo=jsClearChildren(_un[1]),_up=E(_m)[1],_uq=takeMVar(_up),_ur=A(_ui,[_uq,_]),_us=E(_ur),_ut=E(_us[1]),_=putMVar(_up,_us[2]),_uu=A(_ut[1],[_un,_]);return _ut[2];}},_uv=unCStr("span"),_uw=function(_47,_){return _2B(_uv,_47,_);},_ux=function(_uy,_uz,_uA,_){var _uB=A(_uz,[_uA,_]),_uC=E(_uB),_uD=E(_uC[1]);return [0,[0,function(_uE,_){var _uF=_2B(_uv,_uE,_),_uG=A(_1,[_n,_uF,_49,_uy,_]),_uH=A(_uD[1],[_uF,_]);return _uF;},_uD[2]],_uC[2]];},_uI=function(_uJ,_uK,_){return [0,_0,_uJ];},_uL=function(_uM,_){return [0,_uM,_uM];},_uN=[0,coercionToken],_uO=new T(function(){return _8K(_7M);}),_uP=new T(function(){return _91(_uN,_uO,_uL,_uI);}),_uQ=new T(function(){return _91(_uN,_uO,_uL,_uI);}),_uR=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_uS=function(_uT){return _j(function(_){var _=0;return eval(E(_uT)[1]);});},_uU=new T(function(){return _uS(_uR);}),_uV=function(_uW,_uX,_uY,_){var _uZ=A(_uP,[_uY,_]),_v0=A(_uQ,[new T(function(){return E(E(_uZ)[2]);}),_]),_v1=E(_v0),_v2=_v1[1],_v3=E(_v1[2]),_v4=_v3[2],_v5=E(_v3[4]),_v6=new T(function(){return E(E(_uZ)[1]);}),_v7=function(_v8){var _v9=new T(function(){return A(_uX,[_v8]);});return function(_va,_){var _vb=A(_v9,[_va,_]),_vc=E(_vb),_vd=E(_vc[1]);return [0,[0,function(_ve,_){var _vf=A(_vd[1],[_ve,_]),_vg=E(_v6),_vh=jsFind(toJSStr(_vg)),_vi=E(_vh);if(!_vi[0]){return _uf(_vg);}else{var _vj=E(_vi[1]),_vk=A(_uU,[E(_vj[1]),_]),_vl=jsKillChild(E(_vj)[1],_vk);return _ve;}},_vd[2]],_vc[2]];};},_vm=_ux(_v6,_uW,[0,_v3[1],_v4,_v3[3],[0,function(_){return _uh(function(_vn,_){var _vo=_ux(_v6,_uW,new T(function(){var _vp=E(_vn);return [0,_vp[1],_v4,_vp[3],_vp[4],_vp[5]];}),_);return [0,[0,_5W,E(E(_vo)[1])[2]],_vn];},_v2,_);},function(_vq,_){var _vr=_uh(new T(function(){return _v7(_vq);}),_v2,_),_vs=E(_vr);return _vs[0]==0?_a:A(_v5[2],[_vs[1],_]);}],_v3[5]],_),_vt=E(_vm),_vu=_vt[2],_vv=E(_vt[1]),_vw=_vv[1],_vx=new T(function(){return _40(_uw,[1,[0,_49,_v2],_9]);}),_vy=E(_vv[2]);if(!_vy[0]){return [0,[0,function(_vz,_){var _vA=A(_vw,[_vz,_]),_vB=A(_vx,[_vz,_]);return _vz;},_a],new T(function(){var _vC=E(_vu);return [0,_vC[1],_vC[2],_vC[3],_v5,_vC[5]];})];}else{var _vD=A(_v7,[_vy[1],new T(function(){var _vE=E(_vu);return [0,_vE[1],_vE[2],_vE[3],_v5,_vE[5]];}),_]),_vF=E(_vD),_vG=E(_vF[1]);return [0,[0,function(_vH,_){var _vI=A(_vw,[_vH,_]),_vJ=A(_vx,[_vH,_]),_vK=A(_vG[1],[_vJ,_]);return _vH;},_vG[2]],_vF[2]];}},_vL=function(_vM,_vN,_vO){return function(_vP,_){var _vQ=A(new T(function(){var _vR=new T(function(){return A(_sS,[_vM,_2k]);}),_vS=new T(function(){return _2d(_vM);}),_vT=new T(function(){return _bJ(_vN);}),_vU=function(_vV){var _vW=new T(function(){return _vU(new T(function(){return A(_vS,[_vV,_vR]);}));}),_vX=new T(function(){return _w(_p,new T(function(){return A(_vT,[_vV]);}));});return function(_aJ,_cx){return _uV(function(_vY,_){var _vZ=_tZ(_sR,_2c,_vY,_),_w0=E(_vZ),_w1=E(_w0[1]);return [0,[0,function(_w2,_){var _w3=A(_vX,[_w2,_]),_w4=A(_w1[1],[_w2,_]);return _w2;},_w1[2]],_w0[2]];},function(_w5){return E(_vW);},_aJ,_cx);};};return _vU(_vO);}),[_vP,_]),_w6=E(_vQ),_w7=E(_w6[1]);return [0,[0,function(_w8,_){var _w9=A(_2x,[_w8,_]),_wa=_2g(_w8,_),_wb=A(_w7[1],[_w8,_]);return _w8;},_w7[2]],_w6[2]];};},_wc=[0,3],_wd=new T(function(){return _vL(_1v,_2b,_wc);}),_we=[0,4],_wf=function(_wg,_wh){return [1,_wh,new T(function(){return _wf(_wg,new T(function(){return A(_wg,[_wh]);}));})];},_wi=[0,1],_wj=[1,_wi,_9],_wk=[0,0],_wl=function(_wm,_wn){return [0,E(_wm)[1]+E(_wn)[1]|0];},_wo=[1,_wk,_9],_wp=function(_wq,_wr,_ws){var _wt=E(_wr);if(!_wt[0]){return [0];}else{var _wu=E(_ws);return _wu[0]==0?[0]:[1,new T(function(){return A(_wq,[_wt[1],_wu[1]]);}),new T(function(){return _wp(_wq,_wt[2],_wu[2]);})];}},_wv=function(_ww){return _wp(_wl,[1,_wk,_ww],new T(function(){return _1w(_ww,_wo);}));},_wx=new T(function(){return _wf(_wv,_wj);}),_wy=unCStr(" rows of the Pascal triangle "),_wz=function(_wA,_wB){return _8W(0,E(_wA)[1],_wB);},_wC=function(_wD){var _wE=new T(function(){return _7l(_wz,_wD,_9);});return function(_aJ,_cx){return _p(_wE,_aJ,_cx);};},_wF=unCStr("style"),_wG=function(_wH,_wI){var _wJ=new T(function(){return _2o(_wC,_wH);});return [1,function(_wK,_){var _wL=A(_wJ,[_wK,_]),_wM=A(_1,[_n,_wL,_wF,_H,_]);return _wL;},_wI];},_wN=function(_wO,_wP){var _wQ=E(_wO);if(!_wQ[0]){return [0];}else{var _wR=_wQ[1];return _wP>1?_wG(_wR,new T(function(){return _wN(_wQ[2],_wP-1|0);})):_wG(_wR,_9);}},_wS=function(_wT){var _wU=new T(function(){return _2o(_p,new T(function(){return unAppCStr("Show ",new T(function(){return _1w(_8W(0,E(_wT)[1],_9),_wy);}));}));});return function(_wV,_){return [0,[0,function(_wW,_){var _wX=A(_wU,[_wW,_]),_wY=_5K(new T(function(){var _wZ=E(_wT)[1];return _wZ>0?_wN(_wx,_wZ):[0];}),_wW,_);return _wW;},_a],_wV];};},_x0=new T(function(){return _wS(_we);}),_x1=[0,3],_x2=function(_47,_){return _2B(_uv,_47,_);},_x3=new T(function(){return _91(_uN,_uO,_uL,_uI);}),_x4=function(_x5,_x6,_x7,_x8,_x9,_xa,_xb,_xc,_){var _xd=E(_xb);return [0,_xd,[0,_x8,_x9,_xa,[0,function(_){return _uh(function(_xe,_){var _xf=A(_x5,[new T(function(){var _xg=E(_xe);return [0,_xg[1],_x9,_xg[3],_xg[4],_xg[5]];}),_]);return [0,[0,_5W,E(E(_xf)[1])[2]],_xe];},_x7,_);},function(_xh,_){var _xi=_uh(new T(function(){return A(_x6,[_xh]);}),_x7,_),_xj=E(_xi);return _xj[0]==0?_a:A(_xd[2],[_xj[1],_]);}],_xc]];},_xk=function(_xl,_xm,_xn,_){var _xo=A(_x3,[_xn,_]),_xp=E(_xo),_xq=_xp[1],_xr=E(_xp[2]),_xs=_x4(_xl,_xm,_xq,_xr[1],_xr[2],_xr[3],_xr[4],_xr[5],_),_xt=A(_xl,[new T(function(){return E(E(_xs)[2]);}),_]),_xu=E(_xt),_xv=_xu[2],_xw=E(_xu[1]),_xx=_xw[1],_xy=new T(function(){return _40(_x2,[1,[0,_49,_xq],_9]);}),_xz=E(_xw[2]);if(!_xz[0]){return [0,[0,function(_xA,_){var _xB=A(_xx,[_xA,_]),_xC=A(_xy,[_xA,_]);return _xA;},_a],new T(function(){var _xD=E(_xv);return [0,_xD[1],_xD[2],_xD[3],new T(function(){return E(E(_xs)[1]);}),_xD[5]];})];}else{var _xE=A(_xm,[_xz[1],new T(function(){var _xF=E(_xv);return [0,_xF[1],_xF[2],_xF[3],new T(function(){return E(E(_xs)[1]);}),_xF[5]];}),_]),_xG=E(_xE),_xH=E(_xG[1]);return [0,[0,function(_xI,_){var _xJ=A(_xx,[_xI,_]),_xK=A(_xy,[_xI,_]),_xL=A(_xH[1],[_xK,_]);return _xI;},_xH[2]],_xG[2]];}},_xM=function(_xN){return _8W(0,E(_xN)[1],_9);},_xO=[1,_0],_xP=unCStr("result: "),_xQ=function(_xR){var _xS=new T(function(){return _w(_p,new T(function(){return _xM(_xR);}));});return function(_xT,_){return [0,[0,function(_xU,_){var _xV=_2g(_xU,_),_xW=_p(_xP,_xU,_),_xX=A(_xS,[_xU,_]);return _xU;},_xO],_xT];};},_xY=unCStr(" numbers and append the result using a fold"),_xZ=[1,_wk],_y0=[0,_5W,_xZ],_y1=function(_y2,_){return [0,_y0,_y2];},_y3=function(_y4,_y5,_){var _y6=A(_y5,[_]);return _y4;},_y7=function(_y8,_y9,_){var _ya=A(_y9,[_]);return new T(function(){return A(_y8,[_ya]);});},_yb=[0,_y7,_y3],_yc=function(_yd){var _ye=E(_yd);return _ye[0]==0?0:E(_ye[1])[1]+_yc(_ye[2])|0;},_yf=function(_yg){return [0,_yc(_yg)];},_yh=[0,_wk,_wl,_yf],_yi=function(_yj,_yk){var _yl=E(_yk);return _yl[0]==0?[0]:[1,new T(function(){return A(_yj,[_yl[1]]);})];},_ym=function(_yn,_yo,_yp,_yq,_yr,_ys){var _yt=new T(function(){return _bF(_yn);});return A(_yo,[new T(function(){return A(_yq,[_ys]);}),function(_yu){var _yv=E(_yu),_yw=E(_yv[1]);return A(_yo,[new T(function(){return A(_yr,[_yv[2]]);}),function(_yx){var _yy=E(_yx),_yz=E(_yy[1]);return A(_yp,[[0,[0,new T(function(){return A(_yt,[_yw[1],_yz[1]]);}),new T(function(){var _yA=E(_yw[2]);if(!_yA[0]){return [0];}else{var _yB=E(_yz[2]);return _yB[0]==0?[0]:[1,new T(function(){return A(_yA[1],[_yB[1]]);})];}})],_yy[2]]]);}]);}]);},_yC=function(_yD){return E(E(_yD)[1]);},_yE=function(_yF,_yG,_yH,_yI,_yJ,_yK){var _yL=new T(function(){return _8P(_yF);});return function(_yM){var _yN=E(_yG);return _ym(_yL,_yN[1],_yN[3],function(_yO){return A(new T(function(){var _yP=new T(function(){return _bF(_yI);});return A(_yC,[_yH,function(_yQ){return [0,new T(function(){var _yR=E(E(_yQ)[1]);return [0,_yR[1],new T(function(){return _yi(_yP,_yR[2]);})];}),new T(function(){return E(E(_yQ)[2]);})];}]);}),[new T(function(){return A(_yJ,[_yO]);})]);},_yK,_yM);};},_yS=function(_yT,_yU){while(1){var _yV=(function(_yW,_yX){var _yY=E(_yX);if(!_yY[0]){return E(_yW);}else{_yT=new T(function(){return _yE(_5Z,_7M,_yb,_yh,_yW,_yY[1]);});_yU=_yY[2];return null;}})(_yT,_yU);if(_yV!=null){return _yV;}}},_yZ=unCStr("text"),_z0=function(_z1,_z2,_z3){var _z4=function(_z5,_z6){var _z7=new T(function(){return _oE(function(_z8){return A(_z1,[_z8,_z5,function(_z9){return A(_z6,[new T(function(){return [0, -E(_z9)[1]];})]);}]);});});return _dB(_p7(function(_za){var _zb=E(_za);if(_zb[0]==4){var _zc=E(_zb[1]);return _zc[0]==0?A(_z1,[_zb,_z5,_z6]):E(E(_zc[1])[1])==45?E(_zc[2])[0]==0?E([1,function(_zd){return A(_mV,[_zd,function(_ze){return E(_z7);}]);}]):A(_z1,[_zb,_z5,_z6]):A(_z1,[_zb,_z5,_z6]);}else{return A(_z1,[_zb,_z5,_z6]);}}),new T(function(){return _pd(_z4,_z6);}));};return _z4(_z2,_z3);},_zf=function(_zg,_zh){return [2];},_zi=function(_qA,_qf){return _zf(_qA,_qf);},_zj=function(_zk){var _zl=E(_zk);return _zl[0]==0?[1,new T(function(){return _fK(new T(function(){return _m0(E(_zl[1])[1]);}),_fJ,_zl[2]);})]:E(_zl[2])[0]==0?E(_zl[3])[0]==0?[1,new T(function(){return _fK(_fI,_fJ,_zl[1]);})]:[0]:[0];},_zm=function(_zn){var _zo=E(_zn);if(_zo[0]==5){var _zp=_zj(_zo[1]);if(!_zp[0]){return E(_zf);}else{var _zq=new T(function(){return [0,_gM(_zp[1])];});return function(_zr,_zs){return A(_zs,[_zq]);};}}else{return E(_zi);}},_zt=function(_qA,_qf){return _z0(_zm,_qA,_qf);},_zu=function(_zv,_zw){return _pw(_zt,_zw);},_zx=new T(function(){return _pw(_zt,_eC);}),_zy=function(_qf){return _dr(_zx,_qf);},_zz=function(_zA){var _zB=new T(function(){return _z0(_zm,_zA,_eC);});return function(_aJ){return _dr(_zB,_aJ);};},_zC=[0,_zz,_zy,_zt,_zu],_zD=function(_zE,_zF){return _7l(_wz,_zE,_zF);},_zG=function(_zH,_zI,_zJ){return _8W(E(_zH)[1],E(_zI)[1],_zJ);},_zK=[0,_zG,_xM,_zD],_zL=unCStr("Int"),_zM=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_2O,_9i,_zL],_zN=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_zM,_9],_zO=function(_zP){return E(_zN);},_zQ=new T(function(){return _cn(_5Z,_7N,_zO,_zK,_zC);}),_zR=new T(function(){return A(_zQ,[_a,_yZ,_a]);}),_zS=function(_zT,_){var _zU=_tZ(_zR,_2c,_zT,_),_zV=E(_zU),_zW=E(_zV[1]);return [0,[0,function(_zX,_){var _zY=A(_zW[1],[_zX,_]),_zZ=_2g(_zX,_);return _zX;},_zW[2]],_zV[2]];},_A0=new T(function(){return [1,_zS,_A0];}),_A1=function(_A2,_A3){var _A4=E(_A2);if(!_A4){return [0];}else{var _A5=E(_A3);return _A5[0]==0?[0]:[1,_A5[1],new T(function(){return _A1(_A4-1|0,_A5[2]);})];}},_A6=function(_A7,_A8){return _A7<0?[0]:_A1(_A7,_A8);},_A9=function(_Aa,_Ab){var _Ac=E(_Aa)[1];return _Ac>0?_A6(_Ac,_Ab):[0];},_Ad=function(_Ae){return E(_Ae);},_Af=function(_Ag){var _Ah=new T(function(){return _yS(_y1,_A9(_Ag,_A0));}),_Ai=new T(function(){return _2o(_p,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1w(_8W(0,E(_Ag)[1],_9),_xY);}));}));});return function(_Aj,_){var _Ak=_xk(_Ah,_xQ,_Aj,_),_Al=E(_Ak),_Am=E(_Al[1]),_An=new T(function(){return _2o(_Ad,_Am[1]);});return [0,[0,function(_Ao,_){var _Ap=A(_Ai,[_Ao,_]),_Aq=A(_An,[_Ao,_]);return _Ao;},_Am[2]],_Al[2]];};},_Ar=new T(function(){return _Af(_x1);}),_As=unCStr("center"),_At=function(_Au){var _Av=jsShow(E(_Au)[1]);return fromJSStr(_Av);},_Aw=function(_Ax){var _Ay=new T(function(){return _At(_Ax);});return function(_aJ){return _1w(_Ay,_aJ);};},_Az=function(_AA,_AB,_AC){var _AD=E(_AC);if(!_AD[0]){return [0];}else{var _AE=_AD[2],_AF=E(_AD[1]);return _AA!=_AF[1]?[1,_AF,new T(function(){return _Az(_AA,_AB,_AE);})]:_1w(_AB,new T(function(){return _Az(_AA,_AB,_AE);}));}},_AG=[0,45],_AH=function(_AI,_AJ,_AK){var _AL=new T(function(){return A(_AI,[[0, -_AK]]);}),_AM=new T(function(){return E(_AJ)[1]<=6?function(_AN){return [1,_AG,new T(function(){return A(_AL,[_AN]);})];}:function(_AO){return [1,_1P,[1,_AG,new T(function(){return A(_AL,[[1,_1O,_AO]]);})]];};});if(_AK>=0){var _AP=isDoubleNegativeZero(_AK);return E(_AP)==0?A(_AI,[[0,_AK]]):E(_AM);}else{return E(_AM);}},_AQ=unCStr("canvas"),_AR=unCStr("id"),_AS=function(_AT,_){var _AU=_2B(_AQ,_AT,_),_AV=A(_1,[_n,_AU,_AR,_AQ,_]);return _AU;},_AW=[1,_0],_AX=[0,_AS,_AW],_AY=function(_AZ,_){return [0,_AX,_AZ];},_B0=unCStr("Pattern match failure in do expression at main.hs:151:5-12"),_B1=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_B2=new T(function(){return _uS(_B1);}),_B3=function(_B4,_){var _B5=jsHasCtx2D(_B4);if(!E(_B5)){return _a;}else{var _B6=jsGetCtx2D(_B4);return [1,[0,[0,_B6],[0,_B4]]];}},_B7=function(_B8,_){return _B3(E(_B8)[1],_);},_B9=function(_Ba,_Bb){return A(_Ba,[function(_){var _Bc=jsFind(toJSStr(E(_Bb))),_Bd=E(_Bc);return _Bd[0]==0?_a:_B7(_Bd[1],_);}]);},_Be=new T(function(){return _B9(_n,_AQ);}),_Bf=[0,-10],_Bg=[0,0],_Bh=[0,_Bf,_Bg],_Bi=[0,10],_Bj=[0,_Bi,_Bg],_Bk=[1,_Bj,_9],_Bl=[1,_Bh,_Bk],_Bm=function(_Bn,_){return _0;},_Bo=function(_Bp){var _Bq=E(_Bp);if(!_Bq[0]){return E(_Bm);}else{var _Br=E(_Bq[1]);return function(_Bs,_){var _Bt=E(_Bs)[1],_Bu=jsMoveTo(_Bt,E(_Br[1])[1],E(_Br[2])[1]);return (function(_Bv,_){while(1){var _Bw=E(_Bv);if(!_Bw[0]){return _0;}else{var _Bx=E(_Bw[1]),_By=jsLineTo(_Bt,E(_Bx[1])[1],E(_Bx[2])[1]);_Bv=_Bw[2];continue;}}})(_Bq[2],_);};}},_Bz=new T(function(){return _Bo(_Bl);}),_BA=[0,30],_BB=[0,_Bg,_BA],_BC=[0,-30],_BD=[0,_Bg,_BC],_BE=[1,_BD,_9],_BF=[1,_BB,_BE],_BG=new T(function(){return _Bo(_BF);}),_BH=new T(function(){return [0,0/0];}),_BI=new T(function(){return [0,-1/0];}),_BJ=new T(function(){return [0,1/0];}),_BK=[0,0],_BL=function(_BM,_BN){while(1){var _BO=E(_BM);if(!_BO[0]){_BM=[1,I_fromInt(_BO[1])];continue;}else{var _BP=E(_BN);if(!_BP[0]){_BM=_BO;_BN=[1,I_fromInt(_BP[1])];continue;}else{return I_fromRat(_BO[1],_BP[1]);}}}},_BQ=function(_BR,_BS){var _BT=E(_BR);if(!_BT[0]){var _BU=_BT[1],_BV=E(_BS);return _BV[0]==0?_BU==_BV[1]:I_compareInt(_BV[1],_BU)==0?true:false;}else{var _BW=_BT[1],_BX=E(_BS);return _BX[0]==0?I_compareInt(_BW,_BX[1])==0?true:false:I_compare(_BW,_BX[1])==0?true:false;}},_BY=function(_BZ,_C0){return !_BQ(_C0,_BK)?[0,_BL(_BZ,_C0)]:!_BQ(_BZ,_BK)?!_1G(_BZ,_BK)?E(_BJ):E(_BI):E(_BH);},_C1=function(_C2){var _C3=E(_C2);return _BY(_C3[1],_C3[2]);},_C4=function(_C5){return [0,1/E(_C5)[1]];},_C6=function(_C7){var _C8=E(_C7),_C9=_C8[1];return _C9<0?[0, -_C9]:E(_C8);},_Ca=function(_Cb){var _Cc=E(_Cb);return _Cc[0]==0?_Cc[1]:I_toNumber(_Cc[1]);},_Cd=function(_Ce){return [0,_Ca(_Ce)];},_Cf=[0,0],_Cg=[0,1],_Ch=[0,-1],_Ci=function(_Cj){var _Ck=E(_Cj)[1];return _Ck!=0?_Ck<=0?E(_Ch):E(_Cg):E(_Cf);},_Cl=function(_Cm,_Cn){return [0,E(_Cm)[1]-E(_Cn)[1]];},_Co=function(_Cp){return [0, -E(_Cp)[1]];},_Cq=function(_Cr,_Cs){return [0,E(_Cr)[1]+E(_Cs)[1]];},_Ct=function(_Cu,_Cv){return [0,E(_Cu)[1]*E(_Cv)[1]];},_Cw=[0,_Cq,_Ct,_Cl,_Co,_C6,_Ci,_Cd],_Cx=function(_Cy,_Cz){return [0,E(_Cy)[1]/E(_Cz)[1]];},_CA=[0,_Cw,_Cx,_C4,_C1],_CB=function(_CC,_CD){return E(_CC)[1]!=E(_CD)[1]?true:false;},_CE=function(_CF,_CG){return E(_CF)[1]==E(_CG)[1];},_CH=[0,_CE,_CB],_CI=function(_CJ,_CK){return E(_CJ)[1]<E(_CK)[1];},_CL=function(_CM,_CN){return E(_CM)[1]<=E(_CN)[1];},_CO=function(_CP,_CQ){return E(_CP)[1]>E(_CQ)[1];},_CR=function(_CS,_CT){return E(_CS)[1]>=E(_CT)[1];},_CU=function(_CV,_CW){var _CX=E(_CV)[1],_CY=E(_CW)[1];return _CX>=_CY?_CX!=_CY?2:1:0;},_CZ=function(_D0,_D1){var _D2=E(_D0),_D3=E(_D1);return _D2[1]>_D3[1]?E(_D2):E(_D3);},_D4=function(_D5,_D6){var _D7=E(_D5),_D8=E(_D6);return _D7[1]>_D8[1]?E(_D8):E(_D7);},_D9=[0,_CH,_CU,_CI,_CR,_CO,_CL,_CZ,_D4],_Da=[0,1],_Db=function(_Dc){return E(E(_Dc)[1]);},_Dd=function(_De){return E(E(_De)[2]);},_Df=function(_Dg){return E(E(_Dg)[6]);},_Dh=[0,2],_Di=function(_Dj,_Dk){var _Dl=E(_Dk);return [1,_Dl,new T(function(){var _Dm=_Db(_Dj);return _Di(_Dj,A(_Dm[1],[_Dl,new T(function(){return A(_Dm[7],[_Da]);})]));})];},_Dn=function(_Do,_Dp){var _Dq=E(_Dp);if(!_Dq[0]){return [0];}else{var _Dr=_Dq[1];return !A(_Do,[_Dr])?[0]:[1,_Dr,new T(function(){return _Dn(_Do,_Dq[2]);})];}},_Ds=function(_Dt,_Du,_Dv,_Dw){var _Dx=new T(function(){return _Df(_Dt);});return _Dn(function(_Dy){return A(_Dx,[_Dy,new T(function(){var _Dz=_Db(_Du),_DA=_Dz[7];return A(_Dz[1],[_Dw,new T(function(){return A(_Dd,[_Du,new T(function(){return A(_DA,[_Da]);}),new T(function(){return A(_DA,[_Dh]);})]);})]);})]);},_Di(_Du,_Dv));},_DB=new T(function(){return _Ds(_D9,_CA,_Bf,_Bi);}),_DC=function(_DD,_DE){var _DF=E(_DD);if(!_DF[0]){return [0];}else{var _DG=E(_DE);return _DG[0]==0?[0]:[1,[0,_DF[1],_DG[1]],new T(function(){return _DC(_DF[2],_DG[2]);})];}},_DH=function(_DI,_DJ,_){var _DK=function(_DL,_){var _DM=E(_DL);if(!_DM[0]){return _9;}else{var _DN=A(_B2,[E(toJSStr(_Az(120,new T(function(){return A(_AH,[_Aw,_9A,E(_DM[1])[1],_9]);}),_DI))),_]),_DO=_DK(_DM[2],_);return [1,[0,_DN],_DO];}};return _xk(_AY,function(_DP,_DQ,_){return (function(_DR,_){return [0,[0,function(_DS,_){var _DT=A(_Be,[_]),_DU=E(_DT);if(!_DU[0]){var _DV=_7K(_B0,_);return _DS;}else{var _DW=_DK(_DB,_),_DX=E(_DU[1]),_DY=jsResetCanvas(E(_DX[2])[1]),_DZ=E(_DX[1]),_E0=_DZ[1],_E1=jsPushState(_E0),_E2=jsScale(_E0,3,1),_E3=jsPushState(_E0),_E4=jsTranslate(_E0,50,130),_E5=jsPushState(_E0),_E6=jsRotate(_E0,3.141592653589793),_E7=jsBeginPath(_E0),_E8=A(_Bz,[_DZ,_]),_E9=A(_BG,[_DZ,_]),_Ea=A(_Bo,[_DC(_DB,_DW),_DZ,_]),_Eb=jsStroke(_E0),_Ec=jsPopState(_E0),_Ed=jsPopState(_E0),_Ee=jsPopState(_E0);return _DS;}},_AW],_DR];})(_DQ,_);},_DJ,_);},_Ef=unCStr("Math.pow(x,2);"),_Eg=[1,_Ef],_Eh=function(_Ei,_Ej,_){return [0,[0,_5W,[1,_Ei]],_Ej];},_Ek=function(_El,_Em,_En,_){return _xk(_El,function(_Eo){return E(_Em);},_En,_);},_Ep=function(_Eq,_Er,_47,_){return _Ek(_Eq,_Er,_47,_);},_Es=function(_Et){return err(_Et);},_Eu=[0,_xk,_Ep,_Eh,_Es],_Ev=function(_Ew){return E(E(_Ew)[1]);},_Ex=function(_Ey,_Ez,_EA,_EB,_EC){var _ED=new T(function(){return _8P(_Ey);}),_EE=new T(function(){return _Ev(_ED);}),_EF=new T(function(){return _7U(_Ez);}),_EG=new T(function(){return _9w(_Ey);}),_EH=new T(function(){return _7Q(_Ez);}),_EI=new T(function(){return _7U(_Ez);}),_EJ=new T(function(){return _7Q(_Ez);});return A(_EA,[function(_EK){return A(_EJ,[new T(function(){return A(_EB,[_EK]);}),function(_EL){var _EM=E(_EL),_EN=E(_EM[1]);return A(_EI,[[0,[0,_EN[1],[1,_EN[2]]],_EM[2]]]);}]);},function(_EO){var _EP=E(_EO);if(!_EP[0]){return function(_EQ){return A(_EF,[[0,[0,_EE,_a],_EQ]]);};}else{var _ER=new T(function(){return A(_EC,[_EP[1]]);});return function(_ES){return A(_EH,[new T(function(){return A(_ER,[_ES]);}),function(_ET){var _EU=E(_ET),_EV=_EU[2],_EW=E(_EU[1]);return _EW[0]==0?A(_EF,[[0,[0,_EE,_EP],_EV]]):A(_EF,[[0,[0,new T(function(){return A(_EG,[_EW[1]]);}),_a],_EV]]);}]);};}}]);},_EX=function(_EY,_EZ,_F0){var _F1=new T(function(){return A(_9g,[_EY,_9]);}),_F2=new T(function(){return _7O(_F0);}),_F3=new T(function(){return _7U(_F2);}),_F4=new T(function(){return _cn(_EY,_F0,_9t,_sN,_qB);});return function(_F5){return _Ex(_EY,_F2,E(_EZ)[1],new T(function(){return A(_F4,[_a,_yZ,_F5]);}),function(_F6,_F7){return E(_F6)[0]==0?A(_F3,[[0,[1,_F1],_F7]]):A(_F3,[[0,_a,_F7]]);});};},_F8=new T(function(){return _EX(_5Z,_Eu,_7N);}),_F9=new T(function(){return A(_F8,[_Eg]);}),_Fa=function(_Fb,_){var _Fc=_tZ(_F9,_2c,_Fb,_),_Fd=E(_Fc),_Fe=E(_Fd[1]);return [0,[0,function(_Ff,_){var _Fg=A(_Fe[1],[_Ff,_]),_Fh=_2g(_Ff,_);return _Ff;},new T(function(){var _Fi=E(_Fe[2]);return _Fi[0]==0?E(_Eg):E(_Fi);})],_Fd[2]];},_Fj=function(_Fk,_){var _Fl=_xk(_Fa,_DH,_Fk,_),_Fm=E(_Fl),_Fn=E(_Fm[1]);return [0,[0,function(_Fo,_){var _Fp=_2B(_As,_Fo,_),_Fq=A(_Fn[1],[_Fp,_]);return _Fp;},_Fn[2]],_Fm[2]];},_Fr=[1,_wk],_Fs=function(_Ft,_){return _Ft;},_Fu=unCStr("main"),_Fv=unCStr("Main"),_Fw=unCStr("GalleryIndex"),_Fx=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Fu,_Fv,_Fw],_Fy=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Fx,_9],_Fz=function(_FA){return E(_Fy);},_FB=function(_FC,_FD){var _FE=hs_leWord64(_FC,_FD);return E(_FE)==0?false:true;},_FF=function(_FG,_FH,_FI,_FJ){var _FK=hs_eqWord64(_FG,_FI);if(!E(_FK)){var _FL=hs_leWord64(_FG,_FI);return E(_FL)==0?false:true;}else{return _FB(_FH,_FJ);}},_FM=function(_FN,_FO){var _FP=E(_FN),_FQ=_FP[1],_FR=_FP[2],_FS=E(_FO),_FT=_FS[1],_FU=_FS[2],_FV=hs_eqWord64(_FQ,_FT);if(!E(_FV)){return !_FF(_FQ,_FR,_FT,_FU)?2:0;}else{var _FW=hs_eqWord64(_FR,_FU);return E(_FW)==0?!_FF(_FQ,_FR,_FT,_FU)?2:0:1;}},_FX=function(_FY,_FZ,_G0,_G1,_G2){while(1){var _G3=E(_G2);if(!_G3[0]){switch(_FM([0,_FY,_FZ,_G0,_G1],_G3[2])){case 0:_G2=_G3[4];continue;case 1:return [1,_G3[3]];default:_G2=_G3[5];continue;}}else{return [0];}}},_G4=function(_G5,_G6){var _G7=E(_G5),_G8=_G7[1],_G9=_G7[2],_Ga=_G7[3],_Gb=_G7[4],_Gc=E(_G6);if(!_Gc[0]){switch(_FM(_G7,_Gc[2])){case 0:return _FX(_G8,_G9,_Ga,_Gb,_Gc[4]);case 1:return [1,_Gc[3]];default:return _FX(_G8,_G9,_Ga,_Gb,_Gc[5]);}}else{return [0];}},_Gd=function(_Ge,_Gf,_Gg,_Gh){var _Gi=E(_Gf),_Gj=_Gi[1],_Gk=_Gi[3],_Gl=new T(function(){return A(_Gh,[_9v]);}),_Gm=new T(function(){return A(_Gk,[_a]);});return A(_Gj,[new T(function(){return A(_Gj,[_Gg,function(_Gn){return A(_Gk,[new T(function(){var _Go=E(_Ge);return E(E(_Gn)[5]);})]);}]);}),function(_Gp){var _Gq=_G4(_Gl,_Gp);return _Gq[0]==0?E(_Gm):A(_Gk,[[1,_Gq[1]]]);}]);},_Gr=new T(function(){return _Gd(_uN,_uO,_uL,_Fz);}),_Gs=function(_Gt,_){var _Gu=A(_Gr,[_Gt,_]);return [0,[0,_Fs,new T(function(){var _Gv=E(E(_Gu)[1]);return _Gv[0]==0?E(_Fr):E(_Gv);})],new T(function(){return E(E(_Gu)[2]);})];},_Gw=unCStr("Failure in Data.Map.balanceL"),_Gx=new T(function(){return err(_Gw);}),_Gy=function(_Gz,_GA,_GB,_GC){var _GD=E(_GC);if(!_GD[0]){var _GE=_GD[1],_GF=E(_GB);if(!_GF[0]){var _GG=_GF[1],_GH=_GF[2],_GI=_GF[3];if(_GG<=(imul(3,_GE)|0)){return [0,(1+_GG|0)+_GE|0,E(E(_Gz)),_GA,E(_GF),E(_GD)];}else{var _GJ=E(_GF[4]);if(!_GJ[0]){var _GK=_GJ[1],_GL=E(_GF[5]);if(!_GL[0]){var _GM=_GL[1],_GN=_GL[2],_GO=_GL[3],_GP=_GL[4];if(_GM>=(imul(2,_GK)|0)){var _GQ=function(_GR){var _GS=E(_GL[5]);return _GS[0]==0?[0,(1+_GG|0)+_GE|0,E(_GN),_GO,E([0,(1+_GK|0)+_GR|0,E(_GH),_GI,E(_GJ),E(_GP)]),E([0,(1+_GE|0)+_GS[1]|0,E(E(_Gz)),_GA,E(_GS),E(_GD)])]:[0,(1+_GG|0)+_GE|0,E(_GN),_GO,E([0,(1+_GK|0)+_GR|0,E(_GH),_GI,E(_GJ),E(_GP)]),E([0,1+_GE|0,E(E(_Gz)),_GA,E(_8),E(_GD)])];},_GT=E(_GP);return _GT[0]==0?_GQ(_GT[1]):_GQ(0);}else{return [0,(1+_GG|0)+_GE|0,E(_GH),_GI,E(_GJ),E([0,(1+_GE|0)+_GM|0,E(E(_Gz)),_GA,E(_GL),E(_GD)])];}}else{return E(_Gx);}}else{return E(_Gx);}}}else{return [0,1+_GE|0,E(E(_Gz)),_GA,E(_8),E(_GD)];}}else{var _GU=E(_GB);if(!_GU[0]){var _GV=_GU[1],_GW=_GU[2],_GX=_GU[3],_GY=_GU[5],_GZ=E(_GU[4]);if(!_GZ[0]){var _H0=_GZ[1],_H1=E(_GY);if(!_H1[0]){var _H2=_H1[1],_H3=_H1[2],_H4=_H1[3],_H5=_H1[4];if(_H2>=(imul(2,_H0)|0)){var _H6=function(_H7){var _H8=E(_H1[5]);return _H8[0]==0?[0,1+_GV|0,E(_H3),_H4,E([0,(1+_H0|0)+_H7|0,E(_GW),_GX,E(_GZ),E(_H5)]),E([0,1+_H8[1]|0,E(E(_Gz)),_GA,E(_H8),E(_8)])]:[0,1+_GV|0,E(_H3),_H4,E([0,(1+_H0|0)+_H7|0,E(_GW),_GX,E(_GZ),E(_H5)]),E([0,1,E(E(_Gz)),_GA,E(_8),E(_8)])];},_H9=E(_H5);return _H9[0]==0?_H6(_H9[1]):_H6(0);}else{return [0,1+_GV|0,E(_GW),_GX,E(_GZ),E([0,1+_H2|0,E(E(_Gz)),_GA,E(_H1),E(_8)])];}}else{return [0,3,E(_GW),_GX,E(_GZ),E([0,1,E(E(_Gz)),_GA,E(_8),E(_8)])];}}else{var _Ha=E(_GY);return _Ha[0]==0?[0,3,E(_Ha[2]),_Ha[3],E([0,1,E(_GW),_GX,E(_8),E(_8)]),E([0,1,E(E(_Gz)),_GA,E(_8),E(_8)])]:[0,2,E(E(_Gz)),_GA,E(_GU),E(_8)];}}else{return [0,1,E(E(_Gz)),_GA,E(_8),E(_8)];}}},_Hb=unCStr("Failure in Data.Map.balanceR"),_Hc=new T(function(){return err(_Hb);}),_Hd=function(_He,_Hf,_Hg,_Hh){var _Hi=E(_Hg);if(!_Hi[0]){var _Hj=_Hi[1],_Hk=E(_Hh);if(!_Hk[0]){var _Hl=_Hk[1],_Hm=_Hk[2],_Hn=_Hk[3];if(_Hl<=(imul(3,_Hj)|0)){return [0,(1+_Hj|0)+_Hl|0,E(E(_He)),_Hf,E(_Hi),E(_Hk)];}else{var _Ho=E(_Hk[4]);if(!_Ho[0]){var _Hp=_Ho[1],_Hq=_Ho[2],_Hr=_Ho[3],_Hs=_Ho[4],_Ht=E(_Hk[5]);if(!_Ht[0]){var _Hu=_Ht[1];if(_Hp>=(imul(2,_Hu)|0)){var _Hv=function(_Hw){var _Hx=E(_He),_Hy=E(_Ho[5]);return _Hy[0]==0?[0,(1+_Hj|0)+_Hl|0,E(_Hq),_Hr,E([0,(1+_Hj|0)+_Hw|0,E(_Hx),_Hf,E(_Hi),E(_Hs)]),E([0,(1+_Hu|0)+_Hy[1]|0,E(_Hm),_Hn,E(_Hy),E(_Ht)])]:[0,(1+_Hj|0)+_Hl|0,E(_Hq),_Hr,E([0,(1+_Hj|0)+_Hw|0,E(_Hx),_Hf,E(_Hi),E(_Hs)]),E([0,1+_Hu|0,E(_Hm),_Hn,E(_8),E(_Ht)])];},_Hz=E(_Hs);return _Hz[0]==0?_Hv(_Hz[1]):_Hv(0);}else{return [0,(1+_Hj|0)+_Hl|0,E(_Hm),_Hn,E([0,(1+_Hj|0)+_Hp|0,E(E(_He)),_Hf,E(_Hi),E(_Ho)]),E(_Ht)];}}else{return E(_Hc);}}else{return E(_Hc);}}}else{return [0,1+_Hj|0,E(E(_He)),_Hf,E(_Hi),E(_8)];}}else{var _HA=E(_Hh);if(!_HA[0]){var _HB=_HA[1],_HC=_HA[2],_HD=_HA[3],_HE=_HA[5],_HF=E(_HA[4]);if(!_HF[0]){var _HG=_HF[1],_HH=_HF[2],_HI=_HF[3],_HJ=_HF[4],_HK=E(_HE);if(!_HK[0]){var _HL=_HK[1];if(_HG>=(imul(2,_HL)|0)){var _HM=function(_HN){var _HO=E(_He),_HP=E(_HF[5]);return _HP[0]==0?[0,1+_HB|0,E(_HH),_HI,E([0,1+_HN|0,E(_HO),_Hf,E(_8),E(_HJ)]),E([0,(1+_HL|0)+_HP[1]|0,E(_HC),_HD,E(_HP),E(_HK)])]:[0,1+_HB|0,E(_HH),_HI,E([0,1+_HN|0,E(_HO),_Hf,E(_8),E(_HJ)]),E([0,1+_HL|0,E(_HC),_HD,E(_8),E(_HK)])];},_HQ=E(_HJ);return _HQ[0]==0?_HM(_HQ[1]):_HM(0);}else{return [0,1+_HB|0,E(_HC),_HD,E([0,1+_HG|0,E(E(_He)),_Hf,E(_8),E(_HF)]),E(_HK)];}}else{return [0,3,E(_HH),_HI,E([0,1,E(E(_He)),_Hf,E(_8),E(_8)]),E([0,1,E(_HC),_HD,E(_8),E(_8)])];}}else{var _HR=E(_HE);return _HR[0]==0?[0,3,E(_HC),_HD,E([0,1,E(E(_He)),_Hf,E(_8),E(_8)]),E(_HR)]:[0,2,E(E(_He)),_Hf,E(_8),E(_HA)];}}else{return [0,1,E(E(_He)),_Hf,E(_8),E(_8)];}}},_HS=function(_HT,_HU,_HV,_HW,_HX,_HY){var _HZ=E(_HY);if(!_HZ[0]){var _I0=_HZ[2],_I1=_HZ[3],_I2=_HZ[4],_I3=_HZ[5];switch(_FM([0,_HT,_HU,_HV,_HW],_I0)){case 0:return _Gy(_I0,_I1,_HS(_HT,_HU,_HV,_HW,_HX,_I2),_I3);case 1:return [0,_HZ[1],E([0,_HT,_HU,_HV,_HW]),_HX,E(_I2),E(_I3)];default:return _Hd(_I0,_I1,_I2,_HS(_HT,_HU,_HV,_HW,_HX,_I3));}}else{return [0,1,E([0,_HT,_HU,_HV,_HW]),_HX,E(_8),E(_8)];}},_I4=unCStr("100%"),_I5=[8,coercionToken],_I6=new T(function(){return A(_sO,[_a,_sP]);}),_I7=[0,62],_I8=[1,_I7,_9],_I9=[1,_I8],_Ia=new T(function(){return A(_I6,[_I9]);}),_Ib=function(_DQ,_){return _tZ(_Ia,_I5,_DQ,_);},_Ic=function(_Id){return E(_Ib);},_Ie=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_If=[1,_Ie,_9],_Ig=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_Ih=[1,_Ig,_If],_Ii=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_Ij=[1,_Ii,_Ih],_Ik=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_Il=[1,_Ik,_Ij],_Im=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_In=[1,_Im,_Il],_Io=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_Ip=[1,_Io,_In],_Iq=unCStr("height"),_Ir=unCStr("img"),_Is=function(_It,_Iu){while(1){var _Iv=E(_It);if(!_Iv[0]){return E(_Iu);}else{_It=_Iv[2];var _Iw=_Iu+1|0;_Iu=_Iw;continue;}}},_Ix=new T(function(){return [0,_Is(_Ip,0)-1|0];}),_Iy=[0,_5W,_xO],_Iz=function(_IA,_){return [0,[0,_5W,[1,_IA]],_IA];},_IB=unCStr("src"),_IC=unCStr("width"),_ID=function(_IE){return function(_aJ,_cx){return _xk(function(_DQ,_){return _xk(_Iz,function(_IF){return function(_IG,_){return [0,_Iy,new T(function(){var _IH=E(_IF);return [0,_IH[1],_IH[2],_IH[3],_IH[4],new T(function(){return _HS(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Fx,_9,new T(function(){var _II=E(_IE)[1];return _II!=E(_Ix)[1]?[0,_II+1|0]:E(_wk);}),_IH[5]);})];})];};},_DQ,_);},function(_IJ,_DQ,_){return (function(_DQ,_){return _xk(function(_IK,_){return [0,[0,function(_IL,_){var _IM=_2B(_Ir,_IL,_),_IN=A(_1,[_n,_IM,_IB,new T(function(){var _IO=E(_IE)[1];return _IO>=0?_qG(_Ip,_IO):E(_qD);}),_]),_IP=A(_1,[_n,_IM,_IC,_I4,_]),_IQ=A(_1,[_n,_IM,_Iq,_I4,_]),_IR=_2g(_IL,_);return _IL;},_AW],_IK];},_Ic,_DQ,_);})(_DQ,_);},_aJ,_cx);};},_IS=function(_DQ,_){return _xk(_Gs,_ID,_DQ,_);},_IT=[0,20000],_IU=function(_IV,_IW,_){return [0,_0,_IV];},_IX=function(_IY,_){return [0,_IY,_IY];},_IZ=[0,coercionToken],_J0=new T(function(){return _8K(_7M);}),_J1=new T(function(){return _91(_IZ,_J0,_IX,_IU);}),_J2=function(_J3,_J4,_J5,_){var _J6=A(_J1,[_J5,_]),_J7=new T(function(){return E(E(_J6)[1]);}),_J8=function(_){var _J9=jsSetTimeout(E(_J3)[1],function(_){var _Ja=_uh(_J4,_J7,_);return E(_Ja)[0]==0?_J8(_):_0;});return _0;},_Jb=_J8(_);return _ux(_J7,_J4,new T(function(){return E(E(_J6)[2]);}),_);},_Jc=function(_DQ,_){return _J2(_IT,_Jd,_DQ,_);},_Je=function(_Jf){return E(_Jc);},_Jd=function(_DQ,_){return _uV(_IS,_Je,_DQ,_);},_Jg=unCStr("style"),_Jh=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_Ji=new T(function(){return _2o(_p,_Jh);}),_Jj=[0,0],_Jk=function(_Jl){var _Jm=new T(function(){return A(_zQ,[_a,_yZ,_Jl]);});return function(_aJ,_cx){return _tZ(_Jm,_2c,_aJ,_cx);};},_Jn=function(_Jo,_Jp){while(1){var _Jq=E(_Jo);if(!_Jq[0]){return E(_Jp)[0]==0?1:0;}else{var _Jr=E(_Jp);if(!_Jr[0]){return 2;}else{var _Js=E(_Jq[1])[1],_Jt=E(_Jr[1])[1];if(_Js!=_Jt){return _Js>_Jt?2:0;}else{_Jo=_Jq[2];_Jp=_Jr[2];continue;}}}}},_Ju=function(_Jv,_Jw,_Jx){var _Jy=E(_Jv),_Jz=E(_Jx);if(!_Jz[0]){var _JA=_Jz[2],_JB=_Jz[3],_JC=_Jz[4],_JD=_Jz[5];switch(_Jn(_Jy,_JA)){case 0:return _Gy(_JA,_JB,_Ju(_Jy,_Jw,_JC),_JD);case 1:return [0,_Jz[1],E(_Jy),_Jw,E(_JC),E(_JD)];default:return _Hd(_JA,_JB,_JC,_Ju(_Jy,_Jw,_JD));}}else{return [0,1,E(_Jy),_Jw,E(_8),E(_8)];}},_JE=function(_JF,_JG,_JH){var _JI=E(_JF),_JJ=_JI[1],_JK=_JI[2],_JL=_JI[3],_JM=_JI[4],_JN=E(_JH);if(!_JN[0]){var _JO=_JN[2],_JP=_JN[3],_JQ=_JN[4],_JR=_JN[5];switch(_FM(_JI,_JO)){case 0:return _Gy(_JO,_JP,_HS(_JJ,_JK,_JL,_JM,_JG,_JQ),_JR);case 1:return [0,_JN[1],E(_JI),_JG,E(_JQ),E(_JR)];default:return _Hd(_JO,_JP,_JQ,_HS(_JJ,_JK,_JL,_JM,_JG,_JR));}}else{return [0,1,E(_JI),_JG,E(_8),E(_8)];}},_JS=function(_JT,_JU){while(1){var _JV=E(_JT),_JW=E(_JU);if(!_JW[0]){switch(_Jn(_JV,_JW[2])){case 0:_JT=_JV;_JU=_JW[4];continue;case 1:return [1,_JW[3]];default:_JT=_JV;_JU=_JW[5];continue;}}else{return [0];}}},_JX=unCStr("containers-0.5.5.1"),_JY=unCStr("Data.Map.Base"),_JZ=unCStr("Map"),_K0=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_JX,_JY,_JZ],_K1=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_K0,_9],_K2=function(_K3){return E(_K1);},_K4=new T(function(){return _3I(_9m,_9r);}),_K5=function(_K6){var _K7=E(_K6);if(!_K7[0]){return [0];}else{var _K8=E(_K7[1]);return [1,[0,_K8[1],_K8[2]],new T(function(){return _K5(_K7[2]);})];}},_K9=function(_Ka,_Kb){return function(_Kc){return E(new T(function(){var _Kd=A(_Ka,[_3H]),_Ke=E(_Kd[3]),_Kf=_Ke[1],_Kg=_Ke[2],_Kh=_1w(_Kd[4],[1,new T(function(){return A(_Kb,[_3H]);}),_9]);if(!_Kh[0]){return [0,_Kf,_Kg,_Ke,_9];}else{var _Ki=_3c(new T(function(){return _30(_3o(_3z,[1,[0,_Kf,_Kg],new T(function(){return _K5(_Kh);})]));}));return [0,_Ki[1],_Ki[2],_Ke,_Kh];}}));};},_Kj=new T(function(){return _K9(_K2,_K4);}),_Kk=new T(function(){return _3I(_Kj,_zO);}),_Kl=new T(function(){return _Gd(_uN,_uO,_uL,_Kk);}),_Km=function(_Kn,_){var _Ko=A(_Kl,[_Kn,_]);return [0,[0,_5W,new T(function(){return E(E(_Ko)[1]);})],new T(function(){return E(E(_Ko)[2]);})];},_Kp=[0,_5W,_a],_Kq=function(_Kr,_){return [0,_Kp,_Kr];},_Ks=new T(function(){return _3I(_Kj,_zO);}),_Kt=[1,_8],_Ku=new T(function(){return _Gd(_uN,_uO,_uL,_Ks);}),_Kv=function(_Kw,_){var _Kx=A(_Ku,[_Kw,_]);return [0,[0,_Fs,new T(function(){var _Ky=E(E(_Kx)[1]);return _Ky[0]==0?E(_Kt):E(_Ky);})],new T(function(){return E(E(_Kx)[2]);})];},_Kz=[0,_5W,_xO],_KA=function(_KB,_KC,_KD){return function(_aJ,_cx){return _xk(function(_KE,_){var _KF=_xk(_Km,function(_KG){var _KH=_JS(_KB,_KG);return _KH[0]==0?E(_Kq):function(_KI,_){return [0,[0,_5W,_KH],_KI];};},_KE,_),_KJ=E(_KF),_KK=E(_KJ[1]);return [0,[0,function(_KL,_){var _KM=A(_KK[1],[_KL,_]);return _KL;},new T(function(){var _KN=E(_KK[2]);return _KN[0]==0?E([1,_KD]):[1,_KN];})],_KJ[2]];},function(_KO){var _KP=new T(function(){return A(_KC,[_KO]);});return function(_aJ,_cx){return _xk(function(_KQ,_){var _KR=A(_KP,[_KQ,_]),_KS=E(_KR),_KT=_KS[2],_KU=E(_KS[1]),_KV=_KU[1],_KW=_KU[2],_KX=E(_KO);return _KX[0]==0?[0,[0,function(_KY,_){var _KZ=A(_KV,[_KY,_]);return _KY;},_KW],_KT]:[0,[0,function(_L0,_){var _L1=A(_KV,[_L0,_]);return _L0;},new T(function(){var _L2=E(_KW);return _L2[0]==0?E(_KX):E(_L2);})],_KT];},function(_L3,_L4,_){return _xk(function(_DQ,_){return _xk(_Kv,function(_L5){var _L6=new T(function(){return _Ju(_KB,_L3,_L5);}),_L7=new T(function(){return A(_Ks,[_L6]);});return function(_aJ,_cx){return _xk(_Iz,function(_L8){return function(_L9,_){return [0,_Kz,new T(function(){var _La=E(_L8);return [0,_La[1],_La[2],_La[3],_La[4],new T(function(){return _JE(_L7,_L6,_La[5]);})];})];};},_aJ,_cx);};},_DQ,_);},function(_Lb,_DQ,_){return (function(_Lc,_){return [0,[0,_5W,[1,_L3]],_Lc];})(_DQ,_);},_L4,_);},_aJ,_cx);};},_aJ,_cx);};},_Ld=function(_Le,_Lf){var _Lg=new T(function(){return _N(_Le,_2k);}),_Lh=new T(function(){return _KA(new T(function(){return _1R(0,_Le,_9);}),_Jk,_a);});return function(_aJ,_cx){return _xk(_Lh,function(_Li){var _Lj=new T(function(){return _Ld(_Lg,new T(function(){return _wl(_Lf,_Li);}));}),_Lk=new T(function(){return _w(_p,new T(function(){return _8W(0,E(_Lf)[1]+E(_Li)[1]|0,_9);}));});return function(_aJ,_cx){return _xk(function(_Ll,_){return [0,[0,function(_Lm,_){var _Ln=A(_Lk,[_Lm,_]),_Lo=_2g(_Lm,_);return _Lm;},_xO],_Ll];},function(_Lp){return E(_Lj);},_aJ,_cx);};},_aJ,_cx);};},_Lq=new T(function(){return _Ld(_Jj,_wk);}),_Lr=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_Ls=new T(function(){return _2o(_p,_Lr);}),_Lt=new T(function(){return A(_zQ,[_a,_yZ,_a]);}),_Lu=function(_DQ,_){return _tZ(_Lt,_2c,_DQ,_);},_Lv=function(_Lw){var _Lx=new T(function(){return _w(_p,new T(function(){return _xM(_Lw);}));});return function(_aJ,_cx){return _xk(_Lu,function(_Ly){var _Lz=E(E(_Ly)[1]);if(!_Lz){return function(_LA,_){return [0,[0,function(_LB,_){var _LC=_2g(_LB,_),_LD=_p(_xP,_LB,_),_LE=A(_Lx,[_LB,_]);return _LB;},_a],_LA];};}else{var _LF=new T(function(){return _Lv(new T(function(){return [0,E(_Lw)[1]+_Lz|0];}));}),_LG=new T(function(){return _w(_p,new T(function(){return _8W(0,E(_Lw)[1]+_Lz|0,_9);}));});return function(_aJ,_cx){return _xk(function(_LH,_){return [0,[0,function(_LI,_){var _LJ=A(_LG,[_LI,_]),_LK=_2g(_LI,_);return _LI;},_xO],_LH];},function(_LL){return E(_LF);},_aJ,_cx);};}},_aJ,_cx);};},_LM=new T(function(){return _Lv(_wk);}),_LN=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_LO=new T(function(){return _2o(_p,_LN);}),_LP=function(_LQ){return function(_LR,_){return [0,[0,new T(function(){var _LS=new T(function(){return _w(_p,new T(function(){return _xM(_LQ);}));});return _2o(_Ad,function(_LT,_){var _LU=_p(_xP,_LT,_),_LV=A(_LS,[_LT,_]);return _LT;});}),_xO],_LR];};},_LW=unCStr(" no more than 2 please"),_LX=new T(function(){return _w(_p,_LW);}),_LY=[1,_LX],_LZ=function(_M0,_M1,_){return E(_M0)[1]>=3?[0,_LY,_M1]:[0,_a,_M1];},_M2=new T(function(){return A(_zQ,[_a,_yZ,_a]);}),_M3=function(_DQ,_){return _tZ(_M2,_2c,_DQ,_);},_M4=new T(function(){return _Ex(_5Z,_7M,_xk,_M3,_LZ);}),_M5=new T(function(){return A(_zQ,[_a,_yZ,_a]);}),_M6=unCStr("second number "),_M7=unCStr("first number"),_M8=function(_M9,_){var _Ma=_tZ(_M5,_2c,_M9,_),_Mb=E(_Ma),_Mc=E(_Mb[1]),_Md=A(_M4,[_Mb[2],_]),_Me=E(_Md),_Mf=E(_Me[1]);return [0,[0,function(_Mg,_){var _Mh=_p(_M7,_Mg,_),_Mi=_2g(_Mg,_),_Mj=A(_Mc[1],[_Mg,_]),_Mk=_2g(_Mg,_),_Ml=_p(_M6,_Mg,_),_Mm=_2g(_Mg,_),_Mn=A(_Mf[1],[_Mg,_]),_Mo=_2g(_Mg,_);return _Mg;},new T(function(){var _Mp=E(_Mc[2]);if(!_Mp[0]){return [0];}else{var _Mq=E(_Mf[2]);return _Mq[0]==0?[0]:[1,new T(function(){return _wl(_Mp[1],_Mq[1]);})];}})],_Me[2]];},_Mr=function(_Ms,_){var _Mt=_xk(_M8,_LP,_Ms,_),_Mu=E(_Mt),_Mv=E(_Mu[1]),_Mw=new T(function(){return _2o(_Ad,_Mv[1]);});return [0,[0,function(_Mx,_){var _My=A(_LO,[_Mx,_]),_Mz=A(_Mw,[_Mx,_]);return _Mx;},_Mv[2]],_Mu[2]];},_MA=unCStr("table"),_MB=unCStr("td"),_MC=unCStr("tr"),_MD=function(_ME,_){var _MF=_Mr(_ME,_),_MG=E(_MF),_MH=A(_Ar,[_MG[2],_]),_MI=E(_MH),_MJ=A(_LM,[_MI[2],_]),_MK=E(_MJ),_ML=A(_wd,[_MK[2],_]),_MM=E(_ML),_MN=A(_Lq,[_MM[2],_]),_MO=E(_MN),_MP=A(_x0,[_MO[2],_]),_MQ=E(_MP),_MR=_Fj(_MQ[2],_),_MS=E(_MR),_MT=_J2(_IT,_Jd,_MS[2],_),_MU=E(_MT);return [0,[0,function(_MV,_){var _MW=_2B(_MA,_MV,_),_MX=_2B(_MC,_MW,_),_MY=_2B(_MB,_MX,_),_MZ=A(E(_MG[1])[1],[_MY,_]),_N0=_2B(_MB,_MX,_),_N1=A(E(_MI[1])[1],[_N0,_]),_N2=_2B(_MB,_MX,_),_N3=A(_Ls,[_N2,_]),_N4=A(E(_MK[1])[1],[_N2,_]),_N5=A(_1,[_n,_MX,_wF,_I,_]),_N6=_2B(_MC,_MW,_),_N7=_2B(_MB,_N6,_),_N8=A(E(_MM[1])[1],[_N7,_]),_N9=_2B(_MB,_N6,_),_Na=A(_Ji,[_N9,_]),_Nb=A(E(_MO[1])[1],[_N9,_]),_Nc=_2B(_MB,_N6,_),_Nd=A(E(_MQ[1])[1],[_Nc,_]),_Ne=A(_1,[_n,_N6,_wF,_I,_]),_Nf=_2B(_MC,_MW,_),_Ng=_2B(_MB,_Nf,_),_Nh=A(E(_MS[1])[1],[_Ng,_]),_Ni=_2B(_MB,_Nf,_),_Nj=A(E(_MU[1])[1],[_Ni,_]),_Nk=A(_1,[_n,_Nf,_Jg,_I,_]);return _MV;},_xO],_MU[2]];},_Nl=unCStr("h1"),_Nm=function(_Nn,_No){var _Np=new T(function(){return A(_Nn,[_No]);});return function(_Nq,_){var _Nr=jsCreateElem(toJSStr(E(_Nl))),_Ns=jsAppendChild(_Nr,E(_Nq)[1]),_Nt=[0,_Nr],_Nu=A(_Np,[_Nt,_]);return _Nt;};},_Nv=unCStr("hplayground"),_Nw=new T(function(){return _Nm(_p,_Nv);}),_Nx=unCStr("idelem"),_Ny=function(_){var _Nz=E(_Nx),_NA=jsFind(toJSStr(_Nz)),_NB=E(_NA);if(!_NB[0]){return _uf(_Nz);}else{var _NC=_NB[1],_ND=E(_m)[1],_NE=takeMVar(_ND),_NF=_MD(_NE,_),_NG=E(_NF),_NH=E(_NG[1]),_=putMVar(_ND,_NG[2]),_NI=A(_Nw,[_NC,_]),_NJ=A(_1,[_n,_NI,_Jg,_H,_]),_NK=A(_NH[1],[_NC,_]),_NL=A(_G,[_NC,_]);return _NH[2];}},_NM=function(_){return _Ny(_);};
var hasteMain = function() {A(_NM, [0]);};window.onload = hasteMain;