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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=unCStr("text-align:center"),_q=unCStr("id"),_r=function(_s,_t,_u,_){var _v=E(_t),_w=A(_s,[_u,_]),_x=A(_1,[_n,_w,_v[1],_v[2],_]);return _w;},_y=function(_z,_A){while(1){var _B=(function(_C,_D){var _E=E(_D);if(!_E[0]){return E(_C);}else{_z=function(_F,_){return _r(_C,_E[1],_F,_);};_A=_E[2];return null;}})(_z,_A);if(_B!=null){return _B;}}},_G=unCStr("span"),_H=function(_I,_J,_){var _K=jsCreateElem(toJSStr(E(_I))),_L=jsAppendChild(_K,E(_J)[1]);return [0,_K];},_M=function(_F,_){return _H(_G,_F,_);},_N=function(_O,_P,_){return [0,_0,_O];},_Q=function(_R,_){return [0,_R,_R];},_S=[0,coercionToken],_T=function(_U,_V,_){var _W=A(_U,[_]);return A(_V,[_]);},_X=function(_Y,_Z,_){return _T(_Y,_Z,_);},_10=function(_11,_12,_){var _13=A(_11,[_]);return A(_12,[_13,_]);},_14=unCStr("base"),_15=unCStr("GHC.IO.Exception"),_16=unCStr("IOException"),_17=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_14,_15,_16],_18=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_17,_9],_19=function(_1a){return E(_18);},_1b=function(_1c){return E(E(_1c)[1]);},_1d=unCStr("Maybe.fromJust: Nothing"),_1e=new T(function(){return err(_1d);}),_1f=function(_1g,_1h,_1i){var _1j=new T(function(){var _1k=A(_1g,[_1i]),_1l=A(_1h,[new T(function(){var _1m=E(_1j);return _1m[0]==0?E(_1e):E(_1m[1]);})]),_1n=hs_eqWord64(_1k[1],_1l[1]);if(!E(_1n)){return [0];}else{var _1o=hs_eqWord64(_1k[2],_1l[2]);return E(_1o)==0?[0]:[1,_1i];}});return E(_1j);},_1p=function(_1q){var _1r=E(_1q);return _1f(_1b(_1r[1]),_19,_1r[2]);},_1s=unCStr(": "),_1t=[0,41],_1u=unCStr(" ("),_1v=function(_1w,_1x){var _1y=E(_1w);return _1y[0]==0?E(_1x):[1,_1y[1],new T(function(){return _1v(_1y[2],_1x);})];},_1z=unCStr("already exists"),_1A=unCStr("does not exist"),_1B=unCStr("protocol error"),_1C=unCStr("failed"),_1D=unCStr("invalid argument"),_1E=unCStr("inappropriate type"),_1F=unCStr("hardware fault"),_1G=unCStr("unsupported operation"),_1H=unCStr("timeout"),_1I=unCStr("resource vanished"),_1J=unCStr("interrupted"),_1K=unCStr("resource busy"),_1L=unCStr("resource exhausted"),_1M=unCStr("end of file"),_1N=unCStr("illegal operation"),_1O=unCStr("permission denied"),_1P=unCStr("user error"),_1Q=unCStr("unsatisified constraints"),_1R=unCStr("system error"),_1S=function(_1T,_1U){switch(E(_1T)){case 0:return _1v(_1z,_1U);case 1:return _1v(_1A,_1U);case 2:return _1v(_1K,_1U);case 3:return _1v(_1L,_1U);case 4:return _1v(_1M,_1U);case 5:return _1v(_1N,_1U);case 6:return _1v(_1O,_1U);case 7:return _1v(_1P,_1U);case 8:return _1v(_1Q,_1U);case 9:return _1v(_1R,_1U);case 10:return _1v(_1B,_1U);case 11:return _1v(_1C,_1U);case 12:return _1v(_1D,_1U);case 13:return _1v(_1E,_1U);case 14:return _1v(_1F,_1U);case 15:return _1v(_1G,_1U);case 16:return _1v(_1H,_1U);case 17:return _1v(_1I,_1U);default:return _1v(_1J,_1U);}},_1V=[0,125],_1W=unCStr("{handle: "),_1X=function(_1Y,_1Z,_20,_21,_22,_23){var _24=new T(function(){var _25=new T(function(){return _1S(_1Z,new T(function(){var _26=E(_21);return _26[0]==0?E(_23):_1v(_1u,new T(function(){return _1v(_26,[1,_1t,_23]);}));}));}),_27=E(_20);return _27[0]==0?E(_25):_1v(_27,new T(function(){return _1v(_1s,_25);}));}),_28=E(_22);if(!_28[0]){var _29=E(_1Y);if(!_29[0]){return E(_24);}else{var _2a=E(_29[1]);return _2a[0]==0?_1v(_1W,new T(function(){return _1v(_2a[1],[1,_1V,new T(function(){return _1v(_1s,_24);})]);})):_1v(_1W,new T(function(){return _1v(_2a[1],[1,_1V,new T(function(){return _1v(_1s,_24);})]);}));}}else{return _1v(_28[1],new T(function(){return _1v(_1s,_24);}));}},_2b=function(_2c){var _2d=E(_2c);return _1X(_2d[1],_2d[2],_2d[3],_2d[4],_2d[6],_9);},_2e=function(_2f,_2g){var _2h=E(_2f);return _1X(_2h[1],_2h[2],_2h[3],_2h[4],_2h[6],_2g);},_2i=[0,44],_2j=[0,93],_2k=[0,91],_2l=function(_2m,_2n,_2o){var _2p=E(_2n);return _2p[0]==0?unAppCStr("[]",_2o):[1,_2k,new T(function(){return A(_2m,[_2p[1],new T(function(){var _2q=function(_2r){var _2s=E(_2r);return _2s[0]==0?E([1,_2j,_2o]):[1,_2i,new T(function(){return A(_2m,[_2s[1],new T(function(){return _2q(_2s[2]);})]);})];};return _2q(_2p[2]);})]);})];},_2t=function(_2u,_2v){return _2l(_2e,_2u,_2v);},_2w=function(_2x,_2y,_2z){var _2A=E(_2y);return _1X(_2A[1],_2A[2],_2A[3],_2A[4],_2A[6],_2z);},_2B=[0,_2w,_2b,_2t],_2C=new T(function(){return [0,_19,_2B,_2D,_1p];}),_2D=function(_2E){return [0,_2C,_2E];},_2F=7,_2G=function(_2H){return [0,_a,_2F,_9,_2H,_a,_a];},_2I=function(_2J,_){return die(new T(function(){return _2D(new T(function(){return _2G(_2J);}));}));},_2K=function(_2L,_){return _2I(_2L,_);},_2M=function(_2N,_){return _2N;},_2O=[0,_10,_X,_2M,_2K],_2P=function(_2Q){return E(E(_2Q)[1]);},_2R=function(_2S,_2T,_2U,_2V){return A(_2P,[_2S,new T(function(){return A(_2T,[_2V]);}),function(_2W){return A(_2U,[new T(function(){return E(E(_2W)[1]);}),new T(function(){return E(E(_2W)[2]);})]);}]);},_2X=function(_2Y,_2Z,_30,_31){return A(_2P,[_2Y,new T(function(){return A(_2Z,[_31]);}),function(_32){return A(_30,[new T(function(){return E(E(_32)[2]);})]);}]);},_33=function(_34,_35,_36,_37){return _2X(_34,_35,_36,_37);},_38=function(_39){return E(E(_39)[4]);},_3a=function(_3b,_3c){var _3d=new T(function(){return A(_38,[_3b,_3c]);});return function(_3e){return E(_3d);};},_3f=function(_3g){return E(E(_3g)[3]);},_3h=function(_3i){var _3j=new T(function(){return _3f(_3i);});return [0,function(_35,_36,_37){return _2R(_3i,_35,_36,_37);},function(_35,_36,_37){return _33(_3i,_35,_36,_37);},function(_3k,_3l){return A(_3j,[[0,_3k,_3l]]);},function(_37){return _3a(_3i,_37);}];},_3m=new T(function(){return _3h(_2O);}),_3n=[0,112],_3o=function(_3p,_3q){var _3r=jsShowI(_3p);return _1v(fromJSStr(_3r),_3q);},_3s=[0,41],_3t=[0,40],_3u=function(_3v,_3w,_3x){return _3w>=0?_3o(_3w,_3x):_3v<=6?_3o(_3w,_3x):[1,_3t,new T(function(){var _3y=jsShowI(_3w);return _1v(fromJSStr(_3y),[1,_3s,_3x]);})];},_3z=function(_3A,_3B,_3C,_3D){var _3E=E(_3B);return A(_3E[1],[new T(function(){var _3F=E(_3A);return E(_3C);}),function(_3G){var _3H=new T(function(){return E(E(_3G)[2]);});return A(_3E[2],[new T(function(){return A(_3D,[new T(function(){var _3I=E(new T(function(){var _3J=E(_3A);return [0,coercionToken];})),_3K=E(_3G);return [0,_3K[1],new T(function(){return [0,E(_3H)[1]+1|0];}),_3K[3],_3K[4],_3K[5]];})]);}),new T(function(){return A(_3E[3],[[1,_3n,new T(function(){return _1v(_3u(0,E(_3H)[1],_9),new T(function(){return E(E(_3G)[1]);}));})]]);})]);}]);},_3L=new T(function(){return _3z(_S,_3m,_Q,_N);}),_3M=unCStr(" could be found!"),_3N=function(_3O){return err(unAppCStr("No element with ID ",new T(function(){return _1v(_3O,_3M);})));},_3P=function(_3Q,_3R,_){var _3S=E(_3R),_3T=jsFind(toJSStr(_3S)),_3U=E(_3T);if(!_3U[0]){return _3N(_3S);}else{var _3V=E(_3U[1]),_3W=jsClearChildren(_3V[1]),_3X=E(_m)[1],_3Y=takeMVar(_3X),_3Z=A(_3Q,[_3Y,_]),_40=E(_3Z),_41=E(_40[1]),_=putMVar(_3X,_40[2]),_42=A(_41[1],[_3V,_]);return _41[2];}},_43=function(_44,_45,_46,_47,_48,_49,_4a,_4b,_){var _4c=E(_4a);return [0,_4c,[0,_47,_48,_49,[0,function(_){return _3P(function(_4d,_){var _4e=A(_44,[new T(function(){var _4f=E(_4d);return [0,_4f[1],_48,_4f[3],_4f[4],_4f[5]];}),_]);return [0,[0,_2M,E(E(_4e)[1])[2]],_4d];},_46,_);},function(_4g,_){var _4h=_3P(new T(function(){return A(_45,[_4g]);}),_46,_),_4i=E(_4h);return _4i[0]==0?_a:A(_4c[2],[_4i[1],_]);}],_4b]];},_4j=function(_4k,_4l,_4m,_){var _4n=A(_3L,[_4m,_]),_4o=E(_4n),_4p=_4o[1],_4q=E(_4o[2]),_4r=_43(_4k,_4l,_4p,_4q[1],_4q[2],_4q[3],_4q[4],_4q[5],_),_4s=A(_4k,[new T(function(){return E(E(_4r)[2]);}),_]),_4t=E(_4s),_4u=_4t[2],_4v=E(_4t[1]),_4w=_4v[1],_4x=new T(function(){return _y(_M,[1,[0,_q,_4p],_9]);}),_4y=E(_4v[2]);if(!_4y[0]){return [0,[0,function(_4z,_){var _4A=A(_4w,[_4z,_]),_4B=A(_4x,[_4z,_]);return _4z;},_a],new T(function(){var _4C=E(_4u);return [0,_4C[1],_4C[2],_4C[3],new T(function(){return E(E(_4r)[1]);}),_4C[5]];})];}else{var _4D=A(_4l,[_4y[1],new T(function(){var _4E=E(_4u);return [0,_4E[1],_4E[2],_4E[3],new T(function(){return E(E(_4r)[1]);}),_4E[5]];}),_]),_4F=E(_4D),_4G=E(_4F[1]);return [0,[0,function(_4H,_){var _4I=A(_4w,[_4H,_]),_4J=A(_4x,[_4H,_]),_4K=A(_4G[1],[_4J,_]);return _4H;},_4G[2]],_4F[2]];}},_4L=unCStr("padding:15px;border-style:dotted"),_4M=unCStr("vertical-align:top"),_4N=[0,3],_4O=function(_4P,_4Q,_){var _4R=jsCreateTextNode(toJSStr(E(_4P))),_4S=jsAppendChild(_4R,E(_4Q)[1]);return [0,_4R];},_4T=[0,112],_4U=[1,_4T,_9],_4V=function(_4W,_4X){var _4Y=new T(function(){return A(_4W,[_4X]);});return function(_4Z,_){var _50=jsCreateElem(toJSStr(_4U)),_51=jsAppendChild(_50,E(_4Z)[1]),_52=[0,_50],_53=A(_4Y,[_52,_]);return _52;};},_54=function(_55){return _3u(0,E(_55)[1],_9);},_56=[0,98],_57=[1,_56,_9],_58=function(_59,_5a){var _5b=new T(function(){return A(_59,[_5a]);});return function(_5c,_){var _5d=jsCreateElem(toJSStr(_57)),_5e=jsAppendChild(_5d,E(_5c)[1]),_5f=[0,_5d],_5g=A(_5b,[_5f,_]);return _5f;};},_5h=unCStr("br"),_5i=function(_5j,_){var _5k=jsCreateElem(toJSStr(E(_5h))),_5l=jsAppendChild(_5k,E(_5j)[1]);return [0,_5k];},_5m=[1,_0],_5n=unCStr("result: "),_5o=function(_5p){var _5q=new T(function(){return _58(_4O,new T(function(){return _54(_5p);}));});return function(_5r,_){return [0,[0,function(_5s,_){var _5t=_5i(_5s,_),_5u=_4O(_5n,_5s,_),_5v=A(_5q,[_5s,_]);return _5s;},_5m],_5r];};},_5w=unCStr(" numbers and append the result using a fold"),_5x=[0,0],_5y=[1,_5x],_5z=[0,_2M,_5y],_5A=function(_5B,_){return [0,_5z,_5B];},_5C=function(_5D,_5E,_5F,_){var _5G=_H(_5D,_5F,_),_5H=A(_5E,[_5G,_]);return _5G;},_5I=unCStr("()"),_5J=unCStr("GHC.Tuple"),_5K=unCStr("ghc-prim"),_5L=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5K,_5J,_5I],_5M=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5L,_9],_5N=function(_5O){return E(_5M);},_5P=unCStr("main"),_5Q=unCStr("Haste.Perch"),_5R=unCStr("PerchM"),_5S=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5P,_5Q,_5R],_5T=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5S,_9],_5U=function(_5V){return E(_5T);},_5W=function(_5X){var _5Y=E(_5X);return _5Y[0]==0?[0]:_1v(_5Y[1],new T(function(){return _5W(_5Y[2]);}));},_5Z=function(_60,_61){var _62=E(_60);if(!_62){return [0,_9,_61];}else{var _63=E(_61);if(!_63[0]){return [0,_9,_9];}else{var _64=new T(function(){var _65=_5Z(_62-1|0,_63[2]);return [0,_65[1],_65[2]];});return [0,[1,_63[1],new T(function(){return E(E(_64)[1]);})],new T(function(){return E(E(_64)[2]);})];}}},_66=[0,120],_67=[0,48],_68=function(_69){var _6a=new T(function(){var _6b=_5Z(8,new T(function(){var _6c=md5(toJSStr(E(_69)));return fromJSStr(_6c);}));return [0,_6b[1],_6b[2]];}),_6d=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(E(_6a)[1]);})]])]),_6e=new T(function(){var _6f=_5Z(8,new T(function(){return E(E(_6a)[2]);}));return [0,_6f[1],_6f[2]];}),_6g=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(E(_6e)[1]);})]])]),_6h=hs_mkWord64(_6d,_6g),_6i=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(_5Z(8,new T(function(){return E(E(_6e)[2]);}))[1]);})]])]),_6j=hs_mkWord64(_6i,_6i);return [0,_6h,_6j];},_6k=function(_6l,_6m){var _6n=E(_6m);return _6n[0]==0?[0]:[1,new T(function(){return A(_6l,[_6n[1]]);}),new T(function(){return _6k(_6l,_6n[2]);})];},_6o=function(_6p,_6q){var _6r=jsShowI(_6p),_6s=md5(_6r);return _1v(fromJSStr(_6s),new T(function(){var _6t=jsShowI(_6q),_6u=md5(_6t);return fromJSStr(_6u);}));},_6v=function(_6w){var _6x=E(_6w);return _6o(_6x[1],_6x[2]);},_6y=function(_6z){var _6A=E(_6z);if(!_6A[0]){return [0];}else{var _6B=E(_6A[1]);return [1,[0,_6B[1],_6B[2]],new T(function(){return _6y(_6A[2]);})];}},_6C=unCStr("Prelude.undefined"),_6D=new T(function(){return err(_6C);}),_6E=function(_6F,_6G){return function(_6H){return E(new T(function(){var _6I=A(_6F,[_6D]),_6J=E(_6I[3]),_6K=_6J[1],_6L=_6J[2],_6M=_1v(_6I[4],[1,new T(function(){return A(_6G,[_6D]);}),_9]);if(!_6M[0]){return [0,_6K,_6L,_6J,_9];}else{var _6N=_68(new T(function(){return _5W(_6k(_6v,[1,[0,_6K,_6L],new T(function(){return _6y(_6M);})]));}));return [0,_6N[1],_6N[2],_6J,_6M];}}));};},_6O=new T(function(){return _6E(_5U,_5N);}),_6P=unCStr("value"),_6Q=unCStr("onclick"),_6R=unCStr("checked"),_6S=[0,_6R,_9],_6T=[1,_6S,_9],_6U=unCStr("type"),_6V=unCStr("input"),_6W=function(_6X,_){return _H(_6V,_6X,_);},_6Y=function(_6Z,_70,_71,_72,_73){var _74=new T(function(){var _75=new T(function(){return _y(_6W,[1,[0,_6U,_70],[1,[0,_q,_6Z],[1,[0,_6P,_71],_9]]]);});return !E(_72)?E(_75):_y(_75,_6T);}),_76=E(_73);return _76[0]==0?E(_74):_y(_74,[1,[0,_6Q,_76[1]],_9]);},_77=unCStr("href"),_78=[0,97],_79=[1,_78,_9],_7a=function(_7b,_){return _H(_79,_7b,_);},_7c=function(_7d,_7e){var _7f=new T(function(){return _y(_7a,[1,[0,_77,_7d],_9]);});return function(_7g,_){var _7h=A(_7f,[_7g,_]),_7i=A(_7e,[_7h,_]);return _7h;};},_7j=function(_7k){return _7c(_7k,function(_F,_){return _4O(_7k,_F,_);});},_7l=unCStr("option"),_7m=function(_7n,_){return _H(_7l,_7n,_);},_7o=unCStr("selected"),_7p=[0,_7o,_9],_7q=[1,_7p,_9],_7r=function(_7s,_7t,_7u){var _7v=new T(function(){return _y(_7m,[1,[0,_6P,_7s],_9]);}),_7w=function(_7x,_){var _7y=A(_7v,[_7x,_]),_7z=A(_7t,[_7y,_]);return _7y;};return !E(_7u)?E(_7w):_y(_7w,_7q);},_7A=function(_7B,_7C){return _7r(_7B,function(_F,_){return _4O(_7B,_F,_);},_7C);},_7D=unCStr("method"),_7E=unCStr("action"),_7F=unCStr("UTF-8"),_7G=unCStr("acceptCharset"),_7H=[0,_7G,_7F],_7I=unCStr("form"),_7J=function(_7K,_){return _H(_7I,_7K,_);},_7L=function(_7M,_7N,_7O){var _7P=new T(function(){return _y(_7J,[1,_7H,[1,[0,_7E,_7M],[1,[0,_7D,_7N],_9]]]);});return function(_7Q,_){var _7R=A(_7P,[_7Q,_]),_7S=A(_7O,[_7R,_]);return _7R;};},_7T=unCStr("select"),_7U=function(_7V,_){return _H(_7T,_7V,_);},_7W=function(_7X,_7Y){var _7Z=new T(function(){return _y(_7U,[1,[0,_q,_7X],_9]);});return function(_80,_){var _81=A(_7Z,[_80,_]),_82=A(_7Y,[_81,_]);return _81;};},_83=unCStr("textarea"),_84=function(_85,_){return _H(_83,_85,_);},_86=function(_87,_88){var _89=new T(function(){return _y(_84,[1,[0,_q,_87],_9]);});return function(_8a,_){var _8b=A(_89,[_8a,_]),_8c=_4O(_88,_8b,_);return _8b;};},_8d=unCStr("color:red"),_8e=unCStr("style"),_8f=[0,_8e,_8d],_8g=[1,_8f,_9],_8h=[0,98],_8i=[1,_8h,_9],_8j=function(_8k){return _y(function(_8l,_){var _8m=_H(_8i,_8l,_),_8n=A(_8k,[_8m,_]);return _8m;},_8g);},_8o=function(_8p,_8q,_){var _8r=E(_8p);if(!_8r[0]){return _8q;}else{var _8s=A(_8r[1],[_8q,_]),_8t=_8o(_8r[2],_8q,_);return _8q;}},_8u=function(_8v,_8w,_8x,_){var _8y=A(_8v,[_8x,_]),_8z=A(_8w,[_8x,_]);return _8x;},_8A=[0,_2M,_8u,_8o],_8B=[0,_8A,_6O,_4O,_4O,_5C,_8j,_7c,_7j,_6Y,_86,_7W,_7r,_7A,_7L,_y],_8C=function(_8D,_8E,_){var _8F=A(_8E,[_]);return _8D;},_8G=function(_8H,_8I,_){var _8J=A(_8I,[_]);return new T(function(){return A(_8H,[_8J]);});},_8K=[0,_8G,_8C],_8L=function(_8M){var _8N=E(_8M);return _8N[0]==0?0:E(_8N[1])[1]+_8L(_8N[2])|0;},_8O=function(_8P){return [0,_8L(_8P)];},_8Q=function(_8R,_8S){return [0,E(_8R)[1]+E(_8S)[1]|0];},_8T=[0,_5x,_8Q,_8O],_8U=function(_8V,_8W){var _8X=E(_8W);return _8X[0]==0?[0]:[1,new T(function(){return A(_8V,[_8X[1]]);})];},_8Y=function(_8Z){return E(E(_8Z)[1]);},_90=function(_91){return E(E(_91)[2]);},_92=function(_93,_94,_95,_96,_97,_98){var _99=new T(function(){return _90(_93);});return A(_94,[new T(function(){return A(_96,[_98]);}),function(_9a){var _9b=E(_9a),_9c=E(_9b[1]);return A(_94,[new T(function(){return A(_97,[_9b[2]]);}),function(_9d){var _9e=E(_9d),_9f=E(_9e[1]);return A(_95,[[0,[0,new T(function(){return A(_99,[_9c[1],_9f[1]]);}),new T(function(){var _9g=E(_9c[2]);if(!_9g[0]){return [0];}else{var _9h=E(_9f[2]);return _9h[0]==0?[0]:[1,new T(function(){return A(_9g[1],[_9h[1]]);})];}})],_9e[2]]]);}]);}]);},_9i=function(_9j){return E(E(_9j)[1]);},_9k=function(_9l,_9m,_9n,_9o,_9p,_9q){var _9r=new T(function(){return _8Y(_9l);});return function(_9s){var _9t=E(_9m);return _92(_9r,_9t[1],_9t[3],function(_9u){return A(new T(function(){var _9v=new T(function(){return _90(_9o);});return A(_9i,[_9n,function(_9w){return [0,new T(function(){var _9x=E(E(_9w)[1]);return [0,_9x[1],new T(function(){return _8U(_9v,_9x[2]);})];}),new T(function(){return E(E(_9w)[2]);})];}]);}),[new T(function(){return A(_9p,[_9u]);})]);},_9q,_9s);};},_9y=function(_9z,_9A){while(1){var _9B=(function(_9C,_9D){var _9E=E(_9D);if(!_9E[0]){return E(_9C);}else{_9z=new T(function(){return _9k(_8B,_2O,_8K,_8T,_9C,_9E[1]);});_9A=_9E[2];return null;}})(_9z,_9A);if(_9B!=null){return _9B;}}},_9F=[13,coercionToken],_9G=unCStr("text"),_9H=[0,_2O,_n],_9I=unCStr("base"),_9J=unCStr("Control.Exception.Base"),_9K=unCStr("PatternMatchFail"),_9L=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9I,_9J,_9K],_9M=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9L,_9],_9N=function(_9O){return E(_9M);},_9P=function(_9Q){var _9R=E(_9Q);return _1f(_1b(_9R[1]),_9N,_9R[2]);},_9S=function(_9T){return E(E(_9T)[1]);},_9U=function(_9V,_9W){return _1v(E(_9V)[1],_9W);},_9X=function(_9Y,_9Z){return _2l(_9U,_9Y,_9Z);},_a0=function(_a1,_a2,_a3){return _1v(E(_a2)[1],_a3);},_a4=[0,_a0,_9S,_9X],_a5=new T(function(){return [0,_9N,_a4,_a6,_9P];}),_a6=function(_a7){return [0,_a5,_a7];},_a8=unCStr("Non-exhaustive patterns in"),_a9=function(_aa,_ab){return die(new T(function(){return A(_ab,[_aa]);}));},_ac=function(_ad,_ae){var _af=E(_ae);if(!_af[0]){return [0,_9,_9];}else{var _ag=_af[1];if(!A(_ad,[_ag])){return [0,_9,_af];}else{var _ah=new T(function(){var _ai=_ac(_ad,_af[2]);return [0,_ai[1],_ai[2]];});return [0,[1,_ag,new T(function(){return E(E(_ah)[1]);})],new T(function(){return E(E(_ah)[2]);})];}}},_aj=[0,32],_ak=[0,10],_al=[1,_ak,_9],_am=function(_an){return E(E(_an)[1])==124?false:true;},_ao=function(_ap,_aq){var _ar=_ac(_am,unCStr(_ap)),_as=_ar[1],_at=function(_au,_av){return _1v(_au,new T(function(){return unAppCStr(": ",new T(function(){return _1v(_aq,new T(function(){return _1v(_av,_al);}));}));}));},_aw=E(_ar[2]);return _aw[0]==0?_at(_as,_9):E(E(_aw[1])[1])==124?_at(_as,[1,_aj,_aw[2]]):_at(_as,_9);},_ax=function(_ay){return _a9([0,new T(function(){return _ao(_ay,_a8);})],_a6);},_az=new T(function(){return _ax("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aA=function(_aB,_aC){while(1){var _aD=(function(_aE,_aF){var _aG=E(_aE);switch(_aG[0]){case 0:var _aH=E(_aF);if(!_aH[0]){return [0];}else{_aB=A(_aG[1],[_aH[1]]);_aC=_aH[2];return null;}break;case 1:var _aI=A(_aG[1],[_aF]),_aJ=_aF;_aB=_aI;_aC=_aJ;return null;case 2:return [0];case 3:return [1,[0,_aG[1],_aF],new T(function(){return _aA(_aG[2],_aF);})];default:return E(_aG[1]);}})(_aB,_aC);if(_aD!=null){return _aD;}}},_aK=function(_aL,_aM){var _aN=new T(function(){var _aO=E(_aM);if(_aO[0]==3){return [3,_aO[1],new T(function(){return _aK(_aL,_aO[2]);})];}else{var _aP=E(_aL);if(_aP[0]==2){return E(_aO);}else{var _aQ=E(_aO);if(_aQ[0]==2){return E(_aP);}else{var _aR=new T(function(){var _aS=E(_aQ);if(_aS[0]==4){return [1,function(_aT){return [4,new T(function(){return _1v(_aA(_aP,_aT),_aS[1]);})];}];}else{var _aU=E(_aP);if(_aU[0]==1){var _aV=_aU[1],_aW=E(_aS);return _aW[0]==0?[1,function(_aX){return _aK(A(_aV,[_aX]),_aW);}]:[1,function(_aY){return _aK(A(_aV,[_aY]),new T(function(){return A(_aW[1],[_aY]);}));}];}else{var _aZ=E(_aS);return _aZ[0]==0?E(_az):[1,function(_b0){return _aK(_aU,new T(function(){return A(_aZ[1],[_b0]);}));}];}}}),_b1=E(_aP);switch(_b1[0]){case 1:var _b2=E(_aQ);return _b2[0]==4?[1,function(_b3){return [4,new T(function(){return _1v(_aA(A(_b1[1],[_b3]),_b3),_b2[1]);})];}]:E(_aR);case 4:var _b4=_b1[1],_b5=E(_aQ);switch(_b5[0]){case 0:return [1,function(_b6){return [4,new T(function(){return _1v(_b4,new T(function(){return _aA(_b5,_b6);}));})];}];case 1:return [1,function(_b7){return [4,new T(function(){return _1v(_b4,new T(function(){return _aA(A(_b5[1],[_b7]),_b7);}));})];}];default:return [4,new T(function(){return _1v(_b4,_b5[1]);})];}break;default:return E(_aR);}}}}}),_b8=E(_aL);switch(_b8[0]){case 0:var _b9=E(_aM);return _b9[0]==0?[0,function(_ba){return _aK(A(_b8[1],[_ba]),new T(function(){return A(_b9[1],[_ba]);}));}]:E(_aN);case 3:return [3,_b8[1],new T(function(){return _aK(_b8[2],_aM);})];default:return E(_aN);}},_bb=function(_bc,_bd){return E(_bc)[1]!=E(_bd)[1];},_be=function(_bf,_bg){return E(_bf)[1]==E(_bg)[1];},_bh=[0,_be,_bb],_bi=function(_bj){return E(E(_bj)[1]);},_bk=function(_bl,_bm,_bn){while(1){var _bo=E(_bm);if(!_bo[0]){return E(_bn)[0]==0?true:false;}else{var _bp=E(_bn);if(!_bp[0]){return false;}else{if(!A(_bi,[_bl,_bo[1],_bp[1]])){return false;}else{_bm=_bo[2];_bn=_bp[2];continue;}}}}},_bq=function(_br,_bs,_bt){return !_bk(_br,_bs,_bt)?true:false;},_bu=function(_bv){return [0,function(_bw,_bx){return _bk(_bv,_bw,_bx);},function(_bw,_bx){return _bq(_bv,_bw,_bx);}];},_by=new T(function(){return _bu(_bh);}),_bz=function(_bA,_bB){var _bC=E(_bA);switch(_bC[0]){case 0:return [0,function(_bD){return _bz(A(_bC[1],[_bD]),_bB);}];case 1:return [1,function(_bE){return _bz(A(_bC[1],[_bE]),_bB);}];case 2:return [2];case 3:return _aK(A(_bB,[_bC[1]]),new T(function(){return _bz(_bC[2],_bB);}));default:var _bF=function(_bG){var _bH=E(_bG);if(!_bH[0]){return [0];}else{var _bI=E(_bH[1]);return _1v(_aA(A(_bB,[_bI[1]]),_bI[2]),new T(function(){return _bF(_bH[2]);}));}},_bJ=_bF(_bC[1]);return _bJ[0]==0?[2]:[4,_bJ];}},_bK=[2],_bL=function(_bM){return [3,_bM,_bK];},_bN=function(_bO,_bP){var _bQ=E(_bO);if(!_bQ){return A(_bP,[_0]);}else{var _bR=new T(function(){return _bN(_bQ-1|0,_bP);});return [0,function(_bS){return E(_bR);}];}},_bT=function(_bU,_bV,_bW){var _bX=new T(function(){return A(_bU,[_bL]);});return [1,function(_bY){return A(function(_bZ,_c0,_c1){while(1){var _c2=(function(_c3,_c4,_c5){var _c6=E(_c3);switch(_c6[0]){case 0:var _c7=E(_c4);if(!_c7[0]){return E(_bV);}else{_bZ=A(_c6[1],[_c7[1]]);_c0=_c7[2];var _c8=_c5+1|0;_c1=_c8;return null;}break;case 1:var _c9=A(_c6[1],[_c4]),_ca=_c4,_c8=_c5;_bZ=_c9;_c0=_ca;_c1=_c8;return null;case 2:return E(_bV);case 3:return function(_cb){var _cc=new T(function(){return _bz(_c6,_cb);});return _bN(_c5,function(_cd){return E(_cc);});};default:return function(_ce){return _bz(_c6,_ce);};}})(_bZ,_c0,_c1);if(_c2!=null){return _c2;}}},[_bX,_bY,0,_bW]);}];},_cf=[6],_cg=unCStr("valDig: Bad base"),_ch=new T(function(){return err(_cg);}),_ci=function(_cj,_ck){var _cl=function(_cm,_cn){var _co=E(_cm);if(!_co[0]){var _cp=new T(function(){return A(_cn,[_9]);});return function(_cq){return A(_cq,[_cp]);};}else{var _cr=E(_co[1])[1],_cs=function(_ct){var _cu=new T(function(){return _cl(_co[2],function(_cv){return A(_cn,[[1,_ct,_cv]]);});});return function(_cw){var _cx=new T(function(){return A(_cu,[_cw]);});return [0,function(_cy){return E(_cx);}];};};switch(E(E(_cj)[1])){case 8:if(48>_cr){var _cz=new T(function(){return A(_cn,[_9]);});return function(_cA){return A(_cA,[_cz]);};}else{if(_cr>55){var _cB=new T(function(){return A(_cn,[_9]);});return function(_cC){return A(_cC,[_cB]);};}else{return _cs([0,_cr-48|0]);}}break;case 10:if(48>_cr){var _cD=new T(function(){return A(_cn,[_9]);});return function(_cE){return A(_cE,[_cD]);};}else{if(_cr>57){var _cF=new T(function(){return A(_cn,[_9]);});return function(_cG){return A(_cG,[_cF]);};}else{return _cs([0,_cr-48|0]);}}break;case 16:var _cH=new T(function(){return 97>_cr?65>_cr?[0]:_cr>70?[0]:[1,[0,(_cr-65|0)+10|0]]:_cr>102?65>_cr?[0]:_cr>70?[0]:[1,[0,(_cr-65|0)+10|0]]:[1,[0,(_cr-97|0)+10|0]];});if(48>_cr){var _cI=E(_cH);if(!_cI[0]){var _cJ=new T(function(){return A(_cn,[_9]);});return function(_cK){return A(_cK,[_cJ]);};}else{return _cs(_cI[1]);}}else{if(_cr>57){var _cL=E(_cH);if(!_cL[0]){var _cM=new T(function(){return A(_cn,[_9]);});return function(_cN){return A(_cN,[_cM]);};}else{return _cs(_cL[1]);}}else{return _cs([0,_cr-48|0]);}}break;default:return E(_ch);}}};return [1,function(_cO){return A(_cl,[_cO,_n,function(_cP){var _cQ=E(_cP);return _cQ[0]==0?[2]:A(_ck,[_cQ]);}]);}];},_cR=[0,10],_cS=[0,1],_cT=[0,2147483647],_cU=function(_cV,_cW){while(1){var _cX=E(_cV);if(!_cX[0]){var _cY=_cX[1],_cZ=E(_cW);if(!_cZ[0]){var _d0=_cZ[1],_d1=addC(_cY,_d0);if(!E(_d1[2])){return [0,_d1[1]];}else{_cV=[1,I_fromInt(_cY)];_cW=[1,I_fromInt(_d0)];continue;}}else{_cV=[1,I_fromInt(_cY)];_cW=_cZ;continue;}}else{var _d2=E(_cW);if(!_d2[0]){_cV=_cX;_cW=[1,I_fromInt(_d2[1])];continue;}else{return [1,I_add(_cX[1],_d2[1])];}}}},_d3=new T(function(){return _cU(_cT,_cS);}),_d4=function(_d5){var _d6=E(_d5);if(!_d6[0]){var _d7=E(_d6[1]);return _d7==(-2147483648)?E(_d3):[0, -_d7];}else{return [1,I_negate(_d6[1])];}},_d8=[0,10],_d9=[0,0],_da=function(_db,_dc){while(1){var _dd=E(_db);if(!_dd[0]){var _de=_dd[1],_df=E(_dc);if(!_df[0]){var _dg=_df[1];if(!(imul(_de,_dg)|0)){return [0,imul(_de,_dg)|0];}else{_db=[1,I_fromInt(_de)];_dc=[1,I_fromInt(_dg)];continue;}}else{_db=[1,I_fromInt(_de)];_dc=_df;continue;}}else{var _dh=E(_dc);if(!_dh[0]){_db=_dd;_dc=[1,I_fromInt(_dh[1])];continue;}else{return [1,I_mul(_dd[1],_dh[1])];}}}},_di=function(_dj,_dk,_dl){while(1){var _dm=E(_dl);if(!_dm[0]){return E(_dk);}else{var _dn=_cU(_da(_dk,_dj),_dm[1]);_dl=_dm[2];_dk=_dn;continue;}}},_do=function(_dp){var _dq=new T(function(){return _aK(_aK([0,function(_dr){return E(E(_dr)[1])==45?_ci(_cR,function(_ds){return A(_dp,[[1,new T(function(){return _d4(_di(_d8,_d9,_ds));})]]);}):[2];}],[0,function(_dt){return E(E(_dt)[1])==43?_ci(_cR,function(_du){return A(_dp,[[1,new T(function(){return _di(_d8,_d9,_du);})]]);}):[2];}]),new T(function(){return _ci(_cR,function(_dv){return A(_dp,[[1,new T(function(){return _di(_d8,_d9,_dv);})]]);});}));});return _aK([0,function(_dw){return E(E(_dw)[1])==101?E(_dq):[2];}],[0,function(_dx){return E(E(_dx)[1])==69?E(_dq):[2];}]);},_dy=function(_dz){return A(_dz,[_a]);},_dA=function(_dB){return A(_dB,[_a]);},_dC=function(_dD){var _dE=new T(function(){return _ci(_cR,function(_dF){return A(_dD,[[1,_dF]]);});});return [0,function(_dG){return E(E(_dG)[1])==46?E(_dE):[2];}];},_dH=function(_dI){return _ci(_cR,function(_dJ){return _bT(_dC,_dy,function(_dK){return _bT(_do,_dA,function(_dL){return A(_dI,[[5,[1,_dJ,_dK,_dL]]]);});});});},_dM=function(_dN,_dO,_dP){while(1){var _dQ=E(_dP);if(!_dQ[0]){return false;}else{if(!A(_bi,[_dN,_dO,_dQ[1]])){_dP=_dQ[2];continue;}else{return true;}}}},_dR=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dS=function(_dT){return _dM(_bh,_dT,_dR);},_dU=[0,8],_dV=[0,16],_dW=function(_dX){var _dY=new T(function(){return _ci(_dV,function(_dZ){return A(_dX,[[5,[0,_dV,_dZ]]]);});}),_e0=new T(function(){return _ci(_dU,function(_e1){return A(_dX,[[5,[0,_dU,_e1]]]);});}),_e2=new T(function(){return _ci(_dV,function(_e3){return A(_dX,[[5,[0,_dV,_e3]]]);});}),_e4=new T(function(){return _ci(_dU,function(_e5){return A(_dX,[[5,[0,_dU,_e5]]]);});});return [0,function(_e6){return E(E(_e6)[1])==48?E([0,function(_e7){switch(E(E(_e7)[1])){case 79:return E(_e4);case 88:return E(_e2);case 111:return E(_e0);case 120:return E(_dY);default:return [2];}}]):[2];}];},_e8=false,_e9=true,_ea=function(_eb){var _ec=new T(function(){return A(_eb,[_dV]);}),_ed=new T(function(){return A(_eb,[_dU]);}),_ee=new T(function(){return A(_eb,[_dV]);}),_ef=new T(function(){return A(_eb,[_dU]);});return [0,function(_eg){switch(E(E(_eg)[1])){case 79:return E(_ef);case 88:return E(_ee);case 111:return E(_ed);case 120:return E(_ec);default:return [2];}}];},_eh=function(_ei){return A(_ei,[_cR]);},_ej=function(_ek){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3u(9,_ek,_9);})));},_el=function(_em){var _en=E(_em);return _en[0]==0?E(_en[1]):I_toInt(_en[1]);},_eo=function(_ep,_eq){var _er=E(_ep);if(!_er[0]){var _es=_er[1],_et=E(_eq);return _et[0]==0?_es<=_et[1]:I_compareInt(_et[1],_es)>=0;}else{var _eu=_er[1],_ev=E(_eq);return _ev[0]==0?I_compareInt(_eu,_ev[1])<=0:I_compare(_eu,_ev[1])<=0;}},_ew=function(_ex){return [2];},_ey=function(_ez){var _eA=E(_ez);if(!_eA[0]){return E(_ew);}else{var _eB=_eA[1],_eC=E(_eA[2]);if(!_eC[0]){return E(_eB);}else{var _eD=new T(function(){return _ey(_eC);});return function(_eE){return _aK(A(_eB,[_eE]),new T(function(){return A(_eD,[_eE]);}));};}}},_eF=unCStr("NUL"),_eG=function(_eH){return [2];},_eI=function(_eJ){return _eG(_eJ);},_eK=function(_eL,_eM){var _eN=function(_eO,_eP){var _eQ=E(_eO);if(!_eQ[0]){return function(_eR){return A(_eR,[_eL]);};}else{var _eS=E(_eP);if(!_eS[0]){return E(_eG);}else{if(E(_eQ[1])[1]!=E(_eS[1])[1]){return E(_eI);}else{var _eT=new T(function(){return _eN(_eQ[2],_eS[2]);});return function(_eU){var _eV=new T(function(){return A(_eT,[_eU]);});return [0,function(_eW){return E(_eV);}];};}}}};return [1,function(_eX){return A(_eN,[_eL,_eX,_eM]);}];},_eY=[0,0],_eZ=function(_f0){var _f1=new T(function(){return A(_f0,[_eY]);});return _eK(_eF,function(_f2){return E(_f1);});},_f3=unCStr("STX"),_f4=[0,2],_f5=function(_f6){var _f7=new T(function(){return A(_f6,[_f4]);});return _eK(_f3,function(_f8){return E(_f7);});},_f9=unCStr("ETX"),_fa=[0,3],_fb=function(_fc){var _fd=new T(function(){return A(_fc,[_fa]);});return _eK(_f9,function(_fe){return E(_fd);});},_ff=unCStr("EOT"),_fg=[0,4],_fh=function(_fi){var _fj=new T(function(){return A(_fi,[_fg]);});return _eK(_ff,function(_fk){return E(_fj);});},_fl=unCStr("ENQ"),_fm=[0,5],_fn=function(_fo){var _fp=new T(function(){return A(_fo,[_fm]);});return _eK(_fl,function(_fq){return E(_fp);});},_fr=unCStr("ACK"),_fs=[0,6],_ft=function(_fu){var _fv=new T(function(){return A(_fu,[_fs]);});return _eK(_fr,function(_fw){return E(_fv);});},_fx=unCStr("BEL"),_fy=[0,7],_fz=function(_fA){var _fB=new T(function(){return A(_fA,[_fy]);});return _eK(_fx,function(_fC){return E(_fB);});},_fD=unCStr("BS"),_fE=[0,8],_fF=function(_fG){var _fH=new T(function(){return A(_fG,[_fE]);});return _eK(_fD,function(_fI){return E(_fH);});},_fJ=unCStr("HT"),_fK=[0,9],_fL=function(_fM){var _fN=new T(function(){return A(_fM,[_fK]);});return _eK(_fJ,function(_fO){return E(_fN);});},_fP=unCStr("LF"),_fQ=[0,10],_fR=function(_fS){var _fT=new T(function(){return A(_fS,[_fQ]);});return _eK(_fP,function(_fU){return E(_fT);});},_fV=unCStr("VT"),_fW=[0,11],_fX=function(_fY){var _fZ=new T(function(){return A(_fY,[_fW]);});return _eK(_fV,function(_g0){return E(_fZ);});},_g1=unCStr("FF"),_g2=[0,12],_g3=function(_g4){var _g5=new T(function(){return A(_g4,[_g2]);});return _eK(_g1,function(_g6){return E(_g5);});},_g7=unCStr("CR"),_g8=[0,13],_g9=function(_ga){var _gb=new T(function(){return A(_ga,[_g8]);});return _eK(_g7,function(_gc){return E(_gb);});},_gd=unCStr("SI"),_ge=[0,15],_gf=function(_gg){var _gh=new T(function(){return A(_gg,[_ge]);});return _eK(_gd,function(_gi){return E(_gh);});},_gj=unCStr("DLE"),_gk=[0,16],_gl=function(_gm){var _gn=new T(function(){return A(_gm,[_gk]);});return _eK(_gj,function(_go){return E(_gn);});},_gp=unCStr("DC1"),_gq=[0,17],_gr=function(_gs){var _gt=new T(function(){return A(_gs,[_gq]);});return _eK(_gp,function(_gu){return E(_gt);});},_gv=unCStr("DC2"),_gw=[0,18],_gx=function(_gy){var _gz=new T(function(){return A(_gy,[_gw]);});return _eK(_gv,function(_gA){return E(_gz);});},_gB=unCStr("DC3"),_gC=[0,19],_gD=function(_gE){var _gF=new T(function(){return A(_gE,[_gC]);});return _eK(_gB,function(_gG){return E(_gF);});},_gH=unCStr("DC4"),_gI=[0,20],_gJ=function(_gK){var _gL=new T(function(){return A(_gK,[_gI]);});return _eK(_gH,function(_gM){return E(_gL);});},_gN=unCStr("NAK"),_gO=[0,21],_gP=function(_gQ){var _gR=new T(function(){return A(_gQ,[_gO]);});return _eK(_gN,function(_gS){return E(_gR);});},_gT=unCStr("SYN"),_gU=[0,22],_gV=function(_gW){var _gX=new T(function(){return A(_gW,[_gU]);});return _eK(_gT,function(_gY){return E(_gX);});},_gZ=unCStr("ETB"),_h0=[0,23],_h1=function(_h2){var _h3=new T(function(){return A(_h2,[_h0]);});return _eK(_gZ,function(_h4){return E(_h3);});},_h5=unCStr("CAN"),_h6=[0,24],_h7=function(_h8){var _h9=new T(function(){return A(_h8,[_h6]);});return _eK(_h5,function(_ha){return E(_h9);});},_hb=unCStr("EM"),_hc=[0,25],_hd=function(_he){var _hf=new T(function(){return A(_he,[_hc]);});return _eK(_hb,function(_hg){return E(_hf);});},_hh=unCStr("SUB"),_hi=[0,26],_hj=function(_hk){var _hl=new T(function(){return A(_hk,[_hi]);});return _eK(_hh,function(_hm){return E(_hl);});},_hn=unCStr("ESC"),_ho=[0,27],_hp=function(_hq){var _hr=new T(function(){return A(_hq,[_ho]);});return _eK(_hn,function(_hs){return E(_hr);});},_ht=unCStr("FS"),_hu=[0,28],_hv=function(_hw){var _hx=new T(function(){return A(_hw,[_hu]);});return _eK(_ht,function(_hy){return E(_hx);});},_hz=unCStr("GS"),_hA=[0,29],_hB=function(_hC){var _hD=new T(function(){return A(_hC,[_hA]);});return _eK(_hz,function(_hE){return E(_hD);});},_hF=unCStr("RS"),_hG=[0,30],_hH=function(_hI){var _hJ=new T(function(){return A(_hI,[_hG]);});return _eK(_hF,function(_hK){return E(_hJ);});},_hL=unCStr("US"),_hM=[0,31],_hN=function(_hO){var _hP=new T(function(){return A(_hO,[_hM]);});return _eK(_hL,function(_hQ){return E(_hP);});},_hR=unCStr("SP"),_hS=[0,32],_hT=function(_hU){var _hV=new T(function(){return A(_hU,[_hS]);});return _eK(_hR,function(_hW){return E(_hV);});},_hX=unCStr("DEL"),_hY=[0,127],_hZ=function(_i0){var _i1=new T(function(){return A(_i0,[_hY]);});return _eK(_hX,function(_i2){return E(_i1);});},_i3=[1,_hZ,_9],_i4=[1,_hT,_i3],_i5=[1,_hN,_i4],_i6=[1,_hH,_i5],_i7=[1,_hB,_i6],_i8=[1,_hv,_i7],_i9=[1,_hp,_i8],_ia=[1,_hj,_i9],_ib=[1,_hd,_ia],_ic=[1,_h7,_ib],_id=[1,_h1,_ic],_ie=[1,_gV,_id],_if=[1,_gP,_ie],_ig=[1,_gJ,_if],_ih=[1,_gD,_ig],_ii=[1,_gx,_ih],_ij=[1,_gr,_ii],_ik=[1,_gl,_ij],_il=[1,_gf,_ik],_im=[1,_g9,_il],_in=[1,_g3,_im],_io=[1,_fX,_in],_ip=[1,_fR,_io],_iq=[1,_fL,_ip],_ir=[1,_fF,_iq],_is=[1,_fz,_ir],_it=[1,_ft,_is],_iu=[1,_fn,_it],_iv=[1,_fh,_iu],_iw=[1,_fb,_iv],_ix=[1,_f5,_iw],_iy=[1,_eZ,_ix],_iz=unCStr("SOH"),_iA=[0,1],_iB=function(_iC){var _iD=new T(function(){return A(_iC,[_iA]);});return _eK(_iz,function(_iE){return E(_iD);});},_iF=unCStr("SO"),_iG=[0,14],_iH=function(_iI){var _iJ=new T(function(){return A(_iI,[_iG]);});return _eK(_iF,function(_iK){return E(_iJ);});},_iL=function(_iM){return _bT(_iB,_iH,_iM);},_iN=[1,_iL,_iy],_iO=new T(function(){return _ey(_iN);}),_iP=[0,1114111],_iQ=[0,34],_iR=[0,_iQ,_e9],_iS=[0,39],_iT=[0,_iS,_e9],_iU=[0,92],_iV=[0,_iU,_e9],_iW=[0,_fy,_e9],_iX=[0,_fE,_e9],_iY=[0,_g2,_e9],_iZ=[0,_fQ,_e9],_j0=[0,_g8,_e9],_j1=[0,_fK,_e9],_j2=[0,_fW,_e9],_j3=[0,_eY,_e9],_j4=[0,_iA,_e9],_j5=[0,_f4,_e9],_j6=[0,_fa,_e9],_j7=[0,_fg,_e9],_j8=[0,_fm,_e9],_j9=[0,_fs,_e9],_ja=[0,_fy,_e9],_jb=[0,_fE,_e9],_jc=[0,_fK,_e9],_jd=[0,_fQ,_e9],_je=[0,_fW,_e9],_jf=[0,_g2,_e9],_jg=[0,_g8,_e9],_jh=[0,_iG,_e9],_ji=[0,_ge,_e9],_jj=[0,_gk,_e9],_jk=[0,_gq,_e9],_jl=[0,_gw,_e9],_jm=[0,_gC,_e9],_jn=[0,_gI,_e9],_jo=[0,_gO,_e9],_jp=[0,_gU,_e9],_jq=[0,_h0,_e9],_jr=[0,_h6,_e9],_js=[0,_hc,_e9],_jt=[0,_hi,_e9],_ju=[0,_ho,_e9],_jv=[0,_hu,_e9],_jw=[0,_hA,_e9],_jx=[0,_hG,_e9],_jy=[0,_hM,_e9],_jz=function(_jA){return [0,_jA];},_jB=function(_jC){var _jD=new T(function(){return A(_jC,[_j2]);}),_jE=new T(function(){return A(_jC,[_j1]);}),_jF=new T(function(){return A(_jC,[_j0]);}),_jG=new T(function(){return A(_jC,[_iZ]);}),_jH=new T(function(){return A(_jC,[_iY]);}),_jI=new T(function(){return A(_jC,[_iX]);}),_jJ=new T(function(){return A(_jC,[_iW]);}),_jK=new T(function(){return A(_jC,[_iV]);}),_jL=new T(function(){return A(_jC,[_iT]);}),_jM=new T(function(){return A(_jC,[_iR]);});return _aK([0,function(_jN){switch(E(E(_jN)[1])){case 34:return E(_jM);case 39:return E(_jL);case 92:return E(_jK);case 97:return E(_jJ);case 98:return E(_jI);case 102:return E(_jH);case 110:return E(_jG);case 114:return E(_jF);case 116:return E(_jE);case 118:return E(_jD);default:return [2];}}],new T(function(){return _aK(_bT(_ea,_eh,function(_jO){var _jP=new T(function(){return _jz(E(_jO)[1]);});return _ci(_jO,function(_jQ){var _jR=_di(_jP,_d9,_jQ);return !_eo(_jR,_iP)?[2]:A(_jC,[[0,new T(function(){var _jS=_el(_jR);return _jS>>>0>1114111?_ej(_jS):[0,_jS];}),_e9]]);});}),new T(function(){var _jT=new T(function(){return A(_jC,[_jy]);}),_jU=new T(function(){return A(_jC,[_jx]);}),_jV=new T(function(){return A(_jC,[_jw]);}),_jW=new T(function(){return A(_jC,[_jv]);}),_jX=new T(function(){return A(_jC,[_ju]);}),_jY=new T(function(){return A(_jC,[_jt]);}),_jZ=new T(function(){return A(_jC,[_js]);}),_k0=new T(function(){return A(_jC,[_jr]);}),_k1=new T(function(){return A(_jC,[_jq]);}),_k2=new T(function(){return A(_jC,[_jp]);}),_k3=new T(function(){return A(_jC,[_jo]);}),_k4=new T(function(){return A(_jC,[_jn]);}),_k5=new T(function(){return A(_jC,[_jm]);}),_k6=new T(function(){return A(_jC,[_jl]);}),_k7=new T(function(){return A(_jC,[_jk]);}),_k8=new T(function(){return A(_jC,[_jj]);}),_k9=new T(function(){return A(_jC,[_ji]);}),_ka=new T(function(){return A(_jC,[_jh]);}),_kb=new T(function(){return A(_jC,[_jg]);}),_kc=new T(function(){return A(_jC,[_jf]);}),_kd=new T(function(){return A(_jC,[_je]);}),_ke=new T(function(){return A(_jC,[_jd]);}),_kf=new T(function(){return A(_jC,[_jc]);}),_kg=new T(function(){return A(_jC,[_jb]);}),_kh=new T(function(){return A(_jC,[_ja]);}),_ki=new T(function(){return A(_jC,[_j9]);}),_kj=new T(function(){return A(_jC,[_j8]);}),_kk=new T(function(){return A(_jC,[_j7]);}),_kl=new T(function(){return A(_jC,[_j6]);}),_km=new T(function(){return A(_jC,[_j5]);}),_kn=new T(function(){return A(_jC,[_j4]);}),_ko=new T(function(){return A(_jC,[_j3]);});return _aK([0,function(_kp){return E(E(_kp)[1])==94?E([0,function(_kq){switch(E(E(_kq)[1])){case 64:return E(_ko);case 65:return E(_kn);case 66:return E(_km);case 67:return E(_kl);case 68:return E(_kk);case 69:return E(_kj);case 70:return E(_ki);case 71:return E(_kh);case 72:return E(_kg);case 73:return E(_kf);case 74:return E(_ke);case 75:return E(_kd);case 76:return E(_kc);case 77:return E(_kb);case 78:return E(_ka);case 79:return E(_k9);case 80:return E(_k8);case 81:return E(_k7);case 82:return E(_k6);case 83:return E(_k5);case 84:return E(_k4);case 85:return E(_k3);case 86:return E(_k2);case 87:return E(_k1);case 88:return E(_k0);case 89:return E(_jZ);case 90:return E(_jY);case 91:return E(_jX);case 92:return E(_jW);case 93:return E(_jV);case 94:return E(_jU);case 95:return E(_jT);default:return [2];}}]):[2];}],new T(function(){return A(_iO,[function(_kr){return A(_jC,[[0,_kr,_e9]]);}]);}));}));}));},_ks=function(_kt){return A(_kt,[_0]);},_ku=function(_kv){var _kw=E(_kv);if(!_kw[0]){return E(_ks);}else{var _kx=_kw[2],_ky=E(E(_kw[1])[1]);switch(_ky){case 9:var _kz=new T(function(){return _ku(_kx);});return function(_kA){var _kB=new T(function(){return A(_kz,[_kA]);});return [0,function(_kC){return E(_kB);}];};case 10:var _kD=new T(function(){return _ku(_kx);});return function(_kE){var _kF=new T(function(){return A(_kD,[_kE]);});return [0,function(_kG){return E(_kF);}];};case 11:var _kH=new T(function(){return _ku(_kx);});return function(_kI){var _kJ=new T(function(){return A(_kH,[_kI]);});return [0,function(_kK){return E(_kJ);}];};case 12:var _kL=new T(function(){return _ku(_kx);});return function(_kM){var _kN=new T(function(){return A(_kL,[_kM]);});return [0,function(_kO){return E(_kN);}];};case 13:var _kP=new T(function(){return _ku(_kx);});return function(_kQ){var _kR=new T(function(){return A(_kP,[_kQ]);});return [0,function(_kS){return E(_kR);}];};case 32:var _kT=new T(function(){return _ku(_kx);});return function(_kU){var _kV=new T(function(){return A(_kT,[_kU]);});return [0,function(_kW){return E(_kV);}];};case 160:var _kX=new T(function(){return _ku(_kx);});return function(_kY){var _kZ=new T(function(){return A(_kX,[_kY]);});return [0,function(_l0){return E(_kZ);}];};default:var _l1=u_iswspace(_ky);if(!E(_l1)){return E(_ks);}else{var _l2=new T(function(){return _ku(_kx);});return function(_l3){var _l4=new T(function(){return A(_l2,[_l3]);});return [0,function(_l5){return E(_l4);}];};}}}},_l6=function(_l7){var _l8=new T(function(){return _jB(_l7);}),_l9=new T(function(){return _l6(_l7);}),_la=[1,function(_lb){return A(_ku,[_lb,function(_lc){return E([0,function(_ld){return E(E(_ld)[1])==92?E(_l9):[2];}]);}]);}];return _aK([0,function(_le){return E(E(_le)[1])==92?E([0,function(_lf){var _lg=E(E(_lf)[1]);switch(_lg){case 9:return E(_la);case 10:return E(_la);case 11:return E(_la);case 12:return E(_la);case 13:return E(_la);case 32:return E(_la);case 38:return E(_l9);case 160:return E(_la);default:var _lh=u_iswspace(_lg);return E(_lh)==0?[2]:E(_la);}}]):[2];}],[0,function(_li){var _lj=E(_li);return E(_lj[1])==92?E(_l8):A(_l7,[[0,_lj,_e8]]);}]);},_lk=function(_ll,_lm){var _ln=new T(function(){return A(_lm,[[1,new T(function(){return A(_ll,[_9]);})]]);});return _l6(function(_lo){var _lp=E(_lo),_lq=E(_lp[1]);return E(_lq[1])==34?!E(_lp[2])?E(_ln):_lk(function(_lr){return A(_ll,[[1,_lq,_lr]]);},_lm):_lk(function(_ls){return A(_ll,[[1,_lq,_ls]]);},_lm);});},_lt=unCStr("_\'"),_lu=function(_lv){var _lw=u_iswalnum(_lv);return E(_lw)==0?_dM(_bh,[0,_lv],_lt):true;},_lx=function(_ly){return _lu(E(_ly)[1]);},_lz=unCStr(",;()[]{}`"),_lA=function(_lB){return A(_lB,[_9]);},_lC=function(_lD,_lE){var _lF=function(_lG){var _lH=E(_lG);if(!_lH[0]){return E(_lA);}else{var _lI=_lH[1];if(!A(_lD,[_lI])){return E(_lA);}else{var _lJ=new T(function(){return _lF(_lH[2]);});return function(_lK){var _lL=new T(function(){return A(_lJ,[function(_lM){return A(_lK,[[1,_lI,_lM]]);}]);});return [0,function(_lN){return E(_lL);}];};}}};return [1,function(_lO){return A(_lF,[_lO,_lE]);}];},_lP=unCStr(".."),_lQ=unCStr("::"),_lR=unCStr("->"),_lS=[0,64],_lT=[1,_lS,_9],_lU=[0,126],_lV=[1,_lU,_9],_lW=unCStr("=>"),_lX=[1,_lW,_9],_lY=[1,_lV,_lX],_lZ=[1,_lT,_lY],_m0=[1,_lR,_lZ],_m1=unCStr("<-"),_m2=[1,_m1,_m0],_m3=[0,124],_m4=[1,_m3,_9],_m5=[1,_m4,_m2],_m6=[1,_iU,_9],_m7=[1,_m6,_m5],_m8=[0,61],_m9=[1,_m8,_9],_ma=[1,_m9,_m7],_mb=[1,_lQ,_ma],_mc=[1,_lP,_mb],_md=function(_me){var _mf=new T(function(){return A(_me,[_cf]);});return _aK([1,function(_mg){return E(_mg)[0]==0?E(_mf):[2];}],new T(function(){var _mh=new T(function(){return _jB(function(_mi){var _mj=E(_mi);return (function(_mk,_ml){var _mm=new T(function(){return A(_me,[[0,_mk]]);});return !E(_ml)?E(E(_mk)[1])==39?[2]:[0,function(_mn){return E(E(_mn)[1])==39?E(_mm):[2];}]:[0,function(_mo){return E(E(_mo)[1])==39?E(_mm):[2];}];})(_mj[1],_mj[2]);});});return _aK([0,function(_mp){return E(E(_mp)[1])==39?E([0,function(_mq){var _mr=E(_mq);switch(E(_mr[1])){case 39:return [2];case 92:return E(_mh);default:var _ms=new T(function(){return A(_me,[[0,_mr]]);});return [0,function(_mt){return E(E(_mt)[1])==39?E(_ms):[2];}];}}]):[2];}],new T(function(){var _mu=new T(function(){return _lk(_n,_me);});return _aK([0,function(_mv){return E(E(_mv)[1])==34?E(_mu):[2];}],new T(function(){return _aK([0,function(_mw){return !_dM(_bh,_mw,_lz)?[2]:A(_me,[[2,[1,_mw,_9]]]);}],new T(function(){return _aK([0,function(_mx){return !_dM(_bh,_mx,_dR)?[2]:_lC(_dS,function(_my){var _mz=[1,_mx,_my];return !_dM(_by,_mz,_mc)?A(_me,[[4,_mz]]):A(_me,[[2,_mz]]);});}],new T(function(){return _aK([0,function(_mA){var _mB=E(_mA),_mC=_mB[1],_mD=u_iswalpha(_mC);return E(_mD)==0?E(_mC)==95?_lC(_lx,function(_mE){return A(_me,[[3,[1,_mB,_mE]]]);}):[2]:_lC(_lx,function(_mF){return A(_me,[[3,[1,_mB,_mF]]]);});}],new T(function(){return _bT(_dW,_dH,_me);}));}));}));}));}));}));},_mG=function(_mH){var _mI=new T(function(){return _md(_mH);});return [1,function(_mJ){return A(_ku,[_mJ,function(_mK){return E(_mI);}]);}];},_mL=[0,0],_mM=function(_mN,_mO){var _mP=new T(function(){return A(_mN,[_mL,function(_mQ){var _mR=new T(function(){return A(_mO,[_mQ]);});return _mG(function(_mS){var _mT=E(_mS);if(_mT[0]==2){var _mU=E(_mT[1]);return _mU[0]==0?[2]:E(E(_mU[1])[1])==41?E(_mU[2])[0]==0?E(_mR):[2]:[2];}else{return [2];}});}]);});return _mG(function(_mV){var _mW=E(_mV);if(_mW[0]==2){var _mX=E(_mW[1]);return _mX[0]==0?[2]:E(E(_mX[1])[1])==40?E(_mX[2])[0]==0?E(_mP):[2]:[2];}else{return [2];}});},_mY=function(_mZ,_n0,_n1){var _n2=function(_n3,_n4){var _n5=new T(function(){return _md(function(_n6){return A(_mZ,[_n6,_n3,function(_n7){return A(_n4,[new T(function(){return [0, -E(_n7)[1]];})]);}]);});});return _aK(_mG(function(_n8){var _n9=E(_n8);if(_n9[0]==4){var _na=E(_n9[1]);return _na[0]==0?A(_mZ,[_n9,_n3,_n4]):E(E(_na[1])[1])==45?E(_na[2])[0]==0?E([1,function(_nb){return A(_ku,[_nb,function(_nc){return E(_n5);}]);}]):A(_mZ,[_n9,_n3,_n4]):A(_mZ,[_n9,_n3,_n4]);}else{return A(_mZ,[_n9,_n3,_n4]);}}),new T(function(){return _mM(_n2,_n4);}));};return _n2(_n0,_n1);},_nd=function(_ne,_nf){return [2];},_ng=function(_nh,_ni){return _nd(_nh,_ni);},_nj=function(_nk){var _nl=E(_nk);return _nl[0]==0?[1,new T(function(){return _di(new T(function(){return _jz(E(_nl[1])[1]);}),_d9,_nl[2]);})]:E(_nl[2])[0]==0?E(_nl[3])[0]==0?[1,new T(function(){return _di(_d8,_d9,_nl[1]);})]:[0]:[0];},_nm=function(_nn){var _no=E(_nn);if(_no[0]==5){var _np=_nj(_no[1]);if(!_np[0]){return E(_nd);}else{var _nq=new T(function(){return [0,_el(_np[1])];});return function(_nr,_ns){return A(_ns,[_nq]);};}}else{return E(_ng);}},_nt=function(_nh,_ni){return _mY(_nm,_nh,_ni);},_nu=function(_nv,_nw){var _nx=function(_ny,_nz){var _nA=new T(function(){return A(_nz,[_9]);}),_nB=new T(function(){return A(_nv,[_mL,function(_nC){return _nx(_e9,function(_nD){return A(_nz,[[1,_nC,_nD]]);});}]);});return _mG(function(_nE){var _nF=E(_nE);if(_nF[0]==2){var _nG=E(_nF[1]);if(!_nG[0]){return [2];}else{var _nH=_nG[2];switch(E(E(_nG[1])[1])){case 44:return E(_nH)[0]==0?!E(_ny)?[2]:E(_nB):[2];case 93:return E(_nH)[0]==0?E(_nA):[2];default:return [2];}}}else{return [2];}});},_nI=function(_nJ){var _nK=new T(function(){return _aK(_nx(_e8,_nJ),new T(function(){return A(_nv,[_mL,function(_nL){return _nx(_e9,function(_nM){return A(_nJ,[[1,_nL,_nM]]);});}]);}));});return _aK(_mG(function(_nN){var _nO=E(_nN);if(_nO[0]==2){var _nP=E(_nO[1]);return _nP[0]==0?[2]:E(E(_nP[1])[1])==91?E(_nP[2])[0]==0?E(_nK):[2]:[2];}else{return [2];}}),new T(function(){return _mM(function(_nQ,_nR){return _nI(_nR);},_nJ);}));};return _nI(_nw);},_nS=function(_nT,_nU){return _nu(_nt,_nU);},_nV=new T(function(){return _nu(_nt,_bL);}),_nW=function(_ni){return _aA(_nV,_ni);},_nX=function(_nY){var _nZ=new T(function(){return _mY(_nm,_nY,_bL);});return function(_ce){return _aA(_nZ,_ce);};},_o0=[0,_nX,_nW,_nt,_nS],_o1=function(_o2,_o3){return _3u(0,E(_o2)[1],_o3);},_o4=function(_o5,_o6){return _2l(_o1,_o5,_o6);},_o7=function(_o8,_o9,_oa){return _3u(E(_o8)[1],E(_o9)[1],_oa);},_ob=[0,_o7,_54,_o4],_oc=unCStr("GHC.Types"),_od=unCStr("Int"),_oe=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5K,_oc,_od],_of=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oe,_9],_og=function(_oh){return E(_of);},_oi=function(_oj){return E(E(_oj)[1]);},_ok=function(_ol){return E(E(_ol)[2]);},_om=function(_on,_oo){var _op=new T(function(){return A(_ok,[_on,_oo]);}),_oq=new T(function(){return _oi(_on);}),_or=new T(function(){return _3f(_oq);}),_os=new T(function(){return _2P(_oq);});return function(_ot){return A(_os,[_op,function(_ou){return A(_or,[[0,_ou,_ot]]);}]);};},_ov=function(_ow,_ox){return [0,_ow,function(_oy){return _om(_ox,_oy);}];},_oz=function(_oA,_oB){return A(_3f,[_oA,[0,_oB,_oB]]);},_oC=function(_oD,_oE,_oF){return A(_3f,[_oD,[0,_0,_oE]]);},_oG=function(_oH,_oI){return [0,_oH,function(_oJ){return _oz(_oI,_oJ);},function(_oK,_oL){return _oC(_oI,_oK,_oL);}];},_oM=function(_oN,_oO){return A(_oN,[function(_){return jsFind(toJSStr(E(_oO)));}]);},_oP=function(_oQ){return E(E(_oQ)[3]);},_oR=unCStr("[]"),_oS=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5K,_oc,_oR],_oT=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oS,_9],_oU=function(_oV){return E(_oT);},_oW=unCStr("Char"),_oX=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5K,_oc,_oW],_oY=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oX,_9],_oZ=function(_p0){return E(_oY);},_p1=new T(function(){return _6E(_oU,_oZ);}),_p2=new T(function(){return A(_p1,[_6D]);}),_p3=new T(function(){return E(_6D);}),_p4=function(_p5){return E(E(_p5)[6]);},_p6=function(_p7){return E(E(_p7)[1]);},_p8=[0,0],_p9=[0,32],_pa=[0,10],_pb=function(_pc){var _pd=E(_pc);if(!_pd[0]){return E(_n);}else{var _pe=_pd[1],_pf=E(_pd[2]);if(!_pf[0]){return _pg(_pa,_pe);}else{var _ph=new T(function(){return _pb(_pf);}),_pi=new T(function(){return _pg(_pa,_pe);});return function(_pj){return A(_pi,[[1,_p9,new T(function(){return A(_ph,[_pj]);})]]);};}}},_pk=unCStr("->"),_pl=[1,_pk,_9],_pm=[1,_oc,_pl],_pn=[1,_5K,_pm],_po=[0,32],_pp=function(_pq){var _pr=E(_pq);if(!_pr[0]){return [0];}else{var _ps=_pr[1],_pt=E(_pr[2]);return _pt[0]==0?E(_ps):_1v(_ps,[1,_po,new T(function(){return _pp(_pt);})]);}},_pu=new T(function(){return _pp(_pn);}),_pv=new T(function(){var _pw=_68(_pu);return [0,_pw[1],_pw[2],_5K,_oc,_pk];}),_px=function(_py,_pz){var _pA=E(_py);return _pA[0]==0?E(_pz):A(_pA[1],[new T(function(){return _px(_pA[2],_pz);})]);},_pB=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pC=[1,_5M,_9],_pD=function(_pE){var _pF=E(_pE);if(!_pF[0]){return [0];}else{var _pG=E(_pF[1]);return [1,[0,_pG[1],_pG[2]],new T(function(){return _pD(_pF[2]);})];}},_pH=new T(function(){var _pI=_1v(_9,_pC);if(!_pI[0]){return E(_oS);}else{var _pJ=_68(new T(function(){return _5W(_6k(_6v,[1,_pB,new T(function(){return _pD(_pI);})]));}));return E(_oS);}}),_pK=[0,40],_pL=function(_pM){return _pg(_pa,_pM);},_pN=[0,8],_pO=unCStr(" -> "),_pP=[0,9],_pQ=[0,93],_pR=[0,91],_pS=[0,41],_pT=[0,44],_pU=function(_pM){return [1,_pT,_pM];},_pV=function(_pW,_pX){var _pY=E(_pX);return _pY[0]==0?[0]:[1,_pW,[1,_pY[1],new T(function(){return _pV(_pW,_pY[2]);})]];},_pg=function(_pZ,_q0){var _q1=E(_q0),_q2=_q1[3],_q3=E(_q1[4]);if(!_q3[0]){return function(_q4){return _1v(E(_q2)[5],_q4);};}else{var _q5=_q3[1],_q6=new T(function(){var _q7=E(_q2)[5],_q8=new T(function(){return _pb(_q3);}),_q9=new T(function(){return E(_pZ)[1]<=9?function(_qa){return _1v(_q7,[1,_p9,new T(function(){return A(_q8,[_qa]);})]);}:function(_qb){return [1,_3t,new T(function(){return _1v(_q7,[1,_p9,new T(function(){return A(_q8,[[1,_3s,_qb]]);})]);})];};}),_qc=E(_q7);if(!_qc[0]){return E(_q9);}else{if(E(E(_qc[1])[1])==40){var _qd=E(_qc[2]);return _qd[0]==0?E(_q9):E(E(_qd[1])[1])==44?function(_qe){return [1,_pK,new T(function(){return A(new T(function(){var _qf=_6k(_pL,_q3);if(!_qf[0]){return E(_n);}else{var _qg=new T(function(){return _pV(_pU,_qf[2]);});return function(_ce){return _px([1,_qf[1],_qg],_ce);};}}),[[1,_pS,_qe]]);})];}:E(_q9);}else{return E(_q9);}}}),_qh=E(_q3[2]);if(!_qh[0]){var _qi=E(_q2),_qj=E(_pH),_qk=hs_eqWord64(_qi[1],_qj[1]);if(!E(_qk)){return E(_q6);}else{var _ql=hs_eqWord64(_qi[2],_qj[2]);if(!E(_ql)){return E(_q6);}else{var _qm=new T(function(){return _pg(_p8,_q5);});return function(_qn){return [1,_pR,new T(function(){return A(_qm,[[1,_pQ,_qn]]);})];};}}}else{if(!E(_qh[2])[0]){var _qo=E(_q2),_qp=E(_pv),_qq=hs_eqWord64(_qo[1],_qp[1]);if(!E(_qq)){return E(_q6);}else{var _qr=hs_eqWord64(_qo[2],_qp[2]);if(!E(_qr)){return E(_q6);}else{var _qs=new T(function(){return _pg(_pN,_qh[1]);}),_qt=new T(function(){return _pg(_pP,_q5);});return E(_pZ)[1]<=8?function(_qu){return A(_qt,[new T(function(){return _1v(_pO,new T(function(){return A(_qs,[_qu]);}));})]);}:function(_qv){return [1,_3t,new T(function(){return A(_qt,[new T(function(){return _1v(_pO,new T(function(){return A(_qs,[[1,_3s,_qv]]);}));})]);})];};}}}else{return E(_q6);}}}},_qw=function(_qx,_qy,_qz,_qA){var _qB=new T(function(){return _3f(_qx);}),_qC=new T(function(){return _oP(_qA);}),_qD=new T(function(){return _p4(_qA);}),_qE=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pg,[_p8,A(_qy,[_p3]),_9]);}));}),_qF=new T(function(){return A(_p6,[_qz,_f]);});return function(_qG){if(!E(new T(function(){var _qH=A(_qy,[_p3]),_qI=E(_p2),_qJ=hs_eqWord64(_qH[1],_qI[1]);if(!E(_qJ)){return false;}else{var _qK=hs_eqWord64(_qH[2],_qI[2]);return E(_qK)==0?false:true;}}))){var _qL=new T(function(){return A(_qB,[[1,_qG,new T(function(){return A(_qD,[new T(function(){return A(_qC,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1v(_qG,_qE);}));})]);})]);})]]);}),_qM=A(_qF,[_qG]);if(!_qM[0]){return E(_qL);}else{var _qN=E(_qM[1]);return E(_qN[2])[0]==0?E(_qM[2])[0]==0?A(_qB,[[2,_qN[1]]]):E(_qL):E(_qL);}}else{return A(_qB,[[2,_qG]]);}};},_qO=[0],_qP=new T(function(){return [0,"value"];}),_qQ=function(_qR,_qS,_qT,_qU,_qV,_qW){var _qX=E(_qR),_qY=_qX[1],_qZ=new T(function(){return A(_qX[3],[_qO]);}),_r0=new T(function(){return _qw(_qX,_qT,_qU,_qV);});return A(_qY,[new T(function(){return _oM(_qS,_qW);}),function(_r1){var _r2=E(_r1);return _r2[0]==0?E(_qZ):A(_qY,[new T(function(){return A(_qS,[function(_){var _r3=jsGet(E(_r2[1])[1],E(_qP)[1]);return [1,new T(function(){return fromJSStr(_r3);})];}]);}),function(_r4){var _r5=E(_r4);return _r5[0]==0?E(_qZ):A(_r0,[_r5[1]]);}]);}]);},_r6=1,_r7=function(_r8){return E(E(_r8)[9]);},_r9=function(_ra){return E(E(_ra)[2]);},_rb=function(_rc){return E(E(_rc)[3]);},_rd=function(_re){return E(E(_re)[2]);},_rf=function(_rg,_rh,_ri,_rj,_rk,_rl,_rm,_rn,_ro,_rp,_rq,_rr){var _rs=_oi(_rl),_rt=_rs[1],_ru=_rs[3],_rv=new T(function(){return _8Y(_rn);}),_rw=new T(function(){return _90(_rv);}),_rx=new T(function(){return _rb(_rm);}),_ry=new T(function(){return _rd(_rh);}),_rz=new T(function(){return _r7(_rn);});return A(_rt,[new T(function(){var _rA=E(_rp);if(!_rA[0]){var _rB=E(_rm);return _3z(_ro,_rB[1],_rB[2],_rB[3]);}else{return A(_ru,[_rA[1]]);}}),function(_rC){return A(_rt,[new T(function(){var _rD=E(_ro);return _r9(_rm);}),function(_rE){return A(_rs[2],[new T(function(){return A(_rx,[new T(function(){var _rF=E(new T(function(){var _rG=E(_ro);return [0,coercionToken];})),_rH=E(_rE);return [0,_rH[1],_rH[2],_r6,_rH[4],_rH[5]];})]);}),new T(function(){var _rI=new T(function(){return A(_ru,[[0,new T(function(){return A(_rz,[_rC,_rq,new T(function(){var _rJ=E(_rr);if(!_rJ[0]){return [0];}else{var _rK=_rJ[1],_rL=_1f(_rk,_p1,_rK);return _rL[0]==0?A(_rd,[_ri,_rK]):E(_rL[1]);}}),_e8,_a]);}),_a]]);});return A(_rt,[new T(function(){var _rM=E(_rl);return _qQ(_rM[1],_rM[2],_rj,_rg,_rn,_rC);}),function(_rN){var _rO=E(_rN);switch(_rO[0]){case 0:return E(_rI);case 1:return A(_ru,[[0,new T(function(){return A(_rw,[new T(function(){return A(_rz,[_rC,_rq,_rO[1],_e8,_a]);}),_rO[2]]);}),_a]]);default:var _rP=_rO[1];return A(_ru,[[0,new T(function(){return A(_rz,[_rC,_rq,new T(function(){var _rQ=_1f(_rj,_p1,_rP);return _rQ[0]==0?A(_ry,[_rP]):E(_rQ[1]);}),_e8,_a]);}),[1,_rP]]]);}}]);})]);}]);}]);},_rR=function(_rS,_rT,_rU,_rV,_rW){var _rX=new T(function(){return _oi(_rT);}),_rY=new T(function(){return _3h(_rX);}),_rZ=new T(function(){return _oG(_rY,_rX);}),_s0=new T(function(){return _ov(_rY,_rT);});return function(_ce,_s1,_s2){return _rf(_rW,_rV,_rV,_rU,_rU,_s0,_rZ,_rS,[0,coercionToken],_ce,_s1,_s2);};},_s3=new T(function(){return _rR(_8B,_9H,_og,_ob,_o0);}),_s4=new T(function(){return A(_s3,[_a,_9G,_a]);}),_s5=unCStr("keydown"),_s6=unCStr("mousemove"),_s7=unCStr("blur"),_s8=unCStr("focus"),_s9=unCStr("change"),_sa=unCStr("unload"),_sb=unCStr("load"),_sc=unCStr("keyup"),_sd=unCStr("keypress"),_se=unCStr("mouseup"),_sf=unCStr("mousedown"),_sg=unCStr("dblclick"),_sh=unCStr("click"),_si=unCStr("mouseout"),_sj=unCStr("mouseover"),_sk=function(_sl){switch(E(_sl)[0]){case 0:return E(_sb);case 1:return E(_sa);case 2:return E(_s9);case 3:return E(_s8);case 4:return E(_s7);case 5:return E(_s6);case 6:return E(_sj);case 7:return E(_si);case 8:return E(_sh);case 9:return E(_sg);case 10:return E(_sf);case 11:return E(_se);case 12:return E(_sd);case 13:return E(_sc);default:return E(_s5);}},_sm=[0],_sn=unCStr("OnLoad"),_so=[0,_sn,_sm],_sp=function(_){var _=0,_sq=newMVar(),_=putMVar(_sq,_so);return [0,_sq];},_sr=new T(function(){return _j(_sp);}),_ss=function(_st,_su,_){var _sv=A(_st,[_]);return die(_su);},_sw=function(_sx,_sy,_sz,_){return _ss(function(_){var _=putMVar(_sy,_sx);return _0;},_sz,_);},_sA=function(_sB,_){var _sC=0;if(!E(_sC)){return (function(_){var _sD=E(_sr)[1],_sE=takeMVar(_sD),_sF=jsCatch(function(_){return (function(_){return _sB;})();},function(_F,_){return _sw(_sE,_sD,_F,_);}),_=putMVar(_sD,_sF);return _0;})();}else{var _sG=E(_sr)[1],_sH=takeMVar(_sG),_sI=jsCatch(function(_){return _sB;},function(_F,_){return _sw(_sH,_sG,_F,_);}),_=putMVar(_sG,_sI);return _0;}},_sJ=unCStr("true"),_sK=function(_sL,_sM){while(1){var _sN=E(_sL);if(!_sN[0]){return E(_sM)[0]==0?true:false;}else{var _sO=E(_sM);if(!_sO[0]){return false;}else{if(E(_sN[1])[1]!=E(_sO[1])[1]){return false;}else{_sL=_sN[2];_sM=_sO[2];continue;}}}}},_sP=new T(function(){return [0,"keydown"];}),_sQ=new T(function(){return [0,"mousemove"];}),_sR=new T(function(){return [0,"blur"];}),_sS=new T(function(){return [0,"focus"];}),_sT=new T(function(){return [0,"change"];}),_sU=new T(function(){return [0,"unload"];}),_sV=new T(function(){return [0,"load"];}),_sW=new T(function(){return [0,"keyup"];}),_sX=new T(function(){return [0,"keypress"];}),_sY=new T(function(){return [0,"mouseup"];}),_sZ=new T(function(){return [0,"mousedown"];}),_t0=new T(function(){return [0,"dblclick"];}),_t1=new T(function(){return [0,"click"];}),_t2=new T(function(){return [0,"mouseout"];}),_t3=new T(function(){return [0,"mouseover"];}),_t4=function(_t5){switch(E(_t5)[0]){case 0:return E(_sV);case 1:return E(_sU);case 2:return E(_sT);case 3:return E(_sS);case 4:return E(_sR);case 5:return E(_sQ);case 6:return E(_t3);case 7:return E(_t2);case 8:return E(_t1);case 9:return E(_t0);case 10:return E(_sZ);case 11:return E(_sY);case 12:return E(_sX);case 13:return E(_sW);default:return E(_sP);}},_t6=function(_t7,_t8,_t9){var _ta=new T(function(){return _sk(_t8);}),_tb=new T(function(){return _t4(_t8);});return function(_tc,_){var _td=A(_t7,[_tc,_]),_te=E(_td),_tf=_te[1],_tg=E(_ta),_th=jsGetAttr(_tf,toJSStr(_tg));if(!_sK(fromJSStr(_th),_sJ)){var _ti=E(_t9),_tj=jsSetCB(_tf,E(_tb)[1],E([0,_t9])[1]),_tk=A(_1,[_n,_te,_tg,_sJ,_]);return _te;}else{return _te;}};},_tl=function(_tm,_tn){var _to=new T(function(){return _sk(_tn);}),_tp=[0,_to,_sm];return function(_tq,_){var _tr=E(_tq),_ts=E(_tr[4]),_tt=_ts[1],_tu=_ts[2],_tv=A(_tm,[_tr,_]),_tw=E(_tv),_tx=E(_tw[1]),_ty=_tx[1];return [0,[0,new T(function(){var _tz=E(_tn);switch(_tz[0]){case 0:return _t6(_ty,_tz,function(_){var _tA=_sA(_tp,_),_tB=A(_tt,[_]),_tC=E(_tB);if(!_tC[0]){return _0;}else{var _tD=A(_tu,[_tC[1],_]);return _0;}});case 1:return _t6(_ty,_tz,function(_){var _tE=_sA(_tp,_),_tF=A(_tt,[_]),_tG=E(_tF);if(!_tG[0]){return _0;}else{var _tH=A(_tu,[_tG[1],_]);return _0;}});case 2:return _t6(_ty,_tz,function(_){var _tI=_sA(_tp,_),_tJ=A(_tt,[_]),_tK=E(_tJ);if(!_tK[0]){return _0;}else{var _tL=A(_tu,[_tK[1],_]);return _0;}});case 3:return _t6(_ty,_tz,function(_){var _tM=_sA(_tp,_),_tN=A(_tt,[_]),_tO=E(_tN);if(!_tO[0]){return _0;}else{var _tP=A(_tu,[_tO[1],_]);return _0;}});case 4:return _t6(_ty,_tz,function(_){var _tQ=_sA(_tp,_),_tR=A(_tt,[_]),_tS=E(_tR);if(!_tS[0]){return _0;}else{var _tT=A(_tu,[_tS[1],_]);return _0;}});case 5:return _t6(_ty,_tz,function(_tU,_){var _tV=_sA([0,_to,[2,E(_tU)]],_),_tW=A(_tt,[_]),_tX=E(_tW);if(!_tX[0]){return _0;}else{var _tY=A(_tu,[_tX[1],_]);return _0;}});case 6:return _t6(_ty,_tz,function(_tZ,_){var _u0=_sA([0,_to,[2,E(_tZ)]],_),_u1=A(_tt,[_]),_u2=E(_u1);if(!_u2[0]){return _0;}else{var _u3=A(_tu,[_u2[1],_]);return _0;}});case 7:return _t6(_ty,_tz,function(_){var _u4=A(_tt,[_]),_u5=E(_u4);if(!_u5[0]){return _0;}else{var _u6=A(_tu,[_u5[1],_]);return _0;}});case 8:return _t6(_ty,_tz,function(_u7,_u8,_){var _u9=_sA([0,_to,[1,_u7,E(_u8)]],_),_ua=A(_tt,[_]),_ub=E(_ua);if(!_ub[0]){return _0;}else{var _uc=A(_tu,[_ub[1],_]);return _0;}});case 9:return _t6(_ty,_tz,function(_ud,_ue,_){var _uf=_sA([0,_to,[1,_ud,E(_ue)]],_),_ug=A(_tt,[_]),_uh=E(_ug);if(!_uh[0]){return _0;}else{var _ui=A(_tu,[_uh[1],_]);return _0;}});case 10:return _t6(_ty,_tz,function(_uj,_uk,_){var _ul=_sA([0,_to,[1,_uj,E(_uk)]],_),_um=A(_tt,[_]),_un=E(_um);if(!_un[0]){return _0;}else{var _uo=A(_tu,[_un[1],_]);return _0;}});case 11:return _t6(_ty,_tz,function(_up,_uq,_){var _ur=_sA([0,_to,[1,_up,E(_uq)]],_),_us=A(_tt,[_]),_ut=E(_us);if(!_ut[0]){return _0;}else{var _uu=A(_tu,[_ut[1],_]);return _0;}});case 12:return _t6(_ty,_tz,function(_uv,_){var _uw=_sA([0,_to,[3,_uv]],_),_ux=A(_tt,[_]),_uy=E(_ux);if(!_uy[0]){return _0;}else{var _uz=A(_tu,[_uy[1],_]);return _0;}});case 13:return _t6(_ty,_tz,function(_uA,_){var _uB=_sA([0,_to,[3,_uA]],_),_uC=A(_tt,[_]),_uD=E(_uC);if(!_uD[0]){return _0;}else{var _uE=A(_tu,[_uD[1],_]);return _0;}});default:return _t6(_ty,_tz,function(_uF,_){var _uG=_sA([0,_to,[3,_uF]],_),_uH=A(_tt,[_]),_uI=E(_uH);if(!_uI[0]){return _0;}else{var _uJ=A(_tu,[_uI[1],_]);return _0;}});}}),_tx[2]],_tw[2]];};},_uK=new T(function(){return _tl(_s4,_9F);}),_uL=function(_uM,_){var _uN=A(_uK,[_uM,_]),_uO=E(_uN),_uP=E(_uO[1]);return [0,[0,function(_uQ,_){var _uR=A(_uP[1],[_uQ,_]),_uS=_5i(_uQ,_);return _uQ;},_uP[2]],_uO[2]];},_uT=new T(function(){return [1,_uL,_uT];}),_uU=function(_uV,_uW){var _uX=E(_uV);if(!_uX){return [0];}else{var _uY=E(_uW);return _uY[0]==0?[0]:[1,_uY[1],new T(function(){return _uU(_uX-1|0,_uY[2]);})];}},_uZ=function(_v0,_v1){return _v0<0?[0]:_uU(_v0,_v1);},_v2=function(_v3,_v4){var _v5=E(_v3)[1];return _v5>0?_uZ(_v5,_v4):[0];},_v6=function(_v7){return E(_v7);},_v8=function(_v9){var _va=new T(function(){return _9y(_5A,_v2(_v9,_uT));}),_vb=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1v(_3u(0,E(_v9)[1],_9),_5w);}));}));});return function(_vc,_){var _vd=_4j(_va,_5o,_vc,_),_ve=E(_vd),_vf=E(_ve[1]),_vg=new T(function(){return _4V(_v6,_vf[1]);});return [0,[0,function(_vh,_){var _vi=A(_vb,[_vh,_]),_vj=A(_vg,[_vh,_]);return _vh;},_vf[2]],_ve[2]];};},_vk=new T(function(){return _v8(_4N);}),_vl=unCStr("center"),_vm=function(_vn,_vo){var _vp=new T(function(){return A(_vn,[_vo]);});return function(_vq,_){var _vr=jsCreateElem(toJSStr(E(_vl))),_vs=jsAppendChild(_vr,E(_vq)[1]),_vt=[0,_vr],_vu=A(_vp,[_vt,_]);return _vt;};},_vv=function(_vw,_){return _vw;},_vx=unCStr("Two counters. One is pure and recursive, the other is stateful"),_vy=new T(function(){return _4V(_4O,_vx);}),_vz=[8,coercionToken],_vA=function(_vB){return _aK(_mG(function(_vC){var _vD=E(_vC);return _vD[0]==0?A(_vB,[_vD[1]]):[2];}),new T(function(){return _mM(_vE,_vB);}));},_vE=function(_vF,_vG){return _vA(_vG);},_vH=function(_vI){return _aK(_aK(_mG(function(_vJ){var _vK=E(_vJ);return _vK[0]==1?A(_vI,[_vK[1]]):[2];}),new T(function(){return _nu(_vE,_vI);})),new T(function(){return _mM(_vL,_vI);}));},_vL=function(_vM,_vN){return _vH(_vN);},_vO=new T(function(){return _mM(_vL,_bL);}),_vP=new T(function(){return _nu(_vE,_bL);}),_vQ=function(_vR){var _vS=E(_vR);return _vS[0]==1?[3,_vS[1],_bK]:[2];},_vT=new T(function(){return _md(_vQ);}),_vU=function(_vV){return E(_vT);},_vW=function(_vX){return A(_ku,[_vX,_vU]);},_vY=[1,_vW],_vZ=new T(function(){return _aK(_vY,_vP);}),_w0=new T(function(){return _aK(_vZ,_vO);}),_w1=function(_ni){return _aA(_w0,_ni);},_w2=new T(function(){return _vA(_bL);}),_w3=function(_ni){return _aA(_w2,_ni);},_w4=function(_w5){return E(_w3);},_w6=[0,_w4,_w1,_vE,_vL],_w7=function(_w8){return E(E(_w8)[4]);},_w9=function(_wa,_wb,_wc){return _nu(new T(function(){return _w7(_wa);}),_wc);},_wd=function(_we){var _wf=new T(function(){return _nu(new T(function(){return _w7(_we);}),_bL);});return function(_ce){return _aA(_wf,_ce);};},_wg=function(_wh,_wi){var _wj=new T(function(){return A(_w7,[_wh,_wi,_bL]);});return function(_ce){return _aA(_wj,_ce);};},_wk=function(_wl){return [0,function(_ni){return _wg(_wl,_ni);},new T(function(){return _wd(_wl);}),new T(function(){return _w7(_wl);}),function(_nh,_ni){return _w9(_wl,_nh,_ni);}];},_wm=new T(function(){return _wk(_w6);}),_wn=unCStr("Prelude.(!!): negative index\n"),_wo=new T(function(){return err(_wn);}),_wp=unCStr("Prelude.(!!): index too large\n"),_wq=new T(function(){return err(_wp);}),_wr=function(_ws,_wt){while(1){var _wu=E(_ws);if(!_wu[0]){return E(_wq);}else{var _wv=E(_wt);if(!_wv){return E(_wu[1]);}else{_ws=_wu[2];_wt=_wv-1|0;continue;}}}},_ww=unCStr("ACK"),_wx=unCStr("BEL"),_wy=unCStr("BS"),_wz=unCStr("SP"),_wA=[1,_wz,_9],_wB=unCStr("US"),_wC=[1,_wB,_wA],_wD=unCStr("RS"),_wE=[1,_wD,_wC],_wF=unCStr("GS"),_wG=[1,_wF,_wE],_wH=unCStr("FS"),_wI=[1,_wH,_wG],_wJ=unCStr("ESC"),_wK=[1,_wJ,_wI],_wL=unCStr("SUB"),_wM=[1,_wL,_wK],_wN=unCStr("EM"),_wO=[1,_wN,_wM],_wP=unCStr("CAN"),_wQ=[1,_wP,_wO],_wR=unCStr("ETB"),_wS=[1,_wR,_wQ],_wT=unCStr("SYN"),_wU=[1,_wT,_wS],_wV=unCStr("NAK"),_wW=[1,_wV,_wU],_wX=unCStr("DC4"),_wY=[1,_wX,_wW],_wZ=unCStr("DC3"),_x0=[1,_wZ,_wY],_x1=unCStr("DC2"),_x2=[1,_x1,_x0],_x3=unCStr("DC1"),_x4=[1,_x3,_x2],_x5=unCStr("DLE"),_x6=[1,_x5,_x4],_x7=unCStr("SI"),_x8=[1,_x7,_x6],_x9=unCStr("SO"),_xa=[1,_x9,_x8],_xb=unCStr("CR"),_xc=[1,_xb,_xa],_xd=unCStr("FF"),_xe=[1,_xd,_xc],_xf=unCStr("VT"),_xg=[1,_xf,_xe],_xh=unCStr("LF"),_xi=[1,_xh,_xg],_xj=unCStr("HT"),_xk=[1,_xj,_xi],_xl=[1,_wy,_xk],_xm=[1,_wx,_xl],_xn=[1,_ww,_xm],_xo=unCStr("ENQ"),_xp=[1,_xo,_xn],_xq=unCStr("EOT"),_xr=[1,_xq,_xp],_xs=unCStr("ETX"),_xt=[1,_xs,_xr],_xu=unCStr("STX"),_xv=[1,_xu,_xt],_xw=unCStr("SOH"),_xx=[1,_xw,_xv],_xy=unCStr("NUL"),_xz=[1,_xy,_xx],_xA=[0,92],_xB=unCStr("\\DEL"),_xC=unCStr("\\a"),_xD=unCStr("\\\\"),_xE=unCStr("\\SO"),_xF=unCStr("\\r"),_xG=unCStr("\\f"),_xH=unCStr("\\v"),_xI=unCStr("\\n"),_xJ=unCStr("\\t"),_xK=unCStr("\\b"),_xL=function(_xM,_xN){if(_xM<=127){var _xO=E(_xM);switch(_xO){case 92:return _1v(_xD,_xN);case 127:return _1v(_xB,_xN);default:if(_xO<32){var _xP=E(_xO);switch(_xP){case 7:return _1v(_xC,_xN);case 8:return _1v(_xK,_xN);case 9:return _1v(_xJ,_xN);case 10:return _1v(_xI,_xN);case 11:return _1v(_xH,_xN);case 12:return _1v(_xG,_xN);case 13:return _1v(_xF,_xN);case 14:return _1v(_xE,new T(function(){var _xQ=E(_xN);return _xQ[0]==0?[0]:E(E(_xQ[1])[1])==72?unAppCStr("\\&",_xQ):E(_xQ);}));default:return _1v([1,_xA,new T(function(){var _xR=_xP;return _xR>=0?_wr(_xz,_xR):E(_wo);})],_xN);}}else{return [1,[0,_xO],_xN];}}}else{return [1,_xA,new T(function(){var _xS=jsShowI(_xM);return _1v(fromJSStr(_xS),new T(function(){var _xT=E(_xN);if(!_xT[0]){return [0];}else{var _xU=E(_xT[1])[1];return _xU<48?E(_xT):_xU>57?E(_xT):unAppCStr("\\&",_xT);}}));})];}},_xV=[0,39],_xW=[1,_xV,_9],_xX=unCStr("\'\\\'\'"),_xY=function(_xZ){var _y0=E(E(_xZ)[1]);return _y0==39?E(_xX):[1,_xV,new T(function(){return _xL(_y0,_xW);})];},_y1=[0,34],_y2=unCStr("\\\""),_y3=function(_y4,_y5){var _y6=E(_y4);if(!_y6[0]){return E(_y5);}else{var _y7=_y6[2],_y8=E(E(_y6[1])[1]);return _y8==34?_1v(_y2,new T(function(){return _y3(_y7,_y5);})):_xL(_y8,new T(function(){return _y3(_y7,_y5);}));}},_y9=function(_ya,_yb){return [1,_y1,new T(function(){return _y3(_ya,[1,_y1,_yb]);})];},_yc=function(_yd){return _1v(_xX,_yd);},_ye=function(_yf,_yg){var _yh=E(E(_yg)[1]);return _yh==39?E(_yc):function(_yi){return [1,_xV,new T(function(){return _xL(_yh,[1,_xV,_yi]);})];};},_yj=[0,_ye,_xY,_y9],_yk=function(_yl){return E(E(_yl)[3]);},_ym=function(_yn,_yo){return A(_yk,[_yn,_yo,_9]);},_yp=function(_yq,_yr,_ys){return _2l(new T(function(){return _yk(_yq);}),_yr,_ys);},_yt=function(_yu){var _yv=new T(function(){return _yk(_yu);});return [0,function(_yw){return E(_yv);},function(_yd){return _ym(_yu,_yd);},function(_yx,_yd){return _yp(_yu,_yx,_yd);}];},_yy=new T(function(){return _yt(_yj);}),_yz=unCStr("submit"),_yA=new T(function(){return A(_rR,[_8B,_9H,_p1,_yy,_wm,_a,_yz]);}),_yB=[0,43],_yC=[1,_yB,_9],_yD=[1,_yC],_yE=new T(function(){return A(_yA,[_yD]);}),_yF=new T(function(){return _tl(_yE,_vz);}),_yG=function(_F,_){return _H(_G,_F,_);},_yH=function(_yI,_yJ,_yK,_){var _yL=A(_yJ,[_yK,_]),_yM=E(_yL),_yN=E(_yM[1]);return [0,[0,function(_yO,_){var _yP=_H(_G,_yO,_),_yQ=A(_1,[_n,_yP,_q,_yI,_]),_yR=A(_yN[1],[_yP,_]);return _yP;},_yN[2]],_yM[2]];},_yS=new T(function(){return _3z(_S,_3m,_Q,_N);}),_yT=new T(function(){return _3z(_S,_3m,_Q,_N);}),_yU=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_yV=function(_yW){return _j(function(_){var _=0;return eval(E(_yW)[1]);});},_yX=new T(function(){return _yV(_yU);}),_yY=function(_yZ,_z0,_z1,_){var _z2=A(_yS,[_z1,_]),_z3=A(_yT,[new T(function(){return E(E(_z2)[2]);}),_]),_z4=E(_z3),_z5=_z4[1],_z6=E(_z4[2]),_z7=_z6[2],_z8=E(_z6[4]),_z9=new T(function(){return E(E(_z2)[1]);}),_za=function(_zb){var _zc=new T(function(){return A(_z0,[_zb]);});return function(_zd,_){var _ze=A(_zc,[_zd,_]),_zf=E(_ze),_zg=E(_zf[1]);return [0,[0,function(_zh,_){var _zi=A(_zg[1],[_zh,_]),_zj=E(_z9),_zk=jsFind(toJSStr(_zj)),_zl=E(_zk);if(!_zl[0]){return _3N(_zj);}else{var _zm=E(_zl[1]),_zn=A(_yX,[E(_zm[1]),_]),_zo=jsKillChild(E(_zm)[1],_zn);return _zh;}},_zg[2]],_zf[2]];};},_zp=_yH(_z9,_yZ,[0,_z6[1],_z7,_z6[3],[0,function(_){return _3P(function(_zq,_){var _zr=_yH(_z9,_yZ,new T(function(){var _zs=E(_zq);return [0,_zs[1],_z7,_zs[3],_zs[4],_zs[5]];}),_);return [0,[0,_2M,E(E(_zr)[1])[2]],_zq];},_z5,_);},function(_zt,_){var _zu=_3P(new T(function(){return _za(_zt);}),_z5,_),_zv=E(_zu);return _zv[0]==0?_a:A(_z8[2],[_zv[1],_]);}],_z6[5]],_),_zw=E(_zp),_zx=_zw[2],_zy=E(_zw[1]),_zz=_zy[1],_zA=new T(function(){return _y(_yG,[1,[0,_q,_z5],_9]);}),_zB=E(_zy[2]);if(!_zB[0]){return [0,[0,function(_zC,_){var _zD=A(_zz,[_zC,_]),_zE=A(_zA,[_zC,_]);return _zC;},_a],new T(function(){var _zF=E(_zx);return [0,_zF[1],_zF[2],_zF[3],_z8,_zF[5]];})];}else{var _zG=A(_za,[_zB[1],new T(function(){var _zH=E(_zx);return [0,_zH[1],_zH[2],_zH[3],_z8,_zH[5]];}),_]),_zI=E(_zG),_zJ=E(_zI[1]);return [0,[0,function(_zK,_){var _zL=A(_zz,[_zK,_]),_zM=A(_zA,[_zK,_]),_zN=A(_zJ[1],[_zM,_]);return _zK;},_zJ[2]],_zI[2]];}},_zO=function(_zP){var _zQ=new T(function(){return _zO(new T(function(){return [0,E(_zP)[1]+1|0];}));}),_zR=new T(function(){return _58(_4O,new T(function(){return _54(_zP);}));});return function(_ce,_s1){return _yY(function(_zS,_){var _zT=A(_yF,[_zS,_]),_zU=E(_zT),_zV=E(_zU[1]);return [0,[0,function(_zW,_){var _zX=A(_zR,[_zW,_]),_zY=A(_zV[1],[_zW,_]);return _zW;},_zV[2]],_zU[2]];},function(_zZ){return E(_zQ);},_ce,_s1);};},_A0=unCStr("main"),_A1=unCStr("Main"),_A2=unCStr("Counter"),_A3=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A0,_A1,_A2],_A4=function(_A5,_A6){var _A7=hs_leWord64(_A5,_A6);return E(_A7)==0?false:true;},_A8=function(_A9,_Aa,_Ab,_Ac){var _Ad=hs_eqWord64(_A9,_Ab);if(!E(_Ad)){var _Ae=hs_leWord64(_A9,_Ab);return E(_Ae)==0?false:true;}else{return _A4(_Aa,_Ac);}},_Af=function(_Ag,_Ah){var _Ai=E(_Ag),_Aj=_Ai[1],_Ak=_Ai[2],_Al=E(_Ah),_Am=_Al[1],_An=_Al[2],_Ao=hs_eqWord64(_Aj,_Am);if(!E(_Ao)){return !_A8(_Aj,_Ak,_Am,_An)?2:0;}else{var _Ap=hs_eqWord64(_Ak,_An);return E(_Ap)==0?!_A8(_Aj,_Ak,_Am,_An)?2:0:1;}},_Aq=unCStr("Failure in Data.Map.balanceL"),_Ar=new T(function(){return err(_Aq);}),_As=function(_At,_Au,_Av,_Aw){var _Ax=E(_Aw);if(!_Ax[0]){var _Ay=_Ax[1],_Az=E(_Av);if(!_Az[0]){var _AA=_Az[1],_AB=_Az[2],_AC=_Az[3];if(_AA<=(imul(3,_Ay)|0)){return [0,(1+_AA|0)+_Ay|0,E(E(_At)),_Au,E(_Az),E(_Ax)];}else{var _AD=E(_Az[4]);if(!_AD[0]){var _AE=_AD[1],_AF=E(_Az[5]);if(!_AF[0]){var _AG=_AF[1],_AH=_AF[2],_AI=_AF[3],_AJ=_AF[4];if(_AG>=(imul(2,_AE)|0)){var _AK=function(_AL){var _AM=E(_AF[5]);return _AM[0]==0?[0,(1+_AA|0)+_Ay|0,E(_AH),_AI,E([0,(1+_AE|0)+_AL|0,E(_AB),_AC,E(_AD),E(_AJ)]),E([0,(1+_Ay|0)+_AM[1]|0,E(E(_At)),_Au,E(_AM),E(_Ax)])]:[0,(1+_AA|0)+_Ay|0,E(_AH),_AI,E([0,(1+_AE|0)+_AL|0,E(_AB),_AC,E(_AD),E(_AJ)]),E([0,1+_Ay|0,E(E(_At)),_Au,E(_8),E(_Ax)])];},_AN=E(_AJ);return _AN[0]==0?_AK(_AN[1]):_AK(0);}else{return [0,(1+_AA|0)+_Ay|0,E(_AB),_AC,E(_AD),E([0,(1+_Ay|0)+_AG|0,E(E(_At)),_Au,E(_AF),E(_Ax)])];}}else{return E(_Ar);}}else{return E(_Ar);}}}else{return [0,1+_Ay|0,E(E(_At)),_Au,E(_8),E(_Ax)];}}else{var _AO=E(_Av);if(!_AO[0]){var _AP=_AO[1],_AQ=_AO[2],_AR=_AO[3],_AS=_AO[5],_AT=E(_AO[4]);if(!_AT[0]){var _AU=_AT[1],_AV=E(_AS);if(!_AV[0]){var _AW=_AV[1],_AX=_AV[2],_AY=_AV[3],_AZ=_AV[4];if(_AW>=(imul(2,_AU)|0)){var _B0=function(_B1){var _B2=E(_AV[5]);return _B2[0]==0?[0,1+_AP|0,E(_AX),_AY,E([0,(1+_AU|0)+_B1|0,E(_AQ),_AR,E(_AT),E(_AZ)]),E([0,1+_B2[1]|0,E(E(_At)),_Au,E(_B2),E(_8)])]:[0,1+_AP|0,E(_AX),_AY,E([0,(1+_AU|0)+_B1|0,E(_AQ),_AR,E(_AT),E(_AZ)]),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])];},_B3=E(_AZ);return _B3[0]==0?_B0(_B3[1]):_B0(0);}else{return [0,1+_AP|0,E(_AQ),_AR,E(_AT),E([0,1+_AW|0,E(E(_At)),_Au,E(_AV),E(_8)])];}}else{return [0,3,E(_AQ),_AR,E(_AT),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])];}}else{var _B4=E(_AS);return _B4[0]==0?[0,3,E(_B4[2]),_B4[3],E([0,1,E(_AQ),_AR,E(_8),E(_8)]),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])]:[0,2,E(E(_At)),_Au,E(_AO),E(_8)];}}else{return [0,1,E(E(_At)),_Au,E(_8),E(_8)];}}},_B5=unCStr("Failure in Data.Map.balanceR"),_B6=new T(function(){return err(_B5);}),_B7=function(_B8,_B9,_Ba,_Bb){var _Bc=E(_Ba);if(!_Bc[0]){var _Bd=_Bc[1],_Be=E(_Bb);if(!_Be[0]){var _Bf=_Be[1],_Bg=_Be[2],_Bh=_Be[3];if(_Bf<=(imul(3,_Bd)|0)){return [0,(1+_Bd|0)+_Bf|0,E(E(_B8)),_B9,E(_Bc),E(_Be)];}else{var _Bi=E(_Be[4]);if(!_Bi[0]){var _Bj=_Bi[1],_Bk=_Bi[2],_Bl=_Bi[3],_Bm=_Bi[4],_Bn=E(_Be[5]);if(!_Bn[0]){var _Bo=_Bn[1];if(_Bj>=(imul(2,_Bo)|0)){var _Bp=function(_Bq){var _Br=E(_B8),_Bs=E(_Bi[5]);return _Bs[0]==0?[0,(1+_Bd|0)+_Bf|0,E(_Bk),_Bl,E([0,(1+_Bd|0)+_Bq|0,E(_Br),_B9,E(_Bc),E(_Bm)]),E([0,(1+_Bo|0)+_Bs[1]|0,E(_Bg),_Bh,E(_Bs),E(_Bn)])]:[0,(1+_Bd|0)+_Bf|0,E(_Bk),_Bl,E([0,(1+_Bd|0)+_Bq|0,E(_Br),_B9,E(_Bc),E(_Bm)]),E([0,1+_Bo|0,E(_Bg),_Bh,E(_8),E(_Bn)])];},_Bt=E(_Bm);return _Bt[0]==0?_Bp(_Bt[1]):_Bp(0);}else{return [0,(1+_Bd|0)+_Bf|0,E(_Bg),_Bh,E([0,(1+_Bd|0)+_Bj|0,E(E(_B8)),_B9,E(_Bc),E(_Bi)]),E(_Bn)];}}else{return E(_B6);}}else{return E(_B6);}}}else{return [0,1+_Bd|0,E(E(_B8)),_B9,E(_Bc),E(_8)];}}else{var _Bu=E(_Bb);if(!_Bu[0]){var _Bv=_Bu[1],_Bw=_Bu[2],_Bx=_Bu[3],_By=_Bu[5],_Bz=E(_Bu[4]);if(!_Bz[0]){var _BA=_Bz[1],_BB=_Bz[2],_BC=_Bz[3],_BD=_Bz[4],_BE=E(_By);if(!_BE[0]){var _BF=_BE[1];if(_BA>=(imul(2,_BF)|0)){var _BG=function(_BH){var _BI=E(_B8),_BJ=E(_Bz[5]);return _BJ[0]==0?[0,1+_Bv|0,E(_BB),_BC,E([0,1+_BH|0,E(_BI),_B9,E(_8),E(_BD)]),E([0,(1+_BF|0)+_BJ[1]|0,E(_Bw),_Bx,E(_BJ),E(_BE)])]:[0,1+_Bv|0,E(_BB),_BC,E([0,1+_BH|0,E(_BI),_B9,E(_8),E(_BD)]),E([0,1+_BF|0,E(_Bw),_Bx,E(_8),E(_BE)])];},_BK=E(_BD);return _BK[0]==0?_BG(_BK[1]):_BG(0);}else{return [0,1+_Bv|0,E(_Bw),_Bx,E([0,1+_BA|0,E(E(_B8)),_B9,E(_8),E(_Bz)]),E(_BE)];}}else{return [0,3,E(_BB),_BC,E([0,1,E(E(_B8)),_B9,E(_8),E(_8)]),E([0,1,E(_Bw),_Bx,E(_8),E(_8)])];}}else{var _BL=E(_By);return _BL[0]==0?[0,3,E(_Bw),_Bx,E([0,1,E(E(_B8)),_B9,E(_8),E(_8)]),E(_BL)]:[0,2,E(E(_B8)),_B9,E(_8),E(_Bu)];}}else{return [0,1,E(E(_B8)),_B9,E(_8),E(_8)];}}},_BM=function(_BN,_BO,_BP,_BQ,_BR,_BS){var _BT=E(_BS);if(!_BT[0]){var _BU=_BT[2],_BV=_BT[3],_BW=_BT[4],_BX=_BT[5];switch(_Af([0,_BN,_BO,_BP,_BQ],_BU)){case 0:return _As(_BU,_BV,_BM(_BN,_BO,_BP,_BQ,_BR,_BW),_BX);case 1:return [0,_BT[1],E([0,_BN,_BO,_BP,_BQ]),_BR,E(_BW),E(_BX)];default:return _B7(_BU,_BV,_BW,_BM(_BN,_BO,_BP,_BQ,_BR,_BX));}}else{return [0,1,E([0,_BN,_BO,_BP,_BQ]),_BR,E(_8),E(_8)];}},_BY=[0,_2M,_5m],_BZ=function(_C0,_){return [0,[0,_2M,[1,_C0]],_C0];},_C1=[1,_0],_C2=function(_C3){var _C4=new T(function(){return [0,E(_C3)[1]+1|0];}),_C5=new T(function(){return _58(_4O,new T(function(){return _54(_C3);}));});return function(_ce,_s1){return _4j(function(_C6,_){return [0,[0,_C5,_C1],_C6];},function(_C7,_C8,_){return (function(_C8,_){return _4j(_BZ,function(_C9){return function(_Ca,_){return [0,_BY,new T(function(){var _Cb=E(_C9);return [0,_Cb[1],_Cb[2],_Cb[3],_Cb[4],new T(function(){return _BM(I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A3,_9,_C4,_Cb[5]);})];})];};},_C8,_);})(_C8,_);},_ce,_s1);};},_Cc=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A3,_9],_Cd=function(_Ce){return E(_Cc);},_Cf=function(_Cg,_Ch,_Ci,_Cj,_Ck){while(1){var _Cl=E(_Ck);if(!_Cl[0]){switch(_Af([0,_Cg,_Ch,_Ci,_Cj],_Cl[2])){case 0:_Ck=_Cl[4];continue;case 1:return [1,_Cl[3]];default:_Ck=_Cl[5];continue;}}else{return [0];}}},_Cm=function(_Cn,_Co){var _Cp=E(_Cn),_Cq=_Cp[1],_Cr=_Cp[2],_Cs=_Cp[3],_Ct=_Cp[4],_Cu=E(_Co);if(!_Cu[0]){switch(_Af(_Cp,_Cu[2])){case 0:return _Cf(_Cq,_Cr,_Cs,_Ct,_Cu[4]);case 1:return [1,_Cu[3]];default:return _Cf(_Cq,_Cr,_Cs,_Ct,_Cu[5]);}}else{return [0];}},_Cv=function(_Cw,_Cx,_Cy,_Cz){var _CA=E(_Cx),_CB=_CA[1],_CC=_CA[3],_CD=new T(function(){return A(_Cz,[_p3]);}),_CE=new T(function(){return A(_CC,[_a]);});return A(_CB,[new T(function(){return A(_CB,[_Cy,function(_CF){return A(_CC,[new T(function(){var _CG=E(_Cw);return E(E(_CF)[5]);})]);}]);}),function(_CH){var _CI=_Cm(_CD,_CH);return _CI[0]==0?E(_CE):A(_CC,[[1,_CI[1]]]);}]);},_CJ=new T(function(){return _Cv(_S,_3m,_Q,_Cd);}),_CK=function(_CL){var _CM=new T(function(){return _zO(_CL);});return function(_CN,_){var _CO=A(_CM,[_CN,_]),_CP=E(_CO),_CQ=E(_CP[1]),_CR=_4j(_yF,function(_CS){return function(_C8,_){return _4j(function(_CT,_){var _CU=A(_CJ,[_CT,_]);return [0,[0,_vv,new T(function(){var _CV=E(E(_CU)[1]);return _CV[0]==0?E([1,_CL]):E(_CV);})],new T(function(){return E(E(_CU)[2]);})];},_C2,_C8,_);};},_CP[2],_),_CW=E(_CR),_CX=E(_CW[1]),_CY=new T(function(){return _vm(_v6,function(_CZ,_){var _D0=A(_CQ[1],[_CZ,_]),_D1=A(_CX[1],[_CZ,_]);return _CZ;});});return [0,[0,function(_D2,_){var _D3=A(_vy,[_D2,_]),_D4=_5i(_D2,_),_D5=A(_CY,[_D2,_]);return _D2;},new T(function(){var _D6=E(_CQ[2]);return _D6[0]==0?E(_CX[2]):E(_D6);})],_CW[2]];};},_D7=new T(function(){return _CK(_4N);}),_D8=[0,4],_D9=function(_Da,_Db){return [1,_Db,new T(function(){return _D9(_Da,new T(function(){return A(_Da,[_Db]);}));})];},_Dc=[0,1],_Dd=[1,_Dc,_9],_De=[1,_5x,_9],_Df=function(_Dg,_Dh,_Di){var _Dj=E(_Dh);if(!_Dj[0]){return [0];}else{var _Dk=E(_Di);return _Dk[0]==0?[0]:[1,new T(function(){return A(_Dg,[_Dj[1],_Dk[1]]);}),new T(function(){return _Df(_Dg,_Dj[2],_Dk[2]);})];}},_Dl=function(_Dm){return _Df(_8Q,[1,_5x,_Dm],new T(function(){return _1v(_Dm,_De);}));},_Dn=new T(function(){return _D9(_Dl,_Dd);}),_Do=unCStr(" rows of the Pascal triangle "),_Dp=function(_Dq){var _Dr=new T(function(){return _2l(_o1,_Dq,_9);});return function(_ce,_s1){return _4O(_Dr,_ce,_s1);};},_Ds=unCStr("style"),_Dt=function(_Du,_Dv){var _Dw=new T(function(){return _4V(_Dp,_Du);});return [1,function(_Dx,_){var _Dy=A(_Dw,[_Dx,_]),_Dz=A(_1,[_n,_Dy,_Ds,_p,_]);return _Dy;},_Dv];},_DA=function(_DB,_DC){var _DD=E(_DB);if(!_DD[0]){return [0];}else{var _DE=_DD[1];return _DC>1?_Dt(_DE,new T(function(){return _DA(_DD[2],_DC-1|0);})):_Dt(_DE,_9);}},_DF=function(_DG){var _DH=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("Show ",new T(function(){return _1v(_3u(0,E(_DG)[1],_9),_Do);}));}));});return function(_DI,_){return [0,[0,function(_DJ,_){var _DK=A(_DH,[_DJ,_]),_DL=_8o(new T(function(){var _DM=E(_DG)[1];return _DM>0?_DA(_Dn,_DM):[0];}),_DJ,_);return _DJ;},_a],_DI];};},_DN=new T(function(){return _DF(_D8);}),_DO=unCStr("Different input elements:"),_DP=new T(function(){return _4V(_4O,_DO);}),_DQ=unCStr(" returns: "),_DR=[1,_y1,_9],_DS=function(_DT){var _DU=new T(function(){return _58(_4O,[1,_y1,new T(function(){return _y3(_DT,_DR);})]);});return function(_DV,_){return [0,[0,function(_DW,_){var _DX=_4O(_DQ,_DW,_),_DY=A(_DU,[_DW,_]);return _DW;},_C1],_DV];};},_DZ=unCStr("blue"),_E0=[1,_DZ,_9],_E1=unCStr("green"),_E2=[1,_E1,_E0],_E3=unCStr("red"),_E4=[1,_E3,_E2],_E5=function(_E6){return E(E(_E6)[15]);},_E7=function(_E8,_E9,_){var _Ea=jsGet(_E8,toJSStr(E(_E9)));return new T(function(){return fromJSStr(_Ea);});},_Eb=function(_Ec,_Ed,_){return _E7(E(_Ec)[1],_Ed,_);},_Ee=unCStr("name"),_Ef=unCStr("true"),_Eg=unCStr("radio"),_Eh=new T(function(){return A(_p1,[_6D]);}),_Ei=function(_Ej,_Ek,_El,_Em){var _En=new T(function(){return _oi(_Ek);}),_Eo=new T(function(){return _3z([0,coercionToken],_3h(_En),function(_Ep){return _oz(_En,_Ep);},function(_Eq,_Er){return _oC(_En,_Eq,_Er);});}),_Es=new T(function(){return _3f(_En);}),_Et=new T(function(){return _3f(_En);}),_Eu=new T(function(){return _2P(_En);}),_Ev=new T(function(){return _2P(_En);}),_Ew=new T(function(){return _3f(_En);}),_Ex=new T(function(){return _2P(_En);}),_Ey=new T(function(){return _3f(_En);}),_Ez=new T(function(){return _2P(_En);}),_EA=new T(function(){return _r7(_Ej);}),_EB=new T(function(){return _E5(_Ej);}),_EC=new T(function(){return _rd(_Em);});return function(_ED,_EE){return function(_EF){return A(_Eu,[new T(function(){return A(_Eo,[_EF]);}),function(_EG){var _EH=new T(function(){return E(E(_EG)[1]);}),_EI=new T(function(){return _om(_Ek,function(_){return jsFind(toJSStr(E(_EH)));});});return A(_Ez,[new T(function(){var _EJ=new T(function(){return E(E(_EG)[2]);});return A(_Ey,[[0,_EJ,_EJ]]);}),function(_EK){return A(_Ex,[new T(function(){return A(_Ew,[[0,_0,new T(function(){var _EL=E(E(_EK)[1]);return [0,_EL[1],_EL[2],_r6,_EL[4],_EL[5]];})]]);}),function(_EM){return A(_Ev,[new T(function(){return A(_EI,[new T(function(){return E(E(_EM)[2]);})]);}),function(_EN){return A(_Eu,[new T(function(){var _EO=E(_EN),_EP=_EO[2],_EQ=E(_EO[1]);return _EQ[0]==0?A(_Et,[[0,_9,_EP]]):A(_om,[_Ek,function(_){return _Eb(_EQ[1],_6R,_);},_EP]);}),function(_ER){var _ES=new T(function(){return !_sK(E(_ER)[1],_Ef)?[0]:E([1,_ED]);});return A(_Es,[[0,[0,new T(function(){return A(_EB,[new T(function(){return A(_EA,[_EH,_Eg,new T(function(){var _ET=A(_El,[_ED]),_EU=E(_Eh),_EV=hs_eqWord64(_ET[1],_EU[1]);if(!E(_EV)){return A(_EC,[_ED]);}else{var _EW=hs_eqWord64(_ET[2],_EU[2]);return E(_EW)==0?A(_EC,[_ED]):E(_ED);}}),new T(function(){return E(_ES)[0]==0?false:true;}),_a]);}),[1,[0,_Ee,_EE],_9]]);}),new T(function(){var _EX=E(_ES);return _EX[0]==0?[0]:[1,_EX[1]];})],new T(function(){return E(E(_ER)[2]);})]]);}]);}]);}]);}]);}]);};};},_EY=new T(function(){return _6E(_oU,_oZ);}),_EZ=new T(function(){return _yt(_yj);}),_F0=new T(function(){return _Ei(_8B,_9H,_EY,_EZ);}),_F1=function(_F2){var _F3=E(_F2);if(!_F3[0]){return [0];}else{var _F4=_F3[1];return [1,function(_F5){var _F6=new T(function(){return _tl(new T(function(){return A(_F0,[_F4,_F5]);}),_vz);});return function(_F7,_){var _F8=A(_F6,[_F7,_]),_F9=E(_F8),_Fa=E(_F9[1]);return [0,[0,function(_Fb,_){var _Fc=_4O(_F4,_Fb,_),_Fd=A(_Fa[1],[_Fb,_]);return _Fb;},_Fa[2]],_F9[2]];};},new T(function(){return _F1(_F3[2]);})];}},_Fe=new T(function(){return _F1(_E4);}),_Ff=function(_Fg){return E(E(_Fg)[1]);},_Fh=function(_Fi,_Fj){var _Fk=new T(function(){return _8Y(_Fj);}),_Fl=new T(function(){return _Ff(_Fk);}),_Fm=new T(function(){return _90(_Fk);}),_Fn=function(_Fo){var _Fp=E(_Fo);if(!_Fp[0]){return [0,_Fl,_a];}else{var _Fq=E(_Fp[1]),_Fr=_Fn(_Fp[2]);return [0,new T(function(){return A(_Fm,[_Fq[1],_Fr[1]]);}),new T(function(){var _Fs=E(_Fq[2]);return _Fs[0]==0?E(_Fr[2]):E(_Fs);})];}},_Ft=new T(function(){return _3f(_Fi);}),_Fu=new T(function(){return _3z([0,coercionToken],_3h(_Fi),function(_Fv){return _oz(_Fi,_Fv);},function(_Fw,_Fx){return _oC(_Fi,_Fw,_Fx);});}),_Fy=new T(function(){return _3f(_Fi);}),_Fz=new T(function(){return _2P(_Fi);}),_FA=new T(function(){return _2P(_Fi);}),_FB=new T(function(){return _2P(_Fi);}),_FC=new T(function(){return _2P(_Fi);});return function(_FD,_FE){return A(_FC,[new T(function(){return A(_Fu,[_FE]);}),function(_FF){return A(_FB,[new T(function(){var _FG=new T(function(){return E(E(_FF)[1]);}),_FH=function(_FI){var _FJ=E(_FI);if(!_FJ[0]){return function(_FK){return A(_Fy,[[0,_9,_FK]]);};}else{var _FL=new T(function(){return _FH(_FJ[2]);}),_FM=new T(function(){return A(_FJ[1],[_FG]);});return function(_FN){return A(_FA,[new T(function(){return A(_FM,[_FN]);}),function(_FO){var _FP=new T(function(){return E(E(_FO)[1]);});return A(_Fz,[new T(function(){return A(_FL,[new T(function(){return E(E(_FO)[2]);})]);}),function(_FQ){return A(_Fy,[[0,[1,_FP,new T(function(){return E(E(_FQ)[1]);})],new T(function(){return E(E(_FQ)[2]);})]]);}]);}]);};}};return A(_FH,[_FD,new T(function(){return E(E(_FF)[2]);})]);}),function(_FR){var _FS=new T(function(){var _FT=_Fn(E(_FR)[1]);return [0,_FT[1],_FT[2]];});return A(_Ft,[[0,[0,new T(function(){return E(E(_FS)[1]);}),new T(function(){var _FU=E(E(_FS)[2]);return _FU[0]==0?[0]:[1,_FU[1]];})],new T(function(){return E(E(_FR)[2]);})]]);}]);}]);};},_FV=new T(function(){return _Fh(_2O,_8B);}),_FW=new T(function(){return A(_FV,[_Fe]);}),_FX=function(_FY){var _FZ=new T(function(){return _58(_4O,new T(function(){return _2l(_y9,_FY,_9);}));});return function(_G0,_){return [0,[0,function(_G1,_){var _G2=_4O(_DQ,_G1,_),_G3=A(_FZ,[_G1,_]);return _G1;},_C1],_G0];};},_G4=function(_G5){return _FX(E(_G5)[1]);},_G6=new T(function(){return _58(_4O,_E1);}),_G7=unCStr("checkbox"),_G8=function(_G9,_Ga){var _Gb=new T(function(){return _oi(_Ga);}),_Gc=new T(function(){return _3z([0,coercionToken],_3h(_Gb),function(_Gd){return _oz(_Gb,_Gd);},function(_Ge,_Gf){return _oC(_Gb,_Ge,_Gf);});}),_Gg=new T(function(){return _3f(_Gb);}),_Gh=new T(function(){return _3f(_Gb);}),_Gi=new T(function(){return _2P(_Gb);}),_Gj=new T(function(){return _2P(_Gb);}),_Gk=new T(function(){return _3f(_Gb);}),_Gl=new T(function(){return _2P(_Gb);}),_Gm=new T(function(){return _3f(_Gb);}),_Gn=new T(function(){return _2P(_Gb);}),_Go=new T(function(){return _r7(_G9);});return function(_Gp,_Gq){return function(_Gr){return A(_Gi,[new T(function(){return A(_Gc,[_Gr]);}),function(_Gs){var _Gt=new T(function(){return E(E(_Gs)[1]);}),_Gu=new T(function(){return _om(_Ga,function(_){return jsFind(toJSStr(E(_Gt)));});});return A(_Gn,[new T(function(){var _Gv=new T(function(){return E(E(_Gs)[2]);});return A(_Gm,[[0,_Gv,_Gv]]);}),function(_Gw){return A(_Gl,[new T(function(){return A(_Gk,[[0,_0,new T(function(){var _Gx=E(E(_Gw)[1]);return [0,_Gx[1],_Gx[2],_r6,_Gx[4],_Gx[5]];})]]);}),function(_Gy){return A(_Gj,[new T(function(){return A(_Gu,[new T(function(){return E(E(_Gy)[2]);})]);}),function(_Gz){return A(_Gi,[new T(function(){var _GA=E(_Gz),_GB=_GA[2],_GC=E(_GA[1]);return _GC[0]==0?A(_Gh,[[0,_9,_GB]]):A(_om,[_Ga,function(_){return _Eb(_GC[1],_6R,_);},_GB]);}),function(_GD){var _GE=new T(function(){return !_sK(E(_GD)[1],_Ef)?[0]:E([1,_Gq,_9]);});return A(_Gg,[[0,[0,new T(function(){return A(_Go,[_Gt,_G7,_Gq,new T(function(){return E(_GE)[0]==0?false:true;}),_a]);}),[1,[0,_GE]]],new T(function(){return E(E(_GD)[2]);})]]);}]);}]);}]);}]);}]);};};},_GF=new T(function(){return _G8(_8B,_9H);}),_GG=unCStr("Green"),_GH=new T(function(){return A(_GF,[_e8,_GG]);}),_GI=function(_GJ,_){var _GK=A(_GH,[_GJ,_]),_GL=E(_GK),_GM=E(_GL[1]);return [0,[0,function(_GN,_){var _GO=A(_GM[1],[_GN,_]),_GP=A(_G6,[_GN,_]);return _GN;},_GM[2]],_GL[2]];},_GQ=new T(function(){return _tl(_GI,_vz);}),_GR=new T(function(){return _58(_4O,_DZ);}),_GS=new T(function(){return A(_GF,[_e8,_DZ]);}),_GT=function(_GU,_){var _GV=A(_GS,[_GU,_]),_GW=E(_GV),_GX=E(_GW[1]);return [0,[0,function(_GY,_){var _GZ=A(_GX[1],[_GY,_]),_H0=A(_GR,[_GY,_]);return _GY;},_GX[2]],_GW[2]];},_H1=new T(function(){return _tl(_GT,_vz);}),_H2=new T(function(){return _58(_4O,_E3);}),_H3=unCStr("Red"),_H4=new T(function(){return A(_GF,[_e8,_H3]);}),_H5=function(_H6,_){var _H7=A(_H4,[_H6,_]),_H8=E(_H7),_H9=E(_H8[1]);return [0,[0,function(_Ha,_){var _Hb=A(_H9[1],[_Ha,_]),_Hc=A(_H2,[_Ha,_]);return _Ha;},_H9[2]],_H8[2]];},_Hd=new T(function(){return _tl(_H5,_vz);}),_He=function(_Hf,_){var _Hg=A(_Hd,[_Hf,_]),_Hh=E(_Hg),_Hi=E(_Hh[1]),_Hj=A(_GQ,[_Hh[2],_]),_Hk=E(_Hj),_Hl=E(_Hk[1]),_Hm=A(_H1,[_Hk[2],_]),_Hn=E(_Hm),_Ho=E(_Hn[1]);return [0,[0,function(_Hp,_){var _Hq=A(_Hi[1],[_Hp,_]),_Hr=A(_Hl[1],[_Hp,_]),_Hs=A(_Ho[1],[_Hp,_]);return _Hp;},new T(function(){var _Ht=E(_Hi[2]);if(!_Ht[0]){return [0];}else{var _Hu=E(_Hl[2]);if(!_Hu[0]){return [0];}else{var _Hv=E(_Ho[2]);return _Hv[0]==0?[0]:[1,new T(function(){return [0,new T(function(){var _Hw=function(_Hx){var _Hy=E(_Hx);return _Hy[0]==0?E(new T(function(){var _Hz=function(_HA){var _HB=E(_HA);return _HB[0]==0?E(E(_Hv[1])[1]):[1,_HB[1],new T(function(){return _Hz(_HB[2]);})];};return _Hz(E(_Hu[1])[1]);})):[1,_Hy[1],new T(function(){return _Hw(_Hy[2]);})];};return _Hw(E(_Ht[1])[1]);})];})];}}})],_Hn[2]];},_HC=function(_HD){var _HE=new T(function(){return _58(_4O,[1,_y1,new T(function(){return _y3(_HD,_DR);})]);});return function(_HF,_){return [0,[0,function(_HG,_){var _HH=_4O(_DQ,_HG,_),_HI=A(_HE,[_HG,_]);return _HG;},_C1],_HF];};},_HJ=new T(function(){return _wk(_w6);}),_HK=function(_HL){return E(E(_HL)[11]);},_HM=function(_HN,_HO,_HP,_HQ){var _HR=new T(function(){return _oi(_HO);}),_HS=new T(function(){return _3h(_HR);}),_HT=new T(function(){return _3z([0,coercionToken],_HS,function(_HU){return _oz(_HR,_HU);},function(_HV,_HW){return _oC(_HR,_HV,_HW);});}),_HX=new T(function(){return _3f(_HR);}),_HY=new T(function(){return _2P(_HR);}),_HZ=new T(function(){return _2P(_HR);}),_I0=new T(function(){return _3f(_HR);}),_I1=new T(function(){return _2P(_HR);}),_I2=new T(function(){return _3f(_HR);}),_I3=new T(function(){return _2P(_HR);}),_I4=new T(function(){return _2P(_HR);}),_I5=new T(function(){return _HK(_HN);});return function(_I6,_I7){return A(_I4,[new T(function(){return A(_HT,[_I7]);}),function(_I8){var _I9=new T(function(){return E(E(_I8)[1]);}),_Ia=new T(function(){return _qQ(_HS,function(_Ib){return _om(_HO,_Ib);},_HP,_HQ,_HN,_I9);});return A(_I3,[new T(function(){var _Ic=new T(function(){return E(E(_I8)[2]);});return A(_I2,[[0,_Ic,_Ic]]);}),function(_Id){return A(_I1,[new T(function(){return A(_I0,[[0,_0,new T(function(){var _Ie=E(E(_Id)[1]);return [0,_Ie[1],_Ie[2],_r6,_Ie[4],_Ie[5]];})]]);}),function(_If){return A(_HZ,[new T(function(){return A(_Ia,[new T(function(){return E(E(_If)[2]);})]);}),function(_Ig){return A(_HY,[new T(function(){return A(_I6,[new T(function(){return E(E(_Ig)[2]);})]);}),function(_Ih){var _Ii=E(_Ih);return A(_HX,[[0,[0,new T(function(){return A(_I5,[_I9,E(_Ii[1])[1]]);}),new T(function(){var _Ij=E(E(_Ig)[1]);return _Ij[0]==2?[1,_Ij[1]]:[0];})],_Ii[2]]]);}]);}]);}]);}]);}]);};},_Ik=new T(function(){return _HM(_8B,_9H,_EY,_HJ);}),_Il=new T(function(){return _y3(_DZ,_DR);}),_Im=new T(function(){return _y3(_DZ,_DR);}),_In=new T(function(){return A(_p1,[_6D]);}),_Io=new T(function(){var _Ip=A(_EY,[_DZ]),_Iq=E(_In),_Ir=hs_eqWord64(_Ip[1],_Iq[1]);if(!E(_Ir)){return [1,_y1,_Il];}else{var _Is=hs_eqWord64(_Ip[2],_Iq[2]);return E(_Is)==0?[1,_y1,_Im]:E(_DZ);}}),_It=[0,_6P,_Io],_Iu=[1,_It,_9],_Iv=new T(function(){return _y(_7m,_Iu);}),_Iw=new T(function(){return _y3(_E1,_DR);}),_Ix=new T(function(){return _y3(_E1,_DR);}),_Iy=new T(function(){var _Iz=A(_EY,[_E1]),_IA=E(_In),_IB=hs_eqWord64(_Iz[1],_IA[1]);if(!E(_IB)){return [1,_y1,_Iw];}else{var _IC=hs_eqWord64(_Iz[2],_IA[2]);return E(_IC)==0?[1,_y1,_Ix]:E(_E1);}}),_ID=[0,_6P,_Iy],_IE=[1,_ID,_9],_IF=new T(function(){return _y(_7m,_IE);}),_IG=new T(function(){return _y3(_E3,_DR);}),_IH=new T(function(){return _y3(_E3,_DR);}),_II=new T(function(){var _IJ=A(_EY,[_E3]),_IK=E(_In),_IL=hs_eqWord64(_IJ[1],_IK[1]);if(!E(_IL)){return [1,_y1,_IG];}else{var _IM=hs_eqWord64(_IJ[2],_IK[2]);return E(_IM)==0?[1,_y1,_IH]:E(_E3);}}),_IN=[0,_6P,_II],_IO=[1,_IN,_9],_IP=new T(function(){return _y(_7m,_IO);}),_IQ=function(_IR,_){var _IS=A(_IP,[_IR,_]),_IT=_4O(_E3,_IS,_),_IU=A(_IF,[_IR,_]),_IV=_4O(_E1,_IU,_),_IW=A(_Iv,[_IR,_]),_IX=_4O(_DZ,_IW,_);return _IR;},_IY=[1,_E3],_IZ=[0,_IQ,_IY],_J0=function(_J1,_){return [0,_IZ,_J1];},_J2=new T(function(){return A(_Ik,[_J0]);}),_J3=new T(function(){return _tl(_J2,_vz);}),_J4=function(_J5,_){var _J6=_4j(_He,_G4,_J5,_),_J7=E(_J6),_J8=_4j(_FW,_DS,_J7[2],_),_J9=E(_J8),_Ja=_4j(_J3,_HC,_J9[2],_),_Jb=E(_Ja),_Jc=E(_Jb[1]);return [0,[0,function(_Jd,_){var _Je=A(_DP,[_Jd,_]),_Jf=A(E(_J7[1])[1],[_Jd,_]),_Jg=_5i(_Jd,_),_Jh=_5i(_Jd,_),_Ji=A(E(_J9[1])[1],[_Jd,_]),_Jj=_5i(_Jd,_),_Jk=_5i(_Jd,_),_Jl=A(_Jc[1],[_Jd,_]);return _Jd;},_Jc[2]],_Jb[2]];},_Jm=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_Jn=new T(function(){return _4V(_4O,_Jm);}),_Jo=function(_Jp){var _Jq=jsShow(E(_Jp)[1]);return fromJSStr(_Jq);},_Jr=function(_Js){var _Jt=new T(function(){return _Jo(_Js);});return function(_ce){return _1v(_Jt,_ce);};},_Ju=function(_Jv,_Jw,_Jx){var _Jy=E(_Jx);if(!_Jy[0]){return [0];}else{var _Jz=_Jy[2],_JA=E(_Jy[1]);return _Jv!=_JA[1]?[1,_JA,new T(function(){return _Ju(_Jv,_Jw,_Jz);})]:_1v(_Jw,new T(function(){return _Ju(_Jv,_Jw,_Jz);}));}},_JB=[0,45],_JC=function(_JD,_JE,_JF){var _JG=new T(function(){return A(_JD,[[0, -_JF]]);}),_JH=new T(function(){return E(_JE)[1]<=6?function(_JI){return [1,_JB,new T(function(){return A(_JG,[_JI]);})];}:function(_JJ){return [1,_3t,[1,_JB,new T(function(){return A(_JG,[[1,_3s,_JJ]]);})]];};});if(_JF>=0){var _JK=isDoubleNegativeZero(_JF);return E(_JK)==0?A(_JD,[[0,_JF]]):E(_JH);}else{return E(_JH);}},_JL=unCStr("canvas"),_JM=unCStr("id"),_JN=unCStr("canvas"),_JO=function(_JP,_JQ){var _JR=new T(function(){return A(_JP,[_JQ]);});return function(_JS,_){var _JT=jsCreateElem(toJSStr(E(_JN))),_JU=jsAppendChild(_JT,E(_JS)[1]),_JV=[0,_JT],_JW=A(_JR,[_JV,_]);return _JV;};},_JX=new T(function(){return _JO(_v6,_2M);}),_JY=function(_JZ,_){var _K0=A(_JX,[_JZ,_]),_K1=A(_1,[_n,_K0,_JM,_JL,_]);return _K0;},_K2=[0,_JY,_C1],_K3=function(_K4,_){return [0,_K2,_K4];},_K5=unCStr("Pattern match failure in do expression at main.hs:179:5-12"),_K6=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_K7=new T(function(){return _yV(_K6);}),_K8=function(_K9,_){var _Ka=jsHasCtx2D(_K9);if(!E(_Ka)){return _a;}else{var _Kb=jsGetCtx2D(_K9);return [1,[0,[0,_Kb],[0,_K9]]];}},_Kc=function(_Kd,_){return _K8(E(_Kd)[1],_);},_Ke=function(_Kf,_Kg){return A(_Kf,[function(_){var _Kh=jsFind(toJSStr(E(_Kg))),_Ki=E(_Kh);return _Ki[0]==0?_a:_Kc(_Ki[1],_);}]);},_Kj=new T(function(){return _Ke(_n,_JL);}),_Kk=[0,-10],_Kl=[0,0],_Km=[0,_Kk,_Kl],_Kn=[0,10],_Ko=[0,_Kn,_Kl],_Kp=[1,_Ko,_9],_Kq=[1,_Km,_Kp],_Kr=function(_Ks,_){return _0;},_Kt=function(_Ku){var _Kv=E(_Ku);if(!_Kv[0]){return E(_Kr);}else{var _Kw=E(_Kv[1]);return function(_Kx,_){var _Ky=E(_Kx)[1],_Kz=jsMoveTo(_Ky,E(_Kw[1])[1],E(_Kw[2])[1]);return (function(_KA,_){while(1){var _KB=E(_KA);if(!_KB[0]){return _0;}else{var _KC=E(_KB[1]),_KD=jsLineTo(_Ky,E(_KC[1])[1],E(_KC[2])[1]);_KA=_KB[2];continue;}}})(_Kv[2],_);};}},_KE=new T(function(){return _Kt(_Kq);}),_KF=[0,30],_KG=[0,_Kl,_KF],_KH=[0,-30],_KI=[0,_Kl,_KH],_KJ=[1,_KI,_9],_KK=[1,_KG,_KJ],_KL=new T(function(){return _Kt(_KK);}),_KM=new T(function(){return [0,0/0];}),_KN=new T(function(){return [0,-1/0];}),_KO=new T(function(){return [0,1/0];}),_KP=[0,0],_KQ=function(_KR,_KS){while(1){var _KT=E(_KR);if(!_KT[0]){_KR=[1,I_fromInt(_KT[1])];continue;}else{var _KU=E(_KS);if(!_KU[0]){_KR=_KT;_KS=[1,I_fromInt(_KU[1])];continue;}else{return I_fromRat(_KT[1],_KU[1]);}}}},_KV=function(_KW,_KX){var _KY=E(_KW);if(!_KY[0]){var _KZ=_KY[1],_L0=E(_KX);return _L0[0]==0?_KZ==_L0[1]:I_compareInt(_L0[1],_KZ)==0?true:false;}else{var _L1=_KY[1],_L2=E(_KX);return _L2[0]==0?I_compareInt(_L1,_L2[1])==0?true:false:I_compare(_L1,_L2[1])==0?true:false;}},_L3=function(_L4,_L5){var _L6=E(_L4);if(!_L6[0]){var _L7=_L6[1],_L8=E(_L5);return _L8[0]==0?_L7<_L8[1]:I_compareInt(_L8[1],_L7)>0;}else{var _L9=_L6[1],_La=E(_L5);return _La[0]==0?I_compareInt(_L9,_La[1])<0:I_compare(_L9,_La[1])<0;}},_Lb=function(_Lc,_Ld){return !_KV(_Ld,_KP)?[0,_KQ(_Lc,_Ld)]:!_KV(_Lc,_KP)?!_L3(_Lc,_KP)?E(_KO):E(_KN):E(_KM);},_Le=function(_Lf){var _Lg=E(_Lf);return _Lb(_Lg[1],_Lg[2]);},_Lh=function(_Li){return [0,1/E(_Li)[1]];},_Lj=function(_Lk){var _Ll=E(_Lk),_Lm=_Ll[1];return _Lm<0?[0, -_Lm]:E(_Ll);},_Ln=function(_Lo){var _Lp=E(_Lo);return _Lp[0]==0?_Lp[1]:I_toNumber(_Lp[1]);},_Lq=function(_Lr){return [0,_Ln(_Lr)];},_Ls=[0,0],_Lt=[0,1],_Lu=[0,-1],_Lv=function(_Lw){var _Lx=E(_Lw)[1];return _Lx!=0?_Lx<=0?E(_Lu):E(_Lt):E(_Ls);},_Ly=function(_Lz,_LA){return [0,E(_Lz)[1]-E(_LA)[1]];},_LB=function(_LC){return [0, -E(_LC)[1]];},_LD=function(_LE,_LF){return [0,E(_LE)[1]+E(_LF)[1]];},_LG=function(_LH,_LI){return [0,E(_LH)[1]*E(_LI)[1]];},_LJ=[0,_LD,_LG,_Ly,_LB,_Lj,_Lv,_Lq],_LK=function(_LL,_LM){return [0,E(_LL)[1]/E(_LM)[1]];},_LN=[0,_LJ,_LK,_Lh,_Le],_LO=function(_LP,_LQ){return E(_LP)[1]!=E(_LQ)[1]?true:false;},_LR=function(_LS,_LT){return E(_LS)[1]==E(_LT)[1];},_LU=[0,_LR,_LO],_LV=function(_LW,_LX){return E(_LW)[1]<E(_LX)[1];},_LY=function(_LZ,_M0){return E(_LZ)[1]<=E(_M0)[1];},_M1=function(_M2,_M3){return E(_M2)[1]>E(_M3)[1];},_M4=function(_M5,_M6){return E(_M5)[1]>=E(_M6)[1];},_M7=function(_M8,_M9){var _Ma=E(_M8)[1],_Mb=E(_M9)[1];return _Ma>=_Mb?_Ma!=_Mb?2:1:0;},_Mc=function(_Md,_Me){var _Mf=E(_Md),_Mg=E(_Me);return _Mf[1]>_Mg[1]?E(_Mf):E(_Mg);},_Mh=function(_Mi,_Mj){var _Mk=E(_Mi),_Ml=E(_Mj);return _Mk[1]>_Ml[1]?E(_Ml):E(_Mk);},_Mm=[0,_LU,_M7,_LV,_M4,_M1,_LY,_Mc,_Mh],_Mn=[0,1],_Mo=function(_Mp){return E(E(_Mp)[1]);},_Mq=function(_Mr){return E(E(_Mr)[2]);},_Ms=function(_Mt){return E(E(_Mt)[6]);},_Mu=[0,2],_Mv=function(_Mw,_Mx){var _My=E(_Mx);return [1,_My,new T(function(){var _Mz=_Mo(_Mw);return _Mv(_Mw,A(_Mz[1],[_My,new T(function(){return A(_Mz[7],[_Mn]);})]));})];},_MA=function(_MB,_MC){var _MD=E(_MC);if(!_MD[0]){return [0];}else{var _ME=_MD[1];return !A(_MB,[_ME])?[0]:[1,_ME,new T(function(){return _MA(_MB,_MD[2]);})];}},_MF=function(_MG,_MH,_MI,_MJ){var _MK=new T(function(){return _Ms(_MG);});return _MA(function(_ML){return A(_MK,[_ML,new T(function(){var _MM=_Mo(_MH),_MN=_MM[7];return A(_MM[1],[_MJ,new T(function(){return A(_Mq,[_MH,new T(function(){return A(_MN,[_Mn]);}),new T(function(){return A(_MN,[_Mu]);})]);})]);})]);},_Mv(_MH,_MI));},_MO=new T(function(){return _MF(_Mm,_LN,_Kk,_Kn);}),_MP=function(_MQ,_MR){var _MS=E(_MQ);if(!_MS[0]){return [0];}else{var _MT=E(_MR);return _MT[0]==0?[0]:[1,[0,_MS[1],_MT[1]],new T(function(){return _MP(_MS[2],_MT[2]);})];}},_MU=function(_MV,_MW,_){var _MX=function(_MY,_){var _MZ=E(_MY);if(!_MZ[0]){return _9;}else{var _N0=A(_K7,[E(toJSStr(_Ju(120,new T(function(){return A(_JC,[_Jr,_p8,E(_MZ[1])[1],_9]);}),_MV))),_]),_N1=_MX(_MZ[2],_);return [1,[0,_N0],_N1];}};return _4j(_K3,function(_N2,_C8,_){return (function(_N3,_){return [0,[0,function(_N4,_){var _N5=A(_Kj,[_]),_N6=E(_N5);if(!_N6[0]){var _N7=_2K(_K5,_);return _N4;}else{var _N8=_MX(_MO,_),_N9=E(_N6[1]),_Na=jsResetCanvas(E(_N9[2])[1]),_Nb=E(_N9[1]),_Nc=_Nb[1],_Nd=jsPushState(_Nc),_Ne=jsScale(_Nc,3,1),_Nf=jsPushState(_Nc),_Ng=jsTranslate(_Nc,50,130),_Nh=jsPushState(_Nc),_Ni=jsRotate(_Nc,3.141592653589793),_Nj=jsBeginPath(_Nc),_Nk=A(_KE,[_Nb,_]),_Nl=A(_KL,[_Nb,_]),_Nm=A(_Kt,[_MP(_MO,_N8),_Nb,_]),_Nn=jsStroke(_Nc),_No=jsPopState(_Nc),_Np=jsPopState(_Nc),_Nq=jsPopState(_Nc);return _N4;}},_C1],_N3];})(_C8,_);},_MW,_);},_Nr=unCStr("Math.pow(x,2)+x+10;"),_Ns=[1,_Nr],_Nt=new T(function(){return _rR(_8B,_9H,_p1,_yy,_wm);}),_Nu=new T(function(){return A(_Nt,[_a,_9G,_Ns]);}),_Nv=new T(function(){return _tl(_Nu,_9F);}),_Nw=function(_Nx,_){var _Ny=A(_Nv,[_Nx,_]),_Nz=E(_Ny),_NA=E(_Nz[1]);return [0,[0,function(_NB,_){var _NC=A(_NA[1],[_NB,_]),_ND=_5i(_NB,_);return _NB;},new T(function(){var _NE=E(_NA[2]);return _NE[0]==0?E(_Ns):E(_NE);})],_Nz[2]];},_NF=function(_NG,_){var _NH=_4j(_Nw,_MU,_NG,_),_NI=E(_NH),_NJ=E(_NI[1]),_NK=new T(function(){return _vm(_v6,_NJ[1]);});return [0,[0,function(_NL,_){var _NM=A(_Jn,[_NL,_]),_NN=A(_NK,[_NL,_]);return _NL;},_NJ[2]],_NI[2]];},_NO=unCStr("work?"),_NP=function(_NQ,_NR,_NS){var _NT=E(_NR);return A(_NT[1],[new T(function(){var _NU=E(_NQ);return E(_NS);}),function(_NV){return A(_NT[3],[[1,_3n,new T(function(){var _NW=E(_NV);return _1v(_3u(0,E(_NW[2])[1],_9),_NW[1]);})]]);}]);},_NX=function(_NY){return E(E(_NY)[5]);},_NZ=unCStr("for"),_O0=unCStr("label"),_O1=function(_O2,_O3){var _O4=new T(function(){return _8Y(_O3);}),_O5=new T(function(){return _90(_O4);}),_O6=new T(function(){return _NP([0,coercionToken],_3h(_O2),function(_O7){return _oz(_O2,_O7);});}),_O8=new T(function(){return _3f(_O2);}),_O9=new T(function(){return _2P(_O2);}),_Oa=new T(function(){return _E5(_O3);}),_Ob=new T(function(){return _2P(_O2);}),_Oc=new T(function(){return _NX(_O3);});return function(_Od,_Oe){var _Of=new T(function(){return A(_Oc,[_O0,_Od]);});return function(_Og){return A(_Ob,[new T(function(){return A(_O6,[_Og]);}),function(_Oh){var _Oi=new T(function(){return A(_Oa,[_Of,[1,[0,_NZ,new T(function(){return E(E(_Oh)[1]);})],_9]]);});return A(_O9,[new T(function(){return A(_Oe,[new T(function(){return E(E(_Oh)[2]);})]);}),function(_Oj){var _Ok=E(_Oj),_Ol=E(_Ok[1]);return A(_O8,[[0,[0,new T(function(){return A(_O5,[_Oi,_Ol[1]]);}),_Ol[2]],_Ok[2]]]);}]);}]);};};},_Om=new T(function(){return _O1(_2O,_8B);}),_On=new T(function(){return _Ei(_8B,_9H,_EY,_EZ);}),_Oo=function(_Op,_Oq){return A(_Om,[function(_C8,_){return _4O(_Op,_C8,_);},new T(function(){return _tl(new T(function(){return A(_On,[_Op,_Oq]);}),_vz);})]);},_Or=function(_Os){return _Oo(_NO,_Os);},_Ot=unCStr("study?"),_Ou=function(_Os){return _Oo(_Ot,_Os);},_Ov=[1,_Ou,_9],_Ow=[1,_Or,_Ov],_Ox=new T(function(){return A(_FV,[_Ow]);}),_Oy=unCStr("Do you "),_Oz=new T(function(){return _58(_4O,_Oy);}),_OA=function(_OB,_){var _OC=A(_Ox,[_OB,_]),_OD=E(_OC),_OE=E(_OD[1]);return [0,[0,function(_OF,_){var _OG=A(_Oz,[_OF,_]),_OH=A(_OE[1],[_OF,_]),_OI=_5i(_OF,_);return _OF;},_OE[2]],_OD[2]];},_OJ=unCStr("ok"),_OK=[1,_OJ],_OL=new T(function(){return A(_yA,[_OK]);}),_OM=new T(function(){return _tl(_OL,_vz);}),_ON=unCStr("do you enjoy your work? "),_OO=new T(function(){return _58(_4O,_ON);}),_OP=function(_OQ,_OR,_){return [0,[0,_2M,[1,_OQ]],_OR];},_OS=function(_OT,_OU,_OV,_){return _4j(_OT,function(_OW){return E(_OU);},_OV,_);},_OX=function(_OY,_OZ,_F,_){return _OS(_OY,_OZ,_F,_);},_P0=function(_P1){return err(_P1);},_P2=[0,_4j,_OX,_OP,_P0],_P3=function(_P4,_P5,_P6,_P7,_P8,_P9){var _Pa=new T(function(){return _90(_P4);});return A(_P5,[new T(function(){return A(_P7,[_P9]);}),function(_Pb){var _Pc=E(_Pb),_Pd=E(_Pc[1]);return A(_P5,[new T(function(){return A(_P8,[_Pc[2]]);}),function(_Pe){var _Pf=E(_Pe),_Pg=E(_Pf[1]);return A(_P6,[[0,[0,new T(function(){return A(_Pa,[_Pd[1],_Pg[1]]);}),new T(function(){var _Ph=E(_Pd[2]);return _Ph[0]==0?E(_Pg[2]):E(_Ph);})],_Pf[2]]]);}]);}]);},_Pi=function(_Pj,_Pk,_Pl,_Pm,_Pn,_Po){var _Pp=new T(function(){return _E5(_Pl);});return A(_Pj,[new T(function(){return A(_Pm,[_Po]);}),function(_Pq){var _Pr=E(_Pq),_Ps=E(_Pr[1]);return A(_Pk,[[0,[0,new T(function(){return A(_Pp,[_Ps[1],_Pn]);}),_Ps[2]],_Pr[2]]]);}]);},_Pt=function(_Pu){return E(E(_Pu)[12]);},_Pv=function(_Pw,_Px,_Py,_Pz,_PA,_PB,_PC){var _PD=new T(function(){return A(_Pt,[_Pw,new T(function(){var _PE=A(_Py,[_PA]),_PF=E(_In),_PG=hs_eqWord64(_PE[1],_PF[1]);if(!E(_PG)){return A(_rd,[_Pz,_PA]);}else{var _PH=hs_eqWord64(_PE[2],_PF[2]);return E(_PH)==0?A(_rd,[_Pz,_PA]):E(_PA);}}),_PB,_PC]);}),_PI=new T(function(){return _3f(_Px);});return function(_PJ){return A(_PI,[[0,[0,_PD,[1,_PA]],_PJ]]);};},_PK=[0,_7o,_Ef],_PL=[1,_PK,_9],_PM=[0,_7o,_Ef],_PN=[1,_PM,_9],_PO=function(_PP,_PQ,_PR,_PS){var _PT=new T(function(){return _HM(_PS,_PR,_p1,_wm);}),_PU=new T(function(){return A(_3f,[_PP,_e8]);}),_PV=new T(function(){return A(_3f,[_PP,_e9]);}),_PW=new T(function(){return _8Y(_PS);}),_PX=new T(function(){return _oi(_PR);}),_PY=new T(function(){return _oP(_PS);}),_PZ=new T(function(){return _2P(_PP);});return function(_Q0,_Q1,_Q2){return A(_PZ,[new T(function(){var _Q3=new T(function(){return !E(_Q0)?E(_PN):[0];}),_Q4=new T(function(){return _Pv(_PS,_PX,_p1,_yy,_Q2,new T(function(){return A(_PY,[_Q2]);}),_e8);}),_Q5=new T(function(){return !E(_Q0)?[0]:E(_PL);}),_Q6=new T(function(){return _Pv(_PS,_PX,_p1,_yy,_Q1,new T(function(){return A(_PY,[_Q1]);}),_e8);});return A(_PT,[function(_Q7){var _Q8=E(_PX);return _P3(_PW,_Q8[1],_Q8[3],function(_Q9){var _Qa=E(_PX);return _Pi(_Qa[1],_Qa[3],_PS,_Q6,_Q5,_Q9);},function(_Qb){var _Qc=E(_PX);return _Pi(_Qc[1],_Qc[3],_PS,_Q4,_Q3,_Qb);},_Q7);}]);}),function(_Qd){return !_sK(_Qd,_Q1)?E(_PU):E(_PV);}]);};},_Qe=new T(function(){return _PO(_P2,_8K,_9H,_8B);}),_Qf=unCStr("yes"),_Qg=unCStr("no"),_Qh=new T(function(){return A(_Qe,[_e9,_Qf,_Qg]);}),_Qi=function(_Qj,_){var _Qk=A(_Qh,[_Qj,_]),_Ql=E(_Qk),_Qm=E(_Ql[1]),_Qn=A(_OM,[_Ql[2],_]),_Qo=E(_Qn);return [0,[0,function(_Qp,_){var _Qq=A(_OO,[_Qp,_]),_Qr=A(_Qm[1],[_Qp,_]),_Qs=A(E(_Qo[1])[1],[_Qp,_]),_Qt=_5i(_Qp,_);return _Qp;},new T(function(){var _Qu=E(_Qm[2]);return _Qu[0]==0?[0]:[1,[0,_Qu[1]]];})],_Qo[2]];},_Qv=unCStr("do you study in "),_Qw=new T(function(){return _58(_4O,_Qv);}),_Qx=unCStr("University"),_Qy=function(_Os){return _Oo(_Qx,_Os);},_Qz=unCStr("High School"),_QA=function(_Os){return _Oo(_Qz,_Os);},_QB=[1,_QA,_9],_QC=[1,_Qy,_QB],_QD=new T(function(){return A(_FV,[_QC]);}),_QE=function(_QF,_){var _QG=A(_QD,[_QF,_]),_QH=E(_QG),_QI=E(_QH[1]);return [0,[0,function(_QJ,_){var _QK=A(_Qw,[_QJ,_]),_QL=A(_QI[1],[_QJ,_]);return _QJ;},new T(function(){var _QM=E(_QI[2]);return _QM[0]==0?[0]:[1,[1,_QM[1]]];})],_QH[2]];},_QN=new T(function(){return _ax("main.hs:(278,11)-(285,64)|case");}),_QO=unCStr(" that you enjoy your work"),_QP=unCStr("False"),_QQ=new T(function(){return _1v(_QP,_QO);}),_QR=unCStr("True"),_QS=new T(function(){return _1v(_QR,_QO);}),_QT=[0,32],_QU=function(_QV,_QW){var _QX=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You are ",new T(function(){return _1v(_QV,[1,_QT,_QW]);}));}));});return function(_ce,_s1){return _4j(_OA,function(_QY){var _QZ=new T(function(){return !_sK(_QY,_Ot)?!_sK(_QY,_NO)?E(_QN):E(_Qi):E(_QE);});return function(_ce,_s1){return _4j(_QZ,function(_R0){return function(_R1,_){var _R2=A(new T(function(){var _R3=E(_R0);if(!_R3[0]){var _R4=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_R3[1])?E(_QQ):E(_QS);}));}));});return function(_R5,_){return [0,[0,function(_R6,_){var _R7=A(_R4,[_R6,_]);return _R6;},_a],_R5];};}else{var _R8=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You study at the ",_R3[1]);}));});return function(_R9,_){return [0,[0,function(_Ra,_){var _Rb=A(_R8,[_Ra,_]);return _Ra;},_a],_R9];};}}),[_R1,_]),_Rc=E(_R2),_Rd=E(_Rc[1]);return [0,[0,function(_Re,_){var _Rf=A(_QX,[_Re,_]),_Rg=A(_Rd[1],[_Re,_]);return _Re;},_Rd[2]],_Rc[2]];};},_ce,_s1);};},_ce,_s1);};},_Rh=function(_Ri){var _Rj=E(_Ri);return _QU(_Rj[1],_Rj[2]);},_Rk=unCStr("Who are you? "),_Rl=new T(function(){return _4V(_4O,_Rk);}),_Rm=unCStr("name"),_Rn=unCStr("placeholder"),_Ro=[0,_Rn,_Rm],_Rp=[1,_Ro,_9],_Rq=unCStr("surname"),_Rr=[0,_Rn,_Rq],_Rs=[1,_Rr,_9],_Rt=[1,_OJ],_Ru=new T(function(){return A(_yA,[_Rt]);}),_Rv=new T(function(){return _tl(_Ru,_vz);}),_Rw=new T(function(){return A(_Nt,[_a,_9G,_a]);}),_Rx=new T(function(){return A(_Nt,[_a,_9G,_a]);}),_Ry=function(_Rz,_){var _RA=A(_Rx,[_Rz,_]),_RB=E(_RA),_RC=E(_RB[1]),_RD=A(_Rw,[_RB[2],_]),_RE=E(_RD),_RF=E(_RE[1]),_RG=A(_Rv,[_RE[2],_]),_RH=E(_RG),_RI=new T(function(){return _y(_RF[1],_Rs);}),_RJ=new T(function(){return _y(_RC[1],_Rp);});return [0,[0,function(_RK,_){var _RL=A(_Rl,[_RK,_]),_RM=A(_RJ,[_RK,_]),_RN=_5i(_RK,_),_RO=A(_RI,[_RK,_]),_RP=_5i(_RK,_),_RQ=A(E(_RH[1])[1],[_RK,_]),_RR=_5i(_RK,_);return _RK;},new T(function(){var _RS=E(_RC[2]);if(!_RS[0]){return [0];}else{var _RT=E(_RF[2]);return _RT[0]==0?[0]:[1,[0,_RS[1],_RT[1]]];}})],_RH[2]];},_RU=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_RV=unCStr("This formulary is the same than the one "),_RW=[0,97],_RX=[1,_RW,_9],_RY=function(_RZ,_S0){var _S1=new T(function(){return A(_RZ,[_S0]);});return function(_S2,_){var _S3=jsCreateElem(toJSStr(_RX)),_S4=jsAppendChild(_S3,E(_S2)[1]),_S5=[0,_S3],_S6=A(_S1,[_S5,_]);return _S5;};},_S7=unCStr("run in the server by MFlow"),_S8=new T(function(){return _RY(_4O,_S7);}),_S9=unCStr("href"),_Sa=function(_Sb,_){var _Sc=_4O(_RV,_Sb,_),_Sd=A(_S8,[_Sb,_]),_Se=A(_1,[_n,_Sd,_S9,_RU,_]);return _Sb;},_Sf=new T(function(){return _4V(_v6,_Sa);}),_Sg=unCStr("Fields of a form appear in sequence. Some of the fields trigger events instantly. Some others use a button to trigger them. It also contains option buttons, radio buttons etc"),_Sh=new T(function(){return _4V(_4O,_Sg);}),_Si=function(_Sj,_){var _Sk=_4j(_Ry,_Rh,_Sj,_),_Sl=E(_Sk),_Sm=E(_Sl[1]);return [0,[0,function(_Sn,_){var _So=A(_Sh,[_Sn,_]),_Sp=A(_Sf,[_Sn,_]),_Sq=A(_Sm[1],[_Sn,_]);return _Sn;},_Sm[2]],_Sl[2]];},_Sr=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_Ss=new T(function(){return _4V(_4O,_Sr);}),_St=[1,_5x],_Su=unCStr("GalleryIndex"),_Sv=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_A0,_A1,_Su],_Sw=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Sv,_9],_Sx=function(_Sy){return E(_Sw);},_Sz=new T(function(){return _Cv(_S,_3m,_Q,_Sx);}),_SA=function(_SB,_){var _SC=A(_Sz,[_SB,_]);return [0,[0,_vv,new T(function(){var _SD=E(E(_SC)[1]);return _SD[0]==0?E(_St):E(_SD);})],new T(function(){return E(E(_SC)[2]);})];},_SE=unCStr("100%"),_SF=[0,62],_SG=[1,_SF,_9],_SH=[1,_SG],_SI=new T(function(){return A(_yA,[_SH]);}),_SJ=new T(function(){return _tl(_SI,_vz);}),_SK=function(_SL){return E(_SJ);},_SM=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_SN=[1,_SM,_9],_SO=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_SP=[1,_SO,_SN],_SQ=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_SR=[1,_SQ,_SP],_SS=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_ST=[1,_SS,_SR],_SU=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_SV=[1,_SU,_ST],_SW=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_SX=[1,_SW,_SV],_SY=unCStr("height"),_SZ=unCStr("img"),_T0=function(_T1,_){var _T2=jsCreateElem(toJSStr(E(_SZ))),_T3=jsAppendChild(_T2,E(_T1)[1]);return [0,_T2];},_T4=function(_T5,_T6){while(1){var _T7=E(_T5);if(!_T7[0]){return E(_T6);}else{_T5=_T7[2];var _T8=_T6+1|0;_T6=_T8;continue;}}},_T9=new T(function(){return [0,_T4(_SX,0)-1|0];}),_Ta=[0,_2M,_5m],_Tb=unCStr("src"),_Tc=unCStr("width"),_Td=function(_Te){return function(_ce,_s1){return _4j(function(_C8,_){return _4j(_BZ,function(_Tf){return function(_Tg,_){return [0,_Ta,new T(function(){var _Th=E(_Tf);return [0,_Th[1],_Th[2],_Th[3],_Th[4],new T(function(){return _BM(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Sv,_9,new T(function(){var _Ti=E(_Te)[1];return _Ti!=E(_T9)[1]?[0,_Ti+1|0]:E(_5x);}),_Th[5]);})];})];};},_C8,_);},function(_Tj,_C8,_){return (function(_C8,_){return _4j(function(_Tk,_){return [0,[0,function(_Tl,_){var _Tm=_T0(_Tl,_),_Tn=A(_1,[_n,_Tm,_Tb,new T(function(){var _To=E(_Te)[1];return _To>=0?_wr(_SX,_To):E(_wo);}),_]),_Tp=A(_1,[_n,_Tm,_Tc,_SE,_]),_Tq=A(_1,[_n,_Tm,_SY,_SE,_]),_Tr=_5i(_Tl,_);return _Tl;},_C1],_Tk];},_SK,_C8,_);})(_C8,_);},_ce,_s1);};},_Ts=function(_C8,_){return _4j(_SA,_Td,_C8,_);},_Tt=function(_Tu,_Tv,_){return _Tw(_Tv,_);},_Tx=function(_C8,_){return _yY(_Ts,_Tt,_C8,_);},_Ty=[0,20000],_Tz=new T(function(){return _3z(_S,_3m,_Q,_N);}),_TA=function(_TB,_TC,_TD,_){var _TE=A(_Tz,[_TD,_]),_TF=new T(function(){return E(E(_TE)[1]);}),_TG=new T(function(){return [0,_TH];}),_TH=function(_){var _TI=jsFind(toJSStr(E(_TF))),_TJ=E(_TI);if(!_TJ[0]){return _0;}else{var _TK=E(_TJ[1]),_TL=jsClearChildren(_TK[1]),_TM=E(_m)[1],_TN=takeMVar(_TM),_TO=A(_TC,[_TN,_]),_TP=E(_TO),_TQ=E(_TP[1]),_=putMVar(_TM,_TP[2]),_TR=A(_TQ[1],[_TK,_]),_TS=E(_TQ[2]);if(!_TS[0]){var _TT=jsSetTimeout(E(_TB)[1],E(_TG)[1]);return _0;}else{var _TU=E(_TS[1]);return _0;}}},_TV=jsSetTimeout(E(_TB)[1],E(_TG)[1]);return _yH(_TF,_TC,new T(function(){return E(E(_TE)[2]);}),_);},_Tw=function(_TW,_){var _TX=_TA(_Ty,_Tx,_TW,_),_TY=E(_TX),_TZ=E(_TY[1]);return [0,[0,function(_U0,_){var _U1=A(_Ss,[_U0,_]),_U2=A(_TZ[1],[_U0,_]);return _U0;},_TZ[2]],_TY[2]];},_U3=function(_U4,_U5,_U6){return A(_U4,[[1,_2i,new T(function(){return A(_U5,[_U6]);})]]);},_U7=unCStr("Key "),_U8=unCStr("Mouse "),_U9=unCStr("Click "),_Ua=unCStr("NoData"),_Ub=function(_Uc){return _1v(_Ua,_Uc);},_Ud=unCStr(": empty list"),_Ue=unCStr("Prelude."),_Uf=function(_Ug){return err(_1v(_Ue,new T(function(){return _1v(_Ug,_Ud);})));},_Uh=unCStr("foldr1"),_Ui=new T(function(){return _Uf(_Uh);}),_Uj=function(_Uk,_Ul){var _Um=E(_Ul);if(!_Um[0]){return E(_Ui);}else{var _Un=_Um[1],_Uo=E(_Um[2]);return _Uo[0]==0?E(_Un):A(_Uk,[_Un,new T(function(){return _Uj(_Uk,_Uo);})]);}},_Up=[0,32],_Uq=function(_Ur,_Us){var _Ut=E(_Us);switch(_Ut[0]){case 0:return E(_Ub);case 1:var _Uu=function(_Uv){return _3u(11,E(_Ut[1])[1],[1,_Up,new T(function(){var _Uw=E(_Ut[2]);return [1,_3t,new T(function(){return A(_Uj,[_U3,[1,function(_Ux){return _3u(0,E(_Uw[1])[1],_Ux);},[1,function(_Uy){return _3u(0,E(_Uw[2])[1],_Uy);},_9]],[1,_3s,_Uv]]);})];})]);};return E(_Ur)[1]<11?function(_Uz){return _1v(_U9,new T(function(){return _Uu(_Uz);}));}:function(_UA){return [1,_3t,new T(function(){return _1v(_U9,new T(function(){return _Uu([1,_3s,_UA]);}));})];};case 2:var _UB=function(_UC){return _1v(_U8,new T(function(){var _UD=E(_Ut[1]);return [1,_3t,new T(function(){return A(_Uj,[_U3,[1,function(_UE){return _3u(0,E(_UD[1])[1],_UE);},[1,function(_UF){return _3u(0,E(_UD[2])[1],_UF);},_9]],[1,_3s,_UC]]);})];}));};return E(_Ur)[1]<11?E(_UB):function(_UG){return [1,_3t,new T(function(){return _UB([1,_3s,_UG]);})];};default:var _UH=_Ut[1];return E(_Ur)[1]<11?function(_UI){return _1v(_U7,new T(function(){return _3u(11,E(_UH)[1],_UI);}));}:function(_UJ){return [1,_3t,new T(function(){return _1v(_U7,new T(function(){return _3u(11,E(_UH)[1],[1,_3s,_UJ]);}));})];};}},_UK=function(_UL){var _UM=new T(function(){return _4V(_4O,new T(function(){var _UN=E(_UL);return _1v(_UN[1],[1,_QT,new T(function(){return A(_Uq,[_p8,_UN[2],_9]);})]);}));});return function(_UO,_){return [0,[0,_UM,_C1],_UO];};},_UP=function(_){var _UQ=E(_sr)[1],_UR=takeMVar(_UQ),_=putMVar(_UQ,_UR);return _UR;},_US=function(_UT,_){var _UU=0;if(!E(_UU)){var _UV=_UP();return [0,[0,_2M,[1,_UV]],_UT];}else{var _UW=E(_sr)[1],_UX=takeMVar(_UW),_=putMVar(_UW,_UX);return [0,[0,_2M,[1,_UX]],_UT];}},_UY=function(_C8,_){return _4j(_US,_UK,_C8,_);},_UZ=function(_V0){return E(_UY);},_V1=[12,coercionToken],_V2=[9,coercionToken],_V3=[11,coercionToken],_V4=[5,coercionToken],_V5=[10,coercionToken],_V6=[6,coercionToken],_V7=[7,coercionToken],_V8=unCStr("height:100px;background-color:lightgreen;position:relative"),_V9=unCStr("div"),_Va=function(_Vb,_Vc){var _Vd=new T(function(){return A(_Vb,[_Vc]);});return function(_Ve,_){var _Vf=jsCreateElem(toJSStr(E(_V9))),_Vg=jsAppendChild(_Vf,E(_Ve)[1]),_Vh=[0,_Vf],_Vi=A(_Vd,[_Vh,_]);return _Vh;};},_Vj=unCStr("h1"),_Vk=function(_Vl,_Vm){var _Vn=new T(function(){return A(_Vl,[_Vm]);});return function(_Vo,_){var _Vp=jsCreateElem(toJSStr(E(_Vj))),_Vq=jsAppendChild(_Vp,E(_Vo)[1]),_Vr=[0,_Vp],_Vs=A(_Vn,[_Vr,_]);return _Vr;};},_Vt=unCStr("Mouse events here"),_Vu=new T(function(){return _Vk(_4O,_Vt);}),_Vv=new T(function(){return _Va(_v6,_Vu);}),_Vw=function(_Vx,_){var _Vy=A(_Vv,[_Vx,_]),_Vz=A(_1,[_n,_Vy,_Ds,_V8,_]);return _Vy;},_VA=[0,_Vw,_C1],_VB=function(_VC,_){return [0,_VA,_VC];},_VD=new T(function(){return _tl(_VB,_V7);}),_VE=new T(function(){return _tl(_VD,_V6);}),_VF=new T(function(){return _tl(_VE,_V5);}),_VG=new T(function(){return _tl(_VF,_V4);}),_VH=new T(function(){return _tl(_VG,_V3);}),_VI=new T(function(){return _tl(_VH,_vz);}),_VJ=new T(function(){return _tl(_VI,_V2);}),_VK=new T(function(){return _tl(_VJ,_V1);}),_VL=unCStr("http://todomvc.com"),_VM=unCStr("Work in progress for a todo application to be added to "),_VN=unCStr("todomvc.com"),_VO=new T(function(){return _RY(_4O,_VN);}),_VP=function(_VQ,_){var _VR=_4O(_VM,_VQ,_),_VS=A(_VO,[_VQ,_]),_VT=A(_1,[_n,_VS,_S9,_VL,_]);return _VQ;},_VU=new T(function(){return _4V(_v6,_VP);}),_VV=unCStr("Tasks"),_VW=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_A0,_A1,_VV],_VX=2,_VY=unCStr("head"),_VZ=new T(function(){return _Uf(_VY);}),_W0=function(_W1,_W2,_W3,_W4,_){var _W5=A(_W3,[_W4,_]),_W6=E(_W5),_W7=_W6[2],_W8=E(_W6[1]),_W9=_W8[1],_Wa=_W8[2],_Wb=E(_W1),_Wc=jsFind(toJSStr(_Wb)),_Wd=E(_Wc);if(!_Wd[0]){return _3N(_Wb);}else{var _We=_Wd[1];switch(E(_W2)){case 0:var _Wf=A(_W9,[_We,_]);return [0,[0,_2M,_Wa],_W7];case 1:var _Wg=jsCreateElem(toJSStr(E(_G))),_Wh=E(_We)[1],_Wi=jsGetChildren(_Wh),_Wj=E(_Wi);if(!_Wj[0]){return E(_VZ);}else{var _Wk=jsAddChildBefore(_Wg,_Wh,E(_Wj[1])[1]),_Wl=A(_W9,[[0,_Wg],_]);return [0,[0,_2M,_Wa],_W7];}break;default:var _Wm=E(_We),_Wn=jsClearChildren(_Wm[1]),_Wo=A(_W9,[_Wm,_]);return [0,[0,_2M,_Wa],_W7];}}},_Wp=[0,_2M,_5m],_Wq=function(_Wr,_){return [0,_Wp,_Wr];},_Ws=unCStr("Pattern match failure in do expression at main.hs:334:7-25"),_Wt=new T(function(){return _P0(_Ws);}),_Wu=function(_Wv,_){var _Ww=0;if(!E(_Ww)){var _Wx=_UP();return [0,[0,_2M,[1,_Wx]],_Wv];}else{var _Wy=E(_sr)[1],_Wz=takeMVar(_Wy),_=putMVar(_Wy,_Wz);return [0,[0,_2M,[1,_Wz]],_Wv];}},_WA=function(_WB,_WC,_WD,_WE){return A(_WB,[new T(function(){return function(_){var _WF=jsSet(E(_WC)[1],toJSStr(E(_WD)),toJSStr(E(_WE)));return _0;};})]);},_WG=unCStr("text"),_WH=unCStr("value"),_WI=new T(function(){return _6E(_oU,_oZ);}),_WJ=new T(function(){return A(_WI,[_6D]);}),_WK=new T(function(){return A(_WI,[_6D]);}),_WL=unCStr("Prelude.read: ambiguous parse"),_WM=unCStr("Prelude.read: no parse"),_WN=function(_WO){return [1,function(_WP){return A(_ku,[_WP,function(_WQ){return E([3,_WO,_bK]);}]);}];},_WR=function(_WS){while(1){var _WT=(function(_WU){var _WV=E(_WU);if(!_WV[0]){return [0];}else{var _WW=_WV[2],_WX=E(_WV[1]);if(!E(_WX[2])[0]){return [1,_WX[1],new T(function(){return _WR(_WW);})];}else{_WS=_WW;return null;}}})(_WS);if(_WT!=null){return _WT;}}},_WY=function(_WZ,_X0){var _X1=_WR(_aA(A(E(_WZ)[3],[_mL,_WN]),_X0));return _X1[0]==0?err(_WM):E(_X1[2])[0]==0?E(_X1[1]):err(_WL);},_X2=function(_X3,_X4,_X5,_X6){var _X7=new T(function(){return _rd(_X4);}),_X8=new T(function(){return _rR(_8B,_9H,_X5,_X4,_X3);});return [0,function(_X9){return A(_X8,[[1,_X6],_WG,_X9]);},function(_Xa,_){var _Xb=E(_X6),_Xc=jsFind(toJSStr(_Xb)),_Xd=E(_Xc);return _Xd[0]==0?_3N(_Xb):A(_WA,[_n,_Xd[1],_WH,new T(function(){var _Xe=A(_X5,[_Xa]),_Xf=E(_WJ),_Xg=hs_eqWord64(_Xe[1],_Xf[1]);if(!E(_Xg)){return A(_X7,[_Xa]);}else{var _Xh=hs_eqWord64(_Xe[2],_Xf[2]);return E(_Xh)==0?A(_X7,[_Xa]):E(_Xa);}}),_]);},function(_){var _Xi=E(_X6),_Xj=jsFind(toJSStr(_Xi)),_Xk=E(_Xj);if(!_Xk[0]){return _3N(_Xi);}else{var _Xl=_E7(E(_Xk[1])[1],_WH,_);return new T(function(){var _Xm=A(_WI,[_Xl]),_Xn=E(_WK),_Xo=hs_eqWord64(_Xm[1],_Xn[1]);if(!E(_Xo)){return _WY(_X3,_Xl);}else{var _Xp=hs_eqWord64(_Xm[2],_Xn[2]);return E(_Xp)==0?_WY(_X3,_Xl):E(_Xl);}});}}];},_Xq=unCStr("todo"),_Xr=new T(function(){var _Xs=_X2(_HJ,_EZ,_EY,_Xq);return [0,_Xs[1],_Xs[2],_Xs[3]];}),_Xt=new T(function(){var _Xu=A(E(_Xr)[2],[_9]);return function(_Xv,_){var _Xw=A(_Xu,[_]);return [0,[0,_2M,[1,_Xw]],_Xv];};}),_Xx=[1,_9],_Xy=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_VW,_9],_Xz=function(_XA){return E(_Xy);},_XB=new T(function(){return _Cv(_S,_3m,_Q,_Xz);}),_XC=function(_XD,_){var _XE=A(_XB,[_XD,_]);return [0,[0,_vv,new T(function(){var _XF=E(E(_XE)[1]);return _XF[0]==0?E(_Xx):E(_XF);})],new T(function(){return E(E(_XE)[2]);})];},_XG=[0,_2M,_5m],_XH=[0,_2M,_5m],_XI=function(_XJ,_XK,_){return [0,_XH,_XK];},_XL=[0,_2M,_5m],_XM=function(_XN,_){return [0,_XL,_XN];},_XO=unCStr("list"),_XP=unCStr("check"),_XQ=new T(function(){return A(_GF,[_e8,_XP]);}),_XR=new T(function(){return _tl(_XQ,_vz);}),_XS=unCStr("nocheck"),_XT=[1,_XS,_9],_XU=[0,_XT],_XV=[1,_XU],_XW=function(_XX,_){var _XY=A(_XR,[_XX,_]),_XZ=E(_XY),_Y0=E(_XZ[1]);return [0,[0,function(_Y1,_){var _Y2=A(_Y0[1],[_Y1,_]);return _Y1;},new T(function(){var _Y3=E(_Y0[2]);return _Y3[0]==0?E(_XV):E(_Y3);})],_XZ[2]];},_Y4=unCStr("text-decoration:line-through;"),_Y5=unCStr("li"),_Y6=function(_Y7,_Y8){var _Y9=new T(function(){return A(_Y7,[_Y8]);});return function(_Ya,_){var _Yb=jsCreateElem(toJSStr(E(_Y5))),_Yc=jsAppendChild(_Yb,E(_Ya)[1]),_Yd=[0,_Yb],_Ye=A(_Y9,[_Yd,_]);return _Yd;};},_Yf=function(_Yg){var _Yh=E(_Yg);if(!_Yh[0]){return [0];}else{var _Yi=new T(function(){return _58(_4O,_Yh[1]);});return [1,function(_Yj,_){var _Yk=_4j(_XW,function(_Yl){return (function(_Ym){var _Yn=E(_Ym);return _Yn[0]==0?function(_Yo,_){return [0,[0,_Yi,_C1],_Yo];}:!_sK(_Yn[1],_XP)?function(_Yp,_){return [0,[0,_Yi,_C1],_Yp];}:E(_Yn[2])[0]==0?function(_Yq,_){return [0,[0,function(_Yr,_){var _Ys=A(_Yi,[_Yr,_]),_Yt=A(_1,[_n,_Ys,_Ds,_Y4,_]);return _Ys;},_C1],_Yq];}:function(_Yu,_){return [0,[0,_Yi,_C1],_Yu];};})(E(_Yl)[1]);},_Yj,_),_Yv=E(_Yk),_Yw=E(_Yv[1]);return [0,[0,new T(function(){return _Y6(_v6,_Yw[1]);}),_Yw[2]],_Yv[2]];},new T(function(){return _Yf(_Yh[2]);})];}},_Yx=function(_Yy,_Yz){while(1){var _YA=(function(_YB,_YC){var _YD=E(_YC);if(!_YD[0]){return E(_YB);}else{_Yy=function(_YE,_){var _YF=A(_YB,[_YE,_]),_YG=E(_YF),_YH=E(_YG[1]),_YI=A(_YD[1],[_YG[2],_]),_YJ=E(_YI),_YK=E(_YJ[1]);return [0,[0,function(_YL,_){var _YM=A(_YH[1],[_YL,_]),_YN=A(_YK[1],[_YL,_]);return _YL;},new T(function(){var _YO=E(_YH[2]);return _YO[0]==0?E(_YK[2]):E(_YO);})],_YJ[2]];};_Yz=_YD[2];return null;}})(_Yy,_Yz);if(_YA!=null){return _YA;}}},_YP=function(_YQ,_YR,_){return _4j(_Wu,function(_YS){var _YT=E(E(_YS)[2]);return _YT[0]==3?E(E(_YT[1])[1])==13?function(_C8,_){return _4j(_Xt,function(_YU){return function(_C8,_){return _4j(_XC,function(_YV){var _YW=new T(function(){return _Yx(_XM,_Yf([1,_YQ,_YV]));});return function(_ce,_s1){return _4j(function(_C8,_){return _4j(_BZ,function(_YX){return function(_YY,_){return [0,_XG,new T(function(){var _YZ=E(_YX);return [0,_YZ[1],_YZ[2],_YZ[3],_YZ[4],new T(function(){return _BM(I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_VW,_9,[1,_YQ,_YV],_YZ[5]);})];})];};},_C8,_);},function(_Z0,_C8,_){return (function(_C8,_){return _4j(function(_C8,_){return _W0(_XO,_VX,_YW,_C8,_);},_XI,_C8,_);})(_C8,_);},_ce,_s1);};},_C8,_);};},_C8,_);}:E(_Wq):E(_Wt);},_YR,_);},_Z1=new T(function(){return A(E(_Xr)[1],[_a]);}),_Z2=new T(function(){return _tl(_Z1,_9F);}),_Z3=unCStr("todos"),_Z4=new T(function(){return _Vk(_4O,_Z3);}),_Z5=new T(function(){return _Va(_v6,_2M);}),_Z6=function(_Z7,_){var _Z8=_4j(_Z2,_YP,_Z7,_),_Z9=E(_Z8),_Za=E(_Z9[1]),_Zb=new T(function(){return _vm(_v6,function(_Zc,_){var _Zd=A(_Z4,[_Zc,_]),_Ze=A(_Za[1],[_Zc,_]);return _Zc;});});return [0,[0,function(_Zf,_){var _Zg=A(_Zb,[_Zf,_]),_Zh=A(_Z5,[_Zf,_]),_Zi=A(_1,[_n,_Zh,_JM,_XO,_]);return _Zf;},new T(function(){var _Zj=E(_Za[2]);return _Zj[0]==0?E(_C1):E(_Zj);})],_Z9[2]];},_Zk=function(_Zl,_Zm,_){return [0,[0,_2M,[1,[1,_Zl]]],_Zm];},_Zn=unCStr("revEntry"),_Zo=new T(function(){var _Zp=_X2(_HJ,_EZ,_EY,_Zn);return [0,_Zp[1],_Zp[2],_Zp[3]];}),_Zq=new T(function(){return A(E(_Zo)[1],[_a]);}),_Zr=new T(function(){return _tl(_Zq,_9F);}),_Zs=function(_Zt,_Zu,_){return [0,[0,_2M,[1,[0,_Zt]]],_Zu];},_Zv=unCStr("entry"),_Zw=new T(function(){var _Zx=_X2(_HJ,_EZ,_EY,_Zv);return [0,_Zx[1],_Zx[2],_Zx[3]];}),_Zy=new T(function(){return A(E(_Zw)[1],[_a]);}),_Zz=new T(function(){return _tl(_Zy,_9F);}),_ZA=function(_ZB,_){var _ZC=_4j(_Zz,_Zs,_ZB,_),_ZD=E(_ZC),_ZE=E(_ZD[1]),_ZF=_4j(_Zr,_Zk,_ZD[2],_),_ZG=E(_ZF),_ZH=E(_ZG[1]);return [0,[0,new T(function(){return _vm(_v6,function(_ZI,_){var _ZJ=A(_ZE[1],[_ZI,_]),_ZK=_5i(_ZI,_),_ZL=A(_ZH[1],[_ZI,_]);return _ZI;});}),new T(function(){var _ZM=E(_ZE[2]);return _ZM[0]==0?E(_ZH[2]):E(_ZM);})],_ZG[2]];},_ZN=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_ZO=new T(function(){return _4V(_4O,_ZN);}),_ZP=function(_ZQ){var _ZR=A(E(_Zo)[2],[_ZQ]);return function(_ZS,_){var _ZT=A(_ZR,[_]);return [0,[0,_2M,[1,_ZT]],_ZS];};},_ZU=function(_ZV,_ZW){while(1){var _ZX=E(_ZV);if(!_ZX[0]){return E(_ZW);}else{_ZV=_ZX[2];var _ZY=[1,_ZX[1],_ZW];_ZW=_ZY;continue;}}},_ZZ=function(_100){var _101=new T(function(){return _ZU(_100,_9);});return function(_102,_){return [0,[0,_2M,[1,_101]],_102];};},_103=new T(function(){var _104=E(E(_Zw)[3]);return function(_105,_){var _106=A(_104,[_]);return [0,[0,_2M,[1,_106]],_105];};}),_107=function(_C8,_){return _4j(_103,_ZZ,_C8,_);},_108=function(_C8,_){return _4j(_107,_ZP,_C8,_);},_109=function(_10a){var _10b=A(E(_Zw)[2],[_10a]);return function(_10c,_){var _10d=A(_10b,[_]);return [0,[0,_2M,[1,_10d]],_10c];};},_10e=new T(function(){var _10f=E(E(_Zo)[3]);return function(_10g,_){var _10h=A(_10f,[_]);return [0,[0,_2M,[1,_10h]],_10g];};}),_10i=function(_10j){var _10k=new T(function(){return _ZU(_10j,_9);});return function(_10l,_){return [0,[0,_2M,[1,_10k]],_10l];};},_10m=function(_C8,_){return _4j(_10e,_10i,_C8,_);},_10n=function(_C8,_){return _4j(_10m,_109,_C8,_);},_10o=function(_10p){return E(_10p)[0]==0?E(_108):E(_10n);},_10q=function(_10r,_){var _10s=_4j(_ZA,_10o,_10r,_),_10t=E(_10s),_10u=E(_10t[1]);return [0,[0,function(_10v,_){var _10w=A(_ZO,[_10v,_]),_10x=A(_10u[1],[_10v,_]);return _10v;},_10u[2]],_10t[2]];},_10y=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_10z=new T(function(){return _4V(_4O,_10y);}),_10A=function(_10B,_10C,_10D){var _10E=E(_10D);if(!_10E[0]){var _10F=_10E[3],_10G=_10E[4],_10H=_10E[5],_10I=E(_10E[2]),_10J=_10I[1];return _10B>=_10J?_10B!=_10J?_B7(_10I,_10F,_10G,_10A(_10B,_10C,_10H)):[0,_10E[1],E([0,_10B]),_10C,E(_10G),E(_10H)]:_As(_10I,_10F,_10A(_10B,_10C,_10G),_10H);}else{return [0,1,E([0,_10B]),_10C,E(_8),E(_8)];}},_10K=function(_10L,_10M,_10N){var _10O=E(_10L),_10P=_10O[1],_10Q=E(_10N);if(!_10Q[0]){var _10R=_10Q[3],_10S=_10Q[4],_10T=_10Q[5],_10U=E(_10Q[2]),_10V=_10U[1];return _10P>=_10V?_10P!=_10V?_B7(_10U,_10R,_10S,_10A(_10P,_10M,_10T)):[0,_10Q[1],E(_10O),_10M,E(_10S),E(_10T)]:_As(_10U,_10R,_10A(_10P,_10M,_10S),_10T);}else{return [0,1,E(_10O),_10M,E(_8),E(_8)];}},_10W=function(_10X,_10Y,_10Z){var _110=E(_10X),_111=_110[1],_112=_110[2],_113=_110[3],_114=_110[4],_115=E(_10Z);if(!_115[0]){var _116=_115[2],_117=_115[3],_118=_115[4],_119=_115[5];switch(_Af(_110,_116)){case 0:return _As(_116,_117,_BM(_111,_112,_113,_114,_10Y,_118),_119);case 1:return [0,_115[1],E(_110),_10Y,E(_118),E(_119)];default:return _B7(_116,_117,_118,_BM(_111,_112,_113,_114,_10Y,_119));}}else{return [0,1,E(_110),_10Y,E(_8),E(_8)];}},_11a=function(_11b,_11c){while(1){var _11d=E(_11c);if(!_11d[0]){var _11e=E(_11d[2])[1];if(_11b>=_11e){if(_11b!=_11e){_11c=_11d[5];continue;}else{return [1,_11d[3]];}}else{_11c=_11d[4];continue;}}else{return [0];}}},_11f=[1,_8],_11g=unCStr("containers-0.5.5.1"),_11h=unCStr("Data.Map.Base"),_11i=unCStr("Map"),_11j=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_11g,_11h,_11i],_11k=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_11j,_9],_11l=function(_11m){return E(_11k);},_11n=function(_11o){var _11p=E(_11o);if(!_11p[0]){return [0];}else{var _11q=E(_11p[1]);return [1,[0,_11q[1],_11q[2]],new T(function(){return _11n(_11p[2]);})];}},_11r=function(_11s,_11t){return function(_11u){return E(new T(function(){var _11v=A(_11s,[_6D]),_11w=E(_11v[3]),_11x=_11w[1],_11y=_11w[2],_11z=_1v(_11v[4],[1,new T(function(){return A(_11t,[_6D]);}),_9]);if(!_11z[0]){return [0,_11x,_11y,_11w,_9];}else{var _11A=_68(new T(function(){return _5W(_6k(_6v,[1,[0,_11x,_11y],new T(function(){return _11n(_11z);})]));}));return [0,_11A[1],_11A[2],_11w,_11z];}}));};},_11B=new T(function(){return _11r(_11l,_og);}),_11C=new T(function(){return _6E(_11B,_og);}),_11D=new T(function(){return _Cv(_S,_3m,_Q,_11C);}),_11E=function(_11F,_){var _11G=A(_11D,[_11F,_]);return [0,[0,_vv,new T(function(){var _11H=E(E(_11G)[1]);return _11H[0]==0?E(_11f):E(_11H);})],new T(function(){return E(E(_11G)[2]);})];},_11I=[0,_2M,_a],_11J=function(_11K,_){return [0,_11I,_11K];},_11L=new T(function(){return _6E(_11B,_og);}),_11M=new T(function(){return _Cv(_S,_3m,_Q,_11L);}),_11N=function(_11O,_){var _11P=A(_11M,[_11O,_]);return [0,[0,_2M,new T(function(){return E(E(_11P)[1]);})],new T(function(){return E(E(_11P)[2]);})];},_11Q=[0,_2M,_5m],_11R=[1,_a],_11S=function(_11T,_11U){var _11V=new T(function(){return [0,E(_11T)[1]+1|0];});return function(_ce,_s1){return _4j(function(_C8,_){return _4j(function(_11W,_){var _11X=_4j(_11N,function(_11Y){var _11Z=_11a(E(_11T)[1],_11Y);return _11Z[0]==0?E(_11J):function(_120,_){return [0,[0,_2M,_11Z],_120];};},_11W,_),_121=E(_11X),_122=E(_121[1]);return [0,[0,function(_123,_){var _124=A(_122[1],[_123,_]);return _123;},new T(function(){var _125=E(_122[2]);return _125[0]==0?E(_11R):[1,_125];})],_121[2]];},function(_126){var _127=new T(function(){return _tl(new T(function(){return A(_s3,[_a,_9G,_126]);}),_9F);});return function(_ce,_s1){return _4j(function(_128,_){var _129=A(_127,[_128,_]),_12a=E(_129),_12b=_12a[2],_12c=E(_12a[1]),_12d=_12c[1],_12e=_12c[2],_12f=E(_126);return _12f[0]==0?[0,[0,function(_12g,_){var _12h=A(_12d,[_12g,_]);return _12g;},_12e],_12b]:[0,[0,function(_12i,_){var _12j=A(_12d,[_12i,_]);return _12i;},new T(function(){var _12k=E(_12e);return _12k[0]==0?E(_12f):E(_12k);})],_12b];},function(_12l,_12m,_){return _4j(function(_C8,_){return _4j(_11E,function(_12n){var _12o=new T(function(){return _10K(_11T,_12l,_12n);}),_12p=new T(function(){return A(_11C,[_12o]);});return function(_ce,_s1){return _4j(_BZ,function(_12q){return function(_12r,_){return [0,_11Q,new T(function(){var _12s=E(_12q);return [0,_12s[1],_12s[2],_12s[3],_12s[4],new T(function(){return _10W(_12p,_12o,_12s[5]);})];})];};},_ce,_s1);};},_C8,_);},function(_12t,_C8,_){return (function(_12u,_){return [0,[0,_2M,[1,_12l]],_12u];})(_C8,_);},_12m,_);},_ce,_s1);};},_C8,_);},function(_12v){var _12w=new T(function(){return _11S(_11V,new T(function(){return _8Q(_11U,_12v);}));}),_12x=new T(function(){return _58(_4O,new T(function(){return _3u(0,E(_11U)[1]+E(_12v)[1]|0,_9);}));});return function(_ce,_s1){return _4j(function(_12y,_){return [0,[0,function(_12z,_){var _12A=A(_12x,[_12z,_]),_12B=_5i(_12z,_);return _12z;},_5m],_12y];},function(_12C){return E(_12w);},_ce,_s1);};},_ce,_s1);};},_12D=new T(function(){return _11S(_5x,_5x);}),_12E=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_12F=new T(function(){return _4V(_4O,_12E);}),_12G=new T(function(){return A(_s3,[_a,_9G,_a]);}),_12H=new T(function(){return _tl(_12G,_9F);}),_12I=function(_12J){var _12K=new T(function(){return _58(_4O,new T(function(){return _54(_12J);}));});return function(_ce,_s1){return _4j(_12H,function(_12L){var _12M=E(E(_12L)[1]);if(!_12M){return function(_12N,_){return [0,[0,function(_12O,_){var _12P=_5i(_12O,_),_12Q=_4O(_5n,_12O,_),_12R=A(_12K,[_12O,_]);return _12O;},_a],_12N];};}else{var _12S=new T(function(){return _12I(new T(function(){return [0,E(_12J)[1]+_12M|0];}));}),_12T=new T(function(){return _58(_4O,new T(function(){return _3u(0,E(_12J)[1]+_12M|0,_9);}));});return function(_ce,_s1){return _4j(function(_12U,_){return [0,[0,function(_12V,_){var _12W=A(_12T,[_12V,_]),_12X=_5i(_12V,_);return _12V;},_5m],_12U];},function(_12Y){return E(_12S);},_ce,_s1);};}},_ce,_s1);};},_12Z=new T(function(){return _12I(_5x);}),_130=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_131=new T(function(){return _4V(_4O,_130);}),_132=function(_133){return function(_134,_){return [0,[0,new T(function(){var _135=new T(function(){return _58(_4O,new T(function(){return _54(_133);}));});return _4V(_v6,function(_136,_){var _137=_4O(_5n,_136,_),_138=A(_135,[_136,_]);return _136;});}),_5m],_134];};},_139=new T(function(){return A(_s3,[_a,_9G,_a]);}),_13a=new T(function(){return _tl(_139,_9F);}),_13b=unCStr("second number "),_13c=unCStr("first number"),_13d=new T(function(){return A(_s3,[_a,_9G,_a]);}),_13e=new T(function(){return _tl(_13d,_9F);}),_13f=function(_13g,_){var _13h=A(_13a,[_13g,_]),_13i=E(_13h),_13j=E(_13i[1]),_13k=A(_13e,[_13i[2],_]),_13l=E(_13k),_13m=E(_13l[1]);return [0,[0,function(_13n,_){var _13o=_4O(_13c,_13n,_),_13p=_5i(_13n,_),_13q=A(_13j[1],[_13n,_]),_13r=_5i(_13n,_),_13s=_4O(_13b,_13n,_),_13t=_5i(_13n,_),_13u=A(_13m[1],[_13n,_]),_13v=_5i(_13n,_);return _13n;},new T(function(){var _13w=E(_13j[2]);if(!_13w[0]){return [0];}else{var _13x=E(_13m[2]);return _13x[0]==0?[0]:[1,new T(function(){return _8Q(_13w[1],_13x[1]);})];}})],_13l[2]];},_13y=function(_13z,_){var _13A=_4j(_13f,_132,_13z,_),_13B=E(_13A),_13C=E(_13B[1]),_13D=new T(function(){return _4V(_v6,_13C[1]);});return [0,[0,function(_13E,_){var _13F=A(_131,[_13E,_]),_13G=A(_13D,[_13E,_]);return _13E;},_13C[2]],_13B[2]];},_13H=unCStr("td"),_13I=function(_13J,_13K){var _13L=new T(function(){return A(_13J,[_13K]);});return function(_13M,_){var _13N=jsCreateElem(toJSStr(E(_13H))),_13O=jsAppendChild(_13N,E(_13M)[1]),_13P=[0,_13N],_13Q=A(_13L,[_13P,_]);return _13P;};},_13R=unCStr("tr"),_13S=function(_13T,_13U){var _13V=new T(function(){return A(_13T,[_13U]);});return function(_13W,_){var _13X=jsCreateElem(toJSStr(E(_13R))),_13Y=jsAppendChild(_13X,E(_13W)[1]),_13Z=[0,_13X],_140=A(_13V,[_13Z,_]);return _13Z;};},_141=function(_142,_){var _143=_13y(_142,_),_144=E(_143),_145=E(_144[1]),_146=A(_vk,[_144[2],_]),_147=E(_146),_148=E(_147[1]),_149=A(_12Z,[_147[2],_]),_14a=E(_149),_14b=E(_14a[1]),_14c=A(_D7,[_14a[2],_]),_14d=E(_14c),_14e=E(_14d[1]),_14f=_J4(_14d[2],_),_14g=E(_14f),_14h=E(_14g[1]),_14i=A(_12D,[_14g[2],_]),_14j=E(_14i),_14k=E(_14j[1]),_14l=A(_DN,[_14j[2],_]),_14m=E(_14l),_14n=E(_14m[1]),_14o=_Si(_14m[2],_),_14p=E(_14o),_14q=E(_14p[1]),_14r=_10q(_14p[2],_),_14s=E(_14r),_14t=E(_14s[1]),_14u=_Z6(_14s[2],_),_14v=E(_14u),_14w=E(_14v[1]),_14x=_NF(_14v[2],_),_14y=E(_14x),_14z=E(_14y[1]),_14A=_Tw(_14y[2],_),_14B=E(_14A),_14C=E(_14B[1]),_14D=_4j(_VK,_UZ,_14B[2],_),_14E=E(_14D),_14F=E(_14E[1]);return [0,[0,function(_14G,_){var _14H=A(new T(function(){var _14I=new T(function(){return _13I(_v6,function(_14J,_){var _14K=A(_12F,[_14J,_]),_14L=A(_14b[1],[_14J,_]);return _14J;});}),_14M=new T(function(){return _13I(_v6,_148[1]);}),_14N=new T(function(){return _13I(_v6,_145[1]);});return _13S(_v6,function(_14O,_){var _14P=A(_14N,[_14O,_]),_14Q=A(_1,[_n,_14P,_Ds,_4L,_]),_14R=A(_14M,[_14O,_]),_14S=A(_1,[_n,_14R,_Ds,_4L,_]),_14T=A(_14I,[_14O,_]),_14U=A(_1,[_n,_14T,_Ds,_4L,_]);return _14O;});}),[_14G,_]),_14V=A(_1,[_n,_14H,_Ds,_4M,_]),_14W=A(new T(function(){var _14X=new T(function(){return _13I(_v6,_14n[1]);}),_14Y=new T(function(){return _13I(_v6,function(_14Z,_){var _150=A(_10z,[_14Z,_]),_151=A(_14k[1],[_14Z,_]);return _14Z;});}),_152=new T(function(){return _13I(_v6,function(_153,_){var _154=A(_14e[1],[_153,_]),_155=A(_14h[1],[_153,_]);return _153;});});return _13S(_v6,function(_156,_){var _157=A(_152,[_156,_]),_158=A(_1,[_n,_157,_Ds,_4L,_]),_159=A(_14Y,[_156,_]),_15a=A(_1,[_n,_159,_Ds,_4L,_]),_15b=A(_14X,[_156,_]),_15c=A(_1,[_n,_15b,_Ds,_4L,_]);return _156;});}),[_14G,_]),_15d=A(_1,[_n,_14W,_Ds,_4M,_]),_15e=A(new T(function(){var _15f=new T(function(){return _13I(_v6,function(_15g,_){var _15h=A(_VU,[_15g,_]),_15i=A(_14w[1],[_15g,_]);return _15g;});}),_15j=new T(function(){return _13I(_v6,_14t[1]);}),_15k=new T(function(){return _13I(_v6,new T(function(){return _vm(_v6,_14q[1]);}));});return _13S(_v6,function(_15l,_){var _15m=A(_15k,[_15l,_]),_15n=A(_1,[_n,_15m,_Ds,_4L,_]),_15o=A(_15j,[_15l,_]),_15p=A(_1,[_n,_15o,_Ds,_4L,_]),_15q=A(_15f,[_15l,_]),_15r=A(_1,[_n,_15q,_Ds,_4L,_]);return _15l;});}),[_14G,_]),_15s=A(_1,[_n,_15e,_Ds,_4M,_]),_15t=A(new T(function(){var _15u=new T(function(){return _13I(_v6,_14F[1]);}),_15v=new T(function(){return _13I(_v6,_14C[1]);}),_15w=new T(function(){return _13I(_v6,_14z[1]);});return _13S(_v6,function(_15x,_){var _15y=A(_15w,[_15x,_]),_15z=A(_1,[_n,_15y,_Ds,_4L,_]),_15A=A(_15v,[_15x,_]),_15B=A(_1,[_n,_15A,_Ds,_4L,_]),_15C=A(_15u,[_15x,_]),_15D=A(_1,[_n,_15C,_Ds,_4L,_]);return _15x;});}),[_14G,_]),_15E=A(_1,[_n,_15t,_Ds,_4M,_]);return _14G;},new T(function(){var _15F=E(_145[2]);if(!_15F[0]){var _15G=E(_148[2]);if(!_15G[0]){var _15H=E(_14b[2]);if(!_15H[0]){var _15I=E(_14e[2]);if(!_15I[0]){var _15J=E(_14h[2]);if(!_15J[0]){var _15K=E(_14k[2]);if(!_15K[0]){var _15L=E(_14n[2]);if(!_15L[0]){var _15M=E(_14q[2]);if(!_15M[0]){var _15N=E(_14t[2]);if(!_15N[0]){var _15O=E(_14w[2]);if(!_15O[0]){var _15P=E(_14z[2]);if(!_15P[0]){var _15Q=E(_14C[2]);return _15Q[0]==0?E(_14F[2]):E(_15Q);}else{return E(_15P);}}else{return E(_15O);}}else{return E(_15N);}}else{return E(_15M);}}else{return E(_15L);}}else{return E(_15K);}}else{return E(_15J);}}else{return E(_15I);}}else{return E(_15H);}}else{return E(_15G);}}else{return E(_15F);}})],_14E[2]];},_15R=unCStr("bottom of the page"),_15S=new T(function(){return _58(_4O,_15R);}),_15T=unCStr("border-collapse:collapse"),_15U=unCStr("hplayground examples"),_15V=new T(function(){return _Vk(_4O,_15U);}),_15W=unCStr("idelem"),_15X=unCStr("h3"),_15Y=function(_15Z,_160){var _161=new T(function(){return A(_15Z,[_160]);});return function(_162,_){var _163=jsCreateElem(toJSStr(E(_15X))),_164=jsAppendChild(_163,E(_162)[1]),_165=[0,_163],_166=A(_161,[_165,_]);return _165;};},_167=unCStr("   "),_168=unCStr("https://github.com/agocorona/hplayground"),_169=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_16a=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_16b=unCStr("Article"),_16c=new T(function(){return _RY(_4O,_16b);}),_16d=unCStr("Examples code"),_16e=new T(function(){return _RY(_4O,_16d);}),_16f=unCStr("Git repository"),_16g=new T(function(){return _RY(_4O,_16f);}),_16h=function(_16i,_){var _16j=A(_16g,[_16i,_]),_16k=A(_1,[_n,_16j,_S9,_168,_]),_16l=_4O(_167,_16i,_),_16m=A(_16e,[_16i,_]),_16n=A(_1,[_n,_16m,_S9,_16a,_]),_16o=_4O(_167,_16i,_),_16p=A(_16c,[_16i,_]),_16q=A(_1,[_n,_16p,_S9,_169,_]);return _16i;},_16r=new T(function(){return _vm(_v6,_16h);}),_16s=new T(function(){return _15Y(_v6,_16r);}),_16t=unCStr("table"),_16u=function(_16v,_16w){var _16x=new T(function(){return A(_16v,[_16w]);});return function(_16y,_){var _16z=jsCreateElem(toJSStr(E(_16t))),_16A=jsAppendChild(_16z,E(_16y)[1]),_16B=[0,_16z],_16C=A(_16x,[_16B,_]);return _16B;};},_16D=function(_){var _16E=E(_15W),_16F=jsFind(toJSStr(_16E)),_16G=E(_16F);if(!_16G[0]){return _3N(_16E);}else{var _16H=_16G[1],_16I=E(_m)[1],_16J=takeMVar(_16I),_16K=_141(_16J,_),_16L=E(_16K),_16M=E(_16L[1]),_=putMVar(_16I,_16L[2]),_16N=A(_15V,[_16H,_]),_16O=A(_1,[_n,_16N,_Ds,_p,_]),_16P=A(_16s,[_16H,_]),_16Q=A(_16u,[_v6,_16M[1],_16H,_]),_16R=A(_1,[_n,_16Q,_Ds,_15T,_]),_16S=A(_15S,[_16H,_]);return _16M[2];}},_16T=function(_){return _16D(_);};
var hasteMain = function() {A(_16T, [0]);};window.onload = hasteMain;