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

var _0=0,_1=[0,0],_2=2,_3=[0],_4=function(_5,_6,_){var _7=A(_5,[[0,_3,_1,_2,function(_){return _4(_5,_6,_);}],_]),_8=E(_6),_9=jsClearChildren(_8[1]),_a=A(E(E(_7)[1])[1],[_8,_]);return _0;},_b=[13,coercionToken],_c=function(_d,_e,_){var _f=jsCreateTextNode(toJSStr(E(_d))),_g=jsAppendChild(_f,E(_e)[1]);return [0,_f];},_h=function(_i,_j){var _k=E(_i);return _k[0]==0?E(_j):[1,_k[1],new T(function(){return _h(_k[2],_j);})];},_l=function(_m,_n){var _o=jsShowI(_m);return _h(fromJSStr(_o),_n);},_p=[0,41],_q=[0,40],_r=function(_s,_t,_u){return _t>=0?_l(_t,_u):_s<=6?_l(_t,_u):[1,_q,new T(function(){var _v=jsShowI(_t);return _h(fromJSStr(_v),[1,_p,_u]);})];},_w=[0],_x=function(_y,_z,_A,_B){return A(_y,[new T(function(){return function(_){var _C=jsSet(E(_z)[1],toJSStr(E(_A)),toJSStr(E(_B)));return _0;};})]);},_D=unCStr("value"),_E=new T(function(){return [0,"keydown"];}),_F=new T(function(){return [0,"mousemove"];}),_G=new T(function(){return [0,"blur"];}),_H=new T(function(){return [0,"focus"];}),_I=new T(function(){return [0,"change"];}),_J=new T(function(){return [0,"unload"];}),_K=new T(function(){return [0,"load"];}),_L=new T(function(){return [0,"keyup"];}),_M=new T(function(){return [0,"keypress"];}),_N=new T(function(){return [0,"mouseup"];}),_O=new T(function(){return [0,"mousedown"];}),_P=new T(function(){return [0,"dblclick"];}),_Q=new T(function(){return [0,"click"];}),_R=new T(function(){return [0,"mouseout"];}),_S=new T(function(){return [0,"mouseover"];}),_T=function(_U){switch(E(_U)[0]){case 0:return E(_K);case 1:return E(_J);case 2:return E(_I);case 3:return E(_H);case 4:return E(_G);case 5:return E(_F);case 6:return E(_S);case 7:return E(_R);case 8:return E(_Q);case 9:return E(_P);case 10:return E(_O);case 11:return E(_N);case 12:return E(_M);case 13:return E(_L);default:return E(_E);}},_V=new T(function(){return [0,"(function(e) {e.focus();})"];}),_W=function(_X){var _Y=A(_X,[_]);return E(_Y);},_Z=function(_10){return _W(function(_){var _=0;return eval(E(_10)[1]);});},_11=new T(function(){return _Z(_V);}),_12=function(_13,_){var _14=A(_11,[E(E(_13)[1]),_]);return _0;},_15=function(_16,_){return _12(_16,_);},_17=new T(function(){return [0,"value"];}),_18=function(_19){return E(_19);},_1a=function(_1b,_1c,_1d,_1e,_){var _1f=A(_1b,[_1e,_]),_1g=E(_1f),_1h=_1g[1],_1i=E(_1d),_1j=jsSetCB(_1h,_T(_1c)[1],_1d),_1k=jsGet(_1h,E(_17)[1]),_1l=A(_x,[_18,_1g,_D,new T(function(){return fromJSStr(_1k);}),_]),_1m=_15(_1g,_);return _1g;},_1n=unCStr("br"),_1o=[0,98],_1p=[1,_1o,_3],_1q=unCStr("text"),_1r=function(_1s,_1t,_){var _1u=jsCreateElem(toJSStr(E(_1s))),_1v=jsAppendChild(_1u,E(_1t)[1]);return [0,_1u];},_1w=function(_1x,_1y,_1z,_){var _1A=_1r(_1x,_1z,_),_1B=A(_1y,[_1A,_]);return _1A;},_1C=unCStr("()"),_1D=unCStr("GHC.Tuple"),_1E=unCStr("ghc-prim"),_1F=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_1E,_1D,_1C],_1G=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_1F,_3],_1H=function(_1I){return E(_1G);},_1J=unCStr("main"),_1K=unCStr("Builder"),_1L=unCStr("JSBuilderM"),_1M=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_1J,_1K,_1L],_1N=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_1M,_3],_1O=function(_1P){return E(_1N);},_1Q=function(_1R){var _1S=E(_1R);return _1S[0]==0?[0]:_h(_1S[1],new T(function(){return _1Q(_1S[2]);}));},_1T=function(_1U,_1V){var _1W=E(_1U);if(!_1W){return [0,_3,_1V];}else{var _1X=E(_1V);if(!_1X[0]){return [0,_3,_3];}else{var _1Y=new T(function(){var _1Z=_1T(_1W-1|0,_1X[2]);return [0,_1Z[1],_1Z[2]];});return [0,[1,_1X[1],new T(function(){return E(E(_1Y)[1]);})],new T(function(){return E(E(_1Y)[2]);})];}}},_20=[0,120],_21=[0,48],_22=function(_23){var _24=new T(function(){var _25=_1T(8,new T(function(){var _26=md5(toJSStr(E(_23)));return fromJSStr(_26);}));return [0,_25[1],_25[2]];}),_27=parseInt([0,toJSStr([1,_21,[1,_20,new T(function(){return E(E(_24)[1]);})]])]),_28=new T(function(){var _29=_1T(8,new T(function(){return E(E(_24)[2]);}));return [0,_29[1],_29[2]];}),_2a=parseInt([0,toJSStr([1,_21,[1,_20,new T(function(){return E(E(_28)[1]);})]])]),_2b=hs_mkWord64(_27,_2a),_2c=parseInt([0,toJSStr([1,_21,[1,_20,new T(function(){return E(_1T(8,new T(function(){return E(E(_28)[2]);}))[1]);})]])]),_2d=hs_mkWord64(_2c,_2c);return [0,_2b,_2d];},_2e=function(_2f,_2g){var _2h=E(_2g);return _2h[0]==0?[0]:[1,new T(function(){return A(_2f,[_2h[1]]);}),new T(function(){return _2e(_2f,_2h[2]);})];},_2i=function(_2j,_2k){var _2l=jsShowI(_2j),_2m=md5(_2l);return _h(fromJSStr(_2m),new T(function(){var _2n=jsShowI(_2k),_2o=md5(_2n);return fromJSStr(_2o);}));},_2p=function(_2q){var _2r=E(_2q);return _2i(_2r[1],_2r[2]);},_2s=function(_2t){var _2u=E(_2t);if(!_2u[0]){return [0];}else{var _2v=E(_2u[1]);return [1,[0,_2v[1],_2v[2]],new T(function(){return _2s(_2u[2]);})];}},_2w=unCStr("Prelude.undefined"),_2x=new T(function(){return err(_2w);}),_2y=function(_2z,_2A){return function(_2B){return E(new T(function(){var _2C=A(_2z,[_2x]),_2D=E(_2C[3]),_2E=_2D[1],_2F=_2D[2],_2G=_h(_2C[4],[1,new T(function(){return A(_2A,[_2x]);}),_3]);if(!_2G[0]){return [0,_2E,_2F,_2D,_3];}else{var _2H=_22(new T(function(){return _1Q(_2e(_2p,[1,[0,_2E,_2F],new T(function(){return _2s(_2G);})]));}));return [0,_2H[1],_2H[2],_2D,_2G];}}));};},_2I=new T(function(){return _2y(_1O,_1H);}),_2J=function(_2K,_2L,_2M,_2N){return A(_2K,[new T(function(){return function(_){var _2O=jsSetAttr(E(_2L)[1],toJSStr(E(_2M)),toJSStr(E(_2N)));return _0;};})]);},_2P=function(_2Q,_2R,_2S,_){var _2T=E(_2R),_2U=A(_2Q,[_2S,_]),_2V=A(_2J,[_18,_2U,_2T[1],_2T[2],_]);return _2U;},_2W=function(_2X,_2Y){while(1){var _2Z=(function(_30,_31){var _32=E(_31);if(!_32[0]){return E(_30);}else{_2X=function(_33,_){return _2P(_30,_32[1],_33,_);};_2Y=_32[2];return null;}})(_2X,_2Y);if(_2Z!=null){return _2Z;}}},_34=unCStr("value"),_35=unCStr("id"),_36=unCStr("onclick"),_37=unCStr("checked"),_38=[0,_37,_3],_39=[1,_38,_3],_3a=unCStr("type"),_3b=unCStr("input"),_3c=function(_3d,_){return _1r(_3b,_3d,_);},_3e=function(_3f,_3g,_3h,_3i,_3j){var _3k=new T(function(){var _3l=new T(function(){return _2W(_3c,[1,[0,_3a,_3g],[1,[0,_35,_3f],[1,[0,_34,_3h],_3]]]);});return !E(_3i)?E(_3l):_2W(_3l,_39);}),_3m=E(_3j);return _3m[0]==0?E(_3k):_2W(_3k,[1,[0,_36,_3m[1]],_3]);},_3n=unCStr("href"),_3o=[0,97],_3p=[1,_3o,_3],_3q=function(_3r,_){return _1r(_3p,_3r,_);},_3s=function(_3t,_3u){var _3v=new T(function(){return _2W(_3q,[1,[0,_3n,_3t],_3]);});return function(_3w,_){var _3x=A(_3v,[_3w,_]),_3y=A(_3u,[_3x,_]);return _3x;};},_3z=function(_3A){return _3s(_3A,function(_33,_){return _c(_3A,_33,_);});},_3B=unCStr("option"),_3C=function(_3D,_){return _1r(_3B,_3D,_);},_3E=unCStr("selected"),_3F=[0,_3E,_3],_3G=[1,_3F,_3],_3H=function(_3I,_3J,_3K){var _3L=new T(function(){return _2W(_3C,[1,[0,_34,_3I],_3]);}),_3M=function(_3N,_){var _3O=A(_3L,[_3N,_]),_3P=A(_3J,[_3O,_]);return _3O;};return !E(_3K)?E(_3M):_2W(_3M,_3G);},_3Q=function(_3R,_3S){return _3H(_3R,function(_33,_){return _c(_3R,_33,_);},_3S);},_3T=unCStr("method"),_3U=unCStr("action"),_3V=unCStr("UTF-8"),_3W=unCStr("acceptCharset"),_3X=[0,_3W,_3V],_3Y=unCStr("form"),_3Z=function(_40,_){return _1r(_3Y,_40,_);},_41=function(_42,_43,_44){var _45=new T(function(){return _2W(_3Z,[1,_3X,[1,[0,_3U,_42],[1,[0,_3T,_43],_3]]]);});return function(_46,_){var _47=A(_45,[_46,_]),_48=A(_44,[_47,_]);return _47;};},_49=unCStr("select"),_4a=function(_4b,_){return _1r(_49,_4b,_);},_4c=function(_4d,_4e){var _4f=new T(function(){return _2W(_4a,[1,[0,_35,_4d],_3]);});return function(_4g,_){var _4h=A(_4f,[_4g,_]),_4i=A(_4e,[_4h,_]);return _4h;};},_4j=unCStr("textarea"),_4k=function(_4l,_){return _1r(_4j,_4l,_);},_4m=function(_4n,_4o){var _4p=new T(function(){return _2W(_4k,[1,[0,_35,_4n],_3]);});return function(_4q,_){var _4r=A(_4p,[_4q,_]),_4s=_c(_4o,_4r,_);return _4r;};},_4t=unCStr("color:red"),_4u=unCStr("style"),_4v=[0,_4u,_4t],_4w=[1,_4v,_3],_4x=[0,98],_4y=[1,_4x,_3],_4z=function(_4A){return _2W(function(_4B,_){var _4C=_1r(_4y,_4B,_),_4D=A(_4A,[_4C,_]);return _4C;},_4w);},_4E=unCStr("toByteString not defined"),_4F=new T(function(){return err(_4E);}),_4G=function(_4H,_4I,_){var _4J=E(_4H);if(!_4J[0]){return _4I;}else{var _4K=A(_4J[1],[_4I,_]),_4L=_4G(_4J[2],_4I,_);return _4I;}},_4M=function(_4N,_4O,_4P,_){var _4Q=A(_4N,[_4P,_]),_4R=A(_4O,[_4P,_]);return _4P;},_4S=function(_4T,_){return _4T;},_4U=[0,_4S,_4M,_4G],_4V=[0,_4U,_2I,_4F,_c,_c,_1w,_4z,_3s,_3z,_3e,_4m,_4c,_3H,_3Q,_41,_2W],_4W=function(_4X,_4Y,_){var _4Z=A(_4X,[_]);return A(_4Y,[_]);},_50=function(_51,_52,_){return _4W(_51,_52,_);},_53=function(_54,_55,_){var _56=A(_54,[_]);return A(_55,[_56,_]);},_57=unCStr("base"),_58=unCStr("GHC.IO.Exception"),_59=unCStr("IOException"),_5a=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_57,_58,_59],_5b=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_5a,_3],_5c=function(_5d){return E(_5b);},_5e=function(_5f){return E(E(_5f)[1]);},_5g=unCStr("Maybe.fromJust: Nothing"),_5h=new T(function(){return err(_5g);}),_5i=function(_5j,_5k,_5l){var _5m=new T(function(){var _5n=A(_5j,[_5l]),_5o=A(_5k,[new T(function(){var _5p=E(_5m);return _5p[0]==0?E(_5h):E(_5p[1]);})]),_5q=hs_eqWord64(_5n[1],_5o[1]);if(!E(_5q)){return [0];}else{var _5r=hs_eqWord64(_5n[2],_5o[2]);return E(_5r)==0?[0]:[1,_5l];}});return E(_5m);},_5s=function(_5t){var _5u=E(_5t);return _5i(_5e(_5u[1]),_5c,_5u[2]);},_5v=unCStr(": "),_5w=[0,41],_5x=unCStr(" ("),_5y=unCStr("already exists"),_5z=unCStr("does not exist"),_5A=unCStr("protocol error"),_5B=unCStr("failed"),_5C=unCStr("invalid argument"),_5D=unCStr("inappropriate type"),_5E=unCStr("hardware fault"),_5F=unCStr("unsupported operation"),_5G=unCStr("timeout"),_5H=unCStr("resource vanished"),_5I=unCStr("interrupted"),_5J=unCStr("resource busy"),_5K=unCStr("resource exhausted"),_5L=unCStr("end of file"),_5M=unCStr("illegal operation"),_5N=unCStr("permission denied"),_5O=unCStr("user error"),_5P=unCStr("unsatisified constraints"),_5Q=unCStr("system error"),_5R=function(_5S,_5T){switch(E(_5S)){case 0:return _h(_5y,_5T);case 1:return _h(_5z,_5T);case 2:return _h(_5J,_5T);case 3:return _h(_5K,_5T);case 4:return _h(_5L,_5T);case 5:return _h(_5M,_5T);case 6:return _h(_5N,_5T);case 7:return _h(_5O,_5T);case 8:return _h(_5P,_5T);case 9:return _h(_5Q,_5T);case 10:return _h(_5A,_5T);case 11:return _h(_5B,_5T);case 12:return _h(_5C,_5T);case 13:return _h(_5D,_5T);case 14:return _h(_5E,_5T);case 15:return _h(_5F,_5T);case 16:return _h(_5G,_5T);case 17:return _h(_5H,_5T);default:return _h(_5I,_5T);}},_5U=[0,125],_5V=unCStr("{handle: "),_5W=function(_5X,_5Y,_5Z,_60,_61,_62){var _63=new T(function(){var _64=new T(function(){return _5R(_5Y,new T(function(){var _65=E(_60);return _65[0]==0?E(_62):_h(_5x,new T(function(){return _h(_65,[1,_5w,_62]);}));}));}),_66=E(_5Z);return _66[0]==0?E(_64):_h(_66,new T(function(){return _h(_5v,_64);}));}),_67=E(_61);if(!_67[0]){var _68=E(_5X);if(!_68[0]){return E(_63);}else{var _69=E(_68[1]);return _69[0]==0?_h(_5V,new T(function(){return _h(_69[1],[1,_5U,new T(function(){return _h(_5v,_63);})]);})):_h(_5V,new T(function(){return _h(_69[1],[1,_5U,new T(function(){return _h(_5v,_63);})]);}));}}else{return _h(_67[1],new T(function(){return _h(_5v,_63);}));}},_6a=function(_6b){var _6c=E(_6b);return _5W(_6c[1],_6c[2],_6c[3],_6c[4],_6c[6],_3);},_6d=function(_6e,_6f){var _6g=E(_6e);return _5W(_6g[1],_6g[2],_6g[3],_6g[4],_6g[6],_6f);},_6h=[0,44],_6i=[0,93],_6j=[0,91],_6k=function(_6l,_6m,_6n){var _6o=E(_6m);return _6o[0]==0?unAppCStr("[]",_6n):[1,_6j,new T(function(){return A(_6l,[_6o[1],new T(function(){var _6p=function(_6q){var _6r=E(_6q);return _6r[0]==0?E([1,_6i,_6n]):[1,_6h,new T(function(){return A(_6l,[_6r[1],new T(function(){return _6p(_6r[2]);})]);})];};return _6p(_6o[2]);})]);})];},_6s=function(_6t,_6u){return _6k(_6d,_6t,_6u);},_6v=function(_6w,_6x,_6y){var _6z=E(_6x);return _5W(_6z[1],_6z[2],_6z[3],_6z[4],_6z[6],_6y);},_6A=[0,_6v,_6a,_6s],_6B=new T(function(){return [0,_5c,_6A,_6C,_5s];}),_6C=function(_6D){return [0,_6B,_6D];},_6E=7,_6F=function(_6G){return [0,_w,_6E,_3,_6G,_w,_w];},_6H=function(_6I,_){return die(new T(function(){return _6C(new T(function(){return _6F(_6I);}));}));},_6J=function(_6K,_){return _6H(_6K,_);},_6L=[0,_53,_50,_4S,_6J],_6M=[0,_6L,_18],_6N=unCStr("base"),_6O=unCStr("Control.Exception.Base"),_6P=unCStr("PatternMatchFail"),_6Q=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_6N,_6O,_6P],_6R=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_6Q,_3],_6S=function(_6T){return E(_6R);},_6U=function(_6V){var _6W=E(_6V);return _5i(_5e(_6W[1]),_6S,_6W[2]);},_6X=function(_6Y){return E(E(_6Y)[1]);},_6Z=function(_70,_71){return _h(E(_70)[1],_71);},_72=function(_73,_74){return _6k(_6Z,_73,_74);},_75=function(_76,_77,_78){return _h(E(_77)[1],_78);},_79=[0,_75,_6X,_72],_7a=new T(function(){return [0,_6S,_79,_7b,_6U];}),_7b=function(_7c){return [0,_7a,_7c];},_7d=unCStr("Non-exhaustive patterns in"),_7e=function(_7f,_7g){return die(new T(function(){return A(_7g,[_7f]);}));},_7h=function(_7i,_7j){var _7k=E(_7j);if(!_7k[0]){return [0,_3,_3];}else{var _7l=_7k[1];if(!A(_7i,[_7l])){return [0,_3,_7k];}else{var _7m=new T(function(){var _7n=_7h(_7i,_7k[2]);return [0,_7n[1],_7n[2]];});return [0,[1,_7l,new T(function(){return E(E(_7m)[1]);})],new T(function(){return E(E(_7m)[2]);})];}}},_7o=[0,32],_7p=[0,10],_7q=[1,_7p,_3],_7r=function(_7s){return E(E(_7s)[1])==124?false:true;},_7t=function(_7u,_7v){var _7w=_7h(_7r,unCStr(_7u)),_7x=_7w[1],_7y=function(_7z,_7A){return _h(_7z,new T(function(){return unAppCStr(": ",new T(function(){return _h(_7v,new T(function(){return _h(_7A,_7q);}));}));}));},_7B=E(_7w[2]);return _7B[0]==0?_7y(_7x,_3):E(E(_7B[1])[1])==124?_7y(_7x,[1,_7o,_7B[2]]):_7y(_7x,_3);},_7C=function(_7D){return _7e([0,new T(function(){return _7t(_7D,_7d);})],_7b);},_7E=new T(function(){return _7C("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_7F=function(_7G,_7H){while(1){var _7I=(function(_7J,_7K){var _7L=E(_7J);switch(_7L[0]){case 0:var _7M=E(_7K);if(!_7M[0]){return [0];}else{_7G=A(_7L[1],[_7M[1]]);_7H=_7M[2];return null;}break;case 1:var _7N=A(_7L[1],[_7K]),_7O=_7K;_7G=_7N;_7H=_7O;return null;case 2:return [0];case 3:return [1,[0,_7L[1],_7K],new T(function(){return _7F(_7L[2],_7K);})];default:return E(_7L[1]);}})(_7G,_7H);if(_7I!=null){return _7I;}}},_7P=function(_7Q,_7R){var _7S=new T(function(){var _7T=E(_7R);if(_7T[0]==3){return [3,_7T[1],new T(function(){return _7P(_7Q,_7T[2]);})];}else{var _7U=E(_7Q);if(_7U[0]==2){return E(_7T);}else{var _7V=E(_7T);if(_7V[0]==2){return E(_7U);}else{var _7W=new T(function(){var _7X=E(_7V);if(_7X[0]==4){return [1,function(_7Y){return [4,new T(function(){return _h(_7F(_7U,_7Y),_7X[1]);})];}];}else{var _7Z=E(_7U);if(_7Z[0]==1){var _80=_7Z[1],_81=E(_7X);return _81[0]==0?[1,function(_82){return _7P(A(_80,[_82]),_81);}]:[1,function(_83){return _7P(A(_80,[_83]),new T(function(){return A(_81[1],[_83]);}));}];}else{var _84=E(_7X);return _84[0]==0?E(_7E):[1,function(_85){return _7P(_7Z,new T(function(){return A(_84[1],[_85]);}));}];}}}),_86=E(_7U);switch(_86[0]){case 1:var _87=E(_7V);return _87[0]==4?[1,function(_88){return [4,new T(function(){return _h(_7F(A(_86[1],[_88]),_88),_87[1]);})];}]:E(_7W);case 4:var _89=_86[1],_8a=E(_7V);switch(_8a[0]){case 0:return [1,function(_8b){return [4,new T(function(){return _h(_89,new T(function(){return _7F(_8a,_8b);}));})];}];case 1:return [1,function(_8c){return [4,new T(function(){return _h(_89,new T(function(){return _7F(A(_8a[1],[_8c]),_8c);}));})];}];default:return [4,new T(function(){return _h(_89,_8a[1]);})];}break;default:return E(_7W);}}}}}),_8d=E(_7Q);switch(_8d[0]){case 0:var _8e=E(_7R);return _8e[0]==0?[0,function(_8f){return _7P(A(_8d[1],[_8f]),new T(function(){return A(_8e[1],[_8f]);}));}]:E(_7S);case 3:return [3,_8d[1],new T(function(){return _7P(_8d[2],_7R);})];default:return E(_7S);}},_8g=function(_8h,_8i){return E(_8h)[1]!=E(_8i)[1];},_8j=function(_8k,_8l){return E(_8k)[1]==E(_8l)[1];},_8m=[0,_8j,_8g],_8n=function(_8o){return E(E(_8o)[1]);},_8p=function(_8q,_8r,_8s){while(1){var _8t=E(_8r);if(!_8t[0]){return E(_8s)[0]==0?true:false;}else{var _8u=E(_8s);if(!_8u[0]){return false;}else{if(!A(_8n,[_8q,_8t[1],_8u[1]])){return false;}else{_8r=_8t[2];_8s=_8u[2];continue;}}}}},_8v=function(_8w,_8x,_8y){return !_8p(_8w,_8x,_8y)?true:false;},_8z=function(_8A){return [0,function(_8B,_8C){return _8p(_8A,_8B,_8C);},function(_8B,_8C){return _8v(_8A,_8B,_8C);}];},_8D=new T(function(){return _8z(_8m);}),_8E=function(_8F,_8G){var _8H=E(_8F);switch(_8H[0]){case 0:return [0,function(_8I){return _8E(A(_8H[1],[_8I]),_8G);}];case 1:return [1,function(_8J){return _8E(A(_8H[1],[_8J]),_8G);}];case 2:return [2];case 3:return _7P(A(_8G,[_8H[1]]),new T(function(){return _8E(_8H[2],_8G);}));default:var _8K=function(_8L){var _8M=E(_8L);if(!_8M[0]){return [0];}else{var _8N=E(_8M[1]);return _h(_7F(A(_8G,[_8N[1]]),_8N[2]),new T(function(){return _8K(_8M[2]);}));}},_8O=_8K(_8H[1]);return _8O[0]==0?[2]:[4,_8O];}},_8P=[2],_8Q=function(_8R){return [3,_8R,_8P];},_8S=function(_8T,_8U){var _8V=E(_8T);if(!_8V){return A(_8U,[_0]);}else{var _8W=new T(function(){return _8S(_8V-1|0,_8U);});return [0,function(_8X){return E(_8W);}];}},_8Y=function(_8Z,_90,_91){var _92=new T(function(){return A(_8Z,[_8Q]);});return [1,function(_93){return A(function(_94,_95,_96){while(1){var _97=(function(_98,_99,_9a){var _9b=E(_98);switch(_9b[0]){case 0:var _9c=E(_99);if(!_9c[0]){return E(_90);}else{_94=A(_9b[1],[_9c[1]]);_95=_9c[2];var _9d=_9a+1|0;_96=_9d;return null;}break;case 1:var _9e=A(_9b[1],[_99]),_9f=_99,_9d=_9a;_94=_9e;_95=_9f;_96=_9d;return null;case 2:return E(_90);case 3:return function(_9g){var _9h=new T(function(){return _8E(_9b,_9g);});return _8S(_9a,function(_9i){return E(_9h);});};default:return function(_9j){return _8E(_9b,_9j);};}})(_94,_95,_96);if(_97!=null){return _97;}}},[_92,_93,0,_91]);}];},_9k=[6],_9l=unCStr("valDig: Bad base"),_9m=new T(function(){return err(_9l);}),_9n=function(_9o,_9p){var _9q=function(_9r,_9s){var _9t=E(_9r);if(!_9t[0]){var _9u=new T(function(){return A(_9s,[_3]);});return function(_9v){return A(_9v,[_9u]);};}else{var _9w=E(_9t[1])[1],_9x=function(_9y){var _9z=new T(function(){return _9q(_9t[2],function(_9A){return A(_9s,[[1,_9y,_9A]]);});});return function(_9B){var _9C=new T(function(){return A(_9z,[_9B]);});return [0,function(_9D){return E(_9C);}];};};switch(E(E(_9o)[1])){case 8:if(48>_9w){var _9E=new T(function(){return A(_9s,[_3]);});return function(_9F){return A(_9F,[_9E]);};}else{if(_9w>55){var _9G=new T(function(){return A(_9s,[_3]);});return function(_9H){return A(_9H,[_9G]);};}else{return _9x([0,_9w-48|0]);}}break;case 10:if(48>_9w){var _9I=new T(function(){return A(_9s,[_3]);});return function(_9J){return A(_9J,[_9I]);};}else{if(_9w>57){var _9K=new T(function(){return A(_9s,[_3]);});return function(_9L){return A(_9L,[_9K]);};}else{return _9x([0,_9w-48|0]);}}break;case 16:var _9M=new T(function(){return 97>_9w?65>_9w?[0]:_9w>70?[0]:[1,[0,(_9w-65|0)+10|0]]:_9w>102?65>_9w?[0]:_9w>70?[0]:[1,[0,(_9w-65|0)+10|0]]:[1,[0,(_9w-97|0)+10|0]];});if(48>_9w){var _9N=E(_9M);if(!_9N[0]){var _9O=new T(function(){return A(_9s,[_3]);});return function(_9P){return A(_9P,[_9O]);};}else{return _9x(_9N[1]);}}else{if(_9w>57){var _9Q=E(_9M);if(!_9Q[0]){var _9R=new T(function(){return A(_9s,[_3]);});return function(_9S){return A(_9S,[_9R]);};}else{return _9x(_9Q[1]);}}else{return _9x([0,_9w-48|0]);}}break;default:return E(_9m);}}};return [1,function(_9T){return A(_9q,[_9T,_18,function(_9U){var _9V=E(_9U);return _9V[0]==0?[2]:A(_9p,[_9V]);}]);}];},_9W=[0,10],_9X=[0,1],_9Y=[0,2147483647],_9Z=function(_a0,_a1){while(1){var _a2=E(_a0);if(!_a2[0]){var _a3=_a2[1],_a4=E(_a1);if(!_a4[0]){var _a5=_a4[1],_a6=addC(_a3,_a5);if(!E(_a6[2])){return [0,_a6[1]];}else{_a0=[1,I_fromInt(_a3)];_a1=[1,I_fromInt(_a5)];continue;}}else{_a0=[1,I_fromInt(_a3)];_a1=_a4;continue;}}else{var _a7=E(_a1);if(!_a7[0]){_a0=_a2;_a1=[1,I_fromInt(_a7[1])];continue;}else{return [1,I_add(_a2[1],_a7[1])];}}}},_a8=new T(function(){return _9Z(_9Y,_9X);}),_a9=function(_aa){var _ab=E(_aa);if(!_ab[0]){var _ac=E(_ab[1]);return _ac==(-2147483648)?E(_a8):[0, -_ac];}else{return [1,I_negate(_ab[1])];}},_ad=[0,10],_ae=[0,0],_af=function(_ag,_ah){while(1){var _ai=E(_ag);if(!_ai[0]){var _aj=_ai[1],_ak=E(_ah);if(!_ak[0]){var _al=_ak[1];if(!(imul(_aj,_al)|0)){return [0,imul(_aj,_al)|0];}else{_ag=[1,I_fromInt(_aj)];_ah=[1,I_fromInt(_al)];continue;}}else{_ag=[1,I_fromInt(_aj)];_ah=_ak;continue;}}else{var _am=E(_ah);if(!_am[0]){_ag=_ai;_ah=[1,I_fromInt(_am[1])];continue;}else{return [1,I_mul(_ai[1],_am[1])];}}}},_an=function(_ao,_ap,_aq){while(1){var _ar=E(_aq);if(!_ar[0]){return E(_ap);}else{var _as=_9Z(_af(_ap,_ao),_ar[1]);_aq=_ar[2];_ap=_as;continue;}}},_at=function(_au){var _av=new T(function(){return _7P(_7P([0,function(_aw){return E(E(_aw)[1])==45?_9n(_9W,function(_ax){return A(_au,[[1,new T(function(){return _a9(_an(_ad,_ae,_ax));})]]);}):[2];}],[0,function(_ay){return E(E(_ay)[1])==43?_9n(_9W,function(_az){return A(_au,[[1,new T(function(){return _an(_ad,_ae,_az);})]]);}):[2];}]),new T(function(){return _9n(_9W,function(_aA){return A(_au,[[1,new T(function(){return _an(_ad,_ae,_aA);})]]);});}));});return _7P([0,function(_aB){return E(E(_aB)[1])==101?E(_av):[2];}],[0,function(_aC){return E(E(_aC)[1])==69?E(_av):[2];}]);},_aD=function(_aE){return A(_aE,[_w]);},_aF=function(_aG){return A(_aG,[_w]);},_aH=function(_aI){var _aJ=new T(function(){return _9n(_9W,function(_aK){return A(_aI,[[1,_aK]]);});});return [0,function(_aL){return E(E(_aL)[1])==46?E(_aJ):[2];}];},_aM=function(_aN){return _9n(_9W,function(_aO){return _8Y(_aH,_aD,function(_aP){return _8Y(_at,_aF,function(_aQ){return A(_aN,[[5,[1,_aO,_aP,_aQ]]]);});});});},_aR=function(_aS,_aT,_aU){while(1){var _aV=E(_aU);if(!_aV[0]){return false;}else{if(!A(_8n,[_aS,_aT,_aV[1]])){_aU=_aV[2];continue;}else{return true;}}}},_aW=unCStr("!@#$%&*+./<=>?\\^|:-~"),_aX=function(_aY){return _aR(_8m,_aY,_aW);},_aZ=[0,8],_b0=[0,16],_b1=function(_b2){var _b3=new T(function(){return _9n(_b0,function(_b4){return A(_b2,[[5,[0,_b0,_b4]]]);});}),_b5=new T(function(){return _9n(_aZ,function(_b6){return A(_b2,[[5,[0,_aZ,_b6]]]);});}),_b7=new T(function(){return _9n(_b0,function(_b8){return A(_b2,[[5,[0,_b0,_b8]]]);});}),_b9=new T(function(){return _9n(_aZ,function(_ba){return A(_b2,[[5,[0,_aZ,_ba]]]);});});return [0,function(_bb){return E(E(_bb)[1])==48?E([0,function(_bc){switch(E(E(_bc)[1])){case 79:return E(_b9);case 88:return E(_b7);case 111:return E(_b5);case 120:return E(_b3);default:return [2];}}]):[2];}];},_bd=false,_be=true,_bf=function(_bg){var _bh=new T(function(){return A(_bg,[_b0]);}),_bi=new T(function(){return A(_bg,[_aZ]);}),_bj=new T(function(){return A(_bg,[_b0]);}),_bk=new T(function(){return A(_bg,[_aZ]);});return [0,function(_bl){switch(E(E(_bl)[1])){case 79:return E(_bk);case 88:return E(_bj);case 111:return E(_bi);case 120:return E(_bh);default:return [2];}}];},_bm=function(_bn){return A(_bn,[_9W]);},_bo=function(_bp){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _r(9,_bp,_3);})));},_bq=function(_br){var _bs=E(_br);return _bs[0]==0?E(_bs[1]):I_toInt(_bs[1]);},_bt=function(_bu,_bv){var _bw=E(_bu);if(!_bw[0]){var _bx=_bw[1],_by=E(_bv);return _by[0]==0?_bx<=_by[1]:I_compareInt(_by[1],_bx)>=0;}else{var _bz=_bw[1],_bA=E(_bv);return _bA[0]==0?I_compareInt(_bz,_bA[1])<=0:I_compare(_bz,_bA[1])<=0;}},_bB=function(_bC){return [2];},_bD=function(_bE){var _bF=E(_bE);if(!_bF[0]){return E(_bB);}else{var _bG=_bF[1],_bH=E(_bF[2]);if(!_bH[0]){return E(_bG);}else{var _bI=new T(function(){return _bD(_bH);});return function(_bJ){return _7P(A(_bG,[_bJ]),new T(function(){return A(_bI,[_bJ]);}));};}}},_bK=unCStr("NUL"),_bL=function(_bM){return [2];},_bN=function(_bO){return _bL(_bO);},_bP=function(_bQ,_bR){var _bS=function(_bT,_bU){var _bV=E(_bT);if(!_bV[0]){return function(_bW){return A(_bW,[_bQ]);};}else{var _bX=E(_bU);if(!_bX[0]){return E(_bL);}else{if(E(_bV[1])[1]!=E(_bX[1])[1]){return E(_bN);}else{var _bY=new T(function(){return _bS(_bV[2],_bX[2]);});return function(_bZ){var _c0=new T(function(){return A(_bY,[_bZ]);});return [0,function(_c1){return E(_c0);}];};}}}};return [1,function(_c2){return A(_bS,[_bQ,_c2,_bR]);}];},_c3=[0,0],_c4=function(_c5){var _c6=new T(function(){return A(_c5,[_c3]);});return _bP(_bK,function(_c7){return E(_c6);});},_c8=unCStr("STX"),_c9=[0,2],_ca=function(_cb){var _cc=new T(function(){return A(_cb,[_c9]);});return _bP(_c8,function(_cd){return E(_cc);});},_ce=unCStr("ETX"),_cf=[0,3],_cg=function(_ch){var _ci=new T(function(){return A(_ch,[_cf]);});return _bP(_ce,function(_cj){return E(_ci);});},_ck=unCStr("EOT"),_cl=[0,4],_cm=function(_cn){var _co=new T(function(){return A(_cn,[_cl]);});return _bP(_ck,function(_cp){return E(_co);});},_cq=unCStr("ENQ"),_cr=[0,5],_cs=function(_ct){var _cu=new T(function(){return A(_ct,[_cr]);});return _bP(_cq,function(_cv){return E(_cu);});},_cw=unCStr("ACK"),_cx=[0,6],_cy=function(_cz){var _cA=new T(function(){return A(_cz,[_cx]);});return _bP(_cw,function(_cB){return E(_cA);});},_cC=unCStr("BEL"),_cD=[0,7],_cE=function(_cF){var _cG=new T(function(){return A(_cF,[_cD]);});return _bP(_cC,function(_cH){return E(_cG);});},_cI=unCStr("BS"),_cJ=[0,8],_cK=function(_cL){var _cM=new T(function(){return A(_cL,[_cJ]);});return _bP(_cI,function(_cN){return E(_cM);});},_cO=unCStr("HT"),_cP=[0,9],_cQ=function(_cR){var _cS=new T(function(){return A(_cR,[_cP]);});return _bP(_cO,function(_cT){return E(_cS);});},_cU=unCStr("LF"),_cV=[0,10],_cW=function(_cX){var _cY=new T(function(){return A(_cX,[_cV]);});return _bP(_cU,function(_cZ){return E(_cY);});},_d0=unCStr("VT"),_d1=[0,11],_d2=function(_d3){var _d4=new T(function(){return A(_d3,[_d1]);});return _bP(_d0,function(_d5){return E(_d4);});},_d6=unCStr("FF"),_d7=[0,12],_d8=function(_d9){var _da=new T(function(){return A(_d9,[_d7]);});return _bP(_d6,function(_db){return E(_da);});},_dc=unCStr("CR"),_dd=[0,13],_de=function(_df){var _dg=new T(function(){return A(_df,[_dd]);});return _bP(_dc,function(_dh){return E(_dg);});},_di=unCStr("SI"),_dj=[0,15],_dk=function(_dl){var _dm=new T(function(){return A(_dl,[_dj]);});return _bP(_di,function(_dn){return E(_dm);});},_do=unCStr("DLE"),_dp=[0,16],_dq=function(_dr){var _ds=new T(function(){return A(_dr,[_dp]);});return _bP(_do,function(_dt){return E(_ds);});},_du=unCStr("DC1"),_dv=[0,17],_dw=function(_dx){var _dy=new T(function(){return A(_dx,[_dv]);});return _bP(_du,function(_dz){return E(_dy);});},_dA=unCStr("DC2"),_dB=[0,18],_dC=function(_dD){var _dE=new T(function(){return A(_dD,[_dB]);});return _bP(_dA,function(_dF){return E(_dE);});},_dG=unCStr("DC3"),_dH=[0,19],_dI=function(_dJ){var _dK=new T(function(){return A(_dJ,[_dH]);});return _bP(_dG,function(_dL){return E(_dK);});},_dM=unCStr("DC4"),_dN=[0,20],_dO=function(_dP){var _dQ=new T(function(){return A(_dP,[_dN]);});return _bP(_dM,function(_dR){return E(_dQ);});},_dS=unCStr("NAK"),_dT=[0,21],_dU=function(_dV){var _dW=new T(function(){return A(_dV,[_dT]);});return _bP(_dS,function(_dX){return E(_dW);});},_dY=unCStr("SYN"),_dZ=[0,22],_e0=function(_e1){var _e2=new T(function(){return A(_e1,[_dZ]);});return _bP(_dY,function(_e3){return E(_e2);});},_e4=unCStr("ETB"),_e5=[0,23],_e6=function(_e7){var _e8=new T(function(){return A(_e7,[_e5]);});return _bP(_e4,function(_e9){return E(_e8);});},_ea=unCStr("CAN"),_eb=[0,24],_ec=function(_ed){var _ee=new T(function(){return A(_ed,[_eb]);});return _bP(_ea,function(_ef){return E(_ee);});},_eg=unCStr("EM"),_eh=[0,25],_ei=function(_ej){var _ek=new T(function(){return A(_ej,[_eh]);});return _bP(_eg,function(_el){return E(_ek);});},_em=unCStr("SUB"),_en=[0,26],_eo=function(_ep){var _eq=new T(function(){return A(_ep,[_en]);});return _bP(_em,function(_er){return E(_eq);});},_es=unCStr("ESC"),_et=[0,27],_eu=function(_ev){var _ew=new T(function(){return A(_ev,[_et]);});return _bP(_es,function(_ex){return E(_ew);});},_ey=unCStr("FS"),_ez=[0,28],_eA=function(_eB){var _eC=new T(function(){return A(_eB,[_ez]);});return _bP(_ey,function(_eD){return E(_eC);});},_eE=unCStr("GS"),_eF=[0,29],_eG=function(_eH){var _eI=new T(function(){return A(_eH,[_eF]);});return _bP(_eE,function(_eJ){return E(_eI);});},_eK=unCStr("RS"),_eL=[0,30],_eM=function(_eN){var _eO=new T(function(){return A(_eN,[_eL]);});return _bP(_eK,function(_eP){return E(_eO);});},_eQ=unCStr("US"),_eR=[0,31],_eS=function(_eT){var _eU=new T(function(){return A(_eT,[_eR]);});return _bP(_eQ,function(_eV){return E(_eU);});},_eW=unCStr("SP"),_eX=[0,32],_eY=function(_eZ){var _f0=new T(function(){return A(_eZ,[_eX]);});return _bP(_eW,function(_f1){return E(_f0);});},_f2=unCStr("DEL"),_f3=[0,127],_f4=function(_f5){var _f6=new T(function(){return A(_f5,[_f3]);});return _bP(_f2,function(_f7){return E(_f6);});},_f8=[1,_f4,_3],_f9=[1,_eY,_f8],_fa=[1,_eS,_f9],_fb=[1,_eM,_fa],_fc=[1,_eG,_fb],_fd=[1,_eA,_fc],_fe=[1,_eu,_fd],_ff=[1,_eo,_fe],_fg=[1,_ei,_ff],_fh=[1,_ec,_fg],_fi=[1,_e6,_fh],_fj=[1,_e0,_fi],_fk=[1,_dU,_fj],_fl=[1,_dO,_fk],_fm=[1,_dI,_fl],_fn=[1,_dC,_fm],_fo=[1,_dw,_fn],_fp=[1,_dq,_fo],_fq=[1,_dk,_fp],_fr=[1,_de,_fq],_fs=[1,_d8,_fr],_ft=[1,_d2,_fs],_fu=[1,_cW,_ft],_fv=[1,_cQ,_fu],_fw=[1,_cK,_fv],_fx=[1,_cE,_fw],_fy=[1,_cy,_fx],_fz=[1,_cs,_fy],_fA=[1,_cm,_fz],_fB=[1,_cg,_fA],_fC=[1,_ca,_fB],_fD=[1,_c4,_fC],_fE=unCStr("SOH"),_fF=[0,1],_fG=function(_fH){var _fI=new T(function(){return A(_fH,[_fF]);});return _bP(_fE,function(_fJ){return E(_fI);});},_fK=unCStr("SO"),_fL=[0,14],_fM=function(_fN){var _fO=new T(function(){return A(_fN,[_fL]);});return _bP(_fK,function(_fP){return E(_fO);});},_fQ=function(_fR){return _8Y(_fG,_fM,_fR);},_fS=[1,_fQ,_fD],_fT=new T(function(){return _bD(_fS);}),_fU=[0,1114111],_fV=[0,34],_fW=[0,_fV,_be],_fX=[0,39],_fY=[0,_fX,_be],_fZ=[0,92],_g0=[0,_fZ,_be],_g1=[0,_cD,_be],_g2=[0,_cJ,_be],_g3=[0,_d7,_be],_g4=[0,_cV,_be],_g5=[0,_dd,_be],_g6=[0,_cP,_be],_g7=[0,_d1,_be],_g8=[0,_c3,_be],_g9=[0,_fF,_be],_ga=[0,_c9,_be],_gb=[0,_cf,_be],_gc=[0,_cl,_be],_gd=[0,_cr,_be],_ge=[0,_cx,_be],_gf=[0,_cD,_be],_gg=[0,_cJ,_be],_gh=[0,_cP,_be],_gi=[0,_cV,_be],_gj=[0,_d1,_be],_gk=[0,_d7,_be],_gl=[0,_dd,_be],_gm=[0,_fL,_be],_gn=[0,_dj,_be],_go=[0,_dp,_be],_gp=[0,_dv,_be],_gq=[0,_dB,_be],_gr=[0,_dH,_be],_gs=[0,_dN,_be],_gt=[0,_dT,_be],_gu=[0,_dZ,_be],_gv=[0,_e5,_be],_gw=[0,_eb,_be],_gx=[0,_eh,_be],_gy=[0,_en,_be],_gz=[0,_et,_be],_gA=[0,_ez,_be],_gB=[0,_eF,_be],_gC=[0,_eL,_be],_gD=[0,_eR,_be],_gE=function(_gF){return [0,_gF];},_gG=function(_gH){var _gI=new T(function(){return A(_gH,[_g7]);}),_gJ=new T(function(){return A(_gH,[_g6]);}),_gK=new T(function(){return A(_gH,[_g5]);}),_gL=new T(function(){return A(_gH,[_g4]);}),_gM=new T(function(){return A(_gH,[_g3]);}),_gN=new T(function(){return A(_gH,[_g2]);}),_gO=new T(function(){return A(_gH,[_g1]);}),_gP=new T(function(){return A(_gH,[_g0]);}),_gQ=new T(function(){return A(_gH,[_fY]);}),_gR=new T(function(){return A(_gH,[_fW]);});return _7P([0,function(_gS){switch(E(E(_gS)[1])){case 34:return E(_gR);case 39:return E(_gQ);case 92:return E(_gP);case 97:return E(_gO);case 98:return E(_gN);case 102:return E(_gM);case 110:return E(_gL);case 114:return E(_gK);case 116:return E(_gJ);case 118:return E(_gI);default:return [2];}}],new T(function(){return _7P(_8Y(_bf,_bm,function(_gT){var _gU=new T(function(){return _gE(E(_gT)[1]);});return _9n(_gT,function(_gV){var _gW=_an(_gU,_ae,_gV);return !_bt(_gW,_fU)?[2]:A(_gH,[[0,new T(function(){var _gX=_bq(_gW);return _gX>>>0>1114111?_bo(_gX):[0,_gX];}),_be]]);});}),new T(function(){var _gY=new T(function(){return A(_gH,[_gD]);}),_gZ=new T(function(){return A(_gH,[_gC]);}),_h0=new T(function(){return A(_gH,[_gB]);}),_h1=new T(function(){return A(_gH,[_gA]);}),_h2=new T(function(){return A(_gH,[_gz]);}),_h3=new T(function(){return A(_gH,[_gy]);}),_h4=new T(function(){return A(_gH,[_gx]);}),_h5=new T(function(){return A(_gH,[_gw]);}),_h6=new T(function(){return A(_gH,[_gv]);}),_h7=new T(function(){return A(_gH,[_gu]);}),_h8=new T(function(){return A(_gH,[_gt]);}),_h9=new T(function(){return A(_gH,[_gs]);}),_ha=new T(function(){return A(_gH,[_gr]);}),_hb=new T(function(){return A(_gH,[_gq]);}),_hc=new T(function(){return A(_gH,[_gp]);}),_hd=new T(function(){return A(_gH,[_go]);}),_he=new T(function(){return A(_gH,[_gn]);}),_hf=new T(function(){return A(_gH,[_gm]);}),_hg=new T(function(){return A(_gH,[_gl]);}),_hh=new T(function(){return A(_gH,[_gk]);}),_hi=new T(function(){return A(_gH,[_gj]);}),_hj=new T(function(){return A(_gH,[_gi]);}),_hk=new T(function(){return A(_gH,[_gh]);}),_hl=new T(function(){return A(_gH,[_gg]);}),_hm=new T(function(){return A(_gH,[_gf]);}),_hn=new T(function(){return A(_gH,[_ge]);}),_ho=new T(function(){return A(_gH,[_gd]);}),_hp=new T(function(){return A(_gH,[_gc]);}),_hq=new T(function(){return A(_gH,[_gb]);}),_hr=new T(function(){return A(_gH,[_ga]);}),_hs=new T(function(){return A(_gH,[_g9]);}),_ht=new T(function(){return A(_gH,[_g8]);});return _7P([0,function(_hu){return E(E(_hu)[1])==94?E([0,function(_hv){switch(E(E(_hv)[1])){case 64:return E(_ht);case 65:return E(_hs);case 66:return E(_hr);case 67:return E(_hq);case 68:return E(_hp);case 69:return E(_ho);case 70:return E(_hn);case 71:return E(_hm);case 72:return E(_hl);case 73:return E(_hk);case 74:return E(_hj);case 75:return E(_hi);case 76:return E(_hh);case 77:return E(_hg);case 78:return E(_hf);case 79:return E(_he);case 80:return E(_hd);case 81:return E(_hc);case 82:return E(_hb);case 83:return E(_ha);case 84:return E(_h9);case 85:return E(_h8);case 86:return E(_h7);case 87:return E(_h6);case 88:return E(_h5);case 89:return E(_h4);case 90:return E(_h3);case 91:return E(_h2);case 92:return E(_h1);case 93:return E(_h0);case 94:return E(_gZ);case 95:return E(_gY);default:return [2];}}]):[2];}],new T(function(){return A(_fT,[function(_hw){return A(_gH,[[0,_hw,_be]]);}]);}));}));}));},_hx=function(_hy){return A(_hy,[_0]);},_hz=function(_hA){var _hB=E(_hA);if(!_hB[0]){return E(_hx);}else{var _hC=_hB[2],_hD=E(E(_hB[1])[1]);switch(_hD){case 9:var _hE=new T(function(){return _hz(_hC);});return function(_hF){var _hG=new T(function(){return A(_hE,[_hF]);});return [0,function(_hH){return E(_hG);}];};case 10:var _hI=new T(function(){return _hz(_hC);});return function(_hJ){var _hK=new T(function(){return A(_hI,[_hJ]);});return [0,function(_hL){return E(_hK);}];};case 11:var _hM=new T(function(){return _hz(_hC);});return function(_hN){var _hO=new T(function(){return A(_hM,[_hN]);});return [0,function(_hP){return E(_hO);}];};case 12:var _hQ=new T(function(){return _hz(_hC);});return function(_hR){var _hS=new T(function(){return A(_hQ,[_hR]);});return [0,function(_hT){return E(_hS);}];};case 13:var _hU=new T(function(){return _hz(_hC);});return function(_hV){var _hW=new T(function(){return A(_hU,[_hV]);});return [0,function(_hX){return E(_hW);}];};case 32:var _hY=new T(function(){return _hz(_hC);});return function(_hZ){var _i0=new T(function(){return A(_hY,[_hZ]);});return [0,function(_i1){return E(_i0);}];};case 160:var _i2=new T(function(){return _hz(_hC);});return function(_i3){var _i4=new T(function(){return A(_i2,[_i3]);});return [0,function(_i5){return E(_i4);}];};default:var _i6=u_iswspace(_hD);if(!E(_i6)){return E(_hx);}else{var _i7=new T(function(){return _hz(_hC);});return function(_i8){var _i9=new T(function(){return A(_i7,[_i8]);});return [0,function(_ia){return E(_i9);}];};}}}},_ib=function(_ic){var _id=new T(function(){return _gG(_ic);}),_ie=new T(function(){return _ib(_ic);}),_if=[1,function(_ig){return A(_hz,[_ig,function(_ih){return E([0,function(_ii){return E(E(_ii)[1])==92?E(_ie):[2];}]);}]);}];return _7P([0,function(_ij){return E(E(_ij)[1])==92?E([0,function(_ik){var _il=E(E(_ik)[1]);switch(_il){case 9:return E(_if);case 10:return E(_if);case 11:return E(_if);case 12:return E(_if);case 13:return E(_if);case 32:return E(_if);case 38:return E(_ie);case 160:return E(_if);default:var _im=u_iswspace(_il);return E(_im)==0?[2]:E(_if);}}]):[2];}],[0,function(_in){var _io=E(_in);return E(_io[1])==92?E(_id):A(_ic,[[0,_io,_bd]]);}]);},_ip=function(_iq,_ir){var _is=new T(function(){return A(_ir,[[1,new T(function(){return A(_iq,[_3]);})]]);});return _ib(function(_it){var _iu=E(_it),_iv=E(_iu[1]);return E(_iv[1])==34?!E(_iu[2])?E(_is):_ip(function(_iw){return A(_iq,[[1,_iv,_iw]]);},_ir):_ip(function(_ix){return A(_iq,[[1,_iv,_ix]]);},_ir);});},_iy=unCStr("_\'"),_iz=function(_iA){var _iB=u_iswalnum(_iA);return E(_iB)==0?_aR(_8m,[0,_iA],_iy):true;},_iC=function(_iD){return _iz(E(_iD)[1]);},_iE=unCStr(",;()[]{}`"),_iF=function(_iG){return A(_iG,[_3]);},_iH=function(_iI,_iJ){var _iK=function(_iL){var _iM=E(_iL);if(!_iM[0]){return E(_iF);}else{var _iN=_iM[1];if(!A(_iI,[_iN])){return E(_iF);}else{var _iO=new T(function(){return _iK(_iM[2]);});return function(_iP){var _iQ=new T(function(){return A(_iO,[function(_iR){return A(_iP,[[1,_iN,_iR]]);}]);});return [0,function(_iS){return E(_iQ);}];};}}};return [1,function(_iT){return A(_iK,[_iT,_iJ]);}];},_iU=unCStr(".."),_iV=unCStr("::"),_iW=unCStr("->"),_iX=[0,64],_iY=[1,_iX,_3],_iZ=[0,126],_j0=[1,_iZ,_3],_j1=unCStr("=>"),_j2=[1,_j1,_3],_j3=[1,_j0,_j2],_j4=[1,_iY,_j3],_j5=[1,_iW,_j4],_j6=unCStr("<-"),_j7=[1,_j6,_j5],_j8=[0,124],_j9=[1,_j8,_3],_ja=[1,_j9,_j7],_jb=[1,_fZ,_3],_jc=[1,_jb,_ja],_jd=[0,61],_je=[1,_jd,_3],_jf=[1,_je,_jc],_jg=[1,_iV,_jf],_jh=[1,_iU,_jg],_ji=function(_jj){var _jk=new T(function(){return A(_jj,[_9k]);});return _7P([1,function(_jl){return E(_jl)[0]==0?E(_jk):[2];}],new T(function(){var _jm=new T(function(){return _gG(function(_jn){var _jo=E(_jn);return (function(_jp,_jq){var _jr=new T(function(){return A(_jj,[[0,_jp]]);});return !E(_jq)?E(E(_jp)[1])==39?[2]:[0,function(_js){return E(E(_js)[1])==39?E(_jr):[2];}]:[0,function(_jt){return E(E(_jt)[1])==39?E(_jr):[2];}];})(_jo[1],_jo[2]);});});return _7P([0,function(_ju){return E(E(_ju)[1])==39?E([0,function(_jv){var _jw=E(_jv);switch(E(_jw[1])){case 39:return [2];case 92:return E(_jm);default:var _jx=new T(function(){return A(_jj,[[0,_jw]]);});return [0,function(_jy){return E(E(_jy)[1])==39?E(_jx):[2];}];}}]):[2];}],new T(function(){var _jz=new T(function(){return _ip(_18,_jj);});return _7P([0,function(_jA){return E(E(_jA)[1])==34?E(_jz):[2];}],new T(function(){return _7P([0,function(_jB){return !_aR(_8m,_jB,_iE)?[2]:A(_jj,[[2,[1,_jB,_3]]]);}],new T(function(){return _7P([0,function(_jC){return !_aR(_8m,_jC,_aW)?[2]:_iH(_aX,function(_jD){var _jE=[1,_jC,_jD];return !_aR(_8D,_jE,_jh)?A(_jj,[[4,_jE]]):A(_jj,[[2,_jE]]);});}],new T(function(){return _7P([0,function(_jF){var _jG=E(_jF),_jH=_jG[1],_jI=u_iswalpha(_jH);return E(_jI)==0?E(_jH)==95?_iH(_iC,function(_jJ){return A(_jj,[[3,[1,_jG,_jJ]]]);}):[2]:_iH(_iC,function(_jK){return A(_jj,[[3,[1,_jG,_jK]]]);});}],new T(function(){return _8Y(_b1,_aM,_jj);}));}));}));}));}));}));},_jL=function(_jM){var _jN=new T(function(){return _ji(_jM);});return [1,function(_jO){return A(_hz,[_jO,function(_jP){return E(_jN);}]);}];},_jQ=[0,0],_jR=function(_jS,_jT){var _jU=new T(function(){return A(_jS,[_jQ,function(_jV){var _jW=new T(function(){return A(_jT,[_jV]);});return _jL(function(_jX){var _jY=E(_jX);if(_jY[0]==2){var _jZ=E(_jY[1]);return _jZ[0]==0?[2]:E(E(_jZ[1])[1])==41?E(_jZ[2])[0]==0?E(_jW):[2]:[2];}else{return [2];}});}]);});return _jL(function(_k0){var _k1=E(_k0);if(_k1[0]==2){var _k2=E(_k1[1]);return _k2[0]==0?[2]:E(E(_k2[1])[1])==40?E(_k2[2])[0]==0?E(_jU):[2]:[2];}else{return [2];}});},_k3=function(_k4,_k5,_k6){var _k7=function(_k8,_k9){var _ka=new T(function(){return _ji(function(_kb){return A(_k4,[_kb,_k8,function(_kc){return A(_k9,[new T(function(){return [0, -E(_kc)[1]];})]);}]);});});return _7P(_jL(function(_kd){var _ke=E(_kd);if(_ke[0]==4){var _kf=E(_ke[1]);return _kf[0]==0?A(_k4,[_ke,_k8,_k9]):E(E(_kf[1])[1])==45?E(_kf[2])[0]==0?E([1,function(_kg){return A(_hz,[_kg,function(_kh){return E(_ka);}]);}]):A(_k4,[_ke,_k8,_k9]):A(_k4,[_ke,_k8,_k9]);}else{return A(_k4,[_ke,_k8,_k9]);}}),new T(function(){return _jR(_k7,_k9);}));};return _k7(_k5,_k6);},_ki=function(_kj,_kk){return [2];},_kl=function(_km,_kn){return _ki(_km,_kn);},_ko=function(_kp){var _kq=E(_kp);return _kq[0]==0?[1,new T(function(){return _an(new T(function(){return _gE(E(_kq[1])[1]);}),_ae,_kq[2]);})]:E(_kq[2])[0]==0?E(_kq[3])[0]==0?[1,new T(function(){return _an(_ad,_ae,_kq[1]);})]:[0]:[0];},_kr=function(_ks){var _kt=E(_ks);if(_kt[0]==5){var _ku=_ko(_kt[1]);if(!_ku[0]){return E(_ki);}else{var _kv=new T(function(){return [0,_bq(_ku[1])];});return function(_kw,_kx){return A(_kx,[_kv]);};}}else{return E(_kl);}},_ky=function(_km,_kn){return _k3(_kr,_km,_kn);},_kz=function(_kA,_kB){var _kC=function(_kD,_kE){var _kF=new T(function(){return A(_kE,[_3]);}),_kG=new T(function(){return A(_kA,[_jQ,function(_kH){return _kC(_be,function(_kI){return A(_kE,[[1,_kH,_kI]]);});}]);});return _jL(function(_kJ){var _kK=E(_kJ);if(_kK[0]==2){var _kL=E(_kK[1]);if(!_kL[0]){return [2];}else{var _kM=_kL[2];switch(E(E(_kL[1])[1])){case 44:return E(_kM)[0]==0?!E(_kD)?[2]:E(_kG):[2];case 93:return E(_kM)[0]==0?E(_kF):[2];default:return [2];}}}else{return [2];}});},_kN=function(_kO){var _kP=new T(function(){return _7P(_kC(_bd,_kO),new T(function(){return A(_kA,[_jQ,function(_kQ){return _kC(_be,function(_kR){return A(_kO,[[1,_kQ,_kR]]);});}]);}));});return _7P(_jL(function(_kS){var _kT=E(_kS);if(_kT[0]==2){var _kU=E(_kT[1]);return _kU[0]==0?[2]:E(E(_kU[1])[1])==91?E(_kU[2])[0]==0?E(_kP):[2]:[2];}else{return [2];}}),new T(function(){return _jR(function(_kV,_kW){return _kN(_kW);},_kO);}));};return _kN(_kB);},_kX=function(_kY,_kZ){return _kz(_ky,_kZ);},_l0=new T(function(){return _kz(_ky,_8Q);}),_l1=function(_kn){return _7F(_l0,_kn);},_l2=function(_l3){var _l4=new T(function(){return _k3(_kr,_l3,_8Q);});return function(_9j){return _7F(_l4,_9j);};},_l5=[0,_l2,_l1,_ky,_kX],_l6=function(_l7){return _r(0,E(_l7)[1],_3);},_l8=function(_l9,_la){return _r(0,E(_l9)[1],_la);},_lb=function(_lc,_ld){return _6k(_l8,_lc,_ld);},_le=function(_lf,_lg,_lh){return _r(E(_lf)[1],E(_lg)[1],_lh);},_li=[0,_le,_l6,_lb],_lj=unCStr("GHC.Types"),_lk=unCStr("Int"),_ll=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_1E,_lj,_lk],_lm=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_ll,_3],_ln=function(_lo){return E(_lm);},_lp=function(_lq){return E(E(_lq)[1]);},_lr=function(_ls,_lt,_lu,_lv){return A(_lp,[_ls,new T(function(){return A(_lt,[_lv]);}),function(_lw){return A(_lu,[new T(function(){return E(E(_lw)[1]);}),new T(function(){return E(E(_lw)[2]);})]);}]);},_lx=function(_ly,_lz,_lA,_lB){return A(_lp,[_ly,new T(function(){return A(_lz,[_lB]);}),function(_lC){return A(_lA,[new T(function(){return E(E(_lC)[2]);})]);}]);},_lD=function(_lE,_lF,_lG,_lH){return _lx(_lE,_lF,_lG,_lH);},_lI=function(_lJ){return E(E(_lJ)[4]);},_lK=function(_lL,_lM){var _lN=new T(function(){return A(_lI,[_lL,_lM]);});return function(_lO){return E(_lN);};},_lP=function(_lQ){return E(E(_lQ)[3]);},_lR=function(_lS){var _lT=new T(function(){return _lP(_lS);});return [0,function(_lF,_lG,_lH){return _lr(_lS,_lF,_lG,_lH);},function(_lF,_lG,_lH){return _lD(_lS,_lF,_lG,_lH);},function(_lU,_lV){return A(_lT,[[0,_lU,_lV]]);},function(_lH){return _lK(_lS,_lH);}];},_lW=function(_lX){return E(E(_lX)[1]);},_lY=function(_lZ){return E(E(_lZ)[1]);},_m0=function(_m1){return E(E(_m1)[2]);},_m2=function(_m3,_m4){var _m5=new T(function(){return A(_m0,[_m3,_m4]);}),_m6=new T(function(){return _lY(_m3);}),_m7=new T(function(){return _lP(_m6);}),_m8=new T(function(){return _lp(_m6);});return function(_m9){return A(_m8,[_m5,function(_ma){return A(_m7,[[0,_ma,_m9]]);}]);};},_mb=[0,112],_mc=function(_md,_me,_mf,_mg){var _mh=E(_me);return A(_mh[1],[new T(function(){var _mi=E(_md);return E(_mf);}),function(_mj){var _mk=new T(function(){return E(E(_mj)[2]);});return A(_mh[2],[new T(function(){return A(_mg,[new T(function(){var _ml=E(new T(function(){var _mm=E(_md);return [0,coercionToken];})),_mn=E(_mj);return [0,_mn[1],new T(function(){return [0,E(_mk)[1]+1|0];}),_mn[3],_mn[4]];})]);}),new T(function(){return A(_mh[3],[[1,_mb,new T(function(){return _h(_r(0,E(_mk)[1],_3),new T(function(){return E(E(_mj)[1]);}));})]]);})]);}]);},_mo=function(_mp,_mq){return A(_mp,[function(_){return jsFind(toJSStr(E(_mq)));}]);},_mr=function(_ms){return E(E(_ms)[4]);},_mt=unCStr("[]"),_mu=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_1E,_lj,_mt],_mv=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_mu,_3],_mw=function(_mx){return E(_mv);},_my=unCStr("Char"),_mz=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_1E,_lj,_my],_mA=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_mz,_3],_mB=function(_mC){return E(_mA);},_mD=new T(function(){return _2y(_mw,_mB);}),_mE=new T(function(){return A(_mD,[_2x]);}),_mF=new T(function(){return E(_2x);}),_mG=function(_mH){return E(E(_mH)[7]);},_mI=function(_mJ){return E(E(_mJ)[1]);},_mK=[0,0],_mL=[0,32],_mM=[0,10],_mN=function(_mO){var _mP=E(_mO);if(!_mP[0]){return E(_18);}else{var _mQ=_mP[1],_mR=E(_mP[2]);if(!_mR[0]){return _mS(_mM,_mQ);}else{var _mT=new T(function(){return _mN(_mR);}),_mU=new T(function(){return _mS(_mM,_mQ);});return function(_mV){return A(_mU,[[1,_mL,new T(function(){return A(_mT,[_mV]);})]]);};}}},_mW=unCStr("->"),_mX=[1,_mW,_3],_mY=[1,_lj,_mX],_mZ=[1,_1E,_mY],_n0=[0,32],_n1=function(_n2){var _n3=E(_n2);if(!_n3[0]){return [0];}else{var _n4=_n3[1],_n5=E(_n3[2]);return _n5[0]==0?E(_n4):_h(_n4,[1,_n0,new T(function(){return _n1(_n5);})]);}},_n6=new T(function(){return _n1(_mZ);}),_n7=new T(function(){var _n8=_22(_n6);return [0,_n8[1],_n8[2],_1E,_lj,_mW];}),_n9=function(_na,_nb){var _nc=E(_na);return _nc[0]==0?E(_nb):A(_nc[1],[new T(function(){return _n9(_nc[2],_nb);})]);},_nd=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_ne=[1,_1G,_3],_nf=function(_ng){var _nh=E(_ng);if(!_nh[0]){return [0];}else{var _ni=E(_nh[1]);return [1,[0,_ni[1],_ni[2]],new T(function(){return _nf(_nh[2]);})];}},_nj=new T(function(){var _nk=_h(_3,_ne);if(!_nk[0]){return E(_mu);}else{var _nl=_22(new T(function(){return _1Q(_2e(_2p,[1,_nd,new T(function(){return _nf(_nk);})]));}));return E(_mu);}}),_nm=[0,40],_nn=function(_no){return _mS(_mM,_no);},_np=[0,8],_nq=unCStr(" -> "),_nr=[0,9],_ns=[0,93],_nt=[0,91],_nu=[0,41],_nv=[0,44],_nw=function(_no){return [1,_nv,_no];},_nx=function(_ny,_nz){var _nA=E(_nz);return _nA[0]==0?[0]:[1,_ny,[1,_nA[1],new T(function(){return _nx(_ny,_nA[2]);})]];},_mS=function(_nB,_nC){var _nD=E(_nC),_nE=_nD[3],_nF=E(_nD[4]);if(!_nF[0]){return function(_nG){return _h(E(_nE)[5],_nG);};}else{var _nH=_nF[1],_nI=new T(function(){var _nJ=E(_nE)[5],_nK=new T(function(){return _mN(_nF);}),_nL=new T(function(){return E(_nB)[1]<=9?function(_nM){return _h(_nJ,[1,_mL,new T(function(){return A(_nK,[_nM]);})]);}:function(_nN){return [1,_q,new T(function(){return _h(_nJ,[1,_mL,new T(function(){return A(_nK,[[1,_p,_nN]]);})]);})];};}),_nO=E(_nJ);if(!_nO[0]){return E(_nL);}else{if(E(E(_nO[1])[1])==40){var _nP=E(_nO[2]);return _nP[0]==0?E(_nL):E(E(_nP[1])[1])==44?function(_nQ){return [1,_nm,new T(function(){return A(new T(function(){var _nR=_2e(_nn,_nF);if(!_nR[0]){return E(_18);}else{var _nS=new T(function(){return _nx(_nw,_nR[2]);});return function(_9j){return _n9([1,_nR[1],_nS],_9j);};}}),[[1,_nu,_nQ]]);})];}:E(_nL);}else{return E(_nL);}}}),_nT=E(_nF[2]);if(!_nT[0]){var _nU=E(_nE),_nV=E(_nj),_nW=hs_eqWord64(_nU[1],_nV[1]);if(!E(_nW)){return E(_nI);}else{var _nX=hs_eqWord64(_nU[2],_nV[2]);if(!E(_nX)){return E(_nI);}else{var _nY=new T(function(){return _mS(_mK,_nH);});return function(_nZ){return [1,_nt,new T(function(){return A(_nY,[[1,_ns,_nZ]]);})];};}}}else{if(!E(_nT[2])[0]){var _o0=E(_nE),_o1=E(_n7),_o2=hs_eqWord64(_o0[1],_o1[1]);if(!E(_o2)){return E(_nI);}else{var _o3=hs_eqWord64(_o0[2],_o1[2]);if(!E(_o3)){return E(_nI);}else{var _o4=new T(function(){return _mS(_np,_nT[1]);}),_o5=new T(function(){return _mS(_nr,_nH);});return E(_nB)[1]<=8?function(_o6){return A(_o5,[new T(function(){return _h(_nq,new T(function(){return A(_o4,[_o6]);}));})]);}:function(_o7){return [1,_q,new T(function(){return A(_o5,[new T(function(){return _h(_nq,new T(function(){return A(_o4,[[1,_p,_o7]]);}));})]);})];};}}}else{return E(_nI);}}}},_o8=function(_o9,_oa,_ob,_oc){var _od=new T(function(){return _lP(_o9);}),_oe=new T(function(){return _mr(_oc);}),_of=new T(function(){return _mG(_oc);}),_og=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_mS,[_mK,A(_oa,[_mF]),_3]);}));}),_oh=new T(function(){return A(_mI,[_ob,_1]);});return function(_oi){if(!E(new T(function(){var _oj=A(_oa,[_mF]),_ok=E(_mE),_ol=hs_eqWord64(_oj[1],_ok[1]);if(!E(_ol)){return false;}else{var _om=hs_eqWord64(_oj[2],_ok[2]);return E(_om)==0?false:true;}}))){var _on=new T(function(){return A(_od,[[1,_oi,new T(function(){return A(_of,[new T(function(){return A(_oe,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _h(_oi,_og);}));})]);})]);})]]);}),_oo=A(_oh,[_oi]);if(!_oo[0]){return E(_on);}else{var _op=E(_oo[1]);return E(_op[2])[0]==0?E(_oo[2])[0]==0?A(_od,[[2,_op[1]]]):E(_on):E(_on);}}else{return A(_od,[[2,_oi]]);}};},_oq=[0],_or=function(_os,_ot,_ou,_ov,_ow,_ox){var _oy=E(_os),_oz=_oy[1],_oA=new T(function(){return A(_oy[3],[_oq]);}),_oB=new T(function(){return _o8(_oy,_ou,_ov,_ow);});return A(_oz,[new T(function(){return _mo(_ot,_ox);}),function(_oC){var _oD=E(_oC);return _oD[0]==0?E(_oA):A(_oz,[new T(function(){return A(_ot,[function(_){var _oE=jsGet(E(_oD[1])[1],E(_17)[1]);return [1,new T(function(){return fromJSStr(_oE);})];}]);}),function(_oF){var _oG=E(_oF);return _oG[0]==0?E(_oA):A(_oB,[_oG[1]]);}]);}]);},_oH=1,_oI=function(_oJ){return E(E(_oJ)[10]);},_oK=function(_oL,_oM){return A(_lP,[_oL,[0,_oM,_oM]]);},_oN=function(_oO){return E(E(_oO)[2]);},_oP=function(_oQ,_oR,_oS){return A(_lP,[_oQ,[0,_0,_oR]]);},_oT=function(_oU){return E(E(_oU)[2]);},_oV=function(_oW,_oX,_oY,_oZ,_p0){var _p1=new T(function(){return _lW(_oW);}),_p2=new T(function(){return _oN(_p1);}),_p3=new T(function(){return _lY(_oX);}),_p4=new T(function(){return _lR(_p3);}),_p5=new T(function(){return _mc([0,coercionToken],_p4,function(_p6){return _oK(_p3,_p6);},function(_p7,_p8){return _oP(_p3,_p7,_p8);});}),_p9=new T(function(){return _lP(_p3);}),_pa=new T(function(){return _lp(_p3);}),_pb=new T(function(){return _lP(_p3);}),_pc=new T(function(){return _lp(_p3);}),_pd=new T(function(){return _lP(_p3);}),_pe=new T(function(){return _lp(_p3);}),_pf=new T(function(){return _lP(_p3);}),_pg=new T(function(){return _lp(_p3);}),_ph=new T(function(){return _oT(_oZ);}),_pi=new T(function(){return _oI(_oW);});return function(_pj,_pk,_pl){return function(_pm){return A(_pg,[new T(function(){var _pn=E(_pj);return _pn[0]==0?A(_p5,[_pm]):A(_pf,[[0,_pn[1],_pm]]);}),function(_po){var _pp=new T(function(){return E(E(_po)[1]);}),_pq=new T(function(){return _or(_p4,function(_pr){return _m2(_oX,_pr);},_oY,_p0,_oW,_pp);}),_ps=new T(function(){return A(_pi,[_pp,_pk,new T(function(){var _pt=E(_pl);if(!_pt[0]){return [0];}else{var _pu=_pt[1],_pv=_5i(_oY,_mD,_pu);return _pv[0]==0?A(_ph,[_pu]):E(_pv[1]);}}),_bd,_w]);});return A(_pe,[new T(function(){var _pw=new T(function(){return E(E(_po)[2]);});return A(_pd,[[0,_pw,_pw]]);}),function(_px){return A(_pc,[new T(function(){return A(_pb,[[0,_0,new T(function(){var _py=E(E(_px)[1]);return [0,_py[1],_py[2],_oH,_py[4]];})]]);}),function(_pz){return A(_pa,[new T(function(){return A(_pq,[new T(function(){return E(E(_pz)[2]);})]);}),function(_pA){var _pB=E(_pA),_pC=_pB[2],_pD=E(_pB[1]);switch(_pD[0]){case 0:return A(_p9,[[0,[0,_ps,_w],_pC]]);case 1:return A(_p9,[[0,[0,new T(function(){return A(_p2,[new T(function(){return A(_pi,[_pp,_pk,_pD[1],_bd,_w]);}),_pD[2]]);}),_w],_pC]]);default:var _pE=_pD[1];return A(_p9,[[0,[0,new T(function(){return A(_pi,[_pp,_pk,new T(function(){var _pF=_5i(_oY,_mD,_pE);return _pF[0]==0?A(_ph,[_pE]):E(_pF[1]);}),_bd,_w]);}),[1,_pE]],_pC]]);}}]);}]);}]);}]);};};},_pG=new T(function(){return _oV(_4V,_6M,_ln,_li,_l5);}),_pH=new T(function(){return A(_pG,[_w,_1q,_w]);}),_pI=new T(function(){return A(_pG,[_w,_1q,_w]);}),_pJ=function(_pK,_){var _pL=A(_pI,[_pK,_]),_pM=E(_pL),_pN=_pM[2],_pO=E(_pM[1]),_pP=A(_pH,[_pN,_]),_pQ=E(_pP),_pR=_pQ[2],_pS=E(_pQ[1]),_pT=new T(function(){return E(E(_pN)[4]);}),_pU=new T(function(){return E(E(_pK)[4]);}),_pV=function(_pW,_){var _pX=_1a(_pO[1],_b,_pU,_pW,_),_pY=_1r(_1n,_pW,_),_pZ=_1a(_pS[1],_b,_pT,_pW,_),_q0=_1r(_1n,_pW,_);return _pW;},_q1=E(_pO[2]);if(!_q1[0]){return [0,[0,_pV,_w],_pR];}else{var _q2=E(_pS[2]);if(!_q2[0]){return [0,[0,_pV,_w],_pR];}else{var _q3=new T(function(){return _r(0,E(_q1[1])[1]+E(_q2[1])[1]|0,_3);});return [0,[0,function(_q4,_){var _q5=_pV(_q4,_),_q6=_1r(_1p,_q4,_),_q7=_c(_q3,_q6,_);return _q4;},_w],_pR];}}},_q8=unCStr("idelem"),_q9=unCStr(" could be found!"),_qa=function(_qb){return err(unAppCStr("No element with ID ",new T(function(){return _h(_qb,_q9);})));},_qc=function(_){var _qd=E(_q8),_qe=jsFind(toJSStr(_qd)),_qf=E(_qe);if(!_qf[0]){return _qa(_qd);}else{var _qg=_4(_pJ,_qf[1],_);return _0;}},_qh=function(_){return _qc(_);};
var hasteMain = function() {A(_qh, [0]);};window.onload = hasteMain;