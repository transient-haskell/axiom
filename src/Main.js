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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=function(_q,_r,_){var _s=jsCreateTextNode(toJSStr(E(_q))),_t=jsAppendChild(_s,E(_r)[1]);return [0,_s];},_u=[0,98],_v=[1,_u,_9],_w=function(_x,_y){var _z=new T(function(){return A(_x,[_y]);});return function(_A,_){var _B=jsCreateElem(toJSStr(_v)),_C=jsAppendChild(_B,E(_A)[1]),_D=[0,_B],_E=A(_z,[_D,_]);return _D;};},_F=unCStr("bottom of the page"),_G=new T(function(){return _w(_p,_F);}),_H=unCStr("text-align:center"),_I=unCStr("id"),_J=function(_K,_L,_M,_){var _N=E(_L),_O=A(_K,[_M,_]),_P=A(_1,[_n,_O,_N[1],_N[2],_]);return _O;},_Q=function(_R,_S){while(1){var _T=(function(_U,_V){var _W=E(_V);if(!_W[0]){return E(_U);}else{_R=function(_X,_){return _J(_U,_W[1],_X,_);};_S=_W[2];return null;}})(_R,_S);if(_T!=null){return _T;}}},_Y=unCStr("span"),_Z=function(_10,_11,_){var _12=jsCreateElem(toJSStr(E(_10))),_13=jsAppendChild(_12,E(_11)[1]);return [0,_12];},_14=function(_X,_){return _Z(_Y,_X,_);},_15=function(_16,_17,_){return [0,_0,_16];},_18=function(_19,_){return [0,_19,_19];},_1a=[0,coercionToken],_1b=function(_1c,_1d,_){var _1e=A(_1c,[_]);return A(_1d,[_]);},_1f=function(_1g,_1h,_){return _1b(_1g,_1h,_);},_1i=function(_1j,_1k,_){var _1l=A(_1j,[_]);return A(_1k,[_1l,_]);},_1m=unCStr("base"),_1n=unCStr("GHC.IO.Exception"),_1o=unCStr("IOException"),_1p=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1m,_1n,_1o],_1q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1p,_9],_1r=function(_1s){return E(_1q);},_1t=function(_1u){return E(E(_1u)[1]);},_1v=unCStr("Maybe.fromJust: Nothing"),_1w=new T(function(){return err(_1v);}),_1x=function(_1y,_1z,_1A){var _1B=new T(function(){var _1C=A(_1y,[_1A]),_1D=A(_1z,[new T(function(){var _1E=E(_1B);return _1E[0]==0?E(_1w):E(_1E[1]);})]),_1F=hs_eqWord64(_1C[1],_1D[1]);if(!E(_1F)){return [0];}else{var _1G=hs_eqWord64(_1C[2],_1D[2]);return E(_1G)==0?[0]:[1,_1A];}});return E(_1B);},_1H=function(_1I){var _1J=E(_1I);return _1x(_1t(_1J[1]),_1r,_1J[2]);},_1K=unCStr(": "),_1L=[0,41],_1M=unCStr(" ("),_1N=function(_1O,_1P){var _1Q=E(_1O);return _1Q[0]==0?E(_1P):[1,_1Q[1],new T(function(){return _1N(_1Q[2],_1P);})];},_1R=unCStr("already exists"),_1S=unCStr("does not exist"),_1T=unCStr("protocol error"),_1U=unCStr("failed"),_1V=unCStr("invalid argument"),_1W=unCStr("inappropriate type"),_1X=unCStr("hardware fault"),_1Y=unCStr("unsupported operation"),_1Z=unCStr("timeout"),_20=unCStr("resource vanished"),_21=unCStr("interrupted"),_22=unCStr("resource busy"),_23=unCStr("resource exhausted"),_24=unCStr("end of file"),_25=unCStr("illegal operation"),_26=unCStr("permission denied"),_27=unCStr("user error"),_28=unCStr("unsatisified constraints"),_29=unCStr("system error"),_2a=function(_2b,_2c){switch(E(_2b)){case 0:return _1N(_1R,_2c);case 1:return _1N(_1S,_2c);case 2:return _1N(_22,_2c);case 3:return _1N(_23,_2c);case 4:return _1N(_24,_2c);case 5:return _1N(_25,_2c);case 6:return _1N(_26,_2c);case 7:return _1N(_27,_2c);case 8:return _1N(_28,_2c);case 9:return _1N(_29,_2c);case 10:return _1N(_1T,_2c);case 11:return _1N(_1U,_2c);case 12:return _1N(_1V,_2c);case 13:return _1N(_1W,_2c);case 14:return _1N(_1X,_2c);case 15:return _1N(_1Y,_2c);case 16:return _1N(_1Z,_2c);case 17:return _1N(_20,_2c);default:return _1N(_21,_2c);}},_2d=[0,125],_2e=unCStr("{handle: "),_2f=function(_2g,_2h,_2i,_2j,_2k,_2l){var _2m=new T(function(){var _2n=new T(function(){return _2a(_2h,new T(function(){var _2o=E(_2j);return _2o[0]==0?E(_2l):_1N(_1M,new T(function(){return _1N(_2o,[1,_1L,_2l]);}));}));}),_2p=E(_2i);return _2p[0]==0?E(_2n):_1N(_2p,new T(function(){return _1N(_1K,_2n);}));}),_2q=E(_2k);if(!_2q[0]){var _2r=E(_2g);if(!_2r[0]){return E(_2m);}else{var _2s=E(_2r[1]);return _2s[0]==0?_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);})):_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);}));}}else{return _1N(_2q[1],new T(function(){return _1N(_1K,_2m);}));}},_2t=function(_2u){var _2v=E(_2u);return _2f(_2v[1],_2v[2],_2v[3],_2v[4],_2v[6],_9);},_2w=function(_2x,_2y){var _2z=E(_2x);return _2f(_2z[1],_2z[2],_2z[3],_2z[4],_2z[6],_2y);},_2A=[0,44],_2B=[0,93],_2C=[0,91],_2D=function(_2E,_2F,_2G){var _2H=E(_2F);return _2H[0]==0?unAppCStr("[]",_2G):[1,_2C,new T(function(){return A(_2E,[_2H[1],new T(function(){var _2I=function(_2J){var _2K=E(_2J);return _2K[0]==0?E([1,_2B,_2G]):[1,_2A,new T(function(){return A(_2E,[_2K[1],new T(function(){return _2I(_2K[2]);})]);})];};return _2I(_2H[2]);})]);})];},_2L=function(_2M,_2N){return _2D(_2w,_2M,_2N);},_2O=function(_2P,_2Q,_2R){var _2S=E(_2Q);return _2f(_2S[1],_2S[2],_2S[3],_2S[4],_2S[6],_2R);},_2T=[0,_2O,_2t,_2L],_2U=new T(function(){return [0,_1r,_2T,_2V,_1H];}),_2V=function(_2W){return [0,_2U,_2W];},_2X=7,_2Y=function(_2Z){return [0,_a,_2X,_9,_2Z,_a,_a];},_30=function(_31,_){return die(new T(function(){return _2V(new T(function(){return _2Y(_31);}));}));},_32=function(_33,_){return _30(_33,_);},_34=function(_35,_){return _35;},_36=[0,_1i,_1f,_34,_32],_37=function(_38){return E(E(_38)[1]);},_39=function(_3a,_3b,_3c,_3d){return A(_37,[_3a,new T(function(){return A(_3b,[_3d]);}),function(_3e){return A(_3c,[new T(function(){return E(E(_3e)[1]);}),new T(function(){return E(E(_3e)[2]);})]);}]);},_3f=function(_3g,_3h,_3i,_3j){return A(_37,[_3g,new T(function(){return A(_3h,[_3j]);}),function(_3k){return A(_3i,[new T(function(){return E(E(_3k)[2]);})]);}]);},_3l=function(_3m,_3n,_3o,_3p){return _3f(_3m,_3n,_3o,_3p);},_3q=function(_3r){return E(E(_3r)[4]);},_3s=function(_3t,_3u){var _3v=new T(function(){return A(_3q,[_3t,_3u]);});return function(_3w){return E(_3v);};},_3x=function(_3y){return E(E(_3y)[3]);},_3z=function(_3A){var _3B=new T(function(){return _3x(_3A);});return [0,function(_3n,_3o,_3p){return _39(_3A,_3n,_3o,_3p);},function(_3n,_3o,_3p){return _3l(_3A,_3n,_3o,_3p);},function(_3C,_3D){return A(_3B,[[0,_3C,_3D]]);},function(_3p){return _3s(_3A,_3p);}];},_3E=new T(function(){return _3z(_36);}),_3F=[0,112],_3G=function(_3H,_3I){var _3J=jsShowI(_3H);return _1N(fromJSStr(_3J),_3I);},_3K=[0,41],_3L=[0,40],_3M=function(_3N,_3O,_3P){return _3O>=0?_3G(_3O,_3P):_3N<=6?_3G(_3O,_3P):[1,_3L,new T(function(){var _3Q=jsShowI(_3O);return _1N(fromJSStr(_3Q),[1,_3K,_3P]);})];},_3R=function(_3S,_3T,_3U,_3V){var _3W=E(_3T);return A(_3W[1],[new T(function(){var _3X=E(_3S);return E(_3U);}),function(_3Y){var _3Z=new T(function(){return E(E(_3Y)[2]);});return A(_3W[2],[new T(function(){return A(_3V,[new T(function(){var _40=E(new T(function(){var _41=E(_3S);return [0,coercionToken];})),_42=E(_3Y);return [0,_42[1],new T(function(){return [0,E(_3Z)[1]+1|0];}),_42[3],_42[4],_42[5]];})]);}),new T(function(){return A(_3W[3],[[1,_3F,new T(function(){return _1N(_3M(0,E(_3Z)[1],_9),new T(function(){return E(E(_3Y)[1]);}));})]]);})]);}]);},_43=new T(function(){return _3R(_1a,_3E,_18,_15);}),_44=unCStr(" could be found!"),_45=function(_46){return err(unAppCStr("No element with ID ",new T(function(){return _1N(_46,_44);})));},_47=function(_48,_49,_){var _4a=E(_49),_4b=jsFind(toJSStr(_4a)),_4c=E(_4b);if(!_4c[0]){return _45(_4a);}else{var _4d=E(_4c[1]),_4e=jsClearChildren(_4d[1]),_4f=E(_m)[1],_4g=takeMVar(_4f),_4h=A(_48,[_4g,_]),_4i=E(_4h),_4j=E(_4i[1]),_=putMVar(_4f,_4i[2]),_4k=A(_4j[1],[_4d,_]);return _4j[2];}},_4l=function(_4m,_4n,_4o,_4p,_4q,_4r,_4s,_4t,_){var _4u=E(_4s);return [0,_4u,[0,_4p,_4q,_4r,[0,function(_){return _47(function(_4v,_){var _4w=A(_4m,[new T(function(){var _4x=E(_4v);return [0,_4x[1],_4q,_4x[3],_4x[4],_4x[5]];}),_]);return [0,[0,_34,E(E(_4w)[1])[2]],_4v];},_4o,_);},function(_4y,_){var _4z=_47(new T(function(){return A(_4n,[_4y]);}),_4o,_),_4A=E(_4z);return _4A[0]==0?_a:A(_4u[2],[_4A[1],_]);}],_4t]];},_4B=function(_4C,_4D,_4E,_){var _4F=A(_43,[_4E,_]),_4G=E(_4F),_4H=_4G[1],_4I=E(_4G[2]),_4J=_4l(_4C,_4D,_4H,_4I[1],_4I[2],_4I[3],_4I[4],_4I[5],_),_4K=A(_4C,[new T(function(){return E(E(_4J)[2]);}),_]),_4L=E(_4K),_4M=_4L[2],_4N=E(_4L[1]),_4O=_4N[1],_4P=new T(function(){return _Q(_14,[1,[0,_I,_4H],_9]);}),_4Q=E(_4N[2]);if(!_4Q[0]){return [0,[0,function(_4R,_){var _4S=A(_4O,[_4R,_]),_4T=A(_4P,[_4R,_]);return _4R;},_a],new T(function(){var _4U=E(_4M);return [0,_4U[1],_4U[2],_4U[3],new T(function(){return E(E(_4J)[1]);}),_4U[5]];})];}else{var _4V=A(_4D,[_4Q[1],new T(function(){var _4W=E(_4M);return [0,_4W[1],_4W[2],_4W[3],new T(function(){return E(E(_4J)[1]);}),_4W[5]];}),_]),_4X=E(_4V),_4Y=E(_4X[1]);return [0,[0,function(_4Z,_){var _50=A(_4O,[_4Z,_]),_51=A(_4P,[_4Z,_]),_52=A(_4Y[1],[_51,_]);return _4Z;},_4Y[2]],_4X[2]];}},_53=unCStr("style"),_54=unCStr("vertical-align:top"),_55=[0,3],_56=[0,112],_57=[1,_56,_9],_58=function(_59,_5a){var _5b=new T(function(){return A(_59,[_5a]);});return function(_5c,_){var _5d=jsCreateElem(toJSStr(_57)),_5e=jsAppendChild(_5d,E(_5c)[1]),_5f=[0,_5d],_5g=A(_5b,[_5f,_]);return _5f;};},_5h=function(_5i){return _3M(0,E(_5i)[1],_9);},_5j=unCStr("br"),_5k=function(_5l,_){var _5m=jsCreateElem(toJSStr(E(_5j))),_5n=jsAppendChild(_5m,E(_5l)[1]);return [0,_5m];},_5o=[1,_0],_5p=unCStr("result: "),_5q=function(_5r){var _5s=new T(function(){return _w(_p,new T(function(){return _5h(_5r);}));});return function(_5t,_){return [0,[0,function(_5u,_){var _5v=_5k(_5u,_),_5w=_p(_5p,_5u,_),_5x=A(_5s,[_5u,_]);return _5u;},_5o],_5t];};},_5y=unCStr(" numbers and append the result using a fold"),_5z=[0,0],_5A=[1,_5z],_5B=[0,_34,_5A],_5C=function(_5D,_){return [0,_5B,_5D];},_5E=function(_5F,_5G,_5H,_){var _5I=_Z(_5F,_5H,_),_5J=A(_5G,[_5I,_]);return _5I;},_5K=unCStr("()"),_5L=unCStr("GHC.Tuple"),_5M=unCStr("ghc-prim"),_5N=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5M,_5L,_5K],_5O=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5N,_9],_5P=function(_5Q){return E(_5O);},_5R=unCStr("main"),_5S=unCStr("Haste.Perch"),_5T=unCStr("PerchM"),_5U=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5R,_5S,_5T],_5V=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5U,_9],_5W=function(_5X){return E(_5V);},_5Y=function(_5Z){var _60=E(_5Z);return _60[0]==0?[0]:_1N(_60[1],new T(function(){return _5Y(_60[2]);}));},_61=function(_62,_63){var _64=E(_62);if(!_64){return [0,_9,_63];}else{var _65=E(_63);if(!_65[0]){return [0,_9,_9];}else{var _66=new T(function(){var _67=_61(_64-1|0,_65[2]);return [0,_67[1],_67[2]];});return [0,[1,_65[1],new T(function(){return E(E(_66)[1]);})],new T(function(){return E(E(_66)[2]);})];}}},_68=[0,120],_69=[0,48],_6a=function(_6b){var _6c=new T(function(){var _6d=_61(8,new T(function(){var _6e=md5(toJSStr(E(_6b)));return fromJSStr(_6e);}));return [0,_6d[1],_6d[2]];}),_6f=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6c)[1]);})]])]),_6g=new T(function(){var _6h=_61(8,new T(function(){return E(E(_6c)[2]);}));return [0,_6h[1],_6h[2]];}),_6i=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6g)[1]);})]])]),_6j=hs_mkWord64(_6f,_6i),_6k=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(_61(8,new T(function(){return E(E(_6g)[2]);}))[1]);})]])]),_6l=hs_mkWord64(_6k,_6k);return [0,_6j,_6l];},_6m=function(_6n,_6o){var _6p=E(_6o);return _6p[0]==0?[0]:[1,new T(function(){return A(_6n,[_6p[1]]);}),new T(function(){return _6m(_6n,_6p[2]);})];},_6q=function(_6r,_6s){var _6t=jsShowI(_6r),_6u=md5(_6t);return _1N(fromJSStr(_6u),new T(function(){var _6v=jsShowI(_6s),_6w=md5(_6v);return fromJSStr(_6w);}));},_6x=function(_6y){var _6z=E(_6y);return _6q(_6z[1],_6z[2]);},_6A=function(_6B){var _6C=E(_6B);if(!_6C[0]){return [0];}else{var _6D=E(_6C[1]);return [1,[0,_6D[1],_6D[2]],new T(function(){return _6A(_6C[2]);})];}},_6E=unCStr("Prelude.undefined"),_6F=new T(function(){return err(_6E);}),_6G=function(_6H,_6I){return function(_6J){return E(new T(function(){var _6K=A(_6H,[_6F]),_6L=E(_6K[3]),_6M=_6L[1],_6N=_6L[2],_6O=_1N(_6K[4],[1,new T(function(){return A(_6I,[_6F]);}),_9]);if(!_6O[0]){return [0,_6M,_6N,_6L,_9];}else{var _6P=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_6M,_6N],new T(function(){return _6A(_6O);})]));}));return [0,_6P[1],_6P[2],_6L,_6O];}}));};},_6Q=new T(function(){return _6G(_5W,_5P);}),_6R=unCStr("value"),_6S=unCStr("onclick"),_6T=unCStr("checked"),_6U=[0,_6T,_9],_6V=[1,_6U,_9],_6W=unCStr("type"),_6X=unCStr("input"),_6Y=function(_6Z,_){return _Z(_6X,_6Z,_);},_70=function(_71,_72,_73,_74,_75){var _76=new T(function(){var _77=new T(function(){return _Q(_6Y,[1,[0,_6W,_72],[1,[0,_I,_71],[1,[0,_6R,_73],_9]]]);});return !E(_74)?E(_77):_Q(_77,_6V);}),_78=E(_75);return _78[0]==0?E(_76):_Q(_76,[1,[0,_6S,_78[1]],_9]);},_79=unCStr("href"),_7a=[0,97],_7b=[1,_7a,_9],_7c=function(_7d,_){return _Z(_7b,_7d,_);},_7e=function(_7f,_7g){var _7h=new T(function(){return _Q(_7c,[1,[0,_79,_7f],_9]);});return function(_7i,_){var _7j=A(_7h,[_7i,_]),_7k=A(_7g,[_7j,_]);return _7j;};},_7l=function(_7m){return _7e(_7m,function(_X,_){return _p(_7m,_X,_);});},_7n=unCStr("option"),_7o=function(_7p,_){return _Z(_7n,_7p,_);},_7q=unCStr("selected"),_7r=[0,_7q,_9],_7s=[1,_7r,_9],_7t=function(_7u,_7v,_7w){var _7x=new T(function(){return _Q(_7o,[1,[0,_6R,_7u],_9]);}),_7y=function(_7z,_){var _7A=A(_7x,[_7z,_]),_7B=A(_7v,[_7A,_]);return _7A;};return !E(_7w)?E(_7y):_Q(_7y,_7s);},_7C=function(_7D,_7E){return _7t(_7D,function(_X,_){return _p(_7D,_X,_);},_7E);},_7F=unCStr("method"),_7G=unCStr("action"),_7H=unCStr("UTF-8"),_7I=unCStr("acceptCharset"),_7J=[0,_7I,_7H],_7K=unCStr("form"),_7L=function(_7M,_){return _Z(_7K,_7M,_);},_7N=function(_7O,_7P,_7Q){var _7R=new T(function(){return _Q(_7L,[1,_7J,[1,[0,_7G,_7O],[1,[0,_7F,_7P],_9]]]);});return function(_7S,_){var _7T=A(_7R,[_7S,_]),_7U=A(_7Q,[_7T,_]);return _7T;};},_7V=unCStr("select"),_7W=function(_7X,_){return _Z(_7V,_7X,_);},_7Y=function(_7Z,_80){var _81=new T(function(){return _Q(_7W,[1,[0,_I,_7Z],_9]);});return function(_82,_){var _83=A(_81,[_82,_]),_84=A(_80,[_83,_]);return _83;};},_85=unCStr("textarea"),_86=function(_87,_){return _Z(_85,_87,_);},_88=function(_89,_8a){var _8b=new T(function(){return _Q(_86,[1,[0,_I,_89],_9]);});return function(_8c,_){var _8d=A(_8b,[_8c,_]),_8e=_p(_8a,_8d,_);return _8d;};},_8f=unCStr("color:red"),_8g=unCStr("style"),_8h=[0,_8g,_8f],_8i=[1,_8h,_9],_8j=[0,98],_8k=[1,_8j,_9],_8l=function(_8m){return _Q(function(_8n,_){var _8o=_Z(_8k,_8n,_),_8p=A(_8m,[_8o,_]);return _8o;},_8i);},_8q=function(_8r,_8s,_){var _8t=E(_8r);if(!_8t[0]){return _8s;}else{var _8u=A(_8t[1],[_8s,_]),_8v=_8q(_8t[2],_8s,_);return _8s;}},_8w=function(_8x,_8y,_8z,_){var _8A=A(_8x,[_8z,_]),_8B=A(_8y,[_8z,_]);return _8z;},_8C=[0,_34,_8w,_8q],_8D=[0,_8C,_6Q,_p,_p,_5E,_8l,_7e,_7l,_70,_88,_7Y,_7t,_7C,_7N,_Q],_8E=function(_8F,_8G,_){var _8H=A(_8G,[_]);return _8F;},_8I=function(_8J,_8K,_){var _8L=A(_8K,[_]);return new T(function(){return A(_8J,[_8L]);});},_8M=[0,_8I,_8E],_8N=function(_8O){var _8P=E(_8O);return _8P[0]==0?0:E(_8P[1])[1]+_8N(_8P[2])|0;},_8Q=function(_8R){return [0,_8N(_8R)];},_8S=function(_8T,_8U){return [0,E(_8T)[1]+E(_8U)[1]|0];},_8V=[0,_5z,_8S,_8Q],_8W=function(_8X,_8Y){var _8Z=E(_8Y);return _8Z[0]==0?[0]:[1,new T(function(){return A(_8X,[_8Z[1]]);})];},_90=function(_91){return E(E(_91)[1]);},_92=function(_93){return E(E(_93)[2]);},_94=function(_95,_96,_97,_98,_99,_9a){var _9b=new T(function(){return _92(_95);});return A(_96,[new T(function(){return A(_98,[_9a]);}),function(_9c){var _9d=E(_9c),_9e=E(_9d[1]);return A(_96,[new T(function(){return A(_99,[_9d[2]]);}),function(_9f){var _9g=E(_9f),_9h=E(_9g[1]);return A(_97,[[0,[0,new T(function(){return A(_9b,[_9e[1],_9h[1]]);}),new T(function(){var _9i=E(_9e[2]);if(!_9i[0]){return [0];}else{var _9j=E(_9h[2]);return _9j[0]==0?[0]:[1,new T(function(){return A(_9i[1],[_9j[1]]);})];}})],_9g[2]]]);}]);}]);},_9k=function(_9l){return E(E(_9l)[1]);},_9m=function(_9n,_9o,_9p,_9q,_9r,_9s){var _9t=new T(function(){return _90(_9n);});return function(_9u){var _9v=E(_9o);return _94(_9t,_9v[1],_9v[3],function(_9w){return A(new T(function(){var _9x=new T(function(){return _92(_9q);});return A(_9k,[_9p,function(_9y){return [0,new T(function(){var _9z=E(E(_9y)[1]);return [0,_9z[1],new T(function(){return _8W(_9x,_9z[2]);})];}),new T(function(){return E(E(_9y)[2]);})];}]);}),[new T(function(){return A(_9r,[_9w]);})]);},_9s,_9u);};},_9A=function(_9B,_9C){while(1){var _9D=(function(_9E,_9F){var _9G=E(_9F);if(!_9G[0]){return E(_9E);}else{_9B=new T(function(){return _9m(_8D,_36,_8M,_8V,_9E,_9G[1]);});_9C=_9G[2];return null;}})(_9B,_9C);if(_9D!=null){return _9D;}}},_9H=[13,coercionToken],_9I=unCStr("text"),_9J=[0,_36,_n],_9K=unCStr("base"),_9L=unCStr("Control.Exception.Base"),_9M=unCStr("PatternMatchFail"),_9N=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9K,_9L,_9M],_9O=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9N,_9],_9P=function(_9Q){return E(_9O);},_9R=function(_9S){var _9T=E(_9S);return _1x(_1t(_9T[1]),_9P,_9T[2]);},_9U=function(_9V){return E(E(_9V)[1]);},_9W=function(_9X,_9Y){return _1N(E(_9X)[1],_9Y);},_9Z=function(_a0,_a1){return _2D(_9W,_a0,_a1);},_a2=function(_a3,_a4,_a5){return _1N(E(_a4)[1],_a5);},_a6=[0,_a2,_9U,_9Z],_a7=new T(function(){return [0,_9P,_a6,_a8,_9R];}),_a8=function(_a9){return [0,_a7,_a9];},_aa=unCStr("Non-exhaustive patterns in"),_ab=function(_ac,_ad){return die(new T(function(){return A(_ad,[_ac]);}));},_ae=function(_af,_ag){var _ah=E(_ag);if(!_ah[0]){return [0,_9,_9];}else{var _ai=_ah[1];if(!A(_af,[_ai])){return [0,_9,_ah];}else{var _aj=new T(function(){var _ak=_ae(_af,_ah[2]);return [0,_ak[1],_ak[2]];});return [0,[1,_ai,new T(function(){return E(E(_aj)[1]);})],new T(function(){return E(E(_aj)[2]);})];}}},_al=[0,32],_am=[0,10],_an=[1,_am,_9],_ao=function(_ap){return E(E(_ap)[1])==124?false:true;},_aq=function(_ar,_as){var _at=_ae(_ao,unCStr(_ar)),_au=_at[1],_av=function(_aw,_ax){return _1N(_aw,new T(function(){return unAppCStr(": ",new T(function(){return _1N(_as,new T(function(){return _1N(_ax,_an);}));}));}));},_ay=E(_at[2]);return _ay[0]==0?_av(_au,_9):E(E(_ay[1])[1])==124?_av(_au,[1,_al,_ay[2]]):_av(_au,_9);},_az=function(_aA){return _ab([0,new T(function(){return _aq(_aA,_aa);})],_a8);},_aB=new T(function(){return _az("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aC=function(_aD,_aE){while(1){var _aF=(function(_aG,_aH){var _aI=E(_aG);switch(_aI[0]){case 0:var _aJ=E(_aH);if(!_aJ[0]){return [0];}else{_aD=A(_aI[1],[_aJ[1]]);_aE=_aJ[2];return null;}break;case 1:var _aK=A(_aI[1],[_aH]),_aL=_aH;_aD=_aK;_aE=_aL;return null;case 2:return [0];case 3:return [1,[0,_aI[1],_aH],new T(function(){return _aC(_aI[2],_aH);})];default:return E(_aI[1]);}})(_aD,_aE);if(_aF!=null){return _aF;}}},_aM=function(_aN,_aO){var _aP=new T(function(){var _aQ=E(_aO);if(_aQ[0]==3){return [3,_aQ[1],new T(function(){return _aM(_aN,_aQ[2]);})];}else{var _aR=E(_aN);if(_aR[0]==2){return E(_aQ);}else{var _aS=E(_aQ);if(_aS[0]==2){return E(_aR);}else{var _aT=new T(function(){var _aU=E(_aS);if(_aU[0]==4){return [1,function(_aV){return [4,new T(function(){return _1N(_aC(_aR,_aV),_aU[1]);})];}];}else{var _aW=E(_aR);if(_aW[0]==1){var _aX=_aW[1],_aY=E(_aU);return _aY[0]==0?[1,function(_aZ){return _aM(A(_aX,[_aZ]),_aY);}]:[1,function(_b0){return _aM(A(_aX,[_b0]),new T(function(){return A(_aY[1],[_b0]);}));}];}else{var _b1=E(_aU);return _b1[0]==0?E(_aB):[1,function(_b2){return _aM(_aW,new T(function(){return A(_b1[1],[_b2]);}));}];}}}),_b3=E(_aR);switch(_b3[0]){case 1:var _b4=E(_aS);return _b4[0]==4?[1,function(_b5){return [4,new T(function(){return _1N(_aC(A(_b3[1],[_b5]),_b5),_b4[1]);})];}]:E(_aT);case 4:var _b6=_b3[1],_b7=E(_aS);switch(_b7[0]){case 0:return [1,function(_b8){return [4,new T(function(){return _1N(_b6,new T(function(){return _aC(_b7,_b8);}));})];}];case 1:return [1,function(_b9){return [4,new T(function(){return _1N(_b6,new T(function(){return _aC(A(_b7[1],[_b9]),_b9);}));})];}];default:return [4,new T(function(){return _1N(_b6,_b7[1]);})];}break;default:return E(_aT);}}}}}),_ba=E(_aN);switch(_ba[0]){case 0:var _bb=E(_aO);return _bb[0]==0?[0,function(_bc){return _aM(A(_ba[1],[_bc]),new T(function(){return A(_bb[1],[_bc]);}));}]:E(_aP);case 3:return [3,_ba[1],new T(function(){return _aM(_ba[2],_aO);})];default:return E(_aP);}},_bd=function(_be,_bf){return E(_be)[1]!=E(_bf)[1];},_bg=function(_bh,_bi){return E(_bh)[1]==E(_bi)[1];},_bj=[0,_bg,_bd],_bk=function(_bl){return E(E(_bl)[1]);},_bm=function(_bn,_bo,_bp){while(1){var _bq=E(_bo);if(!_bq[0]){return E(_bp)[0]==0?true:false;}else{var _br=E(_bp);if(!_br[0]){return false;}else{if(!A(_bk,[_bn,_bq[1],_br[1]])){return false;}else{_bo=_bq[2];_bp=_br[2];continue;}}}}},_bs=function(_bt,_bu,_bv){return !_bm(_bt,_bu,_bv)?true:false;},_bw=function(_bx){return [0,function(_by,_bz){return _bm(_bx,_by,_bz);},function(_by,_bz){return _bs(_bx,_by,_bz);}];},_bA=new T(function(){return _bw(_bj);}),_bB=function(_bC,_bD){var _bE=E(_bC);switch(_bE[0]){case 0:return [0,function(_bF){return _bB(A(_bE[1],[_bF]),_bD);}];case 1:return [1,function(_bG){return _bB(A(_bE[1],[_bG]),_bD);}];case 2:return [2];case 3:return _aM(A(_bD,[_bE[1]]),new T(function(){return _bB(_bE[2],_bD);}));default:var _bH=function(_bI){var _bJ=E(_bI);if(!_bJ[0]){return [0];}else{var _bK=E(_bJ[1]);return _1N(_aC(A(_bD,[_bK[1]]),_bK[2]),new T(function(){return _bH(_bJ[2]);}));}},_bL=_bH(_bE[1]);return _bL[0]==0?[2]:[4,_bL];}},_bM=[2],_bN=function(_bO){return [3,_bO,_bM];},_bP=function(_bQ,_bR){var _bS=E(_bQ);if(!_bS){return A(_bR,[_0]);}else{var _bT=new T(function(){return _bP(_bS-1|0,_bR);});return [0,function(_bU){return E(_bT);}];}},_bV=function(_bW,_bX,_bY){var _bZ=new T(function(){return A(_bW,[_bN]);});return [1,function(_c0){return A(function(_c1,_c2,_c3){while(1){var _c4=(function(_c5,_c6,_c7){var _c8=E(_c5);switch(_c8[0]){case 0:var _c9=E(_c6);if(!_c9[0]){return E(_bX);}else{_c1=A(_c8[1],[_c9[1]]);_c2=_c9[2];var _ca=_c7+1|0;_c3=_ca;return null;}break;case 1:var _cb=A(_c8[1],[_c6]),_cc=_c6,_ca=_c7;_c1=_cb;_c2=_cc;_c3=_ca;return null;case 2:return E(_bX);case 3:return function(_cd){var _ce=new T(function(){return _bB(_c8,_cd);});return _bP(_c7,function(_cf){return E(_ce);});};default:return function(_cg){return _bB(_c8,_cg);};}})(_c1,_c2,_c3);if(_c4!=null){return _c4;}}},[_bZ,_c0,0,_bY]);}];},_ch=[6],_ci=unCStr("valDig: Bad base"),_cj=new T(function(){return err(_ci);}),_ck=function(_cl,_cm){var _cn=function(_co,_cp){var _cq=E(_co);if(!_cq[0]){var _cr=new T(function(){return A(_cp,[_9]);});return function(_cs){return A(_cs,[_cr]);};}else{var _ct=E(_cq[1])[1],_cu=function(_cv){var _cw=new T(function(){return _cn(_cq[2],function(_cx){return A(_cp,[[1,_cv,_cx]]);});});return function(_cy){var _cz=new T(function(){return A(_cw,[_cy]);});return [0,function(_cA){return E(_cz);}];};};switch(E(E(_cl)[1])){case 8:if(48>_ct){var _cB=new T(function(){return A(_cp,[_9]);});return function(_cC){return A(_cC,[_cB]);};}else{if(_ct>55){var _cD=new T(function(){return A(_cp,[_9]);});return function(_cE){return A(_cE,[_cD]);};}else{return _cu([0,_ct-48|0]);}}break;case 10:if(48>_ct){var _cF=new T(function(){return A(_cp,[_9]);});return function(_cG){return A(_cG,[_cF]);};}else{if(_ct>57){var _cH=new T(function(){return A(_cp,[_9]);});return function(_cI){return A(_cI,[_cH]);};}else{return _cu([0,_ct-48|0]);}}break;case 16:var _cJ=new T(function(){return 97>_ct?65>_ct?[0]:_ct>70?[0]:[1,[0,(_ct-65|0)+10|0]]:_ct>102?65>_ct?[0]:_ct>70?[0]:[1,[0,(_ct-65|0)+10|0]]:[1,[0,(_ct-97|0)+10|0]];});if(48>_ct){var _cK=E(_cJ);if(!_cK[0]){var _cL=new T(function(){return A(_cp,[_9]);});return function(_cM){return A(_cM,[_cL]);};}else{return _cu(_cK[1]);}}else{if(_ct>57){var _cN=E(_cJ);if(!_cN[0]){var _cO=new T(function(){return A(_cp,[_9]);});return function(_cP){return A(_cP,[_cO]);};}else{return _cu(_cN[1]);}}else{return _cu([0,_ct-48|0]);}}break;default:return E(_cj);}}};return [1,function(_cQ){return A(_cn,[_cQ,_n,function(_cR){var _cS=E(_cR);return _cS[0]==0?[2]:A(_cm,[_cS]);}]);}];},_cT=[0,10],_cU=[0,1],_cV=[0,2147483647],_cW=function(_cX,_cY){while(1){var _cZ=E(_cX);if(!_cZ[0]){var _d0=_cZ[1],_d1=E(_cY);if(!_d1[0]){var _d2=_d1[1],_d3=addC(_d0,_d2);if(!E(_d3[2])){return [0,_d3[1]];}else{_cX=[1,I_fromInt(_d0)];_cY=[1,I_fromInt(_d2)];continue;}}else{_cX=[1,I_fromInt(_d0)];_cY=_d1;continue;}}else{var _d4=E(_cY);if(!_d4[0]){_cX=_cZ;_cY=[1,I_fromInt(_d4[1])];continue;}else{return [1,I_add(_cZ[1],_d4[1])];}}}},_d5=new T(function(){return _cW(_cV,_cU);}),_d6=function(_d7){var _d8=E(_d7);if(!_d8[0]){var _d9=E(_d8[1]);return _d9==(-2147483648)?E(_d5):[0, -_d9];}else{return [1,I_negate(_d8[1])];}},_da=[0,10],_db=[0,0],_dc=function(_dd,_de){while(1){var _df=E(_dd);if(!_df[0]){var _dg=_df[1],_dh=E(_de);if(!_dh[0]){var _di=_dh[1];if(!(imul(_dg,_di)|0)){return [0,imul(_dg,_di)|0];}else{_dd=[1,I_fromInt(_dg)];_de=[1,I_fromInt(_di)];continue;}}else{_dd=[1,I_fromInt(_dg)];_de=_dh;continue;}}else{var _dj=E(_de);if(!_dj[0]){_dd=_df;_de=[1,I_fromInt(_dj[1])];continue;}else{return [1,I_mul(_df[1],_dj[1])];}}}},_dk=function(_dl,_dm,_dn){while(1){var _do=E(_dn);if(!_do[0]){return E(_dm);}else{var _dp=_cW(_dc(_dm,_dl),_do[1]);_dn=_do[2];_dm=_dp;continue;}}},_dq=function(_dr){var _ds=new T(function(){return _aM(_aM([0,function(_dt){return E(E(_dt)[1])==45?_ck(_cT,function(_du){return A(_dr,[[1,new T(function(){return _d6(_dk(_da,_db,_du));})]]);}):[2];}],[0,function(_dv){return E(E(_dv)[1])==43?_ck(_cT,function(_dw){return A(_dr,[[1,new T(function(){return _dk(_da,_db,_dw);})]]);}):[2];}]),new T(function(){return _ck(_cT,function(_dx){return A(_dr,[[1,new T(function(){return _dk(_da,_db,_dx);})]]);});}));});return _aM([0,function(_dy){return E(E(_dy)[1])==101?E(_ds):[2];}],[0,function(_dz){return E(E(_dz)[1])==69?E(_ds):[2];}]);},_dA=function(_dB){return A(_dB,[_a]);},_dC=function(_dD){return A(_dD,[_a]);},_dE=function(_dF){var _dG=new T(function(){return _ck(_cT,function(_dH){return A(_dF,[[1,_dH]]);});});return [0,function(_dI){return E(E(_dI)[1])==46?E(_dG):[2];}];},_dJ=function(_dK){return _ck(_cT,function(_dL){return _bV(_dE,_dA,function(_dM){return _bV(_dq,_dC,function(_dN){return A(_dK,[[5,[1,_dL,_dM,_dN]]]);});});});},_dO=function(_dP,_dQ,_dR){while(1){var _dS=E(_dR);if(!_dS[0]){return false;}else{if(!A(_bk,[_dP,_dQ,_dS[1]])){_dR=_dS[2];continue;}else{return true;}}}},_dT=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dU=function(_dV){return _dO(_bj,_dV,_dT);},_dW=[0,8],_dX=[0,16],_dY=function(_dZ){var _e0=new T(function(){return _ck(_dX,function(_e1){return A(_dZ,[[5,[0,_dX,_e1]]]);});}),_e2=new T(function(){return _ck(_dW,function(_e3){return A(_dZ,[[5,[0,_dW,_e3]]]);});}),_e4=new T(function(){return _ck(_dX,function(_e5){return A(_dZ,[[5,[0,_dX,_e5]]]);});}),_e6=new T(function(){return _ck(_dW,function(_e7){return A(_dZ,[[5,[0,_dW,_e7]]]);});});return [0,function(_e8){return E(E(_e8)[1])==48?E([0,function(_e9){switch(E(E(_e9)[1])){case 79:return E(_e6);case 88:return E(_e4);case 111:return E(_e2);case 120:return E(_e0);default:return [2];}}]):[2];}];},_ea=false,_eb=true,_ec=function(_ed){var _ee=new T(function(){return A(_ed,[_dX]);}),_ef=new T(function(){return A(_ed,[_dW]);}),_eg=new T(function(){return A(_ed,[_dX]);}),_eh=new T(function(){return A(_ed,[_dW]);});return [0,function(_ei){switch(E(E(_ei)[1])){case 79:return E(_eh);case 88:return E(_eg);case 111:return E(_ef);case 120:return E(_ee);default:return [2];}}];},_ej=function(_ek){return A(_ek,[_cT]);},_el=function(_em){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3M(9,_em,_9);})));},_en=function(_eo){var _ep=E(_eo);return _ep[0]==0?E(_ep[1]):I_toInt(_ep[1]);},_eq=function(_er,_es){var _et=E(_er);if(!_et[0]){var _eu=_et[1],_ev=E(_es);return _ev[0]==0?_eu<=_ev[1]:I_compareInt(_ev[1],_eu)>=0;}else{var _ew=_et[1],_ex=E(_es);return _ex[0]==0?I_compareInt(_ew,_ex[1])<=0:I_compare(_ew,_ex[1])<=0;}},_ey=function(_ez){return [2];},_eA=function(_eB){var _eC=E(_eB);if(!_eC[0]){return E(_ey);}else{var _eD=_eC[1],_eE=E(_eC[2]);if(!_eE[0]){return E(_eD);}else{var _eF=new T(function(){return _eA(_eE);});return function(_eG){return _aM(A(_eD,[_eG]),new T(function(){return A(_eF,[_eG]);}));};}}},_eH=unCStr("NUL"),_eI=function(_eJ){return [2];},_eK=function(_eL){return _eI(_eL);},_eM=function(_eN,_eO){var _eP=function(_eQ,_eR){var _eS=E(_eQ);if(!_eS[0]){return function(_eT){return A(_eT,[_eN]);};}else{var _eU=E(_eR);if(!_eU[0]){return E(_eI);}else{if(E(_eS[1])[1]!=E(_eU[1])[1]){return E(_eK);}else{var _eV=new T(function(){return _eP(_eS[2],_eU[2]);});return function(_eW){var _eX=new T(function(){return A(_eV,[_eW]);});return [0,function(_eY){return E(_eX);}];};}}}};return [1,function(_eZ){return A(_eP,[_eN,_eZ,_eO]);}];},_f0=[0,0],_f1=function(_f2){var _f3=new T(function(){return A(_f2,[_f0]);});return _eM(_eH,function(_f4){return E(_f3);});},_f5=unCStr("STX"),_f6=[0,2],_f7=function(_f8){var _f9=new T(function(){return A(_f8,[_f6]);});return _eM(_f5,function(_fa){return E(_f9);});},_fb=unCStr("ETX"),_fc=[0,3],_fd=function(_fe){var _ff=new T(function(){return A(_fe,[_fc]);});return _eM(_fb,function(_fg){return E(_ff);});},_fh=unCStr("EOT"),_fi=[0,4],_fj=function(_fk){var _fl=new T(function(){return A(_fk,[_fi]);});return _eM(_fh,function(_fm){return E(_fl);});},_fn=unCStr("ENQ"),_fo=[0,5],_fp=function(_fq){var _fr=new T(function(){return A(_fq,[_fo]);});return _eM(_fn,function(_fs){return E(_fr);});},_ft=unCStr("ACK"),_fu=[0,6],_fv=function(_fw){var _fx=new T(function(){return A(_fw,[_fu]);});return _eM(_ft,function(_fy){return E(_fx);});},_fz=unCStr("BEL"),_fA=[0,7],_fB=function(_fC){var _fD=new T(function(){return A(_fC,[_fA]);});return _eM(_fz,function(_fE){return E(_fD);});},_fF=unCStr("BS"),_fG=[0,8],_fH=function(_fI){var _fJ=new T(function(){return A(_fI,[_fG]);});return _eM(_fF,function(_fK){return E(_fJ);});},_fL=unCStr("HT"),_fM=[0,9],_fN=function(_fO){var _fP=new T(function(){return A(_fO,[_fM]);});return _eM(_fL,function(_fQ){return E(_fP);});},_fR=unCStr("LF"),_fS=[0,10],_fT=function(_fU){var _fV=new T(function(){return A(_fU,[_fS]);});return _eM(_fR,function(_fW){return E(_fV);});},_fX=unCStr("VT"),_fY=[0,11],_fZ=function(_g0){var _g1=new T(function(){return A(_g0,[_fY]);});return _eM(_fX,function(_g2){return E(_g1);});},_g3=unCStr("FF"),_g4=[0,12],_g5=function(_g6){var _g7=new T(function(){return A(_g6,[_g4]);});return _eM(_g3,function(_g8){return E(_g7);});},_g9=unCStr("CR"),_ga=[0,13],_gb=function(_gc){var _gd=new T(function(){return A(_gc,[_ga]);});return _eM(_g9,function(_ge){return E(_gd);});},_gf=unCStr("SI"),_gg=[0,15],_gh=function(_gi){var _gj=new T(function(){return A(_gi,[_gg]);});return _eM(_gf,function(_gk){return E(_gj);});},_gl=unCStr("DLE"),_gm=[0,16],_gn=function(_go){var _gp=new T(function(){return A(_go,[_gm]);});return _eM(_gl,function(_gq){return E(_gp);});},_gr=unCStr("DC1"),_gs=[0,17],_gt=function(_gu){var _gv=new T(function(){return A(_gu,[_gs]);});return _eM(_gr,function(_gw){return E(_gv);});},_gx=unCStr("DC2"),_gy=[0,18],_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gy]);});return _eM(_gx,function(_gC){return E(_gB);});},_gD=unCStr("DC3"),_gE=[0,19],_gF=function(_gG){var _gH=new T(function(){return A(_gG,[_gE]);});return _eM(_gD,function(_gI){return E(_gH);});},_gJ=unCStr("DC4"),_gK=[0,20],_gL=function(_gM){var _gN=new T(function(){return A(_gM,[_gK]);});return _eM(_gJ,function(_gO){return E(_gN);});},_gP=unCStr("NAK"),_gQ=[0,21],_gR=function(_gS){var _gT=new T(function(){return A(_gS,[_gQ]);});return _eM(_gP,function(_gU){return E(_gT);});},_gV=unCStr("SYN"),_gW=[0,22],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _eM(_gV,function(_h0){return E(_gZ);});},_h1=unCStr("ETB"),_h2=[0,23],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _eM(_h1,function(_h6){return E(_h5);});},_h7=unCStr("CAN"),_h8=[0,24],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _eM(_h7,function(_hc){return E(_hb);});},_hd=unCStr("EM"),_he=[0,25],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _eM(_hd,function(_hi){return E(_hh);});},_hj=unCStr("SUB"),_hk=[0,26],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _eM(_hj,function(_ho){return E(_hn);});},_hp=unCStr("ESC"),_hq=[0,27],_hr=function(_hs){var _ht=new T(function(){return A(_hs,[_hq]);});return _eM(_hp,function(_hu){return E(_ht);});},_hv=unCStr("FS"),_hw=[0,28],_hx=function(_hy){var _hz=new T(function(){return A(_hy,[_hw]);});return _eM(_hv,function(_hA){return E(_hz);});},_hB=unCStr("GS"),_hC=[0,29],_hD=function(_hE){var _hF=new T(function(){return A(_hE,[_hC]);});return _eM(_hB,function(_hG){return E(_hF);});},_hH=unCStr("RS"),_hI=[0,30],_hJ=function(_hK){var _hL=new T(function(){return A(_hK,[_hI]);});return _eM(_hH,function(_hM){return E(_hL);});},_hN=unCStr("US"),_hO=[0,31],_hP=function(_hQ){var _hR=new T(function(){return A(_hQ,[_hO]);});return _eM(_hN,function(_hS){return E(_hR);});},_hT=unCStr("SP"),_hU=[0,32],_hV=function(_hW){var _hX=new T(function(){return A(_hW,[_hU]);});return _eM(_hT,function(_hY){return E(_hX);});},_hZ=unCStr("DEL"),_i0=[0,127],_i1=function(_i2){var _i3=new T(function(){return A(_i2,[_i0]);});return _eM(_hZ,function(_i4){return E(_i3);});},_i5=[1,_i1,_9],_i6=[1,_hV,_i5],_i7=[1,_hP,_i6],_i8=[1,_hJ,_i7],_i9=[1,_hD,_i8],_ia=[1,_hx,_i9],_ib=[1,_hr,_ia],_ic=[1,_hl,_ib],_id=[1,_hf,_ic],_ie=[1,_h9,_id],_if=[1,_h3,_ie],_ig=[1,_gX,_if],_ih=[1,_gR,_ig],_ii=[1,_gL,_ih],_ij=[1,_gF,_ii],_ik=[1,_gz,_ij],_il=[1,_gt,_ik],_im=[1,_gn,_il],_in=[1,_gh,_im],_io=[1,_gb,_in],_ip=[1,_g5,_io],_iq=[1,_fZ,_ip],_ir=[1,_fT,_iq],_is=[1,_fN,_ir],_it=[1,_fH,_is],_iu=[1,_fB,_it],_iv=[1,_fv,_iu],_iw=[1,_fp,_iv],_ix=[1,_fj,_iw],_iy=[1,_fd,_ix],_iz=[1,_f7,_iy],_iA=[1,_f1,_iz],_iB=unCStr("SOH"),_iC=[0,1],_iD=function(_iE){var _iF=new T(function(){return A(_iE,[_iC]);});return _eM(_iB,function(_iG){return E(_iF);});},_iH=unCStr("SO"),_iI=[0,14],_iJ=function(_iK){var _iL=new T(function(){return A(_iK,[_iI]);});return _eM(_iH,function(_iM){return E(_iL);});},_iN=function(_iO){return _bV(_iD,_iJ,_iO);},_iP=[1,_iN,_iA],_iQ=new T(function(){return _eA(_iP);}),_iR=[0,1114111],_iS=[0,34],_iT=[0,_iS,_eb],_iU=[0,39],_iV=[0,_iU,_eb],_iW=[0,92],_iX=[0,_iW,_eb],_iY=[0,_fA,_eb],_iZ=[0,_fG,_eb],_j0=[0,_g4,_eb],_j1=[0,_fS,_eb],_j2=[0,_ga,_eb],_j3=[0,_fM,_eb],_j4=[0,_fY,_eb],_j5=[0,_f0,_eb],_j6=[0,_iC,_eb],_j7=[0,_f6,_eb],_j8=[0,_fc,_eb],_j9=[0,_fi,_eb],_ja=[0,_fo,_eb],_jb=[0,_fu,_eb],_jc=[0,_fA,_eb],_jd=[0,_fG,_eb],_je=[0,_fM,_eb],_jf=[0,_fS,_eb],_jg=[0,_fY,_eb],_jh=[0,_g4,_eb],_ji=[0,_ga,_eb],_jj=[0,_iI,_eb],_jk=[0,_gg,_eb],_jl=[0,_gm,_eb],_jm=[0,_gs,_eb],_jn=[0,_gy,_eb],_jo=[0,_gE,_eb],_jp=[0,_gK,_eb],_jq=[0,_gQ,_eb],_jr=[0,_gW,_eb],_js=[0,_h2,_eb],_jt=[0,_h8,_eb],_ju=[0,_he,_eb],_jv=[0,_hk,_eb],_jw=[0,_hq,_eb],_jx=[0,_hw,_eb],_jy=[0,_hC,_eb],_jz=[0,_hI,_eb],_jA=[0,_hO,_eb],_jB=function(_jC){return [0,_jC];},_jD=function(_jE){var _jF=new T(function(){return A(_jE,[_j4]);}),_jG=new T(function(){return A(_jE,[_j3]);}),_jH=new T(function(){return A(_jE,[_j2]);}),_jI=new T(function(){return A(_jE,[_j1]);}),_jJ=new T(function(){return A(_jE,[_j0]);}),_jK=new T(function(){return A(_jE,[_iZ]);}),_jL=new T(function(){return A(_jE,[_iY]);}),_jM=new T(function(){return A(_jE,[_iX]);}),_jN=new T(function(){return A(_jE,[_iV]);}),_jO=new T(function(){return A(_jE,[_iT]);});return _aM([0,function(_jP){switch(E(E(_jP)[1])){case 34:return E(_jO);case 39:return E(_jN);case 92:return E(_jM);case 97:return E(_jL);case 98:return E(_jK);case 102:return E(_jJ);case 110:return E(_jI);case 114:return E(_jH);case 116:return E(_jG);case 118:return E(_jF);default:return [2];}}],new T(function(){return _aM(_bV(_ec,_ej,function(_jQ){var _jR=new T(function(){return _jB(E(_jQ)[1]);});return _ck(_jQ,function(_jS){var _jT=_dk(_jR,_db,_jS);return !_eq(_jT,_iR)?[2]:A(_jE,[[0,new T(function(){var _jU=_en(_jT);return _jU>>>0>1114111?_el(_jU):[0,_jU];}),_eb]]);});}),new T(function(){var _jV=new T(function(){return A(_jE,[_jA]);}),_jW=new T(function(){return A(_jE,[_jz]);}),_jX=new T(function(){return A(_jE,[_jy]);}),_jY=new T(function(){return A(_jE,[_jx]);}),_jZ=new T(function(){return A(_jE,[_jw]);}),_k0=new T(function(){return A(_jE,[_jv]);}),_k1=new T(function(){return A(_jE,[_ju]);}),_k2=new T(function(){return A(_jE,[_jt]);}),_k3=new T(function(){return A(_jE,[_js]);}),_k4=new T(function(){return A(_jE,[_jr]);}),_k5=new T(function(){return A(_jE,[_jq]);}),_k6=new T(function(){return A(_jE,[_jp]);}),_k7=new T(function(){return A(_jE,[_jo]);}),_k8=new T(function(){return A(_jE,[_jn]);}),_k9=new T(function(){return A(_jE,[_jm]);}),_ka=new T(function(){return A(_jE,[_jl]);}),_kb=new T(function(){return A(_jE,[_jk]);}),_kc=new T(function(){return A(_jE,[_jj]);}),_kd=new T(function(){return A(_jE,[_ji]);}),_ke=new T(function(){return A(_jE,[_jh]);}),_kf=new T(function(){return A(_jE,[_jg]);}),_kg=new T(function(){return A(_jE,[_jf]);}),_kh=new T(function(){return A(_jE,[_je]);}),_ki=new T(function(){return A(_jE,[_jd]);}),_kj=new T(function(){return A(_jE,[_jc]);}),_kk=new T(function(){return A(_jE,[_jb]);}),_kl=new T(function(){return A(_jE,[_ja]);}),_km=new T(function(){return A(_jE,[_j9]);}),_kn=new T(function(){return A(_jE,[_j8]);}),_ko=new T(function(){return A(_jE,[_j7]);}),_kp=new T(function(){return A(_jE,[_j6]);}),_kq=new T(function(){return A(_jE,[_j5]);});return _aM([0,function(_kr){return E(E(_kr)[1])==94?E([0,function(_ks){switch(E(E(_ks)[1])){case 64:return E(_kq);case 65:return E(_kp);case 66:return E(_ko);case 67:return E(_kn);case 68:return E(_km);case 69:return E(_kl);case 70:return E(_kk);case 71:return E(_kj);case 72:return E(_ki);case 73:return E(_kh);case 74:return E(_kg);case 75:return E(_kf);case 76:return E(_ke);case 77:return E(_kd);case 78:return E(_kc);case 79:return E(_kb);case 80:return E(_ka);case 81:return E(_k9);case 82:return E(_k8);case 83:return E(_k7);case 84:return E(_k6);case 85:return E(_k5);case 86:return E(_k4);case 87:return E(_k3);case 88:return E(_k2);case 89:return E(_k1);case 90:return E(_k0);case 91:return E(_jZ);case 92:return E(_jY);case 93:return E(_jX);case 94:return E(_jW);case 95:return E(_jV);default:return [2];}}]):[2];}],new T(function(){return A(_iQ,[function(_kt){return A(_jE,[[0,_kt,_eb]]);}]);}));}));}));},_ku=function(_kv){return A(_kv,[_0]);},_kw=function(_kx){var _ky=E(_kx);if(!_ky[0]){return E(_ku);}else{var _kz=_ky[2],_kA=E(E(_ky[1])[1]);switch(_kA){case 9:var _kB=new T(function(){return _kw(_kz);});return function(_kC){var _kD=new T(function(){return A(_kB,[_kC]);});return [0,function(_kE){return E(_kD);}];};case 10:var _kF=new T(function(){return _kw(_kz);});return function(_kG){var _kH=new T(function(){return A(_kF,[_kG]);});return [0,function(_kI){return E(_kH);}];};case 11:var _kJ=new T(function(){return _kw(_kz);});return function(_kK){var _kL=new T(function(){return A(_kJ,[_kK]);});return [0,function(_kM){return E(_kL);}];};case 12:var _kN=new T(function(){return _kw(_kz);});return function(_kO){var _kP=new T(function(){return A(_kN,[_kO]);});return [0,function(_kQ){return E(_kP);}];};case 13:var _kR=new T(function(){return _kw(_kz);});return function(_kS){var _kT=new T(function(){return A(_kR,[_kS]);});return [0,function(_kU){return E(_kT);}];};case 32:var _kV=new T(function(){return _kw(_kz);});return function(_kW){var _kX=new T(function(){return A(_kV,[_kW]);});return [0,function(_kY){return E(_kX);}];};case 160:var _kZ=new T(function(){return _kw(_kz);});return function(_l0){var _l1=new T(function(){return A(_kZ,[_l0]);});return [0,function(_l2){return E(_l1);}];};default:var _l3=u_iswspace(_kA);if(!E(_l3)){return E(_ku);}else{var _l4=new T(function(){return _kw(_kz);});return function(_l5){var _l6=new T(function(){return A(_l4,[_l5]);});return [0,function(_l7){return E(_l6);}];};}}}},_l8=function(_l9){var _la=new T(function(){return _jD(_l9);}),_lb=new T(function(){return _l8(_l9);}),_lc=[1,function(_ld){return A(_kw,[_ld,function(_le){return E([0,function(_lf){return E(E(_lf)[1])==92?E(_lb):[2];}]);}]);}];return _aM([0,function(_lg){return E(E(_lg)[1])==92?E([0,function(_lh){var _li=E(E(_lh)[1]);switch(_li){case 9:return E(_lc);case 10:return E(_lc);case 11:return E(_lc);case 12:return E(_lc);case 13:return E(_lc);case 32:return E(_lc);case 38:return E(_lb);case 160:return E(_lc);default:var _lj=u_iswspace(_li);return E(_lj)==0?[2]:E(_lc);}}]):[2];}],[0,function(_lk){var _ll=E(_lk);return E(_ll[1])==92?E(_la):A(_l9,[[0,_ll,_ea]]);}]);},_lm=function(_ln,_lo){var _lp=new T(function(){return A(_lo,[[1,new T(function(){return A(_ln,[_9]);})]]);});return _l8(function(_lq){var _lr=E(_lq),_ls=E(_lr[1]);return E(_ls[1])==34?!E(_lr[2])?E(_lp):_lm(function(_lt){return A(_ln,[[1,_ls,_lt]]);},_lo):_lm(function(_lu){return A(_ln,[[1,_ls,_lu]]);},_lo);});},_lv=unCStr("_\'"),_lw=function(_lx){var _ly=u_iswalnum(_lx);return E(_ly)==0?_dO(_bj,[0,_lx],_lv):true;},_lz=function(_lA){return _lw(E(_lA)[1]);},_lB=unCStr(",;()[]{}`"),_lC=function(_lD){return A(_lD,[_9]);},_lE=function(_lF,_lG){var _lH=function(_lI){var _lJ=E(_lI);if(!_lJ[0]){return E(_lC);}else{var _lK=_lJ[1];if(!A(_lF,[_lK])){return E(_lC);}else{var _lL=new T(function(){return _lH(_lJ[2]);});return function(_lM){var _lN=new T(function(){return A(_lL,[function(_lO){return A(_lM,[[1,_lK,_lO]]);}]);});return [0,function(_lP){return E(_lN);}];};}}};return [1,function(_lQ){return A(_lH,[_lQ,_lG]);}];},_lR=unCStr(".."),_lS=unCStr("::"),_lT=unCStr("->"),_lU=[0,64],_lV=[1,_lU,_9],_lW=[0,126],_lX=[1,_lW,_9],_lY=unCStr("=>"),_lZ=[1,_lY,_9],_m0=[1,_lX,_lZ],_m1=[1,_lV,_m0],_m2=[1,_lT,_m1],_m3=unCStr("<-"),_m4=[1,_m3,_m2],_m5=[0,124],_m6=[1,_m5,_9],_m7=[1,_m6,_m4],_m8=[1,_iW,_9],_m9=[1,_m8,_m7],_ma=[0,61],_mb=[1,_ma,_9],_mc=[1,_mb,_m9],_md=[1,_lS,_mc],_me=[1,_lR,_md],_mf=function(_mg){var _mh=new T(function(){return A(_mg,[_ch]);});return _aM([1,function(_mi){return E(_mi)[0]==0?E(_mh):[2];}],new T(function(){var _mj=new T(function(){return _jD(function(_mk){var _ml=E(_mk);return (function(_mm,_mn){var _mo=new T(function(){return A(_mg,[[0,_mm]]);});return !E(_mn)?E(E(_mm)[1])==39?[2]:[0,function(_mp){return E(E(_mp)[1])==39?E(_mo):[2];}]:[0,function(_mq){return E(E(_mq)[1])==39?E(_mo):[2];}];})(_ml[1],_ml[2]);});});return _aM([0,function(_mr){return E(E(_mr)[1])==39?E([0,function(_ms){var _mt=E(_ms);switch(E(_mt[1])){case 39:return [2];case 92:return E(_mj);default:var _mu=new T(function(){return A(_mg,[[0,_mt]]);});return [0,function(_mv){return E(E(_mv)[1])==39?E(_mu):[2];}];}}]):[2];}],new T(function(){var _mw=new T(function(){return _lm(_n,_mg);});return _aM([0,function(_mx){return E(E(_mx)[1])==34?E(_mw):[2];}],new T(function(){return _aM([0,function(_my){return !_dO(_bj,_my,_lB)?[2]:A(_mg,[[2,[1,_my,_9]]]);}],new T(function(){return _aM([0,function(_mz){return !_dO(_bj,_mz,_dT)?[2]:_lE(_dU,function(_mA){var _mB=[1,_mz,_mA];return !_dO(_bA,_mB,_me)?A(_mg,[[4,_mB]]):A(_mg,[[2,_mB]]);});}],new T(function(){return _aM([0,function(_mC){var _mD=E(_mC),_mE=_mD[1],_mF=u_iswalpha(_mE);return E(_mF)==0?E(_mE)==95?_lE(_lz,function(_mG){return A(_mg,[[3,[1,_mD,_mG]]]);}):[2]:_lE(_lz,function(_mH){return A(_mg,[[3,[1,_mD,_mH]]]);});}],new T(function(){return _bV(_dY,_dJ,_mg);}));}));}));}));}));}));},_mI=function(_mJ){var _mK=new T(function(){return _mf(_mJ);});return [1,function(_mL){return A(_kw,[_mL,function(_mM){return E(_mK);}]);}];},_mN=[0,0],_mO=function(_mP,_mQ){var _mR=new T(function(){return A(_mP,[_mN,function(_mS){var _mT=new T(function(){return A(_mQ,[_mS]);});return _mI(function(_mU){var _mV=E(_mU);if(_mV[0]==2){var _mW=E(_mV[1]);return _mW[0]==0?[2]:E(E(_mW[1])[1])==41?E(_mW[2])[0]==0?E(_mT):[2]:[2];}else{return [2];}});}]);});return _mI(function(_mX){var _mY=E(_mX);if(_mY[0]==2){var _mZ=E(_mY[1]);return _mZ[0]==0?[2]:E(E(_mZ[1])[1])==40?E(_mZ[2])[0]==0?E(_mR):[2]:[2];}else{return [2];}});},_n0=function(_n1,_n2,_n3){var _n4=function(_n5,_n6){var _n7=new T(function(){return _mf(function(_n8){return A(_n1,[_n8,_n5,function(_n9){return A(_n6,[new T(function(){return [0, -E(_n9)[1]];})]);}]);});});return _aM(_mI(function(_na){var _nb=E(_na);if(_nb[0]==4){var _nc=E(_nb[1]);return _nc[0]==0?A(_n1,[_nb,_n5,_n6]):E(E(_nc[1])[1])==45?E(_nc[2])[0]==0?E([1,function(_nd){return A(_kw,[_nd,function(_ne){return E(_n7);}]);}]):A(_n1,[_nb,_n5,_n6]):A(_n1,[_nb,_n5,_n6]);}else{return A(_n1,[_nb,_n5,_n6]);}}),new T(function(){return _mO(_n4,_n6);}));};return _n4(_n2,_n3);},_nf=function(_ng,_nh){return [2];},_ni=function(_nj,_nk){return _nf(_nj,_nk);},_nl=function(_nm){var _nn=E(_nm);return _nn[0]==0?[1,new T(function(){return _dk(new T(function(){return _jB(E(_nn[1])[1]);}),_db,_nn[2]);})]:E(_nn[2])[0]==0?E(_nn[3])[0]==0?[1,new T(function(){return _dk(_da,_db,_nn[1]);})]:[0]:[0];},_no=function(_np){var _nq=E(_np);if(_nq[0]==5){var _nr=_nl(_nq[1]);if(!_nr[0]){return E(_nf);}else{var _ns=new T(function(){return [0,_en(_nr[1])];});return function(_nt,_nu){return A(_nu,[_ns]);};}}else{return E(_ni);}},_nv=function(_nj,_nk){return _n0(_no,_nj,_nk);},_nw=function(_nx,_ny){var _nz=function(_nA,_nB){var _nC=new T(function(){return A(_nB,[_9]);}),_nD=new T(function(){return A(_nx,[_mN,function(_nE){return _nz(_eb,function(_nF){return A(_nB,[[1,_nE,_nF]]);});}]);});return _mI(function(_nG){var _nH=E(_nG);if(_nH[0]==2){var _nI=E(_nH[1]);if(!_nI[0]){return [2];}else{var _nJ=_nI[2];switch(E(E(_nI[1])[1])){case 44:return E(_nJ)[0]==0?!E(_nA)?[2]:E(_nD):[2];case 93:return E(_nJ)[0]==0?E(_nC):[2];default:return [2];}}}else{return [2];}});},_nK=function(_nL){var _nM=new T(function(){return _aM(_nz(_ea,_nL),new T(function(){return A(_nx,[_mN,function(_nN){return _nz(_eb,function(_nO){return A(_nL,[[1,_nN,_nO]]);});}]);}));});return _aM(_mI(function(_nP){var _nQ=E(_nP);if(_nQ[0]==2){var _nR=E(_nQ[1]);return _nR[0]==0?[2]:E(E(_nR[1])[1])==91?E(_nR[2])[0]==0?E(_nM):[2]:[2];}else{return [2];}}),new T(function(){return _mO(function(_nS,_nT){return _nK(_nT);},_nL);}));};return _nK(_ny);},_nU=function(_nV,_nW){return _nw(_nv,_nW);},_nX=new T(function(){return _nw(_nv,_bN);}),_nY=function(_nk){return _aC(_nX,_nk);},_nZ=function(_o0){var _o1=new T(function(){return _n0(_no,_o0,_bN);});return function(_cg){return _aC(_o1,_cg);};},_o2=[0,_nZ,_nY,_nv,_nU],_o3=function(_o4,_o5){return _3M(0,E(_o4)[1],_o5);},_o6=function(_o7,_o8){return _2D(_o3,_o7,_o8);},_o9=function(_oa,_ob,_oc){return _3M(E(_oa)[1],E(_ob)[1],_oc);},_od=[0,_o9,_5h,_o6],_oe=unCStr("GHC.Types"),_of=unCStr("Int"),_og=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5M,_oe,_of],_oh=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_og,_9],_oi=function(_oj){return E(_oh);},_ok=function(_ol){return E(E(_ol)[1]);},_om=function(_on){return E(E(_on)[2]);},_oo=function(_op,_oq){var _or=new T(function(){return A(_om,[_op,_oq]);}),_os=new T(function(){return _ok(_op);}),_ot=new T(function(){return _3x(_os);}),_ou=new T(function(){return _37(_os);});return function(_ov){return A(_ou,[_or,function(_ow){return A(_ot,[[0,_ow,_ov]]);}]);};},_ox=function(_oy,_oz){return [0,_oy,function(_oA){return _oo(_oz,_oA);}];},_oB=function(_oC,_oD){return A(_3x,[_oC,[0,_oD,_oD]]);},_oE=function(_oF,_oG,_oH){return A(_3x,[_oF,[0,_0,_oG]]);},_oI=function(_oJ,_oK){return [0,_oJ,function(_oL){return _oB(_oK,_oL);},function(_oM,_oN){return _oE(_oK,_oM,_oN);}];},_oO=function(_oP,_oQ){return A(_oP,[function(_){return jsFind(toJSStr(E(_oQ)));}]);},_oR=function(_oS){return E(E(_oS)[3]);},_oT=unCStr("[]"),_oU=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5M,_oe,_oT],_oV=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oU,_9],_oW=function(_oX){return E(_oV);},_oY=unCStr("Char"),_oZ=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5M,_oe,_oY],_p0=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oZ,_9],_p1=function(_p2){return E(_p0);},_p3=new T(function(){return _6G(_oW,_p1);}),_p4=new T(function(){return A(_p3,[_6F]);}),_p5=new T(function(){return E(_6F);}),_p6=function(_p7){return E(E(_p7)[6]);},_p8=function(_p9){return E(E(_p9)[1]);},_pa=[0,0],_pb=[0,32],_pc=[0,10],_pd=function(_pe){var _pf=E(_pe);if(!_pf[0]){return E(_n);}else{var _pg=_pf[1],_ph=E(_pf[2]);if(!_ph[0]){return _pi(_pc,_pg);}else{var _pj=new T(function(){return _pd(_ph);}),_pk=new T(function(){return _pi(_pc,_pg);});return function(_pl){return A(_pk,[[1,_pb,new T(function(){return A(_pj,[_pl]);})]]);};}}},_pm=unCStr("->"),_pn=[1,_pm,_9],_po=[1,_oe,_pn],_pp=[1,_5M,_po],_pq=[0,32],_pr=function(_ps){var _pt=E(_ps);if(!_pt[0]){return [0];}else{var _pu=_pt[1],_pv=E(_pt[2]);return _pv[0]==0?E(_pu):_1N(_pu,[1,_pq,new T(function(){return _pr(_pv);})]);}},_pw=new T(function(){return _pr(_pp);}),_px=new T(function(){var _py=_6a(_pw);return [0,_py[1],_py[2],_5M,_oe,_pm];}),_pz=function(_pA,_pB){var _pC=E(_pA);return _pC[0]==0?E(_pB):A(_pC[1],[new T(function(){return _pz(_pC[2],_pB);})]);},_pD=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pE=[1,_5O,_9],_pF=function(_pG){var _pH=E(_pG);if(!_pH[0]){return [0];}else{var _pI=E(_pH[1]);return [1,[0,_pI[1],_pI[2]],new T(function(){return _pF(_pH[2]);})];}},_pJ=new T(function(){var _pK=_1N(_9,_pE);if(!_pK[0]){return E(_oU);}else{var _pL=_6a(new T(function(){return _5Y(_6m(_6x,[1,_pD,new T(function(){return _pF(_pK);})]));}));return E(_oU);}}),_pM=[0,40],_pN=function(_pO){return _pi(_pc,_pO);},_pP=[0,8],_pQ=unCStr(" -> "),_pR=[0,9],_pS=[0,93],_pT=[0,91],_pU=[0,41],_pV=[0,44],_pW=function(_pO){return [1,_pV,_pO];},_pX=function(_pY,_pZ){var _q0=E(_pZ);return _q0[0]==0?[0]:[1,_pY,[1,_q0[1],new T(function(){return _pX(_pY,_q0[2]);})]];},_pi=function(_q1,_q2){var _q3=E(_q2),_q4=_q3[3],_q5=E(_q3[4]);if(!_q5[0]){return function(_q6){return _1N(E(_q4)[5],_q6);};}else{var _q7=_q5[1],_q8=new T(function(){var _q9=E(_q4)[5],_qa=new T(function(){return _pd(_q5);}),_qb=new T(function(){return E(_q1)[1]<=9?function(_qc){return _1N(_q9,[1,_pb,new T(function(){return A(_qa,[_qc]);})]);}:function(_qd){return [1,_3L,new T(function(){return _1N(_q9,[1,_pb,new T(function(){return A(_qa,[[1,_3K,_qd]]);})]);})];};}),_qe=E(_q9);if(!_qe[0]){return E(_qb);}else{if(E(E(_qe[1])[1])==40){var _qf=E(_qe[2]);return _qf[0]==0?E(_qb):E(E(_qf[1])[1])==44?function(_qg){return [1,_pM,new T(function(){return A(new T(function(){var _qh=_6m(_pN,_q5);if(!_qh[0]){return E(_n);}else{var _qi=new T(function(){return _pX(_pW,_qh[2]);});return function(_cg){return _pz([1,_qh[1],_qi],_cg);};}}),[[1,_pU,_qg]]);})];}:E(_qb);}else{return E(_qb);}}}),_qj=E(_q5[2]);if(!_qj[0]){var _qk=E(_q4),_ql=E(_pJ),_qm=hs_eqWord64(_qk[1],_ql[1]);if(!E(_qm)){return E(_q8);}else{var _qn=hs_eqWord64(_qk[2],_ql[2]);if(!E(_qn)){return E(_q8);}else{var _qo=new T(function(){return _pi(_pa,_q7);});return function(_qp){return [1,_pT,new T(function(){return A(_qo,[[1,_pS,_qp]]);})];};}}}else{if(!E(_qj[2])[0]){var _qq=E(_q4),_qr=E(_px),_qs=hs_eqWord64(_qq[1],_qr[1]);if(!E(_qs)){return E(_q8);}else{var _qt=hs_eqWord64(_qq[2],_qr[2]);if(!E(_qt)){return E(_q8);}else{var _qu=new T(function(){return _pi(_pP,_qj[1]);}),_qv=new T(function(){return _pi(_pR,_q7);});return E(_q1)[1]<=8?function(_qw){return A(_qv,[new T(function(){return _1N(_pQ,new T(function(){return A(_qu,[_qw]);}));})]);}:function(_qx){return [1,_3L,new T(function(){return A(_qv,[new T(function(){return _1N(_pQ,new T(function(){return A(_qu,[[1,_3K,_qx]]);}));})]);})];};}}}else{return E(_q8);}}}},_qy=function(_qz,_qA,_qB,_qC){var _qD=new T(function(){return _3x(_qz);}),_qE=new T(function(){return _oR(_qC);}),_qF=new T(function(){return _p6(_qC);}),_qG=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pi,[_pa,A(_qA,[_p5]),_9]);}));}),_qH=new T(function(){return A(_p8,[_qB,_f]);});return function(_qI){if(!E(new T(function(){var _qJ=A(_qA,[_p5]),_qK=E(_p4),_qL=hs_eqWord64(_qJ[1],_qK[1]);if(!E(_qL)){return false;}else{var _qM=hs_eqWord64(_qJ[2],_qK[2]);return E(_qM)==0?false:true;}}))){var _qN=new T(function(){return A(_qD,[[1,_qI,new T(function(){return A(_qF,[new T(function(){return A(_qE,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1N(_qI,_qG);}));})]);})]);})]]);}),_qO=A(_qH,[_qI]);if(!_qO[0]){return E(_qN);}else{var _qP=E(_qO[1]);return E(_qP[2])[0]==0?E(_qO[2])[0]==0?A(_qD,[[2,_qP[1]]]):E(_qN):E(_qN);}}else{return A(_qD,[[2,_qI]]);}};},_qQ=[0],_qR=new T(function(){return [0,"value"];}),_qS=function(_qT,_qU,_qV,_qW,_qX,_qY){var _qZ=E(_qT),_r0=_qZ[1],_r1=new T(function(){return A(_qZ[3],[_qQ]);}),_r2=new T(function(){return _qy(_qZ,_qV,_qW,_qX);});return A(_r0,[new T(function(){return _oO(_qU,_qY);}),function(_r3){var _r4=E(_r3);return _r4[0]==0?E(_r1):A(_r0,[new T(function(){return A(_qU,[function(_){var _r5=jsGet(E(_r4[1])[1],E(_qR)[1]);return [1,new T(function(){return fromJSStr(_r5);})];}]);}),function(_r6){var _r7=E(_r6);return _r7[0]==0?E(_r1):A(_r2,[_r7[1]]);}]);}]);},_r8=1,_r9=function(_ra){return E(E(_ra)[9]);},_rb=function(_rc){return E(E(_rc)[2]);},_rd=function(_re){return E(E(_re)[3]);},_rf=function(_rg){return E(E(_rg)[2]);},_rh=function(_ri,_rj,_rk,_rl,_rm,_rn,_ro,_rp,_rq,_rr,_rs,_rt){var _ru=_ok(_rn),_rv=_ru[1],_rw=_ru[3],_rx=new T(function(){return _90(_rp);}),_ry=new T(function(){return _92(_rx);}),_rz=new T(function(){return _rd(_ro);}),_rA=new T(function(){return _rf(_rj);}),_rB=new T(function(){return _r9(_rp);});return A(_rv,[new T(function(){var _rC=E(_rr);if(!_rC[0]){var _rD=E(_ro);return _3R(_rq,_rD[1],_rD[2],_rD[3]);}else{return A(_rw,[_rC[1]]);}}),function(_rE){return A(_rv,[new T(function(){var _rF=E(_rq);return _rb(_ro);}),function(_rG){return A(_ru[2],[new T(function(){return A(_rz,[new T(function(){var _rH=E(new T(function(){var _rI=E(_rq);return [0,coercionToken];})),_rJ=E(_rG);return [0,_rJ[1],_rJ[2],_r8,_rJ[4],_rJ[5]];})]);}),new T(function(){var _rK=new T(function(){return A(_rw,[[0,new T(function(){return A(_rB,[_rE,_rs,new T(function(){var _rL=E(_rt);if(!_rL[0]){return [0];}else{var _rM=_rL[1],_rN=_1x(_rm,_p3,_rM);return _rN[0]==0?A(_rf,[_rk,_rM]):E(_rN[1]);}}),_ea,_a]);}),_a]]);});return A(_rv,[new T(function(){var _rO=E(_rn);return _qS(_rO[1],_rO[2],_rl,_ri,_rp,_rE);}),function(_rP){var _rQ=E(_rP);switch(_rQ[0]){case 0:return E(_rK);case 1:return A(_rw,[[0,new T(function(){return A(_ry,[new T(function(){return A(_rB,[_rE,_rs,_rQ[1],_ea,_a]);}),_rQ[2]]);}),_a]]);default:var _rR=_rQ[1];return A(_rw,[[0,new T(function(){return A(_rB,[_rE,_rs,new T(function(){var _rS=_1x(_rl,_p3,_rR);return _rS[0]==0?A(_rA,[_rR]):E(_rS[1]);}),_ea,_a]);}),[1,_rR]]]);}}]);})]);}]);}]);},_rT=function(_rU,_rV,_rW,_rX,_rY){var _rZ=new T(function(){return _ok(_rV);}),_s0=new T(function(){return _3z(_rZ);}),_s1=new T(function(){return _oI(_s0,_rZ);}),_s2=new T(function(){return _ox(_s0,_rV);});return function(_cg,_s3,_s4){return _rh(_rY,_rX,_rX,_rW,_rW,_s2,_s1,_rU,[0,coercionToken],_cg,_s3,_s4);};},_s5=new T(function(){return _rT(_8D,_9J,_oi,_od,_o2);}),_s6=new T(function(){return A(_s5,[_a,_9I,_a]);}),_s7=unCStr("keydown"),_s8=unCStr("mousemove"),_s9=unCStr("blur"),_sa=unCStr("focus"),_sb=unCStr("change"),_sc=unCStr("unload"),_sd=unCStr("load"),_se=unCStr("keyup"),_sf=unCStr("keypress"),_sg=unCStr("mouseup"),_sh=unCStr("mousedown"),_si=unCStr("dblclick"),_sj=unCStr("click"),_sk=unCStr("mouseout"),_sl=unCStr("mouseover"),_sm=function(_sn){switch(E(_sn)[0]){case 0:return E(_sd);case 1:return E(_sc);case 2:return E(_sb);case 3:return E(_sa);case 4:return E(_s9);case 5:return E(_s8);case 6:return E(_sl);case 7:return E(_sk);case 8:return E(_sj);case 9:return E(_si);case 10:return E(_sh);case 11:return E(_sg);case 12:return E(_sf);case 13:return E(_se);default:return E(_s7);}},_so=[0],_sp=unCStr("OnLoad"),_sq=[0,_sp,_so],_sr=function(_){var _=0,_ss=newMVar(),_=putMVar(_ss,_sq);return [0,_ss];},_st=new T(function(){return _j(_sr);}),_su=function(_sv,_sw,_){var _sx=A(_sv,[_]);return die(_sw);},_sy=function(_sz,_sA,_sB,_){return _su(function(_){var _=putMVar(_sA,_sz);return _0;},_sB,_);},_sC=function(_sD,_){var _sE=0;if(!E(_sE)){return (function(_){var _sF=E(_st)[1],_sG=takeMVar(_sF),_sH=jsCatch(function(_){return (function(_){return _sD;})();},function(_X,_){return _sy(_sG,_sF,_X,_);}),_=putMVar(_sF,_sH);return _0;})();}else{var _sI=E(_st)[1],_sJ=takeMVar(_sI),_sK=jsCatch(function(_){return _sD;},function(_X,_){return _sy(_sJ,_sI,_X,_);}),_=putMVar(_sI,_sK);return _0;}},_sL=unCStr("true"),_sM=function(_sN,_sO){while(1){var _sP=E(_sN);if(!_sP[0]){return E(_sO)[0]==0?true:false;}else{var _sQ=E(_sO);if(!_sQ[0]){return false;}else{if(E(_sP[1])[1]!=E(_sQ[1])[1]){return false;}else{_sN=_sP[2];_sO=_sQ[2];continue;}}}}},_sR=new T(function(){return [0,"keydown"];}),_sS=new T(function(){return [0,"mousemove"];}),_sT=new T(function(){return [0,"blur"];}),_sU=new T(function(){return [0,"focus"];}),_sV=new T(function(){return [0,"change"];}),_sW=new T(function(){return [0,"unload"];}),_sX=new T(function(){return [0,"load"];}),_sY=new T(function(){return [0,"keyup"];}),_sZ=new T(function(){return [0,"keypress"];}),_t0=new T(function(){return [0,"mouseup"];}),_t1=new T(function(){return [0,"mousedown"];}),_t2=new T(function(){return [0,"dblclick"];}),_t3=new T(function(){return [0,"click"];}),_t4=new T(function(){return [0,"mouseout"];}),_t5=new T(function(){return [0,"mouseover"];}),_t6=function(_t7){switch(E(_t7)[0]){case 0:return E(_sX);case 1:return E(_sW);case 2:return E(_sV);case 3:return E(_sU);case 4:return E(_sT);case 5:return E(_sS);case 6:return E(_t5);case 7:return E(_t4);case 8:return E(_t3);case 9:return E(_t2);case 10:return E(_t1);case 11:return E(_t0);case 12:return E(_sZ);case 13:return E(_sY);default:return E(_sR);}},_t8=function(_t9,_ta,_tb){var _tc=new T(function(){return _sm(_ta);}),_td=new T(function(){return _t6(_ta);});return function(_te,_){var _tf=A(_t9,[_te,_]),_tg=E(_tf),_th=_tg[1],_ti=E(_tc),_tj=jsGetAttr(_th,toJSStr(_ti));if(!_sM(fromJSStr(_tj),_sL)){var _tk=E(_tb),_tl=jsSetCB(_th,E(_td)[1],E([0,_tb])[1]),_tm=A(_1,[_n,_tg,_ti,_sL,_]);return _tg;}else{return _tg;}};},_tn=function(_to,_tp){var _tq=new T(function(){return _sm(_tp);}),_tr=[0,_tq,_so];return function(_ts,_){var _tt=E(_ts),_tu=E(_tt[4]),_tv=_tu[1],_tw=_tu[2],_tx=A(_to,[_tt,_]),_ty=E(_tx),_tz=E(_ty[1]),_tA=_tz[1];return [0,[0,new T(function(){var _tB=E(_tp);switch(_tB[0]){case 0:return _t8(_tA,_tB,function(_){var _tC=_sC(_tr,_),_tD=A(_tv,[_]),_tE=E(_tD);if(!_tE[0]){return _0;}else{var _tF=A(_tw,[_tE[1],_]);return _0;}});case 1:return _t8(_tA,_tB,function(_){var _tG=_sC(_tr,_),_tH=A(_tv,[_]),_tI=E(_tH);if(!_tI[0]){return _0;}else{var _tJ=A(_tw,[_tI[1],_]);return _0;}});case 2:return _t8(_tA,_tB,function(_){var _tK=_sC(_tr,_),_tL=A(_tv,[_]),_tM=E(_tL);if(!_tM[0]){return _0;}else{var _tN=A(_tw,[_tM[1],_]);return _0;}});case 3:return _t8(_tA,_tB,function(_){var _tO=_sC(_tr,_),_tP=A(_tv,[_]),_tQ=E(_tP);if(!_tQ[0]){return _0;}else{var _tR=A(_tw,[_tQ[1],_]);return _0;}});case 4:return _t8(_tA,_tB,function(_){var _tS=_sC(_tr,_),_tT=A(_tv,[_]),_tU=E(_tT);if(!_tU[0]){return _0;}else{var _tV=A(_tw,[_tU[1],_]);return _0;}});case 5:return _t8(_tA,_tB,function(_tW,_){var _tX=_sC([0,_tq,[2,E(_tW)]],_),_tY=A(_tv,[_]),_tZ=E(_tY);if(!_tZ[0]){return _0;}else{var _u0=A(_tw,[_tZ[1],_]);return _0;}});case 6:return _t8(_tA,_tB,function(_u1,_){var _u2=_sC([0,_tq,[2,E(_u1)]],_),_u3=A(_tv,[_]),_u4=E(_u3);if(!_u4[0]){return _0;}else{var _u5=A(_tw,[_u4[1],_]);return _0;}});case 7:return _t8(_tA,_tB,function(_){var _u6=A(_tv,[_]),_u7=E(_u6);if(!_u7[0]){return _0;}else{var _u8=A(_tw,[_u7[1],_]);return _0;}});case 8:return _t8(_tA,_tB,function(_u9,_ua,_){var _ub=_sC([0,_tq,[1,_u9,E(_ua)]],_),_uc=A(_tv,[_]),_ud=E(_uc);if(!_ud[0]){return _0;}else{var _ue=A(_tw,[_ud[1],_]);return _0;}});case 9:return _t8(_tA,_tB,function(_uf,_ug,_){var _uh=_sC([0,_tq,[1,_uf,E(_ug)]],_),_ui=A(_tv,[_]),_uj=E(_ui);if(!_uj[0]){return _0;}else{var _uk=A(_tw,[_uj[1],_]);return _0;}});case 10:return _t8(_tA,_tB,function(_ul,_um,_){var _un=_sC([0,_tq,[1,_ul,E(_um)]],_),_uo=A(_tv,[_]),_up=E(_uo);if(!_up[0]){return _0;}else{var _uq=A(_tw,[_up[1],_]);return _0;}});case 11:return _t8(_tA,_tB,function(_ur,_us,_){var _ut=_sC([0,_tq,[1,_ur,E(_us)]],_),_uu=A(_tv,[_]),_uv=E(_uu);if(!_uv[0]){return _0;}else{var _uw=A(_tw,[_uv[1],_]);return _0;}});case 12:return _t8(_tA,_tB,function(_ux,_){var _uy=_sC([0,_tq,[3,_ux]],_),_uz=A(_tv,[_]),_uA=E(_uz);if(!_uA[0]){return _0;}else{var _uB=A(_tw,[_uA[1],_]);return _0;}});case 13:return _t8(_tA,_tB,function(_uC,_){var _uD=_sC([0,_tq,[3,_uC]],_),_uE=A(_tv,[_]),_uF=E(_uE);if(!_uF[0]){return _0;}else{var _uG=A(_tw,[_uF[1],_]);return _0;}});default:return _t8(_tA,_tB,function(_uH,_){var _uI=_sC([0,_tq,[3,_uH]],_),_uJ=A(_tv,[_]),_uK=E(_uJ);if(!_uK[0]){return _0;}else{var _uL=A(_tw,[_uK[1],_]);return _0;}});}}),_tz[2]],_ty[2]];};},_uM=new T(function(){return _tn(_s6,_9H);}),_uN=function(_uO,_){var _uP=A(_uM,[_uO,_]),_uQ=E(_uP),_uR=E(_uQ[1]);return [0,[0,function(_uS,_){var _uT=A(_uR[1],[_uS,_]),_uU=_5k(_uS,_);return _uS;},_uR[2]],_uQ[2]];},_uV=new T(function(){return [1,_uN,_uV];}),_uW=function(_uX,_uY){var _uZ=E(_uX);if(!_uZ){return [0];}else{var _v0=E(_uY);return _v0[0]==0?[0]:[1,_v0[1],new T(function(){return _uW(_uZ-1|0,_v0[2]);})];}},_v1=function(_v2,_v3){return _v2<0?[0]:_uW(_v2,_v3);},_v4=function(_v5,_v6){var _v7=E(_v5)[1];return _v7>0?_v1(_v7,_v6):[0];},_v8=function(_v9){return E(_v9);},_va=function(_vb){var _vc=new T(function(){return _9A(_5C,_v4(_vb,_uV));}),_vd=new T(function(){return _58(_p,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1N(_3M(0,E(_vb)[1],_9),_5y);}));}));});return function(_ve,_){var _vf=_4B(_vc,_5q,_ve,_),_vg=E(_vf),_vh=E(_vg[1]),_vi=new T(function(){return _58(_v8,_vh[1]);});return [0,[0,function(_vj,_){var _vk=A(_vd,[_vj,_]),_vl=A(_vi,[_vj,_]);return _vj;},_vh[2]],_vg[2]];};},_vm=new T(function(){return _va(_55);}),_vn=unCStr(" A counter. wcallback erases the previous rendering of the widget an regenerates it again "),_vo=new T(function(){return _58(_p,_vn);}),_vp=[8,coercionToken],_vq=function(_vr){return _aM(_mI(function(_vs){var _vt=E(_vs);return _vt[0]==0?A(_vr,[_vt[1]]):[2];}),new T(function(){return _mO(_vu,_vr);}));},_vu=function(_vv,_vw){return _vq(_vw);},_vx=function(_vy){return _aM(_aM(_mI(function(_vz){var _vA=E(_vz);return _vA[0]==1?A(_vy,[_vA[1]]):[2];}),new T(function(){return _nw(_vu,_vy);})),new T(function(){return _mO(_vB,_vy);}));},_vB=function(_vC,_vD){return _vx(_vD);},_vE=new T(function(){return _mO(_vB,_bN);}),_vF=new T(function(){return _nw(_vu,_bN);}),_vG=function(_vH){var _vI=E(_vH);return _vI[0]==1?[3,_vI[1],_bM]:[2];},_vJ=new T(function(){return _mf(_vG);}),_vK=function(_vL){return E(_vJ);},_vM=function(_vN){return A(_kw,[_vN,_vK]);},_vO=[1,_vM],_vP=new T(function(){return _aM(_vO,_vF);}),_vQ=new T(function(){return _aM(_vP,_vE);}),_vR=function(_nk){return _aC(_vQ,_nk);},_vS=new T(function(){return _vq(_bN);}),_vT=function(_nk){return _aC(_vS,_nk);},_vU=function(_vV){return E(_vT);},_vW=[0,_vU,_vR,_vu,_vB],_vX=function(_vY){return E(E(_vY)[4]);},_vZ=function(_w0,_w1,_w2){return _nw(new T(function(){return _vX(_w0);}),_w2);},_w3=function(_w4){var _w5=new T(function(){return _nw(new T(function(){return _vX(_w4);}),_bN);});return function(_cg){return _aC(_w5,_cg);};},_w6=function(_w7,_w8){var _w9=new T(function(){return A(_vX,[_w7,_w8,_bN]);});return function(_cg){return _aC(_w9,_cg);};},_wa=function(_wb){return [0,function(_nk){return _w6(_wb,_nk);},new T(function(){return _w3(_wb);}),new T(function(){return _vX(_wb);}),function(_nj,_nk){return _vZ(_wb,_nj,_nk);}];},_wc=new T(function(){return _wa(_vW);}),_wd=unCStr("Prelude.(!!): negative index\n"),_we=new T(function(){return err(_wd);}),_wf=unCStr("Prelude.(!!): index too large\n"),_wg=new T(function(){return err(_wf);}),_wh=function(_wi,_wj){while(1){var _wk=E(_wi);if(!_wk[0]){return E(_wg);}else{var _wl=E(_wj);if(!_wl){return E(_wk[1]);}else{_wi=_wk[2];_wj=_wl-1|0;continue;}}}},_wm=unCStr("ACK"),_wn=unCStr("BEL"),_wo=unCStr("BS"),_wp=unCStr("SP"),_wq=[1,_wp,_9],_wr=unCStr("US"),_ws=[1,_wr,_wq],_wt=unCStr("RS"),_wu=[1,_wt,_ws],_wv=unCStr("GS"),_ww=[1,_wv,_wu],_wx=unCStr("FS"),_wy=[1,_wx,_ww],_wz=unCStr("ESC"),_wA=[1,_wz,_wy],_wB=unCStr("SUB"),_wC=[1,_wB,_wA],_wD=unCStr("EM"),_wE=[1,_wD,_wC],_wF=unCStr("CAN"),_wG=[1,_wF,_wE],_wH=unCStr("ETB"),_wI=[1,_wH,_wG],_wJ=unCStr("SYN"),_wK=[1,_wJ,_wI],_wL=unCStr("NAK"),_wM=[1,_wL,_wK],_wN=unCStr("DC4"),_wO=[1,_wN,_wM],_wP=unCStr("DC3"),_wQ=[1,_wP,_wO],_wR=unCStr("DC2"),_wS=[1,_wR,_wQ],_wT=unCStr("DC1"),_wU=[1,_wT,_wS],_wV=unCStr("DLE"),_wW=[1,_wV,_wU],_wX=unCStr("SI"),_wY=[1,_wX,_wW],_wZ=unCStr("SO"),_x0=[1,_wZ,_wY],_x1=unCStr("CR"),_x2=[1,_x1,_x0],_x3=unCStr("FF"),_x4=[1,_x3,_x2],_x5=unCStr("VT"),_x6=[1,_x5,_x4],_x7=unCStr("LF"),_x8=[1,_x7,_x6],_x9=unCStr("HT"),_xa=[1,_x9,_x8],_xb=[1,_wo,_xa],_xc=[1,_wn,_xb],_xd=[1,_wm,_xc],_xe=unCStr("ENQ"),_xf=[1,_xe,_xd],_xg=unCStr("EOT"),_xh=[1,_xg,_xf],_xi=unCStr("ETX"),_xj=[1,_xi,_xh],_xk=unCStr("STX"),_xl=[1,_xk,_xj],_xm=unCStr("SOH"),_xn=[1,_xm,_xl],_xo=unCStr("NUL"),_xp=[1,_xo,_xn],_xq=[0,92],_xr=unCStr("\\DEL"),_xs=unCStr("\\a"),_xt=unCStr("\\\\"),_xu=unCStr("\\SO"),_xv=unCStr("\\r"),_xw=unCStr("\\f"),_xx=unCStr("\\v"),_xy=unCStr("\\n"),_xz=unCStr("\\t"),_xA=unCStr("\\b"),_xB=function(_xC,_xD){if(_xC<=127){var _xE=E(_xC);switch(_xE){case 92:return _1N(_xt,_xD);case 127:return _1N(_xr,_xD);default:if(_xE<32){var _xF=E(_xE);switch(_xF){case 7:return _1N(_xs,_xD);case 8:return _1N(_xA,_xD);case 9:return _1N(_xz,_xD);case 10:return _1N(_xy,_xD);case 11:return _1N(_xx,_xD);case 12:return _1N(_xw,_xD);case 13:return _1N(_xv,_xD);case 14:return _1N(_xu,new T(function(){var _xG=E(_xD);return _xG[0]==0?[0]:E(E(_xG[1])[1])==72?unAppCStr("\\&",_xG):E(_xG);}));default:return _1N([1,_xq,new T(function(){var _xH=_xF;return _xH>=0?_wh(_xp,_xH):E(_we);})],_xD);}}else{return [1,[0,_xE],_xD];}}}else{return [1,_xq,new T(function(){var _xI=jsShowI(_xC);return _1N(fromJSStr(_xI),new T(function(){var _xJ=E(_xD);if(!_xJ[0]){return [0];}else{var _xK=E(_xJ[1])[1];return _xK<48?E(_xJ):_xK>57?E(_xJ):unAppCStr("\\&",_xJ);}}));})];}},_xL=[0,39],_xM=[1,_xL,_9],_xN=unCStr("\'\\\'\'"),_xO=function(_xP){var _xQ=E(E(_xP)[1]);return _xQ==39?E(_xN):[1,_xL,new T(function(){return _xB(_xQ,_xM);})];},_xR=[0,34],_xS=unCStr("\\\""),_xT=function(_xU,_xV){var _xW=E(_xU);if(!_xW[0]){return E(_xV);}else{var _xX=_xW[2],_xY=E(E(_xW[1])[1]);return _xY==34?_1N(_xS,new T(function(){return _xT(_xX,_xV);})):_xB(_xY,new T(function(){return _xT(_xX,_xV);}));}},_xZ=function(_y0,_y1){return [1,_xR,new T(function(){return _xT(_y0,[1,_xR,_y1]);})];},_y2=function(_y3){return _1N(_xN,_y3);},_y4=function(_y5,_y6){var _y7=E(E(_y6)[1]);return _y7==39?E(_y2):function(_y8){return [1,_xL,new T(function(){return _xB(_y7,[1,_xL,_y8]);})];};},_y9=[0,_y4,_xO,_xZ],_ya=function(_yb){return E(E(_yb)[3]);},_yc=function(_yd,_ye){return A(_ya,[_yd,_ye,_9]);},_yf=function(_yg,_yh,_yi){return _2D(new T(function(){return _ya(_yg);}),_yh,_yi);},_yj=function(_yk){var _yl=new T(function(){return _ya(_yk);});return [0,function(_ym){return E(_yl);},function(_y3){return _yc(_yk,_y3);},function(_yn,_y3){return _yf(_yk,_yn,_y3);}];},_yo=new T(function(){return _yj(_y9);}),_yp=unCStr("submit"),_yq=new T(function(){return A(_rT,[_8D,_9J,_p3,_yo,_wc,_a,_yp]);}),_yr=[0,43],_ys=[1,_yr,_9],_yt=[1,_ys],_yu=new T(function(){return A(_yq,[_yt]);}),_yv=new T(function(){return _tn(_yu,_vp);}),_yw=function(_X,_){return _Z(_Y,_X,_);},_yx=function(_yy,_yz,_yA,_){var _yB=A(_yz,[_yA,_]),_yC=E(_yB),_yD=E(_yC[1]);return [0,[0,function(_yE,_){var _yF=_Z(_Y,_yE,_),_yG=A(_1,[_n,_yF,_I,_yy,_]),_yH=A(_yD[1],[_yF,_]);return _yF;},_yD[2]],_yC[2]];},_yI=new T(function(){return _3R(_1a,_3E,_18,_15);}),_yJ=new T(function(){return _3R(_1a,_3E,_18,_15);}),_yK=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_yL=function(_yM){return _j(function(_){var _=0;return eval(E(_yM)[1]);});},_yN=new T(function(){return _yL(_yK);}),_yO=function(_yP,_yQ,_yR,_){var _yS=A(_yI,[_yR,_]),_yT=A(_yJ,[new T(function(){return E(E(_yS)[2]);}),_]),_yU=E(_yT),_yV=_yU[1],_yW=E(_yU[2]),_yX=_yW[2],_yY=E(_yW[4]),_yZ=new T(function(){return E(E(_yS)[1]);}),_z0=function(_z1){var _z2=new T(function(){return A(_yQ,[_z1]);});return function(_z3,_){var _z4=A(_z2,[_z3,_]),_z5=E(_z4),_z6=E(_z5[1]);return [0,[0,function(_z7,_){var _z8=A(_z6[1],[_z7,_]),_z9=E(_yZ),_za=jsFind(toJSStr(_z9)),_zb=E(_za);if(!_zb[0]){return _45(_z9);}else{var _zc=E(_zb[1]),_zd=A(_yN,[E(_zc[1]),_]),_ze=jsKillChild(E(_zc)[1],_zd);return _z7;}},_z6[2]],_z5[2]];};},_zf=_yx(_yZ,_yP,[0,_yW[1],_yX,_yW[3],[0,function(_){return _47(function(_zg,_){var _zh=_yx(_yZ,_yP,new T(function(){var _zi=E(_zg);return [0,_zi[1],_yX,_zi[3],_zi[4],_zi[5]];}),_);return [0,[0,_34,E(E(_zh)[1])[2]],_zg];},_yV,_);},function(_zj,_){var _zk=_47(new T(function(){return _z0(_zj);}),_yV,_),_zl=E(_zk);return _zl[0]==0?_a:A(_yY[2],[_zl[1],_]);}],_yW[5]],_),_zm=E(_zf),_zn=_zm[2],_zo=E(_zm[1]),_zp=_zo[1],_zq=new T(function(){return _Q(_yw,[1,[0,_I,_yV],_9]);}),_zr=E(_zo[2]);if(!_zr[0]){return [0,[0,function(_zs,_){var _zt=A(_zp,[_zs,_]),_zu=A(_zq,[_zs,_]);return _zs;},_a],new T(function(){var _zv=E(_zn);return [0,_zv[1],_zv[2],_zv[3],_yY,_zv[5]];})];}else{var _zw=A(_z0,[_zr[1],new T(function(){var _zx=E(_zn);return [0,_zx[1],_zx[2],_zx[3],_yY,_zx[5]];}),_]),_zy=E(_zw),_zz=E(_zy[1]);return [0,[0,function(_zA,_){var _zB=A(_zp,[_zA,_]),_zC=A(_zq,[_zA,_]),_zD=A(_zz[1],[_zC,_]);return _zA;},_zz[2]],_zy[2]];}},_zE=function(_zF){var _zG=new T(function(){return _zE(new T(function(){return [0,E(_zF)[1]+1|0];}));}),_zH=new T(function(){return _w(_p,new T(function(){return _5h(_zF);}));});return function(_cg,_s3){return _yO(function(_zI,_){var _zJ=A(_yv,[_zI,_]),_zK=E(_zJ),_zL=E(_zK[1]);return [0,[0,function(_zM,_){var _zN=A(_zH,[_zM,_]),_zO=A(_zL[1],[_zM,_]);return _zM;},_zL[2]],_zK[2]];},function(_zP){return E(_zG);},_cg,_s3);};},_zQ=function(_zR){var _zS=new T(function(){return _zE(_zR);});return function(_zT,_){var _zU=A(_zS,[_zT,_]),_zV=E(_zU),_zW=E(_zV[1]);return [0,[0,function(_zX,_){var _zY=A(_vo,[_zX,_]),_zZ=_5k(_zX,_),_A0=A(_zW[1],[_zX,_]);return _zX;},_zW[2]],_zV[2]];};},_A1=new T(function(){return _zQ(_55);}),_A2=[0,4],_A3=function(_A4,_A5){return [1,_A5,new T(function(){return _A3(_A4,new T(function(){return A(_A4,[_A5]);}));})];},_A6=[0,1],_A7=[1,_A6,_9],_A8=[1,_5z,_9],_A9=function(_Aa,_Ab,_Ac){var _Ad=E(_Ab);if(!_Ad[0]){return [0];}else{var _Ae=E(_Ac);return _Ae[0]==0?[0]:[1,new T(function(){return A(_Aa,[_Ad[1],_Ae[1]]);}),new T(function(){return _A9(_Aa,_Ad[2],_Ae[2]);})];}},_Af=function(_Ag){return _A9(_8S,[1,_5z,_Ag],new T(function(){return _1N(_Ag,_A8);}));},_Ah=new T(function(){return _A3(_Af,_A7);}),_Ai=unCStr(" rows of the Pascal triangle "),_Aj=function(_Ak){var _Al=new T(function(){return _2D(_o3,_Ak,_9);});return function(_cg,_s3){return _p(_Al,_cg,_s3);};},_Am=function(_An,_Ao){var _Ap=new T(function(){return _58(_Aj,_An);});return [1,function(_Aq,_){var _Ar=A(_Ap,[_Aq,_]),_As=A(_1,[_n,_Ar,_53,_H,_]);return _Ar;},_Ao];},_At=function(_Au,_Av){var _Aw=E(_Au);if(!_Aw[0]){return [0];}else{var _Ax=_Aw[1];return _Av>1?_Am(_Ax,new T(function(){return _At(_Aw[2],_Av-1|0);})):_Am(_Ax,_9);}},_Ay=function(_Az){var _AA=new T(function(){return _58(_p,new T(function(){return unAppCStr("Show ",new T(function(){return _1N(_3M(0,E(_Az)[1],_9),_Ai);}));}));});return function(_AB,_){return [0,[0,function(_AC,_){var _AD=A(_AA,[_AC,_]),_AE=_8q(new T(function(){var _AF=E(_Az)[1];return _AF>0?_At(_Ah,_AF):[0];}),_AC,_);return _AC;},_a],_AB];};},_AG=new T(function(){return _Ay(_A2);}),_AH=unCStr("center"),_AI=function(_AJ,_AK){var _AL=new T(function(){return A(_AJ,[_AK]);});return function(_AM,_){var _AN=jsCreateElem(toJSStr(E(_AH))),_AO=jsAppendChild(_AN,E(_AM)[1]),_AP=[0,_AN],_AQ=A(_AL,[_AP,_]);return _AP;};},_AR=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_AS=new T(function(){return _58(_p,_AR);}),_AT=function(_AU){var _AV=jsShow(E(_AU)[1]);return fromJSStr(_AV);},_AW=function(_AX){var _AY=new T(function(){return _AT(_AX);});return function(_cg){return _1N(_AY,_cg);};},_AZ=function(_B0,_B1,_B2){var _B3=E(_B2);if(!_B3[0]){return [0];}else{var _B4=_B3[2],_B5=E(_B3[1]);return _B0!=_B5[1]?[1,_B5,new T(function(){return _AZ(_B0,_B1,_B4);})]:_1N(_B1,new T(function(){return _AZ(_B0,_B1,_B4);}));}},_B6=[0,45],_B7=function(_B8,_B9,_Ba){var _Bb=new T(function(){return A(_B8,[[0, -_Ba]]);}),_Bc=new T(function(){return E(_B9)[1]<=6?function(_Bd){return [1,_B6,new T(function(){return A(_Bb,[_Bd]);})];}:function(_Be){return [1,_3L,[1,_B6,new T(function(){return A(_Bb,[[1,_3K,_Be]]);})]];};});if(_Ba>=0){var _Bf=isDoubleNegativeZero(_Ba);return E(_Bf)==0?A(_B8,[[0,_Ba]]):E(_Bc);}else{return E(_Bc);}},_Bg=unCStr("canvas"),_Bh=unCStr("id"),_Bi=unCStr("canvas"),_Bj=function(_Bk,_Bl){var _Bm=new T(function(){return A(_Bk,[_Bl]);});return function(_Bn,_){var _Bo=jsCreateElem(toJSStr(E(_Bi))),_Bp=jsAppendChild(_Bo,E(_Bn)[1]),_Bq=[0,_Bo],_Br=A(_Bm,[_Bq,_]);return _Bq;};},_Bs=new T(function(){return _Bj(_v8,_34);}),_Bt=function(_Bu,_){var _Bv=A(_Bs,[_Bu,_]),_Bw=A(_1,[_n,_Bv,_Bh,_Bg,_]);return _Bv;},_Bx=[1,_0],_By=[0,_Bt,_Bx],_Bz=function(_BA,_){return [0,_By,_BA];},_BB=unCStr("Pattern match failure in do expression at main.hs:164:5-12"),_BC=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_BD=new T(function(){return _yL(_BC);}),_BE=function(_BF,_){var _BG=jsHasCtx2D(_BF);if(!E(_BG)){return _a;}else{var _BH=jsGetCtx2D(_BF);return [1,[0,[0,_BH],[0,_BF]]];}},_BI=function(_BJ,_){return _BE(E(_BJ)[1],_);},_BK=function(_BL,_BM){return A(_BL,[function(_){var _BN=jsFind(toJSStr(E(_BM))),_BO=E(_BN);return _BO[0]==0?_a:_BI(_BO[1],_);}]);},_BP=new T(function(){return _BK(_n,_Bg);}),_BQ=[0,-10],_BR=[0,0],_BS=[0,_BQ,_BR],_BT=[0,10],_BU=[0,_BT,_BR],_BV=[1,_BU,_9],_BW=[1,_BS,_BV],_BX=function(_BY,_){return _0;},_BZ=function(_C0){var _C1=E(_C0);if(!_C1[0]){return E(_BX);}else{var _C2=E(_C1[1]);return function(_C3,_){var _C4=E(_C3)[1],_C5=jsMoveTo(_C4,E(_C2[1])[1],E(_C2[2])[1]);return (function(_C6,_){while(1){var _C7=E(_C6);if(!_C7[0]){return _0;}else{var _C8=E(_C7[1]),_C9=jsLineTo(_C4,E(_C8[1])[1],E(_C8[2])[1]);_C6=_C7[2];continue;}}})(_C1[2],_);};}},_Ca=new T(function(){return _BZ(_BW);}),_Cb=[0,30],_Cc=[0,_BR,_Cb],_Cd=[0,-30],_Ce=[0,_BR,_Cd],_Cf=[1,_Ce,_9],_Cg=[1,_Cc,_Cf],_Ch=new T(function(){return _BZ(_Cg);}),_Ci=new T(function(){return [0,0/0];}),_Cj=new T(function(){return [0,-1/0];}),_Ck=new T(function(){return [0,1/0];}),_Cl=[0,0],_Cm=function(_Cn,_Co){while(1){var _Cp=E(_Cn);if(!_Cp[0]){_Cn=[1,I_fromInt(_Cp[1])];continue;}else{var _Cq=E(_Co);if(!_Cq[0]){_Cn=_Cp;_Co=[1,I_fromInt(_Cq[1])];continue;}else{return I_fromRat(_Cp[1],_Cq[1]);}}}},_Cr=function(_Cs,_Ct){var _Cu=E(_Cs);if(!_Cu[0]){var _Cv=_Cu[1],_Cw=E(_Ct);return _Cw[0]==0?_Cv==_Cw[1]:I_compareInt(_Cw[1],_Cv)==0?true:false;}else{var _Cx=_Cu[1],_Cy=E(_Ct);return _Cy[0]==0?I_compareInt(_Cx,_Cy[1])==0?true:false:I_compare(_Cx,_Cy[1])==0?true:false;}},_Cz=function(_CA,_CB){var _CC=E(_CA);if(!_CC[0]){var _CD=_CC[1],_CE=E(_CB);return _CE[0]==0?_CD<_CE[1]:I_compareInt(_CE[1],_CD)>0;}else{var _CF=_CC[1],_CG=E(_CB);return _CG[0]==0?I_compareInt(_CF,_CG[1])<0:I_compare(_CF,_CG[1])<0;}},_CH=function(_CI,_CJ){return !_Cr(_CJ,_Cl)?[0,_Cm(_CI,_CJ)]:!_Cr(_CI,_Cl)?!_Cz(_CI,_Cl)?E(_Ck):E(_Cj):E(_Ci);},_CK=function(_CL){var _CM=E(_CL);return _CH(_CM[1],_CM[2]);},_CN=function(_CO){return [0,1/E(_CO)[1]];},_CP=function(_CQ){var _CR=E(_CQ),_CS=_CR[1];return _CS<0?[0, -_CS]:E(_CR);},_CT=function(_CU){var _CV=E(_CU);return _CV[0]==0?_CV[1]:I_toNumber(_CV[1]);},_CW=function(_CX){return [0,_CT(_CX)];},_CY=[0,0],_CZ=[0,1],_D0=[0,-1],_D1=function(_D2){var _D3=E(_D2)[1];return _D3!=0?_D3<=0?E(_D0):E(_CZ):E(_CY);},_D4=function(_D5,_D6){return [0,E(_D5)[1]-E(_D6)[1]];},_D7=function(_D8){return [0, -E(_D8)[1]];},_D9=function(_Da,_Db){return [0,E(_Da)[1]+E(_Db)[1]];},_Dc=function(_Dd,_De){return [0,E(_Dd)[1]*E(_De)[1]];},_Df=[0,_D9,_Dc,_D4,_D7,_CP,_D1,_CW],_Dg=function(_Dh,_Di){return [0,E(_Dh)[1]/E(_Di)[1]];},_Dj=[0,_Df,_Dg,_CN,_CK],_Dk=function(_Dl,_Dm){return E(_Dl)[1]!=E(_Dm)[1]?true:false;},_Dn=function(_Do,_Dp){return E(_Do)[1]==E(_Dp)[1];},_Dq=[0,_Dn,_Dk],_Dr=function(_Ds,_Dt){return E(_Ds)[1]<E(_Dt)[1];},_Du=function(_Dv,_Dw){return E(_Dv)[1]<=E(_Dw)[1];},_Dx=function(_Dy,_Dz){return E(_Dy)[1]>E(_Dz)[1];},_DA=function(_DB,_DC){return E(_DB)[1]>=E(_DC)[1];},_DD=function(_DE,_DF){var _DG=E(_DE)[1],_DH=E(_DF)[1];return _DG>=_DH?_DG!=_DH?2:1:0;},_DI=function(_DJ,_DK){var _DL=E(_DJ),_DM=E(_DK);return _DL[1]>_DM[1]?E(_DL):E(_DM);},_DN=function(_DO,_DP){var _DQ=E(_DO),_DR=E(_DP);return _DQ[1]>_DR[1]?E(_DR):E(_DQ);},_DS=[0,_Dq,_DD,_Dr,_DA,_Dx,_Du,_DI,_DN],_DT=[0,1],_DU=function(_DV){return E(E(_DV)[1]);},_DW=function(_DX){return E(E(_DX)[2]);},_DY=function(_DZ){return E(E(_DZ)[6]);},_E0=[0,2],_E1=function(_E2,_E3){var _E4=E(_E3);return [1,_E4,new T(function(){var _E5=_DU(_E2);return _E1(_E2,A(_E5[1],[_E4,new T(function(){return A(_E5[7],[_DT]);})]));})];},_E6=function(_E7,_E8){var _E9=E(_E8);if(!_E9[0]){return [0];}else{var _Ea=_E9[1];return !A(_E7,[_Ea])?[0]:[1,_Ea,new T(function(){return _E6(_E7,_E9[2]);})];}},_Eb=function(_Ec,_Ed,_Ee,_Ef){var _Eg=new T(function(){return _DY(_Ec);});return _E6(function(_Eh){return A(_Eg,[_Eh,new T(function(){var _Ei=_DU(_Ed),_Ej=_Ei[7];return A(_Ei[1],[_Ef,new T(function(){return A(_DW,[_Ed,new T(function(){return A(_Ej,[_DT]);}),new T(function(){return A(_Ej,[_E0]);})]);})]);})]);},_E1(_Ed,_Ee));},_Ek=new T(function(){return _Eb(_DS,_Dj,_BQ,_BT);}),_El=function(_Em,_En){var _Eo=E(_Em);if(!_Eo[0]){return [0];}else{var _Ep=E(_En);return _Ep[0]==0?[0]:[1,[0,_Eo[1],_Ep[1]],new T(function(){return _El(_Eo[2],_Ep[2]);})];}},_Eq=function(_Er,_Es,_){var _Et=function(_Eu,_){var _Ev=E(_Eu);if(!_Ev[0]){return _9;}else{var _Ew=A(_BD,[E(toJSStr(_AZ(120,new T(function(){return A(_B7,[_AW,_pa,E(_Ev[1])[1],_9]);}),_Er))),_]),_Ex=_Et(_Ev[2],_);return [1,[0,_Ew],_Ex];}};return _4B(_Bz,function(_Ey,_Ez,_){return (function(_EA,_){return [0,[0,function(_EB,_){var _EC=A(_BP,[_]),_ED=E(_EC);if(!_ED[0]){var _EE=_32(_BB,_);return _EB;}else{var _EF=_Et(_Ek,_),_EG=E(_ED[1]),_EH=jsResetCanvas(E(_EG[2])[1]),_EI=E(_EG[1]),_EJ=_EI[1],_EK=jsPushState(_EJ),_EL=jsScale(_EJ,3,1),_EM=jsPushState(_EJ),_EN=jsTranslate(_EJ,50,130),_EO=jsPushState(_EJ),_EP=jsRotate(_EJ,3.141592653589793),_EQ=jsBeginPath(_EJ),_ER=A(_Ca,[_EI,_]),_ES=A(_Ch,[_EI,_]),_ET=A(_BZ,[_El(_Ek,_EF),_EI,_]),_EU=jsStroke(_EJ),_EV=jsPopState(_EJ),_EW=jsPopState(_EJ),_EX=jsPopState(_EJ);return _EB;}},_Bx],_EA];})(_Ez,_);},_Es,_);},_EY=unCStr("Math.pow(x,2)+x+10;"),_EZ=[1,_EY],_F0=function(_F1,_F2,_){return [0,[0,_34,[1,_F1]],_F2];},_F3=function(_F4,_F5,_F6,_){return _4B(_F4,function(_F7){return E(_F5);},_F6,_);},_F8=function(_F9,_Fa,_X,_){return _F3(_F9,_Fa,_X,_);},_Fb=function(_Fc){return err(_Fc);},_Fd=[0,_4B,_F8,_F0,_Fb],_Fe=function(_Ff){return E(E(_Ff)[1]);},_Fg=function(_Fh,_Fi,_Fj,_Fk,_Fl){var _Fm=new T(function(){return _90(_Fh);}),_Fn=new T(function(){return _Fe(_Fm);}),_Fo=new T(function(){return _3x(_Fi);}),_Fp=new T(function(){return _p6(_Fh);}),_Fq=new T(function(){return _37(_Fi);}),_Fr=new T(function(){return _3x(_Fi);}),_Fs=new T(function(){return _37(_Fi);});return A(_Fj,[function(_Ft){return A(_Fs,[new T(function(){return A(_Fk,[_Ft]);}),function(_Fu){var _Fv=E(_Fu),_Fw=E(_Fv[1]);return A(_Fr,[[0,[0,_Fw[1],[1,_Fw[2]]],_Fv[2]]]);}]);},function(_Fx){var _Fy=E(_Fx);if(!_Fy[0]){return function(_Fz){return A(_Fo,[[0,[0,_Fn,_a],_Fz]]);};}else{var _FA=new T(function(){return A(_Fl,[_Fy[1]]);});return function(_FB){return A(_Fq,[new T(function(){return A(_FA,[_FB]);}),function(_FC){var _FD=E(_FC),_FE=_FD[2],_FF=E(_FD[1]);return _FF[0]==0?A(_Fo,[[0,[0,_Fn,_Fy],_FE]]):A(_Fo,[[0,[0,new T(function(){return A(_Fp,[_FF[1]]);}),_a],_FE]]);}]);};}}]);},_FG=function(_FH,_FI,_FJ){var _FK=new T(function(){return A(_oR,[_FH,_9]);}),_FL=new T(function(){return _ok(_FJ);}),_FM=new T(function(){return _3x(_FL);}),_FN=new T(function(){return _rT(_FH,_FJ,_p3,_yo,_wc);});return function(_FO){return _Fg(_FH,_FL,E(_FI)[1],new T(function(){return A(_FN,[_a,_9I,_FO]);}),function(_FP,_FQ){return E(_FP)[0]==0?A(_FM,[[0,[1,_FK],_FQ]]):A(_FM,[[0,_a,_FQ]]);});};},_FR=new T(function(){return _FG(_8D,_Fd,_9J);}),_FS=new T(function(){return A(_FR,[_EZ]);}),_FT=new T(function(){return _tn(_FS,_9H);}),_FU=function(_FV,_){var _FW=A(_FT,[_FV,_]),_FX=E(_FW),_FY=E(_FX[1]);return [0,[0,function(_FZ,_){var _G0=A(_FY[1],[_FZ,_]),_G1=_5k(_FZ,_);return _FZ;},new T(function(){var _G2=E(_FY[2]);return _G2[0]==0?E(_EZ):E(_G2);})],_FX[2]];},_G3=function(_G4,_){var _G5=_4B(_FU,_Eq,_G4,_),_G6=E(_G5),_G7=E(_G6[1]),_G8=new T(function(){return _AI(_v8,_G7[1]);});return [0,[0,function(_G9,_){var _Ga=A(_AS,[_G9,_]),_Gb=A(_G8,[_G9,_]);return _G9;},_G7[2]],_G6[2]];},_Gc=[1,_5z],_Gd=function(_Ge,_){return _Ge;},_Gf=unCStr("main"),_Gg=unCStr("Main"),_Gh=unCStr("GalleryIndex"),_Gi=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Gf,_Gg,_Gh],_Gj=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Gi,_9],_Gk=function(_Gl){return E(_Gj);},_Gm=function(_Gn,_Go){var _Gp=hs_leWord64(_Gn,_Go);return E(_Gp)==0?false:true;},_Gq=function(_Gr,_Gs,_Gt,_Gu){var _Gv=hs_eqWord64(_Gr,_Gt);if(!E(_Gv)){var _Gw=hs_leWord64(_Gr,_Gt);return E(_Gw)==0?false:true;}else{return _Gm(_Gs,_Gu);}},_Gx=function(_Gy,_Gz){var _GA=E(_Gy),_GB=_GA[1],_GC=_GA[2],_GD=E(_Gz),_GE=_GD[1],_GF=_GD[2],_GG=hs_eqWord64(_GB,_GE);if(!E(_GG)){return !_Gq(_GB,_GC,_GE,_GF)?2:0;}else{var _GH=hs_eqWord64(_GC,_GF);return E(_GH)==0?!_Gq(_GB,_GC,_GE,_GF)?2:0:1;}},_GI=function(_GJ,_GK,_GL,_GM,_GN){while(1){var _GO=E(_GN);if(!_GO[0]){switch(_Gx([0,_GJ,_GK,_GL,_GM],_GO[2])){case 0:_GN=_GO[4];continue;case 1:return [1,_GO[3]];default:_GN=_GO[5];continue;}}else{return [0];}}},_GP=function(_GQ,_GR){var _GS=E(_GQ),_GT=_GS[1],_GU=_GS[2],_GV=_GS[3],_GW=_GS[4],_GX=E(_GR);if(!_GX[0]){switch(_Gx(_GS,_GX[2])){case 0:return _GI(_GT,_GU,_GV,_GW,_GX[4]);case 1:return [1,_GX[3]];default:return _GI(_GT,_GU,_GV,_GW,_GX[5]);}}else{return [0];}},_GY=function(_GZ,_H0,_H1,_H2){var _H3=E(_H0),_H4=_H3[1],_H5=_H3[3],_H6=new T(function(){return A(_H2,[_p5]);}),_H7=new T(function(){return A(_H5,[_a]);});return A(_H4,[new T(function(){return A(_H4,[_H1,function(_H8){return A(_H5,[new T(function(){var _H9=E(_GZ);return E(E(_H8)[5]);})]);}]);}),function(_Ha){var _Hb=_GP(_H6,_Ha);return _Hb[0]==0?E(_H7):A(_H5,[[1,_Hb[1]]]);}]);},_Hc=new T(function(){return _GY(_1a,_3E,_18,_Gk);}),_Hd=function(_He,_){var _Hf=A(_Hc,[_He,_]);return [0,[0,_Gd,new T(function(){var _Hg=E(E(_Hf)[1]);return _Hg[0]==0?E(_Gc):E(_Hg);})],new T(function(){return E(E(_Hf)[2]);})];},_Hh=unCStr("Failure in Data.Map.balanceL"),_Hi=new T(function(){return err(_Hh);}),_Hj=function(_Hk,_Hl,_Hm,_Hn){var _Ho=E(_Hn);if(!_Ho[0]){var _Hp=_Ho[1],_Hq=E(_Hm);if(!_Hq[0]){var _Hr=_Hq[1],_Hs=_Hq[2],_Ht=_Hq[3];if(_Hr<=(imul(3,_Hp)|0)){return [0,(1+_Hr|0)+_Hp|0,E(E(_Hk)),_Hl,E(_Hq),E(_Ho)];}else{var _Hu=E(_Hq[4]);if(!_Hu[0]){var _Hv=_Hu[1],_Hw=E(_Hq[5]);if(!_Hw[0]){var _Hx=_Hw[1],_Hy=_Hw[2],_Hz=_Hw[3],_HA=_Hw[4];if(_Hx>=(imul(2,_Hv)|0)){var _HB=function(_HC){var _HD=E(_Hw[5]);return _HD[0]==0?[0,(1+_Hr|0)+_Hp|0,E(_Hy),_Hz,E([0,(1+_Hv|0)+_HC|0,E(_Hs),_Ht,E(_Hu),E(_HA)]),E([0,(1+_Hp|0)+_HD[1]|0,E(E(_Hk)),_Hl,E(_HD),E(_Ho)])]:[0,(1+_Hr|0)+_Hp|0,E(_Hy),_Hz,E([0,(1+_Hv|0)+_HC|0,E(_Hs),_Ht,E(_Hu),E(_HA)]),E([0,1+_Hp|0,E(E(_Hk)),_Hl,E(_8),E(_Ho)])];},_HE=E(_HA);return _HE[0]==0?_HB(_HE[1]):_HB(0);}else{return [0,(1+_Hr|0)+_Hp|0,E(_Hs),_Ht,E(_Hu),E([0,(1+_Hp|0)+_Hx|0,E(E(_Hk)),_Hl,E(_Hw),E(_Ho)])];}}else{return E(_Hi);}}else{return E(_Hi);}}}else{return [0,1+_Hp|0,E(E(_Hk)),_Hl,E(_8),E(_Ho)];}}else{var _HF=E(_Hm);if(!_HF[0]){var _HG=_HF[1],_HH=_HF[2],_HI=_HF[3],_HJ=_HF[5],_HK=E(_HF[4]);if(!_HK[0]){var _HL=_HK[1],_HM=E(_HJ);if(!_HM[0]){var _HN=_HM[1],_HO=_HM[2],_HP=_HM[3],_HQ=_HM[4];if(_HN>=(imul(2,_HL)|0)){var _HR=function(_HS){var _HT=E(_HM[5]);return _HT[0]==0?[0,1+_HG|0,E(_HO),_HP,E([0,(1+_HL|0)+_HS|0,E(_HH),_HI,E(_HK),E(_HQ)]),E([0,1+_HT[1]|0,E(E(_Hk)),_Hl,E(_HT),E(_8)])]:[0,1+_HG|0,E(_HO),_HP,E([0,(1+_HL|0)+_HS|0,E(_HH),_HI,E(_HK),E(_HQ)]),E([0,1,E(E(_Hk)),_Hl,E(_8),E(_8)])];},_HU=E(_HQ);return _HU[0]==0?_HR(_HU[1]):_HR(0);}else{return [0,1+_HG|0,E(_HH),_HI,E(_HK),E([0,1+_HN|0,E(E(_Hk)),_Hl,E(_HM),E(_8)])];}}else{return [0,3,E(_HH),_HI,E(_HK),E([0,1,E(E(_Hk)),_Hl,E(_8),E(_8)])];}}else{var _HV=E(_HJ);return _HV[0]==0?[0,3,E(_HV[2]),_HV[3],E([0,1,E(_HH),_HI,E(_8),E(_8)]),E([0,1,E(E(_Hk)),_Hl,E(_8),E(_8)])]:[0,2,E(E(_Hk)),_Hl,E(_HF),E(_8)];}}else{return [0,1,E(E(_Hk)),_Hl,E(_8),E(_8)];}}},_HW=unCStr("Failure in Data.Map.balanceR"),_HX=new T(function(){return err(_HW);}),_HY=function(_HZ,_I0,_I1,_I2){var _I3=E(_I1);if(!_I3[0]){var _I4=_I3[1],_I5=E(_I2);if(!_I5[0]){var _I6=_I5[1],_I7=_I5[2],_I8=_I5[3];if(_I6<=(imul(3,_I4)|0)){return [0,(1+_I4|0)+_I6|0,E(E(_HZ)),_I0,E(_I3),E(_I5)];}else{var _I9=E(_I5[4]);if(!_I9[0]){var _Ia=_I9[1],_Ib=_I9[2],_Ic=_I9[3],_Id=_I9[4],_Ie=E(_I5[5]);if(!_Ie[0]){var _If=_Ie[1];if(_Ia>=(imul(2,_If)|0)){var _Ig=function(_Ih){var _Ii=E(_HZ),_Ij=E(_I9[5]);return _Ij[0]==0?[0,(1+_I4|0)+_I6|0,E(_Ib),_Ic,E([0,(1+_I4|0)+_Ih|0,E(_Ii),_I0,E(_I3),E(_Id)]),E([0,(1+_If|0)+_Ij[1]|0,E(_I7),_I8,E(_Ij),E(_Ie)])]:[0,(1+_I4|0)+_I6|0,E(_Ib),_Ic,E([0,(1+_I4|0)+_Ih|0,E(_Ii),_I0,E(_I3),E(_Id)]),E([0,1+_If|0,E(_I7),_I8,E(_8),E(_Ie)])];},_Ik=E(_Id);return _Ik[0]==0?_Ig(_Ik[1]):_Ig(0);}else{return [0,(1+_I4|0)+_I6|0,E(_I7),_I8,E([0,(1+_I4|0)+_Ia|0,E(E(_HZ)),_I0,E(_I3),E(_I9)]),E(_Ie)];}}else{return E(_HX);}}else{return E(_HX);}}}else{return [0,1+_I4|0,E(E(_HZ)),_I0,E(_I3),E(_8)];}}else{var _Il=E(_I2);if(!_Il[0]){var _Im=_Il[1],_In=_Il[2],_Io=_Il[3],_Ip=_Il[5],_Iq=E(_Il[4]);if(!_Iq[0]){var _Ir=_Iq[1],_Is=_Iq[2],_It=_Iq[3],_Iu=_Iq[4],_Iv=E(_Ip);if(!_Iv[0]){var _Iw=_Iv[1];if(_Ir>=(imul(2,_Iw)|0)){var _Ix=function(_Iy){var _Iz=E(_HZ),_IA=E(_Iq[5]);return _IA[0]==0?[0,1+_Im|0,E(_Is),_It,E([0,1+_Iy|0,E(_Iz),_I0,E(_8),E(_Iu)]),E([0,(1+_Iw|0)+_IA[1]|0,E(_In),_Io,E(_IA),E(_Iv)])]:[0,1+_Im|0,E(_Is),_It,E([0,1+_Iy|0,E(_Iz),_I0,E(_8),E(_Iu)]),E([0,1+_Iw|0,E(_In),_Io,E(_8),E(_Iv)])];},_IB=E(_Iu);return _IB[0]==0?_Ix(_IB[1]):_Ix(0);}else{return [0,1+_Im|0,E(_In),_Io,E([0,1+_Ir|0,E(E(_HZ)),_I0,E(_8),E(_Iq)]),E(_Iv)];}}else{return [0,3,E(_Is),_It,E([0,1,E(E(_HZ)),_I0,E(_8),E(_8)]),E([0,1,E(_In),_Io,E(_8),E(_8)])];}}else{var _IC=E(_Ip);return _IC[0]==0?[0,3,E(_In),_Io,E([0,1,E(E(_HZ)),_I0,E(_8),E(_8)]),E(_IC)]:[0,2,E(E(_HZ)),_I0,E(_8),E(_Il)];}}else{return [0,1,E(E(_HZ)),_I0,E(_8),E(_8)];}}},_ID=function(_IE,_IF,_IG,_IH,_II,_IJ){var _IK=E(_IJ);if(!_IK[0]){var _IL=_IK[2],_IM=_IK[3],_IN=_IK[4],_IO=_IK[5];switch(_Gx([0,_IE,_IF,_IG,_IH],_IL)){case 0:return _Hj(_IL,_IM,_ID(_IE,_IF,_IG,_IH,_II,_IN),_IO);case 1:return [0,_IK[1],E([0,_IE,_IF,_IG,_IH]),_II,E(_IN),E(_IO)];default:return _HY(_IL,_IM,_IN,_ID(_IE,_IF,_IG,_IH,_II,_IO));}}else{return [0,1,E([0,_IE,_IF,_IG,_IH]),_II,E(_8),E(_8)];}},_IP=unCStr("100%"),_IQ=[0,62],_IR=[1,_IQ,_9],_IS=[1,_IR],_IT=new T(function(){return A(_yq,[_IS]);}),_IU=new T(function(){return _tn(_IT,_vp);}),_IV=function(_IW){return E(_IU);},_IX=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_IY=[1,_IX,_9],_IZ=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_J0=[1,_IZ,_IY],_J1=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_J2=[1,_J1,_J0],_J3=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_J4=[1,_J3,_J2],_J5=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_J6=[1,_J5,_J4],_J7=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_J8=[1,_J7,_J6],_J9=unCStr("height"),_Ja=function(_Jb,_Jc){while(1){var _Jd=E(_Jb);if(!_Jd[0]){return E(_Jc);}else{_Jb=_Jd[2];var _Je=_Jc+1|0;_Jc=_Je;continue;}}},_Jf=new T(function(){return [0,_Ja(_J8,0)-1|0];}),_Jg=[0,_34,_5o],_Jh=function(_Ji,_){return [0,[0,_34,[1,_Ji]],_Ji];},_Jj=unCStr("src"),_Jk=unCStr("img"),_Jl=function(_Jm,_Jn){var _Jo=new T(function(){return A(_Jm,[_Jn]);});return function(_Jp,_){var _Jq=jsCreateElem(toJSStr(E(_Jk))),_Jr=jsAppendChild(_Jq,E(_Jp)[1]),_Js=[0,_Jq],_Jt=A(_Jo,[_Js,_]);return _Js;};},_Ju=new T(function(){return _Jl(_v8,_34);}),_Jv=unCStr("width"),_Jw=function(_Jx){return function(_cg,_s3){return _4B(function(_Ez,_){return _4B(_Jh,function(_Jy){return function(_Jz,_){return [0,_Jg,new T(function(){var _JA=E(_Jy);return [0,_JA[1],_JA[2],_JA[3],_JA[4],new T(function(){return _ID(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Gi,_9,new T(function(){var _JB=E(_Jx)[1];return _JB!=E(_Jf)[1]?[0,_JB+1|0]:E(_5z);}),_JA[5]);})];})];};},_Ez,_);},function(_JC,_Ez,_){return (function(_Ez,_){return _4B(function(_JD,_){return [0,[0,function(_JE,_){var _JF=A(_Ju,[_JE,_]),_JG=A(_1,[_n,_JF,_Jj,new T(function(){var _JH=E(_Jx)[1];return _JH>=0?_wh(_J8,_JH):E(_we);}),_]),_JI=A(_1,[_n,_JF,_Jv,_IP,_]),_JJ=A(_1,[_n,_JF,_J9,_IP,_]),_JK=_5k(_JE,_);return _JE;},_Bx],_JD];},_IV,_Ez,_);})(_Ez,_);},_cg,_s3);};},_JL=function(_Ez,_){return _4B(_Hd,_Jw,_Ez,_);},_JM=function(_JN,_JO,_){return _JP(_JO,_);},_JQ=function(_Ez,_){return _yO(_JL,_JM,_Ez,_);},_JR=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_JS=new T(function(){return _58(_p,_JR);}),_JT=[0,20000],_JU=new T(function(){return _3R(_1a,_3E,_18,_15);}),_JV=function(_JW,_JX,_JY,_){var _JZ=A(_JU,[_JY,_]),_K0=new T(function(){return E(E(_JZ)[1]);}),_K1=new T(function(){return [0,_K2];}),_K2=function(_){var _K3=jsFind(toJSStr(E(_K0))),_K4=E(_K3);if(!_K4[0]){return _0;}else{var _K5=E(_K4[1]),_K6=jsClearChildren(_K5[1]),_K7=E(_m)[1],_K8=takeMVar(_K7),_K9=A(_JX,[_K8,_]),_Ka=E(_K9),_Kb=E(_Ka[1]),_=putMVar(_K7,_Ka[2]),_Kc=A(_Kb[1],[_K5,_]),_Kd=E(_Kb[2]);if(!_Kd[0]){var _Ke=jsSetTimeout(E(_JW)[1],E(_K1)[1]);return _0;}else{var _Kf=E(_Kd[1]);return _0;}}},_Kg=jsSetTimeout(E(_JW)[1],E(_K1)[1]);return _yx(_K0,_JX,new T(function(){return E(E(_JZ)[2]);}),_);},_JP=function(_Kh,_){var _Ki=_JV(_JT,_JQ,_Kh,_),_Kj=E(_Ki),_Kk=E(_Kj[1]);return [0,[0,function(_Kl,_){var _Km=A(_JS,[_Kl,_]),_Kn=A(_Kk[1],[_Kl,_]);return _Kl;},_Kk[2]],_Kj[2]];},_Ko=function(_Kp,_Kq,_Kr){return A(_Kp,[[1,_2A,new T(function(){return A(_Kq,[_Kr]);})]]);},_Ks=unCStr("Key "),_Kt=unCStr("Mouse "),_Ku=unCStr("MouseClick "),_Kv=unCStr("NoData"),_Kw=function(_Kx){return _1N(_Kv,_Kx);},_Ky=unCStr(": empty list"),_Kz=unCStr("Prelude."),_KA=function(_KB){return err(_1N(_Kz,new T(function(){return _1N(_KB,_Ky);})));},_KC=unCStr("foldr1"),_KD=new T(function(){return _KA(_KC);}),_KE=function(_KF,_KG){var _KH=E(_KG);if(!_KH[0]){return E(_KD);}else{var _KI=_KH[1],_KJ=E(_KH[2]);return _KJ[0]==0?E(_KI):A(_KF,[_KI,new T(function(){return _KE(_KF,_KJ);})]);}},_KK=[0,32],_KL=function(_KM,_KN){var _KO=E(_KN);switch(_KO[0]){case 0:return E(_Kw);case 1:var _KP=function(_KQ){return _3M(11,E(_KO[1])[1],[1,_KK,new T(function(){var _KR=E(_KO[2]);return [1,_3L,new T(function(){return A(_KE,[_Ko,[1,function(_KS){return _3M(0,E(_KR[1])[1],_KS);},[1,function(_KT){return _3M(0,E(_KR[2])[1],_KT);},_9]],[1,_3K,_KQ]]);})];})]);};return E(_KM)[1]<11?function(_KU){return _1N(_Ku,new T(function(){return _KP(_KU);}));}:function(_KV){return [1,_3L,new T(function(){return _1N(_Ku,new T(function(){return _KP([1,_3K,_KV]);}));})];};case 2:var _KW=function(_KX){return _1N(_Kt,new T(function(){var _KY=E(_KO[1]);return [1,_3L,new T(function(){return A(_KE,[_Ko,[1,function(_KZ){return _3M(0,E(_KY[1])[1],_KZ);},[1,function(_L0){return _3M(0,E(_KY[2])[1],_L0);},_9]],[1,_3K,_KX]]);})];}));};return E(_KM)[1]<11?E(_KW):function(_L1){return [1,_3L,new T(function(){return _KW([1,_3K,_L1]);})];};default:var _L2=_KO[1];return E(_KM)[1]<11?function(_L3){return _1N(_Ks,new T(function(){return _3M(11,E(_L2)[1],_L3);}));}:function(_L4){return [1,_3L,new T(function(){return _1N(_Ks,new T(function(){return _3M(11,E(_L2)[1],[1,_3K,_L4]);}));})];};}},_L5=[0,32],_L6=function(_L7){var _L8=new T(function(){return _58(_p,new T(function(){var _L9=E(_L7);return _1N(_L9[1],[1,_L5,new T(function(){return A(_KL,[_pa,_L9[2],_9]);})]);}));});return function(_La,_){return [0,[0,_L8,_Bx],_La];};},_Lb=function(_){var _Lc=E(_st)[1],_Ld=takeMVar(_Lc),_=putMVar(_Lc,_Ld);return _Ld;},_Le=function(_Lf,_){var _Lg=0;if(!E(_Lg)){var _Lh=_Lb();return [0,[0,_34,[1,_Lh]],_Lf];}else{var _Li=E(_st)[1],_Lj=takeMVar(_Li),_=putMVar(_Li,_Lj);return [0,[0,_34,[1,_Lj]],_Lf];}},_Lk=function(_Ez,_){return _4B(_Le,_L6,_Ez,_);},_Ll=function(_Lm){return E(_Lk);},_Ln=[12,coercionToken],_Lo=[9,coercionToken],_Lp=[11,coercionToken],_Lq=[5,coercionToken],_Lr=[10,coercionToken],_Ls=[6,coercionToken],_Lt=[7,coercionToken],_Lu=unCStr("height:100px;background-color:green"),_Lv=unCStr("div"),_Lw=function(_Lx,_Ly){var _Lz=new T(function(){return A(_Lx,[_Ly]);});return function(_LA,_){var _LB=jsCreateElem(toJSStr(E(_Lv))),_LC=jsAppendChild(_LB,E(_LA)[1]),_LD=[0,_LB],_LE=A(_Lz,[_LD,_]);return _LD;};},_LF=unCStr("mouse events here"),_LG=new T(function(){return _Lw(_p,_LF);}),_LH=unCStr("style"),_LI=function(_LJ,_){var _LK=A(_LG,[_LJ,_]),_LL=A(_1,[_n,_LK,_LH,_Lu,_]);return _LK;},_LM=[0,_LI,_Bx],_LN=function(_LO,_){return [0,_LM,_LO];},_LP=new T(function(){return _tn(_LN,_Lt);}),_LQ=new T(function(){return _tn(_LP,_Ls);}),_LR=new T(function(){return _tn(_LQ,_Lr);}),_LS=new T(function(){return _tn(_LR,_Lq);}),_LT=new T(function(){return _tn(_LS,_Lp);}),_LU=new T(function(){return _tn(_LT,_vp);}),_LV=new T(function(){return _tn(_LU,_Lo);}),_LW=new T(function(){return _tn(_LV,_Ln);}),_LX=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_LY=new T(function(){return _58(_p,_LX);}),_LZ=function(_M0,_M1,_M2){var _M3=E(_M2);if(!_M3[0]){var _M4=_M3[3],_M5=_M3[4],_M6=_M3[5],_M7=E(_M3[2]),_M8=_M7[1];return _M0>=_M8?_M0!=_M8?_HY(_M7,_M4,_M5,_LZ(_M0,_M1,_M6)):[0,_M3[1],E([0,_M0]),_M1,E(_M5),E(_M6)]:_Hj(_M7,_M4,_LZ(_M0,_M1,_M5),_M6);}else{return [0,1,E([0,_M0]),_M1,E(_8),E(_8)];}},_M9=function(_Ma,_Mb,_Mc){var _Md=E(_Ma),_Me=_Md[1],_Mf=E(_Mc);if(!_Mf[0]){var _Mg=_Mf[3],_Mh=_Mf[4],_Mi=_Mf[5],_Mj=E(_Mf[2]),_Mk=_Mj[1];return _Me>=_Mk?_Me!=_Mk?_HY(_Mj,_Mg,_Mh,_LZ(_Me,_Mb,_Mi)):[0,_Mf[1],E(_Md),_Mb,E(_Mh),E(_Mi)]:_Hj(_Mj,_Mg,_LZ(_Me,_Mb,_Mh),_Mi);}else{return [0,1,E(_Md),_Mb,E(_8),E(_8)];}},_Ml=function(_Mm,_Mn,_Mo){var _Mp=E(_Mm),_Mq=_Mp[1],_Mr=_Mp[2],_Ms=_Mp[3],_Mt=_Mp[4],_Mu=E(_Mo);if(!_Mu[0]){var _Mv=_Mu[2],_Mw=_Mu[3],_Mx=_Mu[4],_My=_Mu[5];switch(_Gx(_Mp,_Mv)){case 0:return _Hj(_Mv,_Mw,_ID(_Mq,_Mr,_Ms,_Mt,_Mn,_Mx),_My);case 1:return [0,_Mu[1],E(_Mp),_Mn,E(_Mx),E(_My)];default:return _HY(_Mv,_Mw,_Mx,_ID(_Mq,_Mr,_Ms,_Mt,_Mn,_My));}}else{return [0,1,E(_Mp),_Mn,E(_8),E(_8)];}},_Mz=function(_MA,_MB){while(1){var _MC=E(_MB);if(!_MC[0]){var _MD=E(_MC[2])[1];if(_MA>=_MD){if(_MA!=_MD){_MB=_MC[5];continue;}else{return [1,_MC[3]];}}else{_MB=_MC[4];continue;}}else{return [0];}}},_ME=[0,_34,_a],_MF=function(_MG,_){return [0,_ME,_MG];},_MH=unCStr("containers-0.5.5.1"),_MI=unCStr("Data.Map.Base"),_MJ=unCStr("Map"),_MK=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_MH,_MI,_MJ],_ML=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_MK,_9],_MM=function(_MN){return E(_ML);},_MO=function(_MP){var _MQ=E(_MP);if(!_MQ[0]){return [0];}else{var _MR=E(_MQ[1]);return [1,[0,_MR[1],_MR[2]],new T(function(){return _MO(_MQ[2]);})];}},_MS=function(_MT,_MU){return function(_MV){return E(new T(function(){var _MW=A(_MT,[_6F]),_MX=E(_MW[3]),_MY=_MX[1],_MZ=_MX[2],_N0=_1N(_MW[4],[1,new T(function(){return A(_MU,[_6F]);}),_9]);if(!_N0[0]){return [0,_MY,_MZ,_MX,_9];}else{var _N1=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_MY,_MZ],new T(function(){return _MO(_N0);})]));}));return [0,_N1[1],_N1[2],_MX,_N0];}}));};},_N2=new T(function(){return _MS(_MM,_oi);}),_N3=new T(function(){return _6G(_N2,_oi);}),_N4=new T(function(){return _GY(_1a,_3E,_18,_N3);}),_N5=function(_N6,_){var _N7=A(_N4,[_N6,_]);return [0,[0,_34,new T(function(){return E(E(_N7)[1]);})],new T(function(){return E(E(_N7)[2]);})];},_N8=new T(function(){return _6G(_N2,_oi);}),_N9=[1,_8],_Na=new T(function(){return _GY(_1a,_3E,_18,_N8);}),_Nb=function(_Nc,_){var _Nd=A(_Na,[_Nc,_]);return [0,[0,_Gd,new T(function(){var _Ne=E(E(_Nd)[1]);return _Ne[0]==0?E(_N9):E(_Ne);})],new T(function(){return E(E(_Nd)[2]);})];},_Nf=[0,_34,_5o],_Ng=[1,_a],_Nh=function(_Ni,_Nj){var _Nk=new T(function(){return [0,E(_Ni)[1]+1|0];});return function(_cg,_s3){return _4B(function(_Ez,_){return _4B(function(_Nl,_){var _Nm=_4B(_N5,function(_Nn){var _No=_Mz(E(_Ni)[1],_Nn);return _No[0]==0?E(_MF):function(_Np,_){return [0,[0,_34,_No],_Np];};},_Nl,_),_Nq=E(_Nm),_Nr=E(_Nq[1]);return [0,[0,function(_Ns,_){var _Nt=A(_Nr[1],[_Ns,_]);return _Ns;},new T(function(){var _Nu=E(_Nr[2]);return _Nu[0]==0?E(_Ng):[1,_Nu];})],_Nq[2]];},function(_Nv){var _Nw=new T(function(){return _tn(new T(function(){return A(_s5,[_a,_9I,_Nv]);}),_9H);});return function(_cg,_s3){return _4B(function(_Nx,_){var _Ny=A(_Nw,[_Nx,_]),_Nz=E(_Ny),_NA=_Nz[2],_NB=E(_Nz[1]),_NC=_NB[1],_ND=_NB[2],_NE=E(_Nv);return _NE[0]==0?[0,[0,function(_NF,_){var _NG=A(_NC,[_NF,_]);return _NF;},_ND],_NA]:[0,[0,function(_NH,_){var _NI=A(_NC,[_NH,_]);return _NH;},new T(function(){var _NJ=E(_ND);return _NJ[0]==0?E(_NE):E(_NJ);})],_NA];},function(_NK,_NL,_){return _4B(function(_Ez,_){return _4B(_Nb,function(_NM){var _NN=new T(function(){return _M9(_Ni,_NK,_NM);}),_NO=new T(function(){return A(_N8,[_NN]);});return function(_cg,_s3){return _4B(_Jh,function(_NP){return function(_NQ,_){return [0,_Nf,new T(function(){var _NR=E(_NP);return [0,_NR[1],_NR[2],_NR[3],_NR[4],new T(function(){return _Ml(_NO,_NN,_NR[5]);})];})];};},_cg,_s3);};},_Ez,_);},function(_NS,_Ez,_){return (function(_NT,_){return [0,[0,_34,[1,_NK]],_NT];})(_Ez,_);},_NL,_);},_cg,_s3);};},_Ez,_);},function(_NU){var _NV=new T(function(){return _Nh(_Nk,new T(function(){return _8S(_Nj,_NU);}));}),_NW=new T(function(){return _w(_p,new T(function(){return _3M(0,E(_Nj)[1]+E(_NU)[1]|0,_9);}));});return function(_cg,_s3){return _4B(function(_NX,_){return [0,[0,function(_NY,_){var _NZ=A(_NW,[_NY,_]),_O0=_5k(_NY,_);return _NY;},_5o],_NX];},function(_O1){return E(_NV);},_cg,_s3);};},_cg,_s3);};},_O2=new T(function(){return _Nh(_5z,_5z);}),_O3=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_O4=new T(function(){return _58(_p,_O3);}),_O5=new T(function(){return A(_s5,[_a,_9I,_a]);}),_O6=new T(function(){return _tn(_O5,_9H);}),_O7=function(_O8){var _O9=new T(function(){return _w(_p,new T(function(){return _5h(_O8);}));});return function(_cg,_s3){return _4B(_O6,function(_Oa){var _Ob=E(E(_Oa)[1]);if(!_Ob){return function(_Oc,_){return [0,[0,function(_Od,_){var _Oe=_5k(_Od,_),_Of=_p(_5p,_Od,_),_Og=A(_O9,[_Od,_]);return _Od;},_a],_Oc];};}else{var _Oh=new T(function(){return _O7(new T(function(){return [0,E(_O8)[1]+_Ob|0];}));}),_Oi=new T(function(){return _w(_p,new T(function(){return _3M(0,E(_O8)[1]+_Ob|0,_9);}));});return function(_cg,_s3){return _4B(function(_Oj,_){return [0,[0,function(_Ok,_){var _Ol=A(_Oi,[_Ok,_]),_Om=_5k(_Ok,_);return _Ok;},_5o],_Oj];},function(_On){return E(_Oh);},_cg,_s3);};}},_cg,_s3);};},_Oo=new T(function(){return _O7(_5z);}),_Op=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_Oq=new T(function(){return _58(_p,_Op);}),_Or=function(_Os){return function(_Ot,_){return [0,[0,new T(function(){var _Ou=new T(function(){return _w(_p,new T(function(){return _5h(_Os);}));});return _58(_v8,function(_Ov,_){var _Ow=_p(_5p,_Ov,_),_Ox=A(_Ou,[_Ov,_]);return _Ov;});}),_5o],_Ot];};},_Oy=new T(function(){return A(_s5,[_a,_9I,_a]);}),_Oz=new T(function(){return _tn(_Oy,_9H);}),_OA=new T(function(){return A(_s5,[_a,_9I,_a]);}),_OB=new T(function(){return _tn(_OA,_9H);}),_OC=unCStr("second number "),_OD=unCStr("first number"),_OE=function(_OF,_){var _OG=A(_OB,[_OF,_]),_OH=E(_OG),_OI=E(_OH[1]),_OJ=A(_Oz,[_OH[2],_]),_OK=E(_OJ),_OL=E(_OK[1]);return [0,[0,function(_OM,_){var _ON=_p(_OD,_OM,_),_OO=_5k(_OM,_),_OP=A(_OI[1],[_OM,_]),_OQ=_5k(_OM,_),_OR=_p(_OC,_OM,_),_OS=_5k(_OM,_),_OT=A(_OL[1],[_OM,_]),_OU=_5k(_OM,_);return _OM;},new T(function(){var _OV=E(_OI[2]);if(!_OV[0]){return [0];}else{var _OW=E(_OL[2]);return _OW[0]==0?[0]:[1,new T(function(){return _8S(_OV[1],_OW[1]);})];}})],_OK[2]];},_OX=function(_OY,_){var _OZ=_4B(_OE,_Or,_OY,_),_P0=E(_OZ),_P1=E(_P0[1]),_P2=new T(function(){return _58(_v8,_P1[1]);});return [0,[0,function(_P3,_){var _P4=A(_Oq,[_P3,_]),_P5=A(_P2,[_P3,_]);return _P3;},_P1[2]],_P0[2]];},_P6=unCStr("td"),_P7=function(_P8,_P9){var _Pa=new T(function(){return A(_P8,[_P9]);});return function(_Pb,_){var _Pc=jsCreateElem(toJSStr(E(_P6))),_Pd=jsAppendChild(_Pc,E(_Pb)[1]),_Pe=[0,_Pc],_Pf=A(_Pa,[_Pe,_]);return _Pe;};},_Pg=unCStr("tr"),_Ph=function(_Pi,_Pj){var _Pk=new T(function(){return A(_Pi,[_Pj]);});return function(_Pl,_){var _Pm=jsCreateElem(toJSStr(E(_Pg))),_Pn=jsAppendChild(_Pm,E(_Pl)[1]),_Po=[0,_Pm],_Pp=A(_Pk,[_Po,_]);return _Po;};},_Pq=function(_Pr,_){var _Ps=_OX(_Pr,_),_Pt=E(_Ps),_Pu=E(_Pt[1]),_Pv=A(_vm,[_Pt[2],_]),_Pw=E(_Pv),_Px=E(_Pw[1]),_Py=A(_Oo,[_Pw[2],_]),_Pz=E(_Py),_PA=E(_Pz[1]),_PB=A(_A1,[_Pz[2],_]),_PC=E(_PB),_PD=E(_PC[1]),_PE=A(_O2,[_PC[2],_]),_PF=E(_PE),_PG=E(_PF[1]),_PH=A(_AG,[_PF[2],_]),_PI=E(_PH),_PJ=E(_PI[1]),_PK=_G3(_PI[2],_),_PL=E(_PK),_PM=E(_PL[1]),_PN=_JP(_PL[2],_),_PO=E(_PN),_PP=E(_PO[1]),_PQ=_4B(_LW,_Ll,_PO[2],_),_PR=E(_PQ),_PS=E(_PR[1]);return [0,[0,function(_PT,_){var _PU=A(new T(function(){var _PV=new T(function(){return _P7(_v8,function(_PW,_){var _PX=A(_O4,[_PW,_]),_PY=A(_PA[1],[_PW,_]);return _PW;});}),_PZ=new T(function(){return _P7(_v8,_Px[1]);}),_Q0=new T(function(){return _P7(_v8,_Pu[1]);});return _Ph(_v8,function(_Q1,_){var _Q2=A(_Q0,[_Q1,_]),_Q3=A(_PZ,[_Q1,_]),_Q4=A(_PV,[_Q1,_]);return _Q1;});}),[_PT,_]),_Q5=A(_1,[_n,_PU,_53,_54,_]),_Q6=A(new T(function(){var _Q7=new T(function(){return _P7(_v8,_PJ[1]);}),_Q8=new T(function(){return _P7(_v8,function(_Q9,_){var _Qa=A(_LY,[_Q9,_]),_Qb=A(_PG[1],[_Q9,_]);return _Q9;});}),_Qc=new T(function(){return _P7(_v8,_PD[1]);});return _Ph(_v8,function(_Qd,_){var _Qe=A(_Qc,[_Qd,_]),_Qf=A(_Q8,[_Qd,_]),_Qg=A(_Q7,[_Qd,_]);return _Qd;});}),[_PT,_]),_Qh=A(_1,[_n,_Q6,_53,_54,_]),_Qi=A(new T(function(){var _Qj=new T(function(){return _P7(_v8,_PS[1]);}),_Qk=new T(function(){return _P7(_v8,_PP[1]);}),_Ql=new T(function(){return _P7(_v8,_PM[1]);});return _Ph(_v8,function(_Qm,_){var _Qn=A(_Ql,[_Qm,_]),_Qo=A(_Qk,[_Qm,_]),_Qp=A(_Qj,[_Qm,_]);return _Qm;});}),[_PT,_]),_Qq=A(_1,[_n,_Qi,_LH,_54,_]);return _PT;},new T(function(){var _Qr=E(_Pu[2]);if(!_Qr[0]){var _Qs=E(_Px[2]);if(!_Qs[0]){var _Qt=E(_PA[2]);if(!_Qt[0]){var _Qu=E(_PD[2]);if(!_Qu[0]){var _Qv=E(_PG[2]);if(!_Qv[0]){var _Qw=E(_PJ[2]);if(!_Qw[0]){var _Qx=E(_PM[2]);if(!_Qx[0]){var _Qy=E(_PP[2]);return _Qy[0]==0?E(_PS[2]):E(_Qy);}else{return E(_Qx);}}else{return E(_Qw);}}else{return E(_Qv);}}else{return E(_Qu);}}else{return E(_Qt);}}else{return E(_Qs);}}else{return E(_Qr);}})],_PR[2]];},_Qz=unCStr("h1"),_QA=function(_QB,_QC){var _QD=new T(function(){return A(_QB,[_QC]);});return function(_QE,_){var _QF=jsCreateElem(toJSStr(E(_Qz))),_QG=jsAppendChild(_QF,E(_QE)[1]),_QH=[0,_QF],_QI=A(_QD,[_QH,_]);return _QH;};},_QJ=unCStr("hplayground examples"),_QK=new T(function(){return _QA(_p,_QJ);}),_QL=unCStr("idelem"),_QM=unCStr("table"),_QN=function(_QO,_QP){var _QQ=new T(function(){return A(_QO,[_QP]);});return function(_QR,_){var _QS=jsCreateElem(toJSStr(E(_QM))),_QT=jsAppendChild(_QS,E(_QR)[1]),_QU=[0,_QS],_QV=A(_QQ,[_QU,_]);return _QU;};},_QW=function(_){var _QX=E(_QL),_QY=jsFind(toJSStr(_QX)),_QZ=E(_QY);if(!_QZ[0]){return _45(_QX);}else{var _R0=_QZ[1],_R1=E(_m)[1],_R2=takeMVar(_R1),_R3=_Pq(_R2,_),_R4=E(_R3),_R5=E(_R4[1]),_=putMVar(_R1,_R4[2]),_R6=A(_QK,[_R0,_]),_R7=A(_1,[_n,_R6,_LH,_H,_]),_R8=A(_QN,[_v8,_R5[1],_R0,_]),_R9=A(_G,[_R0,_]);return _R5[2];}},_Ra=function(_){return _QW(_);};
var hasteMain = function() {A(_Ra, [0]);};window.onload = hasteMain;