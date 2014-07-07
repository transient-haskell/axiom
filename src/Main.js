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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=function(_q,_r,_){var _s=jsCreateTextNode(toJSStr(E(_q))),_t=jsAppendChild(_s,E(_r)[1]);return [0,_s];},_u=[0,98],_v=[1,_u,_9],_w=function(_x,_y){var _z=new T(function(){return A(_x,[_y]);});return function(_A,_){var _B=jsCreateElem(toJSStr(_v)),_C=jsAppendChild(_B,E(_A)[1]),_D=[0,_B],_E=A(_z,[_D,_]);return _D;};},_F=unCStr("bottom of the page"),_G=new T(function(){return _w(_p,_F);}),_H=unCStr("text-align:center"),_I=unCStr("id"),_J=function(_K,_L,_M,_){var _N=E(_L),_O=A(_K,[_M,_]),_P=A(_1,[_n,_O,_N[1],_N[2],_]);return _O;},_Q=function(_R,_S){while(1){var _T=(function(_U,_V){var _W=E(_V);if(!_W[0]){return E(_U);}else{_R=function(_X,_){return _J(_U,_W[1],_X,_);};_S=_W[2];return null;}})(_R,_S);if(_T!=null){return _T;}}},_Y=unCStr("span"),_Z=function(_10,_11,_){var _12=jsCreateElem(toJSStr(E(_10))),_13=jsAppendChild(_12,E(_11)[1]);return [0,_12];},_14=function(_X,_){return _Z(_Y,_X,_);},_15=function(_16,_17,_){return [0,_0,_16];},_18=function(_19,_){return [0,_19,_19];},_1a=[0,coercionToken],_1b=function(_1c,_1d,_){var _1e=A(_1c,[_]);return A(_1d,[_]);},_1f=function(_1g,_1h,_){return _1b(_1g,_1h,_);},_1i=function(_1j,_1k,_){var _1l=A(_1j,[_]);return A(_1k,[_1l,_]);},_1m=unCStr("base"),_1n=unCStr("GHC.IO.Exception"),_1o=unCStr("IOException"),_1p=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1m,_1n,_1o],_1q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1p,_9],_1r=function(_1s){return E(_1q);},_1t=function(_1u){return E(E(_1u)[1]);},_1v=unCStr("Maybe.fromJust: Nothing"),_1w=new T(function(){return err(_1v);}),_1x=function(_1y,_1z,_1A){var _1B=new T(function(){var _1C=A(_1y,[_1A]),_1D=A(_1z,[new T(function(){var _1E=E(_1B);return _1E[0]==0?E(_1w):E(_1E[1]);})]),_1F=hs_eqWord64(_1C[1],_1D[1]);if(!E(_1F)){return [0];}else{var _1G=hs_eqWord64(_1C[2],_1D[2]);return E(_1G)==0?[0]:[1,_1A];}});return E(_1B);},_1H=function(_1I){var _1J=E(_1I);return _1x(_1t(_1J[1]),_1r,_1J[2]);},_1K=unCStr(": "),_1L=[0,41],_1M=unCStr(" ("),_1N=function(_1O,_1P){var _1Q=E(_1O);return _1Q[0]==0?E(_1P):[1,_1Q[1],new T(function(){return _1N(_1Q[2],_1P);})];},_1R=unCStr("already exists"),_1S=unCStr("does not exist"),_1T=unCStr("protocol error"),_1U=unCStr("failed"),_1V=unCStr("invalid argument"),_1W=unCStr("inappropriate type"),_1X=unCStr("hardware fault"),_1Y=unCStr("unsupported operation"),_1Z=unCStr("timeout"),_20=unCStr("resource vanished"),_21=unCStr("interrupted"),_22=unCStr("resource busy"),_23=unCStr("resource exhausted"),_24=unCStr("end of file"),_25=unCStr("illegal operation"),_26=unCStr("permission denied"),_27=unCStr("user error"),_28=unCStr("unsatisified constraints"),_29=unCStr("system error"),_2a=function(_2b,_2c){switch(E(_2b)){case 0:return _1N(_1R,_2c);case 1:return _1N(_1S,_2c);case 2:return _1N(_22,_2c);case 3:return _1N(_23,_2c);case 4:return _1N(_24,_2c);case 5:return _1N(_25,_2c);case 6:return _1N(_26,_2c);case 7:return _1N(_27,_2c);case 8:return _1N(_28,_2c);case 9:return _1N(_29,_2c);case 10:return _1N(_1T,_2c);case 11:return _1N(_1U,_2c);case 12:return _1N(_1V,_2c);case 13:return _1N(_1W,_2c);case 14:return _1N(_1X,_2c);case 15:return _1N(_1Y,_2c);case 16:return _1N(_1Z,_2c);case 17:return _1N(_20,_2c);default:return _1N(_21,_2c);}},_2d=[0,125],_2e=unCStr("{handle: "),_2f=function(_2g,_2h,_2i,_2j,_2k,_2l){var _2m=new T(function(){var _2n=new T(function(){return _2a(_2h,new T(function(){var _2o=E(_2j);return _2o[0]==0?E(_2l):_1N(_1M,new T(function(){return _1N(_2o,[1,_1L,_2l]);}));}));}),_2p=E(_2i);return _2p[0]==0?E(_2n):_1N(_2p,new T(function(){return _1N(_1K,_2n);}));}),_2q=E(_2k);if(!_2q[0]){var _2r=E(_2g);if(!_2r[0]){return E(_2m);}else{var _2s=E(_2r[1]);return _2s[0]==0?_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);})):_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);}));}}else{return _1N(_2q[1],new T(function(){return _1N(_1K,_2m);}));}},_2t=function(_2u){var _2v=E(_2u);return _2f(_2v[1],_2v[2],_2v[3],_2v[4],_2v[6],_9);},_2w=function(_2x,_2y){var _2z=E(_2x);return _2f(_2z[1],_2z[2],_2z[3],_2z[4],_2z[6],_2y);},_2A=[0,44],_2B=[0,93],_2C=[0,91],_2D=function(_2E,_2F,_2G){var _2H=E(_2F);return _2H[0]==0?unAppCStr("[]",_2G):[1,_2C,new T(function(){return A(_2E,[_2H[1],new T(function(){var _2I=function(_2J){var _2K=E(_2J);return _2K[0]==0?E([1,_2B,_2G]):[1,_2A,new T(function(){return A(_2E,[_2K[1],new T(function(){return _2I(_2K[2]);})]);})];};return _2I(_2H[2]);})]);})];},_2L=function(_2M,_2N){return _2D(_2w,_2M,_2N);},_2O=function(_2P,_2Q,_2R){var _2S=E(_2Q);return _2f(_2S[1],_2S[2],_2S[3],_2S[4],_2S[6],_2R);},_2T=[0,_2O,_2t,_2L],_2U=new T(function(){return [0,_1r,_2T,_2V,_1H];}),_2V=function(_2W){return [0,_2U,_2W];},_2X=7,_2Y=function(_2Z){return [0,_a,_2X,_9,_2Z,_a,_a];},_30=function(_31,_){return die(new T(function(){return _2V(new T(function(){return _2Y(_31);}));}));},_32=function(_33,_){return _30(_33,_);},_34=function(_35,_){return _35;},_36=[0,_1i,_1f,_34,_32],_37=function(_38){return E(E(_38)[1]);},_39=function(_3a,_3b,_3c,_3d){return A(_37,[_3a,new T(function(){return A(_3b,[_3d]);}),function(_3e){return A(_3c,[new T(function(){return E(E(_3e)[1]);}),new T(function(){return E(E(_3e)[2]);})]);}]);},_3f=function(_3g,_3h,_3i,_3j){return A(_37,[_3g,new T(function(){return A(_3h,[_3j]);}),function(_3k){return A(_3i,[new T(function(){return E(E(_3k)[2]);})]);}]);},_3l=function(_3m,_3n,_3o,_3p){return _3f(_3m,_3n,_3o,_3p);},_3q=function(_3r){return E(E(_3r)[4]);},_3s=function(_3t,_3u){var _3v=new T(function(){return A(_3q,[_3t,_3u]);});return function(_3w){return E(_3v);};},_3x=function(_3y){return E(E(_3y)[3]);},_3z=function(_3A){var _3B=new T(function(){return _3x(_3A);});return [0,function(_3n,_3o,_3p){return _39(_3A,_3n,_3o,_3p);},function(_3n,_3o,_3p){return _3l(_3A,_3n,_3o,_3p);},function(_3C,_3D){return A(_3B,[[0,_3C,_3D]]);},function(_3p){return _3s(_3A,_3p);}];},_3E=new T(function(){return _3z(_36);}),_3F=[0,112],_3G=function(_3H,_3I){var _3J=jsShowI(_3H);return _1N(fromJSStr(_3J),_3I);},_3K=[0,41],_3L=[0,40],_3M=function(_3N,_3O,_3P){return _3O>=0?_3G(_3O,_3P):_3N<=6?_3G(_3O,_3P):[1,_3L,new T(function(){var _3Q=jsShowI(_3O);return _1N(fromJSStr(_3Q),[1,_3K,_3P]);})];},_3R=function(_3S,_3T,_3U,_3V){var _3W=E(_3T);return A(_3W[1],[new T(function(){var _3X=E(_3S);return E(_3U);}),function(_3Y){var _3Z=new T(function(){return E(E(_3Y)[2]);});return A(_3W[2],[new T(function(){return A(_3V,[new T(function(){var _40=E(new T(function(){var _41=E(_3S);return [0,coercionToken];})),_42=E(_3Y);return [0,_42[1],new T(function(){return [0,E(_3Z)[1]+1|0];}),_42[3],_42[4],_42[5]];})]);}),new T(function(){return A(_3W[3],[[1,_3F,new T(function(){return _1N(_3M(0,E(_3Z)[1],_9),new T(function(){return E(E(_3Y)[1]);}));})]]);})]);}]);},_43=new T(function(){return _3R(_1a,_3E,_18,_15);}),_44=unCStr(" could be found!"),_45=function(_46){return err(unAppCStr("No element with ID ",new T(function(){return _1N(_46,_44);})));},_47=function(_48,_49,_){var _4a=E(_49),_4b=jsFind(toJSStr(_4a)),_4c=E(_4b);if(!_4c[0]){return _45(_4a);}else{var _4d=E(_4c[1]),_4e=jsClearChildren(_4d[1]),_4f=E(_m)[1],_4g=takeMVar(_4f),_4h=A(_48,[_4g,_]),_4i=E(_4h),_4j=E(_4i[1]),_=putMVar(_4f,_4i[2]),_4k=A(_4j[1],[_4d,_]);return _4j[2];}},_4l=function(_4m,_4n,_4o,_4p,_4q,_4r,_4s,_4t,_){var _4u=E(_4s);return [0,_4u,[0,_4p,_4q,_4r,[0,function(_){return _47(function(_4v,_){var _4w=A(_4m,[new T(function(){var _4x=E(_4v);return [0,_4x[1],_4q,_4x[3],_4x[4],_4x[5]];}),_]);return [0,[0,_34,E(E(_4w)[1])[2]],_4v];},_4o,_);},function(_4y,_){var _4z=_47(new T(function(){return A(_4n,[_4y]);}),_4o,_),_4A=E(_4z);return _4A[0]==0?_a:A(_4u[2],[_4A[1],_]);}],_4t]];},_4B=function(_4C,_4D,_4E,_){var _4F=A(_43,[_4E,_]),_4G=E(_4F),_4H=_4G[1],_4I=E(_4G[2]),_4J=_4l(_4C,_4D,_4H,_4I[1],_4I[2],_4I[3],_4I[4],_4I[5],_),_4K=A(_4C,[new T(function(){return E(E(_4J)[2]);}),_]),_4L=E(_4K),_4M=_4L[2],_4N=E(_4L[1]),_4O=_4N[1],_4P=new T(function(){return _Q(_14,[1,[0,_I,_4H],_9]);}),_4Q=E(_4N[2]);if(!_4Q[0]){return [0,[0,function(_4R,_){var _4S=A(_4O,[_4R,_]),_4T=A(_4P,[_4R,_]);return _4R;},_a],new T(function(){var _4U=E(_4M);return [0,_4U[1],_4U[2],_4U[3],new T(function(){return E(E(_4J)[1]);}),_4U[5]];})];}else{var _4V=A(_4D,[_4Q[1],new T(function(){var _4W=E(_4M);return [0,_4W[1],_4W[2],_4W[3],new T(function(){return E(E(_4J)[1]);}),_4W[5]];}),_]),_4X=E(_4V),_4Y=E(_4X[1]);return [0,[0,function(_4Z,_){var _50=A(_4O,[_4Z,_]),_51=A(_4P,[_4Z,_]),_52=A(_4Y[1],[_51,_]);return _4Z;},_4Y[2]],_4X[2]];}},_53=unCStr("style"),_54=unCStr("vertical-align:top"),_55=[0,3],_56=[0,112],_57=[1,_56,_9],_58=function(_59,_5a){var _5b=new T(function(){return A(_59,[_5a]);});return function(_5c,_){var _5d=jsCreateElem(toJSStr(_57)),_5e=jsAppendChild(_5d,E(_5c)[1]),_5f=[0,_5d],_5g=A(_5b,[_5f,_]);return _5f;};},_5h=function(_5i){return _3M(0,E(_5i)[1],_9);},_5j=unCStr("br"),_5k=function(_5l,_){var _5m=jsCreateElem(toJSStr(E(_5j))),_5n=jsAppendChild(_5m,E(_5l)[1]);return [0,_5m];},_5o=[1,_0],_5p=unCStr("result: "),_5q=function(_5r){var _5s=new T(function(){return _w(_p,new T(function(){return _5h(_5r);}));});return function(_5t,_){return [0,[0,function(_5u,_){var _5v=_5k(_5u,_),_5w=_p(_5p,_5u,_),_5x=A(_5s,[_5u,_]);return _5u;},_5o],_5t];};},_5y=unCStr(" numbers and append the result using a fold"),_5z=[0,0],_5A=[1,_5z],_5B=[0,_34,_5A],_5C=function(_5D,_){return [0,_5B,_5D];},_5E=function(_5F,_5G,_5H,_){var _5I=_Z(_5F,_5H,_),_5J=A(_5G,[_5I,_]);return _5I;},_5K=unCStr("()"),_5L=unCStr("GHC.Tuple"),_5M=unCStr("ghc-prim"),_5N=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5M,_5L,_5K],_5O=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5N,_9],_5P=function(_5Q){return E(_5O);},_5R=unCStr("main"),_5S=unCStr("Haste.Perch"),_5T=unCStr("PerchM"),_5U=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5R,_5S,_5T],_5V=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5U,_9],_5W=function(_5X){return E(_5V);},_5Y=function(_5Z){var _60=E(_5Z);return _60[0]==0?[0]:_1N(_60[1],new T(function(){return _5Y(_60[2]);}));},_61=function(_62,_63){var _64=E(_62);if(!_64){return [0,_9,_63];}else{var _65=E(_63);if(!_65[0]){return [0,_9,_9];}else{var _66=new T(function(){var _67=_61(_64-1|0,_65[2]);return [0,_67[1],_67[2]];});return [0,[1,_65[1],new T(function(){return E(E(_66)[1]);})],new T(function(){return E(E(_66)[2]);})];}}},_68=[0,120],_69=[0,48],_6a=function(_6b){var _6c=new T(function(){var _6d=_61(8,new T(function(){var _6e=md5(toJSStr(E(_6b)));return fromJSStr(_6e);}));return [0,_6d[1],_6d[2]];}),_6f=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6c)[1]);})]])]),_6g=new T(function(){var _6h=_61(8,new T(function(){return E(E(_6c)[2]);}));return [0,_6h[1],_6h[2]];}),_6i=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6g)[1]);})]])]),_6j=hs_mkWord64(_6f,_6i),_6k=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(_61(8,new T(function(){return E(E(_6g)[2]);}))[1]);})]])]),_6l=hs_mkWord64(_6k,_6k);return [0,_6j,_6l];},_6m=function(_6n,_6o){var _6p=E(_6o);return _6p[0]==0?[0]:[1,new T(function(){return A(_6n,[_6p[1]]);}),new T(function(){return _6m(_6n,_6p[2]);})];},_6q=function(_6r,_6s){var _6t=jsShowI(_6r),_6u=md5(_6t);return _1N(fromJSStr(_6u),new T(function(){var _6v=jsShowI(_6s),_6w=md5(_6v);return fromJSStr(_6w);}));},_6x=function(_6y){var _6z=E(_6y);return _6q(_6z[1],_6z[2]);},_6A=function(_6B){var _6C=E(_6B);if(!_6C[0]){return [0];}else{var _6D=E(_6C[1]);return [1,[0,_6D[1],_6D[2]],new T(function(){return _6A(_6C[2]);})];}},_6E=unCStr("Prelude.undefined"),_6F=new T(function(){return err(_6E);}),_6G=function(_6H,_6I){return function(_6J){return E(new T(function(){var _6K=A(_6H,[_6F]),_6L=E(_6K[3]),_6M=_6L[1],_6N=_6L[2],_6O=_1N(_6K[4],[1,new T(function(){return A(_6I,[_6F]);}),_9]);if(!_6O[0]){return [0,_6M,_6N,_6L,_9];}else{var _6P=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_6M,_6N],new T(function(){return _6A(_6O);})]));}));return [0,_6P[1],_6P[2],_6L,_6O];}}));};},_6Q=new T(function(){return _6G(_5W,_5P);}),_6R=unCStr("value"),_6S=unCStr("onclick"),_6T=unCStr("checked"),_6U=[0,_6T,_9],_6V=[1,_6U,_9],_6W=unCStr("type"),_6X=unCStr("input"),_6Y=function(_6Z,_){return _Z(_6X,_6Z,_);},_70=function(_71,_72,_73,_74,_75){var _76=new T(function(){var _77=new T(function(){return _Q(_6Y,[1,[0,_6W,_72],[1,[0,_I,_71],[1,[0,_6R,_73],_9]]]);});return !E(_74)?E(_77):_Q(_77,_6V);}),_78=E(_75);return _78[0]==0?E(_76):_Q(_76,[1,[0,_6S,_78[1]],_9]);},_79=unCStr("href"),_7a=[0,97],_7b=[1,_7a,_9],_7c=function(_7d,_){return _Z(_7b,_7d,_);},_7e=function(_7f,_7g){var _7h=new T(function(){return _Q(_7c,[1,[0,_79,_7f],_9]);});return function(_7i,_){var _7j=A(_7h,[_7i,_]),_7k=A(_7g,[_7j,_]);return _7j;};},_7l=function(_7m){return _7e(_7m,function(_X,_){return _p(_7m,_X,_);});},_7n=unCStr("option"),_7o=function(_7p,_){return _Z(_7n,_7p,_);},_7q=unCStr("selected"),_7r=[0,_7q,_9],_7s=[1,_7r,_9],_7t=function(_7u,_7v,_7w){var _7x=new T(function(){return _Q(_7o,[1,[0,_6R,_7u],_9]);}),_7y=function(_7z,_){var _7A=A(_7x,[_7z,_]),_7B=A(_7v,[_7A,_]);return _7A;};return !E(_7w)?E(_7y):_Q(_7y,_7s);},_7C=function(_7D,_7E){return _7t(_7D,function(_X,_){return _p(_7D,_X,_);},_7E);},_7F=unCStr("method"),_7G=unCStr("action"),_7H=unCStr("UTF-8"),_7I=unCStr("acceptCharset"),_7J=[0,_7I,_7H],_7K=unCStr("form"),_7L=function(_7M,_){return _Z(_7K,_7M,_);},_7N=function(_7O,_7P,_7Q){var _7R=new T(function(){return _Q(_7L,[1,_7J,[1,[0,_7G,_7O],[1,[0,_7F,_7P],_9]]]);});return function(_7S,_){var _7T=A(_7R,[_7S,_]),_7U=A(_7Q,[_7T,_]);return _7T;};},_7V=unCStr("select"),_7W=function(_7X,_){return _Z(_7V,_7X,_);},_7Y=function(_7Z,_80){var _81=new T(function(){return _Q(_7W,[1,[0,_I,_7Z],_9]);});return function(_82,_){var _83=A(_81,[_82,_]),_84=A(_80,[_83,_]);return _83;};},_85=unCStr("textarea"),_86=function(_87,_){return _Z(_85,_87,_);},_88=function(_89,_8a){var _8b=new T(function(){return _Q(_86,[1,[0,_I,_89],_9]);});return function(_8c,_){var _8d=A(_8b,[_8c,_]),_8e=_p(_8a,_8d,_);return _8d;};},_8f=unCStr("color:red"),_8g=unCStr("style"),_8h=[0,_8g,_8f],_8i=[1,_8h,_9],_8j=[0,98],_8k=[1,_8j,_9],_8l=function(_8m){return _Q(function(_8n,_){var _8o=_Z(_8k,_8n,_),_8p=A(_8m,[_8o,_]);return _8o;},_8i);},_8q=function(_8r,_8s,_){var _8t=E(_8r);if(!_8t[0]){return _8s;}else{var _8u=A(_8t[1],[_8s,_]),_8v=_8q(_8t[2],_8s,_);return _8s;}},_8w=function(_8x,_8y,_8z,_){var _8A=A(_8x,[_8z,_]),_8B=A(_8y,[_8z,_]);return _8z;},_8C=[0,_34,_8w,_8q],_8D=[0,_8C,_6Q,_p,_p,_5E,_8l,_7e,_7l,_70,_88,_7Y,_7t,_7C,_7N,_Q],_8E=function(_8F,_8G,_){var _8H=A(_8G,[_]);return _8F;},_8I=function(_8J,_8K,_){var _8L=A(_8K,[_]);return new T(function(){return A(_8J,[_8L]);});},_8M=[0,_8I,_8E],_8N=function(_8O){var _8P=E(_8O);return _8P[0]==0?0:E(_8P[1])[1]+_8N(_8P[2])|0;},_8Q=function(_8R){return [0,_8N(_8R)];},_8S=function(_8T,_8U){return [0,E(_8T)[1]+E(_8U)[1]|0];},_8V=[0,_5z,_8S,_8Q],_8W=function(_8X,_8Y){var _8Z=E(_8Y);return _8Z[0]==0?[0]:[1,new T(function(){return A(_8X,[_8Z[1]]);})];},_90=function(_91){return E(E(_91)[1]);},_92=function(_93){return E(E(_93)[2]);},_94=function(_95,_96,_97,_98,_99,_9a){var _9b=new T(function(){return _92(_95);});return A(_96,[new T(function(){return A(_98,[_9a]);}),function(_9c){var _9d=E(_9c),_9e=E(_9d[1]);return A(_96,[new T(function(){return A(_99,[_9d[2]]);}),function(_9f){var _9g=E(_9f),_9h=E(_9g[1]);return A(_97,[[0,[0,new T(function(){return A(_9b,[_9e[1],_9h[1]]);}),new T(function(){var _9i=E(_9e[2]);if(!_9i[0]){return [0];}else{var _9j=E(_9h[2]);return _9j[0]==0?[0]:[1,new T(function(){return A(_9i[1],[_9j[1]]);})];}})],_9g[2]]]);}]);}]);},_9k=function(_9l){return E(E(_9l)[1]);},_9m=function(_9n,_9o,_9p,_9q,_9r,_9s){var _9t=new T(function(){return _90(_9n);});return function(_9u){var _9v=E(_9o);return _94(_9t,_9v[1],_9v[3],function(_9w){return A(new T(function(){var _9x=new T(function(){return _92(_9q);});return A(_9k,[_9p,function(_9y){return [0,new T(function(){var _9z=E(E(_9y)[1]);return [0,_9z[1],new T(function(){return _8W(_9x,_9z[2]);})];}),new T(function(){return E(E(_9y)[2]);})];}]);}),[new T(function(){return A(_9r,[_9w]);})]);},_9s,_9u);};},_9A=function(_9B,_9C){while(1){var _9D=(function(_9E,_9F){var _9G=E(_9F);if(!_9G[0]){return E(_9E);}else{_9B=new T(function(){return _9m(_8D,_36,_8M,_8V,_9E,_9G[1]);});_9C=_9G[2];return null;}})(_9B,_9C);if(_9D!=null){return _9D;}}},_9H=[13,coercionToken],_9I=unCStr("text"),_9J=[0,_36,_n],_9K=unCStr("base"),_9L=unCStr("Control.Exception.Base"),_9M=unCStr("PatternMatchFail"),_9N=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9K,_9L,_9M],_9O=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9N,_9],_9P=function(_9Q){return E(_9O);},_9R=function(_9S){var _9T=E(_9S);return _1x(_1t(_9T[1]),_9P,_9T[2]);},_9U=function(_9V){return E(E(_9V)[1]);},_9W=function(_9X,_9Y){return _1N(E(_9X)[1],_9Y);},_9Z=function(_a0,_a1){return _2D(_9W,_a0,_a1);},_a2=function(_a3,_a4,_a5){return _1N(E(_a4)[1],_a5);},_a6=[0,_a2,_9U,_9Z],_a7=new T(function(){return [0,_9P,_a6,_a8,_9R];}),_a8=function(_a9){return [0,_a7,_a9];},_aa=unCStr("Non-exhaustive patterns in"),_ab=function(_ac,_ad){return die(new T(function(){return A(_ad,[_ac]);}));},_ae=function(_af,_ag){var _ah=E(_ag);if(!_ah[0]){return [0,_9,_9];}else{var _ai=_ah[1];if(!A(_af,[_ai])){return [0,_9,_ah];}else{var _aj=new T(function(){var _ak=_ae(_af,_ah[2]);return [0,_ak[1],_ak[2]];});return [0,[1,_ai,new T(function(){return E(E(_aj)[1]);})],new T(function(){return E(E(_aj)[2]);})];}}},_al=[0,32],_am=[0,10],_an=[1,_am,_9],_ao=function(_ap){return E(E(_ap)[1])==124?false:true;},_aq=function(_ar,_as){var _at=_ae(_ao,unCStr(_ar)),_au=_at[1],_av=function(_aw,_ax){return _1N(_aw,new T(function(){return unAppCStr(": ",new T(function(){return _1N(_as,new T(function(){return _1N(_ax,_an);}));}));}));},_ay=E(_at[2]);return _ay[0]==0?_av(_au,_9):E(E(_ay[1])[1])==124?_av(_au,[1,_al,_ay[2]]):_av(_au,_9);},_az=function(_aA){return _ab([0,new T(function(){return _aq(_aA,_aa);})],_a8);},_aB=new T(function(){return _az("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aC=function(_aD,_aE){while(1){var _aF=(function(_aG,_aH){var _aI=E(_aG);switch(_aI[0]){case 0:var _aJ=E(_aH);if(!_aJ[0]){return [0];}else{_aD=A(_aI[1],[_aJ[1]]);_aE=_aJ[2];return null;}break;case 1:var _aK=A(_aI[1],[_aH]),_aL=_aH;_aD=_aK;_aE=_aL;return null;case 2:return [0];case 3:return [1,[0,_aI[1],_aH],new T(function(){return _aC(_aI[2],_aH);})];default:return E(_aI[1]);}})(_aD,_aE);if(_aF!=null){return _aF;}}},_aM=function(_aN,_aO){var _aP=new T(function(){var _aQ=E(_aO);if(_aQ[0]==3){return [3,_aQ[1],new T(function(){return _aM(_aN,_aQ[2]);})];}else{var _aR=E(_aN);if(_aR[0]==2){return E(_aQ);}else{var _aS=E(_aQ);if(_aS[0]==2){return E(_aR);}else{var _aT=new T(function(){var _aU=E(_aS);if(_aU[0]==4){return [1,function(_aV){return [4,new T(function(){return _1N(_aC(_aR,_aV),_aU[1]);})];}];}else{var _aW=E(_aR);if(_aW[0]==1){var _aX=_aW[1],_aY=E(_aU);return _aY[0]==0?[1,function(_aZ){return _aM(A(_aX,[_aZ]),_aY);}]:[1,function(_b0){return _aM(A(_aX,[_b0]),new T(function(){return A(_aY[1],[_b0]);}));}];}else{var _b1=E(_aU);return _b1[0]==0?E(_aB):[1,function(_b2){return _aM(_aW,new T(function(){return A(_b1[1],[_b2]);}));}];}}}),_b3=E(_aR);switch(_b3[0]){case 1:var _b4=E(_aS);return _b4[0]==4?[1,function(_b5){return [4,new T(function(){return _1N(_aC(A(_b3[1],[_b5]),_b5),_b4[1]);})];}]:E(_aT);case 4:var _b6=_b3[1],_b7=E(_aS);switch(_b7[0]){case 0:return [1,function(_b8){return [4,new T(function(){return _1N(_b6,new T(function(){return _aC(_b7,_b8);}));})];}];case 1:return [1,function(_b9){return [4,new T(function(){return _1N(_b6,new T(function(){return _aC(A(_b7[1],[_b9]),_b9);}));})];}];default:return [4,new T(function(){return _1N(_b6,_b7[1]);})];}break;default:return E(_aT);}}}}}),_ba=E(_aN);switch(_ba[0]){case 0:var _bb=E(_aO);return _bb[0]==0?[0,function(_bc){return _aM(A(_ba[1],[_bc]),new T(function(){return A(_bb[1],[_bc]);}));}]:E(_aP);case 3:return [3,_ba[1],new T(function(){return _aM(_ba[2],_aO);})];default:return E(_aP);}},_bd=function(_be,_bf){return E(_be)[1]!=E(_bf)[1];},_bg=function(_bh,_bi){return E(_bh)[1]==E(_bi)[1];},_bj=[0,_bg,_bd],_bk=function(_bl){return E(E(_bl)[1]);},_bm=function(_bn,_bo,_bp){while(1){var _bq=E(_bo);if(!_bq[0]){return E(_bp)[0]==0?true:false;}else{var _br=E(_bp);if(!_br[0]){return false;}else{if(!A(_bk,[_bn,_bq[1],_br[1]])){return false;}else{_bo=_bq[2];_bp=_br[2];continue;}}}}},_bs=function(_bt,_bu,_bv){return !_bm(_bt,_bu,_bv)?true:false;},_bw=function(_bx){return [0,function(_by,_bz){return _bm(_bx,_by,_bz);},function(_by,_bz){return _bs(_bx,_by,_bz);}];},_bA=new T(function(){return _bw(_bj);}),_bB=function(_bC,_bD){var _bE=E(_bC);switch(_bE[0]){case 0:return [0,function(_bF){return _bB(A(_bE[1],[_bF]),_bD);}];case 1:return [1,function(_bG){return _bB(A(_bE[1],[_bG]),_bD);}];case 2:return [2];case 3:return _aM(A(_bD,[_bE[1]]),new T(function(){return _bB(_bE[2],_bD);}));default:var _bH=function(_bI){var _bJ=E(_bI);if(!_bJ[0]){return [0];}else{var _bK=E(_bJ[1]);return _1N(_aC(A(_bD,[_bK[1]]),_bK[2]),new T(function(){return _bH(_bJ[2]);}));}},_bL=_bH(_bE[1]);return _bL[0]==0?[2]:[4,_bL];}},_bM=[2],_bN=function(_bO){return [3,_bO,_bM];},_bP=function(_bQ,_bR){var _bS=E(_bQ);if(!_bS){return A(_bR,[_0]);}else{var _bT=new T(function(){return _bP(_bS-1|0,_bR);});return [0,function(_bU){return E(_bT);}];}},_bV=function(_bW,_bX,_bY){var _bZ=new T(function(){return A(_bW,[_bN]);});return [1,function(_c0){return A(function(_c1,_c2,_c3){while(1){var _c4=(function(_c5,_c6,_c7){var _c8=E(_c5);switch(_c8[0]){case 0:var _c9=E(_c6);if(!_c9[0]){return E(_bX);}else{_c1=A(_c8[1],[_c9[1]]);_c2=_c9[2];var _ca=_c7+1|0;_c3=_ca;return null;}break;case 1:var _cb=A(_c8[1],[_c6]),_cc=_c6,_ca=_c7;_c1=_cb;_c2=_cc;_c3=_ca;return null;case 2:return E(_bX);case 3:return function(_cd){var _ce=new T(function(){return _bB(_c8,_cd);});return _bP(_c7,function(_cf){return E(_ce);});};default:return function(_cg){return _bB(_c8,_cg);};}})(_c1,_c2,_c3);if(_c4!=null){return _c4;}}},[_bZ,_c0,0,_bY]);}];},_ch=[6],_ci=unCStr("valDig: Bad base"),_cj=new T(function(){return err(_ci);}),_ck=function(_cl,_cm){var _cn=function(_co,_cp){var _cq=E(_co);if(!_cq[0]){var _cr=new T(function(){return A(_cp,[_9]);});return function(_cs){return A(_cs,[_cr]);};}else{var _ct=E(_cq[1])[1],_cu=function(_cv){var _cw=new T(function(){return _cn(_cq[2],function(_cx){return A(_cp,[[1,_cv,_cx]]);});});return function(_cy){var _cz=new T(function(){return A(_cw,[_cy]);});return [0,function(_cA){return E(_cz);}];};};switch(E(E(_cl)[1])){case 8:if(48>_ct){var _cB=new T(function(){return A(_cp,[_9]);});return function(_cC){return A(_cC,[_cB]);};}else{if(_ct>55){var _cD=new T(function(){return A(_cp,[_9]);});return function(_cE){return A(_cE,[_cD]);};}else{return _cu([0,_ct-48|0]);}}break;case 10:if(48>_ct){var _cF=new T(function(){return A(_cp,[_9]);});return function(_cG){return A(_cG,[_cF]);};}else{if(_ct>57){var _cH=new T(function(){return A(_cp,[_9]);});return function(_cI){return A(_cI,[_cH]);};}else{return _cu([0,_ct-48|0]);}}break;case 16:var _cJ=new T(function(){return 97>_ct?65>_ct?[0]:_ct>70?[0]:[1,[0,(_ct-65|0)+10|0]]:_ct>102?65>_ct?[0]:_ct>70?[0]:[1,[0,(_ct-65|0)+10|0]]:[1,[0,(_ct-97|0)+10|0]];});if(48>_ct){var _cK=E(_cJ);if(!_cK[0]){var _cL=new T(function(){return A(_cp,[_9]);});return function(_cM){return A(_cM,[_cL]);};}else{return _cu(_cK[1]);}}else{if(_ct>57){var _cN=E(_cJ);if(!_cN[0]){var _cO=new T(function(){return A(_cp,[_9]);});return function(_cP){return A(_cP,[_cO]);};}else{return _cu(_cN[1]);}}else{return _cu([0,_ct-48|0]);}}break;default:return E(_cj);}}};return [1,function(_cQ){return A(_cn,[_cQ,_n,function(_cR){var _cS=E(_cR);return _cS[0]==0?[2]:A(_cm,[_cS]);}]);}];},_cT=[0,10],_cU=[0,1],_cV=[0,2147483647],_cW=function(_cX,_cY){while(1){var _cZ=E(_cX);if(!_cZ[0]){var _d0=_cZ[1],_d1=E(_cY);if(!_d1[0]){var _d2=_d1[1],_d3=addC(_d0,_d2);if(!E(_d3[2])){return [0,_d3[1]];}else{_cX=[1,I_fromInt(_d0)];_cY=[1,I_fromInt(_d2)];continue;}}else{_cX=[1,I_fromInt(_d0)];_cY=_d1;continue;}}else{var _d4=E(_cY);if(!_d4[0]){_cX=_cZ;_cY=[1,I_fromInt(_d4[1])];continue;}else{return [1,I_add(_cZ[1],_d4[1])];}}}},_d5=new T(function(){return _cW(_cV,_cU);}),_d6=function(_d7){var _d8=E(_d7);if(!_d8[0]){var _d9=E(_d8[1]);return _d9==(-2147483648)?E(_d5):[0, -_d9];}else{return [1,I_negate(_d8[1])];}},_da=[0,10],_db=[0,0],_dc=function(_dd,_de){while(1){var _df=E(_dd);if(!_df[0]){var _dg=_df[1],_dh=E(_de);if(!_dh[0]){var _di=_dh[1];if(!(imul(_dg,_di)|0)){return [0,imul(_dg,_di)|0];}else{_dd=[1,I_fromInt(_dg)];_de=[1,I_fromInt(_di)];continue;}}else{_dd=[1,I_fromInt(_dg)];_de=_dh;continue;}}else{var _dj=E(_de);if(!_dj[0]){_dd=_df;_de=[1,I_fromInt(_dj[1])];continue;}else{return [1,I_mul(_df[1],_dj[1])];}}}},_dk=function(_dl,_dm,_dn){while(1){var _do=E(_dn);if(!_do[0]){return E(_dm);}else{var _dp=_cW(_dc(_dm,_dl),_do[1]);_dn=_do[2];_dm=_dp;continue;}}},_dq=function(_dr){var _ds=new T(function(){return _aM(_aM([0,function(_dt){return E(E(_dt)[1])==45?_ck(_cT,function(_du){return A(_dr,[[1,new T(function(){return _d6(_dk(_da,_db,_du));})]]);}):[2];}],[0,function(_dv){return E(E(_dv)[1])==43?_ck(_cT,function(_dw){return A(_dr,[[1,new T(function(){return _dk(_da,_db,_dw);})]]);}):[2];}]),new T(function(){return _ck(_cT,function(_dx){return A(_dr,[[1,new T(function(){return _dk(_da,_db,_dx);})]]);});}));});return _aM([0,function(_dy){return E(E(_dy)[1])==101?E(_ds):[2];}],[0,function(_dz){return E(E(_dz)[1])==69?E(_ds):[2];}]);},_dA=function(_dB){return A(_dB,[_a]);},_dC=function(_dD){return A(_dD,[_a]);},_dE=function(_dF){var _dG=new T(function(){return _ck(_cT,function(_dH){return A(_dF,[[1,_dH]]);});});return [0,function(_dI){return E(E(_dI)[1])==46?E(_dG):[2];}];},_dJ=function(_dK){return _ck(_cT,function(_dL){return _bV(_dE,_dA,function(_dM){return _bV(_dq,_dC,function(_dN){return A(_dK,[[5,[1,_dL,_dM,_dN]]]);});});});},_dO=function(_dP,_dQ,_dR){while(1){var _dS=E(_dR);if(!_dS[0]){return false;}else{if(!A(_bk,[_dP,_dQ,_dS[1]])){_dR=_dS[2];continue;}else{return true;}}}},_dT=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dU=function(_dV){return _dO(_bj,_dV,_dT);},_dW=[0,8],_dX=[0,16],_dY=function(_dZ){var _e0=new T(function(){return _ck(_dX,function(_e1){return A(_dZ,[[5,[0,_dX,_e1]]]);});}),_e2=new T(function(){return _ck(_dW,function(_e3){return A(_dZ,[[5,[0,_dW,_e3]]]);});}),_e4=new T(function(){return _ck(_dX,function(_e5){return A(_dZ,[[5,[0,_dX,_e5]]]);});}),_e6=new T(function(){return _ck(_dW,function(_e7){return A(_dZ,[[5,[0,_dW,_e7]]]);});});return [0,function(_e8){return E(E(_e8)[1])==48?E([0,function(_e9){switch(E(E(_e9)[1])){case 79:return E(_e6);case 88:return E(_e4);case 111:return E(_e2);case 120:return E(_e0);default:return [2];}}]):[2];}];},_ea=false,_eb=true,_ec=function(_ed){var _ee=new T(function(){return A(_ed,[_dX]);}),_ef=new T(function(){return A(_ed,[_dW]);}),_eg=new T(function(){return A(_ed,[_dX]);}),_eh=new T(function(){return A(_ed,[_dW]);});return [0,function(_ei){switch(E(E(_ei)[1])){case 79:return E(_eh);case 88:return E(_eg);case 111:return E(_ef);case 120:return E(_ee);default:return [2];}}];},_ej=function(_ek){return A(_ek,[_cT]);},_el=function(_em){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3M(9,_em,_9);})));},_en=function(_eo){var _ep=E(_eo);return _ep[0]==0?E(_ep[1]):I_toInt(_ep[1]);},_eq=function(_er,_es){var _et=E(_er);if(!_et[0]){var _eu=_et[1],_ev=E(_es);return _ev[0]==0?_eu<=_ev[1]:I_compareInt(_ev[1],_eu)>=0;}else{var _ew=_et[1],_ex=E(_es);return _ex[0]==0?I_compareInt(_ew,_ex[1])<=0:I_compare(_ew,_ex[1])<=0;}},_ey=function(_ez){return [2];},_eA=function(_eB){var _eC=E(_eB);if(!_eC[0]){return E(_ey);}else{var _eD=_eC[1],_eE=E(_eC[2]);if(!_eE[0]){return E(_eD);}else{var _eF=new T(function(){return _eA(_eE);});return function(_eG){return _aM(A(_eD,[_eG]),new T(function(){return A(_eF,[_eG]);}));};}}},_eH=unCStr("NUL"),_eI=function(_eJ){return [2];},_eK=function(_eL){return _eI(_eL);},_eM=function(_eN,_eO){var _eP=function(_eQ,_eR){var _eS=E(_eQ);if(!_eS[0]){return function(_eT){return A(_eT,[_eN]);};}else{var _eU=E(_eR);if(!_eU[0]){return E(_eI);}else{if(E(_eS[1])[1]!=E(_eU[1])[1]){return E(_eK);}else{var _eV=new T(function(){return _eP(_eS[2],_eU[2]);});return function(_eW){var _eX=new T(function(){return A(_eV,[_eW]);});return [0,function(_eY){return E(_eX);}];};}}}};return [1,function(_eZ){return A(_eP,[_eN,_eZ,_eO]);}];},_f0=[0,0],_f1=function(_f2){var _f3=new T(function(){return A(_f2,[_f0]);});return _eM(_eH,function(_f4){return E(_f3);});},_f5=unCStr("STX"),_f6=[0,2],_f7=function(_f8){var _f9=new T(function(){return A(_f8,[_f6]);});return _eM(_f5,function(_fa){return E(_f9);});},_fb=unCStr("ETX"),_fc=[0,3],_fd=function(_fe){var _ff=new T(function(){return A(_fe,[_fc]);});return _eM(_fb,function(_fg){return E(_ff);});},_fh=unCStr("EOT"),_fi=[0,4],_fj=function(_fk){var _fl=new T(function(){return A(_fk,[_fi]);});return _eM(_fh,function(_fm){return E(_fl);});},_fn=unCStr("ENQ"),_fo=[0,5],_fp=function(_fq){var _fr=new T(function(){return A(_fq,[_fo]);});return _eM(_fn,function(_fs){return E(_fr);});},_ft=unCStr("ACK"),_fu=[0,6],_fv=function(_fw){var _fx=new T(function(){return A(_fw,[_fu]);});return _eM(_ft,function(_fy){return E(_fx);});},_fz=unCStr("BEL"),_fA=[0,7],_fB=function(_fC){var _fD=new T(function(){return A(_fC,[_fA]);});return _eM(_fz,function(_fE){return E(_fD);});},_fF=unCStr("BS"),_fG=[0,8],_fH=function(_fI){var _fJ=new T(function(){return A(_fI,[_fG]);});return _eM(_fF,function(_fK){return E(_fJ);});},_fL=unCStr("HT"),_fM=[0,9],_fN=function(_fO){var _fP=new T(function(){return A(_fO,[_fM]);});return _eM(_fL,function(_fQ){return E(_fP);});},_fR=unCStr("LF"),_fS=[0,10],_fT=function(_fU){var _fV=new T(function(){return A(_fU,[_fS]);});return _eM(_fR,function(_fW){return E(_fV);});},_fX=unCStr("VT"),_fY=[0,11],_fZ=function(_g0){var _g1=new T(function(){return A(_g0,[_fY]);});return _eM(_fX,function(_g2){return E(_g1);});},_g3=unCStr("FF"),_g4=[0,12],_g5=function(_g6){var _g7=new T(function(){return A(_g6,[_g4]);});return _eM(_g3,function(_g8){return E(_g7);});},_g9=unCStr("CR"),_ga=[0,13],_gb=function(_gc){var _gd=new T(function(){return A(_gc,[_ga]);});return _eM(_g9,function(_ge){return E(_gd);});},_gf=unCStr("SI"),_gg=[0,15],_gh=function(_gi){var _gj=new T(function(){return A(_gi,[_gg]);});return _eM(_gf,function(_gk){return E(_gj);});},_gl=unCStr("DLE"),_gm=[0,16],_gn=function(_go){var _gp=new T(function(){return A(_go,[_gm]);});return _eM(_gl,function(_gq){return E(_gp);});},_gr=unCStr("DC1"),_gs=[0,17],_gt=function(_gu){var _gv=new T(function(){return A(_gu,[_gs]);});return _eM(_gr,function(_gw){return E(_gv);});},_gx=unCStr("DC2"),_gy=[0,18],_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gy]);});return _eM(_gx,function(_gC){return E(_gB);});},_gD=unCStr("DC3"),_gE=[0,19],_gF=function(_gG){var _gH=new T(function(){return A(_gG,[_gE]);});return _eM(_gD,function(_gI){return E(_gH);});},_gJ=unCStr("DC4"),_gK=[0,20],_gL=function(_gM){var _gN=new T(function(){return A(_gM,[_gK]);});return _eM(_gJ,function(_gO){return E(_gN);});},_gP=unCStr("NAK"),_gQ=[0,21],_gR=function(_gS){var _gT=new T(function(){return A(_gS,[_gQ]);});return _eM(_gP,function(_gU){return E(_gT);});},_gV=unCStr("SYN"),_gW=[0,22],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _eM(_gV,function(_h0){return E(_gZ);});},_h1=unCStr("ETB"),_h2=[0,23],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _eM(_h1,function(_h6){return E(_h5);});},_h7=unCStr("CAN"),_h8=[0,24],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _eM(_h7,function(_hc){return E(_hb);});},_hd=unCStr("EM"),_he=[0,25],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _eM(_hd,function(_hi){return E(_hh);});},_hj=unCStr("SUB"),_hk=[0,26],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _eM(_hj,function(_ho){return E(_hn);});},_hp=unCStr("ESC"),_hq=[0,27],_hr=function(_hs){var _ht=new T(function(){return A(_hs,[_hq]);});return _eM(_hp,function(_hu){return E(_ht);});},_hv=unCStr("FS"),_hw=[0,28],_hx=function(_hy){var _hz=new T(function(){return A(_hy,[_hw]);});return _eM(_hv,function(_hA){return E(_hz);});},_hB=unCStr("GS"),_hC=[0,29],_hD=function(_hE){var _hF=new T(function(){return A(_hE,[_hC]);});return _eM(_hB,function(_hG){return E(_hF);});},_hH=unCStr("RS"),_hI=[0,30],_hJ=function(_hK){var _hL=new T(function(){return A(_hK,[_hI]);});return _eM(_hH,function(_hM){return E(_hL);});},_hN=unCStr("US"),_hO=[0,31],_hP=function(_hQ){var _hR=new T(function(){return A(_hQ,[_hO]);});return _eM(_hN,function(_hS){return E(_hR);});},_hT=unCStr("SP"),_hU=[0,32],_hV=function(_hW){var _hX=new T(function(){return A(_hW,[_hU]);});return _eM(_hT,function(_hY){return E(_hX);});},_hZ=unCStr("DEL"),_i0=[0,127],_i1=function(_i2){var _i3=new T(function(){return A(_i2,[_i0]);});return _eM(_hZ,function(_i4){return E(_i3);});},_i5=[1,_i1,_9],_i6=[1,_hV,_i5],_i7=[1,_hP,_i6],_i8=[1,_hJ,_i7],_i9=[1,_hD,_i8],_ia=[1,_hx,_i9],_ib=[1,_hr,_ia],_ic=[1,_hl,_ib],_id=[1,_hf,_ic],_ie=[1,_h9,_id],_if=[1,_h3,_ie],_ig=[1,_gX,_if],_ih=[1,_gR,_ig],_ii=[1,_gL,_ih],_ij=[1,_gF,_ii],_ik=[1,_gz,_ij],_il=[1,_gt,_ik],_im=[1,_gn,_il],_in=[1,_gh,_im],_io=[1,_gb,_in],_ip=[1,_g5,_io],_iq=[1,_fZ,_ip],_ir=[1,_fT,_iq],_is=[1,_fN,_ir],_it=[1,_fH,_is],_iu=[1,_fB,_it],_iv=[1,_fv,_iu],_iw=[1,_fp,_iv],_ix=[1,_fj,_iw],_iy=[1,_fd,_ix],_iz=[1,_f7,_iy],_iA=[1,_f1,_iz],_iB=unCStr("SOH"),_iC=[0,1],_iD=function(_iE){var _iF=new T(function(){return A(_iE,[_iC]);});return _eM(_iB,function(_iG){return E(_iF);});},_iH=unCStr("SO"),_iI=[0,14],_iJ=function(_iK){var _iL=new T(function(){return A(_iK,[_iI]);});return _eM(_iH,function(_iM){return E(_iL);});},_iN=function(_iO){return _bV(_iD,_iJ,_iO);},_iP=[1,_iN,_iA],_iQ=new T(function(){return _eA(_iP);}),_iR=[0,1114111],_iS=[0,34],_iT=[0,_iS,_eb],_iU=[0,39],_iV=[0,_iU,_eb],_iW=[0,92],_iX=[0,_iW,_eb],_iY=[0,_fA,_eb],_iZ=[0,_fG,_eb],_j0=[0,_g4,_eb],_j1=[0,_fS,_eb],_j2=[0,_ga,_eb],_j3=[0,_fM,_eb],_j4=[0,_fY,_eb],_j5=[0,_f0,_eb],_j6=[0,_iC,_eb],_j7=[0,_f6,_eb],_j8=[0,_fc,_eb],_j9=[0,_fi,_eb],_ja=[0,_fo,_eb],_jb=[0,_fu,_eb],_jc=[0,_fA,_eb],_jd=[0,_fG,_eb],_je=[0,_fM,_eb],_jf=[0,_fS,_eb],_jg=[0,_fY,_eb],_jh=[0,_g4,_eb],_ji=[0,_ga,_eb],_jj=[0,_iI,_eb],_jk=[0,_gg,_eb],_jl=[0,_gm,_eb],_jm=[0,_gs,_eb],_jn=[0,_gy,_eb],_jo=[0,_gE,_eb],_jp=[0,_gK,_eb],_jq=[0,_gQ,_eb],_jr=[0,_gW,_eb],_js=[0,_h2,_eb],_jt=[0,_h8,_eb],_ju=[0,_he,_eb],_jv=[0,_hk,_eb],_jw=[0,_hq,_eb],_jx=[0,_hw,_eb],_jy=[0,_hC,_eb],_jz=[0,_hI,_eb],_jA=[0,_hO,_eb],_jB=function(_jC){return [0,_jC];},_jD=function(_jE){var _jF=new T(function(){return A(_jE,[_j4]);}),_jG=new T(function(){return A(_jE,[_j3]);}),_jH=new T(function(){return A(_jE,[_j2]);}),_jI=new T(function(){return A(_jE,[_j1]);}),_jJ=new T(function(){return A(_jE,[_j0]);}),_jK=new T(function(){return A(_jE,[_iZ]);}),_jL=new T(function(){return A(_jE,[_iY]);}),_jM=new T(function(){return A(_jE,[_iX]);}),_jN=new T(function(){return A(_jE,[_iV]);}),_jO=new T(function(){return A(_jE,[_iT]);});return _aM([0,function(_jP){switch(E(E(_jP)[1])){case 34:return E(_jO);case 39:return E(_jN);case 92:return E(_jM);case 97:return E(_jL);case 98:return E(_jK);case 102:return E(_jJ);case 110:return E(_jI);case 114:return E(_jH);case 116:return E(_jG);case 118:return E(_jF);default:return [2];}}],new T(function(){return _aM(_bV(_ec,_ej,function(_jQ){var _jR=new T(function(){return _jB(E(_jQ)[1]);});return _ck(_jQ,function(_jS){var _jT=_dk(_jR,_db,_jS);return !_eq(_jT,_iR)?[2]:A(_jE,[[0,new T(function(){var _jU=_en(_jT);return _jU>>>0>1114111?_el(_jU):[0,_jU];}),_eb]]);});}),new T(function(){var _jV=new T(function(){return A(_jE,[_jA]);}),_jW=new T(function(){return A(_jE,[_jz]);}),_jX=new T(function(){return A(_jE,[_jy]);}),_jY=new T(function(){return A(_jE,[_jx]);}),_jZ=new T(function(){return A(_jE,[_jw]);}),_k0=new T(function(){return A(_jE,[_jv]);}),_k1=new T(function(){return A(_jE,[_ju]);}),_k2=new T(function(){return A(_jE,[_jt]);}),_k3=new T(function(){return A(_jE,[_js]);}),_k4=new T(function(){return A(_jE,[_jr]);}),_k5=new T(function(){return A(_jE,[_jq]);}),_k6=new T(function(){return A(_jE,[_jp]);}),_k7=new T(function(){return A(_jE,[_jo]);}),_k8=new T(function(){return A(_jE,[_jn]);}),_k9=new T(function(){return A(_jE,[_jm]);}),_ka=new T(function(){return A(_jE,[_jl]);}),_kb=new T(function(){return A(_jE,[_jk]);}),_kc=new T(function(){return A(_jE,[_jj]);}),_kd=new T(function(){return A(_jE,[_ji]);}),_ke=new T(function(){return A(_jE,[_jh]);}),_kf=new T(function(){return A(_jE,[_jg]);}),_kg=new T(function(){return A(_jE,[_jf]);}),_kh=new T(function(){return A(_jE,[_je]);}),_ki=new T(function(){return A(_jE,[_jd]);}),_kj=new T(function(){return A(_jE,[_jc]);}),_kk=new T(function(){return A(_jE,[_jb]);}),_kl=new T(function(){return A(_jE,[_ja]);}),_km=new T(function(){return A(_jE,[_j9]);}),_kn=new T(function(){return A(_jE,[_j8]);}),_ko=new T(function(){return A(_jE,[_j7]);}),_kp=new T(function(){return A(_jE,[_j6]);}),_kq=new T(function(){return A(_jE,[_j5]);});return _aM([0,function(_kr){return E(E(_kr)[1])==94?E([0,function(_ks){switch(E(E(_ks)[1])){case 64:return E(_kq);case 65:return E(_kp);case 66:return E(_ko);case 67:return E(_kn);case 68:return E(_km);case 69:return E(_kl);case 70:return E(_kk);case 71:return E(_kj);case 72:return E(_ki);case 73:return E(_kh);case 74:return E(_kg);case 75:return E(_kf);case 76:return E(_ke);case 77:return E(_kd);case 78:return E(_kc);case 79:return E(_kb);case 80:return E(_ka);case 81:return E(_k9);case 82:return E(_k8);case 83:return E(_k7);case 84:return E(_k6);case 85:return E(_k5);case 86:return E(_k4);case 87:return E(_k3);case 88:return E(_k2);case 89:return E(_k1);case 90:return E(_k0);case 91:return E(_jZ);case 92:return E(_jY);case 93:return E(_jX);case 94:return E(_jW);case 95:return E(_jV);default:return [2];}}]):[2];}],new T(function(){return A(_iQ,[function(_kt){return A(_jE,[[0,_kt,_eb]]);}]);}));}));}));},_ku=function(_kv){return A(_kv,[_0]);},_kw=function(_kx){var _ky=E(_kx);if(!_ky[0]){return E(_ku);}else{var _kz=_ky[2],_kA=E(E(_ky[1])[1]);switch(_kA){case 9:var _kB=new T(function(){return _kw(_kz);});return function(_kC){var _kD=new T(function(){return A(_kB,[_kC]);});return [0,function(_kE){return E(_kD);}];};case 10:var _kF=new T(function(){return _kw(_kz);});return function(_kG){var _kH=new T(function(){return A(_kF,[_kG]);});return [0,function(_kI){return E(_kH);}];};case 11:var _kJ=new T(function(){return _kw(_kz);});return function(_kK){var _kL=new T(function(){return A(_kJ,[_kK]);});return [0,function(_kM){return E(_kL);}];};case 12:var _kN=new T(function(){return _kw(_kz);});return function(_kO){var _kP=new T(function(){return A(_kN,[_kO]);});return [0,function(_kQ){return E(_kP);}];};case 13:var _kR=new T(function(){return _kw(_kz);});return function(_kS){var _kT=new T(function(){return A(_kR,[_kS]);});return [0,function(_kU){return E(_kT);}];};case 32:var _kV=new T(function(){return _kw(_kz);});return function(_kW){var _kX=new T(function(){return A(_kV,[_kW]);});return [0,function(_kY){return E(_kX);}];};case 160:var _kZ=new T(function(){return _kw(_kz);});return function(_l0){var _l1=new T(function(){return A(_kZ,[_l0]);});return [0,function(_l2){return E(_l1);}];};default:var _l3=u_iswspace(_kA);if(!E(_l3)){return E(_ku);}else{var _l4=new T(function(){return _kw(_kz);});return function(_l5){var _l6=new T(function(){return A(_l4,[_l5]);});return [0,function(_l7){return E(_l6);}];};}}}},_l8=function(_l9){var _la=new T(function(){return _jD(_l9);}),_lb=new T(function(){return _l8(_l9);}),_lc=[1,function(_ld){return A(_kw,[_ld,function(_le){return E([0,function(_lf){return E(E(_lf)[1])==92?E(_lb):[2];}]);}]);}];return _aM([0,function(_lg){return E(E(_lg)[1])==92?E([0,function(_lh){var _li=E(E(_lh)[1]);switch(_li){case 9:return E(_lc);case 10:return E(_lc);case 11:return E(_lc);case 12:return E(_lc);case 13:return E(_lc);case 32:return E(_lc);case 38:return E(_lb);case 160:return E(_lc);default:var _lj=u_iswspace(_li);return E(_lj)==0?[2]:E(_lc);}}]):[2];}],[0,function(_lk){var _ll=E(_lk);return E(_ll[1])==92?E(_la):A(_l9,[[0,_ll,_ea]]);}]);},_lm=function(_ln,_lo){var _lp=new T(function(){return A(_lo,[[1,new T(function(){return A(_ln,[_9]);})]]);});return _l8(function(_lq){var _lr=E(_lq),_ls=E(_lr[1]);return E(_ls[1])==34?!E(_lr[2])?E(_lp):_lm(function(_lt){return A(_ln,[[1,_ls,_lt]]);},_lo):_lm(function(_lu){return A(_ln,[[1,_ls,_lu]]);},_lo);});},_lv=unCStr("_\'"),_lw=function(_lx){var _ly=u_iswalnum(_lx);return E(_ly)==0?_dO(_bj,[0,_lx],_lv):true;},_lz=function(_lA){return _lw(E(_lA)[1]);},_lB=unCStr(",;()[]{}`"),_lC=function(_lD){return A(_lD,[_9]);},_lE=function(_lF,_lG){var _lH=function(_lI){var _lJ=E(_lI);if(!_lJ[0]){return E(_lC);}else{var _lK=_lJ[1];if(!A(_lF,[_lK])){return E(_lC);}else{var _lL=new T(function(){return _lH(_lJ[2]);});return function(_lM){var _lN=new T(function(){return A(_lL,[function(_lO){return A(_lM,[[1,_lK,_lO]]);}]);});return [0,function(_lP){return E(_lN);}];};}}};return [1,function(_lQ){return A(_lH,[_lQ,_lG]);}];},_lR=unCStr(".."),_lS=unCStr("::"),_lT=unCStr("->"),_lU=[0,64],_lV=[1,_lU,_9],_lW=[0,126],_lX=[1,_lW,_9],_lY=unCStr("=>"),_lZ=[1,_lY,_9],_m0=[1,_lX,_lZ],_m1=[1,_lV,_m0],_m2=[1,_lT,_m1],_m3=unCStr("<-"),_m4=[1,_m3,_m2],_m5=[0,124],_m6=[1,_m5,_9],_m7=[1,_m6,_m4],_m8=[1,_iW,_9],_m9=[1,_m8,_m7],_ma=[0,61],_mb=[1,_ma,_9],_mc=[1,_mb,_m9],_md=[1,_lS,_mc],_me=[1,_lR,_md],_mf=function(_mg){var _mh=new T(function(){return A(_mg,[_ch]);});return _aM([1,function(_mi){return E(_mi)[0]==0?E(_mh):[2];}],new T(function(){var _mj=new T(function(){return _jD(function(_mk){var _ml=E(_mk);return (function(_mm,_mn){var _mo=new T(function(){return A(_mg,[[0,_mm]]);});return !E(_mn)?E(E(_mm)[1])==39?[2]:[0,function(_mp){return E(E(_mp)[1])==39?E(_mo):[2];}]:[0,function(_mq){return E(E(_mq)[1])==39?E(_mo):[2];}];})(_ml[1],_ml[2]);});});return _aM([0,function(_mr){return E(E(_mr)[1])==39?E([0,function(_ms){var _mt=E(_ms);switch(E(_mt[1])){case 39:return [2];case 92:return E(_mj);default:var _mu=new T(function(){return A(_mg,[[0,_mt]]);});return [0,function(_mv){return E(E(_mv)[1])==39?E(_mu):[2];}];}}]):[2];}],new T(function(){var _mw=new T(function(){return _lm(_n,_mg);});return _aM([0,function(_mx){return E(E(_mx)[1])==34?E(_mw):[2];}],new T(function(){return _aM([0,function(_my){return !_dO(_bj,_my,_lB)?[2]:A(_mg,[[2,[1,_my,_9]]]);}],new T(function(){return _aM([0,function(_mz){return !_dO(_bj,_mz,_dT)?[2]:_lE(_dU,function(_mA){var _mB=[1,_mz,_mA];return !_dO(_bA,_mB,_me)?A(_mg,[[4,_mB]]):A(_mg,[[2,_mB]]);});}],new T(function(){return _aM([0,function(_mC){var _mD=E(_mC),_mE=_mD[1],_mF=u_iswalpha(_mE);return E(_mF)==0?E(_mE)==95?_lE(_lz,function(_mG){return A(_mg,[[3,[1,_mD,_mG]]]);}):[2]:_lE(_lz,function(_mH){return A(_mg,[[3,[1,_mD,_mH]]]);});}],new T(function(){return _bV(_dY,_dJ,_mg);}));}));}));}));}));}));},_mI=function(_mJ){var _mK=new T(function(){return _mf(_mJ);});return [1,function(_mL){return A(_kw,[_mL,function(_mM){return E(_mK);}]);}];},_mN=[0,0],_mO=function(_mP,_mQ){var _mR=new T(function(){return A(_mP,[_mN,function(_mS){var _mT=new T(function(){return A(_mQ,[_mS]);});return _mI(function(_mU){var _mV=E(_mU);if(_mV[0]==2){var _mW=E(_mV[1]);return _mW[0]==0?[2]:E(E(_mW[1])[1])==41?E(_mW[2])[0]==0?E(_mT):[2]:[2];}else{return [2];}});}]);});return _mI(function(_mX){var _mY=E(_mX);if(_mY[0]==2){var _mZ=E(_mY[1]);return _mZ[0]==0?[2]:E(E(_mZ[1])[1])==40?E(_mZ[2])[0]==0?E(_mR):[2]:[2];}else{return [2];}});},_n0=function(_n1,_n2,_n3){var _n4=function(_n5,_n6){var _n7=new T(function(){return _mf(function(_n8){return A(_n1,[_n8,_n5,function(_n9){return A(_n6,[new T(function(){return [0, -E(_n9)[1]];})]);}]);});});return _aM(_mI(function(_na){var _nb=E(_na);if(_nb[0]==4){var _nc=E(_nb[1]);return _nc[0]==0?A(_n1,[_nb,_n5,_n6]):E(E(_nc[1])[1])==45?E(_nc[2])[0]==0?E([1,function(_nd){return A(_kw,[_nd,function(_ne){return E(_n7);}]);}]):A(_n1,[_nb,_n5,_n6]):A(_n1,[_nb,_n5,_n6]);}else{return A(_n1,[_nb,_n5,_n6]);}}),new T(function(){return _mO(_n4,_n6);}));};return _n4(_n2,_n3);},_nf=function(_ng,_nh){return [2];},_ni=function(_nj,_nk){return _nf(_nj,_nk);},_nl=function(_nm){var _nn=E(_nm);return _nn[0]==0?[1,new T(function(){return _dk(new T(function(){return _jB(E(_nn[1])[1]);}),_db,_nn[2]);})]:E(_nn[2])[0]==0?E(_nn[3])[0]==0?[1,new T(function(){return _dk(_da,_db,_nn[1]);})]:[0]:[0];},_no=function(_np){var _nq=E(_np);if(_nq[0]==5){var _nr=_nl(_nq[1]);if(!_nr[0]){return E(_nf);}else{var _ns=new T(function(){return [0,_en(_nr[1])];});return function(_nt,_nu){return A(_nu,[_ns]);};}}else{return E(_ni);}},_nv=function(_nj,_nk){return _n0(_no,_nj,_nk);},_nw=function(_nx,_ny){var _nz=function(_nA,_nB){var _nC=new T(function(){return A(_nB,[_9]);}),_nD=new T(function(){return A(_nx,[_mN,function(_nE){return _nz(_eb,function(_nF){return A(_nB,[[1,_nE,_nF]]);});}]);});return _mI(function(_nG){var _nH=E(_nG);if(_nH[0]==2){var _nI=E(_nH[1]);if(!_nI[0]){return [2];}else{var _nJ=_nI[2];switch(E(E(_nI[1])[1])){case 44:return E(_nJ)[0]==0?!E(_nA)?[2]:E(_nD):[2];case 93:return E(_nJ)[0]==0?E(_nC):[2];default:return [2];}}}else{return [2];}});},_nK=function(_nL){var _nM=new T(function(){return _aM(_nz(_ea,_nL),new T(function(){return A(_nx,[_mN,function(_nN){return _nz(_eb,function(_nO){return A(_nL,[[1,_nN,_nO]]);});}]);}));});return _aM(_mI(function(_nP){var _nQ=E(_nP);if(_nQ[0]==2){var _nR=E(_nQ[1]);return _nR[0]==0?[2]:E(E(_nR[1])[1])==91?E(_nR[2])[0]==0?E(_nM):[2]:[2];}else{return [2];}}),new T(function(){return _mO(function(_nS,_nT){return _nK(_nT);},_nL);}));};return _nK(_ny);},_nU=function(_nV,_nW){return _nw(_nv,_nW);},_nX=new T(function(){return _nw(_nv,_bN);}),_nY=function(_nk){return _aC(_nX,_nk);},_nZ=function(_o0){var _o1=new T(function(){return _n0(_no,_o0,_bN);});return function(_cg){return _aC(_o1,_cg);};},_o2=[0,_nZ,_nY,_nv,_nU],_o3=function(_o4,_o5){return _3M(0,E(_o4)[1],_o5);},_o6=function(_o7,_o8){return _2D(_o3,_o7,_o8);},_o9=function(_oa,_ob,_oc){return _3M(E(_oa)[1],E(_ob)[1],_oc);},_od=[0,_o9,_5h,_o6],_oe=unCStr("GHC.Types"),_of=unCStr("Int"),_og=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5M,_oe,_of],_oh=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_og,_9],_oi=function(_oj){return E(_oh);},_ok=function(_ol){return E(E(_ol)[1]);},_om=function(_on){return E(E(_on)[2]);},_oo=function(_op,_oq){var _or=new T(function(){return A(_om,[_op,_oq]);}),_os=new T(function(){return _ok(_op);}),_ot=new T(function(){return _3x(_os);}),_ou=new T(function(){return _37(_os);});return function(_ov){return A(_ou,[_or,function(_ow){return A(_ot,[[0,_ow,_ov]]);}]);};},_ox=function(_oy,_oz){return [0,_oy,function(_oA){return _oo(_oz,_oA);}];},_oB=function(_oC,_oD){return A(_3x,[_oC,[0,_oD,_oD]]);},_oE=function(_oF,_oG,_oH){return A(_3x,[_oF,[0,_0,_oG]]);},_oI=function(_oJ,_oK){return [0,_oJ,function(_oL){return _oB(_oK,_oL);},function(_oM,_oN){return _oE(_oK,_oM,_oN);}];},_oO=function(_oP,_oQ){return A(_oP,[function(_){return jsFind(toJSStr(E(_oQ)));}]);},_oR=function(_oS){return E(E(_oS)[3]);},_oT=unCStr("[]"),_oU=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5M,_oe,_oT],_oV=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oU,_9],_oW=function(_oX){return E(_oV);},_oY=unCStr("Char"),_oZ=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5M,_oe,_oY],_p0=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oZ,_9],_p1=function(_p2){return E(_p0);},_p3=new T(function(){return _6G(_oW,_p1);}),_p4=new T(function(){return A(_p3,[_6F]);}),_p5=new T(function(){return E(_6F);}),_p6=function(_p7){return E(E(_p7)[6]);},_p8=function(_p9){return E(E(_p9)[1]);},_pa=[0,0],_pb=[0,32],_pc=[0,10],_pd=function(_pe){var _pf=E(_pe);if(!_pf[0]){return E(_n);}else{var _pg=_pf[1],_ph=E(_pf[2]);if(!_ph[0]){return _pi(_pc,_pg);}else{var _pj=new T(function(){return _pd(_ph);}),_pk=new T(function(){return _pi(_pc,_pg);});return function(_pl){return A(_pk,[[1,_pb,new T(function(){return A(_pj,[_pl]);})]]);};}}},_pm=unCStr("->"),_pn=[1,_pm,_9],_po=[1,_oe,_pn],_pp=[1,_5M,_po],_pq=[0,32],_pr=function(_ps){var _pt=E(_ps);if(!_pt[0]){return [0];}else{var _pu=_pt[1],_pv=E(_pt[2]);return _pv[0]==0?E(_pu):_1N(_pu,[1,_pq,new T(function(){return _pr(_pv);})]);}},_pw=new T(function(){return _pr(_pp);}),_px=new T(function(){var _py=_6a(_pw);return [0,_py[1],_py[2],_5M,_oe,_pm];}),_pz=function(_pA,_pB){var _pC=E(_pA);return _pC[0]==0?E(_pB):A(_pC[1],[new T(function(){return _pz(_pC[2],_pB);})]);},_pD=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pE=[1,_5O,_9],_pF=function(_pG){var _pH=E(_pG);if(!_pH[0]){return [0];}else{var _pI=E(_pH[1]);return [1,[0,_pI[1],_pI[2]],new T(function(){return _pF(_pH[2]);})];}},_pJ=new T(function(){var _pK=_1N(_9,_pE);if(!_pK[0]){return E(_oU);}else{var _pL=_6a(new T(function(){return _5Y(_6m(_6x,[1,_pD,new T(function(){return _pF(_pK);})]));}));return E(_oU);}}),_pM=[0,40],_pN=function(_pO){return _pi(_pc,_pO);},_pP=[0,8],_pQ=unCStr(" -> "),_pR=[0,9],_pS=[0,93],_pT=[0,91],_pU=[0,41],_pV=[0,44],_pW=function(_pO){return [1,_pV,_pO];},_pX=function(_pY,_pZ){var _q0=E(_pZ);return _q0[0]==0?[0]:[1,_pY,[1,_q0[1],new T(function(){return _pX(_pY,_q0[2]);})]];},_pi=function(_q1,_q2){var _q3=E(_q2),_q4=_q3[3],_q5=E(_q3[4]);if(!_q5[0]){return function(_q6){return _1N(E(_q4)[5],_q6);};}else{var _q7=_q5[1],_q8=new T(function(){var _q9=E(_q4)[5],_qa=new T(function(){return _pd(_q5);}),_qb=new T(function(){return E(_q1)[1]<=9?function(_qc){return _1N(_q9,[1,_pb,new T(function(){return A(_qa,[_qc]);})]);}:function(_qd){return [1,_3L,new T(function(){return _1N(_q9,[1,_pb,new T(function(){return A(_qa,[[1,_3K,_qd]]);})]);})];};}),_qe=E(_q9);if(!_qe[0]){return E(_qb);}else{if(E(E(_qe[1])[1])==40){var _qf=E(_qe[2]);return _qf[0]==0?E(_qb):E(E(_qf[1])[1])==44?function(_qg){return [1,_pM,new T(function(){return A(new T(function(){var _qh=_6m(_pN,_q5);if(!_qh[0]){return E(_n);}else{var _qi=new T(function(){return _pX(_pW,_qh[2]);});return function(_cg){return _pz([1,_qh[1],_qi],_cg);};}}),[[1,_pU,_qg]]);})];}:E(_qb);}else{return E(_qb);}}}),_qj=E(_q5[2]);if(!_qj[0]){var _qk=E(_q4),_ql=E(_pJ),_qm=hs_eqWord64(_qk[1],_ql[1]);if(!E(_qm)){return E(_q8);}else{var _qn=hs_eqWord64(_qk[2],_ql[2]);if(!E(_qn)){return E(_q8);}else{var _qo=new T(function(){return _pi(_pa,_q7);});return function(_qp){return [1,_pT,new T(function(){return A(_qo,[[1,_pS,_qp]]);})];};}}}else{if(!E(_qj[2])[0]){var _qq=E(_q4),_qr=E(_px),_qs=hs_eqWord64(_qq[1],_qr[1]);if(!E(_qs)){return E(_q8);}else{var _qt=hs_eqWord64(_qq[2],_qr[2]);if(!E(_qt)){return E(_q8);}else{var _qu=new T(function(){return _pi(_pP,_qj[1]);}),_qv=new T(function(){return _pi(_pR,_q7);});return E(_q1)[1]<=8?function(_qw){return A(_qv,[new T(function(){return _1N(_pQ,new T(function(){return A(_qu,[_qw]);}));})]);}:function(_qx){return [1,_3L,new T(function(){return A(_qv,[new T(function(){return _1N(_pQ,new T(function(){return A(_qu,[[1,_3K,_qx]]);}));})]);})];};}}}else{return E(_q8);}}}},_qy=function(_qz,_qA,_qB,_qC){var _qD=new T(function(){return _3x(_qz);}),_qE=new T(function(){return _oR(_qC);}),_qF=new T(function(){return _p6(_qC);}),_qG=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pi,[_pa,A(_qA,[_p5]),_9]);}));}),_qH=new T(function(){return A(_p8,[_qB,_f]);});return function(_qI){if(!E(new T(function(){var _qJ=A(_qA,[_p5]),_qK=E(_p4),_qL=hs_eqWord64(_qJ[1],_qK[1]);if(!E(_qL)){return false;}else{var _qM=hs_eqWord64(_qJ[2],_qK[2]);return E(_qM)==0?false:true;}}))){var _qN=new T(function(){return A(_qD,[[1,_qI,new T(function(){return A(_qF,[new T(function(){return A(_qE,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1N(_qI,_qG);}));})]);})]);})]]);}),_qO=A(_qH,[_qI]);if(!_qO[0]){return E(_qN);}else{var _qP=E(_qO[1]);return E(_qP[2])[0]==0?E(_qO[2])[0]==0?A(_qD,[[2,_qP[1]]]):E(_qN):E(_qN);}}else{return A(_qD,[[2,_qI]]);}};},_qQ=[0],_qR=new T(function(){return [0,"value"];}),_qS=function(_qT,_qU,_qV,_qW,_qX,_qY){var _qZ=E(_qT),_r0=_qZ[1],_r1=new T(function(){return A(_qZ[3],[_qQ]);}),_r2=new T(function(){return _qy(_qZ,_qV,_qW,_qX);});return A(_r0,[new T(function(){return _oO(_qU,_qY);}),function(_r3){var _r4=E(_r3);return _r4[0]==0?E(_r1):A(_r0,[new T(function(){return A(_qU,[function(_){var _r5=jsGet(E(_r4[1])[1],E(_qR)[1]);return [1,new T(function(){return fromJSStr(_r5);})];}]);}),function(_r6){var _r7=E(_r6);return _r7[0]==0?E(_r1):A(_r2,[_r7[1]]);}]);}]);},_r8=1,_r9=function(_ra){return E(E(_ra)[9]);},_rb=function(_rc){return E(E(_rc)[2]);},_rd=function(_re){return E(E(_re)[3]);},_rf=function(_rg){return E(E(_rg)[2]);},_rh=function(_ri,_rj,_rk,_rl,_rm,_rn,_ro,_rp,_rq,_rr,_rs,_rt){var _ru=_ok(_rn),_rv=_ru[1],_rw=_ru[3],_rx=new T(function(){return _90(_rp);}),_ry=new T(function(){return _92(_rx);}),_rz=new T(function(){return _rd(_ro);}),_rA=new T(function(){return _rf(_rj);}),_rB=new T(function(){return _r9(_rp);});return A(_rv,[new T(function(){var _rC=E(_rr);if(!_rC[0]){var _rD=E(_ro);return _3R(_rq,_rD[1],_rD[2],_rD[3]);}else{return A(_rw,[_rC[1]]);}}),function(_rE){return A(_rv,[new T(function(){var _rF=E(_rq);return _rb(_ro);}),function(_rG){return A(_ru[2],[new T(function(){return A(_rz,[new T(function(){var _rH=E(new T(function(){var _rI=E(_rq);return [0,coercionToken];})),_rJ=E(_rG);return [0,_rJ[1],_rJ[2],_r8,_rJ[4],_rJ[5]];})]);}),new T(function(){var _rK=new T(function(){return A(_rw,[[0,new T(function(){return A(_rB,[_rE,_rs,new T(function(){var _rL=E(_rt);if(!_rL[0]){return [0];}else{var _rM=_rL[1],_rN=_1x(_rm,_p3,_rM);return _rN[0]==0?A(_rf,[_rk,_rM]):E(_rN[1]);}}),_ea,_a]);}),_a]]);});return A(_rv,[new T(function(){var _rO=E(_rn);return _qS(_rO[1],_rO[2],_rl,_ri,_rp,_rE);}),function(_rP){var _rQ=E(_rP);switch(_rQ[0]){case 0:return E(_rK);case 1:return A(_rw,[[0,new T(function(){return A(_ry,[new T(function(){return A(_rB,[_rE,_rs,_rQ[1],_ea,_a]);}),_rQ[2]]);}),_a]]);default:var _rR=_rQ[1];return A(_rw,[[0,new T(function(){return A(_rB,[_rE,_rs,new T(function(){var _rS=_1x(_rl,_p3,_rR);return _rS[0]==0?A(_rA,[_rR]):E(_rS[1]);}),_ea,_a]);}),[1,_rR]]]);}}]);})]);}]);}]);},_rT=function(_rU,_rV,_rW,_rX,_rY){var _rZ=new T(function(){return _ok(_rV);}),_s0=new T(function(){return _3z(_rZ);}),_s1=new T(function(){return _oI(_s0,_rZ);}),_s2=new T(function(){return _ox(_s0,_rV);});return function(_cg,_s3,_s4){return _rh(_rY,_rX,_rX,_rW,_rW,_s2,_s1,_rU,[0,coercionToken],_cg,_s3,_s4);};},_s5=new T(function(){return _rT(_8D,_9J,_oi,_od,_o2);}),_s6=new T(function(){return A(_s5,[_a,_9I,_a]);}),_s7=unCStr("keydown"),_s8=unCStr("mousemove"),_s9=unCStr("blur"),_sa=unCStr("focus"),_sb=unCStr("change"),_sc=unCStr("unload"),_sd=unCStr("load"),_se=unCStr("keyup"),_sf=unCStr("keypress"),_sg=unCStr("mouseup"),_sh=unCStr("mousedown"),_si=unCStr("dblclick"),_sj=unCStr("click"),_sk=unCStr("mouseout"),_sl=unCStr("mouseover"),_sm=function(_sn){switch(E(_sn)[0]){case 0:return E(_sd);case 1:return E(_sc);case 2:return E(_sb);case 3:return E(_sa);case 4:return E(_s9);case 5:return E(_s8);case 6:return E(_sl);case 7:return E(_sk);case 8:return E(_sj);case 9:return E(_si);case 10:return E(_sh);case 11:return E(_sg);case 12:return E(_sf);case 13:return E(_se);default:return E(_s7);}},_so=[0],_sp=unCStr("OnLoad"),_sq=[0,_sp,_so],_sr=function(_){var _=0,_ss=newMVar(),_=putMVar(_ss,_sq);return [0,_ss];},_st=new T(function(){return _j(_sr);}),_su=function(_sv,_sw,_){var _sx=A(_sv,[_]);return die(_sw);},_sy=function(_sz,_sA,_sB,_){return _su(function(_){var _=putMVar(_sA,_sz);return _0;},_sB,_);},_sC=function(_sD,_){var _sE=0;if(!E(_sE)){return (function(_){var _sF=E(_st)[1],_sG=takeMVar(_sF),_sH=jsCatch(function(_){return (function(_){return _sD;})();},function(_X,_){return _sy(_sG,_sF,_X,_);}),_=putMVar(_sF,_sH);return _0;})();}else{var _sI=E(_st)[1],_sJ=takeMVar(_sI),_sK=jsCatch(function(_){return _sD;},function(_X,_){return _sy(_sJ,_sI,_X,_);}),_=putMVar(_sI,_sK);return _0;}},_sL=unCStr("true"),_sM=function(_sN,_sO){while(1){var _sP=E(_sN);if(!_sP[0]){return E(_sO)[0]==0?true:false;}else{var _sQ=E(_sO);if(!_sQ[0]){return false;}else{if(E(_sP[1])[1]!=E(_sQ[1])[1]){return false;}else{_sN=_sP[2];_sO=_sQ[2];continue;}}}}},_sR=new T(function(){return [0,"keydown"];}),_sS=new T(function(){return [0,"mousemove"];}),_sT=new T(function(){return [0,"blur"];}),_sU=new T(function(){return [0,"focus"];}),_sV=new T(function(){return [0,"change"];}),_sW=new T(function(){return [0,"unload"];}),_sX=new T(function(){return [0,"load"];}),_sY=new T(function(){return [0,"keyup"];}),_sZ=new T(function(){return [0,"keypress"];}),_t0=new T(function(){return [0,"mouseup"];}),_t1=new T(function(){return [0,"mousedown"];}),_t2=new T(function(){return [0,"dblclick"];}),_t3=new T(function(){return [0,"click"];}),_t4=new T(function(){return [0,"mouseout"];}),_t5=new T(function(){return [0,"mouseover"];}),_t6=function(_t7){switch(E(_t7)[0]){case 0:return E(_sX);case 1:return E(_sW);case 2:return E(_sV);case 3:return E(_sU);case 4:return E(_sT);case 5:return E(_sS);case 6:return E(_t5);case 7:return E(_t4);case 8:return E(_t3);case 9:return E(_t2);case 10:return E(_t1);case 11:return E(_t0);case 12:return E(_sZ);case 13:return E(_sY);default:return E(_sR);}},_t8=function(_t9,_ta,_tb){var _tc=new T(function(){return _sm(_ta);}),_td=new T(function(){return _t6(_ta);});return function(_te,_){var _tf=A(_t9,[_te,_]),_tg=E(_tf),_th=_tg[1],_ti=E(_tc),_tj=jsGetAttr(_th,toJSStr(_ti));if(!_sM(fromJSStr(_tj),_sL)){var _tk=E(_tb),_tl=jsSetCB(_th,E(_td)[1],E([0,_tb])[1]),_tm=A(_1,[_n,_tg,_ti,_sL,_]);return _tg;}else{return _tg;}};},_tn=function(_to,_tp){var _tq=new T(function(){return _sm(_tp);}),_tr=[0,_tq,_so];return function(_ts,_){var _tt=E(_ts),_tu=E(_tt[4]),_tv=_tu[1],_tw=_tu[2],_tx=A(_to,[_tt,_]),_ty=E(_tx),_tz=E(_ty[1]),_tA=_tz[1];return [0,[0,new T(function(){var _tB=E(_tp);switch(_tB[0]){case 0:return _t8(_tA,_tB,function(_){var _tC=_sC(_tr,_),_tD=A(_tv,[_]),_tE=E(_tD);if(!_tE[0]){return _0;}else{var _tF=A(_tw,[_tE[1],_]);return _0;}});case 1:return _t8(_tA,_tB,function(_){var _tG=_sC(_tr,_),_tH=A(_tv,[_]),_tI=E(_tH);if(!_tI[0]){return _0;}else{var _tJ=A(_tw,[_tI[1],_]);return _0;}});case 2:return _t8(_tA,_tB,function(_){var _tK=_sC(_tr,_),_tL=A(_tv,[_]),_tM=E(_tL);if(!_tM[0]){return _0;}else{var _tN=A(_tw,[_tM[1],_]);return _0;}});case 3:return _t8(_tA,_tB,function(_){var _tO=_sC(_tr,_),_tP=A(_tv,[_]),_tQ=E(_tP);if(!_tQ[0]){return _0;}else{var _tR=A(_tw,[_tQ[1],_]);return _0;}});case 4:return _t8(_tA,_tB,function(_){var _tS=_sC(_tr,_),_tT=A(_tv,[_]),_tU=E(_tT);if(!_tU[0]){return _0;}else{var _tV=A(_tw,[_tU[1],_]);return _0;}});case 5:return _t8(_tA,_tB,function(_tW,_){var _tX=_sC([0,_tq,[2,E(_tW)]],_),_tY=A(_tv,[_]),_tZ=E(_tY);if(!_tZ[0]){return _0;}else{var _u0=A(_tw,[_tZ[1],_]);return _0;}});case 6:return _t8(_tA,_tB,function(_u1,_){var _u2=_sC([0,_tq,[2,E(_u1)]],_),_u3=A(_tv,[_]),_u4=E(_u3);if(!_u4[0]){return _0;}else{var _u5=A(_tw,[_u4[1],_]);return _0;}});case 7:return _t8(_tA,_tB,function(_){var _u6=A(_tv,[_]),_u7=E(_u6);if(!_u7[0]){return _0;}else{var _u8=A(_tw,[_u7[1],_]);return _0;}});case 8:return _t8(_tA,_tB,function(_u9,_ua,_){var _ub=_sC([0,_tq,[1,_u9,E(_ua)]],_),_uc=A(_tv,[_]),_ud=E(_uc);if(!_ud[0]){return _0;}else{var _ue=A(_tw,[_ud[1],_]);return _0;}});case 9:return _t8(_tA,_tB,function(_uf,_ug,_){var _uh=_sC([0,_tq,[1,_uf,E(_ug)]],_),_ui=A(_tv,[_]),_uj=E(_ui);if(!_uj[0]){return _0;}else{var _uk=A(_tw,[_uj[1],_]);return _0;}});case 10:return _t8(_tA,_tB,function(_ul,_um,_){var _un=_sC([0,_tq,[1,_ul,E(_um)]],_),_uo=A(_tv,[_]),_up=E(_uo);if(!_up[0]){return _0;}else{var _uq=A(_tw,[_up[1],_]);return _0;}});case 11:return _t8(_tA,_tB,function(_ur,_us,_){var _ut=_sC([0,_tq,[1,_ur,E(_us)]],_),_uu=A(_tv,[_]),_uv=E(_uu);if(!_uv[0]){return _0;}else{var _uw=A(_tw,[_uv[1],_]);return _0;}});case 12:return _t8(_tA,_tB,function(_ux,_){var _uy=_sC([0,_tq,[3,_ux]],_),_uz=A(_tv,[_]),_uA=E(_uz);if(!_uA[0]){return _0;}else{var _uB=A(_tw,[_uA[1],_]);return _0;}});case 13:return _t8(_tA,_tB,function(_uC,_){var _uD=_sC([0,_tq,[3,_uC]],_),_uE=A(_tv,[_]),_uF=E(_uE);if(!_uF[0]){return _0;}else{var _uG=A(_tw,[_uF[1],_]);return _0;}});default:return _t8(_tA,_tB,function(_uH,_){var _uI=_sC([0,_tq,[3,_uH]],_),_uJ=A(_tv,[_]),_uK=E(_uJ);if(!_uK[0]){return _0;}else{var _uL=A(_tw,[_uK[1],_]);return _0;}});}}),_tz[2]],_ty[2]];};},_uM=new T(function(){return _tn(_s6,_9H);}),_uN=function(_uO,_){var _uP=A(_uM,[_uO,_]),_uQ=E(_uP),_uR=E(_uQ[1]);return [0,[0,function(_uS,_){var _uT=A(_uR[1],[_uS,_]),_uU=_5k(_uS,_);return _uS;},_uR[2]],_uQ[2]];},_uV=new T(function(){return [1,_uN,_uV];}),_uW=function(_uX,_uY){var _uZ=E(_uX);if(!_uZ){return [0];}else{var _v0=E(_uY);return _v0[0]==0?[0]:[1,_v0[1],new T(function(){return _uW(_uZ-1|0,_v0[2]);})];}},_v1=function(_v2,_v3){return _v2<0?[0]:_uW(_v2,_v3);},_v4=function(_v5,_v6){var _v7=E(_v5)[1];return _v7>0?_v1(_v7,_v6):[0];},_v8=function(_v9){return E(_v9);},_va=function(_vb){var _vc=new T(function(){return _9A(_5C,_v4(_vb,_uV));}),_vd=new T(function(){return _58(_p,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1N(_3M(0,E(_vb)[1],_9),_5y);}));}));});return function(_ve,_){var _vf=_4B(_vc,_5q,_ve,_),_vg=E(_vf),_vh=E(_vg[1]),_vi=new T(function(){return _58(_v8,_vh[1]);});return [0,[0,function(_vj,_){var _vk=A(_vd,[_vj,_]),_vl=A(_vi,[_vj,_]);return _vj;},_vh[2]],_vg[2]];};},_vm=new T(function(){return _va(_55);}),_vn=unCStr(" A counter. wcallback erases the previous rendering of the widget an regenerates it again "),_vo=new T(function(){return _58(_p,_vn);}),_vp=[8,coercionToken],_vq=function(_vr){return _aM(_mI(function(_vs){var _vt=E(_vs);return _vt[0]==0?A(_vr,[_vt[1]]):[2];}),new T(function(){return _mO(_vu,_vr);}));},_vu=function(_vv,_vw){return _vq(_vw);},_vx=function(_vy){return _aM(_aM(_mI(function(_vz){var _vA=E(_vz);return _vA[0]==1?A(_vy,[_vA[1]]):[2];}),new T(function(){return _nw(_vu,_vy);})),new T(function(){return _mO(_vB,_vy);}));},_vB=function(_vC,_vD){return _vx(_vD);},_vE=new T(function(){return _mO(_vB,_bN);}),_vF=new T(function(){return _nw(_vu,_bN);}),_vG=function(_vH){var _vI=E(_vH);return _vI[0]==1?[3,_vI[1],_bM]:[2];},_vJ=new T(function(){return _mf(_vG);}),_vK=function(_vL){return E(_vJ);},_vM=function(_vN){return A(_kw,[_vN,_vK]);},_vO=[1,_vM],_vP=new T(function(){return _aM(_vO,_vF);}),_vQ=new T(function(){return _aM(_vP,_vE);}),_vR=function(_nk){return _aC(_vQ,_nk);},_vS=new T(function(){return _vq(_bN);}),_vT=function(_nk){return _aC(_vS,_nk);},_vU=function(_vV){return E(_vT);},_vW=[0,_vU,_vR,_vu,_vB],_vX=function(_vY){return E(E(_vY)[4]);},_vZ=function(_w0,_w1,_w2){return _nw(new T(function(){return _vX(_w0);}),_w2);},_w3=function(_w4){var _w5=new T(function(){return _nw(new T(function(){return _vX(_w4);}),_bN);});return function(_cg){return _aC(_w5,_cg);};},_w6=function(_w7,_w8){var _w9=new T(function(){return A(_vX,[_w7,_w8,_bN]);});return function(_cg){return _aC(_w9,_cg);};},_wa=function(_wb){return [0,function(_nk){return _w6(_wb,_nk);},new T(function(){return _w3(_wb);}),new T(function(){return _vX(_wb);}),function(_nj,_nk){return _vZ(_wb,_nj,_nk);}];},_wc=new T(function(){return _wa(_vW);}),_wd=unCStr("Prelude.(!!): negative index\n"),_we=new T(function(){return err(_wd);}),_wf=unCStr("Prelude.(!!): index too large\n"),_wg=new T(function(){return err(_wf);}),_wh=function(_wi,_wj){while(1){var _wk=E(_wi);if(!_wk[0]){return E(_wg);}else{var _wl=E(_wj);if(!_wl){return E(_wk[1]);}else{_wi=_wk[2];_wj=_wl-1|0;continue;}}}},_wm=unCStr("ACK"),_wn=unCStr("BEL"),_wo=unCStr("BS"),_wp=unCStr("SP"),_wq=[1,_wp,_9],_wr=unCStr("US"),_ws=[1,_wr,_wq],_wt=unCStr("RS"),_wu=[1,_wt,_ws],_wv=unCStr("GS"),_ww=[1,_wv,_wu],_wx=unCStr("FS"),_wy=[1,_wx,_ww],_wz=unCStr("ESC"),_wA=[1,_wz,_wy],_wB=unCStr("SUB"),_wC=[1,_wB,_wA],_wD=unCStr("EM"),_wE=[1,_wD,_wC],_wF=unCStr("CAN"),_wG=[1,_wF,_wE],_wH=unCStr("ETB"),_wI=[1,_wH,_wG],_wJ=unCStr("SYN"),_wK=[1,_wJ,_wI],_wL=unCStr("NAK"),_wM=[1,_wL,_wK],_wN=unCStr("DC4"),_wO=[1,_wN,_wM],_wP=unCStr("DC3"),_wQ=[1,_wP,_wO],_wR=unCStr("DC2"),_wS=[1,_wR,_wQ],_wT=unCStr("DC1"),_wU=[1,_wT,_wS],_wV=unCStr("DLE"),_wW=[1,_wV,_wU],_wX=unCStr("SI"),_wY=[1,_wX,_wW],_wZ=unCStr("SO"),_x0=[1,_wZ,_wY],_x1=unCStr("CR"),_x2=[1,_x1,_x0],_x3=unCStr("FF"),_x4=[1,_x3,_x2],_x5=unCStr("VT"),_x6=[1,_x5,_x4],_x7=unCStr("LF"),_x8=[1,_x7,_x6],_x9=unCStr("HT"),_xa=[1,_x9,_x8],_xb=[1,_wo,_xa],_xc=[1,_wn,_xb],_xd=[1,_wm,_xc],_xe=unCStr("ENQ"),_xf=[1,_xe,_xd],_xg=unCStr("EOT"),_xh=[1,_xg,_xf],_xi=unCStr("ETX"),_xj=[1,_xi,_xh],_xk=unCStr("STX"),_xl=[1,_xk,_xj],_xm=unCStr("SOH"),_xn=[1,_xm,_xl],_xo=unCStr("NUL"),_xp=[1,_xo,_xn],_xq=[0,92],_xr=unCStr("\\DEL"),_xs=unCStr("\\a"),_xt=unCStr("\\\\"),_xu=unCStr("\\SO"),_xv=unCStr("\\r"),_xw=unCStr("\\f"),_xx=unCStr("\\v"),_xy=unCStr("\\n"),_xz=unCStr("\\t"),_xA=unCStr("\\b"),_xB=function(_xC,_xD){if(_xC<=127){var _xE=E(_xC);switch(_xE){case 92:return _1N(_xt,_xD);case 127:return _1N(_xr,_xD);default:if(_xE<32){var _xF=E(_xE);switch(_xF){case 7:return _1N(_xs,_xD);case 8:return _1N(_xA,_xD);case 9:return _1N(_xz,_xD);case 10:return _1N(_xy,_xD);case 11:return _1N(_xx,_xD);case 12:return _1N(_xw,_xD);case 13:return _1N(_xv,_xD);case 14:return _1N(_xu,new T(function(){var _xG=E(_xD);return _xG[0]==0?[0]:E(E(_xG[1])[1])==72?unAppCStr("\\&",_xG):E(_xG);}));default:return _1N([1,_xq,new T(function(){var _xH=_xF;return _xH>=0?_wh(_xp,_xH):E(_we);})],_xD);}}else{return [1,[0,_xE],_xD];}}}else{return [1,_xq,new T(function(){var _xI=jsShowI(_xC);return _1N(fromJSStr(_xI),new T(function(){var _xJ=E(_xD);if(!_xJ[0]){return [0];}else{var _xK=E(_xJ[1])[1];return _xK<48?E(_xJ):_xK>57?E(_xJ):unAppCStr("\\&",_xJ);}}));})];}},_xL=[0,39],_xM=[1,_xL,_9],_xN=unCStr("\'\\\'\'"),_xO=function(_xP){var _xQ=E(E(_xP)[1]);return _xQ==39?E(_xN):[1,_xL,new T(function(){return _xB(_xQ,_xM);})];},_xR=[0,34],_xS=unCStr("\\\""),_xT=function(_xU,_xV){var _xW=E(_xU);if(!_xW[0]){return E(_xV);}else{var _xX=_xW[2],_xY=E(E(_xW[1])[1]);return _xY==34?_1N(_xS,new T(function(){return _xT(_xX,_xV);})):_xB(_xY,new T(function(){return _xT(_xX,_xV);}));}},_xZ=function(_y0,_y1){return [1,_xR,new T(function(){return _xT(_y0,[1,_xR,_y1]);})];},_y2=function(_y3){return _1N(_xN,_y3);},_y4=function(_y5,_y6){var _y7=E(E(_y6)[1]);return _y7==39?E(_y2):function(_y8){return [1,_xL,new T(function(){return _xB(_y7,[1,_xL,_y8]);})];};},_y9=[0,_y4,_xO,_xZ],_ya=function(_yb){return E(E(_yb)[3]);},_yc=function(_yd,_ye){return A(_ya,[_yd,_ye,_9]);},_yf=function(_yg,_yh,_yi){return _2D(new T(function(){return _ya(_yg);}),_yh,_yi);},_yj=function(_yk){var _yl=new T(function(){return _ya(_yk);});return [0,function(_ym){return E(_yl);},function(_y3){return _yc(_yk,_y3);},function(_yn,_y3){return _yf(_yk,_yn,_y3);}];},_yo=new T(function(){return _yj(_y9);}),_yp=unCStr("submit"),_yq=new T(function(){return A(_rT,[_8D,_9J,_p3,_yo,_wc,_a,_yp]);}),_yr=[0,43],_ys=[1,_yr,_9],_yt=[1,_ys],_yu=new T(function(){return A(_yq,[_yt]);}),_yv=new T(function(){return _tn(_yu,_vp);}),_yw=function(_X,_){return _Z(_Y,_X,_);},_yx=function(_yy,_yz,_yA,_){var _yB=A(_yz,[_yA,_]),_yC=E(_yB),_yD=E(_yC[1]);return [0,[0,function(_yE,_){var _yF=_Z(_Y,_yE,_),_yG=A(_1,[_n,_yF,_I,_yy,_]),_yH=A(_yD[1],[_yF,_]);return _yF;},_yD[2]],_yC[2]];},_yI=new T(function(){return _3R(_1a,_3E,_18,_15);}),_yJ=new T(function(){return _3R(_1a,_3E,_18,_15);}),_yK=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_yL=function(_yM){return _j(function(_){var _=0;return eval(E(_yM)[1]);});},_yN=new T(function(){return _yL(_yK);}),_yO=function(_yP,_yQ,_yR,_){var _yS=A(_yI,[_yR,_]),_yT=A(_yJ,[new T(function(){return E(E(_yS)[2]);}),_]),_yU=E(_yT),_yV=_yU[1],_yW=E(_yU[2]),_yX=_yW[2],_yY=E(_yW[4]),_yZ=new T(function(){return E(E(_yS)[1]);}),_z0=function(_z1){var _z2=new T(function(){return A(_yQ,[_z1]);});return function(_z3,_){var _z4=A(_z2,[_z3,_]),_z5=E(_z4),_z6=E(_z5[1]);return [0,[0,function(_z7,_){var _z8=A(_z6[1],[_z7,_]),_z9=E(_yZ),_za=jsFind(toJSStr(_z9)),_zb=E(_za);if(!_zb[0]){return _45(_z9);}else{var _zc=E(_zb[1]),_zd=A(_yN,[E(_zc[1]),_]),_ze=jsKillChild(E(_zc)[1],_zd);return _z7;}},_z6[2]],_z5[2]];};},_zf=_yx(_yZ,_yP,[0,_yW[1],_yX,_yW[3],[0,function(_){return _47(function(_zg,_){var _zh=_yx(_yZ,_yP,new T(function(){var _zi=E(_zg);return [0,_zi[1],_yX,_zi[3],_zi[4],_zi[5]];}),_);return [0,[0,_34,E(E(_zh)[1])[2]],_zg];},_yV,_);},function(_zj,_){var _zk=_47(new T(function(){return _z0(_zj);}),_yV,_),_zl=E(_zk);return _zl[0]==0?_a:A(_yY[2],[_zl[1],_]);}],_yW[5]],_),_zm=E(_zf),_zn=_zm[2],_zo=E(_zm[1]),_zp=_zo[1],_zq=new T(function(){return _Q(_yw,[1,[0,_I,_yV],_9]);}),_zr=E(_zo[2]);if(!_zr[0]){return [0,[0,function(_zs,_){var _zt=A(_zp,[_zs,_]),_zu=A(_zq,[_zs,_]);return _zs;},_a],new T(function(){var _zv=E(_zn);return [0,_zv[1],_zv[2],_zv[3],_yY,_zv[5]];})];}else{var _zw=A(_z0,[_zr[1],new T(function(){var _zx=E(_zn);return [0,_zx[1],_zx[2],_zx[3],_yY,_zx[5]];}),_]),_zy=E(_zw),_zz=E(_zy[1]);return [0,[0,function(_zA,_){var _zB=A(_zp,[_zA,_]),_zC=A(_zq,[_zA,_]),_zD=A(_zz[1],[_zC,_]);return _zA;},_zz[2]],_zy[2]];}},_zE=function(_zF){var _zG=new T(function(){return _zE(new T(function(){return [0,E(_zF)[1]+1|0];}));}),_zH=new T(function(){return _w(_p,new T(function(){return _5h(_zF);}));});return function(_cg,_s3){return _yO(function(_zI,_){var _zJ=A(_yv,[_zI,_]),_zK=E(_zJ),_zL=E(_zK[1]);return [0,[0,function(_zM,_){var _zN=A(_zH,[_zM,_]),_zO=A(_zL[1],[_zM,_]);return _zM;},_zL[2]],_zK[2]];},function(_zP){return E(_zG);},_cg,_s3);};},_zQ=function(_zR){var _zS=new T(function(){return _zE(_zR);});return function(_zT,_){var _zU=A(_zS,[_zT,_]),_zV=E(_zU),_zW=E(_zV[1]);return [0,[0,function(_zX,_){var _zY=A(_vo,[_zX,_]),_zZ=_5k(_zX,_),_A0=A(_zW[1],[_zX,_]);return _zX;},_zW[2]],_zV[2]];};},_A1=new T(function(){return _zQ(_55);}),_A2=[0,4],_A3=function(_A4,_A5){return [1,_A5,new T(function(){return _A3(_A4,new T(function(){return A(_A4,[_A5]);}));})];},_A6=[0,1],_A7=[1,_A6,_9],_A8=[1,_5z,_9],_A9=function(_Aa,_Ab,_Ac){var _Ad=E(_Ab);if(!_Ad[0]){return [0];}else{var _Ae=E(_Ac);return _Ae[0]==0?[0]:[1,new T(function(){return A(_Aa,[_Ad[1],_Ae[1]]);}),new T(function(){return _A9(_Aa,_Ad[2],_Ae[2]);})];}},_Af=function(_Ag){return _A9(_8S,[1,_5z,_Ag],new T(function(){return _1N(_Ag,_A8);}));},_Ah=new T(function(){return _A3(_Af,_A7);}),_Ai=unCStr(" rows of the Pascal triangle "),_Aj=function(_Ak){var _Al=new T(function(){return _2D(_o3,_Ak,_9);});return function(_cg,_s3){return _p(_Al,_cg,_s3);};},_Am=function(_An,_Ao){var _Ap=new T(function(){return _58(_Aj,_An);});return [1,function(_Aq,_){var _Ar=A(_Ap,[_Aq,_]),_As=A(_1,[_n,_Ar,_53,_H,_]);return _Ar;},_Ao];},_At=function(_Au,_Av){var _Aw=E(_Au);if(!_Aw[0]){return [0];}else{var _Ax=_Aw[1];return _Av>1?_Am(_Ax,new T(function(){return _At(_Aw[2],_Av-1|0);})):_Am(_Ax,_9);}},_Ay=function(_Az){var _AA=new T(function(){return _58(_p,new T(function(){return unAppCStr("Show ",new T(function(){return _1N(_3M(0,E(_Az)[1],_9),_Ai);}));}));});return function(_AB,_){return [0,[0,function(_AC,_){var _AD=A(_AA,[_AC,_]),_AE=_8q(new T(function(){var _AF=E(_Az)[1];return _AF>0?_At(_Ah,_AF):[0];}),_AC,_);return _AC;},_a],_AB];};},_AG=new T(function(){return _Ay(_A2);}),_AH=[1,_0],_AI=function(_AJ){var _AK=new T(function(){return _58(_p,new T(function(){return _2D(_xZ,_AJ,_9);}));});return function(_AL,_){return [0,[0,_AK,_AH],_AL];};},_AM=function(_AN){return _AI(E(_AN)[1]);},_AO=unCStr("green"),_AP=new T(function(){return _w(_p,_AO);}),_AQ=unCStr("Green"),_AR=function(_AS,_AT,_){var _AU=jsGet(_AS,toJSStr(E(_AT)));return new T(function(){return fromJSStr(_AU);});},_AV=function(_AW,_AX,_){return _AR(E(_AW)[1],_AX,_);},_AY=unCStr("true"),_AZ=unCStr("checkbox"),_B0=function(_B1,_B2){var _B3=new T(function(){return _ok(_B2);}),_B4=new T(function(){return _3R([0,coercionToken],_3z(_B3),function(_B5){return _oB(_B3,_B5);},function(_B6,_B7){return _oE(_B3,_B6,_B7);});}),_B8=new T(function(){return _3x(_B3);}),_B9=new T(function(){return _3x(_B3);}),_Ba=new T(function(){return _37(_B3);}),_Bb=new T(function(){return _37(_B3);}),_Bc=new T(function(){return _3x(_B3);}),_Bd=new T(function(){return _37(_B3);}),_Be=new T(function(){return _3x(_B3);}),_Bf=new T(function(){return _37(_B3);}),_Bg=new T(function(){return _r9(_B1);});return function(_Bh,_Bi){return function(_Bj){return A(_Ba,[new T(function(){return A(_B4,[_Bj]);}),function(_Bk){var _Bl=new T(function(){return E(E(_Bk)[1]);}),_Bm=new T(function(){return _oo(_B2,function(_){return jsFind(toJSStr(E(_Bl)));});});return A(_Bf,[new T(function(){var _Bn=new T(function(){return E(E(_Bk)[2]);});return A(_Be,[[0,_Bn,_Bn]]);}),function(_Bo){return A(_Bd,[new T(function(){return A(_Bc,[[0,_0,new T(function(){var _Bp=E(E(_Bo)[1]);return [0,_Bp[1],_Bp[2],_r8,_Bp[4],_Bp[5]];})]]);}),function(_Bq){return A(_Bb,[new T(function(){return A(_Bm,[new T(function(){return E(E(_Bq)[2]);})]);}),function(_Br){return A(_Ba,[new T(function(){var _Bs=E(_Br),_Bt=_Bs[2],_Bu=E(_Bs[1]);return _Bu[0]==0?A(_B9,[[0,_9,_Bt]]):A(_oo,[_B2,function(_){return _AV(_Bu[1],_6T,_);},_Bt]);}),function(_Bv){var _Bw=new T(function(){return !_sM(E(_Bv)[1],_AY)?[0]:E([1,_Bi,_9]);});return A(_B8,[[0,[0,new T(function(){return A(_Bg,[_Bl,_AZ,_Bi,new T(function(){return E(_Bw)[0]==0?false:true;}),_a]);}),[1,[0,_Bw]]],new T(function(){return E(E(_Bv)[2]);})]]);}]);}]);}]);}]);}]);};};},_Bx=new T(function(){return _B0(_8D,_9J);}),_By=new T(function(){return A(_Bx,[_ea,_AQ]);}),_Bz=function(_BA,_){var _BB=A(_By,[_BA,_]),_BC=E(_BB),_BD=E(_BC[1]);return [0,[0,function(_BE,_){var _BF=A(_BD[1],[_BE,_]),_BG=A(_AP,[_BE,_]);return _BE;},_BD[2]],_BC[2]];},_BH=new T(function(){return _tn(_Bz,_vp);}),_BI=unCStr("blue"),_BJ=new T(function(){return _w(_p,_BI);}),_BK=new T(function(){return A(_Bx,[_ea,_BI]);}),_BL=function(_BM,_){var _BN=A(_BK,[_BM,_]),_BO=E(_BN),_BP=E(_BO[1]);return [0,[0,function(_BQ,_){var _BR=A(_BP[1],[_BQ,_]),_BS=A(_BJ,[_BQ,_]);return _BQ;},_BP[2]],_BO[2]];},_BT=new T(function(){return _tn(_BL,_vp);}),_BU=unCStr("red"),_BV=new T(function(){return _w(_p,_BU);}),_BW=unCStr("Red"),_BX=new T(function(){return A(_Bx,[_ea,_BW]);}),_BY=function(_BZ,_){var _C0=A(_BX,[_BZ,_]),_C1=E(_C0),_C2=E(_C1[1]);return [0,[0,function(_C3,_){var _C4=A(_C2[1],[_C3,_]),_C5=A(_BV,[_C3,_]);return _C3;},_C2[2]],_C1[2]];},_C6=new T(function(){return _tn(_BY,_vp);}),_C7=function(_C8,_){var _C9=A(_C6,[_C8,_]),_Ca=E(_C9),_Cb=E(_Ca[1]),_Cc=A(_BH,[_Ca[2],_]),_Cd=E(_Cc),_Ce=E(_Cd[1]),_Cf=A(_BT,[_Cd[2],_]),_Cg=E(_Cf),_Ch=E(_Cg[1]);return [0,[0,function(_Ci,_){var _Cj=A(_Cb[1],[_Ci,_]),_Ck=A(_Ce[1],[_Ci,_]),_Cl=A(_Ch[1],[_Ci,_]);return _Ci;},new T(function(){var _Cm=E(_Cb[2]);if(!_Cm[0]){return [0];}else{var _Cn=E(_Ce[2]);if(!_Cn[0]){return [0];}else{var _Co=E(_Ch[2]);return _Co[0]==0?[0]:[1,new T(function(){return [0,new T(function(){var _Cp=function(_Cq){var _Cr=E(_Cq);return _Cr[0]==0?E(new T(function(){var _Cs=function(_Ct){var _Cu=E(_Ct);return _Cu[0]==0?E(E(_Co[1])[1]):[1,_Cu[1],new T(function(){return _Cs(_Cu[2]);})];};return _Cs(E(_Cn[1])[1]);})):[1,_Cr[1],new T(function(){return _Cp(_Cr[2]);})];};return _Cp(E(_Cm[1])[1]);})];})];}}})],_Cg[2]];},_Cv=unCStr("center"),_Cw=function(_Cx,_Cy){var _Cz=new T(function(){return A(_Cx,[_Cy]);});return function(_CA,_){var _CB=jsCreateElem(toJSStr(E(_Cv))),_CC=jsAppendChild(_CB,E(_CA)[1]),_CD=[0,_CB],_CE=A(_Cz,[_CD,_]);return _CD;};},_CF=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_CG=new T(function(){return _58(_p,_CF);}),_CH=function(_CI){var _CJ=jsShow(E(_CI)[1]);return fromJSStr(_CJ);},_CK=function(_CL){var _CM=new T(function(){return _CH(_CL);});return function(_cg){return _1N(_CM,_cg);};},_CN=function(_CO,_CP,_CQ){var _CR=E(_CQ);if(!_CR[0]){return [0];}else{var _CS=_CR[2],_CT=E(_CR[1]);return _CO!=_CT[1]?[1,_CT,new T(function(){return _CN(_CO,_CP,_CS);})]:_1N(_CP,new T(function(){return _CN(_CO,_CP,_CS);}));}},_CU=[0,45],_CV=function(_CW,_CX,_CY){var _CZ=new T(function(){return A(_CW,[[0, -_CY]]);}),_D0=new T(function(){return E(_CX)[1]<=6?function(_D1){return [1,_CU,new T(function(){return A(_CZ,[_D1]);})];}:function(_D2){return [1,_3L,[1,_CU,new T(function(){return A(_CZ,[[1,_3K,_D2]]);})]];};});if(_CY>=0){var _D3=isDoubleNegativeZero(_CY);return E(_D3)==0?A(_CW,[[0,_CY]]):E(_D0);}else{return E(_D0);}},_D4=unCStr("canvas"),_D5=unCStr("id"),_D6=unCStr("canvas"),_D7=function(_D8,_D9){var _Da=new T(function(){return A(_D8,[_D9]);});return function(_Db,_){var _Dc=jsCreateElem(toJSStr(E(_D6))),_Dd=jsAppendChild(_Dc,E(_Db)[1]),_De=[0,_Dc],_Df=A(_Da,[_De,_]);return _De;};},_Dg=new T(function(){return _D7(_v8,_34);}),_Dh=function(_Di,_){var _Dj=A(_Dg,[_Di,_]),_Dk=A(_1,[_n,_Dj,_D5,_D4,_]);return _Dj;},_Dl=[0,_Dh,_AH],_Dm=function(_Dn,_){return [0,_Dl,_Dn];},_Do=unCStr("Pattern match failure in do expression at main.hs:168:5-12"),_Dp=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_Dq=new T(function(){return _yL(_Dp);}),_Dr=function(_Ds,_){var _Dt=jsHasCtx2D(_Ds);if(!E(_Dt)){return _a;}else{var _Du=jsGetCtx2D(_Ds);return [1,[0,[0,_Du],[0,_Ds]]];}},_Dv=function(_Dw,_){return _Dr(E(_Dw)[1],_);},_Dx=function(_Dy,_Dz){return A(_Dy,[function(_){var _DA=jsFind(toJSStr(E(_Dz))),_DB=E(_DA);return _DB[0]==0?_a:_Dv(_DB[1],_);}]);},_DC=new T(function(){return _Dx(_n,_D4);}),_DD=[0,-10],_DE=[0,0],_DF=[0,_DD,_DE],_DG=[0,10],_DH=[0,_DG,_DE],_DI=[1,_DH,_9],_DJ=[1,_DF,_DI],_DK=function(_DL,_){return _0;},_DM=function(_DN){var _DO=E(_DN);if(!_DO[0]){return E(_DK);}else{var _DP=E(_DO[1]);return function(_DQ,_){var _DR=E(_DQ)[1],_DS=jsMoveTo(_DR,E(_DP[1])[1],E(_DP[2])[1]);return (function(_DT,_){while(1){var _DU=E(_DT);if(!_DU[0]){return _0;}else{var _DV=E(_DU[1]),_DW=jsLineTo(_DR,E(_DV[1])[1],E(_DV[2])[1]);_DT=_DU[2];continue;}}})(_DO[2],_);};}},_DX=new T(function(){return _DM(_DJ);}),_DY=[0,30],_DZ=[0,_DE,_DY],_E0=[0,-30],_E1=[0,_DE,_E0],_E2=[1,_E1,_9],_E3=[1,_DZ,_E2],_E4=new T(function(){return _DM(_E3);}),_E5=new T(function(){return [0,0/0];}),_E6=new T(function(){return [0,-1/0];}),_E7=new T(function(){return [0,1/0];}),_E8=[0,0],_E9=function(_Ea,_Eb){while(1){var _Ec=E(_Ea);if(!_Ec[0]){_Ea=[1,I_fromInt(_Ec[1])];continue;}else{var _Ed=E(_Eb);if(!_Ed[0]){_Ea=_Ec;_Eb=[1,I_fromInt(_Ed[1])];continue;}else{return I_fromRat(_Ec[1],_Ed[1]);}}}},_Ee=function(_Ef,_Eg){var _Eh=E(_Ef);if(!_Eh[0]){var _Ei=_Eh[1],_Ej=E(_Eg);return _Ej[0]==0?_Ei==_Ej[1]:I_compareInt(_Ej[1],_Ei)==0?true:false;}else{var _Ek=_Eh[1],_El=E(_Eg);return _El[0]==0?I_compareInt(_Ek,_El[1])==0?true:false:I_compare(_Ek,_El[1])==0?true:false;}},_Em=function(_En,_Eo){var _Ep=E(_En);if(!_Ep[0]){var _Eq=_Ep[1],_Er=E(_Eo);return _Er[0]==0?_Eq<_Er[1]:I_compareInt(_Er[1],_Eq)>0;}else{var _Es=_Ep[1],_Et=E(_Eo);return _Et[0]==0?I_compareInt(_Es,_Et[1])<0:I_compare(_Es,_Et[1])<0;}},_Eu=function(_Ev,_Ew){return !_Ee(_Ew,_E8)?[0,_E9(_Ev,_Ew)]:!_Ee(_Ev,_E8)?!_Em(_Ev,_E8)?E(_E7):E(_E6):E(_E5);},_Ex=function(_Ey){var _Ez=E(_Ey);return _Eu(_Ez[1],_Ez[2]);},_EA=function(_EB){return [0,1/E(_EB)[1]];},_EC=function(_ED){var _EE=E(_ED),_EF=_EE[1];return _EF<0?[0, -_EF]:E(_EE);},_EG=function(_EH){var _EI=E(_EH);return _EI[0]==0?_EI[1]:I_toNumber(_EI[1]);},_EJ=function(_EK){return [0,_EG(_EK)];},_EL=[0,0],_EM=[0,1],_EN=[0,-1],_EO=function(_EP){var _EQ=E(_EP)[1];return _EQ!=0?_EQ<=0?E(_EN):E(_EM):E(_EL);},_ER=function(_ES,_ET){return [0,E(_ES)[1]-E(_ET)[1]];},_EU=function(_EV){return [0, -E(_EV)[1]];},_EW=function(_EX,_EY){return [0,E(_EX)[1]+E(_EY)[1]];},_EZ=function(_F0,_F1){return [0,E(_F0)[1]*E(_F1)[1]];},_F2=[0,_EW,_EZ,_ER,_EU,_EC,_EO,_EJ],_F3=function(_F4,_F5){return [0,E(_F4)[1]/E(_F5)[1]];},_F6=[0,_F2,_F3,_EA,_Ex],_F7=function(_F8,_F9){return E(_F8)[1]!=E(_F9)[1]?true:false;},_Fa=function(_Fb,_Fc){return E(_Fb)[1]==E(_Fc)[1];},_Fd=[0,_Fa,_F7],_Fe=function(_Ff,_Fg){return E(_Ff)[1]<E(_Fg)[1];},_Fh=function(_Fi,_Fj){return E(_Fi)[1]<=E(_Fj)[1];},_Fk=function(_Fl,_Fm){return E(_Fl)[1]>E(_Fm)[1];},_Fn=function(_Fo,_Fp){return E(_Fo)[1]>=E(_Fp)[1];},_Fq=function(_Fr,_Fs){var _Ft=E(_Fr)[1],_Fu=E(_Fs)[1];return _Ft>=_Fu?_Ft!=_Fu?2:1:0;},_Fv=function(_Fw,_Fx){var _Fy=E(_Fw),_Fz=E(_Fx);return _Fy[1]>_Fz[1]?E(_Fy):E(_Fz);},_FA=function(_FB,_FC){var _FD=E(_FB),_FE=E(_FC);return _FD[1]>_FE[1]?E(_FE):E(_FD);},_FF=[0,_Fd,_Fq,_Fe,_Fn,_Fk,_Fh,_Fv,_FA],_FG=[0,1],_FH=function(_FI){return E(E(_FI)[1]);},_FJ=function(_FK){return E(E(_FK)[2]);},_FL=function(_FM){return E(E(_FM)[6]);},_FN=[0,2],_FO=function(_FP,_FQ){var _FR=E(_FQ);return [1,_FR,new T(function(){var _FS=_FH(_FP);return _FO(_FP,A(_FS[1],[_FR,new T(function(){return A(_FS[7],[_FG]);})]));})];},_FT=function(_FU,_FV){var _FW=E(_FV);if(!_FW[0]){return [0];}else{var _FX=_FW[1];return !A(_FU,[_FX])?[0]:[1,_FX,new T(function(){return _FT(_FU,_FW[2]);})];}},_FY=function(_FZ,_G0,_G1,_G2){var _G3=new T(function(){return _FL(_FZ);});return _FT(function(_G4){return A(_G3,[_G4,new T(function(){var _G5=_FH(_G0),_G6=_G5[7];return A(_G5[1],[_G2,new T(function(){return A(_FJ,[_G0,new T(function(){return A(_G6,[_FG]);}),new T(function(){return A(_G6,[_FN]);})]);})]);})]);},_FO(_G0,_G1));},_G7=new T(function(){return _FY(_FF,_F6,_DD,_DG);}),_G8=function(_G9,_Ga){var _Gb=E(_G9);if(!_Gb[0]){return [0];}else{var _Gc=E(_Ga);return _Gc[0]==0?[0]:[1,[0,_Gb[1],_Gc[1]],new T(function(){return _G8(_Gb[2],_Gc[2]);})];}},_Gd=function(_Ge,_Gf,_){var _Gg=function(_Gh,_){var _Gi=E(_Gh);if(!_Gi[0]){return _9;}else{var _Gj=A(_Dq,[E(toJSStr(_CN(120,new T(function(){return A(_CV,[_CK,_pa,E(_Gi[1])[1],_9]);}),_Ge))),_]),_Gk=_Gg(_Gi[2],_);return [1,[0,_Gj],_Gk];}};return _4B(_Dm,function(_Gl,_Gm,_){return (function(_Gn,_){return [0,[0,function(_Go,_){var _Gp=A(_DC,[_]),_Gq=E(_Gp);if(!_Gq[0]){var _Gr=_32(_Do,_);return _Go;}else{var _Gs=_Gg(_G7,_),_Gt=E(_Gq[1]),_Gu=jsResetCanvas(E(_Gt[2])[1]),_Gv=E(_Gt[1]),_Gw=_Gv[1],_Gx=jsPushState(_Gw),_Gy=jsScale(_Gw,3,1),_Gz=jsPushState(_Gw),_GA=jsTranslate(_Gw,50,130),_GB=jsPushState(_Gw),_GC=jsRotate(_Gw,3.141592653589793),_GD=jsBeginPath(_Gw),_GE=A(_DX,[_Gv,_]),_GF=A(_E4,[_Gv,_]),_GG=A(_DM,[_G8(_G7,_Gs),_Gv,_]),_GH=jsStroke(_Gw),_GI=jsPopState(_Gw),_GJ=jsPopState(_Gw),_GK=jsPopState(_Gw);return _Go;}},_AH],_Gn];})(_Gm,_);},_Gf,_);},_GL=unCStr("Math.pow(x,2)+x+10;"),_GM=[1,_GL],_GN=function(_GO,_GP,_){return [0,[0,_34,[1,_GO]],_GP];},_GQ=function(_GR,_GS,_GT,_){return _4B(_GR,function(_GU){return E(_GS);},_GT,_);},_GV=function(_GW,_GX,_X,_){return _GQ(_GW,_GX,_X,_);},_GY=function(_GZ){return err(_GZ);},_H0=[0,_4B,_GV,_GN,_GY],_H1=function(_H2){return E(E(_H2)[1]);},_H3=function(_H4,_H5,_H6,_H7,_H8){var _H9=new T(function(){return _90(_H4);}),_Ha=new T(function(){return _H1(_H9);}),_Hb=new T(function(){return _3x(_H5);}),_Hc=new T(function(){return _p6(_H4);}),_Hd=new T(function(){return _37(_H5);}),_He=new T(function(){return _3x(_H5);}),_Hf=new T(function(){return _37(_H5);});return A(_H6,[function(_Hg){return A(_Hf,[new T(function(){return A(_H7,[_Hg]);}),function(_Hh){var _Hi=E(_Hh),_Hj=E(_Hi[1]);return A(_He,[[0,[0,_Hj[1],[1,_Hj[2]]],_Hi[2]]]);}]);},function(_Hk){var _Hl=E(_Hk);if(!_Hl[0]){return function(_Hm){return A(_Hb,[[0,[0,_Ha,_a],_Hm]]);};}else{var _Hn=new T(function(){return A(_H8,[_Hl[1]]);});return function(_Ho){return A(_Hd,[new T(function(){return A(_Hn,[_Ho]);}),function(_Hp){var _Hq=E(_Hp),_Hr=_Hq[2],_Hs=E(_Hq[1]);return _Hs[0]==0?A(_Hb,[[0,[0,_Ha,_Hl],_Hr]]):A(_Hb,[[0,[0,new T(function(){return A(_Hc,[_Hs[1]]);}),_a],_Hr]]);}]);};}}]);},_Ht=function(_Hu,_Hv,_Hw){var _Hx=new T(function(){return A(_oR,[_Hu,_9]);}),_Hy=new T(function(){return _ok(_Hw);}),_Hz=new T(function(){return _3x(_Hy);}),_HA=new T(function(){return _rT(_Hu,_Hw,_p3,_yo,_wc);});return function(_HB){return _H3(_Hu,_Hy,E(_Hv)[1],new T(function(){return A(_HA,[_a,_9I,_HB]);}),function(_HC,_HD){return E(_HC)[0]==0?A(_Hz,[[0,[1,_Hx],_HD]]):A(_Hz,[[0,_a,_HD]]);});};},_HE=new T(function(){return _Ht(_8D,_H0,_9J);}),_HF=new T(function(){return A(_HE,[_GM]);}),_HG=new T(function(){return _tn(_HF,_9H);}),_HH=function(_HI,_){var _HJ=A(_HG,[_HI,_]),_HK=E(_HJ),_HL=E(_HK[1]);return [0,[0,function(_HM,_){var _HN=A(_HL[1],[_HM,_]),_HO=_5k(_HM,_);return _HM;},new T(function(){var _HP=E(_HL[2]);return _HP[0]==0?E(_GM):E(_HP);})],_HK[2]];},_HQ=function(_HR,_){var _HS=_4B(_HH,_Gd,_HR,_),_HT=E(_HS),_HU=E(_HT[1]),_HV=new T(function(){return _Cw(_v8,_HU[1]);});return [0,[0,function(_HW,_){var _HX=A(_CG,[_HW,_]),_HY=A(_HV,[_HW,_]);return _HW;},_HU[2]],_HT[2]];},_HZ=[1,_5z],_I0=function(_I1,_){return _I1;},_I2=unCStr("main"),_I3=unCStr("Main"),_I4=unCStr("GalleryIndex"),_I5=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_I2,_I3,_I4],_I6=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_I5,_9],_I7=function(_I8){return E(_I6);},_I9=function(_Ia,_Ib){var _Ic=hs_leWord64(_Ia,_Ib);return E(_Ic)==0?false:true;},_Id=function(_Ie,_If,_Ig,_Ih){var _Ii=hs_eqWord64(_Ie,_Ig);if(!E(_Ii)){var _Ij=hs_leWord64(_Ie,_Ig);return E(_Ij)==0?false:true;}else{return _I9(_If,_Ih);}},_Ik=function(_Il,_Im){var _In=E(_Il),_Io=_In[1],_Ip=_In[2],_Iq=E(_Im),_Ir=_Iq[1],_Is=_Iq[2],_It=hs_eqWord64(_Io,_Ir);if(!E(_It)){return !_Id(_Io,_Ip,_Ir,_Is)?2:0;}else{var _Iu=hs_eqWord64(_Ip,_Is);return E(_Iu)==0?!_Id(_Io,_Ip,_Ir,_Is)?2:0:1;}},_Iv=function(_Iw,_Ix,_Iy,_Iz,_IA){while(1){var _IB=E(_IA);if(!_IB[0]){switch(_Ik([0,_Iw,_Ix,_Iy,_Iz],_IB[2])){case 0:_IA=_IB[4];continue;case 1:return [1,_IB[3]];default:_IA=_IB[5];continue;}}else{return [0];}}},_IC=function(_ID,_IE){var _IF=E(_ID),_IG=_IF[1],_IH=_IF[2],_II=_IF[3],_IJ=_IF[4],_IK=E(_IE);if(!_IK[0]){switch(_Ik(_IF,_IK[2])){case 0:return _Iv(_IG,_IH,_II,_IJ,_IK[4]);case 1:return [1,_IK[3]];default:return _Iv(_IG,_IH,_II,_IJ,_IK[5]);}}else{return [0];}},_IL=function(_IM,_IN,_IO,_IP){var _IQ=E(_IN),_IR=_IQ[1],_IS=_IQ[3],_IT=new T(function(){return A(_IP,[_p5]);}),_IU=new T(function(){return A(_IS,[_a]);});return A(_IR,[new T(function(){return A(_IR,[_IO,function(_IV){return A(_IS,[new T(function(){var _IW=E(_IM);return E(E(_IV)[5]);})]);}]);}),function(_IX){var _IY=_IC(_IT,_IX);return _IY[0]==0?E(_IU):A(_IS,[[1,_IY[1]]]);}]);},_IZ=new T(function(){return _IL(_1a,_3E,_18,_I7);}),_J0=function(_J1,_){var _J2=A(_IZ,[_J1,_]);return [0,[0,_I0,new T(function(){var _J3=E(E(_J2)[1]);return _J3[0]==0?E(_HZ):E(_J3);})],new T(function(){return E(E(_J2)[2]);})];},_J4=unCStr("Failure in Data.Map.balanceL"),_J5=new T(function(){return err(_J4);}),_J6=function(_J7,_J8,_J9,_Ja){var _Jb=E(_Ja);if(!_Jb[0]){var _Jc=_Jb[1],_Jd=E(_J9);if(!_Jd[0]){var _Je=_Jd[1],_Jf=_Jd[2],_Jg=_Jd[3];if(_Je<=(imul(3,_Jc)|0)){return [0,(1+_Je|0)+_Jc|0,E(E(_J7)),_J8,E(_Jd),E(_Jb)];}else{var _Jh=E(_Jd[4]);if(!_Jh[0]){var _Ji=_Jh[1],_Jj=E(_Jd[5]);if(!_Jj[0]){var _Jk=_Jj[1],_Jl=_Jj[2],_Jm=_Jj[3],_Jn=_Jj[4];if(_Jk>=(imul(2,_Ji)|0)){var _Jo=function(_Jp){var _Jq=E(_Jj[5]);return _Jq[0]==0?[0,(1+_Je|0)+_Jc|0,E(_Jl),_Jm,E([0,(1+_Ji|0)+_Jp|0,E(_Jf),_Jg,E(_Jh),E(_Jn)]),E([0,(1+_Jc|0)+_Jq[1]|0,E(E(_J7)),_J8,E(_Jq),E(_Jb)])]:[0,(1+_Je|0)+_Jc|0,E(_Jl),_Jm,E([0,(1+_Ji|0)+_Jp|0,E(_Jf),_Jg,E(_Jh),E(_Jn)]),E([0,1+_Jc|0,E(E(_J7)),_J8,E(_8),E(_Jb)])];},_Jr=E(_Jn);return _Jr[0]==0?_Jo(_Jr[1]):_Jo(0);}else{return [0,(1+_Je|0)+_Jc|0,E(_Jf),_Jg,E(_Jh),E([0,(1+_Jc|0)+_Jk|0,E(E(_J7)),_J8,E(_Jj),E(_Jb)])];}}else{return E(_J5);}}else{return E(_J5);}}}else{return [0,1+_Jc|0,E(E(_J7)),_J8,E(_8),E(_Jb)];}}else{var _Js=E(_J9);if(!_Js[0]){var _Jt=_Js[1],_Ju=_Js[2],_Jv=_Js[3],_Jw=_Js[5],_Jx=E(_Js[4]);if(!_Jx[0]){var _Jy=_Jx[1],_Jz=E(_Jw);if(!_Jz[0]){var _JA=_Jz[1],_JB=_Jz[2],_JC=_Jz[3],_JD=_Jz[4];if(_JA>=(imul(2,_Jy)|0)){var _JE=function(_JF){var _JG=E(_Jz[5]);return _JG[0]==0?[0,1+_Jt|0,E(_JB),_JC,E([0,(1+_Jy|0)+_JF|0,E(_Ju),_Jv,E(_Jx),E(_JD)]),E([0,1+_JG[1]|0,E(E(_J7)),_J8,E(_JG),E(_8)])]:[0,1+_Jt|0,E(_JB),_JC,E([0,(1+_Jy|0)+_JF|0,E(_Ju),_Jv,E(_Jx),E(_JD)]),E([0,1,E(E(_J7)),_J8,E(_8),E(_8)])];},_JH=E(_JD);return _JH[0]==0?_JE(_JH[1]):_JE(0);}else{return [0,1+_Jt|0,E(_Ju),_Jv,E(_Jx),E([0,1+_JA|0,E(E(_J7)),_J8,E(_Jz),E(_8)])];}}else{return [0,3,E(_Ju),_Jv,E(_Jx),E([0,1,E(E(_J7)),_J8,E(_8),E(_8)])];}}else{var _JI=E(_Jw);return _JI[0]==0?[0,3,E(_JI[2]),_JI[3],E([0,1,E(_Ju),_Jv,E(_8),E(_8)]),E([0,1,E(E(_J7)),_J8,E(_8),E(_8)])]:[0,2,E(E(_J7)),_J8,E(_Js),E(_8)];}}else{return [0,1,E(E(_J7)),_J8,E(_8),E(_8)];}}},_JJ=unCStr("Failure in Data.Map.balanceR"),_JK=new T(function(){return err(_JJ);}),_JL=function(_JM,_JN,_JO,_JP){var _JQ=E(_JO);if(!_JQ[0]){var _JR=_JQ[1],_JS=E(_JP);if(!_JS[0]){var _JT=_JS[1],_JU=_JS[2],_JV=_JS[3];if(_JT<=(imul(3,_JR)|0)){return [0,(1+_JR|0)+_JT|0,E(E(_JM)),_JN,E(_JQ),E(_JS)];}else{var _JW=E(_JS[4]);if(!_JW[0]){var _JX=_JW[1],_JY=_JW[2],_JZ=_JW[3],_K0=_JW[4],_K1=E(_JS[5]);if(!_K1[0]){var _K2=_K1[1];if(_JX>=(imul(2,_K2)|0)){var _K3=function(_K4){var _K5=E(_JM),_K6=E(_JW[5]);return _K6[0]==0?[0,(1+_JR|0)+_JT|0,E(_JY),_JZ,E([0,(1+_JR|0)+_K4|0,E(_K5),_JN,E(_JQ),E(_K0)]),E([0,(1+_K2|0)+_K6[1]|0,E(_JU),_JV,E(_K6),E(_K1)])]:[0,(1+_JR|0)+_JT|0,E(_JY),_JZ,E([0,(1+_JR|0)+_K4|0,E(_K5),_JN,E(_JQ),E(_K0)]),E([0,1+_K2|0,E(_JU),_JV,E(_8),E(_K1)])];},_K7=E(_K0);return _K7[0]==0?_K3(_K7[1]):_K3(0);}else{return [0,(1+_JR|0)+_JT|0,E(_JU),_JV,E([0,(1+_JR|0)+_JX|0,E(E(_JM)),_JN,E(_JQ),E(_JW)]),E(_K1)];}}else{return E(_JK);}}else{return E(_JK);}}}else{return [0,1+_JR|0,E(E(_JM)),_JN,E(_JQ),E(_8)];}}else{var _K8=E(_JP);if(!_K8[0]){var _K9=_K8[1],_Ka=_K8[2],_Kb=_K8[3],_Kc=_K8[5],_Kd=E(_K8[4]);if(!_Kd[0]){var _Ke=_Kd[1],_Kf=_Kd[2],_Kg=_Kd[3],_Kh=_Kd[4],_Ki=E(_Kc);if(!_Ki[0]){var _Kj=_Ki[1];if(_Ke>=(imul(2,_Kj)|0)){var _Kk=function(_Kl){var _Km=E(_JM),_Kn=E(_Kd[5]);return _Kn[0]==0?[0,1+_K9|0,E(_Kf),_Kg,E([0,1+_Kl|0,E(_Km),_JN,E(_8),E(_Kh)]),E([0,(1+_Kj|0)+_Kn[1]|0,E(_Ka),_Kb,E(_Kn),E(_Ki)])]:[0,1+_K9|0,E(_Kf),_Kg,E([0,1+_Kl|0,E(_Km),_JN,E(_8),E(_Kh)]),E([0,1+_Kj|0,E(_Ka),_Kb,E(_8),E(_Ki)])];},_Ko=E(_Kh);return _Ko[0]==0?_Kk(_Ko[1]):_Kk(0);}else{return [0,1+_K9|0,E(_Ka),_Kb,E([0,1+_Ke|0,E(E(_JM)),_JN,E(_8),E(_Kd)]),E(_Ki)];}}else{return [0,3,E(_Kf),_Kg,E([0,1,E(E(_JM)),_JN,E(_8),E(_8)]),E([0,1,E(_Ka),_Kb,E(_8),E(_8)])];}}else{var _Kp=E(_Kc);return _Kp[0]==0?[0,3,E(_Ka),_Kb,E([0,1,E(E(_JM)),_JN,E(_8),E(_8)]),E(_Kp)]:[0,2,E(E(_JM)),_JN,E(_8),E(_K8)];}}else{return [0,1,E(E(_JM)),_JN,E(_8),E(_8)];}}},_Kq=function(_Kr,_Ks,_Kt,_Ku,_Kv,_Kw){var _Kx=E(_Kw);if(!_Kx[0]){var _Ky=_Kx[2],_Kz=_Kx[3],_KA=_Kx[4],_KB=_Kx[5];switch(_Ik([0,_Kr,_Ks,_Kt,_Ku],_Ky)){case 0:return _J6(_Ky,_Kz,_Kq(_Kr,_Ks,_Kt,_Ku,_Kv,_KA),_KB);case 1:return [0,_Kx[1],E([0,_Kr,_Ks,_Kt,_Ku]),_Kv,E(_KA),E(_KB)];default:return _JL(_Ky,_Kz,_KA,_Kq(_Kr,_Ks,_Kt,_Ku,_Kv,_KB));}}else{return [0,1,E([0,_Kr,_Ks,_Kt,_Ku]),_Kv,E(_8),E(_8)];}},_KC=unCStr("100%"),_KD=[0,62],_KE=[1,_KD,_9],_KF=[1,_KE],_KG=new T(function(){return A(_yq,[_KF]);}),_KH=new T(function(){return _tn(_KG,_vp);}),_KI=function(_KJ){return E(_KH);},_KK=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_KL=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_KM=[1,_KL,_9],_KN=[1,_KK,_KM],_KO=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_KP=[1,_KO,_KN],_KQ=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_KR=[1,_KQ,_KP],_KS=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_KT=[1,_KS,_KR],_KU=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_KV=[1,_KU,_KT],_KW=unCStr("height"),_KX=function(_KY,_KZ){while(1){var _L0=E(_KY);if(!_L0[0]){return E(_KZ);}else{_KY=_L0[2];var _L1=_KZ+1|0;_KZ=_L1;continue;}}},_L2=new T(function(){return [0,_KX(_KV,0)-1|0];}),_L3=[0,_34,_5o],_L4=function(_L5,_){return [0,[0,_34,[1,_L5]],_L5];},_L6=unCStr("src"),_L7=unCStr("img"),_L8=function(_L9,_La){var _Lb=new T(function(){return A(_L9,[_La]);});return function(_Lc,_){var _Ld=jsCreateElem(toJSStr(E(_L7))),_Le=jsAppendChild(_Ld,E(_Lc)[1]),_Lf=[0,_Ld],_Lg=A(_Lb,[_Lf,_]);return _Lf;};},_Lh=new T(function(){return _L8(_v8,_34);}),_Li=unCStr("width"),_Lj=function(_Lk){return function(_cg,_s3){return _4B(function(_Gm,_){return _4B(_L4,function(_Ll){return function(_Lm,_){return [0,_L3,new T(function(){var _Ln=E(_Ll);return [0,_Ln[1],_Ln[2],_Ln[3],_Ln[4],new T(function(){return _Kq(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_I5,_9,new T(function(){var _Lo=E(_Lk)[1];return _Lo!=E(_L2)[1]?[0,_Lo+1|0]:E(_5z);}),_Ln[5]);})];})];};},_Gm,_);},function(_Lp,_Gm,_){return (function(_Gm,_){return _4B(function(_Lq,_){return [0,[0,function(_Lr,_){var _Ls=A(_Lh,[_Lr,_]),_Lt=A(_1,[_n,_Ls,_L6,new T(function(){var _Lu=E(_Lk)[1];return _Lu>=0?_wh(_KV,_Lu):E(_we);}),_]),_Lv=A(_1,[_n,_Ls,_Li,_KC,_]),_Lw=A(_1,[_n,_Ls,_KW,_KC,_]),_Lx=_5k(_Lr,_);return _Lr;},_AH],_Lq];},_KI,_Gm,_);})(_Gm,_);},_cg,_s3);};},_Ly=function(_Gm,_){return _4B(_J0,_Lj,_Gm,_);},_Lz=function(_LA,_LB,_){return _LC(_LB,_);},_LD=function(_Gm,_){return _yO(_Ly,_Lz,_Gm,_);},_LE=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_LF=new T(function(){return _58(_p,_LE);}),_LG=[0,20000],_LH=new T(function(){return _3R(_1a,_3E,_18,_15);}),_LI=function(_LJ,_LK,_LL,_){var _LM=A(_LH,[_LL,_]),_LN=new T(function(){return E(E(_LM)[1]);}),_LO=new T(function(){return [0,_LP];}),_LP=function(_){var _LQ=jsFind(toJSStr(E(_LN))),_LR=E(_LQ);if(!_LR[0]){return _0;}else{var _LS=E(_LR[1]),_LT=jsClearChildren(_LS[1]),_LU=E(_m)[1],_LV=takeMVar(_LU),_LW=A(_LK,[_LV,_]),_LX=E(_LW),_LY=E(_LX[1]),_=putMVar(_LU,_LX[2]),_LZ=A(_LY[1],[_LS,_]),_M0=E(_LY[2]);if(!_M0[0]){var _M1=jsSetTimeout(E(_LJ)[1],E(_LO)[1]);return _0;}else{var _M2=E(_M0[1]);return _0;}}},_M3=jsSetTimeout(E(_LJ)[1],E(_LO)[1]);return _yx(_LN,_LK,new T(function(){return E(E(_LM)[2]);}),_);},_LC=function(_M4,_){var _M5=_LI(_LG,_LD,_M4,_),_M6=E(_M5),_M7=E(_M6[1]);return [0,[0,function(_M8,_){var _M9=A(_LF,[_M8,_]),_Ma=A(_M7[1],[_M8,_]);return _M8;},_M7[2]],_M6[2]];},_Mb=function(_Mc,_Md,_Me){return A(_Mc,[[1,_2A,new T(function(){return A(_Md,[_Me]);})]]);},_Mf=unCStr("Key "),_Mg=unCStr("Mouse "),_Mh=unCStr("Click "),_Mi=unCStr("NoData"),_Mj=function(_Mk){return _1N(_Mi,_Mk);},_Ml=unCStr(": empty list"),_Mm=unCStr("Prelude."),_Mn=function(_Mo){return err(_1N(_Mm,new T(function(){return _1N(_Mo,_Ml);})));},_Mp=unCStr("foldr1"),_Mq=new T(function(){return _Mn(_Mp);}),_Mr=function(_Ms,_Mt){var _Mu=E(_Mt);if(!_Mu[0]){return E(_Mq);}else{var _Mv=_Mu[1],_Mw=E(_Mu[2]);return _Mw[0]==0?E(_Mv):A(_Ms,[_Mv,new T(function(){return _Mr(_Ms,_Mw);})]);}},_Mx=[0,32],_My=function(_Mz,_MA){var _MB=E(_MA);switch(_MB[0]){case 0:return E(_Mj);case 1:var _MC=function(_MD){return _3M(11,E(_MB[1])[1],[1,_Mx,new T(function(){var _ME=E(_MB[2]);return [1,_3L,new T(function(){return A(_Mr,[_Mb,[1,function(_MF){return _3M(0,E(_ME[1])[1],_MF);},[1,function(_MG){return _3M(0,E(_ME[2])[1],_MG);},_9]],[1,_3K,_MD]]);})];})]);};return E(_Mz)[1]<11?function(_MH){return _1N(_Mh,new T(function(){return _MC(_MH);}));}:function(_MI){return [1,_3L,new T(function(){return _1N(_Mh,new T(function(){return _MC([1,_3K,_MI]);}));})];};case 2:var _MJ=function(_MK){return _1N(_Mg,new T(function(){var _ML=E(_MB[1]);return [1,_3L,new T(function(){return A(_Mr,[_Mb,[1,function(_MM){return _3M(0,E(_ML[1])[1],_MM);},[1,function(_MN){return _3M(0,E(_ML[2])[1],_MN);},_9]],[1,_3K,_MK]]);})];}));};return E(_Mz)[1]<11?E(_MJ):function(_MO){return [1,_3L,new T(function(){return _MJ([1,_3K,_MO]);})];};default:var _MP=_MB[1];return E(_Mz)[1]<11?function(_MQ){return _1N(_Mf,new T(function(){return _3M(11,E(_MP)[1],_MQ);}));}:function(_MR){return [1,_3L,new T(function(){return _1N(_Mf,new T(function(){return _3M(11,E(_MP)[1],[1,_3K,_MR]);}));})];};}},_MS=[0,32],_MT=function(_MU){var _MV=new T(function(){return _58(_p,new T(function(){var _MW=E(_MU);return _1N(_MW[1],[1,_MS,new T(function(){return A(_My,[_pa,_MW[2],_9]);})]);}));});return function(_MX,_){return [0,[0,_MV,_AH],_MX];};},_MY=function(_){var _MZ=E(_st)[1],_N0=takeMVar(_MZ),_=putMVar(_MZ,_N0);return _N0;},_N1=function(_N2,_){var _N3=0;if(!E(_N3)){var _N4=_MY();return [0,[0,_34,[1,_N4]],_N2];}else{var _N5=E(_st)[1],_N6=takeMVar(_N5),_=putMVar(_N5,_N6);return [0,[0,_34,[1,_N6]],_N2];}},_N7=function(_Gm,_){return _4B(_N1,_MT,_Gm,_);},_N8=function(_N9){return E(_N7);},_Na=[12,coercionToken],_Nb=[9,coercionToken],_Nc=[11,coercionToken],_Nd=[5,coercionToken],_Ne=[10,coercionToken],_Nf=[6,coercionToken],_Ng=[7,coercionToken],_Nh=unCStr("height:100px;background-color:green;position:relative"),_Ni=unCStr("div"),_Nj=function(_Nk,_Nl){var _Nm=new T(function(){return A(_Nk,[_Nl]);});return function(_Nn,_){var _No=jsCreateElem(toJSStr(E(_Ni))),_Np=jsAppendChild(_No,E(_Nn)[1]),_Nq=[0,_No],_Nr=A(_Nm,[_Nq,_]);return _Nq;};},_Ns=unCStr("mouse events here"),_Nt=new T(function(){return _Nj(_p,_Ns);}),_Nu=unCStr("style"),_Nv=function(_Nw,_){var _Nx=A(_Nt,[_Nw,_]),_Ny=A(_1,[_n,_Nx,_Nu,_Nh,_]);return _Nx;},_Nz=[0,_Nv,_AH],_NA=function(_NB,_){return [0,_Nz,_NB];},_NC=new T(function(){return _tn(_NA,_Ng);}),_ND=new T(function(){return _tn(_NC,_Nf);}),_NE=new T(function(){return _tn(_ND,_Ne);}),_NF=new T(function(){return _tn(_NE,_Nd);}),_NG=new T(function(){return _tn(_NF,_Nc);}),_NH=new T(function(){return _tn(_NG,_vp);}),_NI=new T(function(){return _tn(_NH,_Nb);}),_NJ=new T(function(){return _tn(_NI,_Na);}),_NK=unCStr(" selected"),_NL=[1,_xR,_9],_NM=function(_NN){var _NO=new T(function(){return _58(_p,new T(function(){return _1N([1,_xR,new T(function(){return _xT(_NN,_NL);})],_NK);}));});return function(_NP,_){return [0,[0,_NO,_AH],_NP];};},_NQ=unCStr("Radio buttons"),_NR=new T(function(){return _w(_p,_NQ);}),_NS=new T(function(){return _58(_v8,_NR);}),_NT=[1,_BI,_9],_NU=[1,_AO,_NT],_NV=[1,_BU,_NU],_NW=new T(function(){return _yj(_y9);}),_NX=function(_NY){return E(E(_NY)[15]);},_NZ=unCStr("radio"),_O0=new T(function(){return A(_p3,[_6F]);}),_O1=unCStr("name"),_O2=function(_O3,_O4,_O5,_O6){var _O7=new T(function(){return _ok(_O4);}),_O8=new T(function(){return _3R([0,coercionToken],_3z(_O7),function(_O9){return _oB(_O7,_O9);},function(_Oa,_Ob){return _oE(_O7,_Oa,_Ob);});}),_Oc=new T(function(){return _3x(_O7);}),_Od=new T(function(){return _3x(_O7);}),_Oe=new T(function(){return _37(_O7);}),_Of=new T(function(){return _37(_O7);}),_Og=new T(function(){return _3x(_O7);}),_Oh=new T(function(){return _37(_O7);}),_Oi=new T(function(){return _3x(_O7);}),_Oj=new T(function(){return _37(_O7);}),_Ok=new T(function(){return _r9(_O3);}),_Ol=new T(function(){return _NX(_O3);}),_Om=new T(function(){return _rf(_O6);});return function(_On,_Oo){return function(_Op){return A(_Oe,[new T(function(){return A(_O8,[_Op]);}),function(_Oq){var _Or=new T(function(){return E(E(_Oq)[1]);}),_Os=new T(function(){return _oo(_O4,function(_){return jsFind(toJSStr(E(_Or)));});});return A(_Oj,[new T(function(){var _Ot=new T(function(){return E(E(_Oq)[2]);});return A(_Oi,[[0,_Ot,_Ot]]);}),function(_Ou){return A(_Oh,[new T(function(){return A(_Og,[[0,_0,new T(function(){var _Ov=E(E(_Ou)[1]);return [0,_Ov[1],_Ov[2],_r8,_Ov[4],_Ov[5]];})]]);}),function(_Ow){return A(_Of,[new T(function(){return A(_Os,[new T(function(){return E(E(_Ow)[2]);})]);}),function(_Ox){return A(_Oe,[new T(function(){var _Oy=E(_Ox),_Oz=_Oy[2],_OA=E(_Oy[1]);return _OA[0]==0?A(_Od,[[0,_9,_Oz]]):A(_oo,[_O4,function(_){return _AV(_OA[1],_6T,_);},_Oz]);}),function(_OB){var _OC=new T(function(){return !_sM(E(_OB)[1],_AY)?[0]:E([1,_On]);});return A(_Oc,[[0,[0,new T(function(){return A(_Ol,[new T(function(){return A(_Ok,[_Or,_NZ,new T(function(){var _OD=A(_O5,[_On]),_OE=E(_O0),_OF=hs_eqWord64(_OD[1],_OE[1]);if(!E(_OF)){return A(_Om,[_On]);}else{var _OG=hs_eqWord64(_OD[2],_OE[2]);return E(_OG)==0?A(_Om,[_On]):E(_On);}}),new T(function(){return E(_OC)[0]==0?false:true;}),_a]);}),[1,[0,_O1,_Oo],_9]]);}),new T(function(){var _OH=E(_OC);return _OH[0]==0?[0]:[1,_OH[1]];})],new T(function(){return E(E(_OB)[2]);})]]);}]);}]);}]);}]);}]);};};},_OI=new T(function(){return _6G(_oW,_p1);}),_OJ=new T(function(){return _O2(_8D,_9J,_OI,_NW);}),_OK=function(_OL){var _OM=E(_OL);if(!_OM[0]){return [0];}else{var _ON=_OM[1];return [1,function(_OO){var _OP=new T(function(){return _tn(new T(function(){return A(_OJ,[_ON,_OO]);}),_vp);});return function(_OQ,_){var _OR=A(_OP,[_OQ,_]),_OS=E(_OR),_OT=E(_OS[1]);return [0,[0,function(_OU,_){var _OV=_p(_ON,_OU,_),_OW=A(_OT[1],[_OU,_]);return _OU;},_OT[2]],_OS[2]];};},new T(function(){return _OK(_OM[2]);})];}},_OX=new T(function(){return _OK(_NV);}),_OY=function(_OZ,_P0){var _P1=new T(function(){return _90(_P0);}),_P2=new T(function(){return _H1(_P1);}),_P3=new T(function(){return _92(_P1);}),_P4=function(_P5){var _P6=E(_P5);if(!_P6[0]){return [0,_P2,_a];}else{var _P7=E(_P6[1]),_P8=_P4(_P6[2]);return [0,new T(function(){return A(_P3,[_P7[1],_P8[1]]);}),new T(function(){var _P9=E(_P7[2]);return _P9[0]==0?E(_P8[2]):E(_P9);})];}},_Pa=new T(function(){return _3x(_OZ);}),_Pb=new T(function(){return _3R([0,coercionToken],_3z(_OZ),function(_Pc){return _oB(_OZ,_Pc);},function(_Pd,_Pe){return _oE(_OZ,_Pd,_Pe);});}),_Pf=new T(function(){return _3x(_OZ);}),_Pg=new T(function(){return _37(_OZ);}),_Ph=new T(function(){return _37(_OZ);}),_Pi=new T(function(){return _37(_OZ);}),_Pj=new T(function(){return _37(_OZ);});return function(_Pk,_Pl){return A(_Pj,[new T(function(){return A(_Pb,[_Pl]);}),function(_Pm){return A(_Pi,[new T(function(){var _Pn=new T(function(){return E(E(_Pm)[1]);}),_Po=function(_Pp){var _Pq=E(_Pp);if(!_Pq[0]){return function(_Pr){return A(_Pf,[[0,_9,_Pr]]);};}else{var _Ps=new T(function(){return _Po(_Pq[2]);}),_Pt=new T(function(){return A(_Pq[1],[_Pn]);});return function(_Pu){return A(_Ph,[new T(function(){return A(_Pt,[_Pu]);}),function(_Pv){var _Pw=new T(function(){return E(E(_Pv)[1]);});return A(_Pg,[new T(function(){return A(_Ps,[new T(function(){return E(E(_Pv)[2]);})]);}),function(_Px){return A(_Pf,[[0,[1,_Pw,new T(function(){return E(E(_Px)[1]);})],new T(function(){return E(E(_Px)[2]);})]]);}]);}]);};}};return A(_Po,[_Pk,new T(function(){return E(E(_Pm)[2]);})]);}),function(_Py){var _Pz=new T(function(){var _PA=_P4(E(_Py)[1]);return [0,_PA[1],_PA[2]];});return A(_Pa,[[0,[0,new T(function(){return E(E(_Pz)[1]);}),new T(function(){var _PB=E(E(_Pz)[2]);return _PB[0]==0?[0]:[1,_PB[1]];})],new T(function(){return E(E(_Py)[2]);})]]);}]);}]);};},_PC=new T(function(){return _OY(_36,_8D);}),_PD=new T(function(){return A(_PC,[_OX]);}),_PE=function(_PF,_){var _PG=A(_PD,[_PF,_]),_PH=E(_PG),_PI=E(_PH[1]);return [0,[0,function(_PJ,_){var _PK=A(_NS,[_PJ,_]),_PL=A(_PI[1],[_PJ,_]);return _PJ;},_PI[2]],_PH[2]];},_PM=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_PN=new T(function(){return _58(_p,_PM);}),_PO=function(_PP,_PQ,_PR){var _PS=E(_PR);if(!_PS[0]){var _PT=_PS[3],_PU=_PS[4],_PV=_PS[5],_PW=E(_PS[2]),_PX=_PW[1];return _PP>=_PX?_PP!=_PX?_JL(_PW,_PT,_PU,_PO(_PP,_PQ,_PV)):[0,_PS[1],E([0,_PP]),_PQ,E(_PU),E(_PV)]:_J6(_PW,_PT,_PO(_PP,_PQ,_PU),_PV);}else{return [0,1,E([0,_PP]),_PQ,E(_8),E(_8)];}},_PY=function(_PZ,_Q0,_Q1){var _Q2=E(_PZ),_Q3=_Q2[1],_Q4=E(_Q1);if(!_Q4[0]){var _Q5=_Q4[3],_Q6=_Q4[4],_Q7=_Q4[5],_Q8=E(_Q4[2]),_Q9=_Q8[1];return _Q3>=_Q9?_Q3!=_Q9?_JL(_Q8,_Q5,_Q6,_PO(_Q3,_Q0,_Q7)):[0,_Q4[1],E(_Q2),_Q0,E(_Q6),E(_Q7)]:_J6(_Q8,_Q5,_PO(_Q3,_Q0,_Q6),_Q7);}else{return [0,1,E(_Q2),_Q0,E(_8),E(_8)];}},_Qa=function(_Qb,_Qc,_Qd){var _Qe=E(_Qb),_Qf=_Qe[1],_Qg=_Qe[2],_Qh=_Qe[3],_Qi=_Qe[4],_Qj=E(_Qd);if(!_Qj[0]){var _Qk=_Qj[2],_Ql=_Qj[3],_Qm=_Qj[4],_Qn=_Qj[5];switch(_Ik(_Qe,_Qk)){case 0:return _J6(_Qk,_Ql,_Kq(_Qf,_Qg,_Qh,_Qi,_Qc,_Qm),_Qn);case 1:return [0,_Qj[1],E(_Qe),_Qc,E(_Qm),E(_Qn)];default:return _JL(_Qk,_Ql,_Qm,_Kq(_Qf,_Qg,_Qh,_Qi,_Qc,_Qn));}}else{return [0,1,E(_Qe),_Qc,E(_8),E(_8)];}},_Qo=function(_Qp,_Qq){while(1){var _Qr=E(_Qq);if(!_Qr[0]){var _Qs=E(_Qr[2])[1];if(_Qp>=_Qs){if(_Qp!=_Qs){_Qq=_Qr[5];continue;}else{return [1,_Qr[3]];}}else{_Qq=_Qr[4];continue;}}else{return [0];}}},_Qt=[0,_34,_a],_Qu=function(_Qv,_){return [0,_Qt,_Qv];},_Qw=unCStr("containers-0.5.5.1"),_Qx=unCStr("Data.Map.Base"),_Qy=unCStr("Map"),_Qz=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Qw,_Qx,_Qy],_QA=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Qz,_9],_QB=function(_QC){return E(_QA);},_QD=function(_QE){var _QF=E(_QE);if(!_QF[0]){return [0];}else{var _QG=E(_QF[1]);return [1,[0,_QG[1],_QG[2]],new T(function(){return _QD(_QF[2]);})];}},_QH=function(_QI,_QJ){return function(_QK){return E(new T(function(){var _QL=A(_QI,[_6F]),_QM=E(_QL[3]),_QN=_QM[1],_QO=_QM[2],_QP=_1N(_QL[4],[1,new T(function(){return A(_QJ,[_6F]);}),_9]);if(!_QP[0]){return [0,_QN,_QO,_QM,_9];}else{var _QQ=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_QN,_QO],new T(function(){return _QD(_QP);})]));}));return [0,_QQ[1],_QQ[2],_QM,_QP];}}));};},_QR=new T(function(){return _QH(_QB,_oi);}),_QS=new T(function(){return _6G(_QR,_oi);}),_QT=new T(function(){return _IL(_1a,_3E,_18,_QS);}),_QU=function(_QV,_){var _QW=A(_QT,[_QV,_]);return [0,[0,_34,new T(function(){return E(E(_QW)[1]);})],new T(function(){return E(E(_QW)[2]);})];},_QX=new T(function(){return _6G(_QR,_oi);}),_QY=[1,_8],_QZ=new T(function(){return _IL(_1a,_3E,_18,_QX);}),_R0=function(_R1,_){var _R2=A(_QZ,[_R1,_]);return [0,[0,_I0,new T(function(){var _R3=E(E(_R2)[1]);return _R3[0]==0?E(_QY):E(_R3);})],new T(function(){return E(E(_R2)[2]);})];},_R4=[0,_34,_5o],_R5=[1,_a],_R6=function(_R7,_R8){var _R9=new T(function(){return [0,E(_R7)[1]+1|0];});return function(_cg,_s3){return _4B(function(_Gm,_){return _4B(function(_Ra,_){var _Rb=_4B(_QU,function(_Rc){var _Rd=_Qo(E(_R7)[1],_Rc);return _Rd[0]==0?E(_Qu):function(_Re,_){return [0,[0,_34,_Rd],_Re];};},_Ra,_),_Rf=E(_Rb),_Rg=E(_Rf[1]);return [0,[0,function(_Rh,_){var _Ri=A(_Rg[1],[_Rh,_]);return _Rh;},new T(function(){var _Rj=E(_Rg[2]);return _Rj[0]==0?E(_R5):[1,_Rj];})],_Rf[2]];},function(_Rk){var _Rl=new T(function(){return _tn(new T(function(){return A(_s5,[_a,_9I,_Rk]);}),_9H);});return function(_cg,_s3){return _4B(function(_Rm,_){var _Rn=A(_Rl,[_Rm,_]),_Ro=E(_Rn),_Rp=_Ro[2],_Rq=E(_Ro[1]),_Rr=_Rq[1],_Rs=_Rq[2],_Rt=E(_Rk);return _Rt[0]==0?[0,[0,function(_Ru,_){var _Rv=A(_Rr,[_Ru,_]);return _Ru;},_Rs],_Rp]:[0,[0,function(_Rw,_){var _Rx=A(_Rr,[_Rw,_]);return _Rw;},new T(function(){var _Ry=E(_Rs);return _Ry[0]==0?E(_Rt):E(_Ry);})],_Rp];},function(_Rz,_RA,_){return _4B(function(_Gm,_){return _4B(_R0,function(_RB){var _RC=new T(function(){return _PY(_R7,_Rz,_RB);}),_RD=new T(function(){return A(_QX,[_RC]);});return function(_cg,_s3){return _4B(_L4,function(_RE){return function(_RF,_){return [0,_R4,new T(function(){var _RG=E(_RE);return [0,_RG[1],_RG[2],_RG[3],_RG[4],new T(function(){return _Qa(_RD,_RC,_RG[5]);})];})];};},_cg,_s3);};},_Gm,_);},function(_RH,_Gm,_){return (function(_RI,_){return [0,[0,_34,[1,_Rz]],_RI];})(_Gm,_);},_RA,_);},_cg,_s3);};},_Gm,_);},function(_RJ){var _RK=new T(function(){return _R6(_R9,new T(function(){return _8S(_R8,_RJ);}));}),_RL=new T(function(){return _w(_p,new T(function(){return _3M(0,E(_R8)[1]+E(_RJ)[1]|0,_9);}));});return function(_cg,_s3){return _4B(function(_RM,_){return [0,[0,function(_RN,_){var _RO=A(_RL,[_RN,_]),_RP=_5k(_RN,_);return _RN;},_5o],_RM];},function(_RQ){return E(_RK);},_cg,_s3);};},_cg,_s3);};},_RR=new T(function(){return _R6(_5z,_5z);}),_RS=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_RT=new T(function(){return _58(_p,_RS);}),_RU=new T(function(){return A(_s5,[_a,_9I,_a]);}),_RV=new T(function(){return _tn(_RU,_9H);}),_RW=function(_RX){var _RY=new T(function(){return _w(_p,new T(function(){return _5h(_RX);}));});return function(_cg,_s3){return _4B(_RV,function(_RZ){var _S0=E(E(_RZ)[1]);if(!_S0){return function(_S1,_){return [0,[0,function(_S2,_){var _S3=_5k(_S2,_),_S4=_p(_5p,_S2,_),_S5=A(_RY,[_S2,_]);return _S2;},_a],_S1];};}else{var _S6=new T(function(){return _RW(new T(function(){return [0,E(_RX)[1]+_S0|0];}));}),_S7=new T(function(){return _w(_p,new T(function(){return _3M(0,E(_RX)[1]+_S0|0,_9);}));});return function(_cg,_s3){return _4B(function(_S8,_){return [0,[0,function(_S9,_){var _Sa=A(_S7,[_S9,_]),_Sb=_5k(_S9,_);return _S9;},_5o],_S8];},function(_Sc){return E(_S6);},_cg,_s3);};}},_cg,_s3);};},_Sd=new T(function(){return _RW(_5z);}),_Se=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_Sf=new T(function(){return _58(_p,_Se);}),_Sg=function(_Sh){return function(_Si,_){return [0,[0,new T(function(){var _Sj=new T(function(){return _w(_p,new T(function(){return _5h(_Sh);}));});return _58(_v8,function(_Sk,_){var _Sl=_p(_5p,_Sk,_),_Sm=A(_Sj,[_Sk,_]);return _Sk;});}),_5o],_Si];};},_Sn=new T(function(){return A(_s5,[_a,_9I,_a]);}),_So=new T(function(){return _tn(_Sn,_9H);}),_Sp=new T(function(){return A(_s5,[_a,_9I,_a]);}),_Sq=new T(function(){return _tn(_Sp,_9H);}),_Sr=unCStr("second number "),_Ss=unCStr("first number"),_St=function(_Su,_){var _Sv=A(_Sq,[_Su,_]),_Sw=E(_Sv),_Sx=E(_Sw[1]),_Sy=A(_So,[_Sw[2],_]),_Sz=E(_Sy),_SA=E(_Sz[1]);return [0,[0,function(_SB,_){var _SC=_p(_Ss,_SB,_),_SD=_5k(_SB,_),_SE=A(_Sx[1],[_SB,_]),_SF=_5k(_SB,_),_SG=_p(_Sr,_SB,_),_SH=_5k(_SB,_),_SI=A(_SA[1],[_SB,_]),_SJ=_5k(_SB,_);return _SB;},new T(function(){var _SK=E(_Sx[2]);if(!_SK[0]){return [0];}else{var _SL=E(_SA[2]);return _SL[0]==0?[0]:[1,new T(function(){return _8S(_SK[1],_SL[1]);})];}})],_Sz[2]];},_SM=function(_SN,_){var _SO=_4B(_St,_Sg,_SN,_),_SP=E(_SO),_SQ=E(_SP[1]),_SR=new T(function(){return _58(_v8,_SQ[1]);});return [0,[0,function(_SS,_){var _ST=A(_Sf,[_SS,_]),_SU=A(_SR,[_SS,_]);return _SS;},_SQ[2]],_SP[2]];},_SV=unCStr("td"),_SW=function(_SX,_SY){var _SZ=new T(function(){return A(_SX,[_SY]);});return function(_T0,_){var _T1=jsCreateElem(toJSStr(E(_SV))),_T2=jsAppendChild(_T1,E(_T0)[1]),_T3=[0,_T1],_T4=A(_SZ,[_T3,_]);return _T3;};},_T5=unCStr("tr"),_T6=function(_T7,_T8){var _T9=new T(function(){return A(_T7,[_T8]);});return function(_Ta,_){var _Tb=jsCreateElem(toJSStr(E(_T5))),_Tc=jsAppendChild(_Tb,E(_Ta)[1]),_Td=[0,_Tb],_Te=A(_T9,[_Td,_]);return _Td;};},_Tf=function(_Tg,_){var _Th=_SM(_Tg,_),_Ti=E(_Th),_Tj=E(_Ti[1]),_Tk=A(_vm,[_Ti[2],_]),_Tl=E(_Tk),_Tm=E(_Tl[1]),_Tn=A(_Sd,[_Tl[2],_]),_To=E(_Tn),_Tp=E(_To[1]),_Tq=A(_A1,[_To[2],_]),_Tr=E(_Tq),_Ts=E(_Tr[1]),_Tt=A(_RR,[_Tr[2],_]),_Tu=E(_Tt),_Tv=E(_Tu[1]),_Tw=A(_AG,[_Tu[2],_]),_Tx=E(_Tw),_Ty=E(_Tx[1]),_Tz=_HQ(_Tx[2],_),_TA=E(_Tz),_TB=E(_TA[1]),_TC=_LC(_TA[2],_),_TD=E(_TC),_TE=E(_TD[1]),_TF=_4B(_NJ,_N8,_TD[2],_),_TG=E(_TF),_TH=E(_TG[1]),_TI=_4B(_C7,_AM,_TG[2],_),_TJ=E(_TI),_TK=E(_TJ[1]),_TL=_4B(_PE,_NM,_TJ[2],_),_TM=E(_TL),_TN=E(_TM[1]);return [0,[0,function(_TO,_){var _TP=A(new T(function(){var _TQ=new T(function(){return _SW(_v8,function(_TR,_){var _TS=A(_RT,[_TR,_]),_TT=A(_Tp[1],[_TR,_]);return _TR;});}),_TU=new T(function(){return _SW(_v8,_Tm[1]);}),_TV=new T(function(){return _SW(_v8,_Tj[1]);});return _T6(_v8,function(_TW,_){var _TX=A(_TV,[_TW,_]),_TY=A(_TU,[_TW,_]),_TZ=A(_TQ,[_TW,_]);return _TW;});}),[_TO,_]),_U0=A(_1,[_n,_TP,_53,_54,_]),_U1=A(new T(function(){var _U2=new T(function(){return _SW(_v8,_Ty[1]);}),_U3=new T(function(){return _SW(_v8,function(_U4,_){var _U5=A(_PN,[_U4,_]),_U6=A(_Tv[1],[_U4,_]);return _U4;});}),_U7=new T(function(){return _SW(_v8,_Ts[1]);});return _T6(_v8,function(_U8,_){var _U9=A(_U7,[_U8,_]),_Ua=A(_U3,[_U8,_]),_Ub=A(_U2,[_U8,_]);return _U8;});}),[_TO,_]),_Uc=A(_1,[_n,_U1,_53,_54,_]),_Ud=A(new T(function(){var _Ue=new T(function(){return _SW(_v8,_TH[1]);}),_Uf=new T(function(){return _SW(_v8,_TE[1]);}),_Ug=new T(function(){return _SW(_v8,_TB[1]);});return _T6(_v8,function(_Uh,_){var _Ui=A(_Ug,[_Uh,_]),_Uj=A(_Uf,[_Uh,_]),_Uk=A(_Ue,[_Uh,_]);return _Uh;});}),[_TO,_]),_Ul=A(_1,[_n,_Ud,_Nu,_54,_]),_Um=A(new T(function(){var _Un=new T(function(){return _SW(_v8,_TN[1]);}),_Uo=new T(function(){return _SW(_v8,_TK[1]);});return _T6(_v8,function(_Up,_){var _Uq=A(_Uo,[_Up,_]),_Ur=A(_Un,[_Up,_]);return _Up;});}),[_TO,_]),_Us=A(_1,[_n,_Um,_Nu,_54,_]);return _TO;},new T(function(){var _Ut=E(_Tj[2]);if(!_Ut[0]){var _Uu=E(_Tm[2]);if(!_Uu[0]){var _Uv=E(_Tp[2]);if(!_Uv[0]){var _Uw=E(_Ts[2]);if(!_Uw[0]){var _Ux=E(_Tv[2]);if(!_Ux[0]){var _Uy=E(_Ty[2]);if(!_Uy[0]){var _Uz=E(_TB[2]);if(!_Uz[0]){var _UA=E(_TE[2]);if(!_UA[0]){var _UB=E(_TH[2]);if(!_UB[0]){var _UC=E(_TK[2]);return _UC[0]==0?E(_TN[2]):E(_UC);}else{return E(_UB);}}else{return E(_UA);}}else{return E(_Uz);}}else{return E(_Uy);}}else{return E(_Ux);}}else{return E(_Uw);}}else{return E(_Uv);}}else{return E(_Uu);}}else{return E(_Ut);}})],_TM[2]];},_UD=unCStr("h1"),_UE=function(_UF,_UG){var _UH=new T(function(){return A(_UF,[_UG]);});return function(_UI,_){var _UJ=jsCreateElem(toJSStr(E(_UD))),_UK=jsAppendChild(_UJ,E(_UI)[1]),_UL=[0,_UJ],_UM=A(_UH,[_UL,_]);return _UL;};},_UN=unCStr("hplayground examples"),_UO=new T(function(){return _UE(_p,_UN);}),_UP=unCStr("idelem"),_UQ=unCStr("table"),_UR=function(_US,_UT){var _UU=new T(function(){return A(_US,[_UT]);});return function(_UV,_){var _UW=jsCreateElem(toJSStr(E(_UQ))),_UX=jsAppendChild(_UW,E(_UV)[1]),_UY=[0,_UW],_UZ=A(_UU,[_UY,_]);return _UY;};},_V0=function(_){var _V1=E(_UP),_V2=jsFind(toJSStr(_V1)),_V3=E(_V2);if(!_V3[0]){return _45(_V1);}else{var _V4=_V3[1],_V5=E(_m)[1],_V6=takeMVar(_V5),_V7=_Tf(_V6,_),_V8=E(_V7),_V9=E(_V8[1]),_=putMVar(_V5,_V8[2]),_Va=A(_UO,[_V4,_]),_Vb=A(_1,[_n,_Va,_Nu,_H,_]),_Vc=A(_UR,[_v8,_V9[1],_V4,_]),_Vd=A(_G,[_V4,_]);return _V9[2];}},_Ve=function(_){return _V0(_);};
var hasteMain = function() {A(_Ve, [0]);};window.onload = hasteMain;