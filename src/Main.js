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

var _0=[0,2],_1=unCStr("id"),_2=0,_3=function(_4,_5,_6,_7){return A(_4,[new T(function(){return function(_){var _8=jsSetAttr(E(_5)[1],toJSStr(E(_6)),toJSStr(E(_7)));return _2;};})]);},_9=[0],_a=function(_b){return E(_b);},_c=unCStr("span"),_d=function(_e,_f,_){var _g=A(_e,[_]);return A(_f,[_]);},_h=function(_i,_j,_){return _d(_i,_j,_);},_k=function(_l,_m,_){var _n=A(_l,[_]);return A(_m,[_n,_]);},_o=unCStr("base"),_p=unCStr("GHC.IO.Exception"),_q=unCStr("IOException"),_r=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_o,_p,_q],_s=[0],_t=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_r,_s],_u=function(_v){return E(_t);},_w=function(_x){return E(E(_x)[1]);},_y=unCStr("Maybe.fromJust: Nothing"),_z=new T(function(){return err(_y);}),_A=function(_B,_C,_D){var _E=new T(function(){var _F=A(_B,[_D]),_G=A(_C,[new T(function(){var _H=E(_E);return _H[0]==0?E(_z):E(_H[1]);})]),_I=hs_eqWord64(_F[1],_G[1]);if(!E(_I)){return [0];}else{var _J=hs_eqWord64(_F[2],_G[2]);return E(_J)==0?[0]:[1,_D];}});return E(_E);},_K=function(_L){var _M=E(_L);return _A(_w(_M[1]),_u,_M[2]);},_N=unCStr(": "),_O=[0,41],_P=unCStr(" ("),_Q=function(_R,_S){var _T=E(_R);return _T[0]==0?E(_S):[1,_T[1],new T(function(){return _Q(_T[2],_S);})];},_U=unCStr("already exists"),_V=unCStr("does not exist"),_W=unCStr("protocol error"),_X=unCStr("failed"),_Y=unCStr("invalid argument"),_Z=unCStr("inappropriate type"),_10=unCStr("hardware fault"),_11=unCStr("unsupported operation"),_12=unCStr("timeout"),_13=unCStr("resource vanished"),_14=unCStr("interrupted"),_15=unCStr("resource busy"),_16=unCStr("resource exhausted"),_17=unCStr("end of file"),_18=unCStr("illegal operation"),_19=unCStr("permission denied"),_1a=unCStr("user error"),_1b=unCStr("unsatisified constraints"),_1c=unCStr("system error"),_1d=function(_1e,_1f){switch(E(_1e)){case 0:return _Q(_U,_1f);case 1:return _Q(_V,_1f);case 2:return _Q(_15,_1f);case 3:return _Q(_16,_1f);case 4:return _Q(_17,_1f);case 5:return _Q(_18,_1f);case 6:return _Q(_19,_1f);case 7:return _Q(_1a,_1f);case 8:return _Q(_1b,_1f);case 9:return _Q(_1c,_1f);case 10:return _Q(_W,_1f);case 11:return _Q(_X,_1f);case 12:return _Q(_Y,_1f);case 13:return _Q(_Z,_1f);case 14:return _Q(_10,_1f);case 15:return _Q(_11,_1f);case 16:return _Q(_12,_1f);case 17:return _Q(_13,_1f);default:return _Q(_14,_1f);}},_1g=[0,125],_1h=unCStr("{handle: "),_1i=function(_1j,_1k,_1l,_1m,_1n,_1o){var _1p=new T(function(){var _1q=new T(function(){return _1d(_1k,new T(function(){var _1r=E(_1m);return _1r[0]==0?E(_1o):_Q(_P,new T(function(){return _Q(_1r,[1,_O,_1o]);}));}));}),_1s=E(_1l);return _1s[0]==0?E(_1q):_Q(_1s,new T(function(){return _Q(_N,_1q);}));}),_1t=E(_1n);if(!_1t[0]){var _1u=E(_1j);if(!_1u[0]){return E(_1p);}else{var _1v=E(_1u[1]);return _1v[0]==0?_Q(_1h,new T(function(){return _Q(_1v[1],[1,_1g,new T(function(){return _Q(_N,_1p);})]);})):_Q(_1h,new T(function(){return _Q(_1v[1],[1,_1g,new T(function(){return _Q(_N,_1p);})]);}));}}else{return _Q(_1t[1],new T(function(){return _Q(_N,_1p);}));}},_1w=function(_1x){var _1y=E(_1x);return _1i(_1y[1],_1y[2],_1y[3],_1y[4],_1y[6],_s);},_1z=function(_1A,_1B){var _1C=E(_1A);return _1i(_1C[1],_1C[2],_1C[3],_1C[4],_1C[6],_1B);},_1D=[0,44],_1E=[0,93],_1F=[0,91],_1G=function(_1H,_1I,_1J){var _1K=E(_1I);return _1K[0]==0?unAppCStr("[]",_1J):[1,_1F,new T(function(){return A(_1H,[_1K[1],new T(function(){var _1L=function(_1M){var _1N=E(_1M);return _1N[0]==0?E([1,_1E,_1J]):[1,_1D,new T(function(){return A(_1H,[_1N[1],new T(function(){return _1L(_1N[2]);})]);})];};return _1L(_1K[2]);})]);})];},_1O=function(_1P,_1Q){return _1G(_1z,_1P,_1Q);},_1R=function(_1S,_1T,_1U){var _1V=E(_1T);return _1i(_1V[1],_1V[2],_1V[3],_1V[4],_1V[6],_1U);},_1W=[0,_1R,_1w,_1O],_1X=new T(function(){return [0,_u,_1W,_1Y,_K];}),_1Y=function(_1Z){return [0,_1X,_1Z];},_20=7,_21=function(_22){return [0,_9,_20,_s,_22,_9,_9];},_23=function(_24,_){return die(new T(function(){return _1Y(new T(function(){return _21(_24);}));}));},_25=function(_26,_){return _23(_26,_);},_27=function(_28,_){return _28;},_29=[0,_k,_h,_27,_25],_2a=function(_2b){return E(E(_2b)[1]);},_2c=function(_2d,_2e,_2f,_2g){return A(_2a,[_2d,new T(function(){return A(_2e,[_2g]);}),function(_2h){return A(_2f,[new T(function(){return E(E(_2h)[1]);}),new T(function(){return E(E(_2h)[2]);})]);}]);},_2i=function(_2j,_2k,_2l,_2m){return A(_2a,[_2j,new T(function(){return A(_2k,[_2m]);}),function(_2n){return A(_2l,[new T(function(){return E(E(_2n)[2]);})]);}]);},_2o=function(_2p,_2q,_2r,_2s){return _2i(_2p,_2q,_2r,_2s);},_2t=function(_2u){return E(E(_2u)[4]);},_2v=function(_2w,_2x){var _2y=new T(function(){return A(_2t,[_2w,_2x]);});return function(_2z){return E(_2y);};},_2A=function(_2B){return E(E(_2B)[3]);},_2C=function(_2D){var _2E=new T(function(){return _2A(_2D);});return [0,function(_2q,_2r,_2s){return _2c(_2D,_2q,_2r,_2s);},function(_2q,_2r,_2s){return _2o(_2D,_2q,_2r,_2s);},function(_2F,_2G){return A(_2E,[[0,_2F,_2G]]);},function(_2s){return _2v(_2D,_2s);}];},_2H=new T(function(){return _2C(_29);}),_2I=function(_2J,_2K){var _2L=jsShowI(_2J);return _Q(fromJSStr(_2L),_2K);},_2M=[0,41],_2N=[0,40],_2O=function(_2P,_2Q,_2R){return _2Q>=0?_2I(_2Q,_2R):_2P<=6?_2I(_2Q,_2R):[1,_2N,new T(function(){var _2S=jsShowI(_2Q);return _Q(fromJSStr(_2S),[1,_2M,_2R]);})];},_2T=[0,112],_2U=function(_2V,_2W,_2X,_2Y){var _2Z=E(_2W);return A(_2Z[1],[new T(function(){var _30=E(_2V);return E(_2X);}),function(_31){var _32=new T(function(){return E(E(_31)[2]);});return A(_2Z[2],[new T(function(){return A(_2Y,[new T(function(){var _33=E(new T(function(){var _34=E(_2V);return [0,coercionToken];})),_35=E(_31);return [0,_35[1],new T(function(){return [0,E(_32)[1]+1|0];}),_35[3],_35[4]];})]);}),new T(function(){return A(_2Z[3],[[1,_2T,new T(function(){return _Q(_2O(0,E(_32)[1],_s),new T(function(){return E(E(_31)[1]);}));})]]);})]);}]);},_36=[0,coercionToken],_37=function(_38,_){return [0,_38,_38];},_39=function(_3a,_3b,_){return [0,_2,_3a];},_3c=new T(function(){return _2U(_36,_2H,_37,_39);}),_3d=2,_3e=[0,0],_3f=function(_){var _=0,_3g=nMV(_3e);return [0,_3g];},_3h=function(_3i){var _3j=A(_3i,[_]);return E(_3j);},_3k=new T(function(){return _3h(_3f);}),_3l=function(_){return _2;},_3m=[1,_2],_3n=[0,_27,_3m],_3o=function(_3p,_3q,_){return [0,[0,_27,[1,new T(function(){return E(E(_3p)[2]);})]],_3q];},_3r=function(_3s,_){return [0,[0,_27,[1,_3s]],_3s];},_3t=function(_3u,_){return _3v(_3r,_3o,_3u,_);},_3w=unCStr(" could be found!"),_3x=function(_3y){return err(unAppCStr("No element with ID ",new T(function(){return _Q(_3y,_3w);})));},_3z=function(_3A,_3B,_){var _3C=E(_3k)[1],_3D=rMV(_3C),_3E=E(_3B),_3F=jsFind(toJSStr(_3E)),_3G=E(_3F);if(!_3G[0]){return _3x(_3E);}else{var _3H=E(_3G[1]),_3I=jsClearChildren(_3H[1]),_3J=A(_3A,[[0,_s,_3D,_3d,_3l],_]),_3K=E(_3J),_3L=function(_3M,_3N,_){return _3O(function(_3P,_){var _=wMV(_3C,_3M);return [0,_3n,_3P];},_3N,_,coercionToken);},_3O=function(_3Q,_3R,_,_3S){var _3T=A(_3c,[_3R,_]),_3U=new T(function(){return E(E(_3T)[1]);}),_3V=A(_3Q,[new T(function(){var _3W=E(E(_3T)[2]);return [0,_3W[1],_3W[2],_3W[3],function(_){var _3X=rMV(_3C),_3Y=E(_3U),_3Z=jsFind(toJSStr(_3Y)),_40=E(_3Z);if(!_40[0]){return _3x(_3Y);}else{var _41=E(_40[1]),_42=jsClearChildren(_41[1]),_43=_3O(function(_44,_){var _45=A(_3Q,[_3W,_]);return [0,[0,_27,E(E(_45)[1])[2]],_44];},[0,_s,_3X,_3d,_3l],_,coercionToken),_46=E(_43),_47=_48(_3t,_3L,_46[2],_),_49=A(E(_46[1])[1],[_41,_]),_4a=A(E(E(_47)[1])[1],[_41,_]);return _2;}}];}),_]),_4b=E(_3V),_4c=_4b[2],_4d=E(_4b[1]),_4e=_4d[1];return E(_4d[2])[0]==0?[0,[0,function(_4f,_){var _4g=A(_4e,[_4f,_]),_4h=E(_3U),_4i=jsFind(toJSStr(_4h));if(!E(_4i)[0]){var _4j=jsCreateElem(toJSStr(E(_c))),_4k=A(_3,[_a,[0,_4j],_1,_4h,_]),_4l=E(_4f),_4m=jsAppendChild(_4j,_4l[1]);return _4l;}else{return _4f;}},_9],_4c]:[0,[0,function(_4n,_){var _4o=A(_4e,[_4n,_]),_4p=E(_3U),_4q=jsFind(toJSStr(_4p));if(!E(_4q)[0]){var _4r=jsCreateElem(toJSStr(E(_c))),_4s=A(_3,[_a,[0,_4r],_1,_4p,_]),_4t=E(_4n),_4u=jsAppendChild(_4r,_4t[1]);return _4t;}else{return _4n;}},_3m],_4c];},_48=function(_4v,_4w,_4x,_){var _4y=A(_3c,[_4x,_]),_4z=new T(function(){return E(E(_4y)[1]);}),_4A=A(_4v,[new T(function(){var _4B=E(E(_4y)[2]);return [0,_4B[1],_4B[2],_4B[3],function(_){var _4C=rMV(_3C),_4D=E(_4z),_4E=jsFind(toJSStr(_4D)),_4F=E(_4E);if(!_4F[0]){return _3x(_4D);}else{var _4G=E(_4F[1]),_4H=jsClearChildren(_4G[1]),_4I=_48(function(_4J,_){var _4K=A(_4v,[_4B,_]);return [0,[0,_27,E(E(_4K)[1])[2]],_4J];},_4w,[0,_s,_4C,_3d,_3l],_),_4L=E(_4I),_4M=_48(_3t,_3L,_4L[2],_),_4N=A(E(_4L[1])[1],[_4G,_]),_4O=A(E(E(_4M)[1])[1],[_4G,_]);return _2;}}];}),_]),_4P=E(_4A),_4Q=_4P[2],_4R=E(_4P[1]),_4S=_4R[1],_4T=E(_4R[2]);if(!_4T[0]){return [0,[0,function(_4U,_){var _4V=A(_4S,[_4U,_]),_4W=E(_4z),_4X=jsFind(toJSStr(_4W));if(!E(_4X)[0]){var _4Y=jsCreateElem(toJSStr(E(_c))),_4Z=A(_3,[_a,[0,_4Y],_1,_4W,_]),_50=E(_4U),_51=jsAppendChild(_4Y,_50[1]);return _50;}else{return _4U;}},_9],_4Q];}else{var _52=A(_4w,[_4T[1],_4Q,_]),_53=E(_52),_54=E(_53[1]),_55=_54[1];return [0,[0,function(_56,_){var _57=A(_4S,[_56,_]),_58=E(_4z),_59=jsFind(toJSStr(_58));if(!E(_59)[0]){var _5a=jsCreateElem(toJSStr(E(_c))),_5b=A(_3,[_a,[0,_5a],_1,_58,_]),_5c=E(_56),_5d=jsAppendChild(_5a,_5c[1]),_5e=A(_55,[_5c,_]);return _5c;}else{var _5f=A(_55,[_56,_]);return _56;}},_54[2]],_53[2]];}},_5g=function(_5h,_5i,_){return _5j(function(_5k,_){var _=wMV(_3C,_5h);return [0,_3n,_5k];},_5i,_,coercionToken);},_5j=function(_5l,_5m,_,_5n){var _5o=A(_3c,[_5m,_]),_5p=new T(function(){return E(E(_5o)[1]);}),_5q=A(_5l,[new T(function(){var _5r=E(E(_5o)[2]);return [0,_5r[1],_5r[2],_5r[3],function(_){var _5s=rMV(_3C),_5t=E(_5p),_5u=jsFind(toJSStr(_5t)),_5v=E(_5u);if(!_5v[0]){return _3x(_5t);}else{var _5w=E(_5v[1]),_5x=jsClearChildren(_5w[1]),_5y=_5j(function(_5z,_){var _5A=A(_5l,[_5r,_]);return [0,[0,_27,E(E(_5A)[1])[2]],_5z];},[0,_s,_5s,_3d,_3l],_,coercionToken),_5B=E(_5y),_5C=_5D(_3t,_5g,_5B[2],_),_5E=A(E(_5B[1])[1],[_5w,_]),_5F=A(E(E(_5C)[1])[1],[_5w,_]);return _2;}}];}),_]),_5G=E(_5q),_5H=_5G[2],_5I=E(_5G[1]),_5J=_5I[1];return E(_5I[2])[0]==0?[0,[0,function(_5K,_){var _5L=A(_5J,[_5K,_]),_5M=E(_5p),_5N=jsFind(toJSStr(_5M));if(!E(_5N)[0]){var _5O=jsCreateElem(toJSStr(E(_c))),_5P=A(_3,[_a,[0,_5O],_1,_5M,_]),_5Q=E(_5K),_5R=jsAppendChild(_5O,_5Q[1]);return _5Q;}else{return _5K;}},_9],_5H]:[0,[0,function(_5S,_){var _5T=A(_5J,[_5S,_]),_5U=E(_5p),_5V=jsFind(toJSStr(_5U));if(!E(_5V)[0]){var _5W=jsCreateElem(toJSStr(E(_c))),_5X=A(_3,[_a,[0,_5W],_1,_5U,_]),_5Y=E(_5S),_5Z=jsAppendChild(_5W,_5Y[1]);return _5Y;}else{return _5S;}},_3m],_5H];},_5D=function(_60,_61,_62,_){var _63=A(_3c,[_62,_]),_64=new T(function(){return E(E(_63)[1]);}),_65=A(_60,[new T(function(){var _66=E(E(_63)[2]);return [0,_66[1],_66[2],_66[3],function(_){var _67=rMV(_3C),_68=E(_64),_69=jsFind(toJSStr(_68)),_6a=E(_69);if(!_6a[0]){return _3x(_68);}else{var _6b=E(_6a[1]),_6c=jsClearChildren(_6b[1]),_6d=_5D(function(_6e,_){var _6f=A(_60,[_66,_]);return [0,[0,_27,E(E(_6f)[1])[2]],_6e];},_61,[0,_s,_67,_3d,_3l],_),_6g=E(_6d),_6h=_5D(_3t,_5g,_6g[2],_),_6i=A(E(_6g[1])[1],[_6b,_]),_6j=A(E(E(_6h)[1])[1],[_6b,_]);return _2;}}];}),_]),_6k=E(_65),_6l=_6k[2],_6m=E(_6k[1]),_6n=_6m[1],_6o=E(_6m[2]);if(!_6o[0]){return [0,[0,function(_6p,_){var _6q=A(_6n,[_6p,_]),_6r=E(_64),_6s=jsFind(toJSStr(_6r));if(!E(_6s)[0]){var _6t=jsCreateElem(toJSStr(E(_c))),_6u=A(_3,[_a,[0,_6t],_1,_6r,_]),_6v=E(_6p),_6w=jsAppendChild(_6t,_6v[1]);return _6v;}else{return _6p;}},_9],_6l];}else{var _6x=A(_61,[_6o[1],_6l,_]),_6y=E(_6x),_6z=E(_6y[1]),_6A=_6z[1];return [0,[0,function(_6B,_){var _6C=A(_6n,[_6B,_]),_6D=E(_64),_6E=jsFind(toJSStr(_6D));if(!E(_6E)[0]){var _6F=jsCreateElem(toJSStr(E(_c))),_6G=A(_3,[_a,[0,_6F],_1,_6D,_]),_6H=E(_6B),_6I=jsAppendChild(_6F,_6H[1]),_6J=A(_6A,[_6H,_]);return _6H;}else{var _6K=A(_6A,[_6B,_]);return _6B;}},_6z[2]],_6y[2]];}},_6L=_48(_3t,_5g,_3K[2],_),_6M=A(E(_3K[1])[1],[_3H,_]),_6N=A(E(E(_6L)[1])[1],[_3H,_]);return _2;}},_3v=function(_6O,_6P,_6Q,_){var _6R=A(_3c,[_6Q,_]),_6S=new T(function(){return E(E(_6R)[1]);}),_6T=A(_6O,[new T(function(){var _6U=E(E(_6R)[2]);return [0,_6U[1],_6U[2],_6U[3],function(_){return _3z(function(_3u,_){return _3v(function(_6V,_){var _6W=A(_6O,[_6U,_]);return [0,[0,_27,E(E(_6W)[1])[2]],_6V];},_6P,_3u,_);},_6S,_);}];}),_]),_6X=E(_6T),_6Y=_6X[2],_6Z=E(_6X[1]),_70=_6Z[1],_71=E(_6Z[2]);if(!_71[0]){return [0,[0,function(_72,_){var _73=A(_70,[_72,_]),_74=E(_6S),_75=jsFind(toJSStr(_74));if(!E(_75)[0]){var _76=jsCreateElem(toJSStr(E(_c))),_77=A(_3,[_a,[0,_76],_1,_74,_]),_78=E(_72),_79=jsAppendChild(_76,_78[1]);return _78;}else{return _72;}},_9],_6Y];}else{var _7a=A(_6P,[_71[1],_6Y,_]),_7b=E(_7a),_7c=E(_7b[1]),_7d=_7c[1];return [0,[0,function(_7e,_){var _7f=A(_70,[_7e,_]),_7g=E(_6S),_7h=jsFind(toJSStr(_7g));if(!E(_7h)[0]){var _7i=jsCreateElem(toJSStr(E(_c))),_7j=A(_3,[_a,[0,_7i],_1,_7g,_]),_7k=E(_7e),_7l=jsAppendChild(_7i,_7k[1]),_7m=A(_7d,[_7k,_]);return _7k;}else{var _7n=A(_7d,[_7e,_]);return _7e;}},_7c[2]],_7b[2]];}},_7o=function(_7p,_7q,_7r,_){var _7s=A(_7p,[_7r,_]),_7t=E(_7s),_7u=A(_7q,[_7t[2],_]),_7v=E(_7u),_7w=E(_7v[1]);return [0,[0,function(_7x,_){var _7y=A(E(_7t[1])[1],[_7x,_]),_7z=A(_7w[1],[_7x,_]);return _7x;},_7w[2]],_7v[2]];},_7A=function(_7B,_7C,_){var _7D=jsWriteHandle(E(_7B)[1],toJSStr(E(_7C)));return _2;},_7E=[0,10],_7F=[1,_7E,_s],_7G=function(_7H,_7I,_){var _7J=E(_7H),_7K=jsWriteHandle(_7J[1],toJSStr(E(_7I)));return _7A(_7J,_7F,_);},_7L=[0,34],_7M=unCStr("reexec"),_7N=[1,_7L,_s],_7O=unCStr("Prelude.(!!): negative index\n"),_7P=new T(function(){return err(_7O);}),_7Q=unCStr("Prelude.(!!): index too large\n"),_7R=new T(function(){return err(_7Q);}),_7S=function(_7T,_7U){while(1){var _7V=E(_7T);if(!_7V[0]){return E(_7R);}else{var _7W=E(_7U);if(!_7W){return E(_7V[1]);}else{_7T=_7V[2];_7U=_7W-1|0;continue;}}}},_7X=unCStr("ACK"),_7Y=unCStr("BEL"),_7Z=unCStr("BS"),_80=unCStr("SP"),_81=[1,_80,_s],_82=unCStr("US"),_83=[1,_82,_81],_84=unCStr("RS"),_85=[1,_84,_83],_86=unCStr("GS"),_87=[1,_86,_85],_88=unCStr("FS"),_89=[1,_88,_87],_8a=unCStr("ESC"),_8b=[1,_8a,_89],_8c=unCStr("SUB"),_8d=[1,_8c,_8b],_8e=unCStr("EM"),_8f=[1,_8e,_8d],_8g=unCStr("CAN"),_8h=[1,_8g,_8f],_8i=unCStr("ETB"),_8j=[1,_8i,_8h],_8k=unCStr("SYN"),_8l=[1,_8k,_8j],_8m=unCStr("NAK"),_8n=[1,_8m,_8l],_8o=unCStr("DC4"),_8p=[1,_8o,_8n],_8q=unCStr("DC3"),_8r=[1,_8q,_8p],_8s=unCStr("DC2"),_8t=[1,_8s,_8r],_8u=unCStr("DC1"),_8v=[1,_8u,_8t],_8w=unCStr("DLE"),_8x=[1,_8w,_8v],_8y=unCStr("SI"),_8z=[1,_8y,_8x],_8A=unCStr("SO"),_8B=[1,_8A,_8z],_8C=unCStr("CR"),_8D=[1,_8C,_8B],_8E=unCStr("FF"),_8F=[1,_8E,_8D],_8G=unCStr("VT"),_8H=[1,_8G,_8F],_8I=unCStr("LF"),_8J=[1,_8I,_8H],_8K=unCStr("HT"),_8L=[1,_8K,_8J],_8M=[1,_7Z,_8L],_8N=[1,_7Y,_8M],_8O=[1,_7X,_8N],_8P=unCStr("ENQ"),_8Q=[1,_8P,_8O],_8R=unCStr("EOT"),_8S=[1,_8R,_8Q],_8T=unCStr("ETX"),_8U=[1,_8T,_8S],_8V=unCStr("STX"),_8W=[1,_8V,_8U],_8X=unCStr("SOH"),_8Y=[1,_8X,_8W],_8Z=unCStr("NUL"),_90=[1,_8Z,_8Y],_91=[0,92],_92=unCStr("\\DEL"),_93=unCStr("\\a"),_94=unCStr("\\\\"),_95=unCStr("\\SO"),_96=unCStr("\\r"),_97=unCStr("\\f"),_98=unCStr("\\v"),_99=unCStr("\\n"),_9a=unCStr("\\t"),_9b=unCStr("\\b"),_9c=function(_9d,_9e){if(_9d<=127){var _9f=E(_9d);switch(_9f){case 92:return _Q(_94,_9e);case 127:return _Q(_92,_9e);default:if(_9f<32){var _9g=E(_9f);switch(_9g){case 7:return _Q(_93,_9e);case 8:return _Q(_9b,_9e);case 9:return _Q(_9a,_9e);case 10:return _Q(_99,_9e);case 11:return _Q(_98,_9e);case 12:return _Q(_97,_9e);case 13:return _Q(_96,_9e);case 14:return _Q(_95,new T(function(){var _9h=E(_9e);return _9h[0]==0?[0]:E(E(_9h[1])[1])==72?unAppCStr("\\&",_9h):E(_9h);}));default:return _Q([1,_91,new T(function(){var _9i=_9g;return _9i>=0?_7S(_90,_9i):E(_7P);})],_9e);}}else{return [1,[0,_9f],_9e];}}}else{return [1,_91,new T(function(){var _9j=jsShowI(_9d);return _Q(fromJSStr(_9j),new T(function(){var _9k=E(_9e);if(!_9k[0]){return [0];}else{var _9l=E(_9k[1])[1];return _9l<48?E(_9k):_9l>57?E(_9k):unAppCStr("\\&",_9k);}}));})];}},_9m=unCStr("\\\""),_9n=function(_9o,_9p){var _9q=E(_9o);if(!_9q[0]){return E(_9p);}else{var _9r=_9q[2],_9s=E(E(_9q[1])[1]);return _9s==34?_Q(_9m,new T(function(){return _9n(_9r,_9p);})):_9c(_9s,new T(function(){return _9n(_9r,_9p);}));}},_9t=new T(function(){return _9n(_7M,_7N);}),_9u=[1,_7L,_9t],_9v=function(_){var _=0,_9w=jsMkStdout();return [0,_9w];},_9x=new T(function(){return _3h(_9v);}),_9y=function(_9z,_){var _9A=_7G(_9x,_9u,_);return [0,[0,_27,[1,_9A]],_9z];},_9B=[8,coercionToken],_9C=unCStr("true"),_9D=unCStr("hasevent"),_9E=function(_9F,_9G){while(1){var _9H=E(_9F);if(!_9H[0]){return E(_9G)[0]==0?true:false;}else{var _9I=E(_9G);if(!_9I[0]){return false;}else{if(E(_9H[1])[1]!=E(_9I[1])[1]){return false;}else{_9F=_9H[2];_9G=_9I[2];continue;}}}}},_9J=new T(function(){return [0,"keydown"];}),_9K=new T(function(){return [0,"mousemove"];}),_9L=new T(function(){return [0,"blur"];}),_9M=new T(function(){return [0,"focus"];}),_9N=new T(function(){return [0,"change"];}),_9O=new T(function(){return [0,"unload"];}),_9P=new T(function(){return [0,"load"];}),_9Q=new T(function(){return [0,"keyup"];}),_9R=new T(function(){return [0,"keypress"];}),_9S=new T(function(){return [0,"mouseup"];}),_9T=new T(function(){return [0,"mousedown"];}),_9U=new T(function(){return [0,"dblclick"];}),_9V=new T(function(){return [0,"click"];}),_9W=new T(function(){return [0,"mouseout"];}),_9X=new T(function(){return [0,"mouseover"];}),_9Y=function(_9Z){switch(E(_9Z)[0]){case 0:return E(_9P);case 1:return E(_9O);case 2:return E(_9N);case 3:return E(_9M);case 4:return E(_9L);case 5:return E(_9K);case 6:return E(_9X);case 7:return E(_9W);case 8:return E(_9V);case 9:return E(_9U);case 10:return E(_9T);case 11:return E(_9S);case 12:return E(_9R);case 13:return E(_9Q);default:return E(_9J);}},_a0=function(_a1,_a2,_a3,_a4,_){var _a5=A(_a1,[_a4,_]),_a6=E(_a5),_a7=_a6[1],_a8=E(_9D),_a9=jsGetAttr(_a7,toJSStr(_a8));if(!_9E(fromJSStr(_a9),_9C)){var _aa=E(_a3),_ab=jsSetCB(_a7,_9Y(_a2)[1],_a3),_ac=A(_3,[_a,_a6,_a8,_9C,_]);return _a6;}else{return _a6;}},_ad=unCStr("br"),_ae=function(_af,_){var _ag=jsCreateElem(toJSStr(E(_ad))),_ah=jsAppendChild(_ag,E(_af)[1]);return [0,_ag];},_ai=function(_aj,_ak,_){var _al=jsCreateElem(toJSStr(E(_aj))),_am=jsAppendChild(_al,E(_ak)[1]);return [0,_al];},_an=function(_ao,_ap,_aq,_){var _ar=_ai(_ao,_aq,_),_as=A(_ap,[_ar,_]);return _ar;},_at=unCStr("()"),_au=unCStr("GHC.Tuple"),_av=unCStr("ghc-prim"),_aw=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_av,_au,_at],_ax=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_aw,_s],_ay=function(_az){return E(_ax);},_aA=unCStr("main"),_aB=unCStr("Builder"),_aC=unCStr("JSBuilderM"),_aD=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_aA,_aB,_aC],_aE=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_aD,_s],_aF=function(_aG){return E(_aE);},_aH=function(_aI){var _aJ=E(_aI);return _aJ[0]==0?[0]:_Q(_aJ[1],new T(function(){return _aH(_aJ[2]);}));},_aK=function(_aL,_aM){var _aN=E(_aL);if(!_aN){return [0,_s,_aM];}else{var _aO=E(_aM);if(!_aO[0]){return [0,_s,_s];}else{var _aP=new T(function(){var _aQ=_aK(_aN-1|0,_aO[2]);return [0,_aQ[1],_aQ[2]];});return [0,[1,_aO[1],new T(function(){return E(E(_aP)[1]);})],new T(function(){return E(E(_aP)[2]);})];}}},_aR=[0,120],_aS=[0,48],_aT=function(_aU){var _aV=new T(function(){var _aW=_aK(8,new T(function(){var _aX=md5(toJSStr(E(_aU)));return fromJSStr(_aX);}));return [0,_aW[1],_aW[2]];}),_aY=parseInt([0,toJSStr([1,_aS,[1,_aR,new T(function(){return E(E(_aV)[1]);})]])]),_aZ=new T(function(){var _b0=_aK(8,new T(function(){return E(E(_aV)[2]);}));return [0,_b0[1],_b0[2]];}),_b1=parseInt([0,toJSStr([1,_aS,[1,_aR,new T(function(){return E(E(_aZ)[1]);})]])]),_b2=hs_mkWord64(_aY,_b1),_b3=parseInt([0,toJSStr([1,_aS,[1,_aR,new T(function(){return E(_aK(8,new T(function(){return E(E(_aZ)[2]);}))[1]);})]])]),_b4=hs_mkWord64(_b3,_b3);return [0,_b2,_b4];},_b5=function(_b6,_b7){var _b8=E(_b7);return _b8[0]==0?[0]:[1,new T(function(){return A(_b6,[_b8[1]]);}),new T(function(){return _b5(_b6,_b8[2]);})];},_b9=function(_ba,_bb){var _bc=jsShowI(_ba),_bd=md5(_bc);return _Q(fromJSStr(_bd),new T(function(){var _be=jsShowI(_bb),_bf=md5(_be);return fromJSStr(_bf);}));},_bg=function(_bh){var _bi=E(_bh);return _b9(_bi[1],_bi[2]);},_bj=function(_bk){var _bl=E(_bk);if(!_bl[0]){return [0];}else{var _bm=E(_bl[1]);return [1,[0,_bm[1],_bm[2]],new T(function(){return _bj(_bl[2]);})];}},_bn=unCStr("Prelude.undefined"),_bo=new T(function(){return err(_bn);}),_bp=function(_bq,_br){return function(_bs){return E(new T(function(){var _bt=A(_bq,[_bo]),_bu=E(_bt[3]),_bv=_bu[1],_bw=_bu[2],_bx=_Q(_bt[4],[1,new T(function(){return A(_br,[_bo]);}),_s]);if(!_bx[0]){return [0,_bv,_bw,_bu,_s];}else{var _by=_aT(new T(function(){return _aH(_b5(_bg,[1,[0,_bv,_bw],new T(function(){return _bj(_bx);})]));}));return [0,_by[1],_by[2],_bu,_bx];}}));};},_bz=new T(function(){return _bp(_aF,_ay);}),_bA=function(_bB,_bC,_bD,_){var _bE=E(_bC),_bF=A(_bB,[_bD,_]),_bG=A(_3,[_a,_bF,_bE[1],_bE[2],_]);return _bF;},_bH=function(_bI,_bJ){while(1){var _bK=(function(_bL,_bM){var _bN=E(_bM);if(!_bN[0]){return E(_bL);}else{_bI=function(_3u,_){return _bA(_bL,_bN[1],_3u,_);};_bJ=_bN[2];return null;}})(_bI,_bJ);if(_bK!=null){return _bK;}}},_bO=unCStr("value"),_bP=unCStr("onclick"),_bQ=unCStr("checked"),_bR=[0,_bQ,_s],_bS=[1,_bR,_s],_bT=unCStr("type"),_bU=unCStr("input"),_bV=function(_bW,_){return _ai(_bU,_bW,_);},_bX=function(_bY,_bZ,_c0,_c1,_c2){var _c3=new T(function(){var _c4=new T(function(){return _bH(_bV,[1,[0,_bT,_bZ],[1,[0,_1,_bY],[1,[0,_bO,_c0],_s]]]);});return !E(_c1)?E(_c4):_bH(_c4,_bS);}),_c5=E(_c2);return _c5[0]==0?E(_c3):_bH(_c3,[1,[0,_bP,_c5[1]],_s]);},_c6=unCStr("href"),_c7=[0,97],_c8=[1,_c7,_s],_c9=function(_ca,_){return _ai(_c8,_ca,_);},_cb=function(_cc,_cd){var _ce=new T(function(){return _bH(_c9,[1,[0,_c6,_cc],_s]);});return function(_cf,_){var _cg=A(_ce,[_cf,_]),_ch=A(_cd,[_cg,_]);return _cg;};},_ci=function(_cj,_ck,_){var _cl=jsCreateTextNode(toJSStr(E(_cj))),_cm=jsAppendChild(_cl,E(_ck)[1]);return [0,_cl];},_cn=function(_co){return _cb(_co,function(_3u,_){return _ci(_co,_3u,_);});},_cp=unCStr("option"),_cq=function(_cr,_){return _ai(_cp,_cr,_);},_cs=unCStr("selected"),_ct=[0,_cs,_s],_cu=[1,_ct,_s],_cv=function(_cw,_cx,_cy){var _cz=new T(function(){return _bH(_cq,[1,[0,_bO,_cw],_s]);}),_cA=function(_cB,_){var _cC=A(_cz,[_cB,_]),_cD=A(_cx,[_cC,_]);return _cC;};return !E(_cy)?E(_cA):_bH(_cA,_cu);},_cE=function(_cF,_cG){return _cv(_cF,function(_3u,_){return _ci(_cF,_3u,_);},_cG);},_cH=unCStr("method"),_cI=unCStr("action"),_cJ=unCStr("UTF-8"),_cK=unCStr("acceptCharset"),_cL=[0,_cK,_cJ],_cM=unCStr("form"),_cN=function(_cO,_){return _ai(_cM,_cO,_);},_cP=function(_cQ,_cR,_cS){var _cT=new T(function(){return _bH(_cN,[1,_cL,[1,[0,_cI,_cQ],[1,[0,_cH,_cR],_s]]]);});return function(_cU,_){var _cV=A(_cT,[_cU,_]),_cW=A(_cS,[_cV,_]);return _cV;};},_cX=unCStr("select"),_cY=function(_cZ,_){return _ai(_cX,_cZ,_);},_d0=function(_d1,_d2){var _d3=new T(function(){return _bH(_cY,[1,[0,_1,_d1],_s]);});return function(_d4,_){var _d5=A(_d3,[_d4,_]),_d6=A(_d2,[_d5,_]);return _d5;};},_d7=unCStr("textarea"),_d8=function(_d9,_){return _ai(_d7,_d9,_);},_da=function(_db,_dc){var _dd=new T(function(){return _bH(_d8,[1,[0,_1,_db],_s]);});return function(_de,_){var _df=A(_dd,[_de,_]),_dg=_ci(_dc,_df,_);return _df;};},_dh=unCStr("color:red"),_di=unCStr("style"),_dj=[0,_di,_dh],_dk=[1,_dj,_s],_dl=[0,98],_dm=[1,_dl,_s],_dn=function(_do){return _bH(function(_dp,_){var _dq=_ai(_dm,_dp,_),_dr=A(_do,[_dq,_]);return _dq;},_dk);},_ds=unCStr("toByteString not defined"),_dt=new T(function(){return err(_ds);}),_du=function(_dv,_dw,_){var _dx=E(_dv);if(!_dx[0]){return _dw;}else{var _dy=A(_dx[1],[_dw,_]),_dz=_du(_dx[2],_dw,_);return _dw;}},_dA=function(_dB,_dC,_dD,_){var _dE=A(_dB,[_dD,_]),_dF=A(_dC,[_dD,_]);return _dD;},_dG=[0,_27,_dA,_du],_dH=[0,_dG,_bz,_dt,_ci,_ci,_an,_dn,_cb,_cn,_bX,_da,_d0,_cv,_cE,_cP,_bH],_dI=[0,_29,_a],_dJ=function(_dK){return E(E(_dK)[1]);},_dL=function(_dM){return E(E(_dM)[1]);},_dN=function(_dO){return E(E(_dO)[2]);},_dP=function(_dQ,_dR){var _dS=new T(function(){return A(_dN,[_dQ,_dR]);}),_dT=new T(function(){return _dL(_dQ);}),_dU=new T(function(){return _2A(_dT);}),_dV=new T(function(){return _2a(_dT);});return function(_dW){return A(_dV,[_dS,function(_dX){return A(_dU,[[0,_dX,_dW]]);}]);};},_dY=function(_dZ,_e0){return A(_dZ,[function(_){return jsFind(toJSStr(E(_e0)));}]);},_e1=function(_e2){return E(E(_e2)[4]);},_e3=unCStr("GHC.Types"),_e4=unCStr("[]"),_e5=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_av,_e3,_e4],_e6=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_e5,_s],_e7=function(_e8){return E(_e6);},_e9=unCStr("Char"),_ea=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_av,_e3,_e9],_eb=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_ea,_s],_ec=function(_ed){return E(_eb);},_ee=new T(function(){return _bp(_e7,_ec);}),_ef=new T(function(){return A(_ee,[_bo]);}),_eg=new T(function(){return E(_bo);}),_eh=function(_ei){return E(E(_ei)[7]);},_ej=function(_ek){return E(E(_ek)[1]);},_el=[0,0],_em=[0,32],_en=[0,10],_eo=function(_ep){var _eq=E(_ep);if(!_eq[0]){return E(_a);}else{var _er=_eq[1],_es=E(_eq[2]);if(!_es[0]){return _et(_en,_er);}else{var _eu=new T(function(){return _eo(_es);}),_ev=new T(function(){return _et(_en,_er);});return function(_ew){return A(_ev,[[1,_em,new T(function(){return A(_eu,[_ew]);})]]);};}}},_ex=unCStr("->"),_ey=[1,_ex,_s],_ez=[1,_e3,_ey],_eA=[1,_av,_ez],_eB=[0,32],_eC=function(_eD){var _eE=E(_eD);if(!_eE[0]){return [0];}else{var _eF=_eE[1],_eG=E(_eE[2]);return _eG[0]==0?E(_eF):_Q(_eF,[1,_eB,new T(function(){return _eC(_eG);})]);}},_eH=new T(function(){return _eC(_eA);}),_eI=new T(function(){var _eJ=_aT(_eH);return [0,_eJ[1],_eJ[2],_av,_e3,_ex];}),_eK=function(_eL,_eM){var _eN=E(_eL);return _eN[0]==0?E(_eM):A(_eN[1],[new T(function(){return _eK(_eN[2],_eM);})]);},_eO=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_eP=[1,_ax,_s],_eQ=function(_eR){var _eS=E(_eR);if(!_eS[0]){return [0];}else{var _eT=E(_eS[1]);return [1,[0,_eT[1],_eT[2]],new T(function(){return _eQ(_eS[2]);})];}},_eU=new T(function(){var _eV=_Q(_s,_eP);if(!_eV[0]){return E(_e5);}else{var _eW=_aT(new T(function(){return _aH(_b5(_bg,[1,_eO,new T(function(){return _eQ(_eV);})]));}));return E(_e5);}}),_eX=[0,40],_eY=function(_eZ){return _et(_en,_eZ);},_f0=[0,8],_f1=unCStr(" -> "),_f2=[0,9],_f3=[0,93],_f4=[0,91],_f5=[0,41],_f6=[0,44],_f7=function(_eZ){return [1,_f6,_eZ];},_f8=function(_f9,_fa){var _fb=E(_fa);return _fb[0]==0?[0]:[1,_f9,[1,_fb[1],new T(function(){return _f8(_f9,_fb[2]);})]];},_et=function(_fc,_fd){var _fe=E(_fd),_ff=_fe[3],_fg=E(_fe[4]);if(!_fg[0]){return function(_fh){return _Q(E(_ff)[5],_fh);};}else{var _fi=_fg[1],_fj=new T(function(){var _fk=E(_ff)[5],_fl=new T(function(){return _eo(_fg);}),_fm=new T(function(){return E(_fc)[1]<=9?function(_fn){return _Q(_fk,[1,_em,new T(function(){return A(_fl,[_fn]);})]);}:function(_fo){return [1,_2N,new T(function(){return _Q(_fk,[1,_em,new T(function(){return A(_fl,[[1,_2M,_fo]]);})]);})];};}),_fp=E(_fk);if(!_fp[0]){return E(_fm);}else{if(E(E(_fp[1])[1])==40){var _fq=E(_fp[2]);return _fq[0]==0?E(_fm):E(E(_fq[1])[1])==44?function(_fr){return [1,_eX,new T(function(){return A(new T(function(){var _fs=_b5(_eY,_fg);if(!_fs[0]){return E(_a);}else{var _ft=new T(function(){return _f8(_f7,_fs[2]);});return function(_fu){return _eK([1,_fs[1],_ft],_fu);};}}),[[1,_f5,_fr]]);})];}:E(_fm);}else{return E(_fm);}}}),_fv=E(_fg[2]);if(!_fv[0]){var _fw=E(_ff),_fx=E(_eU),_fy=hs_eqWord64(_fw[1],_fx[1]);if(!E(_fy)){return E(_fj);}else{var _fz=hs_eqWord64(_fw[2],_fx[2]);if(!E(_fz)){return E(_fj);}else{var _fA=new T(function(){return _et(_el,_fi);});return function(_fB){return [1,_f4,new T(function(){return A(_fA,[[1,_f3,_fB]]);})];};}}}else{if(!E(_fv[2])[0]){var _fC=E(_ff),_fD=E(_eI),_fE=hs_eqWord64(_fC[1],_fD[1]);if(!E(_fE)){return E(_fj);}else{var _fF=hs_eqWord64(_fC[2],_fD[2]);if(!E(_fF)){return E(_fj);}else{var _fG=new T(function(){return _et(_f0,_fv[1]);}),_fH=new T(function(){return _et(_f2,_fi);});return E(_fc)[1]<=8?function(_fI){return A(_fH,[new T(function(){return _Q(_f1,new T(function(){return A(_fG,[_fI]);}));})]);}:function(_fJ){return [1,_2N,new T(function(){return A(_fH,[new T(function(){return _Q(_f1,new T(function(){return A(_fG,[[1,_2M,_fJ]]);}));})]);})];};}}}else{return E(_fj);}}}},_fK=function(_fL,_fM,_fN,_fO){var _fP=new T(function(){return _2A(_fL);}),_fQ=new T(function(){return _e1(_fO);}),_fR=new T(function(){return _eh(_fO);}),_fS=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_et,[_el,A(_fM,[_eg]),_s]);}));}),_fT=new T(function(){return A(_ej,[_fN,_3e]);});return function(_fU){if(!E(new T(function(){var _fV=A(_fM,[_eg]),_fW=E(_ef),_fX=hs_eqWord64(_fV[1],_fW[1]);if(!E(_fX)){return false;}else{var _fY=hs_eqWord64(_fV[2],_fW[2]);return E(_fY)==0?false:true;}}))){var _fZ=new T(function(){return A(_fP,[[1,_fU,new T(function(){return A(_fR,[new T(function(){return A(_fQ,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _Q(_fU,_fS);}));})]);})]);})]]);}),_g0=A(_fT,[_fU]);if(!_g0[0]){return E(_fZ);}else{var _g1=E(_g0[1]);return E(_g1[2])[0]==0?E(_g0[2])[0]==0?A(_fP,[[2,_g1[1]]]):E(_fZ):E(_fZ);}}else{return A(_fP,[[2,_fU]]);}};},_g2=[0],_g3=new T(function(){return [0,"value"];}),_g4=function(_g5,_g6,_g7,_g8,_g9,_ga){var _gb=E(_g5),_gc=_gb[1],_gd=new T(function(){return A(_gb[3],[_g2]);}),_ge=new T(function(){return _fK(_gb,_g7,_g8,_g9);});return A(_gc,[new T(function(){return _dY(_g6,_ga);}),function(_gf){var _gg=E(_gf);return _gg[0]==0?E(_gd):A(_gc,[new T(function(){return A(_g6,[function(_){var _gh=jsGet(E(_gg[1])[1],E(_g3)[1]);return [1,new T(function(){return fromJSStr(_gh);})];}]);}),function(_gi){var _gj=E(_gi);return _gj[0]==0?E(_gd):A(_ge,[_gj[1]]);}]);}]);},_gk=false,_gl=1,_gm=function(_gn){return E(E(_gn)[10]);},_go=function(_gp,_gq){return A(_2A,[_gp,[0,_gq,_gq]]);},_gr=function(_gs){return E(E(_gs)[2]);},_gt=function(_gu,_gv,_gw){return A(_2A,[_gu,[0,_2,_gv]]);},_gx=function(_gy){return E(E(_gy)[2]);},_gz=function(_gA,_gB,_gC,_gD,_gE){var _gF=new T(function(){return _dJ(_gA);}),_gG=new T(function(){return _gr(_gF);}),_gH=new T(function(){return _dL(_gB);}),_gI=new T(function(){return _2C(_gH);}),_gJ=new T(function(){return _2U([0,coercionToken],_gI,function(_gK){return _go(_gH,_gK);},function(_gL,_gM){return _gt(_gH,_gL,_gM);});}),_gN=new T(function(){return _2A(_gH);}),_gO=new T(function(){return _2a(_gH);}),_gP=new T(function(){return _2A(_gH);}),_gQ=new T(function(){return _2a(_gH);}),_gR=new T(function(){return _2A(_gH);}),_gS=new T(function(){return _2a(_gH);}),_gT=new T(function(){return _2A(_gH);}),_gU=new T(function(){return _2a(_gH);}),_gV=new T(function(){return _gx(_gD);}),_gW=new T(function(){return _gm(_gA);});return function(_gX,_gY,_gZ){return function(_h0){return A(_gU,[new T(function(){var _h1=E(_gX);return _h1[0]==0?A(_gJ,[_h0]):A(_gT,[[0,_h1[1],_h0]]);}),function(_h2){var _h3=new T(function(){return E(E(_h2)[1]);}),_h4=new T(function(){return _g4(_gI,function(_h5){return _dP(_gB,_h5);},_gC,_gE,_gA,_h3);}),_h6=new T(function(){return A(_gW,[_h3,_gY,new T(function(){var _h7=E(_gZ);if(!_h7[0]){return [0];}else{var _h8=_h7[1],_h9=_A(_gC,_ee,_h8);return _h9[0]==0?A(_gV,[_h8]):E(_h9[1]);}}),_gk,_9]);});return A(_gS,[new T(function(){var _ha=new T(function(){return E(E(_h2)[2]);});return A(_gR,[[0,_ha,_ha]]);}),function(_hb){return A(_gQ,[new T(function(){return A(_gP,[[0,_2,new T(function(){var _hc=E(E(_hb)[1]);return [0,_hc[1],_hc[2],_gl,_hc[4]];})]]);}),function(_hd){return A(_gO,[new T(function(){return A(_h4,[new T(function(){return E(E(_hd)[2]);})]);}),function(_he){var _hf=E(_he),_hg=_hf[2],_hh=E(_hf[1]);switch(_hh[0]){case 0:return A(_gN,[[0,[0,_h6,_9],_hg]]);case 1:return A(_gN,[[0,[0,new T(function(){return A(_gG,[new T(function(){return A(_gW,[_h3,_gY,_hh[1],_gk,_9]);}),_hh[2]]);}),_9],_hg]]);default:var _hi=_hh[1];return A(_gN,[[0,[0,new T(function(){return A(_gW,[_h3,_gY,new T(function(){var _hj=_A(_gC,_ee,_hi);return _hj[0]==0?A(_gV,[_hi]):E(_hj[1]);}),_gk,_9]);}),[1,_hi]],_hg]]);}}]);}]);}]);}]);};};},_hk=unCStr("base"),_hl=unCStr("Control.Exception.Base"),_hm=unCStr("PatternMatchFail"),_hn=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_hk,_hl,_hm],_ho=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_hn,_s],_hp=function(_hq){return E(_ho);},_hr=function(_hs){var _ht=E(_hs);return _A(_w(_ht[1]),_hp,_ht[2]);},_hu=function(_hv){return E(E(_hv)[1]);},_hw=function(_hx,_hy){return _Q(E(_hx)[1],_hy);},_hz=function(_hA,_hB){return _1G(_hw,_hA,_hB);},_hC=function(_hD,_hE,_hF){return _Q(E(_hE)[1],_hF);},_hG=[0,_hC,_hu,_hz],_hH=new T(function(){return [0,_hp,_hG,_hI,_hr];}),_hI=function(_hJ){return [0,_hH,_hJ];},_hK=unCStr("Non-exhaustive patterns in"),_hL=function(_hM,_hN){return die(new T(function(){return A(_hN,[_hM]);}));},_hO=function(_hP,_hQ){var _hR=E(_hQ);if(!_hR[0]){return [0,_s,_s];}else{var _hS=_hR[1];if(!A(_hP,[_hS])){return [0,_s,_hR];}else{var _hT=new T(function(){var _hU=_hO(_hP,_hR[2]);return [0,_hU[1],_hU[2]];});return [0,[1,_hS,new T(function(){return E(E(_hT)[1]);})],new T(function(){return E(E(_hT)[2]);})];}}},_hV=[0,32],_hW=[0,10],_hX=[1,_hW,_s],_hY=function(_hZ){return E(E(_hZ)[1])==124?false:true;},_i0=function(_i1,_i2){var _i3=_hO(_hY,unCStr(_i1)),_i4=_i3[1],_i5=function(_i6,_i7){return _Q(_i6,new T(function(){return unAppCStr(": ",new T(function(){return _Q(_i2,new T(function(){return _Q(_i7,_hX);}));}));}));},_i8=E(_i3[2]);return _i8[0]==0?_i5(_i4,_s):E(E(_i8[1])[1])==124?_i5(_i4,[1,_hV,_i8[2]]):_i5(_i4,_s);},_i9=function(_ia){return _hL([0,new T(function(){return _i0(_ia,_hK);})],_hI);},_ib=new T(function(){return _i9("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_ic=function(_id,_ie){while(1){var _if=(function(_ig,_ih){var _ii=E(_ig);switch(_ii[0]){case 0:var _ij=E(_ih);if(!_ij[0]){return [0];}else{_id=A(_ii[1],[_ij[1]]);_ie=_ij[2];return null;}break;case 1:var _ik=A(_ii[1],[_ih]),_il=_ih;_id=_ik;_ie=_il;return null;case 2:return [0];case 3:return [1,[0,_ii[1],_ih],new T(function(){return _ic(_ii[2],_ih);})];default:return E(_ii[1]);}})(_id,_ie);if(_if!=null){return _if;}}},_im=function(_in,_io){var _ip=new T(function(){var _iq=E(_io);if(_iq[0]==3){return [3,_iq[1],new T(function(){return _im(_in,_iq[2]);})];}else{var _ir=E(_in);if(_ir[0]==2){return E(_iq);}else{var _is=E(_iq);if(_is[0]==2){return E(_ir);}else{var _it=new T(function(){var _iu=E(_is);if(_iu[0]==4){return [1,function(_iv){return [4,new T(function(){return _Q(_ic(_ir,_iv),_iu[1]);})];}];}else{var _iw=E(_ir);if(_iw[0]==1){var _ix=_iw[1],_iy=E(_iu);return _iy[0]==0?[1,function(_iz){return _im(A(_ix,[_iz]),_iy);}]:[1,function(_iA){return _im(A(_ix,[_iA]),new T(function(){return A(_iy[1],[_iA]);}));}];}else{var _iB=E(_iu);return _iB[0]==0?E(_ib):[1,function(_iC){return _im(_iw,new T(function(){return A(_iB[1],[_iC]);}));}];}}}),_iD=E(_ir);switch(_iD[0]){case 1:var _iE=E(_is);return _iE[0]==4?[1,function(_iF){return [4,new T(function(){return _Q(_ic(A(_iD[1],[_iF]),_iF),_iE[1]);})];}]:E(_it);case 4:var _iG=_iD[1],_iH=E(_is);switch(_iH[0]){case 0:return [1,function(_iI){return [4,new T(function(){return _Q(_iG,new T(function(){return _ic(_iH,_iI);}));})];}];case 1:return [1,function(_iJ){return [4,new T(function(){return _Q(_iG,new T(function(){return _ic(A(_iH[1],[_iJ]),_iJ);}));})];}];default:return [4,new T(function(){return _Q(_iG,_iH[1]);})];}break;default:return E(_it);}}}}}),_iK=E(_in);switch(_iK[0]){case 0:var _iL=E(_io);return _iL[0]==0?[0,function(_iM){return _im(A(_iK[1],[_iM]),new T(function(){return A(_iL[1],[_iM]);}));}]:E(_ip);case 3:return [3,_iK[1],new T(function(){return _im(_iK[2],_io);})];default:return E(_ip);}},_iN=function(_iO,_iP){return E(_iO)[1]!=E(_iP)[1];},_iQ=function(_iR,_iS){return E(_iR)[1]==E(_iS)[1];},_iT=[0,_iQ,_iN],_iU=function(_iV){return E(E(_iV)[1]);},_iW=function(_iX,_iY,_iZ){while(1){var _j0=E(_iY);if(!_j0[0]){return E(_iZ)[0]==0?true:false;}else{var _j1=E(_iZ);if(!_j1[0]){return false;}else{if(!A(_iU,[_iX,_j0[1],_j1[1]])){return false;}else{_iY=_j0[2];_iZ=_j1[2];continue;}}}}},_j2=function(_j3,_j4,_j5){return !_iW(_j3,_j4,_j5)?true:false;},_j6=function(_j7){return [0,function(_j8,_j9){return _iW(_j7,_j8,_j9);},function(_j8,_j9){return _j2(_j7,_j8,_j9);}];},_ja=new T(function(){return _j6(_iT);}),_jb=function(_jc,_jd){var _je=E(_jc);switch(_je[0]){case 0:return [0,function(_jf){return _jb(A(_je[1],[_jf]),_jd);}];case 1:return [1,function(_jg){return _jb(A(_je[1],[_jg]),_jd);}];case 2:return [2];case 3:return _im(A(_jd,[_je[1]]),new T(function(){return _jb(_je[2],_jd);}));default:var _jh=function(_ji){var _jj=E(_ji);if(!_jj[0]){return [0];}else{var _jk=E(_jj[1]);return _Q(_ic(A(_jd,[_jk[1]]),_jk[2]),new T(function(){return _jh(_jj[2]);}));}},_jl=_jh(_je[1]);return _jl[0]==0?[2]:[4,_jl];}},_jm=[2],_jn=function(_jo){return [3,_jo,_jm];},_jp=function(_jq,_jr){var _js=E(_jq);if(!_js){return A(_jr,[_2]);}else{var _jt=new T(function(){return _jp(_js-1|0,_jr);});return [0,function(_ju){return E(_jt);}];}},_jv=function(_jw,_jx,_jy){var _jz=new T(function(){return A(_jw,[_jn]);});return [1,function(_jA){return A(function(_jB,_jC,_jD){while(1){var _jE=(function(_jF,_jG,_jH){var _jI=E(_jF);switch(_jI[0]){case 0:var _jJ=E(_jG);if(!_jJ[0]){return E(_jx);}else{_jB=A(_jI[1],[_jJ[1]]);_jC=_jJ[2];var _jK=_jH+1|0;_jD=_jK;return null;}break;case 1:var _jL=A(_jI[1],[_jG]),_jM=_jG,_jK=_jH;_jB=_jL;_jC=_jM;_jD=_jK;return null;case 2:return E(_jx);case 3:return function(_jN){var _jO=new T(function(){return _jb(_jI,_jN);});return _jp(_jH,function(_jP){return E(_jO);});};default:return function(_fu){return _jb(_jI,_fu);};}})(_jB,_jC,_jD);if(_jE!=null){return _jE;}}},[_jz,_jA,0,_jy]);}];},_jQ=[6],_jR=unCStr("valDig: Bad base"),_jS=new T(function(){return err(_jR);}),_jT=function(_jU,_jV){var _jW=function(_jX,_jY){var _jZ=E(_jX);if(!_jZ[0]){var _k0=new T(function(){return A(_jY,[_s]);});return function(_k1){return A(_k1,[_k0]);};}else{var _k2=E(_jZ[1])[1],_k3=function(_k4){var _k5=new T(function(){return _jW(_jZ[2],function(_k6){return A(_jY,[[1,_k4,_k6]]);});});return function(_k7){var _k8=new T(function(){return A(_k5,[_k7]);});return [0,function(_k9){return E(_k8);}];};};switch(E(E(_jU)[1])){case 8:if(48>_k2){var _ka=new T(function(){return A(_jY,[_s]);});return function(_kb){return A(_kb,[_ka]);};}else{if(_k2>55){var _kc=new T(function(){return A(_jY,[_s]);});return function(_kd){return A(_kd,[_kc]);};}else{return _k3([0,_k2-48|0]);}}break;case 10:if(48>_k2){var _ke=new T(function(){return A(_jY,[_s]);});return function(_kf){return A(_kf,[_ke]);};}else{if(_k2>57){var _kg=new T(function(){return A(_jY,[_s]);});return function(_kh){return A(_kh,[_kg]);};}else{return _k3([0,_k2-48|0]);}}break;case 16:var _ki=new T(function(){return 97>_k2?65>_k2?[0]:_k2>70?[0]:[1,[0,(_k2-65|0)+10|0]]:_k2>102?65>_k2?[0]:_k2>70?[0]:[1,[0,(_k2-65|0)+10|0]]:[1,[0,(_k2-97|0)+10|0]];});if(48>_k2){var _kj=E(_ki);if(!_kj[0]){var _kk=new T(function(){return A(_jY,[_s]);});return function(_kl){return A(_kl,[_kk]);};}else{return _k3(_kj[1]);}}else{if(_k2>57){var _km=E(_ki);if(!_km[0]){var _kn=new T(function(){return A(_jY,[_s]);});return function(_ko){return A(_ko,[_kn]);};}else{return _k3(_km[1]);}}else{return _k3([0,_k2-48|0]);}}break;default:return E(_jS);}}};return [1,function(_kp){return A(_jW,[_kp,_a,function(_kq){var _kr=E(_kq);return _kr[0]==0?[2]:A(_jV,[_kr]);}]);}];},_ks=[0,10],_kt=[0,1],_ku=[0,2147483647],_kv=function(_kw,_kx){while(1){var _ky=E(_kw);if(!_ky[0]){var _kz=_ky[1],_kA=E(_kx);if(!_kA[0]){var _kB=_kA[1],_kC=addC(_kz,_kB);if(!E(_kC[2])){return [0,_kC[1]];}else{_kw=[1,I_fromInt(_kz)];_kx=[1,I_fromInt(_kB)];continue;}}else{_kw=[1,I_fromInt(_kz)];_kx=_kA;continue;}}else{var _kD=E(_kx);if(!_kD[0]){_kw=_ky;_kx=[1,I_fromInt(_kD[1])];continue;}else{return [1,I_add(_ky[1],_kD[1])];}}}},_kE=new T(function(){return _kv(_ku,_kt);}),_kF=function(_kG){var _kH=E(_kG);if(!_kH[0]){var _kI=E(_kH[1]);return _kI==(-2147483648)?E(_kE):[0, -_kI];}else{return [1,I_negate(_kH[1])];}},_kJ=[0,10],_kK=[0,0],_kL=function(_kM,_kN){while(1){var _kO=E(_kM);if(!_kO[0]){var _kP=_kO[1],_kQ=E(_kN);if(!_kQ[0]){var _kR=_kQ[1];if(!(imul(_kP,_kR)|0)){return [0,imul(_kP,_kR)|0];}else{_kM=[1,I_fromInt(_kP)];_kN=[1,I_fromInt(_kR)];continue;}}else{_kM=[1,I_fromInt(_kP)];_kN=_kQ;continue;}}else{var _kS=E(_kN);if(!_kS[0]){_kM=_kO;_kN=[1,I_fromInt(_kS[1])];continue;}else{return [1,I_mul(_kO[1],_kS[1])];}}}},_kT=function(_kU,_kV,_kW){while(1){var _kX=E(_kW);if(!_kX[0]){return E(_kV);}else{var _kY=_kv(_kL(_kV,_kU),_kX[1]);_kW=_kX[2];_kV=_kY;continue;}}},_kZ=function(_l0){var _l1=new T(function(){return _im(_im([0,function(_l2){return E(E(_l2)[1])==45?_jT(_ks,function(_l3){return A(_l0,[[1,new T(function(){return _kF(_kT(_kJ,_kK,_l3));})]]);}):[2];}],[0,function(_l4){return E(E(_l4)[1])==43?_jT(_ks,function(_l5){return A(_l0,[[1,new T(function(){return _kT(_kJ,_kK,_l5);})]]);}):[2];}]),new T(function(){return _jT(_ks,function(_l6){return A(_l0,[[1,new T(function(){return _kT(_kJ,_kK,_l6);})]]);});}));});return _im([0,function(_l7){return E(E(_l7)[1])==101?E(_l1):[2];}],[0,function(_l8){return E(E(_l8)[1])==69?E(_l1):[2];}]);},_l9=function(_la){return A(_la,[_9]);},_lb=function(_lc){return A(_lc,[_9]);},_ld=function(_le){var _lf=new T(function(){return _jT(_ks,function(_lg){return A(_le,[[1,_lg]]);});});return [0,function(_lh){return E(E(_lh)[1])==46?E(_lf):[2];}];},_li=function(_lj){return _jT(_ks,function(_lk){return _jv(_ld,_l9,function(_ll){return _jv(_kZ,_lb,function(_lm){return A(_lj,[[5,[1,_lk,_ll,_lm]]]);});});});},_ln=function(_lo,_lp,_lq){while(1){var _lr=E(_lq);if(!_lr[0]){return false;}else{if(!A(_iU,[_lo,_lp,_lr[1]])){_lq=_lr[2];continue;}else{return true;}}}},_ls=unCStr("!@#$%&*+./<=>?\\^|:-~"),_lt=function(_lu){return _ln(_iT,_lu,_ls);},_lv=[0,8],_lw=[0,16],_lx=function(_ly){var _lz=new T(function(){return _jT(_lw,function(_lA){return A(_ly,[[5,[0,_lw,_lA]]]);});}),_lB=new T(function(){return _jT(_lv,function(_lC){return A(_ly,[[5,[0,_lv,_lC]]]);});}),_lD=new T(function(){return _jT(_lw,function(_lE){return A(_ly,[[5,[0,_lw,_lE]]]);});}),_lF=new T(function(){return _jT(_lv,function(_lG){return A(_ly,[[5,[0,_lv,_lG]]]);});});return [0,function(_lH){return E(E(_lH)[1])==48?E([0,function(_lI){switch(E(E(_lI)[1])){case 79:return E(_lF);case 88:return E(_lD);case 111:return E(_lB);case 120:return E(_lz);default:return [2];}}]):[2];}];},_lJ=true,_lK=function(_lL){var _lM=new T(function(){return A(_lL,[_lw]);}),_lN=new T(function(){return A(_lL,[_lv]);}),_lO=new T(function(){return A(_lL,[_lw]);}),_lP=new T(function(){return A(_lL,[_lv]);});return [0,function(_lQ){switch(E(E(_lQ)[1])){case 79:return E(_lP);case 88:return E(_lO);case 111:return E(_lN);case 120:return E(_lM);default:return [2];}}];},_lR=function(_lS){return A(_lS,[_ks]);},_lT=function(_lU){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _2O(9,_lU,_s);})));},_lV=function(_lW){var _lX=E(_lW);return _lX[0]==0?E(_lX[1]):I_toInt(_lX[1]);},_lY=function(_lZ,_m0){var _m1=E(_lZ);if(!_m1[0]){var _m2=_m1[1],_m3=E(_m0);return _m3[0]==0?_m2<=_m3[1]:I_compareInt(_m3[1],_m2)>=0;}else{var _m4=_m1[1],_m5=E(_m0);return _m5[0]==0?I_compareInt(_m4,_m5[1])<=0:I_compare(_m4,_m5[1])<=0;}},_m6=function(_m7){return [2];},_m8=function(_m9){var _ma=E(_m9);if(!_ma[0]){return E(_m6);}else{var _mb=_ma[1],_mc=E(_ma[2]);if(!_mc[0]){return E(_mb);}else{var _md=new T(function(){return _m8(_mc);});return function(_me){return _im(A(_mb,[_me]),new T(function(){return A(_md,[_me]);}));};}}},_mf=unCStr("NUL"),_mg=function(_mh){return [2];},_mi=function(_mj){return _mg(_mj);},_mk=function(_ml,_mm){var _mn=function(_mo,_mp){var _mq=E(_mo);if(!_mq[0]){return function(_mr){return A(_mr,[_ml]);};}else{var _ms=E(_mp);if(!_ms[0]){return E(_mg);}else{if(E(_mq[1])[1]!=E(_ms[1])[1]){return E(_mi);}else{var _mt=new T(function(){return _mn(_mq[2],_ms[2]);});return function(_mu){var _mv=new T(function(){return A(_mt,[_mu]);});return [0,function(_mw){return E(_mv);}];};}}}};return [1,function(_mx){return A(_mn,[_ml,_mx,_mm]);}];},_my=[0,0],_mz=function(_mA){var _mB=new T(function(){return A(_mA,[_my]);});return _mk(_mf,function(_mC){return E(_mB);});},_mD=unCStr("STX"),_mE=[0,2],_mF=function(_mG){var _mH=new T(function(){return A(_mG,[_mE]);});return _mk(_mD,function(_mI){return E(_mH);});},_mJ=unCStr("ETX"),_mK=[0,3],_mL=function(_mM){var _mN=new T(function(){return A(_mM,[_mK]);});return _mk(_mJ,function(_mO){return E(_mN);});},_mP=unCStr("EOT"),_mQ=[0,4],_mR=function(_mS){var _mT=new T(function(){return A(_mS,[_mQ]);});return _mk(_mP,function(_mU){return E(_mT);});},_mV=unCStr("ENQ"),_mW=[0,5],_mX=function(_mY){var _mZ=new T(function(){return A(_mY,[_mW]);});return _mk(_mV,function(_n0){return E(_mZ);});},_n1=unCStr("ACK"),_n2=[0,6],_n3=function(_n4){var _n5=new T(function(){return A(_n4,[_n2]);});return _mk(_n1,function(_n6){return E(_n5);});},_n7=unCStr("BEL"),_n8=[0,7],_n9=function(_na){var _nb=new T(function(){return A(_na,[_n8]);});return _mk(_n7,function(_nc){return E(_nb);});},_nd=unCStr("BS"),_ne=[0,8],_nf=function(_ng){var _nh=new T(function(){return A(_ng,[_ne]);});return _mk(_nd,function(_ni){return E(_nh);});},_nj=unCStr("HT"),_nk=[0,9],_nl=function(_nm){var _nn=new T(function(){return A(_nm,[_nk]);});return _mk(_nj,function(_no){return E(_nn);});},_np=unCStr("LF"),_nq=[0,10],_nr=function(_ns){var _nt=new T(function(){return A(_ns,[_nq]);});return _mk(_np,function(_nu){return E(_nt);});},_nv=unCStr("VT"),_nw=[0,11],_nx=function(_ny){var _nz=new T(function(){return A(_ny,[_nw]);});return _mk(_nv,function(_nA){return E(_nz);});},_nB=unCStr("FF"),_nC=[0,12],_nD=function(_nE){var _nF=new T(function(){return A(_nE,[_nC]);});return _mk(_nB,function(_nG){return E(_nF);});},_nH=unCStr("CR"),_nI=[0,13],_nJ=function(_nK){var _nL=new T(function(){return A(_nK,[_nI]);});return _mk(_nH,function(_nM){return E(_nL);});},_nN=unCStr("SI"),_nO=[0,15],_nP=function(_nQ){var _nR=new T(function(){return A(_nQ,[_nO]);});return _mk(_nN,function(_nS){return E(_nR);});},_nT=unCStr("DLE"),_nU=[0,16],_nV=function(_nW){var _nX=new T(function(){return A(_nW,[_nU]);});return _mk(_nT,function(_nY){return E(_nX);});},_nZ=unCStr("DC1"),_o0=[0,17],_o1=function(_o2){var _o3=new T(function(){return A(_o2,[_o0]);});return _mk(_nZ,function(_o4){return E(_o3);});},_o5=unCStr("DC2"),_o6=[0,18],_o7=function(_o8){var _o9=new T(function(){return A(_o8,[_o6]);});return _mk(_o5,function(_oa){return E(_o9);});},_ob=unCStr("DC3"),_oc=[0,19],_od=function(_oe){var _of=new T(function(){return A(_oe,[_oc]);});return _mk(_ob,function(_og){return E(_of);});},_oh=unCStr("DC4"),_oi=[0,20],_oj=function(_ok){var _ol=new T(function(){return A(_ok,[_oi]);});return _mk(_oh,function(_om){return E(_ol);});},_on=unCStr("NAK"),_oo=[0,21],_op=function(_oq){var _or=new T(function(){return A(_oq,[_oo]);});return _mk(_on,function(_os){return E(_or);});},_ot=unCStr("SYN"),_ou=[0,22],_ov=function(_ow){var _ox=new T(function(){return A(_ow,[_ou]);});return _mk(_ot,function(_oy){return E(_ox);});},_oz=unCStr("ETB"),_oA=[0,23],_oB=function(_oC){var _oD=new T(function(){return A(_oC,[_oA]);});return _mk(_oz,function(_oE){return E(_oD);});},_oF=unCStr("CAN"),_oG=[0,24],_oH=function(_oI){var _oJ=new T(function(){return A(_oI,[_oG]);});return _mk(_oF,function(_oK){return E(_oJ);});},_oL=unCStr("EM"),_oM=[0,25],_oN=function(_oO){var _oP=new T(function(){return A(_oO,[_oM]);});return _mk(_oL,function(_oQ){return E(_oP);});},_oR=unCStr("SUB"),_oS=[0,26],_oT=function(_oU){var _oV=new T(function(){return A(_oU,[_oS]);});return _mk(_oR,function(_oW){return E(_oV);});},_oX=unCStr("ESC"),_oY=[0,27],_oZ=function(_p0){var _p1=new T(function(){return A(_p0,[_oY]);});return _mk(_oX,function(_p2){return E(_p1);});},_p3=unCStr("FS"),_p4=[0,28],_p5=function(_p6){var _p7=new T(function(){return A(_p6,[_p4]);});return _mk(_p3,function(_p8){return E(_p7);});},_p9=unCStr("GS"),_pa=[0,29],_pb=function(_pc){var _pd=new T(function(){return A(_pc,[_pa]);});return _mk(_p9,function(_pe){return E(_pd);});},_pf=unCStr("RS"),_pg=[0,30],_ph=function(_pi){var _pj=new T(function(){return A(_pi,[_pg]);});return _mk(_pf,function(_pk){return E(_pj);});},_pl=unCStr("US"),_pm=[0,31],_pn=function(_po){var _pp=new T(function(){return A(_po,[_pm]);});return _mk(_pl,function(_pq){return E(_pp);});},_pr=unCStr("SP"),_ps=[0,32],_pt=function(_pu){var _pv=new T(function(){return A(_pu,[_ps]);});return _mk(_pr,function(_pw){return E(_pv);});},_px=unCStr("DEL"),_py=[0,127],_pz=function(_pA){var _pB=new T(function(){return A(_pA,[_py]);});return _mk(_px,function(_pC){return E(_pB);});},_pD=[1,_pz,_s],_pE=[1,_pt,_pD],_pF=[1,_pn,_pE],_pG=[1,_ph,_pF],_pH=[1,_pb,_pG],_pI=[1,_p5,_pH],_pJ=[1,_oZ,_pI],_pK=[1,_oT,_pJ],_pL=[1,_oN,_pK],_pM=[1,_oH,_pL],_pN=[1,_oB,_pM],_pO=[1,_ov,_pN],_pP=[1,_op,_pO],_pQ=[1,_oj,_pP],_pR=[1,_od,_pQ],_pS=[1,_o7,_pR],_pT=[1,_o1,_pS],_pU=[1,_nV,_pT],_pV=[1,_nP,_pU],_pW=[1,_nJ,_pV],_pX=[1,_nD,_pW],_pY=[1,_nx,_pX],_pZ=[1,_nr,_pY],_q0=[1,_nl,_pZ],_q1=[1,_nf,_q0],_q2=[1,_n9,_q1],_q3=[1,_n3,_q2],_q4=[1,_mX,_q3],_q5=[1,_mR,_q4],_q6=[1,_mL,_q5],_q7=[1,_mF,_q6],_q8=[1,_mz,_q7],_q9=unCStr("SOH"),_qa=[0,1],_qb=function(_qc){var _qd=new T(function(){return A(_qc,[_qa]);});return _mk(_q9,function(_qe){return E(_qd);});},_qf=unCStr("SO"),_qg=[0,14],_qh=function(_qi){var _qj=new T(function(){return A(_qi,[_qg]);});return _mk(_qf,function(_qk){return E(_qj);});},_ql=function(_qm){return _jv(_qb,_qh,_qm);},_qn=[1,_ql,_q8],_qo=new T(function(){return _m8(_qn);}),_qp=[0,1114111],_qq=[0,34],_qr=[0,_qq,_lJ],_qs=[0,39],_qt=[0,_qs,_lJ],_qu=[0,92],_qv=[0,_qu,_lJ],_qw=[0,_n8,_lJ],_qx=[0,_ne,_lJ],_qy=[0,_nC,_lJ],_qz=[0,_nq,_lJ],_qA=[0,_nI,_lJ],_qB=[0,_nk,_lJ],_qC=[0,_nw,_lJ],_qD=[0,_my,_lJ],_qE=[0,_qa,_lJ],_qF=[0,_mE,_lJ],_qG=[0,_mK,_lJ],_qH=[0,_mQ,_lJ],_qI=[0,_mW,_lJ],_qJ=[0,_n2,_lJ],_qK=[0,_n8,_lJ],_qL=[0,_ne,_lJ],_qM=[0,_nk,_lJ],_qN=[0,_nq,_lJ],_qO=[0,_nw,_lJ],_qP=[0,_nC,_lJ],_qQ=[0,_nI,_lJ],_qR=[0,_qg,_lJ],_qS=[0,_nO,_lJ],_qT=[0,_nU,_lJ],_qU=[0,_o0,_lJ],_qV=[0,_o6,_lJ],_qW=[0,_oc,_lJ],_qX=[0,_oi,_lJ],_qY=[0,_oo,_lJ],_qZ=[0,_ou,_lJ],_r0=[0,_oA,_lJ],_r1=[0,_oG,_lJ],_r2=[0,_oM,_lJ],_r3=[0,_oS,_lJ],_r4=[0,_oY,_lJ],_r5=[0,_p4,_lJ],_r6=[0,_pa,_lJ],_r7=[0,_pg,_lJ],_r8=[0,_pm,_lJ],_r9=function(_ra){return [0,_ra];},_rb=function(_rc){var _rd=new T(function(){return A(_rc,[_qC]);}),_re=new T(function(){return A(_rc,[_qB]);}),_rf=new T(function(){return A(_rc,[_qA]);}),_rg=new T(function(){return A(_rc,[_qz]);}),_rh=new T(function(){return A(_rc,[_qy]);}),_ri=new T(function(){return A(_rc,[_qx]);}),_rj=new T(function(){return A(_rc,[_qw]);}),_rk=new T(function(){return A(_rc,[_qv]);}),_rl=new T(function(){return A(_rc,[_qt]);}),_rm=new T(function(){return A(_rc,[_qr]);});return _im([0,function(_rn){switch(E(E(_rn)[1])){case 34:return E(_rm);case 39:return E(_rl);case 92:return E(_rk);case 97:return E(_rj);case 98:return E(_ri);case 102:return E(_rh);case 110:return E(_rg);case 114:return E(_rf);case 116:return E(_re);case 118:return E(_rd);default:return [2];}}],new T(function(){return _im(_jv(_lK,_lR,function(_ro){var _rp=new T(function(){return _r9(E(_ro)[1]);});return _jT(_ro,function(_rq){var _rr=_kT(_rp,_kK,_rq);return !_lY(_rr,_qp)?[2]:A(_rc,[[0,new T(function(){var _rs=_lV(_rr);return _rs>>>0>1114111?_lT(_rs):[0,_rs];}),_lJ]]);});}),new T(function(){var _rt=new T(function(){return A(_rc,[_r8]);}),_ru=new T(function(){return A(_rc,[_r7]);}),_rv=new T(function(){return A(_rc,[_r6]);}),_rw=new T(function(){return A(_rc,[_r5]);}),_rx=new T(function(){return A(_rc,[_r4]);}),_ry=new T(function(){return A(_rc,[_r3]);}),_rz=new T(function(){return A(_rc,[_r2]);}),_rA=new T(function(){return A(_rc,[_r1]);}),_rB=new T(function(){return A(_rc,[_r0]);}),_rC=new T(function(){return A(_rc,[_qZ]);}),_rD=new T(function(){return A(_rc,[_qY]);}),_rE=new T(function(){return A(_rc,[_qX]);}),_rF=new T(function(){return A(_rc,[_qW]);}),_rG=new T(function(){return A(_rc,[_qV]);}),_rH=new T(function(){return A(_rc,[_qU]);}),_rI=new T(function(){return A(_rc,[_qT]);}),_rJ=new T(function(){return A(_rc,[_qS]);}),_rK=new T(function(){return A(_rc,[_qR]);}),_rL=new T(function(){return A(_rc,[_qQ]);}),_rM=new T(function(){return A(_rc,[_qP]);}),_rN=new T(function(){return A(_rc,[_qO]);}),_rO=new T(function(){return A(_rc,[_qN]);}),_rP=new T(function(){return A(_rc,[_qM]);}),_rQ=new T(function(){return A(_rc,[_qL]);}),_rR=new T(function(){return A(_rc,[_qK]);}),_rS=new T(function(){return A(_rc,[_qJ]);}),_rT=new T(function(){return A(_rc,[_qI]);}),_rU=new T(function(){return A(_rc,[_qH]);}),_rV=new T(function(){return A(_rc,[_qG]);}),_rW=new T(function(){return A(_rc,[_qF]);}),_rX=new T(function(){return A(_rc,[_qE]);}),_rY=new T(function(){return A(_rc,[_qD]);});return _im([0,function(_rZ){return E(E(_rZ)[1])==94?E([0,function(_s0){switch(E(E(_s0)[1])){case 64:return E(_rY);case 65:return E(_rX);case 66:return E(_rW);case 67:return E(_rV);case 68:return E(_rU);case 69:return E(_rT);case 70:return E(_rS);case 71:return E(_rR);case 72:return E(_rQ);case 73:return E(_rP);case 74:return E(_rO);case 75:return E(_rN);case 76:return E(_rM);case 77:return E(_rL);case 78:return E(_rK);case 79:return E(_rJ);case 80:return E(_rI);case 81:return E(_rH);case 82:return E(_rG);case 83:return E(_rF);case 84:return E(_rE);case 85:return E(_rD);case 86:return E(_rC);case 87:return E(_rB);case 88:return E(_rA);case 89:return E(_rz);case 90:return E(_ry);case 91:return E(_rx);case 92:return E(_rw);case 93:return E(_rv);case 94:return E(_ru);case 95:return E(_rt);default:return [2];}}]):[2];}],new T(function(){return A(_qo,[function(_s1){return A(_rc,[[0,_s1,_lJ]]);}]);}));}));}));},_s2=function(_s3){return A(_s3,[_2]);},_s4=function(_s5){var _s6=E(_s5);if(!_s6[0]){return E(_s2);}else{var _s7=_s6[2],_s8=E(E(_s6[1])[1]);switch(_s8){case 9:var _s9=new T(function(){return _s4(_s7);});return function(_sa){var _sb=new T(function(){return A(_s9,[_sa]);});return [0,function(_sc){return E(_sb);}];};case 10:var _sd=new T(function(){return _s4(_s7);});return function(_se){var _sf=new T(function(){return A(_sd,[_se]);});return [0,function(_sg){return E(_sf);}];};case 11:var _sh=new T(function(){return _s4(_s7);});return function(_si){var _sj=new T(function(){return A(_sh,[_si]);});return [0,function(_sk){return E(_sj);}];};case 12:var _sl=new T(function(){return _s4(_s7);});return function(_sm){var _sn=new T(function(){return A(_sl,[_sm]);});return [0,function(_so){return E(_sn);}];};case 13:var _sp=new T(function(){return _s4(_s7);});return function(_sq){var _sr=new T(function(){return A(_sp,[_sq]);});return [0,function(_ss){return E(_sr);}];};case 32:var _st=new T(function(){return _s4(_s7);});return function(_su){var _sv=new T(function(){return A(_st,[_su]);});return [0,function(_sw){return E(_sv);}];};case 160:var _sx=new T(function(){return _s4(_s7);});return function(_sy){var _sz=new T(function(){return A(_sx,[_sy]);});return [0,function(_sA){return E(_sz);}];};default:var _sB=u_iswspace(_s8);if(!E(_sB)){return E(_s2);}else{var _sC=new T(function(){return _s4(_s7);});return function(_sD){var _sE=new T(function(){return A(_sC,[_sD]);});return [0,function(_sF){return E(_sE);}];};}}}},_sG=function(_sH){var _sI=new T(function(){return _rb(_sH);}),_sJ=new T(function(){return _sG(_sH);}),_sK=[1,function(_sL){return A(_s4,[_sL,function(_sM){return E([0,function(_sN){return E(E(_sN)[1])==92?E(_sJ):[2];}]);}]);}];return _im([0,function(_sO){return E(E(_sO)[1])==92?E([0,function(_sP){var _sQ=E(E(_sP)[1]);switch(_sQ){case 9:return E(_sK);case 10:return E(_sK);case 11:return E(_sK);case 12:return E(_sK);case 13:return E(_sK);case 32:return E(_sK);case 38:return E(_sJ);case 160:return E(_sK);default:var _sR=u_iswspace(_sQ);return E(_sR)==0?[2]:E(_sK);}}]):[2];}],[0,function(_sS){var _sT=E(_sS);return E(_sT[1])==92?E(_sI):A(_sH,[[0,_sT,_gk]]);}]);},_sU=function(_sV,_sW){var _sX=new T(function(){return A(_sW,[[1,new T(function(){return A(_sV,[_s]);})]]);});return _sG(function(_sY){var _sZ=E(_sY),_t0=E(_sZ[1]);return E(_t0[1])==34?!E(_sZ[2])?E(_sX):_sU(function(_t1){return A(_sV,[[1,_t0,_t1]]);},_sW):_sU(function(_t2){return A(_sV,[[1,_t0,_t2]]);},_sW);});},_t3=unCStr("_\'"),_t4=function(_t5){var _t6=u_iswalnum(_t5);return E(_t6)==0?_ln(_iT,[0,_t5],_t3):true;},_t7=function(_t8){return _t4(E(_t8)[1]);},_t9=unCStr(",;()[]{}`"),_ta=function(_tb){return A(_tb,[_s]);},_tc=function(_td,_te){var _tf=function(_tg){var _th=E(_tg);if(!_th[0]){return E(_ta);}else{var _ti=_th[1];if(!A(_td,[_ti])){return E(_ta);}else{var _tj=new T(function(){return _tf(_th[2]);});return function(_tk){var _tl=new T(function(){return A(_tj,[function(_tm){return A(_tk,[[1,_ti,_tm]]);}]);});return [0,function(_tn){return E(_tl);}];};}}};return [1,function(_to){return A(_tf,[_to,_te]);}];},_tp=unCStr(".."),_tq=unCStr("::"),_tr=unCStr("->"),_ts=[0,64],_tt=[1,_ts,_s],_tu=[0,126],_tv=[1,_tu,_s],_tw=unCStr("=>"),_tx=[1,_tw,_s],_ty=[1,_tv,_tx],_tz=[1,_tt,_ty],_tA=[1,_tr,_tz],_tB=unCStr("<-"),_tC=[1,_tB,_tA],_tD=[0,124],_tE=[1,_tD,_s],_tF=[1,_tE,_tC],_tG=[1,_qu,_s],_tH=[1,_tG,_tF],_tI=[0,61],_tJ=[1,_tI,_s],_tK=[1,_tJ,_tH],_tL=[1,_tq,_tK],_tM=[1,_tp,_tL],_tN=function(_tO){var _tP=new T(function(){return A(_tO,[_jQ]);});return _im([1,function(_tQ){return E(_tQ)[0]==0?E(_tP):[2];}],new T(function(){var _tR=new T(function(){return _rb(function(_tS){var _tT=E(_tS);return (function(_tU,_tV){var _tW=new T(function(){return A(_tO,[[0,_tU]]);});return !E(_tV)?E(E(_tU)[1])==39?[2]:[0,function(_tX){return E(E(_tX)[1])==39?E(_tW):[2];}]:[0,function(_tY){return E(E(_tY)[1])==39?E(_tW):[2];}];})(_tT[1],_tT[2]);});});return _im([0,function(_tZ){return E(E(_tZ)[1])==39?E([0,function(_u0){var _u1=E(_u0);switch(E(_u1[1])){case 39:return [2];case 92:return E(_tR);default:var _u2=new T(function(){return A(_tO,[[0,_u1]]);});return [0,function(_u3){return E(E(_u3)[1])==39?E(_u2):[2];}];}}]):[2];}],new T(function(){var _u4=new T(function(){return _sU(_a,_tO);});return _im([0,function(_u5){return E(E(_u5)[1])==34?E(_u4):[2];}],new T(function(){return _im([0,function(_u6){return !_ln(_iT,_u6,_t9)?[2]:A(_tO,[[2,[1,_u6,_s]]]);}],new T(function(){return _im([0,function(_u7){return !_ln(_iT,_u7,_ls)?[2]:_tc(_lt,function(_u8){var _u9=[1,_u7,_u8];return !_ln(_ja,_u9,_tM)?A(_tO,[[4,_u9]]):A(_tO,[[2,_u9]]);});}],new T(function(){return _im([0,function(_ua){var _ub=E(_ua),_uc=_ub[1],_ud=u_iswalpha(_uc);return E(_ud)==0?E(_uc)==95?_tc(_t7,function(_ue){return A(_tO,[[3,[1,_ub,_ue]]]);}):[2]:_tc(_t7,function(_uf){return A(_tO,[[3,[1,_ub,_uf]]]);});}],new T(function(){return _jv(_lx,_li,_tO);}));}));}));}));}));}));},_ug=function(_uh){var _ui=new T(function(){return _tN(_uh);});return [1,function(_uj){return A(_s4,[_uj,function(_uk){return E(_ui);}]);}];},_ul=[0,0],_um=function(_un,_uo){var _up=new T(function(){return A(_un,[_ul,function(_uq){var _ur=new T(function(){return A(_uo,[_uq]);});return _ug(function(_us){var _ut=E(_us);if(_ut[0]==2){var _uu=E(_ut[1]);return _uu[0]==0?[2]:E(E(_uu[1])[1])==41?E(_uu[2])[0]==0?E(_ur):[2]:[2];}else{return [2];}});}]);});return _ug(function(_uv){var _uw=E(_uv);if(_uw[0]==2){var _ux=E(_uw[1]);return _ux[0]==0?[2]:E(E(_ux[1])[1])==40?E(_ux[2])[0]==0?E(_up):[2]:[2];}else{return [2];}});},_uy=function(_uz){return _im(_ug(function(_uA){var _uB=E(_uA);return _uB[0]==0?A(_uz,[_uB[1]]):[2];}),new T(function(){return _um(_uC,_uz);}));},_uC=function(_uD,_uE){return _uy(_uE);},_uF=function(_uG,_uH){var _uI=function(_uJ,_uK){var _uL=new T(function(){return A(_uK,[_s]);}),_uM=new T(function(){return A(_uG,[_ul,function(_uN){return _uI(_lJ,function(_uO){return A(_uK,[[1,_uN,_uO]]);});}]);});return _ug(function(_uP){var _uQ=E(_uP);if(_uQ[0]==2){var _uR=E(_uQ[1]);if(!_uR[0]){return [2];}else{var _uS=_uR[2];switch(E(E(_uR[1])[1])){case 44:return E(_uS)[0]==0?!E(_uJ)?[2]:E(_uM):[2];case 93:return E(_uS)[0]==0?E(_uL):[2];default:return [2];}}}else{return [2];}});},_uT=function(_uU){var _uV=new T(function(){return _im(_uI(_gk,_uU),new T(function(){return A(_uG,[_ul,function(_uW){return _uI(_lJ,function(_uX){return A(_uU,[[1,_uW,_uX]]);});}]);}));});return _im(_ug(function(_uY){var _uZ=E(_uY);if(_uZ[0]==2){var _v0=E(_uZ[1]);return _v0[0]==0?[2]:E(E(_v0[1])[1])==91?E(_v0[2])[0]==0?E(_uV):[2]:[2];}else{return [2];}}),new T(function(){return _um(function(_v1,_v2){return _uT(_v2);},_uU);}));};return _uT(_uH);},_v3=function(_v4){return _im(_im(_ug(function(_v5){var _v6=E(_v5);return _v6[0]==1?A(_v4,[_v6[1]]):[2];}),new T(function(){return _uF(_uC,_v4);})),new T(function(){return _um(_v7,_v4);}));},_v7=function(_v8,_v9){return _v3(_v9);},_va=new T(function(){return _um(_v7,_jn);}),_vb=new T(function(){return _uF(_uC,_jn);}),_vc=function(_vd){var _ve=E(_vd);return _ve[0]==1?[3,_ve[1],_jm]:[2];},_vf=new T(function(){return _tN(_vc);}),_vg=function(_vh){return E(_vf);},_vi=function(_vj){return A(_s4,[_vj,_vg]);},_vk=[1,_vi],_vl=new T(function(){return _im(_vk,_vb);}),_vm=new T(function(){return _im(_vl,_va);}),_vn=function(_vo){return _ic(_vm,_vo);},_vp=new T(function(){return _uy(_jn);}),_vq=function(_vo){return _ic(_vp,_vo);},_vr=function(_vs){return E(_vq);},_vt=[0,_vr,_vn,_uC,_v7],_vu=function(_vv){return E(E(_vv)[4]);},_vw=function(_vx,_vy,_vz){return _uF(new T(function(){return _vu(_vx);}),_vz);},_vA=function(_vB){var _vC=new T(function(){return _uF(new T(function(){return _vu(_vB);}),_jn);});return function(_fu){return _ic(_vC,_fu);};},_vD=function(_vE,_vF){var _vG=new T(function(){return A(_vu,[_vE,_vF,_jn]);});return function(_fu){return _ic(_vG,_fu);};},_vH=function(_vI){return [0,function(_vo){return _vD(_vI,_vo);},new T(function(){return _vA(_vI);}),new T(function(){return _vu(_vI);}),function(_vJ,_vo){return _vw(_vI,_vJ,_vo);}];},_vK=new T(function(){return _vH(_vt);}),_vL=[0,39],_vM=[1,_vL,_s],_vN=unCStr("\'\\\'\'"),_vO=function(_vP){var _vQ=E(E(_vP)[1]);return _vQ==39?E(_vN):[1,_vL,new T(function(){return _9c(_vQ,_vM);})];},_vR=function(_vS,_vT){return [1,_7L,new T(function(){return _9n(_vS,[1,_7L,_vT]);})];},_vU=function(_vV){return _Q(_vN,_vV);},_vW=function(_vX,_vY){var _vZ=E(E(_vY)[1]);return _vZ==39?E(_vU):function(_w0){return [1,_vL,new T(function(){return _9c(_vZ,[1,_vL,_w0]);})];};},_w1=[0,_vW,_vO,_vR],_w2=function(_w3){return E(E(_w3)[3]);},_w4=function(_w5,_w6){return A(_w2,[_w5,_w6,_s]);},_w7=function(_w8,_w9,_wa){return _1G(new T(function(){return _w2(_w8);}),_w9,_wa);},_wb=function(_wc){var _wd=new T(function(){return _w2(_wc);});return [0,function(_we){return E(_wd);},function(_vV){return _w4(_wc,_vV);},function(_wf,_vV){return _w7(_wc,_wf,_vV);}];},_wg=new T(function(){return _wb(_w1);}),_wh=new T(function(){return _gz(_dH,_dI,_ee,_wg,_vK);}),_wi=[0,43],_wj=[1,_wi,_s],_wk=[1,_wj],_wl=unCStr("submit"),_wm=new T(function(){return A(_wh,[_9,_wl,_wk]);}),_wn=function(_wo,_){var _wp=A(_wm,[_wo,_]),_wq=E(_wp),_wr=E(_wq[1]),_ws=new T(function(){return E(E(_wo)[4]);});return [0,[0,function(_wt,_){var _wu=_a0(_wr[1],_9B,_ws,_wt,_),_wv=_ae(_wt,_);return _wt;},_wr[2]],_wq[2]];},_ww=unCStr("This widget sums three numbers and append the result using a fold"),_wx=[0,112],_wy=[1,_wx,_s],_wz=function(_wA,_wB){var _wC=new T(function(){return A(_wA,[_wB]);});return function(_wD,_){var _wE=jsCreateElem(toJSStr(_wy)),_wF=jsAppendChild(_wE,E(_wD)[1]),_wG=[0,_wE],_wH=A(_wC,[_wG,_]);return _wG;};},_wI=new T(function(){return _wz(_ci,_ww);}),_wJ=function(_wK){return _2O(0,E(_wK)[1],_s);},_wL=[0,98],_wM=[1,_wL,_s],_wN=function(_wO,_wP){var _wQ=new T(function(){return A(_wO,[_wP]);});return function(_wR,_){var _wS=jsCreateElem(toJSStr(_wM)),_wT=jsAppendChild(_wS,E(_wR)[1]),_wU=[0,_wS],_wV=A(_wQ,[_wU,_]);return _wU;};},_wW=[1,_2],_wX=unCStr("result: "),_wY=function(_wZ){var _x0=new T(function(){return _wN(_ci,new T(function(){return _wJ(_wZ);}));});return function(_x1,_){return [0,[0,function(_x2,_){var _x3=_ae(_x2,_),_x4=_ci(_wX,_x2,_),_x5=A(_x0,[_x2,_]);return _x2;},_wW],_x1];};},_x6=[0,0],_x7=[1,_x6],_x8=[0,_27,_x7],_x9=function(_xa,_){return [0,_x8,_xa];},_xb=function(_xc,_xd,_){var _xe=A(_xd,[_]);return _xc;},_xf=function(_xg,_xh,_){var _xi=A(_xh,[_]);return new T(function(){return A(_xg,[_xi]);});},_xj=[0,_xf,_xb],_xk=function(_xl){var _xm=E(_xl);return _xm[0]==0?0:E(_xm[1])[1]+_xk(_xm[2])|0;},_xn=function(_xo){return [0,_xk(_xo)];},_xp=function(_xq,_xr){return [0,E(_xq)[1]+E(_xr)[1]|0];},_xs=[0,_x6,_xp,_xn],_xt=function(_xu,_xv){var _xw=E(_xv);return _xw[0]==0?[0]:[1,new T(function(){return A(_xu,[_xw[1]]);})];},_xx=function(_xy,_xz,_xA,_xB,_xC,_xD){var _xE=new T(function(){return _gr(_xy);});return A(_xz,[new T(function(){return A(_xB,[_xD]);}),function(_xF){var _xG=E(_xF),_xH=E(_xG[1]);return A(_xz,[new T(function(){return A(_xC,[_xG[2]]);}),function(_xI){var _xJ=E(_xI),_xK=E(_xJ[1]);return A(_xA,[[0,[0,new T(function(){return A(_xE,[_xH[1],_xK[1]]);}),new T(function(){var _xL=E(_xH[2]);if(!_xL[0]){return [0];}else{var _xM=E(_xK[2]);return _xM[0]==0?[0]:[1,new T(function(){return A(_xL[1],[_xM[1]]);})];}})],_xJ[2]]]);}]);}]);},_xN=function(_xO){return E(E(_xO)[1]);},_xP=function(_xQ,_xR,_xS,_xT,_xU,_xV){var _xW=new T(function(){return _dJ(_xQ);});return function(_xX){var _xY=E(_xR);return _xx(_xW,_xY[1],_xY[3],function(_xZ){return A(new T(function(){var _y0=new T(function(){return _gr(_xT);});return A(_xN,[_xS,function(_y1){return [0,new T(function(){var _y2=E(E(_y1)[1]);return [0,_y2[1],new T(function(){return _xt(_y0,_y2[2]);})];}),new T(function(){return E(E(_y1)[2]);})];}]);}),[new T(function(){return A(_xU,[_xZ]);})]);},_xV,_xX);};},_y3=function(_y4,_y5){while(1){var _y6=(function(_y7,_y8){var _y9=E(_y8);if(!_y9[0]){return E(_y7);}else{_y4=new T(function(){return _xP(_dH,_29,_xj,_xs,_y7,_y9[1]);});_y5=_y9[2];return null;}})(_y4,_y5);if(_y6!=null){return _y6;}}},_ya=[13,coercionToken],_yb=unCStr("text"),_yc=function(_yd,_ye,_yf){var _yg=function(_yh,_yi){var _yj=new T(function(){return _tN(function(_yk){return A(_yd,[_yk,_yh,function(_yl){return A(_yi,[new T(function(){return [0, -E(_yl)[1]];})]);}]);});});return _im(_ug(function(_ym){var _yn=E(_ym);if(_yn[0]==4){var _yo=E(_yn[1]);return _yo[0]==0?A(_yd,[_yn,_yh,_yi]):E(E(_yo[1])[1])==45?E(_yo[2])[0]==0?E([1,function(_yp){return A(_s4,[_yp,function(_yq){return E(_yj);}]);}]):A(_yd,[_yn,_yh,_yi]):A(_yd,[_yn,_yh,_yi]);}else{return A(_yd,[_yn,_yh,_yi]);}}),new T(function(){return _um(_yg,_yi);}));};return _yg(_ye,_yf);},_yr=function(_ys,_yt){return [2];},_yu=function(_vJ,_vo){return _yr(_vJ,_vo);},_yv=function(_yw){var _yx=E(_yw);return _yx[0]==0?[1,new T(function(){return _kT(new T(function(){return _r9(E(_yx[1])[1]);}),_kK,_yx[2]);})]:E(_yx[2])[0]==0?E(_yx[3])[0]==0?[1,new T(function(){return _kT(_kJ,_kK,_yx[1]);})]:[0]:[0];},_yy=function(_yz){var _yA=E(_yz);if(_yA[0]==5){var _yB=_yv(_yA[1]);if(!_yB[0]){return E(_yr);}else{var _yC=new T(function(){return [0,_lV(_yB[1])];});return function(_yD,_yE){return A(_yE,[_yC]);};}}else{return E(_yu);}},_yF=function(_vJ,_vo){return _yc(_yy,_vJ,_vo);},_yG=function(_yH,_yI){return _uF(_yF,_yI);},_yJ=new T(function(){return _uF(_yF,_jn);}),_yK=function(_vo){return _ic(_yJ,_vo);},_yL=function(_yM){var _yN=new T(function(){return _yc(_yy,_yM,_jn);});return function(_fu){return _ic(_yN,_fu);};},_yO=[0,_yL,_yK,_yF,_yG],_yP=function(_yQ,_yR){return _2O(0,E(_yQ)[1],_yR);},_yS=function(_yT,_yU){return _1G(_yP,_yT,_yU);},_yV=function(_yW,_yX,_yY){return _2O(E(_yW)[1],E(_yX)[1],_yY);},_yZ=[0,_yV,_wJ,_yS],_z0=unCStr("Int"),_z1=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_av,_e3,_z0],_z2=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_z1,_s],_z3=function(_z4){return E(_z2);},_z5=new T(function(){return _gz(_dH,_dI,_z3,_yZ,_yO);}),_z6=new T(function(){return A(_z5,[_9,_yb,_9]);}),_z7=function(_z8,_){var _z9=A(_z6,[_z8,_]),_za=E(_z9),_zb=E(_za[1]),_zc=new T(function(){return E(E(_z8)[4]);});return [0,[0,function(_zd,_){var _ze=_a0(_zb[1],_ya,_zc,_zd,_),_zf=_ae(_zd,_);return _zd;},_zb[2]],_za[2]];},_zg=new T(function(){return [1,_z7,_zg];}),_zh=function(_zi,_zj){var _zk=E(_zi);if(!_zk){return [0];}else{var _zl=E(_zj);return _zl[0]==0?[0]:[1,_zl[1],new T(function(){return _zh(_zk-1|0,_zl[2]);})];}},_zm=function(_zn,_zo){return _zn<0?[0]:_zh(_zn,_zo);},_zp=function(_zq,_zr){var _zs=E(_zq)[1];return _zs>0?_zm(_zs,_zr):[0];},_zt=function(_zu){return E(_zu);},_zv=function(_zw){var _zx=new T(function(){return _y3(_x9,_zp(_zw,_zg));});return function(_zy,_){var _zz=_3v(_zx,_wY,_zy,_),_zA=E(_zz),_zB=E(_zA[1]),_zC=new T(function(){return _wz(_zt,_zB[1]);});return [0,[0,function(_zD,_){var _zE=A(_wI,[_zD,_]),_zF=A(_zC,[_zD,_]);return _zD;},_zB[2]],_zA[2]];};},_zG=function(_zH){var _zI=new T(function(){return _zG(new T(function(){return [0,E(_zH)[1]+1|0];}));}),_zJ=new T(function(){return _zv(_zH);});return function(_fu,_zK){return _3v(function(_zL,_){return _7o(_zJ,_wn,_zL,_);},function(_zM,_zL,_){return (function(_zL,_){return _3v(_9y,function(_zN){return E(_zI);},_zL,_);})(_zL,_);},_fu,_zK);};},_zO=new T(function(){return _zG(_0);}),_zP=unCStr("idelem"),_zQ=[0,_27,_3m],_zR=function(_zS,_zT,_){return [0,_zQ,_zT];},_zU=function(_zV,_zW,_){return _3v(function(_zX,_){var _=wMV(E(_3k)[1],_zV);return [0,_3n,_zX];},_zR,_zW,_);},_zY=function(_zZ,_A0,_){var _A1=E(_3k)[1],_A2=rMV(_A1),_A3=A(_zZ,[[0,_s,_A2,_3d,function(_){var _A4=function(_A5,_A6,_){var _A7=rMV(_A1),_A8=A(_A5,[[0,_s,_A7,_3d,function(_){return _A4(_A5,_A6,_);}],_]),_A9=E(_A8),_Aa=_3v(_3t,_zU,_A9[2],_),_Ab=A(E(_A9[1])[1],[_A6,_]),_Ac=A(E(E(_Aa)[1])[1],[_A6,_]);return _2;};return _A4(_zZ,_A0,_);}],_]),_Ad=E(_A3),_Ae=_3v(_3t,_zU,_Ad[2],_),_Af=A(E(_Ad[1])[1],[_A0,_]),_Ag=A(E(E(_Ae)[1])[1],[_A0,_]);return _2;},_Ah=function(_){var _Ai=E(_zP),_Aj=jsFind(toJSStr(_Ai)),_Ak=E(_Aj);return _Ak[0]==0?_3x(_Ai):_zY(_zO,_Ak[1],_);},_Al=function(_){return _Ah(_);};
var hasteMain = function() {A(_Al, [0]);};window.onload = hasteMain;