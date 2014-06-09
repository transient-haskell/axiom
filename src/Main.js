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

var _0=0,_1=[0,0],_2=2,_3=[0],_4=function(_5,_6,_){var _7=A(_5,[[0,_3,_1,_2,function(_){return _4(_5,_6,_);}],_]),_8=A(E(E(_7)[1])[1],[_6,_]);return _0;},_9=function(_a,_b,_){var _c=jsCreateTextNode(toJSStr(E(_a))),_d=jsAppendChild(_c,E(_b)[1]);return [0,_c];},_e=function(_f,_g){var _h=E(_f);return _h[0]==0?E(_g):[1,_h[1],new T(function(){return _e(_h[2],_g);})];},_i=function(_j,_k){var _l=jsShowI(_j);return _e(fromJSStr(_l),_k);},_m=[0,41],_n=[0,40],_o=function(_p,_q,_r){return _q>=0?_i(_q,_r):_p<=6?_i(_q,_r):[1,_n,new T(function(){var _s=jsShowI(_q);return _e(fromJSStr(_s),[1,_m,_r]);})];},_t=[0],_u=[0,98],_v=[1,_u,_3],_w=function(_x,_y,_){var _z=A(_x,[_]);return A(_y,[_]);},_A=function(_B,_C,_){return _w(_B,_C,_);},_D=function(_E,_F,_){var _G=A(_E,[_]);return A(_F,[_G,_]);},_H=unCStr("base"),_I=unCStr("GHC.IO.Exception"),_J=unCStr("IOException"),_K=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_H,_I,_J],_L=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_K,_3],_M=function(_N){return E(_L);},_O=function(_P){return E(E(_P)[1]);},_Q=unCStr("Maybe.fromJust: Nothing"),_R=new T(function(){return err(_Q);}),_S=function(_T,_U,_V){var _W=new T(function(){var _X=A(_T,[_V]),_Y=A(_U,[new T(function(){var _Z=E(_W);return _Z[0]==0?E(_R):E(_Z[1]);})]),_10=hs_eqWord64(_X[1],_Y[1]);if(!E(_10)){return [0];}else{var _11=hs_eqWord64(_X[2],_Y[2]);return E(_11)==0?[0]:[1,_V];}});return E(_W);},_12=function(_13){var _14=E(_13);return _S(_O(_14[1]),_M,_14[2]);},_15=unCStr(": "),_16=[0,41],_17=unCStr(" ("),_18=unCStr("already exists"),_19=unCStr("does not exist"),_1a=unCStr("protocol error"),_1b=unCStr("failed"),_1c=unCStr("invalid argument"),_1d=unCStr("inappropriate type"),_1e=unCStr("hardware fault"),_1f=unCStr("unsupported operation"),_1g=unCStr("timeout"),_1h=unCStr("resource vanished"),_1i=unCStr("interrupted"),_1j=unCStr("resource busy"),_1k=unCStr("resource exhausted"),_1l=unCStr("end of file"),_1m=unCStr("illegal operation"),_1n=unCStr("permission denied"),_1o=unCStr("user error"),_1p=unCStr("unsatisified constraints"),_1q=unCStr("system error"),_1r=function(_1s,_1t){switch(E(_1s)){case 0:return _e(_18,_1t);case 1:return _e(_19,_1t);case 2:return _e(_1j,_1t);case 3:return _e(_1k,_1t);case 4:return _e(_1l,_1t);case 5:return _e(_1m,_1t);case 6:return _e(_1n,_1t);case 7:return _e(_1o,_1t);case 8:return _e(_1p,_1t);case 9:return _e(_1q,_1t);case 10:return _e(_1a,_1t);case 11:return _e(_1b,_1t);case 12:return _e(_1c,_1t);case 13:return _e(_1d,_1t);case 14:return _e(_1e,_1t);case 15:return _e(_1f,_1t);case 16:return _e(_1g,_1t);case 17:return _e(_1h,_1t);default:return _e(_1i,_1t);}},_1u=[0,125],_1v=unCStr("{handle: "),_1w=function(_1x,_1y,_1z,_1A,_1B,_1C){var _1D=new T(function(){var _1E=new T(function(){return _1r(_1y,new T(function(){var _1F=E(_1A);return _1F[0]==0?E(_1C):_e(_17,new T(function(){return _e(_1F,[1,_16,_1C]);}));}));}),_1G=E(_1z);return _1G[0]==0?E(_1E):_e(_1G,new T(function(){return _e(_15,_1E);}));}),_1H=E(_1B);if(!_1H[0]){var _1I=E(_1x);if(!_1I[0]){return E(_1D);}else{var _1J=E(_1I[1]);return _1J[0]==0?_e(_1v,new T(function(){return _e(_1J[1],[1,_1u,new T(function(){return _e(_15,_1D);})]);})):_e(_1v,new T(function(){return _e(_1J[1],[1,_1u,new T(function(){return _e(_15,_1D);})]);}));}}else{return _e(_1H[1],new T(function(){return _e(_15,_1D);}));}},_1K=function(_1L){var _1M=E(_1L);return _1w(_1M[1],_1M[2],_1M[3],_1M[4],_1M[6],_3);},_1N=function(_1O,_1P){var _1Q=E(_1O);return _1w(_1Q[1],_1Q[2],_1Q[3],_1Q[4],_1Q[6],_1P);},_1R=[0,44],_1S=[0,93],_1T=[0,91],_1U=function(_1V,_1W,_1X){var _1Y=E(_1W);return _1Y[0]==0?unAppCStr("[]",_1X):[1,_1T,new T(function(){return A(_1V,[_1Y[1],new T(function(){var _1Z=function(_20){var _21=E(_20);return _21[0]==0?E([1,_1S,_1X]):[1,_1R,new T(function(){return A(_1V,[_21[1],new T(function(){return _1Z(_21[2]);})]);})];};return _1Z(_1Y[2]);})]);})];},_22=function(_23,_24){return _1U(_1N,_23,_24);},_25=function(_26,_27,_28){var _29=E(_27);return _1w(_29[1],_29[2],_29[3],_29[4],_29[6],_28);},_2a=[0,_25,_1K,_22],_2b=new T(function(){return [0,_M,_2a,_2c,_12];}),_2c=function(_2d){return [0,_2b,_2d];},_2e=7,_2f=function(_2g){return [0,_t,_2e,_3,_2g,_t,_t];},_2h=function(_2i,_){return die(new T(function(){return _2c(new T(function(){return _2f(_2i);}));}));},_2j=function(_2k,_){return _2h(_2k,_);},_2l=function(_2m,_){return _2m;},_2n=[0,_D,_A,_2l,_2j],_2o=function(_2p){return E(_2p);},_2q=[0,_2n,_2o],_2r=unCStr("id"),_2s=function(_2t){return E(E(_2t)[1]);},_2u=function(_2v,_2w,_2x,_2y){return A(_2s,[_2v,new T(function(){return A(_2w,[_2y]);}),function(_2z){return A(_2x,[new T(function(){return E(E(_2z)[1]);}),new T(function(){return E(E(_2z)[2]);})]);}]);},_2A=function(_2B,_2C,_2D,_2E){return A(_2s,[_2B,new T(function(){return A(_2C,[_2E]);}),function(_2F){return A(_2D,[new T(function(){return E(E(_2F)[2]);})]);}]);},_2G=function(_2H,_2I,_2J,_2K){return _2A(_2H,_2I,_2J,_2K);},_2L=function(_2M){return E(E(_2M)[4]);},_2N=function(_2O,_2P){var _2Q=new T(function(){return A(_2L,[_2O,_2P]);});return function(_2R){return E(_2Q);};},_2S=function(_2T){return E(E(_2T)[3]);},_2U=function(_2V){var _2W=new T(function(){return _2S(_2V);});return [0,function(_2I,_2J,_2K){return _2u(_2V,_2I,_2J,_2K);},function(_2I,_2J,_2K){return _2G(_2V,_2I,_2J,_2K);},function(_2X,_2Y){return A(_2W,[[0,_2X,_2Y]]);},function(_2K){return _2N(_2V,_2K);}];},_2Z=function(_30){return E(E(_30)[1]);},_31=function(_32){return E(E(_32)[2]);},_33=function(_34,_35){var _36=new T(function(){return A(_31,[_34,_35]);}),_37=new T(function(){return _2Z(_34);}),_38=new T(function(){return _2S(_37);}),_39=new T(function(){return _2s(_37);});return function(_3a){return A(_39,[_36,function(_3b){return A(_38,[[0,_3b,_3a]]);}]);};},_3c=[0,112],_3d=function(_3e,_3f,_3g,_3h){var _3i=E(_3f);return A(_3i[1],[new T(function(){var _3j=E(_3e);return E(_3g);}),function(_3k){var _3l=new T(function(){return E(E(_3k)[2]);});return A(_3i[2],[new T(function(){return A(_3h,[new T(function(){var _3m=E(new T(function(){var _3n=E(_3e);return [0,coercionToken];})),_3o=E(_3k);return [0,_3o[1],new T(function(){return [0,E(_3l)[1]+1|0];}),_3o[3],_3o[4]];})]);}),new T(function(){return A(_3i[3],[[1,_3c,new T(function(){return _e(_o(0,E(_3l)[1],_3),new T(function(){return E(E(_3k)[1]);}));})]]);})]);}]);},_3p=function(_3q,_3r,_3s,_3t){return A(_3q,[new T(function(){return function(_){var _3u=jsSetAttr(E(_3r)[1],toJSStr(E(_3s)),toJSStr(E(_3t)));return _0;};})]);},_3v=function(_3w,_3x){return A(_2S,[_3w,[0,_3x,_3x]]);},_3y=unCStr("div"),_3z=function(_3A,_3B,_){var _3C=jsCreateElem(toJSStr(E(_3A))),_3D=jsAppendChild(_3C,E(_3B)[1]);return [0,_3C];},_3E=function(_3F,_3G,_3H){return A(_2S,[_3F,[0,_0,_3G]]);},_3I=function(_3J){var _3K=new T(function(){return _2Z(_3J);}),_3L=new T(function(){return _3d([0,coercionToken],_2U(_3K),function(_3M){return _3v(_3K,_3M);},function(_3N,_3O){return _3E(_3K,_3N,_3O);});}),_3P=new T(function(){return _2S(_3K);}),_3Q=new T(function(){return _2s(_3K);}),_3R=new T(function(){return _2s(_3K);}),_3S=new T(function(){return _2s(_3K);}),_3T=new T(function(){return _2s(_3K);});return function(_3U,_3V){return A(_3T,[new T(function(){return A(_3L,[_3V]);}),function(_3W){var _3X=new T(function(){return E(E(_3W)[1]);});return A(_3S,[new T(function(){return A(_33,[_3J,function(_){return jsFind(toJSStr(E(_3X)));},new T(function(){return E(E(_3W)[2]);})]);}),function(_3Y){return A(_3R,[new T(function(){return A(_3U,[new T(function(){return E(E(_3Y)[2]);})]);}),function(_3Z){var _40=E(_3Z),_41=_40[2],_42=E(_40[1]),_43=_42[1],_44=_42[2],_45=E(E(_3Y)[1]);if(!_45[0]){return A(_3P,[[0,[0,function(_46,_){var _47=_3z(_3y,_46,_),_48=A(_3p,[_2o,_47,_2r,_3X,_]),_49=A(_43,[_47,_]);return _47;},_44],_41]]);}else{var _4a=_45[1],_4b=new T(function(){return A(_43,[_4a]);});return A(_3Q,[new T(function(){return A(_33,[_3J,new T(function(){return function(_){var _4c=jsClearChildren(E(_4a)[1]);return _0;};}),_41]);}),function(_4d){return A(_3P,[[0,[0,function(_4e){return E(_4b);},_44],new T(function(){return E(E(_4d)[2]);})]]);}]);}}]);}]);}]);};},_4f=new T(function(){return _3I(_2q);}),_4g=[12,coercionToken],_4h=function(_4i,_4j){return [0,E(_4i)[1]+E(_4j)[1]|0];},_4k=new T(function(){return [0,"keydown"];}),_4l=new T(function(){return [0,"mousemove"];}),_4m=new T(function(){return [0,"blur"];}),_4n=new T(function(){return [0,"focus"];}),_4o=new T(function(){return [0,"change"];}),_4p=new T(function(){return [0,"unload"];}),_4q=new T(function(){return [0,"load"];}),_4r=new T(function(){return [0,"keyup"];}),_4s=new T(function(){return [0,"keypress"];}),_4t=new T(function(){return [0,"mouseup"];}),_4u=new T(function(){return [0,"mousedown"];}),_4v=new T(function(){return [0,"dblclick"];}),_4w=new T(function(){return [0,"click"];}),_4x=new T(function(){return [0,"mouseout"];}),_4y=new T(function(){return [0,"mouseover"];}),_4z=function(_4A){switch(E(_4A)[0]){case 0:return E(_4q);case 1:return E(_4p);case 2:return E(_4o);case 3:return E(_4n);case 4:return E(_4m);case 5:return E(_4l);case 6:return E(_4y);case 7:return E(_4x);case 8:return E(_4w);case 9:return E(_4v);case 10:return E(_4u);case 11:return E(_4t);case 12:return E(_4s);case 13:return E(_4r);default:return E(_4k);}},_4B=new T(function(){return [0,"(function(e) {e.focus();})"];}),_4C=function(_4D){var _4E=A(_4D,[_]);return E(_4E);},_4F=function(_4G){return _4C(function(_){var _=0;return eval(E(_4G)[1]);});},_4H=new T(function(){return _4F(_4B);}),_4I=function(_4J,_){var _4K=A(_4H,[E(E(_4J)[1]),_]);return _0;},_4L=function(_4M,_){return _4I(_4M,_);},_4N=function(_4O,_4P,_4Q,_4R,_){var _4S=A(_4O,[_4R,_]),_4T=jsSetCB(E(_4R)[1],_4z(_4P)[1],function(_){var _4U=A(_4Q,[_]);return _4L(_4R,_);});return _4S;},_4V=unCStr("br"),_4W=function(_4X,_){var _4Y=jsCreateElem(toJSStr(E(_4V))),_4Z=jsAppendChild(_4Y,E(_4X)[1]);return [0,_4Y];},_50=unCStr("text"),_51=function(_52,_53,_54,_){var _55=_3z(_52,_54,_),_56=A(_53,[_55,_]);return _55;},_57=unCStr("()"),_58=unCStr("GHC.Tuple"),_59=unCStr("ghc-prim"),_5a=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_59,_58,_57],_5b=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5a,_3],_5c=function(_5d){return E(_5b);},_5e=unCStr("main"),_5f=unCStr("Builder"),_5g=unCStr("JSBuilderM"),_5h=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5e,_5f,_5g],_5i=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5h,_3],_5j=function(_5k){return E(_5i);},_5l=function(_5m){var _5n=E(_5m);return _5n[0]==0?[0]:_e(_5n[1],new T(function(){return _5l(_5n[2]);}));},_5o=function(_5p,_5q){var _5r=E(_5p);if(!_5r){return [0,_3,_5q];}else{var _5s=E(_5q);if(!_5s[0]){return [0,_3,_3];}else{var _5t=new T(function(){var _5u=_5o(_5r-1|0,_5s[2]);return [0,_5u[1],_5u[2]];});return [0,[1,_5s[1],new T(function(){return E(E(_5t)[1]);})],new T(function(){return E(E(_5t)[2]);})];}}},_5v=[0,120],_5w=[0,48],_5x=function(_5y){var _5z=new T(function(){var _5A=_5o(8,new T(function(){var _5B=md5(toJSStr(E(_5y)));return fromJSStr(_5B);}));return [0,_5A[1],_5A[2]];}),_5C=parseInt([0,toJSStr([1,_5w,[1,_5v,new T(function(){return E(E(_5z)[1]);})]])]),_5D=new T(function(){var _5E=_5o(8,new T(function(){return E(E(_5z)[2]);}));return [0,_5E[1],_5E[2]];}),_5F=parseInt([0,toJSStr([1,_5w,[1,_5v,new T(function(){return E(E(_5D)[1]);})]])]),_5G=hs_mkWord64(_5C,_5F),_5H=parseInt([0,toJSStr([1,_5w,[1,_5v,new T(function(){return E(_5o(8,new T(function(){return E(E(_5D)[2]);}))[1]);})]])]),_5I=hs_mkWord64(_5H,_5H);return [0,_5G,_5I];},_5J=function(_5K,_5L){var _5M=E(_5L);return _5M[0]==0?[0]:[1,new T(function(){return A(_5K,[_5M[1]]);}),new T(function(){return _5J(_5K,_5M[2]);})];},_5N=function(_5O,_5P){var _5Q=jsShowI(_5O),_5R=md5(_5Q);return _e(fromJSStr(_5R),new T(function(){var _5S=jsShowI(_5P),_5T=md5(_5S);return fromJSStr(_5T);}));},_5U=function(_5V){var _5W=E(_5V);return _5N(_5W[1],_5W[2]);},_5X=function(_5Y){var _5Z=E(_5Y);if(!_5Z[0]){return [0];}else{var _60=E(_5Z[1]);return [1,[0,_60[1],_60[2]],new T(function(){return _5X(_5Z[2]);})];}},_61=unCStr("Prelude.undefined"),_62=new T(function(){return err(_61);}),_63=function(_64,_65){return function(_66){return E(new T(function(){var _67=A(_64,[_62]),_68=E(_67[3]),_69=_68[1],_6a=_68[2],_6b=_e(_67[4],[1,new T(function(){return A(_65,[_62]);}),_3]);if(!_6b[0]){return [0,_69,_6a,_68,_3];}else{var _6c=_5x(new T(function(){return _5l(_5J(_5U,[1,[0,_69,_6a],new T(function(){return _5X(_6b);})]));}));return [0,_6c[1],_6c[2],_68,_6b];}}));};},_6d=new T(function(){return _63(_5j,_5c);}),_6e=function(_6f,_6g,_6h,_){var _6i=E(_6g),_6j=A(_6f,[_6h,_]),_6k=A(_3p,[_2o,_6j,_6i[1],_6i[2],_]);return _6j;},_6l=function(_6m,_6n){while(1){var _6o=(function(_6p,_6q){var _6r=E(_6q);if(!_6r[0]){return E(_6p);}else{_6m=function(_6s,_){return _6e(_6p,_6r[1],_6s,_);};_6n=_6r[2];return null;}})(_6m,_6n);if(_6o!=null){return _6o;}}},_6t=unCStr("value"),_6u=unCStr("onclick"),_6v=unCStr("checked"),_6w=[0,_6v,_3],_6x=[1,_6w,_3],_6y=unCStr("type"),_6z=unCStr("input"),_6A=function(_6B,_){return _3z(_6z,_6B,_);},_6C=function(_6D,_6E,_6F,_6G,_6H){var _6I=new T(function(){var _6J=new T(function(){return _6l(_6A,[1,[0,_6y,_6E],[1,[0,_2r,_6D],[1,[0,_6t,_6F],_3]]]);});return !E(_6G)?E(_6J):_6l(_6J,_6x);}),_6K=E(_6H);return _6K[0]==0?E(_6I):_6l(_6I,[1,[0,_6u,_6K[1]],_3]);},_6L=unCStr("href"),_6M=[0,97],_6N=[1,_6M,_3],_6O=function(_6P,_){return _3z(_6N,_6P,_);},_6Q=function(_6R,_6S){var _6T=new T(function(){return _6l(_6O,[1,[0,_6L,_6R],_3]);});return function(_6U,_){var _6V=A(_6T,[_6U,_]),_6W=A(_6S,[_6V,_]);return _6V;};},_6X=function(_6Y){return _6Q(_6Y,function(_6s,_){return _9(_6Y,_6s,_);});},_6Z=unCStr("option"),_70=function(_71,_){return _3z(_6Z,_71,_);},_72=unCStr("selected"),_73=[0,_72,_3],_74=[1,_73,_3],_75=function(_76,_77,_78){var _79=new T(function(){return _6l(_70,[1,[0,_6t,_76],_3]);}),_7a=function(_7b,_){var _7c=A(_79,[_7b,_]),_7d=A(_77,[_7c,_]);return _7c;};return !E(_78)?E(_7a):_6l(_7a,_74);},_7e=function(_7f,_7g){return _75(_7f,function(_6s,_){return _9(_7f,_6s,_);},_7g);},_7h=unCStr("method"),_7i=unCStr("action"),_7j=unCStr("UTF-8"),_7k=unCStr("acceptCharset"),_7l=[0,_7k,_7j],_7m=unCStr("form"),_7n=function(_7o,_){return _3z(_7m,_7o,_);},_7p=function(_7q,_7r,_7s){var _7t=new T(function(){return _6l(_7n,[1,_7l,[1,[0,_7i,_7q],[1,[0,_7h,_7r],_3]]]);});return function(_7u,_){var _7v=A(_7t,[_7u,_]),_7w=A(_7s,[_7v,_]);return _7v;};},_7x=unCStr("select"),_7y=function(_7z,_){return _3z(_7x,_7z,_);},_7A=function(_7B,_7C){var _7D=new T(function(){return _6l(_7y,[1,[0,_2r,_7B],_3]);});return function(_7E,_){var _7F=A(_7D,[_7E,_]),_7G=A(_7C,[_7F,_]);return _7F;};},_7H=unCStr("textarea"),_7I=function(_7J,_){return _3z(_7H,_7J,_);},_7K=function(_7L,_7M){var _7N=new T(function(){return _6l(_7I,[1,[0,_2r,_7L],_3]);});return function(_7O,_){var _7P=A(_7N,[_7O,_]),_7Q=_9(_7M,_7P,_);return _7P;};},_7R=unCStr("color:red"),_7S=unCStr("style"),_7T=[0,_7S,_7R],_7U=[1,_7T,_3],_7V=[0,98],_7W=[1,_7V,_3],_7X=function(_7Y){return _6l(function(_7Z,_){var _80=_3z(_7W,_7Z,_),_81=A(_7Y,[_80,_]);return _80;},_7U);},_82=unCStr("toByteString not defined"),_83=new T(function(){return err(_82);}),_84=function(_85,_86,_){var _87=E(_85);if(!_87[0]){return _86;}else{var _88=A(_87[1],[_86,_]),_89=_84(_87[2],_86,_);return _86;}},_8a=function(_8b,_8c,_8d,_){var _8e=A(_8b,[_8d,_]),_8f=A(_8c,[_8d,_]);return _8d;},_8g=[0,_2l,_8a,_84],_8h=[0,_8g,_6d,_83,_9,_9,_51,_7X,_6Q,_6X,_6C,_7K,_7A,_75,_7e,_7p,_6l],_8i=unCStr("base"),_8j=unCStr("Control.Exception.Base"),_8k=unCStr("PatternMatchFail"),_8l=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8i,_8j,_8k],_8m=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8l,_3],_8n=function(_8o){return E(_8m);},_8p=function(_8q){var _8r=E(_8q);return _S(_O(_8r[1]),_8n,_8r[2]);},_8s=function(_8t){return E(E(_8t)[1]);},_8u=function(_8v,_8w){return _e(E(_8v)[1],_8w);},_8x=function(_8y,_8z){return _1U(_8u,_8y,_8z);},_8A=function(_8B,_8C,_8D){return _e(E(_8C)[1],_8D);},_8E=[0,_8A,_8s,_8x],_8F=new T(function(){return [0,_8n,_8E,_8G,_8p];}),_8G=function(_8H){return [0,_8F,_8H];},_8I=unCStr("Non-exhaustive patterns in"),_8J=function(_8K,_8L){return die(new T(function(){return A(_8L,[_8K]);}));},_8M=function(_8N,_8O){var _8P=E(_8O);if(!_8P[0]){return [0,_3,_3];}else{var _8Q=_8P[1];if(!A(_8N,[_8Q])){return [0,_3,_8P];}else{var _8R=new T(function(){var _8S=_8M(_8N,_8P[2]);return [0,_8S[1],_8S[2]];});return [0,[1,_8Q,new T(function(){return E(E(_8R)[1]);})],new T(function(){return E(E(_8R)[2]);})];}}},_8T=[0,32],_8U=[0,10],_8V=[1,_8U,_3],_8W=function(_8X){return E(E(_8X)[1])==124?false:true;},_8Y=function(_8Z,_90){var _91=_8M(_8W,unCStr(_8Z)),_92=_91[1],_93=function(_94,_95){return _e(_94,new T(function(){return unAppCStr(": ",new T(function(){return _e(_90,new T(function(){return _e(_95,_8V);}));}));}));},_96=E(_91[2]);return _96[0]==0?_93(_92,_3):E(E(_96[1])[1])==124?_93(_92,[1,_8T,_96[2]]):_93(_92,_3);},_97=function(_98){return _8J([0,new T(function(){return _8Y(_98,_8I);})],_8G);},_99=new T(function(){return _97("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_9a=function(_9b,_9c){while(1){var _9d=(function(_9e,_9f){var _9g=E(_9e);switch(_9g[0]){case 0:var _9h=E(_9f);if(!_9h[0]){return [0];}else{_9b=A(_9g[1],[_9h[1]]);_9c=_9h[2];return null;}break;case 1:var _9i=A(_9g[1],[_9f]),_9j=_9f;_9b=_9i;_9c=_9j;return null;case 2:return [0];case 3:return [1,[0,_9g[1],_9f],new T(function(){return _9a(_9g[2],_9f);})];default:return E(_9g[1]);}})(_9b,_9c);if(_9d!=null){return _9d;}}},_9k=function(_9l,_9m){var _9n=new T(function(){var _9o=E(_9m);if(_9o[0]==3){return [3,_9o[1],new T(function(){return _9k(_9l,_9o[2]);})];}else{var _9p=E(_9l);if(_9p[0]==2){return E(_9o);}else{var _9q=E(_9o);if(_9q[0]==2){return E(_9p);}else{var _9r=new T(function(){var _9s=E(_9q);if(_9s[0]==4){return [1,function(_9t){return [4,new T(function(){return _e(_9a(_9p,_9t),_9s[1]);})];}];}else{var _9u=E(_9p);if(_9u[0]==1){var _9v=_9u[1],_9w=E(_9s);return _9w[0]==0?[1,function(_9x){return _9k(A(_9v,[_9x]),_9w);}]:[1,function(_9y){return _9k(A(_9v,[_9y]),new T(function(){return A(_9w[1],[_9y]);}));}];}else{var _9z=E(_9s);return _9z[0]==0?E(_99):[1,function(_9A){return _9k(_9u,new T(function(){return A(_9z[1],[_9A]);}));}];}}}),_9B=E(_9p);switch(_9B[0]){case 1:var _9C=E(_9q);return _9C[0]==4?[1,function(_9D){return [4,new T(function(){return _e(_9a(A(_9B[1],[_9D]),_9D),_9C[1]);})];}]:E(_9r);case 4:var _9E=_9B[1],_9F=E(_9q);switch(_9F[0]){case 0:return [1,function(_9G){return [4,new T(function(){return _e(_9E,new T(function(){return _9a(_9F,_9G);}));})];}];case 1:return [1,function(_9H){return [4,new T(function(){return _e(_9E,new T(function(){return _9a(A(_9F[1],[_9H]),_9H);}));})];}];default:return [4,new T(function(){return _e(_9E,_9F[1]);})];}break;default:return E(_9r);}}}}}),_9I=E(_9l);switch(_9I[0]){case 0:var _9J=E(_9m);return _9J[0]==0?[0,function(_9K){return _9k(A(_9I[1],[_9K]),new T(function(){return A(_9J[1],[_9K]);}));}]:E(_9n);case 3:return [3,_9I[1],new T(function(){return _9k(_9I[2],_9m);})];default:return E(_9n);}},_9L=function(_9M,_9N){return E(_9M)[1]!=E(_9N)[1];},_9O=function(_9P,_9Q){return E(_9P)[1]==E(_9Q)[1];},_9R=[0,_9O,_9L],_9S=function(_9T){return E(E(_9T)[1]);},_9U=function(_9V,_9W,_9X){while(1){var _9Y=E(_9W);if(!_9Y[0]){return E(_9X)[0]==0?true:false;}else{var _9Z=E(_9X);if(!_9Z[0]){return false;}else{if(!A(_9S,[_9V,_9Y[1],_9Z[1]])){return false;}else{_9W=_9Y[2];_9X=_9Z[2];continue;}}}}},_a0=function(_a1,_a2,_a3){return !_9U(_a1,_a2,_a3)?true:false;},_a4=function(_a5){return [0,function(_a6,_a7){return _9U(_a5,_a6,_a7);},function(_a6,_a7){return _a0(_a5,_a6,_a7);}];},_a8=new T(function(){return _a4(_9R);}),_a9=function(_aa,_ab){var _ac=E(_aa);switch(_ac[0]){case 0:return [0,function(_ad){return _a9(A(_ac[1],[_ad]),_ab);}];case 1:return [1,function(_ae){return _a9(A(_ac[1],[_ae]),_ab);}];case 2:return [2];case 3:return _9k(A(_ab,[_ac[1]]),new T(function(){return _a9(_ac[2],_ab);}));default:var _af=function(_ag){var _ah=E(_ag);if(!_ah[0]){return [0];}else{var _ai=E(_ah[1]);return _e(_9a(A(_ab,[_ai[1]]),_ai[2]),new T(function(){return _af(_ah[2]);}));}},_aj=_af(_ac[1]);return _aj[0]==0?[2]:[4,_aj];}},_ak=[2],_al=function(_am){return [3,_am,_ak];},_an=function(_ao,_ap){var _aq=E(_ao);if(!_aq){return A(_ap,[_0]);}else{var _ar=new T(function(){return _an(_aq-1|0,_ap);});return [0,function(_as){return E(_ar);}];}},_at=function(_au,_av,_aw){var _ax=new T(function(){return A(_au,[_al]);});return [1,function(_ay){return A(function(_az,_aA,_aB){while(1){var _aC=(function(_aD,_aE,_aF){var _aG=E(_aD);switch(_aG[0]){case 0:var _aH=E(_aE);if(!_aH[0]){return E(_av);}else{_az=A(_aG[1],[_aH[1]]);_aA=_aH[2];var _aI=_aF+1|0;_aB=_aI;return null;}break;case 1:var _aJ=A(_aG[1],[_aE]),_aK=_aE,_aI=_aF;_az=_aJ;_aA=_aK;_aB=_aI;return null;case 2:return E(_av);case 3:return function(_aL){var _aM=new T(function(){return _a9(_aG,_aL);});return _an(_aF,function(_aN){return E(_aM);});};default:return function(_aO){return _a9(_aG,_aO);};}})(_az,_aA,_aB);if(_aC!=null){return _aC;}}},[_ax,_ay,0,_aw]);}];},_aP=[6],_aQ=unCStr("valDig: Bad base"),_aR=new T(function(){return err(_aQ);}),_aS=function(_aT,_aU){var _aV=function(_aW,_aX){var _aY=E(_aW);if(!_aY[0]){var _aZ=new T(function(){return A(_aX,[_3]);});return function(_b0){return A(_b0,[_aZ]);};}else{var _b1=E(_aY[1])[1],_b2=function(_b3){var _b4=new T(function(){return _aV(_aY[2],function(_b5){return A(_aX,[[1,_b3,_b5]]);});});return function(_b6){var _b7=new T(function(){return A(_b4,[_b6]);});return [0,function(_b8){return E(_b7);}];};};switch(E(E(_aT)[1])){case 8:if(48>_b1){var _b9=new T(function(){return A(_aX,[_3]);});return function(_ba){return A(_ba,[_b9]);};}else{if(_b1>55){var _bb=new T(function(){return A(_aX,[_3]);});return function(_bc){return A(_bc,[_bb]);};}else{return _b2([0,_b1-48|0]);}}break;case 10:if(48>_b1){var _bd=new T(function(){return A(_aX,[_3]);});return function(_be){return A(_be,[_bd]);};}else{if(_b1>57){var _bf=new T(function(){return A(_aX,[_3]);});return function(_bg){return A(_bg,[_bf]);};}else{return _b2([0,_b1-48|0]);}}break;case 16:var _bh=new T(function(){return 97>_b1?65>_b1?[0]:_b1>70?[0]:[1,[0,(_b1-65|0)+10|0]]:_b1>102?65>_b1?[0]:_b1>70?[0]:[1,[0,(_b1-65|0)+10|0]]:[1,[0,(_b1-97|0)+10|0]];});if(48>_b1){var _bi=E(_bh);if(!_bi[0]){var _bj=new T(function(){return A(_aX,[_3]);});return function(_bk){return A(_bk,[_bj]);};}else{return _b2(_bi[1]);}}else{if(_b1>57){var _bl=E(_bh);if(!_bl[0]){var _bm=new T(function(){return A(_aX,[_3]);});return function(_bn){return A(_bn,[_bm]);};}else{return _b2(_bl[1]);}}else{return _b2([0,_b1-48|0]);}}break;default:return E(_aR);}}};return [1,function(_bo){return A(_aV,[_bo,_2o,function(_bp){var _bq=E(_bp);return _bq[0]==0?[2]:A(_aU,[_bq]);}]);}];},_br=[0,10],_bs=[0,1],_bt=[0,2147483647],_bu=function(_bv,_bw){while(1){var _bx=E(_bv);if(!_bx[0]){var _by=_bx[1],_bz=E(_bw);if(!_bz[0]){var _bA=_bz[1],_bB=addC(_by,_bA);if(!E(_bB[2])){return [0,_bB[1]];}else{_bv=[1,I_fromInt(_by)];_bw=[1,I_fromInt(_bA)];continue;}}else{_bv=[1,I_fromInt(_by)];_bw=_bz;continue;}}else{var _bC=E(_bw);if(!_bC[0]){_bv=_bx;_bw=[1,I_fromInt(_bC[1])];continue;}else{return [1,I_add(_bx[1],_bC[1])];}}}},_bD=new T(function(){return _bu(_bt,_bs);}),_bE=function(_bF){var _bG=E(_bF);if(!_bG[0]){var _bH=E(_bG[1]);return _bH==(-2147483648)?E(_bD):[0, -_bH];}else{return [1,I_negate(_bG[1])];}},_bI=[0,10],_bJ=[0,0],_bK=function(_bL,_bM){while(1){var _bN=E(_bL);if(!_bN[0]){var _bO=_bN[1],_bP=E(_bM);if(!_bP[0]){var _bQ=_bP[1];if(!(imul(_bO,_bQ)|0)){return [0,imul(_bO,_bQ)|0];}else{_bL=[1,I_fromInt(_bO)];_bM=[1,I_fromInt(_bQ)];continue;}}else{_bL=[1,I_fromInt(_bO)];_bM=_bP;continue;}}else{var _bR=E(_bM);if(!_bR[0]){_bL=_bN;_bM=[1,I_fromInt(_bR[1])];continue;}else{return [1,I_mul(_bN[1],_bR[1])];}}}},_bS=function(_bT,_bU,_bV){while(1){var _bW=E(_bV);if(!_bW[0]){return E(_bU);}else{var _bX=_bu(_bK(_bU,_bT),_bW[1]);_bV=_bW[2];_bU=_bX;continue;}}},_bY=function(_bZ){var _c0=new T(function(){return _9k(_9k([0,function(_c1){return E(E(_c1)[1])==45?_aS(_br,function(_c2){return A(_bZ,[[1,new T(function(){return _bE(_bS(_bI,_bJ,_c2));})]]);}):[2];}],[0,function(_c3){return E(E(_c3)[1])==43?_aS(_br,function(_c4){return A(_bZ,[[1,new T(function(){return _bS(_bI,_bJ,_c4);})]]);}):[2];}]),new T(function(){return _aS(_br,function(_c5){return A(_bZ,[[1,new T(function(){return _bS(_bI,_bJ,_c5);})]]);});}));});return _9k([0,function(_c6){return E(E(_c6)[1])==101?E(_c0):[2];}],[0,function(_c7){return E(E(_c7)[1])==69?E(_c0):[2];}]);},_c8=function(_c9){return A(_c9,[_t]);},_ca=function(_cb){return A(_cb,[_t]);},_cc=function(_cd){var _ce=new T(function(){return _aS(_br,function(_cf){return A(_cd,[[1,_cf]]);});});return [0,function(_cg){return E(E(_cg)[1])==46?E(_ce):[2];}];},_ch=function(_ci){return _aS(_br,function(_cj){return _at(_cc,_c8,function(_ck){return _at(_bY,_ca,function(_cl){return A(_ci,[[5,[1,_cj,_ck,_cl]]]);});});});},_cm=function(_cn,_co,_cp){while(1){var _cq=E(_cp);if(!_cq[0]){return false;}else{if(!A(_9S,[_cn,_co,_cq[1]])){_cp=_cq[2];continue;}else{return true;}}}},_cr=unCStr("!@#$%&*+./<=>?\\^|:-~"),_cs=function(_ct){return _cm(_9R,_ct,_cr);},_cu=[0,8],_cv=[0,16],_cw=function(_cx){var _cy=new T(function(){return _aS(_cv,function(_cz){return A(_cx,[[5,[0,_cv,_cz]]]);});}),_cA=new T(function(){return _aS(_cu,function(_cB){return A(_cx,[[5,[0,_cu,_cB]]]);});}),_cC=new T(function(){return _aS(_cv,function(_cD){return A(_cx,[[5,[0,_cv,_cD]]]);});}),_cE=new T(function(){return _aS(_cu,function(_cF){return A(_cx,[[5,[0,_cu,_cF]]]);});});return [0,function(_cG){return E(E(_cG)[1])==48?E([0,function(_cH){switch(E(E(_cH)[1])){case 79:return E(_cE);case 88:return E(_cC);case 111:return E(_cA);case 120:return E(_cy);default:return [2];}}]):[2];}];},_cI=false,_cJ=true,_cK=function(_cL){var _cM=new T(function(){return A(_cL,[_cv]);}),_cN=new T(function(){return A(_cL,[_cu]);}),_cO=new T(function(){return A(_cL,[_cv]);}),_cP=new T(function(){return A(_cL,[_cu]);});return [0,function(_cQ){switch(E(E(_cQ)[1])){case 79:return E(_cP);case 88:return E(_cO);case 111:return E(_cN);case 120:return E(_cM);default:return [2];}}];},_cR=function(_cS){return A(_cS,[_br]);},_cT=function(_cU){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _o(9,_cU,_3);})));},_cV=function(_cW){var _cX=E(_cW);return _cX[0]==0?E(_cX[1]):I_toInt(_cX[1]);},_cY=function(_cZ,_d0){var _d1=E(_cZ);if(!_d1[0]){var _d2=_d1[1],_d3=E(_d0);return _d3[0]==0?_d2<=_d3[1]:I_compareInt(_d3[1],_d2)>=0;}else{var _d4=_d1[1],_d5=E(_d0);return _d5[0]==0?I_compareInt(_d4,_d5[1])<=0:I_compare(_d4,_d5[1])<=0;}},_d6=function(_d7){return [2];},_d8=function(_d9){var _da=E(_d9);if(!_da[0]){return E(_d6);}else{var _db=_da[1],_dc=E(_da[2]);if(!_dc[0]){return E(_db);}else{var _dd=new T(function(){return _d8(_dc);});return function(_de){return _9k(A(_db,[_de]),new T(function(){return A(_dd,[_de]);}));};}}},_df=unCStr("NUL"),_dg=function(_dh){return [2];},_di=function(_dj){return _dg(_dj);},_dk=function(_dl,_dm){var _dn=function(_do,_dp){var _dq=E(_do);if(!_dq[0]){return function(_dr){return A(_dr,[_dl]);};}else{var _ds=E(_dp);if(!_ds[0]){return E(_dg);}else{if(E(_dq[1])[1]!=E(_ds[1])[1]){return E(_di);}else{var _dt=new T(function(){return _dn(_dq[2],_ds[2]);});return function(_du){var _dv=new T(function(){return A(_dt,[_du]);});return [0,function(_dw){return E(_dv);}];};}}}};return [1,function(_dx){return A(_dn,[_dl,_dx,_dm]);}];},_dy=[0,0],_dz=function(_dA){var _dB=new T(function(){return A(_dA,[_dy]);});return _dk(_df,function(_dC){return E(_dB);});},_dD=unCStr("STX"),_dE=[0,2],_dF=function(_dG){var _dH=new T(function(){return A(_dG,[_dE]);});return _dk(_dD,function(_dI){return E(_dH);});},_dJ=unCStr("ETX"),_dK=[0,3],_dL=function(_dM){var _dN=new T(function(){return A(_dM,[_dK]);});return _dk(_dJ,function(_dO){return E(_dN);});},_dP=unCStr("EOT"),_dQ=[0,4],_dR=function(_dS){var _dT=new T(function(){return A(_dS,[_dQ]);});return _dk(_dP,function(_dU){return E(_dT);});},_dV=unCStr("ENQ"),_dW=[0,5],_dX=function(_dY){var _dZ=new T(function(){return A(_dY,[_dW]);});return _dk(_dV,function(_e0){return E(_dZ);});},_e1=unCStr("ACK"),_e2=[0,6],_e3=function(_e4){var _e5=new T(function(){return A(_e4,[_e2]);});return _dk(_e1,function(_e6){return E(_e5);});},_e7=unCStr("BEL"),_e8=[0,7],_e9=function(_ea){var _eb=new T(function(){return A(_ea,[_e8]);});return _dk(_e7,function(_ec){return E(_eb);});},_ed=unCStr("BS"),_ee=[0,8],_ef=function(_eg){var _eh=new T(function(){return A(_eg,[_ee]);});return _dk(_ed,function(_ei){return E(_eh);});},_ej=unCStr("HT"),_ek=[0,9],_el=function(_em){var _en=new T(function(){return A(_em,[_ek]);});return _dk(_ej,function(_eo){return E(_en);});},_ep=unCStr("LF"),_eq=[0,10],_er=function(_es){var _et=new T(function(){return A(_es,[_eq]);});return _dk(_ep,function(_eu){return E(_et);});},_ev=unCStr("VT"),_ew=[0,11],_ex=function(_ey){var _ez=new T(function(){return A(_ey,[_ew]);});return _dk(_ev,function(_eA){return E(_ez);});},_eB=unCStr("FF"),_eC=[0,12],_eD=function(_eE){var _eF=new T(function(){return A(_eE,[_eC]);});return _dk(_eB,function(_eG){return E(_eF);});},_eH=unCStr("CR"),_eI=[0,13],_eJ=function(_eK){var _eL=new T(function(){return A(_eK,[_eI]);});return _dk(_eH,function(_eM){return E(_eL);});},_eN=unCStr("SI"),_eO=[0,15],_eP=function(_eQ){var _eR=new T(function(){return A(_eQ,[_eO]);});return _dk(_eN,function(_eS){return E(_eR);});},_eT=unCStr("DLE"),_eU=[0,16],_eV=function(_eW){var _eX=new T(function(){return A(_eW,[_eU]);});return _dk(_eT,function(_eY){return E(_eX);});},_eZ=unCStr("DC1"),_f0=[0,17],_f1=function(_f2){var _f3=new T(function(){return A(_f2,[_f0]);});return _dk(_eZ,function(_f4){return E(_f3);});},_f5=unCStr("DC2"),_f6=[0,18],_f7=function(_f8){var _f9=new T(function(){return A(_f8,[_f6]);});return _dk(_f5,function(_fa){return E(_f9);});},_fb=unCStr("DC3"),_fc=[0,19],_fd=function(_fe){var _ff=new T(function(){return A(_fe,[_fc]);});return _dk(_fb,function(_fg){return E(_ff);});},_fh=unCStr("DC4"),_fi=[0,20],_fj=function(_fk){var _fl=new T(function(){return A(_fk,[_fi]);});return _dk(_fh,function(_fm){return E(_fl);});},_fn=unCStr("NAK"),_fo=[0,21],_fp=function(_fq){var _fr=new T(function(){return A(_fq,[_fo]);});return _dk(_fn,function(_fs){return E(_fr);});},_ft=unCStr("SYN"),_fu=[0,22],_fv=function(_fw){var _fx=new T(function(){return A(_fw,[_fu]);});return _dk(_ft,function(_fy){return E(_fx);});},_fz=unCStr("ETB"),_fA=[0,23],_fB=function(_fC){var _fD=new T(function(){return A(_fC,[_fA]);});return _dk(_fz,function(_fE){return E(_fD);});},_fF=unCStr("CAN"),_fG=[0,24],_fH=function(_fI){var _fJ=new T(function(){return A(_fI,[_fG]);});return _dk(_fF,function(_fK){return E(_fJ);});},_fL=unCStr("EM"),_fM=[0,25],_fN=function(_fO){var _fP=new T(function(){return A(_fO,[_fM]);});return _dk(_fL,function(_fQ){return E(_fP);});},_fR=unCStr("SUB"),_fS=[0,26],_fT=function(_fU){var _fV=new T(function(){return A(_fU,[_fS]);});return _dk(_fR,function(_fW){return E(_fV);});},_fX=unCStr("ESC"),_fY=[0,27],_fZ=function(_g0){var _g1=new T(function(){return A(_g0,[_fY]);});return _dk(_fX,function(_g2){return E(_g1);});},_g3=unCStr("FS"),_g4=[0,28],_g5=function(_g6){var _g7=new T(function(){return A(_g6,[_g4]);});return _dk(_g3,function(_g8){return E(_g7);});},_g9=unCStr("GS"),_ga=[0,29],_gb=function(_gc){var _gd=new T(function(){return A(_gc,[_ga]);});return _dk(_g9,function(_ge){return E(_gd);});},_gf=unCStr("RS"),_gg=[0,30],_gh=function(_gi){var _gj=new T(function(){return A(_gi,[_gg]);});return _dk(_gf,function(_gk){return E(_gj);});},_gl=unCStr("US"),_gm=[0,31],_gn=function(_go){var _gp=new T(function(){return A(_go,[_gm]);});return _dk(_gl,function(_gq){return E(_gp);});},_gr=unCStr("SP"),_gs=[0,32],_gt=function(_gu){var _gv=new T(function(){return A(_gu,[_gs]);});return _dk(_gr,function(_gw){return E(_gv);});},_gx=unCStr("DEL"),_gy=[0,127],_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gy]);});return _dk(_gx,function(_gC){return E(_gB);});},_gD=[1,_gz,_3],_gE=[1,_gt,_gD],_gF=[1,_gn,_gE],_gG=[1,_gh,_gF],_gH=[1,_gb,_gG],_gI=[1,_g5,_gH],_gJ=[1,_fZ,_gI],_gK=[1,_fT,_gJ],_gL=[1,_fN,_gK],_gM=[1,_fH,_gL],_gN=[1,_fB,_gM],_gO=[1,_fv,_gN],_gP=[1,_fp,_gO],_gQ=[1,_fj,_gP],_gR=[1,_fd,_gQ],_gS=[1,_f7,_gR],_gT=[1,_f1,_gS],_gU=[1,_eV,_gT],_gV=[1,_eP,_gU],_gW=[1,_eJ,_gV],_gX=[1,_eD,_gW],_gY=[1,_ex,_gX],_gZ=[1,_er,_gY],_h0=[1,_el,_gZ],_h1=[1,_ef,_h0],_h2=[1,_e9,_h1],_h3=[1,_e3,_h2],_h4=[1,_dX,_h3],_h5=[1,_dR,_h4],_h6=[1,_dL,_h5],_h7=[1,_dF,_h6],_h8=[1,_dz,_h7],_h9=unCStr("SOH"),_ha=[0,1],_hb=function(_hc){var _hd=new T(function(){return A(_hc,[_ha]);});return _dk(_h9,function(_he){return E(_hd);});},_hf=unCStr("SO"),_hg=[0,14],_hh=function(_hi){var _hj=new T(function(){return A(_hi,[_hg]);});return _dk(_hf,function(_hk){return E(_hj);});},_hl=function(_hm){return _at(_hb,_hh,_hm);},_hn=[1,_hl,_h8],_ho=new T(function(){return _d8(_hn);}),_hp=[0,1114111],_hq=[0,34],_hr=[0,_hq,_cJ],_hs=[0,39],_ht=[0,_hs,_cJ],_hu=[0,92],_hv=[0,_hu,_cJ],_hw=[0,_e8,_cJ],_hx=[0,_ee,_cJ],_hy=[0,_eC,_cJ],_hz=[0,_eq,_cJ],_hA=[0,_eI,_cJ],_hB=[0,_ek,_cJ],_hC=[0,_ew,_cJ],_hD=[0,_dy,_cJ],_hE=[0,_ha,_cJ],_hF=[0,_dE,_cJ],_hG=[0,_dK,_cJ],_hH=[0,_dQ,_cJ],_hI=[0,_dW,_cJ],_hJ=[0,_e2,_cJ],_hK=[0,_e8,_cJ],_hL=[0,_ee,_cJ],_hM=[0,_ek,_cJ],_hN=[0,_eq,_cJ],_hO=[0,_ew,_cJ],_hP=[0,_eC,_cJ],_hQ=[0,_eI,_cJ],_hR=[0,_hg,_cJ],_hS=[0,_eO,_cJ],_hT=[0,_eU,_cJ],_hU=[0,_f0,_cJ],_hV=[0,_f6,_cJ],_hW=[0,_fc,_cJ],_hX=[0,_fi,_cJ],_hY=[0,_fo,_cJ],_hZ=[0,_fu,_cJ],_i0=[0,_fA,_cJ],_i1=[0,_fG,_cJ],_i2=[0,_fM,_cJ],_i3=[0,_fS,_cJ],_i4=[0,_fY,_cJ],_i5=[0,_g4,_cJ],_i6=[0,_ga,_cJ],_i7=[0,_gg,_cJ],_i8=[0,_gm,_cJ],_i9=function(_ia){return [0,_ia];},_ib=function(_ic){var _id=new T(function(){return A(_ic,[_hC]);}),_ie=new T(function(){return A(_ic,[_hB]);}),_if=new T(function(){return A(_ic,[_hA]);}),_ig=new T(function(){return A(_ic,[_hz]);}),_ih=new T(function(){return A(_ic,[_hy]);}),_ii=new T(function(){return A(_ic,[_hx]);}),_ij=new T(function(){return A(_ic,[_hw]);}),_ik=new T(function(){return A(_ic,[_hv]);}),_il=new T(function(){return A(_ic,[_ht]);}),_im=new T(function(){return A(_ic,[_hr]);});return _9k([0,function(_in){switch(E(E(_in)[1])){case 34:return E(_im);case 39:return E(_il);case 92:return E(_ik);case 97:return E(_ij);case 98:return E(_ii);case 102:return E(_ih);case 110:return E(_ig);case 114:return E(_if);case 116:return E(_ie);case 118:return E(_id);default:return [2];}}],new T(function(){return _9k(_at(_cK,_cR,function(_io){var _ip=new T(function(){return _i9(E(_io)[1]);});return _aS(_io,function(_iq){var _ir=_bS(_ip,_bJ,_iq);return !_cY(_ir,_hp)?[2]:A(_ic,[[0,new T(function(){var _is=_cV(_ir);return _is>>>0>1114111?_cT(_is):[0,_is];}),_cJ]]);});}),new T(function(){var _it=new T(function(){return A(_ic,[_i8]);}),_iu=new T(function(){return A(_ic,[_i7]);}),_iv=new T(function(){return A(_ic,[_i6]);}),_iw=new T(function(){return A(_ic,[_i5]);}),_ix=new T(function(){return A(_ic,[_i4]);}),_iy=new T(function(){return A(_ic,[_i3]);}),_iz=new T(function(){return A(_ic,[_i2]);}),_iA=new T(function(){return A(_ic,[_i1]);}),_iB=new T(function(){return A(_ic,[_i0]);}),_iC=new T(function(){return A(_ic,[_hZ]);}),_iD=new T(function(){return A(_ic,[_hY]);}),_iE=new T(function(){return A(_ic,[_hX]);}),_iF=new T(function(){return A(_ic,[_hW]);}),_iG=new T(function(){return A(_ic,[_hV]);}),_iH=new T(function(){return A(_ic,[_hU]);}),_iI=new T(function(){return A(_ic,[_hT]);}),_iJ=new T(function(){return A(_ic,[_hS]);}),_iK=new T(function(){return A(_ic,[_hR]);}),_iL=new T(function(){return A(_ic,[_hQ]);}),_iM=new T(function(){return A(_ic,[_hP]);}),_iN=new T(function(){return A(_ic,[_hO]);}),_iO=new T(function(){return A(_ic,[_hN]);}),_iP=new T(function(){return A(_ic,[_hM]);}),_iQ=new T(function(){return A(_ic,[_hL]);}),_iR=new T(function(){return A(_ic,[_hK]);}),_iS=new T(function(){return A(_ic,[_hJ]);}),_iT=new T(function(){return A(_ic,[_hI]);}),_iU=new T(function(){return A(_ic,[_hH]);}),_iV=new T(function(){return A(_ic,[_hG]);}),_iW=new T(function(){return A(_ic,[_hF]);}),_iX=new T(function(){return A(_ic,[_hE]);}),_iY=new T(function(){return A(_ic,[_hD]);});return _9k([0,function(_iZ){return E(E(_iZ)[1])==94?E([0,function(_j0){switch(E(E(_j0)[1])){case 64:return E(_iY);case 65:return E(_iX);case 66:return E(_iW);case 67:return E(_iV);case 68:return E(_iU);case 69:return E(_iT);case 70:return E(_iS);case 71:return E(_iR);case 72:return E(_iQ);case 73:return E(_iP);case 74:return E(_iO);case 75:return E(_iN);case 76:return E(_iM);case 77:return E(_iL);case 78:return E(_iK);case 79:return E(_iJ);case 80:return E(_iI);case 81:return E(_iH);case 82:return E(_iG);case 83:return E(_iF);case 84:return E(_iE);case 85:return E(_iD);case 86:return E(_iC);case 87:return E(_iB);case 88:return E(_iA);case 89:return E(_iz);case 90:return E(_iy);case 91:return E(_ix);case 92:return E(_iw);case 93:return E(_iv);case 94:return E(_iu);case 95:return E(_it);default:return [2];}}]):[2];}],new T(function(){return A(_ho,[function(_j1){return A(_ic,[[0,_j1,_cJ]]);}]);}));}));}));},_j2=function(_j3){return A(_j3,[_0]);},_j4=function(_j5){var _j6=E(_j5);if(!_j6[0]){return E(_j2);}else{var _j7=_j6[2],_j8=E(E(_j6[1])[1]);switch(_j8){case 9:var _j9=new T(function(){return _j4(_j7);});return function(_ja){var _jb=new T(function(){return A(_j9,[_ja]);});return [0,function(_jc){return E(_jb);}];};case 10:var _jd=new T(function(){return _j4(_j7);});return function(_je){var _jf=new T(function(){return A(_jd,[_je]);});return [0,function(_jg){return E(_jf);}];};case 11:var _jh=new T(function(){return _j4(_j7);});return function(_ji){var _jj=new T(function(){return A(_jh,[_ji]);});return [0,function(_jk){return E(_jj);}];};case 12:var _jl=new T(function(){return _j4(_j7);});return function(_jm){var _jn=new T(function(){return A(_jl,[_jm]);});return [0,function(_jo){return E(_jn);}];};case 13:var _jp=new T(function(){return _j4(_j7);});return function(_jq){var _jr=new T(function(){return A(_jp,[_jq]);});return [0,function(_js){return E(_jr);}];};case 32:var _jt=new T(function(){return _j4(_j7);});return function(_ju){var _jv=new T(function(){return A(_jt,[_ju]);});return [0,function(_jw){return E(_jv);}];};case 160:var _jx=new T(function(){return _j4(_j7);});return function(_jy){var _jz=new T(function(){return A(_jx,[_jy]);});return [0,function(_jA){return E(_jz);}];};default:var _jB=u_iswspace(_j8);if(!E(_jB)){return E(_j2);}else{var _jC=new T(function(){return _j4(_j7);});return function(_jD){var _jE=new T(function(){return A(_jC,[_jD]);});return [0,function(_jF){return E(_jE);}];};}}}},_jG=function(_jH){var _jI=new T(function(){return _ib(_jH);}),_jJ=new T(function(){return _jG(_jH);}),_jK=[1,function(_jL){return A(_j4,[_jL,function(_jM){return E([0,function(_jN){return E(E(_jN)[1])==92?E(_jJ):[2];}]);}]);}];return _9k([0,function(_jO){return E(E(_jO)[1])==92?E([0,function(_jP){var _jQ=E(E(_jP)[1]);switch(_jQ){case 9:return E(_jK);case 10:return E(_jK);case 11:return E(_jK);case 12:return E(_jK);case 13:return E(_jK);case 32:return E(_jK);case 38:return E(_jJ);case 160:return E(_jK);default:var _jR=u_iswspace(_jQ);return E(_jR)==0?[2]:E(_jK);}}]):[2];}],[0,function(_jS){var _jT=E(_jS);return E(_jT[1])==92?E(_jI):A(_jH,[[0,_jT,_cI]]);}]);},_jU=function(_jV,_jW){var _jX=new T(function(){return A(_jW,[[1,new T(function(){return A(_jV,[_3]);})]]);});return _jG(function(_jY){var _jZ=E(_jY),_k0=E(_jZ[1]);return E(_k0[1])==34?!E(_jZ[2])?E(_jX):_jU(function(_k1){return A(_jV,[[1,_k0,_k1]]);},_jW):_jU(function(_k2){return A(_jV,[[1,_k0,_k2]]);},_jW);});},_k3=unCStr("_\'"),_k4=function(_k5){var _k6=u_iswalnum(_k5);return E(_k6)==0?_cm(_9R,[0,_k5],_k3):true;},_k7=function(_k8){return _k4(E(_k8)[1]);},_k9=unCStr(",;()[]{}`"),_ka=function(_kb){return A(_kb,[_3]);},_kc=function(_kd,_ke){var _kf=function(_kg){var _kh=E(_kg);if(!_kh[0]){return E(_ka);}else{var _ki=_kh[1];if(!A(_kd,[_ki])){return E(_ka);}else{var _kj=new T(function(){return _kf(_kh[2]);});return function(_kk){var _kl=new T(function(){return A(_kj,[function(_km){return A(_kk,[[1,_ki,_km]]);}]);});return [0,function(_kn){return E(_kl);}];};}}};return [1,function(_ko){return A(_kf,[_ko,_ke]);}];},_kp=unCStr(".."),_kq=unCStr("::"),_kr=unCStr("->"),_ks=[0,64],_kt=[1,_ks,_3],_ku=[0,126],_kv=[1,_ku,_3],_kw=unCStr("=>"),_kx=[1,_kw,_3],_ky=[1,_kv,_kx],_kz=[1,_kt,_ky],_kA=[1,_kr,_kz],_kB=unCStr("<-"),_kC=[1,_kB,_kA],_kD=[0,124],_kE=[1,_kD,_3],_kF=[1,_kE,_kC],_kG=[1,_hu,_3],_kH=[1,_kG,_kF],_kI=[0,61],_kJ=[1,_kI,_3],_kK=[1,_kJ,_kH],_kL=[1,_kq,_kK],_kM=[1,_kp,_kL],_kN=function(_kO){var _kP=new T(function(){return A(_kO,[_aP]);});return _9k([1,function(_kQ){return E(_kQ)[0]==0?E(_kP):[2];}],new T(function(){var _kR=new T(function(){return _ib(function(_kS){var _kT=E(_kS);return (function(_kU,_kV){var _kW=new T(function(){return A(_kO,[[0,_kU]]);});return !E(_kV)?E(E(_kU)[1])==39?[2]:[0,function(_kX){return E(E(_kX)[1])==39?E(_kW):[2];}]:[0,function(_kY){return E(E(_kY)[1])==39?E(_kW):[2];}];})(_kT[1],_kT[2]);});});return _9k([0,function(_kZ){return E(E(_kZ)[1])==39?E([0,function(_l0){var _l1=E(_l0);switch(E(_l1[1])){case 39:return [2];case 92:return E(_kR);default:var _l2=new T(function(){return A(_kO,[[0,_l1]]);});return [0,function(_l3){return E(E(_l3)[1])==39?E(_l2):[2];}];}}]):[2];}],new T(function(){var _l4=new T(function(){return _jU(_2o,_kO);});return _9k([0,function(_l5){return E(E(_l5)[1])==34?E(_l4):[2];}],new T(function(){return _9k([0,function(_l6){return !_cm(_9R,_l6,_k9)?[2]:A(_kO,[[2,[1,_l6,_3]]]);}],new T(function(){return _9k([0,function(_l7){return !_cm(_9R,_l7,_cr)?[2]:_kc(_cs,function(_l8){var _l9=[1,_l7,_l8];return !_cm(_a8,_l9,_kM)?A(_kO,[[4,_l9]]):A(_kO,[[2,_l9]]);});}],new T(function(){return _9k([0,function(_la){var _lb=E(_la),_lc=_lb[1],_ld=u_iswalpha(_lc);return E(_ld)==0?E(_lc)==95?_kc(_k7,function(_le){return A(_kO,[[3,[1,_lb,_le]]]);}):[2]:_kc(_k7,function(_lf){return A(_kO,[[3,[1,_lb,_lf]]]);});}],new T(function(){return _at(_cw,_ch,_kO);}));}));}));}));}));}));},_lg=function(_lh){var _li=new T(function(){return _kN(_lh);});return [1,function(_lj){return A(_j4,[_lj,function(_lk){return E(_li);}]);}];},_ll=[0,0],_lm=function(_ln,_lo){var _lp=new T(function(){return A(_ln,[_ll,function(_lq){var _lr=new T(function(){return A(_lo,[_lq]);});return _lg(function(_ls){var _lt=E(_ls);if(_lt[0]==2){var _lu=E(_lt[1]);return _lu[0]==0?[2]:E(E(_lu[1])[1])==41?E(_lu[2])[0]==0?E(_lr):[2]:[2];}else{return [2];}});}]);});return _lg(function(_lv){var _lw=E(_lv);if(_lw[0]==2){var _lx=E(_lw[1]);return _lx[0]==0?[2]:E(E(_lx[1])[1])==40?E(_lx[2])[0]==0?E(_lp):[2]:[2];}else{return [2];}});},_ly=function(_lz,_lA,_lB){var _lC=function(_lD,_lE){var _lF=new T(function(){return _kN(function(_lG){return A(_lz,[_lG,_lD,function(_lH){return A(_lE,[new T(function(){return [0, -E(_lH)[1]];})]);}]);});});return _9k(_lg(function(_lI){var _lJ=E(_lI);if(_lJ[0]==4){var _lK=E(_lJ[1]);return _lK[0]==0?A(_lz,[_lJ,_lD,_lE]):E(E(_lK[1])[1])==45?E(_lK[2])[0]==0?E([1,function(_lL){return A(_j4,[_lL,function(_lM){return E(_lF);}]);}]):A(_lz,[_lJ,_lD,_lE]):A(_lz,[_lJ,_lD,_lE]);}else{return A(_lz,[_lJ,_lD,_lE]);}}),new T(function(){return _lm(_lC,_lE);}));};return _lC(_lA,_lB);},_lN=function(_lO,_lP){return [2];},_lQ=function(_lR,_lS){return _lN(_lR,_lS);},_lT=function(_lU){var _lV=E(_lU);return _lV[0]==0?[1,new T(function(){return _bS(new T(function(){return _i9(E(_lV[1])[1]);}),_bJ,_lV[2]);})]:E(_lV[2])[0]==0?E(_lV[3])[0]==0?[1,new T(function(){return _bS(_bI,_bJ,_lV[1]);})]:[0]:[0];},_lW=function(_lX){var _lY=E(_lX);if(_lY[0]==5){var _lZ=_lT(_lY[1]);if(!_lZ[0]){return E(_lN);}else{var _m0=new T(function(){return [0,_cV(_lZ[1])];});return function(_m1,_m2){return A(_m2,[_m0]);};}}else{return E(_lQ);}},_m3=function(_lR,_lS){return _ly(_lW,_lR,_lS);},_m4=function(_m5,_m6){var _m7=function(_m8,_m9){var _ma=new T(function(){return A(_m9,[_3]);}),_mb=new T(function(){return A(_m5,[_ll,function(_mc){return _m7(_cJ,function(_md){return A(_m9,[[1,_mc,_md]]);});}]);});return _lg(function(_me){var _mf=E(_me);if(_mf[0]==2){var _mg=E(_mf[1]);if(!_mg[0]){return [2];}else{var _mh=_mg[2];switch(E(E(_mg[1])[1])){case 44:return E(_mh)[0]==0?!E(_m8)?[2]:E(_mb):[2];case 93:return E(_mh)[0]==0?E(_ma):[2];default:return [2];}}}else{return [2];}});},_mi=function(_mj){var _mk=new T(function(){return _9k(_m7(_cI,_mj),new T(function(){return A(_m5,[_ll,function(_ml){return _m7(_cJ,function(_mm){return A(_mj,[[1,_ml,_mm]]);});}]);}));});return _9k(_lg(function(_mn){var _mo=E(_mn);if(_mo[0]==2){var _mp=E(_mo[1]);return _mp[0]==0?[2]:E(E(_mp[1])[1])==91?E(_mp[2])[0]==0?E(_mk):[2]:[2];}else{return [2];}}),new T(function(){return _lm(function(_mq,_mr){return _mi(_mr);},_mj);}));};return _mi(_m6);},_ms=function(_mt,_mu){return _m4(_m3,_mu);},_mv=new T(function(){return _m4(_m3,_al);}),_mw=function(_lS){return _9a(_mv,_lS);},_mx=function(_my){var _mz=new T(function(){return _ly(_lW,_my,_al);});return function(_aO){return _9a(_mz,_aO);};},_mA=[0,_mx,_mw,_m3,_ms],_mB=function(_mC){return _o(0,E(_mC)[1],_3);},_mD=function(_mE,_mF){return _o(0,E(_mE)[1],_mF);},_mG=function(_mH,_mI){return _1U(_mD,_mH,_mI);},_mJ=function(_mK,_mL,_mM){return _o(E(_mK)[1],E(_mL)[1],_mM);},_mN=[0,_mJ,_mB,_mG],_mO=unCStr("GHC.Types"),_mP=unCStr("Int"),_mQ=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_59,_mO,_mP],_mR=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_mQ,_3],_mS=function(_mT){return E(_mR);},_mU=function(_mV){return E(E(_mV)[1]);},_mW=function(_mX,_mY){return A(_mX,[function(_){return jsFind(toJSStr(E(_mY)));}]);},_mZ=function(_n0){return E(E(_n0)[4]);},_n1=unCStr("[]"),_n2=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_59,_mO,_n1],_n3=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_n2,_3],_n4=function(_n5){return E(_n3);},_n6=unCStr("Char"),_n7=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_59,_mO,_n6],_n8=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_n7,_3],_n9=function(_na){return E(_n8);},_nb=new T(function(){return _63(_n4,_n9);}),_nc=new T(function(){return A(_nb,[_62]);}),_nd=new T(function(){return E(_62);}),_ne=function(_nf){return E(E(_nf)[7]);},_ng=function(_nh){return E(E(_nh)[1]);},_ni=[0,0],_nj=[0,32],_nk=[0,10],_nl=function(_nm){var _nn=E(_nm);if(!_nn[0]){return E(_2o);}else{var _no=_nn[1],_np=E(_nn[2]);if(!_np[0]){return _nq(_nk,_no);}else{var _nr=new T(function(){return _nl(_np);}),_ns=new T(function(){return _nq(_nk,_no);});return function(_nt){return A(_ns,[[1,_nj,new T(function(){return A(_nr,[_nt]);})]]);};}}},_nu=unCStr("->"),_nv=[1,_nu,_3],_nw=[1,_mO,_nv],_nx=[1,_59,_nw],_ny=[0,32],_nz=function(_nA){var _nB=E(_nA);if(!_nB[0]){return [0];}else{var _nC=_nB[1],_nD=E(_nB[2]);return _nD[0]==0?E(_nC):_e(_nC,[1,_ny,new T(function(){return _nz(_nD);})]);}},_nE=new T(function(){return _nz(_nx);}),_nF=new T(function(){var _nG=_5x(_nE);return [0,_nG[1],_nG[2],_59,_mO,_nu];}),_nH=function(_nI,_nJ){var _nK=E(_nI);return _nK[0]==0?E(_nJ):A(_nK[1],[new T(function(){return _nH(_nK[2],_nJ);})]);},_nL=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_nM=[1,_5b,_3],_nN=function(_nO){var _nP=E(_nO);if(!_nP[0]){return [0];}else{var _nQ=E(_nP[1]);return [1,[0,_nQ[1],_nQ[2]],new T(function(){return _nN(_nP[2]);})];}},_nR=new T(function(){var _nS=_e(_3,_nM);if(!_nS[0]){return E(_n2);}else{var _nT=_5x(new T(function(){return _5l(_5J(_5U,[1,_nL,new T(function(){return _nN(_nS);})]));}));return E(_n2);}}),_nU=[0,40],_nV=function(_nW){return _nq(_nk,_nW);},_nX=[0,8],_nY=unCStr(" -> "),_nZ=[0,9],_o0=[0,93],_o1=[0,91],_o2=[0,41],_o3=[0,44],_o4=function(_nW){return [1,_o3,_nW];},_o5=function(_o6,_o7){var _o8=E(_o7);return _o8[0]==0?[0]:[1,_o6,[1,_o8[1],new T(function(){return _o5(_o6,_o8[2]);})]];},_nq=function(_o9,_oa){var _ob=E(_oa),_oc=_ob[3],_od=E(_ob[4]);if(!_od[0]){return function(_oe){return _e(E(_oc)[5],_oe);};}else{var _of=_od[1],_og=new T(function(){var _oh=E(_oc)[5],_oi=new T(function(){return _nl(_od);}),_oj=new T(function(){return E(_o9)[1]<=9?function(_ok){return _e(_oh,[1,_nj,new T(function(){return A(_oi,[_ok]);})]);}:function(_ol){return [1,_n,new T(function(){return _e(_oh,[1,_nj,new T(function(){return A(_oi,[[1,_m,_ol]]);})]);})];};}),_om=E(_oh);if(!_om[0]){return E(_oj);}else{if(E(E(_om[1])[1])==40){var _on=E(_om[2]);return _on[0]==0?E(_oj):E(E(_on[1])[1])==44?function(_oo){return [1,_nU,new T(function(){return A(new T(function(){var _op=_5J(_nV,_od);if(!_op[0]){return E(_2o);}else{var _oq=new T(function(){return _o5(_o4,_op[2]);});return function(_aO){return _nH([1,_op[1],_oq],_aO);};}}),[[1,_o2,_oo]]);})];}:E(_oj);}else{return E(_oj);}}}),_or=E(_od[2]);if(!_or[0]){var _os=E(_oc),_ot=E(_nR),_ou=hs_eqWord64(_os[1],_ot[1]);if(!E(_ou)){return E(_og);}else{var _ov=hs_eqWord64(_os[2],_ot[2]);if(!E(_ov)){return E(_og);}else{var _ow=new T(function(){return _nq(_ni,_of);});return function(_ox){return [1,_o1,new T(function(){return A(_ow,[[1,_o0,_ox]]);})];};}}}else{if(!E(_or[2])[0]){var _oy=E(_oc),_oz=E(_nF),_oA=hs_eqWord64(_oy[1],_oz[1]);if(!E(_oA)){return E(_og);}else{var _oB=hs_eqWord64(_oy[2],_oz[2]);if(!E(_oB)){return E(_og);}else{var _oC=new T(function(){return _nq(_nX,_or[1]);}),_oD=new T(function(){return _nq(_nZ,_of);});return E(_o9)[1]<=8?function(_oE){return A(_oD,[new T(function(){return _e(_nY,new T(function(){return A(_oC,[_oE]);}));})]);}:function(_oF){return [1,_n,new T(function(){return A(_oD,[new T(function(){return _e(_nY,new T(function(){return A(_oC,[[1,_m,_oF]]);}));})]);})];};}}}else{return E(_og);}}}},_oG=function(_oH,_oI,_oJ,_oK){var _oL=new T(function(){return _2S(_oH);}),_oM=new T(function(){return _mZ(_oK);}),_oN=new T(function(){return _ne(_oK);}),_oO=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_nq,[_ni,A(_oI,[_nd]),_3]);}));}),_oP=new T(function(){return A(_ng,[_oJ,_1]);});return function(_oQ){if(!E(new T(function(){var _oR=A(_oI,[_nd]),_oS=E(_nc),_oT=hs_eqWord64(_oR[1],_oS[1]);if(!E(_oT)){return false;}else{var _oU=hs_eqWord64(_oR[2],_oS[2]);return E(_oU)==0?false:true;}}))){var _oV=new T(function(){return A(_oL,[[1,_oQ,new T(function(){return A(_oN,[new T(function(){return A(_oM,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _e(_oQ,_oO);}));})]);})]);})]]);}),_oW=A(_oP,[_oQ]);if(!_oW[0]){return E(_oV);}else{var _oX=E(_oW[1]);return E(_oX[2])[0]==0?E(_oW[2])[0]==0?A(_oL,[[2,_oX[1]]]):E(_oV):E(_oV);}}else{return A(_oL,[[2,_oQ]]);}};},_oY=[0],_oZ=new T(function(){return [0,"value"];}),_p0=function(_p1,_p2,_p3,_p4,_p5,_p6){var _p7=E(_p1),_p8=_p7[1],_p9=new T(function(){return A(_p7[3],[_oY]);}),_pa=new T(function(){return _oG(_p7,_p3,_p4,_p5);});return A(_p8,[new T(function(){return _mW(_p2,_p6);}),function(_pb){var _pc=E(_pb);return _pc[0]==0?E(_p9):A(_p8,[new T(function(){return A(_p2,[function(_){var _pd=jsGet(E(_pc[1])[1],E(_oZ)[1]);return [1,new T(function(){return fromJSStr(_pd);})];}]);}),function(_pe){var _pf=E(_pe);return _pf[0]==0?E(_p9):A(_pa,[_pf[1]]);}]);}]);},_pg=1,_ph=function(_pi){return E(E(_pi)[10]);},_pj=function(_pk){return E(E(_pk)[2]);},_pl=function(_pm){return E(E(_pm)[2]);},_pn=function(_po,_pp,_pq,_pr,_ps){var _pt=new T(function(){return _mU(_po);}),_pu=new T(function(){return _pj(_pt);}),_pv=new T(function(){return _2Z(_pp);}),_pw=new T(function(){return _2U(_pv);}),_px=new T(function(){return _3d([0,coercionToken],_pw,function(_py){return _3v(_pv,_py);},function(_pz,_pA){return _3E(_pv,_pz,_pA);});}),_pB=new T(function(){return _2S(_pv);}),_pC=new T(function(){return _2s(_pv);}),_pD=new T(function(){return _2S(_pv);}),_pE=new T(function(){return _2s(_pv);}),_pF=new T(function(){return _2S(_pv);}),_pG=new T(function(){return _2s(_pv);}),_pH=new T(function(){return _2S(_pv);}),_pI=new T(function(){return _2s(_pv);}),_pJ=new T(function(){return _pl(_pr);}),_pK=new T(function(){return _ph(_po);});return function(_pL,_pM,_pN){return function(_pO){return A(_pI,[new T(function(){var _pP=E(_pL);return _pP[0]==0?A(_px,[_pO]):A(_pH,[[0,_pP[1],_pO]]);}),function(_pQ){var _pR=new T(function(){return E(E(_pQ)[1]);}),_pS=new T(function(){return _p0(_pw,function(_pT){return _33(_pp,_pT);},_pq,_ps,_po,_pR);}),_pU=new T(function(){return A(_pK,[_pR,_pM,new T(function(){var _pV=E(_pN);if(!_pV[0]){return [0];}else{var _pW=_pV[1],_pX=_S(_pq,_nb,_pW);return _pX[0]==0?A(_pJ,[_pW]):E(_pX[1]);}}),_cI,_t]);});return A(_pG,[new T(function(){var _pY=new T(function(){return E(E(_pQ)[2]);});return A(_pF,[[0,_pY,_pY]]);}),function(_pZ){return A(_pE,[new T(function(){return A(_pD,[[0,_0,new T(function(){var _q0=E(E(_pZ)[1]);return [0,_q0[1],_q0[2],_pg,_q0[4]];})]]);}),function(_q1){return A(_pC,[new T(function(){return A(_pS,[new T(function(){return E(E(_q1)[2]);})]);}),function(_q2){var _q3=E(_q2),_q4=_q3[2],_q5=E(_q3[1]);switch(_q5[0]){case 0:return A(_pB,[[0,[0,_pU,_t],_q4]]);case 1:return A(_pB,[[0,[0,new T(function(){return A(_pu,[new T(function(){return A(_pK,[_pR,_pM,_q5[1],_cI,_t]);}),_q5[2]]);}),_t],_q4]]);default:var _q6=_q5[1];return A(_pB,[[0,[0,new T(function(){return A(_pK,[_pR,_pM,new T(function(){var _q7=_S(_pq,_nb,_q6);return _q7[0]==0?A(_pJ,[_q6]):E(_q7[1]);}),_cI,_t]);}),[1,_q6]],_q4]]);}}]);}]);}]);}]);};};},_q8=new T(function(){return _pn(_8h,_2q,_mS,_mN,_mA);}),_q9=new T(function(){return A(_q8,[_t,_50,_t]);}),_qa=new T(function(){return A(_q8,[_t,_50,_t]);}),_qb=function(_qc,_){var _qd=A(_qa,[_qc,_]),_qe=E(_qd),_qf=_qe[2],_qg=E(_qe[1]),_qh=A(_q9,[_qf,_]),_qi=E(_qh),_qj=E(_qi[1]),_qk=new T(function(){return E(E(_qf)[4]);}),_ql=new T(function(){return E(E(_qc)[4]);});return [0,[0,function(_qm,_){var _qn=_4N(_qg[1],_4g,_ql,_qm,_),_qo=_4W(_qm,_),_qp=_4N(_qj[1],_4g,_qk,_qm,_),_qq=_4W(_qm,_);return _qm;},new T(function(){var _qr=E(_qg[2]);if(!_qr[0]){return [0];}else{var _qs=E(_qj[2]);return _qs[0]==0?[0]:[1,new T(function(){return _4h(_qr[1],_qs[1]);})];}})],_qi[2]];},_qt=function(_qu,_qv){var _qw=new T(function(){return _2Z(_qv);}),_qx=new T(function(){return _3d([0,coercionToken],_2U(_qw),function(_qy){return _3v(_qw,_qy);},function(_qz,_qA){return _3E(_qw,_qz,_qA);});}),_qB=new T(function(){return _2S(_qw);}),_qC=new T(function(){return _2s(_qw);}),_qD=new T(function(){return _2s(_qw);}),_qE=new T(function(){return _2s(_qw);});return function(_qF,_qG){return A(_qE,[new T(function(){return A(_qx,[_qG]);}),function(_qH){var _qI=new T(function(){return E(E(_qH)[1]);});return A(_qD,[new T(function(){return A(_33,[_qv,function(_){return jsFind(toJSStr(E(_qI)));},new T(function(){return E(E(_qH)[2]);})]);}),function(_qJ){return A(_qC,[new T(function(){return A(_qF,[new T(function(){return E(E(_qJ)[2]);})]);}),function(_qK){var _qL=E(_qK),_qM=_qL[2],_qN=E(_qL[1]),_qO=_qN[2];if(!E(E(_qJ)[1])[0]){var _qP=new T(function(){return A(_qu,[_qN[1]]);});return A(_qB,[[0,[0,function(_qQ,_){var _qR=_3z(_3y,_qQ,_),_qS=A(_3p,[_2o,_qR,_2r,_qI,_]),_qT=A(_qP,[_qR,_]);return _qR;},_qO],_qM]]);}else{return A(_qB,[[0,[0,_2l,_qO],_qM]]);}}]);}]);}]);};},_qU=function(_qV){return E(_qV);},_qW=new T(function(){return _qt(_qU,_2q);}),_qX=new T(function(){return A(_qW,[_qb]);}),_qY=function(_qZ,_){var _r0=A(_qX,[_qZ,_]),_r1=E(_r0),_r2=_r1[2],_r3=E(_r1[1]),_r4=_r3[1],_r5=E(_r3[2]);if(!_r5[0]){return [0,[0,_r4,_t],_r2];}else{var _r6=new T(function(){return _o(0,E(_r5[1])[1],_3);}),_r7=A(_4f,[function(_r8,_){return [0,[0,function(_r9,_){var _ra=_3z(_v,_r9,_),_rb=_9(_r6,_ra,_);return _r9;},_t],_r8];},_r2,_]),_rc=E(_r7),_rd=E(_rc[1]);return [0,[0,function(_re,_){var _rf=A(_r4,[_re,_]),_rg=A(_rd[1],[_re,_]);return _re;},_rd[2]],_rc[2]];}},_rh=unCStr("idelem"),_ri=unCStr(" could be found!"),_rj=function(_rk){return err(unAppCStr("No element with ID ",new T(function(){return _e(_rk,_ri);})));},_rl=function(_){var _rm=E(_rh),_rn=jsFind(toJSStr(_rm)),_ro=E(_rn);if(!_ro[0]){return _rj(_rm);}else{var _rp=_4(_qY,_ro[1],_);return _0;}},_rq=function(_){return _rl(_);};
var hasteMain = function() {A(_rq, [0]);};window.onload = hasteMain;