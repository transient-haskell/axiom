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

var _0=unCStr("id"),_1=0,_2=function(_3,_4,_5,_6){return A(_3,[new T(function(){return function(_){var _7=jsSetAttr(E(_4)[1],toJSStr(E(_5)),toJSStr(E(_6)));return _1;};})]);},_8=[0],_9=function(_a){return E(_a);},_b=unCStr("span"),_c=function(_d,_e,_){var _f=A(_d,[_]);return A(_e,[_]);},_g=function(_h,_i,_){return _c(_h,_i,_);},_j=function(_k,_l,_){var _m=A(_k,[_]);return A(_l,[_m,_]);},_n=unCStr("base"),_o=unCStr("GHC.IO.Exception"),_p=unCStr("IOException"),_q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_n,_o,_p],_r=[0],_s=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_q,_r],_t=function(_u){return E(_s);},_v=function(_w){return E(E(_w)[1]);},_x=unCStr("Maybe.fromJust: Nothing"),_y=new T(function(){return err(_x);}),_z=function(_A,_B,_C){var _D=new T(function(){var _E=A(_A,[_C]),_F=A(_B,[new T(function(){var _G=E(_D);return _G[0]==0?E(_y):E(_G[1]);})]),_H=hs_eqWord64(_E[1],_F[1]);if(!E(_H)){return [0];}else{var _I=hs_eqWord64(_E[2],_F[2]);return E(_I)==0?[0]:[1,_C];}});return E(_D);},_J=function(_K){var _L=E(_K);return _z(_v(_L[1]),_t,_L[2]);},_M=unCStr(": "),_N=[0,41],_O=unCStr(" ("),_P=function(_Q,_R){var _S=E(_Q);return _S[0]==0?E(_R):[1,_S[1],new T(function(){return _P(_S[2],_R);})];},_T=unCStr("already exists"),_U=unCStr("does not exist"),_V=unCStr("protocol error"),_W=unCStr("failed"),_X=unCStr("invalid argument"),_Y=unCStr("inappropriate type"),_Z=unCStr("hardware fault"),_10=unCStr("unsupported operation"),_11=unCStr("timeout"),_12=unCStr("resource vanished"),_13=unCStr("interrupted"),_14=unCStr("resource busy"),_15=unCStr("resource exhausted"),_16=unCStr("end of file"),_17=unCStr("illegal operation"),_18=unCStr("permission denied"),_19=unCStr("user error"),_1a=unCStr("unsatisified constraints"),_1b=unCStr("system error"),_1c=function(_1d,_1e){switch(E(_1d)){case 0:return _P(_T,_1e);case 1:return _P(_U,_1e);case 2:return _P(_14,_1e);case 3:return _P(_15,_1e);case 4:return _P(_16,_1e);case 5:return _P(_17,_1e);case 6:return _P(_18,_1e);case 7:return _P(_19,_1e);case 8:return _P(_1a,_1e);case 9:return _P(_1b,_1e);case 10:return _P(_V,_1e);case 11:return _P(_W,_1e);case 12:return _P(_X,_1e);case 13:return _P(_Y,_1e);case 14:return _P(_Z,_1e);case 15:return _P(_10,_1e);case 16:return _P(_11,_1e);case 17:return _P(_12,_1e);default:return _P(_13,_1e);}},_1f=[0,125],_1g=unCStr("{handle: "),_1h=function(_1i,_1j,_1k,_1l,_1m,_1n){var _1o=new T(function(){var _1p=new T(function(){return _1c(_1j,new T(function(){var _1q=E(_1l);return _1q[0]==0?E(_1n):_P(_O,new T(function(){return _P(_1q,[1,_N,_1n]);}));}));}),_1r=E(_1k);return _1r[0]==0?E(_1p):_P(_1r,new T(function(){return _P(_M,_1p);}));}),_1s=E(_1m);if(!_1s[0]){var _1t=E(_1i);if(!_1t[0]){return E(_1o);}else{var _1u=E(_1t[1]);return _1u[0]==0?_P(_1g,new T(function(){return _P(_1u[1],[1,_1f,new T(function(){return _P(_M,_1o);})]);})):_P(_1g,new T(function(){return _P(_1u[1],[1,_1f,new T(function(){return _P(_M,_1o);})]);}));}}else{return _P(_1s[1],new T(function(){return _P(_M,_1o);}));}},_1v=function(_1w){var _1x=E(_1w);return _1h(_1x[1],_1x[2],_1x[3],_1x[4],_1x[6],_r);},_1y=function(_1z,_1A){var _1B=E(_1z);return _1h(_1B[1],_1B[2],_1B[3],_1B[4],_1B[6],_1A);},_1C=[0,44],_1D=[0,93],_1E=[0,91],_1F=function(_1G,_1H,_1I){var _1J=E(_1H);return _1J[0]==0?unAppCStr("[]",_1I):[1,_1E,new T(function(){return A(_1G,[_1J[1],new T(function(){var _1K=function(_1L){var _1M=E(_1L);return _1M[0]==0?E([1,_1D,_1I]):[1,_1C,new T(function(){return A(_1G,[_1M[1],new T(function(){return _1K(_1M[2]);})]);})];};return _1K(_1J[2]);})]);})];},_1N=function(_1O,_1P){return _1F(_1y,_1O,_1P);},_1Q=function(_1R,_1S,_1T){var _1U=E(_1S);return _1h(_1U[1],_1U[2],_1U[3],_1U[4],_1U[6],_1T);},_1V=[0,_1Q,_1v,_1N],_1W=new T(function(){return [0,_t,_1V,_1X,_J];}),_1X=function(_1Y){return [0,_1W,_1Y];},_1Z=7,_20=function(_21){return [0,_8,_1Z,_r,_21,_8,_8];},_22=function(_23,_){return die(new T(function(){return _1X(new T(function(){return _20(_23);}));}));},_24=function(_25,_){return _22(_25,_);},_26=function(_27,_){return _27;},_28=[0,_j,_g,_26,_24],_29=function(_2a){return E(E(_2a)[1]);},_2b=function(_2c,_2d,_2e,_2f){return A(_29,[_2c,new T(function(){return A(_2d,[_2f]);}),function(_2g){return A(_2e,[new T(function(){return E(E(_2g)[1]);}),new T(function(){return E(E(_2g)[2]);})]);}]);},_2h=function(_2i,_2j,_2k,_2l){return A(_29,[_2i,new T(function(){return A(_2j,[_2l]);}),function(_2m){return A(_2k,[new T(function(){return E(E(_2m)[2]);})]);}]);},_2n=function(_2o,_2p,_2q,_2r){return _2h(_2o,_2p,_2q,_2r);},_2s=function(_2t){return E(E(_2t)[4]);},_2u=function(_2v,_2w){var _2x=new T(function(){return A(_2s,[_2v,_2w]);});return function(_2y){return E(_2x);};},_2z=function(_2A){return E(E(_2A)[3]);},_2B=function(_2C){var _2D=new T(function(){return _2z(_2C);});return [0,function(_2p,_2q,_2r){return _2b(_2C,_2p,_2q,_2r);},function(_2p,_2q,_2r){return _2n(_2C,_2p,_2q,_2r);},function(_2E,_2F){return A(_2D,[[0,_2E,_2F]]);},function(_2r){return _2u(_2C,_2r);}];},_2G=new T(function(){return _2B(_28);}),_2H=function(_2I,_2J){var _2K=jsShowI(_2I);return _P(fromJSStr(_2K),_2J);},_2L=[0,41],_2M=[0,40],_2N=function(_2O,_2P,_2Q){return _2P>=0?_2H(_2P,_2Q):_2O<=6?_2H(_2P,_2Q):[1,_2M,new T(function(){var _2R=jsShowI(_2P);return _P(fromJSStr(_2R),[1,_2L,_2Q]);})];},_2S=[0,112],_2T=function(_2U,_2V,_2W,_2X){var _2Y=E(_2V);return A(_2Y[1],[new T(function(){var _2Z=E(_2U);return E(_2W);}),function(_30){var _31=new T(function(){return E(E(_30)[2]);});return A(_2Y[2],[new T(function(){return A(_2X,[new T(function(){var _32=E(new T(function(){var _33=E(_2U);return [0,coercionToken];})),_34=E(_30);return [0,_34[1],new T(function(){return [0,E(_31)[1]+1|0];}),_34[3],_34[4],_34[5]];})]);}),new T(function(){return A(_2Y[3],[[1,_2S,new T(function(){return _P(_2N(0,E(_31)[1],_r),new T(function(){return E(E(_30)[1]);}));})]]);})]);}]);},_35=[0,coercionToken],_36=function(_37,_38,_){return [0,_1,_37];},_39=function(_3a,_){return [0,_3a,_3a];},_3b=new T(function(){return _2T(_35,_2G,_39,_36);}),_3c=[0,0],_3d=2,_3e=unCStr(" could be found!"),_3f=function(_3g){return err(unAppCStr("No element with ID ",new T(function(){return _P(_3g,_3e);})));},_3h=function(_3i,_3j,_){var _3k=E(_3i),_3l=jsFind(toJSStr(_3k)),_3m=E(_3l);return _3m[0]==0?_3f(_3k):A(_3j,[_3m[1],_]);},_3n=function(_3o,_){var _3p=E(_3o);if(!_3p[0]){return _r;}else{var _3q=E(_3p[1]),_3r=_3h(_3q[1],_3q[2],_),_3s=_3n(_3p[2],_);return [1,_3r,_3s];}},_3t=function(_3u,_3v,_){var _3w=A(_3u,[[0,_r,_3c,_3d,function(_){return _3t(_3u,_3v,_);},_r],_]),_3x=E(_3w),_3y=A(E(_3x[1])[1],[_3v,_]),_3z=_3n(E(_3x[2])[5],_);return _1;},_3A=function(_3B,_3C,_){var _3D=E(_3C),_3E=jsFind(toJSStr(_3D)),_3F=E(_3E);if(!_3F[0]){return _3f(_3D);}else{var _3G=E(_3F[1]),_3H=jsClearChildren(_3G[1]);return _3t(_3B,_3G,_);}},_3I=function(_3J,_3K,_3L,_){var _3M=A(_3b,[_3L,_]),_3N=new T(function(){return E(E(_3M)[1]);}),_3O=A(_3J,[new T(function(){var _3P=E(E(_3M)[2]);return [0,_3P[1],_3P[2],_3P[3],function(_){return _3A(function(_3Q,_){return _3I(function(_3R,_){var _3S=A(_3J,[_3P,_]),_3T=E(_3S);return [0,[0,_26,E(_3T[1])[2]],_3T[2]];},_3K,_3Q,_);},_3N,_);},_3P[5]];}),_]),_3U=E(_3O),_3V=_3U[2],_3W=E(_3U[1]),_3X=_3W[1],_3Y=E(_3W[2]);if(!_3Y[0]){return [0,[0,function(_3Z,_){var _40=A(_3X,[_3Z,_]),_41=E(_3N),_42=jsFind(toJSStr(_41));if(!E(_42)[0]){var _43=jsCreateElem(toJSStr(E(_b))),_44=A(_2,[_9,[0,_43],_0,_41,_]),_45=E(_3Z),_46=jsAppendChild(_43,_45[1]);return _45;}else{return _3Z;}},_8],_3V];}else{var _47=A(_3K,[_3Y[1],_3V,_]),_48=E(_47),_49=E(_48[1]),_4a=_49[1];return [0,[0,function(_4b,_){var _4c=A(_3X,[_4b,_]),_4d=E(_3N),_4e=jsFind(toJSStr(_4d));if(!E(_4e)[0]){var _4f=jsCreateElem(toJSStr(E(_b))),_4g=A(_2,[_9,[0,_4f],_0,_4d,_]),_4h=E(_4b),_4i=jsAppendChild(_4f,_4h[1]),_4j=A(_4a,[_4h,_]);return _4h;}else{var _4k=A(_4a,[_4b,_]);return _4b;}},_49[2]],_48[2]];}},_4l=function(_4m,_4n,_){var _4o=jsCreateTextNode(toJSStr(E(_4m))),_4p=jsAppendChild(_4o,E(_4n)[1]);return [0,_4o];},_4q=function(_4r,_4s,_){var _4t=jsWriteHandle(E(_4r)[1],toJSStr(E(_4s)));return _1;},_4u=[0,10],_4v=[1,_4u,_r],_4w=function(_4x,_4y,_){var _4z=E(_4x),_4A=jsWriteHandle(_4z[1],toJSStr(E(_4y)));return _4q(_4z,_4v,_);},_4B=[0,98],_4C=[1,_4B,_r],_4D=unCStr("br"),_4E=function(_4F,_){var _4G=jsCreateElem(toJSStr(E(_4D))),_4H=jsAppendChild(_4G,E(_4F)[1]);return [0,_4G];},_4I=[1,_1],_4J=unCStr("result: "),_4K=function(_4L,_4M,_){var _4N=jsCreateElem(toJSStr(E(_4L))),_4O=jsAppendChild(_4N,E(_4M)[1]);return [0,_4N];},_4P=function(_){var _=0,_4Q=jsMkStdout();return [0,_4Q];},_4R=function(_4S){var _4T=A(_4S,[_]);return E(_4T);},_4U=new T(function(){return _4R(_4P);}),_4V=function(_4W){var _4X=new T(function(){return _2N(0,E(_4W)[1],_r);}),_4Y=new T(function(){return _2N(0,E(_4W)[1],_r);});return function(_4Z,_50){return _3I(function(_51,_){var _52=_4w(_4U,_4Y,_);return [0,[0,_26,[1,_52]],_51];},function(_53,_54,_){return (function(_55,_){return [0,[0,function(_56,_){var _57=_4E(_56,_),_58=_4l(_4J,_56,_),_59=_4K(_4C,_56,_),_5a=_4l(_4X,_59,_);return _56;},_4I],_55];})(_54,_);},_4Z,_50);};},_5b=[13,coercionToken],_5c=unCStr("true"),_5d=unCStr("hasevent"),_5e=function(_5f,_5g){while(1){var _5h=E(_5f);if(!_5h[0]){return E(_5g)[0]==0?true:false;}else{var _5i=E(_5g);if(!_5i[0]){return false;}else{if(E(_5h[1])[1]!=E(_5i[1])[1]){return false;}else{_5f=_5h[2];_5g=_5i[2];continue;}}}}},_5j=new T(function(){return [0,"keydown"];}),_5k=new T(function(){return [0,"mousemove"];}),_5l=new T(function(){return [0,"blur"];}),_5m=new T(function(){return [0,"focus"];}),_5n=new T(function(){return [0,"change"];}),_5o=new T(function(){return [0,"unload"];}),_5p=new T(function(){return [0,"load"];}),_5q=new T(function(){return [0,"keyup"];}),_5r=new T(function(){return [0,"keypress"];}),_5s=new T(function(){return [0,"mouseup"];}),_5t=new T(function(){return [0,"mousedown"];}),_5u=new T(function(){return [0,"dblclick"];}),_5v=new T(function(){return [0,"click"];}),_5w=new T(function(){return [0,"mouseout"];}),_5x=new T(function(){return [0,"mouseover"];}),_5y=function(_5z){switch(E(_5z)[0]){case 0:return E(_5p);case 1:return E(_5o);case 2:return E(_5n);case 3:return E(_5m);case 4:return E(_5l);case 5:return E(_5k);case 6:return E(_5x);case 7:return E(_5w);case 8:return E(_5v);case 9:return E(_5u);case 10:return E(_5t);case 11:return E(_5s);case 12:return E(_5r);case 13:return E(_5q);default:return E(_5j);}},_5A=function(_5B,_5C,_5D,_5E,_){var _5F=A(_5B,[_5E,_]),_5G=E(_5F),_5H=_5G[1],_5I=E(_5d),_5J=jsGetAttr(_5H,toJSStr(_5I));if(!_5e(fromJSStr(_5J),_5c)){var _5K=E(_5D),_5L=jsSetCB(_5H,_5y(_5C)[1],_5D),_5M=A(_2,[_9,_5G,_5I,_5c,_]);return _5G;}else{return _5G;}},_5N=unCStr("text"),_5O=function(_5P,_5Q,_5R,_){var _5S=_4K(_5P,_5R,_),_5T=A(_5Q,[_5S,_]);return _5S;},_5U=unCStr("()"),_5V=unCStr("GHC.Tuple"),_5W=unCStr("ghc-prim"),_5X=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5W,_5V,_5U],_5Y=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5X,_r],_5Z=function(_60){return E(_5Y);},_61=unCStr("main"),_62=unCStr("Builder"),_63=unCStr("JSBuilderM"),_64=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_61,_62,_63],_65=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_64,_r],_66=function(_67){return E(_65);},_68=function(_69){var _6a=E(_69);return _6a[0]==0?[0]:_P(_6a[1],new T(function(){return _68(_6a[2]);}));},_6b=function(_6c,_6d){var _6e=E(_6c);if(!_6e){return [0,_r,_6d];}else{var _6f=E(_6d);if(!_6f[0]){return [0,_r,_r];}else{var _6g=new T(function(){var _6h=_6b(_6e-1|0,_6f[2]);return [0,_6h[1],_6h[2]];});return [0,[1,_6f[1],new T(function(){return E(E(_6g)[1]);})],new T(function(){return E(E(_6g)[2]);})];}}},_6i=[0,120],_6j=[0,48],_6k=function(_6l){var _6m=new T(function(){var _6n=_6b(8,new T(function(){var _6o=md5(toJSStr(E(_6l)));return fromJSStr(_6o);}));return [0,_6n[1],_6n[2]];}),_6p=parseInt([0,toJSStr([1,_6j,[1,_6i,new T(function(){return E(E(_6m)[1]);})]])]),_6q=new T(function(){var _6r=_6b(8,new T(function(){return E(E(_6m)[2]);}));return [0,_6r[1],_6r[2]];}),_6s=parseInt([0,toJSStr([1,_6j,[1,_6i,new T(function(){return E(E(_6q)[1]);})]])]),_6t=hs_mkWord64(_6p,_6s),_6u=parseInt([0,toJSStr([1,_6j,[1,_6i,new T(function(){return E(_6b(8,new T(function(){return E(E(_6q)[2]);}))[1]);})]])]),_6v=hs_mkWord64(_6u,_6u);return [0,_6t,_6v];},_6w=function(_6x,_6y){var _6z=E(_6y);return _6z[0]==0?[0]:[1,new T(function(){return A(_6x,[_6z[1]]);}),new T(function(){return _6w(_6x,_6z[2]);})];},_6A=function(_6B,_6C){var _6D=jsShowI(_6B),_6E=md5(_6D);return _P(fromJSStr(_6E),new T(function(){var _6F=jsShowI(_6C),_6G=md5(_6F);return fromJSStr(_6G);}));},_6H=function(_6I){var _6J=E(_6I);return _6A(_6J[1],_6J[2]);},_6K=function(_6L){var _6M=E(_6L);if(!_6M[0]){return [0];}else{var _6N=E(_6M[1]);return [1,[0,_6N[1],_6N[2]],new T(function(){return _6K(_6M[2]);})];}},_6O=unCStr("Prelude.undefined"),_6P=new T(function(){return err(_6O);}),_6Q=function(_6R,_6S){return function(_6T){return E(new T(function(){var _6U=A(_6R,[_6P]),_6V=E(_6U[3]),_6W=_6V[1],_6X=_6V[2],_6Y=_P(_6U[4],[1,new T(function(){return A(_6S,[_6P]);}),_r]);if(!_6Y[0]){return [0,_6W,_6X,_6V,_r];}else{var _6Z=_6k(new T(function(){return _68(_6w(_6H,[1,[0,_6W,_6X],new T(function(){return _6K(_6Y);})]));}));return [0,_6Z[1],_6Z[2],_6V,_6Y];}}));};},_70=new T(function(){return _6Q(_66,_5Z);}),_71=function(_72,_73,_74,_){var _75=E(_73),_76=A(_72,[_74,_]),_77=A(_2,[_9,_76,_75[1],_75[2],_]);return _76;},_78=function(_79,_7a){while(1){var _7b=(function(_7c,_7d){var _7e=E(_7d);if(!_7e[0]){return E(_7c);}else{_79=function(_3Q,_){return _71(_7c,_7e[1],_3Q,_);};_7a=_7e[2];return null;}})(_79,_7a);if(_7b!=null){return _7b;}}},_7f=unCStr("value"),_7g=unCStr("onclick"),_7h=unCStr("checked"),_7i=[0,_7h,_r],_7j=[1,_7i,_r],_7k=unCStr("type"),_7l=unCStr("input"),_7m=function(_7n,_){return _4K(_7l,_7n,_);},_7o=function(_7p,_7q,_7r,_7s,_7t){var _7u=new T(function(){var _7v=new T(function(){return _78(_7m,[1,[0,_7k,_7q],[1,[0,_0,_7p],[1,[0,_7f,_7r],_r]]]);});return !E(_7s)?E(_7v):_78(_7v,_7j);}),_7w=E(_7t);return _7w[0]==0?E(_7u):_78(_7u,[1,[0,_7g,_7w[1]],_r]);},_7x=unCStr("href"),_7y=[0,97],_7z=[1,_7y,_r],_7A=function(_7B,_){return _4K(_7z,_7B,_);},_7C=function(_7D,_7E){var _7F=new T(function(){return _78(_7A,[1,[0,_7x,_7D],_r]);});return function(_7G,_){var _7H=A(_7F,[_7G,_]),_7I=A(_7E,[_7H,_]);return _7H;};},_7J=function(_7K){return _7C(_7K,function(_3Q,_){return _4l(_7K,_3Q,_);});},_7L=unCStr("option"),_7M=function(_7N,_){return _4K(_7L,_7N,_);},_7O=unCStr("selected"),_7P=[0,_7O,_r],_7Q=[1,_7P,_r],_7R=function(_7S,_7T,_7U){var _7V=new T(function(){return _78(_7M,[1,[0,_7f,_7S],_r]);}),_7W=function(_7X,_){var _7Y=A(_7V,[_7X,_]),_7Z=A(_7T,[_7Y,_]);return _7Y;};return !E(_7U)?E(_7W):_78(_7W,_7Q);},_80=function(_81,_82){return _7R(_81,function(_3Q,_){return _4l(_81,_3Q,_);},_82);},_83=unCStr("method"),_84=unCStr("action"),_85=unCStr("UTF-8"),_86=unCStr("acceptCharset"),_87=[0,_86,_85],_88=unCStr("form"),_89=function(_8a,_){return _4K(_88,_8a,_);},_8b=function(_8c,_8d,_8e){var _8f=new T(function(){return _78(_89,[1,_87,[1,[0,_84,_8c],[1,[0,_83,_8d],_r]]]);});return function(_8g,_){var _8h=A(_8f,[_8g,_]),_8i=A(_8e,[_8h,_]);return _8h;};},_8j=unCStr("select"),_8k=function(_8l,_){return _4K(_8j,_8l,_);},_8m=function(_8n,_8o){var _8p=new T(function(){return _78(_8k,[1,[0,_0,_8n],_r]);});return function(_8q,_){var _8r=A(_8p,[_8q,_]),_8s=A(_8o,[_8r,_]);return _8r;};},_8t=unCStr("textarea"),_8u=function(_8v,_){return _4K(_8t,_8v,_);},_8w=function(_8x,_8y){var _8z=new T(function(){return _78(_8u,[1,[0,_0,_8x],_r]);});return function(_8A,_){var _8B=A(_8z,[_8A,_]),_8C=_4l(_8y,_8B,_);return _8B;};},_8D=unCStr("color:red"),_8E=unCStr("style"),_8F=[0,_8E,_8D],_8G=[1,_8F,_r],_8H=[0,98],_8I=[1,_8H,_r],_8J=function(_8K){return _78(function(_8L,_){var _8M=_4K(_8I,_8L,_),_8N=A(_8K,[_8M,_]);return _8M;},_8G);},_8O=unCStr("toByteString not defined"),_8P=new T(function(){return err(_8O);}),_8Q=function(_8R,_8S,_){var _8T=E(_8R);if(!_8T[0]){return _8S;}else{var _8U=A(_8T[1],[_8S,_]),_8V=_8Q(_8T[2],_8S,_);return _8S;}},_8W=function(_8X,_8Y,_8Z,_){var _90=A(_8X,[_8Z,_]),_91=A(_8Y,[_8Z,_]);return _8Z;},_92=[0,_26,_8W,_8Q],_93=[0,_92,_70,_8P,_4l,_4l,_5O,_8J,_7C,_7J,_7o,_8w,_8m,_7R,_80,_8b,_78],_94=[0,_28,_9],_95=unCStr("base"),_96=unCStr("Control.Exception.Base"),_97=unCStr("PatternMatchFail"),_98=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_95,_96,_97],_99=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_98,_r],_9a=function(_9b){return E(_99);},_9c=function(_9d){var _9e=E(_9d);return _z(_v(_9e[1]),_9a,_9e[2]);},_9f=function(_9g){return E(E(_9g)[1]);},_9h=function(_9i,_9j){return _P(E(_9i)[1],_9j);},_9k=function(_9l,_9m){return _1F(_9h,_9l,_9m);},_9n=function(_9o,_9p,_9q){return _P(E(_9p)[1],_9q);},_9r=[0,_9n,_9f,_9k],_9s=new T(function(){return [0,_9a,_9r,_9t,_9c];}),_9t=function(_9u){return [0,_9s,_9u];},_9v=unCStr("Non-exhaustive patterns in"),_9w=function(_9x,_9y){return die(new T(function(){return A(_9y,[_9x]);}));},_9z=function(_9A,_9B){var _9C=E(_9B);if(!_9C[0]){return [0,_r,_r];}else{var _9D=_9C[1];if(!A(_9A,[_9D])){return [0,_r,_9C];}else{var _9E=new T(function(){var _9F=_9z(_9A,_9C[2]);return [0,_9F[1],_9F[2]];});return [0,[1,_9D,new T(function(){return E(E(_9E)[1]);})],new T(function(){return E(E(_9E)[2]);})];}}},_9G=[0,32],_9H=[0,10],_9I=[1,_9H,_r],_9J=function(_9K){return E(E(_9K)[1])==124?false:true;},_9L=function(_9M,_9N){var _9O=_9z(_9J,unCStr(_9M)),_9P=_9O[1],_9Q=function(_9R,_9S){return _P(_9R,new T(function(){return unAppCStr(": ",new T(function(){return _P(_9N,new T(function(){return _P(_9S,_9I);}));}));}));},_9T=E(_9O[2]);return _9T[0]==0?_9Q(_9P,_r):E(E(_9T[1])[1])==124?_9Q(_9P,[1,_9G,_9T[2]]):_9Q(_9P,_r);},_9U=function(_9V){return _9w([0,new T(function(){return _9L(_9V,_9v);})],_9t);},_9W=new T(function(){return _9U("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_9X=function(_9Y,_9Z){while(1){var _a0=(function(_a1,_a2){var _a3=E(_a1);switch(_a3[0]){case 0:var _a4=E(_a2);if(!_a4[0]){return [0];}else{_9Y=A(_a3[1],[_a4[1]]);_9Z=_a4[2];return null;}break;case 1:var _a5=A(_a3[1],[_a2]),_a6=_a2;_9Y=_a5;_9Z=_a6;return null;case 2:return [0];case 3:return [1,[0,_a3[1],_a2],new T(function(){return _9X(_a3[2],_a2);})];default:return E(_a3[1]);}})(_9Y,_9Z);if(_a0!=null){return _a0;}}},_a7=function(_a8,_a9){var _aa=new T(function(){var _ab=E(_a9);if(_ab[0]==3){return [3,_ab[1],new T(function(){return _a7(_a8,_ab[2]);})];}else{var _ac=E(_a8);if(_ac[0]==2){return E(_ab);}else{var _ad=E(_ab);if(_ad[0]==2){return E(_ac);}else{var _ae=new T(function(){var _af=E(_ad);if(_af[0]==4){return [1,function(_ag){return [4,new T(function(){return _P(_9X(_ac,_ag),_af[1]);})];}];}else{var _ah=E(_ac);if(_ah[0]==1){var _ai=_ah[1],_aj=E(_af);return _aj[0]==0?[1,function(_ak){return _a7(A(_ai,[_ak]),_aj);}]:[1,function(_al){return _a7(A(_ai,[_al]),new T(function(){return A(_aj[1],[_al]);}));}];}else{var _am=E(_af);return _am[0]==0?E(_9W):[1,function(_an){return _a7(_ah,new T(function(){return A(_am[1],[_an]);}));}];}}}),_ao=E(_ac);switch(_ao[0]){case 1:var _ap=E(_ad);return _ap[0]==4?[1,function(_aq){return [4,new T(function(){return _P(_9X(A(_ao[1],[_aq]),_aq),_ap[1]);})];}]:E(_ae);case 4:var _ar=_ao[1],_as=E(_ad);switch(_as[0]){case 0:return [1,function(_at){return [4,new T(function(){return _P(_ar,new T(function(){return _9X(_as,_at);}));})];}];case 1:return [1,function(_au){return [4,new T(function(){return _P(_ar,new T(function(){return _9X(A(_as[1],[_au]),_au);}));})];}];default:return [4,new T(function(){return _P(_ar,_as[1]);})];}break;default:return E(_ae);}}}}}),_av=E(_a8);switch(_av[0]){case 0:var _aw=E(_a9);return _aw[0]==0?[0,function(_ax){return _a7(A(_av[1],[_ax]),new T(function(){return A(_aw[1],[_ax]);}));}]:E(_aa);case 3:return [3,_av[1],new T(function(){return _a7(_av[2],_a9);})];default:return E(_aa);}},_ay=function(_az,_aA){return E(_az)[1]!=E(_aA)[1];},_aB=function(_aC,_aD){return E(_aC)[1]==E(_aD)[1];},_aE=[0,_aB,_ay],_aF=function(_aG){return E(E(_aG)[1]);},_aH=function(_aI,_aJ,_aK){while(1){var _aL=E(_aJ);if(!_aL[0]){return E(_aK)[0]==0?true:false;}else{var _aM=E(_aK);if(!_aM[0]){return false;}else{if(!A(_aF,[_aI,_aL[1],_aM[1]])){return false;}else{_aJ=_aL[2];_aK=_aM[2];continue;}}}}},_aN=function(_aO,_aP,_aQ){return !_aH(_aO,_aP,_aQ)?true:false;},_aR=function(_aS){return [0,function(_aT,_aU){return _aH(_aS,_aT,_aU);},function(_aT,_aU){return _aN(_aS,_aT,_aU);}];},_aV=new T(function(){return _aR(_aE);}),_aW=function(_aX,_aY){var _aZ=E(_aX);switch(_aZ[0]){case 0:return [0,function(_b0){return _aW(A(_aZ[1],[_b0]),_aY);}];case 1:return [1,function(_b1){return _aW(A(_aZ[1],[_b1]),_aY);}];case 2:return [2];case 3:return _a7(A(_aY,[_aZ[1]]),new T(function(){return _aW(_aZ[2],_aY);}));default:var _b2=function(_b3){var _b4=E(_b3);if(!_b4[0]){return [0];}else{var _b5=E(_b4[1]);return _P(_9X(A(_aY,[_b5[1]]),_b5[2]),new T(function(){return _b2(_b4[2]);}));}},_b6=_b2(_aZ[1]);return _b6[0]==0?[2]:[4,_b6];}},_b7=[2],_b8=function(_b9){return [3,_b9,_b7];},_ba=function(_bb,_bc){var _bd=E(_bb);if(!_bd){return A(_bc,[_1]);}else{var _be=new T(function(){return _ba(_bd-1|0,_bc);});return [0,function(_bf){return E(_be);}];}},_bg=function(_bh,_bi,_bj){var _bk=new T(function(){return A(_bh,[_b8]);});return [1,function(_bl){return A(function(_bm,_bn,_bo){while(1){var _bp=(function(_bq,_br,_bs){var _bt=E(_bq);switch(_bt[0]){case 0:var _bu=E(_br);if(!_bu[0]){return E(_bi);}else{_bm=A(_bt[1],[_bu[1]]);_bn=_bu[2];var _bv=_bs+1|0;_bo=_bv;return null;}break;case 1:var _bw=A(_bt[1],[_br]),_bx=_br,_bv=_bs;_bm=_bw;_bn=_bx;_bo=_bv;return null;case 2:return E(_bi);case 3:return function(_by){var _bz=new T(function(){return _aW(_bt,_by);});return _ba(_bs,function(_bA){return E(_bz);});};default:return function(_4Z){return _aW(_bt,_4Z);};}})(_bm,_bn,_bo);if(_bp!=null){return _bp;}}},[_bk,_bl,0,_bj]);}];},_bB=[6],_bC=unCStr("valDig: Bad base"),_bD=new T(function(){return err(_bC);}),_bE=function(_bF,_bG){var _bH=function(_bI,_bJ){var _bK=E(_bI);if(!_bK[0]){var _bL=new T(function(){return A(_bJ,[_r]);});return function(_bM){return A(_bM,[_bL]);};}else{var _bN=E(_bK[1])[1],_bO=function(_bP){var _bQ=new T(function(){return _bH(_bK[2],function(_bR){return A(_bJ,[[1,_bP,_bR]]);});});return function(_bS){var _bT=new T(function(){return A(_bQ,[_bS]);});return [0,function(_bU){return E(_bT);}];};};switch(E(E(_bF)[1])){case 8:if(48>_bN){var _bV=new T(function(){return A(_bJ,[_r]);});return function(_bW){return A(_bW,[_bV]);};}else{if(_bN>55){var _bX=new T(function(){return A(_bJ,[_r]);});return function(_bY){return A(_bY,[_bX]);};}else{return _bO([0,_bN-48|0]);}}break;case 10:if(48>_bN){var _bZ=new T(function(){return A(_bJ,[_r]);});return function(_c0){return A(_c0,[_bZ]);};}else{if(_bN>57){var _c1=new T(function(){return A(_bJ,[_r]);});return function(_c2){return A(_c2,[_c1]);};}else{return _bO([0,_bN-48|0]);}}break;case 16:var _c3=new T(function(){return 97>_bN?65>_bN?[0]:_bN>70?[0]:[1,[0,(_bN-65|0)+10|0]]:_bN>102?65>_bN?[0]:_bN>70?[0]:[1,[0,(_bN-65|0)+10|0]]:[1,[0,(_bN-97|0)+10|0]];});if(48>_bN){var _c4=E(_c3);if(!_c4[0]){var _c5=new T(function(){return A(_bJ,[_r]);});return function(_c6){return A(_c6,[_c5]);};}else{return _bO(_c4[1]);}}else{if(_bN>57){var _c7=E(_c3);if(!_c7[0]){var _c8=new T(function(){return A(_bJ,[_r]);});return function(_c9){return A(_c9,[_c8]);};}else{return _bO(_c7[1]);}}else{return _bO([0,_bN-48|0]);}}break;default:return E(_bD);}}};return [1,function(_ca){return A(_bH,[_ca,_9,function(_cb){var _cc=E(_cb);return _cc[0]==0?[2]:A(_bG,[_cc]);}]);}];},_cd=[0,10],_ce=[0,1],_cf=[0,2147483647],_cg=function(_ch,_ci){while(1){var _cj=E(_ch);if(!_cj[0]){var _ck=_cj[1],_cl=E(_ci);if(!_cl[0]){var _cm=_cl[1],_cn=addC(_ck,_cm);if(!E(_cn[2])){return [0,_cn[1]];}else{_ch=[1,I_fromInt(_ck)];_ci=[1,I_fromInt(_cm)];continue;}}else{_ch=[1,I_fromInt(_ck)];_ci=_cl;continue;}}else{var _co=E(_ci);if(!_co[0]){_ch=_cj;_ci=[1,I_fromInt(_co[1])];continue;}else{return [1,I_add(_cj[1],_co[1])];}}}},_cp=new T(function(){return _cg(_cf,_ce);}),_cq=function(_cr){var _cs=E(_cr);if(!_cs[0]){var _ct=E(_cs[1]);return _ct==(-2147483648)?E(_cp):[0, -_ct];}else{return [1,I_negate(_cs[1])];}},_cu=[0,10],_cv=[0,0],_cw=function(_cx,_cy){while(1){var _cz=E(_cx);if(!_cz[0]){var _cA=_cz[1],_cB=E(_cy);if(!_cB[0]){var _cC=_cB[1];if(!(imul(_cA,_cC)|0)){return [0,imul(_cA,_cC)|0];}else{_cx=[1,I_fromInt(_cA)];_cy=[1,I_fromInt(_cC)];continue;}}else{_cx=[1,I_fromInt(_cA)];_cy=_cB;continue;}}else{var _cD=E(_cy);if(!_cD[0]){_cx=_cz;_cy=[1,I_fromInt(_cD[1])];continue;}else{return [1,I_mul(_cz[1],_cD[1])];}}}},_cE=function(_cF,_cG,_cH){while(1){var _cI=E(_cH);if(!_cI[0]){return E(_cG);}else{var _cJ=_cg(_cw(_cG,_cF),_cI[1]);_cH=_cI[2];_cG=_cJ;continue;}}},_cK=function(_cL){var _cM=new T(function(){return _a7(_a7([0,function(_cN){return E(E(_cN)[1])==45?_bE(_cd,function(_cO){return A(_cL,[[1,new T(function(){return _cq(_cE(_cu,_cv,_cO));})]]);}):[2];}],[0,function(_cP){return E(E(_cP)[1])==43?_bE(_cd,function(_cQ){return A(_cL,[[1,new T(function(){return _cE(_cu,_cv,_cQ);})]]);}):[2];}]),new T(function(){return _bE(_cd,function(_cR){return A(_cL,[[1,new T(function(){return _cE(_cu,_cv,_cR);})]]);});}));});return _a7([0,function(_cS){return E(E(_cS)[1])==101?E(_cM):[2];}],[0,function(_cT){return E(E(_cT)[1])==69?E(_cM):[2];}]);},_cU=function(_cV){return A(_cV,[_8]);},_cW=function(_cX){return A(_cX,[_8]);},_cY=function(_cZ){var _d0=new T(function(){return _bE(_cd,function(_d1){return A(_cZ,[[1,_d1]]);});});return [0,function(_d2){return E(E(_d2)[1])==46?E(_d0):[2];}];},_d3=function(_d4){return _bE(_cd,function(_d5){return _bg(_cY,_cU,function(_d6){return _bg(_cK,_cW,function(_d7){return A(_d4,[[5,[1,_d5,_d6,_d7]]]);});});});},_d8=function(_d9,_da,_db){while(1){var _dc=E(_db);if(!_dc[0]){return false;}else{if(!A(_aF,[_d9,_da,_dc[1]])){_db=_dc[2];continue;}else{return true;}}}},_dd=unCStr("!@#$%&*+./<=>?\\^|:-~"),_de=function(_df){return _d8(_aE,_df,_dd);},_dg=[0,8],_dh=[0,16],_di=function(_dj){var _dk=new T(function(){return _bE(_dh,function(_dl){return A(_dj,[[5,[0,_dh,_dl]]]);});}),_dm=new T(function(){return _bE(_dg,function(_dn){return A(_dj,[[5,[0,_dg,_dn]]]);});}),_do=new T(function(){return _bE(_dh,function(_dp){return A(_dj,[[5,[0,_dh,_dp]]]);});}),_dq=new T(function(){return _bE(_dg,function(_dr){return A(_dj,[[5,[0,_dg,_dr]]]);});});return [0,function(_ds){return E(E(_ds)[1])==48?E([0,function(_dt){switch(E(E(_dt)[1])){case 79:return E(_dq);case 88:return E(_do);case 111:return E(_dm);case 120:return E(_dk);default:return [2];}}]):[2];}];},_du=false,_dv=true,_dw=function(_dx){var _dy=new T(function(){return A(_dx,[_dh]);}),_dz=new T(function(){return A(_dx,[_dg]);}),_dA=new T(function(){return A(_dx,[_dh]);}),_dB=new T(function(){return A(_dx,[_dg]);});return [0,function(_dC){switch(E(E(_dC)[1])){case 79:return E(_dB);case 88:return E(_dA);case 111:return E(_dz);case 120:return E(_dy);default:return [2];}}];},_dD=function(_dE){return A(_dE,[_cd]);},_dF=function(_dG){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _2N(9,_dG,_r);})));},_dH=function(_dI){var _dJ=E(_dI);return _dJ[0]==0?E(_dJ[1]):I_toInt(_dJ[1]);},_dK=function(_dL,_dM){var _dN=E(_dL);if(!_dN[0]){var _dO=_dN[1],_dP=E(_dM);return _dP[0]==0?_dO<=_dP[1]:I_compareInt(_dP[1],_dO)>=0;}else{var _dQ=_dN[1],_dR=E(_dM);return _dR[0]==0?I_compareInt(_dQ,_dR[1])<=0:I_compare(_dQ,_dR[1])<=0;}},_dS=function(_dT){return [2];},_dU=function(_dV){var _dW=E(_dV);if(!_dW[0]){return E(_dS);}else{var _dX=_dW[1],_dY=E(_dW[2]);if(!_dY[0]){return E(_dX);}else{var _dZ=new T(function(){return _dU(_dY);});return function(_e0){return _a7(A(_dX,[_e0]),new T(function(){return A(_dZ,[_e0]);}));};}}},_e1=unCStr("NUL"),_e2=function(_e3){return [2];},_e4=function(_e5){return _e2(_e5);},_e6=function(_e7,_e8){var _e9=function(_ea,_eb){var _ec=E(_ea);if(!_ec[0]){return function(_ed){return A(_ed,[_e7]);};}else{var _ee=E(_eb);if(!_ee[0]){return E(_e2);}else{if(E(_ec[1])[1]!=E(_ee[1])[1]){return E(_e4);}else{var _ef=new T(function(){return _e9(_ec[2],_ee[2]);});return function(_eg){var _eh=new T(function(){return A(_ef,[_eg]);});return [0,function(_ei){return E(_eh);}];};}}}};return [1,function(_ej){return A(_e9,[_e7,_ej,_e8]);}];},_ek=[0,0],_el=function(_em){var _en=new T(function(){return A(_em,[_ek]);});return _e6(_e1,function(_eo){return E(_en);});},_ep=unCStr("STX"),_eq=[0,2],_er=function(_es){var _et=new T(function(){return A(_es,[_eq]);});return _e6(_ep,function(_eu){return E(_et);});},_ev=unCStr("ETX"),_ew=[0,3],_ex=function(_ey){var _ez=new T(function(){return A(_ey,[_ew]);});return _e6(_ev,function(_eA){return E(_ez);});},_eB=unCStr("EOT"),_eC=[0,4],_eD=function(_eE){var _eF=new T(function(){return A(_eE,[_eC]);});return _e6(_eB,function(_eG){return E(_eF);});},_eH=unCStr("ENQ"),_eI=[0,5],_eJ=function(_eK){var _eL=new T(function(){return A(_eK,[_eI]);});return _e6(_eH,function(_eM){return E(_eL);});},_eN=unCStr("ACK"),_eO=[0,6],_eP=function(_eQ){var _eR=new T(function(){return A(_eQ,[_eO]);});return _e6(_eN,function(_eS){return E(_eR);});},_eT=unCStr("BEL"),_eU=[0,7],_eV=function(_eW){var _eX=new T(function(){return A(_eW,[_eU]);});return _e6(_eT,function(_eY){return E(_eX);});},_eZ=unCStr("BS"),_f0=[0,8],_f1=function(_f2){var _f3=new T(function(){return A(_f2,[_f0]);});return _e6(_eZ,function(_f4){return E(_f3);});},_f5=unCStr("HT"),_f6=[0,9],_f7=function(_f8){var _f9=new T(function(){return A(_f8,[_f6]);});return _e6(_f5,function(_fa){return E(_f9);});},_fb=unCStr("LF"),_fc=[0,10],_fd=function(_fe){var _ff=new T(function(){return A(_fe,[_fc]);});return _e6(_fb,function(_fg){return E(_ff);});},_fh=unCStr("VT"),_fi=[0,11],_fj=function(_fk){var _fl=new T(function(){return A(_fk,[_fi]);});return _e6(_fh,function(_fm){return E(_fl);});},_fn=unCStr("FF"),_fo=[0,12],_fp=function(_fq){var _fr=new T(function(){return A(_fq,[_fo]);});return _e6(_fn,function(_fs){return E(_fr);});},_ft=unCStr("CR"),_fu=[0,13],_fv=function(_fw){var _fx=new T(function(){return A(_fw,[_fu]);});return _e6(_ft,function(_fy){return E(_fx);});},_fz=unCStr("SI"),_fA=[0,15],_fB=function(_fC){var _fD=new T(function(){return A(_fC,[_fA]);});return _e6(_fz,function(_fE){return E(_fD);});},_fF=unCStr("DLE"),_fG=[0,16],_fH=function(_fI){var _fJ=new T(function(){return A(_fI,[_fG]);});return _e6(_fF,function(_fK){return E(_fJ);});},_fL=unCStr("DC1"),_fM=[0,17],_fN=function(_fO){var _fP=new T(function(){return A(_fO,[_fM]);});return _e6(_fL,function(_fQ){return E(_fP);});},_fR=unCStr("DC2"),_fS=[0,18],_fT=function(_fU){var _fV=new T(function(){return A(_fU,[_fS]);});return _e6(_fR,function(_fW){return E(_fV);});},_fX=unCStr("DC3"),_fY=[0,19],_fZ=function(_g0){var _g1=new T(function(){return A(_g0,[_fY]);});return _e6(_fX,function(_g2){return E(_g1);});},_g3=unCStr("DC4"),_g4=[0,20],_g5=function(_g6){var _g7=new T(function(){return A(_g6,[_g4]);});return _e6(_g3,function(_g8){return E(_g7);});},_g9=unCStr("NAK"),_ga=[0,21],_gb=function(_gc){var _gd=new T(function(){return A(_gc,[_ga]);});return _e6(_g9,function(_ge){return E(_gd);});},_gf=unCStr("SYN"),_gg=[0,22],_gh=function(_gi){var _gj=new T(function(){return A(_gi,[_gg]);});return _e6(_gf,function(_gk){return E(_gj);});},_gl=unCStr("ETB"),_gm=[0,23],_gn=function(_go){var _gp=new T(function(){return A(_go,[_gm]);});return _e6(_gl,function(_gq){return E(_gp);});},_gr=unCStr("CAN"),_gs=[0,24],_gt=function(_gu){var _gv=new T(function(){return A(_gu,[_gs]);});return _e6(_gr,function(_gw){return E(_gv);});},_gx=unCStr("EM"),_gy=[0,25],_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gy]);});return _e6(_gx,function(_gC){return E(_gB);});},_gD=unCStr("SUB"),_gE=[0,26],_gF=function(_gG){var _gH=new T(function(){return A(_gG,[_gE]);});return _e6(_gD,function(_gI){return E(_gH);});},_gJ=unCStr("ESC"),_gK=[0,27],_gL=function(_gM){var _gN=new T(function(){return A(_gM,[_gK]);});return _e6(_gJ,function(_gO){return E(_gN);});},_gP=unCStr("FS"),_gQ=[0,28],_gR=function(_gS){var _gT=new T(function(){return A(_gS,[_gQ]);});return _e6(_gP,function(_gU){return E(_gT);});},_gV=unCStr("GS"),_gW=[0,29],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _e6(_gV,function(_h0){return E(_gZ);});},_h1=unCStr("RS"),_h2=[0,30],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _e6(_h1,function(_h6){return E(_h5);});},_h7=unCStr("US"),_h8=[0,31],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _e6(_h7,function(_hc){return E(_hb);});},_hd=unCStr("SP"),_he=[0,32],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _e6(_hd,function(_hi){return E(_hh);});},_hj=unCStr("DEL"),_hk=[0,127],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _e6(_hj,function(_ho){return E(_hn);});},_hp=[1,_hl,_r],_hq=[1,_hf,_hp],_hr=[1,_h9,_hq],_hs=[1,_h3,_hr],_ht=[1,_gX,_hs],_hu=[1,_gR,_ht],_hv=[1,_gL,_hu],_hw=[1,_gF,_hv],_hx=[1,_gz,_hw],_hy=[1,_gt,_hx],_hz=[1,_gn,_hy],_hA=[1,_gh,_hz],_hB=[1,_gb,_hA],_hC=[1,_g5,_hB],_hD=[1,_fZ,_hC],_hE=[1,_fT,_hD],_hF=[1,_fN,_hE],_hG=[1,_fH,_hF],_hH=[1,_fB,_hG],_hI=[1,_fv,_hH],_hJ=[1,_fp,_hI],_hK=[1,_fj,_hJ],_hL=[1,_fd,_hK],_hM=[1,_f7,_hL],_hN=[1,_f1,_hM],_hO=[1,_eV,_hN],_hP=[1,_eP,_hO],_hQ=[1,_eJ,_hP],_hR=[1,_eD,_hQ],_hS=[1,_ex,_hR],_hT=[1,_er,_hS],_hU=[1,_el,_hT],_hV=unCStr("SOH"),_hW=[0,1],_hX=function(_hY){var _hZ=new T(function(){return A(_hY,[_hW]);});return _e6(_hV,function(_i0){return E(_hZ);});},_i1=unCStr("SO"),_i2=[0,14],_i3=function(_i4){var _i5=new T(function(){return A(_i4,[_i2]);});return _e6(_i1,function(_i6){return E(_i5);});},_i7=function(_i8){return _bg(_hX,_i3,_i8);},_i9=[1,_i7,_hU],_ia=new T(function(){return _dU(_i9);}),_ib=[0,1114111],_ic=[0,34],_id=[0,_ic,_dv],_ie=[0,39],_if=[0,_ie,_dv],_ig=[0,92],_ih=[0,_ig,_dv],_ii=[0,_eU,_dv],_ij=[0,_f0,_dv],_ik=[0,_fo,_dv],_il=[0,_fc,_dv],_im=[0,_fu,_dv],_in=[0,_f6,_dv],_io=[0,_fi,_dv],_ip=[0,_ek,_dv],_iq=[0,_hW,_dv],_ir=[0,_eq,_dv],_is=[0,_ew,_dv],_it=[0,_eC,_dv],_iu=[0,_eI,_dv],_iv=[0,_eO,_dv],_iw=[0,_eU,_dv],_ix=[0,_f0,_dv],_iy=[0,_f6,_dv],_iz=[0,_fc,_dv],_iA=[0,_fi,_dv],_iB=[0,_fo,_dv],_iC=[0,_fu,_dv],_iD=[0,_i2,_dv],_iE=[0,_fA,_dv],_iF=[0,_fG,_dv],_iG=[0,_fM,_dv],_iH=[0,_fS,_dv],_iI=[0,_fY,_dv],_iJ=[0,_g4,_dv],_iK=[0,_ga,_dv],_iL=[0,_gg,_dv],_iM=[0,_gm,_dv],_iN=[0,_gs,_dv],_iO=[0,_gy,_dv],_iP=[0,_gE,_dv],_iQ=[0,_gK,_dv],_iR=[0,_gQ,_dv],_iS=[0,_gW,_dv],_iT=[0,_h2,_dv],_iU=[0,_h8,_dv],_iV=function(_iW){return [0,_iW];},_iX=function(_iY){var _iZ=new T(function(){return A(_iY,[_io]);}),_j0=new T(function(){return A(_iY,[_in]);}),_j1=new T(function(){return A(_iY,[_im]);}),_j2=new T(function(){return A(_iY,[_il]);}),_j3=new T(function(){return A(_iY,[_ik]);}),_j4=new T(function(){return A(_iY,[_ij]);}),_j5=new T(function(){return A(_iY,[_ii]);}),_j6=new T(function(){return A(_iY,[_ih]);}),_j7=new T(function(){return A(_iY,[_if]);}),_j8=new T(function(){return A(_iY,[_id]);});return _a7([0,function(_j9){switch(E(E(_j9)[1])){case 34:return E(_j8);case 39:return E(_j7);case 92:return E(_j6);case 97:return E(_j5);case 98:return E(_j4);case 102:return E(_j3);case 110:return E(_j2);case 114:return E(_j1);case 116:return E(_j0);case 118:return E(_iZ);default:return [2];}}],new T(function(){return _a7(_bg(_dw,_dD,function(_ja){var _jb=new T(function(){return _iV(E(_ja)[1]);});return _bE(_ja,function(_jc){var _jd=_cE(_jb,_cv,_jc);return !_dK(_jd,_ib)?[2]:A(_iY,[[0,new T(function(){var _je=_dH(_jd);return _je>>>0>1114111?_dF(_je):[0,_je];}),_dv]]);});}),new T(function(){var _jf=new T(function(){return A(_iY,[_iU]);}),_jg=new T(function(){return A(_iY,[_iT]);}),_jh=new T(function(){return A(_iY,[_iS]);}),_ji=new T(function(){return A(_iY,[_iR]);}),_jj=new T(function(){return A(_iY,[_iQ]);}),_jk=new T(function(){return A(_iY,[_iP]);}),_jl=new T(function(){return A(_iY,[_iO]);}),_jm=new T(function(){return A(_iY,[_iN]);}),_jn=new T(function(){return A(_iY,[_iM]);}),_jo=new T(function(){return A(_iY,[_iL]);}),_jp=new T(function(){return A(_iY,[_iK]);}),_jq=new T(function(){return A(_iY,[_iJ]);}),_jr=new T(function(){return A(_iY,[_iI]);}),_js=new T(function(){return A(_iY,[_iH]);}),_jt=new T(function(){return A(_iY,[_iG]);}),_ju=new T(function(){return A(_iY,[_iF]);}),_jv=new T(function(){return A(_iY,[_iE]);}),_jw=new T(function(){return A(_iY,[_iD]);}),_jx=new T(function(){return A(_iY,[_iC]);}),_jy=new T(function(){return A(_iY,[_iB]);}),_jz=new T(function(){return A(_iY,[_iA]);}),_jA=new T(function(){return A(_iY,[_iz]);}),_jB=new T(function(){return A(_iY,[_iy]);}),_jC=new T(function(){return A(_iY,[_ix]);}),_jD=new T(function(){return A(_iY,[_iw]);}),_jE=new T(function(){return A(_iY,[_iv]);}),_jF=new T(function(){return A(_iY,[_iu]);}),_jG=new T(function(){return A(_iY,[_it]);}),_jH=new T(function(){return A(_iY,[_is]);}),_jI=new T(function(){return A(_iY,[_ir]);}),_jJ=new T(function(){return A(_iY,[_iq]);}),_jK=new T(function(){return A(_iY,[_ip]);});return _a7([0,function(_jL){return E(E(_jL)[1])==94?E([0,function(_jM){switch(E(E(_jM)[1])){case 64:return E(_jK);case 65:return E(_jJ);case 66:return E(_jI);case 67:return E(_jH);case 68:return E(_jG);case 69:return E(_jF);case 70:return E(_jE);case 71:return E(_jD);case 72:return E(_jC);case 73:return E(_jB);case 74:return E(_jA);case 75:return E(_jz);case 76:return E(_jy);case 77:return E(_jx);case 78:return E(_jw);case 79:return E(_jv);case 80:return E(_ju);case 81:return E(_jt);case 82:return E(_js);case 83:return E(_jr);case 84:return E(_jq);case 85:return E(_jp);case 86:return E(_jo);case 87:return E(_jn);case 88:return E(_jm);case 89:return E(_jl);case 90:return E(_jk);case 91:return E(_jj);case 92:return E(_ji);case 93:return E(_jh);case 94:return E(_jg);case 95:return E(_jf);default:return [2];}}]):[2];}],new T(function(){return A(_ia,[function(_jN){return A(_iY,[[0,_jN,_dv]]);}]);}));}));}));},_jO=function(_jP){return A(_jP,[_1]);},_jQ=function(_jR){var _jS=E(_jR);if(!_jS[0]){return E(_jO);}else{var _jT=_jS[2],_jU=E(E(_jS[1])[1]);switch(_jU){case 9:var _jV=new T(function(){return _jQ(_jT);});return function(_jW){var _jX=new T(function(){return A(_jV,[_jW]);});return [0,function(_jY){return E(_jX);}];};case 10:var _jZ=new T(function(){return _jQ(_jT);});return function(_k0){var _k1=new T(function(){return A(_jZ,[_k0]);});return [0,function(_k2){return E(_k1);}];};case 11:var _k3=new T(function(){return _jQ(_jT);});return function(_k4){var _k5=new T(function(){return A(_k3,[_k4]);});return [0,function(_k6){return E(_k5);}];};case 12:var _k7=new T(function(){return _jQ(_jT);});return function(_k8){var _k9=new T(function(){return A(_k7,[_k8]);});return [0,function(_ka){return E(_k9);}];};case 13:var _kb=new T(function(){return _jQ(_jT);});return function(_kc){var _kd=new T(function(){return A(_kb,[_kc]);});return [0,function(_ke){return E(_kd);}];};case 32:var _kf=new T(function(){return _jQ(_jT);});return function(_kg){var _kh=new T(function(){return A(_kf,[_kg]);});return [0,function(_ki){return E(_kh);}];};case 160:var _kj=new T(function(){return _jQ(_jT);});return function(_kk){var _kl=new T(function(){return A(_kj,[_kk]);});return [0,function(_km){return E(_kl);}];};default:var _kn=u_iswspace(_jU);if(!E(_kn)){return E(_jO);}else{var _ko=new T(function(){return _jQ(_jT);});return function(_kp){var _kq=new T(function(){return A(_ko,[_kp]);});return [0,function(_kr){return E(_kq);}];};}}}},_ks=function(_kt){var _ku=new T(function(){return _iX(_kt);}),_kv=new T(function(){return _ks(_kt);}),_kw=[1,function(_kx){return A(_jQ,[_kx,function(_ky){return E([0,function(_kz){return E(E(_kz)[1])==92?E(_kv):[2];}]);}]);}];return _a7([0,function(_kA){return E(E(_kA)[1])==92?E([0,function(_kB){var _kC=E(E(_kB)[1]);switch(_kC){case 9:return E(_kw);case 10:return E(_kw);case 11:return E(_kw);case 12:return E(_kw);case 13:return E(_kw);case 32:return E(_kw);case 38:return E(_kv);case 160:return E(_kw);default:var _kD=u_iswspace(_kC);return E(_kD)==0?[2]:E(_kw);}}]):[2];}],[0,function(_kE){var _kF=E(_kE);return E(_kF[1])==92?E(_ku):A(_kt,[[0,_kF,_du]]);}]);},_kG=function(_kH,_kI){var _kJ=new T(function(){return A(_kI,[[1,new T(function(){return A(_kH,[_r]);})]]);});return _ks(function(_kK){var _kL=E(_kK),_kM=E(_kL[1]);return E(_kM[1])==34?!E(_kL[2])?E(_kJ):_kG(function(_kN){return A(_kH,[[1,_kM,_kN]]);},_kI):_kG(function(_kO){return A(_kH,[[1,_kM,_kO]]);},_kI);});},_kP=unCStr("_\'"),_kQ=function(_kR){var _kS=u_iswalnum(_kR);return E(_kS)==0?_d8(_aE,[0,_kR],_kP):true;},_kT=function(_kU){return _kQ(E(_kU)[1]);},_kV=unCStr(",;()[]{}`"),_kW=function(_kX){return A(_kX,[_r]);},_kY=function(_kZ,_l0){var _l1=function(_l2){var _l3=E(_l2);if(!_l3[0]){return E(_kW);}else{var _l4=_l3[1];if(!A(_kZ,[_l4])){return E(_kW);}else{var _l5=new T(function(){return _l1(_l3[2]);});return function(_l6){var _l7=new T(function(){return A(_l5,[function(_l8){return A(_l6,[[1,_l4,_l8]]);}]);});return [0,function(_l9){return E(_l7);}];};}}};return [1,function(_la){return A(_l1,[_la,_l0]);}];},_lb=unCStr(".."),_lc=unCStr("::"),_ld=unCStr("->"),_le=[0,64],_lf=[1,_le,_r],_lg=[0,126],_lh=[1,_lg,_r],_li=unCStr("=>"),_lj=[1,_li,_r],_lk=[1,_lh,_lj],_ll=[1,_lf,_lk],_lm=[1,_ld,_ll],_ln=unCStr("<-"),_lo=[1,_ln,_lm],_lp=[0,124],_lq=[1,_lp,_r],_lr=[1,_lq,_lo],_ls=[1,_ig,_r],_lt=[1,_ls,_lr],_lu=[0,61],_lv=[1,_lu,_r],_lw=[1,_lv,_lt],_lx=[1,_lc,_lw],_ly=[1,_lb,_lx],_lz=function(_lA){var _lB=new T(function(){return A(_lA,[_bB]);});return _a7([1,function(_lC){return E(_lC)[0]==0?E(_lB):[2];}],new T(function(){var _lD=new T(function(){return _iX(function(_lE){var _lF=E(_lE);return (function(_lG,_lH){var _lI=new T(function(){return A(_lA,[[0,_lG]]);});return !E(_lH)?E(E(_lG)[1])==39?[2]:[0,function(_lJ){return E(E(_lJ)[1])==39?E(_lI):[2];}]:[0,function(_lK){return E(E(_lK)[1])==39?E(_lI):[2];}];})(_lF[1],_lF[2]);});});return _a7([0,function(_lL){return E(E(_lL)[1])==39?E([0,function(_lM){var _lN=E(_lM);switch(E(_lN[1])){case 39:return [2];case 92:return E(_lD);default:var _lO=new T(function(){return A(_lA,[[0,_lN]]);});return [0,function(_lP){return E(E(_lP)[1])==39?E(_lO):[2];}];}}]):[2];}],new T(function(){var _lQ=new T(function(){return _kG(_9,_lA);});return _a7([0,function(_lR){return E(E(_lR)[1])==34?E(_lQ):[2];}],new T(function(){return _a7([0,function(_lS){return !_d8(_aE,_lS,_kV)?[2]:A(_lA,[[2,[1,_lS,_r]]]);}],new T(function(){return _a7([0,function(_lT){return !_d8(_aE,_lT,_dd)?[2]:_kY(_de,function(_lU){var _lV=[1,_lT,_lU];return !_d8(_aV,_lV,_ly)?A(_lA,[[4,_lV]]):A(_lA,[[2,_lV]]);});}],new T(function(){return _a7([0,function(_lW){var _lX=E(_lW),_lY=_lX[1],_lZ=u_iswalpha(_lY);return E(_lZ)==0?E(_lY)==95?_kY(_kT,function(_m0){return A(_lA,[[3,[1,_lX,_m0]]]);}):[2]:_kY(_kT,function(_m1){return A(_lA,[[3,[1,_lX,_m1]]]);});}],new T(function(){return _bg(_di,_d3,_lA);}));}));}));}));}));}));},_m2=function(_m3){var _m4=new T(function(){return _lz(_m3);});return [1,function(_m5){return A(_jQ,[_m5,function(_m6){return E(_m4);}]);}];},_m7=[0,0],_m8=function(_m9,_ma){var _mb=new T(function(){return A(_m9,[_m7,function(_mc){var _md=new T(function(){return A(_ma,[_mc]);});return _m2(function(_me){var _mf=E(_me);if(_mf[0]==2){var _mg=E(_mf[1]);return _mg[0]==0?[2]:E(E(_mg[1])[1])==41?E(_mg[2])[0]==0?E(_md):[2]:[2];}else{return [2];}});}]);});return _m2(function(_mh){var _mi=E(_mh);if(_mi[0]==2){var _mj=E(_mi[1]);return _mj[0]==0?[2]:E(E(_mj[1])[1])==40?E(_mj[2])[0]==0?E(_mb):[2]:[2];}else{return [2];}});},_mk=function(_ml,_mm,_mn){var _mo=function(_mp,_mq){var _mr=new T(function(){return _lz(function(_ms){return A(_ml,[_ms,_mp,function(_mt){return A(_mq,[new T(function(){return [0, -E(_mt)[1]];})]);}]);});});return _a7(_m2(function(_mu){var _mv=E(_mu);if(_mv[0]==4){var _mw=E(_mv[1]);return _mw[0]==0?A(_ml,[_mv,_mp,_mq]):E(E(_mw[1])[1])==45?E(_mw[2])[0]==0?E([1,function(_mx){return A(_jQ,[_mx,function(_my){return E(_mr);}]);}]):A(_ml,[_mv,_mp,_mq]):A(_ml,[_mv,_mp,_mq]);}else{return A(_ml,[_mv,_mp,_mq]);}}),new T(function(){return _m8(_mo,_mq);}));};return _mo(_mm,_mn);},_mz=function(_mA,_mB){return [2];},_mC=function(_mD,_mE){return _mz(_mD,_mE);},_mF=function(_mG){var _mH=E(_mG);return _mH[0]==0?[1,new T(function(){return _cE(new T(function(){return _iV(E(_mH[1])[1]);}),_cv,_mH[2]);})]:E(_mH[2])[0]==0?E(_mH[3])[0]==0?[1,new T(function(){return _cE(_cu,_cv,_mH[1]);})]:[0]:[0];},_mI=function(_mJ){var _mK=E(_mJ);if(_mK[0]==5){var _mL=_mF(_mK[1]);if(!_mL[0]){return E(_mz);}else{var _mM=new T(function(){return [0,_dH(_mL[1])];});return function(_mN,_mO){return A(_mO,[_mM]);};}}else{return E(_mC);}},_mP=function(_mD,_mE){return _mk(_mI,_mD,_mE);},_mQ=function(_mR,_mS){var _mT=function(_mU,_mV){var _mW=new T(function(){return A(_mV,[_r]);}),_mX=new T(function(){return A(_mR,[_m7,function(_mY){return _mT(_dv,function(_mZ){return A(_mV,[[1,_mY,_mZ]]);});}]);});return _m2(function(_n0){var _n1=E(_n0);if(_n1[0]==2){var _n2=E(_n1[1]);if(!_n2[0]){return [2];}else{var _n3=_n2[2];switch(E(E(_n2[1])[1])){case 44:return E(_n3)[0]==0?!E(_mU)?[2]:E(_mX):[2];case 93:return E(_n3)[0]==0?E(_mW):[2];default:return [2];}}}else{return [2];}});},_n4=function(_n5){var _n6=new T(function(){return _a7(_mT(_du,_n5),new T(function(){return A(_mR,[_m7,function(_n7){return _mT(_dv,function(_n8){return A(_n5,[[1,_n7,_n8]]);});}]);}));});return _a7(_m2(function(_n9){var _na=E(_n9);if(_na[0]==2){var _nb=E(_na[1]);return _nb[0]==0?[2]:E(E(_nb[1])[1])==91?E(_nb[2])[0]==0?E(_n6):[2]:[2];}else{return [2];}}),new T(function(){return _m8(function(_nc,_nd){return _n4(_nd);},_n5);}));};return _n4(_mS);},_ne=function(_nf,_ng){return _mQ(_mP,_ng);},_nh=new T(function(){return _mQ(_mP,_b8);}),_ni=function(_mE){return _9X(_nh,_mE);},_nj=function(_nk){var _nl=new T(function(){return _mk(_mI,_nk,_b8);});return function(_4Z){return _9X(_nl,_4Z);};},_nm=[0,_nj,_ni,_mP,_ne],_nn=function(_no){return _2N(0,E(_no)[1],_r);},_np=function(_nq,_nr){return _2N(0,E(_nq)[1],_nr);},_ns=function(_nt,_nu){return _1F(_np,_nt,_nu);},_nv=function(_nw,_nx,_ny){return _2N(E(_nw)[1],E(_nx)[1],_ny);},_nz=[0,_nv,_nn,_ns],_nA=unCStr("GHC.Types"),_nB=unCStr("Int"),_nC=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5W,_nA,_nB],_nD=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_nC,_r],_nE=function(_nF){return E(_nD);},_nG=function(_nH){return E(E(_nH)[1]);},_nI=function(_nJ){return E(E(_nJ)[1]);},_nK=function(_nL){return E(E(_nL)[2]);},_nM=function(_nN,_nO){var _nP=new T(function(){return A(_nK,[_nN,_nO]);}),_nQ=new T(function(){return _nI(_nN);}),_nR=new T(function(){return _2z(_nQ);}),_nS=new T(function(){return _29(_nQ);});return function(_nT){return A(_nS,[_nP,function(_nU){return A(_nR,[[0,_nU,_nT]]);}]);};},_nV=function(_nW,_nX){return A(_nW,[function(_){return jsFind(toJSStr(E(_nX)));}]);},_nY=function(_nZ){return E(E(_nZ)[4]);},_o0=unCStr("[]"),_o1=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5W,_nA,_o0],_o2=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_o1,_r],_o3=function(_o4){return E(_o2);},_o5=unCStr("Char"),_o6=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5W,_nA,_o5],_o7=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_o6,_r],_o8=function(_o9){return E(_o7);},_oa=new T(function(){return _6Q(_o3,_o8);}),_ob=new T(function(){return A(_oa,[_6P]);}),_oc=new T(function(){return E(_6P);}),_od=function(_oe){return E(E(_oe)[7]);},_of=function(_og){return E(E(_og)[1]);},_oh=[0,0],_oi=[0,32],_oj=[0,10],_ok=function(_ol){var _om=E(_ol);if(!_om[0]){return E(_9);}else{var _on=_om[1],_oo=E(_om[2]);if(!_oo[0]){return _op(_oj,_on);}else{var _oq=new T(function(){return _ok(_oo);}),_or=new T(function(){return _op(_oj,_on);});return function(_os){return A(_or,[[1,_oi,new T(function(){return A(_oq,[_os]);})]]);};}}},_ot=unCStr("->"),_ou=[1,_ot,_r],_ov=[1,_nA,_ou],_ow=[1,_5W,_ov],_ox=[0,32],_oy=function(_oz){var _oA=E(_oz);if(!_oA[0]){return [0];}else{var _oB=_oA[1],_oC=E(_oA[2]);return _oC[0]==0?E(_oB):_P(_oB,[1,_ox,new T(function(){return _oy(_oC);})]);}},_oD=new T(function(){return _oy(_ow);}),_oE=new T(function(){var _oF=_6k(_oD);return [0,_oF[1],_oF[2],_5W,_nA,_ot];}),_oG=function(_oH,_oI){var _oJ=E(_oH);return _oJ[0]==0?E(_oI):A(_oJ[1],[new T(function(){return _oG(_oJ[2],_oI);})]);},_oK=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_oL=[1,_5Y,_r],_oM=function(_oN){var _oO=E(_oN);if(!_oO[0]){return [0];}else{var _oP=E(_oO[1]);return [1,[0,_oP[1],_oP[2]],new T(function(){return _oM(_oO[2]);})];}},_oQ=new T(function(){var _oR=_P(_r,_oL);if(!_oR[0]){return E(_o1);}else{var _oS=_6k(new T(function(){return _68(_6w(_6H,[1,_oK,new T(function(){return _oM(_oR);})]));}));return E(_o1);}}),_oT=[0,40],_oU=function(_oV){return _op(_oj,_oV);},_oW=[0,8],_oX=unCStr(" -> "),_oY=[0,9],_oZ=[0,93],_p0=[0,91],_p1=[0,41],_p2=[0,44],_p3=function(_oV){return [1,_p2,_oV];},_p4=function(_p5,_p6){var _p7=E(_p6);return _p7[0]==0?[0]:[1,_p5,[1,_p7[1],new T(function(){return _p4(_p5,_p7[2]);})]];},_op=function(_p8,_p9){var _pa=E(_p9),_pb=_pa[3],_pc=E(_pa[4]);if(!_pc[0]){return function(_pd){return _P(E(_pb)[5],_pd);};}else{var _pe=_pc[1],_pf=new T(function(){var _pg=E(_pb)[5],_ph=new T(function(){return _ok(_pc);}),_pi=new T(function(){return E(_p8)[1]<=9?function(_pj){return _P(_pg,[1,_oi,new T(function(){return A(_ph,[_pj]);})]);}:function(_pk){return [1,_2M,new T(function(){return _P(_pg,[1,_oi,new T(function(){return A(_ph,[[1,_2L,_pk]]);})]);})];};}),_pl=E(_pg);if(!_pl[0]){return E(_pi);}else{if(E(E(_pl[1])[1])==40){var _pm=E(_pl[2]);return _pm[0]==0?E(_pi):E(E(_pm[1])[1])==44?function(_pn){return [1,_oT,new T(function(){return A(new T(function(){var _po=_6w(_oU,_pc);if(!_po[0]){return E(_9);}else{var _pp=new T(function(){return _p4(_p3,_po[2]);});return function(_4Z){return _oG([1,_po[1],_pp],_4Z);};}}),[[1,_p1,_pn]]);})];}:E(_pi);}else{return E(_pi);}}}),_pq=E(_pc[2]);if(!_pq[0]){var _pr=E(_pb),_ps=E(_oQ),_pt=hs_eqWord64(_pr[1],_ps[1]);if(!E(_pt)){return E(_pf);}else{var _pu=hs_eqWord64(_pr[2],_ps[2]);if(!E(_pu)){return E(_pf);}else{var _pv=new T(function(){return _op(_oh,_pe);});return function(_pw){return [1,_p0,new T(function(){return A(_pv,[[1,_oZ,_pw]]);})];};}}}else{if(!E(_pq[2])[0]){var _px=E(_pb),_py=E(_oE),_pz=hs_eqWord64(_px[1],_py[1]);if(!E(_pz)){return E(_pf);}else{var _pA=hs_eqWord64(_px[2],_py[2]);if(!E(_pA)){return E(_pf);}else{var _pB=new T(function(){return _op(_oW,_pq[1]);}),_pC=new T(function(){return _op(_oY,_pe);});return E(_p8)[1]<=8?function(_pD){return A(_pC,[new T(function(){return _P(_oX,new T(function(){return A(_pB,[_pD]);}));})]);}:function(_pE){return [1,_2M,new T(function(){return A(_pC,[new T(function(){return _P(_oX,new T(function(){return A(_pB,[[1,_2L,_pE]]);}));})]);})];};}}}else{return E(_pf);}}}},_pF=function(_pG,_pH,_pI,_pJ){var _pK=new T(function(){return _2z(_pG);}),_pL=new T(function(){return _nY(_pJ);}),_pM=new T(function(){return _od(_pJ);}),_pN=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_op,[_oh,A(_pH,[_oc]),_r]);}));}),_pO=new T(function(){return A(_of,[_pI,_3c]);});return function(_pP){if(!E(new T(function(){var _pQ=A(_pH,[_oc]),_pR=E(_ob),_pS=hs_eqWord64(_pQ[1],_pR[1]);if(!E(_pS)){return false;}else{var _pT=hs_eqWord64(_pQ[2],_pR[2]);return E(_pT)==0?false:true;}}))){var _pU=new T(function(){return A(_pK,[[1,_pP,new T(function(){return A(_pM,[new T(function(){return A(_pL,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _P(_pP,_pN);}));})]);})]);})]]);}),_pV=A(_pO,[_pP]);if(!_pV[0]){return E(_pU);}else{var _pW=E(_pV[1]);return E(_pW[2])[0]==0?E(_pV[2])[0]==0?A(_pK,[[2,_pW[1]]]):E(_pU):E(_pU);}}else{return A(_pK,[[2,_pP]]);}};},_pX=[0],_pY=new T(function(){return [0,"value"];}),_pZ=function(_q0,_q1,_q2,_q3,_q4,_q5){var _q6=E(_q0),_q7=_q6[1],_q8=new T(function(){return A(_q6[3],[_pX]);}),_q9=new T(function(){return _pF(_q6,_q2,_q3,_q4);});return A(_q7,[new T(function(){return _nV(_q1,_q5);}),function(_qa){var _qb=E(_qa);return _qb[0]==0?E(_q8):A(_q7,[new T(function(){return A(_q1,[function(_){var _qc=jsGet(E(_qb[1])[1],E(_pY)[1]);return [1,new T(function(){return fromJSStr(_qc);})];}]);}),function(_qd){var _qe=E(_qd);return _qe[0]==0?E(_q8):A(_q9,[_qe[1]]);}]);}]);},_qf=1,_qg=function(_qh){return E(E(_qh)[10]);},_qi=function(_qj,_qk){return A(_2z,[_qj,[0,_qk,_qk]]);},_ql=function(_qm){return E(E(_qm)[2]);},_qn=function(_qo,_qp,_qq){return A(_2z,[_qo,[0,_1,_qp]]);},_qr=function(_qs){return E(E(_qs)[2]);},_qt=function(_qu,_qv,_qw,_qx,_qy){var _qz=new T(function(){return _nG(_qu);}),_qA=new T(function(){return _ql(_qz);}),_qB=new T(function(){return _nI(_qv);}),_qC=new T(function(){return _2B(_qB);}),_qD=new T(function(){return _2T([0,coercionToken],_qC,function(_qE){return _qi(_qB,_qE);},function(_qF,_qG){return _qn(_qB,_qF,_qG);});}),_qH=new T(function(){return _2z(_qB);}),_qI=new T(function(){return _29(_qB);}),_qJ=new T(function(){return _2z(_qB);}),_qK=new T(function(){return _29(_qB);}),_qL=new T(function(){return _2z(_qB);}),_qM=new T(function(){return _29(_qB);}),_qN=new T(function(){return _2z(_qB);}),_qO=new T(function(){return _29(_qB);}),_qP=new T(function(){return _qr(_qx);}),_qQ=new T(function(){return _qg(_qu);});return function(_qR,_qS,_qT){return function(_qU){return A(_qO,[new T(function(){var _qV=E(_qR);return _qV[0]==0?A(_qD,[_qU]):A(_qN,[[0,_qV[1],_qU]]);}),function(_qW){var _qX=new T(function(){return E(E(_qW)[1]);}),_qY=new T(function(){return _pZ(_qC,function(_qZ){return _nM(_qv,_qZ);},_qw,_qy,_qu,_qX);}),_r0=new T(function(){return A(_qQ,[_qX,_qS,new T(function(){var _r1=E(_qT);if(!_r1[0]){return [0];}else{var _r2=_r1[1],_r3=_z(_qw,_oa,_r2);return _r3[0]==0?A(_qP,[_r2]):E(_r3[1]);}}),_du,_8]);});return A(_qM,[new T(function(){var _r4=new T(function(){return E(E(_qW)[2]);});return A(_qL,[[0,_r4,_r4]]);}),function(_r5){return A(_qK,[new T(function(){return A(_qJ,[[0,_1,new T(function(){var _r6=E(E(_r5)[1]);return [0,_r6[1],_r6[2],_qf,_r6[4],_r6[5]];})]]);}),function(_r7){return A(_qI,[new T(function(){return A(_qY,[new T(function(){return E(E(_r7)[2]);})]);}),function(_r8){var _r9=E(_r8),_ra=_r9[2],_rb=E(_r9[1]);switch(_rb[0]){case 0:return A(_qH,[[0,[0,_r0,_8],_ra]]);case 1:return A(_qH,[[0,[0,new T(function(){return A(_qA,[new T(function(){return A(_qQ,[_qX,_qS,_rb[1],_du,_8]);}),_rb[2]]);}),_8],_ra]]);default:var _rc=_rb[1];return A(_qH,[[0,[0,new T(function(){return A(_qQ,[_qX,_qS,new T(function(){var _rd=_z(_qw,_oa,_rc);return _rd[0]==0?A(_qP,[_rc]):E(_rd[1]);}),_du,_8]);}),[1,_rc]],_ra]]);}}]);}]);}]);}]);};};},_re=new T(function(){return _qt(_93,_94,_nE,_nz,_nm);}),_rf=new T(function(){return A(_re,[_8,_5N,_8]);}),_rg=function(_rh,_){var _ri=A(_rf,[_rh,_]),_rj=E(_ri),_rk=E(_rj[1]),_rl=new T(function(){return E(E(_rh)[4]);});return [0,[0,function(_rm,_){var _rn=_5A(_rk[1],_5b,_rl,_rm,_),_ro=_4E(_rm,_);return _rm;},_rk[2]],_rj[2]];},_rp=[1,_rg,_r],_rq=function(_rr){return _rr>1?[1,_rg,new T(function(){return _rq(_rr-1|0);})]:E(_rp);},_rs=new T(function(){return _rq(3);}),_rt=[0,0],_ru=[1,_rt],_rv=[0,_26,_ru],_rw=function(_rx,_){return [0,_rv,_rx];},_ry=function(_rz,_rA,_){var _rB=A(_rA,[_]);return _rz;},_rC=function(_rD,_rE,_){var _rF=A(_rE,[_]);return new T(function(){return A(_rD,[_rF]);});},_rG=[0,_rC,_ry],_rH=function(_rI){var _rJ=E(_rI);return _rJ[0]==0?0:E(_rJ[1])[1]+_rH(_rJ[2])|0;},_rK=function(_rL){return [0,_rH(_rL)];},_rM=function(_rN,_rO){return [0,E(_rN)[1]+E(_rO)[1]|0];},_rP=[0,_rt,_rM,_rK],_rQ=function(_rR,_rS){var _rT=E(_rS);return _rT[0]==0?[0]:[1,new T(function(){return A(_rR,[_rT[1]]);})];},_rU=function(_rV,_rW,_rX,_rY,_rZ,_s0){var _s1=new T(function(){return _ql(_rV);});return A(_rW,[new T(function(){return A(_rY,[_s0]);}),function(_s2){var _s3=E(_s2),_s4=E(_s3[1]);return A(_rW,[new T(function(){return A(_rZ,[_s3[2]]);}),function(_s5){var _s6=E(_s5),_s7=E(_s6[1]);return A(_rX,[[0,[0,new T(function(){return A(_s1,[_s4[1],_s7[1]]);}),new T(function(){var _s8=E(_s4[2]);if(!_s8[0]){return [0];}else{var _s9=E(_s7[2]);return _s9[0]==0?[0]:[1,new T(function(){return A(_s8[1],[_s9[1]]);})];}})],_s6[2]]]);}]);}]);},_sa=function(_sb){return E(E(_sb)[1]);},_sc=function(_sd,_se,_sf,_sg,_sh,_si){var _sj=new T(function(){return _nG(_sd);});return function(_sk){var _sl=E(_se);return _rU(_sj,_sl[1],_sl[3],function(_sm){return A(new T(function(){var _sn=new T(function(){return _ql(_sg);});return A(_sa,[_sf,function(_so){return [0,new T(function(){var _sp=E(E(_so)[1]);return [0,_sp[1],new T(function(){return _rQ(_sn,_sp[2]);})];}),new T(function(){return E(E(_so)[2]);})];}]);}),[new T(function(){return A(_sh,[_sm]);})]);},_si,_sk);};},_sq=function(_sr,_ss){while(1){var _st=(function(_su,_sv){var _sw=E(_sv);if(!_sw[0]){return E(_su);}else{_sr=new T(function(){return _sc(_93,_28,_rG,_rP,_su,_sw[1]);});_ss=_sw[2];return null;}})(_sr,_ss);if(_st!=null){return _st;}}},_sx=new T(function(){return _sq(_rw,_rs);}),_sy=function(_54,_){return _3I(_sx,_4V,_54,_);},_sz=unCStr("idelem"),_sA=function(_){var _sB=E(_sz),_sC=jsFind(toJSStr(_sB)),_sD=E(_sC);return _sD[0]==0?_3f(_sB):_3t(_sy,_sD[1],_);},_sE=function(_){return _sA(_);};
var hasteMain = function() {A(_sE, [0]);};window.onload = hasteMain;