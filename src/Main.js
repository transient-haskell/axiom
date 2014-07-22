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

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=2,_9=[1],_a=[0],_b=[0],_c=function(_d,_){return _b;},_e=function(_){return _b;},_f=[0,_e,_c],_g=[0,0],_h=[0,_a,_g,_8,_f,_0,_9],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=unCStr("id"),_A=0,_B=function(_C,_D,_E,_F){return A(_C,[new T(function(){return function(_){var _G=jsSetAttr(E(_D)[1],toJSStr(E(_E)),toJSStr(E(_F)));return _A;};})]);},_H=function(_I){return E(_I);},_J=function(_K,_L,_M,_){var _N=E(_L),_O=A(_K,[_M,_]),_P=A(_B,[_H,_O,_N[1],_N[2],_]);return _O;},_Q=function(_R,_S){while(1){var _T=(function(_U,_V){var _W=E(_V);if(!_W[0]){return E(_U);}else{_R=function(_X,_){return _J(_U,_W[1],_X,_);};_S=_W[2];return null;}})(_R,_S);if(_T!=null){return _T;}}},_Y=unCStr("span"),_Z=function(_10,_11,_){var _12=jsCreateElem(toJSStr(E(_10))),_13=jsAppendChild(_12,E(_11)[1]);return [0,_12];},_14=function(_X,_){return _Z(_Y,_X,_);},_15=[0,coercionToken],_16=function(_17,_18,_){return [0,_A,_17];},_19=function(_1a,_){return [0,_1a,_1a];},_1b=function(_1c,_1d,_){var _1e=A(_1c,[_]);return A(_1d,[_]);},_1f=function(_1g,_1h,_){return _1b(_1g,_1h,_);},_1i=function(_1j,_1k,_){var _1l=A(_1j,[_]);return A(_1k,[_1l,_]);},_1m=unCStr("base"),_1n=unCStr("GHC.IO.Exception"),_1o=unCStr("IOException"),_1p=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1m,_1n,_1o],_1q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1p,_a],_1r=function(_1s){return E(_1q);},_1t=function(_1u){return E(E(_1u)[1]);},_1v=unCStr("Maybe.fromJust: Nothing"),_1w=new T(function(){return err(_1v);}),_1x=function(_1y,_1z,_1A){var _1B=new T(function(){var _1C=A(_1y,[_1A]),_1D=A(_1z,[new T(function(){var _1E=E(_1B);return _1E[0]==0?E(_1w):E(_1E[1]);})]),_1F=hs_eqWord64(_1C[1],_1D[1]);if(!E(_1F)){return [0];}else{var _1G=hs_eqWord64(_1C[2],_1D[2]);return E(_1G)==0?[0]:[1,_1A];}});return E(_1B);},_1H=function(_1I){var _1J=E(_1I);return _1x(_1t(_1J[1]),_1r,_1J[2]);},_1K=unCStr(": "),_1L=[0,41],_1M=unCStr(" ("),_1N=function(_1O,_1P){var _1Q=E(_1O);return _1Q[0]==0?E(_1P):[1,_1Q[1],new T(function(){return _1N(_1Q[2],_1P);})];},_1R=unCStr("already exists"),_1S=unCStr("does not exist"),_1T=unCStr("protocol error"),_1U=unCStr("failed"),_1V=unCStr("invalid argument"),_1W=unCStr("inappropriate type"),_1X=unCStr("hardware fault"),_1Y=unCStr("unsupported operation"),_1Z=unCStr("timeout"),_20=unCStr("resource vanished"),_21=unCStr("interrupted"),_22=unCStr("resource busy"),_23=unCStr("resource exhausted"),_24=unCStr("end of file"),_25=unCStr("illegal operation"),_26=unCStr("permission denied"),_27=unCStr("user error"),_28=unCStr("unsatisified constraints"),_29=unCStr("system error"),_2a=function(_2b,_2c){switch(E(_2b)){case 0:return _1N(_1R,_2c);case 1:return _1N(_1S,_2c);case 2:return _1N(_22,_2c);case 3:return _1N(_23,_2c);case 4:return _1N(_24,_2c);case 5:return _1N(_25,_2c);case 6:return _1N(_26,_2c);case 7:return _1N(_27,_2c);case 8:return _1N(_28,_2c);case 9:return _1N(_29,_2c);case 10:return _1N(_1T,_2c);case 11:return _1N(_1U,_2c);case 12:return _1N(_1V,_2c);case 13:return _1N(_1W,_2c);case 14:return _1N(_1X,_2c);case 15:return _1N(_1Y,_2c);case 16:return _1N(_1Z,_2c);case 17:return _1N(_20,_2c);default:return _1N(_21,_2c);}},_2d=[0,125],_2e=unCStr("{handle: "),_2f=function(_2g,_2h,_2i,_2j,_2k,_2l){var _2m=new T(function(){var _2n=new T(function(){return _2a(_2h,new T(function(){var _2o=E(_2j);return _2o[0]==0?E(_2l):_1N(_1M,new T(function(){return _1N(_2o,[1,_1L,_2l]);}));}));}),_2p=E(_2i);return _2p[0]==0?E(_2n):_1N(_2p,new T(function(){return _1N(_1K,_2n);}));}),_2q=E(_2k);if(!_2q[0]){var _2r=E(_2g);if(!_2r[0]){return E(_2m);}else{var _2s=E(_2r[1]);return _2s[0]==0?_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);})):_1N(_2e,new T(function(){return _1N(_2s[1],[1,_2d,new T(function(){return _1N(_1K,_2m);})]);}));}}else{return _1N(_2q[1],new T(function(){return _1N(_1K,_2m);}));}},_2t=function(_2u){var _2v=E(_2u);return _2f(_2v[1],_2v[2],_2v[3],_2v[4],_2v[6],_a);},_2w=function(_2x,_2y){var _2z=E(_2x);return _2f(_2z[1],_2z[2],_2z[3],_2z[4],_2z[6],_2y);},_2A=[0,44],_2B=[0,93],_2C=[0,91],_2D=function(_2E,_2F,_2G){var _2H=E(_2F);return _2H[0]==0?unAppCStr("[]",_2G):[1,_2C,new T(function(){return A(_2E,[_2H[1],new T(function(){var _2I=function(_2J){var _2K=E(_2J);return _2K[0]==0?E([1,_2B,_2G]):[1,_2A,new T(function(){return A(_2E,[_2K[1],new T(function(){return _2I(_2K[2]);})]);})];};return _2I(_2H[2]);})]);})];},_2L=function(_2M,_2N){return _2D(_2w,_2M,_2N);},_2O=function(_2P,_2Q,_2R){var _2S=E(_2Q);return _2f(_2S[1],_2S[2],_2S[3],_2S[4],_2S[6],_2R);},_2T=[0,_2O,_2t,_2L],_2U=new T(function(){return [0,_1r,_2T,_2V,_1H];}),_2V=function(_2W){return [0,_2U,_2W];},_2X=7,_2Y=function(_2Z){return [0,_b,_2X,_a,_2Z,_b,_b];},_30=function(_31,_){return die(new T(function(){return _2V(new T(function(){return _2Y(_31);}));}));},_32=function(_33,_){return _30(_33,_);},_34=function(_35,_){return _35;},_36=[0,_1i,_1f,_34,_32],_37=function(_38){return E(E(_38)[1]);},_39=function(_3a,_3b,_3c,_3d){return A(_37,[_3a,new T(function(){return A(_3b,[_3d]);}),function(_3e){return A(_3c,[new T(function(){return E(E(_3e)[1]);}),new T(function(){return E(E(_3e)[2]);})]);}]);},_3f=function(_3g,_3h,_3i,_3j){return A(_37,[_3g,new T(function(){return A(_3h,[_3j]);}),function(_3k){return A(_3i,[new T(function(){return E(E(_3k)[2]);})]);}]);},_3l=function(_3m,_3n,_3o,_3p){return _3f(_3m,_3n,_3o,_3p);},_3q=function(_3r){return E(E(_3r)[4]);},_3s=function(_3t,_3u){var _3v=new T(function(){return A(_3q,[_3t,_3u]);});return function(_3w){return E(_3v);};},_3x=function(_3y){return E(E(_3y)[3]);},_3z=function(_3A){var _3B=new T(function(){return _3x(_3A);});return [0,function(_3n,_3o,_3p){return _39(_3A,_3n,_3o,_3p);},function(_3n,_3o,_3p){return _3l(_3A,_3n,_3o,_3p);},function(_3C,_3D){return A(_3B,[[0,_3C,_3D]]);},function(_3p){return _3s(_3A,_3p);}];},_3E=new T(function(){return _3z(_36);}),_3F=[0,112],_3G=function(_3H,_3I){var _3J=jsShowI(_3H);return _1N(fromJSStr(_3J),_3I);},_3K=[0,41],_3L=[0,40],_3M=function(_3N,_3O,_3P){return _3O>=0?_3G(_3O,_3P):_3N<=6?_3G(_3O,_3P):[1,_3L,new T(function(){var _3Q=jsShowI(_3O);return _1N(fromJSStr(_3Q),[1,_3K,_3P]);})];},_3R=function(_3S,_3T,_3U,_3V){var _3W=E(_3T);return A(_3W[1],[new T(function(){var _3X=E(_3S);return E(_3U);}),function(_3Y){var _3Z=new T(function(){return E(E(_3Y)[2]);});return A(_3W[2],[new T(function(){return A(_3V,[new T(function(){var _40=E(new T(function(){var _41=E(_3S);return [0,coercionToken];})),_42=E(_3Y);return [0,_42[1],new T(function(){return [0,E(_3Z)[1]+1|0];}),_42[3],_42[4],_42[5],_42[6]];})]);}),new T(function(){return A(_3W[3],[[1,_3F,new T(function(){return _1N(_3M(0,E(_3Z)[1],_a),new T(function(){return E(E(_3Y)[1]);}));})]]);})]);}]);},_43=new T(function(){return _3R(_15,_3E,_19,_16);}),_44=unCStr(" could be found!"),_45=function(_46){return err(unAppCStr("No element with ID ",new T(function(){return _1N(_46,_44);})));},_47=function(_48,_49,_){var _4a=E(_49),_4b=jsFind(toJSStr(_4a)),_4c=E(_4b);if(!_4c[0]){return _45(_4a);}else{var _4d=E(_4c[1]),_4e=jsClearChildren(_4d[1]);return _l(_48,_4d,_);}},_4f=function(_4g,_4h,_4i,_4j,_4k,_4l,_4m,_4n,_4o,_){var _4p=E(_4m);return [0,_4p,[0,_4j,_4k,_4l,[0,function(_){return _47(function(_4q,_){var _4r=A(_4g,[new T(function(){var _4s=E(_4q);return [0,_4s[1],_4k,_4s[3],_4s[4],_4s[5],_4s[6]];}),_]);return [0,[0,_34,E(E(_4r)[1])[2]],_4q];},_4i,_);},function(_4t,_){var _4u=_47(new T(function(){return A(_4h,[_4t]);}),_4i,_),_4v=E(_4u);return _4v[0]==0?_b:A(_4p[2],[_4v[1],_]);}],_4n,_4o]];},_4w=function(_4x,_4y,_4z,_){var _4A=A(_43,[_4z,_]),_4B=E(_4A),_4C=_4B[1],_4D=E(_4B[2]),_4E=_4f(_4x,_4y,_4C,_4D[1],_4D[2],_4D[3],_4D[4],_4D[5],_4D[6],_),_4F=A(_4x,[new T(function(){return E(E(_4E)[2]);}),_]),_4G=E(_4F),_4H=_4G[2],_4I=E(_4G[1]),_4J=_4I[1],_4K=new T(function(){return _Q(_14,[1,[0,_z,_4C],_a]);}),_4L=E(_4I[2]);if(!_4L[0]){return [0,[0,function(_4M,_){var _4N=A(_4J,[_4M,_]),_4O=A(_4K,[_4M,_]);return _4M;},_b],new T(function(){var _4P=E(_4H);return [0,_4P[1],_4P[2],_4P[3],new T(function(){return E(E(_4E)[1]);}),_4P[5],_4P[6]];})];}else{var _4Q=A(_4y,[_4L[1],new T(function(){var _4R=E(_4H);return [0,_4R[1],_4R[2],_4R[3],new T(function(){return E(E(_4E)[1]);}),_4R[5],_4R[6]];}),_]),_4S=E(_4Q),_4T=E(_4S[1]);return [0,[0,function(_4U,_){var _4V=A(_4J,[_4U,_]),_4W=A(_4K,[_4U,_]),_4X=A(_4T[1],[_4W,_]);return _4U;},_4T[2]],_4S[2]];}},_4Y=unCStr("padding:15px;border-style:dotted"),_4Z=unCStr("border-collapse:collapse"),_50=unCStr("vertical-align:top"),_51=[0,3],_52=function(_53,_54,_){var _55=jsCreateTextNode(toJSStr(E(_53))),_56=jsAppendChild(_55,E(_54)[1]);return [0,_55];},_57=[0,112],_58=[1,_57,_a],_59=function(_5a,_5b){var _5c=new T(function(){return A(_5a,[_5b]);});return function(_5d,_){var _5e=jsCreateElem(toJSStr(_58)),_5f=jsAppendChild(_5e,E(_5d)[1]),_5g=[0,_5e],_5h=A(_5c,[_5g,_]);return _5g;};},_5i=function(_5j){return _3M(0,E(_5j)[1],_a);},_5k=[0,98],_5l=[1,_5k,_a],_5m=function(_5n,_5o){var _5p=new T(function(){return A(_5n,[_5o]);});return function(_5q,_){var _5r=jsCreateElem(toJSStr(_5l)),_5s=jsAppendChild(_5r,E(_5q)[1]),_5t=[0,_5r],_5u=A(_5p,[_5t,_]);return _5t;};},_5v=unCStr("br"),_5w=function(_5x,_){var _5y=jsCreateElem(toJSStr(E(_5v))),_5z=jsAppendChild(_5y,E(_5x)[1]);return [0,_5y];},_5A=[1,_A],_5B=unCStr("result: "),_5C=function(_5D){var _5E=new T(function(){return _5m(_52,new T(function(){return _5i(_5D);}));});return function(_5F,_){return [0,[0,function(_5G,_){var _5H=_5w(_5G,_),_5I=_52(_5B,_5G,_),_5J=A(_5E,[_5G,_]);return _5G;},_5A],_5F];};},_5K=unCStr(" numbers and append the result using a fold"),_5L=[0,0],_5M=[1,_5L],_5N=[0,_34,_5M],_5O=function(_5P,_){return [0,_5N,_5P];},_5Q=function(_5R,_5S,_5T,_){var _5U=_Z(_5R,_5T,_),_5V=A(_5S,[_5U,_]);return _5U;},_5W=unCStr("()"),_5X=unCStr("GHC.Tuple"),_5Y=unCStr("ghc-prim"),_5Z=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5Y,_5X,_5W],_60=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5Z,_a],_61=function(_62){return E(_60);},_63=unCStr("haste-perch-0.1.0.1"),_64=unCStr("Haste.Perch"),_65=unCStr("PerchM"),_66=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_63,_64,_65],_67=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_66,_a],_68=function(_69){return E(_67);},_6a=function(_6b){var _6c=E(_6b);return _6c[0]==0?[0]:_1N(_6c[1],new T(function(){return _6a(_6c[2]);}));},_6d=function(_6e,_6f){var _6g=E(_6e);if(!_6g){return [0,_a,_6f];}else{var _6h=E(_6f);if(!_6h[0]){return [0,_a,_a];}else{var _6i=new T(function(){var _6j=_6d(_6g-1|0,_6h[2]);return [0,_6j[1],_6j[2]];});return [0,[1,_6h[1],new T(function(){return E(E(_6i)[1]);})],new T(function(){return E(E(_6i)[2]);})];}}},_6k=[0,120],_6l=[0,48],_6m=function(_6n){var _6o=new T(function(){var _6p=_6d(8,new T(function(){var _6q=md5(toJSStr(E(_6n)));return fromJSStr(_6q);}));return [0,_6p[1],_6p[2]];}),_6r=parseInt([0,toJSStr([1,_6l,[1,_6k,new T(function(){return E(E(_6o)[1]);})]])]),_6s=new T(function(){var _6t=_6d(8,new T(function(){return E(E(_6o)[2]);}));return [0,_6t[1],_6t[2]];}),_6u=parseInt([0,toJSStr([1,_6l,[1,_6k,new T(function(){return E(E(_6s)[1]);})]])]),_6v=hs_mkWord64(_6r,_6u),_6w=parseInt([0,toJSStr([1,_6l,[1,_6k,new T(function(){return E(_6d(8,new T(function(){return E(E(_6s)[2]);}))[1]);})]])]),_6x=hs_mkWord64(_6w,_6w);return [0,_6v,_6x];},_6y=function(_6z,_6A){var _6B=E(_6A);return _6B[0]==0?[0]:[1,new T(function(){return A(_6z,[_6B[1]]);}),new T(function(){return _6y(_6z,_6B[2]);})];},_6C=function(_6D,_6E){var _6F=jsShowI(_6D),_6G=md5(_6F);return _1N(fromJSStr(_6G),new T(function(){var _6H=jsShowI(_6E),_6I=md5(_6H);return fromJSStr(_6I);}));},_6J=function(_6K){var _6L=E(_6K);return _6C(_6L[1],_6L[2]);},_6M=function(_6N){var _6O=E(_6N);if(!_6O[0]){return [0];}else{var _6P=E(_6O[1]);return [1,[0,_6P[1],_6P[2]],new T(function(){return _6M(_6O[2]);})];}},_6Q=unCStr("Prelude.undefined"),_6R=new T(function(){return err(_6Q);}),_6S=function(_6T,_6U){return function(_6V){return E(new T(function(){var _6W=A(_6T,[_6R]),_6X=E(_6W[3]),_6Y=_6X[1],_6Z=_6X[2],_70=_1N(_6W[4],[1,new T(function(){return A(_6U,[_6R]);}),_a]);if(!_70[0]){return [0,_6Y,_6Z,_6X,_a];}else{var _71=_6m(new T(function(){return _6a(_6y(_6J,[1,[0,_6Y,_6Z],new T(function(){return _6M(_70);})]));}));return [0,_71[1],_71[2],_6X,_70];}}));};},_72=new T(function(){return _6S(_68,_61);}),_73=unCStr("value"),_74=unCStr("onclick"),_75=unCStr("checked"),_76=[0,_75,_a],_77=[1,_76,_a],_78=unCStr("type"),_79=unCStr("input"),_7a=function(_7b,_){return _Z(_79,_7b,_);},_7c=function(_7d,_7e,_7f,_7g,_7h){var _7i=new T(function(){var _7j=new T(function(){return _Q(_7a,[1,[0,_78,_7e],[1,[0,_z,_7d],[1,[0,_73,_7f],_a]]]);});return !E(_7g)?E(_7j):_Q(_7j,_77);}),_7k=E(_7h);return _7k[0]==0?E(_7i):_Q(_7i,[1,[0,_74,_7k[1]],_a]);},_7l=unCStr("href"),_7m=[0,97],_7n=[1,_7m,_a],_7o=function(_7p,_){return _Z(_7n,_7p,_);},_7q=function(_7r,_7s){var _7t=new T(function(){return _Q(_7o,[1,[0,_7l,_7r],_a]);});return function(_7u,_){var _7v=A(_7t,[_7u,_]),_7w=A(_7s,[_7v,_]);return _7v;};},_7x=function(_7y){return _7q(_7y,function(_X,_){return _52(_7y,_X,_);});},_7z=unCStr("option"),_7A=function(_7B,_){return _Z(_7z,_7B,_);},_7C=unCStr("selected"),_7D=[0,_7C,_a],_7E=[1,_7D,_a],_7F=function(_7G,_7H,_7I){var _7J=new T(function(){return _Q(_7A,[1,[0,_73,_7G],_a]);}),_7K=function(_7L,_){var _7M=A(_7J,[_7L,_]),_7N=A(_7H,[_7M,_]);return _7M;};return !E(_7I)?E(_7K):_Q(_7K,_7E);},_7O=function(_7P,_7Q){return _7F(_7P,function(_X,_){return _52(_7P,_X,_);},_7Q);},_7R=unCStr("method"),_7S=unCStr("action"),_7T=unCStr("UTF-8"),_7U=unCStr("acceptCharset"),_7V=[0,_7U,_7T],_7W=unCStr("form"),_7X=function(_7Y,_){return _Z(_7W,_7Y,_);},_7Z=function(_80,_81,_82){var _83=new T(function(){return _Q(_7X,[1,_7V,[1,[0,_7S,_80],[1,[0,_7R,_81],_a]]]);});return function(_84,_){var _85=A(_83,[_84,_]),_86=A(_82,[_85,_]);return _85;};},_87=unCStr("select"),_88=function(_89,_){return _Z(_87,_89,_);},_8a=function(_8b,_8c){var _8d=new T(function(){return _Q(_88,[1,[0,_z,_8b],_a]);});return function(_8e,_){var _8f=A(_8d,[_8e,_]),_8g=A(_8c,[_8f,_]);return _8f;};},_8h=unCStr("textarea"),_8i=function(_8j,_){return _Z(_8h,_8j,_);},_8k=function(_8l,_8m){var _8n=new T(function(){return _Q(_8i,[1,[0,_z,_8l],_a]);});return function(_8o,_){var _8p=A(_8n,[_8o,_]),_8q=_52(_8m,_8p,_);return _8p;};},_8r=unCStr("color:red"),_8s=unCStr("style"),_8t=[0,_8s,_8r],_8u=[1,_8t,_a],_8v=[0,98],_8w=[1,_8v,_a],_8x=function(_8y){return _Q(function(_8z,_){var _8A=_Z(_8w,_8z,_),_8B=A(_8y,[_8A,_]);return _8A;},_8u);},_8C=function(_8D,_8E,_){var _8F=E(_8D);if(!_8F[0]){return _8E;}else{var _8G=A(_8F[1],[_8E,_]),_8H=_8C(_8F[2],_8E,_);return _8E;}},_8I=function(_8J,_8K,_8L,_){var _8M=A(_8J,[_8L,_]),_8N=A(_8K,[_8L,_]);return _8L;},_8O=[0,_34,_8I,_8C],_8P=[0,_8O,_72,_52,_52,_5Q,_8x,_7q,_7x,_7c,_8k,_8a,_7F,_7O,_7Z,_Q],_8Q=function(_8R,_8S,_){var _8T=A(_8S,[_]);return _8R;},_8U=function(_8V,_8W,_){var _8X=A(_8W,[_]);return new T(function(){return A(_8V,[_8X]);});},_8Y=[0,_8U,_8Q],_8Z=function(_90){var _91=E(_90);return _91[0]==0?0:E(_91[1])[1]+_8Z(_91[2])|0;},_92=function(_93){return [0,_8Z(_93)];},_94=function(_95,_96){return [0,E(_95)[1]+E(_96)[1]|0];},_97=[0,_5L,_94,_92],_98=function(_99,_9a){var _9b=E(_9a);return _9b[0]==0?[0]:[1,new T(function(){return A(_99,[_9b[1]]);})];},_9c=function(_9d){return E(E(_9d)[1]);},_9e=function(_9f){return E(E(_9f)[2]);},_9g=function(_9h,_9i,_9j,_9k,_9l,_9m){var _9n=new T(function(){return _9e(_9h);});return A(_9i,[new T(function(){return A(_9k,[_9m]);}),function(_9o){var _9p=E(_9o),_9q=E(_9p[1]);return A(_9i,[new T(function(){return A(_9l,[_9p[2]]);}),function(_9r){var _9s=E(_9r),_9t=E(_9s[1]);return A(_9j,[[0,[0,new T(function(){return A(_9n,[_9q[1],_9t[1]]);}),new T(function(){var _9u=E(_9q[2]);if(!_9u[0]){return [0];}else{var _9v=E(_9t[2]);return _9v[0]==0?[0]:[1,new T(function(){return A(_9u[1],[_9v[1]]);})];}})],_9s[2]]]);}]);}]);},_9w=function(_9x){return E(E(_9x)[1]);},_9y=function(_9z,_9A,_9B,_9C,_9D,_9E){var _9F=new T(function(){return _9c(_9z);});return function(_9G){var _9H=E(_9A);return _9g(_9F,_9H[1],_9H[3],function(_9I){return A(new T(function(){var _9J=new T(function(){return _9e(_9C);});return A(_9w,[_9B,function(_9K){return [0,new T(function(){var _9L=E(E(_9K)[1]);return [0,_9L[1],new T(function(){return _98(_9J,_9L[2]);})];}),new T(function(){return E(E(_9K)[2]);})];}]);}),[new T(function(){return A(_9D,[_9I]);})]);},_9E,_9G);};},_9M=function(_9N,_9O){while(1){var _9P=(function(_9Q,_9R){var _9S=E(_9R);if(!_9S[0]){return E(_9Q);}else{_9N=new T(function(){return _9y(_8P,_36,_8Y,_97,_9Q,_9S[1]);});_9O=_9S[2];return null;}})(_9N,_9O);if(_9P!=null){return _9P;}}},_9T=[13,coercionToken],_9U=unCStr("text"),_9V=[0,_36,_H],_9W=unCStr("base"),_9X=unCStr("Control.Exception.Base"),_9Y=unCStr("PatternMatchFail"),_9Z=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9W,_9X,_9Y],_a0=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9Z,_a],_a1=function(_a2){return E(_a0);},_a3=function(_a4){var _a5=E(_a4);return _1x(_1t(_a5[1]),_a1,_a5[2]);},_a6=function(_a7){return E(E(_a7)[1]);},_a8=function(_a9,_aa){return _1N(E(_a9)[1],_aa);},_ab=function(_ac,_ad){return _2D(_a8,_ac,_ad);},_ae=function(_af,_ag,_ah){return _1N(E(_ag)[1],_ah);},_ai=[0,_ae,_a6,_ab],_aj=new T(function(){return [0,_a1,_ai,_ak,_a3];}),_ak=function(_al){return [0,_aj,_al];},_am=unCStr("Non-exhaustive patterns in"),_an=function(_ao,_ap){return die(new T(function(){return A(_ap,[_ao]);}));},_aq=function(_ar,_as){var _at=E(_as);if(!_at[0]){return [0,_a,_a];}else{var _au=_at[1];if(!A(_ar,[_au])){return [0,_a,_at];}else{var _av=new T(function(){var _aw=_aq(_ar,_at[2]);return [0,_aw[1],_aw[2]];});return [0,[1,_au,new T(function(){return E(E(_av)[1]);})],new T(function(){return E(E(_av)[2]);})];}}},_ax=[0,32],_ay=[0,10],_az=[1,_ay,_a],_aA=function(_aB){return E(E(_aB)[1])==124?false:true;},_aC=function(_aD,_aE){var _aF=_aq(_aA,unCStr(_aD)),_aG=_aF[1],_aH=function(_aI,_aJ){return _1N(_aI,new T(function(){return unAppCStr(": ",new T(function(){return _1N(_aE,new T(function(){return _1N(_aJ,_az);}));}));}));},_aK=E(_aF[2]);return _aK[0]==0?_aH(_aG,_a):E(E(_aK[1])[1])==124?_aH(_aG,[1,_ax,_aK[2]]):_aH(_aG,_a);},_aL=function(_aM){return _an([0,new T(function(){return _aC(_aM,_am);})],_ak);},_aN=new T(function(){return _aL("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aO=function(_aP,_aQ){while(1){var _aR=(function(_aS,_aT){var _aU=E(_aS);switch(_aU[0]){case 0:var _aV=E(_aT);if(!_aV[0]){return [0];}else{_aP=A(_aU[1],[_aV[1]]);_aQ=_aV[2];return null;}break;case 1:var _aW=A(_aU[1],[_aT]),_aX=_aT;_aP=_aW;_aQ=_aX;return null;case 2:return [0];case 3:return [1,[0,_aU[1],_aT],new T(function(){return _aO(_aU[2],_aT);})];default:return E(_aU[1]);}})(_aP,_aQ);if(_aR!=null){return _aR;}}},_aY=function(_aZ,_b0){var _b1=new T(function(){var _b2=E(_b0);if(_b2[0]==3){return [3,_b2[1],new T(function(){return _aY(_aZ,_b2[2]);})];}else{var _b3=E(_aZ);if(_b3[0]==2){return E(_b2);}else{var _b4=E(_b2);if(_b4[0]==2){return E(_b3);}else{var _b5=new T(function(){var _b6=E(_b4);if(_b6[0]==4){return [1,function(_b7){return [4,new T(function(){return _1N(_aO(_b3,_b7),_b6[1]);})];}];}else{var _b8=E(_b3);if(_b8[0]==1){var _b9=_b8[1],_ba=E(_b6);return _ba[0]==0?[1,function(_bb){return _aY(A(_b9,[_bb]),_ba);}]:[1,function(_bc){return _aY(A(_b9,[_bc]),new T(function(){return A(_ba[1],[_bc]);}));}];}else{var _bd=E(_b6);return _bd[0]==0?E(_aN):[1,function(_be){return _aY(_b8,new T(function(){return A(_bd[1],[_be]);}));}];}}}),_bf=E(_b3);switch(_bf[0]){case 1:var _bg=E(_b4);return _bg[0]==4?[1,function(_bh){return [4,new T(function(){return _1N(_aO(A(_bf[1],[_bh]),_bh),_bg[1]);})];}]:E(_b5);case 4:var _bi=_bf[1],_bj=E(_b4);switch(_bj[0]){case 0:return [1,function(_bk){return [4,new T(function(){return _1N(_bi,new T(function(){return _aO(_bj,_bk);}));})];}];case 1:return [1,function(_bl){return [4,new T(function(){return _1N(_bi,new T(function(){return _aO(A(_bj[1],[_bl]),_bl);}));})];}];default:return [4,new T(function(){return _1N(_bi,_bj[1]);})];}break;default:return E(_b5);}}}}}),_bm=E(_aZ);switch(_bm[0]){case 0:var _bn=E(_b0);return _bn[0]==0?[0,function(_bo){return _aY(A(_bm[1],[_bo]),new T(function(){return A(_bn[1],[_bo]);}));}]:E(_b1);case 3:return [3,_bm[1],new T(function(){return _aY(_bm[2],_b0);})];default:return E(_b1);}},_bp=function(_bq,_br){return E(_bq)[1]!=E(_br)[1];},_bs=function(_bt,_bu){return E(_bt)[1]==E(_bu)[1];},_bv=[0,_bs,_bp],_bw=function(_bx){return E(E(_bx)[1]);},_by=function(_bz,_bA,_bB){while(1){var _bC=E(_bA);if(!_bC[0]){return E(_bB)[0]==0?true:false;}else{var _bD=E(_bB);if(!_bD[0]){return false;}else{if(!A(_bw,[_bz,_bC[1],_bD[1]])){return false;}else{_bA=_bC[2];_bB=_bD[2];continue;}}}}},_bE=function(_bF,_bG,_bH){return !_by(_bF,_bG,_bH)?true:false;},_bI=function(_bJ){return [0,function(_bK,_bL){return _by(_bJ,_bK,_bL);},function(_bK,_bL){return _bE(_bJ,_bK,_bL);}];},_bM=new T(function(){return _bI(_bv);}),_bN=function(_bO,_bP){var _bQ=E(_bO);switch(_bQ[0]){case 0:return [0,function(_bR){return _bN(A(_bQ[1],[_bR]),_bP);}];case 1:return [1,function(_bS){return _bN(A(_bQ[1],[_bS]),_bP);}];case 2:return [2];case 3:return _aY(A(_bP,[_bQ[1]]),new T(function(){return _bN(_bQ[2],_bP);}));default:var _bT=function(_bU){var _bV=E(_bU);if(!_bV[0]){return [0];}else{var _bW=E(_bV[1]);return _1N(_aO(A(_bP,[_bW[1]]),_bW[2]),new T(function(){return _bT(_bV[2]);}));}},_bX=_bT(_bQ[1]);return _bX[0]==0?[2]:[4,_bX];}},_bY=[2],_bZ=function(_c0){return [3,_c0,_bY];},_c1=function(_c2,_c3){var _c4=E(_c2);if(!_c4){return A(_c3,[_A]);}else{var _c5=new T(function(){return _c1(_c4-1|0,_c3);});return [0,function(_c6){return E(_c5);}];}},_c7=function(_c8,_c9,_ca){var _cb=new T(function(){return A(_c8,[_bZ]);});return [1,function(_cc){return A(function(_cd,_ce,_cf){while(1){var _cg=(function(_ch,_ci,_cj){var _ck=E(_ch);switch(_ck[0]){case 0:var _cl=E(_ci);if(!_cl[0]){return E(_c9);}else{_cd=A(_ck[1],[_cl[1]]);_ce=_cl[2];var _cm=_cj+1|0;_cf=_cm;return null;}break;case 1:var _cn=A(_ck[1],[_ci]),_co=_ci,_cm=_cj;_cd=_cn;_ce=_co;_cf=_cm;return null;case 2:return E(_c9);case 3:return function(_cp){var _cq=new T(function(){return _bN(_ck,_cp);});return _c1(_cj,function(_cr){return E(_cq);});};default:return function(_cs){return _bN(_ck,_cs);};}})(_cd,_ce,_cf);if(_cg!=null){return _cg;}}},[_cb,_cc,0,_ca]);}];},_ct=[6],_cu=unCStr("valDig: Bad base"),_cv=new T(function(){return err(_cu);}),_cw=function(_cx,_cy){var _cz=function(_cA,_cB){var _cC=E(_cA);if(!_cC[0]){var _cD=new T(function(){return A(_cB,[_a]);});return function(_cE){return A(_cE,[_cD]);};}else{var _cF=E(_cC[1])[1],_cG=function(_cH){var _cI=new T(function(){return _cz(_cC[2],function(_cJ){return A(_cB,[[1,_cH,_cJ]]);});});return function(_cK){var _cL=new T(function(){return A(_cI,[_cK]);});return [0,function(_cM){return E(_cL);}];};};switch(E(E(_cx)[1])){case 8:if(48>_cF){var _cN=new T(function(){return A(_cB,[_a]);});return function(_cO){return A(_cO,[_cN]);};}else{if(_cF>55){var _cP=new T(function(){return A(_cB,[_a]);});return function(_cQ){return A(_cQ,[_cP]);};}else{return _cG([0,_cF-48|0]);}}break;case 10:if(48>_cF){var _cR=new T(function(){return A(_cB,[_a]);});return function(_cS){return A(_cS,[_cR]);};}else{if(_cF>57){var _cT=new T(function(){return A(_cB,[_a]);});return function(_cU){return A(_cU,[_cT]);};}else{return _cG([0,_cF-48|0]);}}break;case 16:var _cV=new T(function(){return 97>_cF?65>_cF?[0]:_cF>70?[0]:[1,[0,(_cF-65|0)+10|0]]:_cF>102?65>_cF?[0]:_cF>70?[0]:[1,[0,(_cF-65|0)+10|0]]:[1,[0,(_cF-97|0)+10|0]];});if(48>_cF){var _cW=E(_cV);if(!_cW[0]){var _cX=new T(function(){return A(_cB,[_a]);});return function(_cY){return A(_cY,[_cX]);};}else{return _cG(_cW[1]);}}else{if(_cF>57){var _cZ=E(_cV);if(!_cZ[0]){var _d0=new T(function(){return A(_cB,[_a]);});return function(_d1){return A(_d1,[_d0]);};}else{return _cG(_cZ[1]);}}else{return _cG([0,_cF-48|0]);}}break;default:return E(_cv);}}};return [1,function(_d2){return A(_cz,[_d2,_H,function(_d3){var _d4=E(_d3);return _d4[0]==0?[2]:A(_cy,[_d4]);}]);}];},_d5=[0,10],_d6=[0,1],_d7=[0,2147483647],_d8=function(_d9,_da){while(1){var _db=E(_d9);if(!_db[0]){var _dc=_db[1],_dd=E(_da);if(!_dd[0]){var _de=_dd[1],_df=addC(_dc,_de);if(!E(_df[2])){return [0,_df[1]];}else{_d9=[1,I_fromInt(_dc)];_da=[1,I_fromInt(_de)];continue;}}else{_d9=[1,I_fromInt(_dc)];_da=_dd;continue;}}else{var _dg=E(_da);if(!_dg[0]){_d9=_db;_da=[1,I_fromInt(_dg[1])];continue;}else{return [1,I_add(_db[1],_dg[1])];}}}},_dh=new T(function(){return _d8(_d7,_d6);}),_di=function(_dj){var _dk=E(_dj);if(!_dk[0]){var _dl=E(_dk[1]);return _dl==(-2147483648)?E(_dh):[0, -_dl];}else{return [1,I_negate(_dk[1])];}},_dm=[0,10],_dn=[0,0],_do=function(_dp,_dq){while(1){var _dr=E(_dp);if(!_dr[0]){var _ds=_dr[1],_dt=E(_dq);if(!_dt[0]){var _du=_dt[1];if(!(imul(_ds,_du)|0)){return [0,imul(_ds,_du)|0];}else{_dp=[1,I_fromInt(_ds)];_dq=[1,I_fromInt(_du)];continue;}}else{_dp=[1,I_fromInt(_ds)];_dq=_dt;continue;}}else{var _dv=E(_dq);if(!_dv[0]){_dp=_dr;_dq=[1,I_fromInt(_dv[1])];continue;}else{return [1,I_mul(_dr[1],_dv[1])];}}}},_dw=function(_dx,_dy,_dz){while(1){var _dA=E(_dz);if(!_dA[0]){return E(_dy);}else{var _dB=_d8(_do(_dy,_dx),_dA[1]);_dz=_dA[2];_dy=_dB;continue;}}},_dC=function(_dD){var _dE=new T(function(){return _aY(_aY([0,function(_dF){return E(E(_dF)[1])==45?_cw(_d5,function(_dG){return A(_dD,[[1,new T(function(){return _di(_dw(_dm,_dn,_dG));})]]);}):[2];}],[0,function(_dH){return E(E(_dH)[1])==43?_cw(_d5,function(_dI){return A(_dD,[[1,new T(function(){return _dw(_dm,_dn,_dI);})]]);}):[2];}]),new T(function(){return _cw(_d5,function(_dJ){return A(_dD,[[1,new T(function(){return _dw(_dm,_dn,_dJ);})]]);});}));});return _aY([0,function(_dK){return E(E(_dK)[1])==101?E(_dE):[2];}],[0,function(_dL){return E(E(_dL)[1])==69?E(_dE):[2];}]);},_dM=function(_dN){return A(_dN,[_b]);},_dO=function(_dP){return A(_dP,[_b]);},_dQ=function(_dR){var _dS=new T(function(){return _cw(_d5,function(_dT){return A(_dR,[[1,_dT]]);});});return [0,function(_dU){return E(E(_dU)[1])==46?E(_dS):[2];}];},_dV=function(_dW){return _cw(_d5,function(_dX){return _c7(_dQ,_dM,function(_dY){return _c7(_dC,_dO,function(_dZ){return A(_dW,[[5,[1,_dX,_dY,_dZ]]]);});});});},_e0=function(_e1,_e2,_e3){while(1){var _e4=E(_e3);if(!_e4[0]){return false;}else{if(!A(_bw,[_e1,_e2,_e4[1]])){_e3=_e4[2];continue;}else{return true;}}}},_e5=unCStr("!@#$%&*+./<=>?\\^|:-~"),_e6=function(_e7){return _e0(_bv,_e7,_e5);},_e8=[0,8],_e9=[0,16],_ea=function(_eb){var _ec=new T(function(){return _cw(_e9,function(_ed){return A(_eb,[[5,[0,_e9,_ed]]]);});}),_ee=new T(function(){return _cw(_e8,function(_ef){return A(_eb,[[5,[0,_e8,_ef]]]);});}),_eg=new T(function(){return _cw(_e9,function(_eh){return A(_eb,[[5,[0,_e9,_eh]]]);});}),_ei=new T(function(){return _cw(_e8,function(_ej){return A(_eb,[[5,[0,_e8,_ej]]]);});});return [0,function(_ek){return E(E(_ek)[1])==48?E([0,function(_el){switch(E(E(_el)[1])){case 79:return E(_ei);case 88:return E(_eg);case 111:return E(_ee);case 120:return E(_ec);default:return [2];}}]):[2];}];},_em=true,_en=function(_eo){var _ep=new T(function(){return A(_eo,[_e9]);}),_eq=new T(function(){return A(_eo,[_e8]);}),_er=new T(function(){return A(_eo,[_e9]);}),_es=new T(function(){return A(_eo,[_e8]);});return [0,function(_et){switch(E(E(_et)[1])){case 79:return E(_es);case 88:return E(_er);case 111:return E(_eq);case 120:return E(_ep);default:return [2];}}];},_eu=function(_ev){return A(_ev,[_d5]);},_ew=function(_ex){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3M(9,_ex,_a);})));},_ey=function(_ez){var _eA=E(_ez);return _eA[0]==0?E(_eA[1]):I_toInt(_eA[1]);},_eB=function(_eC,_eD){var _eE=E(_eC);if(!_eE[0]){var _eF=_eE[1],_eG=E(_eD);return _eG[0]==0?_eF<=_eG[1]:I_compareInt(_eG[1],_eF)>=0;}else{var _eH=_eE[1],_eI=E(_eD);return _eI[0]==0?I_compareInt(_eH,_eI[1])<=0:I_compare(_eH,_eI[1])<=0;}},_eJ=function(_eK){return [2];},_eL=function(_eM){var _eN=E(_eM);if(!_eN[0]){return E(_eJ);}else{var _eO=_eN[1],_eP=E(_eN[2]);if(!_eP[0]){return E(_eO);}else{var _eQ=new T(function(){return _eL(_eP);});return function(_eR){return _aY(A(_eO,[_eR]),new T(function(){return A(_eQ,[_eR]);}));};}}},_eS=unCStr("NUL"),_eT=function(_eU){return [2];},_eV=function(_eW){return _eT(_eW);},_eX=function(_eY,_eZ){var _f0=function(_f1,_f2){var _f3=E(_f1);if(!_f3[0]){return function(_f4){return A(_f4,[_eY]);};}else{var _f5=E(_f2);if(!_f5[0]){return E(_eT);}else{if(E(_f3[1])[1]!=E(_f5[1])[1]){return E(_eV);}else{var _f6=new T(function(){return _f0(_f3[2],_f5[2]);});return function(_f7){var _f8=new T(function(){return A(_f6,[_f7]);});return [0,function(_f9){return E(_f8);}];};}}}};return [1,function(_fa){return A(_f0,[_eY,_fa,_eZ]);}];},_fb=[0,0],_fc=function(_fd){var _fe=new T(function(){return A(_fd,[_fb]);});return _eX(_eS,function(_ff){return E(_fe);});},_fg=unCStr("STX"),_fh=[0,2],_fi=function(_fj){var _fk=new T(function(){return A(_fj,[_fh]);});return _eX(_fg,function(_fl){return E(_fk);});},_fm=unCStr("ETX"),_fn=[0,3],_fo=function(_fp){var _fq=new T(function(){return A(_fp,[_fn]);});return _eX(_fm,function(_fr){return E(_fq);});},_fs=unCStr("EOT"),_ft=[0,4],_fu=function(_fv){var _fw=new T(function(){return A(_fv,[_ft]);});return _eX(_fs,function(_fx){return E(_fw);});},_fy=unCStr("ENQ"),_fz=[0,5],_fA=function(_fB){var _fC=new T(function(){return A(_fB,[_fz]);});return _eX(_fy,function(_fD){return E(_fC);});},_fE=unCStr("ACK"),_fF=[0,6],_fG=function(_fH){var _fI=new T(function(){return A(_fH,[_fF]);});return _eX(_fE,function(_fJ){return E(_fI);});},_fK=unCStr("BEL"),_fL=[0,7],_fM=function(_fN){var _fO=new T(function(){return A(_fN,[_fL]);});return _eX(_fK,function(_fP){return E(_fO);});},_fQ=unCStr("BS"),_fR=[0,8],_fS=function(_fT){var _fU=new T(function(){return A(_fT,[_fR]);});return _eX(_fQ,function(_fV){return E(_fU);});},_fW=unCStr("HT"),_fX=[0,9],_fY=function(_fZ){var _g0=new T(function(){return A(_fZ,[_fX]);});return _eX(_fW,function(_g1){return E(_g0);});},_g2=unCStr("LF"),_g3=[0,10],_g4=function(_g5){var _g6=new T(function(){return A(_g5,[_g3]);});return _eX(_g2,function(_g7){return E(_g6);});},_g8=unCStr("VT"),_g9=[0,11],_ga=function(_gb){var _gc=new T(function(){return A(_gb,[_g9]);});return _eX(_g8,function(_gd){return E(_gc);});},_ge=unCStr("FF"),_gf=[0,12],_gg=function(_gh){var _gi=new T(function(){return A(_gh,[_gf]);});return _eX(_ge,function(_gj){return E(_gi);});},_gk=unCStr("CR"),_gl=[0,13],_gm=function(_gn){var _go=new T(function(){return A(_gn,[_gl]);});return _eX(_gk,function(_gp){return E(_go);});},_gq=unCStr("SI"),_gr=[0,15],_gs=function(_gt){var _gu=new T(function(){return A(_gt,[_gr]);});return _eX(_gq,function(_gv){return E(_gu);});},_gw=unCStr("DLE"),_gx=[0,16],_gy=function(_gz){var _gA=new T(function(){return A(_gz,[_gx]);});return _eX(_gw,function(_gB){return E(_gA);});},_gC=unCStr("DC1"),_gD=[0,17],_gE=function(_gF){var _gG=new T(function(){return A(_gF,[_gD]);});return _eX(_gC,function(_gH){return E(_gG);});},_gI=unCStr("DC2"),_gJ=[0,18],_gK=function(_gL){var _gM=new T(function(){return A(_gL,[_gJ]);});return _eX(_gI,function(_gN){return E(_gM);});},_gO=unCStr("DC3"),_gP=[0,19],_gQ=function(_gR){var _gS=new T(function(){return A(_gR,[_gP]);});return _eX(_gO,function(_gT){return E(_gS);});},_gU=unCStr("DC4"),_gV=[0,20],_gW=function(_gX){var _gY=new T(function(){return A(_gX,[_gV]);});return _eX(_gU,function(_gZ){return E(_gY);});},_h0=unCStr("NAK"),_h1=[0,21],_h2=function(_h3){var _h4=new T(function(){return A(_h3,[_h1]);});return _eX(_h0,function(_h5){return E(_h4);});},_h6=unCStr("SYN"),_h7=[0,22],_h8=function(_h9){var _ha=new T(function(){return A(_h9,[_h7]);});return _eX(_h6,function(_hb){return E(_ha);});},_hc=unCStr("ETB"),_hd=[0,23],_he=function(_hf){var _hg=new T(function(){return A(_hf,[_hd]);});return _eX(_hc,function(_hh){return E(_hg);});},_hi=unCStr("CAN"),_hj=[0,24],_hk=function(_hl){var _hm=new T(function(){return A(_hl,[_hj]);});return _eX(_hi,function(_hn){return E(_hm);});},_ho=unCStr("EM"),_hp=[0,25],_hq=function(_hr){var _hs=new T(function(){return A(_hr,[_hp]);});return _eX(_ho,function(_ht){return E(_hs);});},_hu=unCStr("SUB"),_hv=[0,26],_hw=function(_hx){var _hy=new T(function(){return A(_hx,[_hv]);});return _eX(_hu,function(_hz){return E(_hy);});},_hA=unCStr("ESC"),_hB=[0,27],_hC=function(_hD){var _hE=new T(function(){return A(_hD,[_hB]);});return _eX(_hA,function(_hF){return E(_hE);});},_hG=unCStr("FS"),_hH=[0,28],_hI=function(_hJ){var _hK=new T(function(){return A(_hJ,[_hH]);});return _eX(_hG,function(_hL){return E(_hK);});},_hM=unCStr("GS"),_hN=[0,29],_hO=function(_hP){var _hQ=new T(function(){return A(_hP,[_hN]);});return _eX(_hM,function(_hR){return E(_hQ);});},_hS=unCStr("RS"),_hT=[0,30],_hU=function(_hV){var _hW=new T(function(){return A(_hV,[_hT]);});return _eX(_hS,function(_hX){return E(_hW);});},_hY=unCStr("US"),_hZ=[0,31],_i0=function(_i1){var _i2=new T(function(){return A(_i1,[_hZ]);});return _eX(_hY,function(_i3){return E(_i2);});},_i4=unCStr("SP"),_i5=[0,32],_i6=function(_i7){var _i8=new T(function(){return A(_i7,[_i5]);});return _eX(_i4,function(_i9){return E(_i8);});},_ia=unCStr("DEL"),_ib=[0,127],_ic=function(_id){var _ie=new T(function(){return A(_id,[_ib]);});return _eX(_ia,function(_if){return E(_ie);});},_ig=[1,_ic,_a],_ih=[1,_i6,_ig],_ii=[1,_i0,_ih],_ij=[1,_hU,_ii],_ik=[1,_hO,_ij],_il=[1,_hI,_ik],_im=[1,_hC,_il],_in=[1,_hw,_im],_io=[1,_hq,_in],_ip=[1,_hk,_io],_iq=[1,_he,_ip],_ir=[1,_h8,_iq],_is=[1,_h2,_ir],_it=[1,_gW,_is],_iu=[1,_gQ,_it],_iv=[1,_gK,_iu],_iw=[1,_gE,_iv],_ix=[1,_gy,_iw],_iy=[1,_gs,_ix],_iz=[1,_gm,_iy],_iA=[1,_gg,_iz],_iB=[1,_ga,_iA],_iC=[1,_g4,_iB],_iD=[1,_fY,_iC],_iE=[1,_fS,_iD],_iF=[1,_fM,_iE],_iG=[1,_fG,_iF],_iH=[1,_fA,_iG],_iI=[1,_fu,_iH],_iJ=[1,_fo,_iI],_iK=[1,_fi,_iJ],_iL=[1,_fc,_iK],_iM=unCStr("SOH"),_iN=[0,1],_iO=function(_iP){var _iQ=new T(function(){return A(_iP,[_iN]);});return _eX(_iM,function(_iR){return E(_iQ);});},_iS=unCStr("SO"),_iT=[0,14],_iU=function(_iV){var _iW=new T(function(){return A(_iV,[_iT]);});return _eX(_iS,function(_iX){return E(_iW);});},_iY=function(_iZ){return _c7(_iO,_iU,_iZ);},_j0=[1,_iY,_iL],_j1=new T(function(){return _eL(_j0);}),_j2=[0,1114111],_j3=[0,34],_j4=[0,_j3,_em],_j5=[0,39],_j6=[0,_j5,_em],_j7=[0,92],_j8=[0,_j7,_em],_j9=[0,_fL,_em],_ja=[0,_fR,_em],_jb=[0,_gf,_em],_jc=[0,_g3,_em],_jd=[0,_gl,_em],_je=[0,_fX,_em],_jf=[0,_g9,_em],_jg=[0,_fb,_em],_jh=[0,_iN,_em],_ji=[0,_fh,_em],_jj=[0,_fn,_em],_jk=[0,_ft,_em],_jl=[0,_fz,_em],_jm=[0,_fF,_em],_jn=[0,_fL,_em],_jo=[0,_fR,_em],_jp=[0,_fX,_em],_jq=[0,_g3,_em],_jr=[0,_g9,_em],_js=[0,_gf,_em],_jt=[0,_gl,_em],_ju=[0,_iT,_em],_jv=[0,_gr,_em],_jw=[0,_gx,_em],_jx=[0,_gD,_em],_jy=[0,_gJ,_em],_jz=[0,_gP,_em],_jA=[0,_gV,_em],_jB=[0,_h1,_em],_jC=[0,_h7,_em],_jD=[0,_hd,_em],_jE=[0,_hj,_em],_jF=[0,_hp,_em],_jG=[0,_hv,_em],_jH=[0,_hB,_em],_jI=[0,_hH,_em],_jJ=[0,_hN,_em],_jK=[0,_hT,_em],_jL=[0,_hZ,_em],_jM=function(_jN){return [0,_jN];},_jO=function(_jP){var _jQ=new T(function(){return A(_jP,[_jf]);}),_jR=new T(function(){return A(_jP,[_je]);}),_jS=new T(function(){return A(_jP,[_jd]);}),_jT=new T(function(){return A(_jP,[_jc]);}),_jU=new T(function(){return A(_jP,[_jb]);}),_jV=new T(function(){return A(_jP,[_ja]);}),_jW=new T(function(){return A(_jP,[_j9]);}),_jX=new T(function(){return A(_jP,[_j8]);}),_jY=new T(function(){return A(_jP,[_j6]);}),_jZ=new T(function(){return A(_jP,[_j4]);});return _aY([0,function(_k0){switch(E(E(_k0)[1])){case 34:return E(_jZ);case 39:return E(_jY);case 92:return E(_jX);case 97:return E(_jW);case 98:return E(_jV);case 102:return E(_jU);case 110:return E(_jT);case 114:return E(_jS);case 116:return E(_jR);case 118:return E(_jQ);default:return [2];}}],new T(function(){return _aY(_c7(_en,_eu,function(_k1){var _k2=new T(function(){return _jM(E(_k1)[1]);});return _cw(_k1,function(_k3){var _k4=_dw(_k2,_dn,_k3);return !_eB(_k4,_j2)?[2]:A(_jP,[[0,new T(function(){var _k5=_ey(_k4);return _k5>>>0>1114111?_ew(_k5):[0,_k5];}),_em]]);});}),new T(function(){var _k6=new T(function(){return A(_jP,[_jL]);}),_k7=new T(function(){return A(_jP,[_jK]);}),_k8=new T(function(){return A(_jP,[_jJ]);}),_k9=new T(function(){return A(_jP,[_jI]);}),_ka=new T(function(){return A(_jP,[_jH]);}),_kb=new T(function(){return A(_jP,[_jG]);}),_kc=new T(function(){return A(_jP,[_jF]);}),_kd=new T(function(){return A(_jP,[_jE]);}),_ke=new T(function(){return A(_jP,[_jD]);}),_kf=new T(function(){return A(_jP,[_jC]);}),_kg=new T(function(){return A(_jP,[_jB]);}),_kh=new T(function(){return A(_jP,[_jA]);}),_ki=new T(function(){return A(_jP,[_jz]);}),_kj=new T(function(){return A(_jP,[_jy]);}),_kk=new T(function(){return A(_jP,[_jx]);}),_kl=new T(function(){return A(_jP,[_jw]);}),_km=new T(function(){return A(_jP,[_jv]);}),_kn=new T(function(){return A(_jP,[_ju]);}),_ko=new T(function(){return A(_jP,[_jt]);}),_kp=new T(function(){return A(_jP,[_js]);}),_kq=new T(function(){return A(_jP,[_jr]);}),_kr=new T(function(){return A(_jP,[_jq]);}),_ks=new T(function(){return A(_jP,[_jp]);}),_kt=new T(function(){return A(_jP,[_jo]);}),_ku=new T(function(){return A(_jP,[_jn]);}),_kv=new T(function(){return A(_jP,[_jm]);}),_kw=new T(function(){return A(_jP,[_jl]);}),_kx=new T(function(){return A(_jP,[_jk]);}),_ky=new T(function(){return A(_jP,[_jj]);}),_kz=new T(function(){return A(_jP,[_ji]);}),_kA=new T(function(){return A(_jP,[_jh]);}),_kB=new T(function(){return A(_jP,[_jg]);});return _aY([0,function(_kC){return E(E(_kC)[1])==94?E([0,function(_kD){switch(E(E(_kD)[1])){case 64:return E(_kB);case 65:return E(_kA);case 66:return E(_kz);case 67:return E(_ky);case 68:return E(_kx);case 69:return E(_kw);case 70:return E(_kv);case 71:return E(_ku);case 72:return E(_kt);case 73:return E(_ks);case 74:return E(_kr);case 75:return E(_kq);case 76:return E(_kp);case 77:return E(_ko);case 78:return E(_kn);case 79:return E(_km);case 80:return E(_kl);case 81:return E(_kk);case 82:return E(_kj);case 83:return E(_ki);case 84:return E(_kh);case 85:return E(_kg);case 86:return E(_kf);case 87:return E(_ke);case 88:return E(_kd);case 89:return E(_kc);case 90:return E(_kb);case 91:return E(_ka);case 92:return E(_k9);case 93:return E(_k8);case 94:return E(_k7);case 95:return E(_k6);default:return [2];}}]):[2];}],new T(function(){return A(_j1,[function(_kE){return A(_jP,[[0,_kE,_em]]);}]);}));}));}));},_kF=function(_kG){return A(_kG,[_A]);},_kH=function(_kI){var _kJ=E(_kI);if(!_kJ[0]){return E(_kF);}else{var _kK=_kJ[2],_kL=E(E(_kJ[1])[1]);switch(_kL){case 9:var _kM=new T(function(){return _kH(_kK);});return function(_kN){var _kO=new T(function(){return A(_kM,[_kN]);});return [0,function(_kP){return E(_kO);}];};case 10:var _kQ=new T(function(){return _kH(_kK);});return function(_kR){var _kS=new T(function(){return A(_kQ,[_kR]);});return [0,function(_kT){return E(_kS);}];};case 11:var _kU=new T(function(){return _kH(_kK);});return function(_kV){var _kW=new T(function(){return A(_kU,[_kV]);});return [0,function(_kX){return E(_kW);}];};case 12:var _kY=new T(function(){return _kH(_kK);});return function(_kZ){var _l0=new T(function(){return A(_kY,[_kZ]);});return [0,function(_l1){return E(_l0);}];};case 13:var _l2=new T(function(){return _kH(_kK);});return function(_l3){var _l4=new T(function(){return A(_l2,[_l3]);});return [0,function(_l5){return E(_l4);}];};case 32:var _l6=new T(function(){return _kH(_kK);});return function(_l7){var _l8=new T(function(){return A(_l6,[_l7]);});return [0,function(_l9){return E(_l8);}];};case 160:var _la=new T(function(){return _kH(_kK);});return function(_lb){var _lc=new T(function(){return A(_la,[_lb]);});return [0,function(_ld){return E(_lc);}];};default:var _le=u_iswspace(_kL);if(!E(_le)){return E(_kF);}else{var _lf=new T(function(){return _kH(_kK);});return function(_lg){var _lh=new T(function(){return A(_lf,[_lg]);});return [0,function(_li){return E(_lh);}];};}}}},_lj=function(_lk){var _ll=new T(function(){return _jO(_lk);}),_lm=new T(function(){return _lj(_lk);}),_ln=[1,function(_lo){return A(_kH,[_lo,function(_lp){return E([0,function(_lq){return E(E(_lq)[1])==92?E(_lm):[2];}]);}]);}];return _aY([0,function(_lr){return E(E(_lr)[1])==92?E([0,function(_ls){var _lt=E(E(_ls)[1]);switch(_lt){case 9:return E(_ln);case 10:return E(_ln);case 11:return E(_ln);case 12:return E(_ln);case 13:return E(_ln);case 32:return E(_ln);case 38:return E(_lm);case 160:return E(_ln);default:var _lu=u_iswspace(_lt);return E(_lu)==0?[2]:E(_ln);}}]):[2];}],[0,function(_lv){var _lw=E(_lv);return E(_lw[1])==92?E(_ll):A(_lk,[[0,_lw,_0]]);}]);},_lx=function(_ly,_lz){var _lA=new T(function(){return A(_lz,[[1,new T(function(){return A(_ly,[_a]);})]]);});return _lj(function(_lB){var _lC=E(_lB),_lD=E(_lC[1]);return E(_lD[1])==34?!E(_lC[2])?E(_lA):_lx(function(_lE){return A(_ly,[[1,_lD,_lE]]);},_lz):_lx(function(_lF){return A(_ly,[[1,_lD,_lF]]);},_lz);});},_lG=unCStr("_\'"),_lH=function(_lI){var _lJ=u_iswalnum(_lI);return E(_lJ)==0?_e0(_bv,[0,_lI],_lG):true;},_lK=function(_lL){return _lH(E(_lL)[1]);},_lM=unCStr(",;()[]{}`"),_lN=function(_lO){return A(_lO,[_a]);},_lP=function(_lQ,_lR){var _lS=function(_lT){var _lU=E(_lT);if(!_lU[0]){return E(_lN);}else{var _lV=_lU[1];if(!A(_lQ,[_lV])){return E(_lN);}else{var _lW=new T(function(){return _lS(_lU[2]);});return function(_lX){var _lY=new T(function(){return A(_lW,[function(_lZ){return A(_lX,[[1,_lV,_lZ]]);}]);});return [0,function(_m0){return E(_lY);}];};}}};return [1,function(_m1){return A(_lS,[_m1,_lR]);}];},_m2=unCStr(".."),_m3=unCStr("::"),_m4=unCStr("->"),_m5=[0,64],_m6=[1,_m5,_a],_m7=[0,126],_m8=[1,_m7,_a],_m9=unCStr("=>"),_ma=[1,_m9,_a],_mb=[1,_m8,_ma],_mc=[1,_m6,_mb],_md=[1,_m4,_mc],_me=unCStr("<-"),_mf=[1,_me,_md],_mg=[0,124],_mh=[1,_mg,_a],_mi=[1,_mh,_mf],_mj=[1,_j7,_a],_mk=[1,_mj,_mi],_ml=[0,61],_mm=[1,_ml,_a],_mn=[1,_mm,_mk],_mo=[1,_m3,_mn],_mp=[1,_m2,_mo],_mq=function(_mr){var _ms=new T(function(){return A(_mr,[_ct]);});return _aY([1,function(_mt){return E(_mt)[0]==0?E(_ms):[2];}],new T(function(){var _mu=new T(function(){return _jO(function(_mv){var _mw=E(_mv);return (function(_mx,_my){var _mz=new T(function(){return A(_mr,[[0,_mx]]);});return !E(_my)?E(E(_mx)[1])==39?[2]:[0,function(_mA){return E(E(_mA)[1])==39?E(_mz):[2];}]:[0,function(_mB){return E(E(_mB)[1])==39?E(_mz):[2];}];})(_mw[1],_mw[2]);});});return _aY([0,function(_mC){return E(E(_mC)[1])==39?E([0,function(_mD){var _mE=E(_mD);switch(E(_mE[1])){case 39:return [2];case 92:return E(_mu);default:var _mF=new T(function(){return A(_mr,[[0,_mE]]);});return [0,function(_mG){return E(E(_mG)[1])==39?E(_mF):[2];}];}}]):[2];}],new T(function(){var _mH=new T(function(){return _lx(_H,_mr);});return _aY([0,function(_mI){return E(E(_mI)[1])==34?E(_mH):[2];}],new T(function(){return _aY([0,function(_mJ){return !_e0(_bv,_mJ,_lM)?[2]:A(_mr,[[2,[1,_mJ,_a]]]);}],new T(function(){return _aY([0,function(_mK){return !_e0(_bv,_mK,_e5)?[2]:_lP(_e6,function(_mL){var _mM=[1,_mK,_mL];return !_e0(_bM,_mM,_mp)?A(_mr,[[4,_mM]]):A(_mr,[[2,_mM]]);});}],new T(function(){return _aY([0,function(_mN){var _mO=E(_mN),_mP=_mO[1],_mQ=u_iswalpha(_mP);return E(_mQ)==0?E(_mP)==95?_lP(_lK,function(_mR){return A(_mr,[[3,[1,_mO,_mR]]]);}):[2]:_lP(_lK,function(_mS){return A(_mr,[[3,[1,_mO,_mS]]]);});}],new T(function(){return _c7(_ea,_dV,_mr);}));}));}));}));}));}));},_mT=function(_mU){var _mV=new T(function(){return _mq(_mU);});return [1,function(_mW){return A(_kH,[_mW,function(_mX){return E(_mV);}]);}];},_mY=[0,0],_mZ=function(_n0,_n1){var _n2=new T(function(){return A(_n0,[_mY,function(_n3){var _n4=new T(function(){return A(_n1,[_n3]);});return _mT(function(_n5){var _n6=E(_n5);if(_n6[0]==2){var _n7=E(_n6[1]);return _n7[0]==0?[2]:E(E(_n7[1])[1])==41?E(_n7[2])[0]==0?E(_n4):[2]:[2];}else{return [2];}});}]);});return _mT(function(_n8){var _n9=E(_n8);if(_n9[0]==2){var _na=E(_n9[1]);return _na[0]==0?[2]:E(E(_na[1])[1])==40?E(_na[2])[0]==0?E(_n2):[2]:[2];}else{return [2];}});},_nb=function(_nc,_nd,_ne){var _nf=function(_ng,_nh){var _ni=new T(function(){return _mq(function(_nj){return A(_nc,[_nj,_ng,function(_nk){return A(_nh,[new T(function(){return [0, -E(_nk)[1]];})]);}]);});});return _aY(_mT(function(_nl){var _nm=E(_nl);if(_nm[0]==4){var _nn=E(_nm[1]);return _nn[0]==0?A(_nc,[_nm,_ng,_nh]):E(E(_nn[1])[1])==45?E(_nn[2])[0]==0?E([1,function(_no){return A(_kH,[_no,function(_np){return E(_ni);}]);}]):A(_nc,[_nm,_ng,_nh]):A(_nc,[_nm,_ng,_nh]);}else{return A(_nc,[_nm,_ng,_nh]);}}),new T(function(){return _mZ(_nf,_nh);}));};return _nf(_nd,_ne);},_nq=function(_nr,_ns){return [2];},_nt=function(_nu,_nv){return _nq(_nu,_nv);},_nw=function(_nx){var _ny=E(_nx);return _ny[0]==0?[1,new T(function(){return _dw(new T(function(){return _jM(E(_ny[1])[1]);}),_dn,_ny[2]);})]:E(_ny[2])[0]==0?E(_ny[3])[0]==0?[1,new T(function(){return _dw(_dm,_dn,_ny[1]);})]:[0]:[0];},_nz=function(_nA){var _nB=E(_nA);if(_nB[0]==5){var _nC=_nw(_nB[1]);if(!_nC[0]){return E(_nq);}else{var _nD=new T(function(){return [0,_ey(_nC[1])];});return function(_nE,_nF){return A(_nF,[_nD]);};}}else{return E(_nt);}},_nG=function(_nu,_nv){return _nb(_nz,_nu,_nv);},_nH=function(_nI,_nJ){var _nK=function(_nL,_nM){var _nN=new T(function(){return A(_nM,[_a]);}),_nO=new T(function(){return A(_nI,[_mY,function(_nP){return _nK(_em,function(_nQ){return A(_nM,[[1,_nP,_nQ]]);});}]);});return _mT(function(_nR){var _nS=E(_nR);if(_nS[0]==2){var _nT=E(_nS[1]);if(!_nT[0]){return [2];}else{var _nU=_nT[2];switch(E(E(_nT[1])[1])){case 44:return E(_nU)[0]==0?!E(_nL)?[2]:E(_nO):[2];case 93:return E(_nU)[0]==0?E(_nN):[2];default:return [2];}}}else{return [2];}});},_nV=function(_nW){var _nX=new T(function(){return _aY(_nK(_0,_nW),new T(function(){return A(_nI,[_mY,function(_nY){return _nK(_em,function(_nZ){return A(_nW,[[1,_nY,_nZ]]);});}]);}));});return _aY(_mT(function(_o0){var _o1=E(_o0);if(_o1[0]==2){var _o2=E(_o1[1]);return _o2[0]==0?[2]:E(E(_o2[1])[1])==91?E(_o2[2])[0]==0?E(_nX):[2]:[2];}else{return [2];}}),new T(function(){return _mZ(function(_o3,_o4){return _nV(_o4);},_nW);}));};return _nV(_nJ);},_o5=function(_o6,_o7){return _nH(_nG,_o7);},_o8=new T(function(){return _nH(_nG,_bZ);}),_o9=function(_nv){return _aO(_o8,_nv);},_oa=function(_ob){var _oc=new T(function(){return _nb(_nz,_ob,_bZ);});return function(_cs){return _aO(_oc,_cs);};},_od=[0,_oa,_o9,_nG,_o5],_oe=function(_of,_og){return _3M(0,E(_of)[1],_og);},_oh=function(_oi,_oj){return _2D(_oe,_oi,_oj);},_ok=function(_ol,_om,_on){return _3M(E(_ol)[1],E(_om)[1],_on);},_oo=[0,_ok,_5i,_oh],_op=unCStr("GHC.Types"),_oq=unCStr("Int"),_or=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5Y,_op,_oq],_os=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_or,_a],_ot=function(_ou){return E(_os);},_ov=function(_ow){return E(E(_ow)[1]);},_ox=function(_oy){return E(E(_oy)[2]);},_oz=function(_oA,_oB){var _oC=new T(function(){return A(_ox,[_oA,_oB]);}),_oD=new T(function(){return _ov(_oA);}),_oE=new T(function(){return _3x(_oD);}),_oF=new T(function(){return _37(_oD);});return function(_oG){return A(_oF,[_oC,function(_oH){return A(_oE,[[0,_oH,_oG]]);}]);};},_oI=function(_oJ,_oK){return [0,_oJ,function(_oL){return _oz(_oK,_oL);}];},_oM=function(_oN,_oO){return A(_3x,[_oN,[0,_oO,_oO]]);},_oP=function(_oQ,_oR,_oS){return A(_3x,[_oQ,[0,_A,_oR]]);},_oT=function(_oU,_oV){return [0,_oU,function(_oW){return _oM(_oV,_oW);},function(_oX,_oY){return _oP(_oV,_oX,_oY);}];},_oZ=function(_p0){return E(E(_p0)[1]);},_p1=function(_p2,_p3){return A(_p2,[function(_){return jsFind(toJSStr(E(_p3)));}]);},_p4=function(_p5){return E(E(_p5)[3]);},_p6=unCStr("[]"),_p7=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5Y,_op,_p6],_p8=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_p7,_a],_p9=function(_pa){return E(_p8);},_pb=unCStr("Char"),_pc=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5Y,_op,_pb],_pd=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_pc,_a],_pe=function(_pf){return E(_pd);},_pg=new T(function(){return _6S(_p9,_pe);}),_ph=new T(function(){return A(_pg,[_6R]);}),_pi=new T(function(){return E(_6R);}),_pj=function(_pk){return E(E(_pk)[6]);},_pl=function(_pm){return E(E(_pm)[1]);},_pn=[0,0],_po=[0,32],_pp=[0,10],_pq=function(_pr){var _ps=E(_pr);if(!_ps[0]){return E(_H);}else{var _pt=_ps[1],_pu=E(_ps[2]);if(!_pu[0]){return _pv(_pp,_pt);}else{var _pw=new T(function(){return _pq(_pu);}),_px=new T(function(){return _pv(_pp,_pt);});return function(_py){return A(_px,[[1,_po,new T(function(){return A(_pw,[_py]);})]]);};}}},_pz=unCStr("->"),_pA=[1,_pz,_a],_pB=[1,_op,_pA],_pC=[1,_5Y,_pB],_pD=[0,32],_pE=function(_pF){var _pG=E(_pF);if(!_pG[0]){return [0];}else{var _pH=_pG[1],_pI=E(_pG[2]);return _pI[0]==0?E(_pH):_1N(_pH,[1,_pD,new T(function(){return _pE(_pI);})]);}},_pJ=new T(function(){return _pE(_pC);}),_pK=new T(function(){var _pL=_6m(_pJ);return [0,_pL[1],_pL[2],_5Y,_op,_pz];}),_pM=function(_pN,_pO){var _pP=E(_pN);return _pP[0]==0?E(_pO):A(_pP[1],[new T(function(){return _pM(_pP[2],_pO);})]);},_pQ=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pR=[1,_60,_a],_pS=function(_pT){var _pU=E(_pT);if(!_pU[0]){return [0];}else{var _pV=E(_pU[1]);return [1,[0,_pV[1],_pV[2]],new T(function(){return _pS(_pU[2]);})];}},_pW=new T(function(){var _pX=_1N(_a,_pR);if(!_pX[0]){return E(_p7);}else{var _pY=_6m(new T(function(){return _6a(_6y(_6J,[1,_pQ,new T(function(){return _pS(_pX);})]));}));return E(_p7);}}),_pZ=[0,40],_q0=function(_q1){return _pv(_pp,_q1);},_q2=[0,8],_q3=unCStr(" -> "),_q4=[0,9],_q5=[0,93],_q6=[0,91],_q7=[0,41],_q8=[0,44],_q9=function(_q1){return [1,_q8,_q1];},_qa=function(_qb,_qc){var _qd=E(_qc);return _qd[0]==0?[0]:[1,_qb,[1,_qd[1],new T(function(){return _qa(_qb,_qd[2]);})]];},_pv=function(_qe,_qf){var _qg=E(_qf),_qh=_qg[3],_qi=E(_qg[4]);if(!_qi[0]){return function(_qj){return _1N(E(_qh)[5],_qj);};}else{var _qk=_qi[1],_ql=new T(function(){var _qm=E(_qh)[5],_qn=new T(function(){return _pq(_qi);}),_qo=new T(function(){return E(_qe)[1]<=9?function(_qp){return _1N(_qm,[1,_po,new T(function(){return A(_qn,[_qp]);})]);}:function(_qq){return [1,_3L,new T(function(){return _1N(_qm,[1,_po,new T(function(){return A(_qn,[[1,_3K,_qq]]);})]);})];};}),_qr=E(_qm);if(!_qr[0]){return E(_qo);}else{if(E(E(_qr[1])[1])==40){var _qs=E(_qr[2]);return _qs[0]==0?E(_qo):E(E(_qs[1])[1])==44?function(_qt){return [1,_pZ,new T(function(){return A(new T(function(){var _qu=_6y(_q0,_qi);if(!_qu[0]){return E(_H);}else{var _qv=new T(function(){return _qa(_q9,_qu[2]);});return function(_cs){return _pM([1,_qu[1],_qv],_cs);};}}),[[1,_q7,_qt]]);})];}:E(_qo);}else{return E(_qo);}}}),_qw=E(_qi[2]);if(!_qw[0]){var _qx=E(_qh),_qy=E(_pW),_qz=hs_eqWord64(_qx[1],_qy[1]);if(!E(_qz)){return E(_ql);}else{var _qA=hs_eqWord64(_qx[2],_qy[2]);if(!E(_qA)){return E(_ql);}else{var _qB=new T(function(){return _pv(_pn,_qk);});return function(_qC){return [1,_q6,new T(function(){return A(_qB,[[1,_q5,_qC]]);})];};}}}else{if(!E(_qw[2])[0]){var _qD=E(_qh),_qE=E(_pK),_qF=hs_eqWord64(_qD[1],_qE[1]);if(!E(_qF)){return E(_ql);}else{var _qG=hs_eqWord64(_qD[2],_qE[2]);if(!E(_qG)){return E(_ql);}else{var _qH=new T(function(){return _pv(_q2,_qw[1]);}),_qI=new T(function(){return _pv(_q4,_qk);});return E(_qe)[1]<=8?function(_qJ){return A(_qI,[new T(function(){return _1N(_q3,new T(function(){return A(_qH,[_qJ]);}));})]);}:function(_qK){return [1,_3L,new T(function(){return A(_qI,[new T(function(){return _1N(_q3,new T(function(){return A(_qH,[[1,_3K,_qK]]);}));})]);})];};}}}else{return E(_ql);}}}},_qL=function(_qM,_qN,_qO,_qP){var _qQ=new T(function(){return _3x(_qM);}),_qR=new T(function(){return _p4(_qP);}),_qS=new T(function(){return _pj(_qP);}),_qT=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pv,[_pn,A(_qN,[_pi]),_a]);}));}),_qU=new T(function(){return A(_pl,[_qO,_g]);});return function(_qV){if(!E(new T(function(){var _qW=A(_qN,[_pi]),_qX=E(_ph),_qY=hs_eqWord64(_qW[1],_qX[1]);if(!E(_qY)){return false;}else{var _qZ=hs_eqWord64(_qW[2],_qX[2]);return E(_qZ)==0?false:true;}}))){var _r0=new T(function(){return A(_qQ,[[1,_qV,new T(function(){return A(_qS,[new T(function(){return A(_qR,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1N(_qV,_qT);}));})]);})]);})]]);}),_r1=A(_qU,[_qV]);if(!_r1[0]){return E(_r0);}else{var _r2=E(_r1[1]);return E(_r2[2])[0]==0?E(_r1[2])[0]==0?A(_qQ,[[2,_r2[1]]]):E(_r0):E(_r0);}}else{return A(_qQ,[[2,_qV]]);}};},_r3=[0],_r4=new T(function(){return [0,"value"];}),_r5=function(_r6,_r7,_r8,_r9,_ra,_rb){var _rc=E(_r6),_rd=_rc[1],_re=new T(function(){return A(_rc[3],[_r3]);}),_rf=new T(function(){return _qL(_rc,_r8,_r9,_ra);});return A(_rd,[new T(function(){return _p1(_r7,_rb);}),function(_rg){var _rh=E(_rg);return _rh[0]==0?E(_re):A(_rd,[new T(function(){return A(_r7,[function(_){var _ri=jsGet(E(_rh[1])[1],E(_r4)[1]);return [1,new T(function(){return fromJSStr(_ri);})];}]);}),function(_rj){var _rk=E(_rj);return _rk[0]==0?E(_re):A(_rf,[_rk[1]]);}]);}]);},_rl=1,_rm=function(_rn){return E(E(_rn)[9]);},_ro=function(_rp){return E(E(_rp)[2]);},_rq=function(_rr){return E(E(_rr)[3]);},_rs=function(_rt){return E(E(_rt)[2]);},_ru=function(_rv,_rw,_rx,_ry,_rz,_rA,_rB,_rC,_rD,_rE,_rF,_rG){var _rH=_oZ(_rA),_rI=_rH[1],_rJ=_rH[3],_rK=new T(function(){return _9c(_rC);}),_rL=new T(function(){return _9e(_rK);}),_rM=new T(function(){return _rq(_rA);}),_rN=new T(function(){return _rs(_rw);}),_rO=new T(function(){return _rm(_rC);});return A(_rI,[new T(function(){var _rP=E(_rE);if(!_rP[0]){var _rQ=E(_rA);return _3R(_rD,_rQ[1],_rQ[2],_rQ[3]);}else{return A(_rJ,[_rP[1]]);}}),function(_rR){return A(_rI,[new T(function(){var _rS=E(_rD);return _ro(_rA);}),function(_rT){return A(_rH[2],[new T(function(){return A(_rM,[new T(function(){var _rU=E(new T(function(){var _rV=E(_rD);return [0,coercionToken];})),_rW=E(_rT);return [0,_rW[1],_rW[2],_rl,_rW[4],_rW[5],_rW[6]];})]);}),new T(function(){var _rX=new T(function(){return A(_rJ,[[0,new T(function(){return A(_rO,[_rR,_rF,new T(function(){var _rY=E(_rG);if(!_rY[0]){return [0];}else{var _rZ=_rY[1],_s0=_1x(_rz,_pg,_rZ);return _s0[0]==0?A(_rs,[_rx,_rZ]):E(_s0[1]);}}),_0,_b]);}),_b]]);});return A(_rI,[new T(function(){var _s1=E(_rB);return _r5(_s1[1],_s1[2],_ry,_rv,_rC,_rR);}),function(_s2){var _s3=E(_s2);switch(_s3[0]){case 0:return E(_rX);case 1:return A(_rJ,[[0,new T(function(){return A(_rL,[new T(function(){return A(_rO,[_rR,_rF,_s3[1],_0,_b]);}),_s3[2]]);}),_b]]);default:var _s4=_s3[1];return A(_rJ,[[0,new T(function(){return A(_rO,[_rR,_rF,new T(function(){var _s5=_1x(_ry,_pg,_s4);return _s5[0]==0?A(_rN,[_s4]):E(_s5[1]);}),_0,_b]);}),[1,_s4]]]);}}]);})]);}]);}]);},_s6=function(_s7,_s8,_s9,_sa,_sb){var _sc=new T(function(){return _ov(_s8);}),_sd=new T(function(){return _3z(_sc);}),_se=new T(function(){return _oI(_sd,_s8);}),_sf=new T(function(){return _oT(_sd,_sc);});return function(_cs,_sg,_sh){return _ru(_sb,_sa,_sa,_s9,_s9,_sf,_se,_s7,[0,coercionToken],_cs,_sg,_sh);};},_si=new T(function(){return _s6(_8P,_9V,_ot,_oo,_od);}),_sj=new T(function(){return A(_si,[_b,_9U,_b]);}),_sk=unCStr("keydown"),_sl=unCStr("mousemove"),_sm=unCStr("blur"),_sn=unCStr("focus"),_so=unCStr("change"),_sp=unCStr("unload"),_sq=unCStr("load"),_sr=unCStr("keyup"),_ss=unCStr("keypress"),_st=unCStr("mouseup"),_su=unCStr("mousedown"),_sv=unCStr("dblclick"),_sw=unCStr("click"),_sx=unCStr("mouseout"),_sy=unCStr("mouseover"),_sz=function(_sA){switch(E(_sA)[0]){case 0:return E(_sq);case 1:return E(_sp);case 2:return E(_so);case 3:return E(_sn);case 4:return E(_sm);case 5:return E(_sl);case 6:return E(_sy);case 7:return E(_sx);case 8:return E(_sw);case 9:return E(_sv);case 10:return E(_su);case 11:return E(_st);case 12:return E(_ss);case 13:return E(_sr);default:return E(_sk);}},_sB=[0],_sC=unCStr("OnLoad"),_sD=[0,_sC,_sB],_sE=function(_){var _=0,_sF=newMVar(),_=putMVar(_sF,_sD);return [0,_sF];},_sG=new T(function(){return _2(_sE);}),_sH=function(_sI,_sJ,_){var _sK=A(_sI,[_]);return die(_sJ);},_sL=function(_sM,_sN,_sO,_){return _sH(function(_){var _=putMVar(_sM,_sN);return _A;},_sO,_);},_sP=function(_sQ,_){var _sR=0;if(!E(_sR)){return (function(_){var _sS=E(_sG)[1],_sT=takeMVar(_sS),_sU=jsCatch(function(_){return (function(_){return _sQ;})();},function(_X,_){return _sL(_sS,_sT,_X,_);}),_=putMVar(_sS,_sU);return _A;})();}else{var _sV=E(_sG)[1],_sW=takeMVar(_sV),_sX=jsCatch(function(_){return _sQ;},function(_X,_){return _sL(_sV,_sW,_X,_);}),_=putMVar(_sV,_sX);return _A;}},_sY=unCStr("true"),_sZ=function(_t0,_t1){while(1){var _t2=E(_t0);if(!_t2[0]){return E(_t1)[0]==0?true:false;}else{var _t3=E(_t1);if(!_t3[0]){return false;}else{if(E(_t2[1])[1]!=E(_t3[1])[1]){return false;}else{_t0=_t2[2];_t1=_t3[2];continue;}}}}},_t4=new T(function(){return [0,"keydown"];}),_t5=new T(function(){return [0,"mousemove"];}),_t6=new T(function(){return [0,"blur"];}),_t7=new T(function(){return [0,"focus"];}),_t8=new T(function(){return [0,"change"];}),_t9=new T(function(){return [0,"unload"];}),_ta=new T(function(){return [0,"load"];}),_tb=new T(function(){return [0,"keyup"];}),_tc=new T(function(){return [0,"keypress"];}),_td=new T(function(){return [0,"mouseup"];}),_te=new T(function(){return [0,"mousedown"];}),_tf=new T(function(){return [0,"dblclick"];}),_tg=new T(function(){return [0,"click"];}),_th=new T(function(){return [0,"mouseout"];}),_ti=new T(function(){return [0,"mouseover"];}),_tj=function(_tk){switch(E(_tk)[0]){case 0:return E(_ta);case 1:return E(_t9);case 2:return E(_t8);case 3:return E(_t7);case 4:return E(_t6);case 5:return E(_t5);case 6:return E(_ti);case 7:return E(_th);case 8:return E(_tg);case 9:return E(_tf);case 10:return E(_te);case 11:return E(_td);case 12:return E(_tc);case 13:return E(_tb);default:return E(_t4);}},_tl=function(_tm,_tn,_to){var _tp=new T(function(){return _sz(_tn);}),_tq=new T(function(){return _tj(_tn);});return function(_tr,_){var _ts=A(_tm,[_tr,_]),_tt=E(_ts),_tu=_tt[1],_tv=E(_tp),_tw=jsGetAttr(_tu,toJSStr(_tv));if(!_sZ(fromJSStr(_tw),_sY)){var _tx=E(_to),_ty=jsSetCB(_tu,E(_tq)[1],E([0,_to])[1]),_tz=A(_B,[_H,_tt,_tv,_sY,_]);return _tt;}else{return _tt;}};},_tA=function(_tB,_tC){var _tD=new T(function(){return _sz(_tC);}),_tE=[0,_tD,_sB];return function(_tF,_){var _tG=E(_tF),_tH=E(_tG[4]),_tI=_tH[1],_tJ=_tH[2],_tK=A(_tB,[_tG,_]),_tL=E(_tK),_tM=E(_tL[1]),_tN=_tM[1];return [0,[0,new T(function(){var _tO=E(_tC);switch(_tO[0]){case 0:return _tl(_tN,_tO,function(_){var _tP=_sP(_tE,_),_tQ=A(_tI,[_]),_tR=E(_tQ);if(!_tR[0]){return _A;}else{var _tS=A(_tJ,[_tR[1],_]);return _A;}});case 1:return _tl(_tN,_tO,function(_){var _tT=_sP(_tE,_),_tU=A(_tI,[_]),_tV=E(_tU);if(!_tV[0]){return _A;}else{var _tW=A(_tJ,[_tV[1],_]);return _A;}});case 2:return _tl(_tN,_tO,function(_){var _tX=_sP(_tE,_),_tY=A(_tI,[_]),_tZ=E(_tY);if(!_tZ[0]){return _A;}else{var _u0=A(_tJ,[_tZ[1],_]);return _A;}});case 3:return _tl(_tN,_tO,function(_){var _u1=_sP(_tE,_),_u2=A(_tI,[_]),_u3=E(_u2);if(!_u3[0]){return _A;}else{var _u4=A(_tJ,[_u3[1],_]);return _A;}});case 4:return _tl(_tN,_tO,function(_){var _u5=_sP(_tE,_),_u6=A(_tI,[_]),_u7=E(_u6);if(!_u7[0]){return _A;}else{var _u8=A(_tJ,[_u7[1],_]);return _A;}});case 5:return _tl(_tN,_tO,function(_u9,_){var _ua=_sP([0,_tD,[2,E(_u9)]],_),_ub=A(_tI,[_]),_uc=E(_ub);if(!_uc[0]){return _A;}else{var _ud=A(_tJ,[_uc[1],_]);return _A;}});case 6:return _tl(_tN,_tO,function(_ue,_){var _uf=_sP([0,_tD,[2,E(_ue)]],_),_ug=A(_tI,[_]),_uh=E(_ug);if(!_uh[0]){return _A;}else{var _ui=A(_tJ,[_uh[1],_]);return _A;}});case 7:return _tl(_tN,_tO,function(_){var _uj=A(_tI,[_]),_uk=E(_uj);if(!_uk[0]){return _A;}else{var _ul=A(_tJ,[_uk[1],_]);return _A;}});case 8:return _tl(_tN,_tO,function(_um,_un,_){var _uo=_sP([0,_tD,[1,_um,E(_un)]],_),_up=A(_tI,[_]),_uq=E(_up);if(!_uq[0]){return _A;}else{var _ur=A(_tJ,[_uq[1],_]);return _A;}});case 9:return _tl(_tN,_tO,function(_us,_ut,_){var _uu=_sP([0,_tD,[1,_us,E(_ut)]],_),_uv=A(_tI,[_]),_uw=E(_uv);if(!_uw[0]){return _A;}else{var _ux=A(_tJ,[_uw[1],_]);return _A;}});case 10:return _tl(_tN,_tO,function(_uy,_uz,_){var _uA=_sP([0,_tD,[1,_uy,E(_uz)]],_),_uB=A(_tI,[_]),_uC=E(_uB);if(!_uC[0]){return _A;}else{var _uD=A(_tJ,[_uC[1],_]);return _A;}});case 11:return _tl(_tN,_tO,function(_uE,_uF,_){var _uG=_sP([0,_tD,[1,_uE,E(_uF)]],_),_uH=A(_tI,[_]),_uI=E(_uH);if(!_uI[0]){return _A;}else{var _uJ=A(_tJ,[_uI[1],_]);return _A;}});case 12:return _tl(_tN,_tO,function(_uK,_){var _uL=_sP([0,_tD,[3,_uK]],_),_uM=A(_tI,[_]),_uN=E(_uM);if(!_uN[0]){return _A;}else{var _uO=A(_tJ,[_uN[1],_]);return _A;}});case 13:return _tl(_tN,_tO,function(_uP,_){var _uQ=_sP([0,_tD,[3,_uP]],_),_uR=A(_tI,[_]),_uS=E(_uR);if(!_uS[0]){return _A;}else{var _uT=A(_tJ,[_uS[1],_]);return _A;}});default:return _tl(_tN,_tO,function(_uU,_){var _uV=_sP([0,_tD,[3,_uU]],_),_uW=A(_tI,[_]),_uX=E(_uW);if(!_uX[0]){return _A;}else{var _uY=A(_tJ,[_uX[1],_]);return _A;}});}}),_tM[2]],_tL[2]];};},_uZ=new T(function(){return _tA(_sj,_9T);}),_v0=function(_v1,_){var _v2=A(_uZ,[_v1,_]),_v3=E(_v2),_v4=E(_v3[1]);return [0,[0,function(_v5,_){var _v6=A(_v4[1],[_v5,_]),_v7=_5w(_v5,_);return _v5;},_v4[2]],_v3[2]];},_v8=new T(function(){return [1,_v0,_v8];}),_v9=function(_va,_vb){var _vc=E(_va);if(!_vc){return [0];}else{var _vd=E(_vb);return _vd[0]==0?[0]:[1,_vd[1],new T(function(){return _v9(_vc-1|0,_vd[2]);})];}},_ve=function(_vf,_vg){return _vf<0?[0]:_v9(_vf,_vg);},_vh=function(_vi,_vj){var _vk=E(_vi)[1];return _vk>0?_ve(_vk,_vj):[0];},_vl=function(_vm){return E(_vm);},_vn=function(_vo){var _vp=new T(function(){return _9M(_5O,_vh(_vo,_v8));}),_vq=new T(function(){return _59(_52,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1N(_3M(0,E(_vo)[1],_a),_5K);}));}));});return function(_vr,_){var _vs=_4w(_vp,_5C,_vr,_),_vt=E(_vs),_vu=E(_vt[1]),_vv=new T(function(){return _59(_vl,_vu[1]);});return [0,[0,function(_vw,_){var _vx=A(_vq,[_vw,_]),_vy=A(_vv,[_vw,_]);return _vw;},_vu[2]],_vt[2]];};},_vz=new T(function(){return _vn(_51);}),_vA=unCStr("center"),_vB=function(_vC,_vD){var _vE=new T(function(){return A(_vC,[_vD]);});return function(_vF,_){var _vG=jsCreateElem(toJSStr(E(_vA))),_vH=jsAppendChild(_vG,E(_vF)[1]),_vI=[0,_vG],_vJ=A(_vE,[_vI,_]);return _vI;};},_vK=function(_vL,_){return _vL;},_vM=unCStr("Two counters. One is pure and recursive, the other is stateful"),_vN=new T(function(){return _59(_52,_vM);}),_vO=[8,coercionToken],_vP=function(_vQ){return _aY(_mT(function(_vR){var _vS=E(_vR);return _vS[0]==0?A(_vQ,[_vS[1]]):[2];}),new T(function(){return _mZ(_vT,_vQ);}));},_vT=function(_vU,_vV){return _vP(_vV);},_vW=function(_vX){return _aY(_aY(_mT(function(_vY){var _vZ=E(_vY);return _vZ[0]==1?A(_vX,[_vZ[1]]):[2];}),new T(function(){return _nH(_vT,_vX);})),new T(function(){return _mZ(_w0,_vX);}));},_w0=function(_w1,_w2){return _vW(_w2);},_w3=new T(function(){return _mZ(_w0,_bZ);}),_w4=new T(function(){return _nH(_vT,_bZ);}),_w5=function(_w6){var _w7=E(_w6);return _w7[0]==1?[3,_w7[1],_bY]:[2];},_w8=new T(function(){return _mq(_w5);}),_w9=function(_wa){return E(_w8);},_wb=function(_wc){return A(_kH,[_wc,_w9]);},_wd=[1,_wb],_we=new T(function(){return _aY(_wd,_w4);}),_wf=new T(function(){return _aY(_we,_w3);}),_wg=function(_nv){return _aO(_wf,_nv);},_wh=new T(function(){return _vP(_bZ);}),_wi=function(_nv){return _aO(_wh,_nv);},_wj=function(_wk){return E(_wi);},_wl=[0,_wj,_wg,_vT,_w0],_wm=function(_wn){return E(E(_wn)[4]);},_wo=function(_wp,_wq,_wr){return _nH(new T(function(){return _wm(_wp);}),_wr);},_ws=function(_wt){var _wu=new T(function(){return _nH(new T(function(){return _wm(_wt);}),_bZ);});return function(_cs){return _aO(_wu,_cs);};},_wv=function(_ww,_wx){var _wy=new T(function(){return A(_wm,[_ww,_wx,_bZ]);});return function(_cs){return _aO(_wy,_cs);};},_wz=function(_wA){return [0,function(_nv){return _wv(_wA,_nv);},new T(function(){return _ws(_wA);}),new T(function(){return _wm(_wA);}),function(_nu,_nv){return _wo(_wA,_nu,_nv);}];},_wB=new T(function(){return _wz(_wl);}),_wC=unCStr("Prelude.(!!): negative index\n"),_wD=new T(function(){return err(_wC);}),_wE=unCStr("Prelude.(!!): index too large\n"),_wF=new T(function(){return err(_wE);}),_wG=function(_wH,_wI){while(1){var _wJ=E(_wH);if(!_wJ[0]){return E(_wF);}else{var _wK=E(_wI);if(!_wK){return E(_wJ[1]);}else{_wH=_wJ[2];_wI=_wK-1|0;continue;}}}},_wL=unCStr("ACK"),_wM=unCStr("BEL"),_wN=unCStr("BS"),_wO=unCStr("SP"),_wP=[1,_wO,_a],_wQ=unCStr("US"),_wR=[1,_wQ,_wP],_wS=unCStr("RS"),_wT=[1,_wS,_wR],_wU=unCStr("GS"),_wV=[1,_wU,_wT],_wW=unCStr("FS"),_wX=[1,_wW,_wV],_wY=unCStr("ESC"),_wZ=[1,_wY,_wX],_x0=unCStr("SUB"),_x1=[1,_x0,_wZ],_x2=unCStr("EM"),_x3=[1,_x2,_x1],_x4=unCStr("CAN"),_x5=[1,_x4,_x3],_x6=unCStr("ETB"),_x7=[1,_x6,_x5],_x8=unCStr("SYN"),_x9=[1,_x8,_x7],_xa=unCStr("NAK"),_xb=[1,_xa,_x9],_xc=unCStr("DC4"),_xd=[1,_xc,_xb],_xe=unCStr("DC3"),_xf=[1,_xe,_xd],_xg=unCStr("DC2"),_xh=[1,_xg,_xf],_xi=unCStr("DC1"),_xj=[1,_xi,_xh],_xk=unCStr("DLE"),_xl=[1,_xk,_xj],_xm=unCStr("SI"),_xn=[1,_xm,_xl],_xo=unCStr("SO"),_xp=[1,_xo,_xn],_xq=unCStr("CR"),_xr=[1,_xq,_xp],_xs=unCStr("FF"),_xt=[1,_xs,_xr],_xu=unCStr("VT"),_xv=[1,_xu,_xt],_xw=unCStr("LF"),_xx=[1,_xw,_xv],_xy=unCStr("HT"),_xz=[1,_xy,_xx],_xA=[1,_wN,_xz],_xB=[1,_wM,_xA],_xC=[1,_wL,_xB],_xD=unCStr("ENQ"),_xE=[1,_xD,_xC],_xF=unCStr("EOT"),_xG=[1,_xF,_xE],_xH=unCStr("ETX"),_xI=[1,_xH,_xG],_xJ=unCStr("STX"),_xK=[1,_xJ,_xI],_xL=unCStr("SOH"),_xM=[1,_xL,_xK],_xN=unCStr("NUL"),_xO=[1,_xN,_xM],_xP=[0,92],_xQ=unCStr("\\DEL"),_xR=unCStr("\\a"),_xS=unCStr("\\\\"),_xT=unCStr("\\SO"),_xU=unCStr("\\r"),_xV=unCStr("\\f"),_xW=unCStr("\\v"),_xX=unCStr("\\n"),_xY=unCStr("\\t"),_xZ=unCStr("\\b"),_y0=function(_y1,_y2){if(_y1<=127){var _y3=E(_y1);switch(_y3){case 92:return _1N(_xS,_y2);case 127:return _1N(_xQ,_y2);default:if(_y3<32){var _y4=E(_y3);switch(_y4){case 7:return _1N(_xR,_y2);case 8:return _1N(_xZ,_y2);case 9:return _1N(_xY,_y2);case 10:return _1N(_xX,_y2);case 11:return _1N(_xW,_y2);case 12:return _1N(_xV,_y2);case 13:return _1N(_xU,_y2);case 14:return _1N(_xT,new T(function(){var _y5=E(_y2);return _y5[0]==0?[0]:E(E(_y5[1])[1])==72?unAppCStr("\\&",_y5):E(_y5);}));default:return _1N([1,_xP,new T(function(){var _y6=_y4;return _y6>=0?_wG(_xO,_y6):E(_wD);})],_y2);}}else{return [1,[0,_y3],_y2];}}}else{return [1,_xP,new T(function(){var _y7=jsShowI(_y1);return _1N(fromJSStr(_y7),new T(function(){var _y8=E(_y2);if(!_y8[0]){return [0];}else{var _y9=E(_y8[1])[1];return _y9<48?E(_y8):_y9>57?E(_y8):unAppCStr("\\&",_y8);}}));})];}},_ya=[0,39],_yb=[1,_ya,_a],_yc=unCStr("\'\\\'\'"),_yd=function(_ye){var _yf=E(E(_ye)[1]);return _yf==39?E(_yc):[1,_ya,new T(function(){return _y0(_yf,_yb);})];},_yg=[0,34],_yh=unCStr("\\\""),_yi=function(_yj,_yk){var _yl=E(_yj);if(!_yl[0]){return E(_yk);}else{var _ym=_yl[2],_yn=E(E(_yl[1])[1]);return _yn==34?_1N(_yh,new T(function(){return _yi(_ym,_yk);})):_y0(_yn,new T(function(){return _yi(_ym,_yk);}));}},_yo=function(_yp,_yq){return [1,_yg,new T(function(){return _yi(_yp,[1,_yg,_yq]);})];},_yr=function(_ys){return _1N(_yc,_ys);},_yt=function(_yu,_yv){var _yw=E(E(_yv)[1]);return _yw==39?E(_yr):function(_yx){return [1,_ya,new T(function(){return _y0(_yw,[1,_ya,_yx]);})];};},_yy=[0,_yt,_yd,_yo],_yz=function(_yA){return E(E(_yA)[3]);},_yB=function(_yC,_yD){return A(_yz,[_yC,_yD,_a]);},_yE=function(_yF,_yG,_yH){return _2D(new T(function(){return _yz(_yF);}),_yG,_yH);},_yI=function(_yJ){var _yK=new T(function(){return _yz(_yJ);});return [0,function(_yL){return E(_yK);},function(_ys){return _yB(_yJ,_ys);},function(_yM,_ys){return _yE(_yJ,_yM,_ys);}];},_yN=new T(function(){return _yI(_yy);}),_yO=unCStr("submit"),_yP=new T(function(){return A(_s6,[_8P,_9V,_pg,_yN,_wB,_b,_yO]);}),_yQ=[0,43],_yR=[1,_yQ,_a],_yS=[1,_yR],_yT=new T(function(){return A(_yP,[_yS]);}),_yU=new T(function(){return _tA(_yT,_vO);}),_yV=function(_X,_){return _Z(_Y,_X,_);},_yW=function(_yX,_yY,_yZ,_){var _z0=A(_yY,[_yZ,_]),_z1=E(_z0),_z2=E(_z1[1]);return [0,[0,function(_z3,_){var _z4=_Z(_Y,_z3,_),_z5=A(_B,[_H,_z4,_z,_yX,_]),_z6=A(_z2[1],[_z4,_]);return _z4;},_z2[2]],_z1[2]];},_z7=new T(function(){return _3R(_15,_3E,_19,_16);}),_z8=new T(function(){return _3R(_15,_3E,_19,_16);}),_z9=function(_za,_zb,_zc,_){var _zd=A(_z7,[_zc,_]),_ze=A(_z8,[new T(function(){return E(E(_zd)[2]);}),_]),_zf=E(_ze),_zg=_zf[1],_zh=E(_zf[2]),_zi=_zh[2],_zj=E(_zh[4]),_zk=new T(function(){return E(E(_zd)[1]);}),_zl=function(_zm){var _zn=new T(function(){return A(_zb,[_zm]);});return function(_zo,_){var _zp=A(_zn,[_zo,_]),_zq=E(_zp),_zr=E(_zq[1]);return [0,[0,function(_zs,_){var _zt=E(_zk),_zu=jsFind(toJSStr(_zt)),_zv=E(_zu);if(!_zv[0]){return _45(_zt);}else{var _zw=E(_zv[1]),_zx=A(_7,[E(_zw[1]),_]),_zy=jsKillChild(E(_zw)[1],_zx),_zz=A(_zr[1],[_zs,_]);return _zs;}},_zr[2]],_zq[2]];};},_zA=_yW(_zk,_za,[0,_zh[1],_zi,_zh[3],[0,function(_){return _47(function(_zB,_){var _zC=_yW(_zk,_za,new T(function(){var _zD=E(_zB);return [0,_zD[1],_zi,_zD[3],_zD[4],_zD[5],_zD[6]];}),_);return [0,[0,_34,E(E(_zC)[1])[2]],_zB];},_zg,_);},function(_zE,_){var _zF=_47(new T(function(){return _zl(_zE);}),_zg,_),_zG=E(_zF);return _zG[0]==0?_b:A(_zj[2],[_zG[1],_]);}],_zh[5],_zh[6]],_),_zH=E(_zA),_zI=_zH[2],_zJ=E(_zH[1]),_zK=_zJ[1],_zL=new T(function(){return _Q(_yV,[1,[0,_z,_zg],_a]);}),_zM=E(_zJ[2]);if(!_zM[0]){return [0,[0,function(_zN,_){var _zO=A(_zK,[_zN,_]),_zP=A(_zL,[_zN,_]);return _zN;},_b],new T(function(){var _zQ=E(_zI);return [0,_zQ[1],_zQ[2],_zQ[3],_zj,_zQ[5],_zQ[6]];})];}else{var _zR=A(_zl,[_zM[1],new T(function(){var _zS=E(_zI);return [0,_zS[1],_zS[2],_zS[3],_zj,_zS[5],_zS[6]];}),_]),_zT=E(_zR),_zU=E(_zT[1]);return [0,[0,function(_zV,_){var _zW=A(_zK,[_zV,_]),_zX=A(_zL,[_zV,_]),_zY=A(_zU[1],[_zX,_]);return _zV;},_zU[2]],_zT[2]];}},_zZ=function(_A0){var _A1=new T(function(){return _zZ(new T(function(){return [0,E(_A0)[1]+1|0];}));}),_A2=new T(function(){return _5m(_52,new T(function(){return _5i(_A0);}));});return function(_cs,_sg){return _z9(function(_A3,_){var _A4=A(_yU,[_A3,_]),_A5=E(_A4),_A6=E(_A5[1]);return [0,[0,function(_A7,_){var _A8=A(_A2,[_A7,_]),_A9=A(_A6[1],[_A7,_]);return _A7;},_A6[2]],_A5[2]];},function(_Aa){return E(_A1);},_cs,_sg);};},_Ab=unCStr("main"),_Ac=unCStr("Main"),_Ad=unCStr("Counter"),_Ae=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_Ab,_Ac,_Ad],_Af=function(_Ag,_Ah){var _Ai=hs_leWord64(_Ag,_Ah);return E(_Ai)==0?false:true;},_Aj=function(_Ak,_Al,_Am,_An){var _Ao=hs_eqWord64(_Ak,_Am);if(!E(_Ao)){var _Ap=hs_leWord64(_Ak,_Am);return E(_Ap)==0?false:true;}else{return _Af(_Al,_An);}},_Aq=function(_Ar,_As){var _At=E(_Ar),_Au=_At[1],_Av=_At[2],_Aw=E(_As),_Ax=_Aw[1],_Ay=_Aw[2],_Az=hs_eqWord64(_Au,_Ax);if(!E(_Az)){return !_Aj(_Au,_Av,_Ax,_Ay)?2:0;}else{var _AA=hs_eqWord64(_Av,_Ay);return E(_AA)==0?!_Aj(_Au,_Av,_Ax,_Ay)?2:0:1;}},_AB=unCStr("Failure in Data.Map.balanceL"),_AC=new T(function(){return err(_AB);}),_AD=function(_AE,_AF,_AG,_AH){var _AI=E(_AH);if(!_AI[0]){var _AJ=_AI[1],_AK=E(_AG);if(!_AK[0]){var _AL=_AK[1],_AM=_AK[2],_AN=_AK[3];if(_AL<=(imul(3,_AJ)|0)){return [0,(1+_AL|0)+_AJ|0,E(E(_AE)),_AF,E(_AK),E(_AI)];}else{var _AO=E(_AK[4]);if(!_AO[0]){var _AP=_AO[1],_AQ=E(_AK[5]);if(!_AQ[0]){var _AR=_AQ[1],_AS=_AQ[2],_AT=_AQ[3],_AU=_AQ[4];if(_AR>=(imul(2,_AP)|0)){var _AV=function(_AW){var _AX=E(_AQ[5]);return _AX[0]==0?[0,(1+_AL|0)+_AJ|0,E(_AS),_AT,E([0,(1+_AP|0)+_AW|0,E(_AM),_AN,E(_AO),E(_AU)]),E([0,(1+_AJ|0)+_AX[1]|0,E(E(_AE)),_AF,E(_AX),E(_AI)])]:[0,(1+_AL|0)+_AJ|0,E(_AS),_AT,E([0,(1+_AP|0)+_AW|0,E(_AM),_AN,E(_AO),E(_AU)]),E([0,1+_AJ|0,E(E(_AE)),_AF,E(_9),E(_AI)])];},_AY=E(_AU);return _AY[0]==0?_AV(_AY[1]):_AV(0);}else{return [0,(1+_AL|0)+_AJ|0,E(_AM),_AN,E(_AO),E([0,(1+_AJ|0)+_AR|0,E(E(_AE)),_AF,E(_AQ),E(_AI)])];}}else{return E(_AC);}}else{return E(_AC);}}}else{return [0,1+_AJ|0,E(E(_AE)),_AF,E(_9),E(_AI)];}}else{var _AZ=E(_AG);if(!_AZ[0]){var _B0=_AZ[1],_B1=_AZ[2],_B2=_AZ[3],_B3=_AZ[5],_B4=E(_AZ[4]);if(!_B4[0]){var _B5=_B4[1],_B6=E(_B3);if(!_B6[0]){var _B7=_B6[1],_B8=_B6[2],_B9=_B6[3],_Ba=_B6[4];if(_B7>=(imul(2,_B5)|0)){var _Bb=function(_Bc){var _Bd=E(_B6[5]);return _Bd[0]==0?[0,1+_B0|0,E(_B8),_B9,E([0,(1+_B5|0)+_Bc|0,E(_B1),_B2,E(_B4),E(_Ba)]),E([0,1+_Bd[1]|0,E(E(_AE)),_AF,E(_Bd),E(_9)])]:[0,1+_B0|0,E(_B8),_B9,E([0,(1+_B5|0)+_Bc|0,E(_B1),_B2,E(_B4),E(_Ba)]),E([0,1,E(E(_AE)),_AF,E(_9),E(_9)])];},_Be=E(_Ba);return _Be[0]==0?_Bb(_Be[1]):_Bb(0);}else{return [0,1+_B0|0,E(_B1),_B2,E(_B4),E([0,1+_B7|0,E(E(_AE)),_AF,E(_B6),E(_9)])];}}else{return [0,3,E(_B1),_B2,E(_B4),E([0,1,E(E(_AE)),_AF,E(_9),E(_9)])];}}else{var _Bf=E(_B3);return _Bf[0]==0?[0,3,E(_Bf[2]),_Bf[3],E([0,1,E(_B1),_B2,E(_9),E(_9)]),E([0,1,E(E(_AE)),_AF,E(_9),E(_9)])]:[0,2,E(E(_AE)),_AF,E(_AZ),E(_9)];}}else{return [0,1,E(E(_AE)),_AF,E(_9),E(_9)];}}},_Bg=unCStr("Failure in Data.Map.balanceR"),_Bh=new T(function(){return err(_Bg);}),_Bi=function(_Bj,_Bk,_Bl,_Bm){var _Bn=E(_Bl);if(!_Bn[0]){var _Bo=_Bn[1],_Bp=E(_Bm);if(!_Bp[0]){var _Bq=_Bp[1],_Br=_Bp[2],_Bs=_Bp[3];if(_Bq<=(imul(3,_Bo)|0)){return [0,(1+_Bo|0)+_Bq|0,E(E(_Bj)),_Bk,E(_Bn),E(_Bp)];}else{var _Bt=E(_Bp[4]);if(!_Bt[0]){var _Bu=_Bt[1],_Bv=_Bt[2],_Bw=_Bt[3],_Bx=_Bt[4],_By=E(_Bp[5]);if(!_By[0]){var _Bz=_By[1];if(_Bu>=(imul(2,_Bz)|0)){var _BA=function(_BB){var _BC=E(_Bj),_BD=E(_Bt[5]);return _BD[0]==0?[0,(1+_Bo|0)+_Bq|0,E(_Bv),_Bw,E([0,(1+_Bo|0)+_BB|0,E(_BC),_Bk,E(_Bn),E(_Bx)]),E([0,(1+_Bz|0)+_BD[1]|0,E(_Br),_Bs,E(_BD),E(_By)])]:[0,(1+_Bo|0)+_Bq|0,E(_Bv),_Bw,E([0,(1+_Bo|0)+_BB|0,E(_BC),_Bk,E(_Bn),E(_Bx)]),E([0,1+_Bz|0,E(_Br),_Bs,E(_9),E(_By)])];},_BE=E(_Bx);return _BE[0]==0?_BA(_BE[1]):_BA(0);}else{return [0,(1+_Bo|0)+_Bq|0,E(_Br),_Bs,E([0,(1+_Bo|0)+_Bu|0,E(E(_Bj)),_Bk,E(_Bn),E(_Bt)]),E(_By)];}}else{return E(_Bh);}}else{return E(_Bh);}}}else{return [0,1+_Bo|0,E(E(_Bj)),_Bk,E(_Bn),E(_9)];}}else{var _BF=E(_Bm);if(!_BF[0]){var _BG=_BF[1],_BH=_BF[2],_BI=_BF[3],_BJ=_BF[5],_BK=E(_BF[4]);if(!_BK[0]){var _BL=_BK[1],_BM=_BK[2],_BN=_BK[3],_BO=_BK[4],_BP=E(_BJ);if(!_BP[0]){var _BQ=_BP[1];if(_BL>=(imul(2,_BQ)|0)){var _BR=function(_BS){var _BT=E(_Bj),_BU=E(_BK[5]);return _BU[0]==0?[0,1+_BG|0,E(_BM),_BN,E([0,1+_BS|0,E(_BT),_Bk,E(_9),E(_BO)]),E([0,(1+_BQ|0)+_BU[1]|0,E(_BH),_BI,E(_BU),E(_BP)])]:[0,1+_BG|0,E(_BM),_BN,E([0,1+_BS|0,E(_BT),_Bk,E(_9),E(_BO)]),E([0,1+_BQ|0,E(_BH),_BI,E(_9),E(_BP)])];},_BV=E(_BO);return _BV[0]==0?_BR(_BV[1]):_BR(0);}else{return [0,1+_BG|0,E(_BH),_BI,E([0,1+_BL|0,E(E(_Bj)),_Bk,E(_9),E(_BK)]),E(_BP)];}}else{return [0,3,E(_BM),_BN,E([0,1,E(E(_Bj)),_Bk,E(_9),E(_9)]),E([0,1,E(_BH),_BI,E(_9),E(_9)])];}}else{var _BW=E(_BJ);return _BW[0]==0?[0,3,E(_BH),_BI,E([0,1,E(E(_Bj)),_Bk,E(_9),E(_9)]),E(_BW)]:[0,2,E(E(_Bj)),_Bk,E(_9),E(_BF)];}}else{return [0,1,E(E(_Bj)),_Bk,E(_9),E(_9)];}}},_BX=function(_BY,_BZ,_C0,_C1,_C2,_C3){var _C4=E(_C3);if(!_C4[0]){var _C5=_C4[2],_C6=_C4[3],_C7=_C4[4],_C8=_C4[5];switch(_Aq([0,_BY,_BZ,_C0,_C1],_C5)){case 0:return _AD(_C5,_C6,_BX(_BY,_BZ,_C0,_C1,_C2,_C7),_C8);case 1:return [0,_C4[1],E([0,_BY,_BZ,_C0,_C1]),_C2,E(_C7),E(_C8)];default:return _Bi(_C5,_C6,_C7,_BX(_BY,_BZ,_C0,_C1,_C2,_C8));}}else{return [0,1,E([0,_BY,_BZ,_C0,_C1]),_C2,E(_9),E(_9)];}},_C9=[0,_34,_5A],_Ca=function(_Cb,_){return [0,[0,_34,[1,_Cb]],_Cb];},_Cc=[1,_A],_Cd=function(_Ce){var _Cf=new T(function(){return [0,E(_Ce)[1]+1|0];}),_Cg=new T(function(){return _5m(_52,new T(function(){return _5i(_Ce);}));});return function(_cs,_sg){return _4w(function(_Ch,_){return [0,[0,_Cg,_Cc],_Ch];},function(_Ci,_Cj,_){return (function(_Cj,_){return _4w(_Ca,function(_Ck){return function(_Cl,_){return [0,_C9,new T(function(){var _Cm=E(_Ck);return [0,_Cm[1],_Cm[2],_Cm[3],_Cm[4],_Cm[5],new T(function(){return _BX(I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_Ae,_a,_Cf,_Cm[6]);})];})];};},_Cj,_);})(_Cj,_);},_cs,_sg);};},_Cn=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_Ae,_a],_Co=function(_Cp){return E(_Cn);},_Cq=function(_Cr,_Cs,_Ct,_Cu,_Cv){while(1){var _Cw=E(_Cv);if(!_Cw[0]){switch(_Aq([0,_Cr,_Cs,_Ct,_Cu],_Cw[2])){case 0:_Cv=_Cw[4];continue;case 1:return [1,_Cw[3]];default:_Cv=_Cw[5];continue;}}else{return [0];}}},_Cx=function(_Cy,_Cz){var _CA=E(_Cy),_CB=_CA[1],_CC=_CA[2],_CD=_CA[3],_CE=_CA[4],_CF=E(_Cz);if(!_CF[0]){switch(_Aq(_CA,_CF[2])){case 0:return _Cq(_CB,_CC,_CD,_CE,_CF[4]);case 1:return [1,_CF[3]];default:return _Cq(_CB,_CC,_CD,_CE,_CF[5]);}}else{return [0];}},_CG=function(_CH,_CI,_CJ,_CK){var _CL=E(_CI),_CM=_CL[1],_CN=_CL[3],_CO=new T(function(){return A(_CK,[_pi]);}),_CP=new T(function(){return A(_CN,[_b]);});return A(_CM,[new T(function(){return A(_CM,[_CJ,function(_CQ){return A(_CN,[new T(function(){var _CR=E(_CH);return E(E(_CQ)[6]);})]);}]);}),function(_CS){var _CT=_Cx(_CO,_CS);return _CT[0]==0?E(_CP):A(_CN,[[1,_CT[1]]]);}]);},_CU=new T(function(){return _CG(_15,_3E,_19,_Co);}),_CV=function(_CW){var _CX=new T(function(){return _zZ(_CW);});return function(_CY,_){var _CZ=A(_CX,[_CY,_]),_D0=E(_CZ),_D1=E(_D0[1]),_D2=_4w(_yU,function(_D3){return function(_Cj,_){return _4w(function(_D4,_){var _D5=A(_CU,[_D4,_]);return [0,[0,_vK,new T(function(){var _D6=E(E(_D5)[1]);return _D6[0]==0?E([1,_CW]):E(_D6);})],new T(function(){return E(E(_D5)[2]);})];},_Cd,_Cj,_);};},_D0[2],_),_D7=E(_D2),_D8=E(_D7[1]),_D9=new T(function(){return _vB(_vl,function(_Da,_){var _Db=A(_D1[1],[_Da,_]),_Dc=A(_D8[1],[_Da,_]);return _Da;});});return [0,[0,function(_Dd,_){var _De=A(_vN,[_Dd,_]),_Df=_5w(_Dd,_),_Dg=A(_D9,[_Dd,_]);return _Dd;},new T(function(){var _Dh=E(_D1[2]);return _Dh[0]==0?E(_D8[2]):E(_Dh);})],_D7[2]];};},_Di=new T(function(){return _CV(_51);}),_Dj=[0,4],_Dk=function(_Dl,_Dm){return [1,_Dm,new T(function(){return _Dk(_Dl,new T(function(){return A(_Dl,[_Dm]);}));})];},_Dn=[0,1],_Do=[1,_Dn,_a],_Dp=[1,_5L,_a],_Dq=function(_Dr,_Ds,_Dt){var _Du=E(_Ds);if(!_Du[0]){return [0];}else{var _Dv=E(_Dt);return _Dv[0]==0?[0]:[1,new T(function(){return A(_Dr,[_Du[1],_Dv[1]]);}),new T(function(){return _Dq(_Dr,_Du[2],_Dv[2]);})];}},_Dw=function(_Dx){return _Dq(_94,[1,_5L,_Dx],new T(function(){return _1N(_Dx,_Dp);}));},_Dy=new T(function(){return _Dk(_Dw,_Do);}),_Dz=unCStr(" rows of the Pascal triangle "),_DA=function(_DB){var _DC=new T(function(){return _2D(_oe,_DB,_a);});return function(_cs,_sg){return _52(_DC,_cs,_sg);};},_DD=unCStr("text-align:center"),_DE=unCStr("style"),_DF=function(_DG,_DH){var _DI=new T(function(){return _59(_DA,_DG);});return [1,function(_DJ,_){var _DK=A(_DI,[_DJ,_]),_DL=A(_B,[_H,_DK,_DE,_DD,_]);return _DK;},_DH];},_DM=function(_DN,_DO){var _DP=E(_DN);if(!_DP[0]){return [0];}else{var _DQ=_DP[1];return _DO>1?_DF(_DQ,new T(function(){return _DM(_DP[2],_DO-1|0);})):_DF(_DQ,_a);}},_DR=function(_DS){var _DT=new T(function(){return _59(_52,new T(function(){return unAppCStr("Show ",new T(function(){return _1N(_3M(0,E(_DS)[1],_a),_Dz);}));}));});return function(_DU,_){return [0,[0,function(_DV,_){var _DW=A(_DT,[_DV,_]),_DX=_8C(new T(function(){var _DY=E(_DS)[1];return _DY>0?_DM(_Dy,_DY):[0];}),_DV,_);return _DV;},_b],_DU];};},_DZ=new T(function(){return _DR(_Dj);}),_E0=unCStr("Different input elements:"),_E1=new T(function(){return _59(_52,_E0);}),_E2=unCStr(" returns: "),_E3=[1,_yg,_a],_E4=function(_E5){var _E6=new T(function(){return _5m(_52,[1,_yg,new T(function(){return _yi(_E5,_E3);})]);});return function(_E7,_){return [0,[0,function(_E8,_){var _E9=_52(_E2,_E8,_),_Ea=A(_E6,[_E8,_]);return _E8;},_Cc],_E7];};},_Eb=unCStr("blue"),_Ec=[1,_Eb,_a],_Ed=unCStr("green"),_Ee=[1,_Ed,_Ec],_Ef=unCStr("red"),_Eg=[1,_Ef,_Ee],_Eh=function(_Ei){return E(E(_Ei)[15]);},_Ej=function(_Ek,_El,_){var _Em=jsGet(_Ek,toJSStr(E(_El)));return new T(function(){return fromJSStr(_Em);});},_En=function(_Eo,_Ep,_){return _Ej(E(_Eo)[1],_Ep,_);},_Eq=new T(function(){return A(_pg,[_6R]);}),_Er=unCStr("name"),_Es=unCStr("true"),_Et=unCStr("radio"),_Eu=function(_Ev,_Ew,_Ex,_Ey){var _Ez=new T(function(){return _ov(_Ew);}),_EA=new T(function(){return _3R([0,coercionToken],_3z(_Ez),function(_EB){return _oM(_Ez,_EB);},function(_EC,_ED){return _oP(_Ez,_EC,_ED);});}),_EE=new T(function(){return _3x(_Ez);}),_EF=new T(function(){return _3x(_Ez);}),_EG=new T(function(){return _37(_Ez);}),_EH=new T(function(){return _37(_Ez);}),_EI=new T(function(){return _3x(_Ez);}),_EJ=new T(function(){return _37(_Ez);}),_EK=new T(function(){return _3x(_Ez);}),_EL=new T(function(){return _37(_Ez);}),_EM=new T(function(){return _rm(_Ev);}),_EN=new T(function(){return _Eh(_Ev);}),_EO=new T(function(){return _rs(_Ey);});return function(_EP,_EQ){return function(_ER){return A(_EG,[new T(function(){return A(_EA,[_ER]);}),function(_ES){var _ET=new T(function(){return E(E(_ES)[1]);}),_EU=new T(function(){return _oz(_Ew,function(_){return jsFind(toJSStr(E(_ET)));});});return A(_EL,[new T(function(){var _EV=new T(function(){return E(E(_ES)[2]);});return A(_EK,[[0,_EV,_EV]]);}),function(_EW){return A(_EJ,[new T(function(){return A(_EI,[[0,_A,new T(function(){var _EX=E(E(_EW)[1]);return [0,_EX[1],_EX[2],_rl,_EX[4],_EX[5],_EX[6]];})]]);}),function(_EY){return A(_EH,[new T(function(){return A(_EU,[new T(function(){return E(E(_EY)[2]);})]);}),function(_EZ){return A(_EG,[new T(function(){var _F0=E(_EZ),_F1=_F0[2],_F2=E(_F0[1]);return _F2[0]==0?A(_EF,[[0,_a,_F1]]):A(_oz,[_Ew,function(_){return _En(_F2[1],_75,_);},_F1]);}),function(_F3){var _F4=new T(function(){return !_sZ(E(_F3)[1],_Es)?[0]:E([1,_EP]);});return A(_EE,[[0,[0,new T(function(){return A(_EN,[new T(function(){return A(_EM,[_ET,_Et,new T(function(){var _F5=A(_Ex,[_EP]),_F6=E(_Eq),_F7=hs_eqWord64(_F5[1],_F6[1]);if(!E(_F7)){return A(_EO,[_EP]);}else{var _F8=hs_eqWord64(_F5[2],_F6[2]);return E(_F8)==0?A(_EO,[_EP]):E(_EP);}}),new T(function(){return E(_F4)[0]==0?false:true;}),_b]);}),[1,[0,_Er,_EQ],_a]]);}),new T(function(){var _F9=E(_F4);return _F9[0]==0?[0]:[1,_F9[1]];})],new T(function(){return E(E(_F3)[2]);})]]);}]);}]);}]);}]);}]);};};},_Fa=new T(function(){return _6S(_p9,_pe);}),_Fb=new T(function(){return _yI(_yy);}),_Fc=new T(function(){return _Eu(_8P,_9V,_Fa,_Fb);}),_Fd=function(_Fe){var _Ff=E(_Fe);if(!_Ff[0]){return [0];}else{var _Fg=_Ff[1];return [1,function(_Fh){var _Fi=new T(function(){return _tA(new T(function(){return A(_Fc,[_Fg,_Fh]);}),_vO);});return function(_Fj,_){var _Fk=A(_Fi,[_Fj,_]),_Fl=E(_Fk),_Fm=E(_Fl[1]);return [0,[0,function(_Fn,_){var _Fo=_52(_Fg,_Fn,_),_Fp=A(_Fm[1],[_Fn,_]);return _Fn;},_Fm[2]],_Fl[2]];};},new T(function(){return _Fd(_Ff[2]);})];}},_Fq=new T(function(){return _Fd(_Eg);}),_Fr=function(_Fs){return E(E(_Fs)[1]);},_Ft=function(_Fu,_Fv){var _Fw=new T(function(){return _9c(_Fv);}),_Fx=new T(function(){return _Fr(_Fw);}),_Fy=new T(function(){return _9e(_Fw);}),_Fz=function(_FA){var _FB=E(_FA);if(!_FB[0]){return [0,_Fx,_b];}else{var _FC=E(_FB[1]),_FD=_Fz(_FB[2]);return [0,new T(function(){return A(_Fy,[_FC[1],_FD[1]]);}),new T(function(){var _FE=E(_FC[2]);return _FE[0]==0?E(_FD[2]):E(_FE);})];}},_FF=new T(function(){return _3x(_Fu);}),_FG=new T(function(){return _3R([0,coercionToken],_3z(_Fu),function(_FH){return _oM(_Fu,_FH);},function(_FI,_FJ){return _oP(_Fu,_FI,_FJ);});}),_FK=new T(function(){return _3x(_Fu);}),_FL=new T(function(){return _37(_Fu);}),_FM=new T(function(){return _37(_Fu);}),_FN=new T(function(){return _37(_Fu);}),_FO=new T(function(){return _37(_Fu);});return function(_FP,_FQ){return A(_FO,[new T(function(){return A(_FG,[_FQ]);}),function(_FR){return A(_FN,[new T(function(){var _FS=new T(function(){return E(E(_FR)[1]);}),_FT=function(_FU){var _FV=E(_FU);if(!_FV[0]){return function(_FW){return A(_FK,[[0,_a,_FW]]);};}else{var _FX=new T(function(){return _FT(_FV[2]);}),_FY=new T(function(){return A(_FV[1],[_FS]);});return function(_FZ){return A(_FM,[new T(function(){return A(_FY,[_FZ]);}),function(_G0){var _G1=new T(function(){return E(E(_G0)[1]);});return A(_FL,[new T(function(){return A(_FX,[new T(function(){return E(E(_G0)[2]);})]);}),function(_G2){return A(_FK,[[0,[1,_G1,new T(function(){return E(E(_G2)[1]);})],new T(function(){return E(E(_G2)[2]);})]]);}]);}]);};}};return A(_FT,[_FP,new T(function(){return E(E(_FR)[2]);})]);}),function(_G3){var _G4=new T(function(){var _G5=_Fz(E(_G3)[1]);return [0,_G5[1],_G5[2]];});return A(_FF,[[0,[0,new T(function(){return E(E(_G4)[1]);}),new T(function(){var _G6=E(E(_G4)[2]);return _G6[0]==0?[0]:[1,_G6[1]];})],new T(function(){return E(E(_G3)[2]);})]]);}]);}]);};},_G7=new T(function(){return _Ft(_36,_8P);}),_G8=new T(function(){return A(_G7,[_Fq]);}),_G9=function(_Ga){var _Gb=new T(function(){return _5m(_52,new T(function(){return _2D(_yo,_Ga,_a);}));});return function(_Gc,_){return [0,[0,function(_Gd,_){var _Ge=_52(_E2,_Gd,_),_Gf=A(_Gb,[_Gd,_]);return _Gd;},_Cc],_Gc];};},_Gg=new T(function(){return _5m(_52,_Ed);}),_Gh=unCStr("checkbox"),_Gi=function(_Gj,_Gk){var _Gl=new T(function(){return _ov(_Gk);}),_Gm=new T(function(){return _3R([0,coercionToken],_3z(_Gl),function(_Gn){return _oM(_Gl,_Gn);},function(_Go,_Gp){return _oP(_Gl,_Go,_Gp);});}),_Gq=new T(function(){return _3x(_Gl);}),_Gr=new T(function(){return _3x(_Gl);}),_Gs=new T(function(){return _37(_Gl);}),_Gt=new T(function(){return _37(_Gl);}),_Gu=new T(function(){return _3x(_Gl);}),_Gv=new T(function(){return _37(_Gl);}),_Gw=new T(function(){return _3x(_Gl);}),_Gx=new T(function(){return _37(_Gl);}),_Gy=new T(function(){return _rm(_Gj);});return function(_Gz,_GA){var _GB=new T(function(){return !E(_Gz)?[0]:E(_Es);});return function(_GC){return A(_Gs,[new T(function(){return A(_Gm,[_GC]);}),function(_GD){var _GE=new T(function(){return E(E(_GD)[1]);}),_GF=new T(function(){return _oz(_Gk,function(_){return jsFind(toJSStr(E(_GE)));});}),_GG=new T(function(){return A(_Gy,[_GE,_Gh,_GA,_Gz,_b]);});return A(_Gx,[new T(function(){var _GH=new T(function(){return E(E(_GD)[2]);});return A(_Gw,[[0,_GH,_GH]]);}),function(_GI){return A(_Gv,[new T(function(){return A(_Gu,[[0,_A,new T(function(){var _GJ=E(E(_GI)[1]);return [0,_GJ[1],_GJ[2],_rl,_GJ[4],_GJ[5],_GJ[6]];})]]);}),function(_GK){return A(_Gt,[new T(function(){return A(_GF,[new T(function(){return E(E(_GK)[2]);})]);}),function(_GL){return A(_Gs,[new T(function(){var _GM=E(_GL),_GN=_GM[2],_GO=E(_GM[1]);return _GO[0]==0?A(_Gr,[[0,_GB,_GN]]):A(_oz,[_Gk,function(_){return _En(_GO[1],_75,_);},_GN]);}),function(_GP){return A(_Gq,[[0,[0,_GG,[1,[0,new T(function(){return !_sZ(E(_GP)[1],_Es)?[0]:E([1,_GA,_a]);})]]],new T(function(){return E(E(_GP)[2]);})]]);}]);}]);}]);}]);}]);};};},_GQ=new T(function(){return _Gi(_8P,_9V);}),_GR=unCStr("Green"),_GS=new T(function(){return A(_GQ,[_0,_GR]);}),_GT=function(_GU,_){var _GV=A(_GS,[_GU,_]),_GW=E(_GV),_GX=E(_GW[1]);return [0,[0,function(_GY,_){var _GZ=A(_GX[1],[_GY,_]),_H0=A(_Gg,[_GY,_]);return _GY;},_GX[2]],_GW[2]];},_H1=new T(function(){return _tA(_GT,_vO);}),_H2=new T(function(){return _5m(_52,_Eb);}),_H3=new T(function(){return A(_GQ,[_0,_Eb]);}),_H4=function(_H5,_){var _H6=A(_H3,[_H5,_]),_H7=E(_H6),_H8=E(_H7[1]);return [0,[0,function(_H9,_){var _Ha=A(_H8[1],[_H9,_]),_Hb=A(_H2,[_H9,_]);return _H9;},_H8[2]],_H7[2]];},_Hc=new T(function(){return _tA(_H4,_vO);}),_Hd=new T(function(){return _5m(_52,_Ef);}),_He=unCStr("Red"),_Hf=new T(function(){return A(_GQ,[_0,_He]);}),_Hg=function(_Hh,_){var _Hi=A(_Hf,[_Hh,_]),_Hj=E(_Hi),_Hk=E(_Hj[1]);return [0,[0,function(_Hl,_){var _Hm=A(_Hk[1],[_Hl,_]),_Hn=A(_Hd,[_Hl,_]);return _Hl;},_Hk[2]],_Hj[2]];},_Ho=new T(function(){return _tA(_Hg,_vO);}),_Hp=function(_Hq,_){var _Hr=A(_Ho,[_Hq,_]),_Hs=E(_Hr),_Ht=E(_Hs[1]),_Hu=A(_H1,[_Hs[2],_]),_Hv=E(_Hu),_Hw=E(_Hv[1]),_Hx=A(_Hc,[_Hv[2],_]),_Hy=E(_Hx),_Hz=E(_Hy[1]);return [0,[0,function(_HA,_){var _HB=A(_Ht[1],[_HA,_]),_HC=A(_Hw[1],[_HA,_]),_HD=A(_Hz[1],[_HA,_]);return _HA;},new T(function(){var _HE=E(_Ht[2]);if(!_HE[0]){return [0];}else{var _HF=E(_Hw[2]);if(!_HF[0]){return [0];}else{var _HG=E(_Hz[2]);return _HG[0]==0?[0]:[1,new T(function(){var _HH=function(_HI){var _HJ=E(_HI);return _HJ[0]==0?E(new T(function(){var _HK=function(_HL){var _HM=E(_HL);return _HM[0]==0?E(E(_HG[1])[1]):[1,_HM[1],new T(function(){return _HK(_HM[2]);})];};return _HK(E(_HF[1])[1]);})):[1,_HJ[1],new T(function(){return _HH(_HJ[2]);})];};return _HH(E(_HE[1])[1]);})];}}})],_Hy[2]];},_HN=function(_HO){var _HP=new T(function(){return _5m(_52,[1,_yg,new T(function(){return _yi(_HO,_E3);})]);});return function(_HQ,_){return [0,[0,function(_HR,_){var _HS=_52(_E2,_HR,_),_HT=A(_HP,[_HR,_]);return _HR;},_Cc],_HQ];};},_HU=new T(function(){return _wz(_wl);}),_HV=function(_HW){return E(E(_HW)[11]);},_HX=function(_HY,_HZ,_I0,_I1){var _I2=new T(function(){return _ov(_HZ);}),_I3=new T(function(){return _3z(_I2);}),_I4=new T(function(){return _3R([0,coercionToken],_I3,function(_I5){return _oM(_I2,_I5);},function(_I6,_I7){return _oP(_I2,_I6,_I7);});}),_I8=new T(function(){return _3x(_I2);}),_I9=new T(function(){return _37(_I2);}),_Ia=new T(function(){return _37(_I2);}),_Ib=new T(function(){return _3x(_I2);}),_Ic=new T(function(){return _37(_I2);}),_Id=new T(function(){return _3x(_I2);}),_Ie=new T(function(){return _37(_I2);}),_If=new T(function(){return _37(_I2);}),_Ig=new T(function(){return _HV(_HY);});return function(_Ih,_Ii){return A(_If,[new T(function(){return A(_I4,[_Ii]);}),function(_Ij){var _Ik=new T(function(){return E(E(_Ij)[1]);}),_Il=new T(function(){return _r5(_I3,function(_Im){return _oz(_HZ,_Im);},_I0,_I1,_HY,_Ik);});return A(_Ie,[new T(function(){var _In=new T(function(){return E(E(_Ij)[2]);});return A(_Id,[[0,_In,_In]]);}),function(_Io){return A(_Ic,[new T(function(){return A(_Ib,[[0,_A,new T(function(){var _Ip=E(E(_Io)[1]);return [0,_Ip[1],_Ip[2],_rl,_Ip[4],_Ip[5],_Ip[6]];})]]);}),function(_Iq){return A(_Ia,[new T(function(){return A(_Il,[new T(function(){return E(E(_Iq)[2]);})]);}),function(_Ir){return A(_I9,[new T(function(){return A(_Ih,[new T(function(){return E(E(_Ir)[2]);})]);}),function(_Is){var _It=E(_Is);return A(_I8,[[0,[0,new T(function(){return A(_Ig,[_Ik,E(_It[1])[1]]);}),new T(function(){var _Iu=E(E(_Ir)[1]);return _Iu[0]==2?[1,_Iu[1]]:[0];})],_It[2]]]);}]);}]);}]);}]);}]);};},_Iv=new T(function(){return _HX(_8P,_9V,_Fa,_HU);}),_Iw=new T(function(){return _yi(_Eb,_E3);}),_Ix=new T(function(){return _yi(_Eb,_E3);}),_Iy=new T(function(){return A(_pg,[_6R]);}),_Iz=new T(function(){var _IA=A(_Fa,[_Eb]),_IB=E(_Iy),_IC=hs_eqWord64(_IA[1],_IB[1]);if(!E(_IC)){return [1,_yg,_Iw];}else{var _ID=hs_eqWord64(_IA[2],_IB[2]);return E(_ID)==0?[1,_yg,_Ix]:E(_Eb);}}),_IE=[0,_73,_Iz],_IF=[1,_IE,_a],_IG=new T(function(){return _Q(_7A,_IF);}),_IH=new T(function(){return _yi(_Ed,_E3);}),_II=new T(function(){return _yi(_Ed,_E3);}),_IJ=new T(function(){var _IK=A(_Fa,[_Ed]),_IL=E(_Iy),_IM=hs_eqWord64(_IK[1],_IL[1]);if(!E(_IM)){return [1,_yg,_IH];}else{var _IN=hs_eqWord64(_IK[2],_IL[2]);return E(_IN)==0?[1,_yg,_II]:E(_Ed);}}),_IO=[0,_73,_IJ],_IP=[1,_IO,_a],_IQ=new T(function(){return _Q(_7A,_IP);}),_IR=new T(function(){return _yi(_Ef,_E3);}),_IS=new T(function(){return _yi(_Ef,_E3);}),_IT=new T(function(){var _IU=A(_Fa,[_Ef]),_IV=E(_Iy),_IW=hs_eqWord64(_IU[1],_IV[1]);if(!E(_IW)){return [1,_yg,_IR];}else{var _IX=hs_eqWord64(_IU[2],_IV[2]);return E(_IX)==0?[1,_yg,_IS]:E(_Ef);}}),_IY=[0,_73,_IT],_IZ=[1,_IY,_a],_J0=new T(function(){return _Q(_7A,_IZ);}),_J1=function(_J2,_){var _J3=A(_J0,[_J2,_]),_J4=_52(_Ef,_J3,_),_J5=A(_IQ,[_J2,_]),_J6=_52(_Ed,_J5,_),_J7=A(_IG,[_J2,_]),_J8=_52(_Eb,_J7,_);return _J2;},_J9=[1,_Ef],_Ja=[0,_J1,_J9],_Jb=function(_Jc,_){return [0,_Ja,_Jc];},_Jd=new T(function(){return A(_Iv,[_Jb]);}),_Je=new T(function(){return _tA(_Jd,_vO);}),_Jf=function(_Jg,_){var _Jh=_4w(_Hp,_G9,_Jg,_),_Ji=E(_Jh),_Jj=_4w(_G8,_E4,_Ji[2],_),_Jk=E(_Jj),_Jl=_4w(_Je,_HN,_Jk[2],_),_Jm=E(_Jl),_Jn=E(_Jm[1]);return [0,[0,function(_Jo,_){var _Jp=A(_E1,[_Jo,_]),_Jq=A(E(_Ji[1])[1],[_Jo,_]),_Jr=_5w(_Jo,_),_Js=_5w(_Jo,_),_Jt=A(E(_Jk[1])[1],[_Jo,_]),_Ju=_5w(_Jo,_),_Jv=_5w(_Jo,_),_Jw=A(_Jn[1],[_Jo,_]),_Jx=_5w(_Jo,_);return _Jo;},_Jn[2]],_Jm[2]];},_Jy=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_Jz=new T(function(){return _59(_52,_Jy);}),_JA=function(_JB){var _JC=jsShow(E(_JB)[1]);return fromJSStr(_JC);},_JD=function(_JE){var _JF=new T(function(){return _JA(_JE);});return function(_cs){return _1N(_JF,_cs);};},_JG=function(_JH,_JI,_JJ){var _JK=E(_JJ);if(!_JK[0]){return [0];}else{var _JL=_JK[2],_JM=E(_JK[1]);return _JH!=_JM[1]?[1,_JM,new T(function(){return _JG(_JH,_JI,_JL);})]:_1N(_JI,new T(function(){return _JG(_JH,_JI,_JL);}));}},_JN=[0,45],_JO=function(_JP,_JQ,_JR){var _JS=new T(function(){return A(_JP,[[0, -_JR]]);}),_JT=new T(function(){return E(_JQ)[1]<=6?function(_JU){return [1,_JN,new T(function(){return A(_JS,[_JU]);})];}:function(_JV){return [1,_3L,[1,_JN,new T(function(){return A(_JS,[[1,_3K,_JV]]);})]];};});if(_JR>=0){var _JW=isDoubleNegativeZero(_JR);return E(_JW)==0?A(_JP,[[0,_JR]]):E(_JT);}else{return E(_JT);}},_JX=unCStr("canvas"),_JY=unCStr("id"),_JZ=unCStr("canvas"),_K0=function(_K1,_K2){var _K3=new T(function(){return A(_K1,[_K2]);});return function(_K4,_){var _K5=jsCreateElem(toJSStr(E(_JZ))),_K6=jsAppendChild(_K5,E(_K4)[1]),_K7=[0,_K5],_K8=A(_K3,[_K7,_]);return _K7;};},_K9=new T(function(){return _K0(_vl,_34);}),_Ka=function(_Kb,_){var _Kc=A(_K9,[_Kb,_]),_Kd=A(_B,[_H,_Kc,_JY,_JX,_]);return _Kc;},_Ke=[0,_Ka,_Cc],_Kf=function(_Kg,_){return [0,_Ke,_Kg];},_Kh=unCStr("Pattern match failure in do expression at main.hs:179:5-12"),_Ki=function(_Kj,_Kk){while(1){var _Kl=E(_Kk);if(!_Kl[0]){return false;}else{if(!A(_Kj,[_Kl[1]])){_Kk=_Kl[2];continue;}else{return true;}}}},_Km=unCStr("x*x+x+10;"),_Kn=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_Ko=new T(function(){return _5(_Kn);}),_Kp=function(_Kq,_){var _Kr=jsHasCtx2D(_Kq);if(!E(_Kr)){return _b;}else{var _Ks=jsGetCtx2D(_Kq);return [1,[0,[0,_Ks],[0,_Kq]]];}},_Kt=function(_Ku,_){return _Kp(E(_Ku)[1],_);},_Kv=function(_Kw,_Kx){return A(_Kw,[function(_){var _Ky=jsFind(toJSStr(E(_Kx))),_Kz=E(_Ky);return _Kz[0]==0?_b:_Kt(_Kz[1],_);}]);},_KA=new T(function(){return _Kv(_H,_JX);}),_KB=[0,-10],_KC=[0,0],_KD=[0,_KB,_KC],_KE=[0,10],_KF=[0,_KE,_KC],_KG=[1,_KF,_a],_KH=[1,_KD,_KG],_KI=function(_KJ,_){return _A;},_KK=function(_KL){var _KM=E(_KL);if(!_KM[0]){return E(_KI);}else{var _KN=E(_KM[1]);return function(_KO,_){var _KP=E(_KO)[1],_KQ=jsMoveTo(_KP,E(_KN[1])[1],E(_KN[2])[1]);return (function(_KR,_){while(1){var _KS=E(_KR);if(!_KS[0]){return _A;}else{var _KT=E(_KS[1]),_KU=jsLineTo(_KP,E(_KT[1])[1],E(_KT[2])[1]);_KR=_KS[2];continue;}}})(_KM[2],_);};}},_KV=new T(function(){return _KK(_KH);}),_KW=[0,30],_KX=[0,_KC,_KW],_KY=[0,-30],_KZ=[0,_KC,_KY],_L0=[1,_KZ,_a],_L1=[1,_KX,_L0],_L2=new T(function(){return _KK(_L1);}),_L3=function(_L4,_L5,_L6){while(1){var _L7=E(_L5);if(!_L7[0]){return true;}else{var _L8=E(_L6);if(!_L8[0]){return false;}else{if(!A(_bw,[_L4,_L7[1],_L8[1]])){return false;}else{_L5=_L7[2];_L6=_L8[2];continue;}}}}},_L9=unCStr("alert"),_La=function(_Lb){return _L3(_bv,_L9,_Lb);},_Lc=new T(function(){return [0,0/0];}),_Ld=new T(function(){return [0,-1/0];}),_Le=new T(function(){return [0,1/0];}),_Lf=[0,0],_Lg=function(_Lh,_Li){while(1){var _Lj=E(_Lh);if(!_Lj[0]){_Lh=[1,I_fromInt(_Lj[1])];continue;}else{var _Lk=E(_Li);if(!_Lk[0]){_Lh=_Lj;_Li=[1,I_fromInt(_Lk[1])];continue;}else{return I_fromRat(_Lj[1],_Lk[1]);}}}},_Ll=function(_Lm,_Ln){var _Lo=E(_Lm);if(!_Lo[0]){var _Lp=_Lo[1],_Lq=E(_Ln);return _Lq[0]==0?_Lp==_Lq[1]:I_compareInt(_Lq[1],_Lp)==0?true:false;}else{var _Lr=_Lo[1],_Ls=E(_Ln);return _Ls[0]==0?I_compareInt(_Lr,_Ls[1])==0?true:false:I_compare(_Lr,_Ls[1])==0?true:false;}},_Lt=function(_Lu,_Lv){var _Lw=E(_Lu);if(!_Lw[0]){var _Lx=_Lw[1],_Ly=E(_Lv);return _Ly[0]==0?_Lx<_Ly[1]:I_compareInt(_Ly[1],_Lx)>0;}else{var _Lz=_Lw[1],_LA=E(_Lv);return _LA[0]==0?I_compareInt(_Lz,_LA[1])<0:I_compare(_Lz,_LA[1])<0;}},_LB=function(_LC,_LD){return !_Ll(_LD,_Lf)?[0,_Lg(_LC,_LD)]:!_Ll(_LC,_Lf)?!_Lt(_LC,_Lf)?E(_Le):E(_Ld):E(_Lc);},_LE=function(_LF){var _LG=E(_LF);return _LB(_LG[1],_LG[2]);},_LH=function(_LI){return [0,1/E(_LI)[1]];},_LJ=function(_LK){var _LL=E(_LK),_LM=_LL[1];return _LM<0?[0, -_LM]:E(_LL);},_LN=function(_LO){var _LP=E(_LO);return _LP[0]==0?_LP[1]:I_toNumber(_LP[1]);},_LQ=function(_LR){return [0,_LN(_LR)];},_LS=[0,0],_LT=[0,1],_LU=[0,-1],_LV=function(_LW){var _LX=E(_LW)[1];return _LX!=0?_LX<=0?E(_LU):E(_LT):E(_LS);},_LY=function(_LZ,_M0){return [0,E(_LZ)[1]-E(_M0)[1]];},_M1=function(_M2){return [0, -E(_M2)[1]];},_M3=function(_M4,_M5){return [0,E(_M4)[1]+E(_M5)[1]];},_M6=function(_M7,_M8){return [0,E(_M7)[1]*E(_M8)[1]];},_M9=[0,_M3,_M6,_LY,_M1,_LJ,_LV,_LQ],_Ma=function(_Mb,_Mc){return [0,E(_Mb)[1]/E(_Mc)[1]];},_Md=[0,_M9,_Ma,_LH,_LE],_Me=function(_Mf,_Mg){return E(_Mf)[1]!=E(_Mg)[1]?true:false;},_Mh=function(_Mi,_Mj){return E(_Mi)[1]==E(_Mj)[1];},_Mk=[0,_Mh,_Me],_Ml=function(_Mm,_Mn){return E(_Mm)[1]<E(_Mn)[1];},_Mo=function(_Mp,_Mq){return E(_Mp)[1]<=E(_Mq)[1];},_Mr=function(_Ms,_Mt){return E(_Ms)[1]>E(_Mt)[1];},_Mu=function(_Mv,_Mw){return E(_Mv)[1]>=E(_Mw)[1];},_Mx=function(_My,_Mz){var _MA=E(_My)[1],_MB=E(_Mz)[1];return _MA>=_MB?_MA!=_MB?2:1:0;},_MC=function(_MD,_ME){var _MF=E(_MD),_MG=E(_ME);return _MF[1]>_MG[1]?E(_MF):E(_MG);},_MH=function(_MI,_MJ){var _MK=E(_MI),_ML=E(_MJ);return _MK[1]>_ML[1]?E(_ML):E(_MK);},_MM=[0,_Mk,_Mx,_Ml,_Mu,_Mr,_Mo,_MC,_MH],_MN=[0,1],_MO=function(_MP){return E(E(_MP)[1]);},_MQ=function(_MR){return E(E(_MR)[2]);},_MS=function(_MT){return E(E(_MT)[6]);},_MU=[0,2],_MV=function(_MW,_MX){var _MY=E(_MX);return [1,_MY,new T(function(){var _MZ=_MO(_MW);return _MV(_MW,A(_MZ[1],[_MY,new T(function(){return A(_MZ[7],[_MN]);})]));})];},_N0=function(_N1,_N2){var _N3=E(_N2);if(!_N3[0]){return [0];}else{var _N4=_N3[1];return !A(_N1,[_N4])?[0]:[1,_N4,new T(function(){return _N0(_N1,_N3[2]);})];}},_N5=function(_N6,_N7,_N8,_N9){var _Na=new T(function(){return _MS(_N6);});return _N0(function(_Nb){return A(_Na,[_Nb,new T(function(){var _Nc=_MO(_N7),_Nd=_Nc[7];return A(_Nc[1],[_N9,new T(function(){return A(_MQ,[_N7,new T(function(){return A(_Nd,[_MN]);}),new T(function(){return A(_Nd,[_MU]);})]);})]);})]);},_MV(_N7,_N8));},_Ne=new T(function(){return _N5(_MM,_Md,_KB,_KE);}),_Nf=function(_Ng){return [1,_Ng,new T(function(){var _Nh=E(_Ng);return _Nh[0]==0?[0]:_Nf(_Nh[2]);})];},_Ni=function(_Nj,_Nk){var _Nl=E(_Nj);if(!_Nl[0]){return [0];}else{var _Nm=E(_Nk);return _Nm[0]==0?[0]:[1,[0,_Nl[1],_Nm[1]],new T(function(){return _Ni(_Nl[2],_Nm[2]);})];}},_Nn=function(_No){var _Np=new T(function(){return !_Ki(_La,_Nf(_No))?E(_No):E(_Km);}),_Nq=function(_Nr,_){var _Ns=E(_Nr);if(!_Ns[0]){return _a;}else{var _Nt=A(_Ko,[E(toJSStr(_JG(120,new T(function(){return A(_JO,[_JD,_pn,E(_Ns[1])[1],_a]);}),_Np))),_]),_Nu=_Nq(_Ns[2],_);return [1,[0,_Nt],_Nu];}};return function(_cs,_sg){return _4w(_Kf,function(_Nv,_Cj,_){return (function(_Nw,_){return [0,[0,function(_Nx,_){var _Ny=A(_KA,[_]),_Nz=E(_Ny);if(!_Nz[0]){var _NA=_32(_Kh,_);return _Nx;}else{var _NB=_Nq(_Ne,_),_NC=E(_Nz[1]),_ND=jsResetCanvas(E(_NC[2])[1]),_NE=E(_NC[1]),_NF=_NE[1],_NG=jsPushState(_NF),_NH=jsScale(_NF,3,1),_NI=jsPushState(_NF),_NJ=jsTranslate(_NF,50,130),_NK=jsPushState(_NF),_NL=jsRotate(_NF,3.141592653589793),_NM=jsBeginPath(_NF),_NN=A(_KV,[_NE,_]),_NO=A(_L2,[_NE,_]),_NP=A(_KK,[_Ni(_Ne,_NB),_NE,_]),_NQ=jsStroke(_NF),_NR=jsPopState(_NF),_NS=jsPopState(_NF),_NT=jsPopState(_NF);return _Nx;}},_Cc],_Nw];})(_Cj,_);},_cs,_sg);};},_NU=[1,_Km],_NV=new T(function(){return _s6(_8P,_9V,_pg,_yN,_wB);}),_NW=new T(function(){return A(_NV,[_b,_9U,_NU]);}),_NX=new T(function(){return _tA(_NW,_9T);}),_NY=function(_NZ,_){var _O0=A(_NX,[_NZ,_]),_O1=E(_O0),_O2=E(_O1[1]);return [0,[0,function(_O3,_){var _O4=A(_O2[1],[_O3,_]),_O5=_5w(_O3,_);return _O3;},new T(function(){var _O6=E(_O2[2]);return _O6[0]==0?E(_NU):E(_O6);})],_O1[2]];},_O7=function(_O8,_){var _O9=_4w(_NY,_Nn,_O8,_),_Oa=E(_O9),_Ob=E(_Oa[1]),_Oc=new T(function(){return _vB(_vl,_Ob[1]);});return [0,[0,function(_Od,_){var _Oe=A(_Jz,[_Od,_]),_Of=A(_Oc,[_Od,_]);return _Od;},_Ob[2]],_Oa[2]];},_Og=unCStr("work?"),_Oh=function(_Oi,_Oj,_Ok){var _Ol=E(_Oj);return A(_Ol[1],[new T(function(){var _Om=E(_Oi);return E(_Ok);}),function(_On){return A(_Ol[3],[[1,_3F,new T(function(){var _Oo=E(_On);return _1N(_3M(0,E(_Oo[2])[1],_a),_Oo[1]);})]]);}]);},_Op=function(_Oq){return E(E(_Oq)[5]);},_Or=unCStr("for"),_Os=unCStr("label"),_Ot=function(_Ou,_Ov){var _Ow=new T(function(){return _9c(_Ov);}),_Ox=new T(function(){return _9e(_Ow);}),_Oy=new T(function(){return _Oh([0,coercionToken],_3z(_Ou),function(_Oz){return _oM(_Ou,_Oz);});}),_OA=new T(function(){return _3x(_Ou);}),_OB=new T(function(){return _37(_Ou);}),_OC=new T(function(){return _Eh(_Ov);}),_OD=new T(function(){return _37(_Ou);}),_OE=new T(function(){return _Op(_Ov);});return function(_OF,_OG){var _OH=new T(function(){return A(_OE,[_Os,_OF]);});return function(_OI){return A(_OD,[new T(function(){return A(_Oy,[_OI]);}),function(_OJ){var _OK=new T(function(){return A(_OC,[_OH,[1,[0,_Or,new T(function(){return E(E(_OJ)[1]);})],_a]]);});return A(_OB,[new T(function(){return A(_OG,[new T(function(){return E(E(_OJ)[2]);})]);}),function(_OL){var _OM=E(_OL),_ON=E(_OM[1]);return A(_OA,[[0,[0,new T(function(){return A(_Ox,[_OK,_ON[1]]);}),_ON[2]],_OM[2]]]);}]);}]);};};},_OO=new T(function(){return _Ot(_36,_8P);}),_OP=new T(function(){return _Eu(_8P,_9V,_Fa,_Fb);}),_OQ=function(_OR,_OS){return A(_OO,[function(_Cj,_){return _52(_OR,_Cj,_);},new T(function(){return _tA(new T(function(){return A(_OP,[_OR,_OS]);}),_vO);})]);},_OT=function(_Lb){return _OQ(_Og,_Lb);},_OU=unCStr("study?"),_OV=function(_Lb){return _OQ(_OU,_Lb);},_OW=[1,_OV,_a],_OX=[1,_OT,_OW],_OY=new T(function(){return A(_G7,[_OX]);}),_OZ=unCStr("Do you "),_P0=new T(function(){return _5m(_52,_OZ);}),_P1=function(_P2,_){var _P3=A(_OY,[_P2,_]),_P4=E(_P3),_P5=E(_P4[1]);return [0,[0,function(_P6,_){var _P7=A(_P0,[_P6,_]),_P8=A(_P5[1],[_P6,_]),_P9=_5w(_P6,_);return _P6;},_P5[2]],_P4[2]];},_Pa=unCStr("do you enjoy your work? "),_Pb=new T(function(){return _5m(_52,_Pa);}),_Pc=function(_Pd,_Pe,_){return [0,[0,_34,[1,_Pd]],_Pe];},_Pf=function(_Pg,_Ph,_Pi,_){return _4w(_Pg,function(_Pj){return E(_Ph);},_Pi,_);},_Pk=function(_Pl,_Pm,_X,_){return _Pf(_Pl,_Pm,_X,_);},_Pn=function(_Po){return err(_Po);},_Pp=[0,_4w,_Pk,_Pc,_Pn],_Pq=function(_Pr,_Ps,_Pt,_Pu,_Pv,_Pw){var _Px=new T(function(){return _9e(_Pr);});return A(_Ps,[new T(function(){return A(_Pu,[_Pw]);}),function(_Py){var _Pz=E(_Py),_PA=E(_Pz[1]);return A(_Ps,[new T(function(){return A(_Pv,[_Pz[2]]);}),function(_PB){var _PC=E(_PB),_PD=E(_PC[1]);return A(_Pt,[[0,[0,new T(function(){return A(_Px,[_PA[1],_PD[1]]);}),new T(function(){var _PE=E(_PA[2]);return _PE[0]==0?E(_PD[2]):E(_PE);})],_PC[2]]]);}]);}]);},_PF=function(_PG,_PH,_PI,_PJ,_PK,_PL){var _PM=new T(function(){return _Eh(_PI);});return A(_PG,[new T(function(){return A(_PJ,[_PL]);}),function(_PN){var _PO=E(_PN),_PP=E(_PO[1]);return A(_PH,[[0,[0,new T(function(){return A(_PM,[_PP[1],_PK]);}),_PP[2]],_PO[2]]]);}]);},_PQ=function(_PR){return E(E(_PR)[12]);},_PS=function(_PT,_PU,_PV,_PW,_PX,_PY,_PZ){var _Q0=new T(function(){return A(_PQ,[_PT,new T(function(){var _Q1=A(_PV,[_PX]),_Q2=E(_Iy),_Q3=hs_eqWord64(_Q1[1],_Q2[1]);if(!E(_Q3)){return A(_rs,[_PW,_PX]);}else{var _Q4=hs_eqWord64(_Q1[2],_Q2[2]);return E(_Q4)==0?A(_rs,[_PW,_PX]):E(_PX);}}),_PY,_PZ]);}),_Q5=new T(function(){return _3x(_PU);});return function(_Q6){return A(_Q5,[[0,[0,_Q0,[1,_PX]],_Q6]]);};},_Q7=[0,_7C,_Es],_Q8=[1,_Q7,_a],_Q9=[0,_7C,_Es],_Qa=[1,_Q9,_a],_Qb=function(_Qc,_Qd,_Qe,_Qf){var _Qg=new T(function(){return _HX(_Qf,_Qe,_pg,_wB);}),_Qh=new T(function(){return A(_3x,[_Qc,_0]);}),_Qi=new T(function(){return A(_3x,[_Qc,_em]);}),_Qj=new T(function(){return _9c(_Qf);}),_Qk=new T(function(){return _ov(_Qe);}),_Ql=new T(function(){return _p4(_Qf);}),_Qm=new T(function(){return _37(_Qc);});return function(_Qn,_Qo,_Qp){return A(_Qm,[new T(function(){var _Qq=new T(function(){return !E(_Qn)?E(_Qa):[0];}),_Qr=new T(function(){return _PS(_Qf,_Qk,_pg,_yN,_Qp,new T(function(){return A(_Ql,[_Qp]);}),_0);}),_Qs=new T(function(){return !E(_Qn)?[0]:E(_Q8);}),_Qt=new T(function(){return _PS(_Qf,_Qk,_pg,_yN,_Qo,new T(function(){return A(_Ql,[_Qo]);}),_0);});return A(_Qg,[function(_Qu){var _Qv=E(_Qk);return _Pq(_Qj,_Qv[1],_Qv[3],function(_Qw){var _Qx=E(_Qk);return _PF(_Qx[1],_Qx[3],_Qf,_Qt,_Qs,_Qw);},function(_Qy){var _Qz=E(_Qk);return _PF(_Qz[1],_Qz[3],_Qf,_Qr,_Qq,_Qy);},_Qu);}]);}),function(_QA){return !_sZ(_QA,_Qo)?E(_Qh):E(_Qi);}]);};},_QB=new T(function(){return _Qb(_Pp,_8Y,_9V,_8P);}),_QC=unCStr("yes"),_QD=unCStr("no"),_QE=new T(function(){return A(_QB,[_em,_QC,_QD]);}),_QF=unCStr("ok"),_QG=[1,_QF],_QH=new T(function(){return A(_yP,[_QG]);}),_QI=new T(function(){return _tA(_QH,_vO);}),_QJ=function(_QK,_){var _QL=A(_QE,[_QK,_]),_QM=E(_QL),_QN=E(_QM[1]),_QO=A(_QI,[_QM[2],_]),_QP=E(_QO);return [0,[0,function(_QQ,_){var _QR=A(_Pb,[_QQ,_]),_QS=A(_QN[1],[_QQ,_]),_QT=A(E(_QP[1])[1],[_QQ,_]),_QU=_5w(_QQ,_);return _QQ;},new T(function(){var _QV=E(_QN[2]);return _QV[0]==0?[0]:[1,[0,_QV[1]]];})],_QP[2]];},_QW=unCStr("do you study in "),_QX=new T(function(){return _5m(_52,_QW);}),_QY=unCStr("University"),_QZ=function(_Lb){return _OQ(_QY,_Lb);},_R0=unCStr("High School"),_R1=function(_Lb){return _OQ(_R0,_Lb);},_R2=[1,_R1,_a],_R3=[1,_QZ,_R2],_R4=new T(function(){return A(_G7,[_R3]);}),_R5=function(_R6,_){var _R7=A(_R4,[_R6,_]),_R8=E(_R7),_R9=E(_R8[1]);return [0,[0,function(_Ra,_){var _Rb=A(_QX,[_Ra,_]),_Rc=A(_R9[1],[_Ra,_]);return _Ra;},new T(function(){var _Rd=E(_R9[2]);return _Rd[0]==0?[0]:[1,[1,_Rd[1]]];})],_R8[2]];},_Re=new T(function(){return _aL("main.hs:(283,11)-(290,64)|case");}),_Rf=unCStr(" that you enjoy your work"),_Rg=unCStr("False"),_Rh=new T(function(){return _1N(_Rg,_Rf);}),_Ri=unCStr("True"),_Rj=new T(function(){return _1N(_Ri,_Rf);}),_Rk=[0,32],_Rl=function(_Rm,_Rn){var _Ro=new T(function(){return _59(_52,new T(function(){return unAppCStr("You are ",new T(function(){return _1N(_Rm,[1,_Rk,_Rn]);}));}));});return function(_cs,_sg){return _4w(_P1,function(_Rp){var _Rq=new T(function(){return !_sZ(_Rp,_OU)?!_sZ(_Rp,_Og)?E(_Re):E(_QJ):E(_R5);});return function(_cs,_sg){return _4w(_Rq,function(_Rr){return function(_Rs,_){var _Rt=A(new T(function(){var _Ru=E(_Rr);if(!_Ru[0]){var _Rv=new T(function(){return _59(_52,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_Ru[1])?E(_Rh):E(_Rj);}));}));});return function(_Rw,_){return [0,[0,function(_Rx,_){var _Ry=A(_Rv,[_Rx,_]);return _Rx;},_b],_Rw];};}else{var _Rz=new T(function(){return _59(_52,new T(function(){return unAppCStr("You study at the ",_Ru[1]);}));});return function(_RA,_){return [0,[0,function(_RB,_){var _RC=A(_Rz,[_RB,_]);return _RB;},_b],_RA];};}}),[_Rs,_]),_RD=E(_Rt),_RE=E(_RD[1]);return [0,[0,function(_RF,_){var _RG=A(_Ro,[_RF,_]),_RH=A(_RE[1],[_RF,_]);return _RF;},_RE[2]],_RD[2]];};},_cs,_sg);};},_cs,_sg);};},_RI=function(_RJ){var _RK=E(_RJ);return _Rl(_RK[1],_RK[2]);},_RL=unCStr("Who are you? "),_RM=new T(function(){return _59(_52,_RL);}),_RN=unCStr("name"),_RO=unCStr("placeholder"),_RP=[0,_RO,_RN],_RQ=[1,_RP,_a],_RR=unCStr("surname"),_RS=[0,_RO,_RR],_RT=[1,_RS,_a],_RU=[1,_QF],_RV=new T(function(){return A(_yP,[_RU]);}),_RW=new T(function(){return _tA(_RV,_vO);}),_RX=new T(function(){return A(_NV,[_b,_9U,_b]);}),_RY=new T(function(){return A(_NV,[_b,_9U,_b]);}),_RZ=function(_S0,_){var _S1=A(_RY,[_S0,_]),_S2=E(_S1),_S3=E(_S2[1]),_S4=A(_RX,[_S2[2],_]),_S5=E(_S4),_S6=E(_S5[1]),_S7=A(_RW,[_S5[2],_]),_S8=E(_S7),_S9=new T(function(){return _Q(_S6[1],_RT);}),_Sa=new T(function(){return _Q(_S3[1],_RQ);});return [0,[0,function(_Sb,_){var _Sc=A(_RM,[_Sb,_]),_Sd=A(_Sa,[_Sb,_]),_Se=_5w(_Sb,_),_Sf=A(_S9,[_Sb,_]),_Sg=_5w(_Sb,_),_Sh=A(E(_S8[1])[1],[_Sb,_]),_Si=_5w(_Sb,_);return _Sb;},new T(function(){var _Sj=E(_S3[2]);if(!_Sj[0]){return [0];}else{var _Sk=E(_S6[2]);return _Sk[0]==0?[0]:[1,[0,_Sj[1],_Sk[1]]];}})],_S8[2]];},_Sl=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_Sm=unCStr("This formulary is the same than the one "),_Sn=[0,97],_So=[1,_Sn,_a],_Sp=function(_Sq,_Sr){var _Ss=new T(function(){return A(_Sq,[_Sr]);});return function(_St,_){var _Su=jsCreateElem(toJSStr(_So)),_Sv=jsAppendChild(_Su,E(_St)[1]),_Sw=[0,_Su],_Sx=A(_Ss,[_Sw,_]);return _Sw;};},_Sy=unCStr("run in the server by MFlow"),_Sz=new T(function(){return _Sp(_52,_Sy);}),_SA=unCStr("href"),_SB=function(_SC,_){var _SD=_52(_Sm,_SC,_),_SE=A(_Sz,[_SC,_]),_SF=A(_B,[_H,_SE,_SA,_Sl,_]);return _SC;},_SG=new T(function(){return _59(_vl,_SB);}),_SH=unCStr("Fields of a form appear in sequence. Some of the fields trigger events instantly. Some others use a button to trigger them. It also contains option buttons, radio buttons etc"),_SI=new T(function(){return _59(_52,_SH);}),_SJ=function(_SK,_){var _SL=_4w(_RZ,_RI,_SK,_),_SM=E(_SL),_SN=E(_SM[1]);return [0,[0,function(_SO,_){var _SP=A(_SI,[_SO,_]),_SQ=A(_SG,[_SO,_]),_SR=A(_SN[1],[_SO,_]);return _SO;},_SN[2]],_SM[2]];},_SS=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_ST=new T(function(){return _59(_52,_SS);}),_SU=[1,_5L],_SV=unCStr("GalleryIndex"),_SW=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Ab,_Ac,_SV],_SX=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_SW,_a],_SY=function(_SZ){return E(_SX);},_T0=new T(function(){return _CG(_15,_3E,_19,_SY);}),_T1=function(_T2,_){var _T3=A(_T0,[_T2,_]);return [0,[0,_vK,new T(function(){var _T4=E(E(_T3)[1]);return _T4[0]==0?E(_SU):E(_T4);})],new T(function(){return E(E(_T3)[2]);})];},_T5=unCStr("100%"),_T6=[0,62],_T7=[1,_T6,_a],_T8=[1,_T7],_T9=new T(function(){return A(_yP,[_T8]);}),_Ta=new T(function(){return _tA(_T9,_vO);}),_Tb=function(_Tc){return E(_Ta);},_Td=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_Te=[1,_Td,_a],_Tf=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_Tg=[1,_Tf,_Te],_Th=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_Ti=[1,_Th,_Tg],_Tj=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_Tk=[1,_Tj,_Ti],_Tl=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_Tm=[1,_Tl,_Tk],_Tn=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_To=[1,_Tn,_Tm],_Tp=unCStr("height"),_Tq=unCStr("img"),_Tr=function(_Ts,_){var _Tt=jsCreateElem(toJSStr(E(_Tq))),_Tu=jsAppendChild(_Tt,E(_Ts)[1]);return [0,_Tt];},_Tv=function(_Tw,_Tx){while(1){var _Ty=E(_Tw);if(!_Ty[0]){return E(_Tx);}else{_Tw=_Ty[2];var _Tz=_Tx+1|0;_Tx=_Tz;continue;}}},_TA=new T(function(){return [0,_Tv(_To,0)-1|0];}),_TB=[0,_34,_5A],_TC=unCStr("src"),_TD=unCStr("width"),_TE=function(_TF){return function(_cs,_sg){return _4w(function(_Cj,_){return _4w(_Ca,function(_TG){return function(_TH,_){return [0,_TB,new T(function(){var _TI=E(_TG);return [0,_TI[1],_TI[2],_TI[3],_TI[4],_TI[5],new T(function(){return _BX(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_SW,_a,new T(function(){var _TJ=E(_TF)[1];return _TJ!=E(_TA)[1]?[0,_TJ+1|0]:E(_5L);}),_TI[6]);})];})];};},_Cj,_);},function(_TK,_Cj,_){return (function(_Cj,_){return _4w(function(_TL,_){return [0,[0,function(_TM,_){var _TN=_Tr(_TM,_),_TO=A(_B,[_H,_TN,_TC,new T(function(){var _TP=E(_TF)[1];return _TP>=0?_wG(_To,_TP):E(_wD);}),_]),_TQ=A(_B,[_H,_TN,_TD,_T5,_]),_TR=A(_B,[_H,_TN,_Tp,_T5,_]),_TS=_5w(_TM,_);return _TM;},_Cc],_TL];},_Tb,_Cj,_);})(_Cj,_);},_cs,_sg);};},_TT=function(_Cj,_){return _4w(_T1,_TE,_Cj,_);},_TU=function(_TV,_TW,_){return _TX(_TW,_);},_TY=function(_Cj,_){return _z9(_TT,_TU,_Cj,_);},_TZ=[0,20000],_U0=new T(function(){return _3R(_15,_3E,_19,_16);}),_U1=function(_U2,_U3,_U4,_){var _U5=A(_U0,[_U4,_]),_U6=new T(function(){return E(E(_U5)[1]);}),_U7=new T(function(){return [0,_U8];}),_U8=function(_){var _U9=jsFind(toJSStr(E(_U6))),_Ua=E(_U9);if(!_Ua[0]){return _A;}else{var _Ub=E(_Ua[1]),_Uc=E(_Ub),_Ud=jsClearChildren(_Ub[1]),_Ue=E(_k)[1],_Uf=takeMVar(_Ue),_Ug=A(_U3,[_Uf,_]),_Uh=E(_Ug),_Ui=E(_Uh[1]),_Uj=_Ui[1],_Uk=_Ui[2],_=putMVar(_Ue,new T(function(){var _Ul=E(_Uh[2]);return [0,_Ul[1],_Ul[2],_Ul[3],_Ul[4],_0,_Ul[6]];}));if(!E(E(_Uf)[5])){var _Um=A(_Uj,[_Uc,_]),_Un=E(_Uk);if(!_Un[0]){var _Uo=jsSetTimeout(E(_U2)[1],E(_U7)[1]);return _A;}else{var _Up=E(_Un[1]);return _A;}}else{var _Uq=A(_7,[E(_Uc[1]),_]),_Ur=A(_Uj,[[0,_Uq],_]),_Us=E(_Uk);if(!_Us[0]){var _Ut=jsSetTimeout(E(_U2)[1],E(_U7)[1]);return _A;}else{var _Uu=E(_Us[1]);return _A;}}}},_Uv=jsSetTimeout(E(_U2)[1],E(_U7)[1]);return _yW(_U6,_U3,new T(function(){return E(E(_U5)[2]);}),_);},_TX=function(_Uw,_){var _Ux=_U1(_TZ,_TY,_Uw,_),_Uy=E(_Ux),_Uz=E(_Uy[1]);return [0,[0,function(_UA,_){var _UB=A(_ST,[_UA,_]),_UC=A(_Uz[1],[_UA,_]);return _UA;},_Uz[2]],_Uy[2]];},_UD=function(_UE){var _UF=new T(function(){return _5m(_52,new T(function(){return unAppCStr(" returns ",_UE);}));});return function(_UG,_){return [0,[0,_UF,_Cc],_UG];};},_UH=unCStr("This link say Hey!"),_UI=function(_Cj,_){return _52(_UH,_Cj,_);},_UJ=unCStr("Hey!"),_UK=function(_UL,_UM,_){var _UN=jsWriteHandle(E(_UL)[1],toJSStr(E(_UM)));return _A;},_UO=[0,10],_UP=[1,_UO,_a],_UQ=function(_UR,_US,_){var _UT=E(_UR),_UU=jsWriteHandle(_UT[1],toJSStr(E(_US)));return _UK(_UT,_UP,_);},_UV=function(_){var _=0,_UW=newMVar(),_=putMVar(_UW,_b);return [0,_UW];},_UX=new T(function(){return _2(_UV);}),_UY=new T(function(){return _3R(_15,_3E,_19,_16);}),_UZ=new T(function(){return A(_pg,[_6R]);}),_V0=unCStr("EMPTY"),_V1=[1,_yg,_a],_V2=new T(function(){return _yi(_V0,_V1);}),_V3=[1,_yg,_V2],_V4=function(_V5,_V6,_){var _=putMVar(E(_V5)[1],_V6);return _A;},_V7=function(_V8,_V9,_){return _sH(function(_){return _V4(_UX,_V8,_);},_V9,_);},_Va=function(_){var _Vb=E(_UX)[1],_Vc=takeMVar(_Vb),_=putMVar(_Vb,_Vc);return _Vc;},_Vd=function(_){var _=0,_Ve=jsMkStdout();return [0,_Ve];},_Vf=new T(function(){return _2(_Vd);}),_Vg=function(_Vh,_Vi,_Vj,_Vk){var _Vl=new T(function(){return _Sp(_vl,_Vk);}),_Vm=new T(function(){return unAppCStr("#/",new T(function(){var _Vn=A(_Vi,[_Vj]),_Vo=E(_UZ),_Vp=hs_eqWord64(_Vn[1],_Vo[1]);if(!E(_Vp)){return A(_rs,[_Vh,_Vj]);}else{var _Vq=hs_eqWord64(_Vn[2],_Vo[2]);return E(_Vq)==0?A(_rs,[_Vh,_Vj]):E(_Vj);}}));});return function(_Vr,_){var _Vs=A(_UY,[_Vr,_]),_Vt=0,_Vu=function(_,_Vv,_Vw){var _Vx=new T(function(){return E(E(_Vs)[1]);}),_Vy=function(_Vz,_){var _VA=A(_Vl,[_Vz,_]),_VB=A(_B,[_H,_VA,_SA,_Vm,_]),_VC=E(_VA),_VD=jsSetCB(_VC[1],E(_tg)[1],E([0,function(_VE,_VF,_){return (function(_){var _VG=0;if(!E(_VG)){return (function(_){var _VH=takeMVar(E(_UX)[1]),_VI=jsCatch(function(_){return (function(_){return [1,_Vx];})();},function(_X,_){return _V7(_VH,_X,_);});return _V4(_UX,_VI,_);})();}else{var _VJ=takeMVar(E(_UX)[1]),_VK=jsCatch(function(_){return [1,_Vx];},function(_X,_){return _V7(_VJ,_X,_);});return _V4(_UX,_VK,_);}})(_);}])[1]);return _VC;},_VL=E(_Vv);if(!_VL[0]){var _VM=_UQ(_Vf,_V3,_);return [0,[0,_Vy,_b],_Vw];}else{if(!_sZ(_VL[1],_Vx)){var _VN=_UQ(_Vf,_V3,_);return [0,[0,_Vy,_b],_Vw];}else{return [0,[0,_Vy,[1,_Vj]],_Vw];}}};if(!E(_Vt)){var _VO=_Va();return _Vu(_,_VO,new T(function(){return E(E(_Vs)[2]);}));}else{var _VP=E(_UX)[1],_VQ=takeMVar(_VP),_=putMVar(_VP,_VQ);return _Vu(_,_VQ,new T(function(){return E(E(_Vs)[2]);}));}};},_VR=new T(function(){return _Vg(_Fb,_Fa,_UJ,_UI);}),_VS=new T(function(){return _tA(_VR,_vO);}),_VT=function(_VU,_){var _VV=A(_VS,[_VU,_]),_VW=E(_VV),_VX=E(_VW[1]);return [0,[0,function(_VY,_){var _VZ=_5w(_VY,_),_W0=A(_VX[1],[_VY,_]);return _VY;},_VX[2]],_VW[2]];},_W1=function(_){var _W2=E(_sG)[1],_W3=takeMVar(_W2),_=putMVar(_W2,_W3);return _W3;},_W4=function(_W5,_){var _W6=0;if(!E(_W6)){var _W7=_W1();return [0,[0,_34,[1,_W7]],new T(function(){var _W8=E(_W5);return [0,_W8[1],_W8[2],_W8[3],_W8[4],_em,_W8[6]];})];}else{var _W9=E(_sG)[1],_Wa=takeMVar(_W9),_=putMVar(_W9,_Wa);return [0,[0,_34,[1,_Wa]],new T(function(){var _Wb=E(_W5);return [0,_Wb[1],_Wb[2],_Wb[3],_Wb[4],_em,_Wb[6]];})];}},_Wc=function(_Wd,_We,_Wf){return A(_Wd,[[1,_2A,new T(function(){return A(_We,[_Wf]);})]]);},_Wg=unCStr("Key "),_Wh=unCStr("Mouse "),_Wi=unCStr("Click "),_Wj=unCStr("NoData"),_Wk=function(_Wl){return _1N(_Wj,_Wl);},_Wm=unCStr(": empty list"),_Wn=unCStr("Prelude."),_Wo=function(_Wp){return err(_1N(_Wn,new T(function(){return _1N(_Wp,_Wm);})));},_Wq=unCStr("foldr1"),_Wr=new T(function(){return _Wo(_Wq);}),_Ws=function(_Wt,_Wu){var _Wv=E(_Wu);if(!_Wv[0]){return E(_Wr);}else{var _Ww=_Wv[1],_Wx=E(_Wv[2]);return _Wx[0]==0?E(_Ww):A(_Wt,[_Ww,new T(function(){return _Ws(_Wt,_Wx);})]);}},_Wy=[0,32],_Wz=function(_WA,_WB){var _WC=E(_WB);switch(_WC[0]){case 0:return E(_Wk);case 1:var _WD=function(_WE){return _3M(11,E(_WC[1])[1],[1,_Wy,new T(function(){var _WF=E(_WC[2]);return [1,_3L,new T(function(){return A(_Ws,[_Wc,[1,function(_WG){return _3M(0,E(_WF[1])[1],_WG);},[1,function(_WH){return _3M(0,E(_WF[2])[1],_WH);},_a]],[1,_3K,_WE]]);})];})]);};return E(_WA)[1]<11?function(_WI){return _1N(_Wi,new T(function(){return _WD(_WI);}));}:function(_WJ){return [1,_3L,new T(function(){return _1N(_Wi,new T(function(){return _WD([1,_3K,_WJ]);}));})];};case 2:var _WK=function(_WL){return _1N(_Wh,new T(function(){var _WM=E(_WC[1]);return [1,_3L,new T(function(){return A(_Ws,[_Wc,[1,function(_WN){return _3M(0,E(_WM[1])[1],_WN);},[1,function(_WO){return _3M(0,E(_WM[2])[1],_WO);},_a]],[1,_3K,_WL]]);})];}));};return E(_WA)[1]<11?E(_WK):function(_WP){return [1,_3L,new T(function(){return _WK([1,_3K,_WP]);})];};default:var _WQ=_WC[1];return E(_WA)[1]<11?function(_WR){return _1N(_Wg,new T(function(){return _3M(11,E(_WQ)[1],_WR);}));}:function(_WS){return [1,_3L,new T(function(){return _1N(_Wg,new T(function(){return _3M(11,E(_WQ)[1],[1,_3K,_WS]);}));})];};}},_WT=function(_WU){var _WV=new T(function(){return _59(_52,new T(function(){var _WW=E(_WU);return _1N(_WW[1],[1,_Rk,new T(function(){return A(_Wz,[_pn,_WW[2],_a]);})]);}));});return function(_WX,_){return [0,[0,_WV,_Cc],_WX];};},_WY=function(_Cj,_){return _4w(_W4,_WT,_Cj,_);},_WZ=function(_X0){return E(_WY);},_X1=[14,coercionToken],_X2=[12,coercionToken],_X3=[9,coercionToken],_X4=[11,coercionToken],_X5=[5,coercionToken],_X6=[10,coercionToken],_X7=[6,coercionToken],_X8=[7,coercionToken],_X9=unCStr("height:100px;background-color:lightgreen;position:relative"),_Xa=unCStr("div"),_Xb=function(_Xc,_Xd){var _Xe=new T(function(){return A(_Xc,[_Xd]);});return function(_Xf,_){var _Xg=jsCreateElem(toJSStr(E(_Xa))),_Xh=jsAppendChild(_Xg,E(_Xf)[1]),_Xi=[0,_Xg],_Xj=A(_Xe,[_Xi,_]);return _Xi;};},_Xk=unCStr("h1"),_Xl=function(_Xm,_Xn){var _Xo=new T(function(){return A(_Xm,[_Xn]);});return function(_Xp,_){var _Xq=jsCreateElem(toJSStr(E(_Xk))),_Xr=jsAppendChild(_Xq,E(_Xp)[1]),_Xs=[0,_Xq],_Xt=A(_Xo,[_Xs,_]);return _Xs;};},_Xu=unCStr("Mouse events here"),_Xv=new T(function(){return _Xl(_52,_Xu);}),_Xw=new T(function(){return _Xb(_vl,_Xv);}),_Xx=function(_Xy,_){var _Xz=A(_Xw,[_Xy,_]),_XA=A(_B,[_H,_Xz,_DE,_X9,_]);return _Xz;},_XB=[0,_Xx,_Cc],_XC=function(_XD,_){return [0,_XB,_XD];},_XE=new T(function(){return _tA(_XC,_X8);}),_XF=new T(function(){return _tA(_XE,_X7);}),_XG=new T(function(){return _tA(_XF,_X6);}),_XH=new T(function(){return _tA(_XG,_X5);}),_XI=new T(function(){return _tA(_XH,_X4);}),_XJ=new T(function(){return _tA(_XI,_vO);}),_XK=new T(function(){return _tA(_XJ,_X3);}),_XL=new T(function(){return _tA(_XK,_X2);}),_XM=new T(function(){return _tA(_XL,_X1);}),_XN=new T(function(){return _tA(_XM,_9T);}),_XO=unCStr("http://todomvc.com"),_XP=unCStr("Work in progress for a todo application to be added to "),_XQ=unCStr("todomvc.com"),_XR=new T(function(){return _Sp(_52,_XQ);}),_XS=function(_XT,_){var _XU=_52(_XP,_XT,_),_XV=A(_XR,[_XT,_]),_XW=A(_B,[_H,_XV,_SA,_XO,_]);return _XT;},_XX=new T(function(){return _59(_vl,_XS);}),_XY=unCStr("Tasks"),_XZ=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Ab,_Ac,_XY],_Y0=2,_Y1=function(_Y2,_Y3,_Y4,_Y5,_){var _Y6=A(_Y4,[_Y5,_]),_Y7=E(_Y6),_Y8=E(_Y7[1]),_Y9=_Y8[1];return [0,[0,function(_Ya,_){var _Yb=jsFind(toJSStr(E(_Y2))),_Yc=E(_Yb);if(!_Yc[0]){return _Ya;}else{var _Yd=_Yc[1];switch(E(_Y3)){case 0:var _Ye=A(_Y9,[_Yd,_]);return _Ya;case 1:var _Yf=E(_Yd),_Yg=_Yf[1],_Yh=jsGetChildren(_Yg),_Yi=E(_Yh);if(!_Yi[0]){var _Yj=A(_Y9,[_Yf,_]);return _Ya;}else{var _Yk=jsCreateElem(toJSStr(E(_Y))),_Yl=jsAddChildBefore(_Yk,_Yg,E(_Yi[1])[1]),_Ym=A(_Y9,[[0,_Yk],_]);return _Ya;}break;default:var _Yn=E(_Yd),_Yo=jsClearChildren(_Yn[1]),_Yp=A(_Y9,[_Yn,_]);return _Ya;}}},_Y8[2]],_Y7[2]];},_Yq=[0,_34,_5A],_Yr=function(_Ys,_){return [0,_Yq,_Ys];},_Yt=unCStr("Pattern match failure in do expression at main.hs:339:7-25"),_Yu=new T(function(){return _Pn(_Yt);}),_Yv=function(_Yw,_Yx,_Yy,_Yz){return A(_Yw,[new T(function(){return function(_){var _YA=jsSet(E(_Yx)[1],toJSStr(E(_Yy)),toJSStr(E(_Yz)));return _A;};})]);},_YB=unCStr("text"),_YC=unCStr("value"),_YD=new T(function(){return _6S(_p9,_pe);}),_YE=new T(function(){return A(_YD,[_6R]);}),_YF=new T(function(){return A(_YD,[_6R]);}),_YG=unCStr("Prelude.read: ambiguous parse"),_YH=unCStr("Prelude.read: no parse"),_YI=function(_YJ){return [1,function(_YK){return A(_kH,[_YK,function(_YL){return E([3,_YJ,_bY]);}]);}];},_YM=function(_YN){while(1){var _YO=(function(_YP){var _YQ=E(_YP);if(!_YQ[0]){return [0];}else{var _YR=_YQ[2],_YS=E(_YQ[1]);if(!E(_YS[2])[0]){return [1,_YS[1],new T(function(){return _YM(_YR);})];}else{_YN=_YR;return null;}}})(_YN);if(_YO!=null){return _YO;}}},_YT=function(_YU,_YV){var _YW=_YM(_aO(A(E(_YU)[3],[_mY,_YI]),_YV));return _YW[0]==0?err(_YH):E(_YW[2])[0]==0?E(_YW[1]):err(_YG);},_YX=function(_YY,_YZ,_Z0,_Z1){var _Z2=new T(function(){return _rs(_YZ);}),_Z3=new T(function(){return _s6(_8P,_9V,_Z0,_YZ,_YY);});return [0,function(_Z4){return A(_Z3,[[1,_Z1],_YB,_Z4]);},function(_Z5,_){var _Z6=E(_Z1),_Z7=jsFind(toJSStr(_Z6)),_Z8=E(_Z7);return _Z8[0]==0?_45(_Z6):A(_Yv,[_H,_Z8[1],_YC,new T(function(){var _Z9=A(_Z0,[_Z5]),_Za=E(_YE),_Zb=hs_eqWord64(_Z9[1],_Za[1]);if(!E(_Zb)){return A(_Z2,[_Z5]);}else{var _Zc=hs_eqWord64(_Z9[2],_Za[2]);return E(_Zc)==0?A(_Z2,[_Z5]):E(_Z5);}}),_]);},function(_){var _Zd=E(_Z1),_Ze=jsFind(toJSStr(_Zd)),_Zf=E(_Ze);if(!_Zf[0]){return _45(_Zd);}else{var _Zg=_Ej(E(_Zf[1])[1],_YC,_);return new T(function(){var _Zh=A(_YD,[_Zg]),_Zi=E(_YF),_Zj=hs_eqWord64(_Zh[1],_Zi[1]);if(!E(_Zj)){return _YT(_YY,_Zg);}else{var _Zk=hs_eqWord64(_Zh[2],_Zi[2]);return E(_Zk)==0?_YT(_YY,_Zg):E(_Zg);}});}}];},_Zl=unCStr("todo"),_Zm=new T(function(){var _Zn=_YX(_HU,_Fb,_Fa,_Zl);return [0,_Zn[1],_Zn[2],_Zn[3]];}),_Zo=new T(function(){var _Zp=A(E(_Zm)[2],[_a]);return function(_Zq,_){var _Zr=A(_Zp,[_]);return [0,[0,_34,[1,_Zr]],_Zq];};}),_Zs=function(_Zt,_){return A(_Zo,[new T(function(){var _Zu=E(_Zt);return [0,_Zu[1],_Zu[2],_Zu[3],_Zu[4],_em,_Zu[6]];}),_]);},_Zv=[1,_a],_Zw=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_XZ,_a],_Zx=function(_Zy){return E(_Zw);},_Zz=new T(function(){return _CG(_15,_3E,_19,_Zx);}),_ZA=function(_ZB,_){var _ZC=A(_Zz,[_ZB,_]);return [0,[0,_vK,new T(function(){var _ZD=E(E(_ZC)[1]);return _ZD[0]==0?E(_Zv):E(_ZD);})],new T(function(){return E(E(_ZC)[2]);})];},_ZE=[0,_34,_5A],_ZF=[0,_34,_5A],_ZG=function(_ZH,_ZI,_){return [0,_ZF,_ZI];},_ZJ=[0,_34,_5A],_ZK=function(_ZL,_){return [0,_ZJ,_ZL];},_ZM=unCStr("list"),_ZN=unCStr("check"),_ZO=new T(function(){return A(_GQ,[_0,_ZN]);}),_ZP=new T(function(){return _tA(_ZO,_vO);}),_ZQ=unCStr("nocheck"),_ZR=[1,_ZQ,_a],_ZS=[0,_ZR],_ZT=[1,_ZS],_ZU=function(_ZV,_){var _ZW=A(_ZP,[_ZV,_]),_ZX=E(_ZW),_ZY=E(_ZX[1]);return [0,[0,function(_ZZ,_){var _100=A(_ZY[1],[_ZZ,_]);return _ZZ;},new T(function(){var _101=E(_ZY[2]);return _101[0]==0?E(_ZT):E(_101);})],_ZX[2]];},_102=unCStr("text-decoration:line-through;"),_103=unCStr("li"),_104=function(_105,_106){var _107=new T(function(){return A(_105,[_106]);});return function(_108,_){var _109=jsCreateElem(toJSStr(E(_103))),_10a=jsAppendChild(_109,E(_108)[1]),_10b=[0,_109],_10c=A(_107,[_10b,_]);return _10b;};},_10d=function(_10e){var _10f=E(_10e);if(!_10f[0]){return [0];}else{var _10g=new T(function(){return _5m(_52,_10f[1]);});return [1,function(_10h,_){var _10i=_4w(_ZU,function(_10j){return (function(_10k){var _10l=E(_10k);return _10l[0]==0?function(_10m,_){return [0,[0,_10g,_Cc],_10m];}:!_sZ(_10l[1],_ZN)?function(_10n,_){return [0,[0,_10g,_Cc],_10n];}:E(_10l[2])[0]==0?function(_10o,_){return [0,[0,function(_10p,_){var _10q=A(_10g,[_10p,_]),_10r=A(_B,[_H,_10q,_DE,_102,_]);return _10q;},_Cc],_10o];}:function(_10s,_){return [0,[0,_10g,_Cc],_10s];};})(E(_10j)[1]);},_10h,_),_10t=E(_10i),_10u=E(_10t[1]);return [0,[0,new T(function(){return _104(_vl,_10u[1]);}),_10u[2]],_10t[2]];},new T(function(){return _10d(_10f[2]);})];}},_10v=function(_10w,_10x){while(1){var _10y=(function(_10z,_10A){var _10B=E(_10A);if(!_10B[0]){return E(_10z);}else{_10w=function(_10C,_){var _10D=A(_10z,[_10C,_]),_10E=E(_10D),_10F=E(_10E[1]),_10G=A(_10B[1],[_10E[2],_]),_10H=E(_10G),_10I=E(_10H[1]);return [0,[0,function(_10J,_){var _10K=A(_10F[1],[_10J,_]),_10L=A(_10I[1],[_10J,_]);return _10J;},new T(function(){var _10M=E(_10F[2]);return _10M[0]==0?E(_10I[2]):E(_10M);})],_10H[2]];};_10x=_10B[2];return null;}})(_10w,_10x);if(_10y!=null){return _10y;}}},_10N=function(_10O,_10P,_){return _4w(_W4,function(_10Q){var _10R=E(E(_10Q)[2]);return _10R[0]==3?E(E(_10R[1])[1])==13?function(_Cj,_){return _4w(_Zs,function(_10S){return function(_Cj,_){return _4w(_ZA,function(_10T){var _10U=new T(function(){return _10v(_ZK,_10d([1,_10O,_10T]));});return function(_cs,_sg){return _4w(function(_Cj,_){return _4w(_Ca,function(_10V){return function(_10W,_){return [0,_ZE,new T(function(){var _10X=E(_10V);return [0,_10X[1],_10X[2],_10X[3],_10X[4],_10X[5],new T(function(){return _BX(I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_XZ,_a,[1,_10O,_10T],_10X[6]);})];})];};},_Cj,_);},function(_10Y,_Cj,_){return (function(_Cj,_){return _4w(function(_Cj,_){return _Y1(_ZM,_Y0,_10U,_Cj,_);},_ZG,_Cj,_);})(_Cj,_);},_cs,_sg);};},_Cj,_);};},_Cj,_);}:E(_Yr):E(_Yu);},_10P,_);},_10Z=new T(function(){return A(E(_Zm)[1],[_b]);}),_110=new T(function(){return _tA(_10Z,_9T);}),_111=unCStr("todos"),_112=new T(function(){return _Xl(_52,_111);}),_113=new T(function(){return _Xb(_vl,_34);}),_114=function(_115,_){var _116=_4w(_110,_10N,_115,_),_117=E(_116),_118=E(_117[1]),_119=new T(function(){return _vB(_vl,function(_11a,_){var _11b=A(_112,[_11a,_]),_11c=A(_118[1],[_11a,_]);return _11a;});});return [0,[0,function(_11d,_){var _11e=A(_119,[_11d,_]),_11f=A(_113,[_11d,_]),_11g=A(_B,[_H,_11f,_JY,_ZM,_]);return _11d;},new T(function(){var _11h=E(_118[2]);return _11h[0]==0?E(_Cc):E(_11h);})],_117[2]];},_11i=function(_11j,_11k,_){return [0,[0,_34,[1,[1,_11j]]],_11k];},_11l=unCStr("revEntry"),_11m=new T(function(){var _11n=_YX(_HU,_Fb,_Fa,_11l);return [0,_11n[1],_11n[2],_11n[3]];}),_11o=new T(function(){return A(E(_11m)[1],[_b]);}),_11p=new T(function(){return _tA(_11o,_9T);}),_11q=function(_11r,_11s,_){return [0,[0,_34,[1,[0,_11r]]],_11s];},_11t=unCStr("entry"),_11u=new T(function(){var _11v=_YX(_HU,_Fb,_Fa,_11t);return [0,_11v[1],_11v[2],_11v[3]];}),_11w=new T(function(){return A(E(_11u)[1],[_b]);}),_11x=new T(function(){return _tA(_11w,_9T);}),_11y=function(_11z,_){var _11A=_4w(_11x,_11q,_11z,_),_11B=E(_11A),_11C=E(_11B[1]),_11D=_4w(_11p,_11i,_11B[2],_),_11E=E(_11D),_11F=E(_11E[1]);return [0,[0,new T(function(){return _vB(_vl,function(_11G,_){var _11H=A(_11C[1],[_11G,_]),_11I=_5w(_11G,_),_11J=A(_11F[1],[_11G,_]);return _11G;});}),new T(function(){var _11K=E(_11C[2]);return _11K[0]==0?E(_11F[2]):E(_11K);})],_11E[2]];},_11L=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_11M=new T(function(){return _59(_52,_11L);}),_11N=function(_11O,_11P,_11Q,_11R){return A(_11O,[new T(function(){return A(_11O,[new T(function(){return A(_11P,[[0,_11R,_11R]]);}),function(_11S){return A(_11P,[[0,_A,new T(function(){var _11T=E(E(_11S)[1]);return [0,_11T[1],_11T[2],_11T[3],_11T[4],_em,_11T[6]];})]]);}]);}),function(_11U){return A(_11Q,[new T(function(){return E(E(_11U)[2]);})]);}]);},_11V=function(_11W,_11X,_11Y,_11Z,_120){var _121=new T(function(){return _Fr(_11W);});return A(_11X,[new T(function(){return A(_11X,[_11Z,function(_122){return A(_11Y,[[0,_122,_120]]);}]);}),function(_123){return A(_11Y,[[0,[0,_121,[1,new T(function(){return E(E(_123)[1]);})]],new T(function(){return E(E(_123)[2]);})]]);}]);},_124=function(_125,_126,_127){var _128=new T(function(){return _ov(_126);});return function(_129){var _12a=E(_128);return _11N(_12a[1],_12a[3],new T(function(){var _12b=new T(function(){return _9c(_125);});return function(_12c){var _12d=E(_128);return _11V(_12b,_12d[1],_12d[3],A(_ox,[_126,_127]),_12c);};}),_129);};},_12e=function(_12f){return _124(_8P,_9V,new T(function(){return A(E(_11m)[2],[_12f]);}));},_12g=function(_12h,_12i){while(1){var _12j=E(_12h);if(!_12j[0]){return E(_12i);}else{_12h=_12j[2];var _12k=[1,_12j[1],_12i];_12i=_12k;continue;}}},_12l=function(_12m){var _12n=new T(function(){return _12g(_12m,_a);});return function(_12o,_){return [0,[0,_34,[1,_12n]],_12o];};},_12p=function(_12q,_){var _12r=A(E(_11u)[3],[_]);return [0,[0,_34,[1,_12r]],new T(function(){var _12s=E(_12q);return [0,_12s[1],_12s[2],_12s[3],_12s[4],_em,_12s[6]];})];},_12t=function(_Cj,_){return _4w(_12p,_12l,_Cj,_);},_12u=function(_Cj,_){return _4w(_12t,_12e,_Cj,_);},_12v=function(_12w){return _124(_8P,_9V,new T(function(){return A(E(_11u)[2],[_12w]);}));},_12x=function(_12y,_){var _12z=A(E(_11m)[3],[_]);return [0,[0,_34,[1,_12z]],new T(function(){var _12A=E(_12y);return [0,_12A[1],_12A[2],_12A[3],_12A[4],_em,_12A[6]];})];},_12B=function(_12C){var _12D=new T(function(){return _12g(_12C,_a);});return function(_12E,_){return [0,[0,_34,[1,_12D]],_12E];};},_12F=function(_Cj,_){return _4w(_12x,_12B,_Cj,_);},_12G=function(_Cj,_){return _4w(_12F,_12v,_Cj,_);},_12H=function(_12I){return E(_12I)[0]==0?E(_12u):E(_12G);},_12J=function(_12K,_){var _12L=_4w(_11y,_12H,_12K,_),_12M=E(_12L),_12N=E(_12M[1]);return [0,[0,function(_12O,_){var _12P=A(_11M,[_12O,_]),_12Q=A(_12N[1],[_12O,_]);return _12O;},_12N[2]],_12M[2]];},_12R=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_12S=new T(function(){return _59(_52,_12R);}),_12T=[0,_34,_b],_12U=function(_12V,_){return [0,_12T,_12V];},_12W=function(_12X,_12Y,_12Z){var _130=E(_12X),_131=_130[1],_132=_130[2],_133=_130[3],_134=_130[4],_135=E(_12Z);if(!_135[0]){var _136=_135[2],_137=_135[3],_138=_135[4],_139=_135[5];switch(_Aq(_130,_136)){case 0:return _AD(_136,_137,_BX(_131,_132,_133,_134,_12Y,_138),_139);case 1:return [0,_135[1],E(_130),_12Y,E(_138),E(_139)];default:return _Bi(_136,_137,_138,_BX(_131,_132,_133,_134,_12Y,_139));}}else{return [0,1,E(_130),_12Y,E(_9),E(_9)];}},_13a=function(_13b,_13c,_13d){var _13e=E(_13d);if(!_13e[0]){var _13f=_13e[3],_13g=_13e[4],_13h=_13e[5],_13i=E(_13e[2]),_13j=_13i[1];return _13b>=_13j?_13b!=_13j?_Bi(_13i,_13f,_13g,_13a(_13b,_13c,_13h)):[0,_13e[1],E([0,_13b]),_13c,E(_13g),E(_13h)]:_AD(_13i,_13f,_13a(_13b,_13c,_13g),_13h);}else{return [0,1,E([0,_13b]),_13c,E(_9),E(_9)];}},_13k=function(_13l,_13m,_13n){var _13o=E(_13l),_13p=_13o[1],_13q=E(_13n);if(!_13q[0]){var _13r=_13q[3],_13s=_13q[4],_13t=_13q[5],_13u=E(_13q[2]),_13v=_13u[1];return _13p>=_13v?_13p!=_13v?_Bi(_13u,_13r,_13s,_13a(_13p,_13m,_13t)):[0,_13q[1],E(_13o),_13m,E(_13s),E(_13t)]:_AD(_13u,_13r,_13a(_13p,_13m,_13s),_13t);}else{return [0,1,E(_13o),_13m,E(_9),E(_9)];}},_13w=function(_13x,_13y){while(1){var _13z=E(_13y);if(!_13z[0]){var _13A=E(_13z[2])[1];if(_13x>=_13A){if(_13x!=_13A){_13y=_13z[5];continue;}else{return [1,_13z[3]];}}else{_13y=_13z[4];continue;}}else{return [0];}}},_13B=unCStr("containers-0.5.5.1"),_13C=unCStr("Data.Map.Base"),_13D=unCStr("Map"),_13E=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_13B,_13C,_13D],_13F=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_13E,_a],_13G=function(_13H){return E(_13F);},_13I=function(_13J){var _13K=E(_13J);if(!_13K[0]){return [0];}else{var _13L=E(_13K[1]);return [1,[0,_13L[1],_13L[2]],new T(function(){return _13I(_13K[2]);})];}},_13M=function(_13N,_13O){return function(_13P){return E(new T(function(){var _13Q=A(_13N,[_6R]),_13R=E(_13Q[3]),_13S=_13R[1],_13T=_13R[2],_13U=_1N(_13Q[4],[1,new T(function(){return A(_13O,[_6R]);}),_a]);if(!_13U[0]){return [0,_13S,_13T,_13R,_a];}else{var _13V=_6m(new T(function(){return _6a(_6y(_6J,[1,[0,_13S,_13T],new T(function(){return _13I(_13U);})]));}));return [0,_13V[1],_13V[2],_13R,_13U];}}));};},_13W=new T(function(){return _13M(_13G,_ot);}),_13X=new T(function(){return _6S(_13W,_ot);}),_13Y=new T(function(){return _CG(_15,_3E,_19,_13X);}),_13Z=function(_140,_){var _141=A(_13Y,[_140,_]);return [0,[0,_34,new T(function(){return E(E(_141)[1]);})],new T(function(){return E(E(_141)[2]);})];},_142=new T(function(){return _6S(_13W,_ot);}),_143=[1,_9],_144=new T(function(){return _CG(_15,_3E,_19,_142);}),_145=function(_146,_){var _147=A(_144,[_146,_]);return [0,[0,_vK,new T(function(){var _148=E(E(_147)[1]);return _148[0]==0?E(_143):E(_148);})],new T(function(){return E(E(_147)[2]);})];},_149=[0,_34,_5A],_14a=[1,_b],_14b=function(_14c,_14d){var _14e=new T(function(){return [0,E(_14c)[1]+1|0];});return function(_cs,_sg){return _4w(function(_Cj,_){return _4w(function(_14f,_){var _14g=_4w(_13Z,function(_14h){var _14i=_13w(E(_14c)[1],_14h);return _14i[0]==0?E(_12U):function(_14j,_){return [0,[0,_34,_14i],_14j];};},_14f,_),_14k=E(_14g),_14l=E(_14k[1]);return [0,[0,function(_14m,_){var _14n=A(_14l[1],[_14m,_]);return _14m;},new T(function(){var _14o=E(_14l[2]);return _14o[0]==0?E(_14a):[1,_14o];})],_14k[2]];},function(_14p){var _14q=new T(function(){return _tA(new T(function(){return A(_si,[_b,_9U,_14p]);}),_9T);});return function(_cs,_sg){return _4w(function(_14r,_){var _14s=A(_14q,[_14r,_]),_14t=E(_14s),_14u=_14t[2],_14v=E(_14t[1]),_14w=_14v[1],_14x=_14v[2],_14y=E(_14p);return _14y[0]==0?[0,[0,function(_14z,_){var _14A=A(_14w,[_14z,_]);return _14z;},_14x],_14u]:[0,[0,function(_14B,_){var _14C=A(_14w,[_14B,_]);return _14B;},new T(function(){var _14D=E(_14x);return _14D[0]==0?E(_14y):E(_14D);})],_14u];},function(_14E,_14F,_){return _4w(function(_Cj,_){return _4w(_145,function(_14G){var _14H=new T(function(){return _13k(_14c,_14E,_14G);}),_14I=new T(function(){return A(_142,[_14H]);});return function(_cs,_sg){return _4w(_Ca,function(_14J){return function(_14K,_){return [0,_149,new T(function(){var _14L=E(_14J);return [0,_14L[1],_14L[2],_14L[3],_14L[4],_14L[5],new T(function(){return _12W(_14I,_14H,_14L[6]);})];})];};},_cs,_sg);};},_Cj,_);},function(_14M,_Cj,_){return (function(_14N,_){return [0,[0,_34,[1,_14E]],_14N];})(_Cj,_);},_14F,_);},_cs,_sg);};},_Cj,_);},function(_14O){var _14P=new T(function(){return _14b(_14e,new T(function(){return _94(_14d,_14O);}));}),_14Q=new T(function(){return _5m(_52,new T(function(){return _3M(0,E(_14d)[1]+E(_14O)[1]|0,_a);}));});return function(_cs,_sg){return _4w(function(_14R,_){return [0,[0,function(_14S,_){var _14T=A(_14Q,[_14S,_]),_14U=_5w(_14S,_);return _14S;},_5A],_14R];},function(_14V){return E(_14P);},_cs,_sg);};},_cs,_sg);};},_14W=new T(function(){return _14b(_5L,_5L);}),_14X=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_14Y=new T(function(){return _59(_52,_14X);}),_14Z=new T(function(){return A(_si,[_b,_9U,_b]);}),_150=new T(function(){return _tA(_14Z,_9T);}),_151=function(_152){var _153=new T(function(){return _5m(_52,new T(function(){return _5i(_152);}));});return function(_cs,_sg){return _4w(_150,function(_154){var _155=E(E(_154)[1]);if(!_155){return function(_156,_){return [0,[0,function(_157,_){var _158=_5w(_157,_),_159=_52(_5B,_157,_),_15a=A(_153,[_157,_]);return _157;},_b],_156];};}else{var _15b=new T(function(){return _151(new T(function(){return [0,E(_152)[1]+_155|0];}));}),_15c=new T(function(){return _5m(_52,new T(function(){return _3M(0,E(_152)[1]+_155|0,_a);}));});return function(_cs,_sg){return _4w(function(_15d,_){return [0,[0,function(_15e,_){var _15f=A(_15c,[_15e,_]),_15g=_5w(_15e,_);return _15e;},_5A],_15d];},function(_15h){return E(_15b);},_cs,_sg);};}},_cs,_sg);};},_15i=new T(function(){return _151(_5L);}),_15j=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_15k=new T(function(){return _59(_52,_15j);}),_15l=function(_15m){return function(_15n,_){return [0,[0,new T(function(){var _15o=new T(function(){return _5m(_52,new T(function(){return _5i(_15m);}));});return _59(_vl,function(_15p,_){var _15q=_52(_5B,_15p,_),_15r=A(_15o,[_15p,_]);return _15p;});}),_5A],_15n];};},_15s=new T(function(){return A(_si,[_b,_9U,_b]);}),_15t=new T(function(){return _tA(_15s,_9T);}),_15u=unCStr("second number "),_15v=unCStr("first number"),_15w=new T(function(){return A(_si,[_b,_9U,_b]);}),_15x=new T(function(){return _tA(_15w,_9T);}),_15y=function(_15z,_){var _15A=A(_15t,[_15z,_]),_15B=E(_15A),_15C=E(_15B[1]),_15D=A(_15x,[_15B[2],_]),_15E=E(_15D),_15F=E(_15E[1]);return [0,[0,function(_15G,_){var _15H=_52(_15v,_15G,_),_15I=_5w(_15G,_),_15J=A(_15C[1],[_15G,_]),_15K=_5w(_15G,_),_15L=_52(_15u,_15G,_),_15M=_5w(_15G,_),_15N=A(_15F[1],[_15G,_]),_15O=_5w(_15G,_);return _15G;},new T(function(){var _15P=E(_15C[2]);if(!_15P[0]){return [0];}else{var _15Q=E(_15F[2]);return _15Q[0]==0?[0]:[1,new T(function(){return _94(_15P[1],_15Q[1]);})];}})],_15E[2]];},_15R=function(_15S,_){var _15T=_4w(_15y,_15l,_15S,_),_15U=E(_15T),_15V=E(_15U[1]),_15W=new T(function(){return _59(_vl,_15V[1]);});return [0,[0,function(_15X,_){var _15Y=A(_15k,[_15X,_]),_15Z=A(_15W,[_15X,_]);return _15X;},_15V[2]],_15U[2]];},_160=unCStr("table"),_161=function(_162,_163){var _164=new T(function(){return A(_162,[_163]);});return function(_165,_){var _166=jsCreateElem(toJSStr(E(_160))),_167=jsAppendChild(_166,E(_165)[1]),_168=[0,_166],_169=A(_164,[_168,_]);return _168;};},_16a=unCStr("hplayground examples"),_16b=new T(function(){return _Xl(_52,_16a);}),_16c=unCStr("td"),_16d=function(_16e,_16f){var _16g=new T(function(){return A(_16e,[_16f]);});return function(_16h,_){var _16i=jsCreateElem(toJSStr(E(_16c))),_16j=jsAppendChild(_16i,E(_16h)[1]),_16k=[0,_16i],_16l=A(_16g,[_16k,_]);return _16k;};},_16m=unCStr("tr"),_16n=function(_16o,_16p){var _16q=new T(function(){return A(_16o,[_16p]);});return function(_16r,_){var _16s=jsCreateElem(toJSStr(E(_16m))),_16t=jsAppendChild(_16s,E(_16r)[1]),_16u=[0,_16s],_16v=A(_16q,[_16u,_]);return _16u;};},_16w=unCStr("bottom of the page"),_16x=new T(function(){return _5m(_52,_16w);}),_16y=unCStr("h3"),_16z=function(_16A,_16B){var _16C=new T(function(){return A(_16A,[_16B]);});return function(_16D,_){var _16E=jsCreateElem(toJSStr(E(_16y))),_16F=jsAppendChild(_16E,E(_16D)[1]),_16G=[0,_16E],_16H=A(_16C,[_16G,_]);return _16G;};},_16I=unCStr("https://github.com/agocorona/hplayground"),_16J=unCStr("   "),_16K=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_16L=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_16M=unCStr("Git repository"),_16N=new T(function(){return _Sp(_52,_16M);}),_16O=unCStr("Examples code"),_16P=new T(function(){return _Sp(_52,_16O);}),_16Q=unCStr("Article"),_16R=new T(function(){return _Sp(_52,_16Q);}),_16S=function(_16T,_){var _16U=A(_16N,[_16T,_]),_16V=A(_B,[_H,_16U,_SA,_16I,_]),_16W=_52(_16J,_16T,_),_16X=A(_16P,[_16T,_]),_16Y=A(_B,[_H,_16X,_SA,_16K,_]),_16Z=_52(_16J,_16T,_),_170=A(_16R,[_16T,_]),_171=A(_B,[_H,_170,_SA,_16L,_]);return _16T;},_172=new T(function(){return _vB(_vl,_16S);}),_173=new T(function(){return _16z(_vl,_172);}),_174=function(_175,_){var _176=_15R(_175,_),_177=E(_176),_178=E(_177[1]),_179=A(_vz,[_177[2],_]),_17a=E(_179),_17b=E(_17a[1]),_17c=A(_15i,[_17a[2],_]),_17d=E(_17c),_17e=E(_17d[1]),_17f=A(_Di,[_17d[2],_]),_17g=E(_17f),_17h=E(_17g[1]),_17i=_Jf(_17g[2],_),_17j=E(_17i),_17k=E(_17j[1]),_17l=_4w(_VT,_UD,_17j[2],_),_17m=E(_17l),_17n=E(_17m[1]),_17o=A(_14W,[_17m[2],_]),_17p=E(_17o),_17q=E(_17p[1]),_17r=A(_DZ,[_17p[2],_]),_17s=E(_17r),_17t=E(_17s[1]),_17u=_SJ(_17s[2],_),_17v=E(_17u),_17w=E(_17v[1]),_17x=_12J(_17v[2],_),_17y=E(_17x),_17z=E(_17y[1]),_17A=_114(_17y[2],_),_17B=E(_17A),_17C=E(_17B[1]),_17D=_O7(_17B[2],_),_17E=E(_17D),_17F=E(_17E[1]),_17G=_TX(_17E[2],_),_17H=E(_17G),_17I=E(_17H[1]),_17J=_4w(_XN,_WZ,_17H[2],_),_17K=E(_17J),_17L=E(_17K[1]),_17M=new T(function(){return _161(_vl,function(_17N,_){var _17O=A(new T(function(){var _17P=new T(function(){return _16d(_vl,function(_17Q,_){var _17R=A(_14Y,[_17Q,_]),_17S=A(_17e[1],[_17Q,_]);return _17Q;});}),_17T=new T(function(){return _16d(_vl,_17b[1]);}),_17U=new T(function(){return _16d(_vl,_178[1]);});return _16n(_vl,function(_17V,_){var _17W=A(_17U,[_17V,_]),_17X=A(_B,[_H,_17W,_DE,_4Y,_]),_17Y=A(_17T,[_17V,_]),_17Z=A(_B,[_H,_17Y,_DE,_4Y,_]),_180=A(_17P,[_17V,_]),_181=A(_B,[_H,_180,_DE,_4Y,_]);return _17V;});}),[_17N,_]),_182=A(_B,[_H,_17O,_DE,_50,_]),_183=A(new T(function(){var _184=new T(function(){return _16d(_vl,_17t[1]);}),_185=new T(function(){return _16d(_vl,function(_186,_){var _187=A(_12S,[_186,_]),_188=A(_17q[1],[_186,_]);return _186;});}),_189=new T(function(){return _16d(_vl,function(_18a,_){var _18b=A(_17h[1],[_18a,_]),_18c=A(_17k[1],[_18a,_]),_18d=A(_17n[1],[_18a,_]);return _18a;});});return _16n(_vl,function(_18e,_){var _18f=A(_189,[_18e,_]),_18g=A(_B,[_H,_18f,_DE,_4Y,_]),_18h=A(_185,[_18e,_]),_18i=A(_B,[_H,_18h,_DE,_4Y,_]),_18j=A(_184,[_18e,_]),_18k=A(_B,[_H,_18j,_DE,_4Y,_]);return _18e;});}),[_17N,_]),_18l=A(_B,[_H,_183,_DE,_50,_]),_18m=A(new T(function(){var _18n=new T(function(){return _16d(_vl,function(_18o,_){var _18p=A(_XX,[_18o,_]),_18q=A(_17C[1],[_18o,_]);return _18o;});}),_18r=new T(function(){return _16d(_vl,_17z[1]);}),_18s=new T(function(){return _16d(_vl,new T(function(){return _vB(_vl,_17w[1]);}));});return _16n(_vl,function(_18t,_){var _18u=A(_18s,[_18t,_]),_18v=A(_B,[_H,_18u,_DE,_4Y,_]),_18w=A(_18r,[_18t,_]),_18x=A(_B,[_H,_18w,_DE,_4Y,_]),_18y=A(_18n,[_18t,_]),_18z=A(_B,[_H,_18y,_DE,_4Y,_]);return _18t;});}),[_17N,_]),_18A=A(_B,[_H,_18m,_DE,_50,_]),_18B=A(new T(function(){var _18C=new T(function(){return _16d(_vl,_17L[1]);}),_18D=new T(function(){return _16d(_vl,_17I[1]);}),_18E=new T(function(){return _16d(_vl,_17F[1]);});return _16n(_vl,function(_18F,_){var _18G=A(_18E,[_18F,_]),_18H=A(_B,[_H,_18G,_DE,_4Y,_]),_18I=A(_18D,[_18F,_]),_18J=A(_B,[_H,_18I,_DE,_4Y,_]),_18K=A(_18C,[_18F,_]),_18L=A(_B,[_H,_18K,_DE,_4Y,_]);return _18F;});}),[_17N,_]),_18M=A(_B,[_H,_18B,_DE,_50,_]);return _17N;});});return [0,[0,function(_18N,_){var _18O=A(_16b,[_18N,_]),_18P=A(_B,[_H,_18O,_DE,_DD,_]),_18Q=A(_173,[_18N,_]),_18R=A(_17M,[_18N,_]),_18S=A(_B,[_H,_18R,_DE,_4Z,_]),_18T=A(_16x,[_18N,_]);return _18N;},new T(function(){var _18U=E(_178[2]);if(!_18U[0]){var _18V=E(_17b[2]);if(!_18V[0]){var _18W=E(_17e[2]);if(!_18W[0]){var _18X=E(_17h[2]);if(!_18X[0]){var _18Y=E(_17k[2]);if(!_18Y[0]){var _18Z=E(_17n[2]);if(!_18Z[0]){var _190=E(_17q[2]);if(!_190[0]){var _191=E(_17t[2]);if(!_191[0]){var _192=E(_17w[2]);if(!_192[0]){var _193=E(_17z[2]);if(!_193[0]){var _194=E(_17C[2]);if(!_194[0]){var _195=E(_17F[2]);if(!_195[0]){var _196=E(_17I[2]);return _196[0]==0?E(_17L[2]):E(_196);}else{return E(_195);}}else{return E(_194);}}else{return E(_193);}}else{return E(_192);}}else{return E(_191);}}else{return E(_190);}}else{return E(_18Z);}}else{return E(_18Y);}}else{return E(_18X);}}else{return E(_18W);}}else{return E(_18V);}}else{return E(_18U);}})],_17K[2]];},_197=unCStr("idelem"),_198=function(_){var _199=E(_197),_19a=jsFind(toJSStr(_199)),_19b=E(_19a);return _19b[0]==0?_45(_199):_l(_174,_19b[1],_);},_19c=function(_){return _198(_);};
var hasteMain = function() {A(_19c, [0]);};window.onload = hasteMain;