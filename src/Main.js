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

var _0=unCStr("id"),_1=0,_2=function(_3,_4,_5,_6){return A(_3,[new T(function(){return function(_){var _7=jsSetAttr(E(_4)[1],toJSStr(E(_5)),toJSStr(E(_6)));return _1;};})]);},_8=function(_9){return E(_9);},_a=function(_b,_c,_d,_){var _e=E(_c),_f=A(_b,[_d,_]),_g=A(_2,[_8,_f,_e[1],_e[2],_]);return _f;},_h=function(_i,_j){while(1){var _k=(function(_l,_m){var _n=E(_m);if(!_n[0]){return E(_l);}else{_i=function(_o,_){return _a(_l,_n[1],_o,_);};_j=_n[2];return null;}})(_i,_j);if(_k!=null){return _k;}}},_p=unCStr("span"),_q=function(_r,_s,_){var _t=jsCreateElem(toJSStr(E(_r))),_u=jsAppendChild(_t,E(_s)[1]);return [0,_t];},_v=function(_o,_){return _q(_p,_o,_);},_w=function(_x,_y,_){return [0,_1,_x];},_z=function(_A,_){return [0,_A,_A];},_B=[0,coercionToken],_C=function(_D,_E,_){var _F=A(_D,[_]);return A(_E,[_]);},_G=function(_H,_I,_){return _C(_H,_I,_);},_J=function(_K,_L,_){var _M=A(_K,[_]);return A(_L,[_M,_]);},_N=unCStr("base"),_O=unCStr("GHC.IO.Exception"),_P=unCStr("IOException"),_Q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_N,_O,_P],_R=[0],_S=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_Q,_R],_T=function(_U){return E(_S);},_V=function(_W){return E(E(_W)[1]);},_X=unCStr("Maybe.fromJust: Nothing"),_Y=new T(function(){return err(_X);}),_Z=function(_10,_11,_12){var _13=new T(function(){var _14=A(_10,[_12]),_15=A(_11,[new T(function(){var _16=E(_13);return _16[0]==0?E(_Y):E(_16[1]);})]),_17=hs_eqWord64(_14[1],_15[1]);if(!E(_17)){return [0];}else{var _18=hs_eqWord64(_14[2],_15[2]);return E(_18)==0?[0]:[1,_12];}});return E(_13);},_19=function(_1a){var _1b=E(_1a);return _Z(_V(_1b[1]),_T,_1b[2]);},_1c=unCStr(": "),_1d=[0,41],_1e=unCStr(" ("),_1f=function(_1g,_1h){var _1i=E(_1g);return _1i[0]==0?E(_1h):[1,_1i[1],new T(function(){return _1f(_1i[2],_1h);})];},_1j=unCStr("already exists"),_1k=unCStr("does not exist"),_1l=unCStr("protocol error"),_1m=unCStr("failed"),_1n=unCStr("invalid argument"),_1o=unCStr("inappropriate type"),_1p=unCStr("hardware fault"),_1q=unCStr("unsupported operation"),_1r=unCStr("timeout"),_1s=unCStr("resource vanished"),_1t=unCStr("interrupted"),_1u=unCStr("resource busy"),_1v=unCStr("resource exhausted"),_1w=unCStr("end of file"),_1x=unCStr("illegal operation"),_1y=unCStr("permission denied"),_1z=unCStr("user error"),_1A=unCStr("unsatisified constraints"),_1B=unCStr("system error"),_1C=function(_1D,_1E){switch(E(_1D)){case 0:return _1f(_1j,_1E);case 1:return _1f(_1k,_1E);case 2:return _1f(_1u,_1E);case 3:return _1f(_1v,_1E);case 4:return _1f(_1w,_1E);case 5:return _1f(_1x,_1E);case 6:return _1f(_1y,_1E);case 7:return _1f(_1z,_1E);case 8:return _1f(_1A,_1E);case 9:return _1f(_1B,_1E);case 10:return _1f(_1l,_1E);case 11:return _1f(_1m,_1E);case 12:return _1f(_1n,_1E);case 13:return _1f(_1o,_1E);case 14:return _1f(_1p,_1E);case 15:return _1f(_1q,_1E);case 16:return _1f(_1r,_1E);case 17:return _1f(_1s,_1E);default:return _1f(_1t,_1E);}},_1F=[0,125],_1G=unCStr("{handle: "),_1H=function(_1I,_1J,_1K,_1L,_1M,_1N){var _1O=new T(function(){var _1P=new T(function(){return _1C(_1J,new T(function(){var _1Q=E(_1L);return _1Q[0]==0?E(_1N):_1f(_1e,new T(function(){return _1f(_1Q,[1,_1d,_1N]);}));}));}),_1R=E(_1K);return _1R[0]==0?E(_1P):_1f(_1R,new T(function(){return _1f(_1c,_1P);}));}),_1S=E(_1M);if(!_1S[0]){var _1T=E(_1I);if(!_1T[0]){return E(_1O);}else{var _1U=E(_1T[1]);return _1U[0]==0?_1f(_1G,new T(function(){return _1f(_1U[1],[1,_1F,new T(function(){return _1f(_1c,_1O);})]);})):_1f(_1G,new T(function(){return _1f(_1U[1],[1,_1F,new T(function(){return _1f(_1c,_1O);})]);}));}}else{return _1f(_1S[1],new T(function(){return _1f(_1c,_1O);}));}},_1V=function(_1W){var _1X=E(_1W);return _1H(_1X[1],_1X[2],_1X[3],_1X[4],_1X[6],_R);},_1Y=function(_1Z,_20){var _21=E(_1Z);return _1H(_21[1],_21[2],_21[3],_21[4],_21[6],_20);},_22=[0,44],_23=[0,93],_24=[0,91],_25=function(_26,_27,_28){var _29=E(_27);return _29[0]==0?unAppCStr("[]",_28):[1,_24,new T(function(){return A(_26,[_29[1],new T(function(){var _2a=function(_2b){var _2c=E(_2b);return _2c[0]==0?E([1,_23,_28]):[1,_22,new T(function(){return A(_26,[_2c[1],new T(function(){return _2a(_2c[2]);})]);})];};return _2a(_29[2]);})]);})];},_2d=function(_2e,_2f){return _25(_1Y,_2e,_2f);},_2g=function(_2h,_2i,_2j){var _2k=E(_2i);return _1H(_2k[1],_2k[2],_2k[3],_2k[4],_2k[6],_2j);},_2l=[0,_2g,_1V,_2d],_2m=new T(function(){return [0,_T,_2l,_2n,_19];}),_2n=function(_2o){return [0,_2m,_2o];},_2p=[0],_2q=7,_2r=function(_2s){return [0,_2p,_2q,_R,_2s,_2p,_2p];},_2t=function(_2u,_){return die(new T(function(){return _2n(new T(function(){return _2r(_2u);}));}));},_2v=function(_2w,_){return _2t(_2w,_);},_2x=function(_2y,_){return _2y;},_2z=[0,_J,_G,_2x,_2v],_2A=function(_2B){return E(E(_2B)[1]);},_2C=function(_2D,_2E,_2F,_2G){return A(_2A,[_2D,new T(function(){return A(_2E,[_2G]);}),function(_2H){return A(_2F,[new T(function(){return E(E(_2H)[1]);}),new T(function(){return E(E(_2H)[2]);})]);}]);},_2I=function(_2J,_2K,_2L,_2M){return A(_2A,[_2J,new T(function(){return A(_2K,[_2M]);}),function(_2N){return A(_2L,[new T(function(){return E(E(_2N)[2]);})]);}]);},_2O=function(_2P,_2Q,_2R,_2S){return _2I(_2P,_2Q,_2R,_2S);},_2T=function(_2U){return E(E(_2U)[4]);},_2V=function(_2W,_2X){var _2Y=new T(function(){return A(_2T,[_2W,_2X]);});return function(_2Z){return E(_2Y);};},_30=function(_31){return E(E(_31)[3]);},_32=function(_33){var _34=new T(function(){return _30(_33);});return [0,function(_2Q,_2R,_2S){return _2C(_33,_2Q,_2R,_2S);},function(_2Q,_2R,_2S){return _2O(_33,_2Q,_2R,_2S);},function(_35,_36){return A(_34,[[0,_35,_36]]);},function(_2S){return _2V(_33,_2S);}];},_37=new T(function(){return _32(_2z);}),_38=[0,112],_39=function(_3a,_3b){var _3c=jsShowI(_3a);return _1f(fromJSStr(_3c),_3b);},_3d=[0,41],_3e=[0,40],_3f=function(_3g,_3h,_3i){return _3h>=0?_39(_3h,_3i):_3g<=6?_39(_3h,_3i):[1,_3e,new T(function(){var _3j=jsShowI(_3h);return _1f(fromJSStr(_3j),[1,_3d,_3i]);})];},_3k=function(_3l,_3m,_3n,_3o){var _3p=E(_3m);return A(_3p[1],[new T(function(){var _3q=E(_3l);return E(_3n);}),function(_3r){var _3s=new T(function(){return E(E(_3r)[2]);});return A(_3p[2],[new T(function(){return A(_3o,[new T(function(){var _3t=E(new T(function(){var _3u=E(_3l);return [0,coercionToken];})),_3v=E(_3r);return [0,_3v[1],new T(function(){return [0,E(_3s)[1]+1|0];}),_3v[3],_3v[4],_3v[5]];})]);}),new T(function(){return A(_3p[3],[[1,_38,new T(function(){return _1f(_3f(0,E(_3s)[1],_R),new T(function(){return E(E(_3r)[1]);}));})]]);})]);}]);},_3w=new T(function(){return _3k(_B,_37,_z,_w);}),_3x=2,_3y=[1],_3z=function(_3A,_){return _2p;},_3B=function(_){return _2p;},_3C=[0,_3B,_3z],_3D=[0,0],_3E=[0,_R,_3D,_3x,_3C,_3y],_3F=function(_){var _=0,_3G=newMVar(),_=putMVar(_3G,_3E);return [0,_3G];},_3H=function(_3I){var _3J=A(_3I,[_]);return E(_3J);},_3K=new T(function(){return _3H(_3F);}),_3L=unCStr(" could be found!"),_3M=function(_3N){return err(unAppCStr("No element with ID ",new T(function(){return _1f(_3N,_3L);})));},_3O=function(_3P,_3Q,_){var _3R=E(_3K)[1],_3S=takeMVar(_3R),_3T=E(_3Q),_3U=jsFind(toJSStr(_3T)),_3V=E(_3U);if(!_3V[0]){return _3M(_3T);}else{var _3W=E(_3V[1]),_3X=jsClearChildren(_3W[1]),_3Y=A(_3P,[_3S,_]),_3Z=E(_3Y),_40=E(_3Z[1]),_=putMVar(_3R,_3Z[2]),_41=A(_40[1],[_3W,_]);return _40[2];}},_42=function(_43,_44,_45,_46,_47,_48,_49,_4a,_){var _4b=E(_49);return [0,_4b,[0,_46,_47,_48,[0,function(_){return _3O(function(_4c,_){var _4d=A(_43,[new T(function(){var _4e=E(_4c);return [0,_4e[1],_47,_4e[3],_4e[4],_4e[5]];}),_]);return [0,[0,_2x,E(E(_4d)[1])[2]],_4c];},_45,_);},function(_4f,_){var _4g=_3O(new T(function(){return A(_44,[_4f]);}),_45,_),_4h=E(_4g);return _4h[0]==0?_2p:A(_4b[2],[_4h[1],_]);}],_4a]];},_4i=function(_4j,_4k,_4l,_){var _4m=A(_3w,[_4l,_]),_4n=E(_4m),_4o=_4n[1],_4p=E(_4n[2]),_4q=_42(_4j,_4k,_4o,_4p[1],_4p[2],_4p[3],_4p[4],_4p[5],_),_4r=A(_4j,[new T(function(){return E(E(_4q)[2]);}),_]),_4s=E(_4r),_4t=_4s[2],_4u=E(_4s[1]),_4v=_4u[1],_4w=new T(function(){return _h(_v,[1,[0,_0,_4o],_R]);}),_4x=E(_4u[2]);if(!_4x[0]){return [0,[0,function(_4y,_){var _4z=A(_4v,[_4y,_]),_4A=A(_4w,[_4y,_]);return _4y;},_2p],new T(function(){var _4B=E(_4t);return [0,_4B[1],_4B[2],_4B[3],new T(function(){return E(E(_4q)[1]);}),_4B[5]];})];}else{var _4C=A(_4k,[_4x[1],new T(function(){var _4D=E(_4t);return [0,_4D[1],_4D[2],_4D[3],new T(function(){return E(E(_4q)[1]);}),_4D[5]];}),_]),_4E=E(_4C),_4F=E(_4E[1]);return [0,[0,function(_4G,_){var _4H=A(_4v,[_4G,_]),_4I=A(_4w,[_4G,_]),_4J=A(_4F[1],[_4I,_]);return _4G;},_4F[2]],_4E[2]];}},_4K=function(_4L,_4M,_){var _4N=jsCreateTextNode(toJSStr(E(_4L))),_4O=jsAppendChild(_4N,E(_4M)[1]);return [0,_4N];},_4P=unCStr("This widget sums two numbers and append the result. Using applicative and monadic expressions"),_4Q=[0,112],_4R=[1,_4Q,_R],_4S=function(_4T,_4U){var _4V=new T(function(){return A(_4T,[_4U]);});return function(_4W,_){var _4X=jsCreateElem(toJSStr(_4R)),_4Y=jsAppendChild(_4X,E(_4W)[1]),_4Z=[0,_4X],_50=A(_4V,[_4Z,_]);return _4Z;};},_51=new T(function(){return _4S(_4K,_4P);}),_52=function(_53){return _3f(0,E(_53)[1],_R);},_54=[0,98],_55=[1,_54,_R],_56=function(_57,_58){var _59=new T(function(){return A(_57,[_58]);});return function(_5a,_){var _5b=jsCreateElem(toJSStr(_55)),_5c=jsAppendChild(_5b,E(_5a)[1]),_5d=[0,_5b],_5e=A(_59,[_5d,_]);return _5d;};},_5f=[1,_1],_5g=unCStr("result: "),_5h=function(_5i){return E(_5i);},_5j=function(_5k){return function(_5l,_){return [0,[0,new T(function(){var _5m=new T(function(){return _56(_4K,new T(function(){return _52(_5k);}));});return _4S(_5h,function(_5n,_){var _5o=_4K(_5g,_5n,_),_5p=A(_5m,[_5n,_]);return _5n;});}),_5f],_5l];};},_5q=[13,coercionToken],_5r=function(_5s,_5t){return [0,E(_5s)[1]+E(_5t)[1]|0];},_5u=unCStr("true"),_5v=unCStr("hasevent"),_5w=function(_5x,_5y){while(1){var _5z=E(_5x);if(!_5z[0]){return E(_5y)[0]==0?true:false;}else{var _5A=E(_5y);if(!_5A[0]){return false;}else{if(E(_5z[1])[1]!=E(_5A[1])[1]){return false;}else{_5x=_5z[2];_5y=_5A[2];continue;}}}}},_5B=new T(function(){return [0,"keydown"];}),_5C=new T(function(){return [0,"mousemove"];}),_5D=new T(function(){return [0,"blur"];}),_5E=new T(function(){return [0,"focus"];}),_5F=new T(function(){return [0,"change"];}),_5G=new T(function(){return [0,"unload"];}),_5H=new T(function(){return [0,"load"];}),_5I=new T(function(){return [0,"keyup"];}),_5J=new T(function(){return [0,"keypress"];}),_5K=new T(function(){return [0,"mouseup"];}),_5L=new T(function(){return [0,"mousedown"];}),_5M=new T(function(){return [0,"dblclick"];}),_5N=new T(function(){return [0,"click"];}),_5O=new T(function(){return [0,"mouseout"];}),_5P=new T(function(){return [0,"mouseover"];}),_5Q=function(_5R){switch(E(_5R)[0]){case 0:return E(_5H);case 1:return E(_5G);case 2:return E(_5F);case 3:return E(_5E);case 4:return E(_5D);case 5:return E(_5C);case 6:return E(_5P);case 7:return E(_5O);case 8:return E(_5N);case 9:return E(_5M);case 10:return E(_5L);case 11:return E(_5K);case 12:return E(_5J);case 13:return E(_5I);default:return E(_5B);}},_5S=function(_5T,_5U,_5V,_5W,_){var _5X=A(_5T,[_5W,_]),_5Y=E(_5X),_5Z=_5Y[1],_60=E(_5v),_61=jsGetAttr(_5Z,toJSStr(_60));if(!_5w(fromJSStr(_61),_5u)){var _62=E(_5V),_63=jsSetCB(_5Z,_5Q(_5U)[1],_5V),_64=A(_2,[_8,_5Y,_60,_5u,_]);return _5Y;}else{return _5Y;}},_65=unCStr("br"),_66=function(_67,_){var _68=jsCreateElem(toJSStr(E(_65))),_69=jsAppendChild(_68,E(_67)[1]);return [0,_68];},_6a=unCStr("first number"),_6b=unCStr("text"),_6c=function(_6d,_6e,_6f,_){var _6g=_q(_6d,_6f,_),_6h=A(_6e,[_6g,_]);return _6g;},_6i=unCStr("()"),_6j=unCStr("GHC.Tuple"),_6k=unCStr("ghc-prim"),_6l=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_6k,_6j,_6i],_6m=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_6l,_R],_6n=function(_6o){return E(_6m);},_6p=unCStr("main"),_6q=unCStr("Haste.Perch"),_6r=unCStr("PerchM"),_6s=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_6p,_6q,_6r],_6t=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_6s,_R],_6u=function(_6v){return E(_6t);},_6w=function(_6x){var _6y=E(_6x);return _6y[0]==0?[0]:_1f(_6y[1],new T(function(){return _6w(_6y[2]);}));},_6z=function(_6A,_6B){var _6C=E(_6A);if(!_6C){return [0,_R,_6B];}else{var _6D=E(_6B);if(!_6D[0]){return [0,_R,_R];}else{var _6E=new T(function(){var _6F=_6z(_6C-1|0,_6D[2]);return [0,_6F[1],_6F[2]];});return [0,[1,_6D[1],new T(function(){return E(E(_6E)[1]);})],new T(function(){return E(E(_6E)[2]);})];}}},_6G=[0,120],_6H=[0,48],_6I=function(_6J){var _6K=new T(function(){var _6L=_6z(8,new T(function(){var _6M=md5(toJSStr(E(_6J)));return fromJSStr(_6M);}));return [0,_6L[1],_6L[2]];}),_6N=parseInt([0,toJSStr([1,_6H,[1,_6G,new T(function(){return E(E(_6K)[1]);})]])]),_6O=new T(function(){var _6P=_6z(8,new T(function(){return E(E(_6K)[2]);}));return [0,_6P[1],_6P[2]];}),_6Q=parseInt([0,toJSStr([1,_6H,[1,_6G,new T(function(){return E(E(_6O)[1]);})]])]),_6R=hs_mkWord64(_6N,_6Q),_6S=parseInt([0,toJSStr([1,_6H,[1,_6G,new T(function(){return E(_6z(8,new T(function(){return E(E(_6O)[2]);}))[1]);})]])]),_6T=hs_mkWord64(_6S,_6S);return [0,_6R,_6T];},_6U=function(_6V,_6W){var _6X=E(_6W);return _6X[0]==0?[0]:[1,new T(function(){return A(_6V,[_6X[1]]);}),new T(function(){return _6U(_6V,_6X[2]);})];},_6Y=function(_6Z,_70){var _71=jsShowI(_6Z),_72=md5(_71);return _1f(fromJSStr(_72),new T(function(){var _73=jsShowI(_70),_74=md5(_73);return fromJSStr(_74);}));},_75=function(_76){var _77=E(_76);return _6Y(_77[1],_77[2]);},_78=function(_79){var _7a=E(_79);if(!_7a[0]){return [0];}else{var _7b=E(_7a[1]);return [1,[0,_7b[1],_7b[2]],new T(function(){return _78(_7a[2]);})];}},_7c=unCStr("Prelude.undefined"),_7d=new T(function(){return err(_7c);}),_7e=function(_7f,_7g){return function(_7h){return E(new T(function(){var _7i=A(_7f,[_7d]),_7j=E(_7i[3]),_7k=_7j[1],_7l=_7j[2],_7m=_1f(_7i[4],[1,new T(function(){return A(_7g,[_7d]);}),_R]);if(!_7m[0]){return [0,_7k,_7l,_7j,_R];}else{var _7n=_6I(new T(function(){return _6w(_6U(_75,[1,[0,_7k,_7l],new T(function(){return _78(_7m);})]));}));return [0,_7n[1],_7n[2],_7j,_7m];}}));};},_7o=new T(function(){return _7e(_6u,_6n);}),_7p=unCStr("value"),_7q=unCStr("onclick"),_7r=unCStr("checked"),_7s=[0,_7r,_R],_7t=[1,_7s,_R],_7u=unCStr("type"),_7v=unCStr("input"),_7w=function(_7x,_){return _q(_7v,_7x,_);},_7y=function(_7z,_7A,_7B,_7C,_7D){var _7E=new T(function(){var _7F=new T(function(){return _h(_7w,[1,[0,_7u,_7A],[1,[0,_0,_7z],[1,[0,_7p,_7B],_R]]]);});return !E(_7C)?E(_7F):_h(_7F,_7t);}),_7G=E(_7D);return _7G[0]==0?E(_7E):_h(_7E,[1,[0,_7q,_7G[1]],_R]);},_7H=unCStr("href"),_7I=[0,97],_7J=[1,_7I,_R],_7K=function(_7L,_){return _q(_7J,_7L,_);},_7M=function(_7N,_7O){var _7P=new T(function(){return _h(_7K,[1,[0,_7H,_7N],_R]);});return function(_7Q,_){var _7R=A(_7P,[_7Q,_]),_7S=A(_7O,[_7R,_]);return _7R;};},_7T=function(_7U){return _7M(_7U,function(_o,_){return _4K(_7U,_o,_);});},_7V=unCStr("option"),_7W=function(_7X,_){return _q(_7V,_7X,_);},_7Y=unCStr("selected"),_7Z=[0,_7Y,_R],_80=[1,_7Z,_R],_81=function(_82,_83,_84){var _85=new T(function(){return _h(_7W,[1,[0,_7p,_82],_R]);}),_86=function(_87,_){var _88=A(_85,[_87,_]),_89=A(_83,[_88,_]);return _88;};return !E(_84)?E(_86):_h(_86,_80);},_8a=function(_8b,_8c){return _81(_8b,function(_o,_){return _4K(_8b,_o,_);},_8c);},_8d=unCStr("method"),_8e=unCStr("action"),_8f=unCStr("UTF-8"),_8g=unCStr("acceptCharset"),_8h=[0,_8g,_8f],_8i=unCStr("form"),_8j=function(_8k,_){return _q(_8i,_8k,_);},_8l=function(_8m,_8n,_8o){var _8p=new T(function(){return _h(_8j,[1,_8h,[1,[0,_8e,_8m],[1,[0,_8d,_8n],_R]]]);});return function(_8q,_){var _8r=A(_8p,[_8q,_]),_8s=A(_8o,[_8r,_]);return _8r;};},_8t=unCStr("select"),_8u=function(_8v,_){return _q(_8t,_8v,_);},_8w=function(_8x,_8y){var _8z=new T(function(){return _h(_8u,[1,[0,_0,_8x],_R]);});return function(_8A,_){var _8B=A(_8z,[_8A,_]),_8C=A(_8y,[_8B,_]);return _8B;};},_8D=unCStr("textarea"),_8E=function(_8F,_){return _q(_8D,_8F,_);},_8G=function(_8H,_8I){var _8J=new T(function(){return _h(_8E,[1,[0,_0,_8H],_R]);});return function(_8K,_){var _8L=A(_8J,[_8K,_]),_8M=_4K(_8I,_8L,_);return _8L;};},_8N=unCStr("color:red"),_8O=unCStr("style"),_8P=[0,_8O,_8N],_8Q=[1,_8P,_R],_8R=[0,98],_8S=[1,_8R,_R],_8T=function(_8U){return _h(function(_8V,_){var _8W=_q(_8S,_8V,_),_8X=A(_8U,[_8W,_]);return _8W;},_8Q);},_8Y=unCStr("toByteString not defined"),_8Z=new T(function(){return err(_8Y);}),_90=function(_91,_92,_){var _93=E(_91);if(!_93[0]){return _92;}else{var _94=A(_93[1],[_92,_]),_95=_90(_93[2],_92,_);return _92;}},_96=function(_97,_98,_99,_){var _9a=A(_97,[_99,_]),_9b=A(_98,[_99,_]);return _99;},_9c=[0,_2x,_96,_90],_9d=[0,_9c,_7o,_8Z,_4K,_4K,_6c,_8T,_7M,_7T,_7y,_8G,_8w,_81,_8a,_8l,_h],_9e=[0,_2z,_8],_9f=unCStr("base"),_9g=unCStr("Control.Exception.Base"),_9h=unCStr("PatternMatchFail"),_9i=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9f,_9g,_9h],_9j=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9i,_R],_9k=function(_9l){return E(_9j);},_9m=function(_9n){var _9o=E(_9n);return _Z(_V(_9o[1]),_9k,_9o[2]);},_9p=function(_9q){return E(E(_9q)[1]);},_9r=function(_9s,_9t){return _1f(E(_9s)[1],_9t);},_9u=function(_9v,_9w){return _25(_9r,_9v,_9w);},_9x=function(_9y,_9z,_9A){return _1f(E(_9z)[1],_9A);},_9B=[0,_9x,_9p,_9u],_9C=new T(function(){return [0,_9k,_9B,_9D,_9m];}),_9D=function(_9E){return [0,_9C,_9E];},_9F=unCStr("Non-exhaustive patterns in"),_9G=function(_9H,_9I){return die(new T(function(){return A(_9I,[_9H]);}));},_9J=function(_9K,_9L){var _9M=E(_9L);if(!_9M[0]){return [0,_R,_R];}else{var _9N=_9M[1];if(!A(_9K,[_9N])){return [0,_R,_9M];}else{var _9O=new T(function(){var _9P=_9J(_9K,_9M[2]);return [0,_9P[1],_9P[2]];});return [0,[1,_9N,new T(function(){return E(E(_9O)[1]);})],new T(function(){return E(E(_9O)[2]);})];}}},_9Q=[0,32],_9R=[0,10],_9S=[1,_9R,_R],_9T=function(_9U){return E(E(_9U)[1])==124?false:true;},_9V=function(_9W,_9X){var _9Y=_9J(_9T,unCStr(_9W)),_9Z=_9Y[1],_a0=function(_a1,_a2){return _1f(_a1,new T(function(){return unAppCStr(": ",new T(function(){return _1f(_9X,new T(function(){return _1f(_a2,_9S);}));}));}));},_a3=E(_9Y[2]);return _a3[0]==0?_a0(_9Z,_R):E(E(_a3[1])[1])==124?_a0(_9Z,[1,_9Q,_a3[2]]):_a0(_9Z,_R);},_a4=function(_a5){return _9G([0,new T(function(){return _9V(_a5,_9F);})],_9D);},_a6=new T(function(){return _a4("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_a7=function(_a8,_a9){while(1){var _aa=(function(_ab,_ac){var _ad=E(_ab);switch(_ad[0]){case 0:var _ae=E(_ac);if(!_ae[0]){return [0];}else{_a8=A(_ad[1],[_ae[1]]);_a9=_ae[2];return null;}break;case 1:var _af=A(_ad[1],[_ac]),_ag=_ac;_a8=_af;_a9=_ag;return null;case 2:return [0];case 3:return [1,[0,_ad[1],_ac],new T(function(){return _a7(_ad[2],_ac);})];default:return E(_ad[1]);}})(_a8,_a9);if(_aa!=null){return _aa;}}},_ah=function(_ai,_aj){var _ak=new T(function(){var _al=E(_aj);if(_al[0]==3){return [3,_al[1],new T(function(){return _ah(_ai,_al[2]);})];}else{var _am=E(_ai);if(_am[0]==2){return E(_al);}else{var _an=E(_al);if(_an[0]==2){return E(_am);}else{var _ao=new T(function(){var _ap=E(_an);if(_ap[0]==4){return [1,function(_aq){return [4,new T(function(){return _1f(_a7(_am,_aq),_ap[1]);})];}];}else{var _ar=E(_am);if(_ar[0]==1){var _as=_ar[1],_at=E(_ap);return _at[0]==0?[1,function(_au){return _ah(A(_as,[_au]),_at);}]:[1,function(_av){return _ah(A(_as,[_av]),new T(function(){return A(_at[1],[_av]);}));}];}else{var _aw=E(_ap);return _aw[0]==0?E(_a6):[1,function(_ax){return _ah(_ar,new T(function(){return A(_aw[1],[_ax]);}));}];}}}),_ay=E(_am);switch(_ay[0]){case 1:var _az=E(_an);return _az[0]==4?[1,function(_aA){return [4,new T(function(){return _1f(_a7(A(_ay[1],[_aA]),_aA),_az[1]);})];}]:E(_ao);case 4:var _aB=_ay[1],_aC=E(_an);switch(_aC[0]){case 0:return [1,function(_aD){return [4,new T(function(){return _1f(_aB,new T(function(){return _a7(_aC,_aD);}));})];}];case 1:return [1,function(_aE){return [4,new T(function(){return _1f(_aB,new T(function(){return _a7(A(_aC[1],[_aE]),_aE);}));})];}];default:return [4,new T(function(){return _1f(_aB,_aC[1]);})];}break;default:return E(_ao);}}}}}),_aF=E(_ai);switch(_aF[0]){case 0:var _aG=E(_aj);return _aG[0]==0?[0,function(_aH){return _ah(A(_aF[1],[_aH]),new T(function(){return A(_aG[1],[_aH]);}));}]:E(_ak);case 3:return [3,_aF[1],new T(function(){return _ah(_aF[2],_aj);})];default:return E(_ak);}},_aI=function(_aJ,_aK){return E(_aJ)[1]!=E(_aK)[1];},_aL=function(_aM,_aN){return E(_aM)[1]==E(_aN)[1];},_aO=[0,_aL,_aI],_aP=function(_aQ){return E(E(_aQ)[1]);},_aR=function(_aS,_aT,_aU){while(1){var _aV=E(_aT);if(!_aV[0]){return E(_aU)[0]==0?true:false;}else{var _aW=E(_aU);if(!_aW[0]){return false;}else{if(!A(_aP,[_aS,_aV[1],_aW[1]])){return false;}else{_aT=_aV[2];_aU=_aW[2];continue;}}}}},_aX=function(_aY,_aZ,_b0){return !_aR(_aY,_aZ,_b0)?true:false;},_b1=function(_b2){return [0,function(_b3,_b4){return _aR(_b2,_b3,_b4);},function(_b3,_b4){return _aX(_b2,_b3,_b4);}];},_b5=new T(function(){return _b1(_aO);}),_b6=function(_b7,_b8){var _b9=E(_b7);switch(_b9[0]){case 0:return [0,function(_ba){return _b6(A(_b9[1],[_ba]),_b8);}];case 1:return [1,function(_bb){return _b6(A(_b9[1],[_bb]),_b8);}];case 2:return [2];case 3:return _ah(A(_b8,[_b9[1]]),new T(function(){return _b6(_b9[2],_b8);}));default:var _bc=function(_bd){var _be=E(_bd);if(!_be[0]){return [0];}else{var _bf=E(_be[1]);return _1f(_a7(A(_b8,[_bf[1]]),_bf[2]),new T(function(){return _bc(_be[2]);}));}},_bg=_bc(_b9[1]);return _bg[0]==0?[2]:[4,_bg];}},_bh=[2],_bi=function(_bj){return [3,_bj,_bh];},_bk=function(_bl,_bm){var _bn=E(_bl);if(!_bn){return A(_bm,[_1]);}else{var _bo=new T(function(){return _bk(_bn-1|0,_bm);});return [0,function(_bp){return E(_bo);}];}},_bq=function(_br,_bs,_bt){var _bu=new T(function(){return A(_br,[_bi]);});return [1,function(_bv){return A(function(_bw,_bx,_by){while(1){var _bz=(function(_bA,_bB,_bC){var _bD=E(_bA);switch(_bD[0]){case 0:var _bE=E(_bB);if(!_bE[0]){return E(_bs);}else{_bw=A(_bD[1],[_bE[1]]);_bx=_bE[2];var _bF=_bC+1|0;_by=_bF;return null;}break;case 1:var _bG=A(_bD[1],[_bB]),_bH=_bB,_bF=_bC;_bw=_bG;_bx=_bH;_by=_bF;return null;case 2:return E(_bs);case 3:return function(_bI){var _bJ=new T(function(){return _b6(_bD,_bI);});return _bk(_bC,function(_bK){return E(_bJ);});};default:return function(_bL){return _b6(_bD,_bL);};}})(_bw,_bx,_by);if(_bz!=null){return _bz;}}},[_bu,_bv,0,_bt]);}];},_bM=[6],_bN=unCStr("valDig: Bad base"),_bO=new T(function(){return err(_bN);}),_bP=function(_bQ,_bR){var _bS=function(_bT,_bU){var _bV=E(_bT);if(!_bV[0]){var _bW=new T(function(){return A(_bU,[_R]);});return function(_bX){return A(_bX,[_bW]);};}else{var _bY=E(_bV[1])[1],_bZ=function(_c0){var _c1=new T(function(){return _bS(_bV[2],function(_c2){return A(_bU,[[1,_c0,_c2]]);});});return function(_c3){var _c4=new T(function(){return A(_c1,[_c3]);});return [0,function(_c5){return E(_c4);}];};};switch(E(E(_bQ)[1])){case 8:if(48>_bY){var _c6=new T(function(){return A(_bU,[_R]);});return function(_c7){return A(_c7,[_c6]);};}else{if(_bY>55){var _c8=new T(function(){return A(_bU,[_R]);});return function(_c9){return A(_c9,[_c8]);};}else{return _bZ([0,_bY-48|0]);}}break;case 10:if(48>_bY){var _ca=new T(function(){return A(_bU,[_R]);});return function(_cb){return A(_cb,[_ca]);};}else{if(_bY>57){var _cc=new T(function(){return A(_bU,[_R]);});return function(_cd){return A(_cd,[_cc]);};}else{return _bZ([0,_bY-48|0]);}}break;case 16:var _ce=new T(function(){return 97>_bY?65>_bY?[0]:_bY>70?[0]:[1,[0,(_bY-65|0)+10|0]]:_bY>102?65>_bY?[0]:_bY>70?[0]:[1,[0,(_bY-65|0)+10|0]]:[1,[0,(_bY-97|0)+10|0]];});if(48>_bY){var _cf=E(_ce);if(!_cf[0]){var _cg=new T(function(){return A(_bU,[_R]);});return function(_ch){return A(_ch,[_cg]);};}else{return _bZ(_cf[1]);}}else{if(_bY>57){var _ci=E(_ce);if(!_ci[0]){var _cj=new T(function(){return A(_bU,[_R]);});return function(_ck){return A(_ck,[_cj]);};}else{return _bZ(_ci[1]);}}else{return _bZ([0,_bY-48|0]);}}break;default:return E(_bO);}}};return [1,function(_cl){return A(_bS,[_cl,_8,function(_cm){var _cn=E(_cm);return _cn[0]==0?[2]:A(_bR,[_cn]);}]);}];},_co=[0,10],_cp=[0,1],_cq=[0,2147483647],_cr=function(_cs,_ct){while(1){var _cu=E(_cs);if(!_cu[0]){var _cv=_cu[1],_cw=E(_ct);if(!_cw[0]){var _cx=_cw[1],_cy=addC(_cv,_cx);if(!E(_cy[2])){return [0,_cy[1]];}else{_cs=[1,I_fromInt(_cv)];_ct=[1,I_fromInt(_cx)];continue;}}else{_cs=[1,I_fromInt(_cv)];_ct=_cw;continue;}}else{var _cz=E(_ct);if(!_cz[0]){_cs=_cu;_ct=[1,I_fromInt(_cz[1])];continue;}else{return [1,I_add(_cu[1],_cz[1])];}}}},_cA=new T(function(){return _cr(_cq,_cp);}),_cB=function(_cC){var _cD=E(_cC);if(!_cD[0]){var _cE=E(_cD[1]);return _cE==(-2147483648)?E(_cA):[0, -_cE];}else{return [1,I_negate(_cD[1])];}},_cF=[0,10],_cG=[0,0],_cH=function(_cI,_cJ){while(1){var _cK=E(_cI);if(!_cK[0]){var _cL=_cK[1],_cM=E(_cJ);if(!_cM[0]){var _cN=_cM[1];if(!(imul(_cL,_cN)|0)){return [0,imul(_cL,_cN)|0];}else{_cI=[1,I_fromInt(_cL)];_cJ=[1,I_fromInt(_cN)];continue;}}else{_cI=[1,I_fromInt(_cL)];_cJ=_cM;continue;}}else{var _cO=E(_cJ);if(!_cO[0]){_cI=_cK;_cJ=[1,I_fromInt(_cO[1])];continue;}else{return [1,I_mul(_cK[1],_cO[1])];}}}},_cP=function(_cQ,_cR,_cS){while(1){var _cT=E(_cS);if(!_cT[0]){return E(_cR);}else{var _cU=_cr(_cH(_cR,_cQ),_cT[1]);_cS=_cT[2];_cR=_cU;continue;}}},_cV=function(_cW){var _cX=new T(function(){return _ah(_ah([0,function(_cY){return E(E(_cY)[1])==45?_bP(_co,function(_cZ){return A(_cW,[[1,new T(function(){return _cB(_cP(_cF,_cG,_cZ));})]]);}):[2];}],[0,function(_d0){return E(E(_d0)[1])==43?_bP(_co,function(_d1){return A(_cW,[[1,new T(function(){return _cP(_cF,_cG,_d1);})]]);}):[2];}]),new T(function(){return _bP(_co,function(_d2){return A(_cW,[[1,new T(function(){return _cP(_cF,_cG,_d2);})]]);});}));});return _ah([0,function(_d3){return E(E(_d3)[1])==101?E(_cX):[2];}],[0,function(_d4){return E(E(_d4)[1])==69?E(_cX):[2];}]);},_d5=function(_d6){return A(_d6,[_2p]);},_d7=function(_d8){return A(_d8,[_2p]);},_d9=function(_da){var _db=new T(function(){return _bP(_co,function(_dc){return A(_da,[[1,_dc]]);});});return [0,function(_dd){return E(E(_dd)[1])==46?E(_db):[2];}];},_de=function(_df){return _bP(_co,function(_dg){return _bq(_d9,_d5,function(_dh){return _bq(_cV,_d7,function(_di){return A(_df,[[5,[1,_dg,_dh,_di]]]);});});});},_dj=function(_dk,_dl,_dm){while(1){var _dn=E(_dm);if(!_dn[0]){return false;}else{if(!A(_aP,[_dk,_dl,_dn[1]])){_dm=_dn[2];continue;}else{return true;}}}},_do=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dp=function(_dq){return _dj(_aO,_dq,_do);},_dr=[0,8],_ds=[0,16],_dt=function(_du){var _dv=new T(function(){return _bP(_ds,function(_dw){return A(_du,[[5,[0,_ds,_dw]]]);});}),_dx=new T(function(){return _bP(_dr,function(_dy){return A(_du,[[5,[0,_dr,_dy]]]);});}),_dz=new T(function(){return _bP(_ds,function(_dA){return A(_du,[[5,[0,_ds,_dA]]]);});}),_dB=new T(function(){return _bP(_dr,function(_dC){return A(_du,[[5,[0,_dr,_dC]]]);});});return [0,function(_dD){return E(E(_dD)[1])==48?E([0,function(_dE){switch(E(E(_dE)[1])){case 79:return E(_dB);case 88:return E(_dz);case 111:return E(_dx);case 120:return E(_dv);default:return [2];}}]):[2];}];},_dF=false,_dG=true,_dH=function(_dI){var _dJ=new T(function(){return A(_dI,[_ds]);}),_dK=new T(function(){return A(_dI,[_dr]);}),_dL=new T(function(){return A(_dI,[_ds]);}),_dM=new T(function(){return A(_dI,[_dr]);});return [0,function(_dN){switch(E(E(_dN)[1])){case 79:return E(_dM);case 88:return E(_dL);case 111:return E(_dK);case 120:return E(_dJ);default:return [2];}}];},_dO=function(_dP){return A(_dP,[_co]);},_dQ=function(_dR){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3f(9,_dR,_R);})));},_dS=function(_dT){var _dU=E(_dT);return _dU[0]==0?E(_dU[1]):I_toInt(_dU[1]);},_dV=function(_dW,_dX){var _dY=E(_dW);if(!_dY[0]){var _dZ=_dY[1],_e0=E(_dX);return _e0[0]==0?_dZ<=_e0[1]:I_compareInt(_e0[1],_dZ)>=0;}else{var _e1=_dY[1],_e2=E(_dX);return _e2[0]==0?I_compareInt(_e1,_e2[1])<=0:I_compare(_e1,_e2[1])<=0;}},_e3=function(_e4){return [2];},_e5=function(_e6){var _e7=E(_e6);if(!_e7[0]){return E(_e3);}else{var _e8=_e7[1],_e9=E(_e7[2]);if(!_e9[0]){return E(_e8);}else{var _ea=new T(function(){return _e5(_e9);});return function(_eb){return _ah(A(_e8,[_eb]),new T(function(){return A(_ea,[_eb]);}));};}}},_ec=unCStr("NUL"),_ed=function(_ee){return [2];},_ef=function(_eg){return _ed(_eg);},_eh=function(_ei,_ej){var _ek=function(_el,_em){var _en=E(_el);if(!_en[0]){return function(_eo){return A(_eo,[_ei]);};}else{var _ep=E(_em);if(!_ep[0]){return E(_ed);}else{if(E(_en[1])[1]!=E(_ep[1])[1]){return E(_ef);}else{var _eq=new T(function(){return _ek(_en[2],_ep[2]);});return function(_er){var _es=new T(function(){return A(_eq,[_er]);});return [0,function(_et){return E(_es);}];};}}}};return [1,function(_eu){return A(_ek,[_ei,_eu,_ej]);}];},_ev=[0,0],_ew=function(_ex){var _ey=new T(function(){return A(_ex,[_ev]);});return _eh(_ec,function(_ez){return E(_ey);});},_eA=unCStr("STX"),_eB=[0,2],_eC=function(_eD){var _eE=new T(function(){return A(_eD,[_eB]);});return _eh(_eA,function(_eF){return E(_eE);});},_eG=unCStr("ETX"),_eH=[0,3],_eI=function(_eJ){var _eK=new T(function(){return A(_eJ,[_eH]);});return _eh(_eG,function(_eL){return E(_eK);});},_eM=unCStr("EOT"),_eN=[0,4],_eO=function(_eP){var _eQ=new T(function(){return A(_eP,[_eN]);});return _eh(_eM,function(_eR){return E(_eQ);});},_eS=unCStr("ENQ"),_eT=[0,5],_eU=function(_eV){var _eW=new T(function(){return A(_eV,[_eT]);});return _eh(_eS,function(_eX){return E(_eW);});},_eY=unCStr("ACK"),_eZ=[0,6],_f0=function(_f1){var _f2=new T(function(){return A(_f1,[_eZ]);});return _eh(_eY,function(_f3){return E(_f2);});},_f4=unCStr("BEL"),_f5=[0,7],_f6=function(_f7){var _f8=new T(function(){return A(_f7,[_f5]);});return _eh(_f4,function(_f9){return E(_f8);});},_fa=unCStr("BS"),_fb=[0,8],_fc=function(_fd){var _fe=new T(function(){return A(_fd,[_fb]);});return _eh(_fa,function(_ff){return E(_fe);});},_fg=unCStr("HT"),_fh=[0,9],_fi=function(_fj){var _fk=new T(function(){return A(_fj,[_fh]);});return _eh(_fg,function(_fl){return E(_fk);});},_fm=unCStr("LF"),_fn=[0,10],_fo=function(_fp){var _fq=new T(function(){return A(_fp,[_fn]);});return _eh(_fm,function(_fr){return E(_fq);});},_fs=unCStr("VT"),_ft=[0,11],_fu=function(_fv){var _fw=new T(function(){return A(_fv,[_ft]);});return _eh(_fs,function(_fx){return E(_fw);});},_fy=unCStr("FF"),_fz=[0,12],_fA=function(_fB){var _fC=new T(function(){return A(_fB,[_fz]);});return _eh(_fy,function(_fD){return E(_fC);});},_fE=unCStr("CR"),_fF=[0,13],_fG=function(_fH){var _fI=new T(function(){return A(_fH,[_fF]);});return _eh(_fE,function(_fJ){return E(_fI);});},_fK=unCStr("SI"),_fL=[0,15],_fM=function(_fN){var _fO=new T(function(){return A(_fN,[_fL]);});return _eh(_fK,function(_fP){return E(_fO);});},_fQ=unCStr("DLE"),_fR=[0,16],_fS=function(_fT){var _fU=new T(function(){return A(_fT,[_fR]);});return _eh(_fQ,function(_fV){return E(_fU);});},_fW=unCStr("DC1"),_fX=[0,17],_fY=function(_fZ){var _g0=new T(function(){return A(_fZ,[_fX]);});return _eh(_fW,function(_g1){return E(_g0);});},_g2=unCStr("DC2"),_g3=[0,18],_g4=function(_g5){var _g6=new T(function(){return A(_g5,[_g3]);});return _eh(_g2,function(_g7){return E(_g6);});},_g8=unCStr("DC3"),_g9=[0,19],_ga=function(_gb){var _gc=new T(function(){return A(_gb,[_g9]);});return _eh(_g8,function(_gd){return E(_gc);});},_ge=unCStr("DC4"),_gf=[0,20],_gg=function(_gh){var _gi=new T(function(){return A(_gh,[_gf]);});return _eh(_ge,function(_gj){return E(_gi);});},_gk=unCStr("NAK"),_gl=[0,21],_gm=function(_gn){var _go=new T(function(){return A(_gn,[_gl]);});return _eh(_gk,function(_gp){return E(_go);});},_gq=unCStr("SYN"),_gr=[0,22],_gs=function(_gt){var _gu=new T(function(){return A(_gt,[_gr]);});return _eh(_gq,function(_gv){return E(_gu);});},_gw=unCStr("ETB"),_gx=[0,23],_gy=function(_gz){var _gA=new T(function(){return A(_gz,[_gx]);});return _eh(_gw,function(_gB){return E(_gA);});},_gC=unCStr("CAN"),_gD=[0,24],_gE=function(_gF){var _gG=new T(function(){return A(_gF,[_gD]);});return _eh(_gC,function(_gH){return E(_gG);});},_gI=unCStr("EM"),_gJ=[0,25],_gK=function(_gL){var _gM=new T(function(){return A(_gL,[_gJ]);});return _eh(_gI,function(_gN){return E(_gM);});},_gO=unCStr("SUB"),_gP=[0,26],_gQ=function(_gR){var _gS=new T(function(){return A(_gR,[_gP]);});return _eh(_gO,function(_gT){return E(_gS);});},_gU=unCStr("ESC"),_gV=[0,27],_gW=function(_gX){var _gY=new T(function(){return A(_gX,[_gV]);});return _eh(_gU,function(_gZ){return E(_gY);});},_h0=unCStr("FS"),_h1=[0,28],_h2=function(_h3){var _h4=new T(function(){return A(_h3,[_h1]);});return _eh(_h0,function(_h5){return E(_h4);});},_h6=unCStr("GS"),_h7=[0,29],_h8=function(_h9){var _ha=new T(function(){return A(_h9,[_h7]);});return _eh(_h6,function(_hb){return E(_ha);});},_hc=unCStr("RS"),_hd=[0,30],_he=function(_hf){var _hg=new T(function(){return A(_hf,[_hd]);});return _eh(_hc,function(_hh){return E(_hg);});},_hi=unCStr("US"),_hj=[0,31],_hk=function(_hl){var _hm=new T(function(){return A(_hl,[_hj]);});return _eh(_hi,function(_hn){return E(_hm);});},_ho=unCStr("SP"),_hp=[0,32],_hq=function(_hr){var _hs=new T(function(){return A(_hr,[_hp]);});return _eh(_ho,function(_ht){return E(_hs);});},_hu=unCStr("DEL"),_hv=[0,127],_hw=function(_hx){var _hy=new T(function(){return A(_hx,[_hv]);});return _eh(_hu,function(_hz){return E(_hy);});},_hA=[1,_hw,_R],_hB=[1,_hq,_hA],_hC=[1,_hk,_hB],_hD=[1,_he,_hC],_hE=[1,_h8,_hD],_hF=[1,_h2,_hE],_hG=[1,_gW,_hF],_hH=[1,_gQ,_hG],_hI=[1,_gK,_hH],_hJ=[1,_gE,_hI],_hK=[1,_gy,_hJ],_hL=[1,_gs,_hK],_hM=[1,_gm,_hL],_hN=[1,_gg,_hM],_hO=[1,_ga,_hN],_hP=[1,_g4,_hO],_hQ=[1,_fY,_hP],_hR=[1,_fS,_hQ],_hS=[1,_fM,_hR],_hT=[1,_fG,_hS],_hU=[1,_fA,_hT],_hV=[1,_fu,_hU],_hW=[1,_fo,_hV],_hX=[1,_fi,_hW],_hY=[1,_fc,_hX],_hZ=[1,_f6,_hY],_i0=[1,_f0,_hZ],_i1=[1,_eU,_i0],_i2=[1,_eO,_i1],_i3=[1,_eI,_i2],_i4=[1,_eC,_i3],_i5=[1,_ew,_i4],_i6=unCStr("SOH"),_i7=[0,1],_i8=function(_i9){var _ia=new T(function(){return A(_i9,[_i7]);});return _eh(_i6,function(_ib){return E(_ia);});},_ic=unCStr("SO"),_id=[0,14],_ie=function(_if){var _ig=new T(function(){return A(_if,[_id]);});return _eh(_ic,function(_ih){return E(_ig);});},_ii=function(_ij){return _bq(_i8,_ie,_ij);},_ik=[1,_ii,_i5],_il=new T(function(){return _e5(_ik);}),_im=[0,1114111],_in=[0,34],_io=[0,_in,_dG],_ip=[0,39],_iq=[0,_ip,_dG],_ir=[0,92],_is=[0,_ir,_dG],_it=[0,_f5,_dG],_iu=[0,_fb,_dG],_iv=[0,_fz,_dG],_iw=[0,_fn,_dG],_ix=[0,_fF,_dG],_iy=[0,_fh,_dG],_iz=[0,_ft,_dG],_iA=[0,_ev,_dG],_iB=[0,_i7,_dG],_iC=[0,_eB,_dG],_iD=[0,_eH,_dG],_iE=[0,_eN,_dG],_iF=[0,_eT,_dG],_iG=[0,_eZ,_dG],_iH=[0,_f5,_dG],_iI=[0,_fb,_dG],_iJ=[0,_fh,_dG],_iK=[0,_fn,_dG],_iL=[0,_ft,_dG],_iM=[0,_fz,_dG],_iN=[0,_fF,_dG],_iO=[0,_id,_dG],_iP=[0,_fL,_dG],_iQ=[0,_fR,_dG],_iR=[0,_fX,_dG],_iS=[0,_g3,_dG],_iT=[0,_g9,_dG],_iU=[0,_gf,_dG],_iV=[0,_gl,_dG],_iW=[0,_gr,_dG],_iX=[0,_gx,_dG],_iY=[0,_gD,_dG],_iZ=[0,_gJ,_dG],_j0=[0,_gP,_dG],_j1=[0,_gV,_dG],_j2=[0,_h1,_dG],_j3=[0,_h7,_dG],_j4=[0,_hd,_dG],_j5=[0,_hj,_dG],_j6=function(_j7){return [0,_j7];},_j8=function(_j9){var _ja=new T(function(){return A(_j9,[_iz]);}),_jb=new T(function(){return A(_j9,[_iy]);}),_jc=new T(function(){return A(_j9,[_ix]);}),_jd=new T(function(){return A(_j9,[_iw]);}),_je=new T(function(){return A(_j9,[_iv]);}),_jf=new T(function(){return A(_j9,[_iu]);}),_jg=new T(function(){return A(_j9,[_it]);}),_jh=new T(function(){return A(_j9,[_is]);}),_ji=new T(function(){return A(_j9,[_iq]);}),_jj=new T(function(){return A(_j9,[_io]);});return _ah([0,function(_jk){switch(E(E(_jk)[1])){case 34:return E(_jj);case 39:return E(_ji);case 92:return E(_jh);case 97:return E(_jg);case 98:return E(_jf);case 102:return E(_je);case 110:return E(_jd);case 114:return E(_jc);case 116:return E(_jb);case 118:return E(_ja);default:return [2];}}],new T(function(){return _ah(_bq(_dH,_dO,function(_jl){var _jm=new T(function(){return _j6(E(_jl)[1]);});return _bP(_jl,function(_jn){var _jo=_cP(_jm,_cG,_jn);return !_dV(_jo,_im)?[2]:A(_j9,[[0,new T(function(){var _jp=_dS(_jo);return _jp>>>0>1114111?_dQ(_jp):[0,_jp];}),_dG]]);});}),new T(function(){var _jq=new T(function(){return A(_j9,[_j5]);}),_jr=new T(function(){return A(_j9,[_j4]);}),_js=new T(function(){return A(_j9,[_j3]);}),_jt=new T(function(){return A(_j9,[_j2]);}),_ju=new T(function(){return A(_j9,[_j1]);}),_jv=new T(function(){return A(_j9,[_j0]);}),_jw=new T(function(){return A(_j9,[_iZ]);}),_jx=new T(function(){return A(_j9,[_iY]);}),_jy=new T(function(){return A(_j9,[_iX]);}),_jz=new T(function(){return A(_j9,[_iW]);}),_jA=new T(function(){return A(_j9,[_iV]);}),_jB=new T(function(){return A(_j9,[_iU]);}),_jC=new T(function(){return A(_j9,[_iT]);}),_jD=new T(function(){return A(_j9,[_iS]);}),_jE=new T(function(){return A(_j9,[_iR]);}),_jF=new T(function(){return A(_j9,[_iQ]);}),_jG=new T(function(){return A(_j9,[_iP]);}),_jH=new T(function(){return A(_j9,[_iO]);}),_jI=new T(function(){return A(_j9,[_iN]);}),_jJ=new T(function(){return A(_j9,[_iM]);}),_jK=new T(function(){return A(_j9,[_iL]);}),_jL=new T(function(){return A(_j9,[_iK]);}),_jM=new T(function(){return A(_j9,[_iJ]);}),_jN=new T(function(){return A(_j9,[_iI]);}),_jO=new T(function(){return A(_j9,[_iH]);}),_jP=new T(function(){return A(_j9,[_iG]);}),_jQ=new T(function(){return A(_j9,[_iF]);}),_jR=new T(function(){return A(_j9,[_iE]);}),_jS=new T(function(){return A(_j9,[_iD]);}),_jT=new T(function(){return A(_j9,[_iC]);}),_jU=new T(function(){return A(_j9,[_iB]);}),_jV=new T(function(){return A(_j9,[_iA]);});return _ah([0,function(_jW){return E(E(_jW)[1])==94?E([0,function(_jX){switch(E(E(_jX)[1])){case 64:return E(_jV);case 65:return E(_jU);case 66:return E(_jT);case 67:return E(_jS);case 68:return E(_jR);case 69:return E(_jQ);case 70:return E(_jP);case 71:return E(_jO);case 72:return E(_jN);case 73:return E(_jM);case 74:return E(_jL);case 75:return E(_jK);case 76:return E(_jJ);case 77:return E(_jI);case 78:return E(_jH);case 79:return E(_jG);case 80:return E(_jF);case 81:return E(_jE);case 82:return E(_jD);case 83:return E(_jC);case 84:return E(_jB);case 85:return E(_jA);case 86:return E(_jz);case 87:return E(_jy);case 88:return E(_jx);case 89:return E(_jw);case 90:return E(_jv);case 91:return E(_ju);case 92:return E(_jt);case 93:return E(_js);case 94:return E(_jr);case 95:return E(_jq);default:return [2];}}]):[2];}],new T(function(){return A(_il,[function(_jY){return A(_j9,[[0,_jY,_dG]]);}]);}));}));}));},_jZ=function(_k0){return A(_k0,[_1]);},_k1=function(_k2){var _k3=E(_k2);if(!_k3[0]){return E(_jZ);}else{var _k4=_k3[2],_k5=E(E(_k3[1])[1]);switch(_k5){case 9:var _k6=new T(function(){return _k1(_k4);});return function(_k7){var _k8=new T(function(){return A(_k6,[_k7]);});return [0,function(_k9){return E(_k8);}];};case 10:var _ka=new T(function(){return _k1(_k4);});return function(_kb){var _kc=new T(function(){return A(_ka,[_kb]);});return [0,function(_kd){return E(_kc);}];};case 11:var _ke=new T(function(){return _k1(_k4);});return function(_kf){var _kg=new T(function(){return A(_ke,[_kf]);});return [0,function(_kh){return E(_kg);}];};case 12:var _ki=new T(function(){return _k1(_k4);});return function(_kj){var _kk=new T(function(){return A(_ki,[_kj]);});return [0,function(_kl){return E(_kk);}];};case 13:var _km=new T(function(){return _k1(_k4);});return function(_kn){var _ko=new T(function(){return A(_km,[_kn]);});return [0,function(_kp){return E(_ko);}];};case 32:var _kq=new T(function(){return _k1(_k4);});return function(_kr){var _ks=new T(function(){return A(_kq,[_kr]);});return [0,function(_kt){return E(_ks);}];};case 160:var _ku=new T(function(){return _k1(_k4);});return function(_kv){var _kw=new T(function(){return A(_ku,[_kv]);});return [0,function(_kx){return E(_kw);}];};default:var _ky=u_iswspace(_k5);if(!E(_ky)){return E(_jZ);}else{var _kz=new T(function(){return _k1(_k4);});return function(_kA){var _kB=new T(function(){return A(_kz,[_kA]);});return [0,function(_kC){return E(_kB);}];};}}}},_kD=function(_kE){var _kF=new T(function(){return _j8(_kE);}),_kG=new T(function(){return _kD(_kE);}),_kH=[1,function(_kI){return A(_k1,[_kI,function(_kJ){return E([0,function(_kK){return E(E(_kK)[1])==92?E(_kG):[2];}]);}]);}];return _ah([0,function(_kL){return E(E(_kL)[1])==92?E([0,function(_kM){var _kN=E(E(_kM)[1]);switch(_kN){case 9:return E(_kH);case 10:return E(_kH);case 11:return E(_kH);case 12:return E(_kH);case 13:return E(_kH);case 32:return E(_kH);case 38:return E(_kG);case 160:return E(_kH);default:var _kO=u_iswspace(_kN);return E(_kO)==0?[2]:E(_kH);}}]):[2];}],[0,function(_kP){var _kQ=E(_kP);return E(_kQ[1])==92?E(_kF):A(_kE,[[0,_kQ,_dF]]);}]);},_kR=function(_kS,_kT){var _kU=new T(function(){return A(_kT,[[1,new T(function(){return A(_kS,[_R]);})]]);});return _kD(function(_kV){var _kW=E(_kV),_kX=E(_kW[1]);return E(_kX[1])==34?!E(_kW[2])?E(_kU):_kR(function(_kY){return A(_kS,[[1,_kX,_kY]]);},_kT):_kR(function(_kZ){return A(_kS,[[1,_kX,_kZ]]);},_kT);});},_l0=unCStr("_\'"),_l1=function(_l2){var _l3=u_iswalnum(_l2);return E(_l3)==0?_dj(_aO,[0,_l2],_l0):true;},_l4=function(_l5){return _l1(E(_l5)[1]);},_l6=unCStr(",;()[]{}`"),_l7=function(_l8){return A(_l8,[_R]);},_l9=function(_la,_lb){var _lc=function(_ld){var _le=E(_ld);if(!_le[0]){return E(_l7);}else{var _lf=_le[1];if(!A(_la,[_lf])){return E(_l7);}else{var _lg=new T(function(){return _lc(_le[2]);});return function(_lh){var _li=new T(function(){return A(_lg,[function(_lj){return A(_lh,[[1,_lf,_lj]]);}]);});return [0,function(_lk){return E(_li);}];};}}};return [1,function(_ll){return A(_lc,[_ll,_lb]);}];},_lm=unCStr(".."),_ln=unCStr("::"),_lo=unCStr("->"),_lp=[0,64],_lq=[1,_lp,_R],_lr=[0,126],_ls=[1,_lr,_R],_lt=unCStr("=>"),_lu=[1,_lt,_R],_lv=[1,_ls,_lu],_lw=[1,_lq,_lv],_lx=[1,_lo,_lw],_ly=unCStr("<-"),_lz=[1,_ly,_lx],_lA=[0,124],_lB=[1,_lA,_R],_lC=[1,_lB,_lz],_lD=[1,_ir,_R],_lE=[1,_lD,_lC],_lF=[0,61],_lG=[1,_lF,_R],_lH=[1,_lG,_lE],_lI=[1,_ln,_lH],_lJ=[1,_lm,_lI],_lK=function(_lL){var _lM=new T(function(){return A(_lL,[_bM]);});return _ah([1,function(_lN){return E(_lN)[0]==0?E(_lM):[2];}],new T(function(){var _lO=new T(function(){return _j8(function(_lP){var _lQ=E(_lP);return (function(_lR,_lS){var _lT=new T(function(){return A(_lL,[[0,_lR]]);});return !E(_lS)?E(E(_lR)[1])==39?[2]:[0,function(_lU){return E(E(_lU)[1])==39?E(_lT):[2];}]:[0,function(_lV){return E(E(_lV)[1])==39?E(_lT):[2];}];})(_lQ[1],_lQ[2]);});});return _ah([0,function(_lW){return E(E(_lW)[1])==39?E([0,function(_lX){var _lY=E(_lX);switch(E(_lY[1])){case 39:return [2];case 92:return E(_lO);default:var _lZ=new T(function(){return A(_lL,[[0,_lY]]);});return [0,function(_m0){return E(E(_m0)[1])==39?E(_lZ):[2];}];}}]):[2];}],new T(function(){var _m1=new T(function(){return _kR(_8,_lL);});return _ah([0,function(_m2){return E(E(_m2)[1])==34?E(_m1):[2];}],new T(function(){return _ah([0,function(_m3){return !_dj(_aO,_m3,_l6)?[2]:A(_lL,[[2,[1,_m3,_R]]]);}],new T(function(){return _ah([0,function(_m4){return !_dj(_aO,_m4,_do)?[2]:_l9(_dp,function(_m5){var _m6=[1,_m4,_m5];return !_dj(_b5,_m6,_lJ)?A(_lL,[[4,_m6]]):A(_lL,[[2,_m6]]);});}],new T(function(){return _ah([0,function(_m7){var _m8=E(_m7),_m9=_m8[1],_ma=u_iswalpha(_m9);return E(_ma)==0?E(_m9)==95?_l9(_l4,function(_mb){return A(_lL,[[3,[1,_m8,_mb]]]);}):[2]:_l9(_l4,function(_mc){return A(_lL,[[3,[1,_m8,_mc]]]);});}],new T(function(){return _bq(_dt,_de,_lL);}));}));}));}));}));}));},_md=function(_me){var _mf=new T(function(){return _lK(_me);});return [1,function(_mg){return A(_k1,[_mg,function(_mh){return E(_mf);}]);}];},_mi=[0,0],_mj=function(_mk,_ml){var _mm=new T(function(){return A(_mk,[_mi,function(_mn){var _mo=new T(function(){return A(_ml,[_mn]);});return _md(function(_mp){var _mq=E(_mp);if(_mq[0]==2){var _mr=E(_mq[1]);return _mr[0]==0?[2]:E(E(_mr[1])[1])==41?E(_mr[2])[0]==0?E(_mo):[2]:[2];}else{return [2];}});}]);});return _md(function(_ms){var _mt=E(_ms);if(_mt[0]==2){var _mu=E(_mt[1]);return _mu[0]==0?[2]:E(E(_mu[1])[1])==40?E(_mu[2])[0]==0?E(_mm):[2]:[2];}else{return [2];}});},_mv=function(_mw,_mx,_my){var _mz=function(_mA,_mB){var _mC=new T(function(){return _lK(function(_mD){return A(_mw,[_mD,_mA,function(_mE){return A(_mB,[new T(function(){return [0, -E(_mE)[1]];})]);}]);});});return _ah(_md(function(_mF){var _mG=E(_mF);if(_mG[0]==4){var _mH=E(_mG[1]);return _mH[0]==0?A(_mw,[_mG,_mA,_mB]):E(E(_mH[1])[1])==45?E(_mH[2])[0]==0?E([1,function(_mI){return A(_k1,[_mI,function(_mJ){return E(_mC);}]);}]):A(_mw,[_mG,_mA,_mB]):A(_mw,[_mG,_mA,_mB]);}else{return A(_mw,[_mG,_mA,_mB]);}}),new T(function(){return _mj(_mz,_mB);}));};return _mz(_mx,_my);},_mK=function(_mL,_mM){return [2];},_mN=function(_mO,_mP){return _mK(_mO,_mP);},_mQ=function(_mR){var _mS=E(_mR);return _mS[0]==0?[1,new T(function(){return _cP(new T(function(){return _j6(E(_mS[1])[1]);}),_cG,_mS[2]);})]:E(_mS[2])[0]==0?E(_mS[3])[0]==0?[1,new T(function(){return _cP(_cF,_cG,_mS[1]);})]:[0]:[0];},_mT=function(_mU){var _mV=E(_mU);if(_mV[0]==5){var _mW=_mQ(_mV[1]);if(!_mW[0]){return E(_mK);}else{var _mX=new T(function(){return [0,_dS(_mW[1])];});return function(_mY,_mZ){return A(_mZ,[_mX]);};}}else{return E(_mN);}},_n0=function(_mO,_mP){return _mv(_mT,_mO,_mP);},_n1=function(_n2,_n3){var _n4=function(_n5,_n6){var _n7=new T(function(){return A(_n6,[_R]);}),_n8=new T(function(){return A(_n2,[_mi,function(_n9){return _n4(_dG,function(_na){return A(_n6,[[1,_n9,_na]]);});}]);});return _md(function(_nb){var _nc=E(_nb);if(_nc[0]==2){var _nd=E(_nc[1]);if(!_nd[0]){return [2];}else{var _ne=_nd[2];switch(E(E(_nd[1])[1])){case 44:return E(_ne)[0]==0?!E(_n5)?[2]:E(_n8):[2];case 93:return E(_ne)[0]==0?E(_n7):[2];default:return [2];}}}else{return [2];}});},_nf=function(_ng){var _nh=new T(function(){return _ah(_n4(_dF,_ng),new T(function(){return A(_n2,[_mi,function(_ni){return _n4(_dG,function(_nj){return A(_ng,[[1,_ni,_nj]]);});}]);}));});return _ah(_md(function(_nk){var _nl=E(_nk);if(_nl[0]==2){var _nm=E(_nl[1]);return _nm[0]==0?[2]:E(E(_nm[1])[1])==91?E(_nm[2])[0]==0?E(_nh):[2]:[2];}else{return [2];}}),new T(function(){return _mj(function(_nn,_no){return _nf(_no);},_ng);}));};return _nf(_n3);},_np=function(_nq,_nr){return _n1(_n0,_nr);},_ns=new T(function(){return _n1(_n0,_bi);}),_nt=function(_mP){return _a7(_ns,_mP);},_nu=function(_nv){var _nw=new T(function(){return _mv(_mT,_nv,_bi);});return function(_bL){return _a7(_nw,_bL);};},_nx=[0,_nu,_nt,_n0,_np],_ny=function(_nz,_nA){return _3f(0,E(_nz)[1],_nA);},_nB=function(_nC,_nD){return _25(_ny,_nC,_nD);},_nE=function(_nF,_nG,_nH){return _3f(E(_nF)[1],E(_nG)[1],_nH);},_nI=[0,_nE,_52,_nB],_nJ=unCStr("GHC.Types"),_nK=unCStr("Int"),_nL=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_6k,_nJ,_nK],_nM=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_nL,_R],_nN=function(_nO){return E(_nM);},_nP=function(_nQ){return E(E(_nQ)[1]);},_nR=function(_nS){return E(E(_nS)[1]);},_nT=function(_nU){return E(E(_nU)[2]);},_nV=function(_nW,_nX){var _nY=new T(function(){return A(_nT,[_nW,_nX]);}),_nZ=new T(function(){return _nR(_nW);}),_o0=new T(function(){return _30(_nZ);}),_o1=new T(function(){return _2A(_nZ);});return function(_o2){return A(_o1,[_nY,function(_o3){return A(_o0,[[0,_o3,_o2]]);}]);};},_o4=function(_o5,_o6){return A(_o5,[function(_){return jsFind(toJSStr(E(_o6)));}]);},_o7=function(_o8){return E(E(_o8)[4]);},_o9=unCStr("[]"),_oa=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_6k,_nJ,_o9],_ob=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oa,_R],_oc=function(_od){return E(_ob);},_oe=unCStr("Char"),_of=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_6k,_nJ,_oe],_og=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_of,_R],_oh=function(_oi){return E(_og);},_oj=new T(function(){return _7e(_oc,_oh);}),_ok=new T(function(){return A(_oj,[_7d]);}),_ol=new T(function(){return E(_7d);}),_om=function(_on){return E(E(_on)[7]);},_oo=function(_op){return E(E(_op)[1]);},_oq=[0,0],_or=[0,32],_os=[0,10],_ot=function(_ou){var _ov=E(_ou);if(!_ov[0]){return E(_8);}else{var _ow=_ov[1],_ox=E(_ov[2]);if(!_ox[0]){return _oy(_os,_ow);}else{var _oz=new T(function(){return _ot(_ox);}),_oA=new T(function(){return _oy(_os,_ow);});return function(_oB){return A(_oA,[[1,_or,new T(function(){return A(_oz,[_oB]);})]]);};}}},_oC=unCStr("->"),_oD=[1,_oC,_R],_oE=[1,_nJ,_oD],_oF=[1,_6k,_oE],_oG=[0,32],_oH=function(_oI){var _oJ=E(_oI);if(!_oJ[0]){return [0];}else{var _oK=_oJ[1],_oL=E(_oJ[2]);return _oL[0]==0?E(_oK):_1f(_oK,[1,_oG,new T(function(){return _oH(_oL);})]);}},_oM=new T(function(){return _oH(_oF);}),_oN=new T(function(){var _oO=_6I(_oM);return [0,_oO[1],_oO[2],_6k,_nJ,_oC];}),_oP=function(_oQ,_oR){var _oS=E(_oQ);return _oS[0]==0?E(_oR):A(_oS[1],[new T(function(){return _oP(_oS[2],_oR);})]);},_oT=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_oU=[1,_6m,_R],_oV=function(_oW){var _oX=E(_oW);if(!_oX[0]){return [0];}else{var _oY=E(_oX[1]);return [1,[0,_oY[1],_oY[2]],new T(function(){return _oV(_oX[2]);})];}},_oZ=new T(function(){var _p0=_1f(_R,_oU);if(!_p0[0]){return E(_oa);}else{var _p1=_6I(new T(function(){return _6w(_6U(_75,[1,_oT,new T(function(){return _oV(_p0);})]));}));return E(_oa);}}),_p2=[0,40],_p3=function(_p4){return _oy(_os,_p4);},_p5=[0,8],_p6=unCStr(" -> "),_p7=[0,9],_p8=[0,93],_p9=[0,91],_pa=[0,41],_pb=[0,44],_pc=function(_p4){return [1,_pb,_p4];},_pd=function(_pe,_pf){var _pg=E(_pf);return _pg[0]==0?[0]:[1,_pe,[1,_pg[1],new T(function(){return _pd(_pe,_pg[2]);})]];},_oy=function(_ph,_pi){var _pj=E(_pi),_pk=_pj[3],_pl=E(_pj[4]);if(!_pl[0]){return function(_pm){return _1f(E(_pk)[5],_pm);};}else{var _pn=_pl[1],_po=new T(function(){var _pp=E(_pk)[5],_pq=new T(function(){return _ot(_pl);}),_pr=new T(function(){return E(_ph)[1]<=9?function(_ps){return _1f(_pp,[1,_or,new T(function(){return A(_pq,[_ps]);})]);}:function(_pt){return [1,_3e,new T(function(){return _1f(_pp,[1,_or,new T(function(){return A(_pq,[[1,_3d,_pt]]);})]);})];};}),_pu=E(_pp);if(!_pu[0]){return E(_pr);}else{if(E(E(_pu[1])[1])==40){var _pv=E(_pu[2]);return _pv[0]==0?E(_pr):E(E(_pv[1])[1])==44?function(_pw){return [1,_p2,new T(function(){return A(new T(function(){var _px=_6U(_p3,_pl);if(!_px[0]){return E(_8);}else{var _py=new T(function(){return _pd(_pc,_px[2]);});return function(_bL){return _oP([1,_px[1],_py],_bL);};}}),[[1,_pa,_pw]]);})];}:E(_pr);}else{return E(_pr);}}}),_pz=E(_pl[2]);if(!_pz[0]){var _pA=E(_pk),_pB=E(_oZ),_pC=hs_eqWord64(_pA[1],_pB[1]);if(!E(_pC)){return E(_po);}else{var _pD=hs_eqWord64(_pA[2],_pB[2]);if(!E(_pD)){return E(_po);}else{var _pE=new T(function(){return _oy(_oq,_pn);});return function(_pF){return [1,_p9,new T(function(){return A(_pE,[[1,_p8,_pF]]);})];};}}}else{if(!E(_pz[2])[0]){var _pG=E(_pk),_pH=E(_oN),_pI=hs_eqWord64(_pG[1],_pH[1]);if(!E(_pI)){return E(_po);}else{var _pJ=hs_eqWord64(_pG[2],_pH[2]);if(!E(_pJ)){return E(_po);}else{var _pK=new T(function(){return _oy(_p5,_pz[1]);}),_pL=new T(function(){return _oy(_p7,_pn);});return E(_ph)[1]<=8?function(_pM){return A(_pL,[new T(function(){return _1f(_p6,new T(function(){return A(_pK,[_pM]);}));})]);}:function(_pN){return [1,_3e,new T(function(){return A(_pL,[new T(function(){return _1f(_p6,new T(function(){return A(_pK,[[1,_3d,_pN]]);}));})]);})];};}}}else{return E(_po);}}}},_pO=function(_pP,_pQ,_pR,_pS){var _pT=new T(function(){return _30(_pP);}),_pU=new T(function(){return _o7(_pS);}),_pV=new T(function(){return _om(_pS);}),_pW=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_oy,[_oq,A(_pQ,[_ol]),_R]);}));}),_pX=new T(function(){return A(_oo,[_pR,_3D]);});return function(_pY){if(!E(new T(function(){var _pZ=A(_pQ,[_ol]),_q0=E(_ok),_q1=hs_eqWord64(_pZ[1],_q0[1]);if(!E(_q1)){return false;}else{var _q2=hs_eqWord64(_pZ[2],_q0[2]);return E(_q2)==0?false:true;}}))){var _q3=new T(function(){return A(_pT,[[1,_pY,new T(function(){return A(_pV,[new T(function(){return A(_pU,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1f(_pY,_pW);}));})]);})]);})]]);}),_q4=A(_pX,[_pY]);if(!_q4[0]){return E(_q3);}else{var _q5=E(_q4[1]);return E(_q5[2])[0]==0?E(_q4[2])[0]==0?A(_pT,[[2,_q5[1]]]):E(_q3):E(_q3);}}else{return A(_pT,[[2,_pY]]);}};},_q6=[0],_q7=new T(function(){return [0,"value"];}),_q8=function(_q9,_qa,_qb,_qc,_qd,_qe){var _qf=E(_q9),_qg=_qf[1],_qh=new T(function(){return A(_qf[3],[_q6]);}),_qi=new T(function(){return _pO(_qf,_qb,_qc,_qd);});return A(_qg,[new T(function(){return _o4(_qa,_qe);}),function(_qj){var _qk=E(_qj);return _qk[0]==0?E(_qh):A(_qg,[new T(function(){return A(_qa,[function(_){var _ql=jsGet(E(_qk[1])[1],E(_q7)[1]);return [1,new T(function(){return fromJSStr(_ql);})];}]);}),function(_qm){var _qn=E(_qm);return _qn[0]==0?E(_qh):A(_qi,[_qn[1]]);}]);}]);},_qo=1,_qp=function(_qq){return E(E(_qq)[10]);},_qr=function(_qs,_qt){return A(_30,[_qs,[0,_qt,_qt]]);},_qu=function(_qv){return E(E(_qv)[2]);},_qw=function(_qx,_qy,_qz){return A(_30,[_qx,[0,_1,_qy]]);},_qA=function(_qB){return E(E(_qB)[2]);},_qC=function(_qD,_qE,_qF,_qG,_qH){var _qI=new T(function(){return _nP(_qD);}),_qJ=new T(function(){return _qu(_qI);}),_qK=new T(function(){return _nR(_qE);}),_qL=new T(function(){return _32(_qK);}),_qM=new T(function(){return _3k([0,coercionToken],_qL,function(_qN){return _qr(_qK,_qN);},function(_qO,_qP){return _qw(_qK,_qO,_qP);});}),_qQ=new T(function(){return _30(_qK);}),_qR=new T(function(){return _2A(_qK);}),_qS=new T(function(){return _30(_qK);}),_qT=new T(function(){return _2A(_qK);}),_qU=new T(function(){return _30(_qK);}),_qV=new T(function(){return _2A(_qK);}),_qW=new T(function(){return _30(_qK);}),_qX=new T(function(){return _2A(_qK);}),_qY=new T(function(){return _qA(_qG);}),_qZ=new T(function(){return _qp(_qD);});return function(_r0,_r1,_r2){return function(_r3){return A(_qX,[new T(function(){var _r4=E(_r0);return _r4[0]==0?A(_qM,[_r3]):A(_qW,[[0,_r4[1],_r3]]);}),function(_r5){var _r6=new T(function(){return E(E(_r5)[1]);}),_r7=new T(function(){return _q8(_qL,function(_r8){return _nV(_qE,_r8);},_qF,_qH,_qD,_r6);}),_r9=new T(function(){return A(_qZ,[_r6,_r1,new T(function(){var _ra=E(_r2);if(!_ra[0]){return [0];}else{var _rb=_ra[1],_rc=_Z(_qF,_oj,_rb);return _rc[0]==0?A(_qY,[_rb]):E(_rc[1]);}}),_dF,_2p]);});return A(_qV,[new T(function(){var _rd=new T(function(){return E(E(_r5)[2]);});return A(_qU,[[0,_rd,_rd]]);}),function(_re){return A(_qT,[new T(function(){return A(_qS,[[0,_1,new T(function(){var _rf=E(E(_re)[1]);return [0,_rf[1],_rf[2],_qo,_rf[4],_rf[5]];})]]);}),function(_rg){return A(_qR,[new T(function(){return A(_r7,[new T(function(){return E(E(_rg)[2]);})]);}),function(_rh){var _ri=E(_rh),_rj=_ri[2],_rk=E(_ri[1]);switch(_rk[0]){case 0:return A(_qQ,[[0,[0,_r9,_2p],_rj]]);case 1:return A(_qQ,[[0,[0,new T(function(){return A(_qJ,[new T(function(){return A(_qZ,[_r6,_r1,_rk[1],_dF,_2p]);}),_rk[2]]);}),_2p],_rj]]);default:var _rl=_rk[1];return A(_qQ,[[0,[0,new T(function(){return A(_qZ,[_r6,_r1,new T(function(){var _rm=_Z(_qF,_oj,_rl);return _rm[0]==0?A(_qY,[_rl]):E(_rm[1]);}),_dF,_2p]);}),[1,_rl]],_rj]]);}}]);}]);}]);}]);};};},_rn=new T(function(){return _qC(_9d,_9e,_nN,_nI,_nx);}),_ro=new T(function(){return A(_rn,[_2p,_6b,_2p]);}),_rp=function(_rq){return E(E(_rq)[1]);},_rr=function(_rs,_rt,_ru,_rv,_rw){var _rx=new T(function(){return _nP(_rs);}),_ry=new T(function(){return _rp(_rx);}),_rz=new T(function(){return _30(_rt);}),_rA=new T(function(){return _om(_rs);}),_rB=new T(function(){return _2A(_rt);}),_rC=new T(function(){return _30(_rt);}),_rD=new T(function(){return _2A(_rt);});return A(_ru,[function(_rE){return A(_rD,[new T(function(){return A(_rv,[_rE]);}),function(_rF){var _rG=E(_rF),_rH=E(_rG[1]);return A(_rC,[[0,[0,_rH[1],[1,_rH[2]]],_rG[2]]]);}]);},function(_rI){var _rJ=E(_rI);if(!_rJ[0]){return function(_rK){return A(_rz,[[0,[0,_ry,_2p],_rK]]);};}else{var _rL=new T(function(){return A(_rw,[_rJ[1]]);});return function(_rM){return A(_rB,[new T(function(){return A(_rL,[_rM]);}),function(_rN){var _rO=E(_rN),_rP=_rO[2],_rQ=E(_rO[1]);return _rQ[0]==0?A(_rz,[[0,[0,_ry,_rJ],_rP]]):A(_rz,[[0,[0,new T(function(){return A(_rA,[_rQ[1]]);}),_2p],_rP]]);}]);};}}]);},_rR=unCStr("more than 2"),_rS=new T(function(){return _56(_4K,_rR);}),_rT=[1,_rS],_rU=function(_rV,_rW,_){return E(_rV)[1]>=3?[0,_rT,_rW]:[0,_2p,_rW];},_rX=new T(function(){return A(_rn,[_2p,_6b,_2p]);}),_rY=function(_rZ,_){var _s0=E(_rZ),_s1=E(_s0[4]),_s2=A(_rX,[_s0,_]),_s3=E(_s2),_s4=E(_s3[1]);return [0,[0,function(_s5,_){return _5S(_s4[1],_5q,function(_){var _s6=A(_s1[1],[_]),_s7=E(_s6);if(!_s7[0]){return _1;}else{var _s8=A(_s1[2],[_s7[1],_]);return _1;}},_s5,_);},_s4[2]],_s3[2]];},_s9=new T(function(){return _rr(_9d,_2z,_4i,_rY,_rU);}),_sa=unCStr("second number "),_sb=function(_sc,_){var _sd=A(_s9,[_sc,_]),_se=E(_sd),_sf=E(_se[1]),_sg=E(_se[2]),_sh=E(_sg[4]),_si=A(_ro,[_sg,_]),_sj=E(_si),_sk=E(_sj[1]);return [0,[0,function(_sl,_){var _sm=_4K(_6a,_sl,_),_sn=_66(_sl,_),_so=A(_sf[1],[_sl,_]),_sp=_66(_sl,_),_sq=_4K(_sa,_sl,_),_sr=_66(_sl,_),_ss=_5S(_sk[1],_5q,function(_){var _st=A(_sh[1],[_]),_su=E(_st);if(!_su[0]){return _1;}else{var _sv=A(_sh[2],[_su[1],_]);return _1;}},_sl,_),_sw=_66(_sl,_);return _sl;},new T(function(){var _sx=E(_sf[2]);if(!_sx[0]){return [0];}else{var _sy=E(_sk[2]);return _sy[0]==0?[0]:[1,new T(function(){return _5r(_sx[1],_sy[1]);})];}})],_sj[2]];},_sz=function(_sA,_){var _sB=E(_3K)[1],_sC=takeMVar(_sB),_sD=_4i(_sb,_5j,_sC,_),_sE=E(_sD),_sF=E(_sE[1]),_=putMVar(_sB,_sE[2]),_sG=A(_51,[_sA,_]),_sH=A(_4S,[_5h,_sF[1],_sA,_]);return _sF[2];},_sI=unCStr("idelem"),_sJ=function(_){var _sK=E(_sI),_sL=jsFind(toJSStr(_sK)),_sM=E(_sL);return _sM[0]==0?_3M(_sK):_sz(_sM[1],_);},_sN=function(_){return _sJ(_);};
var hasteMain = function() {A(_sN, [0]);};window.onload = hasteMain;