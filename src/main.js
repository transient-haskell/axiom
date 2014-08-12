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

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=[0,0],_9=[0],_a=function(_b,_){return _9;},_c=function(_){return _9;},_d=[0,_c,_a],_e=2,_f=[1],_g=[0],_h=[0,_g,_8,_e,_d,_0,_f],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=function(_A,_B,_){var _C=jsCreateTextNode(toJSStr(E(_A))),_D=jsAppendChild(_C,E(_B)[1]);return [0,_C];},_E=[0,34],_F=unCStr("br"),_G=function(_H,_){var _I=jsCreateElem(toJSStr(E(_F))),_J=jsAppendChild(_I,E(_H)[1]);return [0,_I];},_K=[1,_E,_g],_L=unCStr("hello, what is your name?"),_M=unCStr("div"),_N=function(_O,_P){var _Q=new T(function(){return A(_O,[_P]);});return function(_R,_){var _S=jsCreateElem(toJSStr(E(_M))),_T=jsAppendChild(_S,E(_R)[1]),_U=[0,_S],_V=A(_Q,[_U,_]);return _U;};},_W=new T(function(){return [0,"(function(){return document.body;})"];}),_X=unCStr("id"),_Y=unCStr("console"),_Z=function(_10,_){return _10;},_11=function(_12){return E(_12);},_13=function(_){var _14=E(_Y),_15=jsFind(toJSStr(_14)),_16=E(_15);if(!_16[0]){var _17=A(_5,[_W,_]),_18=A(_N,[_11,_Z,[0,_17],_]),_19=E(_18),_1a=jsSetAttr(_19[1],toJSStr(E(_X)),toJSStr(_14));return _19;}else{return _16[1];}},_1b=unCStr("id"),_1c=0,_1d=function(_1e,_1f,_1g,_1h){return A(_1e,[new T(function(){return function(_){var _1i=jsSetAttr(E(_1f)[1],toJSStr(E(_1g)),toJSStr(E(_1h)));return _1c;};})]);},_1j=function(_1k){return E(_1k);},_1l=function(_1m,_1n,_1o,_){var _1p=E(_1n),_1q=A(_1m,[_1o,_]),_1r=A(_1d,[_1j,_1q,_1p[1],_1p[2],_]);return _1q;},_1s=function(_1t,_1u){while(1){var _1v=(function(_1w,_1x){var _1y=E(_1x);if(!_1y[0]){return E(_1w);}else{_1t=function(_1z,_){return _1l(_1w,_1y[1],_1z,_);};_1u=_1y[2];return null;}})(_1t,_1u);if(_1v!=null){return _1v;}}},_1A=function(_1B,_1C,_){return [0,_1c,_1B];},_1D=function(_1E,_){return [0,_1E,_1E];},_1F=[0,coercionToken],_1G=function(_1H,_1I,_){var _1J=A(_1H,[_]);return A(_1I,[_]);},_1K=function(_1L,_1M,_){return _1G(_1L,_1M,_);},_1N=function(_1O,_1P,_){var _1Q=A(_1O,[_]);return A(_1P,[_1Q,_]);},_1R=unCStr("base"),_1S=unCStr("GHC.IO.Exception"),_1T=unCStr("IOException"),_1U=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1R,_1S,_1T],_1V=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1U,_g],_1W=function(_1X){return E(_1V);},_1Y=function(_1Z){return E(E(_1Z)[1]);},_20=unCStr("Maybe.fromJust: Nothing"),_21=new T(function(){return err(_20);}),_22=function(_23,_24,_25){var _26=new T(function(){var _27=A(_23,[_25]),_28=A(_24,[new T(function(){var _29=E(_26);return _29[0]==0?E(_21):E(_29[1]);})]),_2a=hs_eqWord64(_27[1],_28[1]);if(!E(_2a)){return [0];}else{var _2b=hs_eqWord64(_27[2],_28[2]);return E(_2b)==0?[0]:[1,_25];}});return E(_26);},_2c=function(_2d){var _2e=E(_2d);return _22(_1Y(_2e[1]),_1W,_2e[2]);},_2f=unCStr(": "),_2g=[0,41],_2h=unCStr(" ("),_2i=function(_2j,_2k){var _2l=E(_2j);return _2l[0]==0?E(_2k):[1,_2l[1],new T(function(){return _2i(_2l[2],_2k);})];},_2m=unCStr("already exists"),_2n=unCStr("does not exist"),_2o=unCStr("protocol error"),_2p=unCStr("failed"),_2q=unCStr("invalid argument"),_2r=unCStr("inappropriate type"),_2s=unCStr("hardware fault"),_2t=unCStr("unsupported operation"),_2u=unCStr("timeout"),_2v=unCStr("resource vanished"),_2w=unCStr("interrupted"),_2x=unCStr("resource busy"),_2y=unCStr("resource exhausted"),_2z=unCStr("end of file"),_2A=unCStr("illegal operation"),_2B=unCStr("permission denied"),_2C=unCStr("user error"),_2D=unCStr("unsatisified constraints"),_2E=unCStr("system error"),_2F=function(_2G,_2H){switch(E(_2G)){case 0:return _2i(_2m,_2H);case 1:return _2i(_2n,_2H);case 2:return _2i(_2x,_2H);case 3:return _2i(_2y,_2H);case 4:return _2i(_2z,_2H);case 5:return _2i(_2A,_2H);case 6:return _2i(_2B,_2H);case 7:return _2i(_2C,_2H);case 8:return _2i(_2D,_2H);case 9:return _2i(_2E,_2H);case 10:return _2i(_2o,_2H);case 11:return _2i(_2p,_2H);case 12:return _2i(_2q,_2H);case 13:return _2i(_2r,_2H);case 14:return _2i(_2s,_2H);case 15:return _2i(_2t,_2H);case 16:return _2i(_2u,_2H);case 17:return _2i(_2v,_2H);default:return _2i(_2w,_2H);}},_2I=[0,125],_2J=unCStr("{handle: "),_2K=function(_2L,_2M,_2N,_2O,_2P,_2Q){var _2R=new T(function(){var _2S=new T(function(){return _2F(_2M,new T(function(){var _2T=E(_2O);return _2T[0]==0?E(_2Q):_2i(_2h,new T(function(){return _2i(_2T,[1,_2g,_2Q]);}));}));}),_2U=E(_2N);return _2U[0]==0?E(_2S):_2i(_2U,new T(function(){return _2i(_2f,_2S);}));}),_2V=E(_2P);if(!_2V[0]){var _2W=E(_2L);if(!_2W[0]){return E(_2R);}else{var _2X=E(_2W[1]);return _2X[0]==0?_2i(_2J,new T(function(){return _2i(_2X[1],[1,_2I,new T(function(){return _2i(_2f,_2R);})]);})):_2i(_2J,new T(function(){return _2i(_2X[1],[1,_2I,new T(function(){return _2i(_2f,_2R);})]);}));}}else{return _2i(_2V[1],new T(function(){return _2i(_2f,_2R);}));}},_2Y=function(_2Z){var _30=E(_2Z);return _2K(_30[1],_30[2],_30[3],_30[4],_30[6],_g);},_31=function(_32,_33){var _34=E(_32);return _2K(_34[1],_34[2],_34[3],_34[4],_34[6],_33);},_35=[0,44],_36=[0,93],_37=[0,91],_38=function(_39,_3a,_3b){var _3c=E(_3a);return _3c[0]==0?unAppCStr("[]",_3b):[1,_37,new T(function(){return A(_39,[_3c[1],new T(function(){var _3d=function(_3e){var _3f=E(_3e);return _3f[0]==0?E([1,_36,_3b]):[1,_35,new T(function(){return A(_39,[_3f[1],new T(function(){return _3d(_3f[2]);})]);})];};return _3d(_3c[2]);})]);})];},_3g=function(_3h,_3i){return _38(_31,_3h,_3i);},_3j=function(_3k,_3l,_3m){var _3n=E(_3l);return _2K(_3n[1],_3n[2],_3n[3],_3n[4],_3n[6],_3m);},_3o=[0,_3j,_2Y,_3g],_3p=new T(function(){return [0,_1W,_3o,_3q,_2c];}),_3q=function(_3r){return [0,_3p,_3r];},_3s=7,_3t=function(_3u){return [0,_9,_3s,_g,_3u,_9,_9];},_3v=function(_3w,_){return die(new T(function(){return _3q(new T(function(){return _3t(_3w);}));}));},_3x=function(_3y,_){return _3v(_3y,_);},_3z=[0,_1N,_1K,_Z,_3x],_3A=function(_3B){return E(E(_3B)[1]);},_3C=function(_3D,_3E,_3F,_3G){return A(_3A,[_3D,new T(function(){return A(_3E,[_3G]);}),function(_3H){return A(_3F,[new T(function(){return E(E(_3H)[1]);}),new T(function(){return E(E(_3H)[2]);})]);}]);},_3I=function(_3J,_3K,_3L,_3M){return A(_3A,[_3J,new T(function(){return A(_3K,[_3M]);}),function(_3N){return A(_3L,[new T(function(){return E(E(_3N)[2]);})]);}]);},_3O=function(_3P,_3Q,_3R,_3S){return _3I(_3P,_3Q,_3R,_3S);},_3T=function(_3U){return E(E(_3U)[4]);},_3V=function(_3W,_3X){var _3Y=new T(function(){return A(_3T,[_3W,_3X]);});return function(_3Z){return E(_3Y);};},_40=function(_41){return E(E(_41)[3]);},_42=function(_43){var _44=new T(function(){return _40(_43);});return [0,function(_3Q,_3R,_3S){return _3C(_43,_3Q,_3R,_3S);},function(_3Q,_3R,_3S){return _3O(_43,_3Q,_3R,_3S);},function(_45,_46){return A(_44,[[0,_45,_46]]);},function(_3S){return _3V(_43,_3S);}];},_47=new T(function(){return _42(_3z);}),_48=[0,112],_49=function(_4a,_4b){var _4c=jsShowI(_4a);return _2i(fromJSStr(_4c),_4b);},_4d=[0,41],_4e=[0,40],_4f=function(_4g,_4h,_4i){return _4h>=0?_49(_4h,_4i):_4g<=6?_49(_4h,_4i):[1,_4e,new T(function(){var _4j=jsShowI(_4h);return _2i(fromJSStr(_4j),[1,_4d,_4i]);})];},_4k=function(_4l,_4m,_4n,_4o){var _4p=E(_4m);return A(_4p[1],[new T(function(){var _4q=E(_4l);return E(_4n);}),function(_4r){var _4s=new T(function(){return E(E(_4r)[2]);});return A(_4p[2],[new T(function(){return A(_4o,[new T(function(){var _4t=E(new T(function(){var _4u=E(_4l);return [0,coercionToken];})),_4v=E(_4r);return [0,_4v[1],new T(function(){return [0,E(_4s)[1]+1|0];}),_4v[3],_4v[4],_4v[5],_4v[6]];})]);}),new T(function(){return A(_4p[3],[[1,_48,new T(function(){return _2i(_4f(0,E(_4s)[1],_g),new T(function(){return E(E(_4r)[1]);}));})]]);})]);}]);},_4w=new T(function(){return _4k(_1F,_47,_1D,_1A);}),_4x=unCStr("span"),_4y=function(_4z,_4A,_){var _4B=jsCreateElem(toJSStr(E(_4z))),_4C=jsAppendChild(_4B,E(_4A)[1]);return [0,_4B];},_4D=function(_1z,_){return _4y(_4x,_1z,_);},_4E=unCStr(" could be found!"),_4F=function(_4G){return err(unAppCStr("No element with ID ",new T(function(){return _2i(_4G,_4E);})));},_4H=function(_4I,_4J,_){var _4K=E(_4J),_4L=jsFind(toJSStr(_4K)),_4M=E(_4L);if(!_4M[0]){return _4F(_4K);}else{var _4N=E(_4M[1]),_4O=jsClearChildren(_4N[1]);return _l(_4I,_4N,_);}},_4P=function(_4Q,_4R,_4S,_){var _4T=A(_4w,[_4S,_]),_4U=E(_4T),_4V=_4U[1],_4W=E(_4U[2]),_4X=_4W[2],_4Y=E(_4W[4]),_4Z=A(_4Q,[[0,_4W[1],_4X,_4W[3],[0,function(_){return _4H(function(_50,_){var _51=A(_4Q,[new T(function(){var _52=E(_50);return [0,_52[1],_4X,_52[3],_52[4],_52[5],_52[6]];}),_]);return [0,[0,_Z,E(E(_51)[1])[2]],_50];},_4V,_);},function(_53,_){var _54=_4H(new T(function(){return A(_4R,[_53]);}),_4V,_),_55=E(_54);return _55[0]==0?_9:A(_4Y[2],[_55[1],_]);}],_4W[5],_4W[6]],_]),_56=E(_4Z),_57=_56[2],_58=E(_56[1]),_59=_58[1],_5a=new T(function(){return _1s(_4D,[1,[0,_1b,_4V],_g]);}),_5b=E(_58[2]);if(!_5b[0]){return [0,[0,function(_5c,_){var _5d=A(_59,[_5c,_]),_5e=A(_5a,[_5c,_]);return _5c;},_9],new T(function(){var _5f=E(_57);return [0,_5f[1],_5f[2],_5f[3],_4Y,_5f[5],_5f[6]];})];}else{var _5g=A(_4R,[_5b[1],new T(function(){var _5h=E(_57);return [0,_5h[1],_5h[2],_5h[3],_4Y,_5h[5],_5h[6]];}),_]),_5i=E(_5g),_5j=E(_5i[1]);return [0,[0,function(_5k,_){var _5l=A(_59,[_5k,_]),_5m=A(_5a,[_5k,_]),_5n=A(_5j[1],[_5m,_]);return _5k;},_5j[2]],_5i[2]];}},_5o=[0],_5p=unCStr("OnLoad"),_5q=[0,_5p,_5o],_5r=function(_){var _=0,_5s=newMVar(),_=putMVar(_5s,_5q);return [0,_5s];},_5t=new T(function(){return _2(_5r);}),_5u=function(_){var _5v=E(_5t)[1],_5w=takeMVar(_5v),_=putMVar(_5v,_5w);return _5w;},_5x=function(_5y,_){var _5z=0;if(!E(_5z)){var _5A=_5u();return [0,[0,_Z,[1,_5A]],_5y];}else{var _5B=E(_5t)[1],_5C=takeMVar(_5B),_=putMVar(_5B,_5C);return [0,[0,_Z,[1,_5C]],_5y];}},_5D=[13,coercionToken],_5E=function(_5F,_5G,_5H,_){var _5I=_4y(_5F,_5H,_),_5J=A(_5G,[_5I,_]);return _5I;},_5K=unCStr("()"),_5L=unCStr("GHC.Tuple"),_5M=unCStr("ghc-prim"),_5N=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5M,_5L,_5K],_5O=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5N,_g],_5P=function(_5Q){return E(_5O);},_5R=unCStr("haste-perch-0.1.0.1"),_5S=unCStr("Haste.Perch"),_5T=unCStr("PerchM"),_5U=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5R,_5S,_5T],_5V=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5U,_g],_5W=function(_5X){return E(_5V);},_5Y=function(_5Z){var _60=E(_5Z);return _60[0]==0?[0]:_2i(_60[1],new T(function(){return _5Y(_60[2]);}));},_61=function(_62,_63){var _64=E(_62);if(!_64){return [0,_g,_63];}else{var _65=E(_63);if(!_65[0]){return [0,_g,_g];}else{var _66=new T(function(){var _67=_61(_64-1|0,_65[2]);return [0,_67[1],_67[2]];});return [0,[1,_65[1],new T(function(){return E(E(_66)[1]);})],new T(function(){return E(E(_66)[2]);})];}}},_68=[0,120],_69=[0,48],_6a=function(_6b){var _6c=new T(function(){var _6d=_61(8,new T(function(){var _6e=md5(toJSStr(E(_6b)));return fromJSStr(_6e);}));return [0,_6d[1],_6d[2]];}),_6f=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6c)[1]);})]])]),_6g=new T(function(){var _6h=_61(8,new T(function(){return E(E(_6c)[2]);}));return [0,_6h[1],_6h[2]];}),_6i=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6g)[1]);})]])]),_6j=hs_mkWord64(_6f,_6i),_6k=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(_61(8,new T(function(){return E(E(_6g)[2]);}))[1]);})]])]),_6l=hs_mkWord64(_6k,_6k);return [0,_6j,_6l];},_6m=function(_6n,_6o){var _6p=E(_6o);return _6p[0]==0?[0]:[1,new T(function(){return A(_6n,[_6p[1]]);}),new T(function(){return _6m(_6n,_6p[2]);})];},_6q=function(_6r,_6s){var _6t=jsShowI(_6r),_6u=md5(_6t);return _2i(fromJSStr(_6u),new T(function(){var _6v=jsShowI(_6s),_6w=md5(_6v);return fromJSStr(_6w);}));},_6x=function(_6y){var _6z=E(_6y);return _6q(_6z[1],_6z[2]);},_6A=function(_6B){var _6C=E(_6B);if(!_6C[0]){return [0];}else{var _6D=E(_6C[1]);return [1,[0,_6D[1],_6D[2]],new T(function(){return _6A(_6C[2]);})];}},_6E=unCStr("Prelude.undefined"),_6F=new T(function(){return err(_6E);}),_6G=function(_6H,_6I){return function(_6J){return E(new T(function(){var _6K=A(_6H,[_6F]),_6L=E(_6K[3]),_6M=_6L[1],_6N=_6L[2],_6O=_2i(_6K[4],[1,new T(function(){return A(_6I,[_6F]);}),_g]);if(!_6O[0]){return [0,_6M,_6N,_6L,_g];}else{var _6P=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_6M,_6N],new T(function(){return _6A(_6O);})]));}));return [0,_6P[1],_6P[2],_6L,_6O];}}));};},_6Q=new T(function(){return _6G(_5W,_5P);}),_6R=unCStr("value"),_6S=unCStr("onclick"),_6T=unCStr("checked"),_6U=[0,_6T,_g],_6V=[1,_6U,_g],_6W=unCStr("type"),_6X=unCStr("input"),_6Y=function(_6Z,_){return _4y(_6X,_6Z,_);},_70=function(_71,_72,_73,_74,_75){var _76=new T(function(){var _77=new T(function(){return _1s(_6Y,[1,[0,_6W,_72],[1,[0,_1b,_71],[1,[0,_6R,_73],_g]]]);});return !E(_74)?E(_77):_1s(_77,_6V);}),_78=E(_75);return _78[0]==0?E(_76):_1s(_76,[1,[0,_6S,_78[1]],_g]);},_79=unCStr("href"),_7a=[0,97],_7b=[1,_7a,_g],_7c=function(_7d,_){return _4y(_7b,_7d,_);},_7e=function(_7f,_7g){var _7h=new T(function(){return _1s(_7c,[1,[0,_79,_7f],_g]);});return function(_7i,_){var _7j=A(_7h,[_7i,_]),_7k=A(_7g,[_7j,_]);return _7j;};},_7l=function(_7m){return _7e(_7m,function(_1z,_){return _z(_7m,_1z,_);});},_7n=unCStr("option"),_7o=function(_7p,_){return _4y(_7n,_7p,_);},_7q=unCStr("selected"),_7r=[0,_7q,_g],_7s=[1,_7r,_g],_7t=function(_7u,_7v,_7w){var _7x=new T(function(){return _1s(_7o,[1,[0,_6R,_7u],_g]);}),_7y=function(_7z,_){var _7A=A(_7x,[_7z,_]),_7B=A(_7v,[_7A,_]);return _7A;};return !E(_7w)?E(_7y):_1s(_7y,_7s);},_7C=function(_7D,_7E){return _7t(_7D,function(_1z,_){return _z(_7D,_1z,_);},_7E);},_7F=unCStr("method"),_7G=unCStr("action"),_7H=unCStr("UTF-8"),_7I=unCStr("acceptCharset"),_7J=[0,_7I,_7H],_7K=unCStr("form"),_7L=function(_7M,_){return _4y(_7K,_7M,_);},_7N=function(_7O,_7P,_7Q){var _7R=new T(function(){return _1s(_7L,[1,_7J,[1,[0,_7G,_7O],[1,[0,_7F,_7P],_g]]]);});return function(_7S,_){var _7T=A(_7R,[_7S,_]),_7U=A(_7Q,[_7T,_]);return _7T;};},_7V=unCStr("select"),_7W=function(_7X,_){return _4y(_7V,_7X,_);},_7Y=function(_7Z,_80){var _81=new T(function(){return _1s(_7W,[1,[0,_1b,_7Z],_g]);});return function(_82,_){var _83=A(_81,[_82,_]),_84=A(_80,[_83,_]);return _83;};},_85=unCStr("textarea"),_86=function(_87,_){return _4y(_85,_87,_);},_88=function(_89,_8a){var _8b=new T(function(){return _1s(_86,[1,[0,_1b,_89],_g]);});return function(_8c,_){var _8d=A(_8b,[_8c,_]),_8e=_z(_8a,_8d,_);return _8d;};},_8f=unCStr("color:red"),_8g=unCStr("style"),_8h=[0,_8g,_8f],_8i=[1,_8h,_g],_8j=[0,98],_8k=[1,_8j,_g],_8l=function(_8m){return _1s(function(_8n,_){var _8o=_4y(_8k,_8n,_),_8p=A(_8m,[_8o,_]);return _8o;},_8i);},_8q=function(_8r,_8s,_){var _8t=E(_8r);if(!_8t[0]){return _8s;}else{var _8u=A(_8t[1],[_8s,_]),_8v=_8q(_8t[2],_8s,_);return _8s;}},_8w=function(_8x,_8y,_8z,_){var _8A=A(_8x,[_8z,_]),_8B=A(_8y,[_8z,_]);return _8z;},_8C=[0,_Z,_8w,_8q],_8D=[0,_8C,_6Q,_z,_z,_5E,_8l,_7e,_7l,_70,_88,_7Y,_7t,_7C,_7N,_1s],_8E=[0,_3z,_1j],_8F=function(_8G){return E(E(_8G)[1]);},_8H=function(_8I){return E(E(_8I)[1]);},_8J=function(_8K){return E(E(_8K)[2]);},_8L=function(_8M,_8N){var _8O=new T(function(){return A(_8J,[_8M,_8N]);}),_8P=new T(function(){return _8H(_8M);}),_8Q=new T(function(){return _40(_8P);}),_8R=new T(function(){return _3A(_8P);});return function(_8S){return A(_8R,[_8O,function(_8T){return A(_8Q,[[0,_8T,_8S]]);}]);};},_8U=function(_8V,_8W){return A(_8V,[function(_){return jsFind(toJSStr(E(_8W)));}]);},_8X=[0],_8Y=function(_8Z){return E(E(_8Z)[3]);},_90=new T(function(){return E(_6F);}),_91=new T(function(){return [0,"value"];}),_92=function(_93){return E(E(_93)[6]);},_94=unCStr("GHC.Types"),_95=unCStr("[]"),_96=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5M,_94,_95],_97=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_96,_g],_98=function(_99){return E(_97);},_9a=unCStr("Char"),_9b=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5M,_94,_9a],_9c=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_9b,_g],_9d=function(_9e){return E(_9c);},_9f=new T(function(){return _6G(_98,_9d);}),_9g=new T(function(){return A(_9f,[_6F]);}),_9h=function(_9i){return E(E(_9i)[1]);},_9j=[0,0],_9k=[0,32],_9l=[0,10],_9m=function(_9n){var _9o=E(_9n);if(!_9o[0]){return E(_1j);}else{var _9p=_9o[1],_9q=E(_9o[2]);if(!_9q[0]){return _9r(_9l,_9p);}else{var _9s=new T(function(){return _9m(_9q);}),_9t=new T(function(){return _9r(_9l,_9p);});return function(_9u){return A(_9t,[[1,_9k,new T(function(){return A(_9s,[_9u]);})]]);};}}},_9v=unCStr("->"),_9w=[1,_9v,_g],_9x=[1,_94,_9w],_9y=[1,_5M,_9x],_9z=[0,32],_9A=function(_9B){var _9C=E(_9B);if(!_9C[0]){return [0];}else{var _9D=_9C[1],_9E=E(_9C[2]);return _9E[0]==0?E(_9D):_2i(_9D,[1,_9z,new T(function(){return _9A(_9E);})]);}},_9F=new T(function(){return _9A(_9y);}),_9G=new T(function(){var _9H=_6a(_9F);return [0,_9H[1],_9H[2],_5M,_94,_9v];}),_9I=function(_9J,_9K){var _9L=E(_9J);return _9L[0]==0?E(_9K):A(_9L[1],[new T(function(){return _9I(_9L[2],_9K);})]);},_9M=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_9N=[1,_5O,_g],_9O=function(_9P){var _9Q=E(_9P);if(!_9Q[0]){return [0];}else{var _9R=E(_9Q[1]);return [1,[0,_9R[1],_9R[2]],new T(function(){return _9O(_9Q[2]);})];}},_9S=new T(function(){var _9T=_2i(_g,_9N);if(!_9T[0]){return E(_96);}else{var _9U=_6a(new T(function(){return _5Y(_6m(_6x,[1,_9M,new T(function(){return _9O(_9T);})]));}));return E(_96);}}),_9V=[0,40],_9W=function(_9X){return _9r(_9l,_9X);},_9Y=[0,8],_9Z=unCStr(" -> "),_a0=[0,9],_a1=[0,93],_a2=[0,91],_a3=[0,41],_a4=[0,44],_a5=function(_9X){return [1,_a4,_9X];},_a6=function(_a7,_a8){var _a9=E(_a8);return _a9[0]==0?[0]:[1,_a7,[1,_a9[1],new T(function(){return _a6(_a7,_a9[2]);})]];},_9r=function(_aa,_ab){var _ac=E(_ab),_ad=_ac[3],_ae=E(_ac[4]);if(!_ae[0]){return function(_af){return _2i(E(_ad)[5],_af);};}else{var _ag=_ae[1],_ah=new T(function(){var _ai=E(_ad)[5],_aj=new T(function(){return _9m(_ae);}),_ak=new T(function(){return E(_aa)[1]<=9?function(_al){return _2i(_ai,[1,_9k,new T(function(){return A(_aj,[_al]);})]);}:function(_am){return [1,_4e,new T(function(){return _2i(_ai,[1,_9k,new T(function(){return A(_aj,[[1,_4d,_am]]);})]);})];};}),_an=E(_ai);if(!_an[0]){return E(_ak);}else{if(E(E(_an[1])[1])==40){var _ao=E(_an[2]);return _ao[0]==0?E(_ak):E(E(_ao[1])[1])==44?function(_ap){return [1,_9V,new T(function(){return A(new T(function(){var _aq=_6m(_9W,_ae);if(!_aq[0]){return E(_1j);}else{var _ar=new T(function(){return _a6(_a5,_aq[2]);});return function(_as){return _9I([1,_aq[1],_ar],_as);};}}),[[1,_a3,_ap]]);})];}:E(_ak);}else{return E(_ak);}}}),_at=E(_ae[2]);if(!_at[0]){var _au=E(_ad),_av=E(_9S),_aw=hs_eqWord64(_au[1],_av[1]);if(!E(_aw)){return E(_ah);}else{var _ax=hs_eqWord64(_au[2],_av[2]);if(!E(_ax)){return E(_ah);}else{var _ay=new T(function(){return _9r(_9j,_ag);});return function(_az){return [1,_a2,new T(function(){return A(_ay,[[1,_a1,_az]]);})];};}}}else{if(!E(_at[2])[0]){var _aA=E(_ad),_aB=E(_9G),_aC=hs_eqWord64(_aA[1],_aB[1]);if(!E(_aC)){return E(_ah);}else{var _aD=hs_eqWord64(_aA[2],_aB[2]);if(!E(_aD)){return E(_ah);}else{var _aE=new T(function(){return _9r(_9Y,_at[1]);}),_aF=new T(function(){return _9r(_a0,_ag);});return E(_aa)[1]<=8?function(_aG){return A(_aF,[new T(function(){return _2i(_9Z,new T(function(){return A(_aE,[_aG]);}));})]);}:function(_aH){return [1,_4e,new T(function(){return A(_aF,[new T(function(){return _2i(_9Z,new T(function(){return A(_aE,[[1,_4d,_aH]]);}));})]);})];};}}}else{return E(_ah);}}}},_aI=function(_aJ,_aK,_aL,_aM,_aN,_aO){var _aP=E(_aJ),_aQ=_aP[1],_aR=_aP[3],_aS=new T(function(){return A(_aR,[_8X]);}),_aT=new T(function(){return _8Y(_aN);}),_aU=new T(function(){return _92(_aN);}),_aV=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_9r,[_9j,A(_aL,[_90]),_g]);}));}),_aW=new T(function(){return A(_9h,[_aM,_8]);});return A(_aQ,[new T(function(){return _8U(_aK,_aO);}),function(_aX){var _aY=E(_aX);return _aY[0]==0?E(_aS):A(_aQ,[new T(function(){return A(_aK,[function(_){var _aZ=jsGet(E(_aY[1])[1],E(_91)[1]);return [1,new T(function(){return fromJSStr(_aZ);})];}]);}),function(_b0){var _b1=E(_b0);if(!_b1[0]){return E(_aS);}else{var _b2=_b1[1];if(!E(new T(function(){var _b3=A(_aL,[_90]),_b4=E(_9g),_b5=hs_eqWord64(_b3[1],_b4[1]);if(!E(_b5)){return false;}else{var _b6=hs_eqWord64(_b3[2],_b4[2]);return E(_b6)==0?false:true;}}))){var _b7=new T(function(){return A(_aR,[[1,_b2,new T(function(){return A(_aU,[new T(function(){return A(_aT,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _2i(_b2,_aV);}));})]);})]);})]]);}),_b8=A(_aW,[_b2]);if(!_b8[0]){return E(_b7);}else{var _b9=E(_b8[1]);return E(_b9[2])[0]==0?E(_b8[2])[0]==0?A(_aR,[[2,_b9[1]]]):E(_b7):E(_b7);}}else{return A(_aR,[[2,_b2]]);}}}]);}]);},_ba=1,_bb=function(_bc){return E(E(_bc)[9]);},_bd=function(_be,_bf){return A(_40,[_be,[0,_bf,_bf]]);},_bg=function(_bh){return E(E(_bh)[2]);},_bi=function(_bj,_bk,_bl){return A(_40,[_bj,[0,_1c,_bk]]);},_bm=function(_bn){return E(E(_bn)[2]);},_bo=function(_bp,_bq,_br,_bs,_bt){var _bu=new T(function(){return _8F(_bp);}),_bv=new T(function(){return _bg(_bu);}),_bw=new T(function(){return _8H(_bq);}),_bx=new T(function(){return _42(_bw);}),_by=new T(function(){return _4k([0,coercionToken],_bx,function(_bz){return _bd(_bw,_bz);},function(_bA,_bB){return _bi(_bw,_bA,_bB);});}),_bC=new T(function(){return _40(_bw);}),_bD=new T(function(){return _3A(_bw);}),_bE=new T(function(){return _40(_bw);}),_bF=new T(function(){return _3A(_bw);}),_bG=new T(function(){return _40(_bw);}),_bH=new T(function(){return _3A(_bw);}),_bI=new T(function(){return _40(_bw);}),_bJ=new T(function(){return _3A(_bw);}),_bK=new T(function(){return _bm(_bs);}),_bL=new T(function(){return _bb(_bp);});return function(_bM,_bN,_bO){return function(_bP){return A(_bJ,[new T(function(){var _bQ=E(_bM);return _bQ[0]==0?A(_by,[_bP]):A(_bI,[[0,_bQ[1],_bP]]);}),function(_bR){var _bS=new T(function(){return E(E(_bR)[1]);}),_bT=new T(function(){return _aI(_bx,function(_bU){return _8L(_bq,_bU);},_br,_bt,_bp,_bS);}),_bV=new T(function(){return A(_bL,[_bS,_bN,new T(function(){var _bW=E(_bO);if(!_bW[0]){return [0];}else{var _bX=_bW[1],_bY=_22(_br,_9f,_bX);return _bY[0]==0?A(_bK,[_bX]):E(_bY[1]);}}),_0,_9]);});return A(_bH,[new T(function(){var _bZ=new T(function(){return E(E(_bR)[2]);});return A(_bG,[[0,_bZ,_bZ]]);}),function(_c0){return A(_bF,[new T(function(){return A(_bE,[[0,_1c,new T(function(){var _c1=E(E(_c0)[1]);return [0,_c1[1],_c1[2],_ba,_c1[4],_c1[5],_c1[6]];})]]);}),function(_c2){return A(_bD,[new T(function(){return A(_bT,[new T(function(){return E(E(_c2)[2]);})]);}),function(_c3){var _c4=E(_c3),_c5=_c4[2],_c6=E(_c4[1]);switch(_c6[0]){case 0:return A(_bC,[[0,[0,_bV,_9],_c5]]);case 1:return A(_bC,[[0,[0,new T(function(){return A(_bv,[new T(function(){return A(_bL,[_bS,_bN,_c6[1],_0,_9]);}),_c6[2]]);}),_9],_c5]]);default:var _c7=_c6[1];return A(_bC,[[0,[0,new T(function(){return A(_bL,[_bS,_bN,new T(function(){var _c8=_22(_br,_9f,_c7);return _c8[0]==0?A(_bK,[_c7]):E(_c8[1]);}),_0,_9]);}),[1,_c7]],_c5]]);}}]);}]);}]);}]);};};},_c9=unCStr("base"),_ca=unCStr("Control.Exception.Base"),_cb=unCStr("PatternMatchFail"),_cc=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_c9,_ca,_cb],_cd=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_cc,_g],_ce=function(_cf){return E(_cd);},_cg=function(_ch){var _ci=E(_ch);return _22(_1Y(_ci[1]),_ce,_ci[2]);},_cj=function(_ck){return E(E(_ck)[1]);},_cl=function(_cm,_cn){return _2i(E(_cm)[1],_cn);},_co=function(_cp,_cq){return _38(_cl,_cp,_cq);},_cr=function(_cs,_ct,_cu){return _2i(E(_ct)[1],_cu);},_cv=[0,_cr,_cj,_co],_cw=new T(function(){return [0,_ce,_cv,_cx,_cg];}),_cx=function(_cy){return [0,_cw,_cy];},_cz=unCStr("Non-exhaustive patterns in"),_cA=function(_cB,_cC){return die(new T(function(){return A(_cC,[_cB]);}));},_cD=function(_cE,_cF){var _cG=E(_cF);if(!_cG[0]){return [0,_g,_g];}else{var _cH=_cG[1];if(!A(_cE,[_cH])){return [0,_g,_cG];}else{var _cI=new T(function(){var _cJ=_cD(_cE,_cG[2]);return [0,_cJ[1],_cJ[2]];});return [0,[1,_cH,new T(function(){return E(E(_cI)[1]);})],new T(function(){return E(E(_cI)[2]);})];}}},_cK=[0,32],_cL=[0,10],_cM=[1,_cL,_g],_cN=function(_cO){return E(E(_cO)[1])==124?false:true;},_cP=function(_cQ,_cR){var _cS=_cD(_cN,unCStr(_cQ)),_cT=_cS[1],_cU=function(_cV,_cW){return _2i(_cV,new T(function(){return unAppCStr(": ",new T(function(){return _2i(_cR,new T(function(){return _2i(_cW,_cM);}));}));}));},_cX=E(_cS[2]);return _cX[0]==0?_cU(_cT,_g):E(E(_cX[1])[1])==124?_cU(_cT,[1,_cK,_cX[2]]):_cU(_cT,_g);},_cY=function(_cZ){return _cA([0,new T(function(){return _cP(_cZ,_cz);})],_cx);},_d0=new T(function(){return _cY("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_d1=function(_d2,_d3){while(1){var _d4=(function(_d5,_d6){var _d7=E(_d5);switch(_d7[0]){case 0:var _d8=E(_d6);if(!_d8[0]){return [0];}else{_d2=A(_d7[1],[_d8[1]]);_d3=_d8[2];return null;}break;case 1:var _d9=A(_d7[1],[_d6]),_da=_d6;_d2=_d9;_d3=_da;return null;case 2:return [0];case 3:return [1,[0,_d7[1],_d6],new T(function(){return _d1(_d7[2],_d6);})];default:return E(_d7[1]);}})(_d2,_d3);if(_d4!=null){return _d4;}}},_db=function(_dc,_dd){var _de=new T(function(){var _df=E(_dd);if(_df[0]==3){return [3,_df[1],new T(function(){return _db(_dc,_df[2]);})];}else{var _dg=E(_dc);if(_dg[0]==2){return E(_df);}else{var _dh=E(_df);if(_dh[0]==2){return E(_dg);}else{var _di=new T(function(){var _dj=E(_dh);if(_dj[0]==4){return [1,function(_dk){return [4,new T(function(){return _2i(_d1(_dg,_dk),_dj[1]);})];}];}else{var _dl=E(_dg);if(_dl[0]==1){var _dm=_dl[1],_dn=E(_dj);return _dn[0]==0?[1,function(_do){return _db(A(_dm,[_do]),_dn);}]:[1,function(_dp){return _db(A(_dm,[_dp]),new T(function(){return A(_dn[1],[_dp]);}));}];}else{var _dq=E(_dj);return _dq[0]==0?E(_d0):[1,function(_dr){return _db(_dl,new T(function(){return A(_dq[1],[_dr]);}));}];}}}),_ds=E(_dg);switch(_ds[0]){case 1:var _dt=E(_dh);return _dt[0]==4?[1,function(_du){return [4,new T(function(){return _2i(_d1(A(_ds[1],[_du]),_du),_dt[1]);})];}]:E(_di);case 4:var _dv=_ds[1],_dw=E(_dh);switch(_dw[0]){case 0:return [1,function(_dx){return [4,new T(function(){return _2i(_dv,new T(function(){return _d1(_dw,_dx);}));})];}];case 1:return [1,function(_dy){return [4,new T(function(){return _2i(_dv,new T(function(){return _d1(A(_dw[1],[_dy]),_dy);}));})];}];default:return [4,new T(function(){return _2i(_dv,_dw[1]);})];}break;default:return E(_di);}}}}}),_dz=E(_dc);switch(_dz[0]){case 0:var _dA=E(_dd);return _dA[0]==0?[0,function(_dB){return _db(A(_dz[1],[_dB]),new T(function(){return A(_dA[1],[_dB]);}));}]:E(_de);case 3:return [3,_dz[1],new T(function(){return _db(_dz[2],_dd);})];default:return E(_de);}},_dC=function(_dD,_dE){return E(_dD)[1]!=E(_dE)[1];},_dF=function(_dG,_dH){return E(_dG)[1]==E(_dH)[1];},_dI=[0,_dF,_dC],_dJ=function(_dK){return E(E(_dK)[1]);},_dL=function(_dM,_dN,_dO){while(1){var _dP=E(_dN);if(!_dP[0]){return E(_dO)[0]==0?true:false;}else{var _dQ=E(_dO);if(!_dQ[0]){return false;}else{if(!A(_dJ,[_dM,_dP[1],_dQ[1]])){return false;}else{_dN=_dP[2];_dO=_dQ[2];continue;}}}}},_dR=function(_dS,_dT,_dU){return !_dL(_dS,_dT,_dU)?true:false;},_dV=function(_dW){return [0,function(_dX,_dY){return _dL(_dW,_dX,_dY);},function(_dX,_dY){return _dR(_dW,_dX,_dY);}];},_dZ=new T(function(){return _dV(_dI);}),_e0=function(_e1,_e2){var _e3=E(_e1);switch(_e3[0]){case 0:return [0,function(_e4){return _e0(A(_e3[1],[_e4]),_e2);}];case 1:return [1,function(_e5){return _e0(A(_e3[1],[_e5]),_e2);}];case 2:return [2];case 3:return _db(A(_e2,[_e3[1]]),new T(function(){return _e0(_e3[2],_e2);}));default:var _e6=function(_e7){var _e8=E(_e7);if(!_e8[0]){return [0];}else{var _e9=E(_e8[1]);return _2i(_d1(A(_e2,[_e9[1]]),_e9[2]),new T(function(){return _e6(_e8[2]);}));}},_ea=_e6(_e3[1]);return _ea[0]==0?[2]:[4,_ea];}},_eb=[2],_ec=function(_ed){return [3,_ed,_eb];},_ee=function(_ef,_eg){var _eh=E(_ef);if(!_eh){return A(_eg,[_1c]);}else{var _ei=new T(function(){return _ee(_eh-1|0,_eg);});return [0,function(_ej){return E(_ei);}];}},_ek=function(_el,_em,_en){var _eo=new T(function(){return A(_el,[_ec]);});return [1,function(_ep){return A(function(_eq,_er,_es){while(1){var _et=(function(_eu,_ev,_ew){var _ex=E(_eu);switch(_ex[0]){case 0:var _ey=E(_ev);if(!_ey[0]){return E(_em);}else{_eq=A(_ex[1],[_ey[1]]);_er=_ey[2];var _ez=_ew+1|0;_es=_ez;return null;}break;case 1:var _eA=A(_ex[1],[_ev]),_eB=_ev,_ez=_ew;_eq=_eA;_er=_eB;_es=_ez;return null;case 2:return E(_em);case 3:return function(_eC){var _eD=new T(function(){return _e0(_ex,_eC);});return _ee(_ew,function(_eE){return E(_eD);});};default:return function(_as){return _e0(_ex,_as);};}})(_eq,_er,_es);if(_et!=null){return _et;}}},[_eo,_ep,0,_en]);}];},_eF=[6],_eG=unCStr("valDig: Bad base"),_eH=new T(function(){return err(_eG);}),_eI=function(_eJ,_eK){var _eL=function(_eM,_eN){var _eO=E(_eM);if(!_eO[0]){var _eP=new T(function(){return A(_eN,[_g]);});return function(_eQ){return A(_eQ,[_eP]);};}else{var _eR=E(_eO[1])[1],_eS=function(_eT){var _eU=new T(function(){return _eL(_eO[2],function(_eV){return A(_eN,[[1,_eT,_eV]]);});});return function(_eW){var _eX=new T(function(){return A(_eU,[_eW]);});return [0,function(_eY){return E(_eX);}];};};switch(E(E(_eJ)[1])){case 8:if(48>_eR){var _eZ=new T(function(){return A(_eN,[_g]);});return function(_f0){return A(_f0,[_eZ]);};}else{if(_eR>55){var _f1=new T(function(){return A(_eN,[_g]);});return function(_f2){return A(_f2,[_f1]);};}else{return _eS([0,_eR-48|0]);}}break;case 10:if(48>_eR){var _f3=new T(function(){return A(_eN,[_g]);});return function(_f4){return A(_f4,[_f3]);};}else{if(_eR>57){var _f5=new T(function(){return A(_eN,[_g]);});return function(_f6){return A(_f6,[_f5]);};}else{return _eS([0,_eR-48|0]);}}break;case 16:var _f7=new T(function(){return 97>_eR?65>_eR?[0]:_eR>70?[0]:[1,[0,(_eR-65|0)+10|0]]:_eR>102?65>_eR?[0]:_eR>70?[0]:[1,[0,(_eR-65|0)+10|0]]:[1,[0,(_eR-97|0)+10|0]];});if(48>_eR){var _f8=E(_f7);if(!_f8[0]){var _f9=new T(function(){return A(_eN,[_g]);});return function(_fa){return A(_fa,[_f9]);};}else{return _eS(_f8[1]);}}else{if(_eR>57){var _fb=E(_f7);if(!_fb[0]){var _fc=new T(function(){return A(_eN,[_g]);});return function(_fd){return A(_fd,[_fc]);};}else{return _eS(_fb[1]);}}else{return _eS([0,_eR-48|0]);}}break;default:return E(_eH);}}};return [1,function(_fe){return A(_eL,[_fe,_1j,function(_ff){var _fg=E(_ff);return _fg[0]==0?[2]:A(_eK,[_fg]);}]);}];},_fh=[0,10],_fi=[0,1],_fj=[0,2147483647],_fk=function(_fl,_fm){while(1){var _fn=E(_fl);if(!_fn[0]){var _fo=_fn[1],_fp=E(_fm);if(!_fp[0]){var _fq=_fp[1],_fr=addC(_fo,_fq);if(!E(_fr[2])){return [0,_fr[1]];}else{_fl=[1,I_fromInt(_fo)];_fm=[1,I_fromInt(_fq)];continue;}}else{_fl=[1,I_fromInt(_fo)];_fm=_fp;continue;}}else{var _fs=E(_fm);if(!_fs[0]){_fl=_fn;_fm=[1,I_fromInt(_fs[1])];continue;}else{return [1,I_add(_fn[1],_fs[1])];}}}},_ft=new T(function(){return _fk(_fj,_fi);}),_fu=function(_fv){var _fw=E(_fv);if(!_fw[0]){var _fx=E(_fw[1]);return _fx==(-2147483648)?E(_ft):[0, -_fx];}else{return [1,I_negate(_fw[1])];}},_fy=[0,10],_fz=[0,0],_fA=function(_fB,_fC){while(1){var _fD=E(_fB);if(!_fD[0]){var _fE=_fD[1],_fF=E(_fC);if(!_fF[0]){var _fG=_fF[1];if(!(imul(_fE,_fG)|0)){return [0,imul(_fE,_fG)|0];}else{_fB=[1,I_fromInt(_fE)];_fC=[1,I_fromInt(_fG)];continue;}}else{_fB=[1,I_fromInt(_fE)];_fC=_fF;continue;}}else{var _fH=E(_fC);if(!_fH[0]){_fB=_fD;_fC=[1,I_fromInt(_fH[1])];continue;}else{return [1,I_mul(_fD[1],_fH[1])];}}}},_fI=function(_fJ,_fK,_fL){while(1){var _fM=E(_fL);if(!_fM[0]){return E(_fK);}else{var _fN=_fk(_fA(_fK,_fJ),_fM[1]);_fL=_fM[2];_fK=_fN;continue;}}},_fO=function(_fP){var _fQ=new T(function(){return _db(_db([0,function(_fR){return E(E(_fR)[1])==45?_eI(_fh,function(_fS){return A(_fP,[[1,new T(function(){return _fu(_fI(_fy,_fz,_fS));})]]);}):[2];}],[0,function(_fT){return E(E(_fT)[1])==43?_eI(_fh,function(_fU){return A(_fP,[[1,new T(function(){return _fI(_fy,_fz,_fU);})]]);}):[2];}]),new T(function(){return _eI(_fh,function(_fV){return A(_fP,[[1,new T(function(){return _fI(_fy,_fz,_fV);})]]);});}));});return _db([0,function(_fW){return E(E(_fW)[1])==101?E(_fQ):[2];}],[0,function(_fX){return E(E(_fX)[1])==69?E(_fQ):[2];}]);},_fY=function(_fZ){return A(_fZ,[_9]);},_g0=function(_g1){return A(_g1,[_9]);},_g2=function(_g3){var _g4=new T(function(){return _eI(_fh,function(_g5){return A(_g3,[[1,_g5]]);});});return [0,function(_g6){return E(E(_g6)[1])==46?E(_g4):[2];}];},_g7=function(_g8){return _eI(_fh,function(_g9){return _ek(_g2,_fY,function(_ga){return _ek(_fO,_g0,function(_gb){return A(_g8,[[5,[1,_g9,_ga,_gb]]]);});});});},_gc=function(_gd,_ge,_gf){while(1){var _gg=E(_gf);if(!_gg[0]){return false;}else{if(!A(_dJ,[_gd,_ge,_gg[1]])){_gf=_gg[2];continue;}else{return true;}}}},_gh=unCStr("!@#$%&*+./<=>?\\^|:-~"),_gi=function(_gj){return _gc(_dI,_gj,_gh);},_gk=[0,8],_gl=[0,16],_gm=function(_gn){var _go=new T(function(){return _eI(_gl,function(_gp){return A(_gn,[[5,[0,_gl,_gp]]]);});}),_gq=new T(function(){return _eI(_gk,function(_gr){return A(_gn,[[5,[0,_gk,_gr]]]);});}),_gs=new T(function(){return _eI(_gl,function(_gt){return A(_gn,[[5,[0,_gl,_gt]]]);});}),_gu=new T(function(){return _eI(_gk,function(_gv){return A(_gn,[[5,[0,_gk,_gv]]]);});});return [0,function(_gw){return E(E(_gw)[1])==48?E([0,function(_gx){switch(E(E(_gx)[1])){case 79:return E(_gu);case 88:return E(_gs);case 111:return E(_gq);case 120:return E(_go);default:return [2];}}]):[2];}];},_gy=true,_gz=function(_gA){var _gB=new T(function(){return A(_gA,[_gl]);}),_gC=new T(function(){return A(_gA,[_gk]);}),_gD=new T(function(){return A(_gA,[_gl]);}),_gE=new T(function(){return A(_gA,[_gk]);});return [0,function(_gF){switch(E(E(_gF)[1])){case 79:return E(_gE);case 88:return E(_gD);case 111:return E(_gC);case 120:return E(_gB);default:return [2];}}];},_gG=function(_gH){return A(_gH,[_fh]);},_gI=function(_gJ){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _4f(9,_gJ,_g);})));},_gK=function(_gL){var _gM=E(_gL);return _gM[0]==0?E(_gM[1]):I_toInt(_gM[1]);},_gN=function(_gO,_gP){var _gQ=E(_gO);if(!_gQ[0]){var _gR=_gQ[1],_gS=E(_gP);return _gS[0]==0?_gR<=_gS[1]:I_compareInt(_gS[1],_gR)>=0;}else{var _gT=_gQ[1],_gU=E(_gP);return _gU[0]==0?I_compareInt(_gT,_gU[1])<=0:I_compare(_gT,_gU[1])<=0;}},_gV=function(_gW){return [2];},_gX=function(_gY){var _gZ=E(_gY);if(!_gZ[0]){return E(_gV);}else{var _h0=_gZ[1],_h1=E(_gZ[2]);if(!_h1[0]){return E(_h0);}else{var _h2=new T(function(){return _gX(_h1);});return function(_h3){return _db(A(_h0,[_h3]),new T(function(){return A(_h2,[_h3]);}));};}}},_h4=unCStr("NUL"),_h5=function(_h6){return [2];},_h7=function(_h8){return _h5(_h8);},_h9=function(_ha,_hb){var _hc=function(_hd,_he){var _hf=E(_hd);if(!_hf[0]){return function(_hg){return A(_hg,[_ha]);};}else{var _hh=E(_he);if(!_hh[0]){return E(_h5);}else{if(E(_hf[1])[1]!=E(_hh[1])[1]){return E(_h7);}else{var _hi=new T(function(){return _hc(_hf[2],_hh[2]);});return function(_hj){var _hk=new T(function(){return A(_hi,[_hj]);});return [0,function(_hl){return E(_hk);}];};}}}};return [1,function(_hm){return A(_hc,[_ha,_hm,_hb]);}];},_hn=[0,0],_ho=function(_hp){var _hq=new T(function(){return A(_hp,[_hn]);});return _h9(_h4,function(_hr){return E(_hq);});},_hs=unCStr("STX"),_ht=[0,2],_hu=function(_hv){var _hw=new T(function(){return A(_hv,[_ht]);});return _h9(_hs,function(_hx){return E(_hw);});},_hy=unCStr("ETX"),_hz=[0,3],_hA=function(_hB){var _hC=new T(function(){return A(_hB,[_hz]);});return _h9(_hy,function(_hD){return E(_hC);});},_hE=unCStr("EOT"),_hF=[0,4],_hG=function(_hH){var _hI=new T(function(){return A(_hH,[_hF]);});return _h9(_hE,function(_hJ){return E(_hI);});},_hK=unCStr("ENQ"),_hL=[0,5],_hM=function(_hN){var _hO=new T(function(){return A(_hN,[_hL]);});return _h9(_hK,function(_hP){return E(_hO);});},_hQ=unCStr("ACK"),_hR=[0,6],_hS=function(_hT){var _hU=new T(function(){return A(_hT,[_hR]);});return _h9(_hQ,function(_hV){return E(_hU);});},_hW=unCStr("BEL"),_hX=[0,7],_hY=function(_hZ){var _i0=new T(function(){return A(_hZ,[_hX]);});return _h9(_hW,function(_i1){return E(_i0);});},_i2=unCStr("BS"),_i3=[0,8],_i4=function(_i5){var _i6=new T(function(){return A(_i5,[_i3]);});return _h9(_i2,function(_i7){return E(_i6);});},_i8=unCStr("HT"),_i9=[0,9],_ia=function(_ib){var _ic=new T(function(){return A(_ib,[_i9]);});return _h9(_i8,function(_id){return E(_ic);});},_ie=unCStr("LF"),_if=[0,10],_ig=function(_ih){var _ii=new T(function(){return A(_ih,[_if]);});return _h9(_ie,function(_ij){return E(_ii);});},_ik=unCStr("VT"),_il=[0,11],_im=function(_in){var _io=new T(function(){return A(_in,[_il]);});return _h9(_ik,function(_ip){return E(_io);});},_iq=unCStr("FF"),_ir=[0,12],_is=function(_it){var _iu=new T(function(){return A(_it,[_ir]);});return _h9(_iq,function(_iv){return E(_iu);});},_iw=unCStr("CR"),_ix=[0,13],_iy=function(_iz){var _iA=new T(function(){return A(_iz,[_ix]);});return _h9(_iw,function(_iB){return E(_iA);});},_iC=unCStr("SI"),_iD=[0,15],_iE=function(_iF){var _iG=new T(function(){return A(_iF,[_iD]);});return _h9(_iC,function(_iH){return E(_iG);});},_iI=unCStr("DLE"),_iJ=[0,16],_iK=function(_iL){var _iM=new T(function(){return A(_iL,[_iJ]);});return _h9(_iI,function(_iN){return E(_iM);});},_iO=unCStr("DC1"),_iP=[0,17],_iQ=function(_iR){var _iS=new T(function(){return A(_iR,[_iP]);});return _h9(_iO,function(_iT){return E(_iS);});},_iU=unCStr("DC2"),_iV=[0,18],_iW=function(_iX){var _iY=new T(function(){return A(_iX,[_iV]);});return _h9(_iU,function(_iZ){return E(_iY);});},_j0=unCStr("DC3"),_j1=[0,19],_j2=function(_j3){var _j4=new T(function(){return A(_j3,[_j1]);});return _h9(_j0,function(_j5){return E(_j4);});},_j6=unCStr("DC4"),_j7=[0,20],_j8=function(_j9){var _ja=new T(function(){return A(_j9,[_j7]);});return _h9(_j6,function(_jb){return E(_ja);});},_jc=unCStr("NAK"),_jd=[0,21],_je=function(_jf){var _jg=new T(function(){return A(_jf,[_jd]);});return _h9(_jc,function(_jh){return E(_jg);});},_ji=unCStr("SYN"),_jj=[0,22],_jk=function(_jl){var _jm=new T(function(){return A(_jl,[_jj]);});return _h9(_ji,function(_jn){return E(_jm);});},_jo=unCStr("ETB"),_jp=[0,23],_jq=function(_jr){var _js=new T(function(){return A(_jr,[_jp]);});return _h9(_jo,function(_jt){return E(_js);});},_ju=unCStr("CAN"),_jv=[0,24],_jw=function(_jx){var _jy=new T(function(){return A(_jx,[_jv]);});return _h9(_ju,function(_jz){return E(_jy);});},_jA=unCStr("EM"),_jB=[0,25],_jC=function(_jD){var _jE=new T(function(){return A(_jD,[_jB]);});return _h9(_jA,function(_jF){return E(_jE);});},_jG=unCStr("SUB"),_jH=[0,26],_jI=function(_jJ){var _jK=new T(function(){return A(_jJ,[_jH]);});return _h9(_jG,function(_jL){return E(_jK);});},_jM=unCStr("ESC"),_jN=[0,27],_jO=function(_jP){var _jQ=new T(function(){return A(_jP,[_jN]);});return _h9(_jM,function(_jR){return E(_jQ);});},_jS=unCStr("FS"),_jT=[0,28],_jU=function(_jV){var _jW=new T(function(){return A(_jV,[_jT]);});return _h9(_jS,function(_jX){return E(_jW);});},_jY=unCStr("GS"),_jZ=[0,29],_k0=function(_k1){var _k2=new T(function(){return A(_k1,[_jZ]);});return _h9(_jY,function(_k3){return E(_k2);});},_k4=unCStr("RS"),_k5=[0,30],_k6=function(_k7){var _k8=new T(function(){return A(_k7,[_k5]);});return _h9(_k4,function(_k9){return E(_k8);});},_ka=unCStr("US"),_kb=[0,31],_kc=function(_kd){var _ke=new T(function(){return A(_kd,[_kb]);});return _h9(_ka,function(_kf){return E(_ke);});},_kg=unCStr("SP"),_kh=[0,32],_ki=function(_kj){var _kk=new T(function(){return A(_kj,[_kh]);});return _h9(_kg,function(_kl){return E(_kk);});},_km=unCStr("DEL"),_kn=[0,127],_ko=function(_kp){var _kq=new T(function(){return A(_kp,[_kn]);});return _h9(_km,function(_kr){return E(_kq);});},_ks=[1,_ko,_g],_kt=[1,_ki,_ks],_ku=[1,_kc,_kt],_kv=[1,_k6,_ku],_kw=[1,_k0,_kv],_kx=[1,_jU,_kw],_ky=[1,_jO,_kx],_kz=[1,_jI,_ky],_kA=[1,_jC,_kz],_kB=[1,_jw,_kA],_kC=[1,_jq,_kB],_kD=[1,_jk,_kC],_kE=[1,_je,_kD],_kF=[1,_j8,_kE],_kG=[1,_j2,_kF],_kH=[1,_iW,_kG],_kI=[1,_iQ,_kH],_kJ=[1,_iK,_kI],_kK=[1,_iE,_kJ],_kL=[1,_iy,_kK],_kM=[1,_is,_kL],_kN=[1,_im,_kM],_kO=[1,_ig,_kN],_kP=[1,_ia,_kO],_kQ=[1,_i4,_kP],_kR=[1,_hY,_kQ],_kS=[1,_hS,_kR],_kT=[1,_hM,_kS],_kU=[1,_hG,_kT],_kV=[1,_hA,_kU],_kW=[1,_hu,_kV],_kX=[1,_ho,_kW],_kY=unCStr("SOH"),_kZ=[0,1],_l0=function(_l1){var _l2=new T(function(){return A(_l1,[_kZ]);});return _h9(_kY,function(_l3){return E(_l2);});},_l4=unCStr("SO"),_l5=[0,14],_l6=function(_l7){var _l8=new T(function(){return A(_l7,[_l5]);});return _h9(_l4,function(_l9){return E(_l8);});},_la=function(_lb){return _ek(_l0,_l6,_lb);},_lc=[1,_la,_kX],_ld=new T(function(){return _gX(_lc);}),_le=[0,1114111],_lf=[0,34],_lg=[0,_lf,_gy],_lh=[0,39],_li=[0,_lh,_gy],_lj=[0,92],_lk=[0,_lj,_gy],_ll=[0,_hX,_gy],_lm=[0,_i3,_gy],_ln=[0,_ir,_gy],_lo=[0,_if,_gy],_lp=[0,_ix,_gy],_lq=[0,_i9,_gy],_lr=[0,_il,_gy],_ls=[0,_hn,_gy],_lt=[0,_kZ,_gy],_lu=[0,_ht,_gy],_lv=[0,_hz,_gy],_lw=[0,_hF,_gy],_lx=[0,_hL,_gy],_ly=[0,_hR,_gy],_lz=[0,_hX,_gy],_lA=[0,_i3,_gy],_lB=[0,_i9,_gy],_lC=[0,_if,_gy],_lD=[0,_il,_gy],_lE=[0,_ir,_gy],_lF=[0,_ix,_gy],_lG=[0,_l5,_gy],_lH=[0,_iD,_gy],_lI=[0,_iJ,_gy],_lJ=[0,_iP,_gy],_lK=[0,_iV,_gy],_lL=[0,_j1,_gy],_lM=[0,_j7,_gy],_lN=[0,_jd,_gy],_lO=[0,_jj,_gy],_lP=[0,_jp,_gy],_lQ=[0,_jv,_gy],_lR=[0,_jB,_gy],_lS=[0,_jH,_gy],_lT=[0,_jN,_gy],_lU=[0,_jT,_gy],_lV=[0,_jZ,_gy],_lW=[0,_k5,_gy],_lX=[0,_kb,_gy],_lY=function(_lZ){return [0,_lZ];},_m0=function(_m1){var _m2=new T(function(){return A(_m1,[_lr]);}),_m3=new T(function(){return A(_m1,[_lq]);}),_m4=new T(function(){return A(_m1,[_lp]);}),_m5=new T(function(){return A(_m1,[_lo]);}),_m6=new T(function(){return A(_m1,[_ln]);}),_m7=new T(function(){return A(_m1,[_lm]);}),_m8=new T(function(){return A(_m1,[_ll]);}),_m9=new T(function(){return A(_m1,[_lk]);}),_ma=new T(function(){return A(_m1,[_li]);}),_mb=new T(function(){return A(_m1,[_lg]);});return _db([0,function(_mc){switch(E(E(_mc)[1])){case 34:return E(_mb);case 39:return E(_ma);case 92:return E(_m9);case 97:return E(_m8);case 98:return E(_m7);case 102:return E(_m6);case 110:return E(_m5);case 114:return E(_m4);case 116:return E(_m3);case 118:return E(_m2);default:return [2];}}],new T(function(){return _db(_ek(_gz,_gG,function(_md){var _me=new T(function(){return _lY(E(_md)[1]);});return _eI(_md,function(_mf){var _mg=_fI(_me,_fz,_mf);return !_gN(_mg,_le)?[2]:A(_m1,[[0,new T(function(){var _mh=_gK(_mg);return _mh>>>0>1114111?_gI(_mh):[0,_mh];}),_gy]]);});}),new T(function(){var _mi=new T(function(){return A(_m1,[_lX]);}),_mj=new T(function(){return A(_m1,[_lW]);}),_mk=new T(function(){return A(_m1,[_lV]);}),_ml=new T(function(){return A(_m1,[_lU]);}),_mm=new T(function(){return A(_m1,[_lT]);}),_mn=new T(function(){return A(_m1,[_lS]);}),_mo=new T(function(){return A(_m1,[_lR]);}),_mp=new T(function(){return A(_m1,[_lQ]);}),_mq=new T(function(){return A(_m1,[_lP]);}),_mr=new T(function(){return A(_m1,[_lO]);}),_ms=new T(function(){return A(_m1,[_lN]);}),_mt=new T(function(){return A(_m1,[_lM]);}),_mu=new T(function(){return A(_m1,[_lL]);}),_mv=new T(function(){return A(_m1,[_lK]);}),_mw=new T(function(){return A(_m1,[_lJ]);}),_mx=new T(function(){return A(_m1,[_lI]);}),_my=new T(function(){return A(_m1,[_lH]);}),_mz=new T(function(){return A(_m1,[_lG]);}),_mA=new T(function(){return A(_m1,[_lF]);}),_mB=new T(function(){return A(_m1,[_lE]);}),_mC=new T(function(){return A(_m1,[_lD]);}),_mD=new T(function(){return A(_m1,[_lC]);}),_mE=new T(function(){return A(_m1,[_lB]);}),_mF=new T(function(){return A(_m1,[_lA]);}),_mG=new T(function(){return A(_m1,[_lz]);}),_mH=new T(function(){return A(_m1,[_ly]);}),_mI=new T(function(){return A(_m1,[_lx]);}),_mJ=new T(function(){return A(_m1,[_lw]);}),_mK=new T(function(){return A(_m1,[_lv]);}),_mL=new T(function(){return A(_m1,[_lu]);}),_mM=new T(function(){return A(_m1,[_lt]);}),_mN=new T(function(){return A(_m1,[_ls]);});return _db([0,function(_mO){return E(E(_mO)[1])==94?E([0,function(_mP){switch(E(E(_mP)[1])){case 64:return E(_mN);case 65:return E(_mM);case 66:return E(_mL);case 67:return E(_mK);case 68:return E(_mJ);case 69:return E(_mI);case 70:return E(_mH);case 71:return E(_mG);case 72:return E(_mF);case 73:return E(_mE);case 74:return E(_mD);case 75:return E(_mC);case 76:return E(_mB);case 77:return E(_mA);case 78:return E(_mz);case 79:return E(_my);case 80:return E(_mx);case 81:return E(_mw);case 82:return E(_mv);case 83:return E(_mu);case 84:return E(_mt);case 85:return E(_ms);case 86:return E(_mr);case 87:return E(_mq);case 88:return E(_mp);case 89:return E(_mo);case 90:return E(_mn);case 91:return E(_mm);case 92:return E(_ml);case 93:return E(_mk);case 94:return E(_mj);case 95:return E(_mi);default:return [2];}}]):[2];}],new T(function(){return A(_ld,[function(_mQ){return A(_m1,[[0,_mQ,_gy]]);}]);}));}));}));},_mR=function(_mS){return A(_mS,[_1c]);},_mT=function(_mU){var _mV=E(_mU);if(!_mV[0]){return E(_mR);}else{var _mW=_mV[2],_mX=E(E(_mV[1])[1]);switch(_mX){case 9:var _mY=new T(function(){return _mT(_mW);});return function(_mZ){var _n0=new T(function(){return A(_mY,[_mZ]);});return [0,function(_n1){return E(_n0);}];};case 10:var _n2=new T(function(){return _mT(_mW);});return function(_n3){var _n4=new T(function(){return A(_n2,[_n3]);});return [0,function(_n5){return E(_n4);}];};case 11:var _n6=new T(function(){return _mT(_mW);});return function(_n7){var _n8=new T(function(){return A(_n6,[_n7]);});return [0,function(_n9){return E(_n8);}];};case 12:var _na=new T(function(){return _mT(_mW);});return function(_nb){var _nc=new T(function(){return A(_na,[_nb]);});return [0,function(_nd){return E(_nc);}];};case 13:var _ne=new T(function(){return _mT(_mW);});return function(_nf){var _ng=new T(function(){return A(_ne,[_nf]);});return [0,function(_nh){return E(_ng);}];};case 32:var _ni=new T(function(){return _mT(_mW);});return function(_nj){var _nk=new T(function(){return A(_ni,[_nj]);});return [0,function(_nl){return E(_nk);}];};case 160:var _nm=new T(function(){return _mT(_mW);});return function(_nn){var _no=new T(function(){return A(_nm,[_nn]);});return [0,function(_np){return E(_no);}];};default:var _nq=u_iswspace(_mX);if(!E(_nq)){return E(_mR);}else{var _nr=new T(function(){return _mT(_mW);});return function(_ns){var _nt=new T(function(){return A(_nr,[_ns]);});return [0,function(_nu){return E(_nt);}];};}}}},_nv=function(_nw){var _nx=new T(function(){return _m0(_nw);}),_ny=new T(function(){return _nv(_nw);}),_nz=[1,function(_nA){return A(_mT,[_nA,function(_nB){return E([0,function(_nC){return E(E(_nC)[1])==92?E(_ny):[2];}]);}]);}];return _db([0,function(_nD){return E(E(_nD)[1])==92?E([0,function(_nE){var _nF=E(E(_nE)[1]);switch(_nF){case 9:return E(_nz);case 10:return E(_nz);case 11:return E(_nz);case 12:return E(_nz);case 13:return E(_nz);case 32:return E(_nz);case 38:return E(_ny);case 160:return E(_nz);default:var _nG=u_iswspace(_nF);return E(_nG)==0?[2]:E(_nz);}}]):[2];}],[0,function(_nH){var _nI=E(_nH);return E(_nI[1])==92?E(_nx):A(_nw,[[0,_nI,_0]]);}]);},_nJ=function(_nK,_nL){var _nM=new T(function(){return A(_nL,[[1,new T(function(){return A(_nK,[_g]);})]]);});return _nv(function(_nN){var _nO=E(_nN),_nP=E(_nO[1]);return E(_nP[1])==34?!E(_nO[2])?E(_nM):_nJ(function(_nQ){return A(_nK,[[1,_nP,_nQ]]);},_nL):_nJ(function(_nR){return A(_nK,[[1,_nP,_nR]]);},_nL);});},_nS=unCStr("_\'"),_nT=function(_nU){var _nV=u_iswalnum(_nU);return E(_nV)==0?_gc(_dI,[0,_nU],_nS):true;},_nW=function(_nX){return _nT(E(_nX)[1]);},_nY=unCStr(",;()[]{}`"),_nZ=function(_o0){return A(_o0,[_g]);},_o1=function(_o2,_o3){var _o4=function(_o5){var _o6=E(_o5);if(!_o6[0]){return E(_nZ);}else{var _o7=_o6[1];if(!A(_o2,[_o7])){return E(_nZ);}else{var _o8=new T(function(){return _o4(_o6[2]);});return function(_o9){var _oa=new T(function(){return A(_o8,[function(_ob){return A(_o9,[[1,_o7,_ob]]);}]);});return [0,function(_oc){return E(_oa);}];};}}};return [1,function(_od){return A(_o4,[_od,_o3]);}];},_oe=unCStr(".."),_of=unCStr("::"),_og=unCStr("->"),_oh=[0,64],_oi=[1,_oh,_g],_oj=[0,126],_ok=[1,_oj,_g],_ol=unCStr("=>"),_om=[1,_ol,_g],_on=[1,_ok,_om],_oo=[1,_oi,_on],_op=[1,_og,_oo],_oq=unCStr("<-"),_or=[1,_oq,_op],_os=[0,124],_ot=[1,_os,_g],_ou=[1,_ot,_or],_ov=[1,_lj,_g],_ow=[1,_ov,_ou],_ox=[0,61],_oy=[1,_ox,_g],_oz=[1,_oy,_ow],_oA=[1,_of,_oz],_oB=[1,_oe,_oA],_oC=function(_oD){var _oE=new T(function(){return A(_oD,[_eF]);});return _db([1,function(_oF){return E(_oF)[0]==0?E(_oE):[2];}],new T(function(){var _oG=new T(function(){return _m0(function(_oH){var _oI=E(_oH);return (function(_oJ,_oK){var _oL=new T(function(){return A(_oD,[[0,_oJ]]);});return !E(_oK)?E(E(_oJ)[1])==39?[2]:[0,function(_oM){return E(E(_oM)[1])==39?E(_oL):[2];}]:[0,function(_oN){return E(E(_oN)[1])==39?E(_oL):[2];}];})(_oI[1],_oI[2]);});});return _db([0,function(_oO){return E(E(_oO)[1])==39?E([0,function(_oP){var _oQ=E(_oP);switch(E(_oQ[1])){case 39:return [2];case 92:return E(_oG);default:var _oR=new T(function(){return A(_oD,[[0,_oQ]]);});return [0,function(_oS){return E(E(_oS)[1])==39?E(_oR):[2];}];}}]):[2];}],new T(function(){var _oT=new T(function(){return _nJ(_1j,_oD);});return _db([0,function(_oU){return E(E(_oU)[1])==34?E(_oT):[2];}],new T(function(){return _db([0,function(_oV){return !_gc(_dI,_oV,_nY)?[2]:A(_oD,[[2,[1,_oV,_g]]]);}],new T(function(){return _db([0,function(_oW){return !_gc(_dI,_oW,_gh)?[2]:_o1(_gi,function(_oX){var _oY=[1,_oW,_oX];return !_gc(_dZ,_oY,_oB)?A(_oD,[[4,_oY]]):A(_oD,[[2,_oY]]);});}],new T(function(){return _db([0,function(_oZ){var _p0=E(_oZ),_p1=_p0[1],_p2=u_iswalpha(_p1);return E(_p2)==0?E(_p1)==95?_o1(_nW,function(_p3){return A(_oD,[[3,[1,_p0,_p3]]]);}):[2]:_o1(_nW,function(_p4){return A(_oD,[[3,[1,_p0,_p4]]]);});}],new T(function(){return _ek(_gm,_g7,_oD);}));}));}));}));}));}));},_p5=function(_p6){var _p7=new T(function(){return _oC(_p6);});return [1,function(_p8){return A(_mT,[_p8,function(_p9){return E(_p7);}]);}];},_pa=[0,0],_pb=function(_pc,_pd){var _pe=new T(function(){return A(_pc,[_pa,function(_pf){var _pg=new T(function(){return A(_pd,[_pf]);});return _p5(function(_ph){var _pi=E(_ph);if(_pi[0]==2){var _pj=E(_pi[1]);return _pj[0]==0?[2]:E(E(_pj[1])[1])==41?E(_pj[2])[0]==0?E(_pg):[2]:[2];}else{return [2];}});}]);});return _p5(function(_pk){var _pl=E(_pk);if(_pl[0]==2){var _pm=E(_pl[1]);return _pm[0]==0?[2]:E(E(_pm[1])[1])==40?E(_pm[2])[0]==0?E(_pe):[2]:[2];}else{return [2];}});},_pn=function(_po){return _db(_p5(function(_pp){var _pq=E(_pp);return _pq[0]==0?A(_po,[_pq[1]]):[2];}),new T(function(){return _pb(_pr,_po);}));},_pr=function(_ps,_pt){return _pn(_pt);},_pu=function(_pv,_pw){var _px=function(_py,_pz){var _pA=new T(function(){return A(_pz,[_g]);}),_pB=new T(function(){return A(_pv,[_pa,function(_pC){return _px(_gy,function(_pD){return A(_pz,[[1,_pC,_pD]]);});}]);});return _p5(function(_pE){var _pF=E(_pE);if(_pF[0]==2){var _pG=E(_pF[1]);if(!_pG[0]){return [2];}else{var _pH=_pG[2];switch(E(E(_pG[1])[1])){case 44:return E(_pH)[0]==0?!E(_py)?[2]:E(_pB):[2];case 93:return E(_pH)[0]==0?E(_pA):[2];default:return [2];}}}else{return [2];}});},_pI=function(_pJ){var _pK=new T(function(){return _db(_px(_0,_pJ),new T(function(){return A(_pv,[_pa,function(_pL){return _px(_gy,function(_pM){return A(_pJ,[[1,_pL,_pM]]);});}]);}));});return _db(_p5(function(_pN){var _pO=E(_pN);if(_pO[0]==2){var _pP=E(_pO[1]);return _pP[0]==0?[2]:E(E(_pP[1])[1])==91?E(_pP[2])[0]==0?E(_pK):[2]:[2];}else{return [2];}}),new T(function(){return _pb(function(_pQ,_pR){return _pI(_pR);},_pJ);}));};return _pI(_pw);},_pS=function(_pT){return _db(_db(_p5(function(_pU){var _pV=E(_pU);return _pV[0]==1?A(_pT,[_pV[1]]):[2];}),new T(function(){return _pu(_pr,_pT);})),new T(function(){return _pb(_pW,_pT);}));},_pW=function(_pX,_pY){return _pS(_pY);},_pZ=new T(function(){return _pb(_pW,_ec);}),_q0=new T(function(){return _pu(_pr,_ec);}),_q1=function(_q2){var _q3=E(_q2);return _q3[0]==1?[3,_q3[1],_eb]:[2];},_q4=new T(function(){return _oC(_q1);}),_q5=function(_q6){return E(_q4);},_q7=function(_q8){return A(_mT,[_q8,_q5]);},_q9=[1,_q7],_qa=new T(function(){return _db(_q9,_q0);}),_qb=new T(function(){return _db(_qa,_pZ);}),_qc=function(_qd){return _d1(_qb,_qd);},_qe=new T(function(){return _pn(_ec);}),_qf=function(_qd){return _d1(_qe,_qd);},_qg=function(_qh){return E(_qf);},_qi=[0,_qg,_qc,_pr,_pW],_qj=function(_qk){return E(E(_qk)[4]);},_ql=function(_qm,_qn,_qo){return _pu(new T(function(){return _qj(_qm);}),_qo);},_qp=function(_qq){var _qr=new T(function(){return _pu(new T(function(){return _qj(_qq);}),_ec);});return function(_as){return _d1(_qr,_as);};},_qs=function(_qt,_qu){var _qv=new T(function(){return A(_qj,[_qt,_qu,_ec]);});return function(_as){return _d1(_qv,_as);};},_qw=function(_qx){return [0,function(_qd){return _qs(_qx,_qd);},new T(function(){return _qp(_qx);}),new T(function(){return _qj(_qx);}),function(_qy,_qd){return _ql(_qx,_qy,_qd);}];},_qz=new T(function(){return _qw(_qi);}),_qA=unCStr("Prelude.(!!): negative index\n"),_qB=new T(function(){return err(_qA);}),_qC=unCStr("Prelude.(!!): index too large\n"),_qD=new T(function(){return err(_qC);}),_qE=function(_qF,_qG){while(1){var _qH=E(_qF);if(!_qH[0]){return E(_qD);}else{var _qI=E(_qG);if(!_qI){return E(_qH[1]);}else{_qF=_qH[2];_qG=_qI-1|0;continue;}}}},_qJ=unCStr("ACK"),_qK=unCStr("BEL"),_qL=unCStr("BS"),_qM=unCStr("SP"),_qN=[1,_qM,_g],_qO=unCStr("US"),_qP=[1,_qO,_qN],_qQ=unCStr("RS"),_qR=[1,_qQ,_qP],_qS=unCStr("GS"),_qT=[1,_qS,_qR],_qU=unCStr("FS"),_qV=[1,_qU,_qT],_qW=unCStr("ESC"),_qX=[1,_qW,_qV],_qY=unCStr("SUB"),_qZ=[1,_qY,_qX],_r0=unCStr("EM"),_r1=[1,_r0,_qZ],_r2=unCStr("CAN"),_r3=[1,_r2,_r1],_r4=unCStr("ETB"),_r5=[1,_r4,_r3],_r6=unCStr("SYN"),_r7=[1,_r6,_r5],_r8=unCStr("NAK"),_r9=[1,_r8,_r7],_ra=unCStr("DC4"),_rb=[1,_ra,_r9],_rc=unCStr("DC3"),_rd=[1,_rc,_rb],_re=unCStr("DC2"),_rf=[1,_re,_rd],_rg=unCStr("DC1"),_rh=[1,_rg,_rf],_ri=unCStr("DLE"),_rj=[1,_ri,_rh],_rk=unCStr("SI"),_rl=[1,_rk,_rj],_rm=unCStr("SO"),_rn=[1,_rm,_rl],_ro=unCStr("CR"),_rp=[1,_ro,_rn],_rq=unCStr("FF"),_rr=[1,_rq,_rp],_rs=unCStr("VT"),_rt=[1,_rs,_rr],_ru=unCStr("LF"),_rv=[1,_ru,_rt],_rw=unCStr("HT"),_rx=[1,_rw,_rv],_ry=[1,_qL,_rx],_rz=[1,_qK,_ry],_rA=[1,_qJ,_rz],_rB=unCStr("ENQ"),_rC=[1,_rB,_rA],_rD=unCStr("EOT"),_rE=[1,_rD,_rC],_rF=unCStr("ETX"),_rG=[1,_rF,_rE],_rH=unCStr("STX"),_rI=[1,_rH,_rG],_rJ=unCStr("SOH"),_rK=[1,_rJ,_rI],_rL=unCStr("NUL"),_rM=[1,_rL,_rK],_rN=[0,92],_rO=unCStr("\\DEL"),_rP=unCStr("\\a"),_rQ=unCStr("\\\\"),_rR=unCStr("\\SO"),_rS=unCStr("\\r"),_rT=unCStr("\\f"),_rU=unCStr("\\v"),_rV=unCStr("\\n"),_rW=unCStr("\\t"),_rX=unCStr("\\b"),_rY=function(_rZ,_s0){if(_rZ<=127){var _s1=E(_rZ);switch(_s1){case 92:return _2i(_rQ,_s0);case 127:return _2i(_rO,_s0);default:if(_s1<32){var _s2=E(_s1);switch(_s2){case 7:return _2i(_rP,_s0);case 8:return _2i(_rX,_s0);case 9:return _2i(_rW,_s0);case 10:return _2i(_rV,_s0);case 11:return _2i(_rU,_s0);case 12:return _2i(_rT,_s0);case 13:return _2i(_rS,_s0);case 14:return _2i(_rR,new T(function(){var _s3=E(_s0);return _s3[0]==0?[0]:E(E(_s3[1])[1])==72?unAppCStr("\\&",_s3):E(_s3);}));default:return _2i([1,_rN,new T(function(){var _s4=_s2;return _s4>=0?_qE(_rM,_s4):E(_qB);})],_s0);}}else{return [1,[0,_s1],_s0];}}}else{return [1,_rN,new T(function(){var _s5=jsShowI(_rZ);return _2i(fromJSStr(_s5),new T(function(){var _s6=E(_s0);if(!_s6[0]){return [0];}else{var _s7=E(_s6[1])[1];return _s7<48?E(_s6):_s7>57?E(_s6):unAppCStr("\\&",_s6);}}));})];}},_s8=[0,39],_s9=[1,_s8,_g],_sa=unCStr("\'\\\'\'"),_sb=function(_sc){var _sd=E(E(_sc)[1]);return _sd==39?E(_sa):[1,_s8,new T(function(){return _rY(_sd,_s9);})];},_se=unCStr("\\\""),_sf=function(_sg,_sh){var _si=E(_sg);if(!_si[0]){return E(_sh);}else{var _sj=_si[2],_sk=E(E(_si[1])[1]);return _sk==34?_2i(_se,new T(function(){return _sf(_sj,_sh);})):_rY(_sk,new T(function(){return _sf(_sj,_sh);}));}},_sl=function(_sm,_sn){return [1,_E,new T(function(){return _sf(_sm,[1,_E,_sn]);})];},_so=function(_sp){return _2i(_sa,_sp);},_sq=function(_sr,_ss){var _st=E(E(_ss)[1]);return _st==39?E(_so):function(_su){return [1,_s8,new T(function(){return _rY(_st,[1,_s8,_su]);})];};},_sv=[0,_sq,_sb,_sl],_sw=function(_sx){return E(E(_sx)[3]);},_sy=function(_sz,_sA){return A(_sw,[_sz,_sA,_g]);},_sB=function(_sC,_sD,_sE){return _38(new T(function(){return _sw(_sC);}),_sD,_sE);},_sF=function(_sG){var _sH=new T(function(){return _sw(_sG);});return [0,function(_sI){return E(_sH);},function(_sp){return _sy(_sG,_sp);},function(_sJ,_sp){return _sB(_sG,_sJ,_sp);}];},_sK=new T(function(){return _sF(_sv);}),_sL=new T(function(){return _bo(_8D,_8E,_9f,_sK,_qz);}),_sM=unCStr("text"),_sN=new T(function(){return A(_sL,[_9,_sM,_9]);}),_sO=unCStr("keydown"),_sP=unCStr("mousemove"),_sQ=unCStr("blur"),_sR=unCStr("focus"),_sS=unCStr("change"),_sT=unCStr("unload"),_sU=unCStr("load"),_sV=unCStr("keyup"),_sW=unCStr("keypress"),_sX=unCStr("mouseup"),_sY=unCStr("mousedown"),_sZ=unCStr("dblclick"),_t0=unCStr("click"),_t1=unCStr("mouseout"),_t2=unCStr("mouseover"),_t3=function(_t4){switch(E(_t4)[0]){case 0:return E(_sU);case 1:return E(_sT);case 2:return E(_sS);case 3:return E(_sR);case 4:return E(_sQ);case 5:return E(_sP);case 6:return E(_t2);case 7:return E(_t1);case 8:return E(_t0);case 9:return E(_sZ);case 10:return E(_sY);case 11:return E(_sX);case 12:return E(_sW);case 13:return E(_sV);default:return E(_sO);}},_t5=function(_t6,_t7,_){var _t8=A(_t6,[_]);return die(_t7);},_t9=function(_ta,_tb,_tc,_){return _t5(function(_){var _=putMVar(_tb,_ta);return _1c;},_tc,_);},_td=function(_te,_){var _tf=0;if(!E(_tf)){return (function(_){var _tg=E(_5t)[1],_th=takeMVar(_tg),_ti=jsCatch(function(_){return (function(_){return _te;})();},function(_1z,_){return _t9(_th,_tg,_1z,_);}),_=putMVar(_tg,_ti);return _1c;})();}else{var _tj=E(_5t)[1],_tk=takeMVar(_tj),_tl=jsCatch(function(_){return _te;},function(_1z,_){return _t9(_tk,_tj,_1z,_);}),_=putMVar(_tj,_tl);return _1c;}},_tm=unCStr("true"),_tn=function(_to,_tp){while(1){var _tq=E(_to);if(!_tq[0]){return E(_tp)[0]==0?true:false;}else{var _tr=E(_tp);if(!_tr[0]){return false;}else{if(E(_tq[1])[1]!=E(_tr[1])[1]){return false;}else{_to=_tq[2];_tp=_tr[2];continue;}}}}},_ts=new T(function(){return [0,"keydown"];}),_tt=new T(function(){return [0,"mousemove"];}),_tu=new T(function(){return [0,"blur"];}),_tv=new T(function(){return [0,"focus"];}),_tw=new T(function(){return [0,"change"];}),_tx=new T(function(){return [0,"unload"];}),_ty=new T(function(){return [0,"load"];}),_tz=new T(function(){return [0,"keyup"];}),_tA=new T(function(){return [0,"keypress"];}),_tB=new T(function(){return [0,"mouseup"];}),_tC=new T(function(){return [0,"mousedown"];}),_tD=new T(function(){return [0,"dblclick"];}),_tE=new T(function(){return [0,"click"];}),_tF=new T(function(){return [0,"mouseout"];}),_tG=new T(function(){return [0,"mouseover"];}),_tH=function(_tI){switch(E(_tI)[0]){case 0:return E(_ty);case 1:return E(_tx);case 2:return E(_tw);case 3:return E(_tv);case 4:return E(_tu);case 5:return E(_tt);case 6:return E(_tG);case 7:return E(_tF);case 8:return E(_tE);case 9:return E(_tD);case 10:return E(_tC);case 11:return E(_tB);case 12:return E(_tA);case 13:return E(_tz);default:return E(_ts);}},_tJ=function(_tK,_tL,_tM){var _tN=new T(function(){return _t3(_tL);}),_tO=new T(function(){return _tH(_tL);});return function(_tP,_){var _tQ=A(_tK,[_tP,_]),_tR=E(_tQ),_tS=_tR[1],_tT=E(_tN),_tU=jsGetAttr(_tS,toJSStr(_tT));if(!_tn(fromJSStr(_tU),_tm)){var _tV=E(_tM),_tW=jsSetCB(_tS,E(_tO)[1],E([0,_tM])[1]),_tX=A(_1d,[_1j,_tR,_tT,_tm,_]);return _tR;}else{return _tR;}};},_tY=function(_tZ,_u0){var _u1=new T(function(){return _t3(_u0);}),_u2=[0,_u1,_5o];return function(_u3,_){var _u4=E(_u3),_u5=E(_u4[4]),_u6=_u5[1],_u7=_u5[2],_u8=A(_tZ,[_u4,_]),_u9=E(_u8),_ua=E(_u9[1]),_ub=_ua[1];return [0,[0,new T(function(){var _uc=E(_u0);switch(_uc[0]){case 0:return _tJ(_ub,_uc,function(_){var _ud=_td(_u2,_),_ue=A(_u6,[_]),_uf=E(_ue);if(!_uf[0]){return _1c;}else{var _ug=A(_u7,[_uf[1],_]);return _1c;}});case 1:return _tJ(_ub,_uc,function(_){var _uh=_td(_u2,_),_ui=A(_u6,[_]),_uj=E(_ui);if(!_uj[0]){return _1c;}else{var _uk=A(_u7,[_uj[1],_]);return _1c;}});case 2:return _tJ(_ub,_uc,function(_){var _ul=_td(_u2,_),_um=A(_u6,[_]),_un=E(_um);if(!_un[0]){return _1c;}else{var _uo=A(_u7,[_un[1],_]);return _1c;}});case 3:return _tJ(_ub,_uc,function(_){var _up=_td(_u2,_),_uq=A(_u6,[_]),_ur=E(_uq);if(!_ur[0]){return _1c;}else{var _us=A(_u7,[_ur[1],_]);return _1c;}});case 4:return _tJ(_ub,_uc,function(_){var _ut=_td(_u2,_),_uu=A(_u6,[_]),_uv=E(_uu);if(!_uv[0]){return _1c;}else{var _uw=A(_u7,[_uv[1],_]);return _1c;}});case 5:return _tJ(_ub,_uc,function(_ux,_){var _uy=_td([0,_u1,[2,E(_ux)]],_),_uz=A(_u6,[_]),_uA=E(_uz);if(!_uA[0]){return _1c;}else{var _uB=A(_u7,[_uA[1],_]);return _1c;}});case 6:return _tJ(_ub,_uc,function(_uC,_){var _uD=_td([0,_u1,[2,E(_uC)]],_),_uE=A(_u6,[_]),_uF=E(_uE);if(!_uF[0]){return _1c;}else{var _uG=A(_u7,[_uF[1],_]);return _1c;}});case 7:return _tJ(_ub,_uc,function(_){var _uH=A(_u6,[_]),_uI=E(_uH);if(!_uI[0]){return _1c;}else{var _uJ=A(_u7,[_uI[1],_]);return _1c;}});case 8:return _tJ(_ub,_uc,function(_uK,_uL,_){var _uM=_td([0,_u1,[1,_uK,E(_uL)]],_),_uN=A(_u6,[_]),_uO=E(_uN);if(!_uO[0]){return _1c;}else{var _uP=A(_u7,[_uO[1],_]);return _1c;}});case 9:return _tJ(_ub,_uc,function(_uQ,_uR,_){var _uS=_td([0,_u1,[1,_uQ,E(_uR)]],_),_uT=A(_u6,[_]),_uU=E(_uT);if(!_uU[0]){return _1c;}else{var _uV=A(_u7,[_uU[1],_]);return _1c;}});case 10:return _tJ(_ub,_uc,function(_uW,_uX,_){var _uY=_td([0,_u1,[1,_uW,E(_uX)]],_),_uZ=A(_u6,[_]),_v0=E(_uZ);if(!_v0[0]){return _1c;}else{var _v1=A(_u7,[_v0[1],_]);return _1c;}});case 11:return _tJ(_ub,_uc,function(_v2,_v3,_){var _v4=_td([0,_u1,[1,_v2,E(_v3)]],_),_v5=A(_u6,[_]),_v6=E(_v5);if(!_v6[0]){return _1c;}else{var _v7=A(_u7,[_v6[1],_]);return _1c;}});case 12:return _tJ(_ub,_uc,function(_v8,_){var _v9=_td([0,_u1,[3,_v8]],_),_va=A(_u6,[_]),_vb=E(_va);if(!_vb[0]){return _1c;}else{var _vc=A(_u7,[_vb[1],_]);return _1c;}});case 13:return _tJ(_ub,_uc,function(_vd,_){var _ve=_td([0,_u1,[3,_vd]],_),_vf=A(_u6,[_]),_vg=E(_vf);if(!_vg[0]){return _1c;}else{var _vh=A(_u7,[_vg[1],_]);return _1c;}});default:return _tJ(_ub,_uc,function(_vi,_){var _vj=_td([0,_u1,[3,_vi]],_),_vk=A(_u6,[_]),_vl=E(_vk);if(!_vl[0]){return _1c;}else{var _vm=A(_u7,[_vl[1],_]);return _1c;}});}}),_ua[2]],_u9[2]];};},_vn=new T(function(){return _tY(_sN,_5D);}),_vo=[1,_1c],_vp=[0,_Z,_vo],_vq=[0,_Z,_vo],_vr=function(_vs,_vt,_){var _vu=function(_vv,_){return _vr(_vs,_vv,_);};return _4P(_vn,function(_vw){return function(_as,_vx){return _4P(_5x,function(_vy){var _vz=E(E(_vy)[2]);switch(_vz[0]){case 0:return E(_vu);case 1:return E(_vu);case 2:return E(_vu);default:return E(E(_vz[1])[1])==13?function(_vv,_){return _4P(function(_vA,_){var _vB=jsAlert(toJSStr(E(_vw)));return [0,_vp,_vA];},function(_vC,_vD,_){var _=putMVar(E(_vs)[1],_vw);return [0,_vq,_vD];},_vv,_);}:E(_vu);}},_as,_vx);};},_vt,_);},_vE=function(_){var _vF=_13(_),_vG=newMVar(),_vH=E(_k)[1],_vI=takeMVar(_vH),_vJ=_vr([0,_vG],_vI,_),_vK=E(_vJ),_vL=E(_vK[1])[1],_=putMVar(_vH,new T(function(){var _vM=E(_vK[2]);return [0,_vM[1],_vM[2],_vM[3],_vM[4],_0,_vM[6]];}));if(!E(E(_vI)[5])){var _vN=A(_vL,[_vF,_]);return takeMVar(_vG);}else{var _vO=A(_7,[E(E(_vF)[1]),_]),_vP=A(_vL,[[0,_vO],_]);return takeMVar(_vG);}},_vQ=unCStr("span"),_vR=function(_vS,_vT){var _vU=new T(function(){return A(_vS,[_vT]);});return function(_vV,_){var _vW=jsCreateElem(toJSStr(E(_vQ))),_vX=jsAppendChild(_vW,E(_vV)[1]),_vY=[0,_vW],_vZ=A(_vU,[_vY,_]);return _vY;};},_w0=function(_){var _w1=_13(_),_w2=A(_vR,[_z,_L,_w1,_]),_w3=_G(_w1,_),_w4=_vE(_),_w5=_13(_),_w6=A(_vR,[_z,[1,_E,new T(function(){return _sf(unAppCStr("hello",_w4),_K);})],_w5,_]),_w7=_G(_w5,_);return _w5;},_w8=unCStr("padding:15px;border-style:dotted"),_w9=unCStr("border-collapse:collapse"),_wa=unCStr("vertical-align:top"),_wb=[0,3],_wc=[0,112],_wd=[1,_wc,_g],_we=function(_wf,_wg){var _wh=new T(function(){return A(_wf,[_wg]);});return function(_wi,_){var _wj=jsCreateElem(toJSStr(_wd)),_wk=jsAppendChild(_wj,E(_wi)[1]),_wl=[0,_wj],_wm=A(_wh,[_wl,_]);return _wl;};},_wn=function(_wo){return _4f(0,E(_wo)[1],_g);},_wp=[0,98],_wq=[1,_wp,_g],_wr=function(_ws,_wt){var _wu=new T(function(){return A(_ws,[_wt]);});return function(_wv,_){var _ww=jsCreateElem(toJSStr(_wq)),_wx=jsAppendChild(_ww,E(_wv)[1]),_wy=[0,_ww],_wz=A(_wu,[_wy,_]);return _wy;};},_wA=[1,_1c],_wB=unCStr("result: "),_wC=function(_wD){var _wE=new T(function(){return _wr(_z,new T(function(){return _wn(_wD);}));});return function(_wF,_){return [0,[0,function(_wG,_){var _wH=_G(_wG,_),_wI=_z(_wB,_wG,_),_wJ=A(_wE,[_wG,_]);return _wG;},_wA],_wF];};},_wK=unCStr(" numbers and append the result using a fold"),_wL=[0,0],_wM=[1,_wL],_wN=[0,_Z,_wM],_wO=function(_wP,_){return [0,_wN,_wP];},_wQ=function(_wR,_wS,_){var _wT=A(_wS,[_]);return _wR;},_wU=function(_wV,_wW,_){var _wX=A(_wW,[_]);return new T(function(){return A(_wV,[_wX]);});},_wY=[0,_wU,_wQ],_wZ=function(_x0){var _x1=E(_x0);return _x1[0]==0?0:E(_x1[1])[1]+_wZ(_x1[2])|0;},_x2=function(_x3){return [0,_wZ(_x3)];},_x4=function(_x5,_x6){return [0,E(_x5)[1]+E(_x6)[1]|0];},_x7=[0,_wL,_x4,_x2],_x8=function(_x9,_xa){var _xb=E(_xa);return _xb[0]==0?[0]:[1,new T(function(){return A(_x9,[_xb[1]]);})];},_xc=function(_xd,_xe,_xf,_xg,_xh,_xi){var _xj=new T(function(){return _bg(_xd);});return A(_xe,[new T(function(){return A(_xg,[_xi]);}),function(_xk){var _xl=E(_xk),_xm=E(_xl[1]);return A(_xe,[new T(function(){return A(_xh,[_xl[2]]);}),function(_xn){var _xo=E(_xn),_xp=E(_xo[1]);return A(_xf,[[0,[0,new T(function(){return A(_xj,[_xm[1],_xp[1]]);}),new T(function(){var _xq=E(_xm[2]);if(!_xq[0]){return [0];}else{var _xr=E(_xp[2]);return _xr[0]==0?[0]:[1,new T(function(){return A(_xq[1],[_xr[1]]);})];}})],_xo[2]]]);}]);}]);},_xs=function(_xt){return E(E(_xt)[1]);},_xu=function(_xv,_xw,_xx,_xy,_xz,_xA){var _xB=new T(function(){return _8F(_xv);});return function(_xC){var _xD=E(_xw);return _xc(_xB,_xD[1],_xD[3],function(_xE){return A(new T(function(){var _xF=new T(function(){return _bg(_xy);});return A(_xs,[_xx,function(_xG){return [0,new T(function(){var _xH=E(E(_xG)[1]);return [0,_xH[1],new T(function(){return _x8(_xF,_xH[2]);})];}),new T(function(){return E(E(_xG)[2]);})];}]);}),[new T(function(){return A(_xz,[_xE]);})]);},_xA,_xC);};},_xI=function(_xJ,_xK){while(1){var _xL=(function(_xM,_xN){var _xO=E(_xN);if(!_xO[0]){return E(_xM);}else{_xJ=new T(function(){return _xu(_8D,_3z,_wY,_x7,_xM,_xO[1]);});_xK=_xO[2];return null;}})(_xJ,_xK);if(_xL!=null){return _xL;}}},_xP=function(_xQ,_xR,_xS){var _xT=function(_xU,_xV){var _xW=new T(function(){return _oC(function(_xX){return A(_xQ,[_xX,_xU,function(_xY){return A(_xV,[new T(function(){return [0, -E(_xY)[1]];})]);}]);});});return _db(_p5(function(_xZ){var _y0=E(_xZ);if(_y0[0]==4){var _y1=E(_y0[1]);return _y1[0]==0?A(_xQ,[_y0,_xU,_xV]):E(E(_y1[1])[1])==45?E(_y1[2])[0]==0?E([1,function(_y2){return A(_mT,[_y2,function(_y3){return E(_xW);}]);}]):A(_xQ,[_y0,_xU,_xV]):A(_xQ,[_y0,_xU,_xV]);}else{return A(_xQ,[_y0,_xU,_xV]);}}),new T(function(){return _pb(_xT,_xV);}));};return _xT(_xR,_xS);},_y4=function(_y5,_y6){return [2];},_y7=function(_qy,_qd){return _y4(_qy,_qd);},_y8=function(_y9){var _ya=E(_y9);return _ya[0]==0?[1,new T(function(){return _fI(new T(function(){return _lY(E(_ya[1])[1]);}),_fz,_ya[2]);})]:E(_ya[2])[0]==0?E(_ya[3])[0]==0?[1,new T(function(){return _fI(_fy,_fz,_ya[1]);})]:[0]:[0];},_yb=function(_yc){var _yd=E(_yc);if(_yd[0]==5){var _ye=_y8(_yd[1]);if(!_ye[0]){return E(_y4);}else{var _yf=new T(function(){return [0,_gK(_ye[1])];});return function(_yg,_yh){return A(_yh,[_yf]);};}}else{return E(_y7);}},_yi=function(_qy,_qd){return _xP(_yb,_qy,_qd);},_yj=function(_yk,_yl){return _pu(_yi,_yl);},_ym=new T(function(){return _pu(_yi,_ec);}),_yn=function(_qd){return _d1(_ym,_qd);},_yo=function(_yp){var _yq=new T(function(){return _xP(_yb,_yp,_ec);});return function(_as){return _d1(_yq,_as);};},_yr=[0,_yo,_yn,_yi,_yj],_ys=function(_yt,_yu){return _4f(0,E(_yt)[1],_yu);},_yv=function(_yw,_yx){return _38(_ys,_yw,_yx);},_yy=function(_yz,_yA,_yB){return _4f(E(_yz)[1],E(_yA)[1],_yB);},_yC=[0,_yy,_wn,_yv],_yD=unCStr("Int"),_yE=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5M,_94,_yD],_yF=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_yE,_g],_yG=function(_yH){return E(_yF);},_yI=new T(function(){return _bo(_8D,_8E,_yG,_yC,_yr);}),_yJ=new T(function(){return A(_yI,[_9,_sM,_9]);}),_yK=new T(function(){return _tY(_yJ,_5D);}),_yL=function(_yM,_){var _yN=A(_yK,[_yM,_]),_yO=E(_yN),_yP=E(_yO[1]);return [0,[0,function(_yQ,_){var _yR=A(_yP[1],[_yQ,_]),_yS=_G(_yQ,_);return _yQ;},_yP[2]],_yO[2]];},_yT=new T(function(){return [1,_yL,_yT];}),_yU=function(_yV,_yW){var _yX=E(_yV);if(!_yX){return [0];}else{var _yY=E(_yW);return _yY[0]==0?[0]:[1,_yY[1],new T(function(){return _yU(_yX-1|0,_yY[2]);})];}},_yZ=function(_z0,_z1){return _z0<0?[0]:_yU(_z0,_z1);},_z2=function(_z3,_z4){var _z5=E(_z3)[1];return _z5>0?_yZ(_z5,_z4):[0];},_z6=function(_z7){var _z8=new T(function(){return _xI(_wO,_z2(_z7,_yT));}),_z9=new T(function(){return _we(_z,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _2i(_4f(0,E(_z7)[1],_g),_wK);}));}));});return function(_za,_){var _zb=_4P(_z8,_wC,_za,_),_zc=E(_zb),_zd=E(_zc[1]),_ze=new T(function(){return _we(_11,_zd[1]);});return [0,[0,function(_zf,_){var _zg=A(_z9,[_zf,_]),_zh=A(_ze,[_zf,_]);return _zf;},_zd[2]],_zc[2]];};},_zi=new T(function(){return _z6(_wb);}),_zj=unCStr("center"),_zk=function(_zl,_zm){var _zn=new T(function(){return A(_zl,[_zm]);});return function(_zo,_){var _zp=jsCreateElem(toJSStr(E(_zj))),_zq=jsAppendChild(_zp,E(_zo)[1]),_zr=[0,_zp],_zs=A(_zn,[_zr,_]);return _zr;};},_zt=function(_zu,_){return _zu;},_zv=unCStr("Two counters. One is pure and recursive, the other is stateful"),_zw=new T(function(){return _we(_z,_zv);}),_zx=[8,coercionToken],_zy=unCStr("submit"),_zz=new T(function(){return A(_bo,[_8D,_8E,_9f,_sK,_qz,_9,_zy]);}),_zA=[0,43],_zB=[1,_zA,_g],_zC=[1,_zB],_zD=new T(function(){return A(_zz,[_zC]);}),_zE=new T(function(){return _tY(_zD,_zx);}),_zF=function(_zG,_zH,_zI,_){var _zJ=A(_zH,[_zI,_]),_zK=E(_zJ),_zL=E(_zK[1]);return [0,[0,function(_zM,_){var _zN=_4y(_4x,_zM,_),_zO=A(_1d,[_1j,_zN,_1b,_zG,_]),_zP=A(_zL[1],[_zN,_]);return _zN;},_zL[2]],_zK[2]];},_zQ=new T(function(){return _4k(_1F,_47,_1D,_1A);}),_zR=new T(function(){return _4k(_1F,_47,_1D,_1A);}),_zS=function(_zT,_zU,_zV,_){var _zW=A(_zR,[_zV,_]),_zX=A(_zQ,[new T(function(){return E(E(_zW)[2]);}),_]),_zY=new T(function(){return E(E(_zW)[1]);});return _4P(function(_1z,_){return _zF(_zY,_zT,_1z,_);},function(_zZ){var _A0=new T(function(){return A(_zU,[_zZ]);});return function(_A1,_){var _A2=A(_A0,[_A1,_]),_A3=E(_A2),_A4=E(_A3[1]);return [0,[0,function(_A5,_){var _A6=E(_zY),_A7=jsFind(toJSStr(_A6)),_A8=E(_A7);if(!_A8[0]){return _4F(_A6);}else{var _A9=E(_A8[1]),_Aa=A(_7,[E(_A9[1]),_]),_Ab=jsKillChild(E(_A9)[1],_Aa),_Ac=A(_A4[1],[_A5,_]);return _A5;}},_A4[2]],_A3[2]];};},new T(function(){return E(E(_zX)[2]);}),_);},_Ad=function(_Ae){var _Af=new T(function(){return _Ad(new T(function(){return [0,E(_Ae)[1]+1|0];}));}),_Ag=new T(function(){return _wr(_z,new T(function(){return _wn(_Ae);}));});return function(_as,_vx){return _zS(function(_Ah,_){var _Ai=A(_zE,[_Ah,_]),_Aj=E(_Ai),_Ak=E(_Aj[1]);return [0,[0,function(_Al,_){var _Am=A(_Ag,[_Al,_]),_An=A(_Ak[1],[_Al,_]);return _Al;},_Ak[2]],_Aj[2]];},function(_Ao){return E(_Af);},_as,_vx);};},_Ap=unCStr("main"),_Aq=unCStr("Main"),_Ar=unCStr("Counter"),_As=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_Ap,_Aq,_Ar],_At=function(_Au,_Av){var _Aw=hs_leWord64(_Au,_Av);return E(_Aw)==0?false:true;},_Ax=function(_Ay,_Az,_AA,_AB){var _AC=hs_eqWord64(_Ay,_AA);if(!E(_AC)){var _AD=hs_leWord64(_Ay,_AA);return E(_AD)==0?false:true;}else{return _At(_Az,_AB);}},_AE=function(_AF,_AG){var _AH=E(_AF),_AI=_AH[1],_AJ=_AH[2],_AK=E(_AG),_AL=_AK[1],_AM=_AK[2],_AN=hs_eqWord64(_AI,_AL);if(!E(_AN)){return !_Ax(_AI,_AJ,_AL,_AM)?2:0;}else{var _AO=hs_eqWord64(_AJ,_AM);return E(_AO)==0?!_Ax(_AI,_AJ,_AL,_AM)?2:0:1;}},_AP=unCStr("Failure in Data.Map.balanceL"),_AQ=new T(function(){return err(_AP);}),_AR=function(_AS,_AT,_AU,_AV){var _AW=E(_AV);if(!_AW[0]){var _AX=_AW[1],_AY=E(_AU);if(!_AY[0]){var _AZ=_AY[1],_B0=_AY[2],_B1=_AY[3];if(_AZ<=(imul(3,_AX)|0)){return [0,(1+_AZ|0)+_AX|0,E(E(_AS)),_AT,E(_AY),E(_AW)];}else{var _B2=E(_AY[4]);if(!_B2[0]){var _B3=_B2[1],_B4=E(_AY[5]);if(!_B4[0]){var _B5=_B4[1],_B6=_B4[2],_B7=_B4[3],_B8=_B4[4];if(_B5>=(imul(2,_B3)|0)){var _B9=function(_Ba){var _Bb=E(_B4[5]);return _Bb[0]==0?[0,(1+_AZ|0)+_AX|0,E(_B6),_B7,E([0,(1+_B3|0)+_Ba|0,E(_B0),_B1,E(_B2),E(_B8)]),E([0,(1+_AX|0)+_Bb[1]|0,E(E(_AS)),_AT,E(_Bb),E(_AW)])]:[0,(1+_AZ|0)+_AX|0,E(_B6),_B7,E([0,(1+_B3|0)+_Ba|0,E(_B0),_B1,E(_B2),E(_B8)]),E([0,1+_AX|0,E(E(_AS)),_AT,E(_f),E(_AW)])];},_Bc=E(_B8);return _Bc[0]==0?_B9(_Bc[1]):_B9(0);}else{return [0,(1+_AZ|0)+_AX|0,E(_B0),_B1,E(_B2),E([0,(1+_AX|0)+_B5|0,E(E(_AS)),_AT,E(_B4),E(_AW)])];}}else{return E(_AQ);}}else{return E(_AQ);}}}else{return [0,1+_AX|0,E(E(_AS)),_AT,E(_f),E(_AW)];}}else{var _Bd=E(_AU);if(!_Bd[0]){var _Be=_Bd[1],_Bf=_Bd[2],_Bg=_Bd[3],_Bh=_Bd[5],_Bi=E(_Bd[4]);if(!_Bi[0]){var _Bj=_Bi[1],_Bk=E(_Bh);if(!_Bk[0]){var _Bl=_Bk[1],_Bm=_Bk[2],_Bn=_Bk[3],_Bo=_Bk[4];if(_Bl>=(imul(2,_Bj)|0)){var _Bp=function(_Bq){var _Br=E(_Bk[5]);return _Br[0]==0?[0,1+_Be|0,E(_Bm),_Bn,E([0,(1+_Bj|0)+_Bq|0,E(_Bf),_Bg,E(_Bi),E(_Bo)]),E([0,1+_Br[1]|0,E(E(_AS)),_AT,E(_Br),E(_f)])]:[0,1+_Be|0,E(_Bm),_Bn,E([0,(1+_Bj|0)+_Bq|0,E(_Bf),_Bg,E(_Bi),E(_Bo)]),E([0,1,E(E(_AS)),_AT,E(_f),E(_f)])];},_Bs=E(_Bo);return _Bs[0]==0?_Bp(_Bs[1]):_Bp(0);}else{return [0,1+_Be|0,E(_Bf),_Bg,E(_Bi),E([0,1+_Bl|0,E(E(_AS)),_AT,E(_Bk),E(_f)])];}}else{return [0,3,E(_Bf),_Bg,E(_Bi),E([0,1,E(E(_AS)),_AT,E(_f),E(_f)])];}}else{var _Bt=E(_Bh);return _Bt[0]==0?[0,3,E(_Bt[2]),_Bt[3],E([0,1,E(_Bf),_Bg,E(_f),E(_f)]),E([0,1,E(E(_AS)),_AT,E(_f),E(_f)])]:[0,2,E(E(_AS)),_AT,E(_Bd),E(_f)];}}else{return [0,1,E(E(_AS)),_AT,E(_f),E(_f)];}}},_Bu=unCStr("Failure in Data.Map.balanceR"),_Bv=new T(function(){return err(_Bu);}),_Bw=function(_Bx,_By,_Bz,_BA){var _BB=E(_Bz);if(!_BB[0]){var _BC=_BB[1],_BD=E(_BA);if(!_BD[0]){var _BE=_BD[1],_BF=_BD[2],_BG=_BD[3];if(_BE<=(imul(3,_BC)|0)){return [0,(1+_BC|0)+_BE|0,E(E(_Bx)),_By,E(_BB),E(_BD)];}else{var _BH=E(_BD[4]);if(!_BH[0]){var _BI=_BH[1],_BJ=_BH[2],_BK=_BH[3],_BL=_BH[4],_BM=E(_BD[5]);if(!_BM[0]){var _BN=_BM[1];if(_BI>=(imul(2,_BN)|0)){var _BO=function(_BP){var _BQ=E(_Bx),_BR=E(_BH[5]);return _BR[0]==0?[0,(1+_BC|0)+_BE|0,E(_BJ),_BK,E([0,(1+_BC|0)+_BP|0,E(_BQ),_By,E(_BB),E(_BL)]),E([0,(1+_BN|0)+_BR[1]|0,E(_BF),_BG,E(_BR),E(_BM)])]:[0,(1+_BC|0)+_BE|0,E(_BJ),_BK,E([0,(1+_BC|0)+_BP|0,E(_BQ),_By,E(_BB),E(_BL)]),E([0,1+_BN|0,E(_BF),_BG,E(_f),E(_BM)])];},_BS=E(_BL);return _BS[0]==0?_BO(_BS[1]):_BO(0);}else{return [0,(1+_BC|0)+_BE|0,E(_BF),_BG,E([0,(1+_BC|0)+_BI|0,E(E(_Bx)),_By,E(_BB),E(_BH)]),E(_BM)];}}else{return E(_Bv);}}else{return E(_Bv);}}}else{return [0,1+_BC|0,E(E(_Bx)),_By,E(_BB),E(_f)];}}else{var _BT=E(_BA);if(!_BT[0]){var _BU=_BT[1],_BV=_BT[2],_BW=_BT[3],_BX=_BT[5],_BY=E(_BT[4]);if(!_BY[0]){var _BZ=_BY[1],_C0=_BY[2],_C1=_BY[3],_C2=_BY[4],_C3=E(_BX);if(!_C3[0]){var _C4=_C3[1];if(_BZ>=(imul(2,_C4)|0)){var _C5=function(_C6){var _C7=E(_Bx),_C8=E(_BY[5]);return _C8[0]==0?[0,1+_BU|0,E(_C0),_C1,E([0,1+_C6|0,E(_C7),_By,E(_f),E(_C2)]),E([0,(1+_C4|0)+_C8[1]|0,E(_BV),_BW,E(_C8),E(_C3)])]:[0,1+_BU|0,E(_C0),_C1,E([0,1+_C6|0,E(_C7),_By,E(_f),E(_C2)]),E([0,1+_C4|0,E(_BV),_BW,E(_f),E(_C3)])];},_C9=E(_C2);return _C9[0]==0?_C5(_C9[1]):_C5(0);}else{return [0,1+_BU|0,E(_BV),_BW,E([0,1+_BZ|0,E(E(_Bx)),_By,E(_f),E(_BY)]),E(_C3)];}}else{return [0,3,E(_C0),_C1,E([0,1,E(E(_Bx)),_By,E(_f),E(_f)]),E([0,1,E(_BV),_BW,E(_f),E(_f)])];}}else{var _Ca=E(_BX);return _Ca[0]==0?[0,3,E(_BV),_BW,E([0,1,E(E(_Bx)),_By,E(_f),E(_f)]),E(_Ca)]:[0,2,E(E(_Bx)),_By,E(_f),E(_BT)];}}else{return [0,1,E(E(_Bx)),_By,E(_f),E(_f)];}}},_Cb=function(_Cc,_Cd,_Ce,_Cf,_Cg,_Ch){var _Ci=E(_Ch);if(!_Ci[0]){var _Cj=_Ci[2],_Ck=_Ci[3],_Cl=_Ci[4],_Cm=_Ci[5];switch(_AE([0,_Cc,_Cd,_Ce,_Cf],_Cj)){case 0:return _AR(_Cj,_Ck,_Cb(_Cc,_Cd,_Ce,_Cf,_Cg,_Cl),_Cm);case 1:return [0,_Ci[1],E([0,_Cc,_Cd,_Ce,_Cf]),_Cg,E(_Cl),E(_Cm)];default:return _Bw(_Cj,_Ck,_Cl,_Cb(_Cc,_Cd,_Ce,_Cf,_Cg,_Cm));}}else{return [0,1,E([0,_Cc,_Cd,_Ce,_Cf]),_Cg,E(_f),E(_f)];}},_Cn=[0,_Z,_wA],_Co=function(_Cp,_){return [0,[0,_Z,[1,_Cp]],_Cp];},_Cq=[1,_1c],_Cr=function(_Cs){var _Ct=new T(function(){return [0,E(_Cs)[1]+1|0];}),_Cu=new T(function(){return _wr(_z,new T(function(){return _wn(_Cs);}));});return function(_as,_vx){return _4P(function(_Cv,_){return [0,[0,_Cu,_Cq],_Cv];},function(_Cw,_Cx,_){return (function(_Cx,_){return _4P(_Co,function(_Cy){return function(_Cz,_){return [0,_Cn,new T(function(){var _CA=E(_Cy);return [0,_CA[1],_CA[2],_CA[3],_CA[4],_CA[5],new T(function(){return _Cb(I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_As,_g,_Ct,_CA[6]);})];})];};},_Cx,_);})(_Cx,_);},_as,_vx);};},_CB=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_As,_g],_CC=function(_CD){return E(_CB);},_CE=function(_CF,_CG,_CH,_CI,_CJ){while(1){var _CK=E(_CJ);if(!_CK[0]){switch(_AE([0,_CF,_CG,_CH,_CI],_CK[2])){case 0:_CJ=_CK[4];continue;case 1:return [1,_CK[3]];default:_CJ=_CK[5];continue;}}else{return [0];}}},_CL=function(_CM,_CN){var _CO=E(_CM),_CP=_CO[1],_CQ=_CO[2],_CR=_CO[3],_CS=_CO[4],_CT=E(_CN);if(!_CT[0]){switch(_AE(_CO,_CT[2])){case 0:return _CE(_CP,_CQ,_CR,_CS,_CT[4]);case 1:return [1,_CT[3]];default:return _CE(_CP,_CQ,_CR,_CS,_CT[5]);}}else{return [0];}},_CU=function(_CV,_CW,_CX,_CY){var _CZ=E(_CW),_D0=_CZ[1],_D1=_CZ[3],_D2=new T(function(){return A(_CY,[_90]);}),_D3=new T(function(){return A(_D1,[_9]);});return A(_D0,[new T(function(){return A(_D0,[_CX,function(_D4){return A(_D1,[new T(function(){var _D5=E(_CV);return E(E(_D4)[6]);})]);}]);}),function(_D6){var _D7=_CL(_D2,_D6);return _D7[0]==0?E(_D3):A(_D1,[[1,_D7[1]]]);}]);},_D8=new T(function(){return _CU(_1F,_47,_1D,_CC);}),_D9=function(_Da){var _Db=new T(function(){return _Ad(_Da);});return function(_Dc,_){var _Dd=A(_Db,[_Dc,_]),_De=E(_Dd),_Df=E(_De[1]),_Dg=_4P(_zE,function(_Dh){return function(_Cx,_){return _4P(function(_Di,_){var _Dj=A(_D8,[_Di,_]);return [0,[0,_zt,new T(function(){var _Dk=E(E(_Dj)[1]);return _Dk[0]==0?E([1,_Da]):E(_Dk);})],new T(function(){return E(E(_Dj)[2]);})];},_Cr,_Cx,_);};},_De[2],_),_Dl=E(_Dg),_Dm=E(_Dl[1]),_Dn=new T(function(){return _zk(_11,function(_Do,_){var _Dp=A(_Df[1],[_Do,_]),_Dq=A(_Dm[1],[_Do,_]);return _Do;});});return [0,[0,function(_Dr,_){var _Ds=A(_zw,[_Dr,_]),_Dt=_G(_Dr,_),_Du=A(_Dn,[_Dr,_]);return _Dr;},new T(function(){var _Dv=E(_Df[2]);return _Dv[0]==0?E(_Dm[2]):E(_Dv);})],_Dl[2]];};},_Dw=new T(function(){return _D9(_wb);}),_Dx=[0,4],_Dy=function(_Dz,_DA){return [1,_DA,new T(function(){return _Dy(_Dz,new T(function(){return A(_Dz,[_DA]);}));})];},_DB=[0,1],_DC=[1,_DB,_g],_DD=[1,_wL,_g],_DE=function(_DF,_DG,_DH){var _DI=E(_DG);if(!_DI[0]){return [0];}else{var _DJ=E(_DH);return _DJ[0]==0?[0]:[1,new T(function(){return A(_DF,[_DI[1],_DJ[1]]);}),new T(function(){return _DE(_DF,_DI[2],_DJ[2]);})];}},_DK=function(_DL){return _DE(_x4,[1,_wL,_DL],new T(function(){return _2i(_DL,_DD);}));},_DM=new T(function(){return _Dy(_DK,_DC);}),_DN=unCStr(" rows of the Pascal triangle "),_DO=function(_DP){var _DQ=new T(function(){return _38(_ys,_DP,_g);});return function(_as,_vx){return _z(_DQ,_as,_vx);};},_DR=unCStr("text-align:center"),_DS=unCStr("style"),_DT=function(_DU,_DV){var _DW=new T(function(){return _we(_DO,_DU);});return [1,function(_DX,_){var _DY=A(_DW,[_DX,_]),_DZ=A(_1d,[_1j,_DY,_DS,_DR,_]);return _DY;},_DV];},_E0=function(_E1,_E2){var _E3=E(_E1);if(!_E3[0]){return [0];}else{var _E4=_E3[1];return _E2>1?_DT(_E4,new T(function(){return _E0(_E3[2],_E2-1|0);})):_DT(_E4,_g);}},_E5=function(_E6){var _E7=new T(function(){return _we(_z,new T(function(){return unAppCStr("Show ",new T(function(){return _2i(_4f(0,E(_E6)[1],_g),_DN);}));}));});return function(_E8,_){return [0,[0,function(_E9,_){var _Ea=A(_E7,[_E9,_]),_Eb=_8q(new T(function(){var _Ec=E(_E6)[1];return _Ec>0?_E0(_DM,_Ec):[0];}),_E9,_);return _E9;},_9],_E8];};},_Ed=new T(function(){return _E5(_Dx);}),_Ee=unCStr("Different input elements:"),_Ef=new T(function(){return _we(_z,_Ee);}),_Eg=unCStr(" returns: "),_Eh=function(_Ei){var _Ej=new T(function(){return _wr(_z,[1,_E,new T(function(){return _sf(_Ei,_K);})]);});return function(_Ek,_){return [0,[0,function(_El,_){var _Em=_z(_Eg,_El,_),_En=A(_Ej,[_El,_]);return _El;},_Cq],_Ek];};},_Eo=unCStr("blue"),_Ep=[1,_Eo,_g],_Eq=unCStr("green"),_Er=[1,_Eq,_Ep],_Es=unCStr("red"),_Et=[1,_Es,_Er],_Eu=function(_Ev){return E(E(_Ev)[15]);},_Ew=function(_Ex,_Ey,_){var _Ez=jsGet(_Ex,toJSStr(E(_Ey)));return new T(function(){return fromJSStr(_Ez);});},_EA=function(_EB,_EC,_){return _Ew(E(_EB)[1],_EC,_);},_ED=new T(function(){return A(_9f,[_6F]);}),_EE=unCStr("name"),_EF=unCStr("true"),_EG=unCStr("radio"),_EH=function(_EI,_EJ,_EK,_EL){var _EM=new T(function(){return _8H(_EJ);}),_EN=new T(function(){return _4k([0,coercionToken],_42(_EM),function(_EO){return _bd(_EM,_EO);},function(_EP,_EQ){return _bi(_EM,_EP,_EQ);});}),_ER=new T(function(){return _40(_EM);}),_ES=new T(function(){return _40(_EM);}),_ET=new T(function(){return _3A(_EM);}),_EU=new T(function(){return _3A(_EM);}),_EV=new T(function(){return _40(_EM);}),_EW=new T(function(){return _3A(_EM);}),_EX=new T(function(){return _40(_EM);}),_EY=new T(function(){return _3A(_EM);}),_EZ=new T(function(){return _bb(_EI);}),_F0=new T(function(){return _Eu(_EI);}),_F1=new T(function(){return _bm(_EL);});return function(_F2,_F3){return function(_F4){return A(_ET,[new T(function(){return A(_EN,[_F4]);}),function(_F5){var _F6=new T(function(){return E(E(_F5)[1]);}),_F7=new T(function(){return _8L(_EJ,function(_){return jsFind(toJSStr(E(_F6)));});});return A(_EY,[new T(function(){var _F8=new T(function(){return E(E(_F5)[2]);});return A(_EX,[[0,_F8,_F8]]);}),function(_F9){return A(_EW,[new T(function(){return A(_EV,[[0,_1c,new T(function(){var _Fa=E(E(_F9)[1]);return [0,_Fa[1],_Fa[2],_ba,_Fa[4],_Fa[5],_Fa[6]];})]]);}),function(_Fb){return A(_EU,[new T(function(){return A(_F7,[new T(function(){return E(E(_Fb)[2]);})]);}),function(_Fc){return A(_ET,[new T(function(){var _Fd=E(_Fc),_Fe=_Fd[2],_Ff=E(_Fd[1]);return _Ff[0]==0?A(_ES,[[0,_g,_Fe]]):A(_8L,[_EJ,function(_){return _EA(_Ff[1],_6T,_);},_Fe]);}),function(_Fg){var _Fh=new T(function(){return !_tn(E(_Fg)[1],_EF)?[0]:E([1,_F2]);});return A(_ER,[[0,[0,new T(function(){return A(_F0,[new T(function(){return A(_EZ,[_F6,_EG,new T(function(){var _Fi=A(_EK,[_F2]),_Fj=E(_ED),_Fk=hs_eqWord64(_Fi[1],_Fj[1]);if(!E(_Fk)){return A(_F1,[_F2]);}else{var _Fl=hs_eqWord64(_Fi[2],_Fj[2]);return E(_Fl)==0?A(_F1,[_F2]):E(_F2);}}),new T(function(){return E(_Fh)[0]==0?false:true;}),_9]);}),[1,[0,_EE,_F3],_g]]);}),new T(function(){var _Fm=E(_Fh);return _Fm[0]==0?[0]:[1,_Fm[1]];})],new T(function(){return E(E(_Fg)[2]);})]]);}]);}]);}]);}]);}]);};};},_Fn=new T(function(){return _6G(_98,_9d);}),_Fo=new T(function(){return _sF(_sv);}),_Fp=new T(function(){return _EH(_8D,_8E,_Fn,_Fo);}),_Fq=function(_Fr){var _Fs=E(_Fr);if(!_Fs[0]){return [0];}else{var _Ft=_Fs[1];return [1,function(_Fu){var _Fv=new T(function(){return _tY(new T(function(){return A(_Fp,[_Ft,_Fu]);}),_zx);});return function(_Fw,_){var _Fx=A(_Fv,[_Fw,_]),_Fy=E(_Fx),_Fz=E(_Fy[1]);return [0,[0,function(_FA,_){var _FB=_z(_Ft,_FA,_),_FC=A(_Fz[1],[_FA,_]);return _FA;},_Fz[2]],_Fy[2]];};},new T(function(){return _Fq(_Fs[2]);})];}},_FD=new T(function(){return _Fq(_Et);}),_FE=function(_FF){return E(E(_FF)[1]);},_FG=function(_FH,_FI){var _FJ=new T(function(){return _8F(_FI);}),_FK=new T(function(){return _FE(_FJ);}),_FL=new T(function(){return _bg(_FJ);}),_FM=function(_FN){var _FO=E(_FN);if(!_FO[0]){return [0,_FK,_9];}else{var _FP=E(_FO[1]),_FQ=_FM(_FO[2]);return [0,new T(function(){return A(_FL,[_FP[1],_FQ[1]]);}),new T(function(){var _FR=E(_FP[2]);return _FR[0]==0?E(_FQ[2]):E(_FR);})];}},_FS=new T(function(){return _40(_FH);}),_FT=new T(function(){return _4k([0,coercionToken],_42(_FH),function(_FU){return _bd(_FH,_FU);},function(_FV,_FW){return _bi(_FH,_FV,_FW);});}),_FX=new T(function(){return _40(_FH);}),_FY=new T(function(){return _3A(_FH);}),_FZ=new T(function(){return _3A(_FH);}),_G0=new T(function(){return _3A(_FH);}),_G1=new T(function(){return _3A(_FH);});return function(_G2,_G3){return A(_G1,[new T(function(){return A(_FT,[_G3]);}),function(_G4){return A(_G0,[new T(function(){var _G5=new T(function(){return E(E(_G4)[1]);}),_G6=function(_G7){var _G8=E(_G7);if(!_G8[0]){return function(_G9){return A(_FX,[[0,_g,_G9]]);};}else{var _Ga=new T(function(){return _G6(_G8[2]);}),_Gb=new T(function(){return A(_G8[1],[_G5]);});return function(_Gc){return A(_FZ,[new T(function(){return A(_Gb,[_Gc]);}),function(_Gd){var _Ge=new T(function(){return E(E(_Gd)[1]);});return A(_FY,[new T(function(){return A(_Ga,[new T(function(){return E(E(_Gd)[2]);})]);}),function(_Gf){return A(_FX,[[0,[1,_Ge,new T(function(){return E(E(_Gf)[1]);})],new T(function(){return E(E(_Gf)[2]);})]]);}]);}]);};}};return A(_G6,[_G2,new T(function(){return E(E(_G4)[2]);})]);}),function(_Gg){var _Gh=new T(function(){var _Gi=_FM(E(_Gg)[1]);return [0,_Gi[1],_Gi[2]];});return A(_FS,[[0,[0,new T(function(){return E(E(_Gh)[1]);}),new T(function(){var _Gj=E(E(_Gh)[2]);return _Gj[0]==0?[0]:[1,_Gj[1]];})],new T(function(){return E(E(_Gg)[2]);})]]);}]);}]);};},_Gk=new T(function(){return _FG(_3z,_8D);}),_Gl=new T(function(){return A(_Gk,[_FD]);}),_Gm=function(_Gn){var _Go=new T(function(){return _wr(_z,new T(function(){return _38(_sl,_Gn,_g);}));});return function(_Gp,_){return [0,[0,function(_Gq,_){var _Gr=_z(_Eg,_Gq,_),_Gs=A(_Go,[_Gq,_]);return _Gq;},_Cq],_Gp];};},_Gt=new T(function(){return _wr(_z,_Eq);}),_Gu=unCStr("checkbox"),_Gv=function(_Gw,_Gx){var _Gy=new T(function(){return _8H(_Gx);}),_Gz=new T(function(){return _4k([0,coercionToken],_42(_Gy),function(_GA){return _bd(_Gy,_GA);},function(_GB,_GC){return _bi(_Gy,_GB,_GC);});}),_GD=new T(function(){return _40(_Gy);}),_GE=new T(function(){return _40(_Gy);}),_GF=new T(function(){return _3A(_Gy);}),_GG=new T(function(){return _3A(_Gy);}),_GH=new T(function(){return _40(_Gy);}),_GI=new T(function(){return _3A(_Gy);}),_GJ=new T(function(){return _40(_Gy);}),_GK=new T(function(){return _3A(_Gy);}),_GL=new T(function(){return _bb(_Gw);});return function(_GM,_GN){var _GO=new T(function(){return !E(_GM)?[0]:E(_EF);});return function(_GP){return A(_GF,[new T(function(){return A(_Gz,[_GP]);}),function(_GQ){var _GR=new T(function(){return E(E(_GQ)[1]);}),_GS=new T(function(){return _8L(_Gx,function(_){return jsFind(toJSStr(E(_GR)));});}),_GT=new T(function(){return A(_GL,[_GR,_Gu,_GN,_GM,_9]);});return A(_GK,[new T(function(){var _GU=new T(function(){return E(E(_GQ)[2]);});return A(_GJ,[[0,_GU,_GU]]);}),function(_GV){return A(_GI,[new T(function(){return A(_GH,[[0,_1c,new T(function(){var _GW=E(E(_GV)[1]);return [0,_GW[1],_GW[2],_ba,_GW[4],_GW[5],_GW[6]];})]]);}),function(_GX){return A(_GG,[new T(function(){return A(_GS,[new T(function(){return E(E(_GX)[2]);})]);}),function(_GY){return A(_GF,[new T(function(){var _GZ=E(_GY),_H0=_GZ[2],_H1=E(_GZ[1]);return _H1[0]==0?A(_GE,[[0,_GO,_H0]]):A(_8L,[_Gx,function(_){return _EA(_H1[1],_6T,_);},_H0]);}),function(_H2){return A(_GD,[[0,[0,_GT,[1,[0,new T(function(){return !_tn(E(_H2)[1],_EF)?[0]:E([1,_GN,_g]);})]]],new T(function(){return E(E(_H2)[2]);})]]);}]);}]);}]);}]);}]);};};},_H3=new T(function(){return _Gv(_8D,_8E);}),_H4=unCStr("Green"),_H5=new T(function(){return A(_H3,[_0,_H4]);}),_H6=function(_H7,_){var _H8=A(_H5,[_H7,_]),_H9=E(_H8),_Ha=E(_H9[1]);return [0,[0,function(_Hb,_){var _Hc=A(_Ha[1],[_Hb,_]),_Hd=A(_Gt,[_Hb,_]);return _Hb;},_Ha[2]],_H9[2]];},_He=new T(function(){return _tY(_H6,_zx);}),_Hf=new T(function(){return _wr(_z,_Eo);}),_Hg=new T(function(){return A(_H3,[_0,_Eo]);}),_Hh=function(_Hi,_){var _Hj=A(_Hg,[_Hi,_]),_Hk=E(_Hj),_Hl=E(_Hk[1]);return [0,[0,function(_Hm,_){var _Hn=A(_Hl[1],[_Hm,_]),_Ho=A(_Hf,[_Hm,_]);return _Hm;},_Hl[2]],_Hk[2]];},_Hp=new T(function(){return _tY(_Hh,_zx);}),_Hq=new T(function(){return _wr(_z,_Es);}),_Hr=unCStr("Red"),_Hs=new T(function(){return A(_H3,[_0,_Hr]);}),_Ht=function(_Hu,_){var _Hv=A(_Hs,[_Hu,_]),_Hw=E(_Hv),_Hx=E(_Hw[1]);return [0,[0,function(_Hy,_){var _Hz=A(_Hx[1],[_Hy,_]),_HA=A(_Hq,[_Hy,_]);return _Hy;},_Hx[2]],_Hw[2]];},_HB=new T(function(){return _tY(_Ht,_zx);}),_HC=function(_HD,_){var _HE=A(_HB,[_HD,_]),_HF=E(_HE),_HG=E(_HF[1]),_HH=A(_He,[_HF[2],_]),_HI=E(_HH),_HJ=E(_HI[1]),_HK=A(_Hp,[_HI[2],_]),_HL=E(_HK),_HM=E(_HL[1]);return [0,[0,function(_HN,_){var _HO=A(_HG[1],[_HN,_]),_HP=A(_HJ[1],[_HN,_]),_HQ=A(_HM[1],[_HN,_]);return _HN;},new T(function(){var _HR=E(_HG[2]);if(!_HR[0]){return [0];}else{var _HS=E(_HJ[2]);if(!_HS[0]){return [0];}else{var _HT=E(_HM[2]);return _HT[0]==0?[0]:[1,new T(function(){var _HU=function(_HV){var _HW=E(_HV);return _HW[0]==0?E(new T(function(){var _HX=function(_HY){var _HZ=E(_HY);return _HZ[0]==0?E(E(_HT[1])[1]):[1,_HZ[1],new T(function(){return _HX(_HZ[2]);})];};return _HX(E(_HS[1])[1]);})):[1,_HW[1],new T(function(){return _HU(_HW[2]);})];};return _HU(E(_HR[1])[1]);})];}}})],_HL[2]];},_I0=function(_I1){var _I2=new T(function(){return _wr(_z,[1,_E,new T(function(){return _sf(_I1,_K);})]);});return function(_I3,_){return [0,[0,function(_I4,_){var _I5=_z(_Eg,_I4,_),_I6=A(_I2,[_I4,_]);return _I4;},_Cq],_I3];};},_I7=new T(function(){return _qw(_qi);}),_I8=function(_I9){return E(E(_I9)[11]);},_Ia=function(_Ib,_Ic,_Id,_Ie){var _If=new T(function(){return _8H(_Ic);}),_Ig=new T(function(){return _42(_If);}),_Ih=new T(function(){return _4k([0,coercionToken],_Ig,function(_Ii){return _bd(_If,_Ii);},function(_Ij,_Ik){return _bi(_If,_Ij,_Ik);});}),_Il=new T(function(){return _40(_If);}),_Im=new T(function(){return _3A(_If);}),_In=new T(function(){return _3A(_If);}),_Io=new T(function(){return _40(_If);}),_Ip=new T(function(){return _3A(_If);}),_Iq=new T(function(){return _40(_If);}),_Ir=new T(function(){return _3A(_If);}),_Is=new T(function(){return _3A(_If);}),_It=new T(function(){return _I8(_Ib);});return function(_Iu,_Iv){return A(_Is,[new T(function(){return A(_Ih,[_Iv]);}),function(_Iw){var _Ix=new T(function(){return E(E(_Iw)[1]);}),_Iy=new T(function(){return _aI(_Ig,function(_Iz){return _8L(_Ic,_Iz);},_Id,_Ie,_Ib,_Ix);});return A(_Ir,[new T(function(){var _IA=new T(function(){return E(E(_Iw)[2]);});return A(_Iq,[[0,_IA,_IA]]);}),function(_IB){return A(_Ip,[new T(function(){return A(_Io,[[0,_1c,new T(function(){var _IC=E(E(_IB)[1]);return [0,_IC[1],_IC[2],_ba,_IC[4],_IC[5],_IC[6]];})]]);}),function(_ID){return A(_In,[new T(function(){return A(_Iy,[new T(function(){return E(E(_ID)[2]);})]);}),function(_IE){return A(_Im,[new T(function(){return A(_Iu,[new T(function(){return E(E(_IE)[2]);})]);}),function(_IF){var _IG=E(_IF);return A(_Il,[[0,[0,new T(function(){return A(_It,[_Ix,E(_IG[1])[1]]);}),new T(function(){var _IH=E(E(_IE)[1]);return _IH[0]==2?[1,_IH[1]]:[0];})],_IG[2]]]);}]);}]);}]);}]);}]);};},_II=new T(function(){return _Ia(_8D,_8E,_Fn,_I7);}),_IJ=new T(function(){return _sf(_Eo,_K);}),_IK=new T(function(){return _sf(_Eo,_K);}),_IL=new T(function(){return A(_9f,[_6F]);}),_IM=new T(function(){var _IN=A(_Fn,[_Eo]),_IO=E(_IL),_IP=hs_eqWord64(_IN[1],_IO[1]);if(!E(_IP)){return [1,_E,_IJ];}else{var _IQ=hs_eqWord64(_IN[2],_IO[2]);return E(_IQ)==0?[1,_E,_IK]:E(_Eo);}}),_IR=[0,_6R,_IM],_IS=[1,_IR,_g],_IT=new T(function(){return _1s(_7o,_IS);}),_IU=new T(function(){return _sf(_Eq,_K);}),_IV=new T(function(){return _sf(_Eq,_K);}),_IW=new T(function(){var _IX=A(_Fn,[_Eq]),_IY=E(_IL),_IZ=hs_eqWord64(_IX[1],_IY[1]);if(!E(_IZ)){return [1,_E,_IU];}else{var _J0=hs_eqWord64(_IX[2],_IY[2]);return E(_J0)==0?[1,_E,_IV]:E(_Eq);}}),_J1=[0,_6R,_IW],_J2=[1,_J1,_g],_J3=new T(function(){return _1s(_7o,_J2);}),_J4=new T(function(){return _sf(_Es,_K);}),_J5=new T(function(){return _sf(_Es,_K);}),_J6=new T(function(){var _J7=A(_Fn,[_Es]),_J8=E(_IL),_J9=hs_eqWord64(_J7[1],_J8[1]);if(!E(_J9)){return [1,_E,_J4];}else{var _Ja=hs_eqWord64(_J7[2],_J8[2]);return E(_Ja)==0?[1,_E,_J5]:E(_Es);}}),_Jb=[0,_6R,_J6],_Jc=[1,_Jb,_g],_Jd=new T(function(){return _1s(_7o,_Jc);}),_Je=function(_Jf,_){var _Jg=A(_Jd,[_Jf,_]),_Jh=_z(_Es,_Jg,_),_Ji=A(_J3,[_Jf,_]),_Jj=_z(_Eq,_Ji,_),_Jk=A(_IT,[_Jf,_]),_Jl=_z(_Eo,_Jk,_);return _Jf;},_Jm=[1,_Es],_Jn=[0,_Je,_Jm],_Jo=function(_Jp,_){return [0,_Jn,_Jp];},_Jq=new T(function(){return A(_II,[_Jo]);}),_Jr=new T(function(){return _tY(_Jq,_zx);}),_Js=function(_Jt,_){var _Ju=_4P(_HC,_Gm,_Jt,_),_Jv=E(_Ju),_Jw=_4P(_Gl,_Eh,_Jv[2],_),_Jx=E(_Jw),_Jy=_4P(_Jr,_I0,_Jx[2],_),_Jz=E(_Jy),_JA=E(_Jz[1]);return [0,[0,function(_JB,_){var _JC=A(_Ef,[_JB,_]),_JD=A(E(_Jv[1])[1],[_JB,_]),_JE=_G(_JB,_),_JF=_G(_JB,_),_JG=A(E(_Jx[1])[1],[_JB,_]),_JH=_G(_JB,_),_JI=_G(_JB,_),_JJ=A(_JA[1],[_JB,_]),_JK=_G(_JB,_);return _JB;},_JA[2]],_Jz[2]];},_JL=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_JM=new T(function(){return _we(_z,_JL);}),_JN=function(_JO){var _JP=jsShow(E(_JO)[1]);return fromJSStr(_JP);},_JQ=function(_JR){var _JS=new T(function(){return _JN(_JR);});return function(_as){return _2i(_JS,_as);};},_JT=function(_JU,_JV,_JW){var _JX=E(_JW);if(!_JX[0]){return [0];}else{var _JY=_JX[2],_JZ=E(_JX[1]);return _JU!=_JZ[1]?[1,_JZ,new T(function(){return _JT(_JU,_JV,_JY);})]:_2i(_JV,new T(function(){return _JT(_JU,_JV,_JY);}));}},_K0=[0,45],_K1=function(_K2,_K3,_K4){var _K5=new T(function(){return A(_K2,[[0, -_K4]]);}),_K6=new T(function(){return E(_K3)[1]<=6?function(_K7){return [1,_K0,new T(function(){return A(_K5,[_K7]);})];}:function(_K8){return [1,_4e,[1,_K0,new T(function(){return A(_K5,[[1,_4d,_K8]]);})]];};});if(_K4>=0){var _K9=isDoubleNegativeZero(_K4);return E(_K9)==0?A(_K2,[[0,_K4]]):E(_K6);}else{return E(_K6);}},_Ka=unCStr("canvas"),_Kb=unCStr("id"),_Kc=unCStr("canvas"),_Kd=function(_Ke,_Kf){var _Kg=new T(function(){return A(_Ke,[_Kf]);});return function(_Kh,_){var _Ki=jsCreateElem(toJSStr(E(_Kc))),_Kj=jsAppendChild(_Ki,E(_Kh)[1]),_Kk=[0,_Ki],_Kl=A(_Kg,[_Kk,_]);return _Kk;};},_Km=new T(function(){return _Kd(_11,_Z);}),_Kn=function(_Ko,_){var _Kp=A(_Km,[_Ko,_]),_Kq=A(_1d,[_1j,_Kp,_Kb,_Ka,_]);return _Kp;},_Kr=[0,_Kn,_Cq],_Ks=function(_Kt,_){return [0,_Kr,_Kt];},_Ku=unCStr("Pattern match failure in do expression at main.hs:190:5-12"),_Kv=function(_Kw,_Kx){while(1){var _Ky=E(_Kx);if(!_Ky[0]){return false;}else{if(!A(_Kw,[_Ky[1]])){_Kx=_Ky[2];continue;}else{return true;}}}},_Kz=unCStr("x*x+x+10;"),_KA=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_KB=new T(function(){return _5(_KA);}),_KC=function(_KD,_){var _KE=jsHasCtx2D(_KD);if(!E(_KE)){return _9;}else{var _KF=jsGetCtx2D(_KD);return [1,[0,[0,_KF],[0,_KD]]];}},_KG=function(_KH,_){return _KC(E(_KH)[1],_);},_KI=function(_KJ,_KK){return A(_KJ,[function(_){var _KL=jsFind(toJSStr(E(_KK))),_KM=E(_KL);return _KM[0]==0?_9:_KG(_KM[1],_);}]);},_KN=new T(function(){return _KI(_1j,_Ka);}),_KO=[0,-10],_KP=[0,0],_KQ=[0,_KO,_KP],_KR=[0,10],_KS=[0,_KR,_KP],_KT=[1,_KS,_g],_KU=[1,_KQ,_KT],_KV=function(_KW,_){return _1c;},_KX=function(_KY){var _KZ=E(_KY);if(!_KZ[0]){return E(_KV);}else{var _L0=E(_KZ[1]);return function(_L1,_){var _L2=E(_L1)[1],_L3=jsMoveTo(_L2,E(_L0[1])[1],E(_L0[2])[1]);return (function(_L4,_){while(1){var _L5=E(_L4);if(!_L5[0]){return _1c;}else{var _L6=E(_L5[1]),_L7=jsLineTo(_L2,E(_L6[1])[1],E(_L6[2])[1]);_L4=_L5[2];continue;}}})(_KZ[2],_);};}},_L8=new T(function(){return _KX(_KU);}),_L9=[0,30],_La=[0,_KP,_L9],_Lb=[0,-30],_Lc=[0,_KP,_Lb],_Ld=[1,_Lc,_g],_Le=[1,_La,_Ld],_Lf=new T(function(){return _KX(_Le);}),_Lg=function(_Lh,_Li,_Lj){while(1){var _Lk=E(_Li);if(!_Lk[0]){return true;}else{var _Ll=E(_Lj);if(!_Ll[0]){return false;}else{if(!A(_dJ,[_Lh,_Lk[1],_Ll[1]])){return false;}else{_Li=_Lk[2];_Lj=_Ll[2];continue;}}}}},_Lm=unCStr("alert"),_Ln=function(_Lo){return _Lg(_dI,_Lm,_Lo);},_Lp=new T(function(){return [0,0/0];}),_Lq=new T(function(){return [0,-1/0];}),_Lr=new T(function(){return [0,1/0];}),_Ls=[0,0],_Lt=function(_Lu,_Lv){while(1){var _Lw=E(_Lu);if(!_Lw[0]){_Lu=[1,I_fromInt(_Lw[1])];continue;}else{var _Lx=E(_Lv);if(!_Lx[0]){_Lu=_Lw;_Lv=[1,I_fromInt(_Lx[1])];continue;}else{return I_fromRat(_Lw[1],_Lx[1]);}}}},_Ly=function(_Lz,_LA){var _LB=E(_Lz);if(!_LB[0]){var _LC=_LB[1],_LD=E(_LA);return _LD[0]==0?_LC==_LD[1]:I_compareInt(_LD[1],_LC)==0?true:false;}else{var _LE=_LB[1],_LF=E(_LA);return _LF[0]==0?I_compareInt(_LE,_LF[1])==0?true:false:I_compare(_LE,_LF[1])==0?true:false;}},_LG=function(_LH,_LI){var _LJ=E(_LH);if(!_LJ[0]){var _LK=_LJ[1],_LL=E(_LI);return _LL[0]==0?_LK<_LL[1]:I_compareInt(_LL[1],_LK)>0;}else{var _LM=_LJ[1],_LN=E(_LI);return _LN[0]==0?I_compareInt(_LM,_LN[1])<0:I_compare(_LM,_LN[1])<0;}},_LO=function(_LP,_LQ){return !_Ly(_LQ,_Ls)?[0,_Lt(_LP,_LQ)]:!_Ly(_LP,_Ls)?!_LG(_LP,_Ls)?E(_Lr):E(_Lq):E(_Lp);},_LR=function(_LS){var _LT=E(_LS);return _LO(_LT[1],_LT[2]);},_LU=function(_LV){return [0,1/E(_LV)[1]];},_LW=function(_LX){var _LY=E(_LX),_LZ=_LY[1];return _LZ<0?[0, -_LZ]:E(_LY);},_M0=function(_M1){var _M2=E(_M1);return _M2[0]==0?_M2[1]:I_toNumber(_M2[1]);},_M3=function(_M4){return [0,_M0(_M4)];},_M5=[0,0],_M6=[0,1],_M7=[0,-1],_M8=function(_M9){var _Ma=E(_M9)[1];return _Ma!=0?_Ma<=0?E(_M7):E(_M6):E(_M5);},_Mb=function(_Mc,_Md){return [0,E(_Mc)[1]-E(_Md)[1]];},_Me=function(_Mf){return [0, -E(_Mf)[1]];},_Mg=function(_Mh,_Mi){return [0,E(_Mh)[1]+E(_Mi)[1]];},_Mj=function(_Mk,_Ml){return [0,E(_Mk)[1]*E(_Ml)[1]];},_Mm=[0,_Mg,_Mj,_Mb,_Me,_LW,_M8,_M3],_Mn=function(_Mo,_Mp){return [0,E(_Mo)[1]/E(_Mp)[1]];},_Mq=[0,_Mm,_Mn,_LU,_LR],_Mr=function(_Ms,_Mt){return E(_Ms)[1]!=E(_Mt)[1]?true:false;},_Mu=function(_Mv,_Mw){return E(_Mv)[1]==E(_Mw)[1];},_Mx=[0,_Mu,_Mr],_My=function(_Mz,_MA){return E(_Mz)[1]<E(_MA)[1];},_MB=function(_MC,_MD){return E(_MC)[1]<=E(_MD)[1];},_ME=function(_MF,_MG){return E(_MF)[1]>E(_MG)[1];},_MH=function(_MI,_MJ){return E(_MI)[1]>=E(_MJ)[1];},_MK=function(_ML,_MM){var _MN=E(_ML)[1],_MO=E(_MM)[1];return _MN>=_MO?_MN!=_MO?2:1:0;},_MP=function(_MQ,_MR){var _MS=E(_MQ),_MT=E(_MR);return _MS[1]>_MT[1]?E(_MS):E(_MT);},_MU=function(_MV,_MW){var _MX=E(_MV),_MY=E(_MW);return _MX[1]>_MY[1]?E(_MY):E(_MX);},_MZ=[0,_Mx,_MK,_My,_MH,_ME,_MB,_MP,_MU],_N0=[0,1],_N1=function(_N2){return E(E(_N2)[1]);},_N3=function(_N4){return E(E(_N4)[2]);},_N5=function(_N6){return E(E(_N6)[6]);},_N7=[0,2],_N8=function(_N9,_Na){var _Nb=E(_Na);return [1,_Nb,new T(function(){var _Nc=_N1(_N9);return _N8(_N9,A(_Nc[1],[_Nb,new T(function(){return A(_Nc[7],[_N0]);})]));})];},_Nd=function(_Ne,_Nf){var _Ng=E(_Nf);if(!_Ng[0]){return [0];}else{var _Nh=_Ng[1];return !A(_Ne,[_Nh])?[0]:[1,_Nh,new T(function(){return _Nd(_Ne,_Ng[2]);})];}},_Ni=function(_Nj,_Nk,_Nl,_Nm){var _Nn=new T(function(){return _N5(_Nj);});return _Nd(function(_No){return A(_Nn,[_No,new T(function(){var _Np=_N1(_Nk),_Nq=_Np[7];return A(_Np[1],[_Nm,new T(function(){return A(_N3,[_Nk,new T(function(){return A(_Nq,[_N0]);}),new T(function(){return A(_Nq,[_N7]);})]);})]);})]);},_N8(_Nk,_Nl));},_Nr=new T(function(){return _Ni(_MZ,_Mq,_KO,_KR);}),_Ns=function(_Nt){return [1,_Nt,new T(function(){var _Nu=E(_Nt);return _Nu[0]==0?[0]:_Ns(_Nu[2]);})];},_Nv=function(_Nw,_Nx){var _Ny=E(_Nw);if(!_Ny[0]){return [0];}else{var _Nz=E(_Nx);return _Nz[0]==0?[0]:[1,[0,_Ny[1],_Nz[1]],new T(function(){return _Nv(_Ny[2],_Nz[2]);})];}},_NA=function(_NB){var _NC=new T(function(){return !_Kv(_Ln,_Ns(_NB))?E(_NB):E(_Kz);}),_ND=function(_NE,_){var _NF=E(_NE);if(!_NF[0]){return _g;}else{var _NG=A(_KB,[E(toJSStr(_JT(120,new T(function(){return A(_K1,[_JQ,_9j,E(_NF[1])[1],_g]);}),_NC))),_]),_NH=_ND(_NF[2],_);return [1,[0,_NG],_NH];}};return function(_as,_vx){return _4P(_Ks,function(_NI,_Cx,_){return (function(_NJ,_){return [0,[0,function(_NK,_){var _NL=A(_KN,[_]),_NM=E(_NL);if(!_NM[0]){var _NN=_3x(_Ku,_);return _NK;}else{var _NO=_ND(_Nr,_),_NP=E(_NM[1]),_NQ=jsResetCanvas(E(_NP[2])[1]),_NR=E(_NP[1]),_NS=_NR[1],_NT=jsPushState(_NS),_NU=jsScale(_NS,3,1),_NV=jsPushState(_NS),_NW=jsTranslate(_NS,50,130),_NX=jsPushState(_NS),_NY=jsRotate(_NS,3.141592653589793),_NZ=jsBeginPath(_NS),_O0=A(_L8,[_NR,_]),_O1=A(_Lf,[_NR,_]),_O2=A(_KX,[_Nv(_Nr,_NO),_NR,_]),_O3=jsStroke(_NS),_O4=jsPopState(_NS),_O5=jsPopState(_NS),_O6=jsPopState(_NS);return _NK;}},_Cq],_NJ];})(_Cx,_);},_as,_vx);};},_O7=[1,_Kz],_O8=new T(function(){return _bo(_8D,_8E,_9f,_sK,_qz);}),_O9=new T(function(){return A(_O8,[_9,_sM,_O7]);}),_Oa=new T(function(){return _tY(_O9,_5D);}),_Ob=function(_Oc,_){var _Od=A(_Oa,[_Oc,_]),_Oe=E(_Od),_Of=E(_Oe[1]);return [0,[0,function(_Og,_){var _Oh=A(_Of[1],[_Og,_]),_Oi=_G(_Og,_);return _Og;},new T(function(){var _Oj=E(_Of[2]);return _Oj[0]==0?E(_O7):E(_Oj);})],_Oe[2]];},_Ok=function(_Ol,_){var _Om=_4P(_Ob,_NA,_Ol,_),_On=E(_Om),_Oo=E(_On[1]),_Op=new T(function(){return _zk(_11,_Oo[1]);});return [0,[0,function(_Oq,_){var _Or=A(_JM,[_Oq,_]),_Os=A(_Op,[_Oq,_]);return _Oq;},_Oo[2]],_On[2]];},_Ot=unCStr("work?"),_Ou=function(_Ov,_Ow,_Ox){var _Oy=E(_Ow);return A(_Oy[1],[new T(function(){var _Oz=E(_Ov);return E(_Ox);}),function(_OA){return A(_Oy[3],[[1,_48,new T(function(){var _OB=E(_OA);return _2i(_4f(0,E(_OB[2])[1],_g),_OB[1]);})]]);}]);},_OC=function(_OD){return E(E(_OD)[5]);},_OE=unCStr("for"),_OF=unCStr("label"),_OG=function(_OH,_OI){var _OJ=new T(function(){return _8F(_OI);}),_OK=new T(function(){return _bg(_OJ);}),_OL=new T(function(){return _Ou([0,coercionToken],_42(_OH),function(_OM){return _bd(_OH,_OM);});}),_ON=new T(function(){return _40(_OH);}),_OO=new T(function(){return _3A(_OH);}),_OP=new T(function(){return _Eu(_OI);}),_OQ=new T(function(){return _3A(_OH);}),_OR=new T(function(){return _OC(_OI);});return function(_OS,_OT){var _OU=new T(function(){return A(_OR,[_OF,_OS]);});return function(_OV){return A(_OQ,[new T(function(){return A(_OL,[_OV]);}),function(_OW){var _OX=new T(function(){return A(_OP,[_OU,[1,[0,_OE,new T(function(){return E(E(_OW)[1]);})],_g]]);});return A(_OO,[new T(function(){return A(_OT,[new T(function(){return E(E(_OW)[2]);})]);}),function(_OY){var _OZ=E(_OY),_P0=E(_OZ[1]);return A(_ON,[[0,[0,new T(function(){return A(_OK,[_OX,_P0[1]]);}),_P0[2]],_OZ[2]]]);}]);}]);};};},_P1=new T(function(){return _OG(_3z,_8D);}),_P2=new T(function(){return _EH(_8D,_8E,_Fn,_Fo);}),_P3=function(_P4,_P5){return A(_P1,[function(_Cx,_){return _z(_P4,_Cx,_);},new T(function(){return _tY(new T(function(){return A(_P2,[_P4,_P5]);}),_zx);})]);},_P6=function(_Lo){return _P3(_Ot,_Lo);},_P7=unCStr("study?"),_P8=function(_Lo){return _P3(_P7,_Lo);},_P9=[1,_P8,_g],_Pa=[1,_P6,_P9],_Pb=new T(function(){return A(_Gk,[_Pa]);}),_Pc=unCStr("Do you "),_Pd=new T(function(){return _wr(_z,_Pc);}),_Pe=function(_Pf,_){var _Pg=A(_Pb,[_Pf,_]),_Ph=E(_Pg),_Pi=E(_Ph[1]);return [0,[0,function(_Pj,_){var _Pk=A(_Pd,[_Pj,_]),_Pl=A(_Pi[1],[_Pj,_]),_Pm=_G(_Pj,_);return _Pj;},_Pi[2]],_Ph[2]];},_Pn=unCStr("do you enjoy your work? "),_Po=new T(function(){return _wr(_z,_Pn);}),_Pp=function(_Pq,_Pr,_){return [0,[0,_Z,[1,_Pq]],_Pr];},_Ps=function(_Pt,_Pu,_Pv,_){return _4P(_Pt,function(_Pw){return E(_Pu);},_Pv,_);},_Px=function(_Py,_Pz,_1z,_){return _Ps(_Py,_Pz,_1z,_);},_PA=function(_PB){return err(_PB);},_PC=[0,_4P,_Px,_Pp,_PA],_PD=function(_PE,_PF,_PG,_PH,_PI,_PJ){var _PK=new T(function(){return _bg(_PE);});return A(_PF,[new T(function(){return A(_PH,[_PJ]);}),function(_PL){var _PM=E(_PL),_PN=E(_PM[1]);return A(_PF,[new T(function(){return A(_PI,[_PM[2]]);}),function(_PO){var _PP=E(_PO),_PQ=E(_PP[1]);return A(_PG,[[0,[0,new T(function(){return A(_PK,[_PN[1],_PQ[1]]);}),new T(function(){var _PR=E(_PN[2]);return _PR[0]==0?E(_PQ[2]):E(_PR);})],_PP[2]]]);}]);}]);},_PS=function(_PT,_PU,_PV,_PW,_PX,_PY){var _PZ=new T(function(){return _Eu(_PV);});return A(_PT,[new T(function(){return A(_PW,[_PY]);}),function(_Q0){var _Q1=E(_Q0),_Q2=E(_Q1[1]);return A(_PU,[[0,[0,new T(function(){return A(_PZ,[_Q2[1],_PX]);}),_Q2[2]],_Q1[2]]]);}]);},_Q3=function(_Q4){return E(E(_Q4)[12]);},_Q5=function(_Q6,_Q7,_Q8,_Q9,_Qa,_Qb,_Qc){var _Qd=new T(function(){return A(_Q3,[_Q6,new T(function(){var _Qe=A(_Q8,[_Qa]),_Qf=E(_IL),_Qg=hs_eqWord64(_Qe[1],_Qf[1]);if(!E(_Qg)){return A(_bm,[_Q9,_Qa]);}else{var _Qh=hs_eqWord64(_Qe[2],_Qf[2]);return E(_Qh)==0?A(_bm,[_Q9,_Qa]):E(_Qa);}}),_Qb,_Qc]);}),_Qi=new T(function(){return _40(_Q7);});return function(_Qj){return A(_Qi,[[0,[0,_Qd,[1,_Qa]],_Qj]]);};},_Qk=[0,_7q,_EF],_Ql=[1,_Qk,_g],_Qm=[0,_7q,_EF],_Qn=[1,_Qm,_g],_Qo=function(_Qp,_Qq,_Qr,_Qs){var _Qt=new T(function(){return _Ia(_Qs,_Qr,_9f,_qz);}),_Qu=new T(function(){return A(_40,[_Qp,_0]);}),_Qv=new T(function(){return A(_40,[_Qp,_gy]);}),_Qw=new T(function(){return _8F(_Qs);}),_Qx=new T(function(){return _8H(_Qr);}),_Qy=new T(function(){return _8Y(_Qs);}),_Qz=new T(function(){return _3A(_Qp);});return function(_QA,_QB,_QC){return A(_Qz,[new T(function(){var _QD=new T(function(){return !E(_QA)?E(_Qn):[0];}),_QE=new T(function(){return _Q5(_Qs,_Qx,_9f,_sK,_QC,new T(function(){return A(_Qy,[_QC]);}),_0);}),_QF=new T(function(){return !E(_QA)?[0]:E(_Ql);}),_QG=new T(function(){return _Q5(_Qs,_Qx,_9f,_sK,_QB,new T(function(){return A(_Qy,[_QB]);}),_0);});return A(_Qt,[function(_QH){var _QI=E(_Qx);return _PD(_Qw,_QI[1],_QI[3],function(_QJ){var _QK=E(_Qx);return _PS(_QK[1],_QK[3],_Qs,_QG,_QF,_QJ);},function(_QL){var _QM=E(_Qx);return _PS(_QM[1],_QM[3],_Qs,_QE,_QD,_QL);},_QH);}]);}),function(_QN){return !_tn(_QN,_QB)?E(_Qu):E(_Qv);}]);};},_QO=new T(function(){return _Qo(_PC,_wY,_8E,_8D);}),_QP=unCStr("yes"),_QQ=unCStr("no"),_QR=new T(function(){return A(_QO,[_gy,_QP,_QQ]);}),_QS=unCStr("ok"),_QT=[1,_QS],_QU=new T(function(){return A(_zz,[_QT]);}),_QV=new T(function(){return _tY(_QU,_zx);}),_QW=function(_QX,_){var _QY=A(_QR,[_QX,_]),_QZ=E(_QY),_R0=E(_QZ[1]),_R1=A(_QV,[_QZ[2],_]),_R2=E(_R1);return [0,[0,function(_R3,_){var _R4=A(_Po,[_R3,_]),_R5=A(_R0[1],[_R3,_]),_R6=A(E(_R2[1])[1],[_R3,_]),_R7=_G(_R3,_);return _R3;},new T(function(){var _R8=E(_R0[2]);return _R8[0]==0?[0]:[1,[0,_R8[1]]];})],_R2[2]];},_R9=unCStr("do you study in "),_Ra=new T(function(){return _wr(_z,_R9);}),_Rb=unCStr("University"),_Rc=function(_Lo){return _P3(_Rb,_Lo);},_Rd=unCStr("High School"),_Re=function(_Lo){return _P3(_Rd,_Lo);},_Rf=[1,_Re,_g],_Rg=[1,_Rc,_Rf],_Rh=new T(function(){return A(_Gk,[_Rg]);}),_Ri=function(_Rj,_){var _Rk=A(_Rh,[_Rj,_]),_Rl=E(_Rk),_Rm=E(_Rl[1]);return [0,[0,function(_Rn,_){var _Ro=A(_Ra,[_Rn,_]),_Rp=A(_Rm[1],[_Rn,_]);return _Rn;},new T(function(){var _Rq=E(_Rm[2]);return _Rq[0]==0?[0]:[1,[1,_Rq[1]]];})],_Rl[2]];},_Rr=new T(function(){return _cY("main.hs:(293,11)-(300,64)|case");}),_Rs=unCStr(" that you enjoy your work"),_Rt=unCStr("False"),_Ru=new T(function(){return _2i(_Rt,_Rs);}),_Rv=unCStr("True"),_Rw=new T(function(){return _2i(_Rv,_Rs);}),_Rx=[0,32],_Ry=function(_Rz,_RA){var _RB=new T(function(){return _we(_z,new T(function(){return unAppCStr("You are ",new T(function(){return _2i(_Rz,[1,_Rx,_RA]);}));}));});return function(_as,_vx){return _4P(_Pe,function(_RC){var _RD=new T(function(){return !_tn(_RC,_P7)?!_tn(_RC,_Ot)?E(_Rr):E(_QW):E(_Ri);});return function(_as,_vx){return _4P(_RD,function(_RE){return function(_RF,_){var _RG=A(new T(function(){var _RH=E(_RE);if(!_RH[0]){var _RI=new T(function(){return _we(_z,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_RH[1])?E(_Ru):E(_Rw);}));}));});return function(_RJ,_){return [0,[0,function(_RK,_){var _RL=A(_RI,[_RK,_]);return _RK;},_9],_RJ];};}else{var _RM=new T(function(){return _we(_z,new T(function(){return unAppCStr("You study at the ",_RH[1]);}));});return function(_RN,_){return [0,[0,function(_RO,_){var _RP=A(_RM,[_RO,_]);return _RO;},_9],_RN];};}}),[_RF,_]),_RQ=E(_RG),_RR=E(_RQ[1]);return [0,[0,function(_RS,_){var _RT=A(_RB,[_RS,_]),_RU=A(_RR[1],[_RS,_]);return _RS;},_RR[2]],_RQ[2]];};},_as,_vx);};},_as,_vx);};},_RV=function(_RW){var _RX=E(_RW);return _Ry(_RX[1],_RX[2]);},_RY=unCStr("Who are you? "),_RZ=new T(function(){return _we(_z,_RY);}),_S0=unCStr("name"),_S1=unCStr("placeholder"),_S2=[0,_S1,_S0],_S3=[1,_S2,_g],_S4=unCStr("surname"),_S5=[0,_S1,_S4],_S6=[1,_S5,_g],_S7=[1,_QS],_S8=new T(function(){return A(_zz,[_S7]);}),_S9=new T(function(){return _tY(_S8,_zx);}),_Sa=new T(function(){return A(_O8,[_9,_sM,_9]);}),_Sb=new T(function(){return A(_O8,[_9,_sM,_9]);}),_Sc=function(_Sd,_){var _Se=A(_Sb,[_Sd,_]),_Sf=E(_Se),_Sg=E(_Sf[1]),_Sh=A(_Sa,[_Sf[2],_]),_Si=E(_Sh),_Sj=E(_Si[1]),_Sk=A(_S9,[_Si[2],_]),_Sl=E(_Sk),_Sm=new T(function(){return _1s(_Sj[1],_S6);}),_Sn=new T(function(){return _1s(_Sg[1],_S3);});return [0,[0,function(_So,_){var _Sp=A(_RZ,[_So,_]),_Sq=A(_Sn,[_So,_]),_Sr=_G(_So,_),_Ss=A(_Sm,[_So,_]),_St=_G(_So,_),_Su=A(E(_Sl[1])[1],[_So,_]),_Sv=_G(_So,_);return _So;},new T(function(){var _Sw=E(_Sg[2]);if(!_Sw[0]){return [0];}else{var _Sx=E(_Sj[2]);return _Sx[0]==0?[0]:[1,[0,_Sw[1],_Sx[1]]];}})],_Sl[2]];},_Sy=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_Sz=unCStr("This formulary is the same than the one "),_SA=[0,97],_SB=[1,_SA,_g],_SC=function(_SD,_SE){var _SF=new T(function(){return A(_SD,[_SE]);});return function(_SG,_){var _SH=jsCreateElem(toJSStr(_SB)),_SI=jsAppendChild(_SH,E(_SG)[1]),_SJ=[0,_SH],_SK=A(_SF,[_SJ,_]);return _SJ;};},_SL=unCStr("run in the server by MFlow"),_SM=new T(function(){return _SC(_z,_SL);}),_SN=unCStr("href"),_SO=function(_SP,_){var _SQ=_z(_Sz,_SP,_),_SR=A(_SM,[_SP,_]),_SS=A(_1d,[_1j,_SR,_SN,_Sy,_]);return _SP;},_ST=new T(function(){return _we(_11,_SO);}),_SU=unCStr("Fields of a form appear in sequence. Some of the fields trigger events instantly. Some others use a button to trigger them. It also contains option buttons, radio buttons etc"),_SV=new T(function(){return _we(_z,_SU);}),_SW=function(_SX,_){var _SY=_4P(_Sc,_RV,_SX,_),_SZ=E(_SY),_T0=E(_SZ[1]);return [0,[0,function(_T1,_){var _T2=A(_SV,[_T1,_]),_T3=A(_ST,[_T1,_]),_T4=A(_T0[1],[_T1,_]);return _T1;},_T0[2]],_SZ[2]];},_T5=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_T6=new T(function(){return _we(_z,_T5);}),_T7=[1,_wL],_T8=unCStr("GalleryIndex"),_T9=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Ap,_Aq,_T8],_Ta=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_T9,_g],_Tb=function(_Tc){return E(_Ta);},_Td=new T(function(){return _CU(_1F,_47,_1D,_Tb);}),_Te=function(_Tf,_){var _Tg=A(_Td,[_Tf,_]);return [0,[0,_zt,new T(function(){var _Th=E(E(_Tg)[1]);return _Th[0]==0?E(_T7):E(_Th);})],new T(function(){return E(E(_Tg)[2]);})];},_Ti=unCStr("100%"),_Tj=[0,62],_Tk=[1,_Tj,_g],_Tl=[1,_Tk],_Tm=new T(function(){return A(_zz,[_Tl]);}),_Tn=new T(function(){return _tY(_Tm,_zx);}),_To=function(_Tp){return E(_Tn);},_Tq=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_Tr=[1,_Tq,_g],_Ts=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_Tt=[1,_Ts,_Tr],_Tu=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_Tv=[1,_Tu,_Tt],_Tw=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_Tx=[1,_Tw,_Tv],_Ty=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_Tz=[1,_Ty,_Tx],_TA=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_TB=[1,_TA,_Tz],_TC=unCStr("height"),_TD=unCStr("img"),_TE=function(_TF,_){var _TG=jsCreateElem(toJSStr(E(_TD))),_TH=jsAppendChild(_TG,E(_TF)[1]);return [0,_TG];},_TI=function(_TJ,_TK){while(1){var _TL=E(_TJ);if(!_TL[0]){return E(_TK);}else{_TJ=_TL[2];var _TM=_TK+1|0;_TK=_TM;continue;}}},_TN=new T(function(){return [0,_TI(_TB,0)-1|0];}),_TO=[0,_Z,_wA],_TP=unCStr("src"),_TQ=unCStr("width"),_TR=function(_TS){return function(_as,_vx){return _4P(function(_Cx,_){return _4P(_Co,function(_TT){return function(_TU,_){return [0,_TO,new T(function(){var _TV=E(_TT);return [0,_TV[1],_TV[2],_TV[3],_TV[4],_TV[5],new T(function(){return _Cb(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_T9,_g,new T(function(){var _TW=E(_TS)[1];return _TW!=E(_TN)[1]?[0,_TW+1|0]:E(_wL);}),_TV[6]);})];})];};},_Cx,_);},function(_TX,_Cx,_){return (function(_Cx,_){return _4P(function(_TY,_){return [0,[0,function(_TZ,_){var _U0=_TE(_TZ,_),_U1=A(_1d,[_1j,_U0,_TP,new T(function(){var _U2=E(_TS)[1];return _U2>=0?_qE(_TB,_U2):E(_qB);}),_]),_U3=A(_1d,[_1j,_U0,_TQ,_Ti,_]),_U4=A(_1d,[_1j,_U0,_TC,_Ti,_]),_U5=_G(_TZ,_);return _TZ;},_Cq],_TY];},_To,_Cx,_);})(_Cx,_);},_as,_vx);};},_U6=function(_Cx,_){return _4P(_Te,_TR,_Cx,_);},_U7=function(_U8,_U9,_){return _Ua(_U9,_);},_Ub=function(_Cx,_){return _zS(_U6,_U7,_Cx,_);},_Uc=[0,20000],_Ud=new T(function(){return _4k(_1F,_47,_1D,_1A);}),_Ue=function(_Uf,_Ug,_Uh,_){var _Ui=A(_Ud,[_Uh,_]),_Uj=new T(function(){return E(E(_Ui)[1]);}),_Uk=new T(function(){return [0,_Ul];}),_Ul=function(_){var _Um=jsFind(toJSStr(E(_Uj))),_Un=E(_Um);if(!_Un[0]){return _1c;}else{var _Uo=E(_Un[1]),_Up=E(_Uo),_Uq=jsClearChildren(_Uo[1]),_Ur=E(_k)[1],_Us=takeMVar(_Ur),_Ut=A(_Ug,[_Us,_]),_Uu=E(_Ut),_Uv=E(_Uu[1]),_Uw=_Uv[1],_Ux=_Uv[2],_=putMVar(_Ur,new T(function(){var _Uy=E(_Uu[2]);return [0,_Uy[1],_Uy[2],_Uy[3],_Uy[4],_0,_Uy[6]];}));if(!E(E(_Us)[5])){var _Uz=A(_Uw,[_Up,_]),_UA=E(_Ux);if(!_UA[0]){var _UB=jsSetTimeout(E(_Uf)[1],E(_Uk)[1]);return _1c;}else{var _UC=E(_UA[1]);return _1c;}}else{var _UD=A(_7,[E(_Up[1]),_]),_UE=A(_Uw,[[0,_UD],_]),_UF=E(_Ux);if(!_UF[0]){var _UG=jsSetTimeout(E(_Uf)[1],E(_Uk)[1]);return _1c;}else{var _UH=E(_UF[1]);return _1c;}}}},_UI=jsSetTimeout(E(_Uf)[1],E(_Uk)[1]);return _zF(_Uj,_Ug,new T(function(){return E(E(_Ui)[2]);}),_);},_Ua=function(_UJ,_){var _UK=_Ue(_Uc,_Ub,_UJ,_),_UL=E(_UK),_UM=E(_UL[1]);return [0,[0,function(_UN,_){var _UO=A(_T6,[_UN,_]),_UP=A(_UM[1],[_UN,_]);return _UN;},_UM[2]],_UL[2]];},_UQ=function(_UR){var _US=new T(function(){return _wr(_z,new T(function(){return unAppCStr(" returns ",_UR);}));});return function(_UT,_){return [0,[0,_US,_Cq],_UT];};},_UU=unCStr("This link say Hey!"),_UV=function(_Cx,_){return _z(_UU,_Cx,_);},_UW=unCStr("Hey!"),_UX=function(_){var _=0,_UY=newMVar(),_=putMVar(_UY,_9);return [0,_UY];},_UZ=new T(function(){return _2(_UX);}),_V0=new T(function(){return _4k(_1F,_47,_1D,_1A);}),_V1=new T(function(){return A(_9f,[_6F]);}),_V2=function(_V3,_V4,_){var _=putMVar(E(_V3)[1],_V4);return _1c;},_V5=function(_V6,_V7,_){return _t5(function(_){return _V2(_UZ,_V6,_);},_V7,_);},_V8=function(_){var _V9=E(_UZ)[1],_Va=takeMVar(_V9),_=putMVar(_V9,_Va);return _Va;},_Vb=function(_Vc,_Vd,_Ve,_Vf){var _Vg=new T(function(){return _SC(_11,_Vf);}),_Vh=new T(function(){return unAppCStr("#/",new T(function(){var _Vi=A(_Vd,[_Ve]),_Vj=E(_V1),_Vk=hs_eqWord64(_Vi[1],_Vj[1]);if(!E(_Vk)){return A(_bm,[_Vc,_Ve]);}else{var _Vl=hs_eqWord64(_Vi[2],_Vj[2]);return E(_Vl)==0?A(_bm,[_Vc,_Ve]):E(_Ve);}}));});return function(_Vm,_){var _Vn=A(_V0,[_Vm,_]),_Vo=0,_Vp=function(_,_Vq,_Vr){var _Vs=new T(function(){return E(E(_Vn)[1]);}),_Vt=function(_Vu,_){var _Vv=A(_Vg,[_Vu,_]),_Vw=A(_1d,[_1j,_Vv,_SN,_Vh,_]),_Vx=E(_Vv),_Vy=jsSetCB(_Vx[1],E(_tE)[1],E([0,function(_Vz,_VA,_){return (function(_){var _VB=0;if(!E(_VB)){return (function(_){var _VC=takeMVar(E(_UZ)[1]),_VD=jsCatch(function(_){return (function(_){return [1,_Vs];})();},function(_1z,_){return _V5(_VC,_1z,_);});return _V2(_UZ,_VD,_);})();}else{var _VE=takeMVar(E(_UZ)[1]),_VF=jsCatch(function(_){return [1,_Vs];},function(_1z,_){return _V5(_VE,_1z,_);});return _V2(_UZ,_VF,_);}})(_);}])[1]);return _Vx;},_VG=E(_Vq);return _VG[0]==0?[0,[0,_Vt,_9],_Vr]:!_tn(_VG[1],_Vs)?[0,[0,_Vt,_9],_Vr]:[0,[0,_Vt,[1,_Ve]],_Vr];};if(!E(_Vo)){var _VH=_V8();return _Vp(_,_VH,new T(function(){return E(E(_Vn)[2]);}));}else{var _VI=E(_UZ)[1],_VJ=takeMVar(_VI),_=putMVar(_VI,_VJ);return _Vp(_,_VJ,new T(function(){return E(E(_Vn)[2]);}));}};},_VK=new T(function(){return _Vb(_Fo,_Fn,_UW,_UV);}),_VL=new T(function(){return _tY(_VK,_zx);}),_VM=function(_VN,_){var _VO=A(_VL,[_VN,_]),_VP=E(_VO),_VQ=E(_VP[1]);return [0,[0,function(_VR,_){var _VS=_G(_VR,_),_VT=A(_VQ[1],[_VR,_]);return _VR;},_VQ[2]],_VP[2]];},_VU=function(_VV,_VW,_VX){return A(_VV,[[1,_35,new T(function(){return A(_VW,[_VX]);})]]);},_VY=unCStr("Key "),_VZ=unCStr("Mouse "),_W0=unCStr("Click "),_W1=unCStr("NoData"),_W2=function(_W3){return _2i(_W1,_W3);},_W4=unCStr(": empty list"),_W5=unCStr("Prelude."),_W6=function(_W7){return err(_2i(_W5,new T(function(){return _2i(_W7,_W4);})));},_W8=unCStr("foldr1"),_W9=new T(function(){return _W6(_W8);}),_Wa=function(_Wb,_Wc){var _Wd=E(_Wc);if(!_Wd[0]){return E(_W9);}else{var _We=_Wd[1],_Wf=E(_Wd[2]);return _Wf[0]==0?E(_We):A(_Wb,[_We,new T(function(){return _Wa(_Wb,_Wf);})]);}},_Wg=[0,32],_Wh=function(_Wi,_Wj){var _Wk=E(_Wj);switch(_Wk[0]){case 0:return E(_W2);case 1:var _Wl=function(_Wm){return _4f(11,E(_Wk[1])[1],[1,_Wg,new T(function(){var _Wn=E(_Wk[2]);return [1,_4e,new T(function(){return A(_Wa,[_VU,[1,function(_Wo){return _4f(0,E(_Wn[1])[1],_Wo);},[1,function(_Wp){return _4f(0,E(_Wn[2])[1],_Wp);},_g]],[1,_4d,_Wm]]);})];})]);};return E(_Wi)[1]<11?function(_Wq){return _2i(_W0,new T(function(){return _Wl(_Wq);}));}:function(_Wr){return [1,_4e,new T(function(){return _2i(_W0,new T(function(){return _Wl([1,_4d,_Wr]);}));})];};case 2:var _Ws=function(_Wt){return _2i(_VZ,new T(function(){var _Wu=E(_Wk[1]);return [1,_4e,new T(function(){return A(_Wa,[_VU,[1,function(_Wv){return _4f(0,E(_Wu[1])[1],_Wv);},[1,function(_Ww){return _4f(0,E(_Wu[2])[1],_Ww);},_g]],[1,_4d,_Wt]]);})];}));};return E(_Wi)[1]<11?E(_Ws):function(_Wx){return [1,_4e,new T(function(){return _Ws([1,_4d,_Wx]);})];};default:var _Wy=_Wk[1];return E(_Wi)[1]<11?function(_Wz){return _2i(_VY,new T(function(){return _4f(11,E(_Wy)[1],_Wz);}));}:function(_WA){return [1,_4e,new T(function(){return _2i(_VY,new T(function(){return _4f(11,E(_Wy)[1],[1,_4d,_WA]);}));})];};}},_WB=function(_WC){var _WD=new T(function(){return _we(_z,new T(function(){var _WE=E(_WC);return _2i(_WE[1],[1,_Rx,new T(function(){return A(_Wh,[_9j,_WE[2],_g]);})]);}));});return function(_WF,_){return [0,[0,_WD,_Cq],_WF];};},_WG=function(_Cx,_){return _4P(_5x,_WB,_Cx,_);},_WH=function(_WI){return E(_WG);},_WJ=[14,coercionToken],_WK=[12,coercionToken],_WL=[9,coercionToken],_WM=[11,coercionToken],_WN=[5,coercionToken],_WO=[10,coercionToken],_WP=[6,coercionToken],_WQ=[7,coercionToken],_WR=unCStr("height:100px;background-color:lightgreen;position:relative"),_WS=unCStr("h1"),_WT=function(_WU,_WV){var _WW=new T(function(){return A(_WU,[_WV]);});return function(_WX,_){var _WY=jsCreateElem(toJSStr(E(_WS))),_WZ=jsAppendChild(_WY,E(_WX)[1]),_X0=[0,_WY],_X1=A(_WW,[_X0,_]);return _X0;};},_X2=unCStr("Mouse events here"),_X3=new T(function(){return _WT(_z,_X2);}),_X4=new T(function(){return _N(_11,_X3);}),_X5=function(_X6,_){var _X7=A(_X4,[_X6,_]),_X8=A(_1d,[_1j,_X7,_DS,_WR,_]);return _X7;},_X9=[0,_X5,_Cq],_Xa=function(_Xb,_){return [0,_X9,_Xb];},_Xc=new T(function(){return _tY(_Xa,_WQ);}),_Xd=new T(function(){return _tY(_Xc,_WP);}),_Xe=new T(function(){return _tY(_Xd,_WO);}),_Xf=new T(function(){return _tY(_Xe,_WN);}),_Xg=new T(function(){return _tY(_Xf,_WM);}),_Xh=new T(function(){return _tY(_Xg,_zx);}),_Xi=new T(function(){return _tY(_Xh,_WL);}),_Xj=new T(function(){return _tY(_Xi,_WK);}),_Xk=new T(function(){return _tY(_Xj,_WJ);}),_Xl=new T(function(){return _tY(_Xk,_5D);}),_Xm=unCStr("http://todomvc.com"),_Xn=unCStr("Work in progress for a todo application to be added to "),_Xo=unCStr("todomvc.com"),_Xp=new T(function(){return _SC(_z,_Xo);}),_Xq=function(_Xr,_){var _Xs=_z(_Xn,_Xr,_),_Xt=A(_Xp,[_Xr,_]),_Xu=A(_1d,[_1j,_Xt,_SN,_Xm,_]);return _Xr;},_Xv=new T(function(){return _we(_11,_Xq);}),_Xw=unCStr("Tasks"),_Xx=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Ap,_Aq,_Xw],_Xy=2,_Xz=function(_XA,_XB,_XC,_XD,_){var _XE=A(_XC,[_XD,_]),_XF=E(_XE),_XG=E(_XF[1]),_XH=_XG[1];return [0,[0,function(_XI,_){var _XJ=jsFind(toJSStr(E(_XA))),_XK=E(_XJ);if(!_XK[0]){return _XI;}else{var _XL=_XK[1];switch(E(_XB)){case 0:var _XM=A(_XH,[_XL,_]);return _XI;case 1:var _XN=E(_XL),_XO=_XN[1],_XP=jsGetChildren(_XO),_XQ=E(_XP);if(!_XQ[0]){var _XR=A(_XH,[_XN,_]);return _XI;}else{var _XS=jsCreateElem(toJSStr(E(_4x))),_XT=jsAddChildBefore(_XS,_XO,E(_XQ[1])[1]),_XU=A(_XH,[[0,_XS],_]);return _XI;}break;default:var _XV=E(_XL),_XW=jsClearChildren(_XV[1]),_XX=A(_XH,[_XV,_]);return _XI;}}},_XG[2]],_XF[2]];},_XY=[0,_Z,_wA],_XZ=function(_Y0,_){return [0,_XY,_Y0];},_Y1=unCStr("Pattern match failure in do expression at main.hs:349:7-25"),_Y2=new T(function(){return _PA(_Y1);}),_Y3=function(_Y4,_Y5,_Y6,_Y7){return A(_Y4,[new T(function(){return function(_){var _Y8=jsSet(E(_Y5)[1],toJSStr(E(_Y6)),toJSStr(E(_Y7)));return _1c;};})]);},_Y9=unCStr("text"),_Ya=unCStr("value"),_Yb=new T(function(){return _6G(_98,_9d);}),_Yc=new T(function(){return A(_Yb,[_6F]);}),_Yd=new T(function(){return A(_Yb,[_6F]);}),_Ye=unCStr("Prelude.read: ambiguous parse"),_Yf=unCStr("Prelude.read: no parse"),_Yg=function(_Yh){return [1,function(_Yi){return A(_mT,[_Yi,function(_Yj){return E([3,_Yh,_eb]);}]);}];},_Yk=function(_Yl){while(1){var _Ym=(function(_Yn){var _Yo=E(_Yn);if(!_Yo[0]){return [0];}else{var _Yp=_Yo[2],_Yq=E(_Yo[1]);if(!E(_Yq[2])[0]){return [1,_Yq[1],new T(function(){return _Yk(_Yp);})];}else{_Yl=_Yp;return null;}}})(_Yl);if(_Ym!=null){return _Ym;}}},_Yr=function(_Ys,_Yt){var _Yu=_Yk(_d1(A(E(_Ys)[3],[_pa,_Yg]),_Yt));return _Yu[0]==0?err(_Yf):E(_Yu[2])[0]==0?E(_Yu[1]):err(_Ye);},_Yv=function(_Yw,_Yx,_Yy,_Yz){var _YA=new T(function(){return _bm(_Yx);}),_YB=new T(function(){return _bo(_8D,_8E,_Yy,_Yx,_Yw);});return [0,function(_YC){return A(_YB,[[1,_Yz],_Y9,_YC]);},function(_YD,_){var _YE=E(_Yz),_YF=jsFind(toJSStr(_YE)),_YG=E(_YF);return _YG[0]==0?_4F(_YE):A(_Y3,[_1j,_YG[1],_Ya,new T(function(){var _YH=A(_Yy,[_YD]),_YI=E(_Yc),_YJ=hs_eqWord64(_YH[1],_YI[1]);if(!E(_YJ)){return A(_YA,[_YD]);}else{var _YK=hs_eqWord64(_YH[2],_YI[2]);return E(_YK)==0?A(_YA,[_YD]):E(_YD);}}),_]);},function(_){var _YL=E(_Yz),_YM=jsFind(toJSStr(_YL)),_YN=E(_YM);if(!_YN[0]){return _4F(_YL);}else{var _YO=_Ew(E(_YN[1])[1],_Ya,_);return new T(function(){var _YP=A(_Yb,[_YO]),_YQ=E(_Yd),_YR=hs_eqWord64(_YP[1],_YQ[1]);if(!E(_YR)){return _Yr(_Yw,_YO);}else{var _YS=hs_eqWord64(_YP[2],_YQ[2]);return E(_YS)==0?_Yr(_Yw,_YO):E(_YO);}});}}];},_YT=unCStr("todo"),_YU=new T(function(){var _YV=_Yv(_I7,_Fo,_Fn,_YT);return [0,_YV[1],_YV[2],_YV[3]];}),_YW=new T(function(){var _YX=A(E(_YU)[2],[_g]);return function(_YY,_){var _YZ=A(_YX,[_]);return [0,[0,_Z,[1,_YZ]],_YY];};}),_Z0=[1,_g],_Z1=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Xx,_g],_Z2=function(_Z3){return E(_Z1);},_Z4=new T(function(){return _CU(_1F,_47,_1D,_Z2);}),_Z5=function(_Z6,_){var _Z7=A(_Z4,[_Z6,_]);return [0,[0,_zt,new T(function(){var _Z8=E(E(_Z7)[1]);return _Z8[0]==0?E(_Z0):E(_Z8);})],new T(function(){return E(E(_Z7)[2]);})];},_Z9=[0,_Z,_wA],_Za=[0,_Z,_wA],_Zb=function(_Zc,_Zd,_){return [0,_Za,_Zd];},_Ze=[0,_Z,_wA],_Zf=function(_Zg,_){return [0,_Ze,_Zg];},_Zh=unCStr("list"),_Zi=unCStr("check"),_Zj=new T(function(){return A(_H3,[_0,_Zi]);}),_Zk=new T(function(){return _tY(_Zj,_zx);}),_Zl=function(_Zm,_){var _Zn=A(_Zk,[_Zm,_]),_Zo=E(_Zn),_Zp=E(_Zo[1]);return [0,[0,_Zp[1],new T(function(){var _Zq=E(_Zp[2]);return _Zq[0]==0?[0]:[1,E(_Zq[1])[1]];})],_Zo[2]];},_Zr=unCStr("text-decoration:line-through;"),_Zs=unCStr("li"),_Zt=function(_Zu,_Zv){var _Zw=new T(function(){return A(_Zu,[_Zv]);});return function(_Zx,_){var _Zy=jsCreateElem(toJSStr(E(_Zs))),_Zz=jsAppendChild(_Zy,E(_Zx)[1]),_ZA=[0,_Zy],_ZB=A(_Zw,[_ZA,_]);return _ZA;};},_ZC=function(_ZD){var _ZE=E(_ZD);if(!_ZE[0]){return [0];}else{var _ZF=new T(function(){return _wr(_z,_ZE[1]);});return [1,function(_ZG,_){var _ZH=_4P(_Zl,function(_ZI){var _ZJ=E(_ZI);return _ZJ[0]==0?function(_ZK,_){return [0,[0,_ZF,_Cq],_ZK];}:!_tn(_ZJ[1],_Zi)?function(_ZL,_){return [0,[0,_ZF,_Cq],_ZL];}:E(_ZJ[2])[0]==0?function(_ZM,_){return [0,[0,function(_ZN,_){var _ZO=A(_ZF,[_ZN,_]),_ZP=A(_1d,[_1j,_ZO,_DS,_Zr,_]);return _ZO;},_Cq],_ZM];}:function(_ZQ,_){return [0,[0,_ZF,_Cq],_ZQ];};},_ZG,_),_ZR=E(_ZH),_ZS=E(_ZR[1]);return [0,[0,new T(function(){return _Zt(_11,_ZS[1]);}),_ZS[2]],_ZR[2]];},new T(function(){return _ZC(_ZE[2]);})];}},_ZT=function(_ZU,_ZV){while(1){var _ZW=(function(_ZX,_ZY){var _ZZ=E(_ZY);if(!_ZZ[0]){return E(_ZX);}else{_ZU=function(_100,_){var _101=A(_ZX,[_100,_]),_102=E(_101),_103=E(_102[1]),_104=A(_ZZ[1],[_102[2],_]),_105=E(_104),_106=E(_105[1]);return [0,[0,function(_107,_){var _108=A(_103[1],[_107,_]),_109=A(_106[1],[_107,_]);return _107;},new T(function(){var _10a=E(_103[2]);return _10a[0]==0?E(_106[2]):E(_10a);})],_105[2]];};_ZV=_ZZ[2];return null;}})(_ZU,_ZV);if(_ZW!=null){return _ZW;}}},_10b=function(_10c,_10d,_){return _4P(_5x,function(_10e){var _10f=E(E(_10e)[2]);return _10f[0]==3?E(E(_10f[1])[1])==13?function(_Cx,_){return _4P(_YW,function(_10g){return function(_Cx,_){return _4P(_Z5,function(_10h){var _10i=new T(function(){return _ZT(_Zf,_ZC([1,_10c,_10h]));});return function(_as,_vx){return _4P(function(_Cx,_){return _4P(_Co,function(_10j){return function(_10k,_){return [0,_Z9,new T(function(){var _10l=E(_10j);return [0,_10l[1],_10l[2],_10l[3],_10l[4],_10l[5],new T(function(){return _Cb(I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Xx,_g,[1,_10c,_10h],_10l[6]);})];})];};},_Cx,_);},function(_10m,_Cx,_){return (function(_Cx,_){return _4P(function(_Cx,_){return _Xz(_Zh,_Xy,_10i,_Cx,_);},_Zb,_Cx,_);})(_Cx,_);},_as,_vx);};},_Cx,_);};},_Cx,_);}:E(_XZ):E(_Y2);},_10d,_);},_10n=new T(function(){return A(E(_YU)[1],[_9]);}),_10o=new T(function(){return _tY(_10n,_5D);}),_10p=unCStr("todos"),_10q=new T(function(){return _WT(_z,_10p);}),_10r=new T(function(){return _N(_11,_Z);}),_10s=function(_10t,_){var _10u=_4P(_10o,_10b,_10t,_),_10v=E(_10u),_10w=E(_10v[1]),_10x=new T(function(){return _zk(_11,function(_10y,_){var _10z=A(_10q,[_10y,_]),_10A=A(_10w[1],[_10y,_]);return _10y;});});return [0,[0,function(_10B,_){var _10C=A(_10x,[_10B,_]),_10D=A(_10r,[_10B,_]),_10E=A(_1d,[_1j,_10D,_Kb,_Zh,_]);return _10B;},new T(function(){var _10F=E(_10w[2]);return _10F[0]==0?E(_Cq):E(_10F);})],_10v[2]];},_10G=function(_10H,_10I,_){return [0,[0,_Z,[1,[1,_10H]]],_10I];},_10J=unCStr("revEntry"),_10K=new T(function(){var _10L=_Yv(_I7,_Fo,_Fn,_10J);return [0,_10L[1],_10L[2],_10L[3]];}),_10M=new T(function(){return A(E(_10K)[1],[_9]);}),_10N=new T(function(){return _tY(_10M,_5D);}),_10O=function(_10P,_10Q,_){return [0,[0,_Z,[1,[0,_10P]]],_10Q];},_10R=unCStr("entry"),_10S=new T(function(){var _10T=_Yv(_I7,_Fo,_Fn,_10R);return [0,_10T[1],_10T[2],_10T[3]];}),_10U=new T(function(){return A(E(_10S)[1],[_9]);}),_10V=new T(function(){return _tY(_10U,_5D);}),_10W=function(_10X,_){var _10Y=_4P(_10V,_10O,_10X,_),_10Z=E(_10Y),_110=E(_10Z[1]),_111=_4P(_10N,_10G,_10Z[2],_),_112=E(_111),_113=E(_112[1]);return [0,[0,new T(function(){return _zk(_11,function(_114,_){var _115=A(_110[1],[_114,_]),_116=_G(_114,_),_117=A(_113[1],[_114,_]);return _114;});}),new T(function(){var _118=E(_110[2]);return _118[0]==0?E(_113[2]):E(_118);})],_112[2]];},_119=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_11a=new T(function(){return _we(_z,_119);}),_11b=function(_11c){var _11d=A(E(_10K)[2],[_11c]);return function(_11e,_){var _11f=A(_11d,[_]);return [0,[0,_Z,[1,_11f]],_11e];};},_11g=function(_11h,_11i){while(1){var _11j=E(_11h);if(!_11j[0]){return E(_11i);}else{_11h=_11j[2];var _11k=[1,_11j[1],_11i];_11i=_11k;continue;}}},_11l=function(_11m){var _11n=new T(function(){return _11g(_11m,_g);});return function(_11o,_){return [0,[0,_Z,[1,_11n]],_11o];};},_11p=new T(function(){var _11q=E(E(_10S)[3]);return function(_11r,_){var _11s=A(_11q,[_]);return [0,[0,_Z,[1,_11s]],_11r];};}),_11t=function(_Cx,_){return _4P(_11p,_11l,_Cx,_);},_11u=function(_Cx,_){return _4P(_11t,_11b,_Cx,_);},_11v=function(_11w){var _11x=A(E(_10S)[2],[_11w]);return function(_11y,_){var _11z=A(_11x,[_]);return [0,[0,_Z,[1,_11z]],_11y];};},_11A=new T(function(){var _11B=E(E(_10K)[3]);return function(_11C,_){var _11D=A(_11B,[_]);return [0,[0,_Z,[1,_11D]],_11C];};}),_11E=function(_11F){var _11G=new T(function(){return _11g(_11F,_g);});return function(_11H,_){return [0,[0,_Z,[1,_11G]],_11H];};},_11I=function(_Cx,_){return _4P(_11A,_11E,_Cx,_);},_11J=function(_Cx,_){return _4P(_11I,_11v,_Cx,_);},_11K=function(_11L){return E(_11L)[0]==0?E(_11u):E(_11J);},_11M=function(_11N,_){var _11O=_4P(_10W,_11K,_11N,_),_11P=E(_11O),_11Q=E(_11P[1]);return [0,[0,function(_11R,_){var _11S=A(_11a,[_11R,_]),_11T=A(_11Q[1],[_11R,_]);return _11R;},_11Q[2]],_11P[2]];},_11U=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_11V=new T(function(){return _we(_z,_11U);}),_11W=[0,_Z,_9],_11X=function(_11Y,_){return [0,_11W,_11Y];},_11Z=function(_120,_121,_122){var _123=E(_120),_124=_123[1],_125=_123[2],_126=_123[3],_127=_123[4],_128=E(_122);if(!_128[0]){var _129=_128[2],_12a=_128[3],_12b=_128[4],_12c=_128[5];switch(_AE(_123,_129)){case 0:return _AR(_129,_12a,_Cb(_124,_125,_126,_127,_121,_12b),_12c);case 1:return [0,_128[1],E(_123),_121,E(_12b),E(_12c)];default:return _Bw(_129,_12a,_12b,_Cb(_124,_125,_126,_127,_121,_12c));}}else{return [0,1,E(_123),_121,E(_f),E(_f)];}},_12d=function(_12e,_12f,_12g){var _12h=E(_12g);if(!_12h[0]){var _12i=_12h[3],_12j=_12h[4],_12k=_12h[5],_12l=E(_12h[2]),_12m=_12l[1];return _12e>=_12m?_12e!=_12m?_Bw(_12l,_12i,_12j,_12d(_12e,_12f,_12k)):[0,_12h[1],E([0,_12e]),_12f,E(_12j),E(_12k)]:_AR(_12l,_12i,_12d(_12e,_12f,_12j),_12k);}else{return [0,1,E([0,_12e]),_12f,E(_f),E(_f)];}},_12n=function(_12o,_12p,_12q){var _12r=E(_12o),_12s=_12r[1],_12t=E(_12q);if(!_12t[0]){var _12u=_12t[3],_12v=_12t[4],_12w=_12t[5],_12x=E(_12t[2]),_12y=_12x[1];return _12s>=_12y?_12s!=_12y?_Bw(_12x,_12u,_12v,_12d(_12s,_12p,_12w)):[0,_12t[1],E(_12r),_12p,E(_12v),E(_12w)]:_AR(_12x,_12u,_12d(_12s,_12p,_12v),_12w);}else{return [0,1,E(_12r),_12p,E(_f),E(_f)];}},_12z=function(_12A,_12B){while(1){var _12C=E(_12B);if(!_12C[0]){var _12D=E(_12C[2])[1];if(_12A>=_12D){if(_12A!=_12D){_12B=_12C[5];continue;}else{return [1,_12C[3]];}}else{_12B=_12C[4];continue;}}else{return [0];}}},_12E=unCStr("containers-0.5.5.1"),_12F=unCStr("Data.Map.Base"),_12G=unCStr("Map"),_12H=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_12E,_12F,_12G],_12I=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_12H,_g],_12J=function(_12K){return E(_12I);},_12L=function(_12M){var _12N=E(_12M);if(!_12N[0]){return [0];}else{var _12O=E(_12N[1]);return [1,[0,_12O[1],_12O[2]],new T(function(){return _12L(_12N[2]);})];}},_12P=function(_12Q,_12R){return function(_12S){return E(new T(function(){var _12T=A(_12Q,[_6F]),_12U=E(_12T[3]),_12V=_12U[1],_12W=_12U[2],_12X=_2i(_12T[4],[1,new T(function(){return A(_12R,[_6F]);}),_g]);if(!_12X[0]){return [0,_12V,_12W,_12U,_g];}else{var _12Y=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_12V,_12W],new T(function(){return _12L(_12X);})]));}));return [0,_12Y[1],_12Y[2],_12U,_12X];}}));};},_12Z=new T(function(){return _12P(_12J,_yG);}),_130=new T(function(){return _6G(_12Z,_yG);}),_131=new T(function(){return _CU(_1F,_47,_1D,_130);}),_132=function(_133,_){var _134=A(_131,[_133,_]);return [0,[0,_Z,new T(function(){return E(E(_134)[1]);})],new T(function(){return E(E(_134)[2]);})];},_135=new T(function(){return _6G(_12Z,_yG);}),_136=[1,_f],_137=new T(function(){return _CU(_1F,_47,_1D,_135);}),_138=function(_139,_){var _13a=A(_137,[_139,_]);return [0,[0,_zt,new T(function(){var _13b=E(E(_13a)[1]);return _13b[0]==0?E(_136):E(_13b);})],new T(function(){return E(E(_13a)[2]);})];},_13c=[0,_Z,_wA],_13d=[1,_9],_13e=function(_13f,_13g){var _13h=new T(function(){return [0,E(_13f)[1]+1|0];});return function(_as,_vx){return _4P(function(_Cx,_){return _4P(function(_13i,_){var _13j=_4P(_132,function(_13k){var _13l=_12z(E(_13f)[1],_13k);return _13l[0]==0?E(_11X):function(_13m,_){return [0,[0,_Z,_13l],_13m];};},_13i,_),_13n=E(_13j),_13o=E(_13n[1]);return [0,[0,function(_13p,_){var _13q=A(_13o[1],[_13p,_]);return _13p;},new T(function(){var _13r=E(_13o[2]);return _13r[0]==0?E(_13d):[1,_13r];})],_13n[2]];},function(_13s){var _13t=new T(function(){return _tY(new T(function(){return A(_yI,[_9,_sM,_13s]);}),_5D);});return function(_as,_vx){return _4P(function(_13u,_){var _13v=A(_13t,[_13u,_]),_13w=E(_13v),_13x=_13w[2],_13y=E(_13w[1]),_13z=_13y[1],_13A=_13y[2],_13B=E(_13s);return _13B[0]==0?[0,[0,function(_13C,_){var _13D=A(_13z,[_13C,_]);return _13C;},_13A],_13x]:[0,[0,function(_13E,_){var _13F=A(_13z,[_13E,_]);return _13E;},new T(function(){var _13G=E(_13A);return _13G[0]==0?E(_13B):E(_13G);})],_13x];},function(_13H,_13I,_){return _4P(function(_Cx,_){return _4P(_138,function(_13J){var _13K=new T(function(){return _12n(_13f,_13H,_13J);}),_13L=new T(function(){return A(_135,[_13K]);});return function(_as,_vx){return _4P(_Co,function(_13M){return function(_13N,_){return [0,_13c,new T(function(){var _13O=E(_13M);return [0,_13O[1],_13O[2],_13O[3],_13O[4],_13O[5],new T(function(){return _11Z(_13L,_13K,_13O[6]);})];})];};},_as,_vx);};},_Cx,_);},function(_13P,_Cx,_){return (function(_13Q,_){return [0,[0,_Z,[1,_13H]],_13Q];})(_Cx,_);},_13I,_);},_as,_vx);};},_Cx,_);},function(_13R){var _13S=new T(function(){return _13e(_13h,new T(function(){return _x4(_13g,_13R);}));}),_13T=new T(function(){return _wr(_z,new T(function(){return _4f(0,E(_13g)[1]+E(_13R)[1]|0,_g);}));});return function(_as,_vx){return _4P(function(_13U,_){return [0,[0,function(_13V,_){var _13W=A(_13T,[_13V,_]),_13X=_G(_13V,_);return _13V;},_wA],_13U];},function(_13Y){return E(_13S);},_as,_vx);};},_as,_vx);};},_13Z=new T(function(){return _13e(_wL,_wL);}),_140=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_141=new T(function(){return _we(_z,_140);}),_142=new T(function(){return A(_yI,[_9,_sM,_9]);}),_143=new T(function(){return _tY(_142,_5D);}),_144=function(_145){var _146=new T(function(){return _wr(_z,new T(function(){return _wn(_145);}));});return function(_as,_vx){return _4P(_143,function(_147){var _148=E(E(_147)[1]);if(!_148){return function(_149,_){return [0,[0,function(_14a,_){var _14b=_G(_14a,_),_14c=_z(_wB,_14a,_),_14d=A(_146,[_14a,_]);return _14a;},_9],_149];};}else{var _14e=new T(function(){return _144(new T(function(){return [0,E(_145)[1]+_148|0];}));}),_14f=new T(function(){return _wr(_z,new T(function(){return _4f(0,E(_145)[1]+_148|0,_g);}));});return function(_as,_vx){return _4P(function(_14g,_){return [0,[0,function(_14h,_){var _14i=A(_14f,[_14h,_]),_14j=_G(_14h,_);return _14h;},_wA],_14g];},function(_14k){return E(_14e);},_as,_vx);};}},_as,_vx);};},_14l=new T(function(){return _144(_wL);}),_14m=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_14n=new T(function(){return _we(_z,_14m);}),_14o=function(_14p){return function(_14q,_){return [0,[0,new T(function(){var _14r=new T(function(){return _wr(_z,new T(function(){return _wn(_14p);}));});return _we(_11,function(_14s,_){var _14t=_z(_wB,_14s,_),_14u=A(_14r,[_14s,_]);return _14s;});}),_wA],_14q];};},_14v=new T(function(){return A(_yI,[_9,_sM,_9]);}),_14w=new T(function(){return _tY(_14v,_5D);}),_14x=unCStr("second number "),_14y=unCStr("first number"),_14z=new T(function(){return A(_yI,[_9,_sM,_9]);}),_14A=new T(function(){return _tY(_14z,_5D);}),_14B=function(_14C,_){var _14D=A(_14w,[_14C,_]),_14E=E(_14D),_14F=E(_14E[1]),_14G=A(_14A,[_14E[2],_]),_14H=E(_14G),_14I=E(_14H[1]);return [0,[0,function(_14J,_){var _14K=_z(_14y,_14J,_),_14L=_G(_14J,_),_14M=A(_14F[1],[_14J,_]),_14N=_G(_14J,_),_14O=_z(_14x,_14J,_),_14P=_G(_14J,_),_14Q=A(_14I[1],[_14J,_]),_14R=_G(_14J,_);return _14J;},new T(function(){var _14S=E(_14F[2]);if(!_14S[0]){return [0];}else{var _14T=E(_14I[2]);return _14T[0]==0?[0]:[1,new T(function(){return _x4(_14S[1],_14T[1]);})];}})],_14H[2]];},_14U=function(_14V,_){var _14W=_4P(_14B,_14o,_14V,_),_14X=E(_14W),_14Y=E(_14X[1]),_14Z=new T(function(){return _we(_11,_14Y[1]);});return [0,[0,function(_150,_){var _151=A(_14n,[_150,_]),_152=A(_14Z,[_150,_]);return _150;},_14Y[2]],_14X[2]];},_153=unCStr("table"),_154=function(_155,_156){var _157=new T(function(){return A(_155,[_156]);});return function(_158,_){var _159=jsCreateElem(toJSStr(E(_153))),_15a=jsAppendChild(_159,E(_158)[1]),_15b=[0,_159],_15c=A(_157,[_15b,_]);return _15b;};},_15d=unCStr("hplayground examples"),_15e=new T(function(){return _WT(_z,_15d);}),_15f=unCStr("td"),_15g=function(_15h,_15i){var _15j=new T(function(){return A(_15h,[_15i]);});return function(_15k,_){var _15l=jsCreateElem(toJSStr(E(_15f))),_15m=jsAppendChild(_15l,E(_15k)[1]),_15n=[0,_15l],_15o=A(_15j,[_15n,_]);return _15n;};},_15p=unCStr("tr"),_15q=function(_15r,_15s){var _15t=new T(function(){return A(_15r,[_15s]);});return function(_15u,_){var _15v=jsCreateElem(toJSStr(E(_15p))),_15w=jsAppendChild(_15v,E(_15u)[1]),_15x=[0,_15v],_15y=A(_15t,[_15x,_]);return _15x;};},_15z=unCStr("bottom of the page"),_15A=new T(function(){return _wr(_z,_15z);}),_15B=unCStr("h3"),_15C=function(_15D,_15E){var _15F=new T(function(){return A(_15D,[_15E]);});return function(_15G,_){var _15H=jsCreateElem(toJSStr(E(_15B))),_15I=jsAppendChild(_15H,E(_15G)[1]),_15J=[0,_15H],_15K=A(_15F,[_15J,_]);return _15J;};},_15L=unCStr("https://github.com/agocorona/hplayground"),_15M=unCStr("   "),_15N=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_15O=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_15P=unCStr("Git repository"),_15Q=new T(function(){return _SC(_z,_15P);}),_15R=unCStr("Examples code"),_15S=new T(function(){return _SC(_z,_15R);}),_15T=unCStr("Article"),_15U=new T(function(){return _SC(_z,_15T);}),_15V=function(_15W,_){var _15X=A(_15Q,[_15W,_]),_15Y=A(_1d,[_1j,_15X,_SN,_15L,_]),_15Z=_z(_15M,_15W,_),_160=A(_15S,[_15W,_]),_161=A(_1d,[_1j,_160,_SN,_15N,_]),_162=_z(_15M,_15W,_),_163=A(_15U,[_15W,_]),_164=A(_1d,[_1j,_163,_SN,_15O,_]);return _15W;},_165=new T(function(){return _zk(_11,_15V);}),_166=new T(function(){return _15C(_11,_165);}),_167=function(_168,_){var _169=_14U(_168,_),_16a=E(_169),_16b=E(_16a[1]),_16c=A(_zi,[_16a[2],_]),_16d=E(_16c),_16e=E(_16d[1]),_16f=A(_14l,[_16d[2],_]),_16g=E(_16f),_16h=E(_16g[1]),_16i=A(_Dw,[_16g[2],_]),_16j=E(_16i),_16k=E(_16j[1]),_16l=_Js(_16j[2],_),_16m=E(_16l),_16n=E(_16m[1]),_16o=_4P(_VM,_UQ,_16m[2],_),_16p=E(_16o),_16q=E(_16p[1]),_16r=A(_13Z,[_16p[2],_]),_16s=E(_16r),_16t=E(_16s[1]),_16u=A(_Ed,[_16s[2],_]),_16v=E(_16u),_16w=E(_16v[1]),_16x=_SW(_16v[2],_),_16y=E(_16x),_16z=E(_16y[1]),_16A=_11M(_16y[2],_),_16B=E(_16A),_16C=E(_16B[1]),_16D=_10s(_16B[2],_),_16E=E(_16D),_16F=E(_16E[1]),_16G=_Ok(_16E[2],_),_16H=E(_16G),_16I=E(_16H[1]),_16J=_Ua(_16H[2],_),_16K=E(_16J),_16L=E(_16K[1]),_16M=_4P(_Xl,_WH,_16K[2],_),_16N=E(_16M),_16O=E(_16N[1]),_16P=new T(function(){return _154(_11,function(_16Q,_){var _16R=A(new T(function(){var _16S=new T(function(){return _15g(_11,function(_16T,_){var _16U=A(_141,[_16T,_]),_16V=A(_16h[1],[_16T,_]);return _16T;});}),_16W=new T(function(){return _15g(_11,_16e[1]);}),_16X=new T(function(){return _15g(_11,_16b[1]);});return _15q(_11,function(_16Y,_){var _16Z=A(_16X,[_16Y,_]),_170=A(_1d,[_1j,_16Z,_DS,_w8,_]),_171=A(_16W,[_16Y,_]),_172=A(_1d,[_1j,_171,_DS,_w8,_]),_173=A(_16S,[_16Y,_]),_174=A(_1d,[_1j,_173,_DS,_w8,_]);return _16Y;});}),[_16Q,_]),_175=A(_1d,[_1j,_16R,_DS,_wa,_]),_176=A(new T(function(){var _177=new T(function(){return _15g(_11,_16w[1]);}),_178=new T(function(){return _15g(_11,function(_179,_){var _17a=A(_11V,[_179,_]),_17b=A(_16t[1],[_179,_]);return _179;});}),_17c=new T(function(){return _15g(_11,function(_17d,_){var _17e=A(_16k[1],[_17d,_]),_17f=A(_16n[1],[_17d,_]),_17g=A(_16q[1],[_17d,_]);return _17d;});});return _15q(_11,function(_17h,_){var _17i=A(_17c,[_17h,_]),_17j=A(_1d,[_1j,_17i,_DS,_w8,_]),_17k=A(_178,[_17h,_]),_17l=A(_1d,[_1j,_17k,_DS,_w8,_]),_17m=A(_177,[_17h,_]),_17n=A(_1d,[_1j,_17m,_DS,_w8,_]);return _17h;});}),[_16Q,_]),_17o=A(_1d,[_1j,_176,_DS,_wa,_]),_17p=A(new T(function(){var _17q=new T(function(){return _15g(_11,function(_17r,_){var _17s=A(_Xv,[_17r,_]),_17t=A(_16F[1],[_17r,_]);return _17r;});}),_17u=new T(function(){return _15g(_11,_16C[1]);}),_17v=new T(function(){return _15g(_11,new T(function(){return _zk(_11,_16z[1]);}));});return _15q(_11,function(_17w,_){var _17x=A(_17v,[_17w,_]),_17y=A(_1d,[_1j,_17x,_DS,_w8,_]),_17z=A(_17u,[_17w,_]),_17A=A(_1d,[_1j,_17z,_DS,_w8,_]),_17B=A(_17q,[_17w,_]),_17C=A(_1d,[_1j,_17B,_DS,_w8,_]);return _17w;});}),[_16Q,_]),_17D=A(_1d,[_1j,_17p,_DS,_wa,_]),_17E=A(new T(function(){var _17F=new T(function(){return _15g(_11,_16O[1]);}),_17G=new T(function(){return _15g(_11,_16L[1]);}),_17H=new T(function(){return _15g(_11,_16I[1]);});return _15q(_11,function(_17I,_){var _17J=A(_17H,[_17I,_]),_17K=A(_1d,[_1j,_17J,_DS,_w8,_]),_17L=A(_17G,[_17I,_]),_17M=A(_1d,[_1j,_17L,_DS,_w8,_]),_17N=A(_17F,[_17I,_]),_17O=A(_1d,[_1j,_17N,_DS,_w8,_]);return _17I;});}),[_16Q,_]),_17P=A(_1d,[_1j,_17E,_DS,_wa,_]);return _16Q;});});return [0,[0,function(_17Q,_){var _17R=A(_15e,[_17Q,_]),_17S=A(_1d,[_1j,_17R,_DS,_DR,_]),_17T=A(_166,[_17Q,_]),_17U=A(_16P,[_17Q,_]),_17V=A(_1d,[_1j,_17U,_DS,_w9,_]),_17W=A(_15A,[_17Q,_]);return _17Q;},new T(function(){var _17X=E(_16b[2]);if(!_17X[0]){var _17Y=E(_16e[2]);if(!_17Y[0]){var _17Z=E(_16h[2]);if(!_17Z[0]){var _180=E(_16k[2]);if(!_180[0]){var _181=E(_16n[2]);if(!_181[0]){var _182=E(_16q[2]);if(!_182[0]){var _183=E(_16t[2]);if(!_183[0]){var _184=E(_16w[2]);if(!_184[0]){var _185=E(_16z[2]);if(!_185[0]){var _186=E(_16C[2]);if(!_186[0]){var _187=E(_16F[2]);if(!_187[0]){var _188=E(_16I[2]);if(!_188[0]){var _189=E(_16L[2]);return _189[0]==0?E(_16O[2]):E(_189);}else{return E(_188);}}else{return E(_187);}}else{return E(_186);}}else{return E(_185);}}else{return E(_184);}}else{return E(_183);}}else{return E(_182);}}else{return E(_181);}}else{return E(_180);}}else{return E(_17Z);}}else{return E(_17Y);}}else{return E(_17X);}})],_16N[2]];},_18a=new T(function(){return [0,"(function(){return document.body;})"];}),_18b=new T(function(){return _5(_18a);}),_18c=function(_){var _18d=_w0(_),_18e=A(_18b,[_]);return _l(_167,[0,_18e],_);},_18f=function(_){return _18c(_);};
var hasteMain = function() {A(_18f, [0]);};window.onload = hasteMain;