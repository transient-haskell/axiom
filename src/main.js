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

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=[0,0],_9=[0],_a=function(_b,_){return _9;},_c=function(_){return _9;},_d=[0,_c,_a],_e=2,_f=[1],_g=[0],_h=[0,_g,_8,_e,_d,_0,_f],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=unCStr("id"),_A=0,_B=function(_C,_D,_E,_F){return A(_C,[new T(function(){return function(_){var _G=jsSetAttr(E(_D)[1],toJSStr(E(_E)),toJSStr(E(_F)));return _A;};})]);},_H=function(_I){return E(_I);},_J=function(_K,_L,_M,_){var _N=E(_L),_O=A(_K,[_M,_]),_P=A(_B,[_H,_O,_N[1],_N[2],_]);return _O;},_Q=function(_R,_S){while(1){var _T=(function(_U,_V){var _W=E(_V);if(!_W[0]){return E(_U);}else{_R=function(_X,_){return _J(_U,_W[1],_X,_);};_S=_W[2];return null;}})(_R,_S);if(_T!=null){return _T;}}},_Y=function(_Z,_10,_){return [0,_A,_Z];},_11=function(_12,_){return [0,_12,_12];},_13=[0,coercionToken],_14=function(_15,_16,_){var _17=A(_15,[_]);return A(_16,[_]);},_18=function(_19,_1a,_){return _14(_19,_1a,_);},_1b=function(_1c,_1d,_){var _1e=A(_1c,[_]);return A(_1d,[_1e,_]);},_1f=unCStr("base"),_1g=unCStr("GHC.IO.Exception"),_1h=unCStr("IOException"),_1i=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1f,_1g,_1h],_1j=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1i,_g],_1k=function(_1l){return E(_1j);},_1m=function(_1n){return E(E(_1n)[1]);},_1o=unCStr("Maybe.fromJust: Nothing"),_1p=new T(function(){return err(_1o);}),_1q=function(_1r,_1s,_1t){var _1u=new T(function(){var _1v=A(_1r,[_1t]),_1w=A(_1s,[new T(function(){var _1x=E(_1u);return _1x[0]==0?E(_1p):E(_1x[1]);})]),_1y=hs_eqWord64(_1v[1],_1w[1]);if(!E(_1y)){return [0];}else{var _1z=hs_eqWord64(_1v[2],_1w[2]);return E(_1z)==0?[0]:[1,_1t];}});return E(_1u);},_1A=function(_1B){var _1C=E(_1B);return _1q(_1m(_1C[1]),_1k,_1C[2]);},_1D=unCStr(": "),_1E=[0,41],_1F=unCStr(" ("),_1G=function(_1H,_1I){var _1J=E(_1H);return _1J[0]==0?E(_1I):[1,_1J[1],new T(function(){return _1G(_1J[2],_1I);})];},_1K=unCStr("already exists"),_1L=unCStr("does not exist"),_1M=unCStr("protocol error"),_1N=unCStr("failed"),_1O=unCStr("invalid argument"),_1P=unCStr("inappropriate type"),_1Q=unCStr("hardware fault"),_1R=unCStr("unsupported operation"),_1S=unCStr("timeout"),_1T=unCStr("resource vanished"),_1U=unCStr("interrupted"),_1V=unCStr("resource busy"),_1W=unCStr("resource exhausted"),_1X=unCStr("end of file"),_1Y=unCStr("illegal operation"),_1Z=unCStr("permission denied"),_20=unCStr("user error"),_21=unCStr("unsatisified constraints"),_22=unCStr("system error"),_23=function(_24,_25){switch(E(_24)){case 0:return _1G(_1K,_25);case 1:return _1G(_1L,_25);case 2:return _1G(_1V,_25);case 3:return _1G(_1W,_25);case 4:return _1G(_1X,_25);case 5:return _1G(_1Y,_25);case 6:return _1G(_1Z,_25);case 7:return _1G(_20,_25);case 8:return _1G(_21,_25);case 9:return _1G(_22,_25);case 10:return _1G(_1M,_25);case 11:return _1G(_1N,_25);case 12:return _1G(_1O,_25);case 13:return _1G(_1P,_25);case 14:return _1G(_1Q,_25);case 15:return _1G(_1R,_25);case 16:return _1G(_1S,_25);case 17:return _1G(_1T,_25);default:return _1G(_1U,_25);}},_26=[0,125],_27=unCStr("{handle: "),_28=function(_29,_2a,_2b,_2c,_2d,_2e){var _2f=new T(function(){var _2g=new T(function(){return _23(_2a,new T(function(){var _2h=E(_2c);return _2h[0]==0?E(_2e):_1G(_1F,new T(function(){return _1G(_2h,[1,_1E,_2e]);}));}));}),_2i=E(_2b);return _2i[0]==0?E(_2g):_1G(_2i,new T(function(){return _1G(_1D,_2g);}));}),_2j=E(_2d);if(!_2j[0]){var _2k=E(_29);if(!_2k[0]){return E(_2f);}else{var _2l=E(_2k[1]);return _2l[0]==0?_1G(_27,new T(function(){return _1G(_2l[1],[1,_26,new T(function(){return _1G(_1D,_2f);})]);})):_1G(_27,new T(function(){return _1G(_2l[1],[1,_26,new T(function(){return _1G(_1D,_2f);})]);}));}}else{return _1G(_2j[1],new T(function(){return _1G(_1D,_2f);}));}},_2m=function(_2n){var _2o=E(_2n);return _28(_2o[1],_2o[2],_2o[3],_2o[4],_2o[6],_g);},_2p=function(_2q,_2r){var _2s=E(_2q);return _28(_2s[1],_2s[2],_2s[3],_2s[4],_2s[6],_2r);},_2t=[0,44],_2u=[0,93],_2v=[0,91],_2w=function(_2x,_2y,_2z){var _2A=E(_2y);return _2A[0]==0?unAppCStr("[]",_2z):[1,_2v,new T(function(){return A(_2x,[_2A[1],new T(function(){var _2B=function(_2C){var _2D=E(_2C);return _2D[0]==0?E([1,_2u,_2z]):[1,_2t,new T(function(){return A(_2x,[_2D[1],new T(function(){return _2B(_2D[2]);})]);})];};return _2B(_2A[2]);})]);})];},_2E=function(_2F,_2G){return _2w(_2p,_2F,_2G);},_2H=function(_2I,_2J,_2K){var _2L=E(_2J);return _28(_2L[1],_2L[2],_2L[3],_2L[4],_2L[6],_2K);},_2M=[0,_2H,_2m,_2E],_2N=new T(function(){return [0,_1k,_2M,_2O,_1A];}),_2O=function(_2P){return [0,_2N,_2P];},_2Q=7,_2R=function(_2S){return [0,_9,_2Q,_g,_2S,_9,_9];},_2T=function(_2U,_){return die(new T(function(){return _2O(new T(function(){return _2R(_2U);}));}));},_2V=function(_2W,_){return _2T(_2W,_);},_2X=function(_2Y,_){return _2Y;},_2Z=[0,_1b,_18,_2X,_2V],_30=function(_31){return E(E(_31)[1]);},_32=function(_33,_34,_35,_36){return A(_30,[_33,new T(function(){return A(_34,[_36]);}),function(_37){return A(_35,[new T(function(){return E(E(_37)[1]);}),new T(function(){return E(E(_37)[2]);})]);}]);},_38=function(_39,_3a,_3b,_3c){return A(_30,[_39,new T(function(){return A(_3a,[_3c]);}),function(_3d){return A(_3b,[new T(function(){return E(E(_3d)[2]);})]);}]);},_3e=function(_3f,_3g,_3h,_3i){return _38(_3f,_3g,_3h,_3i);},_3j=function(_3k){return E(E(_3k)[4]);},_3l=function(_3m,_3n){var _3o=new T(function(){return A(_3j,[_3m,_3n]);});return function(_3p){return E(_3o);};},_3q=function(_3r){return E(E(_3r)[3]);},_3s=function(_3t){var _3u=new T(function(){return _3q(_3t);});return [0,function(_3g,_3h,_3i){return _32(_3t,_3g,_3h,_3i);},function(_3g,_3h,_3i){return _3e(_3t,_3g,_3h,_3i);},function(_3v,_3w){return A(_3u,[[0,_3v,_3w]]);},function(_3i){return _3l(_3t,_3i);}];},_3x=new T(function(){return _3s(_2Z);}),_3y=[0,112],_3z=function(_3A,_3B){var _3C=jsShowI(_3A);return _1G(fromJSStr(_3C),_3B);},_3D=[0,41],_3E=[0,40],_3F=function(_3G,_3H,_3I){return _3H>=0?_3z(_3H,_3I):_3G<=6?_3z(_3H,_3I):[1,_3E,new T(function(){var _3J=jsShowI(_3H);return _1G(fromJSStr(_3J),[1,_3D,_3I]);})];},_3K=function(_3L,_3M,_3N,_3O){var _3P=E(_3M);return A(_3P[1],[new T(function(){var _3Q=E(_3L);return E(_3N);}),function(_3R){var _3S=new T(function(){return E(E(_3R)[2]);});return A(_3P[2],[new T(function(){return A(_3O,[new T(function(){var _3T=E(new T(function(){var _3U=E(_3L);return [0,coercionToken];})),_3V=E(_3R);return [0,_3V[1],new T(function(){return [0,E(_3S)[1]+1|0];}),_3V[3],_3V[4],_3V[5],_3V[6]];})]);}),new T(function(){return A(_3P[3],[[1,_3y,new T(function(){return _1G(_3F(0,E(_3S)[1],_g),new T(function(){return E(E(_3R)[1]);}));})]]);})]);}]);},_3W=new T(function(){return _3K(_13,_3x,_11,_Y);}),_3X=unCStr("span"),_3Y=function(_3Z,_40,_){var _41=jsCreateElem(toJSStr(E(_3Z))),_42=jsAppendChild(_41,E(_40)[1]);return [0,_41];},_43=function(_X,_){return _3Y(_3X,_X,_);},_44=unCStr(" could be found!"),_45=function(_46){return err(unAppCStr("No element with ID ",new T(function(){return _1G(_46,_44);})));},_47=function(_48,_49,_){var _4a=E(_49),_4b=jsFind(toJSStr(_4a)),_4c=E(_4b);if(!_4c[0]){return _45(_4a);}else{var _4d=E(_4c[1]),_4e=jsClearChildren(_4d[1]);return _l(_48,_4d,_);}},_4f=function(_4g,_4h,_4i,_){var _4j=A(_3W,[_4i,_]),_4k=E(_4j),_4l=_4k[1],_4m=E(_4k[2]),_4n=_4m[2],_4o=E(_4m[4]),_4p=A(_4g,[[0,_4m[1],_4n,_4m[3],[0,function(_){return _47(function(_4q,_){var _4r=A(_4g,[new T(function(){var _4s=E(_4q);return [0,_4s[1],_4n,_4s[3],_4s[4],_4s[5],_4s[6]];}),_]);return [0,[0,_2X,E(E(_4r)[1])[2]],_4q];},_4l,_);},function(_4t,_){var _4u=_47(new T(function(){return A(_4h,[_4t]);}),_4l,_),_4v=E(_4u);return _4v[0]==0?_9:A(_4o[2],[_4v[1],_]);}],_4m[5],_4m[6]],_]),_4w=E(_4p),_4x=_4w[2],_4y=E(_4w[1]),_4z=_4y[1],_4A=new T(function(){return _Q(_43,[1,[0,_z,_4l],_g]);}),_4B=E(_4y[2]);if(!_4B[0]){return [0,[0,function(_4C,_){var _4D=A(_4z,[_4C,_]),_4E=A(_4A,[_4C,_]);return _4C;},_9],new T(function(){var _4F=E(_4x);return [0,_4F[1],_4F[2],_4F[3],_4o,_4F[5],_4F[6]];})];}else{var _4G=A(_4h,[_4B[1],new T(function(){var _4H=E(_4x);return [0,_4H[1],_4H[2],_4H[3],_4o,_4H[5],_4H[6]];}),_]),_4I=E(_4G),_4J=E(_4I[1]);return [0,[0,function(_4K,_){var _4L=A(_4z,[_4K,_]),_4M=A(_4A,[_4K,_]),_4N=A(_4J[1],[_4M,_]);return _4K;},_4J[2]],_4I[2]];}},_4O=unCStr("padding:15px;border-style:dotted"),_4P=unCStr("border-collapse:collapse"),_4Q=unCStr("vertical-align:top"),_4R=[0,3],_4S=function(_4T,_4U,_){var _4V=jsCreateTextNode(toJSStr(E(_4T))),_4W=jsAppendChild(_4V,E(_4U)[1]);return [0,_4V];},_4X=[0,112],_4Y=[1,_4X,_g],_4Z=function(_50,_51){var _52=new T(function(){return A(_50,[_51]);});return function(_53,_){var _54=jsCreateElem(toJSStr(_4Y)),_55=jsAppendChild(_54,E(_53)[1]),_56=[0,_54],_57=A(_52,[_56,_]);return _56;};},_58=function(_59){return _3F(0,E(_59)[1],_g);},_5a=[0,98],_5b=[1,_5a,_g],_5c=function(_5d,_5e){var _5f=new T(function(){return A(_5d,[_5e]);});return function(_5g,_){var _5h=jsCreateElem(toJSStr(_5b)),_5i=jsAppendChild(_5h,E(_5g)[1]),_5j=[0,_5h],_5k=A(_5f,[_5j,_]);return _5j;};},_5l=unCStr("br"),_5m=function(_5n,_){var _5o=jsCreateElem(toJSStr(E(_5l))),_5p=jsAppendChild(_5o,E(_5n)[1]);return [0,_5o];},_5q=[1,_A],_5r=unCStr("result: "),_5s=function(_5t){var _5u=new T(function(){return _5c(_4S,new T(function(){return _58(_5t);}));});return function(_5v,_){return [0,[0,function(_5w,_){var _5x=_5m(_5w,_),_5y=_4S(_5r,_5w,_),_5z=A(_5u,[_5w,_]);return _5w;},_5q],_5v];};},_5A=unCStr(" numbers and append the result using a fold"),_5B=[0,0],_5C=[1,_5B],_5D=[0,_2X,_5C],_5E=function(_5F,_){return [0,_5D,_5F];},_5G=function(_5H,_5I,_5J,_){var _5K=_3Y(_5H,_5J,_),_5L=A(_5I,[_5K,_]);return _5K;},_5M=unCStr("()"),_5N=unCStr("GHC.Tuple"),_5O=unCStr("ghc-prim"),_5P=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5O,_5N,_5M],_5Q=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5P,_g],_5R=function(_5S){return E(_5Q);},_5T=unCStr("haste-perch-0.1.0.1"),_5U=unCStr("Haste.Perch"),_5V=unCStr("PerchM"),_5W=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5T,_5U,_5V],_5X=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5W,_g],_5Y=function(_5Z){return E(_5X);},_60=function(_61){var _62=E(_61);return _62[0]==0?[0]:_1G(_62[1],new T(function(){return _60(_62[2]);}));},_63=function(_64,_65){var _66=E(_64);if(!_66){return [0,_g,_65];}else{var _67=E(_65);if(!_67[0]){return [0,_g,_g];}else{var _68=new T(function(){var _69=_63(_66-1|0,_67[2]);return [0,_69[1],_69[2]];});return [0,[1,_67[1],new T(function(){return E(E(_68)[1]);})],new T(function(){return E(E(_68)[2]);})];}}},_6a=[0,120],_6b=[0,48],_6c=function(_6d){var _6e=new T(function(){var _6f=_63(8,new T(function(){var _6g=md5(toJSStr(E(_6d)));return fromJSStr(_6g);}));return [0,_6f[1],_6f[2]];}),_6h=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(E(_6e)[1]);})]])]),_6i=new T(function(){var _6j=_63(8,new T(function(){return E(E(_6e)[2]);}));return [0,_6j[1],_6j[2]];}),_6k=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(E(_6i)[1]);})]])]),_6l=hs_mkWord64(_6h,_6k),_6m=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(_63(8,new T(function(){return E(E(_6i)[2]);}))[1]);})]])]),_6n=hs_mkWord64(_6m,_6m);return [0,_6l,_6n];},_6o=function(_6p,_6q){var _6r=E(_6q);return _6r[0]==0?[0]:[1,new T(function(){return A(_6p,[_6r[1]]);}),new T(function(){return _6o(_6p,_6r[2]);})];},_6s=function(_6t,_6u){var _6v=jsShowI(_6t),_6w=md5(_6v);return _1G(fromJSStr(_6w),new T(function(){var _6x=jsShowI(_6u),_6y=md5(_6x);return fromJSStr(_6y);}));},_6z=function(_6A){var _6B=E(_6A);return _6s(_6B[1],_6B[2]);},_6C=function(_6D){var _6E=E(_6D);if(!_6E[0]){return [0];}else{var _6F=E(_6E[1]);return [1,[0,_6F[1],_6F[2]],new T(function(){return _6C(_6E[2]);})];}},_6G=unCStr("Prelude.undefined"),_6H=new T(function(){return err(_6G);}),_6I=function(_6J,_6K){return function(_6L){return E(new T(function(){var _6M=A(_6J,[_6H]),_6N=E(_6M[3]),_6O=_6N[1],_6P=_6N[2],_6Q=_1G(_6M[4],[1,new T(function(){return A(_6K,[_6H]);}),_g]);if(!_6Q[0]){return [0,_6O,_6P,_6N,_g];}else{var _6R=_6c(new T(function(){return _60(_6o(_6z,[1,[0,_6O,_6P],new T(function(){return _6C(_6Q);})]));}));return [0,_6R[1],_6R[2],_6N,_6Q];}}));};},_6S=new T(function(){return _6I(_5Y,_5R);}),_6T=unCStr("value"),_6U=unCStr("onclick"),_6V=unCStr("checked"),_6W=[0,_6V,_g],_6X=[1,_6W,_g],_6Y=unCStr("type"),_6Z=unCStr("input"),_70=function(_71,_){return _3Y(_6Z,_71,_);},_72=function(_73,_74,_75,_76,_77){var _78=new T(function(){var _79=new T(function(){return _Q(_70,[1,[0,_6Y,_74],[1,[0,_z,_73],[1,[0,_6T,_75],_g]]]);});return !E(_76)?E(_79):_Q(_79,_6X);}),_7a=E(_77);return _7a[0]==0?E(_78):_Q(_78,[1,[0,_6U,_7a[1]],_g]);},_7b=unCStr("href"),_7c=[0,97],_7d=[1,_7c,_g],_7e=function(_7f,_){return _3Y(_7d,_7f,_);},_7g=function(_7h,_7i){var _7j=new T(function(){return _Q(_7e,[1,[0,_7b,_7h],_g]);});return function(_7k,_){var _7l=A(_7j,[_7k,_]),_7m=A(_7i,[_7l,_]);return _7l;};},_7n=function(_7o){return _7g(_7o,function(_X,_){return _4S(_7o,_X,_);});},_7p=unCStr("option"),_7q=function(_7r,_){return _3Y(_7p,_7r,_);},_7s=unCStr("selected"),_7t=[0,_7s,_g],_7u=[1,_7t,_g],_7v=function(_7w,_7x,_7y){var _7z=new T(function(){return _Q(_7q,[1,[0,_6T,_7w],_g]);}),_7A=function(_7B,_){var _7C=A(_7z,[_7B,_]),_7D=A(_7x,[_7C,_]);return _7C;};return !E(_7y)?E(_7A):_Q(_7A,_7u);},_7E=function(_7F,_7G){return _7v(_7F,function(_X,_){return _4S(_7F,_X,_);},_7G);},_7H=unCStr("method"),_7I=unCStr("action"),_7J=unCStr("UTF-8"),_7K=unCStr("acceptCharset"),_7L=[0,_7K,_7J],_7M=unCStr("form"),_7N=function(_7O,_){return _3Y(_7M,_7O,_);},_7P=function(_7Q,_7R,_7S){var _7T=new T(function(){return _Q(_7N,[1,_7L,[1,[0,_7I,_7Q],[1,[0,_7H,_7R],_g]]]);});return function(_7U,_){var _7V=A(_7T,[_7U,_]),_7W=A(_7S,[_7V,_]);return _7V;};},_7X=unCStr("select"),_7Y=function(_7Z,_){return _3Y(_7X,_7Z,_);},_80=function(_81,_82){var _83=new T(function(){return _Q(_7Y,[1,[0,_z,_81],_g]);});return function(_84,_){var _85=A(_83,[_84,_]),_86=A(_82,[_85,_]);return _85;};},_87=unCStr("textarea"),_88=function(_89,_){return _3Y(_87,_89,_);},_8a=function(_8b,_8c){var _8d=new T(function(){return _Q(_88,[1,[0,_z,_8b],_g]);});return function(_8e,_){var _8f=A(_8d,[_8e,_]),_8g=_4S(_8c,_8f,_);return _8f;};},_8h=unCStr("color:red"),_8i=unCStr("style"),_8j=[0,_8i,_8h],_8k=[1,_8j,_g],_8l=[0,98],_8m=[1,_8l,_g],_8n=function(_8o){return _Q(function(_8p,_){var _8q=_3Y(_8m,_8p,_),_8r=A(_8o,[_8q,_]);return _8q;},_8k);},_8s=function(_8t,_8u,_){var _8v=E(_8t);if(!_8v[0]){return _8u;}else{var _8w=A(_8v[1],[_8u,_]),_8x=_8s(_8v[2],_8u,_);return _8u;}},_8y=function(_8z,_8A,_8B,_){var _8C=A(_8z,[_8B,_]),_8D=A(_8A,[_8B,_]);return _8B;},_8E=[0,_2X,_8y,_8s],_8F=[0,_8E,_6S,_4S,_4S,_5G,_8n,_7g,_7n,_72,_8a,_80,_7v,_7E,_7P,_Q],_8G=function(_8H,_8I,_){var _8J=A(_8I,[_]);return _8H;},_8K=function(_8L,_8M,_){var _8N=A(_8M,[_]);return new T(function(){return A(_8L,[_8N]);});},_8O=[0,_8K,_8G],_8P=function(_8Q){var _8R=E(_8Q);return _8R[0]==0?0:E(_8R[1])[1]+_8P(_8R[2])|0;},_8S=function(_8T){return [0,_8P(_8T)];},_8U=function(_8V,_8W){return [0,E(_8V)[1]+E(_8W)[1]|0];},_8X=[0,_5B,_8U,_8S],_8Y=function(_8Z,_90){var _91=E(_90);return _91[0]==0?[0]:[1,new T(function(){return A(_8Z,[_91[1]]);})];},_92=function(_93){return E(E(_93)[1]);},_94=function(_95){return E(E(_95)[2]);},_96=function(_97,_98,_99,_9a,_9b,_9c){var _9d=new T(function(){return _94(_97);});return A(_98,[new T(function(){return A(_9a,[_9c]);}),function(_9e){var _9f=E(_9e),_9g=E(_9f[1]);return A(_98,[new T(function(){return A(_9b,[_9f[2]]);}),function(_9h){var _9i=E(_9h),_9j=E(_9i[1]);return A(_99,[[0,[0,new T(function(){return A(_9d,[_9g[1],_9j[1]]);}),new T(function(){var _9k=E(_9g[2]);if(!_9k[0]){return [0];}else{var _9l=E(_9j[2]);return _9l[0]==0?[0]:[1,new T(function(){return A(_9k[1],[_9l[1]]);})];}})],_9i[2]]]);}]);}]);},_9m=function(_9n){return E(E(_9n)[1]);},_9o=function(_9p,_9q,_9r,_9s,_9t,_9u){var _9v=new T(function(){return _92(_9p);});return function(_9w){var _9x=E(_9q);return _96(_9v,_9x[1],_9x[3],function(_9y){return A(new T(function(){var _9z=new T(function(){return _94(_9s);});return A(_9m,[_9r,function(_9A){return [0,new T(function(){var _9B=E(E(_9A)[1]);return [0,_9B[1],new T(function(){return _8Y(_9z,_9B[2]);})];}),new T(function(){return E(E(_9A)[2]);})];}]);}),[new T(function(){return A(_9t,[_9y]);})]);},_9u,_9w);};},_9C=function(_9D,_9E){while(1){var _9F=(function(_9G,_9H){var _9I=E(_9H);if(!_9I[0]){return E(_9G);}else{_9D=new T(function(){return _9o(_8F,_2Z,_8O,_8X,_9G,_9I[1]);});_9E=_9I[2];return null;}})(_9D,_9E);if(_9F!=null){return _9F;}}},_9J=[13,coercionToken],_9K=unCStr("text"),_9L=[0,_2Z,_H],_9M=unCStr("base"),_9N=unCStr("Control.Exception.Base"),_9O=unCStr("PatternMatchFail"),_9P=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9M,_9N,_9O],_9Q=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9P,_g],_9R=function(_9S){return E(_9Q);},_9T=function(_9U){var _9V=E(_9U);return _1q(_1m(_9V[1]),_9R,_9V[2]);},_9W=function(_9X){return E(E(_9X)[1]);},_9Y=function(_9Z,_a0){return _1G(E(_9Z)[1],_a0);},_a1=function(_a2,_a3){return _2w(_9Y,_a2,_a3);},_a4=function(_a5,_a6,_a7){return _1G(E(_a6)[1],_a7);},_a8=[0,_a4,_9W,_a1],_a9=new T(function(){return [0,_9R,_a8,_aa,_9T];}),_aa=function(_ab){return [0,_a9,_ab];},_ac=unCStr("Non-exhaustive patterns in"),_ad=function(_ae,_af){return die(new T(function(){return A(_af,[_ae]);}));},_ag=function(_ah,_ai){var _aj=E(_ai);if(!_aj[0]){return [0,_g,_g];}else{var _ak=_aj[1];if(!A(_ah,[_ak])){return [0,_g,_aj];}else{var _al=new T(function(){var _am=_ag(_ah,_aj[2]);return [0,_am[1],_am[2]];});return [0,[1,_ak,new T(function(){return E(E(_al)[1]);})],new T(function(){return E(E(_al)[2]);})];}}},_an=[0,32],_ao=[0,10],_ap=[1,_ao,_g],_aq=function(_ar){return E(E(_ar)[1])==124?false:true;},_as=function(_at,_au){var _av=_ag(_aq,unCStr(_at)),_aw=_av[1],_ax=function(_ay,_az){return _1G(_ay,new T(function(){return unAppCStr(": ",new T(function(){return _1G(_au,new T(function(){return _1G(_az,_ap);}));}));}));},_aA=E(_av[2]);return _aA[0]==0?_ax(_aw,_g):E(E(_aA[1])[1])==124?_ax(_aw,[1,_an,_aA[2]]):_ax(_aw,_g);},_aB=function(_aC){return _ad([0,new T(function(){return _as(_aC,_ac);})],_aa);},_aD=new T(function(){return _aB("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aE=function(_aF,_aG){while(1){var _aH=(function(_aI,_aJ){var _aK=E(_aI);switch(_aK[0]){case 0:var _aL=E(_aJ);if(!_aL[0]){return [0];}else{_aF=A(_aK[1],[_aL[1]]);_aG=_aL[2];return null;}break;case 1:var _aM=A(_aK[1],[_aJ]),_aN=_aJ;_aF=_aM;_aG=_aN;return null;case 2:return [0];case 3:return [1,[0,_aK[1],_aJ],new T(function(){return _aE(_aK[2],_aJ);})];default:return E(_aK[1]);}})(_aF,_aG);if(_aH!=null){return _aH;}}},_aO=function(_aP,_aQ){var _aR=new T(function(){var _aS=E(_aQ);if(_aS[0]==3){return [3,_aS[1],new T(function(){return _aO(_aP,_aS[2]);})];}else{var _aT=E(_aP);if(_aT[0]==2){return E(_aS);}else{var _aU=E(_aS);if(_aU[0]==2){return E(_aT);}else{var _aV=new T(function(){var _aW=E(_aU);if(_aW[0]==4){return [1,function(_aX){return [4,new T(function(){return _1G(_aE(_aT,_aX),_aW[1]);})];}];}else{var _aY=E(_aT);if(_aY[0]==1){var _aZ=_aY[1],_b0=E(_aW);return _b0[0]==0?[1,function(_b1){return _aO(A(_aZ,[_b1]),_b0);}]:[1,function(_b2){return _aO(A(_aZ,[_b2]),new T(function(){return A(_b0[1],[_b2]);}));}];}else{var _b3=E(_aW);return _b3[0]==0?E(_aD):[1,function(_b4){return _aO(_aY,new T(function(){return A(_b3[1],[_b4]);}));}];}}}),_b5=E(_aT);switch(_b5[0]){case 1:var _b6=E(_aU);return _b6[0]==4?[1,function(_b7){return [4,new T(function(){return _1G(_aE(A(_b5[1],[_b7]),_b7),_b6[1]);})];}]:E(_aV);case 4:var _b8=_b5[1],_b9=E(_aU);switch(_b9[0]){case 0:return [1,function(_ba){return [4,new T(function(){return _1G(_b8,new T(function(){return _aE(_b9,_ba);}));})];}];case 1:return [1,function(_bb){return [4,new T(function(){return _1G(_b8,new T(function(){return _aE(A(_b9[1],[_bb]),_bb);}));})];}];default:return [4,new T(function(){return _1G(_b8,_b9[1]);})];}break;default:return E(_aV);}}}}}),_bc=E(_aP);switch(_bc[0]){case 0:var _bd=E(_aQ);return _bd[0]==0?[0,function(_be){return _aO(A(_bc[1],[_be]),new T(function(){return A(_bd[1],[_be]);}));}]:E(_aR);case 3:return [3,_bc[1],new T(function(){return _aO(_bc[2],_aQ);})];default:return E(_aR);}},_bf=function(_bg,_bh){return E(_bg)[1]!=E(_bh)[1];},_bi=function(_bj,_bk){return E(_bj)[1]==E(_bk)[1];},_bl=[0,_bi,_bf],_bm=function(_bn){return E(E(_bn)[1]);},_bo=function(_bp,_bq,_br){while(1){var _bs=E(_bq);if(!_bs[0]){return E(_br)[0]==0?true:false;}else{var _bt=E(_br);if(!_bt[0]){return false;}else{if(!A(_bm,[_bp,_bs[1],_bt[1]])){return false;}else{_bq=_bs[2];_br=_bt[2];continue;}}}}},_bu=function(_bv,_bw,_bx){return !_bo(_bv,_bw,_bx)?true:false;},_by=function(_bz){return [0,function(_bA,_bB){return _bo(_bz,_bA,_bB);},function(_bA,_bB){return _bu(_bz,_bA,_bB);}];},_bC=new T(function(){return _by(_bl);}),_bD=function(_bE,_bF){var _bG=E(_bE);switch(_bG[0]){case 0:return [0,function(_bH){return _bD(A(_bG[1],[_bH]),_bF);}];case 1:return [1,function(_bI){return _bD(A(_bG[1],[_bI]),_bF);}];case 2:return [2];case 3:return _aO(A(_bF,[_bG[1]]),new T(function(){return _bD(_bG[2],_bF);}));default:var _bJ=function(_bK){var _bL=E(_bK);if(!_bL[0]){return [0];}else{var _bM=E(_bL[1]);return _1G(_aE(A(_bF,[_bM[1]]),_bM[2]),new T(function(){return _bJ(_bL[2]);}));}},_bN=_bJ(_bG[1]);return _bN[0]==0?[2]:[4,_bN];}},_bO=[2],_bP=function(_bQ){return [3,_bQ,_bO];},_bR=function(_bS,_bT){var _bU=E(_bS);if(!_bU){return A(_bT,[_A]);}else{var _bV=new T(function(){return _bR(_bU-1|0,_bT);});return [0,function(_bW){return E(_bV);}];}},_bX=function(_bY,_bZ,_c0){var _c1=new T(function(){return A(_bY,[_bP]);});return [1,function(_c2){return A(function(_c3,_c4,_c5){while(1){var _c6=(function(_c7,_c8,_c9){var _ca=E(_c7);switch(_ca[0]){case 0:var _cb=E(_c8);if(!_cb[0]){return E(_bZ);}else{_c3=A(_ca[1],[_cb[1]]);_c4=_cb[2];var _cc=_c9+1|0;_c5=_cc;return null;}break;case 1:var _cd=A(_ca[1],[_c8]),_ce=_c8,_cc=_c9;_c3=_cd;_c4=_ce;_c5=_cc;return null;case 2:return E(_bZ);case 3:return function(_cf){var _cg=new T(function(){return _bD(_ca,_cf);});return _bR(_c9,function(_ch){return E(_cg);});};default:return function(_ci){return _bD(_ca,_ci);};}})(_c3,_c4,_c5);if(_c6!=null){return _c6;}}},[_c1,_c2,0,_c0]);}];},_cj=[6],_ck=unCStr("valDig: Bad base"),_cl=new T(function(){return err(_ck);}),_cm=function(_cn,_co){var _cp=function(_cq,_cr){var _cs=E(_cq);if(!_cs[0]){var _ct=new T(function(){return A(_cr,[_g]);});return function(_cu){return A(_cu,[_ct]);};}else{var _cv=E(_cs[1])[1],_cw=function(_cx){var _cy=new T(function(){return _cp(_cs[2],function(_cz){return A(_cr,[[1,_cx,_cz]]);});});return function(_cA){var _cB=new T(function(){return A(_cy,[_cA]);});return [0,function(_cC){return E(_cB);}];};};switch(E(E(_cn)[1])){case 8:if(48>_cv){var _cD=new T(function(){return A(_cr,[_g]);});return function(_cE){return A(_cE,[_cD]);};}else{if(_cv>55){var _cF=new T(function(){return A(_cr,[_g]);});return function(_cG){return A(_cG,[_cF]);};}else{return _cw([0,_cv-48|0]);}}break;case 10:if(48>_cv){var _cH=new T(function(){return A(_cr,[_g]);});return function(_cI){return A(_cI,[_cH]);};}else{if(_cv>57){var _cJ=new T(function(){return A(_cr,[_g]);});return function(_cK){return A(_cK,[_cJ]);};}else{return _cw([0,_cv-48|0]);}}break;case 16:var _cL=new T(function(){return 97>_cv?65>_cv?[0]:_cv>70?[0]:[1,[0,(_cv-65|0)+10|0]]:_cv>102?65>_cv?[0]:_cv>70?[0]:[1,[0,(_cv-65|0)+10|0]]:[1,[0,(_cv-97|0)+10|0]];});if(48>_cv){var _cM=E(_cL);if(!_cM[0]){var _cN=new T(function(){return A(_cr,[_g]);});return function(_cO){return A(_cO,[_cN]);};}else{return _cw(_cM[1]);}}else{if(_cv>57){var _cP=E(_cL);if(!_cP[0]){var _cQ=new T(function(){return A(_cr,[_g]);});return function(_cR){return A(_cR,[_cQ]);};}else{return _cw(_cP[1]);}}else{return _cw([0,_cv-48|0]);}}break;default:return E(_cl);}}};return [1,function(_cS){return A(_cp,[_cS,_H,function(_cT){var _cU=E(_cT);return _cU[0]==0?[2]:A(_co,[_cU]);}]);}];},_cV=[0,10],_cW=[0,1],_cX=[0,2147483647],_cY=function(_cZ,_d0){while(1){var _d1=E(_cZ);if(!_d1[0]){var _d2=_d1[1],_d3=E(_d0);if(!_d3[0]){var _d4=_d3[1],_d5=addC(_d2,_d4);if(!E(_d5[2])){return [0,_d5[1]];}else{_cZ=[1,I_fromInt(_d2)];_d0=[1,I_fromInt(_d4)];continue;}}else{_cZ=[1,I_fromInt(_d2)];_d0=_d3;continue;}}else{var _d6=E(_d0);if(!_d6[0]){_cZ=_d1;_d0=[1,I_fromInt(_d6[1])];continue;}else{return [1,I_add(_d1[1],_d6[1])];}}}},_d7=new T(function(){return _cY(_cX,_cW);}),_d8=function(_d9){var _da=E(_d9);if(!_da[0]){var _db=E(_da[1]);return _db==(-2147483648)?E(_d7):[0, -_db];}else{return [1,I_negate(_da[1])];}},_dc=[0,10],_dd=[0,0],_de=function(_df,_dg){while(1){var _dh=E(_df);if(!_dh[0]){var _di=_dh[1],_dj=E(_dg);if(!_dj[0]){var _dk=_dj[1];if(!(imul(_di,_dk)|0)){return [0,imul(_di,_dk)|0];}else{_df=[1,I_fromInt(_di)];_dg=[1,I_fromInt(_dk)];continue;}}else{_df=[1,I_fromInt(_di)];_dg=_dj;continue;}}else{var _dl=E(_dg);if(!_dl[0]){_df=_dh;_dg=[1,I_fromInt(_dl[1])];continue;}else{return [1,I_mul(_dh[1],_dl[1])];}}}},_dm=function(_dn,_do,_dp){while(1){var _dq=E(_dp);if(!_dq[0]){return E(_do);}else{var _dr=_cY(_de(_do,_dn),_dq[1]);_dp=_dq[2];_do=_dr;continue;}}},_ds=function(_dt){var _du=new T(function(){return _aO(_aO([0,function(_dv){return E(E(_dv)[1])==45?_cm(_cV,function(_dw){return A(_dt,[[1,new T(function(){return _d8(_dm(_dc,_dd,_dw));})]]);}):[2];}],[0,function(_dx){return E(E(_dx)[1])==43?_cm(_cV,function(_dy){return A(_dt,[[1,new T(function(){return _dm(_dc,_dd,_dy);})]]);}):[2];}]),new T(function(){return _cm(_cV,function(_dz){return A(_dt,[[1,new T(function(){return _dm(_dc,_dd,_dz);})]]);});}));});return _aO([0,function(_dA){return E(E(_dA)[1])==101?E(_du):[2];}],[0,function(_dB){return E(E(_dB)[1])==69?E(_du):[2];}]);},_dC=function(_dD){return A(_dD,[_9]);},_dE=function(_dF){return A(_dF,[_9]);},_dG=function(_dH){var _dI=new T(function(){return _cm(_cV,function(_dJ){return A(_dH,[[1,_dJ]]);});});return [0,function(_dK){return E(E(_dK)[1])==46?E(_dI):[2];}];},_dL=function(_dM){return _cm(_cV,function(_dN){return _bX(_dG,_dC,function(_dO){return _bX(_ds,_dE,function(_dP){return A(_dM,[[5,[1,_dN,_dO,_dP]]]);});});});},_dQ=function(_dR,_dS,_dT){while(1){var _dU=E(_dT);if(!_dU[0]){return false;}else{if(!A(_bm,[_dR,_dS,_dU[1]])){_dT=_dU[2];continue;}else{return true;}}}},_dV=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dW=function(_dX){return _dQ(_bl,_dX,_dV);},_dY=[0,8],_dZ=[0,16],_e0=function(_e1){var _e2=new T(function(){return _cm(_dZ,function(_e3){return A(_e1,[[5,[0,_dZ,_e3]]]);});}),_e4=new T(function(){return _cm(_dY,function(_e5){return A(_e1,[[5,[0,_dY,_e5]]]);});}),_e6=new T(function(){return _cm(_dZ,function(_e7){return A(_e1,[[5,[0,_dZ,_e7]]]);});}),_e8=new T(function(){return _cm(_dY,function(_e9){return A(_e1,[[5,[0,_dY,_e9]]]);});});return [0,function(_ea){return E(E(_ea)[1])==48?E([0,function(_eb){switch(E(E(_eb)[1])){case 79:return E(_e8);case 88:return E(_e6);case 111:return E(_e4);case 120:return E(_e2);default:return [2];}}]):[2];}];},_ec=true,_ed=function(_ee){var _ef=new T(function(){return A(_ee,[_dZ]);}),_eg=new T(function(){return A(_ee,[_dY]);}),_eh=new T(function(){return A(_ee,[_dZ]);}),_ei=new T(function(){return A(_ee,[_dY]);});return [0,function(_ej){switch(E(E(_ej)[1])){case 79:return E(_ei);case 88:return E(_eh);case 111:return E(_eg);case 120:return E(_ef);default:return [2];}}];},_ek=function(_el){return A(_el,[_cV]);},_em=function(_en){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3F(9,_en,_g);})));},_eo=function(_ep){var _eq=E(_ep);return _eq[0]==0?E(_eq[1]):I_toInt(_eq[1]);},_er=function(_es,_et){var _eu=E(_es);if(!_eu[0]){var _ev=_eu[1],_ew=E(_et);return _ew[0]==0?_ev<=_ew[1]:I_compareInt(_ew[1],_ev)>=0;}else{var _ex=_eu[1],_ey=E(_et);return _ey[0]==0?I_compareInt(_ex,_ey[1])<=0:I_compare(_ex,_ey[1])<=0;}},_ez=function(_eA){return [2];},_eB=function(_eC){var _eD=E(_eC);if(!_eD[0]){return E(_ez);}else{var _eE=_eD[1],_eF=E(_eD[2]);if(!_eF[0]){return E(_eE);}else{var _eG=new T(function(){return _eB(_eF);});return function(_eH){return _aO(A(_eE,[_eH]),new T(function(){return A(_eG,[_eH]);}));};}}},_eI=unCStr("NUL"),_eJ=function(_eK){return [2];},_eL=function(_eM){return _eJ(_eM);},_eN=function(_eO,_eP){var _eQ=function(_eR,_eS){var _eT=E(_eR);if(!_eT[0]){return function(_eU){return A(_eU,[_eO]);};}else{var _eV=E(_eS);if(!_eV[0]){return E(_eJ);}else{if(E(_eT[1])[1]!=E(_eV[1])[1]){return E(_eL);}else{var _eW=new T(function(){return _eQ(_eT[2],_eV[2]);});return function(_eX){var _eY=new T(function(){return A(_eW,[_eX]);});return [0,function(_eZ){return E(_eY);}];};}}}};return [1,function(_f0){return A(_eQ,[_eO,_f0,_eP]);}];},_f1=[0,0],_f2=function(_f3){var _f4=new T(function(){return A(_f3,[_f1]);});return _eN(_eI,function(_f5){return E(_f4);});},_f6=unCStr("STX"),_f7=[0,2],_f8=function(_f9){var _fa=new T(function(){return A(_f9,[_f7]);});return _eN(_f6,function(_fb){return E(_fa);});},_fc=unCStr("ETX"),_fd=[0,3],_fe=function(_ff){var _fg=new T(function(){return A(_ff,[_fd]);});return _eN(_fc,function(_fh){return E(_fg);});},_fi=unCStr("EOT"),_fj=[0,4],_fk=function(_fl){var _fm=new T(function(){return A(_fl,[_fj]);});return _eN(_fi,function(_fn){return E(_fm);});},_fo=unCStr("ENQ"),_fp=[0,5],_fq=function(_fr){var _fs=new T(function(){return A(_fr,[_fp]);});return _eN(_fo,function(_ft){return E(_fs);});},_fu=unCStr("ACK"),_fv=[0,6],_fw=function(_fx){var _fy=new T(function(){return A(_fx,[_fv]);});return _eN(_fu,function(_fz){return E(_fy);});},_fA=unCStr("BEL"),_fB=[0,7],_fC=function(_fD){var _fE=new T(function(){return A(_fD,[_fB]);});return _eN(_fA,function(_fF){return E(_fE);});},_fG=unCStr("BS"),_fH=[0,8],_fI=function(_fJ){var _fK=new T(function(){return A(_fJ,[_fH]);});return _eN(_fG,function(_fL){return E(_fK);});},_fM=unCStr("HT"),_fN=[0,9],_fO=function(_fP){var _fQ=new T(function(){return A(_fP,[_fN]);});return _eN(_fM,function(_fR){return E(_fQ);});},_fS=unCStr("LF"),_fT=[0,10],_fU=function(_fV){var _fW=new T(function(){return A(_fV,[_fT]);});return _eN(_fS,function(_fX){return E(_fW);});},_fY=unCStr("VT"),_fZ=[0,11],_g0=function(_g1){var _g2=new T(function(){return A(_g1,[_fZ]);});return _eN(_fY,function(_g3){return E(_g2);});},_g4=unCStr("FF"),_g5=[0,12],_g6=function(_g7){var _g8=new T(function(){return A(_g7,[_g5]);});return _eN(_g4,function(_g9){return E(_g8);});},_ga=unCStr("CR"),_gb=[0,13],_gc=function(_gd){var _ge=new T(function(){return A(_gd,[_gb]);});return _eN(_ga,function(_gf){return E(_ge);});},_gg=unCStr("SI"),_gh=[0,15],_gi=function(_gj){var _gk=new T(function(){return A(_gj,[_gh]);});return _eN(_gg,function(_gl){return E(_gk);});},_gm=unCStr("DLE"),_gn=[0,16],_go=function(_gp){var _gq=new T(function(){return A(_gp,[_gn]);});return _eN(_gm,function(_gr){return E(_gq);});},_gs=unCStr("DC1"),_gt=[0,17],_gu=function(_gv){var _gw=new T(function(){return A(_gv,[_gt]);});return _eN(_gs,function(_gx){return E(_gw);});},_gy=unCStr("DC2"),_gz=[0,18],_gA=function(_gB){var _gC=new T(function(){return A(_gB,[_gz]);});return _eN(_gy,function(_gD){return E(_gC);});},_gE=unCStr("DC3"),_gF=[0,19],_gG=function(_gH){var _gI=new T(function(){return A(_gH,[_gF]);});return _eN(_gE,function(_gJ){return E(_gI);});},_gK=unCStr("DC4"),_gL=[0,20],_gM=function(_gN){var _gO=new T(function(){return A(_gN,[_gL]);});return _eN(_gK,function(_gP){return E(_gO);});},_gQ=unCStr("NAK"),_gR=[0,21],_gS=function(_gT){var _gU=new T(function(){return A(_gT,[_gR]);});return _eN(_gQ,function(_gV){return E(_gU);});},_gW=unCStr("SYN"),_gX=[0,22],_gY=function(_gZ){var _h0=new T(function(){return A(_gZ,[_gX]);});return _eN(_gW,function(_h1){return E(_h0);});},_h2=unCStr("ETB"),_h3=[0,23],_h4=function(_h5){var _h6=new T(function(){return A(_h5,[_h3]);});return _eN(_h2,function(_h7){return E(_h6);});},_h8=unCStr("CAN"),_h9=[0,24],_ha=function(_hb){var _hc=new T(function(){return A(_hb,[_h9]);});return _eN(_h8,function(_hd){return E(_hc);});},_he=unCStr("EM"),_hf=[0,25],_hg=function(_hh){var _hi=new T(function(){return A(_hh,[_hf]);});return _eN(_he,function(_hj){return E(_hi);});},_hk=unCStr("SUB"),_hl=[0,26],_hm=function(_hn){var _ho=new T(function(){return A(_hn,[_hl]);});return _eN(_hk,function(_hp){return E(_ho);});},_hq=unCStr("ESC"),_hr=[0,27],_hs=function(_ht){var _hu=new T(function(){return A(_ht,[_hr]);});return _eN(_hq,function(_hv){return E(_hu);});},_hw=unCStr("FS"),_hx=[0,28],_hy=function(_hz){var _hA=new T(function(){return A(_hz,[_hx]);});return _eN(_hw,function(_hB){return E(_hA);});},_hC=unCStr("GS"),_hD=[0,29],_hE=function(_hF){var _hG=new T(function(){return A(_hF,[_hD]);});return _eN(_hC,function(_hH){return E(_hG);});},_hI=unCStr("RS"),_hJ=[0,30],_hK=function(_hL){var _hM=new T(function(){return A(_hL,[_hJ]);});return _eN(_hI,function(_hN){return E(_hM);});},_hO=unCStr("US"),_hP=[0,31],_hQ=function(_hR){var _hS=new T(function(){return A(_hR,[_hP]);});return _eN(_hO,function(_hT){return E(_hS);});},_hU=unCStr("SP"),_hV=[0,32],_hW=function(_hX){var _hY=new T(function(){return A(_hX,[_hV]);});return _eN(_hU,function(_hZ){return E(_hY);});},_i0=unCStr("DEL"),_i1=[0,127],_i2=function(_i3){var _i4=new T(function(){return A(_i3,[_i1]);});return _eN(_i0,function(_i5){return E(_i4);});},_i6=[1,_i2,_g],_i7=[1,_hW,_i6],_i8=[1,_hQ,_i7],_i9=[1,_hK,_i8],_ia=[1,_hE,_i9],_ib=[1,_hy,_ia],_ic=[1,_hs,_ib],_id=[1,_hm,_ic],_ie=[1,_hg,_id],_if=[1,_ha,_ie],_ig=[1,_h4,_if],_ih=[1,_gY,_ig],_ii=[1,_gS,_ih],_ij=[1,_gM,_ii],_ik=[1,_gG,_ij],_il=[1,_gA,_ik],_im=[1,_gu,_il],_in=[1,_go,_im],_io=[1,_gi,_in],_ip=[1,_gc,_io],_iq=[1,_g6,_ip],_ir=[1,_g0,_iq],_is=[1,_fU,_ir],_it=[1,_fO,_is],_iu=[1,_fI,_it],_iv=[1,_fC,_iu],_iw=[1,_fw,_iv],_ix=[1,_fq,_iw],_iy=[1,_fk,_ix],_iz=[1,_fe,_iy],_iA=[1,_f8,_iz],_iB=[1,_f2,_iA],_iC=unCStr("SOH"),_iD=[0,1],_iE=function(_iF){var _iG=new T(function(){return A(_iF,[_iD]);});return _eN(_iC,function(_iH){return E(_iG);});},_iI=unCStr("SO"),_iJ=[0,14],_iK=function(_iL){var _iM=new T(function(){return A(_iL,[_iJ]);});return _eN(_iI,function(_iN){return E(_iM);});},_iO=function(_iP){return _bX(_iE,_iK,_iP);},_iQ=[1,_iO,_iB],_iR=new T(function(){return _eB(_iQ);}),_iS=[0,1114111],_iT=[0,34],_iU=[0,_iT,_ec],_iV=[0,39],_iW=[0,_iV,_ec],_iX=[0,92],_iY=[0,_iX,_ec],_iZ=[0,_fB,_ec],_j0=[0,_fH,_ec],_j1=[0,_g5,_ec],_j2=[0,_fT,_ec],_j3=[0,_gb,_ec],_j4=[0,_fN,_ec],_j5=[0,_fZ,_ec],_j6=[0,_f1,_ec],_j7=[0,_iD,_ec],_j8=[0,_f7,_ec],_j9=[0,_fd,_ec],_ja=[0,_fj,_ec],_jb=[0,_fp,_ec],_jc=[0,_fv,_ec],_jd=[0,_fB,_ec],_je=[0,_fH,_ec],_jf=[0,_fN,_ec],_jg=[0,_fT,_ec],_jh=[0,_fZ,_ec],_ji=[0,_g5,_ec],_jj=[0,_gb,_ec],_jk=[0,_iJ,_ec],_jl=[0,_gh,_ec],_jm=[0,_gn,_ec],_jn=[0,_gt,_ec],_jo=[0,_gz,_ec],_jp=[0,_gF,_ec],_jq=[0,_gL,_ec],_jr=[0,_gR,_ec],_js=[0,_gX,_ec],_jt=[0,_h3,_ec],_ju=[0,_h9,_ec],_jv=[0,_hf,_ec],_jw=[0,_hl,_ec],_jx=[0,_hr,_ec],_jy=[0,_hx,_ec],_jz=[0,_hD,_ec],_jA=[0,_hJ,_ec],_jB=[0,_hP,_ec],_jC=function(_jD){return [0,_jD];},_jE=function(_jF){var _jG=new T(function(){return A(_jF,[_j5]);}),_jH=new T(function(){return A(_jF,[_j4]);}),_jI=new T(function(){return A(_jF,[_j3]);}),_jJ=new T(function(){return A(_jF,[_j2]);}),_jK=new T(function(){return A(_jF,[_j1]);}),_jL=new T(function(){return A(_jF,[_j0]);}),_jM=new T(function(){return A(_jF,[_iZ]);}),_jN=new T(function(){return A(_jF,[_iY]);}),_jO=new T(function(){return A(_jF,[_iW]);}),_jP=new T(function(){return A(_jF,[_iU]);});return _aO([0,function(_jQ){switch(E(E(_jQ)[1])){case 34:return E(_jP);case 39:return E(_jO);case 92:return E(_jN);case 97:return E(_jM);case 98:return E(_jL);case 102:return E(_jK);case 110:return E(_jJ);case 114:return E(_jI);case 116:return E(_jH);case 118:return E(_jG);default:return [2];}}],new T(function(){return _aO(_bX(_ed,_ek,function(_jR){var _jS=new T(function(){return _jC(E(_jR)[1]);});return _cm(_jR,function(_jT){var _jU=_dm(_jS,_dd,_jT);return !_er(_jU,_iS)?[2]:A(_jF,[[0,new T(function(){var _jV=_eo(_jU);return _jV>>>0>1114111?_em(_jV):[0,_jV];}),_ec]]);});}),new T(function(){var _jW=new T(function(){return A(_jF,[_jB]);}),_jX=new T(function(){return A(_jF,[_jA]);}),_jY=new T(function(){return A(_jF,[_jz]);}),_jZ=new T(function(){return A(_jF,[_jy]);}),_k0=new T(function(){return A(_jF,[_jx]);}),_k1=new T(function(){return A(_jF,[_jw]);}),_k2=new T(function(){return A(_jF,[_jv]);}),_k3=new T(function(){return A(_jF,[_ju]);}),_k4=new T(function(){return A(_jF,[_jt]);}),_k5=new T(function(){return A(_jF,[_js]);}),_k6=new T(function(){return A(_jF,[_jr]);}),_k7=new T(function(){return A(_jF,[_jq]);}),_k8=new T(function(){return A(_jF,[_jp]);}),_k9=new T(function(){return A(_jF,[_jo]);}),_ka=new T(function(){return A(_jF,[_jn]);}),_kb=new T(function(){return A(_jF,[_jm]);}),_kc=new T(function(){return A(_jF,[_jl]);}),_kd=new T(function(){return A(_jF,[_jk]);}),_ke=new T(function(){return A(_jF,[_jj]);}),_kf=new T(function(){return A(_jF,[_ji]);}),_kg=new T(function(){return A(_jF,[_jh]);}),_kh=new T(function(){return A(_jF,[_jg]);}),_ki=new T(function(){return A(_jF,[_jf]);}),_kj=new T(function(){return A(_jF,[_je]);}),_kk=new T(function(){return A(_jF,[_jd]);}),_kl=new T(function(){return A(_jF,[_jc]);}),_km=new T(function(){return A(_jF,[_jb]);}),_kn=new T(function(){return A(_jF,[_ja]);}),_ko=new T(function(){return A(_jF,[_j9]);}),_kp=new T(function(){return A(_jF,[_j8]);}),_kq=new T(function(){return A(_jF,[_j7]);}),_kr=new T(function(){return A(_jF,[_j6]);});return _aO([0,function(_ks){return E(E(_ks)[1])==94?E([0,function(_kt){switch(E(E(_kt)[1])){case 64:return E(_kr);case 65:return E(_kq);case 66:return E(_kp);case 67:return E(_ko);case 68:return E(_kn);case 69:return E(_km);case 70:return E(_kl);case 71:return E(_kk);case 72:return E(_kj);case 73:return E(_ki);case 74:return E(_kh);case 75:return E(_kg);case 76:return E(_kf);case 77:return E(_ke);case 78:return E(_kd);case 79:return E(_kc);case 80:return E(_kb);case 81:return E(_ka);case 82:return E(_k9);case 83:return E(_k8);case 84:return E(_k7);case 85:return E(_k6);case 86:return E(_k5);case 87:return E(_k4);case 88:return E(_k3);case 89:return E(_k2);case 90:return E(_k1);case 91:return E(_k0);case 92:return E(_jZ);case 93:return E(_jY);case 94:return E(_jX);case 95:return E(_jW);default:return [2];}}]):[2];}],new T(function(){return A(_iR,[function(_ku){return A(_jF,[[0,_ku,_ec]]);}]);}));}));}));},_kv=function(_kw){return A(_kw,[_A]);},_kx=function(_ky){var _kz=E(_ky);if(!_kz[0]){return E(_kv);}else{var _kA=_kz[2],_kB=E(E(_kz[1])[1]);switch(_kB){case 9:var _kC=new T(function(){return _kx(_kA);});return function(_kD){var _kE=new T(function(){return A(_kC,[_kD]);});return [0,function(_kF){return E(_kE);}];};case 10:var _kG=new T(function(){return _kx(_kA);});return function(_kH){var _kI=new T(function(){return A(_kG,[_kH]);});return [0,function(_kJ){return E(_kI);}];};case 11:var _kK=new T(function(){return _kx(_kA);});return function(_kL){var _kM=new T(function(){return A(_kK,[_kL]);});return [0,function(_kN){return E(_kM);}];};case 12:var _kO=new T(function(){return _kx(_kA);});return function(_kP){var _kQ=new T(function(){return A(_kO,[_kP]);});return [0,function(_kR){return E(_kQ);}];};case 13:var _kS=new T(function(){return _kx(_kA);});return function(_kT){var _kU=new T(function(){return A(_kS,[_kT]);});return [0,function(_kV){return E(_kU);}];};case 32:var _kW=new T(function(){return _kx(_kA);});return function(_kX){var _kY=new T(function(){return A(_kW,[_kX]);});return [0,function(_kZ){return E(_kY);}];};case 160:var _l0=new T(function(){return _kx(_kA);});return function(_l1){var _l2=new T(function(){return A(_l0,[_l1]);});return [0,function(_l3){return E(_l2);}];};default:var _l4=u_iswspace(_kB);if(!E(_l4)){return E(_kv);}else{var _l5=new T(function(){return _kx(_kA);});return function(_l6){var _l7=new T(function(){return A(_l5,[_l6]);});return [0,function(_l8){return E(_l7);}];};}}}},_l9=function(_la){var _lb=new T(function(){return _jE(_la);}),_lc=new T(function(){return _l9(_la);}),_ld=[1,function(_le){return A(_kx,[_le,function(_lf){return E([0,function(_lg){return E(E(_lg)[1])==92?E(_lc):[2];}]);}]);}];return _aO([0,function(_lh){return E(E(_lh)[1])==92?E([0,function(_li){var _lj=E(E(_li)[1]);switch(_lj){case 9:return E(_ld);case 10:return E(_ld);case 11:return E(_ld);case 12:return E(_ld);case 13:return E(_ld);case 32:return E(_ld);case 38:return E(_lc);case 160:return E(_ld);default:var _lk=u_iswspace(_lj);return E(_lk)==0?[2]:E(_ld);}}]):[2];}],[0,function(_ll){var _lm=E(_ll);return E(_lm[1])==92?E(_lb):A(_la,[[0,_lm,_0]]);}]);},_ln=function(_lo,_lp){var _lq=new T(function(){return A(_lp,[[1,new T(function(){return A(_lo,[_g]);})]]);});return _l9(function(_lr){var _ls=E(_lr),_lt=E(_ls[1]);return E(_lt[1])==34?!E(_ls[2])?E(_lq):_ln(function(_lu){return A(_lo,[[1,_lt,_lu]]);},_lp):_ln(function(_lv){return A(_lo,[[1,_lt,_lv]]);},_lp);});},_lw=unCStr("_\'"),_lx=function(_ly){var _lz=u_iswalnum(_ly);return E(_lz)==0?_dQ(_bl,[0,_ly],_lw):true;},_lA=function(_lB){return _lx(E(_lB)[1]);},_lC=unCStr(",;()[]{}`"),_lD=function(_lE){return A(_lE,[_g]);},_lF=function(_lG,_lH){var _lI=function(_lJ){var _lK=E(_lJ);if(!_lK[0]){return E(_lD);}else{var _lL=_lK[1];if(!A(_lG,[_lL])){return E(_lD);}else{var _lM=new T(function(){return _lI(_lK[2]);});return function(_lN){var _lO=new T(function(){return A(_lM,[function(_lP){return A(_lN,[[1,_lL,_lP]]);}]);});return [0,function(_lQ){return E(_lO);}];};}}};return [1,function(_lR){return A(_lI,[_lR,_lH]);}];},_lS=unCStr(".."),_lT=unCStr("::"),_lU=unCStr("->"),_lV=[0,64],_lW=[1,_lV,_g],_lX=[0,126],_lY=[1,_lX,_g],_lZ=unCStr("=>"),_m0=[1,_lZ,_g],_m1=[1,_lY,_m0],_m2=[1,_lW,_m1],_m3=[1,_lU,_m2],_m4=unCStr("<-"),_m5=[1,_m4,_m3],_m6=[0,124],_m7=[1,_m6,_g],_m8=[1,_m7,_m5],_m9=[1,_iX,_g],_ma=[1,_m9,_m8],_mb=[0,61],_mc=[1,_mb,_g],_md=[1,_mc,_ma],_me=[1,_lT,_md],_mf=[1,_lS,_me],_mg=function(_mh){var _mi=new T(function(){return A(_mh,[_cj]);});return _aO([1,function(_mj){return E(_mj)[0]==0?E(_mi):[2];}],new T(function(){var _mk=new T(function(){return _jE(function(_ml){var _mm=E(_ml);return (function(_mn,_mo){var _mp=new T(function(){return A(_mh,[[0,_mn]]);});return !E(_mo)?E(E(_mn)[1])==39?[2]:[0,function(_mq){return E(E(_mq)[1])==39?E(_mp):[2];}]:[0,function(_mr){return E(E(_mr)[1])==39?E(_mp):[2];}];})(_mm[1],_mm[2]);});});return _aO([0,function(_ms){return E(E(_ms)[1])==39?E([0,function(_mt){var _mu=E(_mt);switch(E(_mu[1])){case 39:return [2];case 92:return E(_mk);default:var _mv=new T(function(){return A(_mh,[[0,_mu]]);});return [0,function(_mw){return E(E(_mw)[1])==39?E(_mv):[2];}];}}]):[2];}],new T(function(){var _mx=new T(function(){return _ln(_H,_mh);});return _aO([0,function(_my){return E(E(_my)[1])==34?E(_mx):[2];}],new T(function(){return _aO([0,function(_mz){return !_dQ(_bl,_mz,_lC)?[2]:A(_mh,[[2,[1,_mz,_g]]]);}],new T(function(){return _aO([0,function(_mA){return !_dQ(_bl,_mA,_dV)?[2]:_lF(_dW,function(_mB){var _mC=[1,_mA,_mB];return !_dQ(_bC,_mC,_mf)?A(_mh,[[4,_mC]]):A(_mh,[[2,_mC]]);});}],new T(function(){return _aO([0,function(_mD){var _mE=E(_mD),_mF=_mE[1],_mG=u_iswalpha(_mF);return E(_mG)==0?E(_mF)==95?_lF(_lA,function(_mH){return A(_mh,[[3,[1,_mE,_mH]]]);}):[2]:_lF(_lA,function(_mI){return A(_mh,[[3,[1,_mE,_mI]]]);});}],new T(function(){return _bX(_e0,_dL,_mh);}));}));}));}));}));}));},_mJ=function(_mK){var _mL=new T(function(){return _mg(_mK);});return [1,function(_mM){return A(_kx,[_mM,function(_mN){return E(_mL);}]);}];},_mO=[0,0],_mP=function(_mQ,_mR){var _mS=new T(function(){return A(_mQ,[_mO,function(_mT){var _mU=new T(function(){return A(_mR,[_mT]);});return _mJ(function(_mV){var _mW=E(_mV);if(_mW[0]==2){var _mX=E(_mW[1]);return _mX[0]==0?[2]:E(E(_mX[1])[1])==41?E(_mX[2])[0]==0?E(_mU):[2]:[2];}else{return [2];}});}]);});return _mJ(function(_mY){var _mZ=E(_mY);if(_mZ[0]==2){var _n0=E(_mZ[1]);return _n0[0]==0?[2]:E(E(_n0[1])[1])==40?E(_n0[2])[0]==0?E(_mS):[2]:[2];}else{return [2];}});},_n1=function(_n2,_n3,_n4){var _n5=function(_n6,_n7){var _n8=new T(function(){return _mg(function(_n9){return A(_n2,[_n9,_n6,function(_na){return A(_n7,[new T(function(){return [0, -E(_na)[1]];})]);}]);});});return _aO(_mJ(function(_nb){var _nc=E(_nb);if(_nc[0]==4){var _nd=E(_nc[1]);return _nd[0]==0?A(_n2,[_nc,_n6,_n7]):E(E(_nd[1])[1])==45?E(_nd[2])[0]==0?E([1,function(_ne){return A(_kx,[_ne,function(_nf){return E(_n8);}]);}]):A(_n2,[_nc,_n6,_n7]):A(_n2,[_nc,_n6,_n7]);}else{return A(_n2,[_nc,_n6,_n7]);}}),new T(function(){return _mP(_n5,_n7);}));};return _n5(_n3,_n4);},_ng=function(_nh,_ni){return [2];},_nj=function(_nk,_nl){return _ng(_nk,_nl);},_nm=function(_nn){var _no=E(_nn);return _no[0]==0?[1,new T(function(){return _dm(new T(function(){return _jC(E(_no[1])[1]);}),_dd,_no[2]);})]:E(_no[2])[0]==0?E(_no[3])[0]==0?[1,new T(function(){return _dm(_dc,_dd,_no[1]);})]:[0]:[0];},_np=function(_nq){var _nr=E(_nq);if(_nr[0]==5){var _ns=_nm(_nr[1]);if(!_ns[0]){return E(_ng);}else{var _nt=new T(function(){return [0,_eo(_ns[1])];});return function(_nu,_nv){return A(_nv,[_nt]);};}}else{return E(_nj);}},_nw=function(_nk,_nl){return _n1(_np,_nk,_nl);},_nx=function(_ny,_nz){var _nA=function(_nB,_nC){var _nD=new T(function(){return A(_nC,[_g]);}),_nE=new T(function(){return A(_ny,[_mO,function(_nF){return _nA(_ec,function(_nG){return A(_nC,[[1,_nF,_nG]]);});}]);});return _mJ(function(_nH){var _nI=E(_nH);if(_nI[0]==2){var _nJ=E(_nI[1]);if(!_nJ[0]){return [2];}else{var _nK=_nJ[2];switch(E(E(_nJ[1])[1])){case 44:return E(_nK)[0]==0?!E(_nB)?[2]:E(_nE):[2];case 93:return E(_nK)[0]==0?E(_nD):[2];default:return [2];}}}else{return [2];}});},_nL=function(_nM){var _nN=new T(function(){return _aO(_nA(_0,_nM),new T(function(){return A(_ny,[_mO,function(_nO){return _nA(_ec,function(_nP){return A(_nM,[[1,_nO,_nP]]);});}]);}));});return _aO(_mJ(function(_nQ){var _nR=E(_nQ);if(_nR[0]==2){var _nS=E(_nR[1]);return _nS[0]==0?[2]:E(E(_nS[1])[1])==91?E(_nS[2])[0]==0?E(_nN):[2]:[2];}else{return [2];}}),new T(function(){return _mP(function(_nT,_nU){return _nL(_nU);},_nM);}));};return _nL(_nz);},_nV=function(_nW,_nX){return _nx(_nw,_nX);},_nY=new T(function(){return _nx(_nw,_bP);}),_nZ=function(_nl){return _aE(_nY,_nl);},_o0=function(_o1){var _o2=new T(function(){return _n1(_np,_o1,_bP);});return function(_ci){return _aE(_o2,_ci);};},_o3=[0,_o0,_nZ,_nw,_nV],_o4=function(_o5,_o6){return _3F(0,E(_o5)[1],_o6);},_o7=function(_o8,_o9){return _2w(_o4,_o8,_o9);},_oa=function(_ob,_oc,_od){return _3F(E(_ob)[1],E(_oc)[1],_od);},_oe=[0,_oa,_58,_o7],_of=unCStr("GHC.Types"),_og=unCStr("Int"),_oh=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5O,_of,_og],_oi=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oh,_g],_oj=function(_ok){return E(_oi);},_ol=function(_om){return E(E(_om)[1]);},_on=function(_oo){return E(E(_oo)[2]);},_op=function(_oq,_or){var _os=new T(function(){return A(_on,[_oq,_or]);}),_ot=new T(function(){return _ol(_oq);}),_ou=new T(function(){return _3q(_ot);}),_ov=new T(function(){return _30(_ot);});return function(_ow){return A(_ov,[_os,function(_ox){return A(_ou,[[0,_ox,_ow]]);}]);};},_oy=function(_oz,_oA){return A(_oz,[function(_){return jsFind(toJSStr(E(_oA)));}]);},_oB=[0],_oC=function(_oD){return E(E(_oD)[3]);},_oE=new T(function(){return E(_6H);}),_oF=new T(function(){return [0,"value"];}),_oG=function(_oH){return E(E(_oH)[6]);},_oI=unCStr("[]"),_oJ=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5O,_of,_oI],_oK=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oJ,_g],_oL=function(_oM){return E(_oK);},_oN=unCStr("Char"),_oO=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5O,_of,_oN],_oP=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oO,_g],_oQ=function(_oR){return E(_oP);},_oS=new T(function(){return _6I(_oL,_oQ);}),_oT=new T(function(){return A(_oS,[_6H]);}),_oU=function(_oV){return E(E(_oV)[1]);},_oW=[0,0],_oX=[0,32],_oY=[0,10],_oZ=function(_p0){var _p1=E(_p0);if(!_p1[0]){return E(_H);}else{var _p2=_p1[1],_p3=E(_p1[2]);if(!_p3[0]){return _p4(_oY,_p2);}else{var _p5=new T(function(){return _oZ(_p3);}),_p6=new T(function(){return _p4(_oY,_p2);});return function(_p7){return A(_p6,[[1,_oX,new T(function(){return A(_p5,[_p7]);})]]);};}}},_p8=unCStr("->"),_p9=[1,_p8,_g],_pa=[1,_of,_p9],_pb=[1,_5O,_pa],_pc=[0,32],_pd=function(_pe){var _pf=E(_pe);if(!_pf[0]){return [0];}else{var _pg=_pf[1],_ph=E(_pf[2]);return _ph[0]==0?E(_pg):_1G(_pg,[1,_pc,new T(function(){return _pd(_ph);})]);}},_pi=new T(function(){return _pd(_pb);}),_pj=new T(function(){var _pk=_6c(_pi);return [0,_pk[1],_pk[2],_5O,_of,_p8];}),_pl=function(_pm,_pn){var _po=E(_pm);return _po[0]==0?E(_pn):A(_po[1],[new T(function(){return _pl(_po[2],_pn);})]);},_pp=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pq=[1,_5Q,_g],_pr=function(_ps){var _pt=E(_ps);if(!_pt[0]){return [0];}else{var _pu=E(_pt[1]);return [1,[0,_pu[1],_pu[2]],new T(function(){return _pr(_pt[2]);})];}},_pv=new T(function(){var _pw=_1G(_g,_pq);if(!_pw[0]){return E(_oJ);}else{var _px=_6c(new T(function(){return _60(_6o(_6z,[1,_pp,new T(function(){return _pr(_pw);})]));}));return E(_oJ);}}),_py=[0,40],_pz=function(_pA){return _p4(_oY,_pA);},_pB=[0,8],_pC=unCStr(" -> "),_pD=[0,9],_pE=[0,93],_pF=[0,91],_pG=[0,41],_pH=[0,44],_pI=function(_pA){return [1,_pH,_pA];},_pJ=function(_pK,_pL){var _pM=E(_pL);return _pM[0]==0?[0]:[1,_pK,[1,_pM[1],new T(function(){return _pJ(_pK,_pM[2]);})]];},_p4=function(_pN,_pO){var _pP=E(_pO),_pQ=_pP[3],_pR=E(_pP[4]);if(!_pR[0]){return function(_pS){return _1G(E(_pQ)[5],_pS);};}else{var _pT=_pR[1],_pU=new T(function(){var _pV=E(_pQ)[5],_pW=new T(function(){return _oZ(_pR);}),_pX=new T(function(){return E(_pN)[1]<=9?function(_pY){return _1G(_pV,[1,_oX,new T(function(){return A(_pW,[_pY]);})]);}:function(_pZ){return [1,_3E,new T(function(){return _1G(_pV,[1,_oX,new T(function(){return A(_pW,[[1,_3D,_pZ]]);})]);})];};}),_q0=E(_pV);if(!_q0[0]){return E(_pX);}else{if(E(E(_q0[1])[1])==40){var _q1=E(_q0[2]);return _q1[0]==0?E(_pX):E(E(_q1[1])[1])==44?function(_q2){return [1,_py,new T(function(){return A(new T(function(){var _q3=_6o(_pz,_pR);if(!_q3[0]){return E(_H);}else{var _q4=new T(function(){return _pJ(_pI,_q3[2]);});return function(_ci){return _pl([1,_q3[1],_q4],_ci);};}}),[[1,_pG,_q2]]);})];}:E(_pX);}else{return E(_pX);}}}),_q5=E(_pR[2]);if(!_q5[0]){var _q6=E(_pQ),_q7=E(_pv),_q8=hs_eqWord64(_q6[1],_q7[1]);if(!E(_q8)){return E(_pU);}else{var _q9=hs_eqWord64(_q6[2],_q7[2]);if(!E(_q9)){return E(_pU);}else{var _qa=new T(function(){return _p4(_oW,_pT);});return function(_qb){return [1,_pF,new T(function(){return A(_qa,[[1,_pE,_qb]]);})];};}}}else{if(!E(_q5[2])[0]){var _qc=E(_pQ),_qd=E(_pj),_qe=hs_eqWord64(_qc[1],_qd[1]);if(!E(_qe)){return E(_pU);}else{var _qf=hs_eqWord64(_qc[2],_qd[2]);if(!E(_qf)){return E(_pU);}else{var _qg=new T(function(){return _p4(_pB,_q5[1]);}),_qh=new T(function(){return _p4(_pD,_pT);});return E(_pN)[1]<=8?function(_qi){return A(_qh,[new T(function(){return _1G(_pC,new T(function(){return A(_qg,[_qi]);}));})]);}:function(_qj){return [1,_3E,new T(function(){return A(_qh,[new T(function(){return _1G(_pC,new T(function(){return A(_qg,[[1,_3D,_qj]]);}));})]);})];};}}}else{return E(_pU);}}}},_qk=function(_ql,_qm,_qn,_qo,_qp,_qq){var _qr=E(_ql),_qs=_qr[1],_qt=_qr[3],_qu=new T(function(){return A(_qt,[_oB]);}),_qv=new T(function(){return _oC(_qp);}),_qw=new T(function(){return _oG(_qp);}),_qx=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_p4,[_oW,A(_qn,[_oE]),_g]);}));}),_qy=new T(function(){return A(_oU,[_qo,_8]);});return A(_qs,[new T(function(){return _oy(_qm,_qq);}),function(_qz){var _qA=E(_qz);return _qA[0]==0?E(_qu):A(_qs,[new T(function(){return A(_qm,[function(_){var _qB=jsGet(E(_qA[1])[1],E(_oF)[1]);return [1,new T(function(){return fromJSStr(_qB);})];}]);}),function(_qC){var _qD=E(_qC);if(!_qD[0]){return E(_qu);}else{var _qE=_qD[1];if(!E(new T(function(){var _qF=A(_qn,[_oE]),_qG=E(_oT),_qH=hs_eqWord64(_qF[1],_qG[1]);if(!E(_qH)){return false;}else{var _qI=hs_eqWord64(_qF[2],_qG[2]);return E(_qI)==0?false:true;}}))){var _qJ=new T(function(){return A(_qt,[[1,_qE,new T(function(){return A(_qw,[new T(function(){return A(_qv,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1G(_qE,_qx);}));})]);})]);})]]);}),_qK=A(_qy,[_qE]);if(!_qK[0]){return E(_qJ);}else{var _qL=E(_qK[1]);return E(_qL[2])[0]==0?E(_qK[2])[0]==0?A(_qt,[[2,_qL[1]]]):E(_qJ):E(_qJ);}}else{return A(_qt,[[2,_qE]]);}}}]);}]);},_qM=1,_qN=function(_qO){return E(E(_qO)[9]);},_qP=function(_qQ,_qR){return A(_3q,[_qQ,[0,_qR,_qR]]);},_qS=function(_qT,_qU,_qV){return A(_3q,[_qT,[0,_A,_qU]]);},_qW=function(_qX){return E(E(_qX)[2]);},_qY=function(_qZ,_r0,_r1,_r2,_r3){var _r4=new T(function(){return _92(_qZ);}),_r5=new T(function(){return _94(_r4);}),_r6=new T(function(){return _ol(_r0);}),_r7=new T(function(){return _3s(_r6);}),_r8=new T(function(){return _3K([0,coercionToken],_r7,function(_r9){return _qP(_r6,_r9);},function(_ra,_rb){return _qS(_r6,_ra,_rb);});}),_rc=new T(function(){return _3q(_r6);}),_rd=new T(function(){return _30(_r6);}),_re=new T(function(){return _3q(_r6);}),_rf=new T(function(){return _30(_r6);}),_rg=new T(function(){return _3q(_r6);}),_rh=new T(function(){return _30(_r6);}),_ri=new T(function(){return _3q(_r6);}),_rj=new T(function(){return _30(_r6);}),_rk=new T(function(){return _qW(_r2);}),_rl=new T(function(){return _qN(_qZ);});return function(_rm,_rn,_ro){return function(_rp){return A(_rj,[new T(function(){var _rq=E(_rm);return _rq[0]==0?A(_r8,[_rp]):A(_ri,[[0,_rq[1],_rp]]);}),function(_rr){var _rs=new T(function(){return E(E(_rr)[1]);}),_rt=new T(function(){return _qk(_r7,function(_ru){return _op(_r0,_ru);},_r1,_r3,_qZ,_rs);}),_rv=new T(function(){return A(_rl,[_rs,_rn,new T(function(){var _rw=E(_ro);if(!_rw[0]){return [0];}else{var _rx=_rw[1],_ry=_1q(_r1,_oS,_rx);return _ry[0]==0?A(_rk,[_rx]):E(_ry[1]);}}),_0,_9]);});return A(_rh,[new T(function(){var _rz=new T(function(){return E(E(_rr)[2]);});return A(_rg,[[0,_rz,_rz]]);}),function(_rA){return A(_rf,[new T(function(){return A(_re,[[0,_A,new T(function(){var _rB=E(E(_rA)[1]);return [0,_rB[1],_rB[2],_qM,_rB[4],_rB[5],_rB[6]];})]]);}),function(_rC){return A(_rd,[new T(function(){return A(_rt,[new T(function(){return E(E(_rC)[2]);})]);}),function(_rD){var _rE=E(_rD),_rF=_rE[2],_rG=E(_rE[1]);switch(_rG[0]){case 0:return A(_rc,[[0,[0,_rv,_9],_rF]]);case 1:return A(_rc,[[0,[0,new T(function(){return A(_r5,[new T(function(){return A(_rl,[_rs,_rn,_rG[1],_0,_9]);}),_rG[2]]);}),_9],_rF]]);default:var _rH=_rG[1];return A(_rc,[[0,[0,new T(function(){return A(_rl,[_rs,_rn,new T(function(){var _rI=_1q(_r1,_oS,_rH);return _rI[0]==0?A(_rk,[_rH]):E(_rI[1]);}),_0,_9]);}),[1,_rH]],_rF]]);}}]);}]);}]);}]);};};},_rJ=new T(function(){return _qY(_8F,_9L,_oj,_oe,_o3);}),_rK=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_rL=unCStr("keydown"),_rM=unCStr("mousemove"),_rN=unCStr("blur"),_rO=unCStr("focus"),_rP=unCStr("change"),_rQ=unCStr("unload"),_rR=unCStr("load"),_rS=unCStr("keyup"),_rT=unCStr("keypress"),_rU=unCStr("mouseup"),_rV=unCStr("mousedown"),_rW=unCStr("dblclick"),_rX=unCStr("click"),_rY=unCStr("mouseout"),_rZ=unCStr("mouseover"),_s0=function(_s1){switch(E(_s1)[0]){case 0:return E(_rR);case 1:return E(_rQ);case 2:return E(_rP);case 3:return E(_rO);case 4:return E(_rN);case 5:return E(_rM);case 6:return E(_rZ);case 7:return E(_rY);case 8:return E(_rX);case 9:return E(_rW);case 10:return E(_rV);case 11:return E(_rU);case 12:return E(_rT);case 13:return E(_rS);default:return E(_rL);}},_s2=[0],_s3=unCStr("OnLoad"),_s4=[0,_s3,_s2],_s5=function(_){var _=0,_s6=newMVar(),_=putMVar(_s6,_s4);return [0,_s6];},_s7=new T(function(){return _2(_s5);}),_s8=function(_s9,_sa,_){var _sb=A(_s9,[_]);return die(_sa);},_sc=function(_sd,_se,_sf,_){return _s8(function(_){var _=putMVar(_se,_sd);return _A;},_sf,_);},_sg=function(_sh,_){var _si=0;if(!E(_si)){return (function(_){var _sj=E(_s7)[1],_sk=takeMVar(_sj),_sl=jsCatch(function(_){return (function(_){return _sh;})();},function(_X,_){return _sc(_sk,_sj,_X,_);}),_=putMVar(_sj,_sl);return _A;})();}else{var _sm=E(_s7)[1],_sn=takeMVar(_sm),_so=jsCatch(function(_){return _sh;},function(_X,_){return _sc(_sn,_sm,_X,_);}),_=putMVar(_sm,_so);return _A;}},_sp=unCStr("true"),_sq=function(_sr,_ss){while(1){var _st=E(_sr);if(!_st[0]){return E(_ss)[0]==0?true:false;}else{var _su=E(_ss);if(!_su[0]){return false;}else{if(E(_st[1])[1]!=E(_su[1])[1]){return false;}else{_sr=_st[2];_ss=_su[2];continue;}}}}},_sv=new T(function(){return [0,"keydown"];}),_sw=new T(function(){return [0,"mousemove"];}),_sx=new T(function(){return [0,"blur"];}),_sy=new T(function(){return [0,"focus"];}),_sz=new T(function(){return [0,"change"];}),_sA=new T(function(){return [0,"unload"];}),_sB=new T(function(){return [0,"load"];}),_sC=new T(function(){return [0,"keyup"];}),_sD=new T(function(){return [0,"keypress"];}),_sE=new T(function(){return [0,"mouseup"];}),_sF=new T(function(){return [0,"mousedown"];}),_sG=new T(function(){return [0,"dblclick"];}),_sH=new T(function(){return [0,"click"];}),_sI=new T(function(){return [0,"mouseout"];}),_sJ=new T(function(){return [0,"mouseover"];}),_sK=function(_sL){switch(E(_sL)[0]){case 0:return E(_sB);case 1:return E(_sA);case 2:return E(_sz);case 3:return E(_sy);case 4:return E(_sx);case 5:return E(_sw);case 6:return E(_sJ);case 7:return E(_sI);case 8:return E(_sH);case 9:return E(_sG);case 10:return E(_sF);case 11:return E(_sE);case 12:return E(_sD);case 13:return E(_sC);default:return E(_sv);}},_sM=function(_sN,_sO,_sP){var _sQ=new T(function(){return _s0(_sO);}),_sR=new T(function(){return _sK(_sO);});return function(_sS,_){var _sT=A(_sN,[_sS,_]),_sU=E(_sT),_sV=_sU[1],_sW=E(_sQ),_sX=jsGetAttr(_sV,toJSStr(_sW));if(!_sq(fromJSStr(_sX),_sp)){var _sY=E(_sP),_sZ=jsSetCB(_sV,E(_sR)[1],E([0,_sP])[1]),_t0=A(_B,[_H,_sU,_sW,_sp,_]);return _sU;}else{return _sU;}};},_t1=function(_t2,_t3){var _t4=new T(function(){return _s0(_t3);}),_t5=[0,_t4,_s2];return function(_t6,_){var _t7=E(_t6),_t8=E(_t7[4]),_t9=_t8[1],_ta=_t8[2],_tb=A(_t2,[_t7,_]),_tc=E(_tb),_td=E(_tc[1]),_te=_td[1];return [0,[0,new T(function(){var _tf=E(_t3);switch(_tf[0]){case 0:return _sM(_te,_tf,function(_){var _tg=_sg(_t5,_),_th=A(_t9,[_]),_ti=E(_th);if(!_ti[0]){return _A;}else{var _tj=A(_ta,[_ti[1],_]);return _A;}});case 1:return _sM(_te,_tf,function(_){var _tk=_sg(_t5,_),_tl=A(_t9,[_]),_tm=E(_tl);if(!_tm[0]){return _A;}else{var _tn=A(_ta,[_tm[1],_]);return _A;}});case 2:return _sM(_te,_tf,function(_){var _to=_sg(_t5,_),_tp=A(_t9,[_]),_tq=E(_tp);if(!_tq[0]){return _A;}else{var _tr=A(_ta,[_tq[1],_]);return _A;}});case 3:return _sM(_te,_tf,function(_){var _ts=_sg(_t5,_),_tt=A(_t9,[_]),_tu=E(_tt);if(!_tu[0]){return _A;}else{var _tv=A(_ta,[_tu[1],_]);return _A;}});case 4:return _sM(_te,_tf,function(_){var _tw=_sg(_t5,_),_tx=A(_t9,[_]),_ty=E(_tx);if(!_ty[0]){return _A;}else{var _tz=A(_ta,[_ty[1],_]);return _A;}});case 5:return _sM(_te,_tf,function(_tA,_){var _tB=_sg([0,_t4,[2,E(_tA)]],_),_tC=A(_t9,[_]),_tD=E(_tC);if(!_tD[0]){return _A;}else{var _tE=A(_ta,[_tD[1],_]);return _A;}});case 6:return _sM(_te,_tf,function(_tF,_){var _tG=_sg([0,_t4,[2,E(_tF)]],_),_tH=A(_t9,[_]),_tI=E(_tH);if(!_tI[0]){return _A;}else{var _tJ=A(_ta,[_tI[1],_]);return _A;}});case 7:return _sM(_te,_tf,function(_){var _tK=A(_t9,[_]),_tL=E(_tK);if(!_tL[0]){return _A;}else{var _tM=A(_ta,[_tL[1],_]);return _A;}});case 8:return _sM(_te,_tf,function(_tN,_tO,_){var _tP=_sg([0,_t4,[1,_tN,E(_tO)]],_),_tQ=A(_t9,[_]),_tR=E(_tQ);if(!_tR[0]){return _A;}else{var _tS=A(_ta,[_tR[1],_]);return _A;}});case 9:return _sM(_te,_tf,function(_tT,_tU,_){var _tV=_sg([0,_t4,[1,_tT,E(_tU)]],_),_tW=A(_t9,[_]),_tX=E(_tW);if(!_tX[0]){return _A;}else{var _tY=A(_ta,[_tX[1],_]);return _A;}});case 10:return _sM(_te,_tf,function(_tZ,_u0,_){var _u1=_sg([0,_t4,[1,_tZ,E(_u0)]],_),_u2=A(_t9,[_]),_u3=E(_u2);if(!_u3[0]){return _A;}else{var _u4=A(_ta,[_u3[1],_]);return _A;}});case 11:return _sM(_te,_tf,function(_u5,_u6,_){var _u7=_sg([0,_t4,[1,_u5,E(_u6)]],_),_u8=A(_t9,[_]),_u9=E(_u8);if(!_u9[0]){return _A;}else{var _ua=A(_ta,[_u9[1],_]);return _A;}});case 12:return _sM(_te,_tf,function(_ub,_){var _uc=_sg([0,_t4,[3,_ub]],_),_ud=A(_t9,[_]),_ue=E(_ud);if(!_ue[0]){return _A;}else{var _uf=A(_ta,[_ue[1],_]);return _A;}});case 13:return _sM(_te,_tf,function(_ug,_){var _uh=_sg([0,_t4,[3,_ug]],_),_ui=A(_t9,[_]),_uj=E(_ui);if(!_uj[0]){return _A;}else{var _uk=A(_ta,[_uj[1],_]);return _A;}});default:return _sM(_te,_tf,function(_ul,_){var _um=_sg([0,_t4,[3,_ul]],_),_un=A(_t9,[_]),_uo=E(_un);if(!_uo[0]){return _A;}else{var _up=A(_ta,[_uo[1],_]);return _A;}});}}),_td[2]],_tc[2]];};},_uq=new T(function(){return _t1(_rK,_9J);}),_ur=function(_us,_){var _ut=A(_uq,[_us,_]),_uu=E(_ut),_uv=E(_uu[1]);return [0,[0,function(_uw,_){var _ux=A(_uv[1],[_uw,_]),_uy=_5m(_uw,_);return _uw;},_uv[2]],_uu[2]];},_uz=new T(function(){return [1,_ur,_uz];}),_uA=function(_uB,_uC){var _uD=E(_uB);if(!_uD){return [0];}else{var _uE=E(_uC);return _uE[0]==0?[0]:[1,_uE[1],new T(function(){return _uA(_uD-1|0,_uE[2]);})];}},_uF=function(_uG,_uH){return _uG<0?[0]:_uA(_uG,_uH);},_uI=function(_uJ,_uK){var _uL=E(_uJ)[1];return _uL>0?_uF(_uL,_uK):[0];},_uM=function(_uN){return E(_uN);},_uO=function(_uP){var _uQ=new T(function(){return _9C(_5E,_uI(_uP,_uz));}),_uR=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1G(_3F(0,E(_uP)[1],_g),_5A);}));}));});return function(_uS,_){var _uT=_4f(_uQ,_5s,_uS,_),_uU=E(_uT),_uV=E(_uU[1]),_uW=new T(function(){return _4Z(_uM,_uV[1]);});return [0,[0,function(_uX,_){var _uY=A(_uR,[_uX,_]),_uZ=A(_uW,[_uX,_]);return _uX;},_uV[2]],_uU[2]];};},_v0=new T(function(){return _uO(_4R);}),_v1=unCStr("center"),_v2=function(_v3,_v4){var _v5=new T(function(){return A(_v3,[_v4]);});return function(_v6,_){var _v7=jsCreateElem(toJSStr(E(_v1))),_v8=jsAppendChild(_v7,E(_v6)[1]),_v9=[0,_v7],_va=A(_v5,[_v9,_]);return _v9;};},_vb=function(_vc,_){return _vc;},_vd=unCStr("Two counters. One is pure and recursive, the other is stateful"),_ve=new T(function(){return _4Z(_4S,_vd);}),_vf=[8,coercionToken],_vg=function(_vh){return _aO(_mJ(function(_vi){var _vj=E(_vi);return _vj[0]==0?A(_vh,[_vj[1]]):[2];}),new T(function(){return _mP(_vk,_vh);}));},_vk=function(_vl,_vm){return _vg(_vm);},_vn=function(_vo){return _aO(_aO(_mJ(function(_vp){var _vq=E(_vp);return _vq[0]==1?A(_vo,[_vq[1]]):[2];}),new T(function(){return _nx(_vk,_vo);})),new T(function(){return _mP(_vr,_vo);}));},_vr=function(_vs,_vt){return _vn(_vt);},_vu=new T(function(){return _mP(_vr,_bP);}),_vv=new T(function(){return _nx(_vk,_bP);}),_vw=function(_vx){var _vy=E(_vx);return _vy[0]==1?[3,_vy[1],_bO]:[2];},_vz=new T(function(){return _mg(_vw);}),_vA=function(_vB){return E(_vz);},_vC=function(_vD){return A(_kx,[_vD,_vA]);},_vE=[1,_vC],_vF=new T(function(){return _aO(_vE,_vv);}),_vG=new T(function(){return _aO(_vF,_vu);}),_vH=function(_nl){return _aE(_vG,_nl);},_vI=new T(function(){return _vg(_bP);}),_vJ=function(_nl){return _aE(_vI,_nl);},_vK=function(_vL){return E(_vJ);},_vM=[0,_vK,_vH,_vk,_vr],_vN=function(_vO){return E(E(_vO)[4]);},_vP=function(_vQ,_vR,_vS){return _nx(new T(function(){return _vN(_vQ);}),_vS);},_vT=function(_vU){var _vV=new T(function(){return _nx(new T(function(){return _vN(_vU);}),_bP);});return function(_ci){return _aE(_vV,_ci);};},_vW=function(_vX,_vY){var _vZ=new T(function(){return A(_vN,[_vX,_vY,_bP]);});return function(_ci){return _aE(_vZ,_ci);};},_w0=function(_w1){return [0,function(_nl){return _vW(_w1,_nl);},new T(function(){return _vT(_w1);}),new T(function(){return _vN(_w1);}),function(_nk,_nl){return _vP(_w1,_nk,_nl);}];},_w2=new T(function(){return _w0(_vM);}),_w3=new T(function(){return E(_w2);}),_w4=unCStr("Prelude.(!!): negative index\n"),_w5=new T(function(){return err(_w4);}),_w6=unCStr("Prelude.(!!): index too large\n"),_w7=new T(function(){return err(_w6);}),_w8=function(_w9,_wa){while(1){var _wb=E(_w9);if(!_wb[0]){return E(_w7);}else{var _wc=E(_wa);if(!_wc){return E(_wb[1]);}else{_w9=_wb[2];_wa=_wc-1|0;continue;}}}},_wd=unCStr("ACK"),_we=unCStr("BEL"),_wf=unCStr("BS"),_wg=unCStr("SP"),_wh=[1,_wg,_g],_wi=unCStr("US"),_wj=[1,_wi,_wh],_wk=unCStr("RS"),_wl=[1,_wk,_wj],_wm=unCStr("GS"),_wn=[1,_wm,_wl],_wo=unCStr("FS"),_wp=[1,_wo,_wn],_wq=unCStr("ESC"),_wr=[1,_wq,_wp],_ws=unCStr("SUB"),_wt=[1,_ws,_wr],_wu=unCStr("EM"),_wv=[1,_wu,_wt],_ww=unCStr("CAN"),_wx=[1,_ww,_wv],_wy=unCStr("ETB"),_wz=[1,_wy,_wx],_wA=unCStr("SYN"),_wB=[1,_wA,_wz],_wC=unCStr("NAK"),_wD=[1,_wC,_wB],_wE=unCStr("DC4"),_wF=[1,_wE,_wD],_wG=unCStr("DC3"),_wH=[1,_wG,_wF],_wI=unCStr("DC2"),_wJ=[1,_wI,_wH],_wK=unCStr("DC1"),_wL=[1,_wK,_wJ],_wM=unCStr("DLE"),_wN=[1,_wM,_wL],_wO=unCStr("SI"),_wP=[1,_wO,_wN],_wQ=unCStr("SO"),_wR=[1,_wQ,_wP],_wS=unCStr("CR"),_wT=[1,_wS,_wR],_wU=unCStr("FF"),_wV=[1,_wU,_wT],_wW=unCStr("VT"),_wX=[1,_wW,_wV],_wY=unCStr("LF"),_wZ=[1,_wY,_wX],_x0=unCStr("HT"),_x1=[1,_x0,_wZ],_x2=[1,_wf,_x1],_x3=[1,_we,_x2],_x4=[1,_wd,_x3],_x5=unCStr("ENQ"),_x6=[1,_x5,_x4],_x7=unCStr("EOT"),_x8=[1,_x7,_x6],_x9=unCStr("ETX"),_xa=[1,_x9,_x8],_xb=unCStr("STX"),_xc=[1,_xb,_xa],_xd=unCStr("SOH"),_xe=[1,_xd,_xc],_xf=unCStr("NUL"),_xg=[1,_xf,_xe],_xh=[0,92],_xi=unCStr("\\DEL"),_xj=unCStr("\\a"),_xk=unCStr("\\\\"),_xl=unCStr("\\SO"),_xm=unCStr("\\r"),_xn=unCStr("\\f"),_xo=unCStr("\\v"),_xp=unCStr("\\n"),_xq=unCStr("\\t"),_xr=unCStr("\\b"),_xs=function(_xt,_xu){if(_xt<=127){var _xv=E(_xt);switch(_xv){case 92:return _1G(_xk,_xu);case 127:return _1G(_xi,_xu);default:if(_xv<32){var _xw=E(_xv);switch(_xw){case 7:return _1G(_xj,_xu);case 8:return _1G(_xr,_xu);case 9:return _1G(_xq,_xu);case 10:return _1G(_xp,_xu);case 11:return _1G(_xo,_xu);case 12:return _1G(_xn,_xu);case 13:return _1G(_xm,_xu);case 14:return _1G(_xl,new T(function(){var _xx=E(_xu);return _xx[0]==0?[0]:E(E(_xx[1])[1])==72?unAppCStr("\\&",_xx):E(_xx);}));default:return _1G([1,_xh,new T(function(){var _xy=_xw;return _xy>=0?_w8(_xg,_xy):E(_w5);})],_xu);}}else{return [1,[0,_xv],_xu];}}}else{return [1,_xh,new T(function(){var _xz=jsShowI(_xt);return _1G(fromJSStr(_xz),new T(function(){var _xA=E(_xu);if(!_xA[0]){return [0];}else{var _xB=E(_xA[1])[1];return _xB<48?E(_xA):_xB>57?E(_xA):unAppCStr("\\&",_xA);}}));})];}},_xC=[0,39],_xD=[1,_xC,_g],_xE=unCStr("\'\\\'\'"),_xF=function(_xG){var _xH=E(E(_xG)[1]);return _xH==39?E(_xE):[1,_xC,new T(function(){return _xs(_xH,_xD);})];},_xI=[0,34],_xJ=unCStr("\\\""),_xK=function(_xL,_xM){var _xN=E(_xL);if(!_xN[0]){return E(_xM);}else{var _xO=_xN[2],_xP=E(E(_xN[1])[1]);return _xP==34?_1G(_xJ,new T(function(){return _xK(_xO,_xM);})):_xs(_xP,new T(function(){return _xK(_xO,_xM);}));}},_xQ=function(_xR,_xS){return [1,_xI,new T(function(){return _xK(_xR,[1,_xI,_xS]);})];},_xT=function(_xU){return _1G(_xE,_xU);},_xV=function(_xW,_xX){var _xY=E(E(_xX)[1]);return _xY==39?E(_xT):function(_xZ){return [1,_xC,new T(function(){return _xs(_xY,[1,_xC,_xZ]);})];};},_y0=[0,_xV,_xF,_xQ],_y1=function(_y2){return E(E(_y2)[3]);},_y3=function(_y4,_y5){return A(_y1,[_y4,_y5,_g]);},_y6=function(_y7,_y8,_y9){return _2w(new T(function(){return _y1(_y7);}),_y8,_y9);},_ya=function(_yb){var _yc=new T(function(){return _y1(_yb);});return [0,function(_yd){return E(_yc);},function(_xU){return _y3(_yb,_xU);},function(_ye,_xU){return _y6(_yb,_ye,_xU);}];},_yf=new T(function(){return _ya(_y0);}),_yg=unCStr("submit"),_yh=new T(function(){return A(_qY,[_8F,_9L,_oS,_yf,_w3,_9,_yg]);}),_yi=[0,43],_yj=[1,_yi,_g],_yk=[1,_yj],_yl=new T(function(){return A(_yh,[_yk]);}),_ym=new T(function(){return _t1(_yl,_vf);}),_yn=function(_yo,_yp,_yq,_){var _yr=A(_yp,[_yq,_]),_ys=E(_yr),_yt=E(_ys[1]);return [0,[0,function(_yu,_){var _yv=_3Y(_3X,_yu,_),_yw=A(_B,[_H,_yv,_z,_yo,_]),_yx=A(_yt[1],[_yv,_]);return _yv;},_yt[2]],_ys[2]];},_yy=new T(function(){return _3K(_13,_3x,_11,_Y);}),_yz=new T(function(){return _3K(_13,_3x,_11,_Y);}),_yA=function(_yB,_yC,_yD,_){var _yE=A(_yz,[_yD,_]),_yF=A(_yy,[new T(function(){return E(E(_yE)[2]);}),_]),_yG=new T(function(){return E(E(_yE)[1]);});return _4f(function(_X,_){return _yn(_yG,_yB,_X,_);},function(_yH){var _yI=new T(function(){return A(_yC,[_yH]);});return function(_yJ,_){var _yK=A(_yI,[_yJ,_]),_yL=E(_yK),_yM=E(_yL[1]);return [0,[0,function(_yN,_){var _yO=E(_yG),_yP=jsFind(toJSStr(_yO)),_yQ=E(_yP);if(!_yQ[0]){return _45(_yO);}else{var _yR=E(_yQ[1]),_yS=A(_7,[E(_yR[1]),_]),_yT=jsKillChild(E(_yR)[1],_yS),_yU=A(_yM[1],[_yN,_]);return _yN;}},_yM[2]],_yL[2]];};},new T(function(){return E(E(_yF)[2]);}),_);},_yV=function(_yW){var _yX=new T(function(){return _yV(new T(function(){return [0,E(_yW)[1]+1|0];}));}),_yY=new T(function(){return _5c(_4S,new T(function(){return _58(_yW);}));});return function(_ci,_yZ){return _yA(function(_z0,_){var _z1=A(_ym,[_z0,_]),_z2=E(_z1),_z3=E(_z2[1]);return [0,[0,function(_z4,_){var _z5=A(_yY,[_z4,_]),_z6=A(_z3[1],[_z4,_]);return _z4;},_z3[2]],_z2[2]];},function(_z7){return E(_yX);},_ci,_yZ);};},_z8=unCStr("main"),_z9=unCStr("Main"),_za=unCStr("Counter"),_zb=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_z8,_z9,_za],_zc=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_zb,_g],_zd=function(_ze,_zf){var _zg=hs_leWord64(_ze,_zf);return E(_zg)==0?false:true;},_zh=function(_zi,_zj,_zk,_zl){var _zm=hs_eqWord64(_zi,_zk);if(!E(_zm)){var _zn=hs_leWord64(_zi,_zk);return E(_zn)==0?false:true;}else{return _zd(_zj,_zl);}},_zo=function(_zp,_zq){var _zr=E(_zp),_zs=_zr[1],_zt=_zr[2],_zu=E(_zq),_zv=_zu[1],_zw=_zu[2],_zx=hs_eqWord64(_zs,_zv);if(!E(_zx)){return !_zh(_zs,_zt,_zv,_zw)?2:0;}else{var _zy=hs_eqWord64(_zt,_zw);return E(_zy)==0?!_zh(_zs,_zt,_zv,_zw)?2:0:1;}},_zz=unCStr("Failure in Data.Map.balanceL"),_zA=new T(function(){return err(_zz);}),_zB=function(_zC,_zD,_zE,_zF){var _zG=E(_zF);if(!_zG[0]){var _zH=_zG[1],_zI=E(_zE);if(!_zI[0]){var _zJ=_zI[1],_zK=_zI[2],_zL=_zI[3];if(_zJ<=(imul(3,_zH)|0)){return [0,(1+_zJ|0)+_zH|0,E(E(_zC)),_zD,E(_zI),E(_zG)];}else{var _zM=E(_zI[4]);if(!_zM[0]){var _zN=_zM[1],_zO=E(_zI[5]);if(!_zO[0]){var _zP=_zO[1],_zQ=_zO[2],_zR=_zO[3],_zS=_zO[4];if(_zP>=(imul(2,_zN)|0)){var _zT=function(_zU){var _zV=E(_zO[5]);return _zV[0]==0?[0,(1+_zJ|0)+_zH|0,E(_zQ),_zR,E([0,(1+_zN|0)+_zU|0,E(_zK),_zL,E(_zM),E(_zS)]),E([0,(1+_zH|0)+_zV[1]|0,E(E(_zC)),_zD,E(_zV),E(_zG)])]:[0,(1+_zJ|0)+_zH|0,E(_zQ),_zR,E([0,(1+_zN|0)+_zU|0,E(_zK),_zL,E(_zM),E(_zS)]),E([0,1+_zH|0,E(E(_zC)),_zD,E(_f),E(_zG)])];},_zW=E(_zS);return _zW[0]==0?_zT(_zW[1]):_zT(0);}else{return [0,(1+_zJ|0)+_zH|0,E(_zK),_zL,E(_zM),E([0,(1+_zH|0)+_zP|0,E(E(_zC)),_zD,E(_zO),E(_zG)])];}}else{return E(_zA);}}else{return E(_zA);}}}else{return [0,1+_zH|0,E(E(_zC)),_zD,E(_f),E(_zG)];}}else{var _zX=E(_zE);if(!_zX[0]){var _zY=_zX[1],_zZ=_zX[2],_A0=_zX[3],_A1=_zX[5],_A2=E(_zX[4]);if(!_A2[0]){var _A3=_A2[1],_A4=E(_A1);if(!_A4[0]){var _A5=_A4[1],_A6=_A4[2],_A7=_A4[3],_A8=_A4[4];if(_A5>=(imul(2,_A3)|0)){var _A9=function(_Aa){var _Ab=E(_A4[5]);return _Ab[0]==0?[0,1+_zY|0,E(_A6),_A7,E([0,(1+_A3|0)+_Aa|0,E(_zZ),_A0,E(_A2),E(_A8)]),E([0,1+_Ab[1]|0,E(E(_zC)),_zD,E(_Ab),E(_f)])]:[0,1+_zY|0,E(_A6),_A7,E([0,(1+_A3|0)+_Aa|0,E(_zZ),_A0,E(_A2),E(_A8)]),E([0,1,E(E(_zC)),_zD,E(_f),E(_f)])];},_Ac=E(_A8);return _Ac[0]==0?_A9(_Ac[1]):_A9(0);}else{return [0,1+_zY|0,E(_zZ),_A0,E(_A2),E([0,1+_A5|0,E(E(_zC)),_zD,E(_A4),E(_f)])];}}else{return [0,3,E(_zZ),_A0,E(_A2),E([0,1,E(E(_zC)),_zD,E(_f),E(_f)])];}}else{var _Ad=E(_A1);return _Ad[0]==0?[0,3,E(_Ad[2]),_Ad[3],E([0,1,E(_zZ),_A0,E(_f),E(_f)]),E([0,1,E(E(_zC)),_zD,E(_f),E(_f)])]:[0,2,E(E(_zC)),_zD,E(_zX),E(_f)];}}else{return [0,1,E(E(_zC)),_zD,E(_f),E(_f)];}}},_Ae=unCStr("Failure in Data.Map.balanceR"),_Af=new T(function(){return err(_Ae);}),_Ag=function(_Ah,_Ai,_Aj,_Ak){var _Al=E(_Aj);if(!_Al[0]){var _Am=_Al[1],_An=E(_Ak);if(!_An[0]){var _Ao=_An[1],_Ap=_An[2],_Aq=_An[3];if(_Ao<=(imul(3,_Am)|0)){return [0,(1+_Am|0)+_Ao|0,E(E(_Ah)),_Ai,E(_Al),E(_An)];}else{var _Ar=E(_An[4]);if(!_Ar[0]){var _As=_Ar[1],_At=_Ar[2],_Au=_Ar[3],_Av=_Ar[4],_Aw=E(_An[5]);if(!_Aw[0]){var _Ax=_Aw[1];if(_As>=(imul(2,_Ax)|0)){var _Ay=function(_Az){var _AA=E(_Ah),_AB=E(_Ar[5]);return _AB[0]==0?[0,(1+_Am|0)+_Ao|0,E(_At),_Au,E([0,(1+_Am|0)+_Az|0,E(_AA),_Ai,E(_Al),E(_Av)]),E([0,(1+_Ax|0)+_AB[1]|0,E(_Ap),_Aq,E(_AB),E(_Aw)])]:[0,(1+_Am|0)+_Ao|0,E(_At),_Au,E([0,(1+_Am|0)+_Az|0,E(_AA),_Ai,E(_Al),E(_Av)]),E([0,1+_Ax|0,E(_Ap),_Aq,E(_f),E(_Aw)])];},_AC=E(_Av);return _AC[0]==0?_Ay(_AC[1]):_Ay(0);}else{return [0,(1+_Am|0)+_Ao|0,E(_Ap),_Aq,E([0,(1+_Am|0)+_As|0,E(E(_Ah)),_Ai,E(_Al),E(_Ar)]),E(_Aw)];}}else{return E(_Af);}}else{return E(_Af);}}}else{return [0,1+_Am|0,E(E(_Ah)),_Ai,E(_Al),E(_f)];}}else{var _AD=E(_Ak);if(!_AD[0]){var _AE=_AD[1],_AF=_AD[2],_AG=_AD[3],_AH=_AD[5],_AI=E(_AD[4]);if(!_AI[0]){var _AJ=_AI[1],_AK=_AI[2],_AL=_AI[3],_AM=_AI[4],_AN=E(_AH);if(!_AN[0]){var _AO=_AN[1];if(_AJ>=(imul(2,_AO)|0)){var _AP=function(_AQ){var _AR=E(_Ah),_AS=E(_AI[5]);return _AS[0]==0?[0,1+_AE|0,E(_AK),_AL,E([0,1+_AQ|0,E(_AR),_Ai,E(_f),E(_AM)]),E([0,(1+_AO|0)+_AS[1]|0,E(_AF),_AG,E(_AS),E(_AN)])]:[0,1+_AE|0,E(_AK),_AL,E([0,1+_AQ|0,E(_AR),_Ai,E(_f),E(_AM)]),E([0,1+_AO|0,E(_AF),_AG,E(_f),E(_AN)])];},_AT=E(_AM);return _AT[0]==0?_AP(_AT[1]):_AP(0);}else{return [0,1+_AE|0,E(_AF),_AG,E([0,1+_AJ|0,E(E(_Ah)),_Ai,E(_f),E(_AI)]),E(_AN)];}}else{return [0,3,E(_AK),_AL,E([0,1,E(E(_Ah)),_Ai,E(_f),E(_f)]),E([0,1,E(_AF),_AG,E(_f),E(_f)])];}}else{var _AU=E(_AH);return _AU[0]==0?[0,3,E(_AF),_AG,E([0,1,E(E(_Ah)),_Ai,E(_f),E(_f)]),E(_AU)]:[0,2,E(E(_Ah)),_Ai,E(_f),E(_AD)];}}else{return [0,1,E(E(_Ah)),_Ai,E(_f),E(_f)];}}},_AV=function(_AW,_AX,_AY){var _AZ=E(_AW),_B0=E(_AY);if(!_B0[0]){var _B1=_B0[2],_B2=_B0[3],_B3=_B0[4],_B4=_B0[5];switch(_zo(_AZ,_B1)){case 0:return _zB(_B1,_B2,_AV(_AZ,_AX,_B3),_B4);case 1:return [0,_B0[1],E(_AZ),_AX,E(_B3),E(_B4)];default:return _Ag(_B1,_B2,_B3,_AV(_AZ,_AX,_B4));}}else{return [0,1,E(_AZ),_AX,E(_f),E(_f)];}},_B5=[0,_2X,_5q],_B6=function(_B7,_){return [0,[0,_2X,[1,_B7]],_B7];},_B8=[1,_A],_B9=function(_Ba){var _Bb=new T(function(){return [0,E(_Ba)[1]+1|0];}),_Bc=new T(function(){return _5c(_4S,new T(function(){return _58(_Ba);}));});return function(_ci,_yZ){return _4f(function(_Bd,_){return [0,[0,_Bc,_B8],_Bd];},function(_Be,_Bf,_){return (function(_Bf,_){return _4f(_B6,function(_Bg){return function(_Bh,_){return [0,_B5,new T(function(){var _Bi=E(_Bg);return [0,_Bi[1],_Bi[2],_Bi[3],_Bi[4],_Bi[5],new T(function(){return _AV(_zc,_Bb,_Bi[6]);})];})];};},_Bf,_);})(_Bf,_);},_ci,_yZ);};},_Bj=function(_Bk){return E(_zc);},_Bl=function(_Bm,_Bn){while(1){var _Bo=E(_Bm),_Bp=E(_Bn);if(!_Bp[0]){switch(_zo(_Bo,_Bp[2])){case 0:_Bm=_Bo;_Bn=_Bp[4];continue;case 1:return [1,_Bp[3]];default:_Bm=_Bo;_Bn=_Bp[5];continue;}}else{return [0];}}},_Bq=function(_Br,_Bs,_Bt,_Bu){var _Bv=E(_Bs),_Bw=_Bv[1],_Bx=_Bv[3],_By=new T(function(){return A(_Bu,[_oE]);}),_Bz=new T(function(){return A(_Bx,[_9]);});return A(_Bw,[new T(function(){return A(_Bw,[_Bt,function(_BA){return A(_Bx,[new T(function(){var _BB=E(_Br);return E(E(_BA)[6]);})]);}]);}),function(_BC){var _BD=_Bl(_By,_BC);return _BD[0]==0?E(_Bz):A(_Bx,[[1,_BD[1]]]);}]);},_BE=new T(function(){return _Bq(_13,_3x,_11,_Bj);}),_BF=function(_BG){var _BH=new T(function(){return _yV(_BG);});return function(_BI,_){var _BJ=A(_BH,[_BI,_]),_BK=E(_BJ),_BL=E(_BK[1]),_BM=_4f(_ym,function(_BN){return function(_Bf,_){return _4f(function(_BO,_){var _BP=A(_BE,[_BO,_]);return [0,[0,_vb,new T(function(){var _BQ=E(E(_BP)[1]);return _BQ[0]==0?E([1,_BG]):E(_BQ);})],new T(function(){return E(E(_BP)[2]);})];},_B9,_Bf,_);};},_BK[2],_),_BR=E(_BM),_BS=E(_BR[1]),_BT=new T(function(){return _v2(_uM,function(_BU,_){var _BV=A(_BL[1],[_BU,_]),_BW=A(_BS[1],[_BU,_]);return _BU;});});return [0,[0,function(_BX,_){var _BY=A(_ve,[_BX,_]),_BZ=_5m(_BX,_),_C0=A(_BT,[_BX,_]);return _BX;},new T(function(){var _C1=E(_BL[2]);return _C1[0]==0?E(_BS[2]):E(_C1);})],_BR[2]];};},_C2=new T(function(){return _BF(_4R);}),_C3=[0,4],_C4=function(_C5,_C6){return [1,_C6,new T(function(){return _C4(_C5,new T(function(){return A(_C5,[_C6]);}));})];},_C7=[0,1],_C8=[1,_C7,_g],_C9=[1,_5B,_g],_Ca=function(_Cb,_Cc,_Cd){var _Ce=E(_Cc);if(!_Ce[0]){return [0];}else{var _Cf=E(_Cd);return _Cf[0]==0?[0]:[1,new T(function(){return A(_Cb,[_Ce[1],_Cf[1]]);}),new T(function(){return _Ca(_Cb,_Ce[2],_Cf[2]);})];}},_Cg=function(_Ch){return _Ca(_8U,[1,_5B,_Ch],new T(function(){return _1G(_Ch,_C9);}));},_Ci=new T(function(){return _C4(_Cg,_C8);}),_Cj=unCStr(" rows of the Pascal triangle "),_Ck=function(_Cl){var _Cm=new T(function(){return _2w(_o4,_Cl,_g);});return function(_ci,_yZ){return _4S(_Cm,_ci,_yZ);};},_Cn=unCStr("text-align:center"),_Co=unCStr("style"),_Cp=function(_Cq,_Cr){var _Cs=new T(function(){return _4Z(_Ck,_Cq);});return [1,function(_Ct,_){var _Cu=A(_Cs,[_Ct,_]),_Cv=A(_B,[_H,_Cu,_Co,_Cn,_]);return _Cu;},_Cr];},_Cw=function(_Cx,_Cy){var _Cz=E(_Cx);if(!_Cz[0]){return [0];}else{var _CA=_Cz[1];return _Cy>1?_Cp(_CA,new T(function(){return _Cw(_Cz[2],_Cy-1|0);})):_Cp(_CA,_g);}},_CB=function(_CC){var _CD=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("Show ",new T(function(){return _1G(_3F(0,E(_CC)[1],_g),_Cj);}));}));});return function(_CE,_){return [0,[0,function(_CF,_){var _CG=A(_CD,[_CF,_]),_CH=_8s(new T(function(){var _CI=E(_CC)[1];return _CI>0?_Cw(_Ci,_CI):[0];}),_CF,_);return _CF;},_9],_CE];};},_CJ=new T(function(){return _CB(_C3);}),_CK=unCStr("Different input elements:"),_CL=new T(function(){return _4Z(_4S,_CK);}),_CM=unCStr(" returns: "),_CN=[1,_xI,_g],_CO=function(_CP){var _CQ=new T(function(){return _5c(_4S,[1,_xI,new T(function(){return _xK(_CP,_CN);})]);});return function(_CR,_){return [0,[0,function(_CS,_){var _CT=_4S(_CM,_CS,_),_CU=A(_CQ,[_CS,_]);return _CS;},_B8],_CR];};},_CV=unCStr("blue"),_CW=[1,_CV,_g],_CX=unCStr("green"),_CY=[1,_CX,_CW],_CZ=unCStr("red"),_D0=[1,_CZ,_CY],_D1=function(_D2){return E(E(_D2)[15]);},_D3=function(_D4,_D5,_){var _D6=jsGet(_D4,toJSStr(E(_D5)));return new T(function(){return fromJSStr(_D6);});},_D7=function(_D8,_D9,_){return _D3(E(_D8)[1],_D9,_);},_Da=new T(function(){return A(_oS,[_6H]);}),_Db=unCStr("name"),_Dc=unCStr("true"),_Dd=unCStr("radio"),_De=function(_Df,_Dg,_Dh,_Di){var _Dj=new T(function(){return _ol(_Dg);}),_Dk=new T(function(){return _3K([0,coercionToken],_3s(_Dj),function(_Dl){return _qP(_Dj,_Dl);},function(_Dm,_Dn){return _qS(_Dj,_Dm,_Dn);});}),_Do=new T(function(){return _3q(_Dj);}),_Dp=new T(function(){return _3q(_Dj);}),_Dq=new T(function(){return _30(_Dj);}),_Dr=new T(function(){return _30(_Dj);}),_Ds=new T(function(){return _3q(_Dj);}),_Dt=new T(function(){return _30(_Dj);}),_Du=new T(function(){return _3q(_Dj);}),_Dv=new T(function(){return _30(_Dj);}),_Dw=new T(function(){return _qN(_Df);}),_Dx=new T(function(){return _D1(_Df);}),_Dy=new T(function(){return _qW(_Di);});return function(_Dz,_DA){return function(_DB){return A(_Dq,[new T(function(){return A(_Dk,[_DB]);}),function(_DC){var _DD=new T(function(){return E(E(_DC)[1]);}),_DE=new T(function(){return _op(_Dg,function(_){return jsFind(toJSStr(E(_DD)));});});return A(_Dv,[new T(function(){var _DF=new T(function(){return E(E(_DC)[2]);});return A(_Du,[[0,_DF,_DF]]);}),function(_DG){return A(_Dt,[new T(function(){return A(_Ds,[[0,_A,new T(function(){var _DH=E(E(_DG)[1]);return [0,_DH[1],_DH[2],_qM,_DH[4],_DH[5],_DH[6]];})]]);}),function(_DI){return A(_Dr,[new T(function(){return A(_DE,[new T(function(){return E(E(_DI)[2]);})]);}),function(_DJ){return A(_Dq,[new T(function(){var _DK=E(_DJ),_DL=_DK[2],_DM=E(_DK[1]);return _DM[0]==0?A(_Dp,[[0,_g,_DL]]):A(_op,[_Dg,function(_){return _D7(_DM[1],_6V,_);},_DL]);}),function(_DN){var _DO=new T(function(){return !_sq(E(_DN)[1],_Dc)?[0]:E([1,_Dz]);});return A(_Do,[[0,[0,new T(function(){return A(_Dx,[new T(function(){return A(_Dw,[_DD,_Dd,new T(function(){var _DP=A(_Dh,[_Dz]),_DQ=E(_Da),_DR=hs_eqWord64(_DP[1],_DQ[1]);if(!E(_DR)){return A(_Dy,[_Dz]);}else{var _DS=hs_eqWord64(_DP[2],_DQ[2]);return E(_DS)==0?A(_Dy,[_Dz]):E(_Dz);}}),new T(function(){return E(_DO)[0]==0?false:true;}),_9]);}),[1,[0,_Db,_DA],_g]]);}),new T(function(){var _DT=E(_DO);return _DT[0]==0?[0]:[1,_DT[1]];})],new T(function(){return E(E(_DN)[2]);})]]);}]);}]);}]);}]);}]);};};},_DU=new T(function(){return _6I(_oL,_oQ);}),_DV=new T(function(){return _ya(_y0);}),_DW=new T(function(){return _De(_8F,_9L,_DU,_DV);}),_DX=function(_DY){var _DZ=E(_DY);if(!_DZ[0]){return [0];}else{var _E0=_DZ[1];return [1,function(_E1){var _E2=new T(function(){return _t1(new T(function(){return A(_DW,[_E0,_E1]);}),_vf);});return function(_E3,_){var _E4=A(_E2,[_E3,_]),_E5=E(_E4),_E6=E(_E5[1]);return [0,[0,function(_E7,_){var _E8=_4S(_E0,_E7,_),_E9=A(_E6[1],[_E7,_]);return _E7;},_E6[2]],_E5[2]];};},new T(function(){return _DX(_DZ[2]);})];}},_Ea=new T(function(){return _DX(_D0);}),_Eb=function(_Ec){return E(E(_Ec)[1]);},_Ed=function(_Ee,_Ef){var _Eg=new T(function(){return _92(_Ef);}),_Eh=new T(function(){return _Eb(_Eg);}),_Ei=new T(function(){return _94(_Eg);}),_Ej=function(_Ek){var _El=E(_Ek);if(!_El[0]){return [0,_Eh,_9];}else{var _Em=E(_El[1]),_En=_Ej(_El[2]);return [0,new T(function(){return A(_Ei,[_Em[1],_En[1]]);}),new T(function(){var _Eo=E(_Em[2]);return _Eo[0]==0?E(_En[2]):E(_Eo);})];}},_Ep=new T(function(){return _3q(_Ee);}),_Eq=new T(function(){return _3K([0,coercionToken],_3s(_Ee),function(_Er){return _qP(_Ee,_Er);},function(_Es,_Et){return _qS(_Ee,_Es,_Et);});}),_Eu=new T(function(){return _3q(_Ee);}),_Ev=new T(function(){return _30(_Ee);}),_Ew=new T(function(){return _30(_Ee);}),_Ex=new T(function(){return _30(_Ee);}),_Ey=new T(function(){return _30(_Ee);});return function(_Ez,_EA){return A(_Ey,[new T(function(){return A(_Eq,[_EA]);}),function(_EB){return A(_Ex,[new T(function(){var _EC=new T(function(){return E(E(_EB)[1]);}),_ED=function(_EE){var _EF=E(_EE);if(!_EF[0]){return function(_EG){return A(_Eu,[[0,_g,_EG]]);};}else{var _EH=new T(function(){return _ED(_EF[2]);}),_EI=new T(function(){return A(_EF[1],[_EC]);});return function(_EJ){return A(_Ew,[new T(function(){return A(_EI,[_EJ]);}),function(_EK){var _EL=new T(function(){return E(E(_EK)[1]);});return A(_Ev,[new T(function(){return A(_EH,[new T(function(){return E(E(_EK)[2]);})]);}),function(_EM){return A(_Eu,[[0,[1,_EL,new T(function(){return E(E(_EM)[1]);})],new T(function(){return E(E(_EM)[2]);})]]);}]);}]);};}};return A(_ED,[_Ez,new T(function(){return E(E(_EB)[2]);})]);}),function(_EN){var _EO=new T(function(){var _EP=_Ej(E(_EN)[1]);return [0,_EP[1],_EP[2]];});return A(_Ep,[[0,[0,new T(function(){return E(E(_EO)[1]);}),new T(function(){var _EQ=E(E(_EO)[2]);return _EQ[0]==0?[0]:[1,_EQ[1]];})],new T(function(){return E(E(_EN)[2]);})]]);}]);}]);};},_ER=new T(function(){return _Ed(_2Z,_8F);}),_ES=new T(function(){return A(_ER,[_Ea]);}),_ET=function(_EU){var _EV=new T(function(){return _5c(_4S,new T(function(){return _2w(_xQ,_EU,_g);}));});return function(_EW,_){return [0,[0,function(_EX,_){var _EY=_4S(_CM,_EX,_),_EZ=A(_EV,[_EX,_]);return _EX;},_B8],_EW];};},_F0=new T(function(){return _5c(_4S,_CX);}),_F1=unCStr("checkbox"),_F2=function(_F3,_F4){var _F5=new T(function(){return _ol(_F4);}),_F6=new T(function(){return _3K([0,coercionToken],_3s(_F5),function(_F7){return _qP(_F5,_F7);},function(_F8,_F9){return _qS(_F5,_F8,_F9);});}),_Fa=new T(function(){return _3q(_F5);}),_Fb=new T(function(){return _3q(_F5);}),_Fc=new T(function(){return _30(_F5);}),_Fd=new T(function(){return _30(_F5);}),_Fe=new T(function(){return _3q(_F5);}),_Ff=new T(function(){return _30(_F5);}),_Fg=new T(function(){return _3q(_F5);}),_Fh=new T(function(){return _30(_F5);}),_Fi=new T(function(){return _qN(_F3);});return function(_Fj,_Fk){var _Fl=new T(function(){return !E(_Fj)?[0]:E(_Dc);});return function(_Fm){return A(_Fc,[new T(function(){return A(_F6,[_Fm]);}),function(_Fn){var _Fo=new T(function(){return E(E(_Fn)[1]);}),_Fp=new T(function(){return _op(_F4,function(_){return jsFind(toJSStr(E(_Fo)));});}),_Fq=new T(function(){return A(_Fi,[_Fo,_F1,_Fk,_Fj,_9]);});return A(_Fh,[new T(function(){var _Fr=new T(function(){return E(E(_Fn)[2]);});return A(_Fg,[[0,_Fr,_Fr]]);}),function(_Fs){return A(_Ff,[new T(function(){return A(_Fe,[[0,_A,new T(function(){var _Ft=E(E(_Fs)[1]);return [0,_Ft[1],_Ft[2],_qM,_Ft[4],_Ft[5],_Ft[6]];})]]);}),function(_Fu){return A(_Fd,[new T(function(){return A(_Fp,[new T(function(){return E(E(_Fu)[2]);})]);}),function(_Fv){return A(_Fc,[new T(function(){var _Fw=E(_Fv),_Fx=_Fw[2],_Fy=E(_Fw[1]);return _Fy[0]==0?A(_Fb,[[0,_Fl,_Fx]]):A(_op,[_F4,function(_){return _D7(_Fy[1],_6V,_);},_Fx]);}),function(_Fz){return A(_Fa,[[0,[0,_Fq,[1,[0,new T(function(){return !_sq(E(_Fz)[1],_Dc)?[0]:E([1,_Fk,_g]);})]]],new T(function(){return E(E(_Fz)[2]);})]]);}]);}]);}]);}]);}]);};};},_FA=new T(function(){return _F2(_8F,_9L);}),_FB=unCStr("Green"),_FC=new T(function(){return A(_FA,[_0,_FB]);}),_FD=function(_FE,_){var _FF=A(_FC,[_FE,_]),_FG=E(_FF),_FH=E(_FG[1]);return [0,[0,function(_FI,_){var _FJ=A(_FH[1],[_FI,_]),_FK=A(_F0,[_FI,_]);return _FI;},_FH[2]],_FG[2]];},_FL=new T(function(){return _t1(_FD,_vf);}),_FM=new T(function(){return _5c(_4S,_CV);}),_FN=new T(function(){return A(_FA,[_0,_CV]);}),_FO=function(_FP,_){var _FQ=A(_FN,[_FP,_]),_FR=E(_FQ),_FS=E(_FR[1]);return [0,[0,function(_FT,_){var _FU=A(_FS[1],[_FT,_]),_FV=A(_FM,[_FT,_]);return _FT;},_FS[2]],_FR[2]];},_FW=new T(function(){return _t1(_FO,_vf);}),_FX=new T(function(){return _5c(_4S,_CZ);}),_FY=unCStr("Red"),_FZ=new T(function(){return A(_FA,[_0,_FY]);}),_G0=function(_G1,_){var _G2=A(_FZ,[_G1,_]),_G3=E(_G2),_G4=E(_G3[1]);return [0,[0,function(_G5,_){var _G6=A(_G4[1],[_G5,_]),_G7=A(_FX,[_G5,_]);return _G5;},_G4[2]],_G3[2]];},_G8=new T(function(){return _t1(_G0,_vf);}),_G9=function(_Ga,_){var _Gb=A(_G8,[_Ga,_]),_Gc=E(_Gb),_Gd=E(_Gc[1]),_Ge=A(_FL,[_Gc[2],_]),_Gf=E(_Ge),_Gg=E(_Gf[1]),_Gh=A(_FW,[_Gf[2],_]),_Gi=E(_Gh),_Gj=E(_Gi[1]);return [0,[0,function(_Gk,_){var _Gl=A(_Gd[1],[_Gk,_]),_Gm=A(_Gg[1],[_Gk,_]),_Gn=A(_Gj[1],[_Gk,_]);return _Gk;},new T(function(){var _Go=E(_Gd[2]);if(!_Go[0]){return [0];}else{var _Gp=E(_Gg[2]);if(!_Gp[0]){return [0];}else{var _Gq=E(_Gj[2]);return _Gq[0]==0?[0]:[1,new T(function(){var _Gr=function(_Gs){var _Gt=E(_Gs);return _Gt[0]==0?E(new T(function(){var _Gu=function(_Gv){var _Gw=E(_Gv);return _Gw[0]==0?E(E(_Gq[1])[1]):[1,_Gw[1],new T(function(){return _Gu(_Gw[2]);})];};return _Gu(E(_Gp[1])[1]);})):[1,_Gt[1],new T(function(){return _Gr(_Gt[2]);})];};return _Gr(E(_Go[1])[1]);})];}}})],_Gi[2]];},_Gx=function(_Gy){var _Gz=new T(function(){return _5c(_4S,[1,_xI,new T(function(){return _xK(_Gy,_CN);})]);});return function(_GA,_){return [0,[0,function(_GB,_){var _GC=_4S(_CM,_GB,_),_GD=A(_Gz,[_GB,_]);return _GB;},_B8],_GA];};},_GE=new T(function(){return _w0(_vM);}),_GF=function(_GG){return E(E(_GG)[11]);},_GH=function(_GI,_GJ,_GK,_GL){var _GM=new T(function(){return _ol(_GJ);}),_GN=new T(function(){return _3s(_GM);}),_GO=new T(function(){return _3K([0,coercionToken],_GN,function(_GP){return _qP(_GM,_GP);},function(_GQ,_GR){return _qS(_GM,_GQ,_GR);});}),_GS=new T(function(){return _3q(_GM);}),_GT=new T(function(){return _30(_GM);}),_GU=new T(function(){return _30(_GM);}),_GV=new T(function(){return _3q(_GM);}),_GW=new T(function(){return _30(_GM);}),_GX=new T(function(){return _3q(_GM);}),_GY=new T(function(){return _30(_GM);}),_GZ=new T(function(){return _30(_GM);}),_H0=new T(function(){return _GF(_GI);});return function(_H1,_H2){return A(_GZ,[new T(function(){return A(_GO,[_H2]);}),function(_H3){var _H4=new T(function(){return E(E(_H3)[1]);}),_H5=new T(function(){return _qk(_GN,function(_H6){return _op(_GJ,_H6);},_GK,_GL,_GI,_H4);});return A(_GY,[new T(function(){var _H7=new T(function(){return E(E(_H3)[2]);});return A(_GX,[[0,_H7,_H7]]);}),function(_H8){return A(_GW,[new T(function(){return A(_GV,[[0,_A,new T(function(){var _H9=E(E(_H8)[1]);return [0,_H9[1],_H9[2],_qM,_H9[4],_H9[5],_H9[6]];})]]);}),function(_Ha){return A(_GU,[new T(function(){return A(_H5,[new T(function(){return E(E(_Ha)[2]);})]);}),function(_Hb){return A(_GT,[new T(function(){return A(_H1,[new T(function(){return E(E(_Hb)[2]);})]);}),function(_Hc){var _Hd=E(_Hc);return A(_GS,[[0,[0,new T(function(){return A(_H0,[_H4,E(_Hd[1])[1]]);}),new T(function(){var _He=E(E(_Hb)[1]);return _He[0]==2?[1,_He[1]]:[0];})],_Hd[2]]]);}]);}]);}]);}]);}]);};},_Hf=new T(function(){return _GH(_8F,_9L,_DU,_GE);}),_Hg=new T(function(){return _xK(_CV,_CN);}),_Hh=new T(function(){return _xK(_CV,_CN);}),_Hi=new T(function(){return A(_oS,[_6H]);}),_Hj=new T(function(){var _Hk=A(_DU,[_CV]),_Hl=E(_Hi),_Hm=hs_eqWord64(_Hk[1],_Hl[1]);if(!E(_Hm)){return [1,_xI,_Hg];}else{var _Hn=hs_eqWord64(_Hk[2],_Hl[2]);return E(_Hn)==0?[1,_xI,_Hh]:E(_CV);}}),_Ho=[0,_6T,_Hj],_Hp=[1,_Ho,_g],_Hq=new T(function(){return _Q(_7q,_Hp);}),_Hr=new T(function(){return _xK(_CX,_CN);}),_Hs=new T(function(){return _xK(_CX,_CN);}),_Ht=new T(function(){var _Hu=A(_DU,[_CX]),_Hv=E(_Hi),_Hw=hs_eqWord64(_Hu[1],_Hv[1]);if(!E(_Hw)){return [1,_xI,_Hr];}else{var _Hx=hs_eqWord64(_Hu[2],_Hv[2]);return E(_Hx)==0?[1,_xI,_Hs]:E(_CX);}}),_Hy=[0,_6T,_Ht],_Hz=[1,_Hy,_g],_HA=new T(function(){return _Q(_7q,_Hz);}),_HB=new T(function(){return _xK(_CZ,_CN);}),_HC=new T(function(){return _xK(_CZ,_CN);}),_HD=new T(function(){var _HE=A(_DU,[_CZ]),_HF=E(_Hi),_HG=hs_eqWord64(_HE[1],_HF[1]);if(!E(_HG)){return [1,_xI,_HB];}else{var _HH=hs_eqWord64(_HE[2],_HF[2]);return E(_HH)==0?[1,_xI,_HC]:E(_CZ);}}),_HI=[0,_6T,_HD],_HJ=[1,_HI,_g],_HK=new T(function(){return _Q(_7q,_HJ);}),_HL=function(_HM,_){var _HN=A(_HK,[_HM,_]),_HO=_4S(_CZ,_HN,_),_HP=A(_HA,[_HM,_]),_HQ=_4S(_CX,_HP,_),_HR=A(_Hq,[_HM,_]),_HS=_4S(_CV,_HR,_);return _HM;},_HT=[1,_CZ],_HU=[0,_HL,_HT],_HV=function(_HW,_){return [0,_HU,_HW];},_HX=new T(function(){return A(_Hf,[_HV]);}),_HY=new T(function(){return _t1(_HX,_vf);}),_HZ=function(_I0,_){var _I1=_4f(_G9,_ET,_I0,_),_I2=E(_I1),_I3=_4f(_ES,_CO,_I2[2],_),_I4=E(_I3),_I5=_4f(_HY,_Gx,_I4[2],_),_I6=E(_I5),_I7=E(_I6[1]);return [0,[0,function(_I8,_){var _I9=A(_CL,[_I8,_]),_Ia=A(E(_I2[1])[1],[_I8,_]),_Ib=_5m(_I8,_),_Ic=_5m(_I8,_),_Id=A(E(_I4[1])[1],[_I8,_]),_Ie=_5m(_I8,_),_If=_5m(_I8,_),_Ig=A(_I7[1],[_I8,_]),_Ih=_5m(_I8,_);return _I8;},_I7[2]],_I6[2]];},_Ii=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_Ij=new T(function(){return _4Z(_4S,_Ii);}),_Ik=function(_Il){var _Im=jsShow(E(_Il)[1]);return fromJSStr(_Im);},_In=function(_Io){var _Ip=new T(function(){return _Ik(_Io);});return function(_ci){return _1G(_Ip,_ci);};},_Iq=function(_Ir,_Is,_It){var _Iu=E(_It);if(!_Iu[0]){return [0];}else{var _Iv=_Iu[2],_Iw=E(_Iu[1]);return _Ir!=_Iw[1]?[1,_Iw,new T(function(){return _Iq(_Ir,_Is,_Iv);})]:_1G(_Is,new T(function(){return _Iq(_Ir,_Is,_Iv);}));}},_Ix=[0,45],_Iy=function(_Iz,_IA,_IB){var _IC=new T(function(){return A(_Iz,[[0, -_IB]]);}),_ID=new T(function(){return E(_IA)[1]<=6?function(_IE){return [1,_Ix,new T(function(){return A(_IC,[_IE]);})];}:function(_IF){return [1,_3E,[1,_Ix,new T(function(){return A(_IC,[[1,_3D,_IF]]);})]];};});if(_IB>=0){var _IG=isDoubleNegativeZero(_IB);return E(_IG)==0?A(_Iz,[[0,_IB]]):E(_ID);}else{return E(_ID);}},_IH=unCStr("canvas"),_II=unCStr("id"),_IJ=unCStr("canvas"),_IK=function(_IL,_IM){var _IN=new T(function(){return A(_IL,[_IM]);});return function(_IO,_){var _IP=jsCreateElem(toJSStr(E(_IJ))),_IQ=jsAppendChild(_IP,E(_IO)[1]),_IR=[0,_IP],_IS=A(_IN,[_IR,_]);return _IR;};},_IT=new T(function(){return _IK(_uM,_2X);}),_IU=function(_IV,_){var _IW=A(_IT,[_IV,_]),_IX=A(_B,[_H,_IW,_II,_IH,_]);return _IW;},_IY=[0,_IU,_B8],_IZ=function(_J0,_){return [0,_IY,_J0];},_J1=unCStr("Pattern match failure in do expression at src\\Main.hs:180:5-12"),_J2=function(_J3,_J4){while(1){var _J5=E(_J4);if(!_J5[0]){return false;}else{if(!A(_J3,[_J5[1]])){_J4=_J5[2];continue;}else{return true;}}}},_J6=unCStr("x*x+x+10;"),_J7=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_J8=new T(function(){return _5(_J7);}),_J9=function(_Ja,_){var _Jb=jsHasCtx2D(_Ja);if(!E(_Jb)){return _9;}else{var _Jc=jsGetCtx2D(_Ja);return [1,[0,[0,_Jc],[0,_Ja]]];}},_Jd=function(_Je,_){return _J9(E(_Je)[1],_);},_Jf=function(_Jg,_Jh){return A(_Jg,[function(_){var _Ji=jsFind(toJSStr(E(_Jh))),_Jj=E(_Ji);return _Jj[0]==0?_9:_Jd(_Jj[1],_);}]);},_Jk=new T(function(){return _Jf(_H,_IH);}),_Jl=[0,-10],_Jm=[0,0],_Jn=[0,_Jl,_Jm],_Jo=[0,10],_Jp=[0,_Jo,_Jm],_Jq=[1,_Jp,_g],_Jr=[1,_Jn,_Jq],_Js=function(_Jt,_){return _A;},_Ju=function(_Jv){var _Jw=E(_Jv);if(!_Jw[0]){return E(_Js);}else{var _Jx=E(_Jw[1]);return function(_Jy,_){var _Jz=E(_Jy)[1],_JA=jsMoveTo(_Jz,E(_Jx[1])[1],E(_Jx[2])[1]);return (function(_JB,_){while(1){var _JC=E(_JB);if(!_JC[0]){return _A;}else{var _JD=E(_JC[1]),_JE=jsLineTo(_Jz,E(_JD[1])[1],E(_JD[2])[1]);_JB=_JC[2];continue;}}})(_Jw[2],_);};}},_JF=new T(function(){return _Ju(_Jr);}),_JG=[0,30],_JH=[0,_Jm,_JG],_JI=[0,-30],_JJ=[0,_Jm,_JI],_JK=[1,_JJ,_g],_JL=[1,_JH,_JK],_JM=new T(function(){return _Ju(_JL);}),_JN=function(_JO,_JP,_JQ){while(1){var _JR=E(_JP);if(!_JR[0]){return true;}else{var _JS=E(_JQ);if(!_JS[0]){return false;}else{if(!A(_bm,[_JO,_JR[1],_JS[1]])){return false;}else{_JP=_JR[2];_JQ=_JS[2];continue;}}}}},_JT=unCStr("alert"),_JU=function(_JV){return _JN(_bl,_JT,_JV);},_JW=new T(function(){return [0,0/0];}),_JX=new T(function(){return [0,-1/0];}),_JY=new T(function(){return [0,1/0];}),_JZ=[0,0],_K0=function(_K1,_K2){while(1){var _K3=E(_K1);if(!_K3[0]){_K1=[1,I_fromInt(_K3[1])];continue;}else{var _K4=E(_K2);if(!_K4[0]){_K1=_K3;_K2=[1,I_fromInt(_K4[1])];continue;}else{return I_fromRat(_K3[1],_K4[1]);}}}},_K5=function(_K6,_K7){var _K8=E(_K6);if(!_K8[0]){var _K9=_K8[1],_Ka=E(_K7);return _Ka[0]==0?_K9==_Ka[1]:I_compareInt(_Ka[1],_K9)==0?true:false;}else{var _Kb=_K8[1],_Kc=E(_K7);return _Kc[0]==0?I_compareInt(_Kb,_Kc[1])==0?true:false:I_compare(_Kb,_Kc[1])==0?true:false;}},_Kd=function(_Ke,_Kf){var _Kg=E(_Ke);if(!_Kg[0]){var _Kh=_Kg[1],_Ki=E(_Kf);return _Ki[0]==0?_Kh<_Ki[1]:I_compareInt(_Ki[1],_Kh)>0;}else{var _Kj=_Kg[1],_Kk=E(_Kf);return _Kk[0]==0?I_compareInt(_Kj,_Kk[1])<0:I_compare(_Kj,_Kk[1])<0;}},_Kl=function(_Km,_Kn){return !_K5(_Kn,_JZ)?[0,_K0(_Km,_Kn)]:!_K5(_Km,_JZ)?!_Kd(_Km,_JZ)?E(_JY):E(_JX):E(_JW);},_Ko=function(_Kp){var _Kq=E(_Kp);return _Kl(_Kq[1],_Kq[2]);},_Kr=function(_Ks){return [0,1/E(_Ks)[1]];},_Kt=function(_Ku){var _Kv=E(_Ku),_Kw=_Kv[1];return _Kw<0?[0, -_Kw]:E(_Kv);},_Kx=function(_Ky){var _Kz=E(_Ky);return _Kz[0]==0?_Kz[1]:I_toNumber(_Kz[1]);},_KA=function(_KB){return [0,_Kx(_KB)];},_KC=[0,0],_KD=[0,1],_KE=[0,-1],_KF=function(_KG){var _KH=E(_KG)[1];return _KH!=0?_KH<=0?E(_KE):E(_KD):E(_KC);},_KI=function(_KJ,_KK){return [0,E(_KJ)[1]-E(_KK)[1]];},_KL=function(_KM){return [0, -E(_KM)[1]];},_KN=function(_KO,_KP){return [0,E(_KO)[1]+E(_KP)[1]];},_KQ=function(_KR,_KS){return [0,E(_KR)[1]*E(_KS)[1]];},_KT=[0,_KN,_KQ,_KI,_KL,_Kt,_KF,_KA],_KU=function(_KV,_KW){return [0,E(_KV)[1]/E(_KW)[1]];},_KX=[0,_KT,_KU,_Kr,_Ko],_KY=function(_KZ,_L0){return E(_KZ)[1]!=E(_L0)[1]?true:false;},_L1=function(_L2,_L3){return E(_L2)[1]==E(_L3)[1];},_L4=[0,_L1,_KY],_L5=function(_L6,_L7){return E(_L6)[1]<E(_L7)[1];},_L8=function(_L9,_La){return E(_L9)[1]<=E(_La)[1];},_Lb=function(_Lc,_Ld){return E(_Lc)[1]>E(_Ld)[1];},_Le=function(_Lf,_Lg){return E(_Lf)[1]>=E(_Lg)[1];},_Lh=function(_Li,_Lj){var _Lk=E(_Li)[1],_Ll=E(_Lj)[1];return _Lk>=_Ll?_Lk!=_Ll?2:1:0;},_Lm=function(_Ln,_Lo){var _Lp=E(_Ln),_Lq=E(_Lo);return _Lp[1]>_Lq[1]?E(_Lp):E(_Lq);},_Lr=function(_Ls,_Lt){var _Lu=E(_Ls),_Lv=E(_Lt);return _Lu[1]>_Lv[1]?E(_Lv):E(_Lu);},_Lw=[0,_L4,_Lh,_L5,_Le,_Lb,_L8,_Lm,_Lr],_Lx=[0,1],_Ly=function(_Lz){return E(E(_Lz)[1]);},_LA=function(_LB){return E(E(_LB)[2]);},_LC=function(_LD){return E(E(_LD)[6]);},_LE=[0,2],_LF=function(_LG,_LH){var _LI=E(_LH);return [1,_LI,new T(function(){var _LJ=_Ly(_LG);return _LF(_LG,A(_LJ[1],[_LI,new T(function(){return A(_LJ[7],[_Lx]);})]));})];},_LK=function(_LL,_LM){var _LN=E(_LM);if(!_LN[0]){return [0];}else{var _LO=_LN[1];return !A(_LL,[_LO])?[0]:[1,_LO,new T(function(){return _LK(_LL,_LN[2]);})];}},_LP=function(_LQ,_LR,_LS,_LT){var _LU=new T(function(){return _LC(_LQ);});return _LK(function(_LV){return A(_LU,[_LV,new T(function(){var _LW=_Ly(_LR),_LX=_LW[7];return A(_LW[1],[_LT,new T(function(){return A(_LA,[_LR,new T(function(){return A(_LX,[_Lx]);}),new T(function(){return A(_LX,[_LE]);})]);})]);})]);},_LF(_LR,_LS));},_LY=new T(function(){return _LP(_Lw,_KX,_Jl,_Jo);}),_LZ=function(_M0){return [1,_M0,new T(function(){var _M1=E(_M0);return _M1[0]==0?[0]:_LZ(_M1[2]);})];},_M2=function(_M3,_M4){var _M5=E(_M3);if(!_M5[0]){return [0];}else{var _M6=E(_M4);return _M6[0]==0?[0]:[1,[0,_M5[1],_M6[1]],new T(function(){return _M2(_M5[2],_M6[2]);})];}},_M7=function(_M8){var _M9=new T(function(){return !_J2(_JU,_LZ(_M8))?E(_M8):E(_J6);}),_Ma=function(_Mb,_){var _Mc=E(_Mb);if(!_Mc[0]){return _g;}else{var _Md=A(_J8,[E(toJSStr(_Iq(120,new T(function(){return A(_Iy,[_In,_oW,E(_Mc[1])[1],_g]);}),_M9))),_]),_Me=_Ma(_Mc[2],_);return [1,[0,_Md],_Me];}};return function(_ci,_yZ){return _4f(_IZ,function(_Mf,_Bf,_){return (function(_Mg,_){return [0,[0,function(_Mh,_){var _Mi=A(_Jk,[_]),_Mj=E(_Mi);if(!_Mj[0]){var _Mk=_2V(_J1,_);return _Mh;}else{var _Ml=_Ma(_LY,_),_Mm=E(_Mj[1]),_Mn=jsResetCanvas(E(_Mm[2])[1]),_Mo=E(_Mm[1]),_Mp=_Mo[1],_Mq=jsPushState(_Mp),_Mr=jsScale(_Mp,3,1),_Ms=jsPushState(_Mp),_Mt=jsTranslate(_Mp,50,130),_Mu=jsPushState(_Mp),_Mv=jsRotate(_Mp,3.141592653589793),_Mw=jsBeginPath(_Mp),_Mx=A(_JF,[_Mo,_]),_My=A(_JM,[_Mo,_]),_Mz=A(_Ju,[_M2(_LY,_Ml),_Mo,_]),_MA=jsStroke(_Mp),_MB=jsPopState(_Mp),_MC=jsPopState(_Mp),_MD=jsPopState(_Mp);return _Mh;}},_B8],_Mg];})(_Bf,_);},_ci,_yZ);};},_ME=[1,_J6],_MF=new T(function(){return _qY(_8F,_9L,_oS,_yf,_w3);}),_MG=new T(function(){return A(_MF,[_9,_9K,_ME]);}),_MH=new T(function(){return _t1(_MG,_9J);}),_MI=function(_MJ,_){var _MK=A(_MH,[_MJ,_]),_ML=E(_MK),_MM=E(_ML[1]);return [0,[0,function(_MN,_){var _MO=A(_MM[1],[_MN,_]),_MP=_5m(_MN,_);return _MN;},new T(function(){var _MQ=E(_MM[2]);return _MQ[0]==0?E(_ME):E(_MQ);})],_ML[2]];},_MR=function(_MS,_){var _MT=_4f(_MI,_M7,_MS,_),_MU=E(_MT),_MV=E(_MU[1]),_MW=new T(function(){return _v2(_uM,_MV[1]);});return [0,[0,function(_MX,_){var _MY=A(_Ij,[_MX,_]),_MZ=A(_MW,[_MX,_]);return _MX;},_MV[2]],_MU[2]];},_N0=unCStr("work?"),_N1=function(_N2,_N3,_N4){var _N5=E(_N3);return A(_N5[1],[new T(function(){var _N6=E(_N2);return E(_N4);}),function(_N7){return A(_N5[3],[[1,_3y,new T(function(){var _N8=E(_N7);return _1G(_3F(0,E(_N8[2])[1],_g),_N8[1]);})]]);}]);},_N9=function(_Na){return E(E(_Na)[5]);},_Nb=unCStr("for"),_Nc=unCStr("label"),_Nd=function(_Ne,_Nf){var _Ng=new T(function(){return _92(_Nf);}),_Nh=new T(function(){return _94(_Ng);}),_Ni=new T(function(){return _N1([0,coercionToken],_3s(_Ne),function(_Nj){return _qP(_Ne,_Nj);});}),_Nk=new T(function(){return _3q(_Ne);}),_Nl=new T(function(){return _30(_Ne);}),_Nm=new T(function(){return _D1(_Nf);}),_Nn=new T(function(){return _30(_Ne);}),_No=new T(function(){return _N9(_Nf);});return function(_Np,_Nq){var _Nr=new T(function(){return A(_No,[_Nc,_Np]);});return function(_Ns){return A(_Nn,[new T(function(){return A(_Ni,[_Ns]);}),function(_Nt){var _Nu=new T(function(){return A(_Nm,[_Nr,[1,[0,_Nb,new T(function(){return E(E(_Nt)[1]);})],_g]]);});return A(_Nl,[new T(function(){return A(_Nq,[new T(function(){return E(E(_Nt)[2]);})]);}),function(_Nv){var _Nw=E(_Nv),_Nx=E(_Nw[1]);return A(_Nk,[[0,[0,new T(function(){return A(_Nh,[_Nu,_Nx[1]]);}),_Nx[2]],_Nw[2]]]);}]);}]);};};},_Ny=new T(function(){return _Nd(_2Z,_8F);}),_Nz=new T(function(){return _De(_8F,_9L,_DU,_DV);}),_NA=function(_NB,_NC){return A(_Ny,[function(_Bf,_){return _4S(_NB,_Bf,_);},new T(function(){return _t1(new T(function(){return A(_Nz,[_NB,_NC]);}),_vf);})]);},_ND=function(_JV){return _NA(_N0,_JV);},_NE=unCStr("study?"),_NF=function(_JV){return _NA(_NE,_JV);},_NG=[1,_NF,_g],_NH=[1,_ND,_NG],_NI=new T(function(){return A(_ER,[_NH]);}),_NJ=unCStr("Do you "),_NK=new T(function(){return _5c(_4S,_NJ);}),_NL=function(_NM,_){var _NN=A(_NI,[_NM,_]),_NO=E(_NN),_NP=E(_NO[1]);return [0,[0,function(_NQ,_){var _NR=A(_NK,[_NQ,_]),_NS=A(_NP[1],[_NQ,_]),_NT=_5m(_NQ,_);return _NQ;},_NP[2]],_NO[2]];},_NU=unCStr("do you enjoy your work? "),_NV=new T(function(){return _5c(_4S,_NU);}),_NW=function(_NX,_NY,_){return [0,[0,_2X,[1,_NX]],_NY];},_NZ=function(_O0,_O1,_O2,_){return _4f(_O0,function(_O3){return E(_O1);},_O2,_);},_O4=function(_O5,_O6,_X,_){return _NZ(_O5,_O6,_X,_);},_O7=function(_O8){return err(_O8);},_O9=[0,_4f,_O4,_NW,_O7],_Oa=function(_Ob,_Oc,_Od,_Oe,_Of,_Og){var _Oh=new T(function(){return _94(_Ob);});return A(_Oc,[new T(function(){return A(_Oe,[_Og]);}),function(_Oi){var _Oj=E(_Oi),_Ok=E(_Oj[1]);return A(_Oc,[new T(function(){return A(_Of,[_Oj[2]]);}),function(_Ol){var _Om=E(_Ol),_On=E(_Om[1]);return A(_Od,[[0,[0,new T(function(){return A(_Oh,[_Ok[1],_On[1]]);}),new T(function(){var _Oo=E(_Ok[2]);return _Oo[0]==0?E(_On[2]):E(_Oo);})],_Om[2]]]);}]);}]);},_Op=function(_Oq,_Or,_Os,_Ot,_Ou,_Ov){var _Ow=new T(function(){return _D1(_Os);});return A(_Oq,[new T(function(){return A(_Ot,[_Ov]);}),function(_Ox){var _Oy=E(_Ox),_Oz=E(_Oy[1]);return A(_Or,[[0,[0,new T(function(){return A(_Ow,[_Oz[1],_Ou]);}),_Oz[2]],_Oy[2]]]);}]);},_OA=function(_OB){return E(E(_OB)[12]);},_OC=function(_OD,_OE,_OF,_OG,_OH,_OI,_OJ){var _OK=new T(function(){return A(_OA,[_OD,new T(function(){var _OL=A(_OF,[_OH]),_OM=E(_Hi),_ON=hs_eqWord64(_OL[1],_OM[1]);if(!E(_ON)){return A(_qW,[_OG,_OH]);}else{var _OO=hs_eqWord64(_OL[2],_OM[2]);return E(_OO)==0?A(_qW,[_OG,_OH]):E(_OH);}}),_OI,_OJ]);}),_OP=new T(function(){return _3q(_OE);});return function(_OQ){return A(_OP,[[0,[0,_OK,[1,_OH]],_OQ]]);};},_OR=[0,_7s,_Dc],_OS=[1,_OR,_g],_OT=[0,_7s,_Dc],_OU=[1,_OT,_g],_OV=function(_OW,_OX,_OY,_OZ){var _P0=new T(function(){return _GH(_OZ,_OY,_oS,_w2);}),_P1=new T(function(){return A(_3q,[_OW,_0]);}),_P2=new T(function(){return A(_3q,[_OW,_ec]);}),_P3=new T(function(){return _92(_OZ);}),_P4=new T(function(){return _ol(_OY);}),_P5=new T(function(){return _oC(_OZ);}),_P6=new T(function(){return _30(_OW);});return function(_P7,_P8,_P9){return A(_P6,[new T(function(){var _Pa=new T(function(){return !E(_P7)?E(_OU):[0];}),_Pb=new T(function(){return _OC(_OZ,_P4,_oS,_yf,_P9,new T(function(){return A(_P5,[_P9]);}),_0);}),_Pc=new T(function(){return !E(_P7)?[0]:E(_OS);}),_Pd=new T(function(){return _OC(_OZ,_P4,_oS,_yf,_P8,new T(function(){return A(_P5,[_P8]);}),_0);});return A(_P0,[function(_Pe){var _Pf=E(_P4);return _Oa(_P3,_Pf[1],_Pf[3],function(_Pg){var _Ph=E(_P4);return _Op(_Ph[1],_Ph[3],_OZ,_Pd,_Pc,_Pg);},function(_Pi){var _Pj=E(_P4);return _Op(_Pj[1],_Pj[3],_OZ,_Pb,_Pa,_Pi);},_Pe);}]);}),function(_Pk){return !_sq(_Pk,_P8)?E(_P1):E(_P2);}]);};},_Pl=new T(function(){return _OV(_O9,_8O,_9L,_8F);}),_Pm=unCStr("yes"),_Pn=unCStr("no"),_Po=new T(function(){return A(_Pl,[_ec,_Pm,_Pn]);}),_Pp=unCStr("ok"),_Pq=[1,_Pp],_Pr=new T(function(){return A(_yh,[_Pq]);}),_Ps=new T(function(){return _t1(_Pr,_vf);}),_Pt=function(_Pu,_){var _Pv=A(_Po,[_Pu,_]),_Pw=E(_Pv),_Px=E(_Pw[1]),_Py=A(_Ps,[_Pw[2],_]),_Pz=E(_Py);return [0,[0,function(_PA,_){var _PB=A(_NV,[_PA,_]),_PC=A(_Px[1],[_PA,_]),_PD=A(E(_Pz[1])[1],[_PA,_]),_PE=_5m(_PA,_);return _PA;},new T(function(){var _PF=E(_Px[2]);return _PF[0]==0?[0]:[1,[0,_PF[1]]];})],_Pz[2]];},_PG=unCStr("do you study in "),_PH=new T(function(){return _5c(_4S,_PG);}),_PI=unCStr("University"),_PJ=function(_JV){return _NA(_PI,_JV);},_PK=unCStr("High School"),_PL=function(_JV){return _NA(_PK,_JV);},_PM=[1,_PL,_g],_PN=[1,_PJ,_PM],_PO=new T(function(){return A(_ER,[_PN]);}),_PP=function(_PQ,_){var _PR=A(_PO,[_PQ,_]),_PS=E(_PR),_PT=E(_PS[1]);return [0,[0,function(_PU,_){var _PV=A(_PH,[_PU,_]),_PW=A(_PT[1],[_PU,_]);return _PU;},new T(function(){var _PX=E(_PT[2]);return _PX[0]==0?[0]:[1,[1,_PX[1]]];})],_PS[2]];},_PY=new T(function(){return _aB("src\\Main.hs:(283,11)-(290,64)|case");}),_PZ=unCStr(" that you enjoy your work"),_Q0=unCStr("False"),_Q1=new T(function(){return _1G(_Q0,_PZ);}),_Q2=unCStr("True"),_Q3=new T(function(){return _1G(_Q2,_PZ);}),_Q4=[0,32],_Q5=function(_Q6,_Q7){var _Q8=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You are ",new T(function(){return _1G(_Q6,[1,_Q4,_Q7]);}));}));});return function(_ci,_yZ){return _4f(_NL,function(_Q9){var _Qa=new T(function(){return !_sq(_Q9,_NE)?!_sq(_Q9,_N0)?E(_PY):E(_Pt):E(_PP);});return function(_ci,_yZ){return _4f(_Qa,function(_Qb){return function(_Qc,_){var _Qd=A(new T(function(){var _Qe=E(_Qb);if(!_Qe[0]){var _Qf=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_Qe[1])?E(_Q1):E(_Q3);}));}));});return function(_Qg,_){return [0,[0,function(_Qh,_){var _Qi=A(_Qf,[_Qh,_]);return _Qh;},_9],_Qg];};}else{var _Qj=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You study at the ",_Qe[1]);}));});return function(_Qk,_){return [0,[0,function(_Ql,_){var _Qm=A(_Qj,[_Ql,_]);return _Ql;},_9],_Qk];};}}),[_Qc,_]),_Qn=E(_Qd),_Qo=E(_Qn[1]);return [0,[0,function(_Qp,_){var _Qq=A(_Q8,[_Qp,_]),_Qr=A(_Qo[1],[_Qp,_]);return _Qp;},_Qo[2]],_Qn[2]];};},_ci,_yZ);};},_ci,_yZ);};},_Qs=function(_Qt){var _Qu=E(_Qt);return _Q5(_Qu[1],_Qu[2]);},_Qv=unCStr("Who are you? "),_Qw=new T(function(){return _4Z(_4S,_Qv);}),_Qx=unCStr("name"),_Qy=unCStr("placeholder"),_Qz=[0,_Qy,_Qx],_QA=[1,_Qz,_g],_QB=unCStr("surname"),_QC=[0,_Qy,_QB],_QD=[1,_QC,_g],_QE=[1,_Pp],_QF=new T(function(){return A(_yh,[_QE]);}),_QG=new T(function(){return _t1(_QF,_vf);}),_QH=new T(function(){return A(_MF,[_9,_9K,_9]);}),_QI=new T(function(){return A(_MF,[_9,_9K,_9]);}),_QJ=function(_QK,_){var _QL=A(_QI,[_QK,_]),_QM=E(_QL),_QN=E(_QM[1]),_QO=A(_QH,[_QM[2],_]),_QP=E(_QO),_QQ=E(_QP[1]),_QR=A(_QG,[_QP[2],_]),_QS=E(_QR),_QT=new T(function(){return _Q(_QQ[1],_QD);}),_QU=new T(function(){return _Q(_QN[1],_QA);});return [0,[0,function(_QV,_){var _QW=A(_Qw,[_QV,_]),_QX=A(_QU,[_QV,_]),_QY=_5m(_QV,_),_QZ=A(_QT,[_QV,_]),_R0=_5m(_QV,_),_R1=A(E(_QS[1])[1],[_QV,_]),_R2=_5m(_QV,_);return _QV;},new T(function(){var _R3=E(_QN[2]);if(!_R3[0]){return [0];}else{var _R4=E(_QQ[2]);return _R4[0]==0?[0]:[1,[0,_R3[1],_R4[1]]];}})],_QS[2]];},_R5=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_R6=unCStr("This formulary is the same than the one "),_R7=[0,97],_R8=[1,_R7,_g],_R9=function(_Ra,_Rb){var _Rc=new T(function(){return A(_Ra,[_Rb]);});return function(_Rd,_){var _Re=jsCreateElem(toJSStr(_R8)),_Rf=jsAppendChild(_Re,E(_Rd)[1]),_Rg=[0,_Re],_Rh=A(_Rc,[_Rg,_]);return _Rg;};},_Ri=unCStr("run in the server by MFlow"),_Rj=new T(function(){return _R9(_4S,_Ri);}),_Rk=unCStr("href"),_Rl=function(_Rm,_){var _Rn=_4S(_R6,_Rm,_),_Ro=A(_Rj,[_Rm,_]),_Rp=A(_B,[_H,_Ro,_Rk,_R5,_]);return _Rm;},_Rq=new T(function(){return _4Z(_uM,_Rl);}),_Rr=unCStr("Fields of a form appear in sequence. Some of the fields trigger events instantly. Some others use a button to trigger them. It also contains option buttons, radio buttons etc"),_Rs=new T(function(){return _4Z(_4S,_Rr);}),_Rt=function(_Ru,_){var _Rv=_4f(_QJ,_Qs,_Ru,_),_Rw=E(_Rv),_Rx=E(_Rw[1]);return [0,[0,function(_Ry,_){var _Rz=A(_Rs,[_Ry,_]),_RA=A(_Rq,[_Ry,_]),_RB=A(_Rx[1],[_Ry,_]);return _Ry;},_Rx[2]],_Rw[2]];},_RC=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_RD=new T(function(){return _4Z(_4S,_RC);}),_RE=[1,_5B],_RF=unCStr("GalleryIndex"),_RG=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_z8,_z9,_RF],_RH=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_RG,_g],_RI=function(_RJ){return E(_RH);},_RK=new T(function(){return _Bq(_13,_3x,_11,_RI);}),_RL=function(_RM,_){var _RN=A(_RK,[_RM,_]);return [0,[0,_vb,new T(function(){var _RO=E(E(_RN)[1]);return _RO[0]==0?E(_RE):E(_RO);})],new T(function(){return E(E(_RN)[2]);})];},_RP=unCStr("100%"),_RQ=[0,62],_RR=[1,_RQ,_g],_RS=[1,_RR],_RT=new T(function(){return A(_yh,[_RS]);}),_RU=new T(function(){return _t1(_RT,_vf);}),_RV=function(_RW){return E(_RU);},_RX=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_RY=[1,_RX,_g],_RZ=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_S0=[1,_RZ,_RY],_S1=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_S2=[1,_S1,_S0],_S3=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_S4=[1,_S3,_S2],_S5=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_S6=[1,_S5,_S4],_S7=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_S8=[1,_S7,_S6],_S9=unCStr("height"),_Sa=unCStr("img"),_Sb=function(_Sc,_){var _Sd=jsCreateElem(toJSStr(E(_Sa))),_Se=jsAppendChild(_Sd,E(_Sc)[1]);return [0,_Sd];},_Sf=function(_Sg,_Sh){while(1){var _Si=E(_Sg);if(!_Si[0]){return E(_Sh);}else{_Sg=_Si[2];var _Sj=_Sh+1|0;_Sh=_Sj;continue;}}},_Sk=new T(function(){return [0,_Sf(_S8,0)-1|0];}),_Sl=[0,_2X,_5q],_Sm=unCStr("src"),_Sn=unCStr("width"),_So=function(_Sp){return function(_ci,_yZ){return _4f(function(_Bf,_){return _4f(_B6,function(_Sq){return function(_Sr,_){return [0,_Sl,new T(function(){var _Ss=E(_Sq);return [0,_Ss[1],_Ss[2],_Ss[3],_Ss[4],_Ss[5],new T(function(){return _AV(_RH,new T(function(){var _St=E(_Sp)[1];return _St!=E(_Sk)[1]?[0,_St+1|0]:E(_5B);}),_Ss[6]);})];})];};},_Bf,_);},function(_Su,_Bf,_){return (function(_Bf,_){return _4f(function(_Sv,_){return [0,[0,function(_Sw,_){var _Sx=_Sb(_Sw,_),_Sy=A(_B,[_H,_Sx,_Sm,new T(function(){var _Sz=E(_Sp)[1];return _Sz>=0?_w8(_S8,_Sz):E(_w5);}),_]),_SA=A(_B,[_H,_Sx,_Sn,_RP,_]),_SB=A(_B,[_H,_Sx,_S9,_RP,_]),_SC=_5m(_Sw,_);return _Sw;},_B8],_Sv];},_RV,_Bf,_);})(_Bf,_);},_ci,_yZ);};},_SD=function(_Bf,_){return _4f(_RL,_So,_Bf,_);},_SE=function(_SF,_SG,_){return _SH(_SG,_);},_SI=function(_Bf,_){return _yA(_SD,_SE,_Bf,_);},_SJ=[0,20000],_SK=new T(function(){return _3K(_13,_3x,_11,_Y);}),_SL=function(_SM,_SN,_SO,_){var _SP=A(_SK,[_SO,_]),_SQ=new T(function(){return E(E(_SP)[1]);}),_SR=function(_){var _SS=jsSetTimeout(E(_SM)[1],function(_){var _ST=jsFind(toJSStr(E(_SQ))),_SU=E(_ST);if(!_SU[0]){return _A;}else{var _SV=E(_SU[1]),_SW=E(_SV),_SX=jsClearChildren(_SV[1]),_SY=E(_k)[1],_SZ=takeMVar(_SY),_T0=A(_SN,[_SZ,_]),_T1=E(_T0),_T2=E(_T1[1]),_T3=_T2[1],_T4=_T2[2],_=putMVar(_SY,new T(function(){var _T5=E(_T1[2]);return [0,_T5[1],_T5[2],_T5[3],_T5[4],_0,_T5[6]];}));if(!E(E(_SZ)[5])){var _T6=A(_T3,[_SW,_]),_T7=E(_T4);if(!_T7[0]){return _SR(_);}else{var _T8=E(_T7[1]);return _A;}}else{var _T9=A(_7,[E(_SW[1]),_]),_Ta=A(_T3,[[0,_T9],_]),_Tb=E(_T4);if(!_Tb[0]){return _SR(_);}else{var _Tc=E(_Tb[1]);return _A;}}}});return _A;},_Td=_SR(_);return _yn(_SQ,_SN,new T(function(){return E(E(_SP)[2]);}),_);},_SH=function(_Te,_){var _Tf=_SL(_SJ,_SI,_Te,_),_Tg=E(_Tf),_Th=E(_Tg[1]);return [0,[0,function(_Ti,_){var _Tj=A(_RD,[_Ti,_]),_Tk=A(_Th[1],[_Ti,_]);return _Ti;},_Th[2]],_Tg[2]];},_Tl=function(_Tm){var _Tn=new T(function(){return _5c(_4S,new T(function(){return unAppCStr(" returns ",_Tm);}));});return function(_To,_){return [0,[0,_Tn,_B8],_To];};},_Tp=unCStr("This link say Hey!"),_Tq=function(_Bf,_){return _4S(_Tp,_Bf,_);},_Tr=unCStr("Hey!"),_Ts=function(_){var _=0,_Tt=newMVar(),_=putMVar(_Tt,_9);return [0,_Tt];},_Tu=new T(function(){return _2(_Ts);}),_Tv=new T(function(){return _3K(_13,_3x,_11,_Y);}),_Tw=new T(function(){return A(_oS,[_6H]);}),_Tx=function(_Ty,_Tz,_){var _=putMVar(E(_Ty)[1],_Tz);return _A;},_TA=function(_TB,_TC,_){return _s8(function(_){return _Tx(_Tu,_TB,_);},_TC,_);},_TD=function(_){var _TE=E(_Tu)[1],_TF=takeMVar(_TE),_=putMVar(_TE,_TF);return _TF;},_TG=function(_TH,_TI,_TJ,_TK){var _TL=new T(function(){return _R9(_uM,_TK);}),_TM=new T(function(){return unAppCStr("#/",new T(function(){var _TN=A(_TI,[_TJ]),_TO=E(_Tw),_TP=hs_eqWord64(_TN[1],_TO[1]);if(!E(_TP)){return A(_qW,[_TH,_TJ]);}else{var _TQ=hs_eqWord64(_TN[2],_TO[2]);return E(_TQ)==0?A(_qW,[_TH,_TJ]):E(_TJ);}}));});return function(_TR,_){var _TS=A(_Tv,[_TR,_]),_TT=0,_TU=function(_,_TV,_TW){var _TX=new T(function(){return E(E(_TS)[1]);}),_TY=function(_TZ,_){var _U0=A(_TL,[_TZ,_]),_U1=A(_B,[_H,_U0,_Rk,_TM,_]),_U2=E(_U0),_U3=jsSetCB(_U2[1],E(_sH)[1],E([0,function(_U4,_U5,_){return (function(_){var _U6=0;if(!E(_U6)){return (function(_){var _U7=takeMVar(E(_Tu)[1]),_U8=jsCatch(function(_){return (function(_){return [1,_TX];})();},function(_X,_){return _TA(_U7,_X,_);});return _Tx(_Tu,_U8,_);})();}else{var _U9=takeMVar(E(_Tu)[1]),_Ua=jsCatch(function(_){return [1,_TX];},function(_X,_){return _TA(_U9,_X,_);});return _Tx(_Tu,_Ua,_);}})(_);}])[1]);return _U2;},_Ub=E(_TV);return _Ub[0]==0?[0,[0,_TY,_9],_TW]:!_sq(_Ub[1],_TX)?[0,[0,_TY,_9],_TW]:[0,[0,_TY,[1,_TJ]],_TW];};if(!E(_TT)){var _Uc=_TD();return _TU(_,_Uc,new T(function(){return E(E(_TS)[2]);}));}else{var _Ud=E(_Tu)[1],_Ue=takeMVar(_Ud),_=putMVar(_Ud,_Ue);return _TU(_,_Ue,new T(function(){return E(E(_TS)[2]);}));}};},_Uf=new T(function(){return _TG(_DV,_DU,_Tr,_Tq);}),_Ug=new T(function(){return _t1(_Uf,_vf);}),_Uh=function(_Ui,_){var _Uj=A(_Ug,[_Ui,_]),_Uk=E(_Uj),_Ul=E(_Uk[1]);return [0,[0,function(_Um,_){var _Un=_5m(_Um,_),_Uo=A(_Ul[1],[_Um,_]);return _Um;},_Ul[2]],_Uk[2]];},_Up=function(_){var _Uq=E(_s7)[1],_Ur=takeMVar(_Uq),_=putMVar(_Uq,_Ur);return _Ur;},_Us=function(_Ut,_){var _Uu=0;if(!E(_Uu)){var _Uv=_Up();return [0,[0,_2X,[1,_Uv]],_Ut];}else{var _Uw=E(_s7)[1],_Ux=takeMVar(_Uw),_=putMVar(_Uw,_Ux);return [0,[0,_2X,[1,_Ux]],_Ut];}},_Uy=function(_Uz,_UA,_UB){return A(_Uz,[[1,_2t,new T(function(){return A(_UA,[_UB]);})]]);},_UC=unCStr("Key "),_UD=unCStr("Mouse "),_UE=unCStr("Click "),_UF=unCStr("NoData"),_UG=function(_UH){return _1G(_UF,_UH);},_UI=unCStr(": empty list"),_UJ=unCStr("Prelude."),_UK=function(_UL){return err(_1G(_UJ,new T(function(){return _1G(_UL,_UI);})));},_UM=unCStr("foldr1"),_UN=new T(function(){return _UK(_UM);}),_UO=function(_UP,_UQ){var _UR=E(_UQ);if(!_UR[0]){return E(_UN);}else{var _US=_UR[1],_UT=E(_UR[2]);return _UT[0]==0?E(_US):A(_UP,[_US,new T(function(){return _UO(_UP,_UT);})]);}},_UU=[0,32],_UV=function(_UW,_UX){var _UY=E(_UX);switch(_UY[0]){case 0:return E(_UG);case 1:var _UZ=function(_V0){return _3F(11,E(_UY[1])[1],[1,_UU,new T(function(){var _V1=E(_UY[2]);return [1,_3E,new T(function(){return A(_UO,[_Uy,[1,function(_V2){return _3F(0,E(_V1[1])[1],_V2);},[1,function(_V3){return _3F(0,E(_V1[2])[1],_V3);},_g]],[1,_3D,_V0]]);})];})]);};return E(_UW)[1]<11?function(_V4){return _1G(_UE,new T(function(){return _UZ(_V4);}));}:function(_V5){return [1,_3E,new T(function(){return _1G(_UE,new T(function(){return _UZ([1,_3D,_V5]);}));})];};case 2:var _V6=function(_V7){return _1G(_UD,new T(function(){var _V8=E(_UY[1]);return [1,_3E,new T(function(){return A(_UO,[_Uy,[1,function(_V9){return _3F(0,E(_V8[1])[1],_V9);},[1,function(_Va){return _3F(0,E(_V8[2])[1],_Va);},_g]],[1,_3D,_V7]]);})];}));};return E(_UW)[1]<11?E(_V6):function(_Vb){return [1,_3E,new T(function(){return _V6([1,_3D,_Vb]);})];};default:var _Vc=_UY[1];return E(_UW)[1]<11?function(_Vd){return _1G(_UC,new T(function(){return _3F(11,E(_Vc)[1],_Vd);}));}:function(_Ve){return [1,_3E,new T(function(){return _1G(_UC,new T(function(){return _3F(11,E(_Vc)[1],[1,_3D,_Ve]);}));})];};}},_Vf=function(_Vg){var _Vh=new T(function(){return _4Z(_4S,new T(function(){var _Vi=E(_Vg);return _1G(_Vi[1],[1,_Q4,new T(function(){return A(_UV,[_oW,_Vi[2],_g]);})]);}));});return function(_Vj,_){return [0,[0,_Vh,_B8],_Vj];};},_Vk=function(_Bf,_){return _4f(_Us,_Vf,_Bf,_);},_Vl=function(_Vm){return E(_Vk);},_Vn=[14,coercionToken],_Vo=[12,coercionToken],_Vp=[9,coercionToken],_Vq=[11,coercionToken],_Vr=[5,coercionToken],_Vs=[10,coercionToken],_Vt=[6,coercionToken],_Vu=[7,coercionToken],_Vv=unCStr("height:100px;background-color:lightgreen;position:relative"),_Vw=unCStr("div"),_Vx=function(_Vy,_Vz){var _VA=new T(function(){return A(_Vy,[_Vz]);});return function(_VB,_){var _VC=jsCreateElem(toJSStr(E(_Vw))),_VD=jsAppendChild(_VC,E(_VB)[1]),_VE=[0,_VC],_VF=A(_VA,[_VE,_]);return _VE;};},_VG=unCStr("h1"),_VH=function(_VI,_VJ){var _VK=new T(function(){return A(_VI,[_VJ]);});return function(_VL,_){var _VM=jsCreateElem(toJSStr(E(_VG))),_VN=jsAppendChild(_VM,E(_VL)[1]),_VO=[0,_VM],_VP=A(_VK,[_VO,_]);return _VO;};},_VQ=unCStr("Mouse events here"),_VR=new T(function(){return _VH(_4S,_VQ);}),_VS=new T(function(){return _Vx(_uM,_VR);}),_VT=function(_VU,_){var _VV=A(_VS,[_VU,_]),_VW=A(_B,[_H,_VV,_Co,_Vv,_]);return _VV;},_VX=[0,_VT,_B8],_VY=function(_VZ,_){return [0,_VX,_VZ];},_W0=new T(function(){return _t1(_VY,_Vu);}),_W1=new T(function(){return _t1(_W0,_Vt);}),_W2=new T(function(){return _t1(_W1,_Vs);}),_W3=new T(function(){return _t1(_W2,_Vr);}),_W4=new T(function(){return _t1(_W3,_Vq);}),_W5=new T(function(){return _t1(_W4,_vf);}),_W6=new T(function(){return _t1(_W5,_Vp);}),_W7=new T(function(){return _t1(_W6,_Vo);}),_W8=new T(function(){return _t1(_W7,_Vn);}),_W9=new T(function(){return _t1(_W8,_9J);}),_Wa=unCStr("http://todomvc.com"),_Wb=unCStr("Work in progress for a todo application to be added to "),_Wc=unCStr("todomvc.com"),_Wd=new T(function(){return _R9(_4S,_Wc);}),_We=function(_Wf,_){var _Wg=_4S(_Wb,_Wf,_),_Wh=A(_Wd,[_Wf,_]),_Wi=A(_B,[_H,_Wh,_Rk,_Wa,_]);return _Wf;},_Wj=new T(function(){return _4Z(_uM,_We);}),_Wk=unCStr("Tasks"),_Wl=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_z8,_z9,_Wk],_Wm=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Wl,_g],_Wn=2,_Wo=function(_Wp,_Wq,_Wr,_Ws,_){var _Wt=A(_Wr,[_Ws,_]),_Wu=E(_Wt),_Wv=E(_Wu[1]),_Ww=_Wv[1];return [0,[0,function(_Wx,_){var _Wy=jsFind(toJSStr(E(_Wp))),_Wz=E(_Wy);if(!_Wz[0]){return _Wx;}else{var _WA=_Wz[1];switch(E(_Wq)){case 0:var _WB=A(_Ww,[_WA,_]);return _Wx;case 1:var _WC=E(_WA),_WD=_WC[1],_WE=jsGetChildren(_WD),_WF=E(_WE);if(!_WF[0]){var _WG=A(_Ww,[_WC,_]);return _Wx;}else{var _WH=jsCreateElem(toJSStr(E(_3X))),_WI=jsAddChildBefore(_WH,_WD,E(_WF[1])[1]),_WJ=A(_Ww,[[0,_WH],_]);return _Wx;}break;default:var _WK=E(_WA),_WL=jsClearChildren(_WK[1]),_WM=A(_Ww,[_WK,_]);return _Wx;}}},_Wv[2]],_Wu[2]];},_WN=[0,_2X,_5q],_WO=function(_WP,_){return [0,_WN,_WP];},_WQ=unCStr("Pattern match failure in do expression at src\\Main.hs:339:7-25"),_WR=new T(function(){return _O7(_WQ);}),_WS=function(_WT,_WU,_WV,_WW){return A(_WT,[new T(function(){return function(_){var _WX=jsSet(E(_WU)[1],toJSStr(E(_WV)),toJSStr(E(_WW)));return _A;};})]);},_WY=unCStr("text"),_WZ=unCStr("value"),_X0=new T(function(){return _6I(_oL,_oQ);}),_X1=new T(function(){return A(_X0,[_6H]);}),_X2=new T(function(){return A(_X0,[_6H]);}),_X3=unCStr("Prelude.read: ambiguous parse"),_X4=unCStr("Prelude.read: no parse"),_X5=function(_X6){return [1,function(_X7){return A(_kx,[_X7,function(_X8){return E([3,_X6,_bO]);}]);}];},_X9=function(_Xa){while(1){var _Xb=(function(_Xc){var _Xd=E(_Xc);if(!_Xd[0]){return [0];}else{var _Xe=_Xd[2],_Xf=E(_Xd[1]);if(!E(_Xf[2])[0]){return [1,_Xf[1],new T(function(){return _X9(_Xe);})];}else{_Xa=_Xe;return null;}}})(_Xa);if(_Xb!=null){return _Xb;}}},_Xg=function(_Xh,_Xi){var _Xj=_X9(_aE(A(E(_Xh)[3],[_mO,_X5]),_Xi));return _Xj[0]==0?err(_X4):E(_Xj[2])[0]==0?E(_Xj[1]):err(_X3);},_Xk=function(_Xl,_Xm,_Xn,_Xo){var _Xp=new T(function(){return _qW(_Xm);}),_Xq=new T(function(){return _qY(_8F,_9L,_Xn,_Xm,_Xl);});return [0,function(_Xr){return A(_Xq,[[1,_Xo],_WY,_Xr]);},function(_Xs,_){var _Xt=E(_Xo),_Xu=jsFind(toJSStr(_Xt)),_Xv=E(_Xu);return _Xv[0]==0?_45(_Xt):A(_WS,[_H,_Xv[1],_WZ,new T(function(){var _Xw=A(_Xn,[_Xs]),_Xx=E(_X1),_Xy=hs_eqWord64(_Xw[1],_Xx[1]);if(!E(_Xy)){return A(_Xp,[_Xs]);}else{var _Xz=hs_eqWord64(_Xw[2],_Xx[2]);return E(_Xz)==0?A(_Xp,[_Xs]):E(_Xs);}}),_]);},function(_){var _XA=E(_Xo),_XB=jsFind(toJSStr(_XA)),_XC=E(_XB);if(!_XC[0]){return _45(_XA);}else{var _XD=_D3(E(_XC[1])[1],_WZ,_);return new T(function(){var _XE=A(_X0,[_XD]),_XF=E(_X2),_XG=hs_eqWord64(_XE[1],_XF[1]);if(!E(_XG)){return _Xg(_Xl,_XD);}else{var _XH=hs_eqWord64(_XE[2],_XF[2]);return E(_XH)==0?_Xg(_Xl,_XD):E(_XD);}});}}];},_XI=unCStr("todo"),_XJ=new T(function(){var _XK=_Xk(_GE,_DV,_DU,_XI);return [0,_XK[1],_XK[2],_XK[3]];}),_XL=new T(function(){var _XM=A(E(_XJ)[2],[_g]);return function(_XN,_){var _XO=A(_XM,[_]);return [0,[0,_2X,[1,_XO]],_XN];};}),_XP=[1,_g],_XQ=function(_XR){return E(_Wm);},_XS=new T(function(){return _Bq(_13,_3x,_11,_XQ);}),_XT=function(_XU,_){var _XV=A(_XS,[_XU,_]);return [0,[0,_vb,new T(function(){var _XW=E(E(_XV)[1]);return _XW[0]==0?E(_XP):E(_XW);})],new T(function(){return E(E(_XV)[2]);})];},_XX=[0,_2X,_5q],_XY=[0,_2X,_5q],_XZ=function(_Y0,_Y1,_){return [0,_XY,_Y1];},_Y2=[0,_2X,_5q],_Y3=function(_Y4,_){return [0,_Y2,_Y4];},_Y5=unCStr("list"),_Y6=unCStr("check"),_Y7=new T(function(){return A(_FA,[_0,_Y6]);}),_Y8=new T(function(){return _t1(_Y7,_vf);}),_Y9=function(_Ya,_){var _Yb=A(_Y8,[_Ya,_]),_Yc=E(_Yb),_Yd=E(_Yc[1]);return [0,[0,_Yd[1],new T(function(){var _Ye=E(_Yd[2]);return _Ye[0]==0?[0]:[1,E(_Ye[1])[1]];})],_Yc[2]];},_Yf=unCStr("text-decoration:line-through;"),_Yg=unCStr("li"),_Yh=function(_Yi,_Yj){var _Yk=new T(function(){return A(_Yi,[_Yj]);});return function(_Yl,_){var _Ym=jsCreateElem(toJSStr(E(_Yg))),_Yn=jsAppendChild(_Ym,E(_Yl)[1]),_Yo=[0,_Ym],_Yp=A(_Yk,[_Yo,_]);return _Yo;};},_Yq=function(_Yr){var _Ys=E(_Yr);if(!_Ys[0]){return [0];}else{var _Yt=new T(function(){return _5c(_4S,_Ys[1]);});return [1,function(_Yu,_){var _Yv=_4f(_Y9,function(_Yw){var _Yx=E(_Yw);return _Yx[0]==0?function(_Yy,_){return [0,[0,_Yt,_B8],_Yy];}:!_sq(_Yx[1],_Y6)?function(_Yz,_){return [0,[0,_Yt,_B8],_Yz];}:E(_Yx[2])[0]==0?function(_YA,_){return [0,[0,function(_YB,_){var _YC=A(_Yt,[_YB,_]),_YD=A(_B,[_H,_YC,_Co,_Yf,_]);return _YC;},_B8],_YA];}:function(_YE,_){return [0,[0,_Yt,_B8],_YE];};},_Yu,_),_YF=E(_Yv),_YG=E(_YF[1]);return [0,[0,new T(function(){return _Yh(_uM,_YG[1]);}),_YG[2]],_YF[2]];},new T(function(){return _Yq(_Ys[2]);})];}},_YH=function(_YI,_YJ){while(1){var _YK=(function(_YL,_YM){var _YN=E(_YM);if(!_YN[0]){return E(_YL);}else{_YI=function(_YO,_){var _YP=A(_YL,[_YO,_]),_YQ=E(_YP),_YR=E(_YQ[1]),_YS=A(_YN[1],[_YQ[2],_]),_YT=E(_YS),_YU=E(_YT[1]);return [0,[0,function(_YV,_){var _YW=A(_YR[1],[_YV,_]),_YX=A(_YU[1],[_YV,_]);return _YV;},new T(function(){var _YY=E(_YR[2]);return _YY[0]==0?E(_YU[2]):E(_YY);})],_YT[2]];};_YJ=_YN[2];return null;}})(_YI,_YJ);if(_YK!=null){return _YK;}}},_YZ=function(_Z0,_Z1,_){return _4f(_Us,function(_Z2){var _Z3=E(E(_Z2)[2]);return _Z3[0]==3?E(E(_Z3[1])[1])==13?function(_Bf,_){return _4f(_XL,function(_Z4){return function(_Bf,_){return _4f(_XT,function(_Z5){var _Z6=new T(function(){return _YH(_Y3,_Yq([1,_Z0,_Z5]));});return function(_ci,_yZ){return _4f(function(_Bf,_){return _4f(_B6,function(_Z7){return function(_Z8,_){return [0,_XX,new T(function(){var _Z9=E(_Z7);return [0,_Z9[1],_Z9[2],_Z9[3],_Z9[4],_Z9[5],new T(function(){return _AV(_Wm,[1,_Z0,_Z5],_Z9[6]);})];})];};},_Bf,_);},function(_Za,_Bf,_){return (function(_Bf,_){return _4f(function(_Bf,_){return _Wo(_Y5,_Wn,_Z6,_Bf,_);},_XZ,_Bf,_);})(_Bf,_);},_ci,_yZ);};},_Bf,_);};},_Bf,_);}:E(_WO):E(_WR);},_Z1,_);},_Zb=new T(function(){return A(E(_XJ)[1],[_9]);}),_Zc=new T(function(){return _t1(_Zb,_9J);}),_Zd=unCStr("todos"),_Ze=new T(function(){return _VH(_4S,_Zd);}),_Zf=new T(function(){return _Vx(_uM,_2X);}),_Zg=function(_Zh,_){var _Zi=_4f(_Zc,_YZ,_Zh,_),_Zj=E(_Zi),_Zk=E(_Zj[1]),_Zl=new T(function(){return _v2(_uM,function(_Zm,_){var _Zn=A(_Ze,[_Zm,_]),_Zo=A(_Zk[1],[_Zm,_]);return _Zm;});});return [0,[0,function(_Zp,_){var _Zq=A(_Zl,[_Zp,_]),_Zr=A(_Zf,[_Zp,_]),_Zs=A(_B,[_H,_Zr,_II,_Y5,_]);return _Zp;},new T(function(){var _Zt=E(_Zk[2]);return _Zt[0]==0?E(_B8):E(_Zt);})],_Zj[2]];},_Zu=function(_Zv,_Zw,_){return [0,[0,_2X,[1,[1,_Zv]]],_Zw];},_Zx=unCStr("revEntry"),_Zy=new T(function(){var _Zz=_Xk(_GE,_DV,_DU,_Zx);return [0,_Zz[1],_Zz[2],_Zz[3]];}),_ZA=new T(function(){return A(E(_Zy)[1],[_9]);}),_ZB=new T(function(){return _t1(_ZA,_9J);}),_ZC=function(_ZD,_ZE,_){return [0,[0,_2X,[1,[0,_ZD]]],_ZE];},_ZF=unCStr("entry"),_ZG=new T(function(){var _ZH=_Xk(_GE,_DV,_DU,_ZF);return [0,_ZH[1],_ZH[2],_ZH[3]];}),_ZI=new T(function(){return A(E(_ZG)[1],[_9]);}),_ZJ=new T(function(){return _t1(_ZI,_9J);}),_ZK=function(_ZL,_){var _ZM=_4f(_ZJ,_ZC,_ZL,_),_ZN=E(_ZM),_ZO=E(_ZN[1]),_ZP=_4f(_ZB,_Zu,_ZN[2],_),_ZQ=E(_ZP),_ZR=E(_ZQ[1]);return [0,[0,new T(function(){return _v2(_uM,function(_ZS,_){var _ZT=A(_ZO[1],[_ZS,_]),_ZU=_5m(_ZS,_),_ZV=A(_ZR[1],[_ZS,_]);return _ZS;});}),new T(function(){var _ZW=E(_ZO[2]);return _ZW[0]==0?E(_ZR[2]):E(_ZW);})],_ZQ[2]];},_ZX=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_ZY=new T(function(){return _4Z(_4S,_ZX);}),_ZZ=function(_100){var _101=A(E(_Zy)[2],[_100]);return function(_102,_){var _103=A(_101,[_]);return [0,[0,_2X,[1,_103]],_102];};},_104=function(_105,_106){while(1){var _107=E(_105);if(!_107[0]){return E(_106);}else{_105=_107[2];var _108=[1,_107[1],_106];_106=_108;continue;}}},_109=function(_10a){var _10b=new T(function(){return _104(_10a,_g);});return function(_10c,_){return [0,[0,_2X,[1,_10b]],_10c];};},_10d=new T(function(){var _10e=E(E(_ZG)[3]);return function(_10f,_){var _10g=A(_10e,[_]);return [0,[0,_2X,[1,_10g]],_10f];};}),_10h=function(_Bf,_){return _4f(_10d,_109,_Bf,_);},_10i=function(_Bf,_){return _4f(_10h,_ZZ,_Bf,_);},_10j=function(_10k){var _10l=A(E(_ZG)[2],[_10k]);return function(_10m,_){var _10n=A(_10l,[_]);return [0,[0,_2X,[1,_10n]],_10m];};},_10o=new T(function(){var _10p=E(E(_Zy)[3]);return function(_10q,_){var _10r=A(_10p,[_]);return [0,[0,_2X,[1,_10r]],_10q];};}),_10s=function(_10t){var _10u=new T(function(){return _104(_10t,_g);});return function(_10v,_){return [0,[0,_2X,[1,_10u]],_10v];};},_10w=function(_Bf,_){return _4f(_10o,_10s,_Bf,_);},_10x=function(_Bf,_){return _4f(_10w,_10j,_Bf,_);},_10y=function(_10z){return E(_10z)[0]==0?E(_10i):E(_10x);},_10A=function(_10B,_){var _10C=_4f(_ZK,_10y,_10B,_),_10D=E(_10C),_10E=E(_10D[1]);return [0,[0,function(_10F,_){var _10G=A(_ZY,[_10F,_]),_10H=A(_10E[1],[_10F,_]);return _10F;},_10E[2]],_10D[2]];},_10I=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_10J=new T(function(){return _4Z(_4S,_10I);}),_10K=[0,_2X,_9],_10L=function(_10M,_){return [0,_10K,_10M];},_10N=function(_10O,_10P,_10Q){var _10R=E(_10Q);if(!_10R[0]){var _10S=_10R[3],_10T=_10R[4],_10U=_10R[5],_10V=E(_10R[2]),_10W=_10V[1];return _10O>=_10W?_10O!=_10W?_Ag(_10V,_10S,_10T,_10N(_10O,_10P,_10U)):[0,_10R[1],E([0,_10O]),_10P,E(_10T),E(_10U)]:_zB(_10V,_10S,_10N(_10O,_10P,_10T),_10U);}else{return [0,1,E([0,_10O]),_10P,E(_f),E(_f)];}},_10X=function(_10Y,_10Z,_110){var _111=E(_10Y),_112=_111[1],_113=E(_110);if(!_113[0]){var _114=_113[3],_115=_113[4],_116=_113[5],_117=E(_113[2]),_118=_117[1];return _112>=_118?_112!=_118?_Ag(_117,_114,_115,_10N(_112,_10Z,_116)):[0,_113[1],E(_111),_10Z,E(_115),E(_116)]:_zB(_117,_114,_10N(_112,_10Z,_115),_116);}else{return [0,1,E(_111),_10Z,E(_f),E(_f)];}},_119=function(_11a,_11b){while(1){var _11c=E(_11b);if(!_11c[0]){var _11d=E(_11c[2])[1];if(_11a>=_11d){if(_11a!=_11d){_11b=_11c[5];continue;}else{return [1,_11c[3]];}}else{_11b=_11c[4];continue;}}else{return [0];}}},_11e=unCStr("containers-0.5.5.1"),_11f=unCStr("Data.Map.Base"),_11g=unCStr("Map"),_11h=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_11e,_11f,_11g],_11i=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_11h,_g],_11j=function(_11k){return E(_11i);},_11l=function(_11m){var _11n=E(_11m);if(!_11n[0]){return [0];}else{var _11o=E(_11n[1]);return [1,[0,_11o[1],_11o[2]],new T(function(){return _11l(_11n[2]);})];}},_11p=function(_11q,_11r){return function(_11s){return E(new T(function(){var _11t=A(_11q,[_6H]),_11u=E(_11t[3]),_11v=_11u[1],_11w=_11u[2],_11x=_1G(_11t[4],[1,new T(function(){return A(_11r,[_6H]);}),_g]);if(!_11x[0]){return [0,_11v,_11w,_11u,_g];}else{var _11y=_6c(new T(function(){return _60(_6o(_6z,[1,[0,_11v,_11w],new T(function(){return _11l(_11x);})]));}));return [0,_11y[1],_11y[2],_11u,_11x];}}));};},_11z=new T(function(){return _11p(_11j,_oj);}),_11A=new T(function(){return _6I(_11z,_oj);}),_11B=new T(function(){return _Bq(_13,_3x,_11,_11A);}),_11C=function(_11D,_){var _11E=A(_11B,[_11D,_]);return [0,[0,_2X,new T(function(){return E(E(_11E)[1]);})],new T(function(){return E(E(_11E)[2]);})];},_11F=new T(function(){return _6I(_11z,_oj);}),_11G=[1,_f],_11H=new T(function(){return _Bq(_13,_3x,_11,_11F);}),_11I=function(_11J,_){var _11K=A(_11H,[_11J,_]);return [0,[0,_vb,new T(function(){var _11L=E(E(_11K)[1]);return _11L[0]==0?E(_11G):E(_11L);})],new T(function(){return E(E(_11K)[2]);})];},_11M=[0,_2X,_5q],_11N=[1,_9],_11O=function(_11P,_11Q){var _11R=new T(function(){return [0,E(_11P)[1]+1|0];});return function(_ci,_yZ){return _4f(function(_Bf,_){return _4f(function(_11S,_){var _11T=_4f(_11C,function(_11U){var _11V=_119(E(_11P)[1],_11U);return _11V[0]==0?E(_10L):function(_11W,_){return [0,[0,_2X,_11V],_11W];};},_11S,_),_11X=E(_11T),_11Y=E(_11X[1]);return [0,[0,function(_11Z,_){var _120=A(_11Y[1],[_11Z,_]);return _11Z;},new T(function(){var _121=E(_11Y[2]);return _121[0]==0?E(_11N):[1,_121];})],_11X[2]];},function(_122){var _123=new T(function(){return _t1(new T(function(){return A(_rJ,[_9,_9K,_122]);}),_9J);});return function(_ci,_yZ){return _4f(function(_124,_){var _125=A(_123,[_124,_]),_126=E(_125),_127=_126[2],_128=E(_126[1]),_129=_128[1],_12a=_128[2],_12b=E(_122);return _12b[0]==0?[0,[0,function(_12c,_){var _12d=A(_129,[_12c,_]);return _12c;},_12a],_127]:[0,[0,function(_12e,_){var _12f=A(_129,[_12e,_]);return _12e;},new T(function(){var _12g=E(_12a);return _12g[0]==0?E(_12b):E(_12g);})],_127];},function(_12h,_12i,_){return _4f(function(_Bf,_){return _4f(_11I,function(_12j){var _12k=new T(function(){return _10X(_11P,_12h,_12j);}),_12l=new T(function(){return A(_11F,[_12k]);});return function(_ci,_yZ){return _4f(_B6,function(_12m){return function(_12n,_){return [0,_11M,new T(function(){var _12o=E(_12m);return [0,_12o[1],_12o[2],_12o[3],_12o[4],_12o[5],new T(function(){return _AV(_12l,_12k,_12o[6]);})];})];};},_ci,_yZ);};},_Bf,_);},function(_12p,_Bf,_){return (function(_12q,_){return [0,[0,_2X,[1,_12h]],_12q];})(_Bf,_);},_12i,_);},_ci,_yZ);};},_Bf,_);},function(_12r){var _12s=new T(function(){return _11O(_11R,new T(function(){return _8U(_11Q,_12r);}));}),_12t=new T(function(){return _5c(_4S,new T(function(){return _3F(0,E(_11Q)[1]+E(_12r)[1]|0,_g);}));});return function(_ci,_yZ){return _4f(function(_12u,_){return [0,[0,function(_12v,_){var _12w=A(_12t,[_12v,_]),_12x=_5m(_12v,_);return _12v;},_5q],_12u];},function(_12y){return E(_12s);},_ci,_yZ);};},_ci,_yZ);};},_12z=new T(function(){return _11O(_5B,_5B);}),_12A=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_12B=new T(function(){return _4Z(_4S,_12A);}),_12C=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_12D=new T(function(){return _t1(_12C,_9J);}),_12E=function(_12F){var _12G=new T(function(){return _5c(_4S,new T(function(){return _58(_12F);}));});return function(_ci,_yZ){return _4f(_12D,function(_12H){var _12I=E(E(_12H)[1]);if(!_12I){return function(_12J,_){return [0,[0,function(_12K,_){var _12L=_5m(_12K,_),_12M=_4S(_5r,_12K,_),_12N=A(_12G,[_12K,_]);return _12K;},_9],_12J];};}else{var _12O=new T(function(){return _12E(new T(function(){return [0,E(_12F)[1]+_12I|0];}));}),_12P=new T(function(){return _5c(_4S,new T(function(){return _3F(0,E(_12F)[1]+_12I|0,_g);}));});return function(_ci,_yZ){return _4f(function(_12Q,_){return [0,[0,function(_12R,_){var _12S=A(_12P,[_12R,_]),_12T=_5m(_12R,_);return _12R;},_5q],_12Q];},function(_12U){return E(_12O);},_ci,_yZ);};}},_ci,_yZ);};},_12V=new T(function(){return _12E(_5B);}),_12W=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_12X=new T(function(){return _4Z(_4S,_12W);}),_12Y=function(_12Z){return function(_130,_){return [0,[0,new T(function(){var _131=new T(function(){return _5c(_4S,new T(function(){return _58(_12Z);}));});return _4Z(_uM,function(_132,_){var _133=_4S(_5r,_132,_),_134=A(_131,[_132,_]);return _132;});}),_5q],_130];};},_135=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_136=new T(function(){return _t1(_135,_9J);}),_137=unCStr("second number "),_138=unCStr("first number"),_139=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_13a=new T(function(){return _t1(_139,_9J);}),_13b=function(_13c,_){var _13d=A(_136,[_13c,_]),_13e=E(_13d),_13f=E(_13e[1]),_13g=A(_13a,[_13e[2],_]),_13h=E(_13g),_13i=E(_13h[1]);return [0,[0,function(_13j,_){var _13k=_4S(_138,_13j,_),_13l=_5m(_13j,_),_13m=A(_13f[1],[_13j,_]),_13n=_5m(_13j,_),_13o=_4S(_137,_13j,_),_13p=_5m(_13j,_),_13q=A(_13i[1],[_13j,_]),_13r=_5m(_13j,_);return _13j;},new T(function(){var _13s=E(_13f[2]);if(!_13s[0]){return [0];}else{var _13t=E(_13i[2]);return _13t[0]==0?[0]:[1,new T(function(){return _8U(_13s[1],_13t[1]);})];}})],_13h[2]];},_13u=function(_13v,_){var _13w=_4f(_13b,_12Y,_13v,_),_13x=E(_13w),_13y=E(_13x[1]),_13z=new T(function(){return _4Z(_uM,_13y[1]);});return [0,[0,function(_13A,_){var _13B=A(_12X,[_13A,_]),_13C=A(_13z,[_13A,_]);return _13A;},_13y[2]],_13x[2]];},_13D=unCStr("table"),_13E=function(_13F,_13G){var _13H=new T(function(){return A(_13F,[_13G]);});return function(_13I,_){var _13J=jsCreateElem(toJSStr(E(_13D))),_13K=jsAppendChild(_13J,E(_13I)[1]),_13L=[0,_13J],_13M=A(_13H,[_13L,_]);return _13L;};},_13N=unCStr("hplayground examples"),_13O=new T(function(){return _VH(_4S,_13N);}),_13P=unCStr("td"),_13Q=function(_13R,_13S){var _13T=new T(function(){return A(_13R,[_13S]);});return function(_13U,_){var _13V=jsCreateElem(toJSStr(E(_13P))),_13W=jsAppendChild(_13V,E(_13U)[1]),_13X=[0,_13V],_13Y=A(_13T,[_13X,_]);return _13X;};},_13Z=unCStr("tr"),_140=function(_141,_142){var _143=new T(function(){return A(_141,[_142]);});return function(_144,_){var _145=jsCreateElem(toJSStr(E(_13Z))),_146=jsAppendChild(_145,E(_144)[1]),_147=[0,_145],_148=A(_143,[_147,_]);return _147;};},_149=unCStr("bottom of the page"),_14a=new T(function(){return _5c(_4S,_149);}),_14b=unCStr("h3"),_14c=function(_14d,_14e){var _14f=new T(function(){return A(_14d,[_14e]);});return function(_14g,_){var _14h=jsCreateElem(toJSStr(E(_14b))),_14i=jsAppendChild(_14h,E(_14g)[1]),_14j=[0,_14h],_14k=A(_14f,[_14j,_]);return _14j;};},_14l=unCStr("https://github.com/agocorona/hplayground"),_14m=unCStr("   "),_14n=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_14o=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_14p=unCStr("Git repository"),_14q=new T(function(){return _R9(_4S,_14p);}),_14r=unCStr("Examples code"),_14s=new T(function(){return _R9(_4S,_14r);}),_14t=unCStr("Article"),_14u=new T(function(){return _R9(_4S,_14t);}),_14v=function(_14w,_){var _14x=A(_14q,[_14w,_]),_14y=A(_B,[_H,_14x,_Rk,_14l,_]),_14z=_4S(_14m,_14w,_),_14A=A(_14s,[_14w,_]),_14B=A(_B,[_H,_14A,_Rk,_14n,_]),_14C=_4S(_14m,_14w,_),_14D=A(_14u,[_14w,_]),_14E=A(_B,[_H,_14D,_Rk,_14o,_]);return _14w;},_14F=new T(function(){return _v2(_uM,_14v);}),_14G=new T(function(){return _14c(_uM,_14F);}),_14H=function(_14I,_){var _14J=_13u(_14I,_),_14K=E(_14J),_14L=E(_14K[1]),_14M=A(_v0,[_14K[2],_]),_14N=E(_14M),_14O=E(_14N[1]),_14P=A(_12V,[_14N[2],_]),_14Q=E(_14P),_14R=E(_14Q[1]),_14S=A(_C2,[_14Q[2],_]),_14T=E(_14S),_14U=E(_14T[1]),_14V=_HZ(_14T[2],_),_14W=E(_14V),_14X=E(_14W[1]),_14Y=_4f(_Uh,_Tl,_14W[2],_),_14Z=E(_14Y),_150=E(_14Z[1]),_151=A(_12z,[_14Z[2],_]),_152=E(_151),_153=E(_152[1]),_154=A(_CJ,[_152[2],_]),_155=E(_154),_156=E(_155[1]),_157=_Rt(_155[2],_),_158=E(_157),_159=E(_158[1]),_15a=_10A(_158[2],_),_15b=E(_15a),_15c=E(_15b[1]),_15d=_Zg(_15b[2],_),_15e=E(_15d),_15f=E(_15e[1]),_15g=_MR(_15e[2],_),_15h=E(_15g),_15i=E(_15h[1]),_15j=_SH(_15h[2],_),_15k=E(_15j),_15l=E(_15k[1]),_15m=_4f(_W9,_Vl,_15k[2],_),_15n=E(_15m),_15o=E(_15n[1]),_15p=new T(function(){return _13E(_uM,function(_15q,_){var _15r=A(new T(function(){var _15s=new T(function(){return _13Q(_uM,function(_15t,_){var _15u=A(_12B,[_15t,_]),_15v=A(_14R[1],[_15t,_]);return _15t;});}),_15w=new T(function(){return _13Q(_uM,_14O[1]);}),_15x=new T(function(){return _13Q(_uM,_14L[1]);});return _140(_uM,function(_15y,_){var _15z=A(_15x,[_15y,_]),_15A=A(_B,[_H,_15z,_Co,_4O,_]),_15B=A(_15w,[_15y,_]),_15C=A(_B,[_H,_15B,_Co,_4O,_]),_15D=A(_15s,[_15y,_]),_15E=A(_B,[_H,_15D,_Co,_4O,_]);return _15y;});}),[_15q,_]),_15F=A(_B,[_H,_15r,_Co,_4Q,_]),_15G=A(new T(function(){var _15H=new T(function(){return _13Q(_uM,_156[1]);}),_15I=new T(function(){return _13Q(_uM,function(_15J,_){var _15K=A(_10J,[_15J,_]),_15L=A(_153[1],[_15J,_]);return _15J;});}),_15M=new T(function(){return _13Q(_uM,function(_15N,_){var _15O=A(_14U[1],[_15N,_]),_15P=A(_14X[1],[_15N,_]),_15Q=A(_150[1],[_15N,_]);return _15N;});});return _140(_uM,function(_15R,_){var _15S=A(_15M,[_15R,_]),_15T=A(_B,[_H,_15S,_Co,_4O,_]),_15U=A(_15I,[_15R,_]),_15V=A(_B,[_H,_15U,_Co,_4O,_]),_15W=A(_15H,[_15R,_]),_15X=A(_B,[_H,_15W,_Co,_4O,_]);return _15R;});}),[_15q,_]),_15Y=A(_B,[_H,_15G,_Co,_4Q,_]),_15Z=A(new T(function(){var _160=new T(function(){return _13Q(_uM,function(_161,_){var _162=A(_Wj,[_161,_]),_163=A(_15f[1],[_161,_]);return _161;});}),_164=new T(function(){return _13Q(_uM,_15c[1]);}),_165=new T(function(){return _13Q(_uM,new T(function(){return _v2(_uM,_159[1]);}));});return _140(_uM,function(_166,_){var _167=A(_165,[_166,_]),_168=A(_B,[_H,_167,_Co,_4O,_]),_169=A(_164,[_166,_]),_16a=A(_B,[_H,_169,_Co,_4O,_]),_16b=A(_160,[_166,_]),_16c=A(_B,[_H,_16b,_Co,_4O,_]);return _166;});}),[_15q,_]),_16d=A(_B,[_H,_15Z,_Co,_4Q,_]),_16e=A(new T(function(){var _16f=new T(function(){return _13Q(_uM,_15o[1]);}),_16g=new T(function(){return _13Q(_uM,_15l[1]);}),_16h=new T(function(){return _13Q(_uM,_15i[1]);});return _140(_uM,function(_16i,_){var _16j=A(_16h,[_16i,_]),_16k=A(_B,[_H,_16j,_Co,_4O,_]),_16l=A(_16g,[_16i,_]),_16m=A(_B,[_H,_16l,_Co,_4O,_]),_16n=A(_16f,[_16i,_]),_16o=A(_B,[_H,_16n,_Co,_4O,_]);return _16i;});}),[_15q,_]),_16p=A(_B,[_H,_16e,_Co,_4Q,_]);return _15q;});});return [0,[0,function(_16q,_){var _16r=A(_13O,[_16q,_]),_16s=A(_B,[_H,_16r,_Co,_Cn,_]),_16t=A(_14G,[_16q,_]),_16u=A(_15p,[_16q,_]),_16v=A(_B,[_H,_16u,_Co,_4P,_]),_16w=A(_14a,[_16q,_]);return _16q;},new T(function(){var _16x=E(_14L[2]);if(!_16x[0]){var _16y=E(_14O[2]);if(!_16y[0]){var _16z=E(_14R[2]);if(!_16z[0]){var _16A=E(_14U[2]);if(!_16A[0]){var _16B=E(_14X[2]);if(!_16B[0]){var _16C=E(_150[2]);if(!_16C[0]){var _16D=E(_153[2]);if(!_16D[0]){var _16E=E(_156[2]);if(!_16E[0]){var _16F=E(_159[2]);if(!_16F[0]){var _16G=E(_15c[2]);if(!_16G[0]){var _16H=E(_15f[2]);if(!_16H[0]){var _16I=E(_15i[2]);if(!_16I[0]){var _16J=E(_15l[2]);return _16J[0]==0?E(_15o[2]):E(_16J);}else{return E(_16I);}}else{return E(_16H);}}else{return E(_16G);}}else{return E(_16F);}}else{return E(_16E);}}else{return E(_16D);}}else{return E(_16C);}}else{return E(_16B);}}else{return E(_16A);}}else{return E(_16z);}}else{return E(_16y);}}else{return E(_16x);}})],_15n[2]];},_16K=new T(function(){return [0,"(function(){return document.body;})"];}),_16L=new T(function(){return _5(_16K);}),_16M=function(_){var _16N=A(_16L,[_]);return _l(_14H,[0,_16N],_);},_16O=function(_){return _16M(_);};
var hasteMain = function() {A(_16O, [0]);};window.onload = hasteMain;