(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.H.y === region.M.y)
	{
		return 'on line ' + region.H.y;
	}
	return 'on lines ' + region.H.y + ' through ' + region.M.y;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aQ,
		impl.bk,
		impl.bd,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		n: func(record.n),
		I: record.I,
		F: record.F
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.n;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.I;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.F) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aQ,
		impl.bk,
		impl.bd,
		function(sendToApp, initialModel) {
			var view = impl.bn;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aQ,
		impl.bk,
		impl.bd,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.G && impl.G(sendToApp)
			var view = impl.bn;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.az);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bg) && (_VirtualDom_doc.title = title = doc.bg);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.aW;
	var onUrlRequest = impl.aX;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		G: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.af === next.af
							&& curr.R === next.R
							&& curr.ac.a === next.ac.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		aQ: function(flags)
		{
			return A3(impl.aQ, flags, _Browser_getUrl(), key);
		},
		bn: impl.bn,
		bk: impl.bk,
		bd: impl.bd
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { aM: 'hidden', aB: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { aM: 'mozHidden', aB: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { aM: 'msHidden', aB: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { aM: 'webkitHidden', aB: 'webkitvisibilitychange' }
		: { aM: 'hidden', aB: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		al: _Browser_getScene(),
		aq: {
			as: _Browser_window.pageXOffset,
			at: _Browser_window.pageYOffset,
			ar: _Browser_doc.documentElement.clientWidth,
			Q: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		ar: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		Q: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			al: {
				ar: node.scrollWidth,
				Q: node.scrollHeight
			},
			aq: {
				as: node.scrollLeft,
				at: node.scrollTop,
				ar: node.clientWidth,
				Q: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			al: _Browser_getScene(),
			aq: {
				as: x,
				at: y,
				ar: _Browser_doc.documentElement.clientWidth,
				Q: _Browser_doc.documentElement.clientHeight
			},
			aD: {
				as: x + rect.left,
				at: y + rect.top,
				ar: rect.width,
				Q: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.aE.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.aE.b, xhr)); });
		$elm$core$Maybe$isJust(request.ap) && _Http_track(router, xhr, request.ap.a);

		try {
			xhr.open(request.aR, request.bl, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.bl));
		}

		_Http_configureRequest(xhr, request);

		request.az.a && xhr.setRequestHeader('Content-Type', request.az.a);
		xhr.send(request.az.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.P; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.bf.a || 0;
	xhr.responseType = request.aE.d;
	xhr.withCredentials = request.av;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		bl: xhr.responseURL,
		bb: xhr.status,
		bc: xhr.statusText,
		P: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			a9: event.loaded,
			am: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			a4: event.loaded,
			am: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.c);
		} else {
			var treeLen = builder.a * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.d) : builder.d;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.c);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{d: nodeList, a: (len / $elm$core$Array$branchFactor) | 0, c: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {O: fragment, R: host, aa: path, ac: port_, af: protocol, ag: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Model$BeginGamePage = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Model$initModel = function (_v0) {
	return _Utils_Tuple2(
		A4($author$project$Model$BeginGamePage, '', '', '', $elm$core$Maybe$Nothing),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Model$NoOp = {$: 12};
var $author$project$Model$ReceivedMessageType = function (a) {
	return {$: 14, a: a};
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Decoders$messageReceiver = _Platform_incomingPort('messageReceiver', $elm$json$Json$Decode$string);
var $author$project$Model$BiddingReconnectionData = F4(
	function (a, b, c, d) {
		return {$: 10, a: a, b: b, c: c, d: d};
	});
var $author$project$Model$ExistingPlayers = function (a) {
	return {$: 1, a: a};
};
var $author$project$Model$GameData = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $author$project$Model$GameFinishedData = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Model$HasQuitBidding = function (a) {
	return {$: 4, a: a};
};
var $author$project$Model$MaximumBid = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Model$NewGame = function (a) {
	return {$: 9, a: a};
};
var $author$project$Model$PlayCard = function (a) {
	return {$: 6, a: a};
};
var $author$project$Model$PlayerJoined = function (a) {
	return {$: 0, a: a};
};
var $author$project$Model$PlayerWithIdAlreadyExists = {$: 13};
var $author$project$Model$PlayerWithNameAlreadyExists = {$: 14};
var $author$project$Model$ReceivedSelectionData = function (a) {
	return {$: 5, a: a};
};
var $author$project$Model$RoundData = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Model$RoundReconnectionData = F7(
	function (a, b, c, d, e, f, g) {
		return {$: 11, a: a, b: b, c: c, d: d, e: e, f: f, g: g};
	});
var $author$project$Model$Undecided = 2;
var $author$project$Model$WebsocketFailed = {$: 12};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Model$BiddingData = F3(
	function (highestBid, highestBidder, firstBidder) {
		return {aG: firstBidder, aN: highestBid, aO: highestBidder};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Model$Player1 = 0;
var $author$project$Model$Player2 = 1;
var $author$project$Model$Player3 = 2;
var $author$project$Model$Player4 = 3;
var $author$project$Model$Player5 = 4;
var $author$project$Model$Player6 = 5;
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Decoders$playerIndexDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		switch (str) {
			case 'Player1':
				return $elm$json$Json$Decode$succeed(0);
			case 'Player2':
				return $elm$json$Json$Decode$succeed(1);
			case 'Player3':
				return $elm$json$Json$Decode$succeed(2);
			case 'Player4':
				return $elm$json$Json$Decode$succeed(3);
			case 'Player5':
				return $elm$json$Json$Decode$succeed(4);
			case 'Player6':
				return $elm$json$Json$Decode$succeed(5);
			default:
				return $elm$json$Json$Decode$fail('Unknown PlayerIndex: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Decoders$biddingDataDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Model$BiddingData,
	A2($elm$json$Json$Decode$field, 'highestBid', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'highestBidder', $author$project$Decoders$playerIndexDecoder),
	A2($elm$json$Json$Decode$field, 'firstBidder', $author$project$Decoders$playerIndexDecoder));
var $author$project$Model$Card$Card = F2(
	function (value, suit) {
		return {be: suit, bm: value};
	});
var $author$project$Model$Card$Club = 0;
var $author$project$Model$Card$Diamond = 2;
var $author$project$Model$Card$Heart = 1;
var $author$project$Model$Card$Spade = 3;
var $author$project$Decoders$suitDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		switch (str) {
			case 'Club':
				return $elm$json$Json$Decode$succeed(0);
			case 'Heart':
				return $elm$json$Json$Decode$succeed(1);
			case 'Diamond':
				return $elm$json$Json$Decode$succeed(2);
			case 'Spade':
				return $elm$json$Json$Decode$succeed(3);
			default:
				return $elm$json$Json$Decode$fail('Unknown Suit: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Model$Card$Ace = 12;
var $author$project$Model$Card$Eight = 6;
var $author$project$Model$Card$Five = 3;
var $author$project$Model$Card$Four = 2;
var $author$project$Model$Card$Jack = 9;
var $author$project$Model$Card$King = 11;
var $author$project$Model$Card$Nine = 7;
var $author$project$Model$Card$Queen = 10;
var $author$project$Model$Card$Seven = 5;
var $author$project$Model$Card$Six = 4;
var $author$project$Model$Card$Ten = 8;
var $author$project$Model$Card$Three = 1;
var $author$project$Decoders$valueDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		switch (str) {
			case 'Ace':
				return $elm$json$Json$Decode$succeed(12);
			case 'Three':
				return $elm$json$Json$Decode$succeed(1);
			case 'Four':
				return $elm$json$Json$Decode$succeed(2);
			case 'Five':
				return $elm$json$Json$Decode$succeed(3);
			case 'Six':
				return $elm$json$Json$Decode$succeed(4);
			case 'Seven':
				return $elm$json$Json$Decode$succeed(5);
			case 'Eight':
				return $elm$json$Json$Decode$succeed(6);
			case 'Nine':
				return $elm$json$Json$Decode$succeed(7);
			case 'Ten':
				return $elm$json$Json$Decode$succeed(8);
			case 'Jack':
				return $elm$json$Json$Decode$succeed(9);
			case 'Queen':
				return $elm$json$Json$Decode$succeed(10);
			case 'King':
				return $elm$json$Json$Decode$succeed(11);
			default:
				return $elm$json$Json$Decode$fail('Unknown CardValue: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Decoders$cardDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Model$Card$Card,
	A2($elm$json$Json$Decode$field, 'value', $author$project$Decoders$valueDecoder),
	A2($elm$json$Json$Decode$field, 'suit', $author$project$Decoders$suitDecoder));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Decoders$cardsDecoder = $elm$json$Json$Decode$list($author$project$Decoders$cardDecoder);
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$json$Json$Decode$map7 = _Json_map7;
var $author$project$Model$MyData = F2(
	function (myIndex, myCards) {
		return {aS: myCards, aU: myIndex};
	});
var $author$project$Decoders$myDataDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Model$MyData,
	A2($elm$json$Json$Decode$field, 'myIndex', $author$project$Decoders$playerIndexDecoder),
	A2($elm$json$Json$Decode$field, 'myCards', $author$project$Decoders$cardsDecoder));
var $author$project$Decoders$playerIndicesDecoder = $elm$json$Json$Decode$list($author$project$Decoders$playerIndexDecoder);
var $author$project$Decoders$PlayerNameSet = F6(
	function (name1, name2, name3, name4, name5, name6) {
		return {U: name1, V: name2, W: name3, X: name4, Y: name5, Z: name6};
	});
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Decoders$playerNameSetDecoder = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Decoders$PlayerNameSet,
	A2($elm$json$Json$Decode$field, 'Player1', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'Player2', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'Player3', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'Player4', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'Player5', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'Player6', $elm$json$Json$Decode$string));
var $author$project$Model$PlayerSet = F6(
	function (player1, player2, player3, player4, player5, player6) {
		return {aZ: player1, a_: player2, a$: player3, a0: player4, a1: player5, a2: player6};
	});
var $author$project$Model$Player = F5(
	function (totalScore, gameScore, name, card, status) {
		return {aA: card, aJ: gameScore, aV: name, ba: status, bh: totalScore};
	});
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Model$AntiTeam = 1;
var $author$project$Model$BiddingTeam = 0;
var $author$project$Decoders$playerStatusDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		switch (str) {
			case 'BiddingTeam':
				return $elm$json$Json$Decode$succeed(0);
			case 'AntiTeam':
				return $elm$json$Json$Decode$succeed(1);
			case 'Undecided':
				return $elm$json$Json$Decode$succeed(2);
			default:
				return $elm$json$Json$Decode$fail('Unknown PlayerStatus: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Decoders$playerDecoder = A6(
	$elm$json$Json$Decode$map5,
	$author$project$Model$Player,
	A2($elm$json$Json$Decode$field, 'totalScore', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'gameScore', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'card',
		$elm$json$Json$Decode$nullable($author$project$Decoders$cardDecoder)),
	A2($elm$json$Json$Decode$field, 'status', $author$project$Decoders$playerStatusDecoder));
var $author$project$Decoders$playerSetDecoder = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Model$PlayerSet,
	A2($elm$json$Json$Decode$field, 'Player1', $author$project$Decoders$playerDecoder),
	A2($elm$json$Json$Decode$field, 'Player2', $author$project$Decoders$playerDecoder),
	A2($elm$json$Json$Decode$field, 'Player3', $author$project$Decoders$playerDecoder),
	A2($elm$json$Json$Decode$field, 'Player4', $author$project$Decoders$playerDecoder),
	A2($elm$json$Json$Decode$field, 'Player5', $author$project$Decoders$playerDecoder),
	A2($elm$json$Json$Decode$field, 'Player6', $author$project$Decoders$playerDecoder));
var $author$project$Model$Round1 = 0;
var $author$project$Model$Round2 = 1;
var $author$project$Model$Round3 = 2;
var $author$project$Model$Round4 = 3;
var $author$project$Model$Round5 = 4;
var $author$project$Model$Round6 = 5;
var $author$project$Model$Round7 = 6;
var $author$project$Model$Round8 = 7;
var $author$project$Decoders$roundDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		switch (str) {
			case 'Round1':
				return $elm$json$Json$Decode$succeed(0);
			case 'Round2':
				return $elm$json$Json$Decode$succeed(1);
			case 'Round3':
				return $elm$json$Json$Decode$succeed(2);
			case 'Round4':
				return $elm$json$Json$Decode$succeed(3);
			case 'Round5':
				return $elm$json$Json$Decode$succeed(4);
			case 'Round6':
				return $elm$json$Json$Decode$succeed(5);
			case 'Round7':
				return $elm$json$Json$Decode$succeed(6);
			case 'Round8':
				return $elm$json$Json$Decode$succeed(7);
			default:
				return $elm$json$Json$Decode$fail('Unknown Round: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Model$SelectionData = F2(
	function (trump, helpers) {
		return {aK: helpers, bi: trump};
	});
var $author$project$Decoders$selectionDataDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Model$SelectionData,
	A2($elm$json$Json$Decode$field, 'trump', $author$project$Decoders$suitDecoder),
	A2(
		$elm$json$Json$Decode$field,
		'helpers',
		$elm$json$Json$Decode$list($author$project$Decoders$cardDecoder)));
var $author$project$Decoders$receivedDataDecoder = function () {
	var playerWithName = function (name) {
		return {aA: $elm$core$Maybe$Nothing, aJ: 0, aV: name, ba: 2, bh: 0};
	};
	var playerNameSetToPlayerSet = function (playerNameSet) {
		return $elm$json$Json$Decode$succeed(
			{
				aZ: playerWithName(playerNameSet.U),
				a_: playerWithName(playerNameSet.V),
				a$: playerWithName(playerNameSet.W),
				a0: playerWithName(playerNameSet.X),
				a1: playerWithName(playerNameSet.Y),
				a2: playerWithName(playerNameSet.Z)
			});
	};
	return A2(
		$elm$json$Json$Decode$andThen,
		function (tag) {
			switch (tag) {
				case 'PlayerJoined':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Model$PlayerJoined,
						A2($elm$json$Json$Decode$field, 'newPlayer', $elm$json$Json$Decode$string));
				case 'ExistingPlayers':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Model$ExistingPlayers,
						A2(
							$elm$json$Json$Decode$field,
							'existingPlayers',
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
				case 'GameData':
					return A5(
						$elm$json$Json$Decode$map4,
						$author$project$Model$GameData,
						A2(
							$elm$json$Json$Decode$field,
							'playerNames',
							A2($elm$json$Json$Decode$andThen, playerNameSetToPlayerSet, $author$project$Decoders$playerNameSetDecoder)),
						A2($elm$json$Json$Decode$field, 'firstBidder', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'myIndex', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'myCards', $author$project$Decoders$cardsDecoder));
				case 'MaximumBid':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Model$MaximumBid,
						A2($elm$json$Json$Decode$field, 'highestBidder', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'highestBid', $elm$json$Json$Decode$int));
				case 'HasQuitBidding':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Model$HasQuitBidding,
						A2($elm$json$Json$Decode$field, 'hasQuitBidding', $author$project$Decoders$playerIndexDecoder));
				case 'SelectionData':
					return A2(
						$elm$json$Json$Decode$andThen,
						A2($elm$core$Basics$composeR, $author$project$Model$ReceivedSelectionData, $elm$json$Json$Decode$succeed),
						$author$project$Decoders$selectionDataDecoder);
				case 'PlayCard':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Model$PlayCard,
						A2($elm$json$Json$Decode$field, 'card', $author$project$Decoders$cardDecoder));
				case 'RoundData':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Model$RoundData,
						A2($elm$json$Json$Decode$field, 'roundWinner', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'roundScore', $elm$json$Json$Decode$int));
				case 'GameFinishedData':
					return A3(
						$elm$json$Json$Decode$map2,
						$author$project$Model$GameFinishedData,
						A2(
							$elm$json$Json$Decode$field,
							'winningTeam',
							$elm$json$Json$Decode$list($author$project$Decoders$playerIndexDecoder)),
						A2($elm$json$Json$Decode$field, 'gameScore', $elm$json$Json$Decode$int));
				case 'NewGame':
					return A2(
						$elm$json$Json$Decode$map,
						$author$project$Model$NewGame,
						A2($elm$json$Json$Decode$field, 'cards', $author$project$Decoders$cardsDecoder));
				case 'BiddingReconnectionData':
					return A5(
						$elm$json$Json$Decode$map4,
						$author$project$Model$BiddingReconnectionData,
						A2($elm$json$Json$Decode$field, 'playerSet', $author$project$Decoders$playerSetDecoder),
						A2($elm$json$Json$Decode$field, 'biddingData', $author$project$Decoders$biddingDataDecoder),
						A2($elm$json$Json$Decode$field, 'myData', $author$project$Decoders$myDataDecoder),
						A2($elm$json$Json$Decode$field, 'bidders', $author$project$Decoders$playerIndicesDecoder));
				case 'RoundReconnectionData':
					return A8(
						$elm$json$Json$Decode$map7,
						$author$project$Model$RoundReconnectionData,
						A2($elm$json$Json$Decode$field, 'playerSet', $author$project$Decoders$playerSetDecoder),
						A2($elm$json$Json$Decode$field, 'biddingData', $author$project$Decoders$biddingDataDecoder),
						A2($elm$json$Json$Decode$field, 'myData', $author$project$Decoders$myDataDecoder),
						A2($elm$json$Json$Decode$field, 'selectionData', $author$project$Decoders$selectionDataDecoder),
						A2($elm$json$Json$Decode$field, 'firstPlayer', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'turn', $author$project$Decoders$playerIndexDecoder),
						A2($elm$json$Json$Decode$field, 'round', $author$project$Decoders$roundDecoder));
				case 'WebsocketFailed':
					return $elm$json$Json$Decode$succeed($author$project$Model$WebsocketFailed);
				case 'PlayerWithIdAlreadyExists':
					return $elm$json$Json$Decode$succeed($author$project$Model$PlayerWithIdAlreadyExists);
				case 'PlayerWithNameAlreadyExists':
					return $elm$json$Json$Decode$succeed($author$project$Model$PlayerWithNameAlreadyExists);
				default:
					return $elm$json$Json$Decode$fail('Unknown tag received: ' + tag);
			}
		},
		A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string));
}();
var $author$project$Decoders$receiveMessage = $author$project$Decoders$messageReceiver(
	function (str) {
		var _v0 = A2($elm$json$Json$Decode$decodeString, $author$project$Decoders$receivedDataDecoder, str);
		if (!_v0.$) {
			var receivedMessage = _v0.a;
			return $author$project$Model$ReceivedMessageType(receivedMessage);
		} else {
			var error = _v0.a;
			return $author$project$Model$NoOp;
		}
	});
var $author$project$Subscriptions$subscriptions = function (_v0) {
	return $author$project$Decoders$receiveMessage;
};
var $author$project$Model$BiddingRound = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Model$EmptyGameName = 2;
var $author$project$Model$EmptyId = 0;
var $author$project$Model$EmptyName = 1;
var $author$project$Model$FirstAndMyTurnOver = {$: 3};
var $author$project$Model$HttpDataType = function (a) {
	return {$: 15, a: a};
};
var $author$project$Model$IntroData = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Model$NotFirstAndMyTurnOver = function (a) {
	return {$: 5, a: a};
};
var $author$project$Model$PlayRound = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Model$PlayedCard = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Model$SendQuit = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Model$SentSelectionData = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Model$Analytics$TotalDataReceived = $elm$core$Basics$identity;
var $author$project$Model$TrumpSelection = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Model$WaitingForServerValidation = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.bb));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {ai: reqs, an: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.ap;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.ai));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.an)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					av: r.av,
					az: r.az,
					aE: A2(_Http_mapExpect, func, r.aE),
					P: r.P,
					aR: r.aR,
					bf: r.bf,
					ap: r.ap,
					bl: r.bl
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{av: false, az: r.az, aE: r.aE, P: r.P, aR: r.aR, bf: r.bf, ap: r.ap, bl: r.bl}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{az: $elm$http$Http$emptyBody, aE: r.aE, P: _List_Nil, aR: 'GET', bf: $elm$core$Maybe$Nothing, ap: $elm$core$Maybe$Nothing, bl: r.bl});
};
var $author$project$Model$AnalyticsPage = function (a) {
	return {$: 1, a: a};
};
var $author$project$Model$Analytics$TotalMode = $elm$core$Basics$identity;
var $author$project$Update$Analytics$handleHttpData = F2(
	function (httpData, model) {
		var playerScoreDataList = httpData;
		return _Utils_Tuple2(
			$author$project$Model$AnalyticsPage(playerScoreDataList),
			$elm$core$Platform$Cmd$none);
	});
var $author$project$Model$DuplicateId = 3;
var $author$project$Model$DuplicateName = 4;
var $author$project$Model$ErrorState = {$: 8};
var $author$project$Model$FirstAndMyTurn = {$: 2};
var $author$project$Model$FirstAndNotMyTurn = function (a) {
	return {$: 0, a: a};
};
var $author$project$Model$GameFinished = {$: 7};
var $author$project$Model$NotFirstAndMyTurn = function (a) {
	return {$: 4, a: a};
};
var $author$project$Model$NotFirstAndNotMyTurn = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Model$RoundFinished = {$: 6};
var $author$project$Model$WaitingForPlayers = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Model$WaitingForTrump = function (a) {
	return {$: 6, a: a};
};
var $author$project$Model$allPlayerIndices = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Model$getPlayers = F2(
	function (f, players) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				0,
				f(players.aZ)),
				_Utils_Tuple2(
				1,
				f(players.a_)),
				_Utils_Tuple2(
				2,
				f(players.a$)),
				_Utils_Tuple2(
				3,
				f(players.a0)),
				_Utils_Tuple2(
				4,
				f(players.a1)),
				_Utils_Tuple2(
				5,
				f(players.a2))
			]);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Update$calculateHelpersRevealed = F2(
	function (playerSet, selectionData) {
		var players = A2(
			$author$project$Model$getPlayers,
			function ($) {
				return $.ba;
			},
			playerSet);
		var hasTeamBeenRevealed = A2(
			$elm$core$List$all,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$second,
				$elm$core$Basics$neq(2)),
			players);
		var biddingTeam = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Tuple$second,
				$elm$core$Basics$eq(0)),
			players);
		return hasTeamBeenRevealed ? $elm$core$List$length(selectionData.aK) : ($elm$core$List$length(biddingTeam) - 1);
	});
var $author$project$Model$getPlayer = F2(
	function (players, index) {
		switch (index) {
			case 0:
				return players.aZ;
			case 1:
				return players.a_;
			case 2:
				return players.a$;
			case 3:
				return players.a0;
			case 4:
				return players.a1;
			default:
				return players.a2;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Update$calculateTurnStatus = F4(
	function (turn, firstPlayer, myIndex, playerSet) {
		var baseCard = A2(
			$elm$core$Maybe$withDefault,
			A2($author$project$Model$Card$Card, 12, 3),
			A2($author$project$Model$getPlayer, playerSet, firstPlayer).aA);
		return _Utils_eq(myIndex, firstPlayer) ? (_Utils_eq(myIndex, turn) ? $author$project$Model$FirstAndMyTurn : $author$project$Model$FirstAndNotMyTurn(turn)) : (_Utils_eq(myIndex, turn) ? $author$project$Model$NotFirstAndMyTurn(baseCard) : A2($author$project$Model$NotFirstAndNotMyTurn, turn, baseCard));
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Model$amIHelper = F2(
	function (myCards, selectionData) {
		return A2(
			$elm$core$List$any,
			function (h) {
				return A2($elm$core$List$member, h, myCards);
			},
			selectionData.aK);
	});
var $author$project$Model$amITheOnlyHelper = F2(
	function (myCards, selectionData) {
		var _v0 = selectionData.aK;
		_v0$2:
		while (true) {
			if (_v0.b) {
				if (!_v0.b.b) {
					var card = _v0.a;
					return A2($elm$core$List$member, card, myCards);
				} else {
					if (!_v0.b.b.b) {
						var card1 = _v0.a;
						var _v1 = _v0.b;
						var card2 = _v1.a;
						return A2($elm$core$List$member, card1, myCards) && A2($elm$core$List$member, card2, myCards);
					} else {
						break _v0$2;
					}
				}
			} else {
				break _v0$2;
			}
		}
		return false;
	});
var $author$project$Model$maxHelpers = function (selectionData) {
	return $elm$core$List$length(selectionData.aK);
};
var $author$project$Model$updatePlayer = F3(
	function (playerIndex, update, players) {
		switch (playerIndex) {
			case 0:
				return _Utils_update(
					players,
					{
						aZ: update(players.aZ)
					});
			case 1:
				return _Utils_update(
					players,
					{
						a_: update(players.a_)
					});
			case 2:
				return _Utils_update(
					players,
					{
						a$: update(players.a$)
					});
			case 3:
				return _Utils_update(
					players,
					{
						a0: update(players.a0)
					});
			case 4:
				return _Utils_update(
					players,
					{
						a1: update(players.a1)
					});
			default:
				return _Utils_update(
					players,
					{
						a2: update(players.a2)
					});
		}
	});
var $author$project$Model$updatePlayerStatus = F2(
	function (playerIndex, status) {
		return A2(
			$author$project$Model$updatePlayer,
			playerIndex,
			function (player) {
				return _Utils_update(
					player,
					{ba: status});
			});
	});
var $author$project$Update$getPlayersStatus = F4(
	function (myData, winnerIndex, selectionData, playerSet) {
		var newPlayerSet = A3($author$project$Model$updatePlayerStatus, winnerIndex, 0, playerSet);
		var allAntiStatus = A3(
			$elm$core$List$foldl,
			F2(
				function (p, pss) {
					return A3($author$project$Model$updatePlayerStatus, p, 1, pss);
				}),
			newPlayerSet,
			A2(
				$elm$core$List$filter,
				$elm$core$Basics$neq(winnerIndex),
				$author$project$Model$allPlayerIndices));
		return (!$author$project$Model$maxHelpers(selectionData)) ? _Utils_Tuple2(allAntiStatus, 0) : (_Utils_eq(myData.aU, winnerIndex) ? _Utils_Tuple2(newPlayerSet, 0) : (A2($author$project$Model$amITheOnlyHelper, myData.aS, selectionData) ? _Utils_Tuple2(
			A3($author$project$Model$updatePlayerStatus, myData.aU, 0, allAntiStatus),
			$author$project$Model$maxHelpers(selectionData)) : (A2($author$project$Model$amIHelper, myData.aS, selectionData) ? _Utils_Tuple2(
			A3($author$project$Model$updatePlayerStatus, myData.aU, 0, newPlayerSet),
			1) : _Utils_Tuple2(
			A3($author$project$Model$updatePlayerStatus, myData.aU, 1, newPlayerSet),
			0))));
	});
var $author$project$Model$isPlayerHelper = F2(
	function (card, selectionData) {
		return A2($elm$core$List$member, card, selectionData.aK);
	});
var $author$project$Model$nextRound = function (round) {
	switch (round) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		default:
			return 0;
	}
};
var $author$project$Model$nextTurn = function (playerIndex) {
	switch (playerIndex) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		default:
			return 0;
	}
};
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Model$updateCardInSet = F2(
	function (playerIndex, card) {
		return A2(
			$author$project$Model$updatePlayer,
			playerIndex,
			function (player) {
				return _Utils_update(
					player,
					{
						aA: $elm$core$Maybe$Just(card)
					});
			});
	});
var $author$project$Update$handleReceivedMessages = F2(
	function (receivedMessage, model) {
		switch (receivedMessage.$) {
			case 0:
				var newPlayer = receivedMessage.a;
				if (model.$ === 3) {
					var players = model.a;
					var gameName = model.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$WaitingForPlayers,
							_Utils_ap(
								players,
								_List_fromArray(
									[newPlayer])),
							gameName),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 1:
				var existingPlayers = receivedMessage.a;
				if (model.$ === 2) {
					var playerId = model.a;
					var playerName = model.b;
					var gameName = model.c;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$WaitingForPlayers,
							_Utils_ap(
								existingPlayers,
								_List_fromArray(
									[playerName])),
							gameName),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 2:
				var playerSet = receivedMessage.a;
				var firstBidder = receivedMessage.b;
				var myIndex = receivedMessage.c;
				var myCards = receivedMessage.d;
				if (model.$ === 3) {
					var players = model.a;
					var gameName = model.b;
					var commonData = {
						ax: {aG: firstBidder, aN: 150, aO: firstBidder},
						aI: gameName,
						aT: {aS: myCards, aU: myIndex},
						a3: playerSet
					};
					return _Utils_Tuple2(
						A2($author$project$Model$BiddingRound, commonData, $author$project$Model$allPlayerIndices),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 3:
				var bidder = receivedMessage.a;
				var bid = receivedMessage.b;
				if (model.$ === 4) {
					var commonData = model.a;
					var bidders = model.b;
					var updateBiddingData = function (biddingData) {
						return _Utils_update(
							biddingData,
							{aN: bid, aO: bidder});
					};
					return _Utils_Tuple2(
						A2(
							$author$project$Model$BiddingRound,
							_Utils_update(
								commonData,
								{
									ax: updateBiddingData(commonData.ax)
								}),
							bidders),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 4:
				var quitter = receivedMessage.a;
				if (model.$ === 4) {
					var commonData = model.a;
					var bidders = model.b;
					var newBidders = A2(
						$elm$core$List$filter,
						$elm$core$Basics$neq(quitter),
						bidders);
					return (!$elm$core$List$length(newBidders)) ? (_Utils_eq(commonData.aT.aU, commonData.ax.aO) ? _Utils_Tuple2(
						A2(
							$author$project$Model$TrumpSelection,
							commonData,
							{aK: _List_Nil, bi: 3}),
						$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						$author$project$Model$WaitingForTrump(commonData),
						$elm$core$Platform$Cmd$none)) : _Utils_Tuple2(
						A2($author$project$Model$BiddingRound, commonData, newBidders),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 5:
				var selectionData = receivedMessage.a;
				switch (model.$) {
					case 5:
						var commonData = model.a;
						var firstBidder = commonData.ax.aG;
						var _v7 = A4($author$project$Update$getPlayersStatus, commonData.aT, commonData.aT.aU, selectionData, commonData.a3);
						var playerSet = _v7.a;
						var helpersRevealed = _v7.b;
						return _Utils_Tuple2(
							A2(
								$author$project$Model$PlayRound,
								_Utils_update(
									commonData,
									{a3: playerSet}),
								{
									aH: firstBidder,
									aL: helpersRevealed,
									a6: 0,
									a8: selectionData,
									bj: _Utils_eq(firstBidder, commonData.aT.aU) ? $author$project$Model$FirstAndMyTurn : $author$project$Model$FirstAndNotMyTurn(firstBidder)
								}),
							$elm$core$Platform$Cmd$none);
					case 6:
						var commonData = model.a;
						var firstBidder = commonData.ax.aG;
						var _v8 = A4($author$project$Update$getPlayersStatus, commonData.aT, commonData.ax.aO, selectionData, commonData.a3);
						var playerSet = _v8.a;
						var helpersRevealed = _v8.b;
						return _Utils_Tuple2(
							A2(
								$author$project$Model$PlayRound,
								_Utils_update(
									commonData,
									{a3: playerSet}),
								{
									aH: firstBidder,
									aL: helpersRevealed,
									a6: 0,
									a8: selectionData,
									bj: _Utils_eq(firstBidder, commonData.aT.aU) ? $author$project$Model$FirstAndMyTurn : $author$project$Model$FirstAndNotMyTurn(firstBidder)
								}),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 6:
				var card = receivedMessage.a;
				if (model.$ === 7) {
					var commonData = model.a;
					var playRoundData = model.b;
					var updateMyData = function (myData) {
						return _Utils_update(
							myData,
							{
								aS: A2(
									$elm$core$List$filter,
									$elm$core$Basics$neq(card),
									myData.aS)
							});
					};
					var myIndex = commonData.aT.aU;
					var hadTeamBeenRevealed = _Utils_eq(
						playRoundData.aL,
						$author$project$Model$maxHelpers(playRoundData.a8));
					var _v10 = function () {
						var _v11 = playRoundData.bj;
						switch (_v11.$) {
							case 0:
								var firstPlayer = _v11.a;
								return _Utils_Tuple2(
									_Utils_eq(
										$author$project$Model$nextTurn(firstPlayer),
										myIndex) ? $author$project$Model$NotFirstAndMyTurn(card) : A2(
										$author$project$Model$NotFirstAndNotMyTurn,
										$author$project$Model$nextTurn(firstPlayer),
										card),
									firstPlayer);
							case 1:
								var player = _v11.a;
								var baseCard = _v11.b;
								return _Utils_Tuple2(
									_Utils_eq(
										$author$project$Model$nextTurn(player),
										playRoundData.aH) ? $author$project$Model$RoundFinished : (_Utils_eq(
										$author$project$Model$nextTurn(player),
										myIndex) ? $author$project$Model$NotFirstAndMyTurn(baseCard) : A2(
										$author$project$Model$NotFirstAndNotMyTurn,
										$author$project$Model$nextTurn(player),
										baseCard)),
									player);
							case 3:
								return _Utils_Tuple2(
									A2(
										$author$project$Model$NotFirstAndNotMyTurn,
										$author$project$Model$nextTurn(myIndex),
										card),
									myIndex);
							case 5:
								var baseCard = _v11.a;
								return _Utils_Tuple2(
									_Utils_eq(
										$author$project$Model$nextTurn(myIndex),
										playRoundData.aH) ? $author$project$Model$RoundFinished : A2(
										$author$project$Model$NotFirstAndNotMyTurn,
										$author$project$Model$nextTurn(myIndex),
										baseCard),
									myIndex);
							default:
								return _Utils_Tuple2(playRoundData.bj, 0);
						}
					}();
					var newTurnStatus = _v10.a;
					var oldTurn = _v10.b;
					var updatePlayerSet = function (oldSet) {
						if (_Utils_eq(oldTurn, commonData.aT.aU) || hadTeamBeenRevealed) {
							return _Utils_Tuple2(oldSet, playRoundData.aL);
						} else {
							if (A2($author$project$Model$isPlayerHelper, card, playRoundData.a8)) {
								var newSet = A3($author$project$Model$updatePlayerStatus, oldTurn, 0, oldSet);
								var newHelpersRevealed = playRoundData.aL + 1;
								var hasTeamBeenRevealed = _Utils_eq(
									newHelpersRevealed,
									$author$project$Model$maxHelpers(playRoundData.a8));
								return hasTeamBeenRevealed ? function (s) {
									return A2($elm$core$Tuple$pair, s, newHelpersRevealed);
								}(
									A3(
										$elm$core$List$foldl,
										F2(
											function (p, pss) {
												return A3($author$project$Model$updatePlayerStatus, p, 1, pss);
											}),
										newSet,
										A2(
											$elm$core$List$map,
											$elm$core$Tuple$first,
											A2(
												$elm$core$List$filter,
												A2(
													$elm$core$Basics$composeR,
													$elm$core$Tuple$second,
													$elm$core$Basics$neq(0)),
												A2(
													$author$project$Model$getPlayers,
													function ($) {
														return $.ba;
													},
													newSet))))) : _Utils_Tuple2(newSet, newHelpersRevealed);
							} else {
								return _Utils_Tuple2(oldSet, playRoundData.aL);
							}
						}
					};
					var _v12 = updatePlayerSet(commonData.a3);
					var newerSet = _v12.a;
					var newerHelpersRevealed = _v12.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$PlayRound,
							_Utils_update(
								commonData,
								{
									aT: updateMyData(commonData.aT),
									a3: A3($author$project$Model$updateCardInSet, oldTurn, card, newerSet)
								}),
							_Utils_update(
								playRoundData,
								{aL: newerHelpersRevealed, bj: newTurnStatus})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 7:
				var winner = receivedMessage.a;
				var score = receivedMessage.b;
				if (model.$ === 7) {
					var commonData = model.a;
					var playRoundData = model.b;
					var newRound = $author$project$Model$nextRound(playRoundData.a6);
					var newPlayerSet = A3(
						$elm$core$List$foldr,
						F2(
							function (playerIndex, playerSet) {
								return A3(
									$author$project$Model$updatePlayer,
									playerIndex,
									function (player) {
										return _Utils_update(
											player,
											{
												aA: $elm$core$Maybe$Nothing,
												aJ: _Utils_eq(playerIndex, winner) ? (player.aJ + score) : player.aJ
											});
									},
									playerSet);
							}),
						commonData.a3,
						$author$project$Model$allPlayerIndices);
					return _Utils_Tuple2(
						A2(
							$author$project$Model$PlayRound,
							_Utils_update(
								commonData,
								{a3: newPlayerSet}),
							_Utils_update(
								playRoundData,
								{
									aH: winner,
									a6: newRound,
									bj: (!newRound) ? $author$project$Model$GameFinished : (_Utils_eq(winner, commonData.aT.aU) ? $author$project$Model$FirstAndMyTurn : $author$project$Model$FirstAndNotMyTurn(winner))
								})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 8:
				var winningTeam = receivedMessage.a;
				var totalScore = receivedMessage.b;
				if (model.$ === 7) {
					var commonData = model.a;
					var playRoundData = model.b;
					var updatedPlayerScores = A3(
						$elm$core$List$foldl,
						F2(
							function (playerIndex, playerSet) {
								return A3(
									$author$project$Model$updatePlayer,
									playerIndex,
									function (player) {
										return _Utils_update(
											player,
											{
												aJ: 0,
												ba: 2,
												bh: player.bh + (A2($elm$core$List$member, playerIndex, winningTeam) ? totalScore : 0)
											});
									},
									playerSet);
							}),
						commonData.a3,
						$author$project$Model$allPlayerIndices);
					return _Utils_Tuple2(
						A2(
							$author$project$Model$PlayRound,
							_Utils_update(
								commonData,
								{a3: updatedPlayerScores}),
							playRoundData),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 9:
				var cards = receivedMessage.a;
				if (model.$ === 7) {
					var commonData = model.a;
					var playRoundData = model.b;
					var nextFirstBidder = $author$project$Model$nextTurn(commonData.ax.aG);
					return _Utils_Tuple2(
						A2(
							$author$project$Model$BiddingRound,
							_Utils_update(
								commonData,
								{
									ax: {aG: nextFirstBidder, aN: 150, aO: nextFirstBidder},
									aT: {aS: cards, aU: commonData.aT.aU}
								}),
							$author$project$Model$allPlayerIndices),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 10:
				var playerSet = receivedMessage.a;
				var biddingData = receivedMessage.b;
				var myData = receivedMessage.c;
				var bidders = receivedMessage.d;
				if (model.$ === 2) {
					var gameName = model.c;
					return (!$elm$core$List$length(bidders)) ? (_Utils_eq(biddingData.aO, myData.aU) ? _Utils_Tuple2(
						A2(
							$author$project$Model$TrumpSelection,
							{ax: biddingData, aI: gameName, aT: myData, a3: playerSet},
							{aK: _List_Nil, bi: 3}),
						$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						$author$project$Model$WaitingForTrump(
							{ax: biddingData, aI: gameName, aT: myData, a3: playerSet}),
						$elm$core$Platform$Cmd$none)) : _Utils_Tuple2(
						A2(
							$author$project$Model$BiddingRound,
							{ax: biddingData, aI: gameName, aT: myData, a3: playerSet},
							bidders),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 11:
				var playerSet = receivedMessage.a;
				var biddingData = receivedMessage.b;
				var myData = receivedMessage.c;
				var selectionData = receivedMessage.d;
				var firstPlayer = receivedMessage.e;
				var turn = receivedMessage.f;
				var round = receivedMessage.g;
				if (model.$ === 2) {
					var gameName = model.c;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$PlayRound,
							{ax: biddingData, aI: gameName, aT: myData, a3: playerSet},
							{
								aH: firstPlayer,
								aL: A2($author$project$Update$calculateHelpersRevealed, playerSet, selectionData),
								a6: round,
								a8: selectionData,
								bj: A4($author$project$Update$calculateTurnStatus, turn, firstPlayer, myData.aU, playerSet)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 12:
				return _Utils_Tuple2($author$project$Model$ErrorState, $elm$core$Platform$Cmd$none);
			case 13:
				if (model.$ === 2) {
					var playerId = model.a;
					var playerName = model.b;
					var gameName = model.c;
					return _Utils_Tuple2(
						A4(
							$author$project$Model$BeginGamePage,
							playerId,
							playerName,
							gameName,
							$elm$core$Maybe$Just(3)),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			default:
				if (model.$ === 2) {
					var playerId = model.a;
					var playerName = model.b;
					var gameName = model.c;
					return _Utils_Tuple2(
						A4(
							$author$project$Model$BeginGamePage,
							playerId,
							playerName,
							gameName,
							$elm$core$Maybe$Just(4)),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Model$IncreaseBid = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Encoders$messageSender = _Platform_outgoingPort('messageSender', $elm$json$Json$Encode$string);
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $author$project$Model$showPlayerIndex = function (playerIndex) {
	switch (playerIndex) {
		case 0:
			return 'Player1';
		case 1:
			return 'Player2';
		case 2:
			return 'Player3';
		case 3:
			return 'Player4';
		case 4:
			return 'Player5';
		default:
			return 'Player6';
	}
};
var $author$project$Encoders$increaseBidEncoder = F3(
	function (gameName, bidder, bid) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'gameName',
					$elm$json$Json$Encode$string(gameName)),
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('IncreaseBid')),
								_Utils_Tuple2(
								'bidder',
								$elm$json$Json$Encode$string(
									$author$project$Model$showPlayerIndex(bidder))),
								_Utils_Tuple2(
								'bid',
								$elm$json$Json$Encode$int(bid))
							])))
				]));
	});
var $author$project$Encoders$introDataEncoder = F3(
	function (playerId, playerName, gameName) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'gameName',
					$elm$json$Json$Encode$string(gameName)),
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('IntroData')),
								_Utils_Tuple2(
								'playerId',
								$elm$json$Json$Encode$string(playerId)),
								_Utils_Tuple2(
								'playerName',
								$elm$json$Json$Encode$string(playerName))
							])))
				]));
	});
var $author$project$Model$Card$showCardValue = function (cardValue) {
	switch (cardValue) {
		case 12:
			return 'Ace';
		case 0:
			return 'Two';
		case 1:
			return 'Three';
		case 2:
			return 'Four';
		case 3:
			return 'Five';
		case 4:
			return 'Six';
		case 5:
			return 'Seven';
		case 6:
			return 'Eight';
		case 7:
			return 'Nine';
		case 8:
			return 'Ten';
		case 9:
			return 'Jack';
		case 10:
			return 'Queen';
		default:
			return 'King';
	}
};
var $author$project$Model$Card$showSuit = F2(
	function (isPlural, suit) {
		var suitStr = function () {
			switch (suit) {
				case 0:
					return 'Club';
				case 1:
					return 'Heart';
				case 2:
					return 'Diamond';
				default:
					return 'Spade';
			}
		}();
		return _Utils_ap(
			suitStr,
			isPlural ? 's' : '');
	});
var $author$project$Encoders$cardEncoder = function (card) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'value',
				$elm$json$Json$Encode$string(
					$author$project$Model$Card$showCardValue(card.bm))),
				_Utils_Tuple2(
				'suit',
				$elm$json$Json$Encode$string(
					A2($author$project$Model$Card$showSuit, false, card.be)))
			]));
};
var $author$project$Encoders$playedCardEncoder = F2(
	function (gameName, card) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'gameName',
					$elm$json$Json$Encode$string(gameName)),
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('PlayedCard')),
								_Utils_Tuple2(
								'playedCard',
								$author$project$Encoders$cardEncoder(card))
							])))
				]));
	});
var $author$project$Encoders$quitBiddingEncoder = F2(
	function (gameName, myIndex) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'gameName',
					$elm$json$Json$Encode$string(gameName)),
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('QuitBidding')),
								_Utils_Tuple2(
								'quitter',
								$elm$json$Json$Encode$string(
									$author$project$Model$showPlayerIndex(myIndex)))
							])))
				]));
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$Encoders$selectionDataEncoder = F3(
	function (gameName, suit, helpers) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'gameName',
					$elm$json$Json$Encode$string(gameName)),
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('SelectionData')),
								_Utils_Tuple2(
								'trump',
								$elm$json$Json$Encode$string(
									A2($author$project$Model$Card$showSuit, false, suit))),
								_Utils_Tuple2(
								'helpers',
								A2($elm$json$Json$Encode$list, $author$project$Encoders$cardEncoder, helpers))
							])))
				]));
	});
var $author$project$Encoders$sentDataEncoder = function (sentData) {
	switch (sentData.$) {
		case 0:
			var playerId = sentData.a;
			var playerName = sentData.b;
			var gameName = sentData.c;
			return A3($author$project$Encoders$introDataEncoder, playerId, playerName, gameName);
		case 1:
			var gameName = sentData.a;
			var bidder = sentData.b;
			var bid = sentData.c;
			return A3($author$project$Encoders$increaseBidEncoder, gameName, bidder, bid);
		case 2:
			var gameName = sentData.a;
			var myIndex = sentData.b;
			return A2($author$project$Encoders$quitBiddingEncoder, gameName, myIndex);
		case 3:
			var gameName = sentData.a;
			var selectionData = sentData.b;
			return A3($author$project$Encoders$selectionDataEncoder, gameName, selectionData.bi, selectionData.aK);
		default:
			var gameName = sentData.a;
			var card = sentData.b;
			return A2($author$project$Encoders$playedCardEncoder, gameName, card);
	}
};
var $author$project$Encoders$sendMessage = function (sentData) {
	return $author$project$Encoders$messageSender(
		A2(
			$elm$json$Json$Encode$encode,
			0,
			$author$project$Encoders$sentDataEncoder(sentData)));
};
var $author$project$Update$sendIncreasedBidMessage = F2(
	function (model, delta) {
		if (model.$ === 4) {
			var commonData = model.a;
			var bidders = model.b;
			var newBid = commonData.ax.aN + delta;
			return _Utils_Tuple2(
				(newBid >= 250) ? A2($author$project$Model$BiddingRound, commonData, _List_Nil) : model,
				$author$project$Encoders$sendMessage(
					A3(
						$author$project$Model$IncreaseBid,
						commonData.aI,
						commonData.aT.aU,
						A2($elm$core$Basics$min, newBid, 250))));
		} else {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Model$Analytics$PlayerScoreData = F3(
	function (score, name, bids) {
		return {ay: bids, aV: name, a7: score};
	});
var $author$project$Decoders$playerScoreDataDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Model$Analytics$PlayerScoreData,
	A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'bids', $elm$json$Json$Decode$int));
var $author$project$Decoders$totalDataDecoder = $elm$json$Json$Decode$list($author$project$Decoders$playerScoreDataDecoder);
var $author$project$Update$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 2:
				var str = msg.a;
				if (!model.$) {
					var playerId = model.a;
					var playerName = model.b;
					var validation = model.d;
					return _Utils_Tuple2(
						A4($author$project$Model$BeginGamePage, playerId, playerName, str, validation),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 1:
				var str = msg.a;
				if (!model.$) {
					var playerId = model.a;
					var gameName = model.c;
					var validation = model.d;
					return _Utils_Tuple2(
						A4($author$project$Model$BeginGamePage, playerId, str, gameName, validation),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 0:
				var str = msg.a;
				if (!model.$) {
					var playerName = model.b;
					var gameName = model.c;
					var validation = model.d;
					return _Utils_Tuple2(
						A4($author$project$Model$BeginGamePage, str, playerName, gameName, validation),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 3:
				var errorHandler = function (result) {
					if (!result.$) {
						var val = result.a;
						return $author$project$Model$HttpDataType(val);
					} else {
						var err = result.a;
						return $author$project$Model$NoOp;
					}
				};
				if (!model.$) {
					var playerId = model.a;
					var playerName = model.b;
					var gameName = model.c;
					var validation = model.d;
					return _Utils_Tuple2(
						A4($author$project$Model$BeginGamePage, playerId, playerName, gameName, validation),
						$elm$http$Http$get(
							{
								aE: A2($elm$http$Http$expectJson, errorHandler, $author$project$Decoders$totalDataDecoder),
								bl: 'http://localhost:8081/total'
							}));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 4:
				if (!model.$) {
					var playerId = model.a;
					var playerName = model.b;
					var gameName = model.c;
					var validation = (playerId === '') ? $elm$core$Maybe$Just(0) : ((playerName === '') ? $elm$core$Maybe$Just(1) : ((gameName === '') ? $elm$core$Maybe$Just(2) : $elm$core$Maybe$Nothing));
					if (validation.$ === 1) {
						return _Utils_Tuple2(
							A3($author$project$Model$WaitingForServerValidation, playerId, playerName, gameName),
							$author$project$Encoders$sendMessage(
								A3($author$project$Model$IntroData, playerId, playerName, gameName)));
					} else {
						var v = validation;
						return _Utils_Tuple2(
							A4($author$project$Model$BeginGamePage, playerId, playerName, gameName, v),
							$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 5:
				return A2($author$project$Update$sendIncreasedBidMessage, model, 5);
			case 6:
				return A2($author$project$Update$sendIncreasedBidMessage, model, 10);
			case 7:
				if (model.$ === 4) {
					var commonData = model.a;
					var bidders = model.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$BiddingRound,
							commonData,
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(commonData.aT.aU),
								bidders)),
						$author$project$Encoders$sendMessage(
							A2($author$project$Model$SendQuit, commonData.aI, commonData.aT.aU)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 8:
				var suit = msg.a;
				if (model.$ === 5) {
					var commonData = model.a;
					var selectionData = model.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$TrumpSelection,
							commonData,
							_Utils_update(
								selectionData,
								{bi: suit})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 9:
				var card = msg.a;
				if (model.$ === 5) {
					var commonData = model.a;
					var selectionData = model.b;
					var newSelectionData = A2($elm$core$List$member, card, selectionData.aK) ? _Utils_update(
						selectionData,
						{
							aK: A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(card),
								selectionData.aK)
						}) : (($elm$core$List$length(selectionData.aK) < 2) ? _Utils_update(
						selectionData,
						{
							aK: A2($elm$core$List$cons, card, selectionData.aK)
						}) : selectionData);
					return _Utils_Tuple2(
						A2($author$project$Model$TrumpSelection, commonData, newSelectionData),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 10:
				if (model.$ === 5) {
					var commonData = model.a;
					var selectionData = model.b;
					return _Utils_Tuple2(
						model,
						$author$project$Encoders$sendMessage(
							A2($author$project$Model$SentSelectionData, commonData.aI, selectionData)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 11:
				var card = msg.a;
				var updateTurn = function (turnStatus) {
					switch (turnStatus.$) {
						case 2:
							return $author$project$Model$FirstAndMyTurnOver;
						case 4:
							var baseCard = turnStatus.a;
							return $author$project$Model$NotFirstAndMyTurnOver(baseCard);
						default:
							return turnStatus;
					}
				};
				if (model.$ === 7) {
					var commonData = model.a;
					var playRoundData = model.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Model$PlayRound,
							commonData,
							_Utils_update(
								playRoundData,
								{
									bj: updateTurn(playRoundData.bj)
								})),
						$author$project$Encoders$sendMessage(
							A2($author$project$Model$PlayedCard, commonData.aI, card)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 14:
				var receivedMessage = msg.a;
				return A2($author$project$Update$handleReceivedMessages, receivedMessage, model);
			case 13:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 15:
				var httpData = msg.a;
				return A2($author$project$Update$Analytics$handleHttpData, httpData, model);
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$View$Analytics$playerScoreDataView = F2(
	function (index, playerScoreData) {
		return A2(
			$elm$html$Html$tr,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$attribute,
					'class',
					(!A2($elm$core$Basics$modBy, 2, index)) ? 'even' : 'odd')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(index + 1))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(playerScoreData.aV)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(playerScoreData.a7))
						]))
				]));
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$core$List$sortBy = _List_sortBy;
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$th = _VirtualDom_node('th');
var $author$project$View$Analytics$totalModeView = function (playerScoreDataList) {
	var sortedList = $elm$core$List$reverse(
		A2(
			$elm$core$List$sortBy,
			function ($) {
				return $.a7;
			},
			playerScoreDataList));
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'class', 'totalModeView')
			]),
		$elm$core$List$singleton(
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'totalScoreView')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'totalScoreHeader')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Highest Total Score')
							])),
						A2(
						$elm$html$Html$table,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'totalScoreTable'),
								A2($elm$html$Html$Attributes$attribute, 'cellpadding', '0px'),
								A2($elm$html$Html$Attributes$attribute, 'cellspacing', '0px')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$th,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('S. No.')
											])),
										A2(
										$elm$html$Html$th,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Player Name')
											])),
										A2(
										$elm$html$Html$th,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Total Score')
											]))
									])),
							A2($elm$core$List$indexedMap, $author$project$View$Analytics$playerScoreDataView, sortedList)))
					]))));
};
var $author$project$View$Analytics$analyticsModeView = function (analyticsMode) {
	var playerScoreDataList = analyticsMode;
	return $author$project$View$Analytics$totalModeView(playerScoreDataList);
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $author$project$View$Analytics$analyticsPageView = function (analyticsMode) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'class', 'analyticsContainer')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h1,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'analyticsHeader')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Scorecard')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'analyticsContent')
					]),
				_List_fromArray(
					[
						$author$project$View$Analytics$analyticsModeView(analyticsMode)
					]))
			]));
};
var $author$project$Model$AnalyticsClicked = {$: 3};
var $author$project$Model$SendGameName = {$: 4};
var $author$project$Model$UpdateGameName = function (a) {
	return {$: 2, a: a};
};
var $author$project$Model$UpdatePlayerId = function (a) {
	return {$: 0, a: a};
};
var $author$project$Model$UpdatePlayerName = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$View$beginGamePageView = F4(
	function (playerId, playerName, gameName, validation) {
		var errorText = function () {
			if (!validation.$) {
				var error = validation.a;
				switch (error) {
					case 0:
						return 'Username can\'t be empty';
					case 1:
						return 'Player Name can\'t be empty';
					case 2:
						return 'Game Name can\'t be empty';
					case 3:
						return 'A Player with the same username already exists. Please choose another.';
					default:
						return 'A Player with same player name already exists. Please choose another.';
				}
			} else {
				return '';
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameContainer')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'beginGame')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameHeader')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Welcome to the card game 250!!')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameInputs')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$label,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Enter your username (this will be used if you get disconnected):')
										])),
									A2(
									$elm$html$Html$input,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(playerId),
												$elm$html$Html$Events$onInput($author$project$Model$UpdatePlayerId)
											]),
										_Utils_eq(
											validation,
											$elm$core$Maybe$Just(0)) ? _List_fromArray(
											[
												A2($elm$html$Html$Attributes$attribute, 'class', 'errorInput')
											]) : _List_Nil),
									_List_fromArray(
										[
											$elm$html$Html$text(playerId)
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameInputs')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$label,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Enter your player name (used for display):')
										])),
									A2(
									$elm$html$Html$input,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(playerName),
												$elm$html$Html$Events$onInput($author$project$Model$UpdatePlayerName)
											]),
										_Utils_eq(
											validation,
											$elm$core$Maybe$Just(1)) ? _List_fromArray(
											[
												A2($elm$html$Html$Attributes$attribute, 'class', 'errorInput')
											]) : _List_Nil),
									_List_fromArray(
										[
											$elm$html$Html$text(playerName)
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameInputs')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$label,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Enter a name for the group:')
										])),
									A2(
									$elm$html$Html$input,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$html$Html$Attributes$value(gameName),
												$elm$html$Html$Events$onInput($author$project$Model$UpdateGameName)
											]),
										_Utils_eq(
											validation,
											$elm$core$Maybe$Just(2)) ? _List_fromArray(
											[
												A2($elm$html$Html$Attributes$attribute, 'class', 'errorInput')
											]) : _List_Nil),
									_List_fromArray(
										[
											$elm$html$Html$text(gameName)
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'beginGameButton'),
									$elm$html$Html$Events$onClick($author$project$Model$SendGameName)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Begin Game')
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'errorView')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(errorText)
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'analytics'),
							$elm$html$Html$Events$onClick($author$project$Model$AnalyticsClicked)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Scorecard')
						])),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'help'),
							$elm$html$Html$Attributes$href('/help.html')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('How It Works')
						]))
				]));
	});
var $author$project$Model$BidPlus10 = {$: 6};
var $author$project$Model$BidPlus5 = {$: 5};
var $author$project$Model$QuitBidding = {$: 7};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$View$biddingZoneView = F2(
	function (commonData, bidders) {
		var highestBidderName = _Utils_eq(commonData.ax.aO, commonData.aT.aU) ? 'You' : A2($author$project$Model$getPlayer, commonData.a3, commonData.ax.aO).aV;
		var buttons = A2(
			$elm$core$List$cons,
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'bidButton'),
						$elm$html$Html$Events$onClick($author$project$Model$BidPlus5)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+5')
					])),
			(commonData.ax.aN > 240) ? _List_Nil : _List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'bidButton'),
							$elm$html$Html$Events$onClick($author$project$Model$BidPlus10)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('+10')
						]))
				]));
		var biddingHtml = A2($elm$core$List$member, commonData.aT.aU, bidders) ? _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'bidButtonContainer')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'increaseBidButtons')
							]),
						buttons),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'quitBiddingButton'),
								$elm$html$Html$Events$onClick($author$project$Model$QuitBidding)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Quit Bidding')
							]))
					]))
			]) : _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'bidButtonContainer')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('You can\'t bid anymore.')
					]))
			]);
		var bidderNames = A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				$author$project$Model$getPlayer(commonData.a3),
				function ($) {
					return $.aV;
				}),
			bidders);
		var bidderDivs = function () {
			var bidder = function (name) {
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(name)
						]));
			};
			return A2($elm$core$List$map, bidder, bidderNames);
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'biddingZone')
				]),
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'bidValueLabel')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Highest Bid')
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'bidValue')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(commonData.ax.aN))
							])),
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('(' + (highestBidderName + ')'))
							]))
					]),
				biddingHtml));
	});
var $author$project$View$gameNameView = function (name) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'class', 'gameName')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(name)
			]));
};
var $author$project$Model$SendCard = function (a) {
	return {$: 11, a: a};
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $author$project$View$cardView = F2(
	function (attrList, card) {
		var path = 'img/' + ($author$project$Model$Card$showCardValue(card.bm) + (' of ' + (A2($author$project$Model$Card$showSuit, true, card.be) + '.png')));
		var attributeList = _Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$src(path)
				]),
			attrList);
		return A2($elm$html$Html$img, attributeList, _List_Nil);
	});
var $author$project$View$myCardsView = F3(
	function (turnStatus, myCards, me) {
		var cardList = function () {
			var attrList = function (card) {
				switch (turnStatus.$) {
					case 2:
						return _List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Model$SendCard(card))
							]);
					case 4:
						var baseCard = turnStatus.a;
						var hasValidCard = A2(
							$elm$core$List$any,
							function (c) {
								return _Utils_eq(c.be, baseCard.be);
							},
							myCards);
						return (hasValidCard && (!_Utils_eq(card.be, baseCard.be))) ? _List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'blurCard')
							]) : _List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Model$SendCard(card))
							]);
					default:
						return _List_Nil;
				}
			};
			return A2(
				$elm$core$List$map,
				function (card) {
					return A2(
						$author$project$View$cardView,
						attrList(card),
						card);
				},
				myCards);
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'myCardsContainer')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'myName')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Your cards')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'myCards')
						]),
					cardList),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'myScores')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'This game\'s score: ' + $elm$core$String$fromInt(me.aJ))
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'Total Score: ' + $elm$core$String$fromInt(me.bh))
								]))
						]))
				]));
	});
var $author$project$Model$lookup = F2(
	function (elem, list) {
		lookup:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x.a, elem)) {
					return $elm$core$Maybe$Just(x.b);
				} else {
					var $temp$elem = elem,
						$temp$list = xs;
					elem = $temp$elem;
					list = $temp$list;
					continue lookup;
				}
			}
		}
	});
var $author$project$View$playerView = F3(
	function (bidders, i, _v0) {
		var playerIndex = _v0.a;
		var player = _v0.b;
		var maybeIsAllied = _v0.c;
		var isBidding = A2($elm$core$List$member, playerIndex, bidders);
		var biddingClass = isBidding ? ('bidder' + ($elm$core$String$fromInt(i) + ' ')) : '';
		var alliedClass = function () {
			if (!maybeIsAllied.$) {
				var isAllied = maybeIsAllied.a;
				return isAllied ? 'ally ' : 'enemy ';
			} else {
				return '';
			}
		}();
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$attribute,
					'class',
					biddingClass + (alliedClass + ('player p' + $elm$core$String$fromInt(i))))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerName')
						]),
					$elm$core$List$singleton(
						$elm$html$Html$text(player.aV))),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerScore')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(player.bh))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerScoreLabel')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Total')
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerScore')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(player.aJ))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerScoreLabel')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Current score')
						]))
				]));
	});
var $author$project$View$otherPlayersView = F4(
	function (myIndex, playerSet, bidders, allStatuses) {
		var rotateOtherPlayers = function (allPlayers) {
			if (allPlayers.b) {
				var x = allPlayers.a;
				var xs = allPlayers.b;
				return _Utils_eq(x.a, myIndex) ? xs : rotateOtherPlayers(
					_Utils_ap(
						xs,
						_List_fromArray(
							[x])));
			} else {
				return _List_Nil;
			}
		};
		var myStatus = A2(
			$elm$core$Maybe$withDefault,
			2,
			A2($author$project$Model$lookup, myIndex, allStatuses));
		var isAllied = function (playerStatus) {
			return ((myStatus === 2) || (playerStatus === 2)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				_Utils_eq(playerStatus, myStatus));
		};
		var otherPlayers = A2(
			$elm$core$List$map,
			function (_v1) {
				var i = _v1.a;
				var s = _v1.b;
				return _Utils_Tuple3(
					i,
					A2($author$project$Model$getPlayer, playerSet, i),
					isAllied(s));
			},
			rotateOtherPlayers(allStatuses));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'playersContainer')
				]),
			$elm$core$List$singleton(
				A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'players')
						]),
					A2(
						$elm$core$List$indexedMap,
						$author$project$View$playerView(bidders),
						otherPlayers))));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$View$playAreaView = F2(
	function (cards, myIndex) {
		var rotateOtherPlayers = function (allPlayers) {
			if (allPlayers.b) {
				var x = allPlayers.a;
				var xs = allPlayers.b;
				return _Utils_eq(x.a, myIndex) ? _Utils_Tuple2(xs, x) : rotateOtherPlayers(
					_Utils_ap(
						xs,
						_List_fromArray(
							[x])));
			} else {
				return _Utils_Tuple2(
					_List_Nil,
					_Utils_Tuple2(myIndex, $elm$core$Maybe$Nothing));
			}
		};
		var playerCardView = F2(
			function (i, _v2) {
				var playerIndex = _v2.a;
				var card = _v2.b;
				var defaultView = A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Attributes$attribute,
							'class',
							'yetToPlay p' + $elm$core$String$fromInt(i))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Yet to play')
								]))
						]));
				return A2(
					$elm$core$Maybe$withDefault,
					defaultView,
					A2(
						$elm$core$Maybe$map,
						$author$project$View$cardView(
							_List_fromArray(
								[
									A2(
									$elm$html$Html$Attributes$attribute,
									'class',
									'playerCard p' + $elm$core$String$fromInt(i))
								])),
						card));
			});
		var _v1 = rotateOtherPlayers(cards);
		var otherPlayers = _v1.a;
		var me = _v1.b;
		var myCard = A2(playerCardView, 5, me);
		var playerCards = A2($elm$core$List$indexedMap, playerCardView, otherPlayers);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'playArea')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'playerCards')
						]),
					playerCards),
					myCard
				]));
	});
var $author$project$Model$showRound = F2(
	function (isJson, round) {
		switch (round) {
			case 0:
				return 'Round' + (isJson ? '1' : ' 1');
			case 1:
				return 'Round' + (isJson ? '2' : ' 2');
			case 2:
				return 'Round' + (isJson ? '3' : ' 3');
			case 3:
				return 'Round' + (isJson ? '4' : ' 4');
			case 4:
				return 'Round' + (isJson ? '5' : ' 5');
			case 5:
				return 'Round' + (isJson ? '6' : ' 6');
			case 6:
				return 'Round' + (isJson ? '7' : ' 7');
			default:
				return 'Round' + (isJson ? '8' : ' 8');
		}
	});
var $author$project$View$staticInfoView = F4(
	function (commonData, selectionData, turnStatus, round) {
		var roundView = function () {
			if (turnStatus.$ === 7) {
				return '';
			} else {
				return A2($author$project$Model$showRound, false, round);
			}
		}();
		var pronounify = function (playerIndex) {
			return _Utils_eq(playerIndex, commonData.aT.aU) ? 'Your' : function (n) {
				return n + '\'s';
			}(
				A2($author$project$Model$getPlayer, commonData.a3, playerIndex).aV);
		};
		var turnView = function () {
			switch (turnStatus.$) {
				case 0:
					var player = turnStatus.a;
					return pronounify(player) + ' Turn';
				case 1:
					var player = turnStatus.a;
					return pronounify(player) + ' Turn';
				case 6:
					return 'Waiting for round to finish..';
				case 7:
					return 'Waiting for game to finish..';
				default:
					return pronounify(commonData.aT.aU) + ' Turn';
			}
		}();
		var helperView = function (helper) {
			return A2($author$project$View$cardView, _List_Nil, helper);
		};
		var helpers = A2($elm$core$List$map, helperView, selectionData.aK);
		var biddingInfoView = A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'biddingInfo')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'bidValueLabel')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							pronounify(commonData.ax.aO) + ' bid')
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'bidValue')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(commonData.ax.aN))
						]))
				]));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'miniTrumpAndHelper')
				]),
			_List_fromArray(
				[
					biddingInfoView,
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'miniTrump')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Trump')
								])),
							A2(
							$author$project$View$cardView,
							_List_Nil,
							A2($author$project$Model$Card$Card, 12, selectionData.bi))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'miniHelper')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Helpers')
								])),
							A2($elm$html$Html$span, _List_Nil, helpers)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'round')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(roundView)
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'turn')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(turnView)
								]))
						]))
				]));
	});
var $author$project$Model$SelectHelper = function (a) {
	return {$: 9, a: a};
};
var $author$project$Model$SelectTrump = function (a) {
	return {$: 8, a: a};
};
var $author$project$Model$SendTrump = {$: 10};
var $author$project$Model$Card$allCardValues = _List_fromArray(
	[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]);
var $author$project$Model$Card$allSuits = _List_fromArray(
	[0, 1, 2, 3]);
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Model$Card$allCards = A2(
	$elm$core$List$concatMap,
	function (cardValue) {
		return A2(
			$elm$core$List$map,
			$author$project$Model$Card$Card(cardValue),
			$author$project$Model$Card$allSuits);
	},
	$author$project$Model$Card$allCardValues);
var $author$project$View$trumpSelectionView = F2(
	function (commonData, selectionData) {
		var trumpView = function (suit) {
			var isSelected = _Utils_eq(suit, selectionData.bi);
			var labelAttr = isSelected ? ' selectedLabel' : '';
			var attrList = _Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$Model$SelectTrump(suit))
					]),
				isSelected ? _List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'selectedTrump')
					]) : _List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'trump')
					]));
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'trumpWithLabel')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'trumpLabel' + labelAttr)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($author$project$Model$Card$showSuit, true, suit))
							])),
						A2(
						$author$project$View$cardView,
						attrList,
						A2($author$project$Model$Card$Card, 12, suit))
					]));
		};
		var me = A2($author$project$Model$getPlayer, commonData.a3, commonData.aT.aU);
		var helperCardAttrList = function (card) {
			return _Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$Model$SelectHelper(card))
					]),
				A2($author$project$Model$isPlayerHelper, card, selectionData) ? _List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'selectedHelper')
					]) : _List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'helper')
					]));
		};
		var filteredCards = A2(
			$elm$core$List$filter,
			function (card) {
				return !A2($elm$core$List$member, card, commonData.aT.aS);
			},
			$author$project$Model$Card$allCards);
		var helperCards = A2(
			$elm$core$List$map,
			function (card) {
				return A2(
					$author$project$View$cardView,
					helperCardAttrList(card),
					card);
			},
			filteredCards);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'trumpContainer')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'class', 'trumpBox')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'trumpBoxHeader')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Select Trump')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'trumps')
								]),
							_List_fromArray(
								[
									trumpView(3),
									trumpView(1),
									trumpView(0),
									trumpView(2)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'helperContainer')
								]),
							A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$attribute, 'class', 'helperHeader')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Select Helpers')
										])),
								helperCards)),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'class', 'proceedButton'),
									$elm$html$Html$Events$onClick($author$project$Model$SendTrump)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Proceed')
								]))
						])),
					A3($author$project$View$myCardsView, $author$project$Model$RoundFinished, commonData.aT.aS, me)
				]));
	});
var $author$project$View$waitingForPlayersView = F2(
	function (playerNames, gameName) {
		var player = function (name) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(name)
					]));
		};
		var players = A2($elm$core$List$map, player, playerNames);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'class', 'waitingForPlayersView')
				]),
			$elm$core$List$singleton(
				A2(
					$elm$html$Html$div,
					_List_Nil,
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'class', 'waitingForPlayersHeader')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Waiting For 6 Players in ' + gameName)
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Current Players:')
									]))
							]),
						players))));
	});
var $author$project$View$view = function (model) {
	switch (model.$) {
		case 0:
			var playerId = model.a;
			var playerName = model.b;
			var gameName = model.c;
			var validation = model.d;
			return A4($author$project$View$beginGamePageView, playerId, playerName, gameName, validation);
		case 1:
			var analyticsMode = model.a;
			return $author$project$View$Analytics$analyticsPageView(analyticsMode);
		case 2:
			var playerId = model.a;
			var playerName = model.b;
			var gameName = model.c;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'serverValidationContainer')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Waiting for server to validate your username and player name')
							]))
					]));
		case 3:
			var playerNames = model.a;
			var gameName = model.b;
			return A2($author$project$View$waitingForPlayersView, playerNames, gameName);
		case 4:
			var commonData = model.a;
			var bidders = model.b;
			var me = A2($author$project$Model$getPlayer, commonData.a3, commonData.aT.aU);
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'biddingRoundView')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'biddingRoundSidebar')
							]),
						_List_fromArray(
							[
								$author$project$View$gameNameView(commonData.aI),
								A2($author$project$View$biddingZoneView, commonData, bidders)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'biddingRoundContent')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$View$otherPlayersView,
								commonData.aT.aU,
								commonData.a3,
								bidders,
								A2(
									$elm$core$List$map,
									function (i) {
										return _Utils_Tuple2(i, 2);
									},
									$author$project$Model$allPlayerIndices)),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'class', 'filler')
									]),
								_List_Nil),
								A3($author$project$View$myCardsView, $author$project$Model$RoundFinished, commonData.aT.aS, me)
							]))
					]));
		case 5:
			var commonData = model.a;
			var selectionData = model.b;
			return A2($author$project$View$trumpSelectionView, commonData, selectionData);
		case 6:
			var commonData = model.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'waitingForTrumpView')
					]),
				$elm$core$List$singleton(
					A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'Waiting for ' + (A2($author$project$Model$getPlayer, commonData.a3, commonData.ax.aO).aV + (' to select trump. Bid Amount: ' + $elm$core$String$fromInt(commonData.ax.aN))))
							]))));
		case 7:
			var commonData = model.a;
			var playRoundData = model.b;
			var playerCards = A2(
				$author$project$Model$getPlayers,
				function ($) {
					return $.aA;
				},
				commonData.a3);
			var myIndex = commonData.aT.aU;
			var me = A2($author$project$Model$getPlayer, commonData.a3, myIndex);
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'playRoundView')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'playRoundSidebar')
							]),
						_List_fromArray(
							[
								$author$project$View$gameNameView(commonData.aI),
								A4($author$project$View$staticInfoView, commonData, playRoundData.a8, playRoundData.bj, playRoundData.a6)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'playRoundContent')
							]),
						_List_fromArray(
							[
								A4(
								$author$project$View$otherPlayersView,
								myIndex,
								commonData.a3,
								_List_Nil,
								A2(
									$author$project$Model$getPlayers,
									function ($) {
										return $.ba;
									},
									commonData.a3)),
								A2($author$project$View$playAreaView, playerCards, myIndex),
								A3($author$project$View$myCardsView, playRoundData.bj, commonData.aT.aS, me)
							]))
					]));
		default:
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'errorContainer')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'class', 'error')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'class', 'errorHeading')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('OOPS!!')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'class', 'errorPara')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('\r\n                  No, I\'m not talking about "Object Oriented Programming Systems".\r\n                  Your websocket connection has failed.\r\n\r\n\r\n                  But no need to worry dear friend!!\r\n                  Just refresh your page and re-enter your details,\r\n                  specifically your id.\r\n\r\n                  You\'ll magically re-join the game,\r\n                  much as your websocket magically got disconnected.\r\n                  ')
									]))
							]))
					]));
	}
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{aQ: $author$project$Model$initModel, bd: $author$project$Subscriptions$subscriptions, bk: $author$project$Update$update, bn: $author$project$View$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));