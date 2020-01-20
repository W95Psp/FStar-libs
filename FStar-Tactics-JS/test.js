//PRINT 'RUNTIME'



    let MK_Curry = (fn, arity, ...args) => {
        if(typeof arity !== 'number')
          throw 'MK_Curry called with a non numeric arity';
	return (arity <= args.length) ?
	  fn(...args) :
	    (...more) => MK_Curry(fn, arity, ...args, ...more)
    };

    function Make_Cons_NamedKeys(consName, ...names){
	let o = function(...v){
            // this.length = names.length;
	    v.forEach((v, i) => {this[names[i]] = v;});
	}
        o.prototype[Symbol.iterator] = function(){
            let self = this;
            return {
              next: function(){
                let i = this._i;
                if(i > names.length)
                  return {done: true};
                let v = i == 0 ? consName : self[names[i-1]];
                this._i++;
                return {done: false, value: v}
              },
              _i: 0
            };
        };
	Object.defineProperty(o, 'name', {value: consName, configurable: true});
	names.forEach(
	    (name, i) =>
		Object.defineProperty(
		    o.prototype, i,
		    {get: function(){return o[name];}}
		)
	);

	let f = MK_Curry((...names) => new o(...names), names.length);
        f.obj = o;
        f.arity = names.length;
        return f;
    }
    let Make_Cons = Make_Cons_NamedKeys;




function match(branches, value){
    let o = {};
    
    let helper = (pat, value) => {
	if(pat instanceof Array){      // Pat_Cons
	    let [cons, ...args] = pat;

            if(cons == "Prims_Cons"){return value instanceof Array 
                  && value.length
                  && helper(args[0], value[0])
                  && helper(args[1], value.slice(1));}
if(cons == "Prims_Nil"){return !value.length;}
            if(!(value||{})[Symbol.iterator])
              return false;
            let valueList = [...value];
            
	    return     (valueList[0] == cons
		    && args.every(
			(pat, i) =>
			    helper(pat, valueList[i+1])
		    )
		   );
        }else if ((pat || {}).bv){// Pat_Var|Wild|Dot_Term 
	    o[pat.bv] = value;
	    return true;
	}else{                         // Pat_Constant
	    return value === pat;
	}
	return false;
    };
    for(let [pat, term] of branches){
	if(helper(pat, value))
	    return term(o);
    }
    throw 'match unsuccessful';
}
let _MkMK_ = (name, arity) =>
    {
        // "^consCode^"

	let o = l => [name, ...l];
	for(let i=0; i < arity; i++){
	    let prev = o;
	    o = l => x => prev([x, ...l]);
	}
	return o([]);
    };
let _Y = (def, body) => {
	let Y = () => def(() => Y ());
	return body(Y());
    };
  

// PRINT nativeConstructors


let Prims_Cons = head => tail => [head, ...tail];

let Prims_Nil = [];;


// PRINT funs


let FStar_Tactics_JavaScript_Natives_currify = f => n => thisArg => MK_Curry(f.bind(thisArg), n);

let FStar_Tactics_JavaScript_Natives_jsWindow = () => window;

let FStar_Tactics_JavaScript_Natives_emptyObject = () => ({});

let FStar_Tactics_JavaScript_Natives_eval = code => eval(code);

let Prims_admit = () => null;

let Prims_magic = () => ['MAGIC'];

let FStar_Tactics_JavaScript_Natives_jsObjectGet = o => k => o[k];

let FStar_Tactics_JavaScript_Natives_jsObjectSet = o => k => v => {o[k] = v; return o;};

let FStar_Tactics_JavaScript_Natives_jsObjectSetPure = o => k => v => {let x = Object.assign({}, o); x[k] = v; return x;};;

let FStar_Tactics_JavaScript_Natives_jsGlobalThis = () => {try{return globalThis;}catch(e){}; try{return window;}catch(e){}; try{return global;}catch(e){}};

let FStar_Tactics_JavaScript_Natives_null = null;

let FStar_Tactics_JavaScript_Natives_undefined = undefined;

let FStar_Tactics_JavaScript_Natives_jsNaN = NaN;

let FStar_Tactics_JavaScript_Natives_jsInfinity = Infinity;

let FStar_Tactics_JavaScript_Natives_constructorOf = f => (...args) => new f(...args);

let UnitType = undefined;

let Prims_strcat = x => y => x + y;

let FStar_Char_char = x => x;

let FStar_String_string_of_list = l => l.join('');

let FStar_String_list_of_string = l => l.split('');

let Prims_string_of_int = i => String(i);

let FStar_Pervasives_Native_snd = y => y[1];

let Prims_unsafe_coerce = x => x;

let FStar_Pervasives_Native_fst = x => x[1];

let Prims_op_Modulus = x => y => x % y;

let Prims_op_AmpAmp = x => y => x && y;

let Prims_op_BarBar = x => y => x || y;

let Prims_op_Equality = x => y => x == y;

let Prims_op_LessThanOrEqual = x => y => x <= y;

let Prims_op_LessThan = x => y => x < y;

let Prims_op_GreaterThanOrEqual = x => y => x >= y;

let Prims_op_GreaterThan = x => y => x > y;

let Prims_op_Addition = x => y => x + y;

let Prims_op_Subtraction = x => y => x - y;

let Example_Leaf = Make_Cons("Example_Leaf", "leaf");

let Example_Node = Make_Cons("Example_Node", "nodes");

let NonRecFun_Prims_op_Hat = ((s1) => ((s2) => Prims_strcat(s1)(s2)));

let Prims_op_Hat = NonRecFun_Prims_op_Hat;

let NonRecFun_FStar_Tactics_JavaScript_Natives_fromJsObject = ((v) => FStar_Tactics_JavaScript_Natives_unsafe_fromJsObject(v));

let FStar_Tactics_JavaScript_Natives_fromJsObject = NonRecFun_FStar_Tactics_JavaScript_Natives_fromJsObject;

let NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_fromJsObject = ((v) => Prims_unsafe_coerce(v));

let FStar_Tactics_JavaScript_Natives_unsafe_fromJsObject = NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_fromJsObject;

let NonRecFun_FStar_Tactics_JavaScript_Natives_json_stringify = ((obj) => ((spaces) => _Y(_ => Prims_unsafe_coerce(FStar_Tactics_JavaScript_Natives_jsGlobalThis), jsGlobalThis => _Y(_ => FStar_Tactics_JavaScript_Natives_unsafe_currifyMethod(FStar_Tactics_JavaScript_Natives_obj_of_jsVal(FStar_Tactics_JavaScript_Natives_op_At_Dot(jsGlobalThis(UnitType))("JSON")))("stringify")(3), f => f(obj)(FStar_Tactics_JavaScript_Natives_null)(spaces) ) )));

let FStar_Tactics_JavaScript_Natives_json_stringify = NonRecFun_FStar_Tactics_JavaScript_Natives_json_stringify;

let NonRecFun_FStar_Tactics_JavaScript_Natives_obj_of_jsVal = ((v) => FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast(v));

let FStar_Tactics_JavaScript_Natives_obj_of_jsVal = NonRecFun_FStar_Tactics_JavaScript_Natives_obj_of_jsVal;

let NonRecFun_FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast = ((v) => Prims_unsafe_coerce(v));

let FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast = NonRecFun_FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast;

let NonRecFun_FStar_Tactics_JavaScript_Natives_op_At_Dot = ((o) => ((key) => FStar_Tactics_JavaScript_Natives_jsObjectGet(o)(key)));

let FStar_Tactics_JavaScript_Natives_op_At_Dot = NonRecFun_FStar_Tactics_JavaScript_Natives_op_At_Dot;

let NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_currifyMethod = ((o) => ((key) => ((arity) => FStar_Tactics_JavaScript_Natives_unsafe_currify(FStar_Tactics_JavaScript_Natives_unsafe_pure_jsVal_cast(FStar_Tactics_JavaScript_Natives_op_At_Dot(o)(key)))(arity)(o))));

let FStar_Tactics_JavaScript_Natives_unsafe_currifyMethod = NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_currifyMethod;

let NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_pure_jsVal_cast = ((v) => FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast(v));

let FStar_Tactics_JavaScript_Natives_unsafe_pure_jsVal_cast = NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_pure_jsVal_cast;

let Example_funny = ((x) => ((n) => match([[true, ({}) => Example_Leaf(Example_fib(x))], [{bv: "uu___"}, ({uu___}) => Example_Node(Prims_Cons(Example_funny(Prims_op_Addition(x)(1))(Prims_op_Subtraction(n)(1)))(Prims_Cons(Example_funny(Prims_op_Addition(x)(2))(Prims_op_Subtraction(n)(1)))(Prims_Cons(Example_funny(Prims_op_Addition(x)(3))(Prims_op_Subtraction(n)(1)))(Prims_Nil))))]], Prims_op_Equality(n)(0))));

let Example_fib = ((n) => match([[true, ({}) => Prims_op_Addition(Example_fib(Prims_op_Subtraction(n)(1)))(Example_fib(Prims_op_Subtraction(n)(2)))], [{bv: "uu___"}, ({uu___}) => 1]], Prims_op_GreaterThan(n)(2)));

let NonRecFun_Example_console_log = ((x) => _Y(_ => _Y(_ => _Y(_ => _Y(_ => FStar_Tactics_JavaScript_Natives_jsGlobalThis(UnitType), uu___ => FStar_Tactics_JavaScript_Natives_op_At_Dot(uu___)("console") ), uu___ => FStar_Tactics_JavaScript_Natives_obj_of_jsVal(uu___) ), uu___ => FStar_Tactics_JavaScript_Natives_currifyMethod(uu___)("log")(1) ), f => f(x) ));

let Example_console_log = NonRecFun_Example_console_log;

let NonRecFun_FStar_Tactics_JavaScript_Natives_currifyMethod = ((o) => ((key) => ((arity) => _Y(_ => FStar_Tactics_JavaScript_Natives_unsafe_jsVal_cast(FStar_Tactics_JavaScript_Natives_op_At_Dot(o)(key)), uu___ => FStar_Tactics_JavaScript_Natives_currify(uu___)(arity)(o) ))));

let FStar_Tactics_JavaScript_Natives_currifyMethod = NonRecFun_FStar_Tactics_JavaScript_Natives_currifyMethod;

let NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_jsVal_cast = ((v) => FStar_Tactics_JavaScript_Natives___unsafe_jsVal_cast(v));

let FStar_Tactics_JavaScript_Natives_unsafe_jsVal_cast = NonRecFun_FStar_Tactics_JavaScript_Natives_unsafe_jsVal_cast;


//ENTRY POINT

_Y(_ => Example_console_log(FStar_Tactics_JavaScript_Natives_json_stringify(Example_funny(0)(5))), uu___ => _Y(_ => Example_console_log(match([[["Example_Node", {bv: "l"}], ({l}) => l], [["Example_Leaf", {bv: "uu___"}], ({uu___}) => Prims_Nil]], Example_funny(0)(5))), uu___ => _Y(_ => FStar_Tactics_JavaScript_Natives_emptyObject(UnitType), wholaObject => _Y(_ => FStar_Tactics_JavaScript_Natives_jsObjectSet(wholaObject)("x")("a"), uu___ => _Y(_ => ((hey) => _Y(_ => FStar_Tactics_JavaScript_Natives_fromJsObject(FStar_Tactics_JavaScript_Natives_jsObjectGet(wholaObject)("x")), s => _Y(_ => FStar_Tactics_JavaScript_Natives_jsObjectSet(wholaObject)("x")(Prims_op_Hat(s)("'")), uu___ => Example_console_log(Prims_op_Hat("helloooo:")(Prims_op_Hat(hey)(s))) ) )), n => _Y(_ => n("a"), uu___ => _Y(_ => n("b"), uu___ => n("c") ) ) ) ) ) ) )