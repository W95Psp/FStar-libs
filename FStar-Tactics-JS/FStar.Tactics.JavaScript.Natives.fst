module FStar.Tactics.JavaScript.Natives

open FStar.Tactics

module S = FStar.String
module L = FStar.List.Tot

open FStar.Tactics.JavaScript.Core
open FStar.Tactics.JavaScript.Helpers

module All = FStar.All
effect JS (a:Type) = All.ALL a (fun (p: All.all_post a) _ -> forall (a:result a) h. p a h)

(*   JS NATIVE LIB *)
assume new type domNode
assume new type jsObject: Type
// a JsVal is anything: a (js) function, a (js) primitive type, an (js) object...
assume new type jsVal: Type

// assume new type jsFun // JS functions (with any arity, might be total, might be super effectful... who knows)

assume new type jsNumber
assume new type jsUndefined
assume new type jsNull

assume val jsInfinity: jsNumber
assume val jsNaN: jsNumber

assume val undefined: jsUndefined
assume val null: jsNull

assume val jsGlobalThis: unit -> JS jsObject
assume val jsWindow: unit -> JS jsObject

// Note we suppose getters are not effectful
// this is not true in general
assume val jsObjectGet: jsObject -> string -> jsVal // Here, we return a jsObject.
assume val jsObjectSet: jsObject -> string -> 'a -> JS unit
assume val jsObjectSetPure: jsObject -> string -> 'a -> jsObject

assume val eval: string -> JS jsVal

// the following should not be used: a jsVal might be an effectful function
let __unsafe_jsVal_cast #a (v: jsVal): a
   = unsafe_coerce v

let unsafe_pure_jsVal_cast v = __unsafe_jsVal_cast v 
let unsafe_jsVal_cast v: JS _ = __unsafe_jsVal_cast v

let obj_of_jsVal v = __unsafe_jsVal_cast v 
let jsVal_of_obj v = __unsafe_jsVal_cast v 

let asJsVal (v: 'a): jsVal
  = unsafe_coerce v

let unsafe_fromJsObject #a (v: jsObject): a
  = unsafe_coerce v
let fromJsObject #a (v: jsObject): JS a
  = unsafe_fromJsObject #a v

let (!) #a v = fromJsObject #a v

assume val unsafe_currify: jsVal -> (arity: nat) -> (thisArg: 't) -> 'a
let currify #a (f: jsVal) (arity: nat) (thisArg: 't): JS a
  = unsafe_currify f arity thisArg
assume val constructorOf: jsVal -> jsVal

let (@.) o key = obj_of_jsVal (jsObjectGet o key)
let (=.) o (key, v) = jsObjectSet o key v

let unsafe_currifyMethod #fn o key (arity: nat): fn = unsafe_currify (unsafe_pure_jsVal_cast (o @. key)) arity o
let currifyMethod #fn o key (arity: nat): JS fn = currify (unsafe_jsVal_cast (o @. key)) arity o

let console_log x: JS unit = 
  let f = currifyMethod (jsGlobalThis () @. "console") "log" 1 in
  f x

let json_stringify (obj: 'a) (spaces: nat): unit =
  let jsGlobalThis: unit -> jsObject = unsafe_coerce jsGlobalThis in
  let f = unsafe_currifyMethod (jsGlobalThis () @. "JSON") "stringify" 3 in
  f obj null spaces

let setTimeout (fn: unit -> JS unit) (ms: nat): JS nat =
  let f = currifyMethod (jsGlobalThis ()) "setTimeout" 2 in
  f fn ms


assume val jsMul (a b: jsVal): jsVal
assume val jsPlus (a b: jsVal): jsVal
assume val jsDiv (a b: jsVal): jsVal
assume val jsMinus (a b: jsVal): jsVal

let mathObject (): jsObject = 
  let jsGlobalThis = unsafe_coerce jsGlobalThis in 
  jsGlobalThis () @. "Math"

let parseInt (v: jsVal): int =
  let jsGlobalThis = unsafe_coerce jsGlobalThis in
  let f = unsafe_currify (jsGlobalThis () @. "parseInt") 1 null in
  f v

let random (): JS jsVal =
  let f = currifyMethod (mathObject ()) "random" 1 in
  f ()

let randomInt (min max: int): JS int =
  let rand = random () in
  let min = asJsVal min in
  let max = asJsVal max in
  parseInt (rand `jsMul` (max `jsMinus` min) `jsPlus` min)

// MOVE ME
let getElementById id: JS domNode =
  let f = currifyMethod (jsWindow () @. "document") "getElementById" 1 in
  f id

assume val emptyObject: unit -> jsObject
(* / JS NATIVE LIB *)

let runtime (l: t_nativeConstructors) = 
  let matchCode = mapJoin' "\n" (fun (name, def) ->
    let name = "\"" ^ dotTo_ name ^ "\"" in
    "if(cons == " ^ name ^ "){" ^
      ncMatch def
    ^ "}"
    ) l in 
"

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

            "^matchCode^"
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
        // \"^consCode^\"

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
  "


let defaultConfig: jsConfig =
  { nativeConstructors = [
    "Prims.Cons", 
    ( (*ncMatch*)     "return value instanceof Array 
                  && value.length
                  && helper(args[0], value[0])
                  && helper(args[1], value.slice(1));" 
    , (*ncConstruct*) "head => tail => [head, ...tail]"
    );
    "Prims.Nil", 
    ( (*ncMatch*)     "return !value.length;" 
    , (*ncConstruct*) "[];"
    )
  ]
  ; funs = [
    `%op_Subtraction, "x => y => x - y";
    `%op_Addition, "x => y => x + y";
    `%op_GreaterThan, "x => y => x > y";
    `%op_GreaterThanOrEqual, "x => y => x >= y";
    `%op_LessThan, "x => y => x < y";
    `%op_LessThanOrEqual, "x => y => x <= y";
    `%op_Equality, "x => y => x == y";
    `%op_BarBar, "x => y => x || y";
    `%op_AmpAmp, "x => y => x && y";
    `%op_Modulus, "x => y => x % y";
    `%fst, "x => x[1]";
    `%unsafe_coerce, "x => x";
    `%snd, "y => y[1]";
    `%string_of_int, "i => String(i)";
    `%FStar.String.list_of_string, "l => l.split('')";
    `%FStar.String.string_of_list, "l => l.join('')";
    `%FStar.Char.char, "x => x";
    `%strcat, "x => y => x + y";
    "UnitType", "undefined";
    `%constructorOf, "f => (...args) => new f(...args)";
    `%jsInfinity, "Infinity";
    `%jsNaN, "NaN";
    `%undefined, "undefined";
    `%null, "null";
    `%jsGlobalThis, "() => {try{return globalThis;}catch(e){}; try{return window;}catch(e){}; try{return global;}catch(e){}}";

    `%jsObjectSetPure, "o => k => v => {let x = Object.assign({}, o); x[k] = v; return x;};";
    `%jsObjectSet, "o => k => v => {o[k] = v; return o;}";
    `%jsObjectGet, "o => k => o[k]";

    `%magic, "() => ['MAGIC']";
    `%admit, "() => null";

    `%eval, "code => eval(code)";
    
    `%emptyObject, "() => ({})";
    
    `%jsMul, "a => b => a * b";
    `%jsPlus, "a => b => a + b";
    `%jsDiv, "a => b => a / b";
    `%jsMinus, "a => b => a - b";
    
    `%jsWindow, "() => window";
    `%unsafe_currify, "f => n => thisArg => MK_Curry(f.bind(thisArg), n)";
  ]
  ; runtime = runtime
  }





