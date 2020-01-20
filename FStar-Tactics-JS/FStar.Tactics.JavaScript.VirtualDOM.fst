module FStar.Tactics.JavaScript.VirtualDOM

open FStar.Tactics.JavaScript
open FStar.Tactics.JavaScript.HTML

module JSC = FStar.Tactics.JavaScript.Core
module L = FStar.List.Tot


let virtualDom (): jsObject = // virtualDom is a constant
  let jsWindow = unsafe_coerce jsWindow in
  obj_of_jsVal (jsWindow () @. "virtualDom")

assume new type patchObject
let vd_createElement (e: elementType): domNode = // no side effect (well...)
  let f = unsafe_currifyMethod (virtualDom ()) "create" 1 in
  f e
let vd_VNode (tag: string): jsObject -> list elementType -> elementType =
  let vnode = constructorOf (obj_of_jsVal (virtualDom () @. "VNode")) in
  let f = unsafe_currify vnode 3 null in f tag
let vd_VText (text: string): elementType =
  let vtext = constructorOf (obj_of_jsVal (virtualDom () @. "VText")) in
  let f = unsafe_currify vtext 1 null in f text
  
let vd_diff (e1 e2: elementType): patchObject =
  let f = unsafe_currifyMethod (virtualDom ()) "diff" 2 in
  f e1 e2
let vd_patch (n: domNode) (po: patchObject): JS unit =
  let f = currifyMethod (virtualDom ()) "patch" 2 in
  f n po

let mkVNode (tag: string) (attrs: list attribute) (children: option (list elementType))
  : elementType
  = 
  let attrs: jsObject = tuplesToJSObject (L.map (fun a -> 
    match a with
    | TextAttribute k (Some v) -> k, unsafe_coerce v
    | TextAttribute k _ -> k, null
    | EventAttribute k f -> k, unsafe_coerce f
  ) attrs) in
  vd_VNode tag attrs (match children with | Some x -> x | _ -> [])

let mkVText (t: string)
  : elementType
  = vd_VText t

let dot_to_ n = String.concat "_" (String.split ['.'] n)  

let additional_cons: JSC.t_nativeConstructors
= [ `%HtmlElement, 
    ( (*ncMatch*)     "throw 'Try to destruct HtmlElement!'" 
    , (*ncConstruct*) dot_to_ ("x => " ^ (`%mkVNode) ^ "(x)")    
    )
  ; `%HtmlRaw, 
    ( (*ncMatch*)     "throw 'Try to destruct HtmlRaw!'" 
    , (*ncConstruct*)  dot_to_ ("x => " ^ (`%mkVText) ^ "(x)")
    )
  ;
  ]

let additional_funs: list (string * string)
= []

let extra = [`%tuplesToJSObject;`%mkVNode;`%mkVText]


noeq type command_ msg =
  | DelayMessage: msg -> command_ msg
  | RandomNumber: (min: int) -> (max: int) -> (int -> msg) -> command_ msg

type command msg = list (command_ msg)

type subscription msg = list msg 

let elmHTML msg = (msg -> JS unit) -> elementType

let startElm
  #model #msg
  (init: unit -> model * command msg)
  (update: msg -> model -> model * command msg)
  (view: model -> elmHTML msg)
  (subscriptions: subscription msg)
  : JS unit
  = let _state = emptyObject () in
    let m0, cmd = init () in
    let set #x k v = jsObjectSet #x _state k v in
    let get #x v: x = unsafe_coerce (jsObjectGet _state v) in
    jsObjectSet _state "model" m0;
    let render (e1: elementType): JS unit =
      let div = (unsafe_coerce (getElementById "render") @. "children") @. "0" in
      let div: domNode = unsafe_pure_jsVal_cast div in
      let e0: elementType = unsafe_pure_jsVal_cast (jsObjectGet _state "e") in
      jsObjectSet _state "e" e1;
      vd_patch div (vd_diff e0 e1)
    in
    set "render" render;
    let update_and_view (the_msg: msg): JS unit
      = let m0 = jsObjectGet _state "model" in
        let m1, cmd = update the_msg (unsafe_coerce m0) in
        jsObjectSet _state "model" m1;
        render (view m1 (get "update_and_view"));
        let process_command: (s: command msg) -> JS unit = get "process_command" in
        process_command cmd
    in
    set "update_and_view" update_and_view;
    let process_command (s: command msg): JS unit
        = match s with
          | [] -> ()
          | hd::tl -> 
            begin
              match hd with
              | DelayMessage msg -> 
                let _ = setTimeout (fun x -> 
                  update_and_view msg
                ) 5000 in ()
              | RandomNumber min max msg ->
                let n = randomInt min max in
                update_and_view (msg n)
            end
    in
    set "process_command" process_command;
    let v0 = view m0 update_and_view in
    jsObjectSet _state "e" v0;
    let div = getElementById "render" in
    let append = currifyMethod (unsafe_coerce div) "appendChild" 1 in
    append (vd_createElement v0);
    ()
