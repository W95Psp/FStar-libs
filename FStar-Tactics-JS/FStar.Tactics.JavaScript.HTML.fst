module FStar.Tactics.JavaScript.HTML

module String = FStar.String
module S = FStar.String
module L = FStar.List.Tot

module JSON = Data.JSON
module JSC = FStar.Tactics.JavaScript.Core
open FStar.Tactics.JavaScript
open FStar.Tactics.JavaScript.Natives

noeq type attribute =
  | TextAttribute: string -> option string -> attribute
  | EventAttribute: string -> (jsObject -> JS unit) -> attribute

noeq type elementType =
  | HtmlElement:
    (tagName: string)
    -> (attrs: list attribute)
    -> (children: option (list elementType))
    -> elementType
  | HtmlRaw:
    (body: string)
    -> elementType


let tuplesToJSObject (l: list (string * 'a)): jsObject =
  L.fold_left (fun o (k, v) -> jsObjectSetPure o k v) (emptyObject ()) l

// let dot_to_ n = String.concat "_" (String.split ['.'] n)  

let rec printElement e
  : Tot string (decreases e)
  = match e with
  | HtmlElement tag attrs children ->
    let attrsStr = S.concat " " (L.map (fun v -> (match v with
      | TextAttribute k (Some v) -> k^"="^JSON.stringify_string v
      | TextAttribute k _ -> k
      | _ -> "event-unsupported"
    )) attrs) in
    "<" ^ tag ^ " " ^ attrsStr ^
    ( match children with
    | None   -> "/>"
    | Some children -> ">" ^ String.concat "" (L.map (
        fun (e': elementType) -> printElement (admit (); e')
      ) (admit (); children)) ^ "</"^tag^">" 
    )
  | HtmlRaw t -> t

let onclick (f: unit -> JS unit) = EventAttribute "onclick" (fun _ -> f ())
let oninput (f: string -> JS unit) = EventAttribute "oninput" (fun e -> f (
    unsafe_pure_jsVal_cast ((e @. "srcElement") @. "value")
  ))
  
let onchange (f: string -> JS unit) = EventAttribute "onchange" (fun e -> f (
    unsafe_pure_jsVal_cast ((e @. "srcElement") @. "value")
  ))
let onkeydown (f: string -> nat -> JS unit) = EventAttribute "onkeydown" (fun e -> f 
    (unsafe_pure_jsVal_cast #string (e @. "code"))
    (unsafe_pure_jsVal_cast #nat (e @. "keyCode"))
  )
let onkeyup (f: string -> nat -> JS unit) = EventAttribute "onkeyup" (fun e -> f 
    (unsafe_pure_jsVal_cast #string (e @. "code"))
    (unsafe_pure_jsVal_cast #nat (e @. "keyCode"))
  )

let html a l = HtmlElement "html" a (Some l)
let base a l = HtmlElement "base" a (Some l)
let head a l = HtmlElement "head" a (Some l)
let link a l = HtmlElement "link" a (Some l)
let meta a l = HtmlElement "meta" a (Some l)
let script a l = HtmlElement "script" a (Some l)
let style a l = HtmlElement "style" a (Some l)
let title a l = HtmlElement "title" a (Some l)
let body a l = HtmlElement "body" a (Some l)
let address a l = HtmlElement "address" a (Some l)
let article a l = HtmlElement "article" a (Some l)
let aside a l = HtmlElement "aside" a (Some l)
let footer a l = HtmlElement "footer" a (Some l)
let header a l = HtmlElement "header" a (Some l)
let h1 a l = HtmlElement "h1" a (Some l)
let h2 a l = HtmlElement "h2" a (Some l)
let h3 a l = HtmlElement "h3" a (Some l)
let h4 a l = HtmlElement "h4" a (Some l)
let h5 a l = HtmlElement "h5" a (Some l)
let h6 a l = HtmlElement "h6" a (Some l)
let hgroup a l = HtmlElement "hgroup" a (Some l)
let main a l = HtmlElement "main" a (Some l)
let nav a l = HtmlElement "nav" a (Some l)
let section a l = HtmlElement "section" a (Some l)
let blockquote a l = HtmlElement "blockquote" a (Some l)
let cite a l = HtmlElement "cite" a (Some l)
let dd a l = HtmlElement "dd" a (Some l)
let dl a l = HtmlElement "dl" a (Some l)
let dt a l = HtmlElement "dt" a (Some l)
let dir a l = HtmlElement "dir" a (Some l)
let ul a l = HtmlElement "ul" a (Some l)
let div a l = HtmlElement "div" a (Some l)
let figcaption a l = HtmlElement "figcaption" a (Some l)
let figure a l = HtmlElement "figure" a (Some l)
let hr a l = HtmlElement "hr" a (Some l)
let li a l = HtmlElement "li" a (Some l)
let ol a l = HtmlElement "ol" a (Some l)
let menu a l = HtmlElement "menu" a (Some l)
let p a l = HtmlElement "p" a (Some l)
let pre a l = HtmlElement "pre" a (Some l)
let a a l = HtmlElement "a" a (Some l)
let abbr a l = HtmlElement "abbr" a (Some l)
let b a l = HtmlElement "b" a (Some l)
let strong a l = HtmlElement "strong" a (Some l)
let bdi a l = HtmlElement "bdi" a (Some l)
let bdo a l = HtmlElement "bdo" a (Some l)
let br a = HtmlElement "br" a None
let code a l = HtmlElement "code" a (Some l)
let data a l = HtmlElement "data" a (Some l)
let dfn a l = HtmlElement "dfn" a (Some l)
let em a l = HtmlElement "em" a (Some l)
let i a l = HtmlElement "i" a (Some l)
let kbd a l = HtmlElement "kbd" a (Some l)
let mark a l = HtmlElement "mark" a (Some l)
let q a l = HtmlElement "q" a (Some l)
let rb a l = HtmlElement "rb" a (Some l)
let ruby a l = HtmlElement "ruby" a (Some l)
let rp a l = HtmlElement "rp" a (Some l)
let rt a l = HtmlElement "rt" a (Some l)
let rtc a l = HtmlElement "rtc" a (Some l)
let s a l = HtmlElement "s" a (Some l)
let del a l = HtmlElement "del" a (Some l)
let ins a l = HtmlElement "ins" a (Some l)
let samp a l = HtmlElement "samp" a (Some l)
let small a l = HtmlElement "small" a (Some l)
let span a l = HtmlElement "span" a (Some l)
let sub a l = HtmlElement "sub" a (Some l)
let sup a l = HtmlElement "sup" a (Some l)
let time a l = HtmlElement "time" a (Some l)
let tt a l = HtmlElement "tt" a (Some l)
let u a l = HtmlElement "u" a (Some l)
let var a l = HtmlElement "var" a (Some l)
let wbr a l = HtmlElement "wbr" a (Some l)
let area a l = HtmlElement "area" a (Some l)
let map a l = HtmlElement "map" a (Some l)
let audio a l = HtmlElement "audio" a (Some l)
let source a l = HtmlElement "source" a (Some l)
let img a = HtmlElement "img" a None
let track a l = HtmlElement "track" a (Some l)
let video a l = HtmlElement "video" a (Some l)
let applet a l = HtmlElement "applet" a (Some l)
// let jsObject a l = HtmlElement "object" a (Some l)
let embed a l = HtmlElement "embed" a (Some l)
let iframe a l = HtmlElement "iframe" a (Some l)
let noembed a l = HtmlElement "noembed" a (Some l)
let param a l = HtmlElement "param" a (Some l)
let picture a l = HtmlElement "picture" a (Some l)
let canvas a l = HtmlElement "canvas" a (Some l)
let noscript a l = HtmlElement "noscript" a (Some l)
let caption a l = HtmlElement "caption" a (Some l)
let table a l = HtmlElement "table" a (Some l)
let col a l = HtmlElement "col" a (Some l)
let colgroup a l = HtmlElement "colgroup" a (Some l)
let tbody a l = HtmlElement "tbody" a (Some l)
let tr a l = HtmlElement "tr" a (Some l)
let td a l = HtmlElement "td" a (Some l)
let tfoot a l = HtmlElement "tfoot" a (Some l)
let th a l = HtmlElement "th" a (Some l)
let thead a l = HtmlElement "thead" a (Some l)
let button a l = HtmlElement "button" a (Some l)
let datalist a l = HtmlElement "datalist" a (Some l)
let option_ a l = HtmlElement "option" a (Some l)
let fieldset a l = HtmlElement "fieldset" a (Some l)
let label a l = HtmlElement "label" a (Some l)
let form a l = HtmlElement "form" a (Some l)
let input a l = HtmlElement "input" a (Some l)
let legend a l = HtmlElement "legend" a (Some l)
let meter a l = HtmlElement "meter" a (Some l)
let optgroup a l = HtmlElement "optgroup" a (Some l)
let select a l = HtmlElement "select" a (Some l)
let output a l = HtmlElement "output" a (Some l)
let progress a l = HtmlElement "progress" a (Some l)
let textarea a l = HtmlElement "textarea" a (Some l)
let details a l = HtmlElement "details" a (Some l)
let summary a l = HtmlElement "summary" a (Some l)
let dialog a l = HtmlElement "dialog" a (Some l)
let menuitem a l = HtmlElement "menuitem" a (Some l)
let content a l = HtmlElement "content" a (Some l)
let element a l = HtmlElement "element" a (Some l)
let shadow a l = HtmlElement "shadow" a (Some l)
let slot a l = HtmlElement "slot" a (Some l)
let template a l = HtmlElement "template" a (Some l)
let acronym a l = HtmlElement "acronym" a (Some l)
let basefont a l = HtmlElement "basefont" a (Some l)
let font a l = HtmlElement "font" a (Some l)
let bgsound a l = HtmlElement "bgsound" a (Some l)
let big a l = HtmlElement "big" a (Some l)
let blink a l = HtmlElement "blink" a (Some l)
let center a l = HtmlElement "center" a (Some l)
let command a l = HtmlElement "command" a (Some l)
let frame a l = HtmlElement "frame" a (Some l)
let frameset a l = HtmlElement "frameset" a (Some l)
let image a l = HtmlElement "image" a (Some l)
let isindex a l = HtmlElement "isindex" a (Some l)
let keygen a l = HtmlElement "keygen" a (Some l)
let listing a l = HtmlElement "listing" a (Some l)
let marquee a l = HtmlElement "marquee" a (Some l)
let multicol a l = HtmlElement "multicol" a (Some l)
let nextid a l = HtmlElement "nextid" a (Some l)
let nobr a l = HtmlElement "nobr" a (Some l)
let noframes a l = HtmlElement "noframes" a (Some l)
let plaintext a l = HtmlElement "plaintext" a (Some l)
let spacer a l = HtmlElement "spacer" a (Some l)
let strike a l = HtmlElement "strike" a (Some l)
let xmp a l = HtmlElement "xmp" a (Some l)











// let l
// = [ `%html, ""
//   ; `%base, ""
//   ; `%head, ""
//   ; `%link, ""
//   ; `%meta, ""
//   ; `%script, ""
//   ; `%style, ""
//   ; `%title, ""
//   ; `%body, ""
//   ; `%address, ""
//   ; `%article, ""
//   ; `%aside, ""
//   ; `%footer, ""
//   ; `%header, ""
//   ; `%h1, ""
//   ; `%h2, ""
//   ; `%h3, ""
//   ; `%h4, ""
//   ; `%h5, ""
//   ; `%h6, ""
//   ; `%hgroup, ""
//   ; `%main, ""
//   ; `%nav, ""
//   ; `%section, ""
//   ; `%blockquote, ""
//   ; `%cite, ""
//   ; `%dd, ""
//   ; `%dl, ""
//   ; `%dt, ""
//   ; `%dir, ""
//   ; `%ul, ""
//   ; `%div, ""
//   ; `%figcaption, ""
//   ; `%figure, ""
//   ; `%hr, ""
//   ; `%li, ""
//   ; `%ol, ""
//   ; `%menu, ""
//   ; `%p, ""
//   ; `%pre, ""
//   ; `%a, ""
//   ; `%abbr, ""
//   ; `%b, ""
//   ; `%strong, ""
//   ; `%bdi, ""
//   ; `%bdo, ""
//   ; `%br, ""
//   ; `%code, ""
//   ; `%data, ""
//   ; `%dfn, ""
//   ; `%em, ""
//   ; `%i, ""
//   ; `%kbd, ""
//   ; `%mark, ""
//   ; `%q, ""
//   ; `%rb, ""
//   ; `%ruby, ""
//   ; `%rp, ""
//   ; `%rt, ""
//   ; `%rtc, ""
//   ; `%s, ""
//   ; `%del, ""
//   ; `%ins, ""
//   ; `%samp, ""
//   ; `%small, ""
//   ; `%span, ""
//   ; `%sub, ""
//   ; `%sup, ""
//   ; `%time, ""
//   ; `%tt, ""
//   ; `%u, ""
//   ; `%var, ""
//   ; `%wbr, ""
//   ; `%area, ""
//   ; `%map, ""
//   ; `%audio, ""
//   ; `%source, ""
//   ; `%img, ""
//   ; `%track, ""
//   ; `%video, ""
//   ; `%applet, ""
//   ; `%object, ""
//   ; `%embed, ""
//   ; `%iframe, ""
//   ; `%noembed, ""
//   ; `%param, ""
//   ; `%picture, ""
//   ; `%canvas, ""
//   ; `%noscript, ""
//   ; `%caption, ""
//   ; `%table, ""
//   ; `%col, ""
//   ; `%colgroup, ""
//   ; `%tbody, ""
//   ; `%tr, ""
//   ; `%td, ""
//   ; `%tfoot, ""
//   ; `%th, ""
//   ; `%thead, ""
//   ; `%button, ""
//   ; `%datalist, ""
//   ; `%option, ""
//   ; `%fieldset, ""
//   ; `%label, ""
//   ; `%form, ""
//   ; `%input, ""
//   ; `%legend, ""
//   ; `%meter, ""
//   ; `%optgroup, ""
//   ; `%select, ""
//   ; `%output, ""
//   ; `%progress, ""
//   ; `%textarea, ""
//   ; `%details, ""
//   ; `%summary, ""
//   ; `%dialog, ""
//   ; `%menuitem, ""
//   ; `%content, ""
//   ; `%element, ""
//   ; `%shadow, ""
//   ; `%slot, ""
//   ; `%template, ""
//   ; `%acronym, ""
//   ; `%basefont, ""
//   ; `%font, ""
//   ; `%bgsound, ""
//   ; `%big, ""
//   ; `%blink, ""
//   ; `%center, ""
//   ; `%command, ""
//   ; `%frame, ""
//   ; `%frameset, ""
//   ; `%image, ""
//   ; `%isindex, ""
//   ; `%keygen, ""
//   ; `%listing, ""
//   ; `%marquee, ""
//   ; `%multicol, ""
//   ; `%nextid, ""
//   ; `%nobr, ""
//   ; `%noframes, ""
//   ; `%plaintext, ""
//   ; `%spacer, ""
//   ; `%strike, ""
//   ; `%xmp, ""
// ], ""
// , ""
// , ""
 
