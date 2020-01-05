module Data.JSON.Parser

open StarCombinator
open Data.JSON.Types

let jsonCharParser =
 (    exact_char '\\'
 <<*> (('\"' *<< exact_char '\"') <|>
       ('/' *<< exact_char '/' ) <|>
       ('' *<< exact_char 'b' ) <|>
       ('' *<< exact_char 'f' ) <|>
       ('\n' *<< exact_char 'n' ) <|>
       ('\r' *<< exact_char 'r' ) <|>
       ('\t' *<< exact_char 't' ) <|>
       (
         '?' *<<
         ( exact_char 'u' <*>>
           ( let p = oneOf ['A';'B';'C';'D';'E';'F';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] in
             p <*> p <*> p <*> p
           )
         )
       )
      )
 ) <|> satisfy_char (fun x -> x <> '\"')
 
let jsonStringParser': parser string =
  FStar.String.string_of_list
  @<<( exact_char '\"'
  <*>> many jsonCharParser
  <<*> exact_char '\"')

let jsonStringParser: parser jsonValue =
  JsonString @<< jsonStringParser'

open FStar.Char

let rec convert (c:list (n: nat{n <= 9})): nat = match c with
    | [] -> 0
    | hd::tl -> hd + FStar.Mul.op_Star 10 (convert tl)

let convert_digit (c:char): (n: nat{n <= 9}) = match c with
              | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
              | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 |  _  -> 0 

let parseBool = 
  JsonBool @<<
  (   (true *<< exact_string "true")
  <|> (false *<< exact_string "false"))

let parseDecimalNumber
  =
  (fun (((s, left), right), exp) ->
    let right = match right with | Some l -> l | None -> [] in
    DecimalNumber (convert (left @ right)) (FStar.List.Tot.length right) exp)
  @<<
  ((( ((fun x -> if Some? x then -1 else 1 ) @<< maybe (exact_char '-'))
  <*> (   ([0] *<< exact_char '0')
      <|> ((fun (head, tail) -> head::tail) @<<
            (   (convert_digit @<< oneOf ['1';'2';'3';'4';'5';'6';'7';'8';'9'])
            <*> many digit_num
            )
          )
      ))
  <*> (maybe (exact_char '.' <*>> many1 digit_num)))
  <*> ((fun x ->
      let r: int = match x with
      | Some (Some '+', x) ->  x
      | Some (Some '-', x) -> -x
      | Some (_       , x) ->  x
      | None           -> 0
      in r
      ) @<<
       maybe (    oneOf ['e'; 'E']
             <*>> (maybe (oneOf ['+';'-']))
             <*>  number
             )
      )
  )

let parseNumber//: parser (((int * list _) * list _) * _)
  = JsonNumber @<< parseDecimalNumber

let parseNull
  = JsonNull *<< exact_string "null"

let match_list l r s i =
  (fun x -> match x with | None -> [] | Some x -> x)
  @<<
  between
    (spaces <*> exact_char l <*> spaces)
    (spaces <*> exact_char r <*> spaces)
    (maybe (sepBy i s)) // <?> "expected a list \""^l^" ... "^(s.description ()).message^" ... "^r^"\""

let rec parseObject (): parser jsonValue =  
  JsonObject @<<
  match_list '{' '}'
    (spaces <*> exact_char ',' <*> spaces)
    (
           jsonStringParser'
      <*> ((spaces <*> exact_char ':' <*> spaces)
      <*>> (delayMe (admitP (parseValue << parseValue); parseValue)))
    )
and parseArray (): parser jsonValue =
  JsonArray @<<
  match_list '[' ']'
    (spaces <*> exact_char ',' <*> spaces)
    (delayMe (admitP (parseValue << parseValue); parseValue))
and parseValue (): parser jsonValue = 
  spaces <*>>
  (    (delayMe (admitP (parseObject << parseObject); parseObject))
   <|> (delayMe (admitP (parseArray << parseArray); parseArray))
   <|> jsonStringParser
   <|> parseNumber
   <|> parseBool
   <|> parseNull)
  <<*> spaces

let parser (source: string) = 
  if String.length source > 0
  then make #jsonValue (parseValue ()) source
  // then make #jsonValue parseNumber source
  else Inr "Empty source"

let test = parser "
{\"a\" : 3, \"asd\": [2,3] }
"

