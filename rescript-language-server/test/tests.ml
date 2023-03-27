open Rescript_language_server.Diagnostic
open Lsp.Types

let () =
  let () =
    ParseLog.parse_path_and_range
      "/Users/chenglou/github/reason-react/src/test.res:1:8-2:3"
    |> Location.yojson_of_t |> Lsp.Import.Json.to_string |> print_endline
  in

  let () =
    ParseLog.parse_path_and_range
      "/Users/chenglou/github/reason-react/src/test.res:3:5-8"
    |> Location.yojson_of_t |> Lsp.Import.Json.to_string |> print_endline
  in

  let range3 =
    ParseLog.parse_path_and_range
      "/Users/chenglou/github/reason-react/src/test.res:3:9"
    |> Location.yojson_of_t |> Lsp.Import.Json.to_string |> print_endline
  in

  let range4 =
    ParseLog.parse_path_and_range
      "/home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:27:19-30:5"
    |> Location.yojson_of_t |> Lsp.Import.Json.to_string |> print_endline
  in

  let range6 =
    ParseLog.parse_path_and_range
      "/home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:102:52-55"
    |> Location.yojson_of_t |> Lsp.Import.Json.to_string |> print_endline
  in

  ()

let () =
  let log = {|#Start(1600519680823)
#Done(1678927956367)
   |} in
  let result = ParseLog.parse log |> List.length in
  assert (result = 0)

let () =
  let log =
    {|#Start(1600519680823)

     Syntax error!
     /Users/chenglou/github/reason-react/src/test.res:1:8-2:3

     1 │ let a =
     2 │ let b =
     3 │

     This let-binding misses an expression

   #Done(1678927956367)
   |}
  in
  let result = ParseLog.parse log in
  assert (List.length result = 1)

let () =
  let log =
    {|#Start(1600519680823)

     Warning number 8
     /Users/chenglou/github/reason-react/src/test.res:3:5-8

     1 │ let a = j`😀`
     2 │ let b = `😀`
     3 │ let None = None
     4 │ let bla: int = "
     5 │   hi

     You forgot to handle a possible case here, for example:
     Some _

   #Done(1678927956367)
   |}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1600519680823)

     We've found a bug for you!
     /Users/chenglou/github/reason-react/src/test.res:3:9

     1 │ let a = 1
     2 │ let b = "hi"
     3 │ let a = b + 1

     This has type: string
     Somewhere wanted: int


   #Done(1678927956367)
   |}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1600519680823)

     Syntax error!
     /Users/chenglou/github/reason-react/src/test.res:1:8-2:3

     1 │ let a =
     2 │ let b =
     3 │

     This let-binding misses an expression


     Warning number 8
     /Users/chenglou/github/reason-react/src/test.res:3:5-8

     1 │ let a = j`😀`
     2 │ let b = `😀`
     3 │ let None = None
     4 │ let bla: int = "
     5 │   hi

     You forgot to handle a possible case here, for example:
     Some _


     We've found a bug for you!
     /Users/chenglou/github/reason-react/src/test.res:3:9

     1 │ let a = 1
     2 │ let b = "hi"
     3 │ let a = b + 1

     This has type: string
     Somewhere wanted: int

   #Done(1600519680836)
   |}
  in
  assert (List.length @@ ParseLog.parse log = 3)

let () =
  let log =
    {|#Start(1679214328122)
   FAILED: dependency cycle: src/ModA.cmj -> src/ModB.cmj -> src/ModA.cmj.
#Done(1679214328144)|}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1678414070985)

     We've found a bug for you!
     /home/pedro/Desktop/projects/rescript-compiler/jscomp/build_tests/cycle1/src/A.res:1:9

     1 │ include A
     2 │
     3 │ let x = 42

     Internal path A-Cycle1 is dangling.
     The compiled interface for module A-Cycle1 was not found.

   FAILED: cannot make progress due to previous errors.
#Done(1678414071066)|}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let code =
    {|  16 │
     17 │
     18 │ let defaultPreviewImg = "/static/Art-3-rescript-launch.jpg" + 1
     19 │
     20 │ // For encoding reasons, see https://shripadk.github.io/react/docs/jsx-
        │ gotchas.html|}
  in

  let result = ParseLog.remove_code (String.split_on_char '\n' code) in
  assert (List.length @@ result = 0)

let () =
  let log =
    {|#Start(1679286063681)

     We've found a bug for you!
     /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:18:25-59

     16 │
     17 │
     18 │ let defaultPreviewImg = "/static/Art-3-rescript-launch.jpg" + 1
     19 │
     20 │ // For encoding reasons, see https://shripadk.github.io/react/docs/jsx-
        │ gotchas.html

     This has type: string
     Somewhere wanted: int

     You can convert string to int with Belt.Int.fromString.

   FAILED: cannot make progress due to previous errors.
#Done(1679286063732)
   |}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|
  15 │ module Link = Next.Link
  16 │ 
  17 │ let a = 1 + 2
  18 │ 
  19 │ let defaultPreviewImg = "/static/Art-3-rescript-launch.jpg"

  unused value a.

#Done(1679288582550)
|}
  in
  assert (
    List.length @@ ParseLog.remove_code (String.split_on_char '\n' log) = 4)

let () =
  let log =
    {|#Start(1679288582297)

     Warning number 32
     /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:17:5

     15 │ module Link = Next.Link
     16 │ 
     17 │ let a = 1 + 2
     18 │ 
     19 │ let defaultPreviewImg = "/static/Art-3-rescript-launch.jpg"

     unused value a.

#Done(1679288582550)
   |}
  in
  let result = ParseLog.parse log in
  assert (List.length result = 1);

  match List.hd result with
  | Warning _ -> assert true
  | _ -> assert false

let () =
  let log =
    {|#Start(1679291996916)

  Warning number 8 (configured as error) 
  /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:27:19-30:5

  25 ┆ @react.component
  26 ┆ let make = (~badge: BlogFrontmatter.Badge.t) => {
  27 ┆   let bgColor = switch badge {
  28 ┆   | Preview | Release => "bg-turtle"
  29 ┆   | Testing => "bg-orange"
  30 ┆   }
  31 ┆ 
  32 ┆   let text = badge->BlogFrontmatter.Badge.toString

  You forgot to handle a possible case here, for example: 
  Roadmap

FAILED: cannot make progress due to previous errors.
#Done(1679291997159)
|}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1679295989295)

  We've found a bug for you!
  /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:59:17-19

  57 ┆ @react.component
  58 ┆ let make = (~selected: selection, ~onSelected: selection => unit) => {
  59 ┆   let tabs = [All, Archived]
  60 ┆ 
  61 ┆   <div className="text-16 w-full flex items-center justify-between text
     ┆ -gray-60">

  The variant constructor All can't be found.
  
  - If it's defined in another module or file, bring it into scope by:
    - Prefixing it with said module name: TheModule.All
    - Or specifying its type: let theValue: TheModule.theType = All
  - Constructors and modules are both capitalized. Did you want the latter?
    Then instead of let foo = Bar, try module Foo = Bar.

FAILED: cannot make progress due to previous errors.
#Done(1679295989384)|}
  in

  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1679297138935)

  We've found a bug for you!
  /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:327:3-332:3

  325 │ 
  326 │ let getStaticProps: Next.GetStaticProps.t<props, params> = async _ctx 
      │ => {
  327 │   let (archived, nonArchived) = BlogApi.getAllPosts()->Belt.Array.part
      │ ition(data => data.archived)
  328 │ 
    . │ ...
  331 │     archived,
  332 │   }
  333 │ 
  334 │ }

  This has type: promise<unit>
  Somewhere wanted:
    Js.Promise.t<{"props": props}> (defined as promise<{"props": props}>)
  
  The incompatible parts:
    unit vs {"props": props}

FAILED: cannot make progress due to previous errors.
#Done(1679297139163)|}
  in
  assert (List.length @@ ParseLog.parse log = 1)

let () =
  let log =
    {|#Start(1679299125636)

  Warning number 32
  /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:225:5-11

  223 │ }
  224 │ 
  225 │ let default = (props: props): React.element => {
  226 │   let {posts, archived} = props
  227 │ 

  unused value default.


  Warning number 32
  /home/pedro/Desktop/projects/rescript-lang.org/src/Blog.res:327:5-18

  325 │ }
  326 │ 
  327 │ let getStaticProps: Next.GetStaticProps.t<props, params> = async _ctx 
      │ => {
  328 │   let (archived, nonArchived) = BlogApi.getAllPosts()->Belt.Array.part
      │ ition(data => data.archived)
  329 │ 

  unused value getStaticProps.


  We've found a bug for you!
  /home/pedro/Desktop/projects/rescript-lang.org/src/BlogArticle.res:168:75-96

  166 ┆   title={title ++ " | ReScript Blog"}
  167 ┆   description=?{description->Js.Null.toOption}
  168 ┆   ogImage={previewImg->Js.Null.toOption->Belt.Option.getWithDefault(Bl
      ┆ og.defaultPreviewImg)}
  169 ┆ />
  170 ┆ <div className="mb-10 md:mb-20">

  The value defaultPreviewImg can't be found in Blog

FAILED: cannot make progress due to previous errors.
#Done(1679299126037)
|}
  in
  let r = ParseLog.parse log in
  let () = List.iter (fun x -> ParseLog.to_stdout x) r in
  assert (List.length @@ ParseLog.parse log = 3)
