module Byobu.Parser

open Byobu.Types

type AttrsState =
    | AttrsStateKey
    | AttrsStateValue

let test name actual expected =
    if expected <> actual then
        failwithf "Test failure: %s - expected %A, got %A" name expected actual
    else
        printfn "Test success: %s" name

// class=foo bar>... -> (Map["class", "foo"; "bar", ""], ...)
// baz="a </> " />... -> (Map["baz", "a </> "], ...)
// qux='""</>'>... -> (Map["qux", "\"\"</>"], ...)
// edge cases:
// foo=/> -> (Map["foo", "/"], "")
// foo/> -> (Map["foo", ""])
let rec parseAttrs (s: string) : Map<string, string> * string =
    let mutable attributes = Map<string, string> []
    let mutable currentState = AttrsStateKey
    let mutable currentKey = ""
    let mutable currentValue = ""
    let mutable currentQuoteState: char Option = None // either '"' or '\''. too lazy to write a DU
    let mutable looping = true
    let mutable chars = System.Collections.Generic.Queue<char> s
    // returns `did actually skip some spaces?`
    let skipSpaces =
        fun () ->
            if currentQuoteState = None && chars.Count > 0 && chars.Peek() = ' ' then
                while chars.Count > 0 && chars.Peek() = ' ' do
                    chars.Dequeue() |> ignore

                true
            else
                false

    while chars.Count > 0 && looping do
        let c = chars.Dequeue()

        match c with
        | q when currentQuoteState = Some q -> currentQuoteState <- None
        | '"'
        | '\'' as q when currentQuoteState = None -> currentQuoteState <- Some q
        | '/' when currentQuoteState = None && currentState = AttrsStateKey ->
            // don't panic if it's just a />
            if chars.Peek() <> '>' then
                failwith "bail: unexpected /"
        | '=' when currentQuoteState = None ->
            currentState <- AttrsStateValue
            skipSpaces () |> ignore
        | ' '
        | '>' when currentQuoteState = None ->
            if currentKey <> "" then
                attributes <- attributes.Add(currentKey, currentValue)
                currentState <- AttrsStateKey
                currentKey <- ""
                currentValue <- ""

            if c = '>' then
                looping <- false
        | char ->
            match currentState with
            | AttrsStateKey ->
                currentKey <- currentKey + string char

                if skipSpaces () && chars.Count > 0 && chars.Peek() <> '=' then
                    attributes <- attributes.Add(currentKey, currentValue)
                    currentState <- AttrsStateKey
                    currentKey <- ""
            | AttrsStateValue -> currentValue <- currentValue + string char

    let rest = System.String.Concat chars
    attributes, rest

let parseAttrsTests () =
    test
        "parseAttrs normal 1"
        (parseAttrs "class=foo bar>...")
        (Map["class", "foo"
             "bar", ""],
         "...")

    test "parseAttrs normal 2" (parseAttrs "baz=\"a </> \" />...") (Map["baz", "a </> "], "...")
    test "parseAttrs normal 2" (parseAttrs "qux='\"\"</>'>...") (Map["qux", "\"\"</>"], "...")
    // edge cases
    test "parseAttrs =/>" (parseAttrs "foo=/> ") (Map["foo", "/"], " ")
    test "parseAttrs =>" (parseAttrs "foo=> ") (Map["foo", ""], " ")
    test "parseAttrs key/>" (parseAttrs "foo/> ") (Map["foo", ""], " ")
    test "parseAttrs spaced out" (parseAttrs "  x = \"foo\" >") (Map["x", "foo"], "")

    test
        "parseAttrs empty values"
        (parseAttrs "foo='' bar=\"\">")
        (Map["foo", ""
             "bar", ""],
         "")

    test
        "parseAttrs bug repro 1"
        (parseAttrs "foo bar=baz>")
        (Map["foo", ""
             "bar", "baz"],
         "")

// <span class="foo">....</span>... -> (Element {}) * ...
let rec parseTag (s: string) : Element * string =
    eprintfn "parseTag %A" s
    assert (s.[0] = '<')

    let tagName =
        let idx = s.IndexOfAny [| ' '; '>' |]

        if idx = -1 then
            failwithf "bail: incomplete tag in parsing tag %A" s

        s.[1 .. idx - 1]
    // <span>children foo</span>barbaz
    // |tag |
    //       |   childrenAndRest     |
    //       | children |       |rest|
    // tagName = span
    //
    // <span />children foo bar baz
    // |tag |
    //       | childrenAndRest  |
    //       | children         |
    // tagName = span
    // <br >foo bar baz fizz buzz
    // |tag|
    //      |  childrenAndRest  |
    //      |       rest        |
    let tagInfo =
        List.tryFind (fun (t: TagDefinition) -> t.name = tagName) Data.tags
        |> Option.defaultWith (fun () ->
            eprintfn "unknown tag: %A" tagName
            Data.defaultTag)

    let isSelfClosing: bool =
        List.exists (fun c -> c = TagControl.SelfClosing) tagInfo.controls

    let attrs, childrenAndRest = parseAttrs s.[tagName.Length + 1 ..]

    let children, rest =
        match isSelfClosing with
        | true -> [], childrenAndRest
        | false ->
            let closingTag = "</" + tagName + ">"
            let childrenEnd = childrenAndRest.IndexOf closingTag

            let children, rest =
                if childrenEnd = -1 then
                    failwith "todo: handle when closingTag is not found"
                else
                    parseContent childrenAndRest.[.. childrenEnd - 1],
                    childrenAndRest.[childrenEnd + closingTag.Length ..]

            children, rest

    let el: Element =
        { tag = tagName
          attributes = attrs
          children = children }

    el, rest

and parseContent (text: string) : Node list =
    eprintfn "parseContent %A" text

    match text with
    | s when s.StartsWith "</" ->
        // floating closing tag, pls handle
        failwith "todo: standalone closing tag"
    | s when s.StartsWith "<" ->
        // tag
        let el, rest = parseTag s
        ElementNode el :: parseContent rest
    | "" -> [] // base case
    | s ->
        // text node
        let bracket = s.IndexOf "<"

        if bracket = -1 then
            // also base case
            [ TextNode s ]
        else
            TextNode s.[.. bracket - 1] :: parseContent s.[bracket..]

let parseTagTests () =
    let el, rest = parseTag "<span tag=value>abc</span>def"
    test "parseTag tagName" el.tag "span"
    test "parseTag children" el.children [ TextNode "abc" ]
    test "parseTag attributes" el.attributes Map[("tag", "value")]
    test "parseTag rest" rest "def"

    let el, rest = parseTag "<br>abc</br>"
    test "parseTag self closing" rest "abc</br>"
    test "parseTag self closing children" el.children []

let parseContentTests () =
    let nodes = parseContent "<span class=text-xl>HELLO!</span>"

    test
        "parseContents base"
        nodes
        [ ElementNode
              { tag = "span"
                attributes = Map["class", "text-xl"]
                children = [ TextNode "HELLO!" ] } ]

let parseHtml (node: string) : Html = failwith "TODO"

let tests () =
    parseAttrsTests ()
    parseTagTests ()
    parseContentTests ()
    ()
