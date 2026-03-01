module core.Parser

open core

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
    let mutable escaped = false
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
        // handle escapes (we assume escapes are only done on values, not keys)
        | c when escaped && currentState = AttrsStateValue -> currentValue <- currentValue + string c
        | '\\' -> escaped <- true

        // handle quotation
        | q when currentQuoteState = Some q -> currentQuoteState <- None
        | '"'
        | '\'' as q when currentQuoteState = None -> currentQuoteState <- Some q

        // handle end of key
        | '=' when currentQuoteState = None ->
            currentState <- AttrsStateValue
            skipSpaces () |> ignore
        // handle end of value and tag close
        | ' '
        | '>' when currentQuoteState = None ->
            if currentKey <> "" then
                attributes <- attributes.Add(currentKey, currentValue)
                currentState <- AttrsStateKey
                currentKey <- ""
                currentValue <- ""

            if c = '>' then
                looping <- false
        | '/' when currentQuoteState = None && currentState = AttrsStateKey ->
            // don't panic if it's just a />
            if chars.Peek() <> '>' then
                failwith "bail: unexpected /"
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

// <span class="foo">....</span>... -> (Element {}) * ... |> Some
// <span -> None
let rec parseTag (s: string) : Element * string =
    assert (s.[0] = '<')

    let tagName =
        let idx = s.IndexOfAny [| ' '; '>' |]

        if idx = -1 then
            eprintfn "bail: incomplete tag in parsing tag %A" s
            failwith "local error: should be caught"
        else
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
    let isSelfClosing: bool = List.contains tagName Data.selfClosingTags
    let isRawText: bool = List.contains tagName Data.rawTextTags

    let attrs, childrenAndRest = parseAttrs s.[tagName.Length + 1 ..]

    let children, rest =
        match isSelfClosing with
        | true -> [], childrenAndRest
        | false ->
            let closingTag = "</" + tagName + ">"

            let childrenEnd =
                match childrenAndRest.IndexOf closingTag with
                | -1 ->
                    eprintfn "warning: closing tag is not found for: %s" tagName
                    childrenAndRest.Length
                | rest -> rest

            let children, rest =
                if isRawText then
                    [ TextNode childrenAndRest.[.. childrenEnd - 1] ],
                    childrenAndRest.[childrenEnd + closingTag.Length ..]
                else
                    parseContent childrenAndRest.[.. childrenEnd - 1],
                    childrenAndRest.[childrenEnd + closingTag.Length ..]

            children, rest

    let el: Element = {
        tag = tagName
        attributes = attrs
        children = children
    }

    el, rest

and parseContent (text: string) : Node list =
    match text with
    // base case
    | "" -> []
    // orphaned close tag node
    | s when s.StartsWith "</" -> failwithf "todo: standalone closing tag %s" s
    // comment node
    | s when s.StartsWith "<!" ->
        let END_TOKEN = if s.ToLower().StartsWith "<!--" then "-->" else ">"
        let last = s.IndexOf END_TOKEN

        if last = -1 then
            []
        else
            parseContent s.[last + END_TOKEN.Length ..]
    // tag node
    | s when s.StartsWith "<" ->
        // tag
        let el, rest = parseTag s
        ElementNode el :: parseContent rest
    // text node
    | s ->
        let node_end =
            match s.IndexOf "<" with
            | -1 -> s.Length
            | found -> found - 1

        TextNode s.[..node_end] :: parseContent s.[node_end + 1 ..]

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
    // base test
    let nodes = parseContent "<span class=text-xl>HELLO!</span>"

    test "parseContents base" nodes [
        ElementNode {
            tag = "span"
            attributes = Map["class", "text-xl"]
            children = [ TextNode "HELLO!" ]
        }
    ]
    // comment test
    let nodes =
        parseContent
            "<!DOCTYPE html><!-- some comments --> <!-- some more! --><!-- connected comments <!-- what if comments open in comments? --><span>aaa<!-- comment in between text nodes -->bbb</span><!--html ends with a comment -->"

    test "parseContents comments" nodes [
        TextNode " "
        ElementNode {
            tag = "span"
            attributes = Map []
            children = [ TextNode "aaa"; TextNode "bbb" ]
        }
    ]

    // rawText test
    let nodes =
        parseContent "<script>const text = 'This is just a text <p></p>'</script>"

    test "parseContents rawText" nodes [
        ElementNode {
            tag = "script"
            attributes = Map []
            children = [ TextNode "const text = 'This is just a text <p></p>'" ]
        }
    ]


let parseHtml (full_html: string) : Html =
    let tree = parseContent full_html
    tree

let parseRealworldTests () =
    System.IO.File.ReadAllText "../tests/example.com" |> parseHtml |> ignore
    eprintfn "parseRealworldTests: success example.com"

    System.IO.File.ReadAllText "../tests/justfuckingusehtml.com"
    |> parseHtml
    |> ignore

    eprintfn "parseRealworldTests: success justfuckingusehtml.com"
    // hell no
    // System.IO.File.ReadAllText "../tests/github.com" |> parseHtml |> ignore
    // eprintfn "parseRealworldTests: success github.com"
    eprintfn "parseRealworldTests: all pass"

let tests () =
    parseAttrsTests ()
    parseTagTests ()
    parseContentTests ()
    parseRealworldTests ()
    ()
