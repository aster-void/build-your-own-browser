module ui.Render

open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.DSL
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Layout
open Avalonia
open Avalonia.FuncUI
open System.Net.Http
open ui.Event
open ui.Model
open Option
open System

// consts
let builtinInheritableStyles: Map<string, TextBlock IAttr list> =
    Map["h1", [ TextBlock.fontSize 60; TextBlock.margin (Thickness(0, 16, 0, 8)) ]
        "h2", [ TextBlock.fontSize 40; TextBlock.margin (Thickness(0, 12, 0, 6)) ]
        "h3", [ TextBlock.fontSize 30; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h4", [ TextBlock.fontSize 24; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h5", [ TextBlock.fontSize 20; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h6", [ TextBlock.fontSize 16; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "blockquote", [ TextBlock.margin (Thickness(40, 8, 0, 8)) ]
        "span", []

        "a",
        [
            TextBlock.textDecorations TextDecorations.Underline
            TextBlock.foreground "blue"
        ]

        "strong", [ TextBlock.fontWeight FontWeight.Bold ]
        "b", [ TextBlock.fontWeight FontWeight.Bold ]
        "em", [ TextBlock.fontStyle FontStyle.Italic ]
        "i", [ TextBlock.fontStyle FontStyle.Italic ]
        "u", [ TextBlock.textDecorations TextDecorations.Underline ]
        "s", [ TextBlock.textDecorations TextDecorations.Strikethrough ]
        "strike", [ TextBlock.textDecorations TextDecorations.Strikethrough ]
        "small", [ TextBlock.fontSize 12 ]
        "code", [ TextBlock.fontFamily "monospace"; TextBlock.foreground "lightgray" ]
        "kbd", [ TextBlock.fontFamily "monospace"; TextBlock.foreground "lightgray" ]
        "samp", [ TextBlock.fontFamily "monospace"; TextBlock.foreground "lightgray" ]
        "sup", [ TextBlock.fontSize 10 ]
        "sub", [ TextBlock.fontSize 10 ]
        "mark", [ TextBlock.background "yellow"; TextBlock.foreground "black" ]]

let builtinSelfStyles =
    Map["p", [ TextBlock.margin (Thickness(0, 8, 0, 8)) ]
        "sup", [ TextBlock.margin (Thickness(0, -4, 0, 0)) ]
        "sub", [ TextBlock.margin (Thickness(0, 4, 0, 0)) ]

        "h1", [ TextBlock.margin (Thickness(0, 16, 0, 8)) ]
        "h2", [ TextBlock.margin (Thickness(0, 12, 0, 6)) ]
        "h3", [ TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h4", [ TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h5", [ TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "h6", [ TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        "blockquote", [ TextBlock.margin (Thickness(40, 8, 0, 8)) ]
        "p", [ TextBlock.margin (Thickness(0, 8, 0, 8)) ]]

let isInline (node: core.Node) =
    match node with
    | core.TextNode _ -> true
    | core.ElementNode e ->
        List.contains e.tag [
            "span"
            "a"
            "em"
            "strong"
            "b"
            "i"
            "u"
            "s"
            "small"
            "mark"
            "code"
            "kbd"
            "samp"
            "var"
            "sub"
            "sup"
            "br"
            "wbr"
            "q"
            "cite"
            "abbr"
            "dfn"
            "data"
            "time"
            "bdi"
            "bdo"
            "ruby"
            "rt"
            "rp"
            "img"
            "input"
            "button"
            "select"
            "textarea"
            "label"
        ]

// utils

let rec getText (node: core.Node list) : string =
    node
    |> List.map (function
        | core.TextNode t -> t
        | core.ElementNode el -> el.children |> getText)
    |> String.Concat

let formatText s =
    Text.RegularExpressions.Regex.Replace(s, "\\s+", " ")
    |> Net.WebUtility.HtmlDecode
    |> fun s -> s.Replace("&check;", "✔")
    |> fun s -> s.Replace("\n", "\\n")

// types

type ListType =
    | NoList
    | Ol of int
    | Ul

type Context = {
    dispatch: Msg -> unit
    text_decorations: TextBlock IAttr list
    list_type: ListType
}

// components
type ImageLoadingState =
    | ImLoading
    | ImError of exn
    | ImData of Bitmap

let client = new HttpClient()

let Image (attrs: Map<string, string>) =
    Component.create (
        "image",
        fun cx ->
            let url = attrs.TryFind "src"
            let data = cx.useState ImLoading

            cx.useEffect (
                fun () ->
                    async {
                        try
                            match url with
                            | None -> failwith "Image URL not specified"
                            | Some url when url.StartsWith "http://" || url.StartsWith "https://" ->
                                let! stream = client.GetStreamAsync url |> Async.AwaitTask
                                data.Set(ImData(new Bitmap(stream)))
                            | Some url when url.StartsWith "file://" -> failwith "file image loading not supported"
                            | Some url -> failwithf "unknown schema: %s" url
                        with ex ->
                            data.Set(ImError ex)
                    }
                    |> Async.Start

                    ()
                , [ EffectTrigger.AfterInit ]
            )

            match data.Current with
            | ImLoading -> Panel.create []
            | ImError ex -> TextBlock.create [ TextBlock.text ex.Message ]
            | ImData bm -> Image.create [ Image.source bm ]
    )

// = renderer
type ContextedNode = { node: core.Node; c: Context }

type AnonymousBlockBox =
    | AnonymousBlockWrap of ContextedNode list
    | AnonymousBlockPassthru of ContextedNode

    member self.canAppend =
        match self with
        | AnonymousBlockWrap _ -> true
        | AnonymousBlockPassthru b -> isInline b.node

    member self.append(n: ContextedNode) : AnonymousBlockBox =
        match self with
        | AnonymousBlockWrap nodes -> nodes @ [ n ] |> AnonymousBlockWrap
        | AnonymousBlockPassthru node -> [ node; n ] |> AnonymousBlockWrap

let rec render ({ c = c; node = node }: ContextedNode) : IView option =
    match node with
    | core.TextNode t ->
        if t.Trim() = "" then
            None
        else
            let block =
                TextBlock.create (
                    [
                        TextBlock.text (formatText t)
                        TextBlock.textWrapping Avalonia.Media.TextWrapping.Wrap
                    ]
                    @ c.text_decorations
                )

            block :> IView |> Some
    | core.ElementNode {
                           tag = tag
                           attributes = attributes
                           children = children
                       } ->


        let appendAttrs selfAttrs customizer =
            let inheritAttrs = builtinInheritableStyles.TryFind tag |> Option.defaultValue []

            let selfAttrs =
                selfAttrs @ (builtinSelfStyles.TryFind tag |> Option.defaultValue [])

            let c =
                customizer {
                    c with
                        text_decorations = inheritAttrs @ c.text_decorations
                }

            toView (children |> List.map (fun node -> { c = c; node = node }))
            |> function
                | [] -> None
                | some -> StackPanel.create ([ StackPanel.children some ] @ selfAttrs) :> IView |> Some

        let passthru = appendAttrs [] id

        match tag with
        // = invisible tags
        | "script"
        | "style"
        | "meta"
        | "link"
        | "title" -> None
        // style-only tags
        | "html"
        | "head"
        | "body"
        | "noscript" // we don't script
        | "header"
        | "footer"
        | "nav"
        | "main"
        | "aside"
        | "article"
        | "section" -> passthru

        // what are those?
        | "search"
        | "hgroup"
        | "base"
        | "pre"
        | "menu"
        | "samp"
        | "var"
        | "q"
        | "cite"
        | "abbr"
        | "dfn"
        | "data"
        | "time"
        | "bdi"
        | "bdo"
        | "ruby"
        | "rt"
        | "rp"

        // not planned
        | "dt"
        | "dd"
        | "dl"
        | "figure"
        | "figcaption"
        | "source"
        | "track"
        | "map"
        | "area"
        | "picture"
        | "canvas"
        | "iframe"
        | "embed"
        | "object"
        | "svg"
        | "math"
        | "template"
        | "slot"
        | "del"
        | "ins"

        // style-only normal
        | "h1"
        | "h2"
        | "h3"
        | "h4"
        | "h5"
        | "h6"
        | "strong"
        | "bold"
        | "div"
        | "hr"
        | "blockquote"
        | "span"
        | "p"
        | "strong"
        | "b"
        | "em"
        | "i"
        | "u"
        | "s"
        | "strike"
        | "small"
        | "code"
        | "kbd"
        | "samp"
        | "sup"
        | "sub"
        | "mark"
        | "wbr"
        | "audio"
        | "video"
        | "center"
        | "font"
        | "big"
        | "small"
        | "marquee"
        | "tt"
        | "nobr"
        | "acronym"
        | "dir"
        | "frame"
        | "frameset"
        | "noframes"
        | "plaintext"
        | "xmp"
        // interactive
        | "details"
        | "summary"
        | "dialog"
        // form
        | "form"
        | "input"
        | "textarea"
        | "select"
        | "option"
        | "optgroup"
        | "label"
        | "fieldset"
        | "legend"
        | "datalist"
        | "output"
        | "meter"
        | "progress" -> passthru
        // custom renderer
        | "button" ->
            let children = getText children
            Button.create [ Button.content children ] :> IView |> Some
        | "a" ->
            let href = attributes.TryFind "href"

            let selfAttrs =
                match href with
                | Some href -> [ TextBlock.onPointerPressed (fun _ -> c.dispatch (NavigateTo href)) ]
                | None -> []

            appendAttrs selfAttrs id
        | "br" -> TextBlock.create [ TextBlock.text "\n" ] :> IView |> Some
        | "ul" -> appendAttrs [] (fun c -> { c with list_type = Ul })
        | "ol" ->
            children
            |> List.mapFold
                (fun i el ->
                    match el with
                    | core.ElementNode e when e.tag = "li" ->
                        {
                            node = core.ElementNode e
                            c = { c with list_type = Ol i }
                        },
                        i + 1
                    | other -> { node = other; c = c }, i)
                1
            |> fst
            |> toView
            |> function
                | [] -> None
                | views -> StackPanel.create [ StackPanel.children views ] :> IView |> Some
        | "li" ->
            let prefix =
                match c.list_type with
                | NoList -> "~ "
                | Ol ct -> $"{ct}. "
                | Ul -> "・ "

            passthru
            |> Option.map (fun k ->
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [ TextBlock.create [ TextBlock.text prefix ]; k ]
                ])
        | "img" -> Image attributes :> IView |> Some
        // unknown
        | _ ->
            eprintfn "warning: Unknown tag: %A" tag
            passthru

and toView (children: ContextedNode list) : IView list =
    List.fold
        (fun (acc: AnonymousBlockBox list) (cnode: ContextedNode) ->
            match isInline cnode.node with
            | true when acc.Length > 0 && acc.Head.canAppend -> acc.[0].append cnode :: acc.[1..]
            | _ -> AnonymousBlockPassthru cnode :: acc)
        []
        children
    |> List.rev
    |> List.map (function
        | AnonymousBlockPassthru cn -> render cn
        | AnonymousBlockWrap cnodes ->
            match cnodes |> List.map render |> List.choose id with
            | [] -> None
            | some -> WrapPanel.create [ WrapPanel.children some ] :> IView |> Some)
    |> List.choose id

let renderHtml dispatch (dom: core.Html) : IView =
    let baseContext = {
        dispatch = dispatch
        text_decorations = []
        list_type = NoList
    }

    let view =
        dom
        |> List.map (fun node -> { node = node; c = baseContext })
        |> List.map render
        |> List.choose id

    StackPanel.create [ StackPanel.children view ]
