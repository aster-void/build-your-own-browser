module ui.Render

open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.DSL
open Avalonia.Media
open Avalonia.Layout
open Avalonia
open ui.Event
open ui.Model
open Option
open System

let isInline (node: core.Node) =
    match node with
    | core.TextNode _ -> true
    | core.ElementNode e ->
        List.contains
            e.tag
            [ "span"
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
              "label" ]

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


type AnonymousBlockBox =
    | AnonymousBlockWrap of core.Node list
    | AnonymousBlockPassthru of core.Node

    member self.canAppend =
        match self with
        | AnonymousBlockWrap _ -> true
        | AnonymousBlockPassthru b -> isInline b

    member self.append(n: core.Node) : AnonymousBlockBox =
        match self with
        | AnonymousBlockWrap nodes -> nodes @ [ n ] |> AnonymousBlockWrap
        | AnonymousBlockPassthru node -> [ node; n ] |> AnonymousBlockWrap

let rec render (dispatch: Msg -> unit) (context: TextBlock IAttr list) (dom_node: core.Node) : IView option =
    match dom_node with
    | core.TextNode t ->
        if t.Trim() = "" then
            None
        else
            let block =
                TextBlock.create (
                    [ TextBlock.text (formatText t)
                      TextBlock.textWrapping Avalonia.Media.TextWrapping.Wrap ]
                    @ context
                )

            block :> IView |> Some
    | core.ElementNode { tag = tag
                         attributes = attributes
                         children = children } ->
        let passthru attrs : IView Option =
            List.fold
                (fun (acc: AnonymousBlockBox list) node ->
                    match isInline node with
                    | true when acc.Length > 0 && acc.Head.canAppend -> acc.[0].append node :: acc.[1..]
                    | _ -> AnonymousBlockPassthru node :: acc)
                []
                children
            |> List.rev
            |> List.map (function
                | AnonymousBlockPassthru node -> render dispatch (attrs @ context) node
                | AnonymousBlockWrap nodes ->
                    match renderAll dispatch (attrs @ context) nodes with
                    | [] -> None
                    | some -> WrapPanel.create [ WrapPanel.children some ] :> IView |> Some)
            |> List.choose id
            |> function
                | [] -> None
                | some -> StackPanel.create [ StackPanel.children some ] :> IView |> Some

        match tag with
        // = invisible tags
        | "script"
        | "style"
        | "meta"
        | "link"
        | "title" -> None
        // = children-only tags
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
        | "section" -> passthru []

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
        | "rp" -> passthru []

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
        | "ins" -> passthru []

        // need custom styles
        // == blocks
        | "h1" -> passthru [ TextBlock.fontSize 60; TextBlock.margin (Thickness(0, 16, 0, 8)) ]
        | "h2" -> passthru [ TextBlock.fontSize 40; TextBlock.margin (Thickness(0, 12, 0, 6)) ]
        | "h3" -> passthru [ TextBlock.fontSize 30; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        | "h4" -> passthru [ TextBlock.fontSize 24; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        | "h5" -> passthru [ TextBlock.fontSize 20; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        | "h6" -> passthru [ TextBlock.fontSize 16; TextBlock.margin (Thickness(0, 8, 0, 4)) ]
        | "strong"
        | "bold"
        | "div"
        | "ol"
        | "ul"
        | "li"
        | "hr"
        | "blockquote" -> passthru [ TextBlock.margin (Thickness(40, 8, 0, 8)) ]
        | "span" -> passthru []
        | "p" -> passthru [ TextBlock.margin (Thickness(0, 8, 0, 8)) ]
        | "a" ->
            let href = attributes.TryFind "href"

            let attrs =
                [ TextBlock.textDecorations TextDecorations.Underline
                  TextBlock.foreground "blue" ]
                @ match href with
                  | Some href -> [ TextBlock.onPointerPressed (fun _ -> dispatch (NavigateTo href)) ]
                  | None -> []

            passthru attrs
        | "button" ->
            let children = getText children
            Button.create [ Button.content children ] :> IView |> Some
        | "strong"
        | "b" -> passthru [ TextBlock.fontWeight FontWeight.Bold ]
        | "em"
        | "i" -> passthru [ TextBlock.fontStyle FontStyle.Italic ]
        | "u" -> passthru [ TextBlock.textDecorations TextDecorations.Underline ]
        | "s"
        | "strike" -> passthru [ TextBlock.textDecorations TextDecorations.Strikethrough ]
        | "small" -> passthru [ TextBlock.fontSize 12 ]
        | "code"
        | "kbd"
        | "samp" -> passthru [ TextBlock.fontFamily "monospace"; TextBlock.foreground "lightgray" ]
        | "sup" -> passthru [ TextBlock.fontSize 10; TextBlock.margin (Thickness(0, -4, 0, 0)) ]
        | "sub" -> passthru [ TextBlock.fontSize 10; TextBlock.margin (Thickness(0, 4, 0, 0)) ]
        | "mark" -> passthru [ TextBlock.background "yellow"; TextBlock.foreground "black" ]
        | "br" -> TextBlock.create [ TextBlock.text "\n" ] :> IView |> Some
        | "wbr"
        | "img"
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
        | "progress" -> passthru []
        | _ ->
            eprintfn "warning: Unknown tag: %A" tag
            passthru []

and renderAll dispatch (context: TextBlock IAttr list) (nodes: core.Node list) : IView list =
    nodes |> List.map (render dispatch []) |> List.choose id

let renderHtml dispatch (dom: core.Html) : IView =
    let view = renderAll dispatch [] dom
    StackPanel.create [ StackPanel.children view ]
