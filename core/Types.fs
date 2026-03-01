namespace core

type Element = {
    tag: string
    attributes: Map<string, string>
    children: Node list
}

and Node =
    | TextNode of string
    | ElementNode of Element

    override self.ToString() : string =
        match self with
        | ElementNode q ->
            let children = q.children |> List.map string |> String.concat ""
            let attrs = q.attributes |> Map.fold (fun acc k v -> acc + $" {k}=\"{v}\"") ""

            // fuck DRY
            let selfClosingTags = [
                "area"
                "base"
                "br"
                "col"
                "embed"
                "hr"
                "img"
                "input"
                "source"
                "track"
                "wbr"
                "meta"
                "link"
            ]

            let selfClosing = List.contains q.tag selfClosingTags

            if selfClosing then
                $"<{q.tag} {attrs}/>"
            else
                $"<{q.tag}>{children}</{q.tag}>"
        | TextNode t -> $"<text>{t}</text>"

type Html = Node list
