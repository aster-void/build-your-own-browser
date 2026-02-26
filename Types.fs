namespace Byobu.Types

type Element =
    { tag: string
      attributes: Map<string, string>
      children: Node list }

and Node =
    | TextNode of string
    | ElementNode of Element

type Html = { head: Node list; body: Node list }

type TagControl =
    | IgnoreContent
    | SelfClosing

type TagDefinition =
    { name: string
      controls: TagControl list }
