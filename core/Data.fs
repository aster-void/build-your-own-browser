module core.Data

open core

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

let rawTextTags = [ "script"; "style"; "title"; "textarea" ]
