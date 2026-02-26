module core.Data

open core.Types

let tags: TagDefinition list =
    [
      // head and meta tags
      { name = "script"
        controls = [ IgnoreContent ] }
      { name = "style"
        controls = [ IgnoreContent ] }
      { name = "meta"
        controls = [ SelfClosing ] }
      { name = "link"
        controls = [ SelfClosing ] }

      // body and contents tags
      { name = "h1"; controls = [] }
      { name = "span"; controls = [] }
      { name = "div"; controls = [] }
      { name = "code"; controls = [] }
      { name = "p"; controls = [] }
      { name = "br"
        controls = [ SelfClosing ] } ]

let defaultTag: TagDefinition = { name = "input"; controls = [] }
