module ui.Model

type ContentState =
    | Loading
    | Errored of exn
    | Dom of core.Html

type State =
    { navbar_content: string
      current_url: string
      content: ContentState }

type Msg =
    | NavigateToNavbarURL
    | NavigateTo of url: string
    | NavbarTyped of string
    | LoadingSuccess of body: string
    | LoadingError of error: exn
