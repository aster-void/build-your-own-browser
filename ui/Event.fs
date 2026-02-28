module ui.Event

open ui.Model
open Elmish
open System
open Avalonia.Threading
open System.Net.Http

let fetchUrl (url: string) =
    async {
        use client = new HttpClient()
        let! response = client.GetStringAsync url |> Async.AwaitTask
        return response
    }

let readFile (url: string) =
    async {
        let uri = Uri url
        let! file = System.IO.File.ReadAllTextAsync uri.LocalPath |> Async.AwaitTask
        return file
    }

let Cmd_NavigateTo (url: string) =
    (fun dispatch ->
        async {
            try
                let! content =
                    match url with
                    | url when url.StartsWith "file:" -> readFile url
                    | url when url.StartsWith "http:" || url.StartsWith "https://" -> fetchUrl url
                    | url when url.Contains ":" ->
                        let colonIdx = url.IndexOf ':'
                        eprintfn "warning: Unknown schema: %s" url.[0..colonIdx]
                        fetchUrl url
                    | url -> "http://" + url |> fetchUrl

                Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> LoadingSuccess content |> dispatch)
            with err ->
                Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> LoadingError err |> dispatch)
        }
        |> Async.Start)
    |> Cmd.ofEffect

let init (argv: string array) =
    let init_url = if Array.length argv = 0 then "" else Array.head argv

    if init_url = "" then
        { navbar_content = ""
          current_url = ""
          content = exn "Please enter URL" |> Errored },
        Cmd.none
    else
        { navbar_content = init_url
          current_url = init_url
          content = Loading },
        Cmd_NavigateTo init_url

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    eprintfn "%A" msg

    match msg with
    | NavigateToNavbarURL -> state, NavigateTo state.navbar_content |> Cmd.ofMsg
    | NavigateTo url ->
        { state with
            navbar_content = url
            current_url = url
            content = Loading },
        let cmd = Cmd_NavigateTo url

        cmd
    | NavbarTyped s -> { state with navbar_content = s }, Cmd.none
    | LoadingSuccess content ->
        let parsed = core.Parser.parseHtml content
        { state with content = Dom parsed }, Cmd.none
    | LoadingError error -> { state with content = Errored error }, Cmd.none
