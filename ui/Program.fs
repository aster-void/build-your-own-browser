module ui.Program

open System
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Hosts
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Elmish
open Elmish

type MainWindow(argv: string array) as self =
    inherit HostWindow()

    do
        base.Title <- "My Browser"
        self.AttachDevTools(Input.KeyGesture Input.Key.F12)

        Program.mkProgram (fun () -> Event.init argv) Event.update View.view
        |> Program.withHost self
        |> Program.run

type App(argv: string array) =
    inherit Application()

    override self.Initialize() : unit = self.Styles.Add(FluentTheme())

    override self.OnFrameworkInitializationCompleted() : unit =
        match self.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- MainWindow argv
        | _ -> ()

[<EntryPoint; STAThread>]
let main argv =
    AppBuilder.Configure<Application>(fun () -> App argv).UsePlatformDetect().StartWithClassicDesktopLifetime argv
