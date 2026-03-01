module ui.View

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open ui.Model

let view (state: State) (dispatch: Msg -> unit) =
    let contents: IView =
        match state.content with
        | Dom dom -> ScrollViewer.create [ ScrollViewer.content (Render.renderHtml dispatch dom) ]
        | Loading -> TextBlock.create [ TextBlock.text "loading..." ]
        | Errored err -> TextBlock.create [ TextBlock.text err.Message ]

    let navbar =
        TextBox.create [
            DockPanel.dock Dock.Top
            TextBox.text state.navbar_content
            TextBox.onTextChanged (NavbarTyped >> dispatch)
            TextBox.onKeyDown (fun e ->
                if e.Key = Key.Enter then
                    dispatch NavigateToNavbarURL
                    e.Handled <- true)
        ]

    let screen = DockPanel.create [ DockPanel.children [ navbar; contents ] ]
    screen
