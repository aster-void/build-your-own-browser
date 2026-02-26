// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main argv : int =
    if argv.Length > 0 && argv.[0] = "test" then
        Byobu.Tests.run ()
        0
    else
        0
