test:
    cd cli; dotnet run -- test

ui:
    cd ui; dotnet run -- "file:///home/aster/workspace/github.com/aster-void/build-your-own-browser/tests/justfuckingusehtml.com"

fmt: format
format:
    dotnet fantomas .

