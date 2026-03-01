test:
    cd cli; dotnet run -- test

ui url="":
    cd ui; dotnet run -- {{url}}

debug:
    just ui "file:///home/aster/workspace/github.com/aster-void/build-your-own-browser/tests/justfuckingusehtml.com"

fmt: format
format:
    dotnet fantomas .

