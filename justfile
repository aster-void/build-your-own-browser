test:
    cd cli; dotnet run -- test

ui:
    cd ui; dotnet run

fmt: format
format:
    dotnet fantomas .

