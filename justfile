build:
    dotnet run --project Rynco.KuiperSans
verify: build
    pnpx ts-node --esm index.mts dist/KuiperSans-Regular.ttf
