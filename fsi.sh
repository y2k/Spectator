dotnet build

clear

fsharpi -r application/bin/Debug/netcoreapp2.1/worker.dll \
        -r application/bin/Debug/netcoreapp2.1/core.dll \
        -r application/bin/Debug/netcoreapp2.1/bot.dll \
        -r $HOME/.nuget/packages/mongodb.driver/2.7.2/lib/netstandard1.5/MongoDB.Driver.dll \
        -r $HOME/.nuget/packages/mongodb.bson/2.7.2/lib/netstandard1.5/MongoDB.Bson.dll \
        -r $HOME/.nuget/packages/mongodb.driver.core/2.7.2/lib/netstandard1.5/MongoDB.Driver.Core.dll
