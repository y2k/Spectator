FROM mcr.microsoft.com/dotnet/core/sdk:3.1.102-alpine3.11

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/core/runtime:2.2.5-alpine3.9

WORKDIR /app
COPY --from=0 /app/application/bin/Release/netcoreapp2.1/linux-x64/publish .

ENTRYPOINT ["dotnet", "application.dll"]
