FROM mcr.microsoft.com/dotnet/sdk:5.0.100-preview.8-alpine3.12

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:5.0.0-preview.8-alpine3.12

WORKDIR /app
COPY --from=0 /app/application/bin/Release/net5.0/linux-x64/publish .

ENTRYPOINT ["dotnet", "application.dll"]
