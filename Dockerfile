FROM mcr.microsoft.com/dotnet/sdk:5.0.100-alpine3.12-amd64

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:5.0.0-alpine3.12-amd64

WORKDIR /app
COPY --from=0 /app/application/bin/Release/net5.0/linux-x64/publish .

HEALTHCHECK --interval=15m --timeout=5s \
  CMD curl -f http://localhost:8888/ || exit 1

ENTRYPOINT ["dotnet", "application.dll"]
