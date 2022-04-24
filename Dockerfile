FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

WORKDIR /app
COPY nuget.config .
COPY spectator.sln .
COPY application/*.fs application/
COPY application/*.fsproj application/
COPY bot/*.fs bot/
COPY bot/*.fsproj bot/
COPY core/*.fs core/
COPY core/*.fsproj core/
COPY notifications/*.fs notifications/
COPY notifications/*.fsproj notifications/
COPY store/*.fs store/
COPY store/*.fsproj store/
COPY telegram/*.fs telegram/
COPY telegram/*.fsproj telegram/
COPY tests/*.fs tests/
COPY tests/*.fsproj tests/
COPY tests/examples tests/examples
COPY web/*.fs web/
COPY web/*.fsproj web/
COPY worker/*.fs worker/
COPY worker/*.fsproj worker/

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

RUN apk add curl && rm -rf /var/cache/apk/*

WORKDIR /app
COPY --from=0 /app/application/bin/Release/net6.0/linux-x64/publish .

HEALTHCHECK --interval=15m --timeout=5s \
  CMD curl -f http://localhost:8888/ || exit 1

ENTRYPOINT ["dotnet", "application.dll"]
