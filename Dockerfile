FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

WORKDIR /app
COPY spectator.sln .

COPY application/*.fsproj application/
COPY bot/*.fsproj bot/
COPY core/*.fsproj core/
COPY notifications/*.fsproj notifications/
COPY prelude/Atom/*.fsproj prelude/Atom/
COPY prelude/Core/*.fsproj prelude/Core/
COPY prelude/event-bus-api/*.fsproj prelude/event-bus-api/
COPY prelude/event-bus/*.fsproj prelude/event-bus/
COPY store/*.fsproj store/
COPY telegram/*.fsproj telegram/
COPY tests/*.fsproj tests/
COPY web/*.fsproj web/
COPY worker/*.fsproj worker/

RUN dotnet restore

COPY application/*.fs application/
COPY bot/*.fs bot/
COPY core/*.fs core/
COPY notifications/*.fs notifications/
COPY prelude/Atom/*.fs prelude/Atom/
COPY prelude/Core/*.fs prelude/Core/
COPY prelude/event-bus-api/*.fs prelude/event-bus-api/
COPY prelude/event-bus/*.fs prelude/event-bus/
COPY store/*.fs store/
COPY telegram/*.fs telegram/
COPY tests/*.fs tests/
COPY tests/common/*.fs tests/common/
COPY tests/examples tests/examples
COPY web/*.fs web/
COPY worker/*.fs worker/

RUN dotnet test --no-restore
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

RUN apk add curl && rm -rf /var/cache/apk/*

WORKDIR /app
COPY --from=0 /app/application/bin/Release/net6.0/linux-x64/publish .

HEALTHCHECK --interval=15m --timeout=5s \
  CMD curl -f http://localhost:8888/ || exit 1

ENTRYPOINT ["dotnet", "application.dll"]
