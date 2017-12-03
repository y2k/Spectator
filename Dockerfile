FROM microsoft/dotnet:2.0.3-sdk-jessie

WORKDIR /app
COPY . /app
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM microsoft/dotnet:2.0.3-runtime-jessie

WORKDIR /app
COPY --from=0 /app/server/bin/Release/netcoreapp2.0/linux-x64/publish ./server
COPY --from=0 /app/bot/bin/Release/netcoreapp2.0/linux-x64/publish ./bot
COPY --from=0 /app/worker/bin/Release/netcoreapp2.0/linux-x64/publish ./worker

RUN echo "rabbitmq-server start & mongod --fork --dbpath /app/resources --logpath /app/resources/mongodb.log & dotnet server/server.dll & dotnet worker/worker.dll & dotnet bot/bot.dll"  > start.sh && \
    chmod +x start.sh

# Install Mongo & RabbitMQ
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6 && \
    echo 'deb http://repo.mongodb.org/apt/debian jessie/mongodb-org/3.4 main' | tee /etc/apt/sources.list.d/mongodb-org-3.4.list && \
    echo 'deb http://www.rabbitmq.com/debian/ testing main' | tee /etc/apt/sources.list.d/rabbitmq.list && \
    apt-get update && \
    apt-get install --no-install-recommends -y --force-yes rabbitmq-server mongodb-org && \
    rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/bin/bash", "-c", "./start.sh"]