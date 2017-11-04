FROM microsoft/dotnet:2.0.0-runtime-jessie

EXPOSE 8080

# Install Mongo
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6 && \
    echo "deb http://repo.mongodb.org/apt/debian jessie/mongodb-org/3.4 main" | tee /etc/apt/sources.list.d/mongodb-org-3.4.list && \
    apt-get update && \
    apt-get install -y mongodb-org

# Install RabbitMQ
RUN echo 'deb http://www.rabbitmq.com/debian/ testing main' | tee /etc/apt/sources.list.d/rabbitmq.list && \
    apt-get update && \
    apt-get install -y --force-yes rabbitmq-server

# Install .NET application
WORKDIR /app
COPY . .

RUN echo "rabbitmq-server start & dotnet server/bin/Debug/netcoreapp2.0/publish/server.dll & dotnet worker/bin/Debug/netcoreapp2.0/publish/worker.dll & dotnet bot/bin/Debug/netcoreapp2.0/publish/bot.dll"  > start.sh && \
    chmod 777 start.sh

ENTRYPOINT ["/bin/bash", "-c", "./start.sh"]