# Запуск в docker

```bash
docker run -d -v "PATH-TO-FILES-DIR":/data --name spectator y2khub/spectator /data/settings.yaml
```

# Настройки (settings.yaml)

```yaml
filesDir: /data/files
mongoDomain: localhost | remote host
restTelegramPassword: NOT USED
restTelegramBaseUrl: NOT USED
telegramToken: telegram bot api token
updateTimeMinutes: 1
```
