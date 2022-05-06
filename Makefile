test:
	dotnet test

build:
	dotnet build

clean:
	dotnet clean
	find . -iname "bin" | xargs rm -rf
	find . -iname "obj" | xargs rm -rf

cbuild: clean build

docker:
	docker build --progress plain .

.PHONY: test build clean cbuild docker
