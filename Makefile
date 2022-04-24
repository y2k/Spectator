test:
	dotnet test

cbuild:
	find . -iname "bin" | xargs rm -rf
	find . -iname "obj" | xargs rm -rf
	dotnet clean && dotnet build

.PHONY: test cbuild
