FW=netcoreapp2.0

MASTER=src/FSheila/
PROJECT=$(MASTER)FSheila-core.fsproj

clean:
	dotnet clean $(PROJECT) -f $(FW)

build:
	dotnet build $(PROJECT) -f $(FW)

run:
	dotnet run -p $(PROJECT) -f $(FW)

test:
	
distcln: