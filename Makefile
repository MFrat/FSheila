FW=netcoreapp2.0

MASTER=src/FSheila/
PROJECT=$(MASTER)FSheila-core.fsproj

install:
	apt-get update
	apt-get install dotnet-sdk-2.1.105

install-macos:
	brew update
	brew tap caskroom/cask
	brew cask install dotnet

clean:
	dotnet clean $(PROJECT) -f $(FW)

build:
	dotnet build $(PROJECT) -f $(FW)

run:
	dotnet run -p $(PROJECT) -f $(FW)

test:
	
distcln: