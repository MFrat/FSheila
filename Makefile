FW=netcoreapp2.0

MASTER=src/FSheila/
PROJECT=$(MASTER)FSheila-core.fsproj

#ATTENTION: Paket ./build did not work for Ubuntu 16.04 LST,
#however you can try install by yourself. Just uncomment those snippets of code

install:
	#git clone https://github.com/fsprojects/Paket.git
	#cd Paket
	#./build.sh
	#./install.sh
	apt-get update
	apt-get install dotnet-sdk-2.1.105

install-macos:
	#git clone https://github.com/fsprojects/Paket.git
	#cd Paket
	#./build.sh
	#./install.sh
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