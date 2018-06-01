build:
	stack build --fast -j4 --extra-include-dirs=/usr/local/opt/openssl/include

build-watch:
	stack build --fast --file-watch --extra-include-dirs=/usr/local/opt/openssl/include

copy-bins:
	stack build --fast -j4 --copy-bins --local-bin-path bin --extra-include-dirs=/usr/local/opt/openssl/include

echo-warn:
	echo "Testing in progress"

test: echo-warn
	stack test -j4

static:
	touch Settings/StaticFiles.hs

ghci:
	stack ghci investments-info:lib

dev:
	stack exec -- yesod devel

ghci-object:
	stack ghci --ghci-options -fobject-code investments-info:lib


copy-remote:
	scp -i  ~/Documents/investments-info/finance-info.pem ~/code/investments-info/bin/investments-info  ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com:/home/ubuntu

deploy-bin: copy-bins
	sudo ./deploy.sh

ssh-aws:
	ssh -i ~/Documents/investments-info/finance-info.pem ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com

install:
	stack build --copy-bins  --local-bin-path bin  --extra-include-dirs=/usr/local/opt/openssl/include
