build:
	stack build --fast -j4

build-watch:
	stack build --fast --file-watch

copy-bins:
	stack build --copy-bins --local-bin-path bin

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

deploy-bin: build copy-bins
	sudo ./deploy.sh

ssh-aws:
	ssh -i ~/Documents/investments-info/finance-info.pem ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com
