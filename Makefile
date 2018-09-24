.PHONY: deploy

deploy:
	nix-shell -p nodejs nodePackages.yarn --command "./result/bin/jgt rebuild"
	nix-shell -p nodejs nodePackages.yarn --command "yarn install"
	nix-shell -p nodejs nodePackages.yarn --command "./result/bin/jgt deploy"
