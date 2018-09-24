.PHONY: deploy

deploy:
	./result/bin/jgt rebuild
	./result/bin/jgt deploy
