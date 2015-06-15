iwatch-test:
	while true; do \
		inotifywait -e close_write,moved_to,create --exclude "(flycheck|\#)" . test;\
		cask exec ert-runner -l test/boron-test.el test/boron-parser-test.el; \
		cask exec ert-runner -l test/boron-test.el test/boron-core-test.el; \
	done;
