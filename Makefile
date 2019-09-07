gen:
	emacs -L $(PWD) --batch --script init.el

.ONESHELL:
publish: clean gen
	pushd public_html
	git init
	git add .
	git remote add origin git@github.com:ereslibre/ereslibre.es
	git commit -a -m "Publish static site"
	git push -f origin master:publish
	popd

clean:
	rm -rf public_html
