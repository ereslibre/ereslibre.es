gen:
	emacs -L $(PWD) --batch --script init.el

.ONESHELL:
publish: clean gen
	pushd public_html
	git init
	git checkout -b main
	git add .
	git commit --no-gpg-sign -a -m "Publish static site"
	git remote add origin git@github.com:ereslibre/ereslibre.es
	git push -f origin main:publish
	popd

clean:
	rm -rf public_html
