gen:
	.devenv/profile/bin/emacs -L $(PWD) --batch --script init.el

publish: clean gen
	pushd public_html
	git init
	git checkout -b main
	git add .
	git commit --no-gpg-sign -a -m "Publish static site"
	git remote add origin git@github.com:ereslibre/ereslibre.es
	git push -f origin main:publish
	popd

serve: gen
	sh -c 'cd public_html && python3 -m http.server'

clean:
	rm -rf public_html
