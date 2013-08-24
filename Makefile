all ::
	runghc Setup configure --user --prefix=c:/src/packages
	runghc Setup build
	runghc Setup install

doc : 
	runghc Setup haddock --haddock-options="--use-contents=flickr.html --use-index=flickr-doc-index.html"
	cp flickr*html dist/doc/html/flickr/
