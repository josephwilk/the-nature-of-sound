swig: src
	cd resources/LibXtract/ && $(MAKE) -C $@
	cp resources/LibXtract/swig/*.java src/java/
	cp resources/LibXtract/swig/libxtract.dylib libxtract.dylib