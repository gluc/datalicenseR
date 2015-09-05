all:
	@if test -d "C:/" ; then echo "building on Windows..."; make -f makefile.dos; else echo "buildin on *nix"; make -f Makefile.uni; fi

clean:
	/bin/rm -f *.o *.so tags core rpw destest des speed libdes.* .nfs* *.old \
	*.bak destest rpw des_opts 
# DO NOT DELETE THIS LINE -- make depend depends on it.
