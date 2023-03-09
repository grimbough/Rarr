Patches created to remove symbols that R CMD check has decided are warnings.  
This include abort(), exit(), fprintf() etc.  We replace them will R compatible
functions like error(), Rprintf() etc.  

If we update any of these compression libraries, apply the patch with:

cd lib/lz4-1.9.2
patch --ignore-whitespace lz4.c < ../../patches/lz4.diff