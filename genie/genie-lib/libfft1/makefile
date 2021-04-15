#
# $Id: makefile 6303 2011-03-30 14:01:52Z gethin-williams $
#

LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(LIBFFT1PREC)

LIBNAME = libfft1
OBJECTS = fax.$(OBJ_EXT) fft991.$(OBJ_EXT) fftrig.$(OBJ_EXT) \
		qpassm.$(OBJ_EXT) rpassm.$(OBJ_EXT) set99.$(OBJ_EXT)

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

fax.$(OBJ_EXT)		: fax.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) fax.f

fft991.$(OBJ_EXT)		: fft991.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) fft991.f

fftrig.$(OBJ_EXT)		: fftrig.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) fftrig.f

qpassm.$(OBJ_EXT)		: qpassm.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) qpassm.f

rpassm.$(OBJ_EXT)		: rpassm.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) rpassm.f

set99.$(OBJ_EXT)		: set99.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) set99.f
