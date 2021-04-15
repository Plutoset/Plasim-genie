#
# $Id: makefile 6303 2011-03-30 14:01:52Z gethin-williams $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(LIBBLAS1PREC)

LIBNAME = libblas1
OBJECTS = cdotc.$(OBJ_EXT) cdotu.$(OBJ_EXT) icamax.$(OBJ_EXT) \
          ilaenv.$(OBJ_EXT) isamax.$(OBJ_EXT) lsame.$(OBJ_EXT) \
          scnrm2.$(OBJ_EXT) sdot.$(OBJ_EXT) sgemm.$(OBJ_EXT) \
          sgemv.$(OBJ_EXT) sger.$(OBJ_EXT) sgetf2.$(OBJ_EXT) sgetrf.$(OBJ_EXT) \
          sgetri.$(OBJ_EXT) slaswp.$(OBJ_EXT) sscal.$(OBJ_EXT) \
          sswap.$(OBJ_EXT) strmm.$(OBJ_EXT) strmv.$(OBJ_EXT) strsm.$(OBJ_EXT) \
          strti2.$(OBJ_EXT) strtri.$(OBJ_EXT) xerbla.$(OBJ_EXT) scabs1.$(OBJ_EXT) 

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

cdotc.$(OBJ_EXT)		: cdotc.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) cdotc.f

cdotu.$(OBJ_EXT)		: cdotu.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) cdotu.f

icamax.$(OBJ_EXT)		: icamax.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) icamax.f

ilaenv.$(OBJ_EXT)		: ilaenv.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) ilaenv.f

isamax.$(OBJ_EXT)		: isamax.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) isamax.f

lsame.$(OBJ_EXT)		: lsame.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lsame.f

scnrm2.$(OBJ_EXT)		: scnrm2.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) scnrm2.f

sdot.$(OBJ_EXT)		: sdot.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sdot.f

sgemm.$(OBJ_EXT)		: sgemm.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sgemm.f

sgemv.$(OBJ_EXT)		: sgemv.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sgemv.f

sger.$(OBJ_EXT)		: sger.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sger.f

sgetf2.$(OBJ_EXT)		: sgetf2.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sgetf2.f

sgetrf.$(OBJ_EXT)		: sgetrf.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sgetrf.f

sgetri.$(OBJ_EXT)		: sgetri.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sgetri.f

slaswp.$(OBJ_EXT)		: slaswp.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) slaswp.f

sscal.$(OBJ_EXT)		: sscal.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sscal.f

sswap.$(OBJ_EXT)		: sswap.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) sswap.f

strmm.$(OBJ_EXT)		: strmm.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) strmm.f

strmv.$(OBJ_EXT)		: strmv.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) strmv.f

strsm.$(OBJ_EXT)		: strsm.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) strsm.f

strti2.$(OBJ_EXT)		: strti2.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) strti2.f

strtri.$(OBJ_EXT)		: strtri.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) strtri.f

xerbla.$(OBJ_EXT)		: xerbla.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) xerbla.f

scabs1.$(OBJ_EXT)		: scabs1.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) scabs1.f
