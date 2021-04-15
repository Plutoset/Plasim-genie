
SRCS=compare.cpp
# Windows compilers append .exe regardless <sigh>
# Rather than fight the tide, we'll append this always
EXES=nccompare.exe

ifeq ($(MACHINE),WIN32)
	SRCS += getopt.cpp
endif

OBJS=$(patsubst %.cpp, %.$(OBJ_EXT), $(SRCS))

all: $(EXES)

$(OBJS): %.$(OBJ_EXT) : %.cpp
ifeq ($(MACHINE),WIN32)
	cp $(NETCDF_DIR)\\src\\win32\\NET\\getopt.c getopt.c
	sed 's/strchr/(char *) strchr/g' getopt.c > getopt.cpp
endif
	$(CXX) $(NETCDF_INC) $(CXXFLAGS) -c $< -o $@ -no-multibyte-chars

$(EXES): $(OBJS)
	$(CXX) -o $@ $(OBJS) $(NETCDF) $(LDFLAGS) -no-multibyte-chars

.PHONY: clean
clean:
	rm -rf $(OBJS) $(EXES) 
