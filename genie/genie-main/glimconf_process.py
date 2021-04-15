# Script to process variables in glimmer files
# -f script input file
# -o script output file
# -e expt id
# -r $RUNTIME_ROOT (this is ~/genie by default)
# -s $GLIMMER_OUTDIR (this is icesheet by default)
# -t $RUNTIME_OUTDIR (this is ~/genie_output/$EXPID by default)

expid_vars=["$EXPID","${EXPID}"]
runtime_root_vars=["$RUNTIME_ROOT","${RUNTIME_ROOT}"]
glimmer_outdir_vars=["$GLIMMER_OUTDIR","${GLIMMER_OUTDIR}"]
runtime_outdir_vars=["$RUNTIME_OUTDIR","${RUNTIME_OUTDIR}"]

import getopt, sys

def usage():
    print "glimconf_process [options]"
    print "   -f script input file"
    print "   -o script output file"
    print "   -e expt id"
    print "   -r $RUNTIME_ROOT (this is ~/genie by default)"
    print "   -s $GLIMMER_OUTDIR (this is icesheet by default)"
    print "   -t $RUNTIME_OUTDIR (this is ~/genie_output/$EXPID by default)"


def main():
    # Process command-line args
    try:
        opts, args = getopt.getopt(sys.argv[1:], "f:o:e:r:s:t:")
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)
    infile=""
    outfile=""
    expid=""
    runtime_root=""
    glimmer_outdir=""
    runtime_outdir=""
    for o, a in opts:
        if o == "-f":
            infile = a
        if o == "-o":
            outfile = a
        if o == "-e":
            expid = a
        if o == "-r":
            runtime_root = a
        if o == "-s":
            glimmer_outdir = a
        if o == "-t":
            runtime_outdir = a
    
    #Open files
    f=open(infile,'r')
    g=open(outfile,'w')

    #Process each line in turn
    for line in f.readlines():
        for a in expid_vars:
            line=line.replace(a,expid)
        for a in runtime_root_vars:
            line=line.replace(a,runtime_root)
        for a in glimmer_outdir_vars:
            line=line.replace(a,glimmer_outdir)
        for a in runtime_outdir_vars:
            line=line.replace(a,runtime_outdir)
        g.write(line)

    f.close()
    g.close()

if __name__ == "__main__":
    main()
