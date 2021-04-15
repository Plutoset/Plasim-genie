/**
 * ################################################################
 *
 * ICENI Open Source Code License
 *
 * The contents of this file are subject to the ICENI Open Source Code
 * License Version 1.0 (the License); You may not use this file except in
 * compliance with the License.
 *
 * You may obtain a copy of the License at http://www.lesc.ic.ac.uk/iceni/licence.html
 *
 * Software distributed under the License is distributed on an AS IS
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is from the ICENI project which has in part been
 * funded through the following activities and organisations whose
 * support is gratefully acknowledged:
 * - Engineering and Physical Sciences Research Council
 *   . High-level Component Based Construction of High Performance
 *     Scientific Application (GR/N13371/01)
 *   . London e-Science Centre (THBB/C/008/028)
 *   . Effective Multi-User and Multi-Job Resource Utilisation
 *     (GR/R74505/01) 
 *   . RealityGrid - a tool for investigating condensed matter and
 *     materials (GR/R67600/01) 
 * - Department of Trade and Industry 
 *   . EPIC: E-science Portal at Imperial College (in collaboration with
 *     Sun Microsystems)
 *   . OSCAR-G (in collaboration with Compusys and Intel)
 * - Natural and Environmental Research Council 
 *   . Grid Enabled Integrated Earth Systems Modelling environment
 *     (NER/T/S/2002/00219)
 * - Biotechnology and Biological Sciences Research Council
 *   . Structure-based Proteome Annotation Grid (BEP17048)
 *
 * The Initial Developer of the Original Code is: Imperial College of
 * Science, Technology and Medicine.
 *
 * Portions created by the ICENI team are Copyright (C) 2003 Imperial
 * College Science, Technology and Medicine.
 *
 * All Rights Reserved.
 *
 * Contributor(s):__________________________________
 *
 * ################################################################
 */
package org.icenigrid.genie.jobTypes;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.logging.Logger;
import java.util.Vector;

import org.icenigrid.config.ConfigurationManager;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class CondorJob extends Job {
    // ----- constant object fields -----
    /**
     * logger
     */
    private static final Logger _logger = Logger.getLogger(CondorJob.class.getName());

    // ----- variable object fields -----

    // ----- constructor -----
    /**
     * default constructor
     */
    public CondorJob() {
	super();
    }

    // ----- interface methods -----
    /**
     * return the name of the executable (OS specific)
     */
    public String getExecutableName() {
	String ret = null;
	if (isLinux()) {
	    ret = getExecutablePrefix()+".LINUX";
	} else if (isSunOS()) {
	    ret = getExecutablePrefix()+".SOLARIS28";
	}
	return ret;
    }

    /**
     * create necessary files for ensemble experiment
     */
    public void createExperiment(Vector parameterList) {
	// initialise variables
	String workingDir = getWorkingDir();
	String homeDir = getHomeDir();
	// create working directory tree
	mkDir(workingDir);
	mkDir(workingDir+SEPARATOR+"inputSandbox");
	mkDir(workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"bin");
	mkDir(workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"goin");
	mkDir(workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"input");
	// copy binary executable
	dirCopy(homeDir+SEPARATOR+"build"+SEPARATOR+"bin",
		workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"bin");
	// make binary executable executable
	chmod(workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"bin"+SEPARATOR+getExecutableName(), "a+x");
	// copy input data files
	dirCopy(homeDir+SEPARATOR+"data"+SEPARATOR+"input",
		workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"input");
	// create parameter files
	createParameterFiles(parameterList,
			     workingDir+SEPARATOR+"inputSandbox"+SEPARATOR+"goin");
	// create condor submission script
	createCondorSubmissionScript(workingDir+SEPARATOR+"inputSandbox", parameterList.size());
	// zip up input sandbox
	zip(workingDir+SEPARATOR+getInputSandboxZip(), workingDir+SEPARATOR+"inputSandbox");
	// create executable shell script
	createShellScript();
    }

    // ----- helper methods -----
    /**
     * return general name of executable
     */
    public String getExecutablePrefix() {
	return "goldstein.condor";
    }

    /**
     * create Condor submission script for this Condor job
     */
    public void createCondorSubmissionScript(String directory, int nbJobs) {
	// initialise variables
	String condorScript = directory+SEPARATOR+"condorSubmissionScript";
	// write to file
	try {
	    FileWriter filo = new FileWriter(condorScript);
	    PrintWriter goin = new PrintWriter(filo);
	    goin.println("universe = standard");
	    goin.println("notification = error");
	    goin.println("input = $(Work_Dir)/goin/goin-$(Process)");
	    goin.println("output = $(Work_Dir)/goout/goout-$(Process)");
	    goin.println("error = $(Work_Dir)/error/error-$(Process)");
	    goin.println("log = $(Work_Dir)/log/log-$(Process)");
	    goin.println("initialdir = $(Work_Dir)/bin");
	    goin.println("requirements = (OpSys == \"LINUX\" && Arch == \"INTEL\") || (OpSys == \"SOLARIS28\" && Arch == \"SUN4u\")");
	    goin.println("executable = $(Work_Dir)/bin/"+getExecutablePrefix()+".$$(OpSys)");
	    goin.println("queue "+nbJobs);
	    filo.close();
	} catch (Exception e) {
	    getLogger().severe("ERROR in CondorJob.createCondorSubmissionScript():\n"+e);
	}
    }

    /**
     * create shell script to run this job
     */
    public void createShellScript() {
	// initialise variables
	String shellScript;
	// write to file
	shellScript = getWorkingDir()+SEPARATOR+getShellScript();
	try {
	    FileWriter filo = new FileWriter(shellScript);
	    PrintWriter goin = new PrintWriter(filo);
	    goin.println("#!/bin/sh");
	    goin.println("EXEC_DIR=`pwd`");
	    goin.println("WORK_DIR="+Job.JOB_PREFIX+getJobID()+"-exec");
	    goin.println("if [ `uname -s` == \"SunOS\" ]");
	    goin.println("then");
	    goin.println("CONDORHOME="+getConfigurationManagerValue("org.icenigrid.genie.condor.home.sunos"));
	    goin.println("else");
	    goin.println("CONDORHOME="+getConfigurationManagerValue("org.icenigrid.genie.condor.home.linux"));
	    goin.println("fi");
	    goin.println("mkdir ${WORK_DIR}");
	    goin.println("cd ${WORK_DIR}");
	    goin.println("unzip -qo ${EXEC_DIR}/"+getInputSandboxZip());
	    goin.println("chmod +x ${EXEC_DIR}/${WORK_DIR}/bin/"+getExecutableName());
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/error");
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/goout");
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/log");
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/results");
	    goin.println("${CONDORHOME}/bin/condor_submit -a \"Work_Dir = ${EXEC_DIR}/${WORK_DIR}\" condorSubmissionScript &> submissionOutput");
            goin.println("CLUSTER=`perl -e 'while(<>){split(\" \",$_)};printf $_[scalar(@_)-1].\"\\n\";' submissionOutput`");
	    goin.println("STATE=\"false\"");
	    goin.println("while");
	    goin.println("[ ${STATE} != \"true\" ]");
	    goin.println("do");
	    goin.println("JOBS=`${CONDORHOME}/bin/condor_q -format \"%d \" ProcId ${CLUSTER} | wc -w`");
	    goin.println("if [ ${JOBS} == \"0\" ]");
	    goin.println("then");
	    goin.println("STATE=\"true\"");
	    goin.println("else");
	    goin.println("sleep "+CHECK_INTERVAL);
	    goin.println("fi");
	    goin.println("done");
	    goin.println("cd ${EXEC_DIR}");
	    goin.println("rm -f "+getOutputSandboxZip());
	    goin.println("zip -Dqmr "+getOutputSandboxZip()+" ${WORK_DIR}/error");
	    goin.println("zip -Dqmr "+getOutputSandboxZip()+" ${WORK_DIR}/goout");
	    goin.println("zip -Dqmr "+getOutputSandboxZip()+" ${WORK_DIR}/log");
	    goin.println("zip -Dqmr "+getOutputSandboxZip()+" ${WORK_DIR}/results");
	    goin.println("rm -rf ${EXEC_DIR}/${WORK_DIR}");
	    goin.println("echo \""+getTerminationMessage()+"\"");
	    filo.close();
	    // make submission script executable
	    chmod(shellScript, "a+x");
	} catch (Exception e) {
	    getLogger().severe("ERROR in CondorJob.createShellScript():\n"+e);
	}
    }
}
