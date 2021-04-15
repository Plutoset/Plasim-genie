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
public class BashJob extends Job {
    // ----- constant object fields -----
    /**
     * logger
     */
    private static final Logger _logger = Logger.getLogger(BashJob.class.getName());

    // ----- variable object fields -----

    // ----- constructor -----
    /**
     * default constructor
     */
    public BashJob() {
	super();
    }

    // ----- interface methods -----

    /**
     * return the name of the executable (OS specific)
     */
    public String getExecutableName() {
	String ret = null;
	if (isLinux()) {
	    ret = "goldstein.LINUX";
	} else if (isSunOS()) {
	    ret = "goldstein.SOLARIS28";
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
	// zip up input sandbox
	zip(workingDir+SEPARATOR+getInputSandboxZip(), workingDir+SEPARATOR+"inputSandbox");
	// create executable shell script
	createShellScript(parameterList.size());
    }

    // ----- helper methods -----
    /**
     * create shell script to run this job
     */
    public void createShellScript(int nbJobs) {
	// initialise variables
	String shellScript;
	// configure number format
	NumberFormat nf = NumberFormat.getInstance();
	nf.setMinimumIntegerDigits(1);
	nf.setMaximumIntegerDigits(4);
	nf.setGroupingUsed(false);
	// write to file
	shellScript = getWorkingDir()+SEPARATOR+getShellScript();
	try {
	    FileWriter filo = new FileWriter(shellScript);
	    PrintWriter goin = new PrintWriter(filo);
	    goin.println("#!/bin/sh");
	    goin.println("EXEC_DIR=`pwd`");
	    goin.println("WORK_DIR="+Job.JOB_PREFIX+getJobID()+"-exec");
	    goin.println("mkdir ${WORK_DIR}");
	    goin.println("cd ${WORK_DIR}");
	    goin.println("unzip -qo ${EXEC_DIR}/"+getInputSandboxZip());
	    goin.println("chmod +x ${EXEC_DIR}/${WORK_DIR}/bin/"+getExecutableName());
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/goout");
	    goin.println("mkdir ${EXEC_DIR}/${WORK_DIR}/results");
	    goin.println("cd ${EXEC_DIR}/${WORK_DIR}/bin");
	    for (int i=0; i<nbJobs; i++) {
		goin.println("");
		goin.println("${EXEC_DIR}/${WORK_DIR}/bin/"+getExecutableName()+" < "+
			     "${EXEC_DIR}/${WORK_DIR}/goin/goin-"+nf.format(i)+" > "+
			     "${EXEC_DIR}/${WORK_DIR}/goout/goout-"+nf.format(i));
	    }
	    goin.println("cd ${EXEC_DIR}");
	    goin.println("rm "+getOutputSandboxZip());
	    goin.println("zip -Dqmr "+getOutputSandboxZip()+" ${WORK_DIR}/goout ${WORK_DIR}/results");
	    goin.println("rm -rf ${EXEC_DIR}/${WORK_DIR}");
	    goin.println("echo \""+getTerminationMessage()+"\"");
	    filo.close();
	    // make submission script executable
	    chmod(shellScript, "a+x");
	} catch (Exception e) {
	    getLogger().severe("ERROR in BashJob.createShellScript():\n"+e);
	}
    }
}
