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
package org.icenigrid.genie.components;

import java.io.File;
import java.util.logging.Logger;
import java.text.NumberFormat;
import java.util.Vector;

import org.icenigrid.component_runtime.component.ContextObject;
import org.icenigrid.genie.jobTypes.*;
import org.icenigrid.genie.misc.ParameterTable;
import org.icenigrid.jdml.BooleanEquation;
import org.icenigrid.jdml.RealEquation;
import org.icenigrid.jdml.SectionEquation;
import org.icenigrid.jdml.StringEquation;
import org.icenigrid.jdml.StringListEquation;
import org.icenigrid.jdml.StringListValueType;
import org.icenigrid.jdml.StringValueType;
import org.icenigrid.runtime.component.PrivateComponentResourceService;
import org.icenigrid.runtime.launcher.LauncherFrameworkService;
import org.icenigrid.services.basic.IceniServiceId;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class EnsembleCreatorComponent_PushDataPushControl_Default
    implements EnsembleCreatorComponent_PushDataPushControl_Interface {

    // ----- constant object fields -----
    /**
     * directory separator string
     */
    public static final String SEPARATOR = System.getProperty("file.separator");

    /**
     * line separator string
     */
    public static final String NEWLINE = System.getProperty("line.separator");

    /**
     * logger
     */
    private final static Logger _logger
	=  Logger.getLogger(EnsembleCreatorComponent_PushDataPushControl_Default.class.getName());

    // ----- variable object fields -----
    /**
     * Grid container context object for this component
     */ 
    private EnsembleCreatorComponent_PushDataPushControl_Context _contextObject;

    /**
     * job type object
     */
    private JobType _jobType;

    /**
     * job range beginning number
     */
    private int _jobBegin;

    /**
     * job range end number
     */
    private int _jobEnd;

    /**
     * buffer of job numbers sent to this component
     */
    private Vector _jobNumberBuffer;

    /**
     * buffer of parameters sent to this component
     */
    private Vector _parameterBuffer;

    /**
     * list of valid parameters
     */
    private Vector _parameterList;

    // ----- get/set methods -----
    /**
     * return the context object for this component
     */
    public EnsembleCreatorComponent_PushDataPushControl_Context getContextObject() {
	return _contextObject;
    }

    /**
     * set the context object for this component
     */
    public void setContextObject(ContextObject contextObject) {
	_contextObject = (EnsembleCreatorComponent_PushDataPushControl_Context) contextObject;
    }

    /**
     * return the job type
     */
    public JobType getJobType() {
	return _jobType;
    }

    /**
     * set the job type
     */
    public void setJobType(JobType jobType) {
	_jobType = jobType;
    }

    /**
     * return the job range beginning number
     */
    public int getJobBegin() {
	return _jobBegin;
    }

    /**
     * set the job range beginning number
     */
    public void setJobBegin(int jobBegin) {
	_jobBegin = jobBegin;
    }

    /**
     * return the job range ending number
     */
    public int getJobEnd() {
	return _jobEnd;
    }

    /**
     * set the job range ending number
     */
    public void setJobEnd(int jobEnd) {
	_jobEnd = jobEnd;
    }

    /**
     * return the buffer of job numbers
     */
    public Vector getJobNumberBuffer() {
	return _jobNumberBuffer;
    }

    /**
     * set the job number buffer
     */
    public void setJobNumberBuffer(Vector jobNumberBuffer) {
	_jobNumberBuffer = jobNumberBuffer;
    }

    /**
     * return the buffer of parameters
     */
    public Vector getParameterBuffer() {
	return _parameterBuffer;
    }

    /**
     * set the parameter buffer
     */
    public void setParameterBuffer(Vector parameterBuffer) {
	_parameterBuffer = parameterBuffer;
    }

    /**
     * return the list of valid parameters
     */
    public Vector getParameterList() {
	return _parameterList;
    }

    /**
     * set the valid parameter list
     */
    public void setParameterList(Vector parameterList) {
	_parameterList = parameterList;
    }

    /**
     * return the logger for this class
     */
    public Logger getLogger() {
	return _logger;
    }

    // ----- ICENI methods -----

    /**
     * initialise method
     */
    public void initialise(ContextObject contextObject) {
	getLogger().info("Initialising EnsembleCreatorComponent");
	// set the context object for this component
        setContextObject(contextObject);
	// initialise variables
	setJobNumberBuffer(new Vector());
	setParameterBuffer(new Vector());
	setParameterList(new Vector());
	getLogger().fine("EnsembleCreatorComponent initialised");
    }

    /**
     * execute method
     */
    public void execute() {
	String jdmlString;
	getLogger().info("Executing EnsembleCreatorComponent");
        // get component values
	getLogger().fine("Getting component values");
	setValues();
	// receive parameter data
	getLogger().fine("Receiving parameter data");
	receiveData();
	// create ensemble experiment
	getLogger().fine("Creating ensemble experiment");
	getJobType().createExperiment(getParameterList());
	// create JDML
	getLogger().fine("Creating JDML");
	jdmlString = createJDMLString();
	// send JDML
	getLogger().fine("Sending JDML");
        getContextObject().JDMLout_Send(jdmlString);
	// end
	getLogger().info("Finished");
	getContextObject().registerFinished();
    }

    /**
     * method to receive job number
     */
    public synchronized void jobNumbersIn_Receive(String jobNumbersInToken) {
        _jobNumberBuffer.add(jobNumbersInToken);
	getLogger().finest("Received job number: "+jobNumbersInToken);
    }

    /**
     * method to receive job parameters
     */
    public synchronized void parametersIn_Receive(String parametersInToken) {
        _parameterBuffer.add(parametersInToken);
	getLogger().finest("Received parameters:\n"+parametersInToken);
    }

    // ----- helper methods ------
    /**
     * return the number of job numbers in the buffer
     */
    public int nbJobNumbers() {
	return _jobNumberBuffer.size();
    }

    /**
     * return the number of job parameters in the buffer
     */
    public int nbJobParameters() {
	return _parameterBuffer.size();
    }

    /**
     * return the number of valid job parameters
     */
    public int nbParameters() {
	return _parameterList.size();
    }
    
    /**
     * set component values from context object
     */
    public void setValues() {
	// set ensemble beginning number
	String begin = (String) getContextObject().getProperty("job.begin");
	if (begin!=null) {
	    setJobBegin(Integer.parseInt(begin));
	} else {
	    setJobBegin(0);
	}
	getLogger().finest("job.begin set to: "+getJobBegin());
	// set ensemble ending number
	String end = (String) getContextObject().getProperty("job.end");
	if (end!=null) {
	    setJobEnd(Integer.parseInt(end));
	} else {
	    setJobEnd(getJobBegin());
	}
	getLogger().finest("job.end set to: "+getJobEnd());
	// set ensemble type
	String type = (String) getContextObject().getProperty("job.type");
	if (type!=null && !type.equals("")) {
	    try {
		Class jobTypeClass = Class.forName(type);
		JobType jobType = (JobType) jobTypeClass.newInstance();
		setJobType(jobType);
	    } catch (Exception e) {
		getLogger().severe("ERROR in EnsembleCreatorComponent_PushDataPushControl_Default.setValues():\n"+e);
		getLogger().severe("Using default job type.");
		setJobType(new BashJob());
	    }
	} else {
	    getLogger().fine("Using default job type.");
	    setJobType(new BashJob());
	}
	getJobType().setContextObject((ContextObject) getContextObject());
	getLogger().finest("job.type set to: "+getJobType().getClass().getName());
    }

    /**
     * wait while data is received from the previous component
     */
    public void receiveData() {
	// initialise variables
	int jobNumber, index;
	int jobCount = getJobEnd()-getJobBegin()+1;
	String parameters;
	// loop until parameter list is filled
	getLogger().fine("Waiting to receive data...");
	while (nbParameters()<jobCount) {
	    // get job parameters from buffer
	    if ((nbJobParameters()>0) && (nbJobParameters()==nbJobNumbers())) {
		jobNumber = Integer.parseInt((String) _jobNumberBuffer.lastElement());
		if ((jobNumber>=getJobBegin()) && (jobNumber<=getJobEnd())) {
		    getLogger().finest("Job number "+jobNumber+" within range");
		    parameters = (String) _parameterBuffer.lastElement();
		    _parameterList.add(parameters);
		    getLogger().finest("Added following to parameter list:\n"+parameters);
		}
		// remove job parameters from buffer
		index = _jobNumberBuffer.lastIndexOf(Integer.toString(jobNumber));
		_jobNumberBuffer.remove(index);
		_parameterBuffer.remove(index);
	    }
	}
    }

    /**
     * create JDML string for ensemble experiment
     */
    public String createJDMLString() {
	// initialise variables
	String workingDir = getJobType().getWorkingDir()+SEPARATOR;
	String inputFile, outputFile;
	StringListValueType stringListValue;
	StringValueType stringValue;
	SectionEquation file, copy, gridFTP;
	// set number format
	NumberFormat nf = NumberFormat.getInstance();
	nf.setMaximumIntegerDigits(4);
	nf.setMinimumIntegerDigits(4);
	nf.setGroupingUsed(false);
	// construct JDML
	// <SectionEquation attribute="JDML">
	SectionEquation root = new SectionEquation("SectionEquation", "JDML");
	// <SectionEquation attribute="ICENI">
	SectionEquation iceni = new SectionEquation("SectionEquation", "ICENI");
	// <StringEquation attribute="LauncherClass">
	StringEquation launcherClass = 
	    new StringEquation("StringEquation", "LauncherClass",
			       "org.icenigrid.basicLaunchers.BashScriptLauncher");
	iceni.addStringEquation(launcherClass);
	root.addSectionEquation(iceni);
	// </SectionEquation> -- ICENI
	// <SectionEquation attribute="Job">
	SectionEquation job = new SectionEquation("SectionEquation", "Job");
	// <StringEquation attribute="Executable">
	StringEquation executable = new StringEquation("StringEquation", "Executable",
						       getJobType().getShellScript());
	job.addStringEquation(executable);
	// <StringEquation attribute="RunCommand">
	StringEquation runCommand = new StringEquation("StringEquation", "RunCommand", "/bin/bash");
	job.addStringEquation(runCommand);
	// <StringEquation attribute="StdInput">
	StringEquation stdin = new StringEquation("StringEquation", "StdInput", "stdin");
	job.addStringEquation(stdin);
	// <StringEquation attribute="StdOutput">
	StringEquation stdout = new StringEquation("StringEquation", "StdOutput", "stdout");
	job.addStringEquation(stdout);
	// <StringEquation attribute="StdError">
	StringEquation stderr = new StringEquation("StringEquation", "StdError", "stderr");
	job.addStringEquation(stderr);
	// <StringListEquation attribute="Arguments">
	// <StringListEquation attribute="InputSandbox">
	StringListEquation inputSandbox = new StringListEquation("StringListEquation",
								 "InputSandbox");
	// <StringListValue>
	stringListValue = new StringListValueType("StringListValue");
	// <StringValue> -- shell script
	stringValue = new StringValueType("StringValue",
					  getJobType().getShellScript());
	stringListValue.addStringValue(stringValue);
	// <StringValue> -- input sandbox zip file
	stringValue = new StringValueType("StringValue",
					  getJobType().getInputSandboxZip());
	stringListValue.addStringValue(stringValue);
	inputSandbox.setStringListValue(stringListValue);
	// </StringListEquation> -- input sandbox
	job.addStringListEquation(inputSandbox);	
	// <StringListEquation attribute="OutputSandbox">
	StringListEquation outputSandbox = new StringListEquation("StringListEquation",
								  "OutputSandbox");
	// <StringListValue>
	stringListValue = new StringListValueType("StringListValue");
	// <StringValue> -- output sandbox zip file
	stringValue = new StringValueType("StringValue",
					  getJobType().getOutputSandboxZip());
	stringListValue.addStringValue(stringValue);
	outputSandbox.setStringListValue(stringListValue);
	// </StringListEquation> -- output sandbox
	job.addStringListEquation(outputSandbox);
	// <SectionEquation attribute="Environment">
	// <RealEquation attribute="RetryCount">
	RealEquation retryCount = new RealEquation("RealEquation", "RetryCount", 3.0);
	job.addRealEquation(retryCount);
	// <BooleanEquation attribute="Requirements">
	BooleanEquation requirements = new BooleanEquation("BooleanEquation", "Requirements", true);
	job.addBooleanEquation(requirements);
	// <RealEquation attribute="Rank">
	RealEquation rank = new RealEquation("RealEquation", "Rank", 100.0);
	job.addRealEquation(rank);
	root.addSectionEquation(job);
	// </SectionEquation> -- Job
	// <SectionEquation attribute="files">
	SectionEquation files = new SectionEquation("SectionEquation", "files");
	// <SectionEquation attribute="..."> -- shell script
	file = new SectionEquation("SectionEquation",
				   getJobType().getShellScript());
	// <SectionEquation attribute="copy">
	copy = copySectionEquation(workingDir);
	file.addSectionEquation(copy);
	// <SectionEquation attribute="gridFTP">
	gridFTP = gridFTPSectionEquation(workingDir);
	file.addSectionEquation(gridFTP);
	files.addSectionEquation(file);
	// <SectionEquation attribute="..."> -- input sandbox zip file
	file = new SectionEquation("SectionEquation",
				   getJobType().getInputSandboxZip());
	copy = copySectionEquation(workingDir);
	file.addSectionEquation(copy);
	// <SectionEquation attribute="gridFTP">
	gridFTP = gridFTPSectionEquation(workingDir);
	file.addSectionEquation(gridFTP);
	files.addSectionEquation(file);
	// <SectionEquation attribute="..."> -- output sandbox zip file
	file = new SectionEquation("SectionEquation",
				   getJobType().getOutputSandboxZip());
	copy = copySectionEquation(workingDir);
	file.addSectionEquation(copy);
	// <SectionEquation attribute="gridFTP">
	gridFTP = gridFTPSectionEquation(workingDir);
	file.addSectionEquation(gridFTP);
	files.addSectionEquation(file);
	// </SectionEquation> -- files
	root.addSectionEquation(files);
	// return JDML string
	return root.streamXMLWithDeclaration();
    }

    /**
     * return JDML object representing 'copy' transport method SectionEquation
     */
    private SectionEquation copySectionEquation(String workingDir) {
	String directory = null;
	Vector nfsExportSpaces = getJobType().getSpacesData("execution.copy.fileSpaces");
	String nfsExportSpace = ((String[]) nfsExportSpaces.get(0))[0];
	String space = ((String[]) nfsExportSpaces.get(0))[1];
	if (workingDir.indexOf(space)>-1) {
	    directory = workingDir.replaceFirst(space, "");
	} else {
	    directory = workingDir;
	}
	// <SectionEquation attribute="copy">
	SectionEquation copy = new SectionEquation("SectionEquation", "copy");
	// <StringEquation attribute="nfsExport">
	StringEquation nfsExport = new StringEquation("StringEquation", "nfsExport", nfsExportSpace);
	copy.addStringEquation(nfsExport);
	// <StringEquation attribute="path">
	StringEquation path = new StringEquation("StringEquation", "path", directory);
	copy.addStringEquation(path);
	return copy;
    }

    /**
     * return JDML object representing 'gridFTP' transport method SectionEquation
     */
    private SectionEquation gridFTPSectionEquation(String workingDir) {
	Vector gFTPSpaces = getJobType().getSpacesData("execution.gridFTP.spaces");
	String gFTPSpace = ((String[]) gFTPSpaces.get(0))[0];
	// <SectionEquation attribute="gridFTP">
	SectionEquation gridFTP = new SectionEquation("SectionEquation", "gridFTP");
	// <StringEquation attribute="gFTPserver">
	StringEquation gFTPserver = new StringEquation("StringEquation", "gFTPserver", gFTPSpace);
	gridFTP.addStringEquation(gFTPserver);
	// <StringEquation attribute="path">
	StringEquation path = new StringEquation("StringEquation", "path", workingDir);
	gridFTP.addStringEquation(path);
	return gridFTP;
    }
}
