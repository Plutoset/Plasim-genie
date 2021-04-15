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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.logging.Logger;
import java.util.Random;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.icenigrid.component_runtime.component.ContextObject;
import org.icenigrid.config.ConfigurationManager;
import org.icenigrid.config.ValueList;
import org.icenigrid.genie.components.TerminatorComponent_PushDataPushControl_Default;
import org.icenigrid.runtime.launcher.LauncherFrameworkService;
import org.icenigrid.xml.parser.Element;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class Job implements JobType {
    // ----- constant object fields -----
    /**
     * number of bytes to read at a time during the file copy processes
     */
    private static final int CHUNK = 8192;

    /**
     * number of seconds to wait before periodically checking the status of jobs
     */
    public static final long CHECK_INTERVAL = 60;

    /**
     * directory separator string
     */
    public static final String SEPARATOR = System.getProperty("file.separator");

    /**
     * prefix string for job directory and filenames
     */
    public static final String JOB_PREFIX = "genie-iceni-";

    /**
     * logger
     */
    private static final Logger _logger = Logger.getLogger(Job.class.getName());

    // ----- variable object fields -----
    /**
     * name of shell script
     */
    private String _shellScript;

    /**
     * name of input sandbox zip file
     */
    private String _inputSandboxZip;

    /**
     * name of output sandbox zip file
     */
    private String _outputSandboxZip;

    /**
     * unique ID for this job
     */
    private String _jobID;

    /**
     * working directory
     */
    private String _workingDir;

    /**
     * context object associated with component referencing this job
     */
    private ContextObject _contextObject;

    /**
     * random number generator
     */
    private final Random _random = new Random();

    // ----- constructor -----
    /**
     * default constructor
     */
    public Job() {
	setJobID();
	setShellScript(JOB_PREFIX+getJobID()+".sh");
	setInputSandboxZip(JOB_PREFIX+getJobID()+"-inputSandbox.zip");
	setOutputSandboxZip(JOB_PREFIX+getJobID()+"-outputSandbox.zip");
    }

    // ----- get/set methods -----
    // INTERFACE METHOD
    /**
     * return ID of this job
     */
    public String getJobID() {
	return _jobID;
    }

    /**
     * set the ID of this job
     */
    private void setJobID() {
	_jobID = getRandomString();
    }

    // INTERFACE METHOD
    /**
     * return name of shell script associated with this job type
     */
    public String getShellScript() {
	return _shellScript;
    }

    /**
     * set name of shell script associated with this job type
     */
    public void setShellScript(String shellScript) {
	_shellScript = shellScript;
    }

    // INTERFACE METHOD
    /**
     * return name of input sandbox zip file associated with this job type
     */
    public String getInputSandboxZip() {
	return _inputSandboxZip;
    }

    /**
     * set name of input sandbox zip file associated with this job type
     */
    public void setInputSandboxZip(String inputSandboxZip) {
	_inputSandboxZip = inputSandboxZip;
    }

    // INTERFACE METHOD
    /**
     * return name of output sandbox zip file associated with this job type
     */
    public String getOutputSandboxZip() {
	return _outputSandboxZip;
    }

    /**
     * set name of output sandbox zip file associated with this job type
     */
    public void setOutputSandboxZip(String outputSandboxZip) {
	_outputSandboxZip = outputSandboxZip;
    }

    // INTERFACE METHOD
    /**
     * return path of working directory
     */
    public String getWorkingDir() {
	String workingDir, experimentDir;
	if (_workingDir==null || _workingDir.equals("")) {
	    experimentDir = getExperimentDir();
	    if (experimentDir==null || experimentDir.equals("")) {
		experimentDir = getTmpDir();
	    }
	    // create working directory name
	    workingDir = experimentDir+SEPARATOR+JOB_PREFIX+getJobID();
	    // set working directory
	    setWorkingDir(workingDir);
	    getLogger().finer("Working directory assigned to: "+workingDir);
	}
	return _workingDir;
    }

    /**
     * set working directory to be specified directory
     */
    public void setWorkingDir(String directory) {
	mkDir(directory);
	_workingDir = directory;
    }

    // INTERFACE METHOD
    /**
     * get context object associated to the component using this job
     */
    public ContextObject getContextObject() {
	return _contextObject;
    }

    // INTERFACE METHOD
    /**
     * set context object associated to the component using this job
     */
    public void setContextObject(ContextObject contextObject) {
	_contextObject = contextObject;
    }

    /**
     * return logger for this object
     */
    public Logger getLogger() {
	return _logger;
    } 

    /**
     * return random number generator
     */
    public Random getRandomNumberGenerator() {
        return _random;
    }

    // ----- interface methods -----
    /**
     * return the location of this module
     */
    public String getHomeDir() {
	return getConfigurationManagerValue("org.icenigrid.genie.home");
    }

    /**
     * return the name of the executable (JobType and OS specific)
     */
    public String getExecutableName() {
	// method MUST be overloaded in JobType subclass
	return "";
    }

    /**
     * create necessary files for ensemble experiment (JobType specific)
     */
    public void createExperiment(Vector parameterList) {
	// method MUST be overloaded in JobType subclass
    }

    /**
     * return list of spaces of specified type (i.e. copy spaces, gFTP spaces, HTTP spaces) that the launcher can access
     *
     * @param name The name of the configuration property that contains the data
     * @return A vector of String[2]. Where each element is [name of item, location in file space].
     */
    public Vector getSpacesData(String name) {
	Vector ret = new Vector();
	try {
	    ValueList list = ConfigurationManager.getInstance().getProperty(name);
	    for (int i=0; i<list.size(); i++) {
		String[] item = new String[2];
		item[0] = ((org.jdom.Element) list.get(i)).getAttributeValue("name");
		item[1] = ((org.jdom.Element) list.get(i)).getAttributeValue("location");
		ret.add(item);
	    }
	} catch (Exception e) {
	    getLogger().severe("ERROR in Job.getSpacesData("+name+"):\n"+e);
	}
	return ret;
    }

    // ----- helper methods -----
    /**
     * return value of specified property from the ICENI ConfigurationManager
     */
    public String getConfigurationManagerValue(String property) {
	String ret;
	try {
	    ret = ConfigurationManager.getInstance().getPropertyAsString(property);
	    if (ret==null || ret.equals("")) {
		throw new Exception(property+" does not appear to be set in the Configuration Manager.");
	    }
	    return ret;
	} catch (Exception e) {
	    getLogger().severe("ERROR in Job.getConfigurationManagerValue("+property+"):\n"+e);
	    return null;
	}
    }

    /**
     * return path of temporary directory
     */
    public String getTmpDir() {
	return getConfigurationManagerValue("iceni.temp");
    }

    /**
     * return the location of ICENI
     */
    public String getIceniHome() {
	return getConfigurationManagerValue("iceni.home");
    }

    /**
     * return path of GENIE experiment directory
     */
    public String getExperimentDir() {
	return getConfigurationManagerValue("org.icenigrid.genie.experiments.dir");
    }

    /**
     * return random 16-digit string
     */
    public String getRandomString() {
        int number;
        String ret = "";
	// set number format
	NumberFormat nf = NumberFormat.getInstance();
        nf.setMinimumIntegerDigits(4);
	nf.setMaximumIntegerDigits(4);
        nf.setGroupingUsed(false);
        for (int i=0; i<4; i++) {
            // generate random number
            number = getRandomNumberGenerator().nextInt(10000);
            // add to ID string
            ret += nf.format(number);
        }
        return ret;
    }

    /**
     * make a new directory with the specified name
     */
    public void mkDir(String directory) {
	File dir = new File(directory);
	if (dir.exists()) {
	    // delete old directory
	    getLogger().finest("Deleting old directory: "+directory);
	    rmDir(directory);
	}
	// create new directory
	getLogger().finest("Creating directory: "+directory);
	dir.mkdirs();
    }

    /**
     * remove specified file from filesystem
     */
    public void rmFile(String filename) {
	File file = new File(filename);
	if (file.exists() && file.isFile()) {
	    // delete file
	    getLogger().finest("Deleting file: "+filename);
	    file.delete();
	}
    }

    /**
     * remove specified directory from filesystem. (recursive)
     */
    public void rmDir(String directory) {
	File dir = new File(directory);
	if (dir.exists() && dir.isDirectory()) {
	    File[] dirlist = dir.listFiles();
	    for (int i=0; i<dirlist.length; i++) {
		if (dirlist[i].isDirectory()) {
		    // remove sub directory
		    rmDir(dirlist[i].getPath());
		    dirlist[i].delete();
		} else {
		    // remove file
		    rmFile(dirlist[i].getPath());
		}
	    }
	    dir.delete();
	}
    }

    /**
     * copy contents of source directory to destination directory
     */ 
    public void dirCopy(String source, String destination) {
	File dir = new File(source);
	if (dir.isDirectory()) {
	    getLogger().finest("Copying directory "+source+" to "+destination);
	    File[] fileList = dir.listFiles();
            for (int i=0; i<fileList.length; i++) {
                if (fileList[i].isDirectory()) {
                    dirCopy(source+SEPARATOR+fileList[i].getName(),
			    destination+SEPARATOR+fileList[i].getName());
		} else {
                    fileCopy(source+SEPARATOR+fileList[i].getName(),
			     destination+SEPARATOR+fileList[i].getName());
                }
            }
	}
    }

    /**
     * copy source file to destination file
     */
    public void fileCopy(String source, String destination) {
	File file = new File(source);
	if (file.exists() && file.isFile()) {
	    try {
		getLogger().finest("Copying file "+source+" to "+destination);
		FileInputStream fis = new FileInputStream(source);
		FileOutputStream fos = new FileOutputStream(destination);
		byte[] data = new byte[CHUNK];
		int count;
		while ((count = fis.read(data, 0, CHUNK))>-1) {
		    fos.write(data, 0, count);
		}
		fis.close();
		fos.close();
	    } catch (Exception e) {
		getLogger().severe("ERROR in Job.fileCopy("+source+","+destination+"):\n"+e);
	    }
	}
    }

    /**
     * set file permissions of specified file (*nix systems only)
     */
    public void chmod(String file, String permissions) {
	Runtime system = Runtime.getRuntime();
	String[] command = new String[]{"/bin/sh","-c","chmod",permissions,file};
	try {
	    if (isLinux() || isSunOS()) {
		Process process = system.exec(command);
		process.waitFor();
	    } else {
		throw new Exception("Operating system does not support the setting of file permissions.");
	    }
	} catch (Exception e) {
	    getLogger().severe("ERROR in Job.chmod("+file+","+permissions+"):\n"+e);
	}
    }

    /**
     * create specified ZIP file of specified directory. (recursive)
     */
    public void zip(String zipfile, String directory) {
        try {
            File dir = new File(directory);
            FileOutputStream fos = new FileOutputStream(zipfile);
            ZipOutputStream zos = new ZipOutputStream(fos);
            if (dir.exists() && dir.isDirectory()) {
		getLogger().finest("Zipping up directory "+directory+" to file "+zipfile);
		zipDir(zos, directory, directory);
            }
            zos.flush();
            zos.close();
            fos.close();
        } catch (Exception e) {
	    getLogger().severe("ERROR in Job.zip("+zipfile+","+directory+"):\n"+e);
        }
    }

    /**
     * ZIP up specified directory into specified zipfile using specified stream. (recursive)
     */
    private void zipDir(ZipOutputStream zos, String directory, String baseDir) {
        File dir = new File(directory);
        if (dir.isDirectory()) {
            File[] fileList = dir.listFiles();
            for (int i=0; i<fileList.length; i++) {
                if (fileList[i].isDirectory()) {
		    // add directory to zipfile
		    getLogger().finest("Adding directory: "+fileList[i].getPath());
                    zipDir(zos, fileList[i].getPath(), baseDir);
                } else {
		    // add file to zip file
                    zipFile(zos, fileList[i].getPath(), baseDir);
                }
            }
        }
    }


    /**
     * ZIP up specified file into specified zipfile using specified stream
     */
    private void zipFile(ZipOutputStream zos, String filename, String baseDir) {
        File file = new File(filename);
        if (file.exists() && file.isFile()) {
            try {
		// add file to zip file using relative filename
		getLogger().finest("Adding file: "+filename);
                FileInputStream fis = new FileInputStream(filename);
                BufferedInputStream bis = new BufferedInputStream(fis);
                ZipEntry fileEntry = new ZipEntry(relativeFilename(filename, baseDir));
                zos.putNextEntry(fileEntry);
                byte[] data = new byte[CHUNK];
                int count;
                while ((count = bis.read(data, 0, CHUNK))>-1) {
                    zos.write(data,0,count);
                }
            } catch (Exception e) {
                getLogger().severe("ERROR in Job.zipFile("+filename+","+baseDir+"):\n"+e);
            }
        }
    }

    /**
     * return filename relative to working directory
     */
    public String relativeFilename(String absoluteFilename, String baseDir) {
	String filename;
	int index = (baseDir.concat(SEPARATOR)).length();
	filename = absoluteFilename.substring(index);
	return filename;
    }

    /**
     * create parameter files in specified directory using specified parameter list
     */
    public void createParameterFiles(Vector parameterList, String directory) {
	// initialise variables
	String parameters;
	// set number format
	NumberFormat nf = NumberFormat.getInstance();
	nf.setMaximumIntegerDigits(4);
	nf.setMinimumIntegerDigits(1);
	nf.setGroupingUsed(false);
	// create parameter file for each parameter string in the list
	for (int i=0; i<parameterList.size(); i++) {
	    parameters = (String) parameterList.get(i);
	    getLogger().finest("Creating parameter file: goin-"+nf.format(i));
	    createParameterFile(parameters, directory+SEPARATOR+"goin-"+nf.format(i));
	}
    }

    /**
     * create a parameter file with specified filename and parameter string
     */
    public void createParameterFile(String parameters, String filename) {
	FileWriter filo;
	try {
	    filo = new FileWriter(filename);
	    PrintWriter goin = new PrintWriter(filo);
	    goin.println(parameters);
	    filo.close();
	} catch (Exception e) {
	    getLogger().severe("ERROR in Job.createParameterFile("+parameters+","+filename+"):\n"+e);
	}
    }

    /**
     * is the JVM running under Linux?
     */
    public boolean isLinux() {
	boolean ret = false;
	if (System.getProperty("os.name").equals("Linux")) {
	    ret = true;
	}
	return ret;
    }

    /**
     * is the JVM running under SunOS?
     */
    public boolean isSunOS() {
	boolean ret = false;
	if (System.getProperty("os.name").equals("SunOS")) {
	    ret = true;
	}
	return ret;
    }

    /**
     * return termination message
     */
    public String getTerminationMessage() {
	return TerminatorComponent_PushDataPushControl_Default.TERMINATION_MESSAGE;
    }
}
