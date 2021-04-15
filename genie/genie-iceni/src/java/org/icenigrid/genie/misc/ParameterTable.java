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
package org.icenigrid.genie.misc;

import java.util.Vector;
import java.util.logging.Logger;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class ParameterTable {
    // ----- constant object fields -----
    /**
     * number of characters to use in 'restart' parameter
     */
    public static final int CHARS_RESTART = 1;

    /**
     * number of characters to use in 'lout' parameter
     */
    public static final int CHARS_LOUT = 4;

    /**
     * number of characters to use in 'lin' parameter
     */
    public static final int CHARS_LIN = 6;

    /**
     * logger
     */
    private static final Logger _logger =
	Logger.getLogger(ParameterTable.class.getName());

    // ----- variable object fields -----
    /**
     * parameter table
     */ 
    private Table _parameters;

    /**
     * output files table
     */
    private Table _outputFiles;

    // ----- constructor(s) -----
    public ParameterTable() {
	getLogger().finer("Creating ParameterTable object");
	_parameters = new Table();
	setDefaultParameters();
	_outputFiles = new Table();
	setDefaultOutputFiles();
    }

    // ----- parameter table get/set methods -----
    /**
     * return name of parameter at specified index in parameter table
     */
    public String getParameterName(int index) {
	return (String) _parameters.get(index);
    }

    /**
     * return value of parameter at specified index in parameter table
     */
    public String getParameterValue(int index) {
	return (String) _parameters.getValue(index);
    }

    /**
     * set value of parameter at specified index in parameter table
     */
    public void setParameterValue(int index, String value) {
	String name = getParameterName(index);
	if (name!=null) {
	    setParameterValue(name,value);
	} else {
	    getLogger().warning("Parameter does not exist at index "+index);
	}
    }

    /**
     * return value of parameter with specified name in parameter table
     */
    public String getParameterValue(String name) {
	return (String) _parameters.get(name);
    }

    /**
     * set value of parameter with specified name in parameter table
     */
    public void setParameterValue(String name, String value) {
	getLogger().finest("Setting: "+name+" = "+value);
	if (name.equals("genie.restart")) {
	    _parameters.put(name,value.substring(0,CHARS_RESTART));
	} else if (name.equals("genie.lout")) {
	    _parameters.put(name,value.substring(0,CHARS_LOUT));
	} else if (name.equals("genie.lin")) {
	    _parameters.put(name,value.substring(0,CHARS_LIN));
	} else {
	    _parameters.put(name,value);
	}
    }

    /**
     * return parameter table
     */
    public Table getParameters() {
	return _parameters;
    }

    /**
     * set parameter table
     */
    public void setParameters(Table parameters) {
	_parameters = parameters;
    }

    // ----- output files table get/set methods -----
    /**
     * return name of output file at specified index in output files table
     */
    public String getOutputFileName(int index) {
	return (String) _outputFiles.get(index);
    }

    /**
     * return value of output file at specified index in output files table
     */
    public boolean getOutputFileValue(int index) {
	String name = getOutputFileName(index);
	return getOutputFileValue(name);
    }

    /**
     * set value of output file at specified index in output files table
     */
    public void setOutputFileValue(int index, boolean value) {
	String name = getOutputFileName(index);
	if (name!=null) {
	    setOutputFileValue(name,value);
	} else {
	    getLogger().warning("OutputFile does not exist at index "+index);
	}
    }

    /**
     * return value of output file with specified name in output files table
     */
    public boolean getOutputFileValue(String name) {
	boolean ret = false;
	String value = (String) _outputFiles.get(name);
	if ((value!=null) && (value.toLowerCase().equals("true"))) {
	    ret = true;
	}
	return ret;
    }

    /**
     * set value of output file with specified name in output files table
     */
    public void setOutputFileValue(String name, boolean value) {
	String svalue;
	getLogger().finest("Setting: "+name+" = "+value);
	if (value) {
	    svalue = "true";
	} else {
	    svalue = "false";
	}
	_outputFiles.put(name, svalue);
    }

    /**
     * return output files table
     */
    public Table getOutputFiles() {
	return _outputFiles;
    }

    /**
     * set output files table
     */
    public void setOutputFiles(Table outputFiles) {
	_outputFiles = outputFiles;
    }

    /**
     * return the logger for this class
     */
    public Logger getLogger() {
	return _logger;
    }

    // ----- helper methods -----

    /**
     * create the default parameter table
     */
    public void setDefaultParameters() {
	setParameterValue("genie.param.x","genie.dfwx");
	setParameterValue("genie.param.x.begin","-0.3");
	setParameterValue("genie.param.x.increment","0.6");
	setParameterValue("genie.param.x.increment.type","linear");
	setParameterValue("genie.param.x.end","0.3");
	setParameterValue("genie.param.y","genie.dfwy");
	setParameterValue("genie.param.y.begin","-0.3");
	setParameterValue("genie.param.y.increment","0.6");
	setParameterValue("genie.param.y.increment.type","linear");
	setParameterValue("genie.param.y.end","0.3");
	setParameterValue("genie.nsteps","1001");
	setParameterValue("genie.npstp","1000");
	setParameterValue("genie.iwstp","1000");
	setParameterValue("genie.itstp","10");
	setParameterValue("genie.restart","n");
	setParameterValue("genie.tv","3.65");
	setParameterValue("genie.ndta","5");
	setParameterValue("genie.temp0","5.0");
	setParameterValue("genie.temp1","5.0");
	setParameterValue("genie.rel","0.9");
	setParameterValue("genie.scf","2.0");
	setParameterValue("genie.diff1","2000.0");
	setParameterValue("genie.diff2","1e-5");
	setParameterValue("genie.adrag","2.5");
	setParameterValue("genie.diffamp1","8111000");
	setParameterValue("genie.diffamp2","79610");
	setParameterValue("genie.betaz1","0.1111");
	setParameterValue("genie.betam1","0.0");
	setParameterValue("genie.betaz2","0.2626");
	setParameterValue("genie.betam2","0.2626");
	setParameterValue("genie.scl_co2","1.0");
	setParameterValue("genie.pc_co2_rise","0.0");
	setParameterValue("genie.diffsic","2000.0");
	setParameterValue("genie.tatm","0.0");
	setParameterValue("genie.relh0_ocean","0.0");
	setParameterValue("genie.relh0_land","0.0");
	setParameterValue("genie.dfwx","0.0");
	setParameterValue("genie.dfwy","0.0");
	setParameterValue("genie.lout","0000");
	setParameterValue("genie.lin","0000.1");
    }

    /**
     * create the default outputFiles table
     */
    public void setDefaultOutputFiles() {
        setOutputFileValue("t",false);
        setOutputFileValue("s",false);
        setOutputFileValue("opsit",false);
        setOutputFileValue("airt",false);
        setOutputFileValue("q",false);
        setOutputFileValue("cost",false);
        setOutputFileValue("psi",false);
        setOutputFileValue("opsi",false);
        setOutputFileValue("opsip",false);
        setOutputFileValue("opsia",false);
        setOutputFileValue("zpsi",false);
        setOutputFileValue("fofy",false);
        setOutputFileValue("rho",false);
        setOutputFileValue("err",false);
        setOutputFileValue("sst",false);
        setOutputFileValue("sss",false);
        setOutputFileValue("tair",false);
        setOutputFileValue("qair",false);
        setOutputFileValue("hice",false);
        setOutputFileValue("aice",false);
        setOutputFileValue("tice",false);
        setOutputFileValue("pptn",false);
        setOutputFileValue("evap",false);
        setOutputFileValue("runoff",false);
        setOutputFileValue("arcice",false);
        setOutputFileValue("fwfxneto",false);
        setOutputFileValue("fx0neto",false);
        setOutputFileValue("fxfa",false);
        setOutputFileValue("temp",false);
        setOutputFileValue("saln",false);
        setOutputFileValue("albedo",false);
    }

    /**
     * return number of parameters in the parameter table
     */
    public int nbParameters() {
	return getParameters().size();
    }

    /**
     * return number of output files in the output files table
     */
    public int nbOutputFiles() {
	return getOutputFiles().size();
    }

    /**
     * return output files value
     */
    public int getOutputFilesValue() {
	int value = 0;
	for (int i=0; i<nbOutputFiles(); i++) {
	    if (getOutputFileValue(i)) {
		value += (int) Math.pow(2,i);
	    }
	}
	return value;
    }

    /**
     * return list of output files denoted by specified output files value
     */
    public static Vector getOutputFilesList(int value) {
	// initialise variables
	int p, q;
	Vector ret = new Vector();
	ParameterTable parameters = new ParameterTable();
	// determine if output files have been flagged
	q = value;
	for (int i=(parameters.nbOutputFiles()-1); i>=0; i--) {
	    p = (int) Math.pow(2,i);
	    if (q>=p) {
		q -= p;
		parameters.setOutputFileValue(i,true);
	    }
	}
	// add valid output file names to returned list
	for (int i=0; i<parameters.nbOutputFiles(); i++) {
	    if (parameters.getOutputFileValue(i)) {
		ret.add(parameters.getOutputFileName(i));
	    }
	}
	return ret;
    }

    /**
     * is the restart parameter set?
     */
    public boolean isRestart() {
	boolean ret = false;
	if (getParameterValue("genie.restart").toLowerCase().startsWith("c")) {
	    ret = true;
	}
	return ret;
    }
}
