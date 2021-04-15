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

import org.icenigrid.component_runtime.component.ContextObject;

import java.lang.Math;
import java.text.NumberFormat;
import java.util.logging.Logger;
import java.util.Vector;

import org.icenigrid.component_runtime.component.ContextObject;
import org.icenigrid.genie.misc.ParameterTable;
import org.icenigrid.runtime.component.PrivateComponentResourceService;
import org.icenigrid.xml.parser.TextElement;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class ParameterGeneratorComponent_PushDataPushControl_Default
    implements ParameterGeneratorComponent_PushDataPushControl_Interface {

    // ----- constant object fields -----
    /**
     * logger
     */
    private final static Logger _logger =
	Logger.getLogger(ParameterGeneratorComponent_PushDataPushControl_Default.class.getName());

    // ----- variable object fields -----
    /**
     * Grid container context object for this component
     */ 
    private ParameterGeneratorComponent_PushDataPushControl_Context _contextObject;

    /**
     * parameter table to store parameter values
     */
    private ParameterTable _parameterTable;

    // ----- get/set methods -----
    /**
     * return the context object for this component
     */
    public ParameterGeneratorComponent_PushDataPushControl_Context getContextObject() {
	return _contextObject;
    }

    /**
     * set the context object for this component
     */
    public void setContextObject(ContextObject contextObject) {
	_contextObject = (ParameterGeneratorComponent_PushDataPushControl_Context) contextObject;
    }

    /**
     * return the parameter table for this component
     */
    public ParameterTable getParameterTable() {
	return _parameterTable;
    }

    /**
     * set the parameter table for this component
     */
    public void setParameterTable(ParameterTable parameterTable) {
	_parameterTable = parameterTable;
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
	getLogger().info("Initialising ParameterGeneratorComponent");
	// set the context object
        setContextObject(contextObject);
        // initialise variables
	setParameterTable(new ParameterTable());
	getLogger().fine("ParameterGeneratorComponent initialised");
    }

    /**
     * execute method
     */
    public void execute() {
	getLogger().info("Executing ParameterGeneratorComponent");
        // get parameter values
	getLogger().fine("Getting parameter values");
	setValues();
	// process and send data
	getLogger().fine("Processing and sending data");
	sendData();
	// end
	getLogger().fine("Finished");
	getContextObject().registerFinished();
    }

    // ----- helper methods -----

    /**
     * set value of specified parameter from the context object property
     */
    private void setParameterValue(String parameterName) {
	String value = (String) getContextObject().getProperty(parameterName);
	if (value!=null) {
	    _parameterTable.setParameterValue(parameterName, value);
	    getLogger().finest(parameterName+" set to: "+value);
	}
    }
    
    /**
     * set value of specified output file from the context object property
     */
    private void setOutputFileValue(String outputFileName) {
	String value = (String) getContextObject().getProperty("genie.output."+outputFileName);
	if (value!=null) {
	    _parameterTable.setOutputFileValue(outputFileName, true);
	    getLogger().finest(outputFileName+" set to: true");
	} else {
	    _parameterTable.setOutputFileValue(outputFileName, false);
	    getLogger().finest(outputFileName+" set to: false");
	}
    }

    /**
     * set property values from the context object
     */
    private void setValues() {
	String name;
	// set parameter values
	for (int i=0; i<getParameterTable().nbParameters(); i++) {
	    name = getParameterTable().getParameterName(i);
	    setParameterValue(name);
	}
	// set output file values
	for (int i=0; i<getParameterTable().nbOutputFiles(); i++) {
	    name = getParameterTable().getOutputFileName(i);
	    setOutputFileValue(name);
	}
    }

    /**
     * process and send data thorough ports
     */
    private void sendData() {
	// initialise variables
	int nx, ny, jobNumber = 0;
	double x, y;
	double[] begin = new double[2];
	double[] inc   = new double[2];
	double[] end   = new double[2];
	String parametersString, metadataString;
	// specify parameter ranges
	begin[0] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.x.begin"));
	begin[1] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.y.begin"));
	inc[0] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.x.increment"));
	inc[1] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.y.increment")); 
	end[0] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.x.end"));
	end[1] = Double.parseDouble(getParameterTable().getParameterValue("genie.param.y.end"));
	// calculate integer parameter counts
	if (getParameterTable().getParameterValue("genie.param.x.increment.type").equals("log")) {
	    nx = (int) Math.round((log10(end[0])-log10(begin[0]))/log10(inc[0]));
	} else {
	    nx = (int) Math.round((end[0]-begin[0])/inc[0]);
	}
	if (getParameterTable().getParameterValue("genie.param.y.increment.type").equals("log")) {
	    ny = (int) Math.round((log10(end[1])-log10(begin[1]))/log10(inc[1]));
	} else {
	    ny = (int) Math.round((end[1]-begin[1])/inc[1]);
	}
	// loop through parameter space values
	for (int i=0; i<=nx; i++) {
	    for (int j=0; j<=ny; j++) {
		// calculate parameter space values
		if (getParameterTable().getParameterValue("genie.param.x.increment.type").equals("log")) {
		    x = Math.pow(10,log10(begin[0])+i*log10(inc[0]));
		} else {
		    x = begin[0]+i*inc[0];
		}
		if (getParameterTable().getParameterValue("genie.param.y.increment.type").equals("log")) {
		    y = Math.pow(10,log10(begin[1])+j*log10(inc[1]));
		} else {
		    y = begin[1]+j*inc[1];
		}
		// check if jobNumbersOut port is connected
		if (portIsConnected("jobNumbersOut")) {
		    // send out the job number
		    getLogger().finest("Sending job number: "+Integer.toString(jobNumber));
		    getContextObject().jobNumbersOut_Send(Integer.toString(jobNumber));
		}
		// check if parametersOut port is connected
		if (portIsConnected("parametersOut")) {
		    // create the parameters string
		    parametersString = generateParametersString(x,y,jobNumber);
		    // send out the parameters string
		    getLogger().finest("Sending parameter string:\n"+parametersString);
		    getContextObject().parametersOut_Send(parametersString);
		}
		// check if metadataOut port is connected
		if (portIsConnected("metadataOut")) {
		    // create the metadata string
		    //metadataString = generateMetadataString(x,y,jobNumber);
		    // send out the metadata files
		    //getLogger().finest("Sending metadata string:\n"+metadataString);
		    //getContextObject().metadataOut_Send(metadataString);
		}
		// increment job number
		jobNumber++;
	    }
	}
    }

    /**
     * create single parameters string
     */
    private String generateParametersString(double x, double y, int jobNumber) {
	String input = "";
	// set number format
        NumberFormat expNumberFormat = NumberFormat.getInstance();
        expNumberFormat.setMinimumIntegerDigits(getParameterTable().CHARS_LOUT);
	expNumberFormat.setMaximumIntegerDigits(getParameterTable().CHARS_LOUT);
        expNumberFormat.setGroupingUsed(false);
	// construct string
	input += addParameter("genie.nsteps", x, y)+" "+
	    addParameter("genie.npstp", x, y)+" "+
	    addParameter("genie.iwstp", x, y)+" "+
	    addParameter("genie.itstp", x, y)+"\n";
	input += getParameterTable().getParameterValue("genie.restart")+"\n";
        input += addParameter("genie.tv", x, y)+" "+
	    addParameter("genie.ndta", x, y)+"\n";
        input += addParameter("genie.temp0", x, y)+" "+
	    addParameter("genie.temp1", x, y)+" "+
	    addParameter("genie.rel", x, y)+" "+
	    addParameter("genie.scf", x, y)+"\n";
        input += addParameter("genie.diff1", x, y)+" "+
	    addParameter("genie.diff2", x, y)+"\n";
        input += addParameter("genie.adrag", x, y)+"\n";
        input += addParameter("genie.diffamp1", x, y)+" "+
	    addParameter("genie.diffamp2", x, y)+"\n";
        input += addParameter("genie.betaz1", x, y)+" "+
	    addParameter("genie.betam1", x, y)+" "+
	    addParameter("genie.betaz2", x, y)+" "+
	    addParameter("genie.betam2", x, y)+"\n";
	input += addParameter("genie.scl_co2", x, y)+" "+
	    addParameter("genie.pc_co2_rise", x, y)+"\n";
	input += addParameter("genie.diffsic", x, y)+"\n";
	input += addParameter("genie.tatm", x, y)+" "+
	    addParameter("genie.relh0_ocean", x, y)+" "+
	    addParameter("genie.relh0_land", x, y)+"\n";
	input += addParameter("genie.dfwx", x, y)+" "+
	    addParameter("genie.dfwy", x, y)+"\n";
	input += expNumberFormat.format(jobNumber)+"\n";
	input += Integer.toString(getParameterTable().getOutputFilesValue())+"\n";
	input += getParameterTable().getParameterValue("genie.lin")+"\n";
	return input;
    }

    /**
     * return value of specified parameter from parameter table or range
     */
    private String addParameter(String name, double x, double y) {
	String ret, param1, param2;
	// set up number formats
	NumberFormat dFormat = NumberFormat.getInstance();
	dFormat.setMinimumIntegerDigits(1);
        dFormat.setMaximumIntegerDigits(8);
        dFormat.setMinimumFractionDigits(1);
        dFormat.setMaximumFractionDigits(8);
	dFormat.setGroupingUsed(false);
	// get parameter names
	param1 = getParameterTable().getParameterValue("genie.param.x");
	param2 = getParameterTable().getParameterValue("genie.param.y");
	if (name.equals(param1)) {
	    ret = dFormat.format(x);
	} else if (name.equals(param2)) {
	    ret = dFormat.format(y);
	} else {
	    ret = getParameterTable().getParameterValue(name);
	}
	return ret;
    }

    /**
     * is the specified port connected to anything?
     */
    private boolean portIsConnected(String portName) {
	String status;
	try {
	    PrivateComponentResourceService compService = getContextObject().getCompService();
	    status = compService.getPortStatus(portName, "connected");
	    return Boolean.valueOf(status.toLowerCase()).booleanValue();
	} catch(Exception e) {
	    return false;
	}
    }

    /**
     * calculate logarithm of specified value to base 10
     */
    public double log10(double x) {
	return (Math.log(x)/Math.log(10));
    }
}
