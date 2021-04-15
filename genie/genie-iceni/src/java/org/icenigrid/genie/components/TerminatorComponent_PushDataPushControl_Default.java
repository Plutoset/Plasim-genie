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

import java.util.logging.Logger;

import org.icenigrid.component_runtime.component.ContextObject;
import org.icenigrid.runtime.component.ep.ExecutionPlan;
import org.icenigrid.runtime.component.ep.Link;
import org.icenigrid.runtime.component.ep.Id;
import org.icenigrid.runtime.component.PrivateApplicationResourceService;
import org.icenigrid.runtime.component.PrivateComponentResourceService;
import org.icenigrid.xml.parser.TextElement;

/**
 * @author   ICENI Team
 * @version  $Name$
 */
public class TerminatorComponent_PushDataPushControl_Default
    implements TerminatorComponent_PushDataPushControl_Interface {

    // ----- constant object fields -----
    /**
     * termination string
     */
    public static final String TERMINATION_MESSAGE = "TERMINATE";

    /**
     * number of seconds for thread to wait between termination checks
     */
    public static final int WAITING_TIME = 60;

    /**
     * logger
     */
    private final static Logger _logger
	=  Logger.getLogger(TerminatorComponent_PushDataPushControl_Default.class.getName());

    // ----- variable object fields -----
    /**
     * Grid container context object for this component
     */ 
    private TerminatorComponent_PushDataPushControl_Context _contextObject;

    /**
     * number of connections made to this component 
     */
    private int _connections;

    /**
     * number of termination messages received by this component
     */
    private int _terminationMessages;

    // ----- get/set methods -----
    /**
     * return the context object for this component
     */
    public TerminatorComponent_PushDataPushControl_Context getContextObject() {
	return _contextObject;
    }

    /**
     * set the context object for this component
     */
    public void setContextObject(ContextObject contextObject) {
	_contextObject = (TerminatorComponent_PushDataPushControl_Context) contextObject;
    }

    /**
     * return the number of connections made to this component
     */
    public int getNbConnections() {
	return _connections;
    }

    /**
     * set the number of connections made to this component
     */
    public void setNbConnections(int connections) {
	_connections = connections;
    }

    /**
     * return the number of termination messages recieved by this component
     */
    public int getNbTerminationMessages() {
	return _terminationMessages;
    }

    /**
     * set the number of termination messages recieved by this component
     */
    public void setNbTerminationMessages(int terminationMessages) {
	_terminationMessages = terminationMessages;
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
	getLogger().info("Initialising TerminatorComponent");
	// set the context object for this component
        setContextObject(contextObject);
	// initialise variables
	setNbTerminationMessages(0);
	// determine number of connections made to this component
	countConnections();
    }

    /**
     * execute method
     */
    public void execute() {
	getLogger().info("Executing TerminatorComponent");
        // loop until number of termination messages equals number of connections
	while (getNbTerminationMessages()<getNbConnections()) {
	    getLogger().finest("Waiting for "+WAITING_TIME+" seconds...");
	    try {
		Thread.sleep(WAITING_TIME*1000);
	    } catch (Exception e) {
		getLogger().severe("ERROR in TerminatorComponent_PushDataPushControl_Default.execute():\n"+e);
	    }
	}
	// kill application
	getLogger().info("Terminating the application");
	getContextObject().applicationTerminate();
    }

    /**
     * method to receive messages from other components
     */
    public synchronized void messagesIn_Receive(String messagesInToken) {
        // check if received message is a termination message
	getLogger().finest("Received message: "+messagesInToken);
	if (messagesInToken.equals(TERMINATION_MESSAGE)) {
	    getLogger().finest("Message matches termination message!!!");
	    // increment number of termination messages received
	    int terminationMessages = getNbTerminationMessages();
	    terminationMessages++;
	    setNbTerminationMessages(terminationMessages);
	}
	getLogger().finest("Termination messages received: "+getNbTerminationMessages());
    }

    // ----- helper methods -----
    /**
     * determine number of components connected to this component
     */
    public void countConnections() {
	// initialise variables
	int connections = 0;
	String toInstanceID, myID;
	try {
	    // retrieve component ID for this component
	    myID = getContextObject().getCompService().getUniqueId();
	    getLogger().finest("My component ID is: "+myID);
	    // retrieve execution plan for this application
	    ExecutionPlan ep = getContextObject().getAppService().getExecutionPlan();
	    // cycle through links in application execution plan
	    for (int i=0; i<ep.nbLinks(); i++) {
		toInstanceID = ep.getLink(i).getToInstance().getValue().toString();
		getLogger().finest("Found link to component "+toInstanceID);
		if (myID.equals(toInstanceID)) {
		    connections++;
		    getLogger().finest("Number of connections to this component: "+connections);
		}
	    }
	} catch (Exception e) {
	    getLogger().severe("ERROR in TerminatorComponent_PushDataPushControl_Default.countConnections():\n"+e);
	}
	setNbConnections(connections);
    }
}
