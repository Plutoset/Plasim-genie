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
import org.icenigrid.component_runtime.component.GridComponent;
import org.icenigrid.runtime.component.IPDO;
import org.icenigrid.runtime.component.OPDO;
import org.icenigrid.runtime.component.ReceiverMarker;
import org.icenigrid.component_runtime.component.Receiver;

/**
 * Automatically Generated context object.
 *
 * @author   ICENI Team
 * @version  $Name$
 */
public class ParameterGeneratorComponent_PushDataPushControl_Context extends ContextObject implements ParameterGeneratorComponent_PushDataPushControl_Advice {

    // The component for which this is a context
    private ParameterGeneratorComponent_PushDataPushControl_Interface _gridComponent;

    public void initialise(GridComponent gc) {
        _gridComponent = (ParameterGeneratorComponent_PushDataPushControl_Interface) gc;
    } // end initialise

    public OPDO jobNumbersOut_Send_OPDO;

    public void setjobNumbersOut_Send_OPDO(OPDO o){
        jobNumbersOut_Send_OPDO = o;
    }

    public void jobNumbersOut_Send(java.lang.String jobNumbersOutToken) {
         if (jobNumbersOut_Send_OPDO != null )jobNumbersOut_Send_OPDO.controlFlowUp(jobNumbersOutToken);
    }

    public OPDO parametersOut_Send_OPDO;

    public void setparametersOut_Send_OPDO(OPDO o){
        parametersOut_Send_OPDO = o;
    }

    public void parametersOut_Send(java.lang.String parametersOutToken) {
         if (parametersOut_Send_OPDO != null )parametersOut_Send_OPDO.controlFlowUp(parametersOutToken);
    }

    public OPDO metadataOut_Send_OPDO;

    public void setmetadataOut_Send_OPDO(OPDO o){
        metadataOut_Send_OPDO = o;
    }

    public void metadataOut_Send(java.lang.String metadataOutToken) {
         if (metadataOut_Send_OPDO != null )metadataOut_Send_OPDO.controlFlowUp(metadataOutToken);
    }


}
