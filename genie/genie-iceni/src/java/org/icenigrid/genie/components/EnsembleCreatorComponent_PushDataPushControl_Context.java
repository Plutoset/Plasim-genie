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
public class EnsembleCreatorComponent_PushDataPushControl_Context extends ContextObject implements EnsembleCreatorComponent_PushDataPushControl_Advice {

    // The component for which this is a context
    private EnsembleCreatorComponent_PushDataPushControl_Interface _gridComponent;

    public void initialise(GridComponent gc) {
        _gridComponent = (EnsembleCreatorComponent_PushDataPushControl_Interface) gc;
    } // end initialise

    // set an IPDO, give it a receiver
    public IPDO jobNumbersIn_Receive_IPDO;

    public void setjobNumbersIn_Receive_IPDO(IPDO i) throws org.icenigrid.services.basic.IceniServiceException {
        jobNumbersIn_Receive_IPDO = i;

        Receiver R = new Receiver() {
            public Object controlFlowDown(Object data) {
                ((EnsembleCreatorComponent_PushDataPushControl_Interface) this.getGridComponent() ).jobNumbersIn_Receive((java.lang.String) data);
                return null;
            }
            };

        R.setGridComponent(this._gridComponent);
        jobNumbersIn_Receive_IPDO.setReceiver(R);
    }

    // set an IPDO, give it a receiver
    public IPDO parametersIn_Receive_IPDO;

    public void setparametersIn_Receive_IPDO(IPDO i) throws org.icenigrid.services.basic.IceniServiceException {
        parametersIn_Receive_IPDO = i;

        Receiver R = new Receiver() {
            public Object controlFlowDown(Object data) {
                ((EnsembleCreatorComponent_PushDataPushControl_Interface) this.getGridComponent() ).parametersIn_Receive((java.lang.String) data);
                return null;
            }
            };

        R.setGridComponent(this._gridComponent);
        parametersIn_Receive_IPDO.setReceiver(R);
    }

    public OPDO JDMLout_Send_OPDO;

    public void setJDMLout_Send_OPDO(OPDO o){
        JDMLout_Send_OPDO = o;
    }

    public void JDMLout_Send(java.lang.String JDMLoutToken) {
         if (JDMLout_Send_OPDO != null )JDMLout_Send_OPDO.controlFlowUp(JDMLoutToken);
    }


}
