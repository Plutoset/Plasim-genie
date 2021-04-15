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

/**
 * class to represent key-value pairs, holding the information
 * in an ordered and synchronised table unlike java.utils.Hashtable
 * or org.apache.commons.collections.SequencedHashMap
 */
public class Table {
    // ------ object fields ------
    /**
     * keys
     */
    private Vector _keys;
    
    /**
     * values
     */
    private Vector _values;

    // ------ constructor ------
    /**
     * default constructor
     */
    public Table() {
	_keys = new Vector();
	_values = new Vector();
    }
    
    // ------ get/set methods ------
    /**
     * return list of keys
     */
    public Vector getKeys() {
	return _keys;
    }

    /**
     * return list of values
     */
    public Vector getValues() {
	return _values;
    }
    
    // ------ helper methods ------
    /**
     * return key at specified index of the table
     */
    public Object get(int index) {
	return _keys.get(index);
    }

    /**
     * return value referenced by specified key
     */
    public Object get(Object key) {
	int index = _keys.indexOf(key);
	return _values.get(index);
    }

    /**
     * return value at specified index of the table
     */
    public Object getValue(int index) {
	return _values.get(index);
    }

    /**
     * insert specified value with specified key into the table
     */
    public synchronized void put(Object key, Object value) {
	int index = _keys.indexOf(key);
	if (index>-1) {
	    // key already exists
	    _values.setElementAt(value, index);
	} else {
	    // key does not exist
	    _keys.add(key);
	    _values.add(value);
	}
    }

    /**
     * return the number of key-value pairs in the table
     */
    public int size() {
	return _keys.size();
    }   
}
