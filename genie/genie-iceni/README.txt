genie-iceni
-----------

Author: Murtaza Gulamali
E-mail: murtaza@doc.ic.ac.uk
Last modified: 15 April 2004

OVERVIEW:
This directory contains the source and binary code responsible for building
the ICENI module: genie-iceni

USAGE INSTRUCTIONS:
(1) Copy this entire directory to the root of your ICENI installation
    (i.e. ${ICENI_HOME}).

(2) Edit the configuration file, data/conf/genie-iceni.xml, to reflect your
    system and copy it to ${ICENI_HOME}/iceni-bootstrap/data/runtime

(3) Edit your ${ICENI_HOME}/iceni-bootstrap/data/runtime/runtime.xml file to
    read the genie-iceni configuration file by adding the following line
    somewhere within the <config> tags:

    <!-- configuration file for genie-iceni -->
    <import url="genie-iceni.xml"/>

(4) Build the Java classes and binary executables with the command:

    ant build

(5) Start your genie-iceni software resources on an ICENI server by using the
    appropriate XML documents in data/components:

    startResource.sh data/components/<your_component>.component.xml


AVAILABLE COMPONENTS:
(1) ParameterGeneratorComponent
(2) EnsembleCreatorComponent
(3) TerminatorComponent
