The Geodise computational toolbox version 0.5

Graeme Pound 4/6/03
Copyright 2003 Geodise Project, University of Southampton.

The subdirectory /lib contains the jar files required to build and run the Geodise computational toolbox. These include the 1.1alpha release of the Java CoG. This represents an upgrade from the last stable version of the Java CoG v0.9.13, due to greater compatibility with recent Globus releases (2.2.x). 

An Apache Ant build file is include to rebuild the java classes within the gdcompute.jar. To use run the following commands within the directory containing the file build.xml
	>>ant clean
	>>ant

To run the Geodise computational toolbox the Java CoG must be installed, see the example cog.properties file or the installation instructions for the Java CoG. IMPORTANT on unix systems please check that the file permissions of proxy certificates created by the Java CoG are not world readable. 

The gdcompute.jar and the jar files contained in the /lib directory must be referenced in the classpath.txt of your local Matlab intallation (Matlab must be restarted for these changes to take effect). On a multi-user system edit a copy of the original Matlab classpath.txt and start Matlab from the directory containing the classpath.txt (in Windows create a new shortcut to MATLAB and set the "Start in" directory to the directory containing your classpath.txt). 


Bug Log:
28/3/03	- The computational toolbox 0.5 is imcompatiable with versions of Geodise database toolkit before 0.4.2. This is due to incompatiablilty between the versions of the Java CoG 1.1a and 0.9.13 on the Matlab classpath.
31/3/03	- gd_proxyinfo and gd_proxyquery are loading proxy certificates from the default proxy certificate location, rather than the  user defined proxy certificate loaction. This behaviour may also be occuring in other functions. Bug fixed in CVS
2/5/03	- gd_createproxy window freezes (taking all Matlab window with it) if focus is changed to another Matlab window (for example a figure window). Spotted by MM. This behaviour is due to a bug in Matlab R13(http://www.mathworks.com/support/solutions/data/33385.shtml), this has been resolved by changing the behaviour of org.geodise.computational.CreateProxyGUI to modal=false and pause statement.


Change Log:

Version 0.5
	-Based upon Java CoG 1.1a

Version 0.4
	-Based upon Java CoG 0.9.13