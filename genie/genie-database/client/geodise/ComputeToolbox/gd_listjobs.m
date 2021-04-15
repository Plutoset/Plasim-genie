function jobHandles = gd_listjobs(host,port) 
%gd_listjobs Returns all the job handles belonging to the user
%   This command queries an MDS server and returns all of the Globus GRAM
%   job handles belonging to the users credentials. These job handles may
%   be used to terminate or query the status of a Globus GRAM job.
%   
%   jobHandles = gd_listjobs(host,port) 
%                    returns a cell array of Globus GRAM job handles for
%                    the current user's credentials following a query to
%                    the MDS on the machine 'host' on the specified 'port'.
%     
%   Example: 
%       jobHandles = gd_listjobs('escience-dept2',2135) 
%
%   Note that a proxy certificate is required, as the queries are based
%   upon the users credentials. This version of gd_listjobs requires the
%   Java CoG 0.9.13 to be on the Matlab classpath
%   
%   See also: gd_jobsubmit, gd_jobstatus, gd_createproxy

%Note this revision does not use the class MDSClient, instead calling the
%MDS classes directly. Also the regex classes com.stevesoft.pat.* are not
%required

%   Copyright 2003 Geodise Project, University of Southampton
%   Graeme Pound 25/3/03
%   Geodise computational toolbox for Matlab

HostName = host;
PortString = int2str(port);
BaseDN = 'Mds-Vo-name=local, o=Grid';
handlePrefix = 'Mds-Gram-Job-globalId=';
jobNum = 0;
jobHandles ={};

%Get the user's credentials 
proxy = [];
try,
    proxy = org.globus.security.GlobusProxy.getDefaultUserProxy;  
catch,
    error(['Unable to load user credentials:',lasterr])
end
userDN = org.globus.security.CertUtil.getUserDN(proxy);
userID = org.globus.mds.MDSCommon.toGlobusDN(userDN);

%Query the MDS server
mds = org.globus.mds.MDS(HostName, PortString);
try,
    mds.connect; 
    %This sets the base DN to a non-default value
    mds.setBaseDN('o=Grid');
    %search for GRAM jobs displayed on the MDS server
    result = mds.search(BaseDN,'(&(objectclass=MdsGramJob))',2);
    e = result.elements;
    while(e.hasMoreElements)
        outline = char(e.nextElement.toString);
        %Search for jobs owned by the user
        if(findstr(outline,userID))
            jobNum = jobNum +1;
            %Parse the jobHandle and store in a cell array
            prefixidx = findstr(outline,handlePrefix) +length(handlePrefix);
            jobHandles{jobNum} = strtok(outline(prefixidx:length(outline)));
        end
    end
    
catch,
    mds.disconnect
    error(lasterr)
end
mds.disconnect
return 