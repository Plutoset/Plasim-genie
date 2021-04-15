function gd_jobkill(jobHandle)
%gd_JobKill Kills a Globus GRAM specified by job handle
%   gd_JobKill(jobHandle) Kills the Globus job specified by the Globus job 
%   handle.
%
%   Example: gd_jobkill(...
%       'https://escience-dept2.sesnet.soton.ac.uk:33342/10288/1029768382/')
%
%   Note that a valid proxy certificate for the correct user credentials is
%   required to kill a GRAM job.
%
%   See also: gd_createproxy, gd_jobsubmit, gd_jobstatus

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 16/10/02
%   Geodise computational toolbox for Matlab

error(nargchk(1,1,nargin));

if ~ischar(jobHandle)
    error('jobHandle must be character array')
end

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required')
end

org.geodise.computational.GramJobClient.jobKill(java.lang.String(jobHandle));


