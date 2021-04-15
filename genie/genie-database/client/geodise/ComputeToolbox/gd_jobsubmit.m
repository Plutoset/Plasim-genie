function jobHandle = gd_jobsubmit(RSL,HOST)
%gd_jobsubmit Submits a compute job to a Globus GRAM job manager
%   This command submits the compute job described by the a Resource
%   Specification Language (RSL) string to a Globus server running a GRAM 
%   job manager. Upon a successful submission the command returns a
%   job handle that may be used to query the status of, or terminate, the
%   job.
%
%   jobHandle = gd_jobsubmit(RSL,HOST)
%                where RSL is a string describing the submitted job, HOST
%                is the name of the Globus server, and jobHandle is the
%                handle for a successfully submitted job.
%   
%   Example:
%       jobHandle = gd_jobsubmit('&(executable =/bin/date)','myhost.mydomain.com')
%
%   Note that a valid proxy certificate is required to submit a GRAM job. 
%   For more information about RSL see http://www.globus.org/gram/.
%
%   See also: gd_createproxy, gd_jobkill, gd_jobstatus

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 16/10/02
%   Geodise computational toolbox for Matlab

error(nargchk(2,2,nargin));

if ~ischar(RSL)
    error('All arguments must be character arrays')
end
if ~ischar(HOST)
    error('All arguments must be character arrays')
end

s1 = java.lang.String(RSL);
s2 = java.lang.String(HOST);

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required')
end

%Submit the job
jobHandle = char(org.geodise.computational.GramJobClient.jobSubmit(s1,s2));
