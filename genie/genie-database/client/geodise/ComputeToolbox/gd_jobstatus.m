function status = gd_jobstatus(jobHandle)
%gd_jobstatus Gets the status of a Globus GRAM job
%   status = gd_jobstatus(jobHandle) returns the status of a Globus GRAM
%   job, where status =
%                   -1 is Unknown
%                    1 is PENDING
%                    2 is ACTIVE
%                    3 is DONE
%                    4 is FAILED
%                    5 is SUSPENDED
%                    6 is UNSUBMITTED
%     
%   Example: status = gd_jobstatus(...
%       'https://escience-dept2.sesnet.soton.ac.uk:33342/10288/1029768382/')
%
%   See also: gd_createproxy, gd_jobsubmit, gd_jobkill

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 16/10/02
%   Geodise computational toolbox for Matlab

error(nargchk(1,1,nargin));

if ~ischar(jobHandle)
    error('jobHandle must be character array')
end

statusStr = org.geodise.computational.GramJobClient.jobStatus(java.lang.String(jobHandle));

if strcmp(statusStr,'PENDING')
    status = 1;
end
if strcmp(statusStr,'ACTIVE')
    status = 2;
end    
if strcmp(statusStr,'DONE')
    status = 3;
end
if strcmp(statusStr,'FAILED')
    status = 4;
end
if strcmp(statusStr,'SUSPENDED')
    status = 5;
end
if strcmp(statusStr,'UNSUBMITTED')
    status = 6;
end
if strcmp(statusStr,'Unknown')    
    status = -1;
end