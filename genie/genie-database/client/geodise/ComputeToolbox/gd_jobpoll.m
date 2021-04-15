function isDone = gd_jobpoll(jobHandle,interval,maxTime)
%gd_jobpoll Queries the status of a Globus GRAM job until complete
%   This command polls the status of a Globus GRAM job specified by the job
%   handle until the job is complete. If the job fails an error is thrown.
%   
%   gd_jobpoll(jobHandle) 
%                   where jobHandle is the handle to a Globus GRAM job
%                   
%   gd_jobpoll(jobHandle, interval) 
%                   where jobHandle is the handle to a Globus GRAM job and
%                   interval is the interval (in seconds) between polling
%                   the job handle.
%
%   isDone = gd_jobpoll(jobHandle,interval,maxTime)
%                   as above. The argument maxTime allows an upper limit
%                   (in seconds) to be placed on the period over which the
%                   job is polled. The return value isDone indicate whether
%                   the job handle returned the DONE state (1), or whether
%                   polling was aborted.
%
%   Note that the state DONE returned by job handle does not neccessarily
%   indicate that the job completed successfully.  
%
%   See also: gd_jobstatus, gd_jobsubmit, gd_jobkill 

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 11/12/02
%   Geodise computational toolbox for Matlab

error(nargchk(1,3,nargin));

if ~ischar(jobHandle)
    error('jobHandle must be character array')
end

if nargin<2
    %Set the poll frequency in seconds
    pollFreq = 5;
    maxTime = inf;
else
    if isnumeric(interval)
        pollFreq = interval;
    else
        error('interval must be a numeric value');
    end
    if nargin<3 
        maxTime = inf;
    end
end

%Start the stopwatch
tic;
isDone = 0;
while ~isDone
    status = gd_jobstatus(jobHandle);
    if status  == 3        
        isDone =1;
    elseif status == 4
        error(['The job "',jobHandle,'" has failed with state FAILED']) 
    elseif status  == 5 
        disp(['The job "',jobHandle,'" has been SUSPENDED'])         
    elseif status  == 6        
        exitstate =  'UNSUBMITTED';        
        %error(['The job "',jobID,'" has failed with state ',exitstate]) 
    end
    if toc > maxTime
       warning('maxTime exceeded, aborting poll')
       return
    end
    % Pause for x seconds
    pause(pollFreq)
end