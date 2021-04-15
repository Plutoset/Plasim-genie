function isValid = gd_proxyquery(flag, minValue)
%gd_proxyquery Queries whether a valid proxy certificate exists.
%   This command determines whether a valid proxy certificate exists for
%   user's certificate. The strength or time remaining for the proxy
%   certificate may also be queried. The location of the user's proxy
%   certificate by the cog.properties file.   
%
%   isValid = gd_proxyquery 
%                where isValid is 1 if a valid proxy certificate exists at
%                the default location, otherwise 0.
%   isValid = gd_proxyquery(flag, numValue)
%                where exists is 1 if the proxy certificate meets the
%                requirements of remaining lifetime or cryptographic
%                strength, otherwise 0. If flag = 'time' the time remaining
%                for the proxy certificate is queried against minValue
%                hours. If flag = 'strength' the cryptographic strength of
%                the proxy certificate is queried against minValue bits.
%   
%   Example:
%      exists = gd_proxyquery('strength',1024) returns exists = 0 for a proxy
%      certificate of strength 512.
%
%   See also: gd_proxyinfo, gd_certinfo, gd_createproxy, gd_destroyproxy

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 28/3/02
%   Geodise computational toolbox for Matlab

isvalid = 0;

%Check the number and type of arguments
if nargin > 0
    if nargin > 2
        error('Number of arguments should not be greater than 2: exists = gd_proxyinfo(flag, numValue)')
    end    
    if ~ischar(flag)
        error('The argument flag should be a string: exists = gd_proxyinfo(flag, numValue)')
    end    
    if ~isnumeric(minValue)
       error('The argument minValue should be numerical: exists = gd_proxyinfo(flag, numValue)')
   end
else
    flag = 'null';
end

%Add X509_USER_PROXY environment variable to the java system variables
X509location = getenv('X509_USER_PROXY');

if ~isempty(X509location)
        java.lang.System.setProperty('X509_USER_PROXY',X509location);
end


%Check whether the certificate is valid against input args
try,
    if strcmp(flag,'null')
        info = org.geodise.computational.ProxyInfo.returnAll;
        isValid = 1;
    elseif strcmp(flag,'time')
        isValid = org.geodise.computational.ProxyInfo.isValidMinHours(minValue);
    elseif strcmp(flag,'strength')
        isValid = org.geodise.computational.ProxyInfo.isValidMinStrength(minValue);
    else
        error(['Argument flag "' flag '" is not recognised'])
    end
catch,
    isValid=0;
    err = lasterr;
    [first,last]=regexp(err, 'Expired credentials (.*)\n');
    if isempty(first)
        [first,last]=regexp(err, 'Proxy file (.*) not found');
        if isempty(first)
            warning(err,'certError')
        else
            %Proxy destroyed
            warning(err(first(1):last(1)))
        end            
    else
        %Proxy expired
        warning(strtok(err(first(1):length(err)),char(13)),'certExpired')
    end
    return
end
