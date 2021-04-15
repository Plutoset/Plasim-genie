function [exists,subject] = gd_proxyinfo()
%gd_proxyinfo Returns information about the user's proxy certificate
%   This command checks the existence of the user's proxy certificate 
%   and prints information to the screen. The command also returns the 
%   subject line of the proxy certificate. 
%
%   exists = gd_proxyinfo 
%                where exists is 1 if the proxy certificate exists at the
%                default location, otherwise 0.
%   [exists,subject] = gd_proxyinfo 
%                where subject is the subject line of the proxy
%                certificate.
%
%   See also: gd_proxyquery, gd_certinfo, gd_createproxy, gd_destroyproxy

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 28/3/02
%   Geodise computational toolbox for Matlab

exists = 0;

%Add X509_USER_PROXY environment variable to the java system variables
X509location = getenv('X509_USER_PROXY');

if ~isempty(X509location)
        java.lang.System.setProperty('X509_USER_PROXY',X509location);
end

% Create a certificate info java object
pinfo = org.geodise.computational.ProxyInfo;

try,
    % Print all certifcate information to the screen
    disp(pinfo.returnAll)
    subject = pinfo.returnSubject;
catch,
    err = lasterr;
    [first,last]=regexp(err, 'Expired credentials (.*)\n');
    if isempty(first)
        [first,last]=regexp(err, 'Proxy file (.*) not found');
        if isempty(first)
            warning(err)
        else
            %Proxy destroyed
            warning(err(first(1):last(1)))
        end   
    else
        %Proxy expired
        warning(strtok(err(first(1):length(err)),char(13)))
    end
    return
end

%So far so good
exists = 1;

