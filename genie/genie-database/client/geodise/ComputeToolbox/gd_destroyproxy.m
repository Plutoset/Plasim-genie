function success =gd_destroyproxy()
%gd_destroyproxy Destroys the local copy of the user's Globus proxy certificate 
%   This command deletes the local copy of the Globus proxy certificate for 
%   the user's credentials at the location specified by the cog.properties file. 
%
%   See also: gd_createproxy, gd_proxyinfo, gd_certinfo

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 14/10/02
%   Geodise computational toolbox for Matlab

try, 
    org.geodise.computational.ProxyInfo.destroyProxy
catch, 
    warning(lasterr)
    success = 0;
    return
end

success =1;
return