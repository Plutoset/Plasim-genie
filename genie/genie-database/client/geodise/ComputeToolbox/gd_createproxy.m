function gd_createproxy()
%gd_createproxy Creates a Globus proxy certificate for the user's credentials
%   This command creates a Globus proxy certificate for the user's 
%   credentials at the location specified by the cog.properties file. 
%   The user is queried for the passphrase to their private key by a 
%   pop-up window.
%
%   See also: gd_proxyinfo, dg_proxyquery, gd_certinfo, gd_destroyproxy

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 30/9/02
%   Geodise computational toolbox for Matlab

% Call the static method main in the class CreateProxyGUI

org.geodise.computational.CreateProxyGUI.main(' ')
pause

clear all