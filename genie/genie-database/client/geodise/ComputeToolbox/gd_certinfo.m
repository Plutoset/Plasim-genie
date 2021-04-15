function subject = gd_certinfo(file)
%gd_certinfo Returns information about the user's certificate
%   This command prints information about the user's certificate to the
%   screen. The command also returns the certificate subject line in a
%   format which is suitable for use in a Globus gridmap file. The default
%   location of the user's certificate is specified by the cog.properties
%   file.  
%   
%   subject = gd_certinfo
%                   where subject is the certificate subject in Globus
%                   format
%   subject = gd_certinfo(file)
%                   as above, where file is the filename of the certificate
%                   to be queried. The certificate must be encoded in pem
%                   format.
%
%   See also: gd_proxyinfo, gd_createproxy, gd_destroyproxy

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 31/3/03
%   Geodise computational toolbox for Matlab

error(nargchk(0,1,nargin))
if (nargin ==1)
    if ~ischar(file)
        error('Certificate filename must be specified with a string')
    end 
    usedefault = 0;
else 
    usedefault = 1;
end

% Create a certificate inforamtion java object
cinfo = org.geodise.computational.CertInfo;

% Print all certifcate information to the screen
if usedefault
    cinfo.printAll
else
    cinfo.printAll(file)
end

% Get the certificate sunbject line 
if usedefault
    subject = cinfo.returnSubject;
else
    subject = cinfo.returnSubject(file);
end

return 
