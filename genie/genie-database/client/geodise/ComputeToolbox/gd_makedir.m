function gd_makedir(host,directory)
%gd_makedir Creates a remote directory using GridFtp

%   Copyright 2003 Geodise Project, University of Southampton
%   Graeme Pound 31/3/2003
%   Geodise computational toolbox for Matlab

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required')
end

error(nargchk(2,2,nargin));
if ~ischar(host)
    error('All arguments must be character arrays')
end
if ~ischar(directory)
    error('All arguments must be character arrays')
end

%Add X509_USER_PROXY environment variable to the java system variables
X509location = getenv('X509_USER_PROXY');

if ~isempty(X509location)
        java.lang.System.setProperty('X509_USER_PROXY',X509location);
end

ftpc = org.geodise.computational.FtpClient;
%Set the port number
%ftpc.hostPort = 2811;
%create the directory
try,
    ftpc.makeDir(host,directory)
catch,
    err = lasterr;
        
    [oney,twoy]=regexp(err, 'directory exists');
    if ~isempty(oney)
        error(['Remote directory: ',directory,' already exists'])
    elseif ~isempty(regexp(err, 'No such file or directory'))
        error(['No such directory: "' directory,'"'])
    elseif ~isempty(regexp(err, 'Permission denied.'))
        error(['Permission denied to create directory: "' directory,'"'])
    else
        error(err,'gridFTPError')
    end
end

