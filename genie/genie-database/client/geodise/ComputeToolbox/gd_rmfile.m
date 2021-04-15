function gd_rmfile(host,file)
%gd_rmfile Deletes a remote file using GridFtp

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
if ~ischar(file)
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
%remove the file
try,
    ftpc.rmFile(host,file)
catch,
    err = lasterr;
       
    [oney,twoy]=regexp(err, 'No such file or directory');
    if ~isempty(oney)
        error(['No such file or directory: "' file,'"'])     
    elseif ~isempty(regexp(err, 'Permission denied.'))
        error(['Permission denied to delete file or directory: "' directory,'"'])
    else
        error(err,'gridFTPError')
    end

end

