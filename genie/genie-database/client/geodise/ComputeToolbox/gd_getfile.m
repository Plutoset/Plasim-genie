function gd_getfile(host,remotefile,localfile,filetype)
%gd_getfile Retrieves a remote file using GridFtp
%   This command retrieves a file from a remote server using GridFtp. The
%   user must specify the remote file location on a remote server and the
%   local destination for the file. The user can also specify the filetype.  
%
%   gd_getfile(host,remotefile,localfile) 
%               transfers the remote ASCII file 'remotefile' from the
%               machine 'host'. The file is saved to the path and file
%               specified by the string 'localfile'.  
%   gd_getfile(...,'ASCII')         
%               as above except the flag sets the file transfer type to
%               ASCII, this is the default setting. 
%   gd_getfile(...,'binary') 
%               sets the file transfer type to binary.
%
%   Examples: 
%       gd_getfile('pablo','data2.gif','C:\data1.gif','binary')
%               copies the file data2.gif from the users home directory on
%               a the remote host pablo to the local file C:\data1.gif on
%               the remote host pablo. The file is transfered as a binary
%               file type. 
%       gd_getfile('pablo','tmp/data2.gif','C:\data1.gif','binary')
%               as above except the file is copied from the subdirectory to
%               the users home directory; 'tmp'. 
%       gd_getfile('pablo','/tmp/data2.gif','C:\data1.gif','binary')
%               as above except the file is copied from the subdirectory to
%               the root directory; 'tmp'.
%
%   Note that a valid proxy certificate is required to use GridFtp.
%   Suitable credentials may be required to transfer files from a remote
%   server. 
%
%   See also: gd_putfile, gd_createproxy

%   Copyright 2002 Geodise Project, University of Southampton
%   Graeme Pound 17/10/02
%   Geodise computational toolbox for Matlab

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required')
end

error(nargchk(3,4,nargin));
if ~ischar(host)
    error('All arguments must be character arrays')
end
if ~ischar(remotefile)
    error('All arguments must be character arrays')
end
if ~ischar(localfile)
    error('All arguments must be character arrays')
end

%Add X509_USER_PROXY environment variable to the java system variables
X509location = getenv('X509_USER_PROXY');

if ~isempty(X509location)
    java.lang.System.setProperty('X509_USER_PROXY',X509location);
end

ftype = '-A';
%check file type switches
if nargin == 4,
    if ~ischar(filetype)
        error('All arguments must be character arrays')
    end     
    if strcmp(filetype,'ASCII')
        ftype = '-A';
    elseif strcmp(filetype,'binary')
        ftype = '-B';
    else
        error(strcat('File type switch: "',filetype,'" is not supported.'))
    end
end

ftpc = org.geodise.computational.FtpClient;
%Set the port number
%ftpc.hostPort = 2811;
%Transfer the file
try,
    ftpc.getFile(host,remotefile,localfile,ftype)
catch,
    err = lasterr;
    [first,last]=regexp(err, 'No such file or directory.');
    if isempty(first)
        error(err,'gridFTPError')
    else
        error(['Server Exception: ' err(first(1):last(1))],'gridFTPdirError')
    end
end

