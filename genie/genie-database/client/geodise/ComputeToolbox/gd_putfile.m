function gd_putfile(host,localfile,remotefile,filetype)
%gd_putfile Puts a remote file using GridFtp
%   This command puts a local file upon a remote server using GridFtp. The
%   user must specify the remote server name, the local file path, and the
%   remote file path. The user can also specify the filetype.
%
%   gd_putfile(host,localfile,remotefile) 
%               transfers the ASCII file 'localfile' to the  machine 'host'. The file
%				 is saved to the path and file specified by the string 'remotefile'.  
%   gd_putfile(...,'ASCII')         
%               as above except the flag sets the file transfer type to
%               ASCII, this is the default setting. 
%   gd_putfile(...,'binary') 
%               sets the file transfer type to binary.
%
%   Examples: 
%       gd_putfile('pablo','C:\data1.gif','data2.gif','binary')
%               places the local file C:\data1.gif on the remote host pablo
%               in the users home directory with the file name data6.gif.
%               The file is transfered as a binary file type.
%       gd_putfile('pablo','C:\data1.gif','tmp/data2.gif','binary')
%               as above except the file is placed in the existing
%               subdirectory to the users home directory; 'tmp'.
%       gd_putfile('pablo','C:\data1.gif','/tmp/data2.gif','binary')
%               as above except the file is placed in the subdirectory to
%               the root directory; 'tmp'.  
%
%   Note that a valid proxy certificate is required to use GridFtp.
%   Suitable credentials may be required to transfer files to remote
%   servers. This FTPclient does not currently create directories on remote
%   machines that do not exist, however support for this feature may be
%   incorporated in future.
%
%   See also: gd_getfile, gd_createproxy

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
if ~ischar(localfile)
    error('All arguments must be character arrays')
end
if ~ischar(remotefile)
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

%transfer the file
try,
    ftpc.putFile(host,localfile,remotefile,ftype)
catch,
    err = lasterr;
    [first,last]=regexp(err, 'No such file or directory.');
    if isempty(first)
        error(err,'gridFTPError')
    else
        error(['Server Exception: ' err(first(1):last(1))],'gridFTPdirError')
    end
end

