function ans = genie_retrieve(fileHandles, localDirectory, prompt)
%
% genie_retrieve(fileHandles, localDirectory [, prompt] )
%
% This script is a wrapper for the gd_retrieve script. Instead of
% a single unique file handle, this script accepts as input a cell
% array of structures which result from the gd_query command. The
% script processes the file information and retrieves all of the
% files from the appropriate Globus file servers (as specified in
% the database).
%
% GENIE Project - 24th November 2003
% Andrew Price, Southampton e-Science Centre

% Check that the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required. Use gd_createproxy to create one.')
end

% Verify that a cell array has been provided.
if ~iscell(fileHandles)
    error('genie_retrieve requires a cell array of file handles.');
end

% Check that a valid directory has been provided
if ~exist(localDirectory,'dir')
    error('genie_retrieve requires a local directory to be specified to receive the desired files.');
end

% Find the number of files to be retrieved.
files=size(fileHandles,2);

% Report the total size of the transfer if this information is available.
totalbytes=0;
if isfield(fileHandles{1}.file, 'byteSize')
    for i=1:files
        totalbytes = totalbytes + fileHandles{i}.file.byteSize;
    end
end

% Ask the user to confirm the transfer.
disp(' ');
disp([' Total number of bytes in this file set: ',num2str(totalbytes)]);
reply=input([' About to transfer ',num2str(files),' files to the local filesystem. Do you wish to continue? y/n [y]: '],'s');
if isempty(reply)
    reply = 'y';
end

% If user does not want to overwrite file
if strcmpi (reply, 'y')
    progresshandle = waitbar(0,'Transferring files...','Name','GENIE GridFTP File Transfer Progress');
    % Process the results structure and retrieve each file.
    for i=1:files
        localFile=[localDirectory,filesep,fileHandles{i}.file.ID,'.gz'];
        if exist('prompt','var')
            if strcmp(prompt, 'overwrite') && exist(localFile)
                gd_retrieve(fileHandles{i}.file.ID,localFile,prompt);
            else
                gd_retrieve(fileHandles{i}.file.ID,localFile);
            end
        else
            gd_retrieve(fileHandles{i}.file.ID,localFile);
        end
        eval(['! gunzip ',localFile]);
        %pause(1)
        waitbar(i/files,progresshandle)
    end
    close(progresshandle);
else
    ans=0;
    return
end

ans=1;
return