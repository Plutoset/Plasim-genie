function localdata = gd_retrieve (handle, localpath, prompt)

%   gd_retrieve retrieves a file or struct from the Geodise repository to
%   the local machine. usage: gd_retrieve (handle, localpath, prompt)
%
%   location = gd_retrieve (id, localpath) will retrieve a file from the
%   repository based on its unique handle (id). The second argument,
%   localpath specifies where the retrieved file should be saved. This can
%   either be the new file's full path name or an existing directory, in 
%   which case the filename will be the file's original name (this is 
%   determined by the localName property of the file's metadata - see 
%   gd_archive and gd_query). The function returns a string specifying the 
%   retrieved file's location. 
%
%   newstruct = gd_retrieve (id) will return a struct that has been
%   retrieved from the repository based on its unique handle (id).
% 
%   If a file already exists with the same name you will be prompted on
%   whether you want to overwrite it. To turn off this prompt and always
%   overwrite set the last argument (prompt) to 'overwrite'.
%
%   You can only retrieve data that you archived or that someone else has
%   given you permission to access.
%
%   Examples:
%   (1) Retrieve a file and save it with a specific file name.
%           location = gd_retrieve ('file_dat_ce868f40-8de0-445e-8ae5-36c05eec25a9', 'C:\myfile.dat')
%           filePath =
%           C:\myfile.dat
%
%   (2) Retrieve a file to a directory and use its original name.
%           location = gd_retrieve ('file_dat_ce868f40-8de0-445e-8ae5-36c05eec25a9', 'C:\files')
%           filePath =
%           C:\files\file.dat
%
%   (3) Retrieve a struct
%           x = gd_retrieve ('var_3dc0cf72-f16f-4b0a-a6s0-bed33fds812f')
%           x = 
%               a: 45.0376
%               b: [1 2.6 4.3 5.4]
%   Notes:  
%    (1) A valid proxy certificate is required to retrieve a file (see
%        gd_createproxy from the Geodise computational toolbox).
%    (2) You must have access to the host machine the files will be
%        retrieved from. Your certificate subject must be added to the
%        gridmap file on the host and to the authorisation database.
%
%   See also: gd_archive, gd_datagroup, gd_datagroupadd, gd_query, gd_createproxy
%
% Copyright 2002-2003 Geodise Project, University of Southampton
% Jasmin Wason (j.l.wason@soton.ac.uk), last modified on 19/05/03
% Geodise database toolbox for Matlab version 0.5

% ----------------
% Init - check arguments

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required. Use gd_createproxy to create one.')
end

error (nargchk (1, 3, nargin))

% check that handle is a string
if ~ischar (handle)
    error('handle must be a character array')
end

% if localpath exists it must be a string
if exist('localpath')
    if ~ischar (localpath)
        error('localpath must be a character array')
    else
        isfile = true;
    end
% if localpath does not exist, assume we are retrieving a struct
else
    isfile = false;
end

% if prompt == 'overwrite' then don't prompt before overwriting a file.
if exist('prompt')
    if strcmp (prompt, 'overwrite')
        overwrite = true;
    else
        error('Prompt argument can only have one value, ''overwrite''.')
    end
%  By default always prompt before overwriting a file.
else
    overwrite = false;
end

% ----------------
% Main code to retrieve file using GdRetrieve class
try    
    retrieveClient = org.geodise.database.GdRetrieve;
    
    % If we are retrieving a file 
    if isfile
        % find the remote location of the file uing GdRetrieve
        serverLocation = retrieveClient.locateFile (handle);
        % if handle not found display a warning and return an empty string
        if isempty (serverLocation(1)) || isempty (serverLocation(2))
            warning(['Could not locate file with ID ' handle])
            localdata = '';
        % if handle found retrieve the file using GdRetrieve   
        else
            host = serverLocation(1);
            remotedir = serverLocation(2);
            % If localPath is a directory get original name (localName)
            % from metadata.
            if exist (localpath) == 7
                localName = getlocalname (handle);
                % Construct full file path
                fs = char(java.lang.System.getProperty ('file.separator'));
                % Only add file separator if directory path doesn't end
                % with one
                if regexp(localpath, ['.*' strrep(fs,'\','\\') '$'])
                    localfile = [localpath localName]; 
                % If localpath doesn't end with fs add fs
                else
                    localfile = [localpath fs localName];
                end
                
            % if localPath is not a directory assume its a file    
            else
                localfile = localpath;
                localName = '';
            end
            
            % If user wants to be prompted and file already exists
            if ~overwrite && exist (localfile) == 2
                localfile = check_overwrite (localfile, localName, handle);
                if strcmp(localfile, '')
                    return;
                end
            end  
            
            % call gdRetrieve method to get file with GridFTP
            localdata = char (retrieveClient.retrieveFile (handle, host, remotedir, localfile));   
        end
        % Otherwise assume we are retrieving a variable 
    else
        xslt_file = 'matlabName2Type.xsl';
        % Assume this is a struct as there was no second argument
        try
            % retrieve variable using GdRetrieve (VarDataWS web service throws
            % an exception if the var with this handle is not there).
            % Inconsistent with 
            xmlByName = retrieveClient.retrieveVar (handle);
            % convert xml into type based format and turn into a struct
            xmlByType = xml_convert(xmlByName, xslt_file);
            localdata = xml_parse(xmlByType);
        % If data not found give warning in case intention was to retrieve a file
        catch
            warning ('To retrieve a file you need to specify where it should be saved as the second argument.');
            disp_exception(lasterr);
        end
    end
catch
    lasterr
    disp_exception(lasterr);
end

return;

% ==========================================================
function localName = getlocalname (handle) 
    fileStr = 'file';
    metadata_results = gd_query ([fileStr '.ID = ' handle], [fileStr '.*']);
    if ~isempty (metadata_results) 
        m = metadata_results{1};
        if isfield (m.(fileStr), 'localName')
            localName = m.(fileStr).localName;
        end
    end
    % if couldn't find localName for some reason (maybe
    % metadata service was down) name it using the handle.
    if ~exist ('localName')
        localName = handle;
    end
return;

% ==========================================================
function newfilepath = check_overwrite(oldfilepath, localName, handle)
    %formatted_oldfilepath = strrep(oldfilepath,'\','\\');    
    reply = input([strrep(oldfilepath,'\','\\') ' already exists. Do you want to overwrite it? y/n [y]: '],'s');
    if isempty(reply)
        reply = 'y';
    end
    % If user does not want to overwrite file
    if strcmpi (reply, 'n')
        % Prompt for new filename
        newname = input('Please enter a new filepath (or q to quit): ','s');
        % If newname is a directory add original name (localName)
        if strcmpi (newname, 'q')
            newfilepath = '';
            return     
        elseif exist (newname) == 7
             if ~exist(localName) || strcmp (localName, '')
                localName = getlocalname (handle);
             end
             fs = char(java.lang.System.getProperty ('file.separator'));
             localfile = [newname fs localName];   
         % If newname is not a directory, assume its a file    
        else
             localfile = newname;
        end
        % If resulting path refers to an existing file, call function
        % again.
        if exist (localfile) == 2
            newfilepath = check_overwrite(localfile, localName, handle);
        else
            newfilepath = localfile;
        end
    % If user does want to overwrite file
    elseif strcmpi (reply, 'y')
        newfilepath = oldfilepath;
    % Otherwise, invalid input, call function again to prompt user
    else
        disp ('Please answer y or n');
        newfilepath = check_overwrite(oldfilepath, localName, handle);
    end   
return;