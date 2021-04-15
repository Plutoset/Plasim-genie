function id = gd_archive (localdata, metadata, datagroupID, datatype)

%   gd_archive archives a file or struct in the Geodise respository with some
%   metadata.
%
%   id = gd_archive(filepath, [metadata], [datagroupID]) takes a string
%   representing the full path name of a local file and stores it in the 
%   remote file store (specified in the ClientConfig.xml file). A handle 
%   (id) is returned which is the unique identifier that can be used to
%   retrieve the file with gd_retrieve.
%
%   id = gd_archive(var, [metadata], [datagroupID], [datatype]) is similar but
%   takes a Matlab variable and stores it in a remote database (currently
%   specified in a config file on the server). If var is a struct this will
%   be automatically detected, otherwise you must set the datatype argument
%   to 'vardata' to indicate that this is a variable and not a file.
%
%   The function optionally takes a metadata structure specifying some
%   infortmation about the file/struct that can be later queried with 
%   gd_query, and a datagroupID which is a handle to a datagroup the 
%   file/struct should be added to (see gd_datagroup).
%
%   The metadata structure contains two special structures called standard 
%   and access. The rest of the fields may be set to any value (e.g. v1, v2, v3).
%   metadata = 
%          standard: [1x1 struct]
%            access: [1x1 struct]     
%                v1: [1, 2, 3.5]
%                v2: 'some text'
%                v3: [1x1 struct]
%
%   If no metadata structure is specified one is generated with some basic
%   properties in metadata.standard. This consists of ID, userID,
%   createDate and archiveDate (plus byteSize, format, and localName for
%   files), see gd_query for a description of these properties. You can add
%   a comment field to metadata.standard and add your own values for format
%   and localName. Any other fields you set in metadata.standard will be 
%   overwritten or removed.
%
%   The metadata.access structure controls who may query and retrieve your data.
%   It can contain two arrays:
%   users - user ID strings specifying which users may access your data.
%   groups - group ID strings specifying which groups may access your data.
%   
%   Examples:
%   (1) Archive a file with no user metadata:
%           handle = gd_archive ('C:\file.dat')
%           handle = 
%               file_dat_ce868f40-8de0-445e-8ae5-36c05eec25a9    
%
%   (2) Archive a file with some user metadata:  
%           metadata.standard.comment = 'Comment about file';
%           % Default local name for file when using gd_retrieve
%           metadata.standard.localName ='myFile.txt';
%           metadata.access.users = {'user1', 'user2'};
%           metadata.iterations = 9000;
%           metadata.component.gamma = 1.4;
%           handle = gd_archive ('C:\file.dat', metadata);
%
%   (3) Archive a struct with some user metadata and add it to a datagroup:
%           mystruct.a = 45.0376;
%           mystruct.b = [1, 2.6, 4.3, 5.4]
%           metadata.standard.comment = 'Description of mystruct';
%           handle = gd_archive (mystruct, metadata, 'datagroup_ID_1234567');
%
%   Notes:  
%    (1) A valid proxy certificate is required to archive a file (see
%        gd_createproxy from the Geodise computational toolbox).
%    (2) You must have access to the host machine the files will be
%        archived on. Your certificate subject must be added to the
%        gridmap file on the host and to the authorisation database.
%
%   See also: gd_retrieve, gd_query, gd_datagroup, gd_datagroupadd, gd_createproxy
%
% Copyright 2002-2003 Geodise Project, University of Southampton
% Jasmin Wason (j.l.wason@soton.ac.uk), last modified on 01/05/03
% Geodise database toolbox for Matlab version 0.5

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required. Use gd_createproxy to create one.')
end

error (nargchk (1, 4, nargin))

% whether the Java code should print messages
verbose = true;

file_field = 'file';
varmeta_field = 'varmeta';

% hostname, hostdir and aliases for metadata types are read in from an XML config file.
try 
    config = org.geodise.database.util.Config ('ClientConfig.xml');
    config.parseNodes ('fileserver');
    % hostname = 'escience-dept2.sesnet.soton.ac.uk';
    hostname = config.getConfig ('hostname');
    % hostdir = '/usr/local/archive';
    hostdir = config.getConfig('hostdir');
    
    config.parseNodes ('datasource_aliases');
    var_alias = char(config.getConfig('vardata'));
catch
    disp_exception(lasterr);
end
% xslt files for converting xml data. These must be in userhome/.geodise
xslt = 'removeLeadingSpace.xsl';
%xslt = 'matlabType2Name.xsl';
%var_xslt = 'matlabType2Name.xsl';
%filemeta_xslt = 'matlabType2Name_File.xsl';
%varmeta_xslt = 'matlabType2Name_VarMeta.xsl';

% if localdata is a variable (indicated by datatype == var)
if exist ('datatype') && strcmp (datatype, var_alias)
    isfile = false;
else
    % if localdata is a string then it must be a valid file path.
    if ischar (localdata)
        if exist (localdata) == 2
            isfile = true;
        else
            error(['''',localdata, ''' is not a valid filename.']);    
        end
        % otherwise localdata must be a struct
    else
        if isstruct (localdata)
            isfile = false;
        else
            error('First argument must be a character array (representing a file path), a structure, or a variable. For a variable the correct syntax is gd_archive (myvariable, [metadata], [datagroup], ''var'')')
        end
    end
end
% check metadata is a struct or an empty array
if exist('metadata') && ~isstruct(metadata) && ~isempty(metadata)
    error('metadata must be a struct or empty array')
end

% check datagroupID is a string
if exist('datagroupID') && ~ischar (datagroupID) && ~isempty(datagroupID)
    error('datagroupID must be a character array')
end

try
    archiveClient = org.geodise.database.GdArchive;
    % Generate metadata for file or structure
    config.parseNodes ('output_xmlroot');
    custom_root = char(config.getConfig('custom'));
    if isfile
        % if file
        genMeta = org.geodise.database.util.MetadataGenerator (localdata);
        standard_field = file_field;
        standard_alias = char(config.getConfig(file_field));
        metadata_root = char(config.getConfig([file_field '_root']));
    else
        % if structure
        genMeta = org.geodise.database.util.MetadataGenerator ('var');
        standard_field = varmeta_field;
        standard_alias = char(config.getConfig(varmeta_field));
        metadata_root = char(config.getConfig([varmeta_field '_root']));
    end
catch
    disp_exception(lasterr);
end

% get handle generated by MetadataGenerator
handle = char (genMeta.getID);

% Extract information from metadata if it exists
users = {};
groups = {};

if exist('metadata')    
    if ~isempty(metadata)
        mdcopy = metadata;
    end
    % If metadata contains a standard field copy it to mdstd
    if isfield(metadata, standard_alias)
        mdstd = metadata.(standard_alias);
    else
        % If metadata has no 'standard' field, set mdstd to empty
        mdstd = [];
    end   
    % If metadata contains an access field
    if isfield(metadata, 'access')
        % if access contains a users array copy it to users
        if isfield(metadata.access, 'users') && iscell(metadata.access.users)
            users = metadata.access.users;
        end
        % if access contains a groups array copy it to groups
        if isfield(metadata.access, 'groups') && iscell(metadata.access.groups)
            groups = metadata.access.groups;
        end
        mdcopy = rmfield (mdcopy, 'access');
    end
else
    % If no metadata structure supplied, set mdstd to empty
    mdstd = [];
end

% Generate and format the metadata

% remove any standard fields that should not be there
% by copying correct fields into another structure
% then copying it back

if isfile
    % localName
    if ~isfield (mdstd, 'localName')
        strd.localName = char (genMeta.getLocalName);
    else
        strd.localName = mdstd.localName;
    end
end
% comment
if (isfield (mdstd, 'comment'))
    strd.comment = mdstd.comment;
end
if isfile
    % byteSize
    strd.byteSize = double (genMeta.getByteSize);
    % createDate
    strd.createDate = char (genMeta.getCreateDate);
    % format
    if ~isfield (mdstd, 'format')
        strd.format = char (genMeta.getFormat);
    else
        strd.format = mdstd.format;
    end
end


% fileIDs
%if exist('datagroupID') && ~isempty(datagroupID)
%    strd.datagroups(1).datagroupID = datagroupID;
%end

% mdcopy.standard = strd;
% No longer copy strd into mdcopy as standard and custom metadata are now
% seperate. Instead, remove the standard field from mdcopy.
if exist('mdcopy') && isfield(mdcopy, standard_alias)
    mdcopy = rmfield (mdcopy, standard_alias);
end

if exist ('strd') && ~isempty('strd')
    standard.(standard_field) = strd;
else
    standard.(standard_field) = struct('empty', {});
end

% Archive the file or the structure
try
    % if file use GdArchive to transfer it to the host
    if isfile
        archiveClient.archiveFile (handle, localdata, hostname, hostdir, users, groups);
    % if struct use GdArchive to store it as XML in a database   
    else
        % Format the structure into an XML string
        %structxmlstring_t = xml_format (localdata);
        vardata_root = char(config.getConfig('vardata_root'));
        structxmlstring_n = xml_format (localdata, [], vardata_root);
        % Convert the XML string into a name-based format       
        %structxmlstring_n = xml_convert (structxmlstring_t, xslt, vardata_root);
        archiveClient.archiveXmlData (handle, structxmlstring_n, users, groups);
    end
catch
    disp_exception(lasterr);
end

% Even if metadata archive fails, id still available
id = handle;

% Archive the metadata

try 
    % Format the std structure into a name-based XML string
    %xmlStdString = xml_format (standard);
    xmlStdString = xml_format (standard, [], metadata_root);
    %xmlStandardString = xml_convert (xmlStdString, xslt, metadata_root);
    xmlStandardString = xml_convert (xmlStdString, xslt);
    
    % Format the mdcopy structure into a name-based XML string
    if exist('mdcopy')
        %xmlCustString = xml_format (mdcopy);
        xmlCustString = xml_format (mdcopy, [], custom_root);
        %xmlCustomString = xml_convert (xmlCustString, xslt, custom_root);
        xmlCustomString = xml_convert (xmlCustString, xslt); 
    else
        % if no custom metadata set xmlCustomString to null
        xmlCustomString = [];
    end
    % If there is no datagroupID set it to the empty string
    if ~exist('datagroupID') || isempty(datagroupID)
        datagroupID = '';
    end
        
    % User GdArchive to store the metadata in a database
    char (archiveClient.archiveMetadata (xmlStandardString, xmlCustomString, handle, datagroupID, standard_field));
    %char (archiveClient.archiveMetadata (xmlNameBasedString, handle, datagroupID));

catch
    disp_exception(lasterr);
end