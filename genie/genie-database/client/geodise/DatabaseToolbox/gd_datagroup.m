function id = gd_datagroup (datagroupname, metadata)

%   gd_datagroup creates an empty datagroup collection, which can be used to group 
%   together data (files and structs) that has been archived with gd_archive.
%   
%   id = gd_datagroup (datagroupname, [metadata]) creates a new, empty datagroup 
%   with a datagroup name. The datagroupname argument can act as a user 
%   defined identifier for the datagroup, although it does not have to be unique.
%   gd_datagroup optionally takes a metadata structure specifying some
%   information about the datagroup that can be later queried with gd_query. 
%   
%   The function creates a datagroup in the database and returns a unique handle 
%   (id) for the group. This handle can then be used to add data to the
%   datagroup while that data is being archived with gd_archive, or after the
%   the data has been archived with gd_datagroupadd.
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
%   properties in metadata.standard. This consists of ID, userID, and
%   archiveDate, see gd_query for a description of these properties. You can add
%   a metadata.standard.comment field but any other fields you set in 
%   metadata.standard will be overwritten or removed.
%
%   The metadata.access structure controls who may query your datagroup.
%   It can contain two arrays:
%   users - user ID strings specifying which users may query your datagroup.
%   groups - group ID strings specifying which groups may query your datagroup.
%   
%   Example: create a datagroup and add some data to it
%       metadata.standard.comment = 'Comment about datagroup';
%       metadata.access.users = {'user1', 'user2'};
%       metadata.problem_domain = 'widget optimisation';
%       datagroup_id = gd_datagroup ('widget opt problem 73', metadata)
%       datagroup_id = 
%       dg_ce868f40-8ds0-455e-9ae5-36c05epc25a9
%       gd_archive ('C:\file.dat', [], datagroup_id);
%       gd_datagroupadd (datagroup_id, 'var_ce868f40-8de0');
%
%   Notes:  
%    (1) A valid proxy certificate is required (see gd_createproxy from the
%        Geodise computational toolbox).
%    (2) Your certificate subject must have beeb added to the authorisation 
%        database.
%
%   See also: gd_datagroupadd, gd_archive, gd_retrieve, gd_query, gd_createproxy
%
%Copyright 2002-2003 Geodise Project, University of Southampton
% Jasmin Wason (j.l.wason@soton.ac.uk), last modified on 01/05/03
%Geodise database toolbox for Matlab version 0.5


%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required. Use gd_createproxy to create one.')
end

error (nargchk (1, 2, nargin))
usage = 'Usage gd_datagroup (string datagroupname, struct metadata)';

try
    datagroupClient = org.geodise.database.GdDatagroup;
    genMeta = org.geodise.database.util.MetadataGenerator ('dg');
catch
    disp_exception(lasterr);
end

% Extract information from metadata if it exists
users = {};
groups = {};

dg_field = 'datagroup';
try 
    config = org.geodise.database.util.Config ('ClientConfig.xml');
    config.parseNodes ('datasource_aliases');
    config.parseNodes ('output_xmlroot');
    custom_root = char(config.getConfig('custom'));
    standard_alias = char(config.getConfig(dg_field));
    metadata_root = char(config.getConfig([dg_field '_root']));
catch
    disp_exception(lasterr);
end

if exist('metadata')    
    if ~isempty(metadata)
        mdcopy = metadata;
    end
    % If metadata contains a standard field
    if isfield(metadata, standard_alias)
        mdstd = metadata.(standard_alias);
    else
        % If metadata has no 'standard' field, set mdstd to empty
        mdstd = [];
    end   
    % If metadata contains an access field
    if isfield(metadata, 'access')
        if isfield(metadata.access, 'users')
            users = metadata.access.users;
        end
        if isfield(metadata.access, 'groups')
            groups = metadata.access.groups;
        end
        mdcopy = rmfield (mdcopy, 'access');
    end
else
    % If no metadata structure supplied, set mdstd to empty
    mdstd = [];
end

% if metadata provided 
if nargin == 2    
    % remove any standard fields that should not be there
    % by copying correct fields into another structure
    % then copying it back
    % comment
    if (isfield (mdstd, 'comment'))
        std.comment = mdstd.comment;
    end
end

% datagroupname
std.datagroupname = datagroupname;

% mdcopy.standard = std;
% No longer copy std into mdcopy as standard and custom metadata are now
% seperate. Instead, remove the standard field from mdcopy.
if exist('mdcopy') && isfield(mdcopy, standard_alias)
    mdcopy = rmfield (mdcopy, standard_alias);
end

standard.(dg_field) = std;

try
    
    %xslt = 'matlabType2Name.xsl';
    xslt = 'removeLeadingSpace.xsl';
    % Format the std structure into a name-based XML string
    %xmlStdString = xml_format (standard);
    xmlStdString = xml_format (standard, [], metadata_root);
    %xmlStandardString = xml_convert (xmlStdString, xslt, metadata_root);
    xmlStandardString = xml_convert (xmlStdString, xslt);
    
    % Format the mdcopy structure into a name-based XML string
    if exist('mdcopy')
        %xmlCustString = xml_format (mdcopy);
        xmlCustString = xml_format (mdcopy, [], custom_root);
        %xmlCustomString = xml_convert (xmlCustString, xslt, 'metadata'); 
        xmlCustomString = xmlCustString;
    else
        % if no custom metadata set xmlCustomString to null
        xmlCustomString = [];
    end
    
    handle = char (genMeta.getID);
    
    % char converts the Java String result into a Matlab char
    result = char (datagroupClient.createDatagroup (xmlStandardString, xmlCustomString, handle, users, groups));
    %result = char (datagroupClient.createDatagroup (xmlNameBasedString, handle, users, groups));
catch
    disp_exception(lasterr);
end

id = handle;
