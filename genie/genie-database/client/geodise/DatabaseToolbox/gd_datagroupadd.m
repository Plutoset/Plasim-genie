function success = gd_datagroupadd (datagroupID, dataID, metadata_type)

%   gd_datagroupadd adds some data to a datagroup. 
%
%   gd_datagroupadd (datagroupID, dataID)
%   The datagroup is specified with the datagroupID handle and the data is 
%   specified with the dataID handle. The datagroup must have been created 
%   with gd_datagroup and the data must be file or a structure that was
%   archived using gd_archive.
%
%   Notes:  
%    (1) A valid proxy certificate is required (see gd_createproxy from the
%        Geodise computational toolbox).
%    (2) Your certificate subject must have beeb added to the authorisation 
%        database.
%
%   See also: gd_datagroup, gd_archive, gd_retrieve, gd_query, gd_createproxy
%
% Copyright 2002-2003 Geodise Project, University of Southampton
% Jasmin Wason (j.l.wason@soton.ac.uk), last modified on 01/05/03
% Geodise database toolbox for Matlab version 0.5

%Check is the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('A valid proxy certificate is required. Use gd_createproxy to create one.')
end

error (nargchk (2, 3, nargin))

if exist('metadata_type')
    try 
        config = org.geodise.database.util.Config ('ClientConfig.xml');
        config.parseValues ('datasource_aliases');
        metadata_type = config.getConfig(metadata_type)
    catch
        disp_exception(lasterr);
    end
else
    metadata_type = 'file';
end

try
    datagroupClient = org.geodise.database.GdDatagroup;
    datagroupClient.updateDatagroup (datagroupID, dataID, metadata_type);
    success = 1;
catch
    disp_exception(lasterr);
end