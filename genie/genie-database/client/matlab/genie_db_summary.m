function genie_db_summary()
% genie_db_summary
%
% Retrieves summary information from the GENIE database.
%
% Script based upon the gd_display function from the
% GEODISE toolkit.
%
% GENIE Project - 26th November 2003
% Andrew Price, Southampton e-Science Centre

% Check that the user has a valid certificate.
isvalid = gd_proxyquery;
if ~isvalid
    error('Warning: A valid proxy certificate is required. Run gd_createproxy to create one.')
end

% Header
clc;
disp(' ');
disp(' ********************************** ');
disp(' GENIE Database Summary Information ');
disp(' ********************************** ');
disp(' ');
disp([' Date: ',datestr(now)]);
disp(' ');
%disp(' This database client is currently using the following proxy certificate:');
disp(' ************************* ');
disp(' Current Proxy Certificate ');
disp(' ************************* ');
disp(' ');
gd_proxyinfo;

% Query the database for unique experiment names
% Until we produce a webservice to process this type of query we must
% use a poor assumption to retrieve this information. This query
% assumes that each experiment will have generated a file named
% 0001.1.gz.
%disp(' Querying the database...');
results = gd_query('file.localName=0001.1.gz','file.ExperimentName');

% Display the ExperimentNames available in the database.
%disp(' ');
%disp(' There is data available for the following experiments:');
disp(' *************** ');
disp(' Experiment Data ');
disp(' *************** ');
disp(' ');
if ~isempty(results)
    if iscell(results)
        [row, col] = size(results);
        for i = 1:col-1
            %disp(' ');
            disp_struct('', results{i})
        end
        %disp( ' ');
        disp_struct('', results{col});
        disp(' ');
    elseif isstruct(results)
         disp_struct('', results)
    else
        disp(['*** Cannot display the results of the database query. ***']);
    end
end

% Display metadata tags available for query
%disp('Querying the database...');
result=gd_query('file.localName=0001.1.gz & file.ExperimentName=tlent_expt_4','file.*');
%disp(' ');
%disp('The database may be queried for information with the following metadata tags.');
disp(' **************** ');
disp(' Metadata Summary ');
disp(' **************** ');

% Process the standard metadata fields.
standard_metadata=fieldnames(result{1}.file);
std_array=['     ',standard_metadata{1}];
for i=2:size(standard_metadata,1)-1
    std_array=[std_array,', ',[standard_metadata{i}]];
end
disp(' ');
disp(' Standard Metadata: ')
disp(' ');
disp(std_array);
disp(' ');

% Process the GENIE specific metadata fields.
genie_metadata=fieldnames(result{1});
disp(' GENIE Metadata:');
disp(' ');

% For display purposes divide the fields up into sets of 10.
sets=ceil(size(genie_metadata,1)/10);
for set=1:sets
    first_index=((set-1)*10)+2;
    meta_array=['     ',genie_metadata{first_index}];
    last_index=min(size(genie_metadata,1),(set*10)+1);
    for i=first_index+1:last_index
        meta_array=[meta_array,', ',[genie_metadata{i}]];
    end
    disp(meta_array);
end