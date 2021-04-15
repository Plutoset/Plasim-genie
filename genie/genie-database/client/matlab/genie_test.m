function genie_test()
% genie_test.m
%
% Performs a simple test of the GENIE Database system. A query is
% performed on the database that should return data information 
% about 10 files in the system. These files are then retrieved
% from the system and the time taken for the transfer is reported.
% The files are removed from the local working directory.
%
% GENIE Project - 26th November 2003
% Andrew Price, Southampton e-Science Centre

% Check for proxy
isvalid = gd_proxyquery;
if ~isvalid
    error('Warning: A valid proxy certificate is required. Run gd_createproxy to create one.')
end

clc;

disp(' ************************** ');
disp(' GENIE Database System Test ');
disp(' ************************** ');
disp(' ');
disp(' Querying the database... ');
disp(' ');
disp('>> results = gd_query(''file.ExperimentName=tlent_expt_1 & file.dfwx=0.0 & file.diffamp2<50000.0'',''file.*'');');

% Perform a query on the database.
results=gd_query('file.ExperimentName=tlent_expt_1 & file.dfwx=0.0 & file.diffamp2<50000.0','file.*');

% Display the results
%genie_display(results);

disp(' ');
disp(' Retrieving the test files to the local filesystem... ');
disp(' ');
disp('>> genie_retrieve(results, ''.'', ''overwrite'');');

% Start the timer.
tic;

% Retrieve the files to a local temporary directory
genie_retrieve(results,'.','overwrite');

% Record the time taken for the file transfers.
time_taken=toc;

% Report the time taken for the file transfers
disp(' ');
disp(' File transfer successful. ');
disp(' ');
disp([' Duration of file transfers = ',num2str(time_taken),' seconds.']);
disp(' ');

% Remove the files that have been transferred.
for i=1:size(results,2)
    if exist(results{i}.file.ID,'file')
        eval(['delete ',results{i}.file.ID]);
    end
end