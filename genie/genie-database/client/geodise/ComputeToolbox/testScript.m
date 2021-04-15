% This testScript demonstrates the use of the Geodise computational toolbox
% for Matlab. A proxy certificate is created, files are transfered to and
% from the Globus server and a GRAM job is submitted. This script should be
% run from within a directory containing the matlabDemo.sh shell script.

% Copyright 2002 Geodise Project, University of Southampton
% Graeme Pound 25/10/02
% Geodise computational toolbox for Matlab

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up the demo variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Globus server upon which to run the shell script. Note that the
% certificate subject line (returned by gd_certinfo) must map to a user
% account on the Globus server.
GLOBUSSERVER = 'escience-dept2.sesnet.soton.ac.uk';
servername = input('What is the name of your Globus server? ','s');
if ~isempty(servername)
    GLOBUSSERVER = servername
else
    disp(GLOBUSSERVER)
end 

DIRNAME = 'mdemo/';

disp('Create a proxy certificate')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a proxy certificate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a Globus proxy certificate for GSI authentication
gd_createproxy
% Return information about the proxy certificate
gd_proxyinfo;

disp('Creating a working directory...')
gd_makedir(GLOBUSSERVER, DIRNAME)

disp('Printing matlabdemo.sh....')
disp('###########################################')
type matlabdemo.sh
disp('###########################################')
disp(' ')

disp('Transfering matlabdemo.sh to Globus server using GridFTP')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Put the remote files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GridFTP the demo shell script from the local directory to the users home
% directory on GLOBUSSERVER 
gd_putfile(GLOBUSSERVER,'matlabDemo.sh',[DIRNAME,'matlabDemo.sh'])

% Modify the permissions on the shell script, currently there is no API to
% perform chmod in the file transfer so this is a work around.
disp(' ')
ff = gd_jobsubmit(['&(executable=/bin/chmod) (arguments = 700 ',DIRNAME,'matlabDemo.sh)'],GLOBUSSERVER);
gd_jobpoll(ff) 

disp('Printing the RSL string...')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run the demonstration script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The RSL string describes the GRAM job to be submitted to GLOBUSSERVER, the
% stdout and stderr are redirected to file. Note that GRAM will append
% existing files of the same name with output
RSLstring = ['&(executable=',DIRNAME,'matlabDemo.sh)(stdout=',DIRNAME,'stdout.out)(stderr=',DIRNAME,'stderr.out)']
%RSLstring = ['&(executable=matlabDemo.sh)(stdout=stdout.out)(stderr=stderr.out)(directory=',DIRNAME,')']
%RSLstring = '&(executable=matlabDemo.sh)(stdout=stdout.out)(stderr=stderr.out)(directory=mdemo)'

disp('Submitting job to the Globus server')
% Submit the job to the GRAM gatekeeper on GLOBUSSERVER
jobHandle = gd_jobsubmit(RSLstring,GLOBUSSERVER)

% Terminate the GRAM job
%gd_jobkill(jobHandle)
disp('Polling jobhandle until completion')
disp(' ')
% Poll the GRAM job ever 4 seconds for a maximum of one minute
gd_jobpoll(jobHandle, 4, 60);

disp('Retrieving output from the Globus server using GridFTP')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the output files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Retrieve the files
disp(' ')
gd_getfile(GLOBUSSERVER,[DIRNAME,'stdout.out'],[pwd,'/stdout.out'])
gd_getfile(GLOBUSSERVER,[DIRNAME,'stderr.out'],[pwd,'/stderr.out'])
    
% Display the output
disp(' ')
disp('stdout...')
type([pwd,'/stdout.out'])
disp('stderr...')
type([pwd,'/stderr.out'])

disp('Cleaning up...')
%local files
delete([pwd,'/stdout.out'])
delete([pwd,'/stderr.out'])

%remote files
gd_rmfile(GLOBUSSERVER,[DIRNAME,'stdout.out'])
gd_rmfile(GLOBUSSERVER,[DIRNAME,'stderr.out'])
gd_rmfile(GLOBUSSERVER,[DIRNAME,'matlabDemo.sh'])

gd_rmdir(GLOBUSSERVER, DIRNAME)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Destroy the proxy certificate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp('Destroying the proxy certificate')
disp(' ')
gd_destroyproxy;
