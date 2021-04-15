function db_test ()

default_file = 'C:\geodise\TestFile.txt';
retrieve_path = 'C:\temp';
fileStr = 'file';

clc

disp ('Geodise Database Toolkit 0.6 test script. Please make sure you have a valid proxy certificate.') 
disp (' ')
disp ('This test script will:')
disp ('(1) Create 2 datagroups with gd_datagroup.')
disp ('(2) Archive a file with metadata and add it to the first datagroup with gd_archive.')
disp ('(3) Add the archived file to the second datagroup with gd_datagroupadd.')
disp ('(4) Query the file''s metadata with gd_query.')
disp ('(5) Retrieve the file with gd_retrieve.')
disp ('(6) Archive a struct and add it to the second datagroup with gd_archive.')
disp ('(7) Query the struct with gd_query.')
disp ('(8) Retrieve the struct to the Matlab workspace with gd_retrieve.')

pause
disp(' ')
disp ('(1) Creating 2 new datagroups')

dgmetadata.standard.comment = 'This is a comment about a test datagroup';
dgmetadata.access.users = {'zj'};
dg_id1 = gd_datagroup ('First test datagroup', dgmetadata)
disp(' ')
dg_id2 = gd_datagroup ('Second test datagroup', dgmetadata)
pause
disp(' ')
disp ('(2.1) Creating a metadata structure')

userstruct.a = 0.03;
userstruct.b = [0.9, 1.4]
file.comment = 'Comment about file'

metadata.(fileStr).comment = file.comment;
metadata.usertext = 'some test text data';
metadata.usernumber = 43;
metadata.usercell = {'text', 5, 'more text'};
metadata.userstruct = userstruct
metadata.access.users = {'zj'};

pause
disp(' ')
disp ('(2.2) Archiving file with the metadata and adding it to first datagroup')
default_file
disp(' ')
disp ('fileID = gd_archive (default_file, metadata, dg_id1)')
fileID = gd_archive (default_file, metadata, dg_id1)
disp(' ')

pause
disp ('(3) Adding file to a second datagroup')
disp ('gd_datagroupadd (dg_id2, fileID)')
gd_datagroupadd (dg_id2, fileID)
disp(' ')

pause
disp (['(4) Querying to find metadata about file where ''', fileStr, '.ID=', fileID, ''''])

% Could query for other fields
disp (['QResults = gd_query (''', fileStr, '.ID = ', fileID,''', ''', fileStr, '.*'')'])
QResults = gd_query ([fileStr '.ID = ' fileID], [fileStr '.*'])
disp (['QResults{1}.' fileStr])
QResults{1}.(fileStr)
disp(' ')
disp (['QResults{1}.' fileStr '.datagroups.datagroupID'])
QResults{1}.(fileStr).datagroups.datagroupID


handle = QResults{1}.(fileStr).ID;

pause
disp(' ')
disp (['(5) Retrieving file with ID ', handle, ' to location ', retrieve_path])


location = gd_retrieve (QResults{1}.(fileStr).ID, retrieve_path)

pause
disp(' ')
disp ('(6) Archiving a struct and adding it to the second datagroup')
formatter = java.text.SimpleDateFormat('yyyy-MM-dd HH:mm:ss');
today = java.util.Date;
date = char(formatter.format(today));
a.text = '  test  ';
a.details.five = 5;
a.details.date = date
gd_display(a)
disp(' ')
disp ('structID = gd_archive (a, [], dg_id2)')
disp(' ')
structID = gd_archive (a, [], dg_id2)

pause

disp(' ')
disp (['(7) Querying to find structs where details.date = ', date ])
results = gd_query (['details.date=', date],'vardata')
gd_display (results{1})

pause
disp(' ')
disp (['(8) Retrieving struct with ID ',structID, 'to Matlab variable c.'])
c = gd_retrieve(structID)