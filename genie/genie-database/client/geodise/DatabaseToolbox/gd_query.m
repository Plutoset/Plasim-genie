function QResults = gd_query(q_cstr, returned_items)
%     function QResults = gd_query(q_cstr, returned_items)
%
% This function can be used to perform queries over the Geodise archive to 
% search for entries in the following databases:
%     (1) database storing metadata about files, datagroups, and Matlab variables;
%     (2) database storing Matlab structures.
%
% To search for entries that satisfy certain criteria, specify the criteria
% in q_cstr which is a string, e.g.
%
%    q_cstr = '<datasource>.userID = me'
%    q_cstr = '<datasource>.VARS.name = BREADTH & <datasource>.VARS.value > 25'
%
%  where <datasource> can be one of the followings:
%    'file'       - referring to metadata about files.
%    'datagroup'  - referring to metadata about datagroups.
%    'varmeta'    - referring to metadata about Matlab variables.
%    'var'        - referring to Matlab structures.
%
% The 'returned_items' can be used to specify the data items to be returned by
% the query, for example,
%     gd_query('file.userID=me', 'file.ID, file.VARS.name')
%     gd_query('datagroup.userID=me', 'datagroup.*')
%
% Using '*' in the returned_items will cause the gd_query() to return all metadata about 
% the data source (e.g. 'datagroup' in the above example). 
%
% To view the query results,  use function gd_display(QResults). 
%
% #############
% # Query GUI # Use gd_query without any input argument will generate a 
% ############# graphical user interface for querying the standard metadata. 
% 
% Example: 
% (1) Query metadata about files:
%       R = gd_query('file.userID=me & file.archiveDate>2003-04-30', 'file.*')
% (2) Query metadata about datagroup:
%       R = gd_query('group.userID=me & group.createDate=2003-05-11', 'group.ID')
% (3) Query metadata about Matlab variables:
%       R = gd_query('varmeta.userID=me', 'varmeta.*')
%       R = gd_query('varmeta.myVar1>=123 & varmeta.myVar2=xyz', 'varmeta.ID')
% (4) Query over Matlab structures:
%     (a) define and store a structure:
%           s.para.name = 'BREADTH'
%           s.para.value = 48.5
%           s.b = [3.3 8.8]
%           s.c = {'xyz', [1 2], {'a', 'b'}}
%           gd_archive(s)
%     (b) query the structure:
%		R = gd_query('var.para.name=BREADTH & var.para.value > 25');
%		R = gd_query('var.b=3.3 8.8', 'var.*');
%
% Note: When querying on date information, please specify the date/time 
% using International Standard Date and Time Notation (ISO 8601)
% which is: "YYYY-MM-DD hh:mm:ss"
%
% The output of the function is a cell of MATLAB structures. 
% Entries returned from querying the metadata database contain the 
% standard metadata as well as the custom-defined metadata.
% Entries returned from querying the Matlab structure database contain 
% only the custom-defined data. The following is a list of 
% standard metadata:
%
%   ID          - file/variable ID (i.e. handle for retrieving the file or variable).
%   localName   - The name of the file on user's local machine.
%   byteSize    - the size of the file.
%   format:     - file format, decided by the suffix of the file name. For exampl: 'jpg', 'dat'.
%   createDate  - The date the file/variable is created.
%   archiveDate - The date the file/variable is archived.
%   userID      - The ID of the user who created the file/variable.
%   comment     - Comments on the files/variables.
%   version     - version of this piece of data.
%   datagroups  - A list of IDs of the datagroups which contain the current file/variable.
%
% Example: 
%   >> R = gd_query('file.userID=me' , 'file.*')
%   >> gd_display(R)
%     *** Total number of structures: 12 ***       
%     *** Content of structure R{1} ***
%                      standard.ID: 'input_dat_0ae10047-9b3d-45'
%               standard.localName: 'input.dat'
%                standard.byteSize: '424'
%                  standard.format: 'dat'
%              standard.createDate: '2003-02-04 07:36:50'
%             standard.archiveDate: '2003-02-21 14:33:13'
%                  standard.userID: 'me'
%                 standard.comment: 'This is an input file for job 10.'
%  standard.datagroups.datagroupID: dg_73c3ef34-92d8-4224
%  standard.datagroups.datagroupID: dg_f5e70615-8274-4297
%                           myVar1: 123.45
%                           myVar2: 'xyz'
%
% See also: gd_display, gd_createproxy, gd_archive, gd_retrieve, gd_datagroup, gd_datagroupadd
%
% Copyright 2002-2003 Geodise Project, University of Southampton,
% Zhuoan Jiao (z.jiao@soton.ac.uk), last modified on 18/07/2003.
% Geodise database toolbox for Matlab, version 0.6

% display the number of input arguments:
% disp(['NARGIN  = ',num2str(nargin)]);
% disp(['NARGOUT = ',num2str(nargout)]);

% new comment - testing cvs

%Check if the user has a valid certificate
isvalid = gd_proxyquery;
if ~isvalid
    error('Warning: A valid proxy certificate is required. Run gd_createproxy to create one.')
end

%if nargout==0
%    error('Warning: No output arguments specified!');
% end 

if nargout > 2
   error('Warning: To many output arguments!');
end 

switch nargin
    case 0
          queryGUI = org.geodise.database.QueryGUI.main(' '); % 
          return;
    case 1
        error('Warning! gd_query requires two input arguments. See help.')
    end

try
    queryClient = org.geodise.database.QueryOracle; % it retrieves the user's certificate.
    %xmlByNameStringList = queryClient.getAuthorizedData(q_cstr, returned_items);
    %disp ('About to send query to web service')
    %tic 
    xmlStringList = queryClient.getAuthorizedData(returned_items, q_cstr);
    %xmlStringList(1)
    %disp('Results returned');toc
catch
    disp_exception(lasterr);
end

% disp(['>>>',xmlByNameStringList,'<<<'])

if ~isempty(xmlStringList)
    
    listSize = xmlStringList.length;
    %
    % Using the XSLT files prepared by jlw to convert general XML to Matlab specific XML, i.e.
    % the 'name-based' XML documents which will be further translates to 'type-based' XML documents.
    % The 'type-based' XML documents can be further converted by mm's XMLToolBox into MATLAB structures.
    %
    %XML2MatlabTime = 0;
    %MatlabName2TypeTime = 0;
    %xml_parseTime = 0;
    try
        %tic
        %xml2MxmlByNameList = xml_convert(xmlStringList, 'XML2Matlab.xsl');
        %XML2MatlabTime = XML2MatlabTime + toc;
    
        %tic
        %xmlByTypeList = xml_convert(xml2MxmlByNameList, 'matlabName2Type.xsl');
        xml2MxmlByNameList = xml_convert(xmlStringList, 'XML2Matlab.xsl');
        %MatlabName2TypeTime = MatlabName2TypeTime + toc;
    catch
        disp_exception(lasterr);
    end
    
    for i = 1:listSize
        % display(xmlByType);
        %tic
        QResults{i} = xml_parse(char(xml2MxmlByNameList(i)));
        %xml_parseTime = xml_parseTime + toc;
    end

    %disp(['Total time to convert with XML2Matlab: ' num2str(XML2MatlabTime) ' (average ' num2str(XML2MatlabTime/listSize) ' per result)'])
    %disp(['Total time to convert with MatlabName2Type: ', num2str(MatlabName2TypeTime), ' (average ', num2str(MatlabName2TypeTime/listSize), ' per result)'])
    %disp(['Total time to xml_parse: ', num2str(xml_parseTime), ' (average ', num2str(xml_parseTime/listSize), ' per result)'])
else
    QResults = {};
    disp(' *** Query result is empty.');
end

