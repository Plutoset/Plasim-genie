function [X] = xml_load( file )
%function [X] = xml_load( file )
%
% Loads XML file and converts it into Matlab structure or variable.
%
% INPUT 
%   file     filename of xml file written with xml_save
%
% OUTPUT 
%   X        structure variable containing file contents
%  [xmlV     !changed: returns empty string to maintain compatibility]
%
% SEE ALSO 
%   xml_format, xml_parse, xml_save, (xmlread, xmlwrite)
%
% --------------------------------------------------------------
% Copyright Geodise/GEM 2002-2003, University of Southampton, UK
% Author: Marc Molinari <m.molinari@soton.ac.uk>
% $Revision: 297 $ $Date: 2003-12-03 01:40:18 +0800 (三, 2003-12-03) $

% Please read the licence in file licence.txt.

% INTERFACE CHANGE:
% xml_load does not return XML version as second output any more.

% ----------------------------------------------------------
% Initialisation and checks on input parameters

% set XML TB version number
xml_tb_version = '2.0';

% check input parameters
if (nargin<1)
  error([mfilename,' requires 1 parameter: filename.']);
end

% append '.xml'
if ~exist(file)
  if isempty(findstr(lower(file),'.xml'))
    file = strcat(file, '.xml');
  end
end

%-----------------------------------------------
% check existence of file
if (~exist(file))
  error([mfilename, ': could not find ', file]);
end

%-----------------------------------------------
fid = fopen(file, 'r');
if fid==-1
  error(['Error while opening file ', file, ' for reading.']);
end

% parse file content into blocks
str = char( fread(fid)' ); % read in whole file
fclose( fid );

if (length(str)<3)
  error([file, ' does not seem to be a valid xml file.']);
end

%-----------------------------------------------
% parse content, identify blocks
X = xml_parse(str);

return
