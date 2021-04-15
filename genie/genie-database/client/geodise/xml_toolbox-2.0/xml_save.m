function xml_save( file, S, att_switch )
%function xml_save( filename, S, att_switch )
%
% Saves structure or variable(s) S in xml format to a file.
%
% INPUT 
%   filename     filename 
%   S            Matlab variable or structure to store in file.
%   att_switch   optional, 'on' stores XML type attributes (default), 
%                'off' doesn't store XML type attributes
%
% OUTPUT 
%   none
%
% RELATED 
%   xml_load, xml_format, xml_parse, (xmlread, xmlwrite)
%
% --------------------------------------------------------------
% Copyright Geodise/GEM 2002-2003, University of Southampton, UK
% Author: Marc Molinari <m.molinari@soton.ac.uk>
% $Revision: 297 $ $Date: 2003-12-03 01:40:18 +0800 (三, 2003-12-03) $

% Please read the licence in file licence.txt.

%---------------------------
% INIT 
if (nargin<2) | ~strcmpi(class(file), 'char')
  disp([mfilename, ' requires 2 or 3 parameters: filename and' ...
		   ' variable, optionally att_switch.']);
  return
end

if nargin<3, att_switch='on'; end
if ~strcmpi(att_switch, 'off'); att_switch = 'on'; end

%-----------------------------------------------
% append '.xml'
if isempty(findstr(lower(file),'.xml'))
  file = strcat(file, '.xml');
end

%-----------------------------------------------
% overwrite ?
%if (exist(file))
%  q = sprintf('Overwrite existing file %s ?', file);
%  title = 'Warning: File already exists!';
%  dosave = questdlg(q, title, 'No', 'Yes', 'No');
%  if strcmpi(dosave, 'no')
%    return;
%  end
%end

%=====================================================
fid = fopen(file, 'w');
if fid==-1
  error(['Error while writing file ', file]);
end

% write file header
fprintf(fid, '<?xml version="1.0"?>\n');
fprintf(fid, sprintf('<!-- written on %s -->\n', datestr(now)));

% write XML string
fprintf(fid, '%s', xml_format(S, att_switch));

% close file
fclose(fid);
