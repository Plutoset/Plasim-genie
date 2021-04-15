function xstr = xml_format(V, att_switch, name, level )
%function xstr = xml_format(V, [att_switch], [name], [level] )
%
% Formats the variable V into a name-based tag XML string xstr.
%
% INPUT 
%   V        Matlab variable or structure.
%            The data types we can deal with are:
%              char, numeric, complex, struct, sparse, cell, logical/boolean
%            Not handled are data types:
%              function_handle, single, intxx, uintxx, java objects
%
%   att_switch   optional, 'on'- writes attributes, 'off'- writes "plain" XML
%   name         optional, give root element a specific name, eg. 'books'
%   level        internal, increases tab padding at beginning of xstr
%
% OUTPUT 
%   xstr         string, containing XML description of variable V
%
% SEE ALSO 
%   xml_help, xml_parse, xml_load, xml_save, (xmlread, xmlwrite)
%
% --------------------------------------------------------------
% Copyright Geodise/GEM 2002-2003, University of Southampton, UK
% Author: Marc Molinari <m.molinari@soton.ac.uk>
% $Revision: 297 $ $Date: 2003-12-03 01:40:18 +0800 (三, 2003-12-03) $

% Please read the licence in file licence.txt.

% ----------------------------------------------------------
% Initialisation and checks on input parameters

% set XML TB version number
xml_tb_version = '2.0';

% we need function name for subsequent recursive calls
thisfunc = mfilename;

% check input parameters
if (nargin<1)
  error([mfilename, ' needs at least 1 parameter.']);
end
if ((nargin<2) | ~strcmp(att_switch, 'off')), att_switch = 'on'; end
if ((nargin<3) | isempty(name)),  name = 'root'; end
if ((nargin<4) | isempty(level)), level = 0; end

% ----------------
% string definitions
xstr = '';
padd = blanks(2*(level+1)); % indentation
NL = sprintf('\n'); % newline
attributes = '';

% determine variable properties
att.name = name;
att.type = typeclass(V);
att.size = deblank(sprintf('%d ', size(V)));
att.idx  = 1;

% add entry tag for level=0
if level==0
  if strcmpi(att_switch, 'on')
    attributes = [' xml_tb_version="',xml_tb_version,'" ', ...
        'idx="', num2str(att.idx), '" ', ...
        'type="', att.type, '" ', ...
        'size="', num2str(att.size),'"'];
  end
  xstr = [xstr, '<', name, attributes];
end

if isempty(V)
  xstr = [xstr, '/> ', NL];
  return
end

% ------------------
switch lower(att.type)
  
  case {'double', 'boolean'}
    content = sprintf('%0.16g ', V(:));
    xstr = [xstr, '>', content(1:end-1), '</', name, '>', NL];
    
  case {'char', 'string'}
    % substitute functional characters &<> etc. with their ascii equivalent
    content = charsubst(V(:)');
    xstr = [xstr, '>', content, '</', name, '>', NL];
    
  case 'struct'
    xstr = [xstr, '>', NL];

    N = fieldnames(V);    
    for cV = 1:prod(size(V))
      for n = 1:length(N)
        % get content
        child.content = getfield( V(cV), N{n} );
        child.attributes = '';
        % write attributes
        if strcmpi(att_switch, 'on')
          child.att.idx = cV;
          child.att.type = typeclass(child.content);
          child.att.size = deblank(sprintf('%d ', size(child.content)));
          child.attributes = [' idx="', num2str(child.att.idx), '" ', ...
              'type="', child.att.type, '" ', ...
              'size="', num2str(child.att.size),'"'];
        end
        % write header
        xstr = [xstr, padd, '<', N{n}, child.attributes];
        % write content
        str = feval(thisfunc, child.content, att_switch, N{n}, level+1);
        xstr = [xstr, sprintf('%s', str)];
      end
    end
    xstr = [xstr, padd(1:end-2), '</', name, '>', NL];    
    
  case 'cell'
    xstr = [xstr, '>', NL];
    for n = 1:length(V);
      child.content = V{n};
      % write header
      xstr = [xstr, padd, '<item'];
      if strcmpi(att_switch, 'on')
        child.att.idx = n;
        child.att.type = typeclass(child.content);
        child.att.size = deblank(sprintf('%d ', size(child.content)));
        child.attributes = [' idx="', num2str(child.att.idx), '" ', ...
            'type="', child.att.type, '" ', ...
            'size="', num2str(child.att.size),'"'];
        xstr = [xstr, child.attributes];  
      end
      % write content
      xstr = [xstr, feval(thisfunc, child.content, att_switch, 'item', level+1)];
    end
    xstr = [xstr, padd(1:end-2), '</', name, '>', NL];      
    
  case 'sparse'
    % save three arrays: indices i, indices j, entries (i,j) as cell arrays
    xstr = [xstr, '> ', NL];
    [i,j,k] = find(V);
    if prod(size(i)) > 0
      L = sprintf('%d ', size(i)); % = size(j) = size(k)

      xstr = [xstr, padd, '<item'];
      if strcmp(att_switch, 'on')
        xstr = [xstr, sprintf(' type="double" idx="1" size="%s"', L(1:end-1))];
      end
      xstr = [xstr, feval(thisfunc, i, att_switch, 'item', level+1)];       
      
      xstr = [xstr, padd, '<item'];
      if strcmp(att_switch, 'on')
        xstr = [xstr, sprintf(' type="double" idx="2" size="%s"', L(1:end-1))];
      end
      xstr = [xstr, feval(thisfunc, j, att_switch, 'item', level+1)];       
      
      xstr = [xstr, padd, '<item'];
      if strcmp(att_switch, 'on')
        xstr = [xstr, sprintf(' type="%s" idx="3" size="%s"', typeclass(k), L(1:end-1))];
      end
      xstr = [xstr, feval(thisfunc, k, att_switch, 'item', level+1)];              
    end
    xstr = [xstr, padd(1:end-2), '</', name, '>', NL];      
    
  case 'complex'
    % save two arrays: real and imag as cell arrays
    xstr = [xstr, '> ', NL];
    R = real(V);
    I = imag(V);
    if prod(size(R)) > 0
      L = sprintf('%d ', size(R)); % = size(I)
      
      xstr = [xstr, padd, '<item'];
      if strcmp(att_switch, 'on')
        xstr = [xstr, sprintf(' type="double" idx="1" size="%s"', L(1:end-1))];
      end
      xstr = [xstr, feval(thisfunc, R, att_switch, 'item', level+1)];
      
      xstr = [xstr, padd, '<item'];
      if strcmp(att_switch, 'on')
        xstr = [xstr, sprintf(' type="double" idx="2" size="%s"', L(1:end-1))];
      end
      xstr = [xstr, feval(thisfunc, I, att_switch, 'item', level+1)];
    end
    xstr = [xstr, padd(1:end-2), '</', name, '>', NL];
    
  otherwise
    disp(['Current Type: ', att.type]);
    error(['Use only implemented types: double char struct cell complex' ...
        ' sparse boolean.']);
    
end

return

% ==========================================================
function C = typeclass(V)
% handled classes are
%  char, numeric, complex, struct, sparse, cell, logical
% not handled yet are:
%  function_handle, single, intxx, uintxx, java objects

C = [''];

if ischar(V)
  C = 'char';   % char
  return
end

if isstruct(V)
  C = 'struct'; % struct
  return
end

if iscell(V)
  C = 'cell';   % cell
  return
end

if isnumeric(V)
  if issparse(V)
    C = 'sparse';   % sparse ...
    return
  end
  
  if isreal(V)
    C = 'double';   % double
    return
  else
    C = 'complex';  % complex
    return
  end
end

if islogical(V)     % logical / boolean
  C = 'boolean';
  return
end


% ==========================================================
function V = charsubst(V)
% substitute functional characters &<> etc. with their ascii equivalent

f = findstr(V, '&');  % &&&&&&&&&&
for i=1:length(f)
  V = [V(1:f(i)-1), '&amp;', V(f(i)+1:end)];
  f = f+4;
end
f = findstr(V, '<');  % <<<<<<<<<<
for i=1:length(f)
  V = [V(1:f(i)-1), '&lt;', V(f(i)+1:end)];
  f = f+3;
end
f = findstr(V, '>');  % >>>>>>>>>>
for i=1:length(f)
  V = [V(1:f(i)-1), '&gt;', V(f(i)+1:end)];
  f = f+3;
end
f = findstr(V, '''');  % ''''''''''
for i=1:length(f)
  V = [V(1:f(i)-1), '&apos;', V(f(i)+1:end)];
  f = f+5;
end
f = findstr(V, '"');  % " " " " " "
for i=1:length(f)
  V = [V(1:f(i)-1), '&quot;', V(f(i)+1:end)];
  f = f+5;
end
