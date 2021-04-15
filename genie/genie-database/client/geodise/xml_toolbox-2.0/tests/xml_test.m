% --------------------------------------------------------------
% Copyright Geodise/GEM 2002-2003, University of Southampton, UK
% Author: Marc Molinari <m.molinari@soton.ac.uk>
% $Revision: 297 $ $Date: 2003-12-03 01:40:18 +0800 (三, 2003-12-03) $

% Please read the licence in file licence.txt.

% perform testing of xml_parse command
func = 'xml_parse'

% =========================================================
% define test strings
T = [];
T{end+1} = '<root>aaa</root>';
T{end+1} = '<item>aaa</item>';
T{end+1} = '<root><item>bbb</item></root>';
T{end+1} = '<item><item>bbb</item></item>';
T{end+1} = '<root><item idx="1">a</item><item idx="2">b</item></root>';
T{end+1} = '<root><a>bbb</a></root>';
T{end+1} = '<root idx="4">hello</root>';
T{end+1} = '<item idx="4">hello</item>';
T{end+1} = '<root><ch idx="4">aaaa</ch></root>';
T{end+1} = '<item><ch idx="4">aaaa</ch></item>';
T{end+1} = '<item idx="3">aaa</item><item idx="1">bbb</item>';
% The following will cause an error as there are two
% root entries at root level:
T{end+1} = '<root idx="3">aaa</root><root idx="1">bbb</root>';
T{end+1} = '<root><item idx="2">bbb</item><item idx="1">ccc</item></root>';
T{end+1} = '<root><a idx="2">bbb</a><a idx="1">ccc</a></root>';
T{end+1} = '<root><a idx="2"><b>ccc</b></a><a idx="1">ccc</a></root>';
T{end+1} = '<item idx="1">a</item><item idx="2">b</item>';
T{end+1} = '<item idx="1">a</item><item idx="2">b</item><item idx="3">c</item>';
T{end+1} = '<item idx="1">a</item><item idx="2">b</item>';
T{end+1} = '<item idx="1">a</item><item idx="2">b</item><item idx="3">c</item>';
T{end+1} = '<root><item idx="1">a</item><item idx="2">b</item><item idx="3">c</item></root>';
T{end+1} = '<root><item idx="1">a</item><item idx="2">b</item></root>';
T{end+1} = '<root><a>bbb</a><a>ccc</a></root>';


% =========================================================
% test parse function
for t=1:length(T)
  s = T{t};
  try
    disp('==========================================================')
    disp(['Evaluating s = ', s])
    v_on = feval(func, s);
    disp('==========================================================')
    disp(['Evaluating s_off = ', s])
    v_off = feval(func, s, 'off');
  catch
    disp(lasterr)
  end
end
  