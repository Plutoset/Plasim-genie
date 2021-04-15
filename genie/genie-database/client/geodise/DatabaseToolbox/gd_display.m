function gd_display(QResults)
%     function gd_display(QResults)
%
% This function can be used to display: 
%    (1) a cell of structures, e.g. the results of gd_query function
%    (2) a structure.
% 
% Example: 
%   >> R = gd_query('standard.userID=me')
%   >> % Display R which is a cell of structures:
%   >> gd_display(R)
% or
%   >> % Display the first result R{1} which is a structure:
%   >> gd_display(R{1})
%
% Copyright 2002-2003 Geodise Project, University of Southampton,
% Zhuoan Jiao (z.jiao@soton.ac.uk), last modified on 25/04/03.
% Geodise database toolbox for Matlab version 0.5.0


if ~isempty(QResults)
    if iscell(QResults)
        [row, col] = size(QResults);
        Total=['(Total structures: ',num2str(col), ') ***'];
        for i = 1:col-1
            disp(' ');
            disp(['*** Content of structure ', inputname(1), '{',num2str(i), '} ', Total]);
            disp_struct('', QResults{i})
            reply=input('Press ENTER to continue ..., q to quit: ', 's');
            if strcmpi(reply, 'q')
                return
            end
        end
        
        disp( ' ');
        disp(['*** Content of the structure ', inputname(1), '{',num2str(col), '} ', Total]);
        disp_struct('', QResults{col});
        disp('*** No more results. ***');       
    elseif isstruct(QResults)
        disp(['*** Content of the structure ', inputname(1), ' ***']);
        disp_struct('', QResults)
    else
        disp(['*** Cannot display "',inputname(1), '" ***']);
    end
end
