function disp_struct(structname, str)
%     function disp_struct(structname, str)
%
% This function is not meant to be used directly, but to be called recursively 
% (e.g. by gd_display()) to show the contents of a cell of structures, or the content 
% of a structure. However, if you really want to use it to display a structure, 
% the syntaxt is:
%      disp_struct('', your-structure)
% or
%      disp_struct(structname, your-structure)
% The second form will prefix the 'structname' in front of its file names.
%
% Example: 
%   >> m.standard.ID = '123';
%   >> m.standard.userID = 'me';
%   >> m.varname = 'xyz';
%
%   >> disp_struct('', m);
%     standard.ID: 123
%     standard.userID: me
%     varname: xyz
%
%   >> disp_struct('mystruc', m);
%     mystruc.standard.ID: 123
%     mystruc.standard.userID: me
%     mystruc.varname: xyz
%
% Copyright 2002-2003 Geodise Project, University of Southampton,
% Zhuoan Jiao (z.jiao@soton.ac.uk), last modified on 24/04/03.
% Geodise database toolbox for Matlab version 0.5.0

if ~isstruct(str)
    disp(['*** Sorry, not a structure, cannot be displayed. ***']);
    return
end

if ~isempty(str)
    [Row, Col] = size(str);
    for i = 1:Col
        fn=fieldnames(str(i));
        [fnum, x] = size(fn);
        for j = 1:fnum
            if isempty(structname)
                strpath = fn{j};
            else
                strpath = [structname, '.', fn{j}];
            end
            elm = str(i).(fn{j});
            if isstruct(elm)
                disp_struct(strpath, elm);
            else
                if isnumeric(elm)
                    if length(elm)==1
                        % a single number
                        disp(sprintf('  %s', [strpath, ': ', num2str(elm)]));
                    else
                        % may be a vectore or a metrix
                        disp(sprintf('  %s', [strpath, ':']));
                        disp(elm);
                    end
                elseif isstr(elm)
                    disp(sprintf('  %s', [strpath, ': ', elm]));
                else
                    disp(sprintf('  %s',[strpath, ':']));
                    disp(elm);
                end
            end
        end
    end
end
