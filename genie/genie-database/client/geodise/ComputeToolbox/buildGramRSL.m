function RSL = buildGramRSL(RSLstruct)
%buildGramRSL aids the construction of RSL GRAM job submission strings
%   This function accepts a structure as the input in which each field
%   corresponds to a valid GRAM attribute and its value the value of that
%   attribute, it returns a valid RSL string. The only attibute required
%   for a GRAM job submission is the 'executable' attribute. 
%
%   Values may either be strings or numerical values, however the GRAM
%   attributes 'arguments' and 'environment' which may have one or more
%   values must be cell arrays with elements that are either strings or
%   numerical values. Note that the rsl_substitution attribute is not
%   supported.
%
%   Recognised GRAM attributes:
%       arguments, count, directory, dryRun, environment, executable,
%       gramMyJob, hostCount, jobType, maxCpuTime, maxMemory, maxTime,
%       maxWallTime, minMemory, project, queue, remote_io_url, restart,
%       save_state, stderr, stderr_position, stdin, stdout,
%       stdout_position, two_phase     
%
%   For more information about these GRAM job submission attributes see: 
%   http://www.globus.org/gram/gram_rsl_parameters.html 
%
%   RSL = buildGramRSL(RSLstruct)
%           where RSLstruct is a structure in which the fields correspond
%           to GRAM attributes. RSL is the valid RSL string.
%
%   Example:
%       RSLstruct.executable = '/bin/date'
%       RSLstruct.arguments = {'-R'}
%       RSLstruct.environment = {'fee','fi','fo','fum'}
%       RSLstruct.stdout = 'test.out'
%       RSLstruct.stderr = 'test.err'
%
%       buildGramRSL(RSLstruct)
%
%       ans = 
%       &( executable = "/bin/date" )( count = "1" )( arguments = "-R" )...
%       ( stderr = "test.err" )( stdout = "test.out" )...
%       ( environment = ( "fee" "fi" ) ( "fo" "fum" ) )
%
%   See also: gd_jobsubmit

%   Copyright 2003 Geodise Project, University of Southampton
%   Graeme Pound 16/7/03
%   Geodise computational toolbox for Matlab

error(nargchk(1,1,nargin));

if ~isstruct(RSLstruct)
    error('Argument RSLstruct must be type struct')
end

%Check for required GRAM attributes
if ~isfield(RSLstruct, 'executable')
    error('Argument RSLstruct must have field ''executable''')
end

%Apply the defaults
if ~isfield(RSLstruct, 'count')
    RSLstruct = setfield(RSLstruct,'count',1);
end

rslatt = org.globus.rsl.RslAttributes;
structfields = fieldnames(RSLstruct);

for ii = 1: length(structfields)    
    switch structfields{ii}        
        case 'executable'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'count'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'directory'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))            
        case 'arguments'
            %Value must be a cell array containing one or more value
            addrmultisltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))            
        case 'stdin'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))            
        case 'stdout'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'stderr'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'count'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'environment'
            %Value must be a cell array containing one or more value
            addrsltattpairs(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'maxTime'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'maxWallTime'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'maxCpuTime'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'jobType'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'gramMyJob'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'queue'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'project'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'hostCount'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'dryRun'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'minMemory'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'maxMemory'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'save_state'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'two_phase'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'restart'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'stdout_position'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'stderr_position'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))
        case 'remote_io_url'
            addrsltatt(rslatt,structfields{ii},getfield(RSLstruct,structfields{ii}))                                 
        otherwise
            error(['Field ',structfields{ii},' is not a recognised GRAM attribute'])   
    end
end

RSL = char(rslatt.toRSL);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function addrsltatt(rslattribs, attrib, value)
%Check the type of the attribute
if ~ischar(attrib)
    error('The attribute must be of type char')
end    

%Check the type of the value
if ~ischar(value)
    if isnumeric(value)
        charval = num2str(value);
    else
        error(['Value of attribute ''',attrib,''' cannot be of type ',class(value)])                      
    end       
else
    charval = value;
end
%Add the attribute to the RSL attibutes
rslattribs.add(attrib,charval);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function addrmultisltatt(rslattribs, attrib, value)
%Check the type of the attribute
if ~ischar(attrib)
    error('The attribute must be of type char')
end    

%Check the type of the value
if ~iscell(value)
    error(['Value of attribute ''',attrib,''' must be a cell array'])
end

stringarray = javaArray('java.lang.String',length(value));
for ii = 1:length(value)
    if ~ischar(value{ii})
        if isnumeric(value{ii})
            rslattribs.add(attrib,num2str(value{ii}));
%            stringarray(ii)= java.lang.String(num2str(value{ii})); 
        else
            error(['Values of attribute ''',attrib,''' cannot be of type ',class(value{ii})])                      
        end
    else
        rslattribs.add(attrib,value{ii});
%        stringarray(ii)= java.lang.String(value{ii});         
    end     
end

%Add the attribute to the RSL attibutes
%rslattribs.addMulti(attrib,stringarray);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function addrsltattpairs(rslattribs, attrib, value)
%Check the type of the attribute
if ~ischar(attrib)
    error('The attribute must be of type char')
end    

%Check the type of the value
if ~iscell(value)
    error(['Value of attribute ''',attrib,''' must be a cell array'])
end

%Check the length of the value
if mod(length(value),2)
    error(['An even number of elements are required for attribute ''',attrib,''''])
end

stringarray = javaArray('java.lang.String',2);
for ii = 1:2:length(value)
    if ~ischar(value{ii})
        if isnumeric(value{ii})
            stringarray(1)= java.lang.String(num2str(value{ii})); 
        else
            error(['Values of attribute ''',attrib,''' cannot be of type ',class(value{ii})])                      
        end
    else
        stringarray(1)= java.lang.String(value{ii});         
    end
    
    if ~ischar(value{ii+1})
        if isnumeric(value{ii+1})
            stringarray(2)= java.lang.String(num2str(value{ii+1})); 
        else
            error(['Values of attribute ''',attrib,''' cannot be of type ',class(value{ii+1})])
        end
    else
        stringarray(2)= java.lang.String(value{ii+1});         
    end
    
    %Add the attribute to the RSL attibutes
    rslattribs.addMulti(attrib,stringarray);    
end


