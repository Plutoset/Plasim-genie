function disp_exception (errmsg)

%   disp_exception is a utility method used for displaying shorter java
%   exception messages.
%
%   disp_exception (errormessage) takes in the message displayed when a
%   Java exception is thrown in Matlab and strips out some text that is
%   unnecessary for a user of the Geodise toolkit to see. The following
%   text is removed:
%       'Java exception occured:'
%       Which class of exception it is.
%       The stack trace.
%   This should leave only the first line of the error message.
%   
%   The function can be customised to only alter error messages of 
%   Exception classes from certain packages, and to display certain 
%   Exceptions as warnings instead of errors. 
%
%Copyright 2003 Geodise Project, University of Southampton
%Jasmin Wason 13/03/03
%Geodise database toolbox for Matlab version 0.4.1

% If the Exception class matches one in warnlist, display a warning, not an
% error.
warnlist = {'org.geodise.database.NonExistingException'};
% Error messages are only formated if the Exception class is in one of
% these packages.
packages = {'org.geodise.database'};

% If convertmessage is set to false disp_exception just displays the
% original error message.
convertmessage = true;
% If showclassname is set to true, the name of the Exception class will
% also be displayed.
showclassname = false;

% Check if this error message contains one of the strings in packages, 
% i.e. see if this Exception class comes from one of those packages.
foundpackage = false;
for i = 1:length(packages)
    if strfind (errmsg, packages{i})
        foundpackage = true;
        break;
    end
end

% Check this is a Java Exception, not an ordinary Matlab error.
isjava = length(strfind (errmsg, 'Java exception occurred:'));

% Only do all this parsing if the Exception is in one of the specified
% packages
if  foundpackage && convertmessage && isjava
    % Remove first part of the message which is always present.
    [first,last]=regexp(errmsg, 'Java exception occurred:');
    errmsg = errmsg(last+3:length (errmsg));
    
    % Remove second part of message which is the classname.
    [first,last]=regexp(errmsg, 'Exception:');
    if ~isempty(first)
        classname = errmsg(1:last-1);  
        errmsg = errmsg(last+2:length (errmsg));
    else
        classname = '';
    end
    % Remove the stack trace which usually starts with the next line
    [first,last]=regexp(errmsg, '\n');
    errmsg = errmsg(1:first-1);
% if the Exception is not in a specified package, just throw an error
else 
    error(errmsg);
end

if showclassname
    errmsg = [classname, ': ', errmsg];
end

% if the class is in warnlist display a warning
if strmatch (classname, warnlist, 'exact')
    warning(errmsg);
% otherwise throw an error
else
    error(errmsg);
end