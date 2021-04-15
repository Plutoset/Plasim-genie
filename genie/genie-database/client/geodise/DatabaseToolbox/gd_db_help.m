function gd_db_help
% -----------------------------------
% GEODISE DATABASE TOOLBOX FOR MATLAB 
% -----------------------------------
%
% FUNCTIONS:
%  gd_archive       stores a file or struct with metadata in an archive
%  gd_retrieve      retrieves a file or struct from an archive
%  gd_query         queries metadata or structs in an archive
%  gd_display       displays the results from a query 
%  gd_datagroup     creates a datagroup representing a set of archived data
%  gd_datagroupadd  add a file or struct to a datagroup
%  db_test          tests the database toolbox by using the above functions
% 
%  UTILITY FUNCTIONS:
%  disp_struct      used by gd_display to display a cell of structs
%  string_parse     used by gd_query to parse query strings
%  xml_convert      used to convert xml to and from a database ready format
%  disp_exception   used by all functions to display Java error messages
%
% FILES
%  install.txt      documentation on how to install the database toolbox
%  .geodise         directory containing files needed by the toolbox
%  gddatabase.jar   contains the Java code needed to run the toolbox
%  lib              directory containing jar files needed by gd_database.jar
%
% RELATED:
%  gd_createproxy, gd_proxyquery, gd_certinfo from the Geodise
%  Computational Toolbox for Matlab.
%  xml_format, xml_parse from the XML Toolbox for Matlab.
%
% Further information can be obtained by using the help command on
% a specific function, e.g. help gd_archive.
%
% ----------------------------------------------------------
% Geodise Database Toolbox for Matlab version 0.5, 1/5/2003, http://www.geodise.org
% Copyright 2003-2003 Geodise Project, University of Southampton, UK
% Authors: Jasmin Wason <j.l.wason@soton.ac.uk> and Zhuoan Jiao <z.jiao@soton.ac.uk>

help gd_db_help

