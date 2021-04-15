% Where results files are
path=('/esdata/beo1data/e187/CVS_code/results/');

% Number of tracers in ts array ie ts(l,i,j,k): max l index
tracers=2;

% Set up standard GENIE grid parameters
% Longitude
glon = -260:10:100;
glon1 = -180:10:180;

% Latitude
glat = -1:(2/36):1;
glat = asin(glat);
glat = (glat / pi) * 180;

% load continental overlay
load ents_mask;
