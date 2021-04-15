function [] = plot_fields_biogem_3d_i_AVE_MOVIE(PVAR,MASK,CSCALE,CMIN,CMAX,CN)
% PLOT_FIELDS_BIOGEM_3D_I_MOVIE
%
%   *********************************************************
%   *** biogem i-SECTION (LAT-LAY) MOVIE                  ***
%   *********************************************************
%
%   PLOT_FIELDS_BIOGEM_3D_I_AVE_MOVIE(PVAR,MASK,CSCALE,CMIN,CMAX,CN)
%   plots a series of i-sections through the BIOGEM 3-D netCDF data file;
%   'fields_biogem_3d.nc'
%   and turns them into an animation
%
%   PLOT_FIELDS_BIOGEM_3D_I_AVE_MOVIE(PVAR,PI,CSCALE,CMIN,CMAX,CN)
%   takes 6 arguments;
%   PVAR ... id the name of the variable to be plotted
%            -> all valid valiable names will be listed if an invalid name
%               is entered
%            THIS PARAMETER MUST BE PASSED AS A STRING;
%            -> e.g., 'ocn_PO4'
%   MASK .. is the filename containing the meridional mask
%           to be used to construct the zonal average
%           -> the full filename must eb give, including any extensions
%   CSCALE . is the scale factor for the plot
%            -> e.g., to plot micro molar (umol kg-1), enter; 1e-6
%            -> the plot is auto-scaled if a value of zero (0.0) is entered
%   CMIN ... is the minimum contor value
%   CMAX ... is the maxumum contor value
%   CN ..... is the number of intervals between minimum and maximum
%            plot values
%
%   PLOT_FIELDS_BIOGEM_3D_I_AVE_MOVIE(PVAR,MASK,CSCALE,CMIN,CMAX,CN)
%   will ask you to provide the following information;
%   Start year ............. start year
%   End year ............... end year
%   Time-slice increment ... the time (years) between time-slices

% \/\/\/ USER SETTINGS \/\/\/
% load additional data
CO2 = load('biogem_series_atm_pCO2.res','-ascii');
% /\/\/\ USER SETTINGS /\/\/\

timeslicemin  = input('   > Start year: ');
timeslicemax  = input('   > End year: ');
dtimeslice    = input('   > Time-slice increment: ');
ptit='';
% *** START LOOP ***
for timeslice = timeslicemin:dtimeslice:timeslicemax,
    timesliceid = num2str(timeslice);
    frame_n = (timeslicemax - timeslice)/dtimeslice;
    frame_nmax = (timeslicemax - timeslicemin)/dtimeslice;
    % \/\/\/ USER SETTINGS \/\/\/
    % set title string
    t = find(CO2(:,1) == timeslice);
    pCO2 = 1E6*CO2(t,3);
    ptit = ['Meridional salinity; ', 'year ',num2str(int32(timeslice - 0.001)), '; \it{p}\rmCO_{2} = ',num2str(int32(pCO2 - 0.001)),' ppm'];
    % /\/\/\ USER SETTINGS /\/\/\
    plot_fields_biogem_3d_i_AVE(timeslice,PVAR,MASK,CSCALE,CMIN,CMAX,CN,ptit);
    F(frame_nmax - frame_n + 1) = getframe;
    close
end
% *** END LOOP ***
filename = [ 'biogem_3d_i_MOVIE_', PVAR, '_', 'i', num2str(MASK),'.avi'];
%movie2avi(F,filename,'fps',3,'quality',100);
movie2avi(F,filename,'fps',3,'quality',100,'compression','None');
