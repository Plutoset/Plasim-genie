function [] = plot_fields_biogem_3d_i(PT,PVAR,PI,CSCALE,CMIN,CMAX,CN,PTIT)
% PLOT_FIELDS_BIOGEM_3D_I
%
%   *********************************************************
%   *** biogem i-SECTION (LAT-LAY) PLOTTING               ***
%   *********************************************************
%
%   PLOT_FIELDS_BIOGEM_3D_I(PT,PVAR,PI,CSCALE,CMIN,CMAX,CN,PTIT)
%   plots an i-section through the BIOGEM 3-D netCDF data file;
%   'fields_biogem_3d.nc'
%
%   PLOT_FIELDS_BIOGEM_3D_I(PT,PVAR,PI,CSCALE,CMIN,CMAX,CN,PTIT)
%   takes 8 arguments;
%   PT .....is the year of the time-slice
%           -> all valid time-slice years will be listed if an invalid
%              year is entered
%   PVAR ...id the name of the variable to be plotted
%           -> all valid valiable names will be listed if an invalid name
%              is entered
%           THIS PARAMETER MUST BE PASSED AS A STRING;
%           -> e.g., 'ocn_PO4'
%   PI .....is the meridional section to be plotted (the 'i' slice)
%   CSCALE .is the scale factor for the plot
%           -> e.g., to plot micro molar (umol kg-1), enter; 1e-6
%           -> the plot is auto-scaled if a value of zero (0.0) is entered
%   CMIN ...is the minimum contor value
%   CMAX ...is the maxumum contor value
%   CN .....is the number of intervals between minimum and maximum
%            plot values
%   PTIT .. is the string for the alternative plot title
%           -> if an empty (i.e., '') value is passed to this parameter
%              then a title is automaticaly generated  
%
%   EXAMPLE;
%           plot_fields_biogem_3d_i(1994.5,'ocn_PO4',21,1e-6,0,2,20'PO_4')
%           will plot the time-slice cenetered on a time of 1994.5,
%           of PO4 concentrations along the i=23 meridian,
%           between 0 and 2 umol kg-1 in 20 contour intervals
%
%   Edit the 'm' file to change other user settings;
%           contour_plot = 'n';    % OVERLAY CONTOL PLOT?
%           contour_mod = 1;       % NUMBER OF COLOR INTERVALS PER CONTOR
%           contour_mod_label = 5; % NUMBER OF LABELED CONTOURS PER CONTOUR
%           contour_label = 'n';   % LABEL CONTOURS?
%           contour_noneg = 'y';   % RESTRICT DATA PLOTTED TO > 0.0?
%           plot_log10 = 'n';      % PLOT LOG10 OF THE DATA
%           dscrsz = 0.60;         % FRACTIONAL FIGURE WINDOW SIZE 
%

% \/\/\/ USER SETTINGS \/\/\/
contour_plot = 'n';    % OVERLAY CONTOL PLOT?
contour_mod = 1;       % NUMBER OF COLOR INTERVALS PER CONTOR
contour_mod_label = 5; % NUMBER OF LABELED CONTOURS PER CONTOUR
contour_label = 'n';   % LABEL CONTOURS?
contour_noneg = 'n';   % RESTRICT DATA PLOTTED TO > 0.0?
plot_log10 = 'n';      % PLOT LOG10 OF THE DATA
dscrsz = 0.60;         % FRACTIONAL FIGURE WINDOW SIZE 
% /\/\/\ USER SETTINGS /\/\/\

% INITIALIZE PARAMETERS & VARIABLES
lat_min = -090;
lat_max = +090;
lon_min = -260;
lon_max = +100;
D_min   = 0000;
D_max   = 5000;
empty_string = '';
empty_value = [];
runid = 'biogem';
timesliceid = PT;
dataid = PVAR;
iplot = PI;
data_scale = CSCALE;
con_min = CMIN;
con_max = CMAX;
con_n = CN;

% VALIDATE INPUT DATA - DATA SCALING
if data_scale == 0.0
    data_scale = 1.0;
    clear con_min;
    clear con_max;
    con_n = 10;
end

%
xm = [];
ym = [];
zm = [];
lonm = [];
lone = [];
lonw = [];
latm = [];
latn = [];
lats = [];
laym = [];
layt = [];
layb = [];
rawdata=[];
data=[];
color_g = [0.75 0.75 0.75];
color_w = [1.00 1.00 1.00];
graph_title = '';
plotfileextension = '.ps';
% open netCDF file
nc=netcdf('fields_biogem_3d.nc','nowrite');
% load in variable names
Varnames = ncload('fields_biogem_3d.nc');

% SET UP GRID
% load level data
theVar = nc{'zt'};
grid_zt = flipud(theVar(:));
theVar = nc{'phys_level'};
grid_k1(:,:) = theVar(:,:);
% calculate grid dimensions
config = size(grid_k1);
imax = config(2);
jmax = config(1);
config = size(grid_zt);
kmax = config(1);
% validate data input
if iplot > imax
    iplot = imax;
elseif iplot < 1
    iplot = imax;
end
% calculate topography
theVar = nc{'zt_edges'};
grid_zt_edges(1:kmax+1) = flipud(theVar(1:kmax+1));
grid_zt_edges(kmax+1) = 0.0;
for i = 1:imax,
    for j = 1:jmax,
        if grid_k1(j,i) <= kmax
            topo(j,i) = -grid_zt_edges(grid_k1(j,i));
        else
            topo(j,i) = 0.0;
        end
    end
end
% load and calculate remaining grid information
theVar = nc{'lat'};
grid_lat = theVar(1:jmax);
theVar = nc{'lon'};
grid_lon = theVar(1:imax);
[latm laym] = meshgrid(grid_lat,-grid_zt);
theVar = nc{'lat_edges'};
grid_lat_edges = theVar(1:imax+1);
theVar = nc{'lon_edges'};
grid_lon_edges = theVar(1:jmax+1);
[lats layb] = meshgrid(grid_lat_edges(1:jmax),-grid_zt_edges(1:kmax));
[latn layt] = meshgrid(grid_lat_edges(2:jmax+1),-grid_zt_edges(2:kmax+1));

% LOAD DATA
% check that the year exists
theVar = nc{'time'};
dim_theVar = size(theVar,1);
clear time;
while exist('time','var') == 0
    for t = 1:dim_theVar,
        if double(int32(100*theVar(t)))/100 == timesliceid
           time = timesliceid;
           ti = t;
        end
    end
    if exist('time','var') == 0
        disp('   > WARNING: Year must be one of the following;');
        format long g;
        double(int32(100*theVar(:)))/100
        format;
        timesliceid = input('   > Time-slice year: ');
    end
end
% check that the variable name exists
theVar = [];
dim_theVar = size(Varnames);
while isempty(theVar)
    theVar = nc{dataid};
    if isempty(theVar)
        disp('   > WARNING: Variable name must be one of the following;');
        Varnames(23:end)
        dataid = input('   > Variable name: ','s');        
    end
end
rawdata = theVar(:);
if ndims(theVar(:)) > 2 
    dim_theVar = size(theVar,1);
    if dim_theVar == 1  
        rawdata = flipdim(rawdata,1);
        data(1:kmax,1:jmax) = rawdata(1:kmax,1:jmax,iplot);
    else
        rawdata = flipdim(rawdata,2);
        data(1:kmax,1:jmax) = rawdata(ti,1:kmax,1:jmax,iplot);   
    end
else
    data(1:jmax,1:imax) = rawdata(1:jmax,1:imax);
end

%
xm = latm;
ym = laym;
zm = data;
for j = jmax:-1:1,
  for k = 1:kmax,
    if topo(j,iplot) > ym(k,j)
      zm(k,j) = NaN;
    elseif (zm(k,j)) < -0.9E6
      zm(k,j) = NaN;
    else
      if plot_log10 == 'y'
        if (zm(k,j) > 0.0)
	      zm(k,j) = log10(zm(k,j)/data_scale);
	    else
	      zm(k,j) = NaN;
	    end
      else
        zm(k,j) = zm(k,j)/data_scale;
      end
      if contour_noneg == 'y'
        if (zm(k,j) < 0.0)
            zm(k,j) = 0.0;
        end
      end
    end
  end
end

% VALIDATE INPUT DATA - DATA SCALING
if exist('con_min','var') == 0
    con_min = min(min(zm));
end
if exist('con_max','var') == 0
    con_max = max(max(zm));
end
if con_min == con_max
    if con_max == 0.0
        con_max = 1.0;
    else
        con_min = (1.00/1.01)*con_min;
        con_max = (1.01)*con_max;
    end
end
if con_min > con_max
  con_min_TEMP = con_min;
  con_max_TEMP = con_max;
  con_min = con_max_TEMP;
  con_max = con_min_TEMP;
end

% PLOT FIGURE

scrsz = get(0,'ScreenSize');
figure('Position',[((1 - dscrsz)/2)*dscrsz*scrsz(3) (1 - dscrsz)*dscrsz*scrsz(4) dscrsz*scrsz(3) 0.60*dscrsz*scrsz(4)])
clf;

fh(1) = axes('Position',[0 0 1 1],'Visible','off');
fh(2) = axes('Position',[0.10 0.05 0.65 0.90]);
fh(3) = axes('Position',[0.80 0.15 0.20 0.70],'Visible','off');

cmap = colormap(jet((2*(con_n+1))+1));
%cmap = colormap(hot((2*(con_n+1))+1));

set(gcf,'CurrentAxes',fh(2));

hold on;

caxis([con_min-(con_max-con_min)/con_n con_max]);
set(gca,'PlotBoxAspectRatio',[1.0 0.5 1.0]);
axis([lat_min lat_max -D_max -D_min]);
set(gca,'TickDir','out');
set(gca,'XLabel',text('String','Latitude','FontSize',15),'XTick',[-90 -60 -30 0 30 60 90]);
set(gca,'YLabel',text('String','Depth (km)','FontSize',15),'YTick',[-D_max:1000:-D_min],'YTickLabel',{'5';'4';'3';'2';'1';'0'});
if size(PTIT) ~= size(empty_string)
    title([PTIT],'FontSize',18);
else
    title(['Year: ',strrep(num2str(time),'_',' '),' / ','Data ID: ',strrep(dataid,'_',' '),' / i = ', num2str(iplot)],'FontSize',12);
end

for j = jmax:-1:1,
  for k = 1:kmax,
    if topo(j,iplot) > ym(k,j)
      h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],color_g);
      set(h,'EdgeColor',color_g);
    else
      if (isnan(zm(k,j)))
        h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],[1 1 1]);
        set(h,'EdgeColor',[1 1 1]);
      else
        col = round((1/2)+con_n*(zm(k,j)-con_min)/(con_max-con_min));
        if col < 1
          col = 1;
        elseif col > con_n
          col = 2*(con_n+1)+1;
        else
          col = 2*col+1;
        end
        h = patch([lats(k,j) lats(k,j) latn(k,j) latn(k,j)],[layb(k,j) layt(k,j) layt(k,j) layb(k,j)],cmap(col,:));
        set(h,'EdgeColor',cmap(col,:));
      end
    end
  end
end

for k = 1:kmax,
  for j = 1:jmax-1,
    if topo(j,iplot) > ym(k,j)
      if topo(j+1,iplot) <= ym(k,j+1)
        h = plot([latn(k,j) latn(k,j)],[layb(k,j) layt(k,j)],'k-');
        set(h,'LineWidth',1.0);
      end
    end
  end
  for j = 2:jmax,
    if topo(j,iplot) > ym(k,j)
      if topo(j-1,iplot) <= ym(k,j-1)
        h = plot([lats(k,j) lats(k,j)],[layb(k,j) layt(k,j)],'k-');
        set(h,'LineWidth',1.0);
      end
    end
  end
end
for j = 1:jmax,
  for k = 2: kmax,
    if topo(j,iplot) < ym(k,j)
      if topo(j,iplot) > ym(k-1,j)
        h = plot([lats(k,j) latn(k,j)],[layb(k,j) layb(k,j)],'k-');
        set(h,'LineWidth',1.0);
      end
    end
  end
end

if contour_plot == 'y'
  v = [con_min:(con_max-con_min)/(con_n/contour_mod):con_max];
  [C,h] = contour(xm,ym,zm,v,'k-');
  set(h,'LineWidth',0.25);
  v = [con_min:(con_max-con_min)/(con_n/contour_mod_label):con_max];
  [C,h] = contour(xm,ym,zm,v,'k');
  set(h,'LineWidth',1.0);
    if plot_log10 == 'y'
      %%%%%%%%
    elseif contour_label == 'y'  
        clabel(C,h);
    end
end

h = plot([lat_min lat_max],[-D_max -D_max],'k-');
set(h,'LineWidth',1.0);
h = plot([lat_min lat_max],[-D_min -D_min],'k-');
set(h,'LineWidth',1.0);
h = plot([lat_min lat_min],[-D_max -D_min],'k-');
set(h,'LineWidth',1.0);
h = plot([lat_max lat_max],[-D_max -D_min],'k-');
set(h,'LineWidth',1.0);

hold off;

set(gcf,'CurrentAxes',fh(3));

hold on;

set(gca,'XTick',[],'YTick',[]);
axis([0 1 0 con_n+2]);

c = 1;
h = fill([0.1 0.2 0.3],[c-0.1 c-0.9 c-0.1],cmap(2*c-1,:));
if plot_log10 == 'y'
  str = ['below ',num2str(10^(con_min + (c-1)*(con_max-con_min)/con_n))];
else
  str = ['below ',num2str(con_min + (c-1)*(con_max-con_min)/con_n)];
end
textsize = 2+round(80/con_n);
if textsize > 10,
  textsize = 10;
end
text(0.40,c-0.5,str,'FontName','Arial','FontSize',textsize);
set(h,'LineWidth',0.5);
set(h,'EdgeColor','k');
for c = 2:con_n+1,
  h = fill([0.1 0.1 0.3 0.3],[c-0.9 c-0.1 c-0.1 c-0.9],cmap(2*c-1,:));
  if plot_log10 == 'y'
    str = [num2str(10^(con_min + (c-2)*(con_max-con_min)/con_n)),' - ',num2str(10^(con_min + (c-1)*(con_max-con_min)/con_n))];
  else
    str = [num2str(con_min + (c-2)*(con_max-con_min)/con_n),' - ',num2str(con_min + (c-1)*(con_max-con_min)/con_n)];
  end
  textsize = 2+round(80/con_n);
  if textsize > 10,
    textsize = 10;
  end
  text(0.40,c-0.5,str,'FontName','Arial','FontSize',textsize);
  set(h,'LineWidth',0.5);
  set(h,'EdgeColor','k');
end
c = con_n+2;
h = fill([0.1 0.2 0.3],[c-0.9 c-0.1 c-0.9],cmap(2*c-1,:));
if plot_log10 == 'y'
  str = ['above ',num2str(10^(con_min + (c-2)*(con_max-con_min)/con_n))];
else
  str = ['above ',num2str(con_min + (c-2)*(con_max-con_min)/con_n)];
end
textsize = 2+round(80/con_n);
if textsize > 10,
  textsize = 10;
end
text(0.40,c-0.5,str,'FontName','Arial','FontSize',textsize);
set(h,'LineWidth',0.5);
set(h,'EdgeColor','k');

hold off;
set(gcf,'CurrentAxes',fh(1));

filename = [runid, '_', num2str(time), '_', dataid, '_', 'i', num2str(iplot), '.ps'];
print('-dpsc2', filename);

nc = close(nc);