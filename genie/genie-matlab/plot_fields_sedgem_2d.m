function [] = plot_fields_sedgem_2d(PVAR,CSCALE,CMIN,CMAX,CN,PTIT)
% PLOT_FIELDS_SEDGEM_2D
%
%   *********************************************************
%   *** sedgem 2-D (LON-LAT) DATA PLOTTING                ***
%   *********************************************************
%
%   PLOT_FIELDS_SEDGEM_2D(PVAR,CSCALE,CMIN,CMAX,CN,PTIT)
%   plots a k-section through the BIOGEM 3-D netCDF data file;
%   'fields_biogem_3d.nc'
%
%   PLOT_FIELDS_SEDGEM_2D(PVAR,CSCALE,CMIN,CMAX,CN,PTIT)
%   takes 6 arguments;
%   PVAR .. id the name of the variable to be plotted
%           -> all valid valiable names will be listed if an invalid name
%              is entered
%           THIS PARAMETER MUST BE PASSED AS A STRING;
%           -> e.g., 'sed_CaCO3'
%   CSCALE  is the scale factor for the plot
%           -> e.g., to plot micro molar (umol kg-1), enter; 1e-6
%           -> the plot is auto-scaled if a value of zero (0.0) is entered
%   CMIN .. is the minimum contor value
%   CMAX .. is the maxumum contor value
%   CN .... is the number of intervals between minimum and maximum
%           plot values
%   PTIT ..is the string for the alternative plot title
%          -> if an empty (i.e., '') value is passed to this parameter
%             then a title is automaticaly generated   
%
%   EXAMPLE;
%           plot_fields_sedgem_2d('sed_CaCO3',1,0,100,20,'wt% CaCO_3')
%           will plot the carbonate content of surface sediments,
%           between 0 and 100 wt% in 20 contour intervals
%
%   Edit the 'm' file to change other user settings;
%           lon_min = -180;        % STARTING LONGITUDE FOR X-AXIS
%           delta_lon = 60;        % INCREMENT OF LONGITUDE ON X-AXIS
%           contour_plot = 'n';    % OVERLAY CONTOL PLOT?
%           contour_mod = 1;       % NUMBER OF COLOR INTERVALS PER CONTOR
%           contour_mod_label = 5; % NUMBER OF LABELED CONTOURS PER CONTOUR
%           contour_label = 'n';   % LABEL CONTOURS?
%           contour_noneg = 'n';   % RESTRICT DATA PLOTTED TO > 0.0?
%           plot_log10 = 'n';      % PLOT LOG10 OF THE DATA
%           dscrsz = 0.60;         % FRACTIONAL FIGURE WINDOW SIZE 
%

% \/\/\/ USER SETTINGS \/\/\/
lon_min = -180;        % STARTING LONGITUDE FOR X-AXIS
delta_lon = 60;        % INCREMENT OF LONGITUDE ON X-AXIS
contour_plot = 'n';    % OVERLAY CONTOL PLOT?
contour_mod = 1;       % NUMBER OF COLOR INTERVALS PER CONTOR
contour_mod_label = 5; % NUMBER OF LABELED CONTOURS PER CONTOUR
contour_label = 'n';   % LABEL CONTOURS?
contour_noneg = 'n';   % RESTRICT DATA PLOTTED TO > 0.0?
plot_log10 = 'n';      % PLOT LOG10 OF THE DATA
dscrsz = 0.60;         % FRACTIONAL FIGURE WINDOW SIZE 
% /\/\/\ USER SETTINGS /\/\/\

% INITIALIZE
lat_min = -090;
lat_max = +090;
lon_max = lon_min+360;
D_min   = 0000;
D_max   = 5000;
empty_string = '';
empty_value = [];
runid = 'sedgem';
dataid = PVAR;
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
nc=netcdf('fields_sedgem_2d.nc','nowrite');
% load in variable names
Varnames = ncload('fields_sedgem_2d.nc');

% SET UP GRID
% calculate grid dimensions
theVar = nc{'lon'};
imax = size(theVar(:,:),1);
theVar = nc{'lat'};
jmax = size(theVar(:,:),1);
%initialize dummy topography
topo = zeros(jmax,imax);
layb = zeros(jmax,imax);
% load and calculate remaining grid information
theVar = nc{'lat'};
grid_lat = theVar(1:jmax);
theVar = nc{'lon'};
grid_lon = theVar(1:imax);
[lonm latm] = meshgrid(grid_lon,grid_lat);
theVar = nc{'lat_edges'};
grid_lat_edges = theVar(1:imax+1);
theVar = nc{'lon_edges'};
grid_lon_edges = theVar(1:jmax+1);
[lonw lats] = meshgrid(grid_lon_edges(1:imax),grid_lat_edges(1:jmax));
[lone latn] = meshgrid(grid_lon_edges(2:imax+1),grid_lat_edges(2:jmax+1));
% Non-uniform grid
lat_max = sin(pi*lat_max/180.0);
lat_min = sin(pi*lat_min/180.0);
latn = sin(pi*latn/180.0);
lats = sin(pi*lats/180.0);

% LOAD DATA
% check that the variable name exists
theVar = [];
dim_theVar = size(Varnames);
while isempty(theVar)
    theVar = nc{dataid};
    if isempty(theVar)
        disp('   > WARNING: Variable name must be one of the following;');
        Varnames(20:end)
        dataid = input('   > Variable name: ','s');        
    end
end
rawdata = theVar(:); 
dim_theVar = size(theVar,1);
if dim_theVar == 1
    data(1:jmax,1:imax) = rawdata(1:jmax,1:imax);
else
    data(1:jmax,1:imax) = rawdata(ti,1:jmax,1:imax);   
end

%
xm = lonm;
ym = latm;
zm = data;
for i = 1:imax,
    for j = 1:jmax,
        if topo(j,i) > layb(j,i)
            zm(j,i) = NaN;
            topo(j,i) = +1.0;
            layb(j,i) = -1.0;
        elseif (zm(j,i) < -1.0E6) | (zm(j,i) > 1.0E30)
            zm(j,i) = NaN;
            topo(j,i) = +1.0;
            layb(j,i) = -1.0;
        else
            if plot_log10 == 'y'
                if (zm(j,i) > 0.0)
                    zm(j,i) = log10(zm(j,i)/data_scale);
                else
                    zm(j,i) = NaN;
                end
            else
                zm(j,i) = zm(j,i)/data_scale;
            end
            topo(j,i) = -1.0;
            layb(j,i) = +1.0;
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

% ADD-ON FOR DEFINABLE STARTING LATITUDE FOR X-AXIS
lon_start = min(min(lonw));
i_start = round((lon_min-(lon_start-360.0))/(360.0/jmax)) + 1;
xm_ex = [xm - 360.0 xm + 000.0 xm + 360.0];
ym_ex = [ym + 000.0 ym + 000.0 ym + 000.0];
zm_ex = [zm zm zm];
topo_ex = [topo - 360.0 topo + 000.0 topo + 360.0];
lonm_ex = [lonm - 360.0 lonm + 000.0 lonm + 360.0];
lone_ex = [lone - 360.0 lone + 000.0 lone + 360.0];
lonw_ex = [lonw - 360.0 lonw + 000.0 lonw + 360.0];
layb_ex = [layb - 360.0 layb + 000.0 layb + 360.0];
xm = xm_ex(:,i_start:i_start+imax-1);
ym = ym_ex(:,i_start:i_start+imax-1);
zm = zm_ex(:,i_start:i_start+imax-1);
topo = topo_ex(:,i_start:i_start+imax-1);
lonm = lonm_ex(:,i_start:i_start+imax-1);
lone = lone_ex(:,i_start:i_start+imax-1);
lonw = lonw_ex(:,i_start:i_start+imax-1);
layb = layb_ex(:,i_start:i_start+imax-1);


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
axis([lon_min lon_max lat_min lat_max]);
set(gca,'TickDir','out');
set(gca,'XLabel',text('String','Longitude','FontSize',15),'XTick',[lon_min:delta_lon:lon_max]);
set(gca,'YLabel',text('String','Latitude','FontSize',15),'YTick',[-1 -0.866 -0.5 0 0.5 0.866 1], 'YTickLabel',{'-90';'-60';'-30';'0';'30';'60';'90'});
if size(PTIT) ~= size(empty_string)
    title([PTIT],'FontSize',18);
else
    title(['Data ID: ',strrep(dataid,'_',' ')],'FontSize',12);
end

for i = 1:imax,
    for j = 1:jmax,
        if topo(j,i) > layb(j,i)
            h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],color_g);
            set(h,'EdgeColor',color_g);
        else
            if (isnan(zm(j,i)))
                h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],[1 1 1]);
                set(h,'EdgeColor',[1 1 1]);
            else
                col = round((1/2)+con_n*(zm(j,i)-con_min)/(con_max-con_min));
                if col < 1
                    col = 1;
                elseif col > con_n
                    col = 2*(con_n+1)+1;
                else
                    col = 2*col+1;
                end
                h = patch([lonw(j,i) lonw(j,i) lone(j,i) lone(j,i)],[lats(j,i) latn(j,i) latn(j,i) lats(j,i)],cmap(col,:));
                set(h,'EdgeColor',cmap(col,:));
            end
        end
    end
end

for j = 1:jmax,
    for i = 1:imax-1,
        if topo(j,i) > layb(j,i)
            if topo(j,i+1) <= layb(j,i+1)
                h = plot([lone(j,i) lone(j,i)],[lats(j,i) latn(j,i)],'k-');
                set(h,'LineWidth',1.0);
            end
        end
    end
    for i = 2:imax,
        if topo(j,i) > layb(j,i)
            if topo(j,i-1) <= layb(j,i-1)
                h = plot([lonw(j,i) lonw(j,i)],[lats(j,i) latn(j,i)],'k-');
                set(h,'LineWidth',1.0);
            end
        end
    end
end
for i = 1:imax,
    for j = 1:jmax-1,
        if topo(j,i) > layb(j,i)
            if topo(j+1,i) <= layb(j+1,i)
                h = plot([lonw(j,i) lone(j,i)],[latn(j,i) latn(j,i)],'k-');
                set(h,'LineWidth',1.0);
            end
        end
    end
    for j = 2:jmax,
        if topo(j,i) > layb(j,i)
            if topo(j-1,i) <= layb(j-1,i)
                h = plot([lonw(j,i) lone(j,i)],[lats(j,i) lats(j,i)],'k-');
                set(h,'LineWidth',1.0);
            end
        end
    end
end

if contour_plot == 'y'
    v = [con_min:(con_max-con_min)/(con_n/contour_mod):con_max];
    [C,h] = contour(xm_ex,sin(pi*ym_ex/180.0),zm_ex,v,'k-');
    set(h,'LineWidth',0.25);
    v = [con_min:(con_max-con_min)/(con_n/contour_mod_label):con_max];
    [C,h] = contour(xm_ex,sin(pi*ym_ex/180.0),zm_ex,v,'k');
    set(h,'LineWidth',1.0);
    if plot_log10 == 'y'
        %%%%%%%%
    elseif contour_label == 'y'  
        clabel(C,h);
    end
end

h = plot([lon_min lon_max],[lat_min lat_min],'k-');
set(h,'LineWidth',1.0);
h = plot([lon_min lon_max],[lat_max lat_max],'k-');
set(h,'LineWidth',1.0);
h = plot([lon_min lon_min],[lat_min lat_max],'k-');
set(h,'LineWidth',1.0);
h = plot([lon_max lon_max],[lat_min lat_max],'k-');
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

filename = [runid, '_', num2str(year), '_', dataid, '.ps'];
print('-dpsc2', filename);

nc = close(nc);