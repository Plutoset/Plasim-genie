% Matlab script used to plot carbon reservoirs and fluxes
% from simpleland.
clear all;

ents_constants;

disp('ENTS_CARBON: Plotting routine for carbon reservoirs and fluxes');
run_id=input('What is the run identifier?\n','s');

imax=36;
jmax=36;
kmax=8;
moltokg=12e-3;

filename=sprintf('%s%s.sland.avg',path,run_id);

plotname(1,:)=char('Photosynthesis (kgC/m^2/yr)  ');
plotname(2,:)=char('Veg. respiration (kgC/m^2/yr)');
plotname(3,:)=char('Leaf litter (kgC/m^2/yr)     ');
plotname(4,:)=char('Soil respiration (kgC/m^2/yr)');
plotname(5,:)=char('Vegetation carbon (kgC/m^2)  ');
plotname(6,:)=char('Soil carbon (kgC/m^2)        ');
plotname(7,:)=char('Vegetation fraction          ');
plotname(8,:)=char('Vegetation self shading frac ');
plotname(9,:)=char('Land carbon flux (mol/m^2/yr)');

option=([1:9]);
for i=1:9;
    x=sprintf('%d...%s',option(i),plotname(i,:));
    disp(x);
end;

column=input('What column number would you like to plot?\n');

x=load(filename);

% Code for timestamp
timestamp=x(end);
x=x(1:end-1);

landC1=reshape(x,imax,jmax,8);

% move matrix into normal format
landC(:,1:28,:)=landC1(:,9:36,:);
landC(:,29:36,:)=landC1(:,1:8,:);


for i=1:imax;
    for j=1:jmax;
        if landC(i,j,:)<-10;
            landC(i,j,:)=NaN;
        end;
     end;
 end;

landC(imax+1,:,:)=NaN;
landC(:,jmax+1,:)=NaN;

for l=1:8;
    plotlandC(:,:,l)=landC(:,:,l);
end;

shorttitle=deblank(plotname(column,:));
%plottitle=sprintf('%s. Run id: %s',shorttitle,run_id);
plottitle=sprintf('%s. Run id: %s, year: %d',shorttitle,run_id,timestamp);

if column==9;
    figure;
    pcolor(glon1,glat,(-plotlandC(:,:,1)+plotlandC(:,:,2)+plotlandC(:,:,4))./0.012);
    shading flat;
%    therm4(31);
    colorbar('horiz');
    title(plottitle);
    hold on;
    plot(olon1,olat1,'k');
    set(gca, 'XTick', [-180 -90 0 90 180]);
    set(gca, 'YTick', [-90 -45 0 45 90]);
    xlabel ('Longitude [{\circ}E]');
    ylabel ('Latitude [{\circ}N]');
    axis image;
else;
    figure;
    pcolor(glon1,glat,plotlandC(:,:,column));
    shading flat;
%    rain(50);
    colorbar('horiz');
    title(plottitle);
    hold on;
    plot(olon1,olat1,'k');
    set(gca, 'XTick', [-180 -90 0 90 180]);
    set(gca, 'YTick', [-90 -45 0 45 90]);
    xlabel ('Longitude [{\circ}E]');
    ylabel ('Latitude [{\circ}N]');
    axis image;
end;

