% Matlab script used to plot two dimensional array outputs
% from simpleland.
clear all;
ents_constants;
 
disp('ENTS_TWODARRAY: Plotting routine for two dimensional arrays');
run_id=input('What is the run identifier?\n','s');

disp('Which two dimensional array do you wish to plot?');
disp('1...Water bucket fullness (m)');
disp('2...Land temperature (^oC)');
disp('3...Surface albedo');
disp('4...Planetary albedo');
disp('5...Run off (m/s)');
disp('6...Fractional snow cover');
disp('7...Precipitation (m/s)');
disp('8...Relative humidity (%)');
disp('9...Stomatal resistance (s/m)');
disp('10..Soil field capacity (m)');
disp('11..Surface roughness length (m)');
disp('12..Evaporation (m/s)');
disp(' ');
var_id=input('Which field do you wish to plot? (1-12)\n');

if var_id==1;
    filename=sprintf('%s%s.lqavg',path,run_id);
    plottitle=sprintf('Water bucket fullness (m): %s.lqavg',run_id);
end;
if var_id==2;
    filename=sprintf('%s%s.ltavg',path,run_id);
    plottitle=sprintf('Land temperature (^{o}C): %s.ltavg',run_id);
end;
if var_id==3;
    filename=sprintf('%s%s.albsavg',path,run_id);
    plottitle=sprintf('Surface albedo: %s.albsavg',run_id);
end;
if var_id==4;
    filename=sprintf('%s%s.palbavg',path,run_id);
    plottitle=sprintf('Planetary albedo: %s.palbavg',run_id);  
end;
if var_id==5;
    filename=sprintf('%s%s.runavg',path,run_id);
    plottitle=sprintf('Run off (m/s): %s.runavg',run_id);
end;
if var_id==6;
    filename=sprintf('%s%s.snowavg',path,run_id);
    plottitle=sprintf('Fractional snow cover: %s.snowavg',run_id);
end;
if var_id==7;
    filename=sprintf('%s%s.pptnavg',path,run_id);
    plottitle=sprintf('Precipitation (m/s): %s.pptnavg',run_id);
end;
if var_id==8;
    filename=sprintf('%s%s.relhavg',path,run_id);
    plottitle=sprintf('Relative humidity: %s.relhavg',run_id);
end;
if var_id==9;
    filename=sprintf('%s%s.rcslavg',path,run_id);
    plottitle=sprintf('Stomatal resistance (s/m): %s.rcslavg',run_id);
end;
if var_id==10;
    filename=sprintf('%s%s.bcapavg',path,run_id);
    plottitle=sprintf('Soil field capacity (m): %s.bcapavg',run_id);
end;
if var_id==11;
    filename=sprintf('%s%s.z0avg',path,run_id);
    plottitle=sprintf('Surface roughness length (m): %s.z0avg',run_id);
end;
if var_id==12;
    filename=sprintf('%s%s.evaplavg',path,run_id);
    plottitle=sprintf('Evaporation (m/s): %s.evaplavg',run_id);
end;


a=load(filename);
asze=size(a);
for i=1:asze(1);
    if a(i)==0;
         a(i)=NaN;
    end;
end;

a=a(1:asze(1));
plotarray1a=reshape(a,36,36);

% move matrix into normal format
plotarray1(1:28,:,:)=plotarray1a(9:36,:,:);
plotarray1(29:36,:,:)=plotarray1a(1:8,:,:);

plotarray1(37,:)=NaN;
plotarray1(:,37)=NaN;

plotarray1=plotarray1';

figure;
pcolor(glon1,glat,plotarray1);
shading flat;
hold on;
%rain(50);
plot(olon1,olat1,'k');
colorbar('horiz');
title(plottitle);
set(gca, 'XTick', [-180 -90 0 90 180]);
set(gca, 'YTick', [-90 -45 0 45 90]);
xlabel ('Longitude [{\circ}E]');
ylabel ('Latitude [{\circ}N]');
axis image;

   
