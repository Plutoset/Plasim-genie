clear all;

ents_constants;

disp('ENTS_PAVGT: A script for plotting global annual average ');
disp('physical quantities.');
file=input('What is the file name? \n','s');

filename=sprintf('%s%s.pslavgt',path,file);

a=load(filename);

asze=size(a);

disp('1......Global annual average temperature on land');
disp('2......Global annual average water on land');
disp('3......Global annual average heat fluxes over land');
disp('4......Global annual average water fluxes over the land');
disp('5......Global annual average relative humidity over land');
disp('6......Global annual average bucket capacity over land');
disp('7......Global annual average surface albedo over land');
disp('8......Global annual average air temperature');
disp('9......Global annual average snow cover');
disp('10.....Global annual average roughness length over land');

disp(' ');
option=input('What would you like to plot? (1-10) \n');

figure;
if option==1;
    plot(a(:,1),a(:,2),'r');
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('land temp. (^{\circ}C)');
    plotitle=sprintf('Global annual average land temperature: %s.pslavgt',file);
    title(plotitle);
end;

if option==2;
    plot(a(:,1),a(:,3));
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('land water. (m)');
    plotitle=sprintf('Global annual average land water: %s.pslavgt',file);
    title(plotitle);
end;

if option==3;
    plot(a(:,1),a(:,4),'g');
    hold on;
    plot(a(:,1),a(:,5));
    plot(a(:,1),a(:,6),'k');
    plot(a(:,1),a(:,7),'r');
    plot(a(:,1),a(:,8)*1000*2.5e6,'c');
    grid on;
    legend('Heat flux into atm','Heat flux into land','Sensible heat','Net LW radiation','Latent heat from evap.',0);
    xlabel('year (relative to start of run)');
    ylabel('Heat flux (W/m^2)');
    plotitle=sprintf('Global annual average land heat fluxes: %s.pslavgt',file);
    title(plotitle);
end;

if option==4;
    plot(a(:,1),a(:,8));
    hold on;
    plot(a(:,1),a(:,9),'r');
    grid on;
    legend('Evaporation','Precipitation',0);
    xlabel('year (relative to start of run)');
    ylabel('Water flux (m/s)');
    plotitle=sprintf('Global annual average land water fluxes: %s.pslavgt',file);
    title(plotitle);
end;

if option==5;
    plot(a(:,1),a(:,10));
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Relative humidity');
    plotitle=sprintf('Global annual average relative humidity over land: %s.pslavgt',file);
    title(plotitle);
end;

if option==6;
    plot(a(:,1),a(:,11),'g');
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('field capacity (m)');
    plotitle=sprintf('Global annual average soil field capacity: %s.pslavgt',file);
    title(plotitle);
end;

if option==7;
    plot(a(:,1),a(:,12),'k');
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Surface albedo');
    plotitle=sprintf('Global annual average surface albedo over land: %s.pslavgt',file);
    title(plotitle);
end;

if option==8;
    clear a filename;
    filename=sprintf('%s%s.gmairt',path,file);

    a=load(filename);

    asze=size(a);
    
    plot(a(:,1),a(:,2),'k');
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('air temperature (^{\circ}C)');
    plotitle=sprintf('Global annual average air temperature: %s.gmairt',file);
    title(plotitle);
end;
    
if option==9;
    plot(a(:,1),a(:,13));
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Fractional snow cover');
    plotitle=sprintf('Global annual average fractional snow cover over land: %s.pslavgt',file);
    title(plotitle);
end;

if option==10;
    plot(a(:,1),a(:,14));
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Roughness length (m)');
    plotitle=sprintf('Global annual average roughness length over land: %s.pslavgt',file);
    title(plotitle);
end;

