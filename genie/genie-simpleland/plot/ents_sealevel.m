clear all;

ents_constants;

disp('ENTS_SEALEVEL: A script for plotting sealevel diagnostics.');
file=input('What is the file name? \n','s');

filename=sprintf('%s%s.sealevel',path,file);

a=load(filename);

asze=size(a);

disp('1......Sea level change');
disp('2......Global average ocean density');
if asze(2)>3;
    disp('3......Annual average air temperature over Greenland at sealevel');
end;
disp(' ');
option=input('What would you like to plot? \n');

figure;
if option==1;
    plot(a(:,1),a(:,2),'r');
    grid on;
    hold on;
    if asze(2)>3;
        plot(a(:,1),a(:,4));
        plot(a(:,1),a(:,6),'k');
        legend('Thermal expansion','Greenland melt','Total',0);
    else;
        legend('Thermal expansion',0);
    end;
    xlabel('year (relative to start of run)');
    ylabel('Change in sealevel (m)');
    plotitle=sprintf('Change in sealevel: %s.sealevel',file);
    title(plotitle);
end;

if option==2;
    plot(a(:,1),a(:,3));
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Global average ocean density (kg/m^3)');
    plotitle=sprintf('Global average ocean density: %s.sealevel',file);
    title(plotitle);
end;

if option==3;
    plot(a(:,1),a(:,5),'r');
    hold on;
    grid on;
    xlabel('year (relative to start of run)');
    ylabel('Air temperature (^oC)');
    plotitle=sprintf('Global annual average air temperature at sealevel over Greenland: %s.sealevel',file);
    title(plotitle);
end;
