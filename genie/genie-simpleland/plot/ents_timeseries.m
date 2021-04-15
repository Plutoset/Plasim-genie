% Matlab script used to plot carbon reservoirs and fluxes
% from simpleland.
clear all;

ents_constants;
 
disp('ENTS_TIMESERIES: Plotting routine for carbon reservoir and flux timeseries');
run_id=input('What is the run identifier?\n','s');

filename=sprintf('%s%s.slandt',path,run_id);
plottitle=sprintf('Timeseries data: %s.slandt',run_id);
x=load(filename);

xsze=size(x);

% summed fluxes around boxes (remember changed for new runs)
vegflux=x(:,7)-x(:,8)-x(:,9);
soilflux=x(:,9)-x(:,10);
atmflux=-x(:,7)+x(:,8)+x(:,10);

gtc=char('Veg carbon','Soil carbon','Atm carbon');
gtf=char('Photosynthesis','Respiration (veg)',...
    'Leaf litter','Respiration (soil)');
gtf1=char('Veg flux','Soil flux','Atmospheric flux');

figure;
plot(x(:,1),x(:,2),'g');
hold on;
plot(x(:,1),x(:,3));
plot(x(:,1),x(:,4),'r');
grid on;
xlabel('Year');
ylabel('GtC in reservoir');
legend(gtc,0);
title(plottitle);

figure;
plot(x(:,1),x(:,5));
grid on;
xlabel('Year');
ylabel('veg fraction');
title(plottitle);

figure;
plot(x(:,1),x(:,7),'g');
hold on;
plot(x(:,1),x(:,8));
plot(x(:,1),x(:,9),'k');
plot(x(:,1),x(:,10),'r');
grid on;
xlabel('Year');
ylabel('GtC/yr');
legend(gtf,0);
title(plottitle);

figure;
plot(x(:,1),vegflux,'g');
hold on;
plot(x(:,1),soilflux);
plot(x(:,1),atmflux,'r');
grid on;
xlabel('Year');
ylabel('GtC/yr');
legend(gtf1,0);
title(plottitle);

figure;
plot(x(:,1),x(:,6),'r');
grid on;
xlabel('Year');
ylabel('pCO_2 (ppmv)');
title(plottitle);

