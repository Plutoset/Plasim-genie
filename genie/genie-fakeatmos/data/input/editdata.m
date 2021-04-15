% matlab script to created boundary condition file for Genie-fakeatmos

% copy first embm_climate.short.nc first to bondc.nc

% ncstartup

ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_omip_woa.nc');
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_ober_woa.nc');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to get heat and salt flux from embm_climate   
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/embm_climate.nc');
saltflux = prate + evap + runoff + waterflux;
% remeber to make net flux zero
heatflux = sensible + netsolar + netlong + latent + conductflux;
% remember to make net flux zero (do this in goldstein?)
ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d.nc');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% use analytical profiles for sea surface temp and salinity
sst = 14.5* cos(latitude*pi/80) +13.5;
%plot(latitude, sst)
temprestore = repmat(sst',36,1);

sss = -1.* cos(4*latitude*pi/160) + 35;
plot(latitude, sss)
saltrestore = repmat(sss',36,1);
ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d.nc');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get sst and sss from Kevin's output
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin.nc');
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/kevin_output.nc');
temprestore = zeros(36,36); saltrestore= zeros(36,36);
temprestore(:,:) = ocn_temp(10,1,:,:) - 273.15;
saltrestore(:,:) = ocn_sal(10,1,:,:);

ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin.nc');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Try a run without buoyancy forcing in SO
%% first cp bcond2d_embm_kevin.nc bcond2d_embm_kevin_lgm.nc

ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm.nc');
temprestore(1:4,:) = -2;
freshwflux(1:4,:) = 0;
ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm.nc');

%% first cp bcond2d_embm_kevin.nc bcond2d_embm_kevin_lgm2.nc

ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm2.nc');
temprestore(1:7,:) = -2;
freshwflux(1:7,:) = 0;
ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm2.nc');

% Make oceanmask from 36x36 model
% Then use it to conserve waterflux in lgm2 model
% cp bcond2d_embm_kevin_lgm2.nc bcond2d_embm_kevin_lgm2_nonet.nc
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm2_nonet.nc');
oceanmask = saltrestore; landmask = saltrestore; 
oceanmask(saltrestore(:,:)<1000) = 1;
oceanmask(saltrestore(:,:)>1000) = 0;
landmask(saltrestore(:,:)<1000) = 0;
landmask(saltrestore(:,:)>1000) = 1;

nocean = sum(sum(oceanmask));		% count oceanpoints

% Don't need for equal area ocean grid cells
%ra = 6.37e6;   				% from initialise_goldstein
%area =  4 * pi * ra^2 / (36*36);

% surf(oceanmask); view(0,90);
tmpfresh = freshwflux;			% temp variable
tmpfresh(landmask(:,:)==1) = nan;
%surf(tmpfresh); view(0,90);

totfresh = nansum(nansum(tmpfresh))	% total flux into ocean (/box area)

% add freshwater only to ocean points north of 7. Make mask for such points
oceannorth = oceanmask;
oceannorth(1:7,:) = 0;
noceann = sum(sum(oceannorth));
addflux = totfresh/noceann;  	
tmpfresh(oceannorth(:,:)==1) = tmpfresh(oceannorth(:,:)==1) - addflux;

% totfresh = nansum(nansum(tmpfresh))  % this should be zero now

freshwflux = tmpfresh;
ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_embm_kevin_lgm2_nonet.nc');



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% use  sea surface temperature and salinity from WOA01
%% downloaded this from http://ferret.pmel.noaa.gov/NVODS/servlets/dataset
%% i.e., National Ocean Virtual Data centre (NOVDC)
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/WOA_sst_sss_100E.nc');
data_lon=LONGITUDE101_460 - 360;
genie_lon=longitude;
data_lat=LATITUDE;
genie_lat=latitude;

temprestore = interp2(data_lon,data_lat,T00AN1,genie_lon,genie_lat');
saltrestore = interp2(data_lon,data_lat,S00AN1,genie_lon,genie_lat');

%xx(xx(:,:)<-1e4)= -5;

%saltrestore(saltrestore(:,:)<-1e4)= 30;
%surface(saltrestore); view(0,90);
%shading interp; 

ncsave('/esdata/env/e099/genie/genie-fakeatmos/data/input/bcond2d_omip_woa.nc');
clear S00AN1 T00AN1 LONGITUDE101_460 LATITUDE DEPTH1_1 DEPTH1_1_bnds ANNUAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read in omip fluxes. 
%% downloaded from 
%% http://www.omip.zmaw.de/omip/forcing365/atlas/data/test4.php
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/omip_mean_freshwat_flux.nc');
ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/omip_mean_net_heat_flux.nc');

tmpf = zeros(160,320);
tmpf(:,:) = (fresh(13,:,:));
tmpf = flipud(tmpf);          % flip up side down (around equator)

tmph = zeros(160,320);
tmph(:,:) = (net_heat(13,:,:));
tmph = flipud(tmph);          % flip up side down (around equator)

% interpolate first onto genie grid from 0 to 360
data_lon=lonpt;
genie_lon=(5:10:355)';
data_lat=flipud(latpt);
genie_lat= latitude;

tmpfresh = interp2(data_lon,data_lat,tmpf,genie_lon,genie_lat');
tmpheat = interp2(data_lon,data_lat,tmph,genie_lon,genie_lat');

%surface(tmpfresh); view(0,90);
%surface(tmp); view(0,90);
%shading interp; 

% convert m/s to mm/s
tmpfresh = tmpfresh .* 1000;

% shift variables to grid going from -260 to 100.
left = tmpfresh(:,1:10);
right = tmpfresh(:,11:36);
freshwflux(:,1:26) = right(:,:);
freshwflux(:,27:36) = left(:,:);

left = tmpheat(:,1:10);
right = tmpheat(:,11:36);
heatflux(:,1:26) = right(:,:);
heatflux(:,27:36) = left(:,:);

heatflux(heatflux(:,:)>1e10)=0;
heatflux(heatflux(:,:)<-1e10)=0;
surface(heatflux); shading interp

%% these fluxes look shitty, expecially the heat fluxes. Try using instead oberhuber
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read in oberhuber fluxes. 
%% downloaded from nvods live access
%% http://ferret.pmel.noaa.gov/NVODS/servlets/constrain?var=17069&var=17070

ncload('/esdata/env/e099/genie/genie-fakeatmos/data/input/omip_mean_freshwat_flux.nc');


evap_z = mean(evap(13,:,:),3);
prec_z = mean(precip(13,:,:),3);
% plot(latpt,evap_z, latpt, prec_z)

% make symmetric around equator
evap_s(1:80) = (evap_z(1:80) + fliplr(evap_z(81:160)))/2;
evap_s(81:160) = fliplr(evap_s(1:80));
prec_s(1:80) = (prec_z(1:80) + fliplr(prec_z(81:160)))/2;
prec_s(81:160) = fliplr(prec_s(1:80));

%plot(evap_s)
% plot(latpt,prec_z, latpt, prec_s)


%ygrid = data(2,:);
newgrid = (-86:4:86)';
newpigrid = newgrid.*(pi/180);

% interpolate onto new grid 
ev = interp1(latpt,evap_s,newgrid);
pr = interp1(latpt,prec_s,newgrid);

% check that sum of evap and prec = 0. It not exactly, make it so
pme = pr + ev;
% To average: int[(evap) dx dy] / int[dx dy] where dx=cos(y)*2piR
meanflux = sum(pme .*abs(cos(newpigrid)))/sum(abs(cos(newpigrid)))
% meanflux = 5.0591e-11. Lets subtract from precip field
% min pr =3.9079e-09 so that should not create negative precip

pr_nor = pr - meanflux;
pme_nor = pr_nor + ev;
meanflux_nor = sum(pme_nor .*abs(cos(newpigrid)))/sum(abs(cos(newpigrid)))
% meanflux_nor = 1.6248e-26. Looks good
checksumev1 = sum(ev .*abs(cos(newpigrid)))/sum(abs(cos(newpigrid)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ncload('/net2/amb/ebm/INPUT/FRH/evap_omip_2bastl.nc');
EVAP = repmat(ev,1,90);

% spread evap over continents into oceans at same latitude.
% First do poles and regions with 2 continental barriers
barsum = sum(EVAP([1:6,15:44],[1,65:66,90]),2)/86;
EVAP([1:6,15:44],[1,65:66,90]) = 0;
temp = repmat(barsum,1,86);
EVAP([1:6,15:44],[2:64,67:89]) = EVAP([1:6,15:44],[2:64,67:89]) + temp;

% Now do region with 1 continental barrier
barsum = sum(EVAP([11:14],[65:66]),2)/88;
EVAP([11:14],[65:66]) = 0;
temp = repmat(barsum,1,88);
EVAP([11:14],[2:89]) = EVAP([11:14],[2:89]) + temp;

%plot(EVAP(:,20))

% put fluxes over Antarctic in adjacent grid box


ant = (abs(cos(newpigrid(1)))*EVAP(1,:) + ...
       abs(cos(newpigrid(2)))*EVAP(2,:) + ...
       abs(cos(newpigrid(3)))*EVAP(3,:))/abs(cos(newpigrid(4)));
EVAP(4,:) = EVAP(4,:) + ant;
EVAP(1:3,:) = 0;

% now do runoff over Atlantic.
atl = (abs(cos(newpigrid(42)))*EVAP(42,67:89) + ...
       abs(cos(newpigrid(43)))*EVAP(43,67:89) + ...
       abs(cos(newpigrid(44)))*EVAP(44,67:89))/abs(cos(newpigrid(41)));
EVAP(41,67:89) = EVAP(41,67:89) + atl;
EVAP(42:44,67:89) = 0;


%Now put flux over Pacific polar box to zero
pac = (abs(cos(newpigrid(40)))*EVAP(40,2:64) + ...
       abs(cos(newpigrid(41)))*EVAP(41,2:64) + ...
       abs(cos(newpigrid(42)))*EVAP(42,2:64) + ...
       abs(cos(newpigrid(43)))*EVAP(43,2:64) + ...
       abs(cos(newpigrid(44)))*EVAP(44,2:64))/abs(cos(newpigrid(39)));
EVAP(39,2:64) = EVAP(39,2:64) + pac;
EVAP(40:44,2:64) = 0;

ncsave('/net2/amb/ebm/INPUT/FRH/evap_omip_2bastl.nc');

% Now do the same for the precipitation field

ncload('/net2/amb/ebm/INPUT/FRH/prec_omip_2bastl.nc');
PREC = repmat(pr,1,90);

% spread evap over continents into oceans at same latitude.
% First do poles and regions with 2 continental barriers
barsum = sum(PREC([1:6,15:44],[1,65:66,90]),2)/86;
PREC([1:6,15:44],[1,65:66,90]) = 0;
temp = repmat(barsum,1,86);
PREC([1:6,15:44],[2:64,67:89]) = PREC([1:6,15:44],[2:64,67:89]) + temp;

% Now do region with 1 continental barrier
barsum = sum(PREC([11:14],[65:66]),2)/88;
PREC([11:14],[65:66]) = 0;
temp = repmat(barsum,1,88);
PREC([11:14],[2:89]) = PREC([11:14],[2:89]) + temp;

plot(PREC(:,20))

% put fluxes over Antarctic in adjacent grid box


ant = (abs(cos(newpigrid(1)))*PREC(1,:) + ...
       abs(cos(newpigrid(2)))*PREC(2,:) + ...
       abs(cos(newpigrid(3)))*PREC(3,:))/abs(cos(newpigrid(4)));
PREC(4,:) = PREC(4,:) + ant;
PREC(1:3,:) = 0;

% now do runoff over Atlantic.
atl = (abs(cos(newpigrid(42)))*PREC(42,67:89) + ...
       abs(cos(newpigrid(43)))*PREC(43,67:89) + ...
       abs(cos(newpigrid(44)))*PREC(44,67:89))/abs(cos(newpigrid(41)));
PREC(41,67:89) = PREC(41,67:89) + atl;
PREC(42:44,67:89) = 0;


%Now put flux over Pacific polar box to zero
pac = (abs(cos(newpigrid(40)))*PREC(40,2:64) + ...
       abs(cos(newpigrid(41)))*PREC(41,2:64) + ...
       abs(cos(newpigrid(42)))*PREC(42,2:64) + ...
       abs(cos(newpigrid(43)))*PREC(43,2:64) + ...
       abs(cos(newpigrid(44)))*PREC(44,2:64))/abs(cos(newpigrid(39)));
PREC(39,2:64) = PREC(39,2:64) + pac;
PREC(40:44,2:64) = 0;

ncsave('/net2/amb/ebm/INPUT/FRH/prec_omip_2bastl.nc');

%check that sum is zero

PME = EVAP + PREC;
sum(abs(cos(newpigrid))'*PME)


surface(PME)


break

% to plot final precip and evap
ncstartup
ncload('/net2/amb/ebm/INPUT/FRH/evap_omip_2bas.nc');
ncload('/net2/amb/ebm/INPUT/FRH/prec_omip_2bas.nc');

plot(GRID_Y_T,PREC(:,14), GRID_Y_T,PREC(:,70))
legend('Atl Precip','Pac Precip');
  print('-dpsc', 'precip.ps')
figure
plot(GRID_Y_T,EVAP(:,14), GRID_Y_T,EVAP(:,70))
legend('Atl Evap','Pac Evap');
  print('-dpsc', 'evap.ps')
figure
pme = PREC+EVAP;
plot(GRID_Y_T,PME(:,14), GRID_Y_T,PME(:,70))
legend('Atl PME','Pac PME');
  print('-dpsc', 'pme.ps')

figure
surface(PME)


