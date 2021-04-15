%
%	Code for interpolating a fine map to 36x36
%

function [xxx1] = WCS(x1);

flo=sprintf('lon%s',num2str(size(x1,2)));
rlo='lon36';
fla=sprintf('lat%s',num2str(size(x1,1)));
rla='lat36';

fine_lon=load(flo);
raw_lon=load(rlo);
fine_lat=load(fla);
raw_lat=load(rla);

for j=1:size(x1,1)
	xx1(j,:)=interp1(fine_lon,x1(j,:),raw_lon);
end
clear j
for j=1:size(xx1,2)
	xxx1(:,j)=interp1(fine_lat,xx1(:,j),raw_lat);
end
S1=isnan(xxx1);
SS1=size(find(S1==1));
if (SS1(1)>0 & isnan(xxx1(1,1)))
	xxx1(1,:)=xx1(1,:);
end
clear S1 SS1;
S1=isnan(xxx1);
SS1=size(find(S1==1));
if (SS1(1)>0 & isnan(xxx1(end,1)))
	xxx1(end,:)=xx1(end,:);
end

return
