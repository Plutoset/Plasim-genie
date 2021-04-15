%Quick and easy program to visualise T and S from goldstein output
% for c-goldstein, plot all variables ie
% T,S,u,v, k=1,kmax, then T,q,h,a,T_ice
%---------------------------
ny= 36;
nx= 36;
nz= 8;
lmax = 2;
%nx = input('input imax');
%ny = input('input jmax');
%nz = input('input kmax');
%lmax = input('input lmax');
%nvar = kmax*(lmax + 2) + 5

%y=-pi/2.0:pi/(ny):pi/2.0;
%phi=0:2*pi/nx:2*pi;

string = input('input file name ','s');
[fid,message] = fopen(string);

size = [(lmax+2)*nz*nx,ny]

[TSU,count] = fscanf(fid,'%g',size);

count

TSU = reshape(TSU,(lmax+2),nz,nx,ny);

size = [2*nx,ny]

[TQ,count] = fscanf(fid,'%g',size);

count

TQ = reshape(TQ,2,nx,ny);

[HA,count] = fscanf(fid,'%g',size);

count

HA = reshape(HA,2,nx,ny);

size = [nx,ny]
[TI,count] = fscanf(fid,'%g',size);
count
fclose(fid);

%A = reshape(B,nvar,nx,ny);
%A = permute(A,[4 3 2 1]);
%************
% image 
%************
%to ignore zeros replace them with NaN's 

for l=1:lmax
  for k=4:2:8   
% for k=nz:nz     
    figure
    for j=1:ny
      for i=1:nx
        Y(j,i) = TSU(l,k,i,j);
        U(j,i) = TSU(3,k,i,j);
        V(j,i) = TSU(4,k,i,j);

% allow plotting of points where one component of vel'y is zero

        if  U(j,i)==0 & V(j,i) == 0
           U(j,i)=NaN;
        end
      end
    end
    Y = changem(Y,NaN    ,0.0);
    Y(ny+1,:) = Y(ny,:);
    Y(:,nx+1) = Y(:,nx);
    pcolor(Y)
%   contourf(Y)
    colorbar('horiz')
    title('ocean','fontsize',14) 
    hold on
%plot velocity vectors
    quiver(U,V,2.0,'k')
  end
end
%plot atmosphere
for l=1:2     
    figure
    for j=1:ny
      for i=1:nx
        Y(j,i) = TQ(l,i,j);
      end
    end
%   Y = changem(Y,NaN    ,0.0);
    Y(ny+1,:) = Y(ny,:);
    Y(:,nx+1) = Y(:,nx);
    pcolor(Y)
    colorbar('horiz')
    title('atm','fontsize',14) 
    hold on
end
%plot ice
for l=1:2
    figure
    for j=1:ny
      for i=1:nx
        Y(j,i) = HA(l,i,j);
      end
    end
    Y = changem(Y,NaN    ,0.0);
    Y(ny+1,:) = Y(ny,:);
    Y(:,nx+1) = Y(:,nx);
    pcolor(Y)
    colorbar('horiz')
    title('ice','fontsize',14) 
    hold on
end
    figure
    for j=1:ny
      for i=1:nx
        Y(j,i) = TI(i,j);
      end
    end
    Y = changem(Y,NaN    ,0.0);
    Y(ny+1,:) = Y(ny,:);
    Y(:,nx+1) = Y(:,nx);
    pcolor(Y)
    colorbar('horiz')
    title('ice','fontsize',14) 
    hold on

print -depsc tmp.eps

%contour(Y,phi,s,10,'k:')
%surf(Y)
%extend array for the lunatic pcolor routine
%surf(Y)
%   contourf(Y,20)
%contour(Y,20)
%plot(c)
%set(gca,'DataAspectRatio',[1 1 1])    
%set(gca,'DataAspectRatio',[1 1 1])    
%xlabel('y','fontsize',14) 
%ylabel('conc.','fontsize',14)  
