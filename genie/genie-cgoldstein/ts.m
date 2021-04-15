%Quick and easy program to visualise T and S from goldstein output
%---------------------------
%ny= 36;
%nx= 36;
%nz= 8;
%nvar=4;
nx = input('input imax');
ny = input('input jmax');
nz = input('input kmax');
nvar = input('input lmax');
itt=1;
%ysize=36;
%y=-ysize/4.0:0.5*ysize/(ny-1):ysize/4.0;
%phi=-ysize/4.0:0.5*ysize/(ny-1):ysize/4.0;
string = input('input file name ','s');
%load tmp.dat
%B = tmp
B = load(string,'-ascii');
%B=string;
size(B)

A = reshape(B,nvar,nz,nx,ny);
%A = permute(A,[4 3 2 1]);
%************
% image 
%************

for l=1:nvar
  for k=1:1:nz  
% for k=nz:nz     
    figure

    for j=1:ny
      for i=1:nx
        Y(j,i) = A(l,k,i,j);
      end
    end

%to ignore zeros replace them with NaN's 

    Y = changem(Y,NaN    ,0.0);

%contour(Y,phi,s,10,'k:')
%surf(Y)
%extend array for the lunatic pcolor routine
Y(ny+1,:) = Y(ny,:);
Y(:,nx+1) = Y(:,nx);
pcolor(Y)
%surf(Y)
%    contourf(Y,20)
%contour(Y,20)
%plot(c)

%on impose que le rapport d'aspect entre les longueurs des axes
%soit celui des donnees 
%set(gca,'DataAspectRatio',[1 1 1])    
%set(gca,'DataAspectRatio',[1 1 1])    

    colorbar('horiz')


    title('temperature','fontsize',14) 
    hold on
  end
end
%xlabel('y','fontsize',14) 
%ylabel('conc.','fontsize',14)  
print -depsc tmp.eps
