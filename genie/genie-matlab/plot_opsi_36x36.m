%nx = input('input imax');
%ny = input('input jmax');
nx= 37;
ny= 37;
%ysize=36;
%y=-ysize/4.0:0.5*ysize/(ny-1):ysize/4.0;
string = input('input file name ','s');
B = load(string,'-ascii');
size(B)

scf = 1592.5

A = reshape(B,nx,ny);
A = permute(A,[2 1]);
A = A.*scf;

figure

%A = changem(A,0.0/0.0,0.0);
%c=B(image,:);

%calcul du maximum de Z

%zmax=max(permute(max(Z),[2 1]));

%plot(y,c)
%contour(A,phi,s,10,'k:')
%surf(A)
contourf(A,20)
%contour(A,20)
%plot(c)

%on impose que le rapport d'aspect entre les longueurs des axes
%soit celui des donnees 
%set(gca,'DataAspectRatio',[1 1 1])    
%set(gca,'DataAspectRatio',[4 1 1])    

colorbar('horiz')
title(['streamfunction: ', string],'fontsize',14) 
%xlabel('i','fontsize',14) 
%ylabel('j','fontsize',14)  
print -depsc tmp.eps

filename = ['opsi_', string, '.ps'];
print('-dpsc2', filename);
