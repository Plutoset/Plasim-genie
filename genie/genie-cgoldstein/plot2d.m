%Dans ce qui suit, il faut initialiser nx ny et itt
% ou les remplacer partout par leur valeur.
%---------------------------
nx= input('nx ');
ny= input('ny ');
itt=1;
%ysize=36;
%y=-ysize/4.0:0.5*ysize/(ny-1):ysize/4.0;
string = input('input file name ','s');
B = load(string,'-ascii');
size(B)

Y = reshape(B,nx,ny);
Y = permute(Y,[2 1]);

%to ignore values, replace with NaN
 Y = changem(Y,NaN    ,0.0);

%dimensionalise
%Y = 1592.0 * Y;

%extend array for the lunatic pcolor routine
Y(ny+1,:) = Y(ny,:);
Y(:,nx+1) = Y(:,nx);

figure
contourf(Y,20) 
colorbar('horiz')
print -depsc tmp.cont
%title('streamfunction','fontsize',14) 

figure
pcolor(Y)

%set(gca,'DataAspectRatio',[1 1 1])    
%set(gca,'DataAspectRatio',[1 1 1])    

colorbar('horiz')
%title('streamfunction','fontsize',14) 
%xlabel('i','fontsize',14) 
%ylabel('j','fontsize',14)  
print -depsc tmp.eps
