% itt is the number of data points in time, ie rows in the input file
% ien as usual is the number of timesteps between outputs
%---------------------------
%itt=100 
%ien=10;
%dt=0.1;
%dt=dt*ien;
%t=dt:dt:itt*dt;
string = input('input file name ','s');
B = load(string,'-ascii');

size(B)
t=B(:,1);

%************
% image 
%************

figure

%Pac_T_d          
c=B(:,2);
plot(t,c,'g')
hold on

%Atl_T_d
c=B(:,3);
plot(t,c,'b')
hold on

%Ind_T_d
c=B(:,4);
plot(t,c,'r')
hold on

%Sou_T_d
c=B(:,5);
plot(t,c,'m')
hold on

%Pac_T_u          
c=B(:,6);
plot(t,c,'g')
hold on

%Atl_T_u
c=B(:,7);
plot(t,c,'b')
hold on

%Ind_T_u
c=B(:,8);
plot(t,c,'r')
hold on

%Sou_T_u
c=B(:,9);
plot(t,c,'m')
hold on

title('Average Temperatures','fontsize',14) 

xlabel('t','fontsize',14) 
ylabel('Temp.','fontsize',14)  
print -depsc tmp.eps  



%plot(t,c,'r>')
%plot(t,c,'r<')
%plot(t,c,'rd')
%plot(t,c,'rx')
%plot(t,c,'c.')
%plot(t,c,'m')
%plot(t,c,'b>')
%plot(t,c,'b^')
%plot(t,c,'y')
%plot(t,c,'k')
%plot(t,c,'bo')

%axis([-ysize/4.0,ysize/4.0,0.0,1.0])
%c=B(:,2)-B(1,2);
%for j=9:15
%  tv1=B(1,j);
%  B(1,j)=0;
%  for i=2:itt
%    tv2=B(i,j);
%    B(i,j)=B(i-1,j) + 0.5*dt*(tv1+tv2);
%%    tv1=tv2;
%  end
%end
