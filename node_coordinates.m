close all
clc
clear


%carriage coordinates

%composition of matrix: [x_coord,y_coord,seat_no.]


seat_locations = zeros(76,3);

seat_locations(1,:) = [0.4,0.29,1];
seat_locations(2,:) = [0.4,0.87,2];
seat_locations(3,:) = [0.4,1.95,3];
seat_locations(4,:) = [0.4,2.53,4];

seat_locations(5,:) = [1.2,0.29,5];
seat_locations(6,:) = [1.2,0.87,6];
seat_locations(7,:) = [1.2,1.95,7];
seat_locations(8,:) = [1.2,2.53,8];

seat_locations(9,:) = [2,0.29,9];
seat_locations(10,:) = [2,0.87,10];
seat_locations(11,:) = [2,1.95,11];
seat_locations(12,:) = [2,2.53,12];

seat_locations(13,:) = [2.8,0.29,13];
seat_locations(14,:) = [2.8,0.87,14];
seat_locations(15,:) = [2.8,1.95,15];
seat_locations(16,:) = [2.8,2.53,16];

seat_locations(17,:) = [3.6,0.29,17];
seat_locations(18,:) = [3.6,0.87,18];
seat_locations(19,:) = [3.6,1.95,19];
seat_locations(20,:) = [3.6,2.53,20];

%table%

seat_locations(21,:) = [5.42,0.29,21];
seat_locations(22,:) = [5.42,0.87,22];
seat_locations(23,:) = [5.42,1.95,23];
seat_locations(24,:) = [5.42,2.53,24];

%back to back seats%

seat_locations(25,:) = [6.236,0.29,25];
seat_locations(26,:) = [6.236,0.87,26];
seat_locations(27,:) = [6.236,1.95,27];
seat_locations(28,:) = [6.236,2.53,28];

seat_locations(29,:) = [7.036,0.29,29];
seat_locations(30,:) = [7.036,0.87,30];
seat_locations(31,:) = [7.036,1.95,31];
seat_locations(32,:) = [7.036,2.53,32];

%table%

seat_locations(33,:) = [8.856,0.29,33];
seat_locations(34,:) = [8.856,0.87,34];
seat_locations(35,:) = [8.856,1.95,35];
seat_locations(36,:) = [8.856,2.53,36];

%back to back seats%

seat_locations(37,:) = [9.672,0.29,37];
seat_locations(38,:) = [9.672,0.87,38];
seat_locations(39,:) = [9.672,1.95,39];
seat_locations(40,:) = [9.672,2.53,40];

%table%

seat_locations(41,:) = [11.492,0.29,41];
seat_locations(42,:) = [11.492,0.87,42];
seat_locations(43,:) = [11.492,1.95,43];
seat_locations(44,:) = [11.492,2.53,44];

%back to back seats%

seat_locations(45,:) = [12.308,0.29,45];
seat_locations(46,:) = [12.308,0.87,46];
seat_locations(47,:) = [12.308,1.95,47];
seat_locations(48,:) = [12.308,2.53,48];

%table

seat_locations(49,:) = [14.128,0.29,49];
seat_locations(50,:) = [14.128,0.87,50];
seat_locations(51,:) = [14.128,1.95,51];
seat_locations(52,:) = [14.128,2.53,52];

seat_locations(53,:) = [14.928,0.29,53];
seat_locations(54,:) = [14.928,0.87,54];
seat_locations(55,:) = [14.928,1.95,55];
seat_locations(56,:) = [14.928,2.53,56];

seat_locations(57,:) = [15.728,0.29,57];
seat_locations(58,:) = [15.728,0.87,58];
seat_locations(59,:) = [15.728,1.95,59];
seat_locations(60,:) = [15.728,2.53,60];

seat_locations(61,:) = [16.528,0.29,61];
seat_locations(62,:) = [16.528,0.87,62];
seat_locations(63,:) = [16.528,1.95,63];
seat_locations(64,:) = [16.528,2.53,64];

seat_locations(65,:) = [17.328,0.29,65];
seat_locations(66,:) = [17.328,0.87,66];
seat_locations(67,:) = [17.328,1.95,67];
seat_locations(68,:) = [17.328,2.53,68];

%back to back seats%

seat_locations(69,:) = [18.144,0.29,69];
seat_locations(70,:) = [18.144,0.87,70];
seat_locations(71,:) = [18.144,1.95,71];
seat_locations(72,:) = [18.144,2.53,72];

%table%

seat_locations(73,:) = [19.964,0.29,73];
seat_locations(74,:) = [19.964,0.87,74];
seat_locations(75,:) = [19.964,1.95,75];
seat_locations(76,:) = [19.964,2.53,76];

csvwrite('seat_locations.csv',seat_locations)

figure()
title("DMS(B)",'Interpreter','latex')
hold on
for i = 1:76
   scatter(seat_locations(i,1),seat_locations(i,2),500,'.k' )
end
plot([0,0],[0,2.82],'-k','linewidth',2)
plot([20.4,20.4],[0,2.82],'-k','linewidth',2)
plot([0,20.4],[2.82,2.82],'-k','linewidth',2)
plot([0,20.4],[0,0],'-k','linewidth',2)
xlabel("$x$",'Interpreter','latex')
ylabel("$y$",'Interpreter','latex')
xlim([0 20.5])



%algorithm to find max capicity of the train


 max_num_of_seats = 76;

 accepted_seats = [];

 radius = 1;

for i = 1:max_num_of_seats

    accepted_seats(i) = i;

end


 for i = 1:numel(accepted_seats)

     num_to_remove = [];

     if i <= numel(accepted_seats)

         %go through each accepted node and determine if too close

         for m = accepted_seats(i):accepted_seats(end)

             fixed_seat = seat_locations(accepted_seats(i),:);
             trial_seat = seat_locations(m,:);

             %if too close then a seat number to list

             if (norm ([fixed_seat(1),fixed_seat(2)] - [trial_seat(1),trial_seat(2)] ) < radius && fixed_seat(3) ~= trial_seat(3))


                 num_to_remove = [num_to_remove, m];

             end

         end


         %remove the nodes too close from the accepted list

         for j = 1:numel(accepted_seats)
             for k = 1:numel(num_to_remove)

                 if j  <= numel(accepted_seats)

                     if accepted_seats(j) == num_to_remove(k)

                         accepted_seats = accepted_seats(accepted_seats~=num_to_remove(k));

                     end
                 end
             end
         end


     end

 end


carriage_capacity = (length(accepted_seats)/max_num_of_seats)*100


figure()
title([ "$\rho = 1$ \quad     No. of nodes =  ", num2str(length(accepted_seats)) ] ,'Interpreter','latex')
hold on
for i = 1:numel(accepted_seats)
   scatter(seat_locations(accepted_seats(i),1),seat_locations(accepted_seats(i),2),500,'.k' )
end
plot([0,0],[0,2.82],'-k','linewidth',2)
plot([20.4,20.4],[0,2.82],'-k','linewidth',2)
plot([0,20.4],[2.82,2.82],'-k','linewidth',2)
plot([0,20.4],[0,0],'-k','linewidth',2)
xlabel("$x$",'Interpreter','latex')
ylabel("$y$",'Interpreter','latex')
xlim([0 20.5])
