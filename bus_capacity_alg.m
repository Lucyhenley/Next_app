close all
clc
clear



seat_locations_bus = zeros(28,3);


seat_locations_bus(1,:) = [0.72175,0.2375,1];

%disabled locaion
seat_locations_bus(2,:) = [1.0835,0.35,2];


seat_locations_bus(3,:) = [1.401,0.2375,3];
seat_locations_bus(4,:) = [1.401,0.7125,4];

seat_locations_bus(5,:) = [1.868,0.2375,5];
seat_locations_bus(6,:) = [1.868,0.7125,6];

seat_locations_bus(7,:) = [2.335,0.2375,7];
seat_locations_bus(8,:) = [2.335,0.7125,8];

seat_locations_bus(9,:) = [2.802,0.2375,9];
seat_locations_bus(10,:) = [2.802,0.7125,10];

seat_locations_bus(11,:) = [3.269,0.2375,11];
seat_locations_bus(12,:) = [3.269,0.7125,12];

seat_locations_bus(13,:) = [3.736,0.2375,13];
seat_locations_bus(14,:) = [3.736,0.7125,14];
seat_locations_bus(15,:) = [3.736,1.1875,15];
seat_locations_bus(16,:) = [3.736,1.6625,16];
seat_locations_bus(17,:) = [3.736,2.1375,17];

seat_locations_bus(18,:) = [3.269,1.6625,18];
seat_locations_bus(19,:) = [3.269,2.1375,19];

seat_locations_bus(20,:) = [2.802,1.6625,20];
seat_locations_bus(21,:) = [2.802,2.1375,21];

seat_locations_bus(22,:) = [2.335,1.6625,22];
seat_locations_bus(23,:) = [2.335,2.1375,23];

seat_locations_bus(24,:) = [1.868,1.6625,24];
seat_locations_bus(25,:) = [1.868,2.1375,25];

seat_locations_bus(26,:) = [1.401,1.6625,26];
seat_locations_bus(27,:) = [1.401,2.1375,27];

seat_locations_bus(28,:) = [0.934,2.1375,28];


figure()
subplot(2,1,1);
title("Bus",'Interpreter','latex')
hold on
for i = 1:28
    scatter(seat_locations_bus(i,1),seat_locations_bus(i,2),500,'.k' )
end
plot([0,0],[0,2.4],'-k','linewidth',2)
plot([4,4],[0,2.4],'-k','linewidth',2)
plot([0,4],[2.4,2.4],'-k','linewidth',2)
plot([0,4],[0,0],'-k','linewidth',2)
xlabel("$x$",'Interpreter','latex')
ylabel("$y$",'Interpreter','latex')
axis equal
set(gca,'visible','off')

%algorithm to find max capicity of the train


max_num_of_seats = 28;

accepted_seats = [];

radius = 2;

for i = 1:max_num_of_seats
    
    accepted_seats(i) = i;
    
end


for i = 1:numel(accepted_seats)
    
    num_to_remove = [];
    
    if i <= numel(accepted_seats)
        
        %go through each accepted node and determine if too close
        
        for m = accepted_seats(i):accepted_seats(end)
            
            fixed_seat = seat_locations_bus(accepted_seats(i),:);
            trial_seat = seat_locations_bus(m,:);
            
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

nodes_for_heatmapper = [];
for i  = 1:numel(accepted_seats)
    nodes_for_heatmapper(i,1) = seat_locations_bus(accepted_seats(i),1);
    nodes_for_heatmapper(i,2) = seat_locations_bus(accepted_seats(i),2);
end


subplot(2,1,2)
title([ "$\rho = 2$ \quad     Capacity =  ", num2str(carriage_capacity) ] ,'Interpreter','latex')
hold on
for i = 1:numel(accepted_seats)
    scatter(seat_locations_bus(accepted_seats(i),1),seat_locations_bus(accepted_seats(i),2),500,'.k' )
end
plot([0,0],[0,2.4],'-k','linewidth',2)
plot([4,4],[0,2.4],'-k','linewidth',2)
plot([0,4],[2.4,2.4],'-k','linewidth',2)
plot([0,4],[0,0],'-k','linewidth',2)
xlabel("$x$",'Interpreter','latex')
ylabel("$y$",'Interpreter','latex')
heatmapper(nodes_for_heatmapper, radius/2,[0,0,0,0],4,2.4);
axis equal
set(gca,'visible','off')


number_of_pass = length(accepted_seats); %max capacity after social
pass = linspace(1,28,28); %total number of passenegers

figure()
plot(pass,emission_per_pass_bus(pass),'k')
hold on
yline(130.4,'--r','linewidth',2)
yline(215.3,'--b','linewidth',2)
xlabel("No. of passenegers",'Interpreter','latex');
ylabel("Emissions per passenegers, $(km^{-1}g)$",'Interpreter','latex')
plot([0,number_of_pass],[emission_per_pass_bus(number_of_pass),emission_per_pass_bus(number_of_pass)],'-k','linewidth',1)
plot([number_of_pass,number_of_pass],[0,emission_per_pass_bus(number_of_pass)],'-k','linewidth',1)
scatter(number_of_pass,emission_per_pass_bus(number_of_pass),300,[0 0.5 0],'x','linewidth',2);

title("Bus",'Interpreter','latex')
legend("Train","Small car","Large car",'Interpreter','latex' )
box off



function out = emission_per_pass_bus(pass_no)
total_bus_150_emissions = 1030.62;
out = total_bus_150_emissions./pass_no;
end