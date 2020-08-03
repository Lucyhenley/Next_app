close all
clc
clear





Width = 2.7;
Length = 26;

radius = 2;

first_x_node = 0;
second_x_node = (sqrt(2)/2)*radius;
third_x_node = 0;


first_row = [];
second_row = [];
third_row = [];

if sqrt(2)*radius < Width
    
    while first_x_node < Length
        
        first_row = [first_row,first_x_node];
        
        first_x_node  = first_x_node + radius;
        
    end
    
    while second_x_node < Length
        
        second_row = [second_row,second_x_node];
        
        second_x_node  = second_x_node + radius;
        
    end
    
    while third_x_node < Length
        
        third_row = [third_row,third_x_node];
        
        third_x_node  = third_x_node + radius;
        
    end
    
    
    
else
    
    
    
    while first_x_node < Length
        
        first_row = [first_row,first_x_node];
        
        first_x_node  = first_x_node + radius;
        
    end
    
    
    while third_x_node < Length
        
        third_row = [third_row,third_x_node];
        
        third_x_node  = third_x_node + radius;
        
    end
    
    
end


total = [first_row,second_row,third_row];

numel(total)
