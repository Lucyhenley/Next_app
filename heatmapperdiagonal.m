

function heatmap = heatmapperdiagonal(nodes, radius,shield,domain_x,domain_y)
%function to visualise radius of risk region for the geometry of the train
%based on people sitting at loctations `nodes` with a perimeter of social
%distancing `radius` and plastic blockades at locations `shields`. This is
%applied into `domain`, which assumes rectangular geometry of a train.
%works if nodes is a Nx2 matrix with first column the x position, second y
%position


%barrier must be diagonal, not horizontal or vertical

% 
% clf
% 
% nodes = [4 4];
% shield = [2 4 4 2;
%     4 6 2 4;
%     2 4 4 6;
%     6 4 4 6];
% radius = 3
% domain_x = 10;
% domain_y = 10;
% heatmapperdiagonal(nodes, radius,shield,domain_x,domain_y)

theta = linspace(0,2*pi,100);
heatmap = zeros(2,100*length(nodes(:,1)))
for j = 1:length(nodes(:,1))
    
    x_circle = radius*cos(theta) + nodes(j,1);%Defines an intial unsafe perimeter
    y_circle = radius*sin(theta) + nodes(j,2);
    
    x_circle(x_circle<0) = 0;
    x_circle(x_circle>domain_x) = domain_x;
    y_circle(y_circle<0) = 0;
    y_circle(y_circle>domain_y) = domain_y;%Snaps unsafe perimeter to domain walls where necessary
    heatmap(1,1+100*(j-1):100*(j-1) + 100) = x_circle;
    heatmap(2,1+100*(j-1):100*(j-1) + 100) = y_circle;
    
    
    for i = 1:length(shield(:,1))
        %iterate through each defined shield
        shield_locations_y = linspace(shield(i,3),shield(i,4));
        shield_locations_x = linspace(shield(i,1),shield(i,2));
        if min(min(sqrt((shield_locations_y-nodes(j,2)).^2 + (shield_locations_x-nodes(j,1)).^2)))<radius %checks shield interaction occurs
            grad = (shield(i,4)-shield(i,3))/(shield(i,2)-shield(i,1));
            intercept = shield(i,4)-grad*shield(i,2);%defines line function which shield defines
            if grad*nodes(j,1)+intercept == nodes(j,2)
                continue %This rules out cases where walls are perpendicular to the gradient of the circle, so does not actually effect the safe region
            else
                [Shortrad,index]=min(sqrt((shield_locations_y-nodes(j,2)).^2 + (shield_locations_x-nodes(j,1)).^2))%Find closest point of shield to the node
                Shield_touch_x = shield_locations_x(sqrt((shield_locations_y-nodes(j,2)).^2 + (shield_locations_x-nodes(j,1)).^2)<radius);
                Shield_touch_y = shield_locations_y(sqrt((shield_locations_y-nodes(j,2)).^2 + (shield_locations_x-nodes(j,1)).^2)<radius);%returns locations of shield which lie inside unsafe region
                %Needed to decide on placement of 'exclusion zone', which is region
                %behind wall which is made safe by the barrier.
                %Also need points on line which we can map excluded terms from the perimeter of the unsafe region to, found by
                %using shield locations closer than radius.
                
                %rest of code divided into 4 scenarious, depending on where the
                %boundary is relative to the node. These quadrants are names 1,2,3,4
                %anticklockwise from the x coordinate of the node.
                
                
                %In every quadrant we must reposition the shield to be defined so that
                %it is anticlockwise, to make sure that when we redraw the perimeter of
                %the safe zone it is continous from the anticlockwise defined circle.
                if sign(shield_locations_x(index)-nodes(j,1)) < 0 %quad2/3?
                    if sign(shield_locations_y(index)-nodes(j,2))<0
                        if shield(i,1)<shield(i,2)%quad3
                            shield(i,1:2)= shield(i,2:-1:1)
                            shield(i,3:4)= shield(i,4:-1:3)
                        end
                        shift1 = 20*([shield(i,1) shield(i,3)] - nodes(j,:));
                        shift2 = 20*([shield(i,2) shield(i,4)] - nodes(j,:));
                        exclusion_zone_x = [shield(i,1) shield(i,2) shield(i,2)+shift2(1) ...
                            shield(i,1)+shift1(1) shield(i,1)];
                        exclusion_zone_y = [shield(i,3) shield(i,4) shield(i,4)+shift2(2) shield(i,3)+shift1(2)  shield(i,3)];
                        in = inpolygon(x_circle,y_circle,exclusion_zone_x, exclusion_zone_y);
                        if length(in) == 1
                            x_circle(in==1) = shield_touch_x(round(length(Shield_touch)/2));
                            y_circle(in==1) = shield_touch_y(round(length(Shield_touch)/2));
                        else
                            x_circle(in==1) = linspace(min(Shield_touch_x),max(Shield_touch_x),sum(in));
                            y_circle(in==1) = grad*(linspace(min(Shield_touch_x),max(Shield_touch_x),sum(in)))+intercept;
                        end
                        
                    else
                        if shield(i,3)<shield(i,4)%quad2
                            shield(i,1:2)= shield(i,2:-1:1)
                            shield(i,3:4)= shield(i,4:-1:3)
                        end
                        shift1 = 20*([shield(i,1) shield(i,3)] - nodes(j,:));
                        shift2 = 20*([shield(i,2) shield(i,4)] - nodes(j,:));
     exclusion_zone_x = [shield(i,1) shield(i,2) shield(i,2)+shift2(1) ...
                            shield(i,1)+shift1(1) shield(i,1)];
                        exclusion_zone_y = [shield(i,3) shield(i,4) shield(i,4)+shift2(2) shield(i,3)+shift1(2)  shield(i,3)];
                        in = inpolygon(x_circle,y_circle,exclusion_zone_x, exclusion_zone_y);

                        
                        if length(in) == 1
                            x_circle(in==1) = shield_touch_x(round(length(Shield_touch)/2));
                            y_circle(in==1) = shield_touch_y(round(length(Shield_touch)/2));
                        else
                            x_circle(in==1) = linspace(max(Shield_touch_x),min(Shield_touch_x),sum(in));
                            y_circle(in==1) = grad*(linspace(max(Shield_touch_x),min(Shield_touch_x),sum(in)))+intercept;
                        end
                    end
                    
                else%quad1 or 4
                    if sign(shield_locations_y(index)-nodes(j,2))<0%quad4
                        if shield(i,3)>shield(i,4)
                            shield(i,1:2)= shield(i,2:-1:1)
                            shield(i,3:4)= shield(i,4:-1:3)
                        end
                        shift1 = 20*([shield(i,1) shield(i,3)] - nodes(j,:));
                        shift2 = 20*([shield(i,2) shield(i,4)] - nodes(j,:));
                         exclusion_zone_x = [shield(i,1) shield(i,2) shield(i,2)+shift2(1) ...
                            shield(i,1)+shift1(1) shield(i,1)];
                        exclusion_zone_y = [shield(i,3) shield(i,4) shield(i,4)+shift2(2) shield(i,3)+shift1(2)  shield(i,3)];
                        in = inpolygon(x_circle,y_circle,exclusion_zone_x, exclusion_zone_y);
                        if length(in) == 1
                            x_circle(in==1) = shield_touch_x(round(length(Shield_touch)/2));
                            y_circle(in==1) = shield_touch_y(round(length(Shield_touch)/2));
                        else
                            a = x_circle(in==1)
                            if a(1) == nodes(j,1)+radius
                            in(1) = 0
                            end

                            x_circle(in==1) = linspace(min(Shield_touch_x),max(Shield_touch_x),sum(in));
                            y_circle(in==1) = grad*(linspace(min(Shield_touch_x),max(Shield_touch_x),sum(in)))+intercept;
                        end
                        
                        
                        
                    else
                        if shield(i,1)<shield(i,2)%quad1
                            shield(i,1:2)= shield(i,2:-1:1)
                            shield(i,3:4)= shield(i,4:-1:3)
                        end
                        shift1 = 20*([shield(i,1) shield(i,3)] - nodes(j,:));
                        shift2 = 20*([shield(i,2) shield(i,4)] - nodes(j,:));
                            exclusion_zone_x = [shield(i,1) shield(i,2) shield(i,2)+shift2(1) ...
                            shield(i,1)+shift1(1) shield(i,1)];
                        exclusion_zone_y = [shield(i,3) shield(i,4) shield(i,4)+shift2(2) shield(i,3)+shift1(2)  shield(i,3)];

                        
                        
                        in = inpolygon(x_circle,y_circle,exclusion_zone_x, exclusion_zone_y);
                        if length(in) == 1
                            x_circle(in==1) = shield_touch_x(round(length(Shield_touch)/2));
                            y_circle(in==1) = shield_touch_y(round(length(Shield_touch)/2));
                        else
                            if a(end) == nodes(j,1)+radius
                            in(end) = 0
                            end
                            x_circle(in==1) = linspace(max(Shield_touch_x),min(Shield_touch_x),sum(in));
                            y_circle(in==1) = grad*(linspace(max(Shield_touch_x),min(Shield_touch_x),sum(in)))+intercept;
                        end
                        
                    end
                end
                
            end

        end
        %Quadrant = 2 or 3, need to
        
        %        exclusion_zone_x = [shield(i,1) shield(i,2) shield(i,2)+sign(shield_locations_x(index)-nodes(j,1))*shift ...
        %             shield(i,1)+sign(shield_locations_x(index)-nodes(j,1))*shift shield(i,1)];
        %
        %        exclusion_zone_y = [shield(i,3) shield(i,4) shield(i,4)+sign(shield_locations_y(index)-nodes(j,2))*shift shield(i,3)+sign(shield_locations_y(index)-nodes(j,2))*shift  shield(i,3)];
        
        
        %       if shield_locations_x(1) < nodes(j,1) %barrier left of the node
        %           x_circle(x_circle < shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y)) = shield_locations_x(x_circle < shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y));
        %       else %barrier to right of node
        %                     x_circle(x_circle > shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y)) = shield_locations_x(x_circle > shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y));
        %       in = inpolygon(shield_locations_x,shield_locations_y,x_circle,y_circle);
        %       theta = zeros(1,length(in));
        %       theta = atan(((shield_locations_y(in)-nodes(j,2))./(shield_locations_x(in)-nodes(j,1))));
        % %       theta(shield_locations_x(in) < nodes(j,1)) = pi - theta;
        % theta(theta<0) = pi+theta(theta<0);
        %       n1= max(1,ceil(min(theta)*100/(2*pi)));
        %       n2= min(100,floor(max(theta)*100/(2*pi)));
        %       x_circle(n1:n2) = linspace(min(shield_locations_x(in)),max(shield_locations_x(in)),n2-n1+1)
        %        y_circle(n1:n2) = linspace(min(shield_locations_y(in)),max(shield_locations_y(in)),n2-n1+1)
        
    end
            heatmap(1,1+100*(j-1):100*(j-1) + 100) = x_circle;
            heatmap(2,1+100*(j-1):100*(j-1) + 100) = y_circle;
            plot(heatmap(1,1+100*(j-1):100*(j-1) + 100),heatmap(2,1+100*(j-1):100*(j-1) + 100),'-k')
            hold on
            patch('Faces',1:100,'Vertices',[heatmap(1,1+100*(j-1):100*(j-1) + 100) ;heatmap(2,1+100*(j-1):100*(j-1) + 100)]','FaceColor','red','FaceAlpha',.3)
end
end