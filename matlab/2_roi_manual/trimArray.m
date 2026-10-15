%trim frames
function [ trimmed, tTarray ] = trimArray( array, tarray, startindex, endindex ) 
tic 
trimmed = array;
tTarray = tarray;
trimmed(:,:, startindex : endindex )=[];
tTarray(:,startindex : endindex )=[];
str = append('trimming took ', num2str(toc), ' seconds' );
disp(str);
end