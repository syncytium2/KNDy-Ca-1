% subtract min value from each frame
function [nst, Smin] = subMinStack( st )
% assumes 3rd 
%Smean = squeeze(mean(st, [1 2]));
%Smin = squeeze( min( st, [], [1 2] ) );
sz = size( st );
Smin = squeeze(zeros(1,sz(3)));
nst = zeros(sz);
for i=1:sz(3)

   % frame = squeeze( st( :, :, i) );
      Smin(i) =  min( st( :, :, i), [], "all" );
    %Smin(i) = quantile( frame, 0.005, "all" );
    nst(:,:,i) =  st( :, :, i) - Smin(i) ; % Smin(i) ;
    % [localmin localmax] = bounds( nst, 'all');
    % if localmin < 0
    %     disp("WTF?")
    % end
end

end