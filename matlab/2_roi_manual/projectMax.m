function MaxProj = projectMax( stack )
% max projection, sd projection?
tic
MaxProj = max( stack, [], 3 );
%MaxProj2 = histeq( MaxProj );

disp(append('projectMax took ',num2str(toc),' seconds'));
end
