%create multiframe array
% need MxNx1xK array from MxNxK array
function myMFarr = stack2MFarray( stack )
tic
height = size( stack, 1);
width = size( stack, 2);
frames = size(stack, 3);
myMFarr = zeros(height, width, 1, frames, 'uint8');
for f = 1:frames
    myMFarr(:,:,1,f) = stack(:,:,f);
end
disp(append('stack2MFarray: ', num2str(toc) ));
end
