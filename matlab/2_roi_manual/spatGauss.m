% calculate mean frame F0 dFoF0
function gaussStack = spatGauss(stack,sigma,npixels)
% assumes stack is height x width x frames
tic
height = size( stack, 1);
width = size( stack, 2);
frames = size( stack, 3);
gaussStack = zeros( height, width, frames, class(stack) );
for f = 1 : frames
    frame = stack(:,:,f);
    gaussf = imgaussfilt( frame, sigma, 'FilterSize', npixels );
    gaussStack(:,:,f) = gaussf;
end
disp(append('gaussian filter took ...',num2str(toc), ' seconds'));
end