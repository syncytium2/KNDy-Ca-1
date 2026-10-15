%test resize
function downsizeStack = tdResize( stack, box )
tic
pmRaw = projectMax( stack );
pmRaw = histeq(pmRaw);
downsizeStack = arrayResize(stack, box );
pmds = projectMax( downsizeStack );
pmds = histeq(pmds);
% figure
% montage({pmRaw pmds});
toc
end