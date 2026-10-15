function im8bit = dbl2uint8( stack )
tic
im8bit = uint8(255 * mat2gray(stack));
[premin, premax] = minmax(stack);
[postmin, postmax] = minmax(im8bit);
disp(append('pre min max: ', num2str(premin), ' , ', num2str(premax)))
disp(append('post min max: ', num2str(postmin), ' , ',num2str(postmax)))
disp(append('dbl2uint8: ', num2str(toc)) );
end