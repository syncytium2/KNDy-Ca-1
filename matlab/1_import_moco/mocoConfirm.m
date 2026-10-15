function mocoConfirm( fig, stack, smoco_stack, name )
    nframes = size(stack,3);
    step_size = floor(nframes/3);
    im1 = MeanMaxSTD( stack(:,:,1:step_size), 'mean' );
    im2 = MeanMaxSTD( stack(:,:,step_size:2*step_size), 'mean' );
    im3 = MeanMaxSTD( stack(:,:,2*step_size:3*step_size), 'mean' );
      
    % scale each image
    bounds = round(0.1 * max(size(im1)));
    nim1 = normArray(im1, bounds);
    nim2 = normArray(im2, bounds);
    nim3 = normArray(im3, bounds);
    imRGBraw = cat(3,nim3,nim2,nim1);

    im1 = MeanMaxSTD( smoco_stack(:,:,1:step_size), 'mean' );
    im2 = MeanMaxSTD( smoco_stack(:,:,step_size:2*step_size), 'mean' );
    im3 = MeanMaxSTD( smoco_stack(:,:,2*step_size:3*step_size), 'mean' );
      
    % scale each image
    bounds = round(0.1 * max(size(im1)));
    nim1 = normArray(im1, bounds);
    nim2 = normArray(im2, bounds);
    nim3 = normArray(im3, bounds);
    imRGBmoco = cat(3,nim3,nim2,nim1);

    h=fig;
    tl=tiledlayout(1,2);
    title(tl,append(name, ' frames: ',num2str(step_size)," : RED ",'end'," GREEN ",'middle'," BLUE ", 'start' ),'interpreter','none')
    t1=nexttile;
    title(t1,append(name,' raw'),'interpreter','none')
    imshow(imRGBraw,InitialMagnification='fit')
    t2=nexttile;
    imshow(imRGBmoco,InitialMagnification='fit')
    title(t2,append(name,' moco'),'interpreter','none')
    pdfname=append(name,'_mocoConfirm.pdf');
    exportgraphics(h,pdfname,'Append',true)



