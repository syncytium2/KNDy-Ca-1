function S = openETSui2()
% open dataset
%ets = '/Users/defazio/Documents/ciatah/TDciatah/data/_20220210a_04_/stack1/frame_t_0.ets';
%name = '20220210_a_m0_kiss.h5';
    % fig = uifigure;
    % d = uiprogressdlg( fig, 'Title','reading ETS','Indeterminate', 'on');
    % drawnow


    [file,path] = uigetfile('*.ets');
    if isequal(file,0)
       disp('User selected Cancel');
       return;
    else
       disp(['User selected ', fullfile(path,file)]);
    end
    ets = fullfile( path, file );
    exp = '\d{8}[a-z]_\d*';
    name = regexp(ets,exp,'match','once');
    % prompt = 'name the dataset, datecode_letter_m0_trt';
    % name = input( prompt, "s" );
    % k=strfind(ets,'_');
    % name = ets( k(2)+1 : k(4)-1 ); %file(1:end-4);
    % %
    disp('master: opening file and extracting image stack');
    tic
    data = bfopen(ets); % creates cell array, frames x 2
    %toc
    % get the frame interval from original metadata
    %interval = bfinterval( data );
    metadata = data{ 1, 2 };
    [timing,interval] = bfTime( metadata );

    % move full image stack to array
    series1 = data{ 1, 1 }; % move cell containing frame cells
    height = size( series1{ 1, 1 }, 1 );
    width = size( series1{ 1, 1 }, 2 );
    frames = size( data{ 1, 1 }, 1);
    stack = zeros(height, width, frames, 'uint16');
    %tic
    for p = 1 : frames
         series1c = series1{ p, 1 }; % moves a frame to temp variable
         stack( :, :, p ) = series1c; 
    end
%[ stack, timing, interval, name, ets, metadata ]
    S.stack = stack;
    S.timing = timing/1000; % convert ms to seconds
    S.interval = interval; % convert ms to seconds
    S.name = name;
    S.ets = ets;
    S.metadata = metadata;
    
    toc
end