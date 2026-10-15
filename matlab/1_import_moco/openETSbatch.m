function S = openETSbatch( full_path_filename )
% open dataset

    ets = full_path_filename; % fullfile( path, file );
    exp = '\d{8}[a-z]_\d*';
    name = regexp(ets,exp,'match','once');

    data = bfopen(ets); % creates cell array, frames x 2
    metadata = data{ 1, 2 };
    [timing,interval] = bfTime( metadata );

    % move full image stack to array
    series1 = data{ 1, 1 }; % move cell containing frame cells
    height = size( series1{ 1, 1 }, 1 );
    width = size( series1{ 1, 1 }, 2 );
    frames = size( data{ 1, 1 }, 1);
    
    stack = zeros(height, width, frames, 'uint16');

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

end