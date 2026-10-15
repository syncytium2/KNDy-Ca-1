% function to load base and roi info from user selected complete.mat file
function S = loadComplete3( complete )
% complete is the full path to the file

% converts to single!
% does not completely load complete! see specific variables loaded below
    % [file,path] = uigetfile('*complete.mat');
    % if isequal(file,0)
    %    disp('User selected Cancel');
    %    return;
    % else
    %    disp(['User selected ', fullfile(path,file)]);
    % end
    % complete = fullfile( path, file );
    % %exp = '\d{8}[a-z]_\d*';
    % %name = regexp(complete,exp,'match','once');
    disp(append('master: opening file and extracting image stack: ', complete));
    temp = load( complete ); %, "base", "hROI" ); %, "base", "");

    if(isfield(temp,'base')) % ancient format
        S.stack = single(temp.base);
        S.timing = temp.timing / 1000; % convert to sec
        S.interval = temp.baseInterval / 1000; % convert to sec
        S.name = temp.baseName;
        S.filename = temp.filename; % complete.mat
        S.ets = temp.baseETS;
        %S.metadata = temp.metadata;
        S.hROI = temp.hROI;
        %S.group = temp.group;
    else
        if(isfield(temp,'S')) % modern format, pre "save -struct"
            % use new format
            if isfield(temp.S,'stack')
                S.stack = single(temp.S.stack);
                S.timing = temp.S.timing; % convert to sec
                S.interval = temp.S.interval; % convert to sec
                S.name = temp.S.name;
                S.filename = complete; % temp.S.filename; % complete.mat
                S.ets = temp.S.ets; % location of ETS file!
                %S.metadata = temp.metadata;
                S.hROI = temp.S.hROI;
                if(isfield(temp.S,'Smoco'))
                    S.Smoco = temp.S.Smoco;
                end
            else
                if isfield(temp.S,'base') % intermediate format
                    S.stack = single(temp.S.base);
                    S.timing = temp.timing; % convert to sec
                    S.interval = temp.interval; % convert to sec
                    S.name = temp.name;
                    S.filename = temp.filename; % complete.mat
                    S.ets = temp.ets; % location of ETS file!
                    %S.metadata = temp.metadata;
                    S.hROI = temp.S.hROI;
                    if(isfield(temp.S,'Smoco'))
                        S.Smoco = temp.S.Smoco;
                    end
                end
            end
        else
            % check for latest format, save -struct, flattened file
            % structure
            if isfield(temp,"stack")
                S.stack = single(temp.stack);
                S.timing = temp.timing; % convert to sec
                S.interval = temp.interval; % convert to sec
                S.name = temp.name;
                %S.filename = temp.filename; % complete.mat
                S.ets = temp.ets; % location of ETS file!
                %S.metadata = temp.metadata;
                S.hROI = temp.hROI; % hidden in here are three sets of ROIs
                if(isfield(temp,'Smoco'))
                    S.Smoco = temp.Smoco;
                end
            else
                disp('failed to locate stack!')
                S.stack=[];
            end
        end
    end
    disp("load complete in app")
end