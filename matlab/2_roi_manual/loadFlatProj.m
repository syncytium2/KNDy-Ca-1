% load minimum of data to allow ROI selection
function S = loadFlatProj(m)
    varlist = who(m);
    % m is the result of a call to matfile

    S.timing = m.timing; % convert to sec

    index = find(strcmp(varlist,'interval'), 1);
    if isempty(index)
        m.interval = mean(diff(S.timing));
        disp(append("NO interval, recreating: ",num2str(m.interval)))
    end
    S.interval = m.interval; % convert to sec

    %S.ets = m.ets; % location of ETS file!
    index = find(strcmp(varlist,'name'), 1);
    if isempty(index)
        exp = '\d{8}[a-z]_?\d*';
       m.name = regexp(S.ets,exp,'match','once');
       disp(append("NO name! recreating: ", m.name))
    end
    S.name = m.name;
   
    %S.filename = m.filename; % complete.mat
    %S.ets = m.ets; % location of ETS file!
    %S.metadata = m.metadata;

    index = find(strcmp(varlist,'hROI'), 1);
    if isempty(index)
        %disp("NO hROI FOR YOU!")
    else
        S.hROI = m.hROI; % new flat files, hROI.(projection_type) to reach ROIs
    end
    index = find(strcmp(varlist,'projections'), 1);
    if(isempty(index))
        disp("NO PROJECTIONS!")
        %S.projections = m.Smoco.projections;
    else
        S.projections = m.projections; % new flat files have precalculated projections.moco.mean/max/std
    end
     % not loading smoco from flat file, too slow
    % if(isfield(m,'Smoco'))
    %     S.Smoco = m.Smoco;
    % end
end