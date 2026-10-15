% load minimum of data to allow ROI selection
function S = loadFlatStackOpt(m,field)
    % m = matfile(fullpath2file);
    % m is the result of a call to matfile

    % not loading stack from flat file, too slow
    %S.stack = single(m.stack);

    S.timing = m.timing; % convert to sec
    S.interval = m.interval; % convert to sec
    S.name = m.name;
    %S.filename = m.filename; % complete.mat
    %S.ets = m.ets; % location of ETS file!
    varlist = who(m);
    index = find(strcmp(varlist,field), 1);
    if(isempty(index))
        disp(append("NO clean stack! ", field))
        whos(m)
    else
        S.(field) = m.(field);
    end
    field2 = 'hROI';
    index = find(strcmp(varlist,field2), 1);
    if(isempty(index))
        disp(append("NO clean stack! ", field2))
        whos(m)
    else
        S.(field2) = m.(field2);
    end
end