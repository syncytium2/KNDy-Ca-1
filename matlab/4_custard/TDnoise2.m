function output = TDnoise2(data,dt,source)
% returns various noise estimates
% output.keys = { "SD", "diffSD", "MLspike_sigmaest", "CaImAn_Sn", "cascade" };
% returns a single value, or a struct containing all estimates
if nargin==1
    dt=0.1;
    source="all";
end
if nargin==2
    source="all";
end
if size(data,1)==1
    data=data';
end

switch source
    case "SD"
        output=std(data);
    case "diffSD"
        output=std(diff(data));
    case "MLspike_sigmaest"
        output=spk_autosigma(data,dt);
    case "CaImAn_Sn"
        output = GetSn(data);
    case "cascade" % lower than other estimates by a factor of 3, not sure why
        frame_rate = 1 / dt;          
        output = median( abs( diff( data ) ), 'omitmissing' ) / sqrt( frame_rate ); 
        % note * 100 to match cascade code
    case "all"
        % simple noise measures
        output.keys = {"SD","diffSD","MLspike_sigmaest","CaImAn_Sn"}; % ,"cascade" };
        output.SD = std(data);
        output.diffSD = std(diff(data));
        % MLspike noise
        output.MLspike_sigmaest = spk_autosigma(data,dt);
        % CaImAn noise
        output.CaImAn_Sn = GetSn(data);
        % % cascade
        % frame_rate = 1 / dt;          
        % output.cascade = median( abs( diff( data ) ), 'omitmissing' ) / sqrt( frame_rate );
end


