function coefs = read042(nBlocks,c, varargin)
% This function reads the for042.dat DATCOM output file
%

if nargin <= 2
    DirName = '.';
else
    DirName = varargin{1};
end

FileName = 'for042.csv';

File = [DirName filesep FileName];


% read the first line in to parse
fid = fopen(File);                                                              
line1 = fgetl(fid);
line2 = fgetl(fid);
fclose(fid);

CommaLoc = strfind(line1,',');
nCommas = length(CommaLoc);

for n = 1:2:(nCommas-1)
    var=strtrim(line1(CommaLoc(n)+1:CommaLoc(n+1)-1));
    eval(lower(['coefs.',var(2:end-1),'=',line1(CommaLoc(n+1)+1:CommaLoc(n+2)-1),';']))
end
coefs.ndelta = nBlocks;

% need to make this statement general for any number of Machs, Alphas and Betas
%data=csvread(File,0,0,[2,0,1+coefs.nalpha*coefs.nmach,coefs.nvars]);
for nB = 1 : nBlocks
    rStart = 2 +(2+coefs.nalpha*coefs.nmach)*(nB-1);
    rEnd = rStart + coefs.nalpha*coefs.nmach - 1;
    cStart = 0;
    cEnd = coefs.nvars-1;
    data(:,:,nB) =dlmread(File,',',[rStart, cStart, rEnd, cEnd]);
end

% parse headers and data into coefs structure

% first use a regular expression to parse the data between single quotes
% into a cell atring
subchunk = regexp(line2, '(?<='')[^'']+(?='')', 'match');
VarNames =  lower(subchunk(1:2:end));
for n = 1 : coefs.nvars
    v = strrep(VarNames{n},'?','');
    v = strrep(v,'/','_over_');
    v = strrep(v,'-','');
    v = strrep(v,'.','');
    coefs.(v)=reshape(squeeze(data(:,n,:)),coefs.nalpha,coefs.nmach,coefs.ndelta);
    % eval(lower(['coefs.',v,'=squeeze(data(:,n,:))']))
    if ~isempty(c)
        c.(v)(:,:,:,end+1) = coefs.(v)(:,:,:,1);
    end
end
if ~isempty(c)
    coefs = c;
end

end

