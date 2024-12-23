% This script will run DATCOM from inputs compiled from in a .xlsx file

% Required functions & scripts
% - Build_for005.m
% - read042.m
% - filllhcol.m     - Creates output array to hold results with Alt, Mach & AoA filled in
% - ReadInputFile.m - Class required to read input .txt file
% - MD0311.exe      - Missile DATCOM executable

%%-------------------- Section 1: Importing Data ------------------------%%

% Read data from .txt file sheet
INfname = input('Enter .txt input file name:    ', 's');                   % Request .txt input file name from user .'s' required to prevent evaluating input  
DATin = ReadInputFile(INfname);                                            % Call ReadInputFilet to read in data from .txt file
% Create required variables from imported data
XList = rmmissing(DATin.data.XList);         % [m]   - XList from input file, rmmissing() removes any NaN's in the array
RList = rmmissing(DATin.data.RList);         % [m]   - RList from input file, rmmissing() removes any NaN's in the array
Discon = rmmissing(DATin.data.Discon);       % [m]   - Discon from input file
LRef = rmmissing(DATin.data.LRef);           % [m]   - LRef from input file
SRef = rmmissing(DATin.data.SRef);           % [m^2] - SRef from input file
MachList = rmmissing(DATin.data.MachList);   % [-]   - MachList from input file
AltList = rmmissing(DATin.data.AltList);     % [m]   - AltList from input file
AoAList = rmmissing(DATin.data.ALPHA);       % [Deg] - AoAList from input file
XSeg = rmmissing(DATin.data.X_Seg);          % [m]   - Read from input file

%%-------------------- Section 2: Running DATCOM ------------------------%%

% Declare index variable
iii = 1;                                                                   % Index variable

% The loop below runs DATCOM for the stations declared by XList, RList, Discon, LRef & SRef at the conditions listed in MachList, AltList & AoAList.
% For each run the next station is added until the entire vehicle is simulated in the final run. This is defined by the inputs for the Build_for005 function
for ijk = 1:length(XList)-1
   % if exist(['Run_' num2str(ijk)])       % This if statement is a safety measure to ensure that directories are not writted over. Commented out because it requests user to rewrite directories 
       %  mdir(['Run_' num2str(ijk)],'s');   % Just commenting out this bit to get code to run through
   % end
    mkdir(['Run_' num2str(ijk)]);                                                           % Creating directory for DATCOM run

    Build_for005(['Run_' num2str(ijk) '\for005.dat'],XList(1:1+ijk),RList(1:1+ijk),Discon,...
        MachList, AltList, AoAList,LRef,SRef); % Building input cards for DATCOM in created directory- XCG & ZCG locations defaulted/entered as zero therefore reference point is at nose tip 
    
    copyfile('MD0311.exe',['Run_' num2str(ijk) '\']);                                       % Copy DATCOM executable into created directory
    
    cd(['Run_' num2str(ijk) '\']);                                                          % Enter created directory for DATCOM run
    
    system('MD0311');                                                                       % Run DATCOM executable
    
    cd('..');                                                                               % Go back a directory
    
    Coeffs(iii) = read042(15,[],['Run_' num2str(ijk)]);                                     % Read and store coefficients from DATCOM output file
    
    iii=iii+1;                                                                              % Index index variable
    
end

%%------------------ Section 3: Calculating Coeffs ----------------------%%

% Below codes created the first entry in CoeffsIndiv such that it can be subtracted in calculations for proceeding rocket stations
CoeffsIndiv = Coeffs(1);                    % Create struct CoeffsIndiv from Coeffs extract from for042.dat files
CoeffsIndiv(1).CN_GI = CoeffsIndiv(1).cn;   % Append CN_GI column to CoeffsIndiv 
CoeffsIndiv(1).CA_GI = CoeffsIndiv(1).ca;   % Append CA_GI column to CoeffsIndiv
CoeffsIndiv(1).CM_GI = CoeffsIndiv(1).cm;   % Append CM_GI column to CoeffsIndiv

% Loop through calculating ca, cn & cm for each segment and append to CoeffsIndiv   
for ijk = 2:length(XList)-1
    CoeffsIndiv(ijk).ca = Coeffs(ijk).ca - Coeffs(ijk-1).ca;               % Subtract ca_i from ca_i-1
    CoeffsIndiv(ijk).cn = Coeffs(ijk).cn - Coeffs(ijk-1).cn;               % Subtract cn_i from cn_i-1
    CoeffsIndiv(ijk).cm = Coeffs(ijk).cm - Coeffs(ijk-1).cm;               % Subtract cm_i from cm_i-1
    CoeffsIndiv(ijk).CN_GI = CoeffsIndiv(ijk).cn;                          % .cn  to CoeffsIndiv
    CoeffsIndiv(ijk).CA_GI = CoeffsIndiv(ijk).ca;                          % .ca to CoeffsIndiv - TODO assess adding abs() to maintain direction of ca's 10.12.19
    CoeffsIndiv(ijk).CM_GI = CoeffsIndiv(ijk).cm;                          % .cm to CoeffsIndiv    
end

%%----------------- Section 4: Compling Output Data ---------------------%%

% Extract coeffs and store in Output for pasting into aero loading
% calculator spreadsheet
NCol = 8;   % Number of columns to include in outout array
CNCol = 6;  % Column for CN's
CACol = 7;  % Column for CA's
CMCol = 8;  % Column for CM's
XListEditted(:) = XList((2:end),:);                                        % Remove first XList element equal to zero. (It vanishes because we execute c_i+1 - c_i) 
Output = filllhcol(AltList, MachList, AoAList, XListEditted, NCol);        % Call filllhcol to create output array to fill with aero coefficients

% Sixth Column will be filled with CN's
i = 0;
ii = 0;
% Create DummyArray to store CoeffsIndiv(:).CA_GI(1,1,1) - First entry in feild for each CN 
DummyArray = zeros(length(AltList)*length(MachList)*length(AoAList)*length(XListEditted),1); % DummyArray to hold values
for p = 1:length(AltList)                                                  % Loop over AltList        
    for o = 1:length(MachList)                                             % Loop over MachList                
        for m = 1:length(AoAList)                                          % Loop over AoAList
            for n = 1:length(XListEditted)                                 % Loop over XListEditted
                DummyArray(n + i,CNCol) = CoeffsIndiv(n).CN_GI(m,o,1);     % Fill element DummyArray(1,CNCol) with CoeffsIndiv(1).CN_GI(1,1,1)
                DummyArray(n + i,CACol) = CoeffsIndiv(n).CA_GI(m,o,1);     % Fill element DummyArray(1,CACol) with CoeffsIndiv(1).CA_GI(1,1,1)
                DummyArray(n + i,CMCol) = CoeffsIndiv(n).CM_GI(m,o,1);     % Fill element DummyArray(1,CMCol) with CoeffsIndiv(1).CM_GI(1,1,1)
            end
            i = i + length(XListEditted);                                  % index i by length(XListEditted)
        end
    end
end

% Add DummyArray with filled with aero coefficient to Output array
Output(:,CNCol:CMCol) = DummyArray(:,CNCol:CMCol);              % Fill coefficient coloumns of Output with coefficients

%%---------------- Section 5: Suming Vehicle Segments -------------------%%

% Find positions in XListEditted where XSeg == XListedEditted
XSegPos = zeros(length(XListEditted),length(XSeg));  % Create empty vector to hold positions of XSeg       
for i = 1:length(XSeg)
    XSegPos(:,i) = XListEditted == XSeg(i);  
end
% Sum together XSeg positions into one column
XSegPosSummed = XSegPos(:,1);
for i = 2:length(XSeg)
    XSegPosSummed = XSegPosSummed + XSegPos(:, i);
end
% Sum coefficients up to where XListEditted 
XSegPos1s = XSegPos;
m = 1;
% Set value of XListEditted to 1 for each segment to be summed together
for n = 1:length(XListEditted)   % Loop over AltList
    if XSegPos1s(n, m) == 0      % if nth value in XSegPos == 0, set = 1 
        XSegPos1s(n, m) = 1;       
    else
        m = m + 1;               % index m by 1 when nth value = 1 to index columns 
    end
end

% Loop to sum coefficients
ii = 0;
jj = 0;

for j = 1:length(Output)/length(XListEditted)
    for i = 1:length(XSeg)
        CiDataSumd = Output((1 + ii):length(XListEditted)+ii,:).*XSegPos(:,i);    % Keep Alt, Mach & AoA values in Output at first segment to sum too.
        OutputSumd(i + jj,:) = CiDataSumd(any(CiDataSumd,2),:);                   % cull rows equal to zero
      
        CNSumd = sum(Output((1 + ii):length(XListEditted)+ii,CNCol).*XSegPos1s(:,i)); % Sum CN Coefficient up for first segment
        CASumd = sum(Output((1 + ii):length(XListEditted)+ii,CACol).*XSegPos1s(:,i)); % Sum CA Coefficient up for first segment
        CMSumd = sum(Output((1 + ii):length(XListEditted)+ii,CMCol).*XSegPos1s(:,i)); % Sum CM Coefficient up for first segment
        OutputSumd(i + jj,CNCol) = CNSumd;                                            % Append CNSumd to OutputSumd((i + jj),CNCol)
        OutputSumd(i + jj,CACol) = CASumd;                                            % Append CASumd to OutputSumd((i + jj),CACol)
        OutputSumd(i + jj,CMCol) = CMSumd;                                            % Append CMSumd to OutputSumd((i + jj),CMCol)
    end
    ii = ii + length(XListEditted);                                        % Increment ii by length of XListEditted
    jj = jj + length(XSeg);                                                % Increment jj by length of XSeg
end

% The code below will create an array of xi Positions which is the center of the vehicle segments where the aero load will applied
XPos = zeros(size(XSeg));                                                  % Create empty array to fill
XPos(1) = XSeg(1)/2;                                                       % [m] - Fill first xi position with half the value of XSeg(1)

for i = 2:length(XSeg)
    XPos(i) = XSeg(i-1) + (XSeg(i) - XSeg(i-1))/2;                         % [m] - Fill XPos with xi value
end
% Call repmat to repeat XPos the correct number of times given the length of DATCOM inputs
xiPos = repmat(XPos,length(AltList)*length(MachList)*length(AoAList),1);   % [m] - xi Position for excel load calculator

% Add xiPos to OutputSumd
OutputSumd(:,5) = xiPos;                                                   % Set 5th column in OutputSumd to xiPos

%%-------------- Section 6: Write DATCOM Output to .txt -----------------%%

% Create Celleries for Column Names
ColNames = {'Altitude';'Mach';'AoA';'xi';'xi_center';'CN';'CA';'CM'};
ColUnits = {'meters' ;'-';'degrees';'meters';'meters';'-';'-';'-'};

% Prompt user for an output filename
OUTfname = input('Enter output filename:      ','s');                      % File name for file to create
fid = fopen(OUTfname, 'w+');                                               % Open file with write permissions

% Write column names into file\
fprintf(fid, '%s\t',ColNames{1:8});
fprintf(fid, '\n');                % Write newline

% Write columne units into file
fprintf(fid, '%s\t',ColUnits{1:8});
fprintf(fid, '\n');                % Write newline 

% Write data to file
for i = 1:length(OutputSumd)
    fprintf(fid, '%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t\n',OutputSumd(i,:));
end

fclose(fid);                       % close open file

%%----------------- Section 7: Move Run_ directories --------------------%%

RUN_dirname = input('Enter name for Run_ directories:   ', 's');           % Prompt user for a dir name which will store Run_ dir's
mkdir(RUN_dirname);                                                        % Create dir to store Run_ dir's
for i = 1:length(XList)-1
    movefile(['Run_' num2str(i)], RUN_dirname)
end
