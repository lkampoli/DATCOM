clc
clear all
close all
% Uploaded to GIT BLAH 

% Required functions & scripts
% - Build_for005.m
% - read042.m
% - filllhcol.m     - Creates output array for results
% Continued 11.11.19 converted back to MATLAB. Removed interpolation as it
% was not being used.
% Extend plotting of coefficients from Output Array to plot CAs & CMs

% TODO assess adding abs() to maintain direction of ca's 10.12.19

% Comments 21.5.19
%{
Code given to Mech Dept. 21.5.19

Rocket has be sized according to the last sizes received in terms of X and R.
The X values (in XList) define the segment breakpoints. 
Essentially a bunch of Missile DATCOM folders as built up with each file defined as one X segment longer than the previous until the entire length of the rocket is reached. 
Then the ith rocket is subtracted from i-1 for CN and CA to get the contribution due to the ith rocket segment.
The data is then stored for the ith segment into the griddedInterpolant class accessed with CoeffsIndiv(ijk).CN_GI(AoA,Altitude,Mach)

Does not calculate center of pressure for each segment, can estimate as about halfway between segments
X breakpoints can be altered to suit pyhsical configuration via editting the Xlist.  
However as the rocket changes base diameter you’ll need to go into Build_for005.m and change SRef.
AltList & MachList are also located in Build_for005.m - code is NOT setup to alter length of these vectors
%}

% Comments 17.9.19
%{
Data to calculate coeffs for Config0700004 - Trade Study Round 2 data will be read from the .xlsx file: DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx

!!! Adjust read in row number to include all required data from spread sheet !!!

%}
                                                                % !!! CHANGE FILE NAME TO A UNIQUE IDENTIFIER !!!
% Read data from excel sheet
XList = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','A2:A28');                   % XList for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
RList = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','B2:B28');                   % RList for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
Discon = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','C2:C6');                   % Discon for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
LRef = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','D2:D2');                     % LRef for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
SRef = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','E2:E2');                     % SRef for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
MachList = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','F2:F21');                % MachList for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
AltList = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','G2:G16');                 % AltList for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx
AoAList = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','I2:I5');                  % AoAList for DATCOM_Inputs_Config1800000_ERIS300_01.xlsx

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
        MachList, AltList, AoAList,LRef,SRef); % Building input cards for DATCOM - XCG & ZCG locations defaulted/entered as zero therefore reference point is at nose tip 
    
    copyfile('MD0311.exe',['Run_' num2str(ijk) '\']);                                       % Copy DATCOM executable into created directory
    
    cd(['Run_' num2str(ijk) '\']);                                                          % Enter created directory for DATCOM run
    
    system('MD0311');                                                                       % Run DATCOM executable
    
    cd('..');                                                                               % Go back a directory
    
    Coeffs(iii) = read042(15,[],['Run_' num2str(ijk)]);                                     % Read and store coefficients from DATCOM output file
    
    iii=iii+1;                                                                              % Index index variable
    
end

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
    CoeffsIndiv(ijk).CA_GI = CoeffsIndiv(ijk).ca;                          % .ca to CoeffsIndiv - TODO assess adding abs() to main direction of ca's 10.12.19
    CoeffsIndiv(ijk).CM_GI = CoeffsIndiv(ijk).cm;                          % .cm to CoeffsIndiv    
end

% Extract coeffs and store in Output for pasting into aero loading calculator spreadsheet 
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
            i = i + length(XListEditted);                                   % index i by length(XListEditted)
        end
    end
end

% Add DummyArray with filled with aero coefficient to Output array
Output(:,CNCol:CMCol) = DummyArray(:,CNCol:CMCol);              % Fill coefficient coloumns of Output with coefficients
% Save Output as a CSV file
xlswrite('DATCOM_Outputs_Config1800000_ERIS300_01.xlsx', Output);          % !!! CHANGE X TO A UNIQUE IDENTIFIER !!!

% Extract Coefficients with original method to confirm correct construction of Output array
% Coefficients are extracted via original method for Alt = 12500 and AoA = 2 (for AoAList = [0 1 2 3])
CAs_12500_AoA2 = zeros(length(MachList),length(XList) - 1);                % Create empty vector of zeros
CAs_12500_AoA2(:,1) = MachList;                                            % Set first column of CAs_12500_AoA2 equal MachNos
CNs_12500_AoA2 = zeros(length(MachList),length(XList) - 1);                % Create empty vector of zeros
CNs_12500_AoA2(:,1) = MachList;                                            % Set first column of CNs_12500_AoA2 equal MachNos
CMs_12500_AoA2 = zeros(length(MachList),length(XList) - 1);                % Create empty vector of zeros
CMs_12500_AoA2(:,1) = MachList;                                            % Set first column of CMs_12500_AoA2 equal MachNos

% Write indivdual coefficients (for Alt = 12500 and AoA = 2 (for AoAList = [0 1 2 3])) to the array Cis_12500_AoA2 
for index = 2:length(XList)-1 
  CNs_12500_AoA2(:,index) = CoeffsIndiv(index).CN_GI(3,:,7);               % Fill CNs_12500_AoAs with CoeffsIndiv(index).CN_GI(3,:,7);
  CAs_12500_AoA2(:,index) = CoeffsIndiv(index).CA_GI(3,:,7);               % Fill CAs_12500_AoAs with CoeffsIndiv(index).CA_GI(3,:,7);
  CMs_12500_AoA2(:,index) = CoeffsIndiv(index).CM_GI(3,:,7);               % Fill CMs_12500_AoAs with CoeffsIndiv(index).CM_GI(3,:,7);
end

% Plot coefficients extracted from original method for Alt = 12500 and AoA = 2 (for AoAList = [0 1 2 3])
% Plot CNs
figure('Name','CNs from DATCOM original method')                           % Create figure window called 'Cis from DATCOM original method'
plot(CNs_12500_AoA2(:,1),CNs_12500_AoA2(:,2:end))                          % plot CN's verse Mach No.
title ('CNs from DATCOM original method')                                  % Create plot title
xlabel('Ma No.')                                                           % Create x label
ylabel('C_N')                                                              % Create y label
legend('Location','NorthEastOutside')                                      % Turn on Legend
grid on                                                                    % Turn on grid
% Plot CAs
figure('Name','CAs from DATCOM original method')                           % Create figure window called 'Cis from DATCOM original method'
plot(CAs_12500_AoA2(:,1),CAs_12500_AoA2(:,2:end))                          % plot CA's verse Mach No.
title ('CAs from DATCOM original method')                                  % Create plot title
xlabel('Ma No.')                                                           % Create x label
ylabel('C_A')                                                              % Create y label
legend('Location','NorthEastOutside')                                      % Turn on Legend
grid on                                                                    % Turn on grid
% Plot CMs
figure('Name','CMs from DATCOM original method')                           % Create figure window called 'Cis from DATCOM original method'
plot(CMs_12500_AoA2(:,1),CMs_12500_AoA2(:,2:end))                          % plot CA's verse Mach No.
title ('CMs from DATCOM original method')                                  % Create plot title
xlabel('Ma No.')                                                           % Create x label
ylabel('C_M')                                                              % Create y label
legend('Location','NorthEastOutside')                                      % Turn on Legend
grid on                                                                    % Turn on grid

% Plot coefficients from constructed Output Array for comparison against coefficients extracted from original method
PArray = zeros(length(MachList), length(XList));                           % Create empty array for plotting
PArray(:,1) = MachList;                                                    % Set first column of PArray to MachList

% Find number of row where AltList = 12500
LA12500 = sum(Output(:,1)==12500);      % NO. of rows for an Alt block
% Find starting row for Alt = 12500
PosAlt12500 = AltList == 12500;                                            % Find where AltList = 12500 to 1 and store as logical array
Pos = 0;                                                                   % Initially declare Pos value
for index = 1:length(AltList)       % Loop over AltList
    if PosAlt12500(index) == 0      % if value in logical array PosAlt12500 == 0 increment Pos by 1
        Pos = Pos + 1;
    else
        break                       % break out of loop when PosAlt12500 ~= 0
    end
end
% Declare start row value
RStart = Pos*LA12500 + 1;                          % Start row for Alt = 12500 
index = 1;                                         % declare index variable and set to 1
for iii = 1:length(MachList)                       % loop over MachList
    R1 = RStart + index*(2*length(XListEditted));  % Calculate start row where AoA = 2 for Alt = 125000 using Rstart
    R2 = R1 + length(XListEditted) - 1;            % Calculate end row where AoA = 2 for Alt = 125000 using R1
    PArray(iii,2:end) = Output(R1:R2,CACol);       % Fill columns of PArry with Output(R1:R2,CACol)
    index = index + 2;                             % Increment index by 2
end

% Plot CAs extracted from original method for Alt = 12500 and AoA = 2 (for AoAList = [0 1 2 3])
figure('Name','Data from Output Array')
plot(PArray(:,1),PArray(:,2:end))
title ('CAs from Output Array')                    % Create plot title
xlabel('Ma No.')                                   % Create x label
ylabel('C_A')                                      % Create y label
legend('Location','NorthEastOutside')              % Turn on Legend
grid on
