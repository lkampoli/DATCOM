% This script will plot the sumd aero coefficients to confirm that thier
% summation was completed accurrately
% Scrip requries MultiLinePlot
% Started 26.11.19

clear
clc

% Import DATCOM_Output_ConfigX_Sumd.xlsx
OutputSumd = xlsread('DATCOM_OUTPUT_X_Sumd.xlsx', 'A1:H13200');
% Declare variables for columns
CNCol = 6;  % Column for CN's
CACol = 7;  % Column for CA's
CMCol = 8;  % Column for CM's

% Import MachList from DATCOM run
MachList = xlsread('DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx','F2:F21');          % MachList for Config0700004 - Trade Study Round.xlsx
AltList = xlsread('DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx','G2:G16');           % AltList for Config0700004 - Trade Study Round.xlsx
XSeg = xlsread('DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx','H2:H28');              % XSeg for Config0700004 - Trade Study Round.xlsx

% Begin extracting data to plot it 
PArray_CN = zeros(length(MachList), length(XSeg) + 1);                     % Create empty array for plotting, + 1 on column number to account for MachList column
PArray_CN(:,1) = MachList;                                                 % Set first column of PArray to MachList
PArray_CA = zeros(length(MachList), length(XSeg) + 1);                     % Create empty array for plotting, + 1 on column number to account for MachList column
PArray_CA(:,1) = MachList;                                                 % Set first column of PArray to MachList
PArray_CM = zeros(length(MachList), length(XSeg) + 1);                     % Create empty array for plotting, + 1 on column number to account for MachList column
PArray_CM(:,1) = MachList;                                                 % Set first column of PArray to MachList

% Find number of rows where AltList = 12500
LA12500 = sum(OutputSumd(:,1)==12500);      % NO. of rows for an Alt block
% Find starting row for Alt = 12500
PosAlt12500 = AltList == 12500;                                           % Find where AltList = 12500, set to 1, and store as logical array
Pos = 0;                                                                  % Initially declare Pos value
for index = 1:length(AltList)       % Loop over AltList
    if PosAlt12500(index) == 0      % if value in logical array PosAlt12500 == 0 increment Pos by 1
        Pos = Pos + 1;
    else
        break                       % break out of loop when PosAlt12500 ~= 0
    end
end
% Declare start row value
RStart = Pos*LA12500 + 1;                          % Start row for Alt = 12500 

% Fill PArray_CN
index = 1;                                         % declare index variable and set to 1
for iii = 1:length(MachList)                       % loop over MachList
    R1 = RStart + index*(2*length(XSeg));          % Calculate start row where AoA = 2 for Alt = 125000 using Rstart
    R2 = R1 + length(XSeg) - 1;                    % Calculate end row where AoA = 2 for Alt = 125000 using R1
    PArray_CN(iii,2:end) = OutputSumd(R1:R2,CNCol);   % Fill columns of PArry with Output(R1:R2,CACol)
    index = index + 2;                             % Increment index by 2
end

% Fill PArray_CA
index = 1;                                         % declare index variable and set to 1
for iii = 1:length(MachList)                       % loop over MachList
    R1 = RStart + index*(2*length(XSeg));          % Calculate start row where AoA = 2 for Alt = 125000 using Rstart
    R2 = R1 + length(XSeg) - 1;                    % Calculate end row where AoA = 2 for Alt = 125000 using R1
    PArray_CA(iii,2:end) = OutputSumd(R1:R2,CACol);   % Fill columns of PArry with Output(R1:R2,CACol)
    index = index + 2;                             % Increment index by 2
end

% Fill PArray_CM
index = 1;                                         % declare index variable and set to 1
for iii = 1:length(MachList)                       % loop over MachList
    R1 = RStart + index*(2*length(XSeg));          % Calculate start row where AoA = 2 for Alt = 125000 using Rstart
    R2 = R1 + length(XSeg) - 1;                    % Calculate end row where AoA = 2 for Alt = 125000 using R1
    PArray_CM(iii,2:end) = OutputSumd(R1:R2,CMCol);   % Fill columns of PArry with Output(R1:R2,CACol)
    index = index + 2;                             % Increment index by 2
end



% Plot CNs for Alt = 12500 & AoA = 2 Deg
figure('Name','CN Data from Output Array')
MultiLinePlot(PArray_CN)                           % Call MultiLinePlot to plot data with distict colours
title ('CNs from OutputSumd Array')                % Create plot title
xlabel('Ma No.')                                   % Create x label
ylabel('C_N')                                      % Create y label
legend('Location','NorthEast')                     % Turn on Legend
grid on

% Plot CAs for Alt = 12500 & AoA = 2 Deg
figure('Name','CA Data from Output Array')
MultiLinePlot(PArray_CA)                           % Call MultiLinePlot to plot data with distict colours
title ('CAs from OutputSumd Array')                % Create plot title
xlabel('Ma No.')                                   % Create x label
ylabel('C_A')                                      % Create y label
legend('Location','NorthEast')                     % Turn on Legend
grid on

% Plot CMs for Alt = 12500 & AoA = 2 Deg
figure('Name','CM Data from OutputSumd Array')
MultiLinePlot(PArray_CM)                           % Call MultiLinePlot to plot data with distict colours
title ('CMs from OutputSumd Array')                % Create plot title
xlabel('Ma No.')                                   % Create x label
ylabel('C_M')                                      % Create y label
legend('Location','NorthEast')                     % Turn on Legend
grid on