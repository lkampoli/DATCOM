% The code will plot coeffs and stations from Config070004 - Trade Study Round 2
% Plotting coeffs per station utilises MultiLinePlot()
% Sums coeffs for segments & plot the results
% Required scripts & functions
% - MulitLinePlot.m
% - Create_CSV.m
% 20.9.19 - Script has been modified to take data from the excel files: 
% - CAs_MaxQ_AoA2.xlsx
% - CNs_MaxQ_AoA2.xlsx
% - CMs_MaxQ_AoA2.xlsx
% Editted to run in MATLAB 11.11.19

% clc                                                                           % Commented out to keep variables generated in RunAllDanny
% clear all                                                                     % Commented out to keep variables generated in RunAllDanny
% close all                                                                     % Commented out to keep variables generated in RunAllDanny

% Read in XList and RList to plot rocket stations                               % -> !!! Update file name and row numbers !!! 
XList = xlsread('DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx','A2:A28');% Read in XList
RList = xlsread('DATCOM_Inputs_Config0700004 - Trade Study Round 2.xlsx','B2:B28');% Read in RList

figure('name','Stations Config070004 - Trade Study Round 2')                    % Create new figure
plot(XList, RList, '--');                                                       % Plot config profile 
axis([XList(1) XList(length(XList)) 0 1])                                       % Set axes limits
hold on                                                                         % Hold figure window open
grid on                                                                         % Grid on  
for i=1:length(XList)
 line([XList(i) XList(i)], [RList(1) RList(i)],'linestyle','--')% Loop through & plot station divisions
end
title('Rocket Stations')                                                        % Create plot title
xlabel('Length [m]')                                                            % Create x label
ylabel('Radius [m]')                                                            % Create y label        
hold off                                                                        % Close hold on figure window

% Read in & plot CA data
CAs_MaxQ_AoA2 = xlsread('CAs_MaxQ_AoA2.xlsx');                                  % Read CA data

figure('name','CAs per station Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CAs_MaxQ_AoA2)                                                    % Plot data with MulitLinePlot
title('C_a Per Stations')                                                       % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_a')                                                                   % Create y label

% Read in & plot the CN data
CNs_MaxQ_AoA2 = xlsread('CNs_MaxQ_AoA2.xlsx');                                  % Read CN data

figure('name','CNs per station Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CNs_MaxQ_AoA2)                                                    % Plot data with MulitLinePlot
title('C_n Per Stations')                                                       % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_n')                                                                   % Create y label

% Read in & plot the CM data
CMs_MaxQ_AoA2 = xlsread('CMs_MaxQ_AoA2.xlsx');                                  % Read CM data

figure('name','CMs per station Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CMs_MaxQ_AoA2)                                                    % Plot data with MulitLinePlot
title('C_m Per Stations')                                                       % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_m')                                                                   % Create y label

% Summing up coeffs for rocket segments for each Ma no.
[nrows, cols] = size(CAs_MaxQ_AoA2);                                             % Determine size of CAs_MaxQ_AoA1
                                                                                % -> !!! Update Number of Segments !!!                                                 
nSegs = 11;                                                                     % Declare number of segments

CA_Segs = zeros(nrows,nSegs + 1);                                               % Create vector of zeros to store Ceoffs per Segment
CN_Segs = zeros(nrows,nSegs + 1);                                               % Create vector of zeros to store Ceoffs per Segment
CM_Segs = zeros(nrows,nSegs + 1);                                               % Create vector of zeros to store Ceoffs per Segment
CA_Segs( :, 1) =  CAs_MaxQ_AoA2( :, 1);                                         % Set Ma no. as first col in CA_Segs
CN_Segs( :, 1) =  CNs_MaxQ_AoA2( :, 1);                                         % Set Ma no. as first col in CN_Segs
CM_Segs( :, 1) =  CMs_MaxQ_AoA2( :, 1);                                         % Set Ma no. as first col in CM_Segs
                                            
% Loop through Ma no. and sum coeffs                                            % !!! -> Update column indices reflect rocket segments !!!
for i = 1:nrows
  CA_Segs(i,1 + 1) =  sum(CAs_MaxQ_AoA2(i,[2:12]));                             % Sum CAs per station for 'Nose' - 'Nose' is made of 11 stations
  CN_Segs(i,1 + 1) =  sum(CNs_MaxQ_AoA2(i,[2:12]));                             % Sum CNs per station for 'Nose'
  CM_Segs(i,1 + 1) =  sum(CMs_MaxQ_AoA2(i,[2:12]));                             % Sum CMs per station for 'Nose'
  
  CA_Segs(i,2 + 1) =  sum(CAs_MaxQ_AoA2(i,[13:13]));                            % Sum CAs per station for 'S3_Itr' - 'S3_Itr' is made of 3 stations
  CN_Segs(i,2 + 1) =  sum(CNs_MaxQ_AoA2(i,[13:13]));                            % Sum CNs per station for 'S3_Itr'
  CM_Segs(i,2 + 1) =  sum(CMs_MaxQ_AoA2(i,[13:13]));                            % Sum CMs per station for 'S3_Itr'
  
  CA_Segs(i,3 + 1) =  sum(CAs_MaxQ_AoA2(i,[14:14]));                            % Sum CAs per station for 'S3_Mcase' - 'S3_Mcase' is made of 1 stations
  CN_Segs(i,3 + 1) =  sum(CNs_MaxQ_AoA2(i,[14:14]));                            % Sum CNs per station for 'S3_Mcase'
  CM_Segs(i,3 + 1) =  sum(CMs_MaxQ_AoA2(i,[14:14]));                            % Sum CMs per station for 'S3_Mcase' 
  
  CA_Segs(i,4 + 1) =  sum(CAs_MaxQ_AoA2(i,[15:15]));                            % Sum CAs per station for 'S2_Con' - 'S2_Con' is made of 1 stations
  CN_Segs(i,4 + 1) =  sum(CNs_MaxQ_AoA2(i,[15:15]));                            % Sum CNs per station for 'S2_Con'
  CM_Segs(i,4 + 1) =  sum(CMs_MaxQ_AoA2(i,[15:15]));                            % Sum CMs per station for 'S2_Con'
  
  CA_Segs(i,5 + 1) =  sum(CAs_MaxQ_AoA2(i,[16:16]));                            % Sum CAs per station for 'S2_OxTk' - 'S2_OxTk' is made of 1 stations
  CN_Segs(i,5 + 1) =  sum(CNs_MaxQ_AoA2(i,[16:16]));                            % Sum CNs per station for 'S2_OxTk'
  CM_Segs(i,5 + 1) =  sum(CMs_MaxQ_AoA2(i,[16:16]));                            % Sum CMs per station for 'S2_OxTk'
  
  CA_Segs(i,6 + 1) =  sum(CAs_MaxQ_AoA2(i,[17:17]));                            % Sum CAs per station for 'S2_Itr' - 'S2_Itr' is made of 1 stations
  CN_Segs(i,6 + 1) =  sum(CNs_MaxQ_AoA2(i,[17:17]));                            % Sum CNs per station for 'S2_Itr'
  CM_Segs(i,6 + 1) =  sum(CMs_MaxQ_AoA2(i,[17:17]));                            % Sum CMs per station for 'S2_Itr'
  
  CA_Segs(i,7 + 1) =  sum(CAs_MaxQ_AoA2(i,[18:19]));                            % Sum CAs per station for 'S2_Mcase' - 'S2_Mcase' is made of 2 stations
  CN_Segs(i,7 + 1) =  sum(CNs_MaxQ_AoA2(i,[18:19]));                            % Sum CNs per station for 'S2_Mcase'
  CM_Segs(i,7 + 1) =  sum(CMs_MaxQ_AoA2(i,[18:19]));                            % Sum CMs per station for 'S2_Mcase'
  
  CA_Segs(i,8 + 1) =  sum(CAs_MaxQ_AoA2(i,[20:20]));                            % Sum CAs per station for 'S1_Con' - 'S1_Con' is made of 1 stations
  CN_Segs(i,8 + 1) =  sum(CNs_MaxQ_AoA2(i,[20:20]));                            % Sum CNs per station for 'S1_Con'
  CM_Segs(i,8 + 1) =  sum(CMs_MaxQ_AoA2(i,[20:20]));                            % Sum CMs per station for 'S1_Con'
  
  CA_Segs(i,9 + 1) =  sum(CAs_MaxQ_AoA2(i,[21:22]));                            % Sum CAs per station for 'S1_OxTk' - 'S1_OxTk' is made of 2 stations
  CN_Segs(i,9 + 1) =  sum(CNs_MaxQ_AoA2(i,[21:22]));                            % Sum CNs per station for 'S1_OxTk'
  CM_Segs(i,9 + 1) =  sum(CMs_MaxQ_AoA2(i,[21:22]));                            % Sum CMs per station for 'S1_OxTk'
  
  CA_Segs(i,10 + 1) =  sum(CAs_MaxQ_AoA2(i,[23:23]));                           % Sum CAs per station for 'S1_Itr' - 'S1_Itr' is made of 1 stations
  CN_Segs(i,10 + 1) =  sum(CNs_MaxQ_AoA2(i,[23:23]));                           % Sum CNs per station for 'S1_Itr'
  CM_Segs(i,10 + 1) =  sum(CMs_MaxQ_AoA2(i,[23:23]));                           % Sum CMs per station for 'S1_Itr'
  
  CA_Segs(i,11 + 1) =  sum(CAs_MaxQ_AoA2(i,[24:26]));                           % Sum CAs per station for 'S1_Mcase' - 'S1_Itr' is made of 3 stations
  CN_Segs(i,11 + 1) =  sum(CNs_MaxQ_AoA2(i,[24:26]));                           % Sum CNs per station for 'S1_Mcase'
  CM_Segs(i,11 + 1) =  sum(CMs_MaxQ_AoA2(i,[24:26]));                           % Sum CMs per station for 'S1_Mcase'
  
end

% Plot CA per Segment
figure('name','CAs per Segment Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CA_Segs)                                                          % Plot data with MulitLinePlot
title('C_a Per Segment')                                                        % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_a')                                                                   % Create y label

% Plot CN per Segment
figure('name','CNs per Segment Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CN_Segs)                                                          % Plot data with MulitLinePlot
title('C_n Per Segment')                                                        % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_n')                                                                   % Create y label

% Plot CM per Segment
figure('name','CMs per Segment Config070004 - Trade Study Round 2')             % Create new figure
MultiLinePlot(CM_Segs)                                                          % Plot data with MulitLinePlot
title('C_m Per Segment')                                                        % Create plot title
xlabel('Ma No.')                                                                % Create x label
ylabel('C_m')                                                                   % Create y label

% Call Create_CSV to create CSV files containing 
CA_Segs_MaxQ_AoA2 = Create_CSV(CA_Segs, 'CA_Segs_MaxQ_AoA2');
CN_Segs_MaxQ_AoA2 = Create_CSV(CN_Segs, 'CN_Segs_MaxQ_AoA2');
CM_Segs_MaxQ_AoA2 = Create_CSV(CM_Segs, 'CM_Segs_MaxQ_AoA2');