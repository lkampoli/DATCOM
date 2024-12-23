% The code will sum the aero coefficients for selected rocket segements as per
% X_Seg in DATCOM input excel file
% The script will output the .xlsx file: DATCOM_OUTPUT_X_Sumd.xlsx 
% Where X should be replaced by a unique identifier
% Script reads in data from: DATCOM_Inputs_X.xlsx & DATCOM_Outputs_X.xlsx

clc                                                                        
% clear                                                              
close all                                                                  

% Read in data from DATCOM Input excel                                               % -> !!! Update file name and row numbers !!! 
XListEditted = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','A3:A28');      % Read in XListEditted which is XList minus the first element
XSeg = xlsread('DATCOM_Inputs_Config1800000_ERIS300_01.xlsx','H2:H12');              % Read in XSeg

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

% Start summing coefficients
% Declare row no. variables where coefficients are stored
CNCol = 6;  % Column for CN's
CACol = 7;  % Column for CA's
CMCol = 8;  % Column for CM's

% Read in .xlsx file output from RunAllDanny.m  
CiData = xlsread('DATCOM_Outputs_Config1800000_ERIS300_01','A1:H31200');   % First & second set of coeffs for XListEditted

% Loop to sum coefficients
ii = 0;
jj = 0;

for j = 1:length(CiData)/length(XListEditted)
    for i = 1:length(XSeg)
        CiDataSumd = CiData((1 + ii):length(XListEditted)+ii,:).*XSegPos(:,i);    % Keep Alt, Mach & AoA values in CiData at first segment to sum too.
        OutputSumd(i + jj,:) = CiDataSumd(any(CiDataSumd,2),:);                   % cull rows equal to zero
      
        CNSumd = sum(CiData((1 + ii):length(XListEditted)+ii,CNCol).*XSegPos1s(:,i)); % Sum CN Coefficient up for first segment
        CASumd = sum(CiData((1 + ii):length(XListEditted)+ii,CACol).*XSegPos1s(:,i)); % Sum CA Coefficient up for first segment
        CMSumd = sum(CiData((1 + ii):length(XListEditted)+ii,CMCol).*XSegPos1s(:,i)); % Sum CM Coefficient up for first segment
        OutputSumd(i + jj,CNCol) = CNSumd;                                            % Append CNSumd to OutputSumd((i + jj),CNCol)
        OutputSumd(i + jj,CACol) = CASumd;                                            % Append CASumd to OutputSumd((i + jj),CACol)
        OutputSumd(i + jj,CMCol) = CMSumd;                                            % Append CMSumd to OutputSumd((i + jj),CMCol)
    end
    ii = ii + length(XListEditted); % Increment ii by length of XListEditted
    jj = jj + length(XSeg);         % Increment jj by length of XSeg
end

% The code below will create an array of xi Positions which is the center of the vehicle segments where the aero load will applied
XPos = zeros(size(XSeg));                                 % Create empty array to fill
XPos(1) = XSeg(1)/2;                                      % [m] - Fill first xi position with half the value of XSeg(1)

for i = 2:length(XSeg)
    XPos(i) = XSeg(i-1) + (XSeg(i) - XSeg(i-1))/2;        % [m] - Fill XPos with xi value
end
% Call repmat to repeat XPos the correct number of times given the length of DATCOM inputs
xiPos = repmat(XPos,length(AltList)*length(MachList)*length(AoAList),1);   % [m] - xi Position for excel load calculator

% Save OutputSumd as a CSV file
xlswrite('DATCOM_Outputs_Config1800000_ERIS300_01_Sumd.xlsx', OutputSumd); % !!! CHANGE X TO A UNIQUE IDENTIFIER !!!