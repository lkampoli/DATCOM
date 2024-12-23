function [output] = filllhcol(AltList,MachList, AoAList, XList, Ncol)
%This function will fill the first 4 columns of the DATCOM output array
%with AltList, MachList & XList
%INPUTS:
%   Ncol - number columns required in output array
% All vectors listed below are entered into the excel input sheet for the
% DATCOM sim
%   AltList - vector of altitudes entered into DATCOM run
%   MachList - vector of Mach No's entered into DATCOM run
%   AoAList - vector of angle of attacks entered into DATCOM run
%   XList - vector of x positions entered into DATCOM run
%
%OUTPUTS:
%   output - an array ready to be filled with aero coefficients (CA, CN & CM) for each XList at every element
%   in the vectors AltList, MachList, AoAList

% Declare empty output vector
Output = zeros(length(AltList)*length(MachList)*length(AoAList)*length(XList),Ncol);

% Fill AltList into first into column of Output
ii = 1;                                                                             % index variable
iii = 1;                                                                            % index variable
for n = 1:(length(Output)/(length(MachList)*length(AoAList)*length(XList)))         % index over length(Output)/(length(MachList)*length(AoAList)*length(XList)) = 6 i.e number of times to repeat AltList
    Output(iii:ii*(length(MachList)*length(AoAList)*length(XList)),1) = AltList(n); % fill rows 1:(length(MachList)*length(AoAList)*length(XList)) of Output column 1 with AltList(1)
    iii = iii + (length(MachList)*length(AoAList)*length(XList));                   % index starting row by (length(MachList)*length(AoAList)*length(XList))
    ii = ii + 1;                                                                    % index ending row by ii*(length(MachList)*length(AoAList)*length(XList))
end

% Fill blocks of MachList into second column of Output
ii = 1;                                                                     % index variable
iii = 1;                                                                    % index variable
for j = 1:length(Output)/(length(MachList)*length(AoAList)*length(XList))   % index over length(Output)/(length(MachList)*length(AoAList)*length(XList)) = 6 i.e number of times to repeat MachList
    for n = 1:length(MachList)                                              % index over length(MachList)
        Output(iii:(ii*length(AoAList)*length(XList)),2) = MachList(n);     % fill rows 1:(length(AoAList)*length(XList)) of Output column 2 with MachList(1)
        iii = iii + (length(AoAList)*length(XList));                        % index starting row by (length(AoAList)*length(XList))
        ii = ii + 1;                                                        % index ending row by ii*(length(AoAList)*length(XList))
    end
end

% Fill blocks of AoAList into third column of Output
ii = 1;                                                   % index variable
iii = 1;                                                  % index variable
for j = 1:length(Output)/(length(AoAList)*length(XList))  % index over length(Output)/(length(AoAList)*length(XList)) = 24 i.e number of times to repeat AoAList
    for n = 1:length(AoAList)                             % index over length(AoAList) 
        Output(iii:(ii*length(XList)),3) = AoAList(n);    % fill rows 1:length(XList) of Output column 3 with AoAList(1)
        iii = iii + length(XList);                        % index starting row by length(XList)
        ii = ii + 1;                                      % index ending row by ii*length(XList)
    end
end

% Fill blocks of XList into forth column of Output
ii = 1;                                        % reset index variable
iii = 1;                                       % reset index variable
for n = 1:length(Output)/length(XList)         % index over length(Output)/length(XList) = 72 i.e number of times to repeat XList
    Output(iii:(ii*length(XList)),4) = XList;  % fill rows 1:length(XList) of Output column 4 with XList                        
    iii = iii + length(XList);                 % index starting row by length(XList)
    ii = ii + 1;                               % index ending row by ii*length(XList)
end
output = Output; % Return output array
end

