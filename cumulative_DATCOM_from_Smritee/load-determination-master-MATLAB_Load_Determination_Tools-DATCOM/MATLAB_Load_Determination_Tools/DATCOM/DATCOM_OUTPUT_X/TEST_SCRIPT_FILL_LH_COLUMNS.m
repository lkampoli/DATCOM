% This scipt will test filling the LH columns of the DATCOM output for copy
% pasta into excel

clc
clear

% Declare vectors for AltList, MachList & AoAList
AList = [100 200 300 400 500 600];          % AltList
MList = [0.5 1.0 1.5 2.0];                  % MachList
AoAList = [ 0 1 2];                         % AoAList
XSeg = [0 1 2 3 4 5];                       % XSeg - x position for aeroload on each segment
NCol = 8;                                   % Number of columns in output array

% Declare empty output vector
Output = zeros(length(AList)*length(MList)*length(AoAList)*length(XSeg),NCol); 

% i = 1;   % Index for AltList
ii = 1;  % Index for multiples of AltList
m = 1;   % Column indexer for Output
iii = 1;

% Fill AltList into first into column of Output
for n = 1:(length(Output)/(length(MList)*length(AoAList)*length(XSeg)))         % index over length(Output)/(length(MList)*length(AoAList)*length(XSeg)) = 6 i.e number of times to repeat AList
    Output(iii:ii*(length(MList)*length(AoAList)*length(XSeg)),1) = AList(n);   % fill rows 1:(length(MList)*length(AoAList)*length(XSeg)) of Output column 1 with AList(1)
    iii = iii + (length(MList)*length(AoAList)*length(XSeg));                   % index starting row by (length(MList)*length(AoAList)*length(XSeg))
    ii = ii + 1;                                                                % index ending row by ii*(length(MList)*length(AoAList)*length(XSeg))
end

% Fill blocks of MList into second column of Output
ii = 1;
iii = 1;
for j = 1:length(Output)/(length(MList)*length(AoAList)*length(XSeg))   % index over length(Output)/(length(MList)*length(AoAList)*length(XSeg)) = 6 i.e number of times to repeat MList
    for n = 1:length(MList)                                             % index over length(MList)
        Output(iii:(ii*length(AoAList)*length(XSeg)),2) = MList(n);     % fill rows 1:(length(AoAList)*length(XSeg)) of Output column 2 with MList(1)
        iii = iii + (length(AoAList)*length(XSeg));                     % index starting row by (length(AoAList)*length(XSeg))
        ii = ii + 1;                                                    % index ending row by ii*(length(AoAList)*length(XSeg))
    end
end

% Fill blocks of AoAList into third column of Output
ii = 1;
iii = 1;
for j = 1:length(Output)/(length(AoAList)*length(XSeg))   % index over length(Output)/(length(AoAList)*length(XSeg)) = 24 i.e number of times to repeat AoAList
    for n = 1:length(AoAList)                             % index over length(AoAList) 
        Output(iii:(ii*length(XSeg)),3) = AoAList(n);     % fill rows 1:length(XSeg) of Output column 3 with AoAList(1)
        iii = iii + length(XSeg);                         % index starting row by length(XSeg)
        ii = ii + 1;                                      % index ending row by ii*length(XSeg)
    end
end

% Fill blocks of XSeg into forth column of Output
ii = 1;                                      % reset index variable
iii = 1;                                     % reset index variable
for n = 1:length(Output)/length(XSeg)        % index over length(Output)/length(XSeg) = 72 i.e number of times to repeat XSeg
    Output(iii:(ii*length(XSeg)),4) = XSeg;  % fill rows 1:length(XSeg) of Output column 4 with XSeg                        
    iii = iii + length(XSeg);                % index starting row by length(XSeg)
    ii = ii + 1;                             % index ending row by ii*length(XSeg)
end

%{
% Fill Helper Column into fifth column of Output
for j = 1:length(Output)                                                   
   Output(j,5) = num2str(Output(j,1) + "|" + Output(j,2) + "|" + Output(j,3) + "|" + Output(j,4));
end

%% Fill Next block of MList in second coloumn of Output
for n = 1:length(MList)
    Output(iii:(ii*length(AoAList)*length(XSeg)),2) = MList(n);
    iii = iii + (length(AoAList)*length(XSeg));
    ii = ii + i;
end


% For loop to population AltList, MachList & AoAList
for n = 1:length(Output)
   
    if n == (ii*(length(MList)*length(AoAList)*length(XSeg)))+1
        i = i + 1;             % Increment index variable for AltList
        ii = ii + 1;           % Increment index variable for multiples of AltList 
    end
    Output(n,m) = AList(i);
    
    if n == length(Output)
        m = m + 1;
        i = 1;
        ii = 1;
        
        for n = 1:length(Output)
            
            % if statement to cover AoAList & XSeg
            if n == (ii*(length(AoAList)*length(XSeg)))+1
                i = i + 1;             % Increment index variable for MList
                ii = ii + 1;
                
                % if statement to cover to restart MList for next value in AList
                if (((ii)*(length(AoAList)*length(XSeg)))+1) > (length(MList)*length(AoAList)*length(XSeg))
                    i = 1;           % Increment index variable for multiples of MList 
                    ii = 1;
                end
            end
            Output(n,m) = MList(i);
            
        end
    end

    
end
%}