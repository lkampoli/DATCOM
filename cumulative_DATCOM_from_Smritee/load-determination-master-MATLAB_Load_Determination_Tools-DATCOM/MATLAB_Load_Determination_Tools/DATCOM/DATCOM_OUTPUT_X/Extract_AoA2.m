% This code is indented to extract the coefficients at AoA = 2
% Created on 18.9.19 by cut'n'paste'n out of RunAllDanny





% Create a vector for Mach No.'s - !!! Ensure this vector matches MachList created within Build_for005 for construction of for005.dat !!!
MachNos = [0.5; 0.9; 1.0; 1.1; 1.2; 1.3; 1.5; 1.8; 2.0; 2.5; 3.0; 3.5; 4.0; 4.5; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0];
CAs_MaxQ_AoA2 = zeros(length(MachNos),length(XList) - 1);                                                            % Create empty vector of zeros
CAs_MaxQ_AoA2(:,1) = MachNos;                                                                                        % Set first column of CAs_MaxQ_AoA1 equal MachNos
CNs_MaxQ_AoA2 = zeros(length(MachNos),length(XList) - 1);                                                            % Create empty vector of zeros
CNs_MaxQ_AoA2(:,1) = MachNos;                                                                                        % Set first column of CNs_MaxQ_AoA1 equal MachNos
CMs_MaxQ_AoA2 = zeros(length(MachNos),length(XList) - 1);                                                            % Create empty vector of zeros
CMs_MaxQ_AoA2(:,1) = MachNos;                                                                                        % Set first column of CMs_MaxQ_AoA1 equal MachNos

% Write indivdual coefficients to the array Ci_MaxQ_AoA2
for index = 2:length(XList)-1 
  CAs_MaxQ_AoA2(:,index) = CoeffsIndiv(index).CA_GI(3,:,7);                     % Changed Index here to retrieve AoA = 2
  CNs_MaxQ_AoA2(:,index) = CoeffsIndiv(index).CN_GI(3,:,7);
  CMs_MaxQ_AoA2(:,index) = CoeffsIndiv(index).CM_GI(3,:,7); 
end

% Call Create_CSV to create CSV files
CAs_MaxQ_AoA2 = Create_CSV(CAs_MaxQ_AoA2, 'CAs_MaxQ_AoA2');
CNs_MaxQ_AoA2 = Create_CSV(CNs_MaxQ_AoA2, 'CNs_MaxQ_AoA2');
CMs_MaxQ_AoA2 = Create_CSV(CMs_MaxQ_AoA2, 'CMs_MaxQ_AoA2');