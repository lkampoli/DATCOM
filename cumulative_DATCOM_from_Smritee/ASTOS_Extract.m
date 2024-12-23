% This script will extract data from simulation_danny.txt output by ASTOS for the spiral2_Baseline Titled: ERIS_Spiral2_500km_Whalers
% simulation_danny.txt is located at: N:\1-Engineering\5-ERIS\1 - Trajectory Optimization\ERIS_Spiral2_500km_Whalers.gtp\exports

% Required Functions and Classes
% - ReadInputFile - (Class) Reads in mass breadkown including mdots & burn times
% - Mass - (Class) Creates instance of Mass class. Contains function to calculate mass vectors


clear; close all; clc;

% Import mass, mdot and time parameters for configuration
MassInfname = input('Enter Mass Breakdown .txt file name:    ', 's');      % Request Mass Breakdown .txt input file name from user .'s' required to prevent evaluating input  
MassInputs = ReadInputFile(MassInfname);                                   % Create instance of ReadInputFile class that contains the mass, mdot and time parameters
m = Mass(MassInputs);                                                      % Create instance of Mass class 

% Create Time Vector with dt = 0.1 s
t = m.createtime;                                                          % [s] - Call createtime function in Mass class

% Import ASTOS .txt file
SimOutfname = input('Enter ASTOS Sim Output .txt file name:    ', 's');    % Request Mass Breakdown .txt input file name from user .'s' required to prevent evaluating input
sim = ASTOSSimulation(SimOutfname);                                        % Call ASTOSSimulation(fname) and create object sim

% Import vectors from ASTOS output
tASTOS = sim.data.Time;                                                    % [s] - Create vector of ASTOS times
aaxial = sim.data.load_factor_axial_Orbital;                               % [g] - Create vector of ASTOS axial load factor
mach = sim.data.mach_Orbital;                                              % [-] - Create vector of ASTOS mach no.
altitude = sim.data.altitude_Orbital_Earth*1000;                           % [m] - Create vector of ASTOS altitude
theta = sim.data.pitch_Orbital_L_Orbital_Earth;                            % [Deg] - Create vector of ASTOS pitch angle
gravity = sim.data.gravity_Orbital;                                        % [m/s^2] - Create vector of ASTOS gravity
thrust = sim.data.thrust_Orbital;                                          % [kN] - Create vector of ASTOS thrust.
mass_tot = sim.data.mass_total_Orbital*1000;                               % [kg] - Create vector of ASTOS total vehicle mass
qASTOS = sim.data.dynamic_pressure_Orbital;                                % [Pa] - Create vector of ASTOS dynamic pressure
rho_infASTOS = sim.data.atmos_density_Orbital;                             % [kg/m^3] - Create Vector of ASTOS atmospheric density

% Find unique values in the vector from ASTOS
[tASTOS, index] = unique(tASTOS,'sorted');                                 % Finds unigue values in tASTOS and outputs the unique values and there index
aaxial = aaxial(index);                                                    % Keep values in aaxial at each index in index
mach = mach(index);                                                        % Keep values in mach at each index in index
altitude = altitude(index);                                                % Keep values in alt at each index in index
theta = theta(index);                                                      % Keep values in theta at each index in index
gravity = gravity(index);                                                  % Keep values in gravity at each index in index
thrust = thrust(index);                                                    % Keep values in thrust at each index in index
mass_tot = mass_tot(index);                                                % Keep values in mass_tot at each index in index
qASTOS = qASTOS(index);                                                    % Keep values in q at each index in index
rho_infASTOS = rho_infASTOS(index);                                        % Keep values in rho_infASTOS at each index in index     

% Interpolate ASTOS vectors at values in t
ax = interp1(tASTOS, aaxial, t);                                           % Linear interpolation of aaxial at t
ma = interp1(tASTOS, mach, t);                                             % Linear interpolation of mach at t
alt = interp1(tASTOS, altitude, t);                                        % Linear interpolation of all at t
th = interp1(tASTOS, theta, t);                                            % Linear interpolation of theta at t
g = interp1(tASTOS, gravity, t);                                           % Linear interpolation of gravity at t
T = interp1(tASTOS, thrust, t);                                            % Linear interpolation of thrust at t 
m_tot = interp1(tASTOS,mass_tot,t);                                        % Linear interpolation of mass_tot at t
q = interp1(tASTOS,qASTOS,t);                                              % Linear interpolation of qASTOS at t
rho_inf = interp1(tASTOS,rho_infASTOS,t);                                  % Linear interpolation of rho_infASTOS at t

% Create vector of masses over time for each mass element
% Stage 1
mtS1Ox = m.mt_S1OxTk(t);                       % S1 Ox Tank mass over time
mtS1Mcase = m.mt_S1Mcase(t);                   % S1 Mcase mass over time
mtS1Itr = m.m_S1Itr.*(t<=m.tS1bout);           % S1 Itr mass over time
mtS1Con = m.m_S1Con.*(t<=m.tS1bout);           % S1 Con mass over time

mtS1EXTRA = 600.*(t<=m.tS1bout);               % Mass to equal ASTOS

mtS1total = mtS1Ox + mtS1Mcase + mtS1Itr + mtS1Con;            

% Stage 2
mtS2Ox = m.mt_S2OxTk(t);                       % S2 Ox Tank mass over time
mtS2Mcase = m.mt_S2Mcase(t);                   % S2 Mcase mass over time
mtS2Itr = m.m_S2Itr.*(t<=m.tS2bout);           % S2 Itr mass over time
mtS2Con = m.m_S2Con.*(t<=m.tS2bout);           % S2 Con mass over time
mtfairing = m.m_fairing.*(t<=m.tS2bout);       % Fairing mass over time

mtS2EXTRA = 300.*(t<=m.tS2bout);               % Mass to equal ASTOS

mtS2total = mtS2Ox + mtS2Mcase + mtS2Itr + mtS2Con + mtfairing;

% Stage 3
mtS3Ox = m.mt_S3OxTk(t);                       % S3 Ox Tank mass over time
mtS3Mcase = m.mt_S3Mcase(t);                   % S3 Mcase mass over time
mtPL = ones(length(t),1);                      % Create empty array to store payload mass 
mtPL = mtPL.*m.m_PL;                           % Calculate and store payload mass
mtS3total = mtS3Ox + mtS3Mcase;

% Sum all mass vectors to get total mass of vehicle
mt_total = mtS1total + mtS2total + mtS3total + mtPL';

% Fill PAGMM array with vectors - Nasty as hardcoding rubbish 
nCols = 18;                                    % Number of columns to include in output excel file
PAGMM = zeros(length(t),nCols);                % Create empty array to store data in
PAGMM(:,1) = t;                                % Set 1st column as time - [s]
PAGMM(:,2) = th;                               % Set 2nd column as Pitch Angle - [Deg]
PAGMM(:,3) = alt;                              % Set 3rd column as Altitude - [m]
PAGMM(:,4) = ax;                               % Set 4th column as Axial Acceleration - [g]
PAGMM(:,5) = g;                                % Set 5th column to vehicle local gravity - [m/s^2]
PAGMM(:,6) = ma;                               % Set 6th column to mach no. - [-]
PAGMM(:,7) = mtfairing;                        % Set 7th column to m(t) RUAG fairing - [kg]
PAGMM(:,8) = mtPL;                             % Set 8th column to m(t) PayLoad - [kg]
PAGMM(:,9) = mtS3Ox;                           % Set 9th column to m(t) 3rd stage ox tank - [kg]
PAGMM(:,10) = mtS3Mcase;                       % Set 10th column to m(t) 3rd stage motor casing - [kg]
PAGMM(:,11) = mtS2Con;                         % Set 11th column to m(t) 2nd stage connection over nozzle - [kg]
PAGMM(:,12) = mtS2Ox;                          % Set 12th column to m(t) 2nd stage ox tank - [kg]
PAGMM(:,13) = mtS2Itr;                         % Set 13th column to m(t) 2nd stage interconnect - [kg]
PAGMM(:,14) = mtS2Mcase;                       % Set 14th column to m(t) 2nd stage motor case - [kg]
PAGMM(:,15) = mtS1Con;                         % Set 15th column to m(t) 1st stage conical - [kg]
PAGMM(:,16) = mtS1Ox;                          % Set 16th column to m(t) 1st stage ox tank - [kg]
PAGMM(:,17) = mtS1Itr;                         % Set 17th column to m(t) 1st stage interconnect - [kg]
PAGMM(:,18) = mtS1Mcase;                       % Set 18th column to m(t) 1st stage motor case - [kg]
  
%{
% Create Celleries for Column Names
% ColNames = {'Time';'PitchAngle';'Altitude';'AxialLoadFactor'};
% Colnames = cell2mat()%'Gravity';'Mach';...
    %'mtfairing';'mtPL';'mtS3Ox';'mtS3Mcase';'mtS2Con';'mtS2Ox';'mtS2Itr';'mtS2Mcase';'mtS1Con';'mtS1Ox';'mtS1Itr';'mtS1Mcase'];
% ColUnits = {'seconds' ;'degrees';'meters';'g';'meters/second^2';'-';...
  %  'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';'kilograms';};
%}
% Prompt user for an output filename
OUTfname = input('Enter output filename (include .txt):      ','s');       % File name for file to create
dlmwrite(OUTfname, PAGMM, 'delimiter', '\t');

%dlmwrite(OUTfname, ColNames, 'delimiter', '/t','-append')
% Below is code that writes .xlsx files that we wish to avoid
%{
% Write to PAGMM to .xlsx file
if exist('PAGMM - TR3.xlsx','file') > 0        % Check if .xlsx of same name exists
    delete 'PAGMM - TR3.xlsx';                 % Delete if it exists - Saftey measure to prevent writting to same file twice   
    xlswrite('PAGMM - TR3.xlsx', PAGMM);       % Write to new .xlsx file
else
    xlswrite('PAGMM - TR3.xlsx', PAGMM);       % Write to .xlsx file
end

% Write Thrust_t in Newtons to .xlsx file
if exist('Thrust_t - TR3.xlsx','file') > 0     % Check if .xlsx of same name exists
    delete 'Thrust_t - TR3.xlsx';              % Delete if it exists - Saftey measure to prevent writting to same file twice   
    xlswrite('Thrust_t - TR3.xlsx', T'.*1000); % Write to new .xlsx file
else
    xlswrite('Thrust_t - TR3.xlsx', T'.*1000); % Write to .xlsx file
end
%}
% Plot mass as function of time
figure('Name', 'm(t) - Entire Vehicle')
plot(t,mt_total)
title('m(t)')
xlabel('time [sec]')
ylabel('mass [kg]')
grid on

% Plot sim'd and interpolated data using subplot
figure('Name','linear interpd aaxial vs sim data')
subplot(2,1,1)
plot(tASTOS,aaxial);                                                       % Plot data extracted from ASTOS
title('aaxial - sim data')
xlabel('time [sec]')
ylabel('acceleration [g]')
grid on

subplot(2,1,2)
plot(t,ax);                                                                % Plot interpolated data
title('aaxial - interpd data')
xlabel('time [sec]')
ylabel('acceleration [g]')
grid on

figure('Name','linear interpd mach vs sim data')
subplot(2,1,1)
plot(tASTOS,mach);                                                         % Plot data extracted from ASTOS
title('mach - sim data')
xlabel('time [sec]')
ylabel('Mach [-]')
grid on

subplot(2,1,2)
plot(t,ma);                                                                % Plot interpolated data
title('mach - interpd data')
xlabel('time [sec]')
ylabel('Mach [-]')
grid on

figure('Name','linear interpd altitude vs sim data')
subplot(2,1,1)
plot(tASTOS,altitude);                                                     % Plot data extracted from ASTOS
title('altitude - sim data')
xlabel('time [sec]')
ylabel('altitude [m]')
grid on

subplot(2,1,2)
plot(t,alt);                                                               % Plot interpolated data
title('altitude - interpd data')
xlabel('time [sec]')
ylabel('altitude [m]')
grid on

figure('Name','linear interpd pitch angle vs sim data')
subplot(2,1,1)
plot(tASTOS,theta);                                                        % Plot data extracted from ASTOS
title('pitch angle - sim data')
xlabel('time [sec]')
ylabel('pitch angle [deg]')
grid on

subplot(2,1,2)
plot(t,th);                                                                % Plot interpolated data
title('pitch angle - interpd data')
xlabel('time [sec]')
ylabel('pitch angle [deg]')
grid on

figure('Name','linear interpd thrust vs sim data')
subplot(2,1,1)
plot(tASTOS,thrust);                                                       % Plot data extracted from ASTOS
title('thrust - sim data')
xlabel('time [sec]')
ylabel('thrust force [kN]')
grid on

subplot(2,1,2)
plot(t,T);                                                                 % Plot interpolated data
title('thrust - interpd data')
xlabel('time [sec]')
ylabel('thrust force [kN]')
grid on

figure('Name','linear interpd total mass vs sim data')
subplot(2,1,1)
plot(tASTOS,mass_tot);                                                     % Plot data extracted from ASTOS
title('total mass - sim data')
xlabel('time [sec]')
ylabel('mass [kg]')
grid on

subplot(2,1,2)
plot(t,m_tot);                                                             % Plot interpolated data
title('total mass - interpd data')
xlabel('time [sec]')
ylabel('mass [kg]')
grid on

figure('Name','Altitude, Mach & Dynamic Pressure')                         % Plot Altitude, Mach & Dynamic Pressure
subplot(4,1,1)
plot(t,alt);                                                               % Plot data extracted from ASTOS
title('Altitude - interpd data')
xlabel('time [sec]')
ylabel('altitude [m]')
grid on

subplot(4,1,2)
plot(t,ma);                                                                % Plot interpolated data
title('mach - interpd data')
xlabel('time [sec]')
ylabel('Mach [-]')
grid on

subplot(4,1,3)
plot(t,q);                                                                 % Plot interpolated data
title('dynamic pressure - interpd data')
xlabel('time [sec]')
ylabel('q [Pa]')
grid on

subplot(4,1,4)
plot(t,rho_inf);                                                           % Plot interpolated data
title('atmospheric density - interpd data')
xlabel('time [sec]')
ylabel('atmospheric density [kg/m^3]')
grid on






