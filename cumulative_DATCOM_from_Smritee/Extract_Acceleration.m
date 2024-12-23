% This script will extract the acceleration in [m/s] from an ASTOS output. This will be done by firstly interpolating the time, mach and
% sonic_velocity_Orbital outputs as per the script ASTOS_Extract.m. Then the mach & sonic_velocity_Orbital will be multipled to give a velocity
% vector which will then be dV/dt  by central difference for main points and forward difference for start and end points

% Required Functions and Classes
% - ASTOSSimulation - (Class) Reads in ASTOS outputs from a .txt file & more

clear; close all; clc;

% Create Time Vector with dt = 0.1 s - These value are read in from the
% masss breakdown .txt file
dt = 0.1;                                                                  % [s] - time step
tstart = 0;                                                                % [s] - start time         
tend = 690.1;                                                              % [s] - end time
t = tstart:dt:tend;                                                        % [s] - Call createtime function in Mass class

% Import ASTOS .txt file
SimOutfname = input('Enter ASTOS Sim Output .txt file name:    ', 's');    % Request Mass Breakdown .txt input file name from user .'s' required to prevent evaluating input
sim = ASTOSSimulation(SimOutfname);                                        % Call ASTOSSimulation(fname) and create object sim

% Import vectors from ASTOS output
tASTOS = sim.data.Time;                                                    % [s] - Create vector of ASTOS times
mach = sim.data.mach_Orbital;                                              % [-] - Create vector of ASTOS mach no.
c = sim.data.sonic_velocity_Orbital;                                       % [m/s] - Create vector of ASTOS speed of sound
aaxial = sim.data.load_factor_axial_Orbital;                               % [g] - Create vector of ASTOS axial load factor

% Find unique values in the vector from ASTOS
[tASTOS, index] = unique(tASTOS,'sorted');                                 % Finds unigue values in tASTOS and outputs the unique values and there index
mach = mach(index);                                                        % Keep values in mach at each index in index
c = c(index);                                                              % Keep values in c at each index in index
aaxial = aaxial(index);                                                    % Keep values in aaxial at each index in index

% Interpolate ASTOS vectors at values in t
mach = interp1(tASTOS, mach, t);                                           % Linear interpolation of mach at t
c = interp1(tASTOS, c, t);                                                 % Linear interpolation of mach at t
aaxial = interp1(tASTOS, aaxial, t);                                       % Linear interpolation of aaxial at t

% Calculate velocity from mach and c
vel = mach.*c;                                                             % [m/s] - Velocity vector

% Create empty accl vector
accl = zeros(length(vel),1);                                               % [m/s^s] - Create empty acceleration vector to fill 

% calculate acceleration using central difference for main points and
% forward difference for start and end points
for i = 1:length(accl)
   
    % Calculate derivative by forward difference for first point
    if i == 1
        accl(i) = (vel(i+1) - vel(i))/(t(i+1) - t(i));
    % Calculate derivative by centeral divelference for main points    
    elseif i == length(accl) - 1 
        accl(i) = (vel(i) - vel(i-1))/(t(i) - t(i-1));
    % Calculate derivative by forward difference for first point
    else
       accl(i) = (vel(i) - vel(i-1))/(t(i) - t(i-1));
    end
    
end

% Plot the derivatives calculated from the three methods
figure('Name','Interpolated ASTOS Results & Acceleration')
subplot(5,1,1)
plot(t, mach)               % Plot mach
grid on
title('Mach')
ylabel('[-]')
subplot(5,1,2)
plot(t, vel)                % Plot velocity
grid on
title('Velocity')
ylabel('[m/s]')
subplot(5,1,3)
plot(t, c)                  % Plot speed of sound
grid on
title('Speed of Sound')
ylabel('[m/s]')
subplot(5,1,4)
plot(t, accl)               % Plot acceleration
grid on
title('Acceleration')
ylabel('[m/s^s]')
subplot(5,1,5)
plot(t, aaxial)             % Plot axial load factor
grid on
title('Axial Load Factor')
ylabel('[g]')
xlabel('time [s]')

% Compile output array
OutputArray = zeros(length(t),3);                                          % Create OutputArray to hold calculated vectors
OutputArray(:,1) = t;                                                      % And time vector to OutputArray
OutputArray(:,2) = accl;                                                   % And accleration vector to OutputArray
OutputArray(:,3) = aaxial;                                                 % And axial load factor vector to OutputArray

% Prompt user for an output filename
OUTfname = input('Enter output filename (include .txt):      ','s');       % File name for file to create
dlmwrite(OUTfname, OutputArray, 'delimiter', '\t');
