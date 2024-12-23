classdef Mass
    % Class to hold mass properties
    % Reads in mass breakdown data from: Mass_Breakdown.xlsx - whos data
    % has been copied from; Load Determination - Parameters & Calculations
    % Time mass flow rate property values have been resourced from: Eris_Trade_Study_Round_2_Detailed.xlsx
    % !!!
    % THIS SCRIPT CONTAINS HARDCODED VALUES
    % !!!
    properties
    % Mass buckets within vehicle - All masses in Kg
    % Third Stage
    m_PL;                       % Mass Payload        -                    Cell = B4
    m_fairing;                  % Mass RUAG Fairing   -                    Cell = B3
    m_S3Itr;                    % S3 interstage(OxTk) - inert + m(t)       Cell = G12
    m_ox3;                      % S3 mass oxidiser    - m(t)               Cell = B11
    m_S3Mcase;                  % S3 motor case       - inert + m(t)       Cell = G18
    m_fuel3;                    % S3 mass fuel grain  - m(t)               Cell = B17 
    % Second Stage
    m_S2Con;                    % S2 Fairing over S3 Nozzle                Cell = G29
    m_S2OxTk;                   % S2 mass OX tank     - inert + m(t)       Cell = G33
    m_ox2;                      % S2 mass oxidiser    - m(t)               Cell = B32
    m_S2Itr;                    % S2 interstage       - inert              Cell = G38
    m_S2Mcase;                  % S2 mass motor case  - inert + m(t)       Cell = G44 
    m_fuel2;                    % S2 mass fuel grain  - m(t)               Cell = B43
    % First Stage
    m_S1Con;                    % S1 fairing over S2 nozzle                Cell = G51
    m_S1OxTk;                   % S1 mass             - inert + m(t)       Cell = G55
    m_ox1;                      % S1 mass oxidiser    - m(t)               Cell = B54
    m_S1Itr;                    % S1 mass interstage  - inert              Cell = G62
    m_S1Mcase;                  % S1 mass motor casing- inert + m(t)       Cell = G66
    m_fuel1;                    % S1 mass fuel grain  - m(t)               Cell = B65
    
    % Mass flowrates - Kg/s
    % Thrid Stage
    dmS3OX;                     % S3 OX mass flowrate 
    dmS3FUEL;                   % S3 Fuel mass flowrate
    % Second Stage
    dmS2OX;                     % S2 OX mass flowrate 
    dmS2FUEL;                   % S2 Fuel mass flowrate
    % First Stage
    dmS1OX;                     % S1 OX mass flowrate
    dmS1FUEL;                   % S1 Fuel mass flowrate
    
    % Time buckets - All time in seconds
    tstart;                     % Start time
    tend;                       % End time
    tS1start;                   % S1 burn time start
    tS1burn;                    % S1 burn time
    tS1bout;                    % S1 burn out time
    tS2start;                   % S1 burn start
    tS2burn;                    % S2 burn time
    tS2bout;                    % S2 burn out time
    tS3start;                   % S3 burn start
    tS3burn;                    % S3 burn time
    tS3bout;                    % S3 burn out time
    end
    
    methods
        % Set property values from imported Inputs class
        function obj = Mass(Inputs)                 
            pnames = properties(obj);                           % Create cell array of property names
            vnames = Inputs.data.Properties.VariableNames';     % Create call array of property names
            for i = 1:length(pnames)
                obj.(pnames{i}) = Inputs.data.(vnames{i});      % Loop through vnames setting values of pnames  
            end
           
            %{
            % Keep Collapsed, Here Lies Dragons
            % cell array of cell references - Remove hardcoding rubbish - Convert list of cell references to a .txt that can be read in
            CellNos = {'B4', 'B3', 'G12', 'B11', 'G18', 'B17','G29','G33','B32','G38', 'G44', 'B43','G51','G55','B54','G62','G66','B65'};  
            for i = 1:18                            % Changes this hard coded rubbish - Maybe a loop thats looks for the first character to be 'm'
                obj.(pnames{i}) = xlsread('Mass_Bdown','Sheet1',CellNos{i}); % Loop over property names and assign values 
            end
            % set mass flow rate properties -  TODO: Calculate these values based
            % on assumption that all fuel & ox is used up and only inert
            % mass remains
            
            obj.dmS3OX = 6.4;                                              % [kg/s] - S3 OX mass flowrate 
            obj.dmS3FUEL = 2.4;                                            % [kg/s] - S3 Fuel mass flowrate
            obj.dmS2OX = 22.65;                                            % [kg/s] - S2 OX mass flowrate 
            obj.dmS2FUEL = 8.25;                                           % [kg/s] - S2 Fuel mass flowrate
            obj.dmS1OX = 89.1;                                             % [kg/s] - S1 OX mass flowrate
            obj.dmS1FUEL = 33.59;                                          % [kg/s] - S1 Fuel mass flowrate
            
            % Set time property values
            obj.tstart = 0;                                                % [s] - Set value for tstart property in Mass class
            obj.tS1start = 0;                                              % [s] - Set value for tS1start property in Mass class
            obj.tS1burn = 116.8;                                           % [s] - Set value for tS1burn property in Mass class
            obj.tS1bout = obj.tS1start + obj.tS1burn;                      % [s] - Set value for tS1bout property in Mass class
            obj.tS2start = 119.8;                                          % [s] - Set value for tS2start property in Mass class
            obj.tS2burn = 116.8;                                           % [s] - Set value for tS2burn property in Mass class
            obj.tS2bout = obj.tS2start + obj.tS2burn;                      % [s] - Set value for tS2bout property in Mass class
            obj.tS3start = 600;                                            % [s] - Set value for tS3start property in Mass class
            obj.tS3burn = 45.2;                                            % [s] - Set value for tS3burn property in Mass class
            obj.tS3bout = obj.tS3start + obj.tS3burn;                      % [s] - Set value for tS3bout property in Mass class
            %}
        end
        
        % The below function utilises the properties declared above 
        function t = createtime(obj)                    % Function to create time vector
            dt = 0.1;                                   % Size of time step - selected to reduce amount of data imported into excel
            obj.tend = obj.tS3start + obj.tS3burn;      % Calculate tend
            t = obj.tstart:dt:obj.tend;
        end
        
        function m_S3Mcase = mt_S3Mcase(obj,t)                             % m(t) for S3 Motor Case
            C3Mcase = obj.m_S3Mcase + obj.m_fuel3;                         % Constant for S3 Mcase
            m_S3Mcase = -obj.dmS3FUEL.*(t - obj.tS3start).*(t>=obj.tS3start).*(t<=obj.tS3bout)...
                + C3Mcase;
        end
        
        function m_S3OxTk = mt_S3OxTk(obj,t)                               % m(t) for S3 Ox Tank
            C3Itr = obj.m_S3Itr + obj.m_ox3;                               % Constant for S3 OxTk
            m_S3OxTk = -obj.dmS3OX.*(t - obj.tS3start).*(t>=obj.tS3start).*(t<=obj.tS3bout)...
                + C3Itr;
        end
        
        function m_S2Mcase = mt_S2Mcase(obj,t)                             % m(t) for S2 Motor Case
            C2Mcase = obj.m_S2Mcase + obj.m_fuel2;                         % Constant for S2 Mcase
            m_S2Mcase = -obj.dmS2FUEL.*(t - obj.tS2start).*(t>=obj.tS2start).*(t<=obj.tS2bout)...
                + C2Mcase.*(t<=obj.tS2bout);
        end
        
        function m_S2OxTk = mt_S2OxTk(obj,t)                               % m(t) for S2 Ox Tank
            C2OxTk = obj.m_S2OxTk + obj.m_ox2;                             % Constant for S2 OxTk
            m_S2OxTk = -obj.dmS2OX.*(t - obj.tS2start).*(t>=obj.tS2start).*(t<=obj.tS2bout)...
                + C2OxTk.*(t<=obj.tS2bout);
        end
        
        function m_S1Mcase = mt_S1Mcase(obj,t)                             % m(t) for S1 Motor Case
            C1Mcase = obj.m_S1Mcase + obj.m_fuel1;                         % Constant for S1 Mcase
            m_S1Mcase = -obj.dmS1FUEL.*t.*(t<=obj.tS1bout) + C1Mcase.*(t<=obj.tS1bout);
        end
        
        function m_S1OxTk = mt_S1OxTk(obj,t)                               % m(t) for S1 Ox Tank
            C1OxTk = obj.m_S1OxTk + obj.m_ox1;                             % Constant for S1 OxTk
            m_S1OxTk = -obj.dmS1OX.*t.*(t<=obj.tS1bout) + C1OxTk.*(t<=obj.tS1bout);            
        end
        
        %{
        % Keep Collapsed, here Lies Dragons
        function mt = mtotal(obj,t)                    % create m(t) vector for entire vehicle
            C1OxTk = obj.m_S1_OxTk + obj.m_ox1;        % Constant for S1 OxTk
            C1Mcase = obj.m_S1_Mcase + obj.m_fuel1;    % Constant for S1 Mcase
            C2OxTk = obj.m_S2_OxTk + obj.m_ox2;        % Constant for S2 OxTk
            C2Mcase = obj.m_S2_Mcase + obj.m_fuel2;    % Constant for S2 Mcase
            C3Itr = obj.m_S3_Itr + obj.m_ox3;          % Constant for S3 OxTk
            C3Mcase = obj.m_S3_Mcase + obj.m_fuel3;    % Constant for S3 Mcase
            tfairing = obj.tS2start + obj.tS2burn;     % t fairing separation
            
            % Define function for m(t) for entire vechicle
            mt = - obj.dmS1OX*t + C1OxTk - obj.dmS1FUEL.*t + C1Mcase + obj.m_S1_Itr + obj.m_S1_Con...
                 - obj.dmS2OX*t*(t>obj.tS2start) + C2OxTk - obj.dmS1FUEL*t*(t>obj.tS2start) + C2Mcase + obj.m_S2_Itr + obj.m_S2_Con...
                 - obj.dmS3OX*t*(t>obj.tS3start) + C3Itr - obj.dmS3FUEL*t*(t>obj.tS3start) + C3Mcase + obj.m_fairing - obj.m_fairing*(t>tfairing) + obj.m_PL;
        end
        %}
        
    end
    

end