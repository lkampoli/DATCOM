function [CPlot] = PlotCoeffs(input1, input2, input3)
    
%   Plots estimated coefficents from DATCOM given an altitude value -
%   COMMIT ME!
    %   input1 is the "sumd" DATCOM outout in table variable type
    %   input2 is your chosen value for altitude in meters
    %   input3 is your chosen value for angle of attack in degrees
    %   DATCOM_Output array needs to have columns in the format:
    %   Altitude    Mach    AoA     XList   Ca  Cn  Cm

    AltList = unique(input1{:,1});                                         % Extracts AltList from OUTPUT
    MaList = unique(input1{:,2});                                          % Extracts MachList from OUTPUT
    AoAList = unique(input1{:,3});                                         % Extracts AoAList from OUTPUT
    XList = unique(input1{:,4});                                           % Extracts Xseg from OUTPUT

    Alt = interp1(AltList, AltList, input2, 'nearest');                    % Rounds altitude input to nearest value - Reviewed will round up if query value lies between two values
    AoA = interp1(AoAList, AoAList, input3, 'nearest');                    % Rounds AoA input to nearest value - Reviewed will round up if query value lies between two values

    c1 = input1{:,1} == Alt;                                               % Reads altitude column (A) from OUTPUT
    c3 = input1{:,3} == AoA;                                               % Reads AoA column (C) from OUTPUT
    % Create new figure to plot coefficients in
    CPlot = figure('Name',sprintf('Aero Coeffs - Alt = %8.2f m and AoA - %3.1f',input2, input3));
    
    % Declare variables for coeffs columns
    CNcol = 6;
    CAcol = 7;
    CMcol = 8;
    
    % Define vectors of plot options
    mkr_style = ['o','*','x','s','d','^','v','>','<','p','h'];       % Vector of markerstyle options
    l_style = ["-";"--";":";"-."];                                   % Cellarray of linestyle options

    % Create vector RGB colours to plot distinct coloured lines
    C = [linspace(0,1,100); ones(1,100); ones(1,100)]';                           % Create hsv triplets at full saturation and value that cover the whole colour (i.e. hues) spectrum
    C = hsv2rgb(C);                                                               % Convert to rgb triplets to be used as a 'colour array'
    colour = zeros(13,3);                                                         % Create empty vector to fill
    keepers = [7,14,21,28,50,58,66,74,82,92,1];                                   % indices of C that have useful colours

    % Construct vector of colours using every 5th entry in C
    colour(1,:) = [0, 0, 0];                                                      % Add Black
    colour(2,:) = [0.5, 0.5, 0.5];                                                % Add Grey
    for j = 1:length(keepers)                                                     % Add colours at indices in keepers
       colour(j + 2,:) = C(keepers(j), :);                                        % Total length of this vector is 13 19.8.19
    end
          
    % For loop to filter the coefficients and create graphs
    for nlines = 1:length(XList)
        X = XList(nlines);
        c4 = input1{:,4} == X;                                             % Reads positon column (D) from OUTPUT

        Cn = input1{c1 & c3 & c4, CNcol};                                  % CN array at chosen altitude, angle and positon
        Ca = input1{c1 & c3 & c4, CAcol};                                  % CA array at chosen altitude, angle and positon
        Cm = input1{c1 & c3 & c4, CMcol};                                  % CM array at chosen altitude, angle and positon

        LnSetn = UniqueLines(nlines);
        % We now have all the data to plot aerodynamic coefficients
        subplot(3,1,1);
        plot(MaList, Cn ,'Color', colour(LnSetn(1),:))
        ylabel('C_N');
        title(sprintf('Aero. Coeffs - Alt = %8.2f and AoA = %3.1f',input2, input3))
        grid on
        hold on
        subplot(3,1,2);
        plot(MaList, Ca ,'Color', colour(LnSetn(1),:))
        ylabel('C_A')
        grid on
        hold on
        subplot(3,1,3);
        % Below works wrt varing LineStlye & Marker
        % plot(MaList, Cm ,'Color', colour(LnSetn(1),:),'LineStyle',l_style(2),'Marker',mkr_style(3))
        plot(MaList, Cm ,'Color', colour(LnSetn(1),:))
        ylabel('C_M');
        grid on
        hold on
        
    end
    xlabel('Mach Number [-]')       % Create xlabel for last subplot only
    
    for i = 1:length(XList)
        Legs{i,1} = ['st ' num2str(i)];                                    % Loop through length of array to create cellarray of legend labels
    end
    legend("show");                                                        % Display Legend
    legend(Legs)                                                           % Displays stored in Legs
    
    
    
    

    
    
    
    
    
    
    
    
    
    
end