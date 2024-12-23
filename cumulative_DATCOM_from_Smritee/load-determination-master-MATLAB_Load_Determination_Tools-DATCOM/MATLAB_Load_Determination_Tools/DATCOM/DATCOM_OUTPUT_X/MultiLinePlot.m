function MLP = MultiLinePlot(array)
  %{
  20.8.19 - This function will take in an array with max size nrows, ncols =< 36.
  Can only plot 35 distinct lines therefore ncols must be =< 36 because x axis values are stored in array(:,1)
  Turns grid on & displays a legend at default position
  Legend Label: 'st x' where x is the rocket station number
  %}
  
  % Define vectors of plot options
  mkr_style = ['o','*','x','s','d','^','v','>','<','p','h'];                    % Vector of markerstyle options
  l_style = {"-";"--";":";"-."};                                                % Cellarray of linestyle options
  
  % Create vector RGB colours to plot distinct coloured lines
  C = [linspace(0,1,100); ones(1,100); ones(1,100)]';                           % Create hsv triplets at full saturation and value that cover the whole colour (i.e. hues) spectrum
  C = hsv2rgb(C);                                                               % Convert to rgb triplets to be used as a 'colour array'
  colour = zeros(13,3);                                                         % Create empty vector to fill
  keepers = [7,14,21,28,50,58,66,74,82,92,1];                                   % indices of C that have useful colours

  % Construct vector of colours using every 5th entry in C
  colour(1,:) = [0, 0, 0];                                                      % Add Black
  colour(2,:) = [0.5, 0.5, 0.5];                                                % Add Grey
  for j = 1:length(keepers)                                                     % Add colours at indices in keepers
    colour(j + 2,:) = C(keepers(j), :);                                         % Total length of this vector is 13 19.8.19
  end
  
  % Declare variables needed for loop
  i_col = 1;                                                                    % index variable for colour vector    
  i_mkr = 1;                                                                    % index variable for markerstyle vector   
  [nrows, ncols] = size(array);                                                  % create variables for size of array
  
  for i = 1:(length(array) - 1) % - 1 becuase first column is for the x axis
    % if statement allows data to be plotted with the distinct colours contained in colour
    if i_col <= length(colour) && i <= (ncols - 1)
      plot(array(:,1), array(:,i + 1), 'color', colour(i,:))                    % Plot data sets varying the line colour 
      i_col = i_col + 1;                                                        % Increment index variable
      hold on
    elseif i_mkr <= length(mkr_style) && i <= (ncols - 1)
      plot(array(:,1), array(:,i + 1),'linestyle',l_style{1,1},...
      mkr_style(i - length(colour)),'color', colour(i - length(colour),:))      % Plot data sets varying the marker type while repeating colours
      i_mkr = i_mkr + 1;                                                        % Increment index variable  
    else
      if i <= (ncols - 1)
        plot(array(:,1), array(:,i + 1),'linestyle',l_style{1,1},...    
        mkr_style(i_mkr - (i_mkr - length(mkr_style))),...
        'color', colour(i_col - length(colour),:));                             % Plot lines starting for max index of mkr_style
        i_mkr = i_mkr + 1;                                                      % Increment index variable
        i_col = i_col + 1;                                                      % Increment index variable
      end
    end
  
  end

  for i = 1:length(array)
  Legs{i,1} = ['st ' num2str(i)];                                               % Loop through length of array to create cellarray of legend labels
  end

  legend("show");                                                               % Display Legend
  legend(Legs)                                                                  % Displays stored in Legs
  grid on                                                                       % Turn on grid
  
  end
