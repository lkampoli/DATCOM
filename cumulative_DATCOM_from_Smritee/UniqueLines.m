function [Output] = UniqueLines(nlines)
%UNTITLED Summary of this function goes here
%   inputArg1 - The number of lines that have been plotted
%   outputArg1 - 1 row 3 column array used to index through plot settings

          % Define vectors of plot options
          mkr_style = ['o','*','x','s','d','^','v','>','<','p','h'];       % Vector of markerstyle options
          l_style = ["-";"--";":";"-."];                                    % Cellarray of linestyle options
  
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
 
        
          if nlines <= length(colour)
             Output = [nlines, 1, 1];
          elseif nlines > length(colour) && nlines <= 2*length(colour)
              Output = [nlines - length(colour), 2 , 1];
          elseif nlines > length(colour) && nlines <= 3*length(colour)
              Output = [nlines - 2*length(colour), 3 , 1];
          elseif nlines > length(colour) && nlines <= 4*length(colour)
              Output = [nlines - 3*length(colour), 4 , 1];
          elseif nlines > length(colour) && nlines <= 5*length(colour)
              Output = [nlines - 4*length(colour), 5 , 1]; 
          elseif nlines > length(colour) && nlines <= 6*length(colour)
              Output = [nlines - 5*length(colour), 6 , 1];                
          else
              disp('What are you doing?!?!?!, RUUUUUN!')
          end
          
          
  
end

