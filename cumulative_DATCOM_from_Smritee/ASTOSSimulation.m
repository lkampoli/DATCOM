classdef ASTOSSimulation
    % When importing long variable names from the simulation.txt file they are cut off
    properties
        fname;                                                             % Declare fname property
        data;                                                              % Declare data property
        units;                                                             % Declare units property
    end
    
    methods
        % The below function populate the data property with a table created from fname
        function obj = ASTOSSimulation(fname)
            obj.fname = fname;                                             % Set fname property to passed fname argument
            opts = detectImportOptions(fname);                             % Locates a table the file fname and returns the import options for importing the table 
                                                                           % ouputs a DelimitedTestImportOptions object (because fname is a .txt)
            opts.VariableNamesLine = 2;                                    % Setting the VariableNamesLine property to 2 specifies the line number where variable names are located
            opts.VariableUnitsLine = 3;                                    % Setting the VariableUnitsLine property to 3 specifies the line number where variable units are located
            obj.data = readtable(fname, opts);                             % Populate data property with table read by readtable() from file fname with import options set to opts
            obj.units = obj.data.Properties.VariableUnits;                 % Populate units 
        end
        
        % The below function will search for keyword within VariableNames - keywords are case sensitive
        function VarsKW = SearchKeyWord(obj,KeyWord) 
            idx = strfind(obj.data.Properties.VariableNames,KeyWord);      % Use strfind to output starting indices of KeyWord within .Variablenames
            idxKW = ~cellfun('isempty',idx);                               % Find idx's that are not empty as they are the .VariableNames containing
            VarsKW = obj.data.Properties.VariableNames(idxKW);             % Store indices of .VariableNames containing the keyword as VarKW
            for i = 1:length(VarsKW)                                       % Loop over VarsKW and print .VariableNames containing the keyword to command window
                disp(VarsKW{i})
            end
        end
        % The below function will look up the unit for the passed VarName
        function Unit = LookupUnits(obj,VarName)
            Params = string(obj.data.Properties.VariableNames);            % Create string array from .VariableNames to populate UnitsTable
            Units = string(obj.units);                                     % Create string array from .units to populate UnitsTable
            UnitsTable = table(Params',Units','VariableNames',{'Parameter','Units'});   % Create unit table that list the output parameter next to its units
            idx = find(strcmp(UnitsTable{:,1},VarName));                   % Find which parameter in UnitTable is equal to the VarName string                         
            Unit = table(UnitsTable{idx,1},UnitsTable{idx,2},'VariableNames',{'Parameter','Units'});  % Print parameter equal to VarName and its Unit    
        end
        %{
                
        % The below function has never been untilized - Dragons may lie within
        
        function interpolated_data = interpolate(obj, dt)
            % Interpolate data with dt time basis
            [C,ia,ic] = unique(obj.data.Time); % get the unique indices

            interpolated_data = table();
            interpolated_data.Time = [obj.data.Time(1):dt:obj.data.Time(end)]';

            for i=obj.data.Properties.VariableNames
                i = i{1}; % Convert to character vector
                interpolated_data.(i) = interp1(obj.data.Time(ia), obj.data.(i)(ia), interpolated_data.Time);
            end
        end
        
        %}
   end
end


