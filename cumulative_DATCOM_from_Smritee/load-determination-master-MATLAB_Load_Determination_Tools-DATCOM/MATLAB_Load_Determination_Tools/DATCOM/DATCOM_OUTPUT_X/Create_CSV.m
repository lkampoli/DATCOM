function CSV = Create_CSV(ARRAY, filename)
  % This function takes in an array (doubles) and filename (char string) to create a CSV file
      % Call xlswrite to create the CSV file
  CSV = xlswrite([filename '.xlsx'], ARRAY);
end                                    
