function Build_for005(filename,X,R,DISCON, MachList, AltList, AoAList ,LRef, SRef)
  % This function builds the for005.dat input card
  % Function editted 11.11.19 to take in MachList & AltList and cross
  % checked against for005.dat created from original function
      
fid = fopen(filename,'w+');


fprintf(fid,'DIM M\n');

fprintf(fid,'$REFQ \n');
fprintf(fid,'    XCG = 0.000,\n');                                         % Longitudinal position of C.G (+ve aft) Default value 0
fprintf(fid,['    SREF = ' num2str(SRef,'%10.3f'), ',\n']);                % Maximum cross sectional area of vehicle base
fprintf(fid,['    LREF = ' num2str(LRef,'%10.3f'), ',\n']);                % Maximum diameter of Vehicle      
fprintf(fid,'$END\n');

fprintf(fid,'$AXIBOD\n');
	fprintf(fid,['	NX = ' num2str(length(X),'%10.3f') ',\n']);
	fprintf(fid,'	X =');
    for i = 1:length(X)
        fprintf(fid,['    ' num2str(X(i),'%10.3f') ',\n']);
    end
    fprintf(fid,'	R =');
    for i = 1:length(R)
        fprintf(fid,['    ' num2str(R(i),'%10.3f') ',\n']);
    end
    if DISCON(1) < X(end)
        fprintf(fid,'	DISCON = ');
    end
    for i = 1:length(DISCON)
        if DISCON(i) < X(end)
            fprintf(fid,['    ' num2str(DISCON(i),'%10.3f') ',\n']);
        end
    end
fprintf(fid,'    DEXIT = 0.000,\n');                                       % DEXIT = 0.0 gives maximum base drag
fprintf(fid,'$END\n');

%fprintf(fid,'DAMP\n');
fprintf(fid,'PRESSURES\n');
fprintf(fid,'PLOT\n');
%fprintf(fid,'PRINT GEOM BODY\n');
for i = 1:length(AltList)
    fprintf(fid,'$FLTCON \n');
    fprintf(fid,['	NALPHA = ' num2str(length(AoAList),'%10.3f') ',\n']);
    fprintf(fid,'	ALPHA = ');
    for j = 1:length(AoAList)
        fprintf(fid,['    ' num2str(AoAList(j),'%10.1f') ',\n']);
    end
    fprintf(fid,['	NMACH = ' num2str(length(MachList),'%10.1f') ',\n']);
    fprintf(fid,'	MACH = ');
    for j = 1:length(MachList)
        fprintf(fid,['    ' num2str(MachList(j),'%10.1f') ',\n']);
    end
    fprintf(fid,'	ALT = ');
    for j = 1:length(MachList)
        fprintf(fid,['    ' num2str(AltList(i),'%10.1f') ',\n']);
    end
    fprintf(fid,'$END\n');

    fprintf(fid,'SAVE\n');
    fprintf(fid,'$END\n');
    fprintf(fid,'NEXT CASE\n');
end

fclose(fid);                                                               % Closes file associated with identifier fid




