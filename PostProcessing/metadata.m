
% Extracting the metadata for all the variables
function metadata = metadata(Name)
% Name is the name of the h5 file
info = h5info(Name); 
nvar = size(info.Datasets,1);
VarNames = extractfield(info.Datasets,'Name');
for i=1:nvar
     a=info.Datasets(i);
     LongName{i} = a.Attributes.Value{1};
     Unit{i} = a.Attributes.Value{2};
     Dimension{i} = a.Attributes.Value{3};  
end
Metadata = [VarNames;LongName;Dimension;Unit]';
Metadata = cell2table(Metadata);
writetable(Metadata,'Metadata.xlsx');
disp('Metadata is saved as the Metadata.xlsx ');
%------------------------------------------------------
% Example:
% metadata('name of the h5 file')