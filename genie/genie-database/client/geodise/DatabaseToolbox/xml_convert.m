function result = xml_convert (source, xslt_file, metadata_type)

error (nargchk (2, 3, nargin))

import java.lang.System
import java.lang.String
trans = org.geodise.database.util.XmlTransformer;

if exist('metadata_type')
    params = {'metadataType', metadata_type};
else
    params = {'metadataType', 'file_metadata'};
end

if ischar(source)
    result = char(trans.transform (source, xslt_file, params));
else
    result = trans.transform (source, xslt_file, params);
end