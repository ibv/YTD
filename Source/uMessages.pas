unit uMessages;
{$INCLUDE 'ytd.inc'}

interface

resourcestring
  EOLN = #10;

  ERR_DOWNLOADER_IS_NOT_PREPARED = 'Downloader is not prepared!';
  ERR_DOWNLOAD_NOT_IMPLEMENTED = 'Download of this content is not implemented.';
  ERR_VALIDATE_FILENAME_FAILED = 'Download to file "%s" was not allowed.';

  ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE = 'Failed to locate media info page.';
  ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE = 'Failed to download media info page.';
  ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE = 'Failed to prepare media info page.';
  ERR_FAILED_TO_LOCATE_MEDIA_INFO = 'Failed to locate media info.';
  ERR_FAILED_TO_LOCATE_MEDIA_TITLE = 'Failed to find media title.';
  ERR_FAILED_TO_LOCATE_MEDIA_URL = 'Failed to find media URL.';
  ERR_FAILED_TO_LOCATE_MEDIA_SERVER = 'Failed to find media server.';
  ERR_FAILED_TO_LOCATE_MEDIA_STREAM = 'Failed to find media stream.';

  ERR_HTTP_RESULT_CODE = 'HTTP request failed with error code %d.';
  ERR_NO_DATA_READ = 'No data read from the connection.';

  ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT = 'Failed to find embedded object.';
  ERR_FAILED_TO_DOWNLOAD_EMBEDDED_OBJECT = 'Failed to download embedded object.';
  ERR_INVALID_EMBEDDED_OBJECT = 'Embedded object is invalid.';

  ERR_VARIABLE_NOT_FOUND = 'Variable "%s" not found.';

  ERR_FAILED_TO_DOWNLOAD_SERVER_LIST = 'Failed to download server list.';

  ERR_SEE_LOGFILE = 'Error message listed in file "%s".';

  ERR_FAILED_TO_LOAD_DLL = 'Failed to load library "%s" or its prerequisites.';

implementation

end.
