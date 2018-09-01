unit uMessages;
{$INCLUDE 'ytd.inc'}

interface

const
  EOLN = #10;
  APPLICATION_TITLE = 'YouTube Downloader';

{gnugettext: scan-all}
const
  ERR_EXCEPTION_MESSAGE = 'Exception %s with message:'#10'%s'; // Message shown when an exception occurs. First %s is an exception type, second %s an exception message

  MSG_PRESS_ANY_KEY_TO_QUIT = 'Press any key to quit.'; // Shown when running YTD command-line from IDE, right before YTD is terminated
  MSG_PLAYLIST_ITEM = 'Playlist item %d'; // Default playlist item name, %d is count

  // Downloader errors
  ERR_DOWNLOADER_IS_NOT_PREPARED = 'Downloader is not prepared!'; // Attempted to access data which depends Prepare without running Prepare
  ERR_DOWNLOAD_NOT_IMPLEMENTED = 'Download of this content is not implemented.';
  ERR_VALIDATE_FILENAME_FAILED = 'Download to file "%s" was not allowed.';

  ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT = 'Failed to locate embedded object.';
  ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE = 'Failed to locate media info page.';
  ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE = 'Failed to download media info page.';
  ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE = 'Failed to prepare media info page.';
  ERR_INVALID_MEDIA_INFO_PAGE = 'Invalid media info page.';
  ERR_FAILED_TO_DOWNLOAD_SERVER_LIST = 'Failed to download server list.';

  ERR_FAILED_TO_LOCATE_MEDIA_INFO = 'Failed to locate media info.';
  ERR_FAILED_TO_LOCATE_MEDIA_TITLE = 'Failed to find media title.';
  ERR_FAILED_TO_LOCATE_MEDIA_URL = 'Failed to find media URL.';
  ERR_FAILED_TO_LOCATE_MEDIA_SERVER = 'Failed to find media server.';
  ERR_FAILED_TO_LOCATE_MEDIA_STREAM = 'Failed to find media stream.';

  ERR_HTTP_RESULT_CODE = 'HTTP request failed with error code %d.';
  ERR_HTTP_NO_DATA_READ = 'No data read from the connection.';

  ERR_VARIABLE_NOT_FOUND = 'Variable "%s" not found.';

  ERR_SEE_LOGFILE = 'Error message listed in file "%s".';

  ERR_FAILED_TO_LOAD_DLL = 'Failed to load library "%s" or its prerequisites.';

{gnugettext: reset}

function _(const Msg: WideString): string;

implementation

uses
  uLanguages;

function _(const Msg: WideString): string;
begin
  Result := uLanguages._(Msg);
end;

end.
