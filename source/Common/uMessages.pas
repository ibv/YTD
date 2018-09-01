(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit uMessages;
{$INCLUDE 'ytd.inc'}

interface

const
  EOLN = #13#10;
  APPLICATION_TITLE = 'YouTube Downloader';
  APPLICATION_VERSION = {$INCLUDE 'ytd.version'};
  APPLICATION_CAPTION = APPLICATION_TITLE + ' v' + APPLICATION_VERSION;
  APPLICATION_URL = 'http://www.pepak.net/download/youtube-downloader/';
  APPLICATION_SHORTCUT = APPLICATION_TITLE + '.lnk';

const
  DONATE_URL = 'https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=paypal.com@pepak.net&currency_code=USD';
  BUGREPORT_URL = 'http://ytd.pepak.net/bugreport.php?version=%s&url=%s&error=%s';


{gnugettext: scan-all}
resourcestring
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
  ERR_MEDIA_REMOVED = 'Media file was removed from the server.';
  ERR_SERVER_ERROR = 'Server returned an error: %s';

  ERR_FAILED_TO_LOAD_DLL = 'Failed to load library "%s" or its prerequisites.';

{gnugettext: reset}

implementation

end.

