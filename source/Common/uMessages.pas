(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
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
  APPLICATION_TITLE {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'YTD' {$IFNDEF XXX} + ' Lite' {$ENDIF} ;
  APPLICATION_VERSION {$IFDEF MINIMIZESIZE} : string {$ENDIF} = {$INCLUDE 'ytd.version'};
  APPLICATION_CAPTION {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'YTD' {$IFNDEF XXX} + ' Lite' {$ENDIF} + ' v' + {$INCLUDE 'ytd.version'};
  APPLICATION_URL {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'http://www.pepak.net/ytd';
  APPLICATION_SHORTCUT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'YTD.lnk';
  {$IFDEF SETUP}
  APPLICATION_UNINSTALL_ID {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'YTD_Pepak';
  {$ENDIF}

const
  DONATE_URL = 'https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=paypal.com@pepak.net&currency_code=USD';
  BUGREPORT_URL = 'http://ytd.pepak.net/bugreport.php?version=%s&defs=%s&url=%s&error=%s';
  MY_OPENSSL_URL = 'http://ytd.pepak.net/openssl';
  OPENSSL_URL = 'http://slproweb.com/products/Win32OpenSSL.html';
  MY_RTMPDUMP_URL = 'http://ytd.pepak.net/rtmpdump';
  RTMPDUMP_URL = MY_RTMPDUMP_URL;
  MY_MSDL_URL = 'http://ytd.pepak.net/msdl';
  MSDL_URL = MY_MSDL_URL;

{gnugettext: scan-all}
resourcestring
  ERR_EXCEPTION_MESSAGE = 'Exception %s with message:'#10'%s'; // Message shown when an exception occurs. First %s is an exception type, second %s an exception message
  ERR_INSTALL_FAILED = 'Installation failed.'; // Shown when installation failed
  MSG_PRESS_ANY_KEY_TO_QUIT = 'Press any key to quit.'; // Shown when running YTD command-line from IDE, right before YTD is terminated
  MSG_PLAYLIST_ITEM = 'Playlist item %d'; // Default playlist item name, %d is count
  MSG_DOWNLOAD_OR_UPGRADE = 'Do you want to upgrade (YES)'#10'or just download the newest version (NO)?'#10#10'Note: Upgrade will stop all transfers.'; // What to do with the newest version
  MSG_UPGRADING = 'Upgrading...'; // Downloading the upgrade data
  MSG_PROVIDER_DEFINITIONS_UPGRADED = 'Provider definitions were upgraded:';
  MSG_FAILED_TO_UPGRADE = 'Failed to execute the upgrade file.'; // Failed to execute the upgrade file
  MSG_FAILED_TO_DOWNLOAD_UPGRADE = 'Failed to download upgrade data: '; // Failed to download the upgrade file
  MSG_FAILED_TO_UPGRADE_DEFINITIONS = 'Failed to upgrade the script definitions.';
  MSG_FAILED_TO_DOWNLOAD_DEFINITIONS = 'Failed to download the script definitions.';
  MSG_OPENSSL_NOT_FOUND = 'You don''t seem to have the OpenSSL library (32bit) installed.'#10'YTD uses OpenSSL to enable the SSL protocol.'#10'Without SSL, you won''t be able to download'#10'from YouTube any many other sites.';
  MSG_OPENSSL_NOT_FOUND_ACTION_SUFFIX = 'Would you like to download and install OpenSSL?';
  MSG_RTMPDUMP_NOT_FOUND = 'You don''t seem to have the RtmpDump library installed.'#10'YTD uses RtmpDump to handle the RTMP protocol.'#10'Without RTMP, you won''t be able to download'#10'from many sites.';
  MSG_RTMPDUMP_NOT_FOUND_ACTION_SUFFIX = 'Would you like to download and install RtmpDump?';
  MSG_MSDL_NOT_FOUND = 'You don''t seem to have the MSDL library installed.'#10'YTD uses MSDL to handle various Microsoft protocols.'#10'Without them, you won''t be able to download'#10'from some sites.';
  MSG_MSDL_NOT_FOUND_ACTION_SUFFIX = 'Would you like to download and install MSDL?';
  MSG_EXTERNAL_LIBS_DOWNLOAD = 'Download URL:';
  MSG_EXTERNAL_LIBS_WERE_DOWNLOADED = 'External libraries have been downloaded and installed.'#10'Please restart YTD to activate them.';
  MSG_WANT_TO_UNINSTALL = 'Are you sure you want to uninstall YTD?';
  MSG_UNINSTALL_COMPLETE = 'Uninstallation completed.';

  // Downloader errors
  ERR_DOWNLOADER_IS_NOT_PREPARED = 'Downloader is not prepared!'; // Attempted to access data which depends Prepare without running Prepare
  ERR_DOWNLOAD_NOT_IMPLEMENTED = 'Download of this content is not implemented.';
  ERR_DOWNLOAD_EMPTY_URL = 'Download URL was not set.';
  ERR_DOWNLOAD_NOT_INITIALIZED = 'Failed to initialize the download structures.';
  ERR_DOWNLOAD_FAILED = 'Download failed.';
  ERR_RTMPDUMP_ERROR = 'RTMPDUMP error: %s';
  ERR_VALIDATE_FILENAME_FAILED = 'Download to file "%s" was not allowed.';

  ERR_FAILED_TO_LOCATE_EMBEDDED_OBJECT = 'Failed to locate embedded object.';
  ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE = 'Failed to locate media info page.';
  ERR_FAILED_TO_DOWNLOAD_MEDIA_INFO_PAGE = 'Failed to download media info page. Check your firewall!';
  ERR_FAILED_TO_PREPARE_MEDIA_INFO_PAGE = 'Failed to prepare media info page.';
  ERR_INVALID_MEDIA_INFO_PAGE = 'Invalid media info page.';
  ERR_FAILED_TO_DOWNLOAD_SERVER_LIST = 'Failed to download server list.';
  ERR_UNKNOWN_URL = 'Unknown URL: %s';

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
  ERR_SECURE_TOKEN_NOT_SET = 'Invalid configuration: Secure token not set.';
  ERR_ENCRYPTION_NOT_SUPPORTED = 'Encryption not yet supported.';
  ERR_LOGIN_FAILED = 'Login failed.';

  ERR_FAILED_TO_LOAD_DLL = 'Failed to load library "%s" or its prerequisites.';

  ERR_SCRIPTS_ERROR = 'Script error: ';
  ERR_SCRIPTS_URL_NOT_FOUND = 'URL not found.';
  ERR_SCRIPTS_EMPTY_URL_ENCOUNTERED = 'Url pattern is empty.';
  ERR_SCRIPTS_SCRIPT_NOT_FOUND = 'Requested script "%s" not found.';
  ERR_SCRIPTS_SCRIPT_MUST_BE_NONEMPTY = 'Script may not be empty.';
  ERR_SCRIPTS_PATTERN_MUST_BE_NONEMPTY = 'Search pattern may not be empty.';
  ERR_SCRIPTS_PATTERN_ID_MUST_BE_NONEMPTY = 'Pattern ID may not be empty.';
  ERR_SCRIPTS_PATTERN_NOT_FOUND = 'Requested regexp "%s" not found.';
  ERR_SCRIPTS_SUBEXPRESSION_MUST_BE_NONEMPTY = 'Subexpression name may not be empty.';
  ERR_SCRIPTS_SUBEXPRESSION_NOT_FOUND = 'Subexpression "%s" not found.';
  ERR_SCRIPTS_FAILED_TO_MATCH_REGEXP = 'Failed to match regular expression.';
  ERR_SCRIPTS_VARIABLE_NOT_FOUND = 'Variable "%s" not found.';
  ERR_SCRIPTS_VARIABLE_NAME_MUST_BE_NONEMPTY = 'Variable name may not be empty.';
  ERR_SCRIPTS_ATTRIBUTE_MUST_BE_NONEMPTY = 'Attribute "%s" may not be empty.';
  ERR_SCRIPTS_INVALID_ATTRIBUTE_VALUE = 'Attribute "%s" has invalid value "%s".';
  ERR_SCRIPTS_UNKNOWN_COMMAND = 'Unknown command.';
  ERR_SCRIPTS_UNKNOWN_COMMAND_NAMED = 'Unknown command "%s".';
  ERR_SCRIPTS_DOWNLOAD_FAILED = 'Download failed: %s';
  ERR_SCRIPTS_UNEXPECTED_NODE_TYPE = 'Unexpected node type.';
  ERR_SCRIPTS_MULTIPLE_ELSE = 'Multiple <else> tags encountered.';
  ERR_SCRIPTS_IF_NOT_SATISFIED = 'No satisfied condition found.';
  ERR_SCRIPTS_UNKNOWN_CONDITION = 'Unknown condition.';
  ERR_SCRIPTS_INVALID_PAUSE = 'Invalid <pause> settings.';
  ERR_SCRIPTS_XML_ELEMENT_NOT_FOUND = 'XML element not found.';
  ERR_SCRIPTS_JSON_ELEMENT_NOT_FOUND = 'JSON element not found.';

{gnugettext: reset}

implementation

end.

