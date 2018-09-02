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

unit guiConsts;
{$INCLUDE 'ytd.inc'}

interface

uses
  uDownloadThread;
  
resourcestring
  THREADSTATE_WAITING = 'Waiting'; // GUI: Download thread state: Waiting for its turn
  THREADSTATE_PREPARING = 'Preparing'; // GUI: Download thread state: Preparing download (getting title, URL...)
  THREADSTATE_DOWNLOADING = 'Downloading'; // GUI: Download thread state: Downloading
  THREADSTATE_FINISHED = 'Finished'; // GUI: Download thread state: Download finished successfully
  THREADSTATE_FAILED = 'Failed'; // GUI: Download thread state: Download failed
  THREADSTATE_ABORTED = 'Aborted'; // GUI: Download thread state: Download was aborted by user
  THREADSTATE_PAUSED = 'Paused'; // GUI: Download thread state: Download was paused by user

{$IFDEF CONVERTERS}
resourcestring
  CONVERTTHREADSTATE_WAITING = 'Awaiting conversion'; // GUI: Convert thread state: Waiting for its turn
  CONVERTTHREADSTATE_CONVERTING = 'Converting'; // GUI: Convert thread state: Converting
  CONVERTTHREADSTATE_FINISHED = 'Converted'; // GUI: Convert thread state: Conversion finishes successfully
  CONVERTTHREADSTATE_FAILED = 'Conversion failed'; // GUI: Convert thread state: Conversion failed
  CONVERTTHREADSTATE_FAILEDRUN = 'Converter not found'; // GUI: Convert thread state: Failed to start the converter

resourcestring
  CONVERTERS_NOCONVERTER = '** None **'; // GUI: description of a "no converter"

{$IFDEF CONVERTERSMUSTBEACTIVATED}
resourcestring
  CONVERTERS_INACTIVE_WARNING =
    'Converters are not activated.'#10#10 +
    'You must activate them through manually editing'#10 +
    'the configuration file. You can find the steps'#10 +
    'needed in the documenation.'#10#10 +
    'The reason why this is necessary is, converters'#10 +
    'NEED to be configured properly, as documented,'#10 +
    'but too many people failed to read the documentation'#10 +
    'and instead complained that converters don''t work.';
{$ENDIF}
{$ENDIF}

resourcestring
  MAINFORM_EDIT_CONFIG =
    'Config file will open now, but please do not edit'#10 +
    'it right away: YTD will overwrite the file before'#10 +
    'it quits, so any changes made would be lost.'#10 +
    'First quit YTD and only then start editing the file.';
  MAINFORM_CAN_CLOSE =
    'There are downloads in progress'#10 +
    'Do you really want to quit?';
  MAINFORM_NEW_VERSION_AVAILABLE =
    'A newer version (%s) is available.'#10 +
    'Do you want to download it?';
  MAINFORM_NEW_DEFS_VERSION_AVAILABLE =
    'A newer version (%s) of script definitions'#10 +
    'is available.'#10 +
    'Do you want to download it?';
  MAINFORM_ENTER_VIDEO_URL =
    'Enter video URL:';
  MAINFORM_ENTER_PAGE_URL =
    'Enter page URL:';
  MAINFORM_URL_NOT_SUPPORTED =
    'This URL is not supported.'#10 +
    'See the documenation for supported URLs.';
  MAINFORM_NO_SUPPORTED_URL =
    'No supported URLs found.'#10 +
    'See the documenation for supported URLs.';
  MAINFORM_DELETE_TRANSFERS =
    'Do you really want to delete selected transfer(s)?';
  MAINFORM_STOP_TRANSFERS =
    'Do you really want to stop selected transfer(s)?';
  MAINFORM_REPORT_BUG =
    'Do you really want to report a bug for this transfer?';
  MAINFORM_NOBUGREPORTIFDOWNLOADSTARTED =
    'Bugreport is not available for this video.'#10 +
    'After the download has successfully started,'#10 +
    'its completion is dependent on the goowill'#10 +
    'of the server and the quality of your network'#10 +
    'connection. The program can''t influence it.';
  MAINFORM_CONVERT_WITH =
    'Convert selected files with';
  MAINFORM_AUTOCONVERT_WITH =
    'Automatically convert with';

const
  ThreadStates: array[TDownloadThreadState] of string
              = (THREADSTATE_WAITING, THREADSTATE_PREPARING, THREADSTATE_DOWNLOADING, THREADSTATE_FINISHED, THREADSTATE_FAILED, THREADSTATE_ABORTED);

{$IFDEF CONVERTERS}
const
  ConvertThreadStates: array[TConvertThreadState] of string
              = (CONVERTTHREADSTATE_WAITING, CONVERTTHREADSTATE_CONVERTING, CONVERTTHREADSTATE_FINISHED, CONVERTTHREADSTATE_FAILED, CONVERTTHREADSTATE_FAILEDRUN);
{$ENDIF}

const
  ThreadStateImgs: array[TDownloadThreadState] of integer
                 = (-1, 3, 3, 2, 1, 0);

{$IFDEF CONVERTERS}
const
  ConvertThreadStateImgs: array[TConvertThreadState] of integer
                 = (4, 6, 5, 1, 1);
{$ENDIF}

{$IFDEF SINGLEINSTANCE}
const
  COPYDATA_URL = 12345;
{$ENDIF}

const
  MAX_DOWNLOAD_SIZE_FOR_BUGREPORT = 128*1024;

implementation

end.
