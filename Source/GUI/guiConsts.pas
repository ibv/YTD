unit guiConsts;
{$INCLUDE 'ytd.inc'}

interface

uses
  uDownloadThread;
  
{gnugettext: scan-all}
const
  THREADSTATE_WAITING = 'Waiting'; // GUI: Download thread state: Waiting for its turn
  THREADSTATE_PREPARING = 'Preparing'; // GUI: Download thread state: Preparing download (getting title, URL...)
  THREADSTATE_DOWNLOADING = 'Downloading'; // GUI: Download thread state: Downloading
  THREADSTATE_FINISHED = 'Finished'; // GUI: Download thread state: Download finished successfully
  THREADSTATE_FAILED = 'Failed'; // GUI: Download thread state: Download failed
  THREADSTATE_ABORTED = 'Aborted'; // GUI: Download thread state: Download was aborted by user

{$IFDEF CONVERTERS}
const
  CONVERTTHREADSTATE_WAITING = 'Awaiting conversion'; // GUI: Convert thread state: Waiting for its turn
  CONVERTTHREADSTATE_CONVERTING = 'Converting'; // GUI: Convert thread state: Converting
  CONVERTTHREADSTATE_FINISHED = 'Converted'; // GUI: Convert thread state: Conversion finishes successfully
  CONVERTTHREADSTATE_FAILED = 'Conversion failed'; // GUI: Convert thread state: Conversion failed

const
  CONVERTERS_NOCONVERTER = '** None **'; // GUI: description of a "no converter"
{$ENDIF}

{gnugettext: reset}

const
  ThreadStates: array[TDownloadThreadState] of string
              = (THREADSTATE_WAITING, THREADSTATE_PREPARING, THREADSTATE_DOWNLOADING, THREADSTATE_FINISHED, THREADSTATE_FAILED, THREADSTATE_ABORTED);

{$IFDEF CONVERTERS}
const
  ConvertThreadStates: array[TConvertThreadState] of string
              = (CONVERTTHREADSTATE_WAITING, CONVERTTHREADSTATE_CONVERTING, CONVERTTHREADSTATE_FINISHED, CONVERTTHREADSTATE_FAILED);
{$ENDIF}

implementation

end.
