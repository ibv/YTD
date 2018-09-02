{$INCLUDE 'lib\pepak\jedi.inc'}

//-----Behavior/functionality--------------------------------------------------

{$DEFINE MULTIDOWNLOADS}
  // Allow multiple downloads per URL (needed for some downloaders)

{$DEFINE SYSTRAY}
  // Minimize to systray

{$DEFINE GETTEXT}
  // Multilanguage support

{$DEFINE THREADEDVERSION}
  // Check for new version done in an extra thread (in background)

{$DEFINE CONVERTERS}
  // Support for converters: applications called after a download is finished

{$DEFINE CONVERTERSMUSTBEACTIVATED}
  // Converters must be manually activated before they can be used. This option
  // is necessary because a vast majority of users didn't bother to read the
  // documentation and then complained that converters don't work.

{$DEFINE MINIMIZESIZE}
  // Minimize size of the application. For example, use fewer virtual methods
  // because each adds four (eight?) bytes to each descendant's size.

{$DEFINE SUBTITLES}
  // Global support for subtitles

{$DEFINE MAXFILENAMELENGTH}
  // Limit filename length

{.DEFINE FASTMM}
  // Use the FastMM memory manager. By default it is only used in debug mode.

{$DEFINE DIRTYHACKS}
  // Allow providers which only work due to dirty hacks. The DEFINE also
  // marks parts of the code which should be rewritten if I only knew how.

{$DEFINE SHAREABLEFILES}
  // By default, the files being downloaded are opened in an exclusive mode
  // (because of some strange idea of Delphi that when a file is created,
  // share flags should not apply). This define provides a workaround, allowing
  // files being played while they are still downloading.

{$DEFINE GUI_WINAPI} 
  // GUI is created with WinAPI. Otherwise GUI is created with VCL. The WinAPI
  // GUI has several advantages, such as a double-buffered download list or
  // a compatibility with FreePascal, not mentioning a seriously reduced
  // executable size, but it is much more difficult to write (and understand, 
  // if the programmer has little or no experience with Windows API).

{$DEFINE DELPHITHREADS}
  // Use Delphi's implementation of TThread. This implementation seems to
  // be incompatible with WinAPI GUI under Delphi 2009, for an unknown reason,
  // so alternative implementations are used if this define is not set.
  // Note: The same is true for FPC.
  // What doesn't work:
  //   - Synchronize hangs until the application finishes. Obviously, since
  //     thread messages appear in TApiForm's message loop rather than 
  //     the TApplication's. "Fixed" by replacing Synchronize's with critical
  //     sections.
  //   - WaitFor fails with an error. No idea why.
  //   - OnTerminate doesn't get called. No idea why. So I call it manually.

{$DEFINE DIRECTDOWNLOADERS}
  // Publish direct downloaders for non-HTTP protocols (RTMP, MMS). Basically,
  // this allows YTD to work as a simplified RTMPDUMP/MSDL (without support
  // for extra parameters, only URL is supported).

{$DEFINE DOWNLOADTOTEMPFILES}
  // Use temporary files for downloads, then rename them when the download
  // is complete.

{.DEFINE JSON}
  // Many providers use JSON for their data storage. While it may be a good idea
  // to use a JSON parser for this data, in fact it is often better to simply
  // use regular expressions: One, it is shorter, and two, it turns out LkJSON
  // has bugs which may prevent the parsing.

{.DEFINE NONWORKING}
  // Build the non-working providers in. By default they are only available
  // in the debug mode.

{$STACKFRAMES OFF}
{$BOOLEVAL OFF}

//----Build options------------------------------------------------------------
// These options are intended to be set in the BUILD.BAT or Project Options.
// They are only listed here for reference and documentation. DO NOT define
// them here.

{.DEFINE DEBUG}
  // Include debug code

{.DEFINE RELEASE} 
  // Remove unnecessary debug information

{.DEFINE NO_GUI}
  // Don't compile the GUI version

{.DEFINE NO_CLI}
  // Don't compile the command-line version

{.DEFINE NO_SETUP}
  // Don't compile the setup/install/upgrade code

{.DEFINE NO_XXX}  
  // Don't compile support for porn providers

//----Don't change this!-------------------------------------------------------

{$UNDEF GUI}
{$UNDEF CLI}
{$UNDEF XXX}
{$UNDEF SETUP}
{$UNDEF SETUP_GUI}

{$IFNDEF NO_GUI}
  {$DEFINE GUI}
{$ENDIF}

{$IFNDEF NO_CLI}
  {$DEFINE CLI}
{$ENDIF}

{$IFNDEF NO_XXX}
  {$DEFINE XXX}
{$ENDIF}

{$IFNDEF NO_SETUP}
  {$DEFINE SETUP}
{$ENDIF}



// A number of features are only available in the GUI version.

{$IFNDEF GUI}
  {$UNDEF SYSTRAY}
  {$UNDEF CONVERTERS}
  {$UNDEF THREADEDVERSION}
  {$UNDEF GUI_WINAPI}
  {$UNDEF SETUP}
{$ENDIF}



// FreePascal's implementation of GetText is faulty.

{$IFDEF FPC}
  {$UNDEF GETTEXT}
{$ENDIF}



// I can't get the systray icon to work under FreePascal.

{$IFDEF FPC}
  {$UNDEF SYSTRAY}
{$ENDIF}



// If there are no converters, I don't need activation for them.

{$IFNDEF CONVERTERS}
  {$UNDEF CONVERTERSMUSTBEACTIVATED}
{$ENDIF}



// Setup functions may provide a GUI
{$IFDEF SETUP}
  {$IFDEF GUI}
    {$DEFINE SETUP_GUI}
  {$ENDIF}
{$ENDIF}



// In the debug mode:
//  - FASTMM is used to detect memory leaks.
//  - non-working providers are built in for testing

{$IFDEF DEBUG}
  {.$DEFINE FASTMM}
  {.$DEFINE NONWORKING}
{$ELSE}
  {$UNDEF NONWORKING}
{$ENDIF}



// See {$DEFINE DELPHITHREADS} above.

{$IFDEF GUI_WINAPI}
  {$IFDEF DELPHI7_UP}
    {$UNDEF DELPHITHREADS}
  {$ENDIF}
  {$IFDEF FPC}
    {$UNDEF DELPHITHREADS}
  {$ENDIF}
{$ENDIF}
