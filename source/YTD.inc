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
  // Allow providers which only work due to dirty hacks

{$DEFINE SHAREABLEFILES}
  // By default, the files being downloaded are opened in an exclusive mode
  // (because of some strange idea of Delphi that when a file is created,
  // share flags should not apply). This define provides a workaround, allowing
  // files being played while they are still downloading.

{$DEFINE GUI_WINAPI} 
  // GUI is created with WinAPI. Otherwise GUI is created with VCL.
  // At the moment, WinAPI GUI only works in Delphi 5 for an unknown reason.

{$DEFINE DELPHITHREADS}
  // Use Delphi's implementation of TThread. This implementation seems to
  // be incompatible with WinAPI GUI under Delphi 2009, for an unknown reason,
  // so alternative implementations are used if this define is not set.
  // Note: The same is true for FPC.
  // What doesn't work:
  //   - Synchronize hangs until the application finishes. Obviously, since
  //     thread messages appear in TApiForm's message loop rather than 
  //     the application's. "Fixed" by removing all Synchronize's.
  //   - WaitFor fails with an error. No idea why.
  //   - OnTerminate doesn't get called. No idea why. So I call it manually.

{$STACKFRAMES OFF}
{$BOOLEVAL OFF}

//----Build options------------------------------------------------------------
// These options are intended to be set in the BUILD.BAT or Project Options

{.DEFINE DEBUG}
  // Include debug code

{.DEFINE RELEASE} 
  // Remove unnecessary debug information

{.DEFINE NO_GUI}  
  // Don't compile GUI version

{.DEFINE NO_CLI}  
  // Don't compile command-line version

{.DEFINE NO_XXX}  
  // Don't compile support for porn providers

//----Don't change this!-------------------------------------------------------

{$UNDEF GUI}
{$UNDEF CLI}
{$UNDEF XXX}

{$IFNDEF NO_GUI}
  {$DEFINE GUI}
{$ENDIF}

{$IFNDEF NO_CLI}
  {$DEFINE CLI}
{$ENDIF}

{$IFNDEF NO_XXX}
  {$DEFINE XXX}
{$ENDIF}



// There's a strange bug with WinAPI version of the GUI that manifests itself
// in Delphi 2009: Displaying any text in the downloads ListView very quickly
// eats up all available memory and/or handles and YTD crashes soon after. It
// doesn't happen with Delphi 5 or FreePascal.

{$IFDEF DELPHI2009_UP}
  {$IFNDEF DEBUG}
    {$UNDEF GUI_WINAPI}
  {$ENDIF}
{$ENDIF}



// A number of features are only available in the GUI version.

{$IFNDEF GUI}
  {$UNDEF SYSTRAY}
  {$UNDEF CONVERTERS}
  {$UNDEF THREADEDVERSION}
  {$UNDEF GUI_WINAPI}
{$ENDIF}



// FreePascal's implementation of GetText is faulty.

{$IFDEF FPC}
  {$UNDEF GETTEXT}
{$ENDIF}



// I can't get the systray icon to work under FreePascal.

{$IFDEF FPC}
  {$UNDEF GETTEXT}
  {$UNDEF SYSTRAY}
{$ENDIF}



// If there are no converters, I don't need activation for them.

{$IFNDEF CONVERTERS}
  {$UNDEF CONVERTERSMUSTBEACTIVATED}
{$ENDIF}



// In the debug mode, FASTMM is used to detect memory leaks.

{$IFDEF DEBUG}
  {$DEFINE FASTMM}
{$ENDIF}



// See {$DEFINE DELPHITHREADS} above.

{$IFDEF GUI_WINAPI}
  {$IFDEF DELPHI2009_UP}
    {$UNDEF DELPHITHREADS}
  {$ENDIF}
  {$IFDEF FPC}
    {$UNDEF DELPHITHREADS}
  {$ENDIF}
{$ENDIF}
