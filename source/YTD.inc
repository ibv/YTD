// What to build into YTD. These options are intended to be set in the BUILD.BAT or Project Options
{.DEFINE DEBUG}  // Include debug code
{.DEFINE NO_GUI} // Don't compile GUI version
{.DEFINE NO_CLI} // Don't compile command-line version
{.DEFINE NO_XXX} // Don't compile support for porn providers

{.DEFINE GUI_WINAPI} // GUI is created with WinAPI. Otherwise GUI is created with VCL

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

{$DEFINE MINIMIZEVIRTUAL}
  // Use virtual only where necessary. Each virtual method adds four (eight?)
  // bytes to each descendant of a class, which means that an unnecessary
  // virtual method in a TDownloader or TCommonDownloader adds an extra 1 KB
  // (v0.52) to the application size.

{$DEFINE SUBTITLES}
  // Global supportfor subtitles

{$DEFINE DIRTYHACKS}
  // Allow providers which only work due to dirty hacks

{$STACKFRAMES OFF}
{$BOOLEVAL OFF}

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

{$IFNDEF GUI}
  {$UNDEF SYSTRAY}
  {$UNDEF CONVERTERS}
  {$UNDEF THREADEDVERSION}
  {$UNDEF GUI_WINAPI}
{$ENDIF}

{$IFDEF FPC}
  {$UNDEF GETTEXT}
{$ENDIF}

{$IFNDEF CONVERTERS}
  {$UNDEF CONVERTERSMUSTBEACTIVATED}
{$ENDIF}

