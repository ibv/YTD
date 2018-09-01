// What to build into YTD. These options are intended to be set in the BUILD.BAT or Project Options
{.DEFINE NO_GUI} // Don't compile GUI version
{.DEFINE NO_CLI} // Don't compile command-line version
{.DEFINE NO_XXX} // Don't compile support for porn providers
{.DEFINE DEBUG}  // Include debug code
{.DEFINE KOL}    // Use Key Objects Library (http://kolmck.net)

// Behavior/functionality
{$DEFINE MULTIDOWNLOADS}  // Allow multiple downloads per URL (needed for some downloaders)
{$DEFINE SYSTRAY}         // Minimize to systray
{$DEFINE GETTEXT}         // Multilanguage support
{$DEFINE THREADEDVERSION} // Check for new version done in an extra thread (in background)
{$DEFINE CONVERTERS}      // Support for converters: applications called after a download is finished



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
{$ENDIF}

{$IFDEF FPC}
  {$UNDEF GETTEXT}
{$ENDIF}
