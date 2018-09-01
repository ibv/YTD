// What to build into YTD. These options are intended to be set in the BUILD.BAT
{.DEFINE NO_GUI} // Don't compile GUI version
{.DEFINE NO_CLI} // Don't compile command-line version
{.DEFINE NO_XXX} // Don't compile support for porn providers
{.DEFINE DEBUG}  // Include debug code
{.DEFINE KOL}    // Use Key Objects Library (http://kolmck.net)

// Behavior/functionality
{$DEFINE MULTIDOWNLOADS} // Allow multiple downloads per URL (needed for some downloaders)
{$DEFINE SYSTRAY}        // Minimize to systray
{$DEFINE INIFILE}        // Use INI file to store default settings
{$DEFINE GETTEXT}        // Multilanguage support



//----Don't change this!-------------------------------------------------------
{$UNDEF GUI}
{$UNDEF CLI}
{$UNDEF XXX}

{$IFNDEF NO_GUI}
  {$DEFINE GUI}
  {$IFDEF FPC}
    {$MESSAGE ERROR 'GUI cannot be built with FPC (yet).'}
  {$ENDIF}
{$ENDIF}

{$IFNDEF NO_CLI}
  {$DEFINE CLI}
{$ENDIF}

{$IFNDEF GUI}
  {$IFNDEF CLI}
    Error: At least one interface (GUI or CLI) must be present
  {$ENDIF}
{$ENDIF}

{$IFNDEF NO_XXX}
  {$DEFINE XXX}
{$ENDIF}

{$IFDEF FPC}
  {$UNDEF SYSTRAY}
{$ENDIF}
