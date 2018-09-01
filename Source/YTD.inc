// What to build into YTD:
{.DEFINE NO_GUI} // Don't compile GUI version
{.DEFINE NO_CLI} // Don't compile command-line version
{.DEFINE NO_XXX} // Don't compile support for porn providers

// Behavior:
{$DEFINE MULTIDOWNLOADS} // Allow multiple downloads per URL (needed for some downloaders)
{$DEFINE SYSTRAY}        // Minimize to systray
{$DEFINE INIFILE}        // Use INI file to store default settings
{.DEFINE DEBUG}          // Include debug code



//----Don't change this!-------------------------------------------------------
{$UNDEF GUI}
{$UNDEF CLI}
{$UNDEF XXX}

{$IFNDEF NO_GUI}
  {$IFNDEF FPC}
    {$DEFINE GUI}
  {$ELSE}
    {$MESSAGE WARN 'GUI cannot be built with FPC (yet).'}
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
