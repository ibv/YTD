{.DEFINE NO_GUI} // Don't compile GUI version
{.DEFINE NO_CLI} // Don't compile command-line version

{$DEFINE MULTIDOWNLOADS} // Allow multiple downloads per URL (needed for some downloaders)





//----Don't change this!-------------------------------------------------------
{$UNDEF GUI}
{$UNDEF CLI}

{$IFNDEF NO_GUI}
  {$DEFINE GUI}
{$ENDIF}

{$IFNDEF NO_CLI}
  {$DEFINE CLI}
{$ENDIF}

{$IFNDEF GUI}
  {$IFNDEF CLI}
    Error: At least one interface (GUI or CLI) must be present
  {$ENDIF}
{$ENDIF}
