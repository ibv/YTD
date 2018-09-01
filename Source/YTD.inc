{.DEFINE NO_GUI}
{.DEFINE NO_CLI}






//----Don't change this!-------------------------------------------------------
{$IFNDEF NO_GUI}
  {$DEFINE GUI}
{$ENDIF}

{$IFNDEF NO_CLI}
  {$DEFINE CLI}
{$ENDIF}

{$IFNDEF GUI}
  {$IFNDEF CLI}
    Error: At least one interface must be present
  {$ENDIF}
{$ENDIF}
