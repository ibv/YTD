{$IFNDEF GUI}
  {$IFNDEF CLI}
    Error: At least one interface (GUI or CLI) must be present.
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF GUI}
    {$MESSAGE ERROR 'GUI cannot be built with FPC (yet).'}
  {$ENDIF}
  {$MESSAGE WARN 'FPC implementation of GetText is faulty. Multilanguage capabilities were removed.'}
  {$MESSAGE WARN 'FPC version needs pcrelib.dll. Find it in Source\Lib\PerlRegEx.'}
{$ENDIF}
