{$IFNDEF GUI}
  {$IFNDEF CLI}
    {$IFDEF FPC}
      {$MESSAGE ERROR 'At least one interface (GUI or CLI) must be present.'}
    {$ELSE}
      {$IFDEF DELPHI6_UP}
        {$MESSAGE ERROR 'At least one interface (GUI or CLI) must be present.'}
      {$ELSE}
        Error: At least one interface (GUI or CLI) must be present.
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF GUI}
    {$IFNDEF GUI_WINAPI}
      {$MESSAGE ERROR 'Graphical version can only be built in GUI_WINAPI mode.'}
    {$ENDIF}
  {$ENDIF}
  {$MESSAGE WARN 'FPC implementation of GetText is faulty. Multilanguage capabilities were removed.'}
  {$MESSAGE WARN 'FPC version needs pcrelib.dll. Find it in Source\Lib\PerlRegEx.'}
{$ENDIF}

