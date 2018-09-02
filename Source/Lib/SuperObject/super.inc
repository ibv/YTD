{$ifdef VER170}
  {$define VER170ORGREATER}
{$endif}
{$ifdef VER180}
  {$define VER170ORGREATER}
{$endif}
{$ifdef VER190}
  {$define VER170ORGREATER}
{$endif}
{$ifdef VER200}
  {$define VER170ORGREATER}
{$endif}
{$ifdef VER210}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
{$endif}
{$ifdef VER220}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
{$endif}
{$ifdef VER230}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER240}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER250}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER260}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER270}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER280}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER290}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER300}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}
{$ifdef VER310}
  {$define VER170ORGREATER}
  {$define VER210ORGREATER}
  {$define VER230ORGREATER}
{$endif}

{$ifndef UNICODE}
  {$ifndef FPC}
    {$define LEGACYVERSION}
  {$endif}
{$else}
  {$ifndef VER230ORGREATER}
    {$define LEGACYVERSION}
  {$ENDIF}
{$endif}

{$ifdef FPC}
  {$DEFINE HAVE_INLINE}
{$endif}
{$ifdef VER170ORGREATER}
  {$DEFINE HAVE_INLINE}
{$endif}

{$ifdef VER210ORGREATER}
  {$define HAVE_RTTI}
{$endif}

{$ifdef VER230ORGREATER}
  {$define NEED_FORMATSETTINGS}
{$endif}

{$ifdef FPC}
  {$ifdef VER2_6}
    {$define NEED_FORMATSETTINGS}
  {$endif}
{$endif}

