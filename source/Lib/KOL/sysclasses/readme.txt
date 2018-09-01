*************************************************
* SysUtils and Classes replacements.            *
* Prepared by Yury Sidorov (jura@blagovest.com) *
* Rev 1.0                                       *
*************************************************

This replacements can be used in your KOL application when you use non-KOL libraries or components. Usually they are linked to SysUtils and Classes units.
Usage of standard SysUtils unit adds ~25K to KOL application size in Delphi5. Usage of standard Classes unit adds ~15,5K more, resulting total ~40,5K increase of application size.
With modified SysUtils and Classes units your application will grow for ~13,5K when using SysUtils and for ~14K when using both SysUtils and Classes! So you can remove 26,5K of unused code from your application when using this replacements.
This replacements can be used with SYSTEM.DCU replacement, that can be found on KOL web site.

Requirements:
-------------
The original source files SysUtils.pas and Classes.pas were taken from Delphi 5 distributive. All tests was performed in Delphi 5 only. But I almost sure that this replacements can be used also with D4 and D6.

Installation:
-------------
DO NOT REPLACE EXISTING FILES with new ones. Place it in any other directory where You wish, and add path to it in your project AS FIRST path.

Performed changes:
------------------

SysConst.pas
************
- All strings made as const instead of resourcestring
- Messages are simplified to be smaller (as in err.pas by Vladimir Kladov)

SysUtils.pas
************

- Removed usage of predefined months and days of weak. If needed English locale data is used.
- Removed usage of overbloated Format, FormatBuf functions inside unit. Instead of them used tiny KOL Format function.

Classes.pas
***********

- Removed unnecessary initialization tasks.
- Removed usage of TReader and TWriter classes.
