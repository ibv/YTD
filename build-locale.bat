@echo off
if exist Source\locale\default.~pot del Source\locale\default.~pot
if exist Source\locale\default.pot move Source\locale\default.pot Source\locale\default.~pot
dxgettext.exe --delphi -r -b Source -o Source\locale
move Source\locale\default.po Source\locale\default.pot
