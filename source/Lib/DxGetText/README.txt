A lot of Object Pascal improvements were made between Delphi 5 and
Delphi 6. For this project, Delphi 5 lacks a lot of features, and
especially the improvement in widestring/string handling makes the
Delphi 5 implementation different in lots of ways.

Instead of cluttering up the gnugettext.pas file with a lot of
ifdefs, and in order to provide good version control and quality
control of the releases, there is a special version of gnugettext.pas
for Delphi 5 users in this directory.

Please report any problems with these files to Lars@dybdahl.dk or
dxgettext@yahoogroups.com.
