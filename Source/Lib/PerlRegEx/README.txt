TPerlRegEx is a Delphi VCL wrapper around the open source PCRE library, which implements Perl-Compatible Regular Expressions.
The supplied pcrelib.dll contains PCRE 7.9, compiled with Unicode support.
The supplied OBJ files contain PCRE 7.9, compiled with Unicode support.

By default, the component uses the DLL.  Using the OBJ files can trigger an internal compiler error, particularly when installing the component into a package.  You can use the OBJ files by editing the compiler directives at the top of the pcre.pas file.

For more information about PCRE, please visit http://www.regular-expressions.info/pcre.html

For more information about regular expressions in general, please visit http://www.regular-expressions.info/

You can download the latest version of TPerlRegEx at http://www.regular-expressions.info/delphi.html

TPerlRegEx is licensed under the Mozilla Public License, version 1.1.

To install this component into Delphi, open the .dpk file for your Delphi version, compile it, and install it into the IDE.  The TPerlRegEx component will appear on the JGsoft page in the component palette.

Alternatively, you can create a new package or add the component to an existing package.  Simply add the files PerlRegEx.pas and pcre.pas to the package.

If you prefer to instantiate TPerlRegEx at runtime instead of dropping it on a form, you don't need to install its package into the Delphi IDE.  Simply add PerlRegEx to the uses clause of the units in which you want to use it.  There's no need to add the pcre unit to the uses clause.  This unit is used internally by TPerlRegEx.