# This is for debug-flavor. Do not remove. Package is stripped conditionally.
#%#define __os_install_post       %{nil}
# %#define __spec_install_post /usr/lib/rpm/brp-compress

%define name msdl
%define version 1.2.7-r1
%define sfx tar.gz
%define release 1
%define descr msdl is a downloader for mms:// or rtsp:// streams

Summary: %{descr}
Name: %{name}
Version: %{version}
Release: %{release}
Source0: %{name}-%{version}.%{sfx}
#Patch0: %{name}-rz.patch
License: see LICENSE
Group: System Environment/Libs
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
#Prefix: %#{_prefix}
#URL: 

%description

%{descr}


%prep

 # -n dirname
%setup -q 


%build



%configure

make

%install


%makeinstall

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYING README AUTHORS ChangeLog INSTALL NEWS
%{_prefix}/*
#%#config /etc/*

%changelog
* Mon Jul 19 2010 Jun Oyama <me_t_ro@yahoo.com>
- updated to 1.2.7-r2
* Sat Feb 27 2010 Jun Oyama <me_t_ro@yahoo.com>
- updated to 1.2.7-r1
* Thu Jan 10 2010 Jun Oyama <me_t_ro@yahoo.com>
- updated to 1.2.7
* Thu Dec 17 2009 Richard Zidlicky <rz@linux-m68k.org>
- created RPM specfile
