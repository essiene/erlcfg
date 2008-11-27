Summary: Config file library for Erlang
Name: erlcfg
Version: 0.4
Release: 1
License: Custom
Group: System/Daemons
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-build
BuildRequires: leex
BuildRequires: erlang
Prereq: erlang


%description
Erlcfg is a config file library for use with Erlang. It has
support for basic erlang terms, and is easy to use.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
make DESTDIR=%buildroot install

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)

/usr/lib/erlang/lib/*
