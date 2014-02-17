Summary: ASF Tools for processing SAR data
Name: %{package_name}
Version: %{asfversion}
Release: %{buildnumber}
License: GPLv3
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr/local

BuildRequires: scons

%description
The ASF MapReady Remote Sensing Toolkit is a set of tools for
processing SAR data, including importing the raw CEOS data,
geocoding to various map projections, terrain correction,
as well as exporting to various common formats including
JPEG and GeoTIFF.

The ASF MapReady Remote Sensing Toolkit now supports the
processing of ALOS data.

%build
scons --pkg_version=%{version}-%{release}

%install
scons install --prefix=%{buildroot}%{prefix} --header_prefix=%{prefix} --pkg_version=%{version}-%{release}

%files
%{prefix}
