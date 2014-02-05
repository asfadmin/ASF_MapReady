Name: %{package_name}
Version: %{asf_version}
Release: %{build_number}
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr/local

%description
The ASF MapReady Remote Sensing Toolkit is a set of tools for
processing SAR data, including importing the raw CEOS data,
geocoding to various map projections, terrain correction,
as well as exporting to various common formats including
JPEG and GeoTIFF.

The ASF MapReady Remote Sensing Toolkit now supports the
processing of ALOS data.

%build
scons

%install
scons install --prefix=%{buildroot}%{_prefix}

%files
