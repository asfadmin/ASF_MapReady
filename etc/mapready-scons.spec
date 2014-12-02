Summary: The ASF Mapready suite for processing SAR and other data
Name: %{package_name}
Version: %{asfversion}
Release: %{buildnumber}
License: GPLv3
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr/local
Autoprov: 0
Requires: proj-nad
BuildRequires: scons

# exclude the internal libasf libraries from being required
%filter_from_requires /libasf/d
%filter_setup

# clean out the share/asf_tools directory to avoid leaving cruft
%pre
rm -rf %{prefix}/share/asf_tools

%description
The ASF MapReady Remote Sensing Toolkit is a set of tools for
processing SAR data, including importing the raw CEOS data,
geocoding to various map projections, terrain correction,
as well as exporting to various common formats including
JPEG and GeoTIFF.

The ASF MapReady Remote Sensing Toolkit now supports the
processing of ALOS data.

%build
scons --pkg_version=%{version}-%{release} --release_build

%install
scons install --prefix=%{buildroot}%{prefix} --header_prefix=%{prefix} --pkg_version=%{version}-%{release} --release_build

%files
%{prefix}
