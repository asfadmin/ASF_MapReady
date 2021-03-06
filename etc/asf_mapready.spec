Summary: ASF Tools for processing SAR data
Name: %{package_name}
Version: %{asfversion}
Release: %{buildnumber}
License: BSD
Group: Applications/Scientific
URL: http://www.asf.alaska.edu
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr/local

Requires: proj-nad

%description
The ASF MapReady Remote Sensing Toolkit is a set of tools for
processing SAR data, including importing the raw CEOS data,
geocoding to various map projections, terrain correction,
as well as exporting to various common formats including
JPEG and GeoTIFF.

The ASF MapReady Remote Sensing Toolkit now supports the
processing of ALOS data.
%prep
echo Executing: %%prep
 
%build
echo Executing: %%build
./configure --prefix=$RPM_BUILD_ROOT/usr/local no-werror
make mapready

%install
echo Executing: %%install
rm -rf $RPM_BUILD_ROOT
make install
rm -rf $RPM_BUILD_ROOT/usr/local/lib

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(666,root,root,777)
%attr(-,root,root) /usr/local/bin
/usr/local/share
