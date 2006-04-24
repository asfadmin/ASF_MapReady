#!/bin/sh

if [ $# -lt 2 ]
then
  echo "Usage: $0 <CEOS-file-basename> <dem-basename>"
  exit 1
fi

asf_import $1 test
cp test.img test_60m.img
cp test.meta test_60m.meta

#resample test test_60m 60
gr2sr test_60m test_sr2_60m

remap -scale 0.99667767023656 0.63338076567152 test_sr2_60m test_sr2_60m_scaled

create_dem_grid -w 4922 -h 8192 delta_fixed.img test_sr2_60m_scaled.img dem_grid
fit_poly dem_grid 5 dem_poly

remap -translate 0 0 -poly dem_poly -width 5589 -height 4906 -bilinear -float $2 dem_big.img

reskew_dem test_sr2_60m_scaled.meta dem_big.img dem_slant.img dem_sim_amp.img

trim -h 5189 -w 4906 dem_sim_amp.img dem_trimsim_amp.img 0 0

fftMatch -m dem.corr test_sr2_60m_scaled.img dem_trimsim_amp.img

neg()
{
  d=$1
  if [ ${d:0:1} == "-" ]
  then
    echo ${d:1}
  else
    echo "-${d}"
  fi   
}

dx=`cut -f1 <dem.corr`
dy=`cut -f2 <dem.corr`


trim -h 5189 -w 4906 dem_sim_amp.img dem_trimsim_amp.img `neg ${dy}` `neg ${dx}`
fftMatch -m dem.corr2 test_sr2_60m_scaled.img dem_trimsim_amp.img

trim -h 5189 -w 4906 dem_slant.img dem_trimmed_slant.img `neg ${dy}` `neg ${dx}`
deskew_dem -i test_sr2_60m_scaled.img 0 dem_trimmed_slant.img test_sr2_tc.img

asf_geocode -p utm test_sr2_tc test_sr2_tc_utm

echo "Terrain Correction Complete!"
