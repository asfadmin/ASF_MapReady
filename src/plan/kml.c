#include "plan.h"
#include "plan_internal.h"

void kml_aoi(FILE *kml_file, double clat, double clon, Polygon *aoi)
{
  double lat_UL, lon_UL;
  double lat_UR, lon_UR;
  double lat_LL, lon_LL;
  double lat_LR, lon_LR;

  int z = utm_zone(clon);
  UTM2latLon(aoi->x[0], aoi->y[0], 0.0, z, &lat_UL, &lon_UL);
  UTM2latLon(aoi->x[1], aoi->y[1], 0.0, z, &lat_UR, &lon_UR);
  UTM2latLon(aoi->x[2], aoi->y[2], 0.0, z, &lat_LR, &lon_LR);
  UTM2latLon(aoi->x[3], aoi->y[3], 0.0, z, &lat_LL, &lon_LL);

  double lat_min=lat_UL, lat_max=lat_UL;
  double lon_min=lon_UL, lon_max=lon_UL;

  if (lat_UR < lat_min) lat_min = lat_UR;
  if (lat_LL < lat_min) lat_min = lat_LL;
  if (lat_LR < lat_min) lat_min = lat_LR;

  if (lat_UR > lat_max) lat_max = lat_UR;
  if (lat_LL > lat_max) lat_max = lat_LL;
  if (lat_LR > lat_max) lat_max = lat_LR;

  if (lon_UR < lon_min) lon_min = lon_UR;
  if (lon_LL < lon_min) lon_min = lon_LL;
  if (lon_LR < lon_min) lon_min = lon_LR;

  if (lon_UR > lon_max) lon_max = lon_UR;
  if (lon_LL > lon_max) lon_max = lon_LL;
  if (lon_LR > lon_max) lon_max = lon_LR;

  fprintf(kml_file, "<Placemark>\n");
  fprintf(kml_file, "  <description><![CDATA[\n");
  //fprintf(kml_file, "<strong>Area Of Interest</strong>\n");
  fprintf(kml_file, "<strong>Latitude Range</strong>: %5.1f to %5.1f<br>\n",
          lat_min, lat_max);
  fprintf(kml_file, "<strong>Longitude Range</strong>: %5.1f to %5.1f<br>\n",
          lon_min, lon_max);
  fprintf(kml_file, "  ]]></description>\n");
  fprintf(kml_file, "  <name>Area Of Interest</name>\n");
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n", clon);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n", clat);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>30</tilt>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <visibility>1</visibility>\n");
  fprintf(kml_file, "  <open>1</open>\n");
  fprintf(kml_file, "  <Style>\n");
  fprintf(kml_file, "    <LineStyle>\n");
  fprintf(kml_file, "      <color>ff0033ff</color>\n");
  fprintf(kml_file, "      <width>3</width>\n");
  fprintf(kml_file, "    </LineStyle>\n");
  fprintf(kml_file, "    <PolyStyle>\n");
  //fprintf(kml_file, "      <color>1fff5500</color>\n");
  fprintf(kml_file, "      <color>1f0011ff</color>\n");
  fprintf(kml_file, "    </PolyStyle>\n");
  fprintf(kml_file, "  </Style>\n");
  fprintf(kml_file, "  <Polygon>\n");
  fprintf(kml_file, "    <extrude>1</extrude>\n");
  fprintf(kml_file, "    <altitudeMode>absolute</altitudeMode>\n");
  fprintf(kml_file, "    <outerBoundaryIs>\n");
  fprintf(kml_file, "      <LinearRing>\n");
  fprintf(kml_file, "        <coordinates>\n");
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LL, lat_LL);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_LR, lat_LR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UR, lat_UR);
  fprintf(kml_file, "          %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "        </coordinates>\n");
  fprintf(kml_file, "      </LinearRing>\n");
  fprintf(kml_file, "    </outerBoundaryIs>\n");
  fprintf(kml_file, "  </Polygon>\n");
  fprintf(kml_file, "</Placemark>\n");
}

static void kml_overlap(FILE *kml_file, OverlapInfo *oi)
{
  double lat_UL, lon_UL;
  double lat_UR, lon_UR;
  double lat_LL, lon_LL;
  double lat_LR, lon_LR;

  UTM2latLon(oi->viewable_region->x[0], oi->viewable_region->y[0], 0.0,
             oi->utm_zone, &lat_UL, &lon_UL);
  UTM2latLon(oi->viewable_region->x[1], oi->viewable_region->y[1], 0.0,
             oi->utm_zone, &lat_UR, &lon_UR);
  UTM2latLon(oi->viewable_region->x[2], oi->viewable_region->y[2], 0.0,
             oi->utm_zone, &lat_LR, &lon_LR);
  UTM2latLon(oi->viewable_region->x[3], oi->viewable_region->y[3], 0.0,
             oi->utm_zone, &lat_LL, &lon_LL);

  fprintf(kml_file, "    <Polygon>\n");
  fprintf(kml_file, "      <extrude>1</extrude>\n");
  fprintf(kml_file, "      <altitudeMode>absolute</altitudeMode>\n");
  fprintf(kml_file, "      <outerBoundaryIs>\n");
  fprintf(kml_file, "        <LinearRing>\n");
  fprintf(kml_file, "          <coordinates>\n");
  fprintf(kml_file, "            %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "            %.12f,%.12f,7000\n", lon_LL, lat_LL);
  fprintf(kml_file, "            %.12f,%.12f,7000\n", lon_LR, lat_LR);
  fprintf(kml_file, "            %.12f,%.12f,7000\n", lon_UR, lat_UR);
  fprintf(kml_file, "            %.12f,%.12f,7000\n", lon_UL, lat_UL);
  fprintf(kml_file, "          </coordinates>\n");
  fprintf(kml_file, "        </LinearRing>\n");
  fprintf(kml_file, "      </outerBoundaryIs>\n");
  fprintf(kml_file, "    </Polygon>\n");

  //free(oi->viewable_region);
}

void write_pass_to_kml(FILE *kml_file, double lat, double lon, PassInfo *pi)
{
  int i;
  double t = pi->start_time;

  fprintf(kml_file, "<Placemark>\n");
  fprintf(kml_file, "  <description><![CDATA[\n");
  fprintf(kml_file, "<strong>Time</strong>: %s<br>\n", date_str(t));
  fprintf(kml_file, "Contains %d frame%s<br><br>\n", pi->num,
          pi->num==1?"":"s");

  for (i=0; i<pi->num; ++i) {
    fprintf(kml_file, "  <strong>Frame %d</strong><br>\n", i+1);
    OverlapInfo *oi = pi->overlaps[i];
    fprintf(kml_file, "    Time: %s<br>    Overlap: %5.1f%%<br>\n",
            date_str(oi->t), oi->pct*100);
  }

  fprintf(kml_file, "  ]]></description>\n");
  fprintf(kml_file, "  <name>%s</name>\n", date_str(t));
  fprintf(kml_file, "  <LookAt>\n");
  fprintf(kml_file, "    <longitude>%.10f</longitude>\n", lon);
  fprintf(kml_file, "    <latitude>%.10f</latitude>\n", lat);
  fprintf(kml_file, "    <range>400000</range>\n");
  fprintf(kml_file, "    <tilt>30</tilt>\n");
  fprintf(kml_file, "  </LookAt>\n");
  fprintf(kml_file, "  <visibility>1</visibility>\n");
  fprintf(kml_file, "  <open>1</open>\n");
  fprintf(kml_file, "  <Style>\n");
  fprintf(kml_file, "    <LineStyle>\n");
  fprintf(kml_file, "      <color>ffff9900</color>\n");
  fprintf(kml_file, "      <width>3</width>\n");
  fprintf(kml_file, "    </LineStyle>\n");
  fprintf(kml_file, "    <PolyStyle>\n");
  fprintf(kml_file, "      <color>1fff5500</color>\n");
  fprintf(kml_file, "    </PolyStyle>\n");
  fprintf(kml_file, "  </Style>\n");
  fprintf(kml_file, "  <MultiGeometry>\n");

  for (i=0; i<pi->num; ++i) {
    kml_overlap(kml_file, pi->overlaps[i]);
  }

  fprintf(kml_file, "  </MultiGeometry>\n");
  fprintf(kml_file, "</Placemark>\n");

/*
      printf("Found one (#%d in this sequence):\n"
             "   Time: %s\n"
             "   State Vector: position= %f, %f, %f\n"
             "                 velocity= %f, %f, %f\n"
             "   Imaged area: zone= %d\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "                %f %f\n"
             "   Percentage: %f\n",
             n, date_str(t), st->pos.x, st->pos.y, st->pos.z,
             st->vel.x, st->vel.y, st->vel.z,
             oi->utm_zone,
             oi->viewable_region->x[0], oi->viewable_region->y[0],
             oi->viewable_region->x[1], oi->viewable_region->y[1],
             oi->viewable_region->x[2], oi->viewable_region->y[2],
             oi->viewable_region->x[3], oi->viewable_region->y[3],
             oi->pct);
*/
}
