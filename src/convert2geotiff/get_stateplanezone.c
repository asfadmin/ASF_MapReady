/*****************************************************************************
NAME: GET_STATEPLANEZONE

PURPOSE:  Returns the geoTIFF code for the LAS State Plane Zone.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   Must be run under TAE.

PROJECT:        LAS
*****************************************************************************/

#include "ddr.h"
#include "protos.h"

unsigned short get_stateplanezone
(
    struct DDR ddr            /* I: LAS image file descriptor record    */
)

{
  unsigned short las_zone = 0; /* geoTIFF coordinate system code  */
  
  if (ddr.datum_code == 0) /* Clarke 1866 (NAD27) */
  {
    if (ddr.zone_code == 101)
      las_zone = 26729;  /* Alabama East           */
    else if (ddr.zone_code == 102)
      las_zone = 26730;  /* Alabama West           */
    else if (ddr.zone_code == 201)
      las_zone = 26748;  /* Arizona East           */
    else if (ddr.zone_code == 202)
      las_zone = 26749;  /* Arizona Central        */
    else if (ddr.zone_code == 203)
      las_zone = 26750;  /* Arizona West           */
    else if (ddr.zone_code == 301)
      las_zone = 26751;  /* Arkansas North         */
    else if (ddr.zone_code == 302)
      las_zone = 26752;  /* Arkansas South         */
    else if (ddr.zone_code == 401)
      las_zone = 26741;  /* California I           */
    else if (ddr.zone_code == 402)
      las_zone = 26742;  /* California II          */
    else if (ddr.zone_code == 403)
      las_zone = 26743;  /* California III         */
    else if (ddr.zone_code == 404)
      las_zone = 26744;  /* California IV          */
    else if (ddr.zone_code == 405)
      las_zone = 26745;  /* California V           */
    else if (ddr.zone_code == 406)
      las_zone = 26746;  /* California VI          */
    else if (ddr.zone_code == 407)
      las_zone = 26747;  /* California VII         */
    else if (ddr.zone_code == 501)
      las_zone = 26753;  /* Colorado North         */
    else if (ddr.zone_code == 502)
      las_zone = 26754;  /* Colorado Central       */
    else if (ddr.zone_code == 503)
      las_zone = 26755;  /* Colorado South         */
    else if (ddr.zone_code == 600)
      las_zone = 26756;  /* Connecticut            */
    else if (ddr.zone_code == 700)
      las_zone = 26757;  /* Delaware               */
    else if (ddr.zone_code == 901)
      las_zone = 26758;  /* Florida East           */
    else if (ddr.zone_code == 902)
      las_zone = 26759;  /* Florida West           */
    else if (ddr.zone_code == 903)
      las_zone = 26760;  /* Florida North          */
    else if (ddr.zone_code == 1001)
      las_zone = 26766;  /* Georgia East           */
    else if (ddr.zone_code == 1002)
      las_zone = 26767;  /* Georgia West           */
    else if (ddr.zone_code == 1101)
      las_zone = 26768;  /* Idaho East             */
    else if (ddr.zone_code == 1102)
      las_zone = 26769;  /* Idaho Central          */
    else if (ddr.zone_code == 1103)
      las_zone = 26770;  /* Idaho West             */
    else if (ddr.zone_code == 1201)
      las_zone = 26771;  /* Illinois East          */
    else if (ddr.zone_code == 1202)
      las_zone = 26772;  /* Illinois West          */
    else if (ddr.zone_code == 1301)
      las_zone = 26773;  /* Indiana East           */
    else if (ddr.zone_code == 1302)
      las_zone = 26774;  /* Indiana West           */
    else if (ddr.zone_code == 1401)
      las_zone = 26775;  /* Iowa North             */
    else if (ddr.zone_code == 1402)
      las_zone = 26776;  /* Iowa South             */
    else if (ddr.zone_code == 1501)
      las_zone = 26777;  /* Kansas North           */
    else if (ddr.zone_code == 1502)
      las_zone = 26778;  /* Kansas South           */
    else if (ddr.zone_code == 1601)
      las_zone = 26779;  /* Kentucky North         */
    else if (ddr.zone_code == 1602)
      las_zone = 26780;  /* Kentucky South         */
    else if (ddr.zone_code == 1701)
      las_zone = 26781;  /* Louisiana North        */
    else if (ddr.zone_code == 1702)
      las_zone = 26782;  /* Louisiana South        */
    else if (ddr.zone_code == 1801)
      las_zone = 26783;  /* Maine East             */
    else if (ddr.zone_code == 1802)
      las_zone = 26784;  /* Maine West             */
    else if (ddr.zone_code == 1900)
      las_zone = 26785;  /* Maryland               */
    else if (ddr.zone_code == 2001)
      las_zone = 26786;  /* Massachusetts          */
    else if (ddr.zone_code == 2002)
      las_zone = 26787;  /* Massachusetts Island   */
    else if (ddr.zone_code == 2101)
      las_zone = 26801;  /* Michigan East (TM)     */
    else if (ddr.zone_code == 2102)
      las_zone = 26802;  /* Michigan Central (TM)  */
    else if (ddr.zone_code == 2103)
      las_zone = 26803;  /* Michigan West (TM)     */
    else if (ddr.zone_code == 2111)
      las_zone = 26788;  /* Michigan North         */
    else if (ddr.zone_code == 2112)
      las_zone = 26789;  /* Michigan Central       */
    else if (ddr.zone_code == 2113)
      las_zone = 26790;  /* Michigan South         */
    else if (ddr.zone_code == 2201)
      las_zone = 26791;  /* Minnesota North        */
    else if (ddr.zone_code == 2202)
      las_zone = 26792;  /* Minnesota Central      */
    else if (ddr.zone_code == 2203)
      las_zone = 26793;  /* Minnesota South        */
    else if (ddr.zone_code == 2301)
      las_zone = 26794;  /* Mississippi East       */
    else if (ddr.zone_code == 2302)
      las_zone = 26795;  /* Mississippi West       */
    else if (ddr.zone_code == 2401)
      las_zone = 26796;  /* Missouri East          */
    else if (ddr.zone_code == 2402)
      las_zone = 26797;  /* Missouri Central       */
    else if (ddr.zone_code == 2403)
      las_zone = 26798;  /* Missouri West          */
    else if (ddr.zone_code == 2501)
      las_zone = 32001;  /* Montana North          */
    else if (ddr.zone_code == 2502)
      las_zone = 32002;  /* Montana Central        */
    else if (ddr.zone_code == 2503)
      las_zone = 32003;  /* Montana South          */
    else if (ddr.zone_code == 2601)
      las_zone = 32005;  /* Nebraska North         */
    else if (ddr.zone_code == 2602)
      las_zone = 32006;  /* Nebraska South         */
    else if (ddr.zone_code == 2701)
      las_zone = 32007;  /* Nevada East            */
    else if (ddr.zone_code == 2702)
      las_zone = 32008;  /* Nevada Central         */
    else if (ddr.zone_code == 2703)
      las_zone = 32009;  /* Nevada West            */
    else if (ddr.zone_code == 2800)
      las_zone = 32010;  /* New Hampshire          */
    else if (ddr.zone_code == 2900)
      las_zone = 32011;  /* New Jersey             */
    else if (ddr.zone_code == 3001)
      las_zone = 32012;  /* New Mexico East        */
    else if (ddr.zone_code == 3002)
      las_zone = 32013;  /* New Mexico Central     */
    else if (ddr.zone_code == 3003)
      las_zone = 32014;  /* New Mexico West        */
    else if (ddr.zone_code == 3101)
      las_zone = 32015;  /* New York East          */
    else if (ddr.zone_code == 3102)
      las_zone = 32016;  /* New York Central       */
    else if (ddr.zone_code == 3103)
      las_zone = 32017;  /* New York West          */
    else if (ddr.zone_code == 3104)
      las_zone = 32018;  /* New York Long Island   */
    else if (ddr.zone_code == 3200)
      las_zone = 32019;  /* North Carolina         */
    else if (ddr.zone_code == 3301)
      las_zone = 32020;  /* North Dakota North     */
    else if (ddr.zone_code == 3302)
      las_zone = 32021;  /* North Dakota South     */
    else if (ddr.zone_code == 3401)
      las_zone = 32022;  /* Ohio North             */
    else if (ddr.zone_code == 3402)
      las_zone = 32023;  /* Ohio South             */
    else if (ddr.zone_code == 3501)
      las_zone = 32024;  /* Oklahoma North         */
    else if (ddr.zone_code == 3502)
      las_zone = 32025;  /* Oklahoma South         */
    else if (ddr.zone_code == 3601)
      las_zone = 32026;  /* Oregon North           */
    else if (ddr.zone_code == 3602)
      las_zone = 32027;  /* Oregon South           */
    else if (ddr.zone_code == 3701)
      las_zone = 32028;  /* Pennsylvania North     */
    else if (ddr.zone_code == 3702)
      las_zone = 32029;  /* Pennsylvania South     */
    else if (ddr.zone_code == 3800)
      las_zone = 32030;  /* Rhode Island           */
    else if (ddr.zone_code == 3901)
      las_zone = 32031;  /* South Carolina North   */
    else if (ddr.zone_code == 3902)
      las_zone = 32033;  /* South Carolina South   */
    else if (ddr.zone_code == 4001)
      las_zone = 32034;  /* South Dakota North     */
    else if (ddr.zone_code == 4002)
      las_zone = 32035;  /* South Dakota South     */
    else if (ddr.zone_code == 4100)
      las_zone = 32036;  /* Tennessee              */
    else if (ddr.zone_code == 4201)
      las_zone = 32037;  /* Texas North            */
    else if (ddr.zone_code == 4202)
      las_zone = 32038;  /* Texas North Central    */
    else if (ddr.zone_code == 4203)
      las_zone = 32039;  /* Texas Central          */
    else if (ddr.zone_code == 4204)
      las_zone = 32040;  /* Texas South Central    */
    else if (ddr.zone_code == 4205)
      las_zone = 32041;  /* Texas South            */
    else if (ddr.zone_code == 4301)
      las_zone = 32042;  /* Utah North             */
    else if (ddr.zone_code == 4302)
      las_zone = 32043;  /* Utah Central           */
    else if (ddr.zone_code == 4303)
      las_zone = 32044;  /* Utah North             */
    else if (ddr.zone_code == 4400)
      las_zone = 32045;  /* Vermont                */
    else if (ddr.zone_code == 4501)
      las_zone = 32046;  /* Virginia North         */
    else if (ddr.zone_code == 4502)
      las_zone = 32047;  /* Virginia South         */
    else if (ddr.zone_code == 4601)
      las_zone = 32048;  /* Washington North       */
    else if (ddr.zone_code == 4602)
      las_zone = 32049;  /* Washington South       */
    else if (ddr.zone_code == 4701)
      las_zone = 32050;  /* West Virginia North    */
    else if (ddr.zone_code == 4702)
      las_zone = 32051;  /* West Virginia South    */
    else if (ddr.zone_code == 4801)
      las_zone = 32052;  /* Wisconsin North        */
    else if (ddr.zone_code == 4802)
      las_zone = 32053;  /* Wisconsin Central      */
    else if (ddr.zone_code == 4803)
      las_zone = 32054;  /* Wisconsin South        */
    else if (ddr.zone_code == 4901)
      las_zone = 32055;  /* Wyoming East           */
    else if (ddr.zone_code == 4902)
      las_zone = 32056;  /* Wyoming East Central   */
    else if (ddr.zone_code == 4903)
      las_zone = 32057;  /* Wyoming West Central   */
    else if (ddr.zone_code == 4904)
      las_zone = 32058;  /* Wyoming West           */
    else if (ddr.zone_code == 5001)
      las_zone = 26731;  /* Alaska zone 1          */
    else if (ddr.zone_code == 5002)
      las_zone = 26732;  /* Alaska zone 2          */
    else if (ddr.zone_code == 5003)
      las_zone = 26733;  /* Alaska zone 3          */
    else if (ddr.zone_code == 5004)
      las_zone = 26734;  /* Alaska zone 4          */
    else if (ddr.zone_code == 5005)
      las_zone = 26735;  /* Alaska zone 5          */
    else if (ddr.zone_code == 5006)
      las_zone = 26736;  /* Alaska zone 6          */
    else if (ddr.zone_code == 5007)
      las_zone = 26737;  /* Alaska zone 7          */
    else if (ddr.zone_code == 5008)
      las_zone = 26738;  /* Alaska zone 8          */
    else if (ddr.zone_code == 5009)
      las_zone = 26739;  /* Alaska zone 9          */
    else if (ddr.zone_code == 5010)
      las_zone = 26740;  /* Alaska zone 10         */
    else if (ddr.zone_code == 5101)
      las_zone = 26761;  /* Hawaii zone 1          */
    else if (ddr.zone_code == 5102)
      las_zone = 26762;  /* Hawaii zone 2          */
    else if (ddr.zone_code == 5103)
      las_zone = 26763;  /* Hawaii zone 3          */
    else if (ddr.zone_code == 5104)
      las_zone = 26764;  /* Hawaii zone 4          */
    else if (ddr.zone_code == 5105)
      las_zone = 26765;  /* Hawaii zone 5          */
    else if (ddr.zone_code == 5201)
      las_zone = 32059;  /* Puerto Rico            */
    else if (ddr.zone_code == 5202)
      las_zone = 32060;  /* Virgin Islands St Croix*/
  }
  else if (ddr.datum_code == 8) /* GRS 1980 (NAD83) */
  {
    if (ddr.zone_code == 101)
      las_zone = 26929;  /* Alabama East           */
    else if (ddr.zone_code == 102)
      las_zone = 26930;  /* Alabama West           */
    else if (ddr.zone_code == 201)
      las_zone = 26948;  /* Arizona East           */
    else if (ddr.zone_code == 202)
      las_zone = 26949;  /* Arizona Central        */
    else if (ddr.zone_code == 203)
      las_zone = 26950;  /* Arizona West           */
    else if (ddr.zone_code == 301)
      las_zone = 26951;  /* Arkansas North         */
    else if (ddr.zone_code == 302)
      las_zone = 26952;  /* Arkansas South         */
    else if (ddr.zone_code == 401)
      las_zone = 26941;  /* California I           */
    else if (ddr.zone_code == 402)
      las_zone = 26942;  /* California II          */
    else if (ddr.zone_code == 403)
      las_zone = 26943;  /* California III         */
    else if (ddr.zone_code == 404)
      las_zone = 26944;  /* California IV          */
    else if (ddr.zone_code == 405)
      las_zone = 26945;  /* California V           */
    else if (ddr.zone_code == 406)
      las_zone = 26946;  /* California VI          */
    else if (ddr.zone_code == 501)
      las_zone = 26953;  /* Colorado North         */
    else if (ddr.zone_code == 502)
      las_zone = 26954;  /* Colorado Central       */
    else if (ddr.zone_code == 503)
      las_zone = 26955;  /* Colorado South         */
    else if (ddr.zone_code == 600)
      las_zone = 26956;  /* Connecticut            */
    else if (ddr.zone_code == 700)
      las_zone = 26957;  /* Delaware               */
    else if (ddr.zone_code == 901)
      las_zone = 26958;  /* Florida East           */
    else if (ddr.zone_code == 902)
      las_zone = 26959;  /* Florida West           */
    else if (ddr.zone_code == 903)
      las_zone = 26960;  /* Florida North          */
    else if (ddr.zone_code == 1001)
      las_zone = 26966;  /* Georgia East           */
    else if (ddr.zone_code == 1002)
      las_zone = 26967;  /* Georgia West           */
    else if (ddr.zone_code == 1101)
      las_zone = 26968;  /* Idaho East             */
    else if (ddr.zone_code == 1102)
      las_zone = 26969;  /* Idaho Central          */
    else if (ddr.zone_code == 1103)
      las_zone = 26970;  /* Idaho West             */
    else if (ddr.zone_code == 1201)
      las_zone = 26971;  /* Illinois East          */
    else if (ddr.zone_code == 1202)
      las_zone = 26972;  /* Illinois West          */
    else if (ddr.zone_code == 1301)
      las_zone = 26973;  /* Indiana East           */
    else if (ddr.zone_code == 1302)
      las_zone = 26974;  /* Indiana West           */
    else if (ddr.zone_code == 1401)
      las_zone = 26975;  /* Iowa North             */
    else if (ddr.zone_code == 1402)
      las_zone = 26976;  /* Iowa South             */
    else if (ddr.zone_code == 1501)
      las_zone = 26977;  /* Kansas North           */
    else if (ddr.zone_code == 1502)
      las_zone = 26978;  /* Kansas South           */
    else if (ddr.zone_code == 1601)
      las_zone = 26979;  /* Kentucky North         */
    else if (ddr.zone_code == 1602)
      las_zone = 26980;  /* Kentucky South         */
    else if (ddr.zone_code == 1701)
      las_zone = 26981;  /* Louisiana North        */
    else if (ddr.zone_code == 1702)
      las_zone = 26982;  /* Louisiana South        */
    else if (ddr.zone_code == 1801)
      las_zone = 26983;  /* Maine East             */
    else if (ddr.zone_code == 1802)
      las_zone = 26984;  /* Maine West             */
    else if (ddr.zone_code == 1900)
      las_zone = 26985;  /* Maryland               */
    else if (ddr.zone_code == 2001)
      las_zone = 26986;  /* Massachusetts          */
    else if (ddr.zone_code == 2002)
      las_zone = 26987;  /* Massachusetts Island   */
    else if (ddr.zone_code == 2111)
      las_zone = 26988;  /* Michigan North         */
    else if (ddr.zone_code == 2112)
      las_zone = 26989;  /* Michigan Central       */
    else if (ddr.zone_code == 2113)
      las_zone = 26990;  /* Michigan South         */
    else if (ddr.zone_code == 2201)
      las_zone = 26991;  /* Minnesota North        */
    else if (ddr.zone_code == 2202)
      las_zone = 26992;  /* Minnesota Central      */
    else if (ddr.zone_code == 2203)
      las_zone = 26993;  /* Minnesota South        */
    else if (ddr.zone_code == 2301)
      las_zone = 26994;  /* Mississippi East       */
    else if (ddr.zone_code == 2302)
      las_zone = 26995;  /* Mississippi West       */
    else if (ddr.zone_code == 2401)
      las_zone = 26996;  /* Missouri East          */
    else if (ddr.zone_code == 2402)
      las_zone = 26997;  /* Missouri Central       */
    else if (ddr.zone_code == 2403)
      las_zone = 26998;  /* Missouri West          */
    else if (ddr.zone_code == 2500)
      las_zone = 32100;  /* Montana                */
    else if (ddr.zone_code == 2600)
      las_zone = 32104;  /* Nebraska               */
    else if (ddr.zone_code == 2701)
      las_zone = 32107;  /* Nevada East            */
    else if (ddr.zone_code == 2702)
      las_zone = 32108;  /* Nevada Central         */
    else if (ddr.zone_code == 2703)
      las_zone = 32109;  /* Nevada West            */
    else if (ddr.zone_code == 2800)
      las_zone = 32110;  /* New Hampshire          */
    else if (ddr.zone_code == 2900)
      las_zone = 32111;  /* New Jersey             */
    else if (ddr.zone_code == 3001)
      las_zone = 32112;  /* New Mexico East        */
    else if (ddr.zone_code == 3002)
      las_zone = 32113;  /* New Mexico Central     */
    else if (ddr.zone_code == 3003)
      las_zone = 32114;  /* New Mexico West        */
    else if (ddr.zone_code == 3101)
      las_zone = 32115;  /* New York East          */
    else if (ddr.zone_code == 3102)
      las_zone = 32116;  /* New York Central       */
    else if (ddr.zone_code == 3103)
      las_zone = 32117;  /* New York West          */
    else if (ddr.zone_code == 3104)
      las_zone = 32118;  /* New York Long Island   */
    else if (ddr.zone_code == 3200)
      las_zone = 32119;  /* North Carolina         */
    else if (ddr.zone_code == 3301)
      las_zone = 32120;  /* North Dakota North     */
    else if (ddr.zone_code == 3302)
      las_zone = 32121;  /* North Dakota South     */
    else if (ddr.zone_code == 3401)
      las_zone = 32122;  /* Ohio North             */
    else if (ddr.zone_code == 3402)
      las_zone = 32123;  /* Ohio South             */
    else if (ddr.zone_code == 3501)
      las_zone = 32124;  /* Oklahoma North         */
    else if (ddr.zone_code == 3502)
      las_zone = 32125;  /* Oklahoma South         */
    else if (ddr.zone_code == 3601)
      las_zone = 32126;  /* Oregon North           */
    else if (ddr.zone_code == 3602)
      las_zone = 32127;  /* Oregon South           */
    else if (ddr.zone_code == 3701)
      las_zone = 32128;  /* Pennsylvania North     */
    else if (ddr.zone_code == 3702)
      las_zone = 32129;  /* Pennsylvania South     */
    else if (ddr.zone_code == 3800)
      las_zone = 32130;  /* Rhode Island           */
    else if (ddr.zone_code == 3900)
      las_zone = 32133;  /* South Carolina         */
    else if (ddr.zone_code == 4001)
      las_zone = 32134;  /* South Dakota North     */
    else if (ddr.zone_code == 4002)
      las_zone = 32135;  /* South Dakota South     */
    else if (ddr.zone_code == 4100)
      las_zone = 32136;  /* Tennessee              */
    else if (ddr.zone_code == 4201)
      las_zone = 32137;  /* Texas North            */
    else if (ddr.zone_code == 4202)
      las_zone = 32138;  /* Texas North Central    */
    else if (ddr.zone_code == 4203)
      las_zone = 32139;  /* Texas Central          */
    else if (ddr.zone_code == 4204)
      las_zone = 32140;  /* Texas South Central    */
    else if (ddr.zone_code == 4205)
      las_zone = 32141;  /* Texas South            */
    else if (ddr.zone_code == 4301)
      las_zone = 32142;  /* Utah North             */
    else if (ddr.zone_code == 4302)
      las_zone = 32143;  /* Utah Central           */
    else if (ddr.zone_code == 4303)
      las_zone = 32144;  /* Utah North             */
    else if (ddr.zone_code == 4400)
      las_zone = 32145;  /* Vermont                */
    else if (ddr.zone_code == 4501)
      las_zone = 32146;  /* Virginia North         */
    else if (ddr.zone_code == 4502)
      las_zone = 32147;  /* Virginia South         */
    else if (ddr.zone_code == 4601)
      las_zone = 32148;  /* Washington North       */
    else if (ddr.zone_code == 4602)
      las_zone = 32149;  /* Washington South       */
    else if (ddr.zone_code == 4701)
      las_zone = 32150;  /* West Virginia North    */
    else if (ddr.zone_code == 4702)
      las_zone = 32151;  /* West Virginia South    */
    else if (ddr.zone_code == 4801)
      las_zone = 32152;  /* Wisconsin North        */
    else if (ddr.zone_code == 4802)
      las_zone = 32153;  /* Wisconsin Central      */
    else if (ddr.zone_code == 4803)
      las_zone = 32154;  /* Wisconsin South        */
    else if (ddr.zone_code == 4901)
      las_zone = 32155;  /* Wyoming East           */
    else if (ddr.zone_code == 4902)
      las_zone = 32156;  /* Wyoming East Central   */
    else if (ddr.zone_code == 4903)
      las_zone = 32157;  /* Wyoming West Central   */
    else if (ddr.zone_code == 4904)
      las_zone = 32158;  /* Wyoming West           */
    else if (ddr.zone_code == 5001)
      las_zone = 26931;  /* Alaska zone 1          */
    else if (ddr.zone_code == 5002)
      las_zone = 26932;  /* Alaska zone 2          */
    else if (ddr.zone_code == 5003)
      las_zone = 26933;  /* Alaska zone 3          */
    else if (ddr.zone_code == 5004)
      las_zone = 26934;  /* Alaska zone 4          */
    else if (ddr.zone_code == 5005)
      las_zone = 26935;  /* Alaska zone 5          */
    else if (ddr.zone_code == 5006)
      las_zone = 26936;  /* Alaska zone 6          */
    else if (ddr.zone_code == 5007)
      las_zone = 26937;  /* Alaska zone 7          */
    else if (ddr.zone_code == 5008)
      las_zone = 26938;  /* Alaska zone 8          */
    else if (ddr.zone_code == 5009)
      las_zone = 26939;  /* Alaska zone 9          */
    else if (ddr.zone_code == 5010)
      las_zone = 26940;  /* Alaska zone 10         */
    else if (ddr.zone_code == 5101)
      las_zone = 26961;  /* Hawaii zone 1          */
    else if (ddr.zone_code == 5102)
      las_zone = 26962;  /* Hawaii zone 2          */
    else if (ddr.zone_code == 5103)
      las_zone = 26963;  /* Hawaii zone 3          */
    else if (ddr.zone_code == 5104)
      las_zone = 26964;  /* Hawaii zone 4          */
    else if (ddr.zone_code == 5105)
      las_zone = 26965;  /* Hawaii zone 5          */
    else if (ddr.zone_code == 5200)
      las_zone = 32161;  /* Puerto Rico Virgin Isl */
  }
  return (las_zone);
}
