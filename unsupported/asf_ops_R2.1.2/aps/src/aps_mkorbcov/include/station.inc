     -/'@(#)station.inc	5.1 98/01/08 APS/ASF\0'/

*.
*   Mask.inc - station mask variables
*
*   History
*	 7/30/91 - move DATA statements to BlockStation.for
*	 5/24/91 - increased MaxAzSteps from 60 to 90
*	 3/25/91 - added tmStation (transformation matrix from IJK to SEZ)
*	 5/ 7/90 - increased MaxStations to 50
*	 3/23/90 - increased MaxStations from 20 to 30
*
*   Last modified
*	7/30/91 17:08
*..
	integer MaxStations,MaxAzSteps
	parameter(MaxStations=50,MaxAzSteps=90)

	integer NumStations

	double precision StationLatd(MaxStations),StationLon(MaxStations)
	double precision StationHeight(MaxStations)
	double precision rStation(3,MaxStations)
	double precision tmStation(3,3,MaxStations)

	character*20 StationName(MaxStations)

	integer AzSteps(MaxStations)

	double precision MaskElev(MaxAzSteps,MaxStations)
	double precision MinElev(MaxStations),MaxElev(MaxStations)

	logical StationSelected(MaxStations)

c..aap	common /eos$mask/ NumStations,StationLatd,StationLon,StationHeight,
c..aap     .				rStation,tmStation,StationName,
c..aap     .				AzSteps,MaskElev,MinElev,MaxElev,
c..aap     .				StationSelected
	common /eos$mask/ StationLatd,StationLon,StationHeight,
     .				rStation,tmStation,
     .				MaskElev,MinElev,MaxElev,
     .				NumStations,AzSteps,StationName,
     .				StationSelected

