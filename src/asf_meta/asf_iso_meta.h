#ifndef __ASF_ISO_META_H__
#define __ASF_ISO_META_H__

#include "asf.h"
#include "asf_meta.h"

#define ISO_META_VERSION 1.0

typedef enum {
  HH_POL=1,
  HV_POL,
  VH_POL,
  VV_POL,
  UNDEF_POL_LAYER
} iso_polLayer_t;

typedef enum {
  MAIN_TYPE=1,
  GEOREF_TYPE,
  GEOCODE_TYPE,
  OTHER_TYPE,
  UNDEF_TYPE
} iso_type_t;

typedef enum {
  OPERATIONAL=1,
  PREOPERATIONAL,
  INSTRUMENT,
  TEST_OPS,
  UNDEF_OPS
} iso_opsType_t;

typedef enum {
  AUTO_APPROVED=1,
  OPERATOR_APPROVED,
  NOT_APPROVED,
  UNDEF_QUALITY
} iso_quality_t;

typedef enum {
  ASCENDING=1,
  DESCENDING,
  UNDEF_ORBIT
} iso_orbit_t;

typedef enum {
  FINE_BEAM=1,
  STANDARD_BEAM,
  STRIPMAP_IMAGE,
  SCANSAR_IMAGE,
  SPOTLIGHT_IMAGE,
  UNDEF_IMAGE_MODE
} iso_imageMode_t;

typedef enum {
  LEFT_LOOK=1,
  RIGHT_LOOK,
  UNDEF_LOOK
} iso_lookDir_t;

typedef enum {
  SINGLE_POL=1,
  DUAL_POL,
  QUAD_POL,
  UNDEF_POL_MODE
} iso_polMode_t;

typedef enum {
  SLC_PRODUCT=1,
  STD_PRODUCT,
  TC_PRODUCT,
  RTC_PRODUCT,
  GEO_PRODUCT,
  SSC_PRODUCT, // original TerraSAR-X
  MGD_PRODUCT, // original TerraSAR-X
  GEC_PRODUCT, // original TerraSAR-X
  EEC_PRODUCT, // original TerraSAR-X
  UNDEF_PRODUCT
} iso_product_t;

typedef enum {
  SLANTRANGE_PROJ,
  GROUNDRANGE_PROJ,
  MAP_PROJ,
  UNDEF_PROJ
} iso_proj_t;

typedef enum {
  UTM_PROJ=1,
  PS_PROJ,
  GEOG_PROJ,
  UNDEF_MAP
} iso_mapProj_t;

typedef enum {
  SE_RES=1,
  RE_RES,
  UNDEF_RES
} iso_resolution_t;

typedef enum {
  CALIBRATED=1,
  RELCALIBRATED,
  NOTCALIBRATED,
  UNDEF_CAL
} iso_radio_t;

typedef enum {
  DETECTED_DATA_TYPE=1,
  COMPLEX_DATA_TYPE,
  RAW_DATA_TYPE,
  UNDEF_DATA_TYPE
} iso_dataType_t;

typedef enum {
  CEOS_DATA_FORMAT=1,
  GEOTIFF_DATA_FORMAT,
  HDF5_DATA_FORMAT,
  COSAR_DATA_FORMAT,
  UNDEF_DATA_FORMAT
} iso_dataFormat_t;

typedef enum {
  ROWBYROW=1,
  COLBYCOL,
  UNDEF_STORE
} iso_store_t;

typedef enum {
  RAW_COORD=1,
  ZERODOPPLER,
  UNDEF_COORD
} iso_imageCoord_t;

typedef enum {
  EARLYAZNEARRG=1,
  EARLYAZFARRG,
  LATEAZNEARRG,
  LATEAZFARRG,
  UNDEF_DATA_START
} iso_dataStart_t;

typedef enum {
  NEAREST_NEIGHBOR_RESAMPLE=1,
  BILINEAR_RESAMPLE,
  CUBIC_CONVOLUTION_RESAMPLE,
  UNDEF_RESAMPLE
} iso_resample_t;

typedef enum {
  PREDICTED_ORBIT=1,
  RESTITUTED_ORBIT,
  PRECISE_ORBIT,
  TLE,
  UNDEF_ORBIT_ACC
} iso_orbitAcc_t;

typedef enum {
  FRAME_SPEC=1,
  TIME_SPEC,
  CENTERCOORDS_SPEC,
  UNDEF_SCENE_SPEC
} iso_sceneSpec_t;

typedef enum {
  PREDICTED_SENSOR=1,
  SINGLE_GPS,
  DIFFERENTIAL_GPS,
  UNDEF_ORBIT_SENSOR
} iso_orbitSensor_t;

typedef enum {
  RANDOM_FILL=1,
  ZERO_FILL,
  UNDEF_FILL
} iso_fill_t;

typedef enum {
  PRE_PROCESSING=1,
  LEVEL_ZERO,
  LEVEL_ONE,
  LEVEL_TWO,
  UNDEF_PROC_LEVEL
} iso_procLevel_t;

typedef struct {
  int year;
  int month;
  int day;
  int hour;
  int min;
  double second;
} iso_dateTime;

typedef struct {
  char host[1024];
  char path[1024];
  char name[1024];
  long size;
} iso_file;

typedef struct {
  iso_type_t type;
  iso_file file;
} iso_filesType;

typedef struct {
  iso_polLayer_t polLayer;
  char beamID[20];
  iso_file file;
} iso_filesPol;

typedef struct {
  int refRow;
  int refColumn;
  double lat;
  double lon;
  iso_dateTime azimuthTimeUTC;
  double rangeTime;
  double incidenceAngle;
} iso_sceneCoord;

typedef struct {
  char imageDataFormat[255];
  int imageDataDepth;
  int numberOfRows;
  int numberOfColumns;
  double columnBlockLength;
  double rowBlockLength;
  double rowSpacing;
  double columnSpacing;
} iso_imageRaster;

typedef struct {
  char polarization[80];
  char color[20];
} iso_polColor;

typedef struct {
  double upperLeftLatitude;
  double upperLeftLongitude;
  double upperRightLatitude;
  double upperRightLongitude;
  double lowerLeftLatitude;
  double lowerLeftLongitude;
  double lowerRightLatitude;
  double lowerRightLongitude;
} iso_geoCoord;

typedef struct {
  double upperLeftEasting;
  double upperLeftNorthing;
  double upperRightEasting;
  double upperRightNorthing;
  double lowerRightEasting;
  double lowerRightNorthing;
  double lowerLeftEasting;
  double lowerLeftNorthing;
} iso_mapCoord;

typedef struct {
  char softwareID[128];
  char softwareVersion[128];
  iso_dateTime processingTimeUTC;
  char description[256];
  char algorithm[1024];
  iso_procLevel_t processingLevel;
} iso_procStep;

typedef struct {
  iso_dateTime timeUTC;
  double posX;
  double posY;
  double posZ;
  double velX;
  double velY;
  double velZ;
} iso_stateVec;

typedef struct {
  iso_dateTime startTimeUTC;
  iso_dateTime stopTimeUTC;
  int numberOfRows;
  double prf;
  double echoWindowPosition;
  double echoWindowLength;
  char pulseType[20];
} iso_settingRecord;

typedef struct {
  iso_polLayer_t polLayer;
  char beamID[20];
  double rxBandwidth;
  double rsf;
  int numberOfPRFChanges;
  int numberOfEchoWindowPositionChanges;
  int numberOfEchoWindowLengthChanges;
  int numberOfSettingRecords;
  iso_settingRecord *settingRecord;
} iso_settings;

typedef struct {
  iso_polLayer_t polLayer;
  int numberOfBlocks;
  int numberOfRejectedBlocks;
  int numberOfDopperRecords;
  iso_dateTime timeUTC;
  double dopplerAtMidRange;
  int polynomialDegree;
  double *coefficient;
} iso_dopplerCentroid;
  
typedef struct {
  iso_imageCoord_t processingInfoCoordinateType;
  float rangeLooks;
  float azimuthLooks;
  double rangeLookBandwidth;
  double azimuthLookBandwidth;
  double totalProcessedRangeBandwidth;
  double totalProcessedAzimuthBandwidth;
  double chirpRate;
  double pulseDuration;
} iso_processingParameter;  

typedef struct {
  unsigned long start;
  int length;
  iso_fill_t fill;
} iso_gap;

typedef struct {
  iso_polLayer_t polLayer;
  char beamID[20];
  int numGaps;
  iso_gap *gap;
  int gapSignificanceFlag;
  int missingLinesSignificanceFlag;
  int bitErrorSignificanceFlag;
  int timeReconstructionSignificanceFlag;
  // averageDopplerCentroidInfo
} iso_rawDataQuality;

typedef struct {
  iso_polLayer_t polLayer;
  char beamID[20];
  double min;
  double max;
  double mean;
  double stdDev;
  int missingLines;
  double bitErrorRate;
  double noData;
} iso_imageDataQuality;

// main structures
typedef struct {
  char itemName[80];
  char mission[20];
  char source[20];
  char destination[20];
  char generationSystem[80];
  iso_dateTime generationTime;
  char *referenceDocument;
  char *revision;
  char *revisionComment;
} iso_generalHeader;

typedef struct {
  iso_filesType *annotation;
  iso_filesPol *imageData;
  iso_filesType *auxRasterFiles;
  iso_filesPol *quicklooks;
  int numAnnotations;
  int numLayers;
  int numAuxRasterFiles;
  iso_file *compositeQuicklook;
  iso_file browseImage;
  iso_file mapPlot;
} iso_productComponents;

typedef struct {
  // generationInfo
  char logicalProductID[1024];
  char receivingStation[20];
  char level0ProcessingFacility[20];
  char level1ProcessingFacility[20];
  iso_opsType_t groundOperationsType;
  char deliveryInfo[1024];
  char copyrightInfo[1024];
  iso_quality_t qualityInspection;
  char *qualityRemark;
  // missionInfo
  char mission[1024];
  int orbitPhase;
  int orbitCycle;
  int absOrbit;
  int relOrbit;
  int numOrbitsInCycle;
  iso_orbit_t orbitDirection;
  // acquisitionInfo
  char sensor[20];
  iso_imageMode_t imageMode;
  iso_lookDir_t lookDirection;
  iso_polMode_t polarizationMode;
  iso_polLayer_t *polLayer;
  char elevationBeamConfiguration[20];
  char azimuthBeamID[20];          // stripMap, scanSAR
  int numberOfBeams;               // scanSAR
  char **beamID;                   // scanSAR
  int numberOfBursts;              // scanSAR
  int numberOfAzimuthBeams;        // spotLight
  char azimuthBeamIDFirst[20];     // spotLight
  char azimuthBeamIDLast[20];      // spotLight
  double azimuthSteeringAngleFirst; // spotLight
  double azimuthSteeringAngleLast;  // spotLight
  // productVariantInfo
  char productType[128];
  iso_product_t productVariant;
  iso_proj_t projection;
  iso_mapProj_t mapProjection;
  iso_resolution_t resolutionVariant;
  iso_radio_t radiometricCorrection;
  // imageDataInfo
  char pixelValueID[128];
  iso_dataType_t imageDataType;
  iso_dataFormat_t imageDataFormat;
  int numberOfLayers;
  int imageDataDepth;
  iso_store_t imageStorageOrder;
  char rowContent[20];
  char columnContent[20];
  int numberOfRows;
  int numberOfColumns;
  int startRow;
  int startColumn;
  double rowScaling;
  double columnScaling;
  double rowSpacing;
  double columnSpacing;
  double groundRangeResolution;
  double azimuthResolution;
  double azimuthLooks;
  double rangeLooks;
  // sceneInfo
  char sceneID[1024];
  iso_dateTime startTimeUTC;
  long startTimeGPS;
  double startTimeGPSFraction;
  iso_dateTime stopTimeUTC;
  long stopTimeGPS;
  double stopTimeGPSFraction;
  double rangeTimeFirstPixel;
  double rangeTimeLastPixel;
  double sceneAzimuthExtent;
  double sceneRangeExtent;
  iso_sceneCoord sceneCenterCoord;
  double sceneAverageHeight;
  iso_sceneCoord sceneCornerCoord[4];
  double yaw;
  double pitch;
  double roll;
  double earthRadius;
  double satelliteHeight;
  double headingAngle;
  // previewInfo
  iso_imageRaster quicklooks;
  char compositeQLImageDataFormat[255];
  int compositeQLImageDataDepth;
  iso_polColor *compositeQLPolLayerCode;
  char browseImageDataFormat[255];
  int browseImageDataDepth;
  char mapPlotFormat[255];
} iso_productInfo;

typedef struct {
  // complexImageInfo
  double commonPRF;
  double commonRSF;
  double slantRangeResolution;
  double projectedSpacingAzimuth;
  double projectedSpacingGroundNearRange;
  double projectedSpacingGroundFarRange;
  double projectedSpacingSlantRange;
  double slantRangeShift;
  iso_imageCoord_t imageCoordinateType;
  iso_dataStart_t imageDataStartWith;
  iso_dataStart_t quicklookDataStartWith;
  // geocodedImageInfo
  int geocodedImageInfoFlag; // image geocoded?
  // mapProjection
  char geodeticDatumID[20];
  char projectionID[50];
  char zoneID[20];
  double projectionCenterLatitude;
  double projectionCenterLongitude;
  double mapOriginEasting;
  double mapOriginNorthing;
  double scaleFactor;
  // geoParameter
  double pixelSpacingEasting;
  double pixelSpacingNorthing;
  iso_geoCoord frameCoordsGeographic;
  iso_mapCoord frameCoordsCartographic;
  iso_geoCoord sceneCoordsGeographic;
  iso_mapCoord sceneCoordsCartographic;
  double sceneCenterCoordLatitude;
  double sceneCenterCoordLongitude;
  double sceneCenterCoordEasting;
  double sceneCenterCoordNorthing;
  iso_resample_t imageResamplingMethod;
  // elevationData
  int elevationDataFlag; // elevation data included?
  char elevationDataSource[50];
  double elevationMinimumHeight;
  double elevationMeanHeight;
  double elevationMaximumHeight;
  // incidenceAngleMaskDescription
  int incidenceAngleMaskDescriptionFlag; // incidence angle mask included?
  char incidenceAnglePixelValueID[128];
  iso_dataFormat_t incidenceAngleImageDataFormat;
  int incidenceAngleImageDataDepth;
  int incidenceAngleNumberOfRows;
  int incidenceAngleNumberOfColumns;
  double incidenceAngleRowSpacing;
  double incidenceAngleColumnSpacing;
} iso_productSpecific;

typedef struct {
  char orderType[128];
  char processingPriority[20];
  iso_orbitAcc_t orbitAccuracy;
  iso_sceneSpec_t sceneSpecification;
  int frameID;
  iso_dateTime sceneStartTimeUTC;
  iso_dateTime sceneStopTimeUTC;
  double sceneCenterLatitude;
  double sceneCenterLongitude;
  iso_imageMode_t imagingMode;
  iso_lookDir_t lookDirection;
  iso_polMode_t polarizationMode;
  iso_polLayer_t polLayer;
  char elevationBeamConfiguration[20];
  iso_product_t productVariant;
  iso_resolution_t resolutionVariant;
  iso_proj_t projection;
  char logicalDataTakeID[1024];
  char level0ProductID[1024];
  iso_dateTime L0SARGenerationTimeUTC;
  int numProcessingSteps;
  iso_procStep *processingStep;
} iso_setup;

typedef struct {
  // Doppler
  char dopplerBasebandEstimationMethod[255];
  // dopplerGeometricEstimationMethod[20]
  iso_imageCoord_t dopplerCentroidCoordinateType;
  iso_dopplerCentroid *doppler;
  // processingParameter
  iso_processingParameter *processingParameter;  
  // processingFlags
  int chirpReplicaUsedFlag;
  int geometricDopplerUsedFlag;
  int azimuthPatternCorrectedFlag;
  int elevationPatternCorrectedFlag;
  int detectedFlag;
  int multiLookedFlag;
  int polarimetricProcessedFlag;
  int terrainCorrectedFlag;
  int layoverShadowMaskGeneratedFlag;
  int geocodedFlag;
  int nominalProcessingPerformedFlag;
} iso_processing;

typedef struct {
  iso_imageCoord_t instrumentInfoCoordinateType;
  double centerFrequency;
  int numSettings;
  iso_settings *settings;
} iso_instrument;

typedef struct {
  // empty shell for now
} iso_calibration;

typedef struct {
  iso_orbitSensor_t sensor;
  iso_orbitAcc_t accuracy;
  char stateVectorRefFrame[80];
  unsigned long numStateVectors;
  iso_dateTime firstStateTimeUTC;
  iso_dateTime lastStateTimeUTC;
  double stateVectorTimeSpacing;
  iso_stateVec *stateVec;
} iso_platform;

typedef struct {
  iso_rawDataQuality *rawDataQuality;
  int dopplerAmbiguityNotZeroFlag;
  int dopplerOutsideLimitsFlag;
  int geolocationQualityLowFlag;
  iso_imageDataQuality *imageDataQuality;
  int gapDefinition;
  double gapPercentageLimit;
  double missingLinePercentageLimit;
  double bitErrorLimit;
  double timeReconstructionPercentageLimit;
  double dopplerCentroidLimit;
  double geolocationQualityLimit;
  char *instrumentStateRemark;
} iso_productQuality;

typedef struct {
  
} iso_geoReference; // originally in a separate GEOREF file

// General ASF ISO metadta structure.  Collection of all above.
typedef struct {
  double meta_version;

  iso_generalHeader     *generalHeader;
  iso_productComponents *productComponents;
  iso_productInfo       *productInfo;
  iso_productSpecific   *productSpecific;
  iso_setup             *setup;
  iso_processing        *processing;
  iso_instrument        *instrument;
  iso_calibration       *calibration;
  iso_platform          *platform;
  iso_productQuality    *productQuality;
  iso_geoReference      *geoReference;
} iso_meta;

// Initialization routines
iso_meta *iso_meta_init(void);
iso_generalHeader *iso_generalHeader_init(void);
iso_productComponents *iso_productComponents_init(void);
iso_productInfo *iso_productInfo_init(void);
iso_productSpecific *iso_productSpecific_init(void);
iso_setup *iso_setup_init(void);
iso_processing *iso_processing_init(void);
iso_instrument *iso_instrument_init(void);
iso_calibration *iso_calibration_init(void);
iso_platform *iso_platform_init(void);
iso_productQuality *iso_productQuality_init(void);
void iso_meta_free(iso_meta *iso);

// Other functions
iso_meta *iso_meta_read(const char *xmlName);
void iso_meta_write(iso_meta *iso, const char *gapFile, const char *outName);
void iso_ext_meta_write(iso_meta *iso, const char *outName, 
			const char *fileIdentifier, 
			const char *iso_meta_configFile,
			const char *gapFile);
iso_meta *meta2iso(meta_parameters *meta);
meta_parameters *iso2meta(iso_meta *iso);

#endif
