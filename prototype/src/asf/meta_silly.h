/* OLD metadata code, never used anywhere */

#if 0 /* This has been superceded by asf/meta.h's enum-based approach. */

/********************* Image Metadata Handling *******************
This is based on Joe Lovick's idea of "intelligent metadata fields",
where a particular field of metadata (e.g., the slant range to first
pixel) can be:
   - Pointing to an ordinary "double" value (wrapped in an object like metadata_storage).
   - Pointing to an object that just aborts when requested (for a nonexistent metadata value).
   - Pointing to an object that calls another object to get the *old* value,
      applies some computation, and then returns the computed version.
The nice part about keeping metadata as *objects* is that it lets
all three of these cases be handled efficiently, and updated piece-by-piece.
*/

/**
  Superclass for all reference-counted metadata objects.
*/
class ASF_COREDLL metadata_refcount {
	int refcount;
protected:
	/// Private destructor--use unref() to throw away.
	virtual ~metadata_refcount();
public:
	/// Create new object.  Must call ref().
	metadata_refcount() :refcount(0) {}
	inline void ref(void) {refcount++;}
	void unref(void) {refcount--; if (refcount<=0) delete this;}
};

/**
  Supplies one field of image metadata.
  For example, metadata_field_src<int> returns an integer.
*/
template <class T>
class ASF_COREDLL metadata_source : public metadata_refcount {
	bool m_exists;
public:
	metadata_source(bool exists_=true) :m_exists(exists_) {}
	/** Get the value of this metadata field.  Throws an exception if the field doesn't exist. */
	virtual T get(void) =0;
	/** Return true if this metata field exists (and "get" will work) */ 
	inline bool exists(void) {return m_exists;}
};

/** Called when a nonexistent metadata field is requested. */
void metadata_bogus_request(const char *fieldName);
/** Rejects requests for one field of image metadata. */
template <class T>
class ASF_COREDLL metadata_bogus : public metadata_source<T> {
public:
	const char *fieldName;
	metadata_bogus(const char *fieldName_) 
		:metadata_source<T>(false), fieldName(fieldName_) {}
	
	/* Get the value of this metadata field. */
	virtual T get(void) { metadata_bogus_request(fieldName); return *(T *)0; }
};

/**
  Stores one field of image metadata.
  For example, metadata_field_storage<int> keeps an integer.
*/
template <class T>
class ASF_COREDLL metadata_storage : public metadata_source<T> {
public:
	T value;
	metadata_storage() {}
	metadata_storage(T v) :value(v) {} 
	
	/* Get the value of this metadata field. */
	virtual T get(void) {return value;}
};

/**
  Keeps a growing metadata log string.
*/
class ASF_COREDLL metadata_log_storage : public metadata_source<std::string> {
public:
	metadata_log_storage(const std::string &val,metadata_source<std::string> *older);
	~metadata_log_storage();
	
	/* Get the value of this metadata field. */
	virtual std::string get(void);
public:
	std::string log;
	metadata_source<std::string> *older;
};

/**
  Represents one field of image metadata.
  A thin wrapper around the actual metadata_src object, mostly
  to provide reference counting.
*/
template <class T, class metadata_src=metadata_source<T> >
class ASF_COREDLL metadata_field {
	typedef metadata_field<T,metadata_src> this_t;
	metadata_src *src;
public:
	/** Default constructor: point to bogus source object that just aborts */
	inline metadata_field(const char *fieldName) { src=new metadata_bogus<T>(fieldName); src->ref(); }
	/** Build field to store this value */
	metadata_field(T f) { src=new metadata_storage<T>(f); src->ref(); }
	/** Point to this specified source object */
	inline metadata_field(metadata_src *s) { src=s; src->ref(); }
	/** Copy this metadata field */
	inline metadata_field(const metadata_field<T> &f) { src=f.src; src->ref(); }
	/** Assign from this value */
	this_t &operator=(T f) { replace(new metadata_storage<T>(f)); return *this; }
	/** Assign from this source object */
	this_t &operator=(metadata_src *s) { replace(s); return *this; }
	/** Assign from this metadata field */
	this_t &operator=(const metadata_field<T> &f) { replace(f.src); return *this; }
	/** Replace our source with this (new?) source.  Works even if s == src. */
	void replace(metadata_src *s)
		{ metadata_src *old=src; s->ref(); src=s; old->unref(); }
	/** Throw away this field */
	inline ~metadata_field() {src->unref();}
	
	/** Extract the value of this field */
	inline operator T (void) const {return src->get();}
	inline T get(void) const {return src->get();}
	/** Return true if this field has a value (& "get" won't throw an exception) */
	inline bool exists(void) const {return src->exists();}
	
	/* Return the object that supplies our value. */
	inline metadata_src *getSource(void) {return src;}
};

/**
  Describes the source of this image--the satellite, orbit, date, etc.
*/
class ASF_COREDLL image_source_metadata {
public:
	metadata_field<std::string> sensor; ///< Imaging sensor (e.g., "ERS1")
	metadata_field<std::string> mode; ///< Sensor or beam mode (e.g., "FN7")
	metadata_field<std::string> processor; ///< SAR processor used (e.g., "AISP 2.07234")
	metadata_field<int> orbit; ///< Orbit number at start of aquisition
	metadata_field<int> satBinTime; ///< Satellite binary time at start of aquistion
	metadata_field<double> satClkTime; ///< Satellite clock time at start of aquistion

	image_source_metadata();
};

/**
  Describes all geometric and radiometric properties of an image.
*/
class ASF_COREDLL image_metadata {
public:
/* FIXME: Add all the other metadata:
	- SAR geometry information (timings, slant ranges, doppler, state vectors)
	- Geographic information (map projection type & scale)
	- Calibration information (noise & gain vs. range)
*/
	/// Describes the source of this image.  
	///   Never NULL, but might not exist (so get will return an error).
	metadata_field<image_source_metadata *> source;
	
	/// Describes the sequence of processing steps that led to this image.
	metadata_field<std::string,metadata_log_storage> log;
	
	/// Add this string to the metadata log:
	void add(const std::string &str);
	
	image_metadata();
};


/************* Metadata ***********/
asf::metadata_refcount::~metadata_refcount() {}

/** Called when a nonexistent metadata field is requested. */
void asf::metadata_bogus_request(const char *fieldName)
{
	die("Bogus metadata field "+(std::string)fieldName+" requested.");
}

asf::image_source_metadata::image_source_metadata() 
	:sensor("sensor"), mode("mode"), processor("processor"), 
	 orbit("orbit"), satBinTime("satBinTime"), satClkTime("satClkTime")
	{}

/**
  Keeps a growing metadata log string.
*/
asf::metadata_log_storage::metadata_log_storage(
	const std::string &val, metadata_source<std::string> *old)
	:log(val)
{
	if (old!=NULL && old->exists()) { older=old; older->ref(); }
	else older=NULL;
}

asf::metadata_log_storage::~metadata_log_storage() {
	if (older) older->unref();
}

/* Get the value of this metadata field. */
std::string asf::metadata_log_storage::get(void) {
	if (older) return older->get()+log;
	else return log;
}

std::string getDateString(void) {
	return "<FIXME: getDateString doesn't work yet>";
}

asf::image_metadata::image_metadata() 
	:source("source"), 
	log(new metadata_log_storage("Log started "+getDateString()+"\n",0))
{}

/* Append this value to our string */
void asf::image_metadata::add(const std::string &str) {
	log=new metadata_log_storage(str,log.getSource());
}


#endif
