#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"

/*******************************************************************************
 * meta_is_valid_char:
 *   Tests to see if the value is MAGIC_UNSET_CHAR. If so return FALSE (not
 *   valid) otherwise return TRUE (valid) */
int meta_is_valid_char(char value)
{
	return (value==MAGIC_UNSET_CHAR) ? FALSE : TRUE;
}

/*******************************************************************************
 * meta_is_valid_string:
 *   Tests to see if the value is MAGIC_UNSET_STRING. If so return FALSE (not
 *   valid) otherwise return TRUE (valid) */
int meta_is_valid_string(char *value)
{
	return (!strcmp(value,MAGIC_UNSET_STRING)) ? FALSE : TRUE;
}

/*******************************************************************************
 * meta_is_valid_int:
 *   Tests to see if the value is MAGIC_UNSET_INT. If so return FALSE (not
 *   valid) otherwise return TRUE (valid) */
int meta_is_valid_int(int value)
{
	return (value==MAGIC_UNSET_INT) ? FALSE : TRUE;
}


/*******************************************************************************
 * meta_is_valid_double:
 *   tests to see if the value is equal to itself... if not, it is NaN and 
 *   therefore invalid. Returns TRUE for valid, FALSE for invalid */
int meta_is_valid_double(double value)
{
	return (value==MAGIC_UNSET_DOUBLE) ? FALSE : TRUE;
}
