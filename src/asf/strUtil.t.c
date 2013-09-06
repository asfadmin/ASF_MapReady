#include "CUnit/Basic.h"
#include "asf.h"

static int within_tol(double a, double b)
{
  return fabs(a-b) < .00001;
}

void test_strUtil()
{
  CU_ASSERT(strcmp(uc("abC123xYz"),"ABC123XYZ")==0);
  CU_ASSERT(strcmp_case("abCD","ABCD")==0);
  CU_ASSERT(strcmp(lc("abC123xYz"),"abc123xyz")==0);
  CU_ASSERT(strncmp_case("abcz","ABCx",3)==0);

  {
    char *s = appendStr("abc","123");
    CU_ASSERT(strcmp_case(s,"ABC123")==0);
    FREE(s);
  }
  {
    char s[6];
    char *t = "hello";
    strncpy_safe(s,t,2);
    CU_ASSERT(strcmp_case(s,"h")==0);
    strncpy_safe(s,t,6);
    CU_ASSERT(strcmp_case(s,"hello")==0);
  }
  {
    char *s = trim_spaces("  abc 123\t   \n");
    CU_ASSERT(strcmp(s,"abc 123")==0);
  }
  {
    char s[20]; strcpy(s,"abc123\n");
    chomp(s);
    CU_ASSERT(strcmp(s,"abc123")==0);
  }

  CU_ASSERT(endsWith("abc123","123")==TRUE);
  CU_ASSERT(endsWith("abc123","3")==TRUE);
  CU_ASSERT(endsWith("abc123","abc123")==TRUE);
  CU_ASSERT(endsWith("abc123","4")==FALSE);
  CU_ASSERT(endsWith("abc123","Xabc123")==FALSE);

  CU_ASSERT(count_char("abacad",'a')==3);
  CU_ASSERT(count_char("abacad",'d')==1);
  CU_ASSERT(count_char("abacad",'e')==0);

  {
    char *s = asf_strReplace("abc123","a","h");
    CU_ASSERT(strcmp(s,"hbc123")==0);
  }
  {
    char *s = asf_strReplace("abc123abc123","bc","h");
    CU_ASSERT(strcmp(s,"ah123ah123")==0);
  }
  {
    char *s = asf_strReplace("abc123abc123","123","456");
    CU_ASSERT(strcmp(s,"abc456abc456")==0);
  }
  {
    char *s = asf_strReplace("abc123abc123","zzz","456");
    CU_ASSERT(strcmp(s,"abc123abc123")==0);
  }
  {
    char *s = asf_strReplace("zzzazzzbc1zzz23azzzbczzz123zzz","zzz","");
    CU_ASSERT(strcmp(s,"abc123abc123")==0);
  }
  {
    char *s = asf_strReplace("zzz","zzz","");
    CU_ASSERT(strlen(s)==0);
  }

  CU_ASSERT(strcmp_case(strstr_case("abc123","123"),"123")==0);
  CU_ASSERT(strcmp_case(strstr_case("abcdef","CDE"),"cdef")==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","d"),"d",1)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","d"),"D",1)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","D"),"D",1)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","D"),"d",1)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","D"),"de",2)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","D"),"dE",2)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","D"),"dE",2)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","bcd"),"bcdXX",3)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","bcd"),"bcdeX",4)==0);
  CU_ASSERT(strncmp_case(strstr_case("abcdef","bcd"),"bcdef",5)==0);
  CU_ASSERT(strcmp_case(strstr_case("abcdef","bcd"),"bcdef")==0);

  {
    char *str = STRDUP("\"hello\",\"there  \", you ,\" jerk!!\"");
    int n;
    char **arr;
    split_into_array(str,',',&n,&arr);
    CU_ASSERT(n==4);
    CU_ASSERT(strcmp(arr[0],"hello")==0);
    CU_ASSERT(strcmp(arr[1],"there  ")==0);
    CU_ASSERT(strcmp(arr[2],"you")==0);
    CU_ASSERT(strcmp(arr[3]," jerk!!")==0);
    {
      char *s = trim_spaces(arr[1]);
      CU_ASSERT(strcmp(s,"there")==0);
    }
    {
      char *s = trim_spaces(arr[3]);
      CU_ASSERT(strcmp(s,"jerk!!")==0);
    }
    free_char_array(&arr,n);

    CU_ASSERT(strcmp(get_str(str,0),"hello")==0);
    CU_ASSERT(strcmp(get_str(str,1),"there  ")==0);
    CU_ASSERT(strcmp(get_str(str,2),"you")==0);
    CU_ASSERT(strcmp(get_str(str,3)," jerk!!")==0);
    CU_ASSERT(strcmp(get_str(str,-1),"")==0);
    CU_ASSERT(strcmp(get_str(str,4),"")==0);

    FREE(str);
  }
  {
    char *str = STRDUP("\"1.234\",4.567,8,\"9\",d,\"e\"");
    CU_ASSERT(within_tol(get_double(str,0),1.234));
    CU_ASSERT(within_tol(get_double(str,1),4.567));
    CU_ASSERT(get_int(str,2)==8);
    CU_ASSERT(get_long(str,3)==9);
    CU_ASSERT(get_long(str,2)==8);
    CU_ASSERT(get_int(str,3)==9);
    CU_ASSERT(get_char(str,4)=='d');
    CU_ASSERT(get_char(str,5)=='e');
    CU_ASSERT(get_int(str,5)==0);
  }
  {
    char *s = STRDUP("abcXdef");
    char *s1,*s2;
    split2(s,'X',&s1,&s2);
    CU_ASSERT(strcmp(s1,"abc")==0);
    CU_ASSERT(strcmp(s2,"def")==0);
    FREE(s1); FREE(s2);
    char *t = asf_strReplace(s,"X",",");
    split2(t,',',&s1,&s2);
    CU_ASSERT(strcmp(s1,"abc")==0);
    CU_ASSERT(strcmp(s2,"def")==0);
    FREE(s1); FREE(s2);
  }
  {
    char *s = STRDUP("abcdef");
    char *s1,*s2;
    split2(s,'X',&s1,&s2);
    CU_ASSERT(strcmp(s1,"abcdef")==0);
    CU_ASSERT(strcmp(s2,"")==0);
    FREE(s1); FREE(s2);
  }
  {
    char *s = STRDUP("abcdef");
    char *s1,*s2;
    split2(s,'a',&s1,&s2);
    CU_ASSERT(strcmp(s1,"")==0);
    CU_ASSERT(strcmp(s2,"bcdef")==0);
    FREE(s1); FREE(s2);
  }
}

