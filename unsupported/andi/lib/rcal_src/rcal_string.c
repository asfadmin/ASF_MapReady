/* rcal_string.c:  calibration subroutines for manipulating strings  
 *
 * these are used principally for manipulating file names
 * or converting types; they are used by the program 'disa.c',
 * for example.
 *
 */

#include LIBINC

void no_null_strcpy(t, s) 
char *t, *s;
{ while (*s != '\0')  { *t++ = *s++; } }

void string_to_string_format (s, t) 
char *s, *t;
{  
  int count = 0;

  /* increment s to start of character field */
  s += 13;

  /* while (source !depleted and !at end of character field) { */
  while (*t != '\0' && count < 28) {
    *s++ = *t++; 
    count++;
  }
}

void integer_to_string_format(s, i)
char *s;
int i;
{
  int j, k, dc, digit;
  char sgn[1];

  /* calc sign character */
  sgn[0] = (i < 0) ? '-' : ' ';
 
  /* calc number of digits dc */
  j = abs(i); dc = 1;
  while (j > 9) { j /= 10; dc++; }

  /* write sign character */
  *(s + 29 - dc) = sgn[0];

  /* write dc digits */
  for (j = 0, k = abs(i); j < dc; j++){
    digit = k - (k/10)*10;
    k /= 10; 
    *(s + 29 - j) = '0' + digit;
  }
  
  /* write CR followed by 12 nulls */
  *(s + 30) = '\n';
  for (j = 0; j < 12; j++) {
    *(s + 31 + j) = '\0';
  }

  return;
}

void double_to_string_format(s, a)
char *s;
double a;
{
  char *sp = s + 14, mant_sgn[1], exp_sgn[1], exp[2], mant[9];
  int i, ec, ind;
  double fc;

  /* exponent/mantissa calcs */
  for (i = 0; i < 9; i++) mant[i] = '0';
  mant_sgn[0] = (a < 0.0) ? '-' : ' ';
  a = fabs(a);
  exp_sgn[0] = (a < .10) ? '-' : '+';
  ec = 0;
  if (a == 0.0) {
    exp_sgn[0] = '+';
    exp[0] = '0';
    exp[1] = '0';
  }
  else if (a < .10) { 
    fc = 10.0;
    while (a < .10) { a *= fc; ec++; }
    ind = 0;
    while (a != 0.0 && ind < 9){
      i = (int)(fc*a);
      mant[ind] = '0' + i; ind++;
      a = (a*fc) - (double)((int)(a*fc));
    }
  }
  else if (a >= 1.0) {
    fc = 0.10;
    while (a >= 1.0) { a *= fc; ec++; }
    ind = 0; fc = 10.0;
    while (a != 0.0 && ind < 9){
      i = (int)(fc*a);
      mant[ind] = '0' + i; ind++;
      a = (a*fc) - (double)((int)(a*fc));
    }
  }
  else {
    fc = 10.0; ind = 0; 
    while (a != 0.0 && ind < 9){
      i = (int)(fc*a);
      mant[ind] = '0' + i; ind++;
      a = (a*fc) - (double)((int)(a*fc));
    }
  }
  exp[0] = '0' + (int)(ec/10);
  exp[1] = '0' + ec - ((int)(ec/10)*10);

  /* lead sign */
  *sp = mant_sgn[0]; sp++;

  /* zero, decimal */
  *sp = '0'; sp++;
  *sp = '.'; sp++;

  /* mantissa in 9 digits */
  for (i = 0; i < 9; i++){ *sp = mant[i]; sp++; }

  /* 'D', exponent sign */
  *sp = 'D'; sp++;
  *sp = exp_sgn[0]; sp++;

  /* exponent */
  *sp = exp[0]; sp++;
  *sp = exp[1]; sp++;

  /* CR followed by 12 nulls */
  *sp = '\n'; sp++;
  for (i = 0; i < 12; i++) {*sp = '\0'; sp++;}

  return;
}

void itoa(n, s)
int n;
char s[];
{
  int i, j, sign;

  sign = n;
  if (sign < 0) n = -n;
  i = 0;
  do {
    s[i] = n%10 + '0';
    i++;
  } while ((n /= 10) > 0);
  if (sign < 0) {
    s[i] = '-';
    i++;
  }
  s[i] = '\0';
  reverse(s);
}

void reverse(s)
char s[];
{
  int c, i, j;
  for (i = 0, j = strlen(s)-1; i < j; i++, j--) {
    c    = s[i];
    s[i] = s[j];
    s[j] = c;
  }
}

void new_fnm_extension(old, new, ext)
char *old, *new, *ext;
{
  int i = 0;

  while (*(old+i)!='.' && *(old+i)!='\0') {
    *(new+i) = *(old+i);
    i++;
  }
  strcpy ((new+i), ext);
  return;
}

/* a is input file; b is leader file */
void rcal_create_leader_filename(a, b)
char *a, *b;
{
  size_t length;
  int    i;
  char   *c;

  length = strlen(a);
  i = 0;
  while (*(a+i) != '.' && i < (int)(length)) {
    *(b+i) = *(a+i); i++;
  }
  c = b+i;
  strcpy (c, ".ldr");
  *(b+i+4) = '\0';

  return;
}

/* a is input file; b is trailer file */
void rcal_create_trailer_filename(a, b)
char *a, *b;
{
  size_t length;
  int    i;
  char   *c;

  length = strlen(a);
  i = 0;
  while (*(a+i) != '.' && i < (int)(length)) {
    *(b+i) = *(a+i); i++;
  }

  c = b+i;
  strcpy (c, ".tlr");
  *(b+i+4) = '\0';

  return;
}

/* a is input file; b is data descriptor file */
void rcal_create_dadescri_filename(a, b)
char *a, *b;
{
  size_t length;
  int    i;
  char   *c;

  length = strlen(a);
  i = 0;
  while (*(a+i) != '.' && i < (int)(length)) {
    *(b+i) = *(a+i); i++;
  }
  c = b+i;
  strcpy (c, ".dadescri");
  *(b+i+9) = '\0';

  return;
}

