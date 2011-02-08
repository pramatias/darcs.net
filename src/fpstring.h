#include <HsFFI.h>
#include <sys/types.h>

int has_funky_char(const char *s, int len);

void conv_to_hex(unsigned char *dest, unsigned char *from, int num_chars);
void conv_from_hex(unsigned char *dest, unsigned char *from, int num_chars);
