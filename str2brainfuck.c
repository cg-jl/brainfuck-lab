#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <getopt.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

// assuming the interpreter groups '+' and '-' for you, it's
// blazing fast. it also runs on 1-byte interpreters, which
// are very efficient in memory.
// as you can see this is pretty simple.
void byte(uint8_t next, uint8_t last) {
  uint8_t diff = next > last ? next - last : last - next;
  uint8_t direction = next > last ? '+' : '-';

  for (uint8_t i = 0; i < diff; i++) putchar(direction);
}

// gets UTF8 length for an initial sequence.
size_t get_length(uint8_t value) {
  if (value & 0x80 == 0) return 1;
  // 2-byte
  if (value & 0xe0 == 0xc0) return 2;
  // 3-byte
  if (value & 0xf0 == 0xe0) return 3;
  // 4-byte
  if (value & 0xf8 == 0xf0) return 4;
  return 0;
}

/// parses escapes such as '\\', '\n', '\e' and '\t'
/// into the escaped meaning value before passint them into
/// the printer.
int unescape(const uint8_t *input, uint8_t *output, const uint8_t **next) {
  if (!*input) return 0;
  if (*input == '\\') {
    input++;
    uint8_t value = 0;
    switch (*input) {
      case 'n': value = '\n'; break;
      case 't': value = '\t'; break;
      case '\\': value = '\\'; break;
      case 'e': value = 0x1b; break;
      default:
                // use 
                strcpy(output, "unknown escape: ");
                size_t l = get_length(*input);
                memcpy(output + 16, input, l);
                output[16 + l] = 0;
                return -1;
    }
    *output = value;
  }
  else {
    *output = *input;
  }
  output[1] = 0;
  *next = input + 1;
  return 1;
}

int main(int argc, uint8_t *const* argv) {

  {
    int opt;
    int longindex = 0;
    while ((opt = getopt(argc, argv, "+h")) != -1) {
      switch (opt) {
        case 'h':
          fprintf(stderr, "Usage: %s [OPTIONS] STRING ...\n", *argv);
          fputs("The following backslash escapes are accepted:\n\
              \\ -> literal backslash\n\
              \e -> ANSI escape (0x1b)\n\
              \n -> line feed\n\
              \t -> horizontal tab\n", stdout);
          return 1;
      }
    }
  }



  uint8_t output[20];
  int result = 0;
  uint8_t last = 0;
  int i = optind;
  for (const char* arg = argv[i]; i < argc ; arg = argv[++i]) {
    while (*arg && (result = unescape(arg, output, &arg)) != 0) {
      if (result < 0) {
        fputs(output, stderr);
        fputc('\n', stderr);
        return 1;
      }
      byte(*output, last);
      putchar('.');
      last = *output;
    }
    if (i != argc - 1) {
      byte(' ', last);
      putchar('.');
      last = ' ';
    }
  }
}

