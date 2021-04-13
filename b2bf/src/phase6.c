#include <stdio.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdint.h>
#include <ctype.h>

uint8_t highest_div_pair(uint8_t val) {
  // compute the highest pair of numbers which give *val* as a result.
  for (uint8_t i = 16; i >= 2; i--) {
    if (val % i != 0) continue;
    return i;
  }
  return 1;
}

void byte(uint8_t next, uint8_t last) {
  if (last == next) return;
  // compute difference and direction.
  uint8_t diff, direction;
  {
    uint8_t diff1 = next - last;
    uint8_t diff2 = last - next;
    if (diff1 < diff2) {
      direction = '+';
      diff = diff1;
    } else {
      diff = diff2;
      direction = '-';
    }
  }

  uint8_t max, min;
  {
    uint8_t div1 = highest_div_pair(diff);
    uint8_t div2 = diff / div1;
    max = div1 > div2 ? div1 : div2;
    min = div1 > div2 ? div2 : div1;
  }

  if (min == 1) {
    // it's prime. use naive way.
    for (uint8_t i = 0; i < max; i++) putchar(direction);
    return;
  }

  // prepare the loop counter in the adjacent cell.
  putchar('>');
  for (uint8_t i = 0; i < max; i++) putchar('+');

  // loop.
  fputs("[<", stdout);
  for (uint8_t i = 0; i < min; i++) putchar(direction);
  fputs(">-]<", stdout);

}

void print_byte(uint8_t next, uint8_t last) {
  byte(next, last);
  putchar('.');
}

int main(int argc, char *const *argv) {
  const char* fname = NULL;
  FILE* input = stdin;
  bool is_message = false;  

  const struct option options[] = {
    { .name = "input", .has_arg = 1, .flag = NULL, .val = 'i' },
    { .name =  "message", .has_arg = 0, .flag = &is_message, .val = true },
    { 0 }
  };

  // argument parsing.
  {
    int opt;
    int longindex = 0;
    while ((opt = getopt_long(argc, argv, "+i:m", options, &longindex)) != -1) {
      switch (opt) {
        case 'i':
          fname = optarg;
          break;
        case 'm':
          is_message = true;
          break;
        case '?':
          fprintf(stderr, "usage: %s [-i --input <file>] [-m --message]\n\
              -i --input        process <file> as input instead of stdin.\n\
              -m --message      message mode: output is filtered for printable characters and\n\
                                ends with newline.\n", *argv);
          return 1;
      }
    }
  }

  if (fname != NULL) {
    input = fopen(fname, "rb");
    if (input == NULL) { perror("fopen"); return 1; }
  }


  uint8_t last = 0;
  for (uint8_t c; !feof(input) && fread(&c, 1, 1, input) == 1; last = c) {
    if (is_message && !isprint(c)) continue;
    print_byte(c, last);
  }

  if (is_message) print_byte('\n', last);

  if (input != stdin) fclose(input);


}
