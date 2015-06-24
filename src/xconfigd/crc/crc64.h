#include <stdint.h>
#include <stdlib.h>

uint64_t crc64(uint64_t crc, const char* data, size_t size);
uint64_t crc64_string(uint64_t crc, const char* data);
uint64_t crc64_byte(uint64_t crc, const char data);
