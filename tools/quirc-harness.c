/* A line protocol around quirc for tools/gen-decode-vectors.py. It includes quirc's
 * decode.c to reach the static correct_block and correct_format.
 *
 *   rs <parity bytes> <block hex>   -> ok <corrected hex> | fail
 *   fmt <15 format bits as read>    -> ok <level> <mask> | fail
 *   grid <size> <modules hex>       -> ok <version> <level> <mask> <payload hex> | err <message>
 *   image <width> <height> <path>   -> codes <n>, then per code:
 *                                      code <size> <8 corner coordinates> <modules hex>
 *                                      and an ok or err line as for grid
 *
 * Module grids are row-major, most significant bit first, as hex. A grid quirc cannot
 * decode is tried again mirrored, as quirc's own tools do.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "quirc.h"
#include "decode.c"

static const char LEVEL[] = "MLHQ";
static char line[1 << 18];

static int unhex(const char *s, uint8_t *out, int max)
{
	int n = 0;
	while (s[0] && s[1] && n < max) {
		unsigned v;
		if (sscanf(s, "%2x", &v) != 1)
			break;
		out[n++] = v;
		s += 2;
	}
	return n;
}

static void puthex(const uint8_t *b, int n)
{
	for (int i = 0; i < n; i++)
		printf("%02x", b[i]);
}

static void put_grid(const struct quirc_code *code)
{
	int n = code->size * code->size;
	uint8_t packed[QUIRC_MAX_BITMAP] = {0};
	for (int i = 0; i < n; i++)
		if (code->cell_bitmap[i >> 3] >> (i & 7) & 1)
			packed[i >> 3] |= 0x80 >> (i & 7);
	puthex(packed, (n + 7) / 8);
}

static void put_decoded(const struct quirc_code *code)
{
	struct quirc_data data;
	struct quirc_code mirrored = *code;
	quirc_decode_error_t err = quirc_decode(code, &data);
	if (err) {
		quirc_flip(&mirrored);
		if (!quirc_decode(&mirrored, &data))
			err = QUIRC_SUCCESS;
	}
	if (err) {
		printf("err %s\n", quirc_strerror(err));
		return;
	}
	printf("ok %d %c %d ", data.version, LEVEL[data.ecc_level], data.mask);
	puthex(data.payload, data.payload_len);
	printf("\n");
}

int main(void)
{
	static char arg[1 << 18];
	while (fgets(line, sizeof line, stdin)) {
		int a, b;
		if (sscanf(line, "rs %d %s", &a, arg) == 2) {
			uint8_t block[256];
			int bs = unhex(arg, block, sizeof block);
			struct quirc_rs_params p = {bs, bs - a, 1};
			if (correct_block(block, &p))
				printf("fail\n");
			else {
				printf("ok ");
				puthex(block, bs);
				printf("\n");
			}
		} else if (sscanf(line, "fmt %d", &a) == 1) {
			uint16_t u = a ^ 0x5412;
			if (correct_format(&u))
				printf("fail\n");
			else
				printf("ok %c %d\n", LEVEL[u >> 13 & 3], u >> 10 & 7);
		} else if (sscanf(line, "grid %d %s", &a, arg) == 2) {
			static struct quirc_code code;
			uint8_t packed[QUIRC_MAX_BITMAP];
			memset(&code, 0, sizeof code);
			code.size = a;
			unhex(arg, packed, sizeof packed);
			for (int i = 0; i < a * a; i++)
				if (packed[i >> 3] & 0x80 >> (i & 7))
					code.cell_bitmap[i >> 3] |= 1 << (i & 7);
			put_decoded(&code);
		} else if (sscanf(line, "image %d %d %s", &a, &b, arg) == 3) {
			struct quirc *q = quirc_new();
			FILE *f = fopen(arg, "rb");
			if (!q || !f || quirc_resize(q, a, b) < 0) {
				fprintf(stderr, "cannot load %s\n", arg);
				return 1;
			}
			uint8_t *buf = quirc_begin(q, NULL, NULL);
			if (fread(buf, 1, (size_t)a * b, f) != (size_t)a * b) {
				fprintf(stderr, "short read %s\n", arg);
				return 1;
			}
			fclose(f);
			quirc_end(q);
			printf("codes %d\n", quirc_count(q));
			for (int i = 0; i < quirc_count(q); i++) {
				static struct quirc_code code;
				quirc_extract(q, i, &code);
				printf("code %d", code.size);
				for (int k = 0; k < 4; k++)
					printf(" %d %d", code.corners[k].x, code.corners[k].y);
				printf(" ");
				put_grid(&code);
				printf("\n");
				put_decoded(&code);
			}
			quirc_destroy(q);
		} else {
			fprintf(stderr, "bad request: %s", line);
			return 1;
		}
		fflush(stdout);
	}
	return 0;
}
