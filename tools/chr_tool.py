#!/usr/bin/env python3
"""
chr_tool: Convert between PNG and CHR formats.
"""
import argparse
import logging
import os

from PIL import Image
from PIL.PngImagePlugin import PngImageFile


def indexes_from_image(image: PngImageFile, sprite_idx: int) -> list:
    assert(0 <= sprite_idx <= 255)
    sprite_x = (sprite_idx % 16) * 8
    sprite_y = (sprite_idx // 16) * 8
    assert (0 <= sprite_x <= (128 - 8))
    assert (0 <= sprite_y <= (128 - 8))
    ret = [0 for _ in range(0, 64)]
    for y in range(0, 8):
        for x in range(0, 8):
            ret[y * 8 + x] = image.getpixel((x + sprite_x, y + sprite_y))
    assert(len(ret) == 64)
    return ret


def pixel_values_to_chr(values: list) -> bytes:
    assert(len(values) == 64)
    # First color bit
    ret_list = [0 for _ in range(0, 16)]
    for byte_id in range(0, 8):
        plane0_byte_val = 0
        plane1_byte_val = 0
        for pixel_id in range(0, 8):
            plane0_byte_val |= (values[byte_id * 8 + pixel_id] & 0x01) << (7 - pixel_id)
            plane1_byte_val |= ((values[byte_id * 8 + pixel_id] >> 1) & 0x01) << (7 - pixel_id)
        ret_list[byte_id] = plane0_byte_val
        ret_list[byte_id + 8] = plane1_byte_val
    return bytes(ret_list)


def png_to_chr(png_file: str, chr_file: str):
    logging.info("PNG to CHR: {} {}".format(png_file, chr_file))
    im = Image.open(png_file)
    # Do some checks
    if not isinstance(im, PngImageFile):
        logging.error("Expected PNG image file")
        return 1
    if im.size != (128, 128):
        # Note: Could change this code to handle any multiple of 8 pixels, but 128x128 is fairly conventional
        logging.error("Expected 128 x 128 image, got {}".format(im.size))
        return 1
    pixel_range = im.getextrema()
    if pixel_range != (0, 3):
        # TODO lax this up a max and a min
        logging.error("Expected indexed data in the range (0, 3), got {}".format(pixel_range))
        return 1
    # Load in each sprite
    sprite_pixel_values = [indexes_from_image(im, idx) for idx in range(0, 256)]
    # Convert to CHR-formatted sprites
    sprite_chr_data = [pixel_values_to_chr(x) for x in sprite_pixel_values]
    # Stitch together
    chr_file_content = b"".join(sprite_chr_data)
    assert(len(chr_file_content) == 4096)
    open(chr_file, "wb").write(chr_file_content)
    return 0


def chr_to_pixel_values(chr_data: bytes):
    assert(len(chr_data) == 16)
    int_data = list(chr_data)
    pixel_values = [0 for _ in range(0, 64)]
    for pixel_id in range(0, 64):
        byte_id_1 = pixel_id // 8
        byte_id_2 = byte_id_1 + 8
        byte_1 = int_data[byte_id_1]
        byte_2 = int_data[byte_id_2]
        bit_id = 7 - (pixel_id % 8)
        bit_1 = (byte_1 >> bit_id) & 0x01
        bit_2 = (byte_2 >> bit_id) & 0x01
        idx = (bit_1 << 1) | bit_2
        pixel_values[pixel_id] = idx
    return pixel_values


def add_indexes_to_image(image: Image, sprite_pixel_values: list, sprite_idx):
    assert(len(sprite_pixel_values) == 64)
    assert(0 <= sprite_idx <= 255)
    sprite_x = (sprite_idx % 16) * 8
    sprite_y = (sprite_idx // 16) * 8
    assert (0 <= sprite_x <= (128 - 8))
    assert (0 <= sprite_y <= (128 - 8))
    for y in range(0, 8):
        for x in range(0, 8):
            image.putpixel((x + sprite_x, y + sprite_y), sprite_pixel_values[y * 8 + x])

def chr_to_png(chr_file: str, png_file: str):
    data_bytes = open(chr_file, "rb").read()
    if len(data_bytes) != 4096:
        logging.error("Expected exactly 4096 bytes in {}, got {}".format(chr_file, len(data_bytes)))
    # Calculate per-sprite pixel values
    sprite_chr_data = [data_bytes[sprite_idx * 16:(sprite_idx + 1) * 16] for sprite_idx in range(0, 256)]
    sprite_pixel_values = [chr_to_pixel_values(x) for x in sprite_chr_data]
    # Set up new image and palette
    im = Image.new(mode="P", size=(128, 128), color=0)
    im_palette = [0 for _ in range(0, 768)]
    display_cols = [
        (0x00, 0x00, 0x00),
        (0x7c, 0x7c, 0x7c),
        (0xbc, 0xbc, 0xbc),
        (0xfc, 0xfc, 0xfc)
    ]
    for x in range(0, 4):
        for y in range(0, 3):
            im_palette[x * 3 + y] = display_cols[x][y]
    im.putpalette(im_palette)
    # Paste in the sprites
    for sprite_idx in range(0, 256):
        add_indexes_to_image(im, sprite_pixel_values[sprite_idx], sprite_idx)
    im.save(png_file)


def main():
    parser = argparse.ArgumentParser(description='Tool to read and write NES CHR graphics data. PNG files must be a '
                                                 '2bpp pixel map at 128x128, while CHR files must be 4096 bytes each')
    parser.add_argument('--verbose', '-v', action='count', default=0)
    parser.add_argument('input', type=str, help='Input filename (required). If this ends with ".PNG" extension, '
                                                'then it will be read and converted to CHR. If it does not, '
                                                'then it will be read as a CHR file, and converted to PNG.',
                        metavar='INPUT')
    parser.add_argument('--output', type=str, help='Output filename', metavar='OUTPUT')
    args = parser.parse_args()

    # Set log level
    if args.verbose > 1:
        log_level = logging.DEBUG
    elif args.verbose == 1:
        log_level = logging.INFO
    else:
        log_level = logging.WARNING
    logging.basicConfig(level=log_level)

    # Actually run
    if args.input.endswith('.png') or args.input.endswith('.PNG'):
        output_filename = os.path.splitext(args.input)[0]+'.chr' if args.output is None else args.output
        return png_to_chr(args.input, output_filename)
    else:
        output_filename = os.path.splitext(args.input)[0]+'.png' if args.output is None else args.output
        return chr_to_png(args.input, output_filename)


if __name__ == "__main__":
    ret = main()
    exit(ret)
