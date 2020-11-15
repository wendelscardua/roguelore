#!/usr/bin/ruby
# frozen_string_literal: true

# compresses maps from asm bytes to binary
# (tile $62 = bit 1, tile $72 = bit 0)
# bits are stored from msb to lsb
# usage:
# ruby bitfy.rb file.s file.bin

input, output = ARGV

TILE_TO_BIT = {
  0x62 => '1',
  0x72 => '0'
}.freeze

bytes = []
File.read(input).lines.each do |line|
  bytes += line.scan(/(?<=\$)[0-9a-f]{2}/).map { |byte| byte.to_i(16) }
end

compressed_bytes = bytes.each_slice(8)
                        .map { |slice| slice.map { |tile| TILE_TO_BIT[tile] }.join.to_i(2) }

File.open(output, 'wb') do |f|
  f.write compressed_bytes.pack('C*')
end
