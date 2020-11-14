#!/usr/bin/ruby
# frozen_string_literal: true

# converts .s nametable into .rle
# ruby rle.rb file.s file.rle

input, output = ARGV

bytes = []
File.read(input).lines.each do |line|
  bytes += line.scan(/(?<=\$)[0-9a-f]{2}/).map { |byte| byte.to_i(16) }
end

rle_tag = 0
rle_tag += 1 while bytes.include?(rle_tag)

compressed_bytes = [rle_tag]
last_byte = nil
counter = 0
bytes.each do |byte|
  if byte != last_byte
    if counter.positive?
      compressed_bytes += [rle_tag, counter]
      counter = 0
    end
    compressed_bytes << byte
    last_byte = byte
  else
    counter += 1
  end
end
if counter.positive?
  compressed_bytes += [rle_tag, counter]
  counter = 0
end
compressed_bytes += [rle_tag, 0]

File.open(output, 'wb') do |f|
  f.write compressed_bytes.pack('C*')
end
