#!/usr/bin/ruby
# frozen_string_literal: true

# converts readable enemy-table.yaml into binary
# usage:
# ruby generate-enemy-table file.yaml file.bin

input, output = ARGV

require 'yaml'

data = YAML.load_file(input)

bytes = data['enemies-per-level'].flat_map do |level_data|
  level_data.chars.map { |char| data['agent-types'][char] }
end

File.open(output, 'wb') do |f|
  f.write bytes.pack('C*')
end
