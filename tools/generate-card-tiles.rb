#!/usr/bin/ruby
# frozen_string_literal: true

# generates precomputed tiles for cards
#
# each card is made of 4 columns, each column with 6 tiles
# first column has two variations depending on the card being or not the first on the hand

blank_card = [
  [0x56, 0x3a, 0x3a, 0x4b],
  [0x2a, 0x4a, 0x4a, 0x2b],
  [0x2a, 0x4a, 0x4a, 0x2b],
  [0x2a, 0x4a, 0x4a, 0x2b],
  [0x2a, 0x4a, 0x4a, 0x2b],
  [0x48, 0x3b, 0x3b, 0x57]
]

joker_card = [
  [0x56, 0x3a, 0x3a, 0x4b],
  [0x2a, 0x8a, 0x8b, 0x2b],
  [0x2a, 0x9a, 0x9b, 0x2b],
  [0x2a, 0xaa, 0xab, 0x2b],
  [0x2a, 0xba, 0xbb, 0x2b],
  [0x48, 0x3b, 0x3b, 0x57]
]

def megatile(top_left_tile)
  [
    [top_left_tile, top_left_tile + 1],
    [top_left_tile + 0x10, top_left_tile + 0x11]
  ]
end

ranks = %i[ace two three four five six seven eight nine ten jack queen king]
suits = %i[hearts diamonds spades clubs]

# indexed by rank
corner_tiles = {
  ace: megatile(0x00),
  two: megatile(0x02),
  three: megatile(0x04),
  four: megatile(0x06),
  five: megatile(0x08),
  six: megatile(0x0a),
  seven: megatile(0x0c),
  eight: megatile(0x0e),
  nine: megatile(0x20),
  ten: megatile(0x22),
  jack: megatile(0x24),
  queen: megatile(0x26),
  king: megatile(0x28)
}

alt_corner_tiles = {
  ace: 0x40,
  two: 0x41,
  three: 0x42,
  four: 0x43,
  five: 0x44,
  six: 0x45,
  seven: 0x46,
  eight: 0x47,
  nine: 0x50,
  ten: 0x51,
  jack: 0x52,
  queen: 0x53,
  king: 0x54
}

anti_corner_tiles = {
  ace: megatile(0x00),
  two: megatile(0x02),
  three: megatile(0x04),
  four: megatile(0x06),
  five: megatile(0x08),
  six: megatile(0x0a),
  seven: megatile(0x0c),
  eight: megatile(0x0e),
  nine: megatile(0x20),
  ten: megatile(0x22),
  jack: megatile(0x24),
  queen: megatile(0x26),
  king: megatile(0x28)
}

suit_tiles = {
  hearts: megatile(0x2c),
  diamonds: megatile(0x2e),
  spades: megatile(0x4c),
  clubs: megatile(0x4e)
}

anti_suit_tiles = {
  hearts: megatile(0x8c),
  diamonds: megatile(0x8e),
  spades: megatile(0xac),
  clubs: megatile(0xae)
}

%w[1 2 3 4 1b].each do |index|
  puts ".define strip_#{index}_pointers \\"
  suits.each do |suit|
    ranks.each do |rank|
      puts "        strip_#{index}_#{rank}_#{suit}, \\"
    end
  end
  puts "        strip_#{index}_joker"
  puts "strip_#{index}_pointers_l: .lobytes strip_#{index}_pointers"
  puts "strip_#{index}_pointers_h: .hibytes strip_#{index}_pointers"
end

suits.each do |suit|
  ranks.each do |rank|
    card_matrix = Marshal.load(Marshal.dump(blank_card))

    [[0, 0], [0, 1], [1, 0], [1, 1]].each do |row, column|
      card_matrix[row][column] = corner_tiles[rank][row][column]
      card_matrix[row + 2][column] = suit_tiles[suit][row][column]
      card_matrix[row + 2][column + 2] = anti_suit_tiles[suit][row][column]
      card_matrix[row + 4][column + 2] = anti_corner_tiles[rank][row][column]
    end

    card_matrix.transpose.each.with_index do |column_strip, index|
      bytes = column_strip.map { |tile| format('$%<byte>02X', byte: tile) }.join(', ')
      puts "strip_#{index + 1}_#{rank}_#{suit}: .byte #{bytes}"
    end

    card_matrix[0][0] = alt_corner_tiles[rank]
    card_matrix[5][0] = 0x49

    bytes = card_matrix.transpose.first.map { |tile| format('$%<byte>02X', byte: tile) }.join(', ')
    puts "strip_1b_#{rank}_#{suit}: .byte #{bytes}"
  end
end

joker_card.transpose.each.with_index do |column_strip, index|
  bytes = column_strip.map { |tile| format('$%<byte>02X', byte: tile) }.join(', ')
  puts "strip_#{index + 1}_joker: .byte #{bytes}"
end

joker_card[0][0] = 0x55
joker_card[5][0] = 0x49

bytes = joker_card.transpose.first.map { |tile| format('$%<byte>02X', byte: tile) }.join(', ')
puts "strip_1b_joker: .byte #{bytes}"
